// The debugging log service's HTTP interface (GLP/phones/docs/debugging.md,
// "The log service", 2 and 3):
//
//   POST /log/<phone>/<run>         a batch of events, one JSON object a line;
//                                   stamped and appended; 204
//   GET  /log/                      the phones and their runs, with each run's
//                                   size and first and last event, as JSON
//   GET  /log/<phone>/<run>         the run's file
//   GET  /log/<phone>/<run>?follow  the file, then what is appended to it, for
//                                   as long as the reader stays
//
// Every request carries the password, `Authorization: Bearer <password>`; one
// without it, or with another, is answered 401.  A GET of a run takes
// `from=<byte offset>` to start there, so a follower that reconnects asks for
// what it has not yet got.  A batch longer than the limit is answered 413.
import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'store.dart';

class DebuglogService {
  final LogStore store;
  final List<int> _password;
  final int maxBatch;
  final Duration poll;
  final int Function() now;

  DebuglogService(this.store, String password,
      {this.maxBatch = 64 << 20, this.poll = const Duration(milliseconds: 250), int Function()? now})
      : _password = utf8.encode(password),
        now = now ?? (() => DateTime.now().millisecondsSinceEpoch) {
    if (password.isEmpty) throw ArgumentError('the password is empty');
  }

  Future<void> handle(HttpRequest q) async {
    try {
      final seg = q.uri.pathSegments.where((s) => s.isNotEmpty).toList();
      if (seg.isEmpty || seg.first != 'log' || seg.length == 2 || seg.length > 3) {
        return await _end(q, HttpStatus.notFound, 'not found');
      }
      if (!_authorised(q)) {
        q.response.headers.set(HttpHeaders.wwwAuthenticateHeader, 'Bearer');
        return await _end(q, HttpStatus.unauthorized, 'the password is missing or wrong');
      }
      if (seg.length == 1) {
        if (q.method != 'GET') return await _end(q, HttpStatus.methodNotAllowed, 'GET only');
        return await _list(q);
      }
      final phone = seg[1], run = seg[2];
      if (!LogStore.validPhone(phone) || !LogStore.validRun(run)) {
        return await _end(q, HttpStatus.badRequest, 'a phone is a key in hex and a run a number');
      }
      if (q.method == 'POST') return await _post(q, phone, run);
      if (q.method == 'GET') return await _get(q, phone, run);
      return await _end(q, HttpStatus.methodNotAllowed, 'GET or POST');
    } catch (e) {
      try {
        await _end(q, HttpStatus.internalServerError, 'error');
      } catch (_) {}
      stderr.writeln('debuglog: ${q.method} ${q.uri}: $e');
    }
  }

  bool _authorised(HttpRequest q) {
    final h = q.headers.value(HttpHeaders.authorizationHeader);
    if (h == null || !h.startsWith('Bearer ')) return false;
    final got = utf8.encode(h.substring(7).trim());
    // In time independent of where the two differ.
    var diff = got.length ^ _password.length;
    for (var i = 0; i < got.length; i++) {
      diff |= got[i] ^ _password[i % _password.length];
    }
    return diff == 0;
  }

  Future<void> _post(HttpRequest q, String phone, String run) async {
    if (q.contentLength > maxBatch) return await _end(q, HttpStatus.requestEntityTooLarge, 'a batch is at most $maxBatch bytes');
    final arrived = now();
    final body = BytesBuilder(copy: false);
    await for (final b in q) {
      body.add(b);
      if (body.length > maxBatch) return await _end(q, HttpStatus.requestEntityTooLarge, 'a batch is at most $maxBatch bytes');
    }
    await store.append(phone, run, body.takeBytes(), arrived);
    q.response.statusCode = HttpStatus.noContent;
    await q.response.close();
  }

  Future<void> _list(HttpRequest q) async {
    final l = await store.list();
    final out = {
      'phones': [
        for (final e in l.entries) {'phone': e.key, 'runs': [for (final r in e.value) r.toJson()]}
      ]
    };
    q.response.headers.contentType = ContentType.json;
    q.response.write(const JsonEncoder.withIndent('  ').convert(out));
    q.response.write('\n');
    await q.response.close();
  }

  Future<void> _get(HttpRequest q, String phone, String run) async {
    final from = int.tryParse(q.uri.queryParameters['from'] ?? '0');
    if (from == null || from < 0) return await _end(q, HttpStatus.badRequest, 'from is a byte offset');
    final f = store.file(phone, run);
    final follow = q.uri.queryParameters.containsKey('follow');
    q.response.headers.contentType = ContentType('application', 'x-ndjson', charset: 'utf-8');
    if (!follow) {
      if (!await f.exists()) return await _end(q, HttpStatus.notFound, 'no such run');
      final len = await f.length();
      if (from < len) await q.response.addStream(f.openRead(from));
      return await q.response.close();
    }
    // Following: what the file holds from `from`, then what is appended, until
    // the reader goes.  A run not yet begun is waited for.  The headers go at
    // once, so the reader knows it is following before the first event, and
    // the body is written in chunks of its own on the socket, which also tells
    // when the reader goes.
    q.response.headers.set(HttpHeaders.cacheControlHeader, 'no-cache');
    q.response.headers.chunkedTransferEncoding = true;
    final sock = await q.response.detachSocket(writeHeaders: true);
    var gone = false;
    sock.listen((_) {}, onDone: () => gone = true, onError: (_) => gone = true, cancelOnError: true);
    var pos = from;
    try {
      await sock.flush();
      while (!gone) {
        final len = await f.exists() ? await f.length() : 0;
        if (len <= pos) {
          await Future<void>.delayed(poll);
          continue;
        }
        final r = await f.open();
        try {
          await r.setPosition(pos);
          while (pos < len) {
            final b = await r.read(len - pos < 1 << 16 ? len - pos : 1 << 16);
            if (b.isEmpty) break;
            sock.add(ascii.encode('${b.length.toRadixString(16)}\r\n'));
            sock.add(b);
            sock.add(const [0x0D, 0x0A]);
            pos += b.length;
          }
        } finally {
          await r.close();
        }
        await sock.flush();
      }
    } catch (_) {
      // The reader went in the middle of a write.
    }
    try {
      sock.add(ascii.encode('0\r\n\r\n'));
      await sock.flush();
    } catch (_) {}
    sock.destroy();
  }

  Future<void> _end(HttpRequest q, int status, String text) async {
    q.response.statusCode = status;
    q.response.headers.contentType = ContentType.text;
    q.response.write('$text\n');
    await q.response.close();
  }
}
