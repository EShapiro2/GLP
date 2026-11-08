// The debugging log service (GLP/phones/docs/debugging.md, "The log
// service"): a batch posted is stamped with its arrival on each line and
// appended to its run, answered 204; the events are kept as sent, never read;
// the listing of phones and runs with each run's first and last event; a run
// returned whole and from an offset; a run followed as it grows, including
// one not yet begun; and a request with no password, or another, refused.
import 'dart:async';
import 'dart:convert';
import 'dart:io';

import 'package:debuglog_server/server.dart';
import 'package:debuglog_server/store.dart';
import 'package:test/test.dart';

const pw = 'sesame-0123';
const phoneA = 'a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c6d7e8f90';
const phoneB = '00ff00ff';

void main() {
  late Directory root;
  late HttpServer server;
  late String base;
  var t = 1762617600000;

  setUp(() async {
    root = Directory.systemTemp.createTempSync('debuglog');
    final service = DebuglogService(LogStore(root), pw, maxBatch: 1024, poll: const Duration(milliseconds: 20), now: () => t);
    server = await HttpServer.bind('127.0.0.1', 0);
    base = 'http://127.0.0.1:${server.port}';
    server.listen(service.handle);
  });
  tearDown(() async {
    await server.close(force: true);
    root.deleteSync(recursive: true);
  });

  Future<HttpClientResponse> send(String method, String path, {String? body, String? password = pw, HttpClient? client}) async {
    final c = client ?? HttpClient();
    final q = await c.openUrl(method, Uri.parse('$base$path'));
    if (password != null) q.headers.set(HttpHeaders.authorizationHeader, 'Bearer $password');
    if (body != null) q.add(utf8.encode(body));
    return q.close();
  }

  Future<(int, String)> call(String method, String path, {String? body, String? password = pw}) async {
    final r = await send(method, path, body: body, password: password);
    return (r.statusCode, await utf8.decodeStream(r));
  }

  String runFile(String phone, String run) => File('${root.path}/$phone/$run.jsonl').readAsStringSync();

  test('a batch is appended to its run, each line stamped with the arrival, answered 204', () async {
    final (s, b) = await call('POST', '/log/$phoneA/1762617599000',
        body: '{"t":1762617599100,"source":"app","kind":"build"}\n\n{"t":1762617599200,"source":"runtime","kind":"output","text":"[INIT] alice"}\r\n');
    expect((s, b), (204, ''));
    t += 2000;
    expect((await call('POST', '/log/$phoneA/1762617599000', body: '{"t":1762617601000,"source":"layer","kind":"onPeerLost"}')).$1, 204);
    final lines = const LineSplitter().convert(runFile(phoneA, '1762617599000'));
    expect(lines, [
      '{"arrived":1762617600000,"event":{"t":1762617599100,"source":"app","kind":"build"}}',
      '{"arrived":1762617600000,"event":{"t":1762617599200,"source":"runtime","kind":"output","text":"[INIT] alice"}}',
      '{"arrived":1762617602000,"event":{"t":1762617601000,"source":"layer","kind":"onPeerLost"}}',
    ]);
    expect(jsonDecode(lines[1])['event']['text'], '[INIT] alice');
  });

  test('the events are never read: a line that is not JSON is kept as it was sent', () async {
    expect((await call('POST', '/log/$phoneB/1', body: 'not json {\n')).$1, 204);
    expect(runFile(phoneB, '1'), '{"arrived":$t,"event":not json {}\n');
  });

  test('the listing gives the phones, their runs in launch order, and each run\'s first and last event', () async {
    expect(await call('GET', '/log/'), (200, '{\n  "phones": []\n}\n'));
    await call('POST', '/log/$phoneA/20', body: '{"n":1}\n{"n":2}\n');
    await call('POST', '/log/$phoneA/3', body: '{"n":3}\n');
    await call('POST', '/log/$phoneB/5', body: '{"n":4}\n{"n":5}\n{"n":6}\n');
    final (s, b) = await call('GET', '/log');
    expect(s, 200);
    final l = jsonDecode(b) as Map<String, dynamic>;
    expect([for (final p in l['phones']) p['phone']], [phoneB, phoneA]..sort());
    final a = (l['phones'] as List).firstWhere((p) => p['phone'] == phoneA)['runs'] as List;
    expect([for (final r in a) r['run']], ['3', '20']);
    expect(a[1]['first'], {'arrived': t, 'event': {'n': 1}});
    expect(a[1]['last'], {'arrived': t, 'event': {'n': 2}});
    expect(a[1]['bytes'], File('${root.path}/$phoneA/20.jsonl').lengthSync());
    final b5 = (l['phones'] as List).firstWhere((p) => p['phone'] == phoneB)['runs'] as List;
    expect(b5.single['first']['event'], {'n': 4});
    expect(b5.single['last']['event'], {'n': 6});
  });

  test('a run is returned whole, and from a byte offset', () async {
    await call('POST', '/log/$phoneA/7', body: '{"n":1}\n{"n":2}\n');
    final whole = runFile(phoneA, '7');
    final (s, b) = await call('GET', '/log/$phoneA/7');
    expect((s, b), (200, whole));
    final first = whole.indexOf('\n') + 1;
    expect(await call('GET', '/log/$phoneA/7?from=$first'), (200, whole.substring(first)));
    expect((await call('GET', '/log/$phoneA/8')).$1, 404);
    expect((await call('GET', '/log/$phoneA/7?from=-1')).$1, 400);
  });

  test('a run is followed as it grows, one not yet begun included, from an offset too', () async {
    final c = HttpClient();
    final r = await send('GET', '/log/$phoneA/9?follow', client: c);
    expect(r.statusCode, 200);
    final got = StringBuffer();
    final more = StreamController<void>.broadcast();
    final sub = r.transform(utf8.decoder).listen((s) {
      got.write(s);
      more.add(null);
    });
    Future<void> until(int lines) async {
      while ('\n'.allMatches(got.toString()).length < lines) {
        await more.stream.first.timeout(const Duration(seconds: 10));
      }
    }

    await call('POST', '/log/$phoneA/9', body: '{"n":1}\n');
    await until(1);
    t += 1;
    await call('POST', '/log/$phoneA/9', body: '{"n":2}\n{"n":3}\n');
    await until(3);
    expect(got.toString(), runFile(phoneA, '9'));
    await sub.cancel();
    c.close(force: true);

    final whole = runFile(phoneA, '9');
    final from = whole.indexOf('\n') + 1;
    final c2 = HttpClient();
    final r2 = await send('GET', '/log/$phoneA/9?follow&from=$from', client: c2);
    final first = await r2.transform(utf8.decoder).first.timeout(const Duration(seconds: 10));
    expect(whole.substring(from), startsWith(first));
    c2.close(force: true);
  });

  test('a request with no password, or another, is refused 401, and nothing is written', () async {
    for (final p in [null, 'wrong', '${pw}x', pw.substring(1)]) {
      final r = await send('POST', '/log/$phoneA/1', body: '{"n":1}\n', password: p);
      expect(r.statusCode, 401, reason: 'password $p');
      expect(r.headers.value(HttpHeaders.wwwAuthenticateHeader), 'Bearer');
      await r.drain<void>();
      expect((await call('GET', '/log/', password: p)).$1, 401);
      expect((await call('GET', '/log/$phoneA/1?follow', password: p)).$1, 401);
    }
    expect(Directory('${root.path}/$phoneA').existsSync(), isFalse);
  });

  test('names that are not a key in hex and a run number are refused, and nothing outside the store is reached', () async {
    expect((await call('POST', '/log/$phoneA/abc', body: '{}')).$1, 400);
    expect((await call('POST', '/log/xyz/1', body: '{}')).$1, 400);
    expect((await call('GET', '/log/..%2F..%2Fetc/1')).$1, 400);
    expect((await call('GET', '/log/$phoneA')).$1, 404);
    expect((await call('GET', '/other')).$1, 404);
    expect((await call('DELETE', '/log/$phoneA/1')).$1, 405);
    expect(root.listSync(), isEmpty);
  });

  test('a batch over the limit is refused 413 and not written', () async {
    expect((await call('POST', '/log/$phoneA/1', body: '{"x":"${'y' * 2000}"}\n')).$1, 413);
    expect(File('${root.path}/$phoneA/1.jsonl').existsSync(), isFalse);
  });
}
