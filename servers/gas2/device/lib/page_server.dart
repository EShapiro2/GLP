// The device's page and its API, on the loopback: GET api/state, POST api/act,
// POST api/add (a person met from afar, by their key), and the page's files.
import 'dart:async';
import 'dart:convert';
import 'dart:io';

import 'host.dart';

class PageServer {
  final SuperHost host;
  final String www;
  final Future<void> Function(String key) addPerson;
  HttpServer? _server;

  PageServer({required this.host, required this.www, required this.addPerson});

  Future<int> start(int port) async {
    final s = _server = await HttpServer.bind(InternetAddress.loopbackIPv4, port);
    s.listen(_serve);
    return s.port;
  }

  Future<void> stop() async => _server?.close(force: true);

  static const _types = {
    'html': 'text/html; charset=utf-8',
    'js': 'text/javascript; charset=utf-8',
    'css': 'text/css; charset=utf-8',
    'json': 'application/json; charset=utf-8',
    'svg': 'image/svg+xml',
    'png': 'image/png',
  };

  Future<void> _json(HttpRequest r, Object? body, [int code = 200]) async {
    r.response.statusCode = code;
    r.response.headers.set('Content-Type', 'application/json; charset=utf-8');
    r.response.headers.set('Cache-Control', 'no-store');
    r.response.write(jsonEncode(body));
    await r.response.close();
  }

  Future<void> _serve(HttpRequest r) async {
    try {
      final p = r.uri.path;
      if (r.method == 'GET' && p == '/api/state') {
        // long poll: answer at once, or when the state changes after the version asked
        final since = int.tryParse(r.uri.queryParameters['since'] ?? '');
        if (since != null && since == host.version) {
          final c = Completer<void>();
          final prev = host.onChange;
          host.onChange = () {
            prev?.call();
            if (!c.isCompleted) c.complete();
          };
          await c.future.timeout(const Duration(seconds: 25), onTimeout: () {});
          host.onChange = prev;
        }
        return await _json(r, host.state());
      }
      if (r.method == 'POST' && p == '/api/act') {
        final j = jsonDecode(await utf8.decoder.bind(r).join()) as Map;
        unawaited(host.act(j).catchError((e) => stderr.writeln('act $j: $e')));
        return await _json(r, {"ok": true});
      }
      if (r.method == 'POST' && p == '/api/add') {
        final j = jsonDecode(await utf8.decoder.bind(r).join()) as Map;
        final k = (j['key'] as String).trim().toLowerCase();
        if (!RegExp(r'^[0-9a-f]{64}$').hasMatch(k)) return await _json(r, {"error": "not a key"}, 400);
        await addPerson(k);
        await host.connect(k);
        return await _json(r, {"ok": true});
      }
      final name = p == '/' ? 'index.html' : p.substring(1);
      final f = File('$www/$name');
      if (!RegExp(r'^[a-z0-9_.-]+$').hasMatch(name) || !f.existsSync()) {
        r.response.statusCode = 404;
        return await r.response.close();
      }
      r.response.headers.set('Content-Type', _types[name.split('.').last] ?? 'application/octet-stream');
      await r.response.addStream(f.openRead());
      await r.response.close();
    } catch (e) {
      try {
        await _json(r, {'error': '$e'}, 500);
      } on Object {
        // the client went away
      }
    }
  }
}
