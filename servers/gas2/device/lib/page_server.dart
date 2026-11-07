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

  static Future<void> _json(HttpRequest r, Object? body, [int code = 200]) async {
    r.response.statusCode = code;
    r.response.headers.set('Content-Type', 'application/json; charset=utf-8');
    r.response.headers.set('Cache-Control', 'no-store');
    r.response.write(jsonEncode(body));
    await r.response.close();
  }

  Future<void> _serve(HttpRequest r) async {
    try {
      if (await api(r, r.uri.path)) return;
      await file(r, www, r.uri.path);
    } catch (e) {
      try {
        await _json(r, {'error': '$e'}, 500);
      } on Object {
        // the client went away
      }
    }
  }

  /// The person's API, at [p]: true where [r] was one of its requests, answered.
  Future<bool> api(HttpRequest r, String p) async {
    if (r.method == 'GET' && p == '/api/state') {
      // long poll: answer at once, or when the state changes after the version asked
      final since = int.tryParse(r.uri.queryParameters['since'] ?? '');
      if (since != null && since == host.version) {
        await host.nextChange().timeout(const Duration(seconds: 25), onTimeout: () {});
      }
      await _json(r, {...host.state(), ...extra});
      return true;
    }
    if (r.method == 'POST' && p == '/api/act') {
      final j = jsonDecode(await utf8.decoder.bind(r).join()) as Map;
      unawaited(host.act(j).catchError((e) => stderr.writeln('act $j: $e')));
      await _json(r, {'ok': true});
      return true;
    }
    if (r.method == 'POST' && p == '/api/add') {
      final j = jsonDecode(await utf8.decoder.bind(r).join()) as Map;
      final k = (j['key'] as String).trim().toLowerCase();
      if (!RegExp(r'^[0-9a-f]{64}$').hasMatch(k)) {
        await _json(r, {'error': 'not a key'}, 400);
        return true;
      }
      await addPerson(k);
      await host.connect(k);
      await _json(r, {'ok': true});
      return true;
    }
    return false;
  }

  /// What the page's state carries besides the host's: on the web, that the key is kept here.
  Map<String, Object?> extra = const {};

  /// One of the page's files.
  static Future<void> file(HttpRequest r, String www, String p) async {
    final name = p == '/' || p.isEmpty ? 'index.html' : p.substring(1);
    final f = File('$www/$name');
    if (!RegExp(r'^[a-z0-9_.-]+$').hasMatch(name) || !f.existsSync()) {
      r.response.statusCode = 404;
      await r.response.close();
      return;
    }
    r.response.headers.set('Content-Type', _types[name.split('.').last] ?? 'application/octet-stream');
    r.response.headers.set('Cache-Control', 'no-cache');
    await r.response.addStream(f.openRead());
    await r.response.close();
  }
}
