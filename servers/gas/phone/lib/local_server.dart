// The app's own server, on the phone's loopback: it serves the GAS app page
// (a copy of servers/gas/build/www) and answers the page's API for the one
// person on this phone, as servers/gas/bin/server.dart answers it for the
// four of a world: /api/<contract>/{state,act,day}.
import 'dart:async';
import 'dart:convert';
import 'dart:io';

import 'person_host.dart';

class LocalServer {
  final PersonHost host;
  final String contract;

  /// The directory of the site's files (app.html, data.js, specs/).
  final String www;

  /// The people this phone's layer reports, each {key, name}.
  final List<Map<String, String>> Function() peers;

  LocalServer({
    required this.host,
    required this.contract,
    required this.www,
    required this.peers,
  });

  HttpServer? _server;
  int get port => _server!.port;

  /// The page to show: the contract's app, for this person alone.
  String get url => 'http://127.0.0.1:$port/app.html?c=$contract&solo';

  Future<void> start({int port = 0}) async {
    _server = await HttpServer.bind(InternetAddress.loopbackIPv4, port);
    _server!.listen((req) => unawaited(_handle(req)));
  }

  Future<void> stop() async => _server?.close(force: true);

  Map<String, Object?> _state() => {...host.state(), 'peers': peers()};

  Future<void> _handle(HttpRequest req) async {
    try {
      final seg = req.uri.pathSegments;
      if (seg.length >= 3 && seg[0] == 'api' && seg[1] == contract) {
        switch (seg[2]) {
          case 'state':
            final since = int.tryParse(req.uri.queryParameters['since'] ?? '') ?? -1;
            final until = DateTime.now().add(const Duration(seconds: 20));
            final peersNow = jsonEncode(peers());
            while (host.version == since &&
                jsonEncode(peers()) == peersNow &&
                DateTime.now().isBefore(until)) {
              await Future<void>.delayed(const Duration(milliseconds: 150));
            }
            return await _json(req, _state());
          case 'act':
            final body = jsonDecode(await utf8.decodeStream(req)) as Map;
            unawaited(host.act(body['id'] as int,
                [for (final x in body['path'] as List) x as int], body['value']));
            return await _json(req, {"ok": true});
          case 'day':
            unawaited(host.nextDay());
            return await _json(req, {"ok": true});
        }
        req.response.statusCode = 404;
        return await req.response.close();
      }
      final path = req.uri.path == '/' ? '/app.html' : req.uri.path;
      final f = File('$www$path');
      if (path.contains('..') || !await f.exists()) {
        req.response.statusCode = 404;
        return await req.response.close();
      }
      req.response.headers.contentType = path.endsWith('.js')
          ? ContentType('application', 'javascript', charset: 'utf-8')
          : path.endsWith('.json')
              ? ContentType.json
              : path.endsWith('.html')
                  ? ContentType.html
                  : ContentType('text', 'plain', charset: 'utf-8');
      await req.response.addStream(f.openRead());
      await req.response.close();
    } catch (e) {
      try {
        req.response.statusCode = 500;
        req.response.write('$e');
        await req.response.close();
      } catch (_) {}
    }
  }

  Future<void> _json(HttpRequest req, Object? o) async {
    req.response.headers.contentType = ContentType.json;
    req.response.headers.set('Cache-Control', 'no-store');
    req.response.write(jsonEncode(o));
    await req.response.close();
  }
}
