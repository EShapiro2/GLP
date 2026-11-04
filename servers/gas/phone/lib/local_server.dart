// The app's own server, on the device's loopback: it serves the app's page
// (www/phone.html, from servers/gas/web) and answers its API from the core
// (core.dart):
//
//   GET  /api/home              the person, their apps, the open session
//   GET  /api/catalog           GAS's catalog
//   POST /api/get {hash}        get an app
//   POST /api/open {name, person?}   open an app, adding the person of an invite
//   POST /api/close             end the session
//   POST /api/link {text}       a pasted or opened link: its app got
//   GET  /api/people            the person, their invite, the people around
//   POST /api/name {key, name}  a name for a person, on this device
//   POST /api/add {text}        add a person far away, by their invite
//   POST /api/relay {on}        the Internet relay on or off
//   POST /api/renew             start over as a new person: a new key pair
//   GET  /api/qr?t=...          a QR code, as SVG
//   GET  /api/<app>/state?since=V, POST /api/<app>/act, /api/<app>/day
//                               the open app's agent, as servers/gas/bin/server.dart
//                               answers for a person of a world
import 'dart:async';
import 'dart:convert';
import 'dart:io';

import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:qr/qr.dart';

import 'contracts.dart';
import 'core.dart';

class LocalServer {
  final Core core;

  /// The directory of the page's files.
  final String www;

  LocalServer({required this.core, required this.www});

  HttpServer? _server;
  int get port => _server!.port;
  String get url => 'http://127.0.0.1:$port/phone.html';

  Future<void> start({int port = 0}) async {
    _server = await HttpServer.bind(InternetAddress.loopbackIPv4, port, shared: true);
    _server!.listen((req) => unawaited(_handle(req)));
  }

  Future<void> stop() async => _server?.close(force: true);

  List<Contract>? _catalog;

  Map<String, Object?> _home() => {
        'version': core.version,
        'me': core.me.hex,
        'name': core.nameOf(core.me.hex),
        'apps': [for (final c in core.store.installed()) {...c.toJson(), 'name': c.name}],
        'open': core.session?.contract.name,
        'notices': core.notices,
      };

  Map<String, Object?> _people() {
    final s = core.session;
    return {
      'version': core.version,
      'me': core.me.hex,
      'name': core.nameOf(core.me.hex),
      'invite': core.invite(),
      'app': s?.contract.name,
      'title': s?.contract.title,
      'peers': s?.people() ?? const [],
      'relay': {'on': core.relayOn, 'state': s?.relay.state ?? (core.relayOn ? 'on' : 'off')},
      'events': s?.events.reversed.take(60).toList() ?? const [],
    };
  }

  Map<String, Object?> _state(Session s) => {
        ...(s.phase.startsWith('failed') ? {'status': s.phase} : _hostState(s)),
        'version': core.version,
        'name': core.nameOf(core.me.hex),
        'peers': s.people(),
        'relay': {'on': core.relayOn, 'state': s.relay.state},
        'notices': core.notices,
      };

  Map<String, Object?> _hostState(Session s) {
    try {
      return s.host.state();
    } catch (_) {
      return {'status': 'starting', 'me': core.me.hex, 'persons': {}};
    }
  }

  static String qrSvg(String text) {
    final q = QrImage(QrCode.fromData(data: text, errorCorrectLevel: QrErrorCorrectLevel.M));
    final n = q.moduleCount, b = StringBuffer();
    for (var y = 0; y < n; y++) {
      for (var x = 0; x < n; x++) {
        if (q.isDark(y, x)) b.write('M${x + 2} ${y + 2}h1v1h-1z');
      }
    }
    return '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ${n + 4} ${n + 4}" shape-rendering="crispEdges">'
        '<rect width="100%" height="100%" fill="#fff"/><path fill="#1d1d1f" d="$b"/></svg>';
  }

  Future<Map> _body(HttpRequest req) async {
    final t = await utf8.decodeStream(req);
    return t.isEmpty ? {} : jsonDecode(t) as Map;
  }

  Future<void> _handle(HttpRequest req) async {
    try {
      final seg = req.uri.pathSegments;
      if (seg.isNotEmpty && seg[0] == 'api') return await _api(req, seg);
      final path = req.uri.path == '/' ? '/phone.html' : req.uri.path;
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
                  : path.endsWith('.svg')
                      ? ContentType('image', 'svg+xml')
                      : ContentType('text', 'plain', charset: 'utf-8');
      req.response.headers.set('Cache-Control', 'no-store');
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

  Future<void> _api(HttpRequest req, List<String> seg) async {
    final op = seg.length > 1 ? seg[1] : '';
    final since = int.tryParse(req.uri.queryParameters['since'] ?? '');
    if (since != null) await core.wait(since, const Duration(seconds: 20));
    switch (op) {
      case 'home':
        return _json(req, _home());
      case 'catalog':
        try {
          _catalog = await core.store.catalog();
        } on Object catch (e) {
          if (_catalog == null) return _json(req, {'error': '$e'});
        }
        return _json(req, [for (final c in _catalog!) {...c.toJson(), 'name': c.name}]);
      case 'get':
        try {
          final c = await core.store.get((await _body(req))['hash'] as String);
          core.changed();
          return await _json(req, {...c.toJson(), 'name': c.name});
        } on Object catch (e) {
          return _json(req, {'error': '$e'});
        }
      case 'open':
        final b = await _body(req);
        final c = core.store.installed().where((x) => x.name == b['name']).firstOrNull;
        if (c == null) return _json(req, {'error': 'no such app here'});
        await core.open(c, invite: b['person'] as String?);
        return _json(req, {'ok': true, 'name': c.name, 'title': c.title});
      case 'close':
        await core.close();
        return _json(req, {'ok': true});
      case 'link':
        return _json(req, await core.link((await _body(req))['text'] as String? ?? ''));
      case 'people':
        return _json(req, _people());
      case 'name':
        final b = await _body(req);
        await core.setName(b['key'] as String, b['name'] as String? ?? '');
        return _json(req, {'ok': true});
      case 'add':
        final s = core.session;
        final l = GasLink.parse((await _body(req))['text'] as String? ?? '');
        if (s == null) return _json(req, {'error': 'Open an app first.'});
        if (l?.person == null) return _json(req, {'error': 'That is not a GAS invite.'});
        if (l!.person == core.me.hex) return _json(req, {'error': 'That is your own invite.'});
        if (l.contract != null && l.contract != s.contract.hash) {
          return _json(req, {'error': 'That invite is for another app.', 'link': true});
        }
        await s.addFar(glp.PubKey.fromHex(l.person!));
        return _json(req, {'ok': true});
      case 'relay':
        await core.setRelay((await _body(req))['on'] == true);
        return _json(req, {'ok': true});
      case 'renew':
        await core.renew();
        return _json(req, {'ok': true, 'me': core.me.hex});
      case 'qr':
        req.response.headers.contentType = ContentType('image', 'svg+xml');
        req.response.headers.set('Cache-Control', 'no-store');
        req.response.write(qrSvg(req.uri.queryParameters['t'] ?? ''));
        return req.response.close();
    }
    // The open app's agent.
    final s = core.session;
    if (s == null || seg.length < 3 || seg[1] != s.contract.name) {
      req.response.statusCode = 404;
      return req.response.close();
    }
    switch (seg[2]) {
      case 'state':
        return _json(req, _state(s));
      case 'act':
        final b = await _body(req);
        unawaited(s.host.act(b['id'] as int, [for (final x in b['path'] as List) x as int], b['value']));
        return _json(req, {'ok': true});
      case 'day':
        unawaited(s.host.nextDay());
        return _json(req, {'ok': true});
    }
    req.response.statusCode = 404;
    await req.response.close();
  }

  Future<void> _json(HttpRequest req, Object? o) async {
    req.response.headers.contentType = ContentType.json;
    req.response.headers.set('Cache-Control', 'no-store');
    req.response.write(jsonEncode(o));
    await req.response.close();
  }
}
