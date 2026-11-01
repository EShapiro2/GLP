// GAS, the GLP app store: the live server.  It serves the site (build/www) and,
// for each contract, the compiled canonical platform in a live harness
// (programs/vglp/cva/gas_worlds/live_<c>, written by tools/build.py, in the
// scope of the canonical platforms, programs/vglp/cva), four persons, alice, bob,
// carol and dave, in one GLP heap.  The server is every person: it keeps what
// each person's dispatcher draws and injects the person's grants,
// input(Id, Path, R), from the web page.
//
// Each contract's world runs in an isolate of its own, so that loading a
// platform (seconds of CPU, minutes on a small host) never blocks the web
// server.  A world starts on its contract's first request; at most MAX_WORLDS
// run at once (default 3), the least recently used dropped first, and a world
// unused for an hour is dropped.
import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:isolate';

import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;

final persons = ['alice', 'bob', 'carol', 'dave'];

// ---------------------------------------------------------------------------
// The worker: one world, in its own isolate.
// ---------------------------------------------------------------------------

void worker(List<Object?> args) async {
  final root = args[0] as String, worldsDir = args[1] as String, contract = args[2] as String;
  final out = args[3] as SendPort;
  final inbox = ReceivePort();
  out.send({'k': 'port', 'port': inbox.sendPort});
  final agent = AgentRuntime(
    agentId: 'host',
    program: '$worldsDir/live_$contract',
    rootSelfGlpPath: '$root/programs/self.glp',
    goalLabel: 'live_ui/3',
  );
  agent.maxQuiescenceCycles = 20000000;
  // send_to_user's elements arrive ground, dereferenced by the _output kernel.
  agent.onUserOutput = (t) => out.send({'k': 'out', 'j': toJson(t)});
  final calendar = File('$worldsDir/live_$contract/self.glp')
      .readAsStringSync()
      .contains('cal(alice, N)');
  var queue = Future<void>.value();
  Future<void> run(Future<void> Function() f) {
    final done = queue.then((_) => f());
    queue = done.catchError((e) => out.send({'k': 'error', 'e': '$e'}));
    return queue;
  }

  var day = 0;
  Future<void> setDay(int d) async {
    day = d;
    out.send({'k': 'day', 'day': d});
    for (final p in persons) {
      await agent.injectUserInput(rt.StructTerm('cal', [rt.ConstTerm(p), rt.ConstTerm(d)]));
    }
  }

  inbox.listen((msg) {
    final m = msg as Map;
    if (m['k'] == 'act') {
      out.send({'k': 'status', 's': 'working'});
      run(() async {
        await agent.injectUserInput(rt.StructTerm('in', [
          rt.ConstTerm(m['person'] as String),
          rt.StructTerm('input', [
            rt.ConstTerm(m['id'] as int),
            listOf([for (final x in m['path'] as List) rt.ConstTerm(x as int)]),
            fromJson(m['value']),
          ]),
        ]));
      }).whenComplete(() => out.send({'k': 'status', 's': 'ready'}));
    } else if (m['k'] == 'day' && calendar) {
      out.send({'k': 'status', 's': 'working'});
      run(() => setDay(day + 1)).whenComplete(() => out.send({'k': 'status', 's': 'ready'}));
    }
  });

  try {
    await agent.initialize();
    if (calendar) await setDay(1);
    out.send({'k': 'status', 's': 'ready', 'calendar': calendar});
  } catch (e) {
    out.send({'k': 'status', 's': 'failed: $e'});
  }
}

// ---------------------------------------------------------------------------
// The server's view of a world: what the worker reported.
// ---------------------------------------------------------------------------

class World {
  final String contract;
  World(this.contract);
  Isolate? isolate;
  SendPort? port;
  final pending = <Map<String, Object?>>[];
  final Map<String, Map<int, Object?>> draws = {for (final p in persons) p: {}};
  final List<Object?> net = [];
  int version = 0;
  int? day;
  String status = 'starting';
  final started = DateTime.now();
  DateTime used = DateTime.now();

  Future<void> start(String root) async {
    final rp = ReceivePort();
    rp.listen(_onMsg);
    isolate = await Isolate.spawn(worker, [root, worldsDir, contract, rp.sendPort]);
  }

  void stop() => isolate?.kill(priority: Isolate.immediate);

  void send(Map<String, Object?> m) {
    used = DateTime.now();
    final p = port;
    if (p == null) {
      pending.add(m);
    } else {
      p.send(m);
    }
  }

  void _onMsg(dynamic msg) {
    final m = msg as Map;
    switch (m['k']) {
      case 'port':
        port = m['port'] as SendPort;
        for (final x in pending) port!.send(x);
        pending.clear();
        return;
      case 'status':
        status = m['s'] as String;
      case 'day':
        day = m['day'] as int;
      case 'error':
        stderr.writeln('[$contract] ${m['e']}');
        return;
      case 'out':
        final j = m['j'];
        if (j is Map && j['f'] == 'd') {
          final a = j['a'] as List;
          final who = a[0] as String, draw = a[1];
          if (draw is Map && draw['f'] == 'draw') {
            final d = draw['a'] as List;
            draws[who]![d[0] as int] = d[2];
          } else if (draw is Map && draw['f'] == 'withdraw') {
            draws[who]!.remove((draw['a'] as List)[0] as int);
          }
        } else if (j is Map && j['f'] == 'net') {
          net.add((j['a'] as List)[0]);
          if (net.length > 300) net.removeAt(0);
        }
    }
    version++;
  }

  Map<String, Object?> state() => {
        'version': version,
        'status': status,
        'since': DateTime.now().difference(started).inSeconds,
        if (day != null) 'day': day,
        'persons': {for (final p in persons) p: draws[p]!.map((k, v) => MapEntry('$k', v))},
        'net': net.length > 60 ? net.sublist(net.length - 60) : net,
      };
}

rt.Term listOf(List<rt.Term> items) =>
    items.reversed.fold<rt.Term>(rt.ConstTerm(rt.nil), (tail, x) => rt.StructTerm('.', [x, tail]));

Object? toJson(rt.Term t) {
  if (t is rt.ConstTerm) {
    if (t.value == rt.nil || t.value == null) return <Object?>[];
    final v = t.value;
    if (v is num || v is String || v is bool) return v;
    return v.toString();
  }
  if (t is rt.StructTerm) {
    if (t.functor == '.' && t.args.length == 2) {
      final out = <Object?>[];
      rt.Term cur = t;
      while (cur is rt.StructTerm && cur.functor == '.' && cur.args.length == 2) {
        out.add(toJson(cur.args[0]));
        cur = cur.args[1];
      }
      if (!(cur is rt.ConstTerm && (cur.value == rt.nil || cur.value == null))) out.add({'tail': toJson(cur)});
      return out;
    }
    return {'f': t.functor, 'a': [for (final a in t.args) toJson(a)]};
  }
  return {'v': 1};
}

rt.Term fromJson(Object? j) {
  if (j is List) return listOf([for (final x in j) fromJson(x)]);
  if (j is Map) return rt.StructTerm(j['f'] as String, [for (final a in (j['a'] as List)) fromJson(a)]);
  return rt.ConstTerm(j);
}

// ---------------------------------------------------------------------------
// The web server.
// ---------------------------------------------------------------------------

final worlds = <String, World>{};
late String root, worldsDir;
late Set<String> contracts;
final maxWorlds = int.tryParse(Platform.environment['MAX_WORLDS'] ?? '') ?? 3;

World world(String c) {
  var w = worlds[c];
  if (w == null) {
    while (worlds.length >= maxWorlds) {
      final old = worlds.entries.reduce((a, b) => a.value.used.isBefore(b.value.used) ? a : b);
      old.value.stop();
      worlds.remove(old.key);
    }
    w = World(c);
    worlds[c] = w;
    unawaited(w.start(root));
  }
  w.used = DateTime.now();
  return w;
}

Future<void> main(List<String> args) async {
  // Run from servers/gas: the repository is two levels up, and build/www is here.
  final here = Directory.current.path;
  root = Platform.environment['GLP_ROOT'] ?? Directory(here).parent.parent.path;
  worldsDir = Platform.environment['WORLDS_DIR'] ?? '$root/programs/vglp/cva/gas_worlds';
  final port = int.parse(Platform.environment['PORT'] ?? '8080');
  final webDir = Platform.environment['WEB_DIR'] ?? '$here/build/www';
  contracts = Directory(worldsDir)
      .listSync()
      .whereType<Directory>()
      .map((d) => d.path.split(RegExp(r'[\\/]')).last)  // '/' or '\\' (Windows)
      .where((n) => n.startsWith('live_'))
      .map((n) => n.substring(5))
      .toSet();
  Timer.periodic(const Duration(minutes: 5), (_) {
    final cut = DateTime.now().subtract(const Duration(hours: 1));
    worlds.removeWhere((c, w) {
      final drop = w.used.isBefore(cut);
      if (drop) w.stop();
      return drop;
    });
  });
  final server = await HttpServer.bind(InternetAddress.anyIPv4, port);
  stdout.writeln('live server on $port: ${contracts.join(', ')}');
  await for (final req in server) {
    unawaited(handle(req, webDir));
  }
}

Future<void> handle(HttpRequest req, String webDir) async {
  try {
    req.response.headers.set('Access-Control-Allow-Origin', '*');
    final seg = req.uri.pathSegments;
    if (seg.length >= 3 && seg[0] == 'api') {
      final c = seg[1];
      if (!contracts.contains(c)) {
        req.response.statusCode = 404;
        await req.response.close();
        return;
      }
      final op = seg[2];
      final w = world(c);
      if (op == 'state') {
        final since = int.tryParse(req.uri.queryParameters['since'] ?? '') ?? -1;
        final until = DateTime.now().add(const Duration(seconds: 20));
        while (w.version == since && DateTime.now().isBefore(until)) {
          await Future.delayed(const Duration(milliseconds: 150));
        }
        _json(req, w.state());
      } else if (op == 'act' && req.method == 'POST') {
        final body = jsonDecode(await utf8.decodeStream(req)) as Map;
        w.send({'k': 'act', 'person': body['person'], 'id': body['id'], 'path': body['path'], 'value': body['value']});
        _json(req, {'ok': true});
      } else if (op == 'day' && req.method == 'POST') {
        w.send({'k': 'day'});
        _json(req, {'ok': true});
      } else if (op == 'reset' && req.method == 'POST') {
        w.stop();
        worlds.remove(c);
        world(c);
        _json(req, {'ok': true});
      } else {
        req.response.statusCode = 404;
        await req.response.close();
      }
      return;
    }
    final path = req.uri.path == '/' ? '/index.html' : req.uri.path;
    final f = File('$webDir$path');
    if (!path.contains('..') && await f.exists()) {
      req.response.headers.contentType = path.endsWith('.js')
          ? ContentType('application', 'javascript', charset: 'utf-8')
          : path.endsWith('.json')
              ? ContentType.json
              : path.endsWith('.html')
                  ? ContentType.html
                  : ContentType('text', 'plain', charset: 'utf-8');
      await req.response.addStream(f.openRead());
      await req.response.close();
    } else {
      req.response.statusCode = 404;
      await req.response.close();
    }
  } catch (e, st) {
    stderr.writeln('$e\n$st');
    try {
      req.response.statusCode = 500;
      req.response.write('$e');
      await req.response.close();
    } catch (_) {}
  }
}

void _json(HttpRequest req, Object? o) {
  req.response.headers.contentType = ContentType.json;
  req.response.headers.set('Cache-Control', 'no-store');
  req.response.write(jsonEncode(o));
  req.response.close();
}
