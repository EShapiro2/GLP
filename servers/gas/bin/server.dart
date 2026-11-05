// GAS, the GLP app store: the live server.  It serves the site (build/www) and,
// for each contract, the compiled canonical platform in a live harness
// (programs/vglp/cva/gas_worlds/live_<c>, written by tools/build.py, in the
// scope of the canonical platforms, programs/vglp/cva), four persons, alice, bob,
// carol and dave, in one GLP heap.  The server is every person: it keeps what
// each person's dispatcher draws and injects the person's grants,
// input(Id, Path, R), from the web page.
//
// Two ways to run a world, chosen by GAS_WORLDS:
//  - mad (the default): every person is an agent of madGLP in an isolate of its
//    own, with its own heap and key pair, the harness's agent/2 told first
//    me(Person), and so running live_person/3;
//    the persons' messages are madGLP's, cold calls by send_to_net, which this
//    server carries between the isolates as the simulation's network does.
//  - heap: the four persons in one heap, one isolate, the harness's agent/2
//    told first world, and so running live_ui/1,
//    their networks joined in GLP by route4.
// Either way loading a platform (seconds of CPU, minutes on a small host) never
// blocks the web server.  A world starts on its contract's first request; at
// most MAX_WORLDS run at once (default 3), the least recently used dropped
// first, and a world unused for an hour is dropped.
//
// The app maker (make.html): POST /api/make {text} runs tools/make.py on the
// text, one at a time, and a package it makes is kept in MADE_DIR, served at
// /made/pkg-<hash>.json (the phone app gets it there), and run here as the
// contract <id>_<first 8 hex of its hash>, as the phone names it.  GET
// /api/makebase/<ID> gives a contract's text, from its cascade record, to start
// from.  PYTHON, GLPC and BASH name the tools make.py runs.
import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:isolate';
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' show PubKey;
import 'package:glp_runtime/multiagent/simulation_network.dart'
    show NetworkDirectory, generateKeyPair;
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
    // The host's first act says which agent this is: the four persons together.
    await agent.injectUserInput(rt.ConstTerm('world'));
    if (calendar) await setDay(1);
    out.send({'k': 'status', 's': 'ready', 'calendar': calendar});
  } catch (e) {
    out.send({'k': 'status', 's': 'failed: $e'});
  }
}

// ---------------------------------------------------------------------------
// The person worker: one person of a world, an agent of madGLP in its own
// isolate.  Its events --- its start, the person's acts, a day of the calendar,
// a message from another person --- are taken one at a time, in order of
// arrival, and each is reported done once its cycle is complete; the messages
// the cycle sent are reported before it, the port keeping their order.
// ---------------------------------------------------------------------------

typedef Keys = Map<String, ({PubKey pub, Uint8List priv})>;

void personWorker(List<Object?> args) async {
  final root = args[0] as String, worldsDir = args[1] as String, contract = args[2] as String;
  final person = args[3] as String, keys = args[4] as Keys, out = args[5] as SendPort;
  final inbox = ReceivePort();
  out.send({'k': 'port', 'who': person, 'port': inbox.sendPort});
  final directory = NetworkDirectory();
  keys.forEach((p, kp) => directory.register(p, kp.pub));
  final agent = AgentRuntime(
    agentId: person,
    program: '$worldsDir/live_$contract',
    rootSelfGlpPath: '$root/programs/self.glp',
    keyPair: keys[person],
    directory: directory,
  );
  agent.maxQuiescenceCycles = 20000000;
  agent.onUserOutput = (t) => out.send({'k': 'out', 'j': toJson(t)});
  agent.onSendMadMessage = (to, payload) async =>
      out.send({'k': 'mad', 'from': person, 'to': to, 'payload': payload});
  var queue = Future<void>.value();
  void run(Future<void> Function() f) {
    queue = queue.then((_) => f()).catchError((Object e) {
      out.send({'k': 'error', 'e': '$person: $e'});
    }).whenComplete(() => out.send({'k': 'done', 'who': person}));
  }

  run(() async {
    try {
      await agent.initialize();
      // The host's first act says which agent this is: this person alone; its
      // second names the compiler the harness trusts and gives the creator's
      // warranty, against which the harness checks the compiled contract unit
      // it loads (trust.json, written beside it by tools/build.py or make.py).
      await agent.injectUserInput(rt.StructTerm('me', [rt.ConstTerm(person)]));
      final t = jsonDecode(File('$worldsDir/live_$contract/trust.json').readAsStringSync()) as Map;
      await agent.injectUserInput(rt.StructTerm('warranty', [rt.ConstTerm(t['compiler'] as String), rt.ConstTerm(t['creator'] as String)]));
      out.send({'k': 'started', 'who': person});
    } catch (e) {
      out.send({'k': 'status', 's': 'failed: $person: $e'});
    }
  });
  inbox.listen((msg) {
    final m = msg as Map;
    switch (m['k']) {
      case 'act':
        run(() => agent.injectUserInput(rt.StructTerm('g', [
              rt.StructTerm('input', [
                rt.ConstTerm(m['id'] as int),
                listOf([for (final x in m['path'] as List) rt.ConstTerm(x as int)]),
                fromJson(m['value']),
              ]),
            ])));
      case 'cal':
        run(() => agent.injectUserInput(rt.StructTerm('cal', [rt.ConstTerm(m['day'] as int)])));
      case 'mad':
        run(() => agent.onMadMessageReceived(m['from'] as String, m['payload'] as Uint8List));
    }
  });
}

// ---------------------------------------------------------------------------
// The server's view of a world: what the worker reported.
// ---------------------------------------------------------------------------

final madWorlds = (Platform.environment['GAS_WORLDS'] ?? 'mad') != 'heap';

class World {
  final String contract;
  World(this.contract);
  final isolates = <Isolate>[];
  // heap: the one worker's port under 'host'; mad: each person's port.
  final ports = <String, SendPort>{};
  final pending = <String, List<Map<String, Object?>>>{};
  final Map<String, Map<int, Object?>> draws = {for (final p in persons) p: {}};
  final List<Object?> net = [];
  int version = 0;
  int? day;
  String status = 'starting';
  final started = DateTime.now();
  DateTime used = DateTime.now();
  // mad: the events sent to the persons and not yet reported done, the persons
  // started, and whether the platform reads a calendar.
  int inflight = 0;
  final up = <String>{};
  bool calendar = false;

  Future<void> start(String root) async {
    final rp = ReceivePort();
    rp.listen(_onMsg);
    if (!madWorlds) {
      isolates.add(await Isolate.spawn(worker, [root, worldsDir, contract, rp.sendPort]));
      return;
    }
    calendar = File('$worldsDir/live_$contract/self.glp').readAsStringSync().contains('cal(alice, N)');
    final Keys keys = {for (final p in persons) p: generateKeyPair()};
    for (final p in persons) {
      inflight++; // its start
      isolates.add(await Isolate.spawn(personWorker, [root, worldsDir, contract, p, keys, rp.sendPort]));
    }
  }

  void stop() {
    for (final i in isolates) {
      i.kill(priority: Isolate.immediate);
    }
  }

  void _to(String who, Map<String, Object?> m) {
    final p = ports[who];
    if (p == null) {
      (pending[who] ??= []).add(m);
    } else {
      p.send(m);
    }
  }

  // An event for a person (mad), counted until it is reported done.
  void _event(String who, Map<String, Object?> m) {
    inflight++;
    if (status == 'ready') status = 'working';
    _to(who, m);
  }

  void act(Map body) {
    used = DateTime.now();
    if (!madWorlds) {
      _to('host', {'k': 'act', 'person': body['person'], 'id': body['id'], 'path': body['path'], 'value': body['value']});
    } else if (persons.contains(body['person'])) {
      _event(body['person'] as String, {'k': 'act', 'id': body['id'], 'path': body['path'], 'value': body['value']});
    }
  }

  void nextDay() {
    used = DateTime.now();
    if (!madWorlds) {
      _to('host', {'k': 'day'});
    } else if (calendar) {
      _setDay((day ?? 0) + 1);
    }
  }

  void _setDay(int d) {
    day = d;
    for (final p in persons) {
      _event(p, {'k': 'cal', 'day': d});
    }
  }

  void _onMsg(dynamic msg) {
    final m = msg as Map;
    switch (m['k']) {
      case 'port':
        final who = (m['who'] as String?) ?? 'host';
        ports[who] = m['port'] as SendPort;
        for (final x in pending.remove(who) ?? const []) {
          ports[who]!.send(x);
        }
        return;
      case 'status':
        status = m['s'] as String;
      case 'day':
        day = m['day'] as int;
      case 'error':
        stderr.writeln('[$contract] ${m['e']}');
        return;
      case 'started':
        up.add(m['who'] as String);
        return;
      case 'mad':
        // A message between two persons, carried as the network carries it.
        final to = m['to'] as String;
        if (persons.contains(to)) {
          _event(to, {'k': 'mad', 'from': m['from'], 'payload': m['payload']});
        }
        return;
      case 'done':
        inflight--;
        if (status.startsWith('failed')) return;
        if (inflight == 0) {
          if (status == 'starting' && up.length == persons.length && calendar && day == null) {
            _setDay(1);
          } else {
            status = 'ready';
          }
        }
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
late String root, worldsDir, madeDir, webRoot;

/// A made package's world, installed beside the others, and its menu where
/// the page reads it; the contract's name.
String installMade(File pkg) {
  final j = jsonDecode(pkg.readAsStringSync()) as Map;
  final hash = RegExp(r'pkg-([0-9a-f]{64})\.json$').firstMatch(pkg.path)!.group(1)!;
  final name = '${(j['id'] as String).toLowerCase()}_${hash.substring(0, 8)}';
  final dir = Directory('$worldsDir/live_$name')..createSync(recursive: true);
  final files = j['files'] as Map;
  // The harness and its contract unit (tools/warrant.py's FILES); the server
  // made the package, and its warranties with it.
  for (final f in ['self.glp', 'contract/self.glp', 'contract/platform.vglp']) {
    File('${dir.path}/$f')
      ..parent.createSync(recursive: true)
      ..writeAsStringSync(files[f] as String);
  }
  Directory('$webRoot/specs').createSync(recursive: true);
  File('$webRoot/specs/$name.json').writeAsStringSync(jsonEncode({...(j['spec'] as Map), 'meta': j['meta']}));
  contracts.add(name);
  return name;
}

Future<void> _making = Future.value();

/// The Python make.py runs under: PYTHON, or the first line of the file
/// PYTHON_PATH_FILE names (talmonlab's C:\\sites\\python.txt), or python3.
String python() {
  final p = Platform.environment['PYTHON'];
  if (p != null) return p;
  final f = Platform.environment['PYTHON_PATH_FILE'];
  if (f != null && File(f).existsSync()) return File(f).readAsLinesSync().first.trim();
  return 'python3';
}

/// tools/make.py on [text], after any make before it.
Future<Map> make(String text) {
  final done = Completer<Map>();
  _making = _making.then((_) async {
    final work = Directory.systemTemp.createTempSync('gas_make_');
    try {
      final f = File('${work.path}/contract.txt')..writeAsStringSync(text);
      final r = await Process.run(python(),
          ['-I', '${Directory.current.path}/tools/make.py', f.path, '${work.path}/out'],
          environment: {'PYTHONIOENCODING': 'utf-8'}).timeout(const Duration(minutes: 10));
      final res = File('${work.path}/out/result.json');
      if (!res.existsSync()) {
        done.complete({'ok': false, 'step': 'tools', 'errors': ['${r.stderr}'.split('\n').where((l) => l.trim().isNotEmpty).take(8).toList()]});
        return;
      }
      final out = jsonDecode(res.readAsStringSync()) as Map;
      if (out['ok'] == true) {
        final pkg = File('${work.path}/out/pkg-${out['hash']}.json');
        Directory(madeDir).createSync(recursive: true);
        final kept = pkg.copySync('$madeDir/pkg-${out['hash']}.json');
        out['name'] = installMade(kept);
        // The unit as the app maker compiled it, and what its harness checks
        // it against, beside the made world's harness.
        for (final f in ['contract.glpw', 'trust.json']) {
          final src = File('${work.path}/out/$f');
          if (src.existsSync()) src.copySync('$worldsDir/live_${out['name']}/$f');
        }
      }
      done.complete(out);
    } catch (e) {
      done.complete({'ok': false, 'step': 'tools', 'errors': ['$e']});
    } finally {
      work.deleteSync(recursive: true);
    }
  });
  return done.future;
}
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
  final host = Platform.environment['HOST'] ?? '0.0.0.0';
  final webDir = Platform.environment['WEB_DIR'] ?? '$here/build/www';
  webRoot = webDir;
  madeDir = Platform.environment['MADE_DIR'] ?? '$here/made';
  contracts = Directory(worldsDir)
      .listSync()
      .whereType<Directory>()
      .map((d) => d.path.split(RegExp(r'[\\/]')).last)  // '/' or '\\' (Windows)
      .where((n) => n.startsWith('live_'))
      .map((n) => n.substring(5))
      .toSet();
  if (Directory(madeDir).existsSync()) {
    for (final f in Directory(madeDir).listSync().whereType<File>()) {
      if (RegExp(r'pkg-[0-9a-f]{64}\.json$').hasMatch(f.path)) {
        try {
          installMade(f);
        } catch (e) {
          stderr.writeln('made ${f.path}: $e');
        }
      }
    }
  }
  Timer.periodic(const Duration(minutes: 5), (_) {
    final cut = DateTime.now().subtract(const Duration(hours: 1));
    worlds.removeWhere((c, w) {
      final drop = w.used.isBefore(cut);
      if (drop) w.stop();
      return drop;
    });
  });
  final server = await HttpServer.bind(host, port);
  stdout.writeln('live server on $port: ${contracts.join(', ')}');
  await for (final req in server) {
    unawaited(handle(req, webDir));
  }
}

Future<void> handle(HttpRequest req, String webDir) async {
  try {
    req.response.headers.set('Access-Control-Allow-Origin', '*');
    final seg = req.uri.pathSegments;
    if (seg.length == 2 && seg[0] == 'api' && seg[1] == 'make' && req.method == 'POST') {
      final body = jsonDecode(await utf8.decodeStream(req)) as Map;
      final text = body['text'];
      if (text is! String || text.length > 300000) {
        req.response.statusCode = 400;
        _json(req, {'ok': false, 'step': 'text', 'errors': ['a text of at most 300 KB']});
        return;
      }
      _json(req, await make(text));
      return;
    }
    if (seg.length == 3 && seg[0] == 'api' && seg[1] == 'makebase') {
      final n = seg[2];
      final t = File('$root/cascade/$n/$n.txt'), sc = File('$root/cascade/$n/schemas.txt');
      if (!RegExp(r'^[A-Z][A-Z0-9]{0,15}$').hasMatch(n) || !t.existsSync() || !sc.existsSync()) {
        req.response.statusCode = 404;
        await req.response.close();
        return;
      }
      req.response.headers.contentType = ContentType('text', 'plain', charset: 'utf-8');
      // Its % lines left out but the one naming what it extends, which the
      // pragmas reader follows.
      final txt = t.readAsLinesSync().where((l) => !l.startsWith('%') || l.startsWith('% Extends:')).join('\n').trim();
      req.response.write('$txt\n\nSchemas\n\n${sc.readAsStringSync()}');
      await req.response.close();
      return;
    }
    if (seg.length == 2 && seg[0] == 'made' && RegExp(r'^pkg-[0-9a-f]{64}\.json$').hasMatch(seg[1])) {
      final f = File('$madeDir/${seg[1]}');
      if (!f.existsSync()) {
        req.response.statusCode = 404;
        await req.response.close();
        return;
      }
      req.response.headers.contentType = ContentType.json;
      await req.response.addStream(f.openRead());
      await req.response.close();
      return;
    }
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
        w.act(body);
        _json(req, {'ok': true});
      } else if (op == 'day' && req.method == 'POST') {
        w.nextDay();
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
