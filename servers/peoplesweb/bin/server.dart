// GAS, the Grassroots App Store, at peoplesweb.org: the live server.  It serves the
// site (build/www; on the server Caddy serves the same tree and proxies /api
// here, deploy/Caddyfile) and, for each contract, the compiled canonical
// platform in a live harness (programs/vglp/cva/web_worlds/live_<c>, written
// by tools/build.py, in the scope of the canonical platforms,
// programs/vglp/cva).  The server is every person: it keeps what
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
// Either way the world's program is compiled once, by the server under a key
// of its own (HOST_SEED, a file of its 32-byte seed, made where absent), in an
// isolate of its own, and each person is given the compiled program and
// compiles nothing (IGLP, Implementation Notes, "Boot": "A host may give an
// agent, at its start, the compiled program it runs in place of the
// program's source").  The compiled program is kept in COMPILED_DIR under a
// key of what it was compiled from --- the world's files, and the path, size
// and time of change of every file of programs/ and glp_runtime/lib --- so
// that a restart of the server compiles nothing that has not changed.
// Until 2026-11-17 each of the four persons compiled the program from its
// source at every start: 54 seconds of CPU for Knowledge, 181 for Community
// Bank, against 17 and 46 in one heap (Website #3, measured on four cores).
// Loading a platform never blocks the web server.  A world starts on its contract's first request; at
// most MAX_WORLDS run at once (default 3), the least recently used dropped
// first, and a world unused for an hour is dropped.
import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:isolate';
import 'dart:math' show Random;
import 'dart:typed_data';

import 'package:crypto/crypto.dart' show sha256;
import 'package:peoplesweb_server/feedback.dart' as feedback;
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/identity.dart' show PersonIdentity;
import 'package:glp_runtime/multiagent/stored_program.dart';
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
  final out = args[3] as SendPort, compiledPath = args[4] as String;
  final inbox = ReceivePort();
  out.send({'k': 'port', 'port': inbox.sendPort});
  final agent = AgentRuntime(
    agentId: 'host',
    program: '$worldsDir/live_$contract',
    rootSelfGlpPath: '$root/programs/self.glp',
    compiled: StoredProgram.fromBytes(File(compiledPath).readAsBytesSync()),
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
  final compiledPath = args[6] as String;
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
    compiled: StoredProgram.fromBytes(File(compiledPath).readAsBytesSync()),
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
      // The host's first act says which agent this is: this person alone.
      await agent.injectUserInput(rt.StructTerm('me', [rt.ConstTerm(person)]));
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

final madWorlds = (Platform.environment['WORLDS'] ?? Platform.environment['GAS_WORLDS'] ?? 'mad') != 'heap';

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
    final String compiledPath;
    try {
      compiledPath = await compiledProgram(contract);
    } catch (e) {
      status = 'failed: $e';
      version++;
      return;
    }
    final rp = ReceivePort();
    rp.listen(_onMsg);
    if (!madWorlds) {
      isolates.add(await Isolate.spawn(worker, [root, worldsDir, contract, rp.sendPort, compiledPath]));
      return;
    }
    calendar = File('$worldsDir/live_$contract/self.glp').readAsStringSync().contains('cal(alice, N)');
    final Keys keys = {for (final p in persons) p: generateKeyPair()};
    for (final p in persons) {
      inflight++; // its start
      isolates.add(await Isolate.spawn(personWorker, [root, worldsDir, contract, p, keys, rp.sendPort, compiledPath]));
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
late String root, worldsDir, webRoot;

late Set<String> contracts;
final maxWorlds = int.tryParse(Platform.environment['MAX_WORLDS'] ?? '') ?? 3;

// ---------------------------------------------------------------------------
// The compiled programs.
// ---------------------------------------------------------------------------

late String compiledDir, hostSeed;
final compiling = <String, Future<String>>{};

/// The file of the compiled program of contract [c]'s world, compiled now
/// where none is kept for what it is compiled from; one compilation at a time
/// for a contract.
Future<String> compiledProgram(String c) {
  // The isolate's globals are its own: what it needs is passed as values.
  final r = root, w = worldsDir, d = compiledDir, s = hostSeed;
  // The block's braces matter: remove returns the future, which whenComplete
  // would otherwise wait on, and so on itself.
  return compiling.putIfAbsent(c, () => Isolate.run(() => compileWorld(r, w, c, d, s))
      .whenComplete(() { compiling.remove(c); }));
}

/// In an isolate: the key of what world [c] is compiled from, and its
/// compiled program in [dir] under that key, compiled and written where it is
/// not there; the files kept for [c] under another key are deleted.
String compileWorld(String root, String worldsDir, String c, String dir, String seedPath) {
  final world = '$worldsDir/live_$c';
  final listing = StringBuffer();
  for (final f in Directory(world).listSync().whereType<File>().toList()..sort((a, b) => a.path.compareTo(b.path))) {
    listing.write('${f.path}\n${sha256.convert(f.readAsBytesSync())}\n');
  }
  for (final d in ['$root/programs', '$root/glp_runtime/lib']) {
    final fs = Directory(d)
        .listSync(recursive: true)
        .whereType<File>()
        .where((f) => !f.path.contains('/web_worlds/'))
        .toList()
      ..sort((a, b) => a.path.compareTo(b.path));
    for (final f in fs) {
      final s = f.statSync();
      listing.write('${f.path} ${s.size} ${s.modified.microsecondsSinceEpoch}\n');
    }
  }
  final key = sha256.convert(utf8.encode(listing.toString())).toString().substring(0, 16);
  final out = File('$dir/$c-$key.bin');
  if (out.existsSync()) return out.path;
  Directory(dir).createSync(recursive: true);
  final seedFile = File(seedPath);
  if (!seedFile.existsSync()) {
    seedFile.parent.createSync(recursive: true);
    final r = Random.secure();
    seedFile.writeAsBytesSync(Uint8List.fromList([for (var i = 0; i < 32; i++) r.nextInt(256)]));
  }
  final watch = Stopwatch()..start();
  final program = StoredProgram.compile(world,
      rootSelfGlpPath: '$root/programs/self.glp',
      host: PersonIdentity.fromSeed(seedFile.readAsBytesSync()));
  final tmp = File('${out.path}.tmp')..writeAsBytesSync(program.toBytes());
  tmp.renameSync(out.path);
  for (final f in Directory(dir).listSync().whereType<File>()) {
    final n = f.path.split('/').last;
    if (n.startsWith('$c-') && n.endsWith('.bin') && f.path != out.path && RegExp(r'^[0-9a-f]{16}\.bin$').hasMatch(n.substring(c.length + 1))) f.deleteSync();
  }
  stdout.writeln('compiled $c in ${watch.elapsed.inMilliseconds} ms');
  return out.path;
}

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
  // Run from servers/peoplesweb: the repository is two levels up, and build/www is here.
  final here = Directory.current.path;
  root = Platform.environment['GLP_ROOT'] ?? Directory(here).parent.parent.path;
  worldsDir = Platform.environment['WORLDS_DIR'] ?? '$root/programs/vglp/cva/web_worlds';
  compiledDir = Platform.environment['COMPILED_DIR'] ?? '$here/cache/compiled';
  hostSeed = Platform.environment['HOST_SEED'] ?? '$here/cache/host.seed';
  final port = int.parse(Platform.environment['PORT'] ?? '8080');
  final host = Platform.environment['HOST'] ?? '0.0.0.0';
  final webDir = Platform.environment['WEB_DIR'] ?? '$here/build/www';
  webRoot = webDir;
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
    // The feedback form (lib/feedback.dart).
    if (seg.length == 3 && seg[0] == 'api' && seg[1] == 'feedback') {
      if (seg[2] == 'post' && req.method == 'POST') {
        final text = await utf8.decodeStream(req.take(64 * 1024));
        Map body;
        try {
          body = jsonDecode(text) as Map;
        } catch (_) {
          body = {};
        }
        _json(req, await feedback.post(body));
      } else if (seg[2] == 'verify' && req.method == 'GET') {
        req.response.headers.contentType = ContentType.html;
        req.response.write(await feedback.verify(req.uri.queryParameters['t'] ?? ''));
        await req.response.close();
      } else {
        req.response.statusCode = 404;
        await req.response.close();
      }
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
