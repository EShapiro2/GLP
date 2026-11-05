// GAS on the web: the GAS app as a web page, for devices the app is not on
// yet (iPhones), each person's agent running here, on the server, over the
// relay alone.  The page is the app's own (www/phone.html); each person is a
// Core of their own (core.dart) over one shared tree of the GLP programs and
// installed apps, answered by a LocalServer of their own (local_server.dart).
//
// A person is a browser's secret: POST api/start makes a key pair here and
// answers a token, which the page keeps (localStorage) and sends with every
// request (X-GAS-Token).  The server keeps, per person, the key pair under
// the token's SHA-256 (users/<hash>.json), and under people/<key>/ what the
// app on a device keeps there: their apps, the people they met, the names
// they give, each app's saved state.  So the server holds the person's key:
// the page says so.
//
// An agent runs while its person's page is open: one with no request for
// IDLE minutes is closed (its state saved; the relay keeps what comes
// meanwhile), and at most MAX agents run at once.
//
// Environment: PORT (4501); DATA (the state, default ./gasweb-data); TREE,
// the app's bundle tree (servers/gas/phone/assets/tree, by sync_assets.sh),
// copied into DATA/base at every start; RELAY_URL (the relay, default
// talmonlab's); LIBSODIUM_PATH (libsodium, where the system has none);
// MAX (25); IDLE (15); HOST (the loopback).
import 'dart:async';
import 'dart:convert';
import 'dart:ffi';
import 'dart:io';
import 'dart:math';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart' show Sha256;
import 'package:gas_core/contracts.dart';
import 'package:gas_core/core.dart';
import 'package:gas_core/local_server.dart';
import 'package:gas_core/offline_network.dart';
import 'package:gas_core/relay_network.dart' show defaultRelay;
import 'package:glp_runtime/engine/glp_engine.dart' show GlpEngine;
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:sodium/sodium_sumo.dart';

final env = Platform.environment;
final data = env['DATA'] ?? 'gasweb-data';
final relayUrl = env['RELAY_URL'] ?? defaultRelay;
final maxAgents = int.tryParse(env['MAX'] ?? '') ?? 25;
final idle = Duration(minutes: int.tryParse(env['IDLE'] ?? '') ?? 15);
final _rnd = Random.secure();

class WebLayer implements Layer {
  @override
  final glp.GlpNetwork net;
  WebLayer(this.net);
  @override
  Future<void> start() async {}
  @override
  Future<void> dispose() async {}
}

class Person {
  final String file;
  final Core core;
  final LocalServer server;
  DateTime seen = DateTime.now();
  Person(this.file, this.core, this.server);
}

late final SodiumSumo sodium;
late final String base;
late final ContractStore store;
final people = <String, Person>{};

Future<String> sha256Hex(String s) async =>
    (await Sha256().hash(utf8.encode(s))).bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

({glp.PubKey pub, Uint8List priv}) newPair() {
  final kp = sodium.crypto.sign.keyPair();
  final pair = (pub: glp.PubKey.fromList(kp.publicKey), priv: kp.secretKey.extractBytes());
  kp.secretKey.dispose();
  return pair;
}

void savePair(String file, ({glp.PubKey pub, Uint8List priv}) k) {
  File(file).parent.createSync(recursive: true);
  File(file).writeAsStringSync(jsonEncode({
    'publicKey': k.pub.hex,
    'privateKey': base64Encode(k.priv),
    'created': DateTime.now().toUtc().toIso8601String(),
  }));
}

Person open(String file) {
  final j = jsonDecode(File(file).readAsStringSync()) as Map;
  final pub = glp.PubKey.fromHex(j['publicKey'] as String);
  final priv = Uint8List.fromList(base64Decode(j['privateKey'] as String));
  late final Core core;
  core = Core(
    base: base,
    store: store,
    sodium: sodium,
    me: pub,
    privateKey: priv,
    hosted: true,
    relayUrl: relayUrl,
    layers: () async => WebLayer(OfflineGlpNetwork(sodium: sodium, me: core.me, privateKey: core.privateKey)),
    fresh: () async {
      // GKK, which every new person has, checked before the first comes.
  for (final c in store.installed().where((c) => c.id == 'GKK')) {
    try {
      GlpEngine(rootSelfGlpPath: '$base/programs/self.glp').loadProgram(store.program(c));
    } on Object {
      // its first person's agent says what is wrong
    }
  }
  final k = newPair();
      savePair(file, k);
      return k;
    },
  );
  return Person(file, core, LocalServer(core: core, www: '$base/www'));
}

/// The bundle tree copied into the base, at every start: the GLP programs
/// and the page, as this version of GAS has them; the apps installed there
/// stay.
Future<void> syncTree(String tree) async {
  final src = Directory(tree);
  for (final e in src.listSync(recursive: true).whereType<File>()) {
    final rel = e.path.substring(src.path.length + 1);
    final to = File('$base/$rel');
    to.parent.createSync(recursive: true);
    e.copySync(to.path);
  }
}

Future<void> json(HttpRequest req, int status, Object o) async {
  req.response.statusCode = status;
  req.response.headers.contentType = ContentType.json;
  req.response.headers.set('Cache-Control', 'no-store');
  req.response.write(jsonEncode(o));
  await req.response.close();
}

Future<void> handle(HttpRequest req) async {
  try {
    final seg = req.uri.pathSegments;
    final api = seg.isNotEmpty && seg[0] == 'api' ? (seg.length > 1 ? seg[1] : '') : null;
    if (api == 'health') {
      return await json(req, 200, {'ok': true, 'apps': store.installed().length, 'people': people.length,
          'agents': people.values.where((p) => p.core.session != null).length});
    }
    if (api == null || api == 'qr') {
      // The page's files and QR codes: anyone's.  Any person's server serves them.
      return await files.handle(req);
    }
    if (api == 'start' && req.method == 'POST') {
      final token = List.generate(32, (_) => _rnd.nextInt(256)).map((b) => b.toRadixString(16).padLeft(2, '0')).join();
      final file = '$data/users/${await sha256Hex(token)}.json';
      savePair(file, newPair());
      return await json(req, 200, {'token': token});
    }
    final token = req.headers.value('x-gas-token') ?? '';
    if (!RegExp(r'^[0-9a-f]{64}$').hasMatch(token)) return await json(req, 401, {'error': 'who are you?'});
    final h = await sha256Hex(token);
    var p = people[h];
    if (p == null) {
      final file = '$data/users/$h.json';
      if (!File(file).existsSync()) return await json(req, 401, {'error': 'who are you?'});
      p = people[h] = open(file);
    }
    p.seen = DateTime.now();
    if (api == 'open' && p.core.session == null &&
        people.values.where((q) => q.core.session != null).length >= maxAgents) {
      return await json(req, 503, {'error': 'GAS on the web is full right now. Try again in a few minutes.'});
    }
    await p.server.handle(req);
  } catch (e) {
    try {
      await json(req, 500, {'error': '$e'});
    } catch (_) {}
  }
}

// The page's files and QR codes, which are no one's.
late final LocalServer files;

Future<void> main() async {
  final port = int.tryParse(env['PORT'] ?? '') ?? 4501;
  base = '$data/base';
  Directory(base).createSync(recursive: true);
  final tree = env['TREE'];
  try {
    if (tree != null) await syncTree(tree);
    await startUp();
  } catch (e, st) {
    // Up anyway, saying why it cannot serve (GET /api/health).
    broken = '$e\n$st';
  }
  final server = await HttpServer.bind(env['HOST'] ?? InternetAddress.loopbackIPv4, port);
  server.listen((req) => unawaited(broken == null ? handle(req) : json(req, 503, {'error': broken})));
  stdout.writeln(broken ?? 'gasweb on $port, ${store.installed().length} apps, relay $relayUrl');
}

String? broken;

Future<void> startUp() async {
  sodium = await SodiumSumoInit.init(() {
    final path = env['LIBSODIUM_PATH'];
    if (path != null && path.isNotEmpty) return DynamicLibrary.open(path);
    for (final n in [
      if (Platform.isLinux) ...['libsodium.so.23', 'libsodium.so'],
      if (Platform.isMacOS) ...['/opt/homebrew/lib/libsodium.dylib', '/usr/local/lib/libsodium.dylib'],
      if (Platform.isWindows) 'libsodium.dll',
    ]) {
      try {
        return DynamicLibrary.open(n);
      } on Object {
        // the next
      }
    }
    throw StateError('no libsodium: set LIBSODIUM_PATH');
  });
  store = ContractStore(base);
  await store.installStarters();
  // GKK, which every new person has, checked before the first comes.
  for (final c in store.installed().where((c) => c.id == 'GKK')) {
    try {
      GlpEngine(rootSelfGlpPath: '$base/programs/self.glp').loadProgram(store.program(c));
    } on Object {
      // its first person's agent says what is wrong
    }
  }
  final k = newPair();
  files = LocalServer(
      core: Core(base: base, store: store, sodium: sodium, me: k.pub, privateKey: k.priv,
          layers: () async => throw StateError('none'), hosted: true),
      www: '$base/www');
  // Agents idle too long are closed.
  Timer.periodic(const Duration(minutes: 1), (_) async {
    final now = DateTime.now();
    for (final e in [...people.entries]) {
      if (now.difference(e.value.seen) < idle) continue;
      await e.value.core.close();
      people.remove(e.key);
    }
  });
}
