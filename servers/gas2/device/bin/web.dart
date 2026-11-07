// GAS2 on the web: the device's page for those with no app yet (iPhones), each
// person's super-app running here, on the server, over the relay alone.
//
// A person is a browser's secret: POST api/start makes a key pair here and answers a
// token, which the page keeps (localStorage) and sends with every request
// (X-GAS2-Token).  The server keeps, per person, the key pair under the token's hash
// (users/<hash>.json) and the person's data under people/<key>/: the mini-apps
// certified under their key, the agent's saved state.  So the server holds the
// person's key, and the page says so.
//
// A person's super-app runs while their page is open: one with no request for IDLE
// minutes is closed (its state is saved in every cycle; the relay keeps what comes
// meanwhile), and at most MAX run at once.
//
// Environment: PORT (5201); HOST (the loopback); DATA (./gas2web-data); TREE, the
// device tree (tools/device_tree.py), or GAS2_DATA, the GAS2 site's data, whose live
// build's device tree is taken; WWW (the device's page, ../web); RELAY_URL (talmonlab's
// relay); LIBSODIUM_PATH; MAX (25); IDLE (15).
import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:math';
import 'dart:typed_data';

import 'package:gas2_device/host.dart';
import 'package:gas2_device/offline_network.dart';
import 'package:gas2_device/page_server.dart';
import 'package:gas2_device/relay_network.dart';
import 'package:gas2_device/setup.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:sodium/sodium_sumo.dart';

final env = Platform.environment;
final data = env['DATA'] ?? 'gas2web-data';
final relayUrl = env['RELAY_URL'] ?? defaultRelay;
final maxRunning = int.tryParse(env['MAX'] ?? '') ?? 25;
final idle = Duration(minutes: int.tryParse(env['IDLE'] ?? '') ?? 15);
final www = env['WWW'] ?? '${File(Platform.script.toFilePath()).parent.parent.path}/web';
final _rnd = Random.secure();
late final SodiumSumo sodium;

class Person {
  final SuperHost host;
  final PageServer page;
  final RelayGlpNetwork relay;
  DateTime seen = DateTime.now();
  Person(this.host, this.page, this.relay);
}

final running = <String, Person>{}; // by the token's hash
final opening = <String, Future<Person>>{};

/// The device tree to copy for a person opened now: TREE, or the GAS2 site's live build's.
String tree() {
  final t = env['TREE'];
  if (t != null) return t;
  final d = env['GAS2_DATA'];
  if (d == null) throw StateError('set TREE or GAS2_DATA');
  final live = File('$d/live.txt').readAsStringSync().trim();
  return '$d/$live/out/device/programs';
}

String hashOf(String token) =>
    sodium.crypto.genericHash(message: Uint8List.fromList(utf8.encode(token)), outLen: 32).map((b) => b.toRadixString(16).padLeft(2, '0')).join();

Future<Person> open(String h) async {
  final j = jsonDecode(File('$data/users/$h.json').readAsStringSync()) as Map;
  final kp = (pub: PubKey.fromHex(j['publicKey'] as String), priv: Uint8List.fromList(base64Decode(j['privateKey'] as String)));
  final dir = '$data/people/${kp.pub.hex}';
  final own = Directory('$dir/tree');
  if (own.existsSync()) own.deleteSync(recursive: true);
  await copyTree(Directory(tree()), own);
  final relay = RelayGlpNetwork(
    inner: OfflineGlpNetwork(sodium: sodium, me: kp.pub, privateKey: kp.priv),
    sodium: sodium,
    privateKey: kp.priv,
    app: 'gas2',
    dir: '$dir/relay',
    relayUrl: relayUrl,
  )..relayOnly = true;
  final host = SuperHost(tree: own.path, keyPair: kp, network: relay, dataDir: dir);
  final page = PageServer(host: host, www: www, addPerson: (k) => relay.addPerson(PubKey.fromHex(k)))
    ..extra = {'hosted': true}
    ..reset = () async {
      // the person gone: their key pair, their data; the page starts a new one
      close(h);
      File('$data/users/$h.json').deleteSync();
      await Future<void>.delayed(const Duration(seconds: 2));
      try {
        final d = Directory(dir);
        if (d.existsSync()) d.deleteSync(recursive: true);
      } on Object catch (e) {
        stderr.writeln('reset: $dir: $e');
      }
    };
  final p = Person(host, page, relay);
  relay.setOn(true);
  unawaited(host.start());
  return p;
}

Future<Person?> person(String token) async {
  final h = hashOf(token);
  final p = running[h];
  if (p != null) return p..seen = DateTime.now();
  if (!File('$data/users/$h.json').existsSync()) return null;
  if (running.length >= maxRunning) closeOldest();
  final o = opening[h] ??= open(h);
  try {
    final q = await o;
    running[h] = q;
    return q;
  } finally {
    opening.remove(h);
  }
}

void close(String h) {
  final p = running.remove(h);
  if (p == null) return;
  p.relay.dispose();
  stdout.writeln('closed ${p.host.me.substring(0, 8)}');
}

void closeOldest() {
  final e = running.entries.reduce((a, b) => a.value.seen.isBefore(b.value.seen) ? a : b);
  close(e.key);
}

Future<void> json(HttpRequest r, int code, Object o) async {
  r.response.statusCode = code;
  r.response.headers.contentType = ContentType.json;
  r.response.headers.set('Cache-Control', 'no-store');
  r.response.write(jsonEncode(o));
  await r.response.close();
}

Future<void> handle(HttpRequest r) async {
  try {
    // The site is served under a prefix (/gas2web) that the proxy strips.
    final p = r.uri.path;
    if (p == '/api/health') {
      return await json(r, 200, {'ok': true, 'running': running.length, 'tree': tree()});
    }
    if (r.method == 'POST' && p == '/api/start') {
      final token = List.generate(32, (_) => _rnd.nextInt(256)).map((b) => b.toRadixString(16).padLeft(2, '0')).join();
      final kp = sodium.crypto.sign.keyPair();
      final f = File('$data/users/${hashOf(token)}.json');
      f.parent.createSync(recursive: true);
      f.writeAsStringSync(jsonEncode({
        'publicKey': PubKey.fromList(kp.publicKey).hex,
        'privateKey': base64Encode(kp.secretKey.extractBytes()),
        'created': DateTime.now().toUtc().toIso8601String(),
      }));
      kp.secretKey.dispose();
      return await json(r, 200, {'token': token});
    }
    if (p.startsWith('/api/')) {
      final token = r.headers.value('X-GAS2-Token');
      final q = token == null ? null : await person(token);
      if (q == null) return await json(r, 401, {'error': 'no such person', 'start': true});
      if (await q.page.api(r, p)) return;
      return await json(r, 404, {'error': 'no such request'});
    }
    await PageServer.file(r, www, p);
  } catch (e) {
    try {
      await json(r, 500, {'error': '$e'});
    } on Object {
      // the page went away
    }
  }
}

Future<void> main() async {
  sodium = await loadSodium();
  final port = int.tryParse(env['PORT'] ?? '') ?? 5201;
  final server = await HttpServer.bind(env['HOST'] ?? InternetAddress.loopbackIPv4, port);
  Timer.periodic(const Duration(minutes: 1), (_) {
    final now = DateTime.now();
    for (final h in [...running.keys]) {
      if (now.difference(running[h]!.seen) > idle) close(h);
    }
  });
  stdout.writeln('GAS2 web: http://${server.address.host}:$port/  (tree ${tree()})');
  await for (final r in server) {
    unawaited(handle(r));
  }
}
