// Persons for the headless tests: each a PersonHost over its own headless
// networking layer on the loopback, named by its key, every one given every
// other's key and address, as discovery gives them on one network; and the
// person's acts as the app page makes them.
import 'dart:async';
import 'dart:io';
import 'dart:math';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart' show Ed25519;
import 'package:flutter_test/flutter_test.dart';
import 'package:gas_core/person_host.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:grassroots_networking_core/grassroots_networking_core.dart';
import 'package:grassroots_networking_glue/headless_glp_network.dart';
import 'package:gas_core/relay_network.dart';

Map<String, Object?> S(String f, List<Object?> a) => {'f': f, 'a': a};

// Far apart, persons are fresh keys: a running relay may still hold what an
// earlier run's persons sent.
final _rnd = Random.secure();

final programs =
    Platform.environment['GAS_PROGRAMS'] ?? Directory('../../../programs').absolute.path;

String world(String contract) => '$programs/vglp/cva/gas_worlds/live_$contract';

/// Why the test cannot run here, or null.
Future<String?> cannotRun(String contract) async {
  if (!Directory(world(contract)).existsSync()) {
    return 'no worlds: run python3 -I ../tools/build.py';
  }
  try {
    await initHeadlessSodium();
  } on Object catch (e) {
    return 'libsodium unavailable: $e';
  }
  return null;
}

/// [n] persons of [contract], started, their layers started and joined; or,
/// given [relay], far apart: their layers not joined, each over a
/// [RelayGlpNetwork] on that relay, the relay on.
Future<List<PersonHost>> people(String contract, int n, {String? relay, String? program}) async {
  final prog = program ?? world(contract);
  final sodium = await initHeadlessSodium();
  final queues = Directory.systemTemp.createTempSync('gas_phone_');
  addTearDown(() => queues.deleteSync(recursive: true));
  final calendar = File('$prog/self.glp').readAsStringSync().contains('cal(alice, N)');
  final hosts = <PersonHost>[], layers = <HeadlessGrassrootsNetwork>[];
  for (var i = 0; i < n; i++) {
    final id = await GrassrootsIdentity.create(
        keyPair: await Ed25519()
            .newKeyPairFromSeed(Uint8List.fromList(
                relay == null ? List.filled(32, 0x41 + i) : List.generate(32, (_) => _rnd.nextInt(256)))));
    final layer = HeadlessGrassrootsNetwork(
      identity: id,
      sodium: sodium,
      outboundQueueStore: FileOutboundQueueStore('${queues.path}/$i.json'),
    );
    addTearDown(layer.dispose);
    final host = PersonHost(
      program: prog,
      rootSelfGlp: '$programs/self.glp',
      keyPair: (pub: glp.PubKey.fromList(id.publicKey), priv: Uint8List.fromList(id.privateKey)),
      network: relay == null
          ? HeadlessGlpNetwork(layer)
          : RelayGlpNetwork(
              inner: HeadlessGlpNetwork(layer),
              sodium: sodium,
              privateKey: Uint8List.fromList(id.privateKey),
              app: contract,
              dir: '${queues.path}/relay$i',
              relayUrl: relay,
            ),
    );
    await host.start(calendar: calendar);
    expect(host.status, 'ready');
    expect(await layer.start(), isTrue);
    hosts.add(host);
    layers.add(layer);
    if (host.network case final RelayGlpNetwork r) {
      r.setOn(true);
      addTearDown(r.dispose);
    }
  }
  if (relay != null) return hosts;
  for (var i = 0; i < n; i++) {
    for (var j = 0; j < n; j++) {
      if (i == j) continue;
      hosts[i].network.putKnownPeer(hosts[j].keyPair.pub);
      hosts[i].network.putPeerAddress(hosts[j].keyPair.pub, '127.0.0.1:${layers[j].boundPort}');
    }
  }
  return hosts;
}

List<int> _path(Map input) => [for (final x in (input['a'] as List)[0] as List) x as int];

/// The person's proposal [v], on the person's menu.
Future<void> propose(PersonHost p, Map<String, Object?> v) async {
  for (final e in p.draws.entries) {
    final w = e.value;
    if (w is Map && w['f'] == 'menu_w') {
      final menu = (w['a'] as List)[0] as Map;
      await p.act(e.key, _path((menu['a'] as List)[2] as Map), S('propose', [v]));
      return;
    }
  }
  throw StateError('no menu');
}

/// Every question now drawn to any of [ps], answered yes; how many.
Future<int> yesAll(List<PersonHost> ps) async {
  var n = 0;
  for (final p in ps) {
    for (final e in [...p.draws.entries]) {
      final w = e.value;
      if (w is Map && w['f'] == 'approval_w') {
        final ap = (w['a'] as List)[0] as Map;
        await p.act(e.key, _path((ap['a'] as List).last as Map), 'yes');
        n++;
      }
    }
  }
  return n;
}

/// Wait until [test] holds, answering yes to every question meanwhile.
Future<void> until(List<PersonHost> ps, bool Function() test, String what,
    {Duration within = const Duration(seconds: 120)}) async {
  final deadline = DateTime.now().add(within);
  while (!test()) {
    await yesAll(ps);
    if (DateTime.now().isAfter(deadline)) throw TimeoutException(what);
    await Future<void>.delayed(const Duration(milliseconds: 200));
  }
}

/// The atoms of the person's last view.
List<Object?> atoms(PersonHost p) {
  final out = p.draws[0];
  if (out is! Map) return const [];
  final views = ((out['a'] as List)[0] as List).where((x) => x is Map && x['f'] == 'view').toList();
  if (views.isEmpty) return const [];
  return ((views.last as Map)['a'] as List)[0] as List;
}

bool hasAtom(PersonHost p, String f, [List<Object?>? args]) => atoms(p).any((a) =>
    a is Map &&
    a['f'] == f &&
    (args == null || args.asMap().entries.every((e) => ((a['a'] as List)[e.key]) == e.value)));
