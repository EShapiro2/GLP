// The app as a person sees it, off the phone: two persons, each the app's core
// (core.dart) and its server (local_server.dart) on its own unpacked bundle,
// their layers the headless one on the loopback, each told of the other as
// the local network tells the phone; the page at http://127.0.0.1:8301/ and
// :8302/, to look at and to play in a browser.  GAS_UI_DEMO=<minutes> keeps
// them up; self-skips without it.  The relay is off.
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart' show Ed25519;
import 'package:flutter_test/flutter_test.dart';
import 'package:gas_core/contracts.dart';
import 'package:gas_core/core.dart';
import 'package:gas_core/local_server.dart';
import 'package:gas_phone/main.dart' show unbundle;
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:grassroots_networking_core/grassroots_networking_core.dart';
import 'package:grassroots_networking_glue/headless_glp_network.dart';
import 'package:sodium/sodium_sumo.dart';

/// The layers started, each told of the others.
final _up = <HeadlessGrassrootsNetwork>[];

class DemoLayer implements Layer {
  final HeadlessGrassrootsNetwork layer;
  @override
  final glp.GlpNetwork net;
  DemoLayer(this.layer) : net = HeadlessGlpNetwork(layer);

  @override
  Future<void> start() async {
    await layer.start();
    for (final o in _up) {
      if (o.identity.publicKey.toString() == layer.identity.publicKey.toString()) continue;
      final me = glp.PubKey.fromList(layer.identity.publicKey), them = glp.PubKey.fromList(o.identity.publicKey);
      net.putKnownPeer(them);
      net.putPeerAddress(them, '127.0.0.1:${o.boundPort}');
      final back = HeadlessGlpNetwork(o);
      back.putKnownPeer(me);
      back.putPeerAddress(me, '127.0.0.1:${layer.boundPort}');
    }
    _up.add(layer);
  }

  @override
  Future<void> dispose() async {
    _up.remove(layer);
    layer.dispose();
  }
}

Future<Core> person(SodiumSumo sodium, Directory root, int i, String name) async {
  final base = await unbundle('${root.path}/p$i');
  final store = ContractStore(base);
  await store.installStarters();
  Directory('$base/relay').createSync(recursive: true);
  File('$base/relay/off').writeAsStringSync('off');
  var id = await GrassrootsIdentity.create(
      keyPair: await Ed25519().newKeyPairFromSeed(Uint8List.fromList(List.filled(32, 0x61 + i))));
  final core = Core(
    base: base,
    store: store,
    sodium: sodium,
    me: glp.PubKey.fromList(id.publicKey),
    privateKey: Uint8List.fromList(id.privateKey),
    layers: () async => DemoLayer(HeadlessGrassrootsNetwork(
          identity: id,
          sodium: sodium,
          outboundQueueStore: FileOutboundQueueStore('$base/queue.json'),
        )),
    fresh: () async {
      id = await GrassrootsIdentity.generate();
      return (pub: glp.PubKey.fromList(id.publicKey), priv: Uint8List.fromList(id.privateKey));
    },
    relayUrl: 'http://127.0.0.1:9',
  );
  await core.setName(core.me.hex, name);
  return core;
}

void main() {
  TestWidgetsFlutterBinding.ensureInitialized();
  test('the app, for two persons, in a browser', () async {
    final minutes = int.tryParse(Platform.environment['GAS_UI_DEMO'] ?? '');
    if (minutes == null) return markTestSkipped('GAS_UI_DEMO not set');
    HttpOverrides.global = null;
    final sodium = await initHeadlessSodium();
    final root = Directory.systemTemp.createTempSync('gas_ui_');
    addTearDown(() => root.deleteSync(recursive: true));
    final ann = await person(sodium, root, 0, 'Ann'), ben = await person(sodium, root, 1, 'Ben');
    await ann.setName(ben.me.hex, 'Ben');
    await ben.setName(ann.me.hex, 'Ann');
    for (final (c, port) in [(ann, 8301), (ben, 8302)]) {
      final s = LocalServer(core: c, www: '${c.base}/www');
      await s.start(port: port);
    }
    // ignore: avoid_print
    print('UI demo up: http://127.0.0.1:8301/ (Ann) and http://127.0.0.1:8302/ (Ben)');
    await Future<void>.delayed(Duration(minutes: minutes));
  }, timeout: const Timeout(Duration(hours: 3)));
}
