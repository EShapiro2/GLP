// Two persons of GKK, each the agent of PersonHost over its own headless
// networking layer (GNA's HeadlessGrassrootsNetwork through the adapter
// HeadlessGlpNetwork), named by their keys: what two phones do, with UDP on
// the loopback in place of the phones' radios.  Alice and Bob each establish
// their identity, Alice asks to acquaint Bob, Bob's dispatcher draws the
// question, Bob says yes, and each comes to know the other.
//
// Needs the worlds built (python3 -I ../tools/build.py) and a native
// libsodium, as GNA's headless tests do; self-skips without either.
import 'dart:async';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart' show Ed25519;
import 'package:flutter_test/flutter_test.dart';
import 'package:gas_core/person_host.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:grassroots_networking_core/grassroots_networking_core.dart';
import 'package:grassroots_networking_glue/headless_glp_network.dart';
import 'package:sodium/sodium_sumo.dart';

Map<String, Object?> S(String f, List<Object?> a) => {'f': f, 'a': a};

void main() {
  final programs = Platform.environment['GAS_PROGRAMS'] ??
      Directory('../../../programs').absolute.path;
  final world = '$programs/vglp/cva/gas_worlds/live_gkk';

  test('GKK between two persons named by their keys, over the headless layer',
      () async {
    if (!Directory(world).existsSync()) {
      markTestSkipped('no worlds: run python3 -I ../tools/build.py');
      return;
    }
    final SodiumSumo sodium;
    try {
      sodium = await initHeadlessSodium();
    } on Object catch (e) {
      markTestSkipped('libsodium unavailable: $e');
      return;
    }
    final queues = Directory.systemTemp.createTempSync('gas_phone_');
    addTearDown(() => queues.deleteSync(recursive: true));

    Future<(PersonHost, HeadlessGrassrootsNetwork)> person(int fill) async {
      final id = await GrassrootsIdentity.create(
          keyPair: await Ed25519()
              .newKeyPairFromSeed(Uint8List.fromList(List.filled(32, fill))));
      final layer = HeadlessGrassrootsNetwork(
        identity: id,
        sodium: sodium,
        outboundQueueStore: FileOutboundQueueStore('${queues.path}/$fill.json'),
      );
      addTearDown(layer.dispose);
      final host = PersonHost(
        program: world,
        rootSelfGlp: '$programs/self.glp',
        keyPair: (
          pub: glp.PubKey.fromList(id.publicKey),
          priv: Uint8List.fromList(id.privateKey)
        ),
        network: HeadlessGlpNetwork(layer),
      );
      return (host, layer);
    }

    final (alice, aliceLayer) = await person(0x41);
    final (bob, bobLayer) = await person(0x42);
    await alice.start();
    await bob.start();
    expect(alice.status, 'ready');
    expect(bob.status, 'ready');
    expect(await aliceLayer.start(), isTrue);
    expect(await bobLayer.start(), isTrue);
    // What discovery gives two phones on one network: each the other's key
    // and address.
    final aNet = alice.network, bNet = bob.network;
    aNet.putKnownPeer(bob.keyPair.pub);
    aNet.putPeerAddress(bob.keyPair.pub, '127.0.0.1:${bobLayer.boundPort}');
    bNet.putKnownPeer(alice.keyPair.pub);
    bNet.putPeerAddress(alice.keyPair.pub, '127.0.0.1:${aliceLayer.boundPort}');

    (int, List<int>) menu(PersonHost p) {
      for (final e in p.draws.entries) {
        final v = e.value;
        if (v is Map && v['f'] == 'menu_w') {
          final input = ((v['a'] as List)[0] as Map)['a'] as List;
          return (e.key, [for (final x in ((input[2] as Map)['a'] as List)[0] as List) x as int]);
        }
      }
      throw StateError('no menu');
    }

    Future<void> propose(PersonHost p, Map<String, Object?> v) async {
      final (id, path) = menu(p);
      await p.act(id, path, S('propose', [v]));
    }

    Future<T> until<T>(T? Function() f, String what) async {
      final deadline = DateTime.now().add(const Duration(seconds: 90));
      while (true) {
        final v = f();
        if (v != null) return v;
        if (DateTime.now().isAfter(deadline)) throw TimeoutException(what);
        await Future<void>.delayed(const Duration(milliseconds: 100));
      }
    }

    List<Object?> atoms(PersonHost p) {
      final out = p.draws[0];
      if (out is! Map) return const [];
      final views = (out['a'] as List)[0] as List;
      return (((views.last as Map)['a'] as List)[0] as List);
    }

    await propose(alice, S('establish', [alice.me]));
    await propose(bob, S('establish', [bob.me]));
    await propose(alice, S('acquaint', [alice.me, bob.me]));

    // Bob's dispatcher draws Alice's hello; Bob answers yes.
    final ask = await until(() {
      for (final e in bob.draws.entries) {
        final v = e.value;
        if (v is Map && v['f'] == 'approval_w') return e;
      }
      return null;
    }, 'Bob was never asked');
    final approval = ((ask.value as Map)['a'] as List)[0] as Map;
    final input = (approval['a'] as List)[2] as Map;
    await bob.act(ask.key, [for (final x in (input['a'] as List)[0] as List) x as int], 'yes');

    bool knows(PersonHost p, String q) => atoms(p).any((a) =>
        a is Map && a['f'] == 'knows' && ((a['a'] as List)[0]) == q);
    await until(() => knows(alice, bob.me) ? true : null, 'Alice never knew Bob');
    await until(() => knows(bob, alice.me) ? true : null, 'Bob never knew Alice');
  }, timeout: const Timeout(Duration(minutes: 5)));
}
