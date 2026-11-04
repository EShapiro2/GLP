// A person who closes the app and opens it again goes on where they left off
// (PersonHost's state file): Ann and Ben of GKK come to know each other; Ann's
// app is closed --- her agent and her networking layer gone --- and opened
// again over the same state file; she still knows Ben, with nothing asked
// again, and her agent goes on: she comes to know Cal, who is new.
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart' show Ed25519;
import 'package:flutter_test/flutter_test.dart';
import 'package:gas_phone/person_host.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:grassroots_networking_core/grassroots_networking_core.dart';
import 'package:grassroots_networking_glue/headless_glp_network.dart';

import 'people.dart';

void main() {
  test('a person goes on from the saved state after the app is closed', () async {
    final why = await cannotRun('gkk');
    if (why != null) return markTestSkipped(why);
    final sodium = await initHeadlessSodium();
    final dir = Directory.systemTemp.createTempSync('gas_restart_');
    addTearDown(() => dir.deleteSync(recursive: true));

    // One run of the person of seed [fill]: a layer and an agent, started.
    Future<(PersonHost, HeadlessGrassrootsNetwork)> run(int fill) async {
      final id = await GrassrootsIdentity.create(
          keyPair: await Ed25519().newKeyPairFromSeed(Uint8List.fromList(List.filled(32, fill))));
      final layer = HeadlessGrassrootsNetwork(
        identity: id,
        sodium: sodium,
        outboundQueueStore: FileOutboundQueueStore('${dir.path}/$fill.queue.json'),
      );
      addTearDown(layer.dispose);
      final host = PersonHost(
        program: world('gkk'),
        rootSelfGlp: '$programs/self.glp',
        keyPair: (pub: glp.PubKey.fromList(id.publicKey), priv: Uint8List.fromList(id.privateKey)),
        network: HeadlessGlpNetwork(layer),
        stateFile: '${dir.path}/$fill.bin',
      );
      await host.start();
      expect(host.status, 'ready');
      expect(await layer.start(), isTrue);
      return (host, layer);
    }

    void join(List<(PersonHost, HeadlessGrassrootsNetwork)> ps) {
      for (final (p, _) in ps) {
        for (final (q, l) in ps) {
          if (p == q) continue;
          p.network.putKnownPeer(q.keyPair.pub);
          p.network.putPeerAddress(q.keyPair.pub, '127.0.0.1:${l.boundPort}');
        }
      }
    }

    bool knows(PersonHost p, PersonHost q) => hasAtom(p, 'knows', [q.me]);

    var ann = await run(0x41);
    final ben = await run(0x42);
    expect(ann.$1.resumed, isFalse);
    join([ann, ben]);
    await propose(ann.$1, S('establish', [ann.$1.me]));
    await propose(ben.$1, S('establish', [ben.$1.me]));
    await propose(ann.$1, S('acquaint', [ann.$1.me, ben.$1.me]));
    await until([ann.$1, ben.$1], () => knows(ann.$1, ben.$1) && knows(ben.$1, ann.$1), 'Ann and Ben never met');

    // Ann closes the app.
    ann.$1.stop();
    await ann.$2.dispose();
    await Future<void>.delayed(const Duration(seconds: 1));

    // And opens it again: she knows Ben at once.
    ann = await run(0x41);
    expect(ann.$1.resumed, isTrue);
    expect(knows(ann.$1, ben.$1), isTrue);

    // Her agent goes on: Cal, new, and she come to know each other.
    final cal = await run(0x43);
    join([ann, ben, cal]);
    await propose(cal.$1, S('establish', [cal.$1.me]));
    await propose(ann.$1, S('acquaint', [ann.$1.me, cal.$1.me]));
    await until([ann.$1, cal.$1], () => knows(ann.$1, cal.$1) && knows(cal.$1, ann.$1), 'Ann never met Cal after the restart');
    expect(knows(ann.$1, ben.$1), isTrue);
  }, timeout: const Timeout(Duration(minutes: 6)));
}
