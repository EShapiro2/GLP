// A relay that replays: Ann's message to Ben, once applied, is put back on the
// relay under a new id (what a relay holding it could do) and is not applied
// again, by Ben's network as it runs nor by a new one after a restart; and a
// message moved to another app is dropped.
//
// Needs GASRELAY=<path to gasrelay/bin/relay.dart> and a native libsodium;
// self-skips without either.
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart' show Ed25519;
import 'package:flutter_test/flutter_test.dart';
import 'package:gas_core/relay_network.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:grassroots_networking_core/grassroots_networking_core.dart';
import 'package:grassroots_networking_glue/headless_glp_network.dart';

String hex(List<int> b) => b.map((x) => x.toRadixString(16).padLeft(2, '0')).join();

void main() {
  test('a replayed message is applied once', () async {
    final src = Platform.environment['GASRELAY'];
    if (src == null || !File(src).existsSync()) return markTestSkipped('GASRELAY not set');
    final sodium = await initHeadlessSodium();
    final tmp = Directory.systemTemp.createTempSync('gas_replay_');
    addTearDown(() => tmp.deleteSync(recursive: true));
    final s = await ServerSocket.bind('127.0.0.1', 0);
    final port = s.port;
    await s.close();
    final url = 'http://127.0.0.1:$port';

    final layers = <String, HeadlessGrassrootsNetwork>{};
    // The person's relay network over their layer, the relay that of [url].
    RelayGlpNetwork net(GrassrootsIdentity id, String name) => RelayGlpNetwork(
          inner: HeadlessGlpNetwork(layers[name]!),
          sodium: sodium,
          privateKey: Uint8List.fromList(id.privateKey),
          app: 'gkk',
          dir: '${tmp.path}/$name',
          relayUrl: url,
        );
    Future<(GrassrootsIdentity, RelayGlpNetwork)> person(String name, int fill) async {
      final id = await GrassrootsIdentity.create(
          keyPair: await Ed25519().newKeyPairFromSeed(List.filled(32, fill)));
      final layer = HeadlessGrassrootsNetwork(
          identity: id,
          sodium: sodium,
          outboundQueueStore: FileOutboundQueueStore('${tmp.path}/$name.q'));
      addTearDown(layer.dispose);
      layers[name] = layer;
      return (id, net(id, name));
    }

    // Ann's message waits in her outbox while the relay is down, so that the
    // test holds the ciphertext a relay would hold.
    final (annId, ann) = await person('ann', 7);
    final (benId, ben) = await person('ben', 8);
    final benPk = glp.PubKey.fromList(benId.publicKey);
    ann.setOn(true);
    await ann.send(benPk, Uint8List.fromList(utf8.encode('hello Ben')));
    final held = (jsonDecode(File('${tmp.path}/ann/gkk-outbox.json').readAsStringSync()) as List)
        .cast<Map>()
        .single;

    final relay = await Process.start('dart', [src, '$port'],
        workingDirectory: File(src).parent.parent.path);
    addTearDown(relay.kill);
    await relay.stdout.transform(utf8.decoder).firstWhere((l) => l.contains('gasrelay on'));

    final got = <String>[];
    void listen(RelayGlpNetwork n) => n.onMessageReceived = (pk, payload, id, u) {
          got.add(utf8.decode(payload));
          n.acknowledge(id);
        };
    listen(ben);
    ben.setOn(true);
    final until = DateTime.now().add(const Duration(seconds: 30));
    while (got.isEmpty && DateTime.now().isBefore(until)) {
      await Future<void>.delayed(const Duration(milliseconds: 200));
    }
    expect(got, ['hello Ben']);

    // The relay puts it back, under a new id, and under another app.
    Future<void> replay(String id, String app) async {
      final kp = await Ed25519().newKeyPairFromSeed(List.filled(32, 7));
      final key = hex(annId.publicKey);
      final b = jsonEncode({
        'msgs': [{...held, 'id': id, 'app': app}]
      });
      final t = DateTime.now().millisecondsSinceEpoch;
      final sig = await Ed25519().sign(utf8.encode('gasrelay1\n/send\n$key\n$t\n$b'), keyPair: kp);
      final c = HttpClient();
      final req = await c.post('127.0.0.1', port, '/send');
      req.write(jsonEncode({'key': key, 't': t, 'b': b, 'sig': hex(sig.bytes)}));
      expect((await req.close()).statusCode, 200);
      c.close();
    }

    await Future<void>.delayed(const Duration(seconds: 1));
    await replay('again1', 'gkk');
    await replay('other', 'gc');
    await Future<void>.delayed(const Duration(seconds: 3));
    expect(got, ['hello Ben'], reason: 'the replay is not applied');

    // Ben's phone restarts: a new network over the same files.
    ben.dispose();
    final ben2 = net(benId, 'ben');
    listen(ben2);
    ben2.setOn(true);
    addTearDown(ben2.dispose);
    await replay('again2', 'gkk');
    await Future<void>.delayed(const Duration(seconds: 4));
    expect(got, ['hello Ben'], reason: 'nor after a restart');
    expect(ben2.state, 'online');
    ann.dispose();
  }, timeout: const Timeout(Duration(minutes: 3)));
}
