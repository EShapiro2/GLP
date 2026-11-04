// A manual demo, not a check: two persons of a contract, each with this app's
// own server, over the headless layer on the loopback, kept running for a
// while so that a browser can play them as two phones.  Runs only with
// GAS_DEMO set: GAS_DEMO=gkk flutter test test/demo_test.dart, then open
// http://127.0.0.1:8201/app.html?c=gkk&solo and :8202 likewise.
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart' show Ed25519;
import 'package:flutter_test/flutter_test.dart';
import 'package:gas_phone/local_server.dart';
import 'package:gas_phone/person_host.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:grassroots_networking_core/grassroots_networking_core.dart';
import 'package:grassroots_networking_glue/headless_glp_network.dart';

void main() {
  final contract = Platform.environment['GAS_DEMO'];
  test('demo', () async {
    if (contract == null) return markTestSkipped('set GAS_DEMO to a contract');
    final programs = Directory('../../../programs').absolute.path;
    final www = Directory('../build/www').absolute.path;
    final sodium = await initHeadlessSodium();
    final queues = Directory.systemTemp.createTempSync('gas_demo_');
    final calendar = File('$programs/vglp/cva/gas_worlds/live_$contract/self.glp')
        .readAsStringSync()
        .contains('cal(alice, N)');
    final hosts = <PersonHost>[], layers = <HeadlessGrassrootsNetwork>[];
    final servers = <LocalServer>[];
    for (final (i, name) in [(0x41, 'Ann'), (0x42, 'Ben')]) {
      final id = await GrassrootsIdentity.create(
          keyPair: await Ed25519()
              .newKeyPairFromSeed(Uint8List.fromList(List.filled(32, i))));
      final layer = HeadlessGrassrootsNetwork(
          identity: id,
          sodium: sodium,
          outboundQueueStore: FileOutboundQueueStore('${queues.path}/$i.json'));
      final host = PersonHost(
        program: '$programs/vglp/cva/gas_worlds/live_$contract',
        rootSelfGlp: '$programs/self.glp',
        keyPair: (pub: glp.PubKey.fromList(id.publicKey), priv: Uint8List.fromList(id.privateKey)),
        network: HeadlessGlpNetwork(layer),
      );
      await host.start(calendar: calendar);
      await layer.start();
      hosts.add(host);
      layers.add(layer);
      final server = LocalServer(
        host: host,
        contract: contract,
        www: www,
        peers: () => [
          for (final o in hosts)
            if (o != host) {'key': o.me, 'state': 'connected'}
        ],
      );
      servers.add(server);
      server.names[host.me] = name;
      await server.start(port: 8200 + hosts.length);
      // ignore: avoid_print
      print('$name: ${server.url.replaceFirst(RegExp(r':\d+/'), ':${8200 + hosts.length}/')}');
    }
    servers[0].names[hosts[1].me] = 'Ben';
    servers[1].names[hosts[0].me] = 'Ann';
    for (var i = 0; i < 2; i++) {
      final o = 1 - i;
      final net = hosts[i].network;
      net.putKnownPeer(hosts[o].keyPair.pub);
      net.putPeerAddress(hosts[o].keyPair.pub, '127.0.0.1:${layers[o].boundPort}');
    }
    await Future<void>.delayed(const Duration(minutes: 25));
  }, timeout: const Timeout(Duration(minutes: 30)));
}
