/// One madGLP agent over its own headless layer through the adapter, in a
/// process of its own: the second process of `glp_adapter_test.dart`.
///
///   dart run test/helpers/glp_adapter_agent.dart <seed-hex> <root-self.glp>
///       <program> <entry> <known-peer-hex>
///
/// The agent's key pair is the Ed25519 pair of the 32-byte seed.  The layer
/// binds an ephemeral UDP port; the agent's host puts the peer's key in the
/// layer's known-peer set through the adapter (`putKnownPeer`), since the
/// headless layer answers no unsolicited inbound contact from a key outside it
/// (GNA, "Cold Calls", Trust levels); then boots the agent on `<entry>`.
///
/// Prints, one per line: `READY <port> <key-hex>` once listening; `OUT <line>`
/// for each line the agent sends its person; `CONNECTED <key-hex> <underlay>
/// <identity>` and `RECEIVED <key-hex> <underlay>` for each connection and
/// delivery the adapter reports.  Ends when its standard input closes.
library;

import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:grassroots_networking_core/glp_adapter.dart';
import 'package:grassroots_networking_core/grassroots_networking_core.dart';

import 'glp_adapter_host.dart';

Future<void> main(List<String> args) async {
  final seed = Uint8List.fromList([
    for (var i = 0; i < args[0].length; i += 2)
      int.parse(args[0].substring(i, i + 2), radix: 16),
  ]);
  final rootSelfGlpPath = args[1];
  final program = args[2];
  final entry = args[3];
  final knownPeer = glp.PubKey.fromHex(args[4]);

  final sodium = await initHeadlessSodium();
  final identity = await GrassrootsIdentity.create(
    keyPair: await Ed25519().newKeyPairFromSeed(seed),
    nickname: 'agent',
  );
  final layer = HeadlessGrassrootsNetwork(
    identity: identity,
    rendezvousServers: const [],
    sodium: sodium,
  );
  final network = HeadlessGlpNetwork(layer);

  network.onPeerConnected = (pk, underlay, attested) =>
      stdout.writeln('CONNECTED ${pk.hex} ${underlay.name} $attested');
  network.putKnownPeer(knownPeer);
  if (!await layer.start()) {
    stderr.writeln('the layer bound no socket');
    exit(1);
  }

  final host = GlpAdapterHost.boot(
    rootSelfGlpPath: rootSelfGlpPath,
    program: program,
    identity: identity,
    network: network,
    entry: entry,
  );
  // The host installs its own onMessageReceived; report each delivery too.
  final deliver = network.onMessageReceived!;
  network.onMessageReceived = (sender, payload, messageId, underlay) {
    stdout.writeln('RECEIVED ${sender.hex} ${underlay.name}');
    deliver(sender, payload, messageId, underlay);
  };
  host.lines.listen((line) => stdout.writeln('OUT $line'));
  for (final line in host.outputs) {
    stdout.writeln('OUT $line');
  }

  stdout.writeln('READY ${layer.boundPort} ${host.name}');

  await stdin.transform(utf8.decoder).drain<void>();
  await layer.dispose();
  exit(0);
}
