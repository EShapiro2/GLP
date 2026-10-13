/// One madGLP agent over its own headless layer through the adapter, in a
/// process of its own, hosted by the runtime's own host, `AgentRuntime`: the
/// second process of `headless_glp_network_test.dart`.
///
///   dart run test/helpers/glp_adapter_agent.dart <seed-hex> <root-self.glp>
///       <program> <goal-label> <known-peer-hex>
///
/// The agent's key pair is the Ed25519 pair of the 32-byte seed, the layer's
/// and the host's alike.  The layer binds an ephemeral UDP port; the peer's
/// key is put in the layer's known-peer set through the adapter
/// (`putKnownPeer`), since the headless layer answers no unsolicited inbound
/// WAN contact from a key outside it (GNA Section 2.7); then the agent is
/// booted on `<goal-label>`, and the layer started.
///
/// Prints, one per line: `READY <port> <key-hex>` once listening; `OUT <line>`
/// for each element the agent sends its person, as its transcript shows it;
/// `CONNECTED <key-hex> <underlay> <identity>` and `RECEIVED <key-hex>
/// <underlay>` for each connection and delivery the adapter reports.  Ends
/// when its standard input closes.
library;

import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:grassroots_networking_glue/headless_glp_network.dart';
import 'package:grassroots_networking_core/grassroots_networking_core.dart';

Future<void> main(List<String> args) async {
  final seed = Uint8List.fromList([
    for (var i = 0; i < args[0].length; i += 2)
      int.parse(args[0].substring(i, i + 2), radix: 16),
  ]);
  final rootSelfGlpPath = args[1];
  final program = args[2];
  final goalLabel = args[3];
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
  network.putKnownPeer(knownPeer);

  final agent = AgentRuntime(
    agentId: 'agent',
    program: program,
    rootSelfGlpPath: rootSelfGlpPath,
    goalLabel: goalLabel,
    keyPair: (
      pub: glp.PubKey.fromList(identity.publicKey),
      priv: Uint8List.fromList(identity.privateKey),
    ),
    network: network,
  );
  // The transcript marks each element sent to the person with `< `.
  agent.onOutput = (line) {
    if (line.startsWith('< ')) stdout.writeln('OUT ${line.substring(2)}');
  };
  agent.onPeerConnected = (pk, underlay, attested) =>
      stdout.writeln('CONNECTED ${pk.hex} ${underlay.name} $attested');

  // The agent is bound to its layer before the layer starts, so that no
  // message the layer delivers finds no agent.
  await agent.initialize();
  // The host installs its own onMessageReceived; report each delivery too.
  final deliver = network.onMessageReceived!;
  network.onMessageReceived = (sender, payload, messageId, underlay) {
    stdout.writeln('RECEIVED ${sender.hex} ${underlay.name}');
    deliver(sender, payload, messageId, underlay);
  };
  if (!await layer.start()) {
    stderr.writeln('the layer bound no socket');
    exit(1);
  }

  stdout.writeln('READY ${layer.boundPort} ${agent.name}');

  await stdin.transform(utf8.decoder).drain<void>();
  agent.dispose();
  await layer.dispose();
  exit(0);
}
