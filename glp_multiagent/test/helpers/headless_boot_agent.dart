/// One agent of a boot over the real networking layer, in a process of its
/// own: an `AgentRuntime` over its own headless layer through GNA's adapter,
/// named by its key (IGLP, Remark "Agent Names").  The processes of
/// `HeadlessBootProcesses` (headless_boot.dart); after glue's
/// test/helpers/glp_adapter_agent.dart.
///
///   dart run test/helpers/headless_boot_agent.dart <seed-hex> <root-self.glp>
///       <program> <goal-label> <extra-args-json> <peer-keys-hex,...>
///
/// The key pair is the Ed25519 pair of the 32-byte seed, the layer's and the
/// host's alike; the peers' keys go into the layer's known-peer set, since the
/// headless layer answers no unsolicited WAN contact from a key outside it
/// (GNA Section 2.7).  The layer binds an ephemeral UDP port and the process
/// prints `READY <port> <key-hex>`; it then reads its standard input: `PEER
/// <key-hex> <address>` gives it a peer's address, `GO` boots the agent on
/// `<goal-label>` with `<extra-args>` between its name and the network input,
/// after which it prints `INIT`.  `OUT <line>` is each element the agent
/// sends its person, as its transcript shows it.  Ends when its standard
/// input closes.
library;

import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:grassroots_networking_glue/headless_glp_network.dart';
import 'package:grassroots_networking_core/grassroots_networking_core.dart';

import 'headless_boot.dart' show bytesOfHex, identityOfSeed;

Future<void> main(List<String> args) async {
  final seed = bytesOfHex(args[0]);
  final rootSelfGlpPath = args[1];
  final program = args[2];
  final goalLabel = args[3];
  final extraArgs = (json.decode(args[4]) as List).cast<String>();
  final peers = args[5].isEmpty ? const <String>[] : args[5].split(',');

  final sodium = await initHeadlessSodium();
  final identity = await identityOfSeed(seed);
  final layer = HeadlessGrassrootsNetwork(
    outboundQueueStore: MemoryOutboundQueueStore(),
    identity: identity,
    sodium: sodium,
  );
  final network = HeadlessGlpNetwork(layer);
  for (final peer in peers) {
    network.putKnownPeer(glp.PubKey.fromHex(peer));
  }
  final agent = AgentRuntime(
    agentId: 'agent',
    program: program,
    rootSelfGlpPath: rootSelfGlpPath,
    goalLabel: goalLabel,
    extraArgs: extraArgs,
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
  if (!await layer.start()) {
    stderr.writeln('the layer bound no socket');
    exit(1);
  }
  stdout.writeln('READY ${layer.boundPort} ${agent.name}');

  final done = Completer<void>();
  stdin
      .transform(utf8.decoder)
      .transform(const LineSplitter())
      .listen((line) async {
    if (line.startsWith('PEER ')) {
      final parts = line.split(' ');
      network.putPeerAddress(glp.PubKey.fromHex(parts[1]), parts[2]);
    } else if (line == 'GO') {
      await agent.initialize();
      stdout.writeln('INIT');
    }
  }, onDone: () => done.complete());
  await done.future;
  agent.dispose();
  await layer.dispose();
  exit(0);
}
