/// One agent of a boot over the real networking layer, in a process of its
/// own: an `AgentRuntime` over its own headless layer through GNA's adapter,
/// named by its key (IGLP, Remark "Agent Names").  The processes of
/// `HeadlessBootProcesses` (headless_boot.dart); after glue's
/// test/helpers/glp_adapter_agent.dart.
///
///   dart <kernel> <persona> <root-self.glp> <program> [<boot-file>]
///
/// where <kernel> is this file compiled once, before any agent starts
/// (headless_boot.dart, `HeadlessAgentProgram`).
///
/// The agent's key pair is made at its spawn from the persona's name, as the
/// simulation makes it (headless_boot.dart, `identityOfPersona`), the layer's
/// and the host's alike.  The layer binds an ephemeral UDP port and the
/// process prints `READY <port> <key-hex>`; it then reads its standard
/// input: `PEER <key-hex> <address>` puts a peer in the layer's known-peer
/// set, since the headless layer answers no unsolicited WAN contact from a
/// key outside it (GNA Section 2.7), and gives it the peer's address; `GO
/// <json>` boots the agent on the one initial goal, `agent(ch(_?,_),
/// ch(_?,_))`, the JSON holding `act`, the person's first act (`functor`,
/// `args`) or null, which the host injects once the agent is initialised, and
/// `person`, the boot form's person goal (`functor`, `args`, `channelIndex`)
/// or null, after which it prints `INIT`.  `OUT
/// <line>` is each element the agent sends its person, as its transcript
/// shows it.  Ends when its standard input closes.
library;

import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart'
    show BootLoader, PersonGoal;
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:grassroots_networking_glue/headless_glp_network.dart';
import 'package:grassroots_networking_core/grassroots_networking_core.dart';

import 'headless_boot.dart' show actTerm, identityOfPersona;

Future<void> main(List<String> args) async {
  final persona = args[0];
  final rootSelfGlpPath = args[1];
  final program = args[2];
  // The boot file, in the boot form: its own module is loaded on top of the
  // program, in the file's scope (headless_boot.dart, HeadlessBootFile).
  final bootPath = args.length > 3 ? args[3] : null;
  final bootSource = bootPath == null
      ? null
      : BootLoader().load(File(bootPath).readAsStringSync()).source;

  final sodium = await initHeadlessSodium();
  final identity = await identityOfPersona(persona);
  final layer = HeadlessGrassrootsNetwork(
    outboundQueueStore: MemoryOutboundQueueStore(),
    identity: identity,
    sodium: sodium,
  );
  final network = HeadlessGlpNetwork(layer);
  if (!await layer.start()) {
    stderr.writeln('the layer bound no socket');
    exit(1);
  }
  stdout.writeln(
      'READY ${layer.boundPort} ${glp.PubKey.fromList(identity.publicKey).hex}');

  AgentRuntime? agent;
  final done = Completer<void>();
  stdin
      .transform(utf8.decoder)
      .transform(const LineSplitter())
      .listen((line) async {
    if (line.startsWith('PEER ')) {
      final parts = line.split(' ');
      final pk = glp.PubKey.fromHex(parts[1]);
      network.putKnownPeer(pk);
      network.putPeerAddress(pk, parts[2]);
    } else if (line.startsWith('GO ')) {
      final go = json.decode(line.substring(3)) as Map<String, dynamic>;
      final person = go['person'] as Map<String, dynamic>?;
      agent = AgentRuntime(
        agentId: persona,
        program: program,
        rootSelfGlpPath: rootSelfGlpPath,
        person: person == null
            ? null
            : PersonGoal(person['functor'] as String,
                (person['args'] as List).cast<String>(),
                person['channelIndex'] as int),
        bootSource: bootSource,
        bootPath: bootPath,
        keyPair: (
          pub: glp.PubKey.fromList(identity.publicKey),
          priv: Uint8List.fromList(identity.privateKey),
        ),
        network: network,
      );
      // The transcript marks each element sent to the person with `< `.
      agent!.onOutput = (l) {
        if (l.startsWith('< ')) stdout.writeln('OUT ${l.substring(2)}');
      };
      await agent!.initialize();
      final act = go['act'] as Map<String, dynamic>?;
      if (act != null) {
        await agent!.injectUserInput(actTerm(act['functor'] as String,
            (act['args'] as List).cast<String>()));
      }
      stdout.writeln('INIT');
    }
  }, onDone: () => done.complete());
  await done.future;
  agent?.dispose();
  await layer.dispose();
  exit(0);
}
