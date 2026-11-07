/// The rendezvous server: a host running the headless layer and the GLP
/// rendezvous agent over it (GNA Section 6, "Rendezvous Server": "a separate
/// process---a host that provides a static, globally-routable IP address and
/// runs a lightweight GLP rendezvous agent").
///
/// The layer is [HeadlessGrassrootsNetwork], instantiated with the parameters
/// of GNA Section 2.2 a rendezvous server has: its key pair, its server
/// address --- the listen port and the static public `ip:port` --- and the
/// mark of a rendezvous server, so that it answers unsolicited WAN contact
/// from any key (GNA Section 2.7); it runs the WAN alone, with no
/// attestation, no location service and no rendezvous servers of its own.
/// The agent is `programs/examples/rendezvous/rv_agent.glp`, booted on its
/// entry `rv_server/3` by the runtime's own host, `AgentRuntime`, over the
/// layer through the adapter, [HeadlessGlpNetwork], as the glue's tests host
/// theirs.  What the agent coordinates and how is the agent's GLP (GNA
/// Section 6); the layer supplies the observed addresses behind
/// `peer_address/2`.
///
/// A library of the glue package because the adapter is: the entrypoint joins
/// the layer's core to the runtime, which is what the glue package is for
/// (GNA, 2026-10-11), and its imports reach neither Flutter nor the Flutter
/// package, so it compiles to a native executable.
///
/// Compile, from `networking/glue` after `flutter pub get`:
///   dart compile exe bin/rendezvous_server.dart -o rendezvous_server
///
/// Run:
///   rendezvous_server --identity rv-identity.json --port 9516 \
///     --public-address 203.0.113.7:9516 --outbound-queue-file rv-queue.json \
///     --compatible 7:<hash> --compatible 8:<hash> \
///     --program <GLP>/programs/examples/rendezvous/rv_agent.glp \
///     --root-self <GLP>/programs/self.glp
///
/// `--identity` is the key pair, a JSON file `{"seed": <hex>}` created with a
/// fresh seed on first run; `--outbound-queue-file` the file that keeps the
/// layer's fair-delivery queue, so that it survives the termination of the
/// server (GNA Section 2.11); `--compatible <version>:<source identity>`, given
/// once for each compatible version of the Grassroots App and at least once,
/// names the version number, a whole number below 2^63 (GNA Implementation
/// Notes), and the source identity of that version's social-graph program,
/// 64 lowercase hexadecimal digits, the Hash `signature/2` reports for a
/// signature made under it (`glp_runtime/bin/source_identity.dart` prints
/// it).  The agent is told them, as the list of `version(V, H)` in the order
/// given, and admits a signed term made under any of them and no other
/// (GNA Section 6, "Versions": "the rendezvous agent is told, when it starts,
/// the compatible versions with the source identities of their social-graph
/// programs, and admits a signed term by any of them"; rv_agent.glp,
/// "Admission").
///
/// Prints `READY <port> <key-hex>` once listening, then `OUT <term>` for each
/// element the agent sends its person and `CONNECTED <key-hex>` and
/// `DISCONNECTED <key-hex>` for each session the layer reports; ends on
/// SIGINT or SIGTERM.
library;

import 'dart:async';
import 'dart:io';
import 'dart:typed_data';

import 'package:args/args.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:grassroots_networking_core/grassroots_networking_core.dart';
import 'package:grassroots_networking_core/src/headless/identity_file.dart';
import 'package:grassroots_networking_glue/headless_glp_network.dart';

Future<void> main(List<String> argv) async {
  final parser = ArgParser()
    ..addOption('identity',
        mandatory: true,
        help: 'The key pair: a JSON identity file, created on first run')
    ..addOption('port', mandatory: true, help: 'The UDP port to listen on')
    ..addOption('public-address',
        mandatory: true,
        help: "The server's static public ip:port; with --port, the server "
            'address of GNA Section 2.2')
    ..addOption('outbound-queue-file',
        mandatory: true,
        help: "The layer's fair-delivery queue: a JSON file, so that the "
            'queue survives the termination of the server (GNA Section 2.11)')
    ..addMultiOption('compatible',
        splitCommas: false,
        valueHelp: 'version:source-identity',
        help: 'A compatible version of the Grassroots App and the source '
            "identity of its social-graph program, under which the agent "
            'admits a signed term; once for each, at least once (GNA Section '
            '6, "Versions")')
    ..addOption('program',
        mandatory: true,
        help: 'The rendezvous agent: programs/examples/rendezvous/rv_agent.glp')
    ..addOption('root-self',
        mandatory: true, help: "The root self.glp: programs/self.glp")
    ..addFlag('help', abbr: 'h', negatable: false);

  final ArgResults args;
  try {
    args = parser.parse(argv);
  } on FormatException catch (e) {
    stderr.writeln(e.message);
    stderr.writeln(parser.usage);
    exit(64);
  }
  if (args['help'] as bool) {
    stdout.writeln(parser.usage);
    return;
  }
  final port = int.tryParse(args['port'] as String);
  if (port == null) {
    stderr.writeln('--port: not a port: ${args['port']}');
    exit(64);
  }

  final versions = <rt.Term>[];
  for (final c in args['compatible'] as List<String>) {
    final version = _compatibleVersion(c);
    if (version == null) {
      stderr.writeln('--compatible: not <version>:<source identity>, the '
          'version a whole number below 2^63 and the source identity 64 '
          'lowercase hexadecimal digits: $c');
      exit(64);
    }
    versions.add(version);
  }
  if (versions.isEmpty) {
    stderr.writeln('--compatible: at least one compatible version is needed');
    stderr.writeln(parser.usage);
    exit(64);
  }

  final sodium = await initHeadlessSodium();
  final identity = await loadOrCreateIdentity(args['identity'] as String);
  final layer = HeadlessGrassrootsNetwork(
    identity: identity,
    serverAddress: ServerAddress(
      listenPort: port,
      publicAddress: args['public-address'] as String,
    ),
    rendezvousServer: true,
    sodium: sodium,
    outboundQueueStore:
        FileOutboundQueueStore(args['outbound-queue-file'] as String),
  );
  final network = HeadlessGlpNetwork(layer);
  final key = glp.PubKey.fromList(identity.publicKey);

  final agent = AgentRuntime(
    agentId: 'rendezvous',
    program: args['program'] as String,
    rootSelfGlpPath: args['root-self'] as String,
    keyPair: (pub: key, priv: Uint8List.fromList(identity.privateKey)),
    network: network,
  );
  // The transcript marks each element sent to the person with `< `.
  agent.onOutput = (line) {
    if (line.startsWith('< ')) stdout.writeln('OUT ${line.substring(2)}');
  };
  agent.onPeerConnected =
      (pk, underlay, _) => stdout.writeln('CONNECTED ${pk.hex}');
  agent.onPeerDisconnected =
      (pk, underlay) => stdout.writeln('DISCONNECTED ${pk.hex}');

  // The agent is bound to its layer before the layer starts, so that no
  // message the layer delivers finds no agent.
  await agent.initialize();
  // The agent is started from agent(ch(_?,_),ch(_?,_)), every agent's initial
  // goal (IGLP, Implementation Notes, "Boot": "a deployed agent is spawned by
  // its application with the same initial goal"); its person's first act is
  // the list of the compatible versions, under whose social-graph programs it
  // admits a signed term.
  await agent.injectUserInput(_list(versions));
  if (!await layer.start()) {
    stderr.writeln('could not bind UDP port $port');
    exit(1);
  }
  stdout.writeln('READY ${layer.boundPort} ${key.hex}');

  final done = Completer<void>();
  void shutDown(ProcessSignal _) {
    if (!done.isCompleted) done.complete();
  }

  final subscriptions = [
    ProcessSignal.sigint.watch().listen(shutDown),
    ProcessSignal.sigterm.watch().listen(shutDown),
  ];
  await done.future;
  for (final s in subscriptions) {
    await s.cancel();
  }
  agent.dispose();
  await layer.dispose();
  exit(0);
}

/// `version(V, H)` of `--compatible V:H`, or null where it is not that form.
rt.Term? _compatibleVersion(String value) {
  final m = RegExp(r'^(\d+):([0-9a-f]{64})$').firstMatch(value);
  if (m == null) return null;
  final version = int.tryParse(m.group(1)!);
  if (version == null) return null;
  return rt.StructTerm(
      'version', [rt.ConstTerm(version), rt.ConstTerm(m.group(2)!)]);
}

/// The GLP list of [items], in their order.
rt.Term _list(List<rt.Term> items) {
  rt.Term list = rt.ConstTerm(rt.nil);
  for (final t in items.reversed) {
    list = rt.StructTerm('.', [t, list]);
  }
  return list;
}
