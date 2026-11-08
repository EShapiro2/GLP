// The Grassroots Directory Service (GNA Section 7): the page, the lookups and
// the verification links, and the directory's agent on the headless layer
// under the directory's own key, over one store of listings and links.
//
// The web server takes its settings from the environment: HOST (127.0.0.1),
// PORT (8090), BASE_URL (http://HOST:PORT), DATA (directory.json) and the
// SMTP relay's (mail.dart).  The agent takes its own from the command line,
// as the rendezvous server does (networking/glue/bin/rendezvous_server.dart):
//
//   directory_server --identity gds-identity.json --port 9518 \
//     --public-address 203.0.113.7:9518 --outbound-queue-file gds-queue.json \
//     --state-file gds-state \
//     --program <GLP>/programs/examples/rendezvous/gds_agent.glp \
//     --root-self <GLP>/programs/self.glp \
//     [--rendezvous <key-hex>@<ip:port> ...]
//
// --identity is the key pair, a JSON file {"seed": <hex>} created with a
// fresh seed on first run; --state-file where the agent's local state is kept
// across a restart, the files <state-file>, .key and .program (IGLP,
// Implementation Notes, "Persistence"); --rendezvous a rendezvous server the layer
// declares, by its key and address, as many as given.
//
// Prints `READY <udp-port> <key-hex> <base>/directory` once both listen, the
// runtime's lines for the agent prefixed `AGENT `, and ends on SIGINT or
// SIGTERM.
import 'dart:async';
import 'dart:io';
import 'dart:typed_data';

import 'package:args/args.dart';
import 'package:directory_server/agent.dart';
import 'package:directory_server/mail.dart';
import 'package:directory_server/server.dart';
import 'package:directory_server/store.dart';
import 'package:glp_runtime/multiagent/agent_state_store.dart';
import 'package:grassroots_networking_core/grassroots_networking_core.dart';
import 'package:grassroots_networking_core/src/headless/identity_file.dart';

Future<void> main(List<String> argv) async {
  final parser = ArgParser()
    ..addOption('identity', mandatory: true, help: "The directory's key pair: a JSON identity file, created on first run")
    ..addOption('port', mandatory: true, help: 'The UDP port the layer listens on')
    ..addOption('public-address', mandatory: true, help: "The directory's static public ip:port; with --port, its server address (GNA Section 2.2)")
    ..addOption('outbound-queue-file', mandatory: true, help: "The layer's fair-delivery queue, a JSON file (GNA Section 2.11)")
    ..addOption('state-file', mandatory: true, help: "Where the agent's local state is kept across a restart (IGLP, Definition Restart)")
    ..addOption('program', mandatory: true, help: "The directory's agent: programs/examples/rendezvous/gds_agent.glp")
    ..addOption('root-self', mandatory: true, help: 'The root self.glp: programs/self.glp')
    ..addMultiOption('rendezvous', help: 'A rendezvous server the layer declares: <key-hex>@<ip:port>')
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
  final udp = int.tryParse(args['port'] as String);
  if (udp == null) {
    stderr.writeln('--port: not a port: ${args['port']}');
    exit(64);
  }
  final rendezvous = <RendezvousServerEntry>[];
  for (final r in args['rendezvous'] as List<String>) {
    final m = RegExp(r'^([0-9a-fA-F]{64})@(.+)$').firstMatch(r);
    if (m == null) {
      stderr.writeln('--rendezvous: not <key-hex>@<ip:port>: $r');
      exit(64);
    }
    final hex = m.group(1)!;
    rendezvous.add((
      key: Uint8List.fromList([for (var i = 0; i < 64; i += 2) int.parse(hex.substring(i, i + 2), radix: 16)]),
      address: m.group(2)!,
    ));
  }

  final env = Platform.environment;
  final host = env['HOST'] ?? '127.0.0.1';
  final port = int.tryParse(env['PORT'] ?? '') ?? 8090;
  final base = env['BASE_URL'] ?? 'http://$host:$port';
  final smtp = SmtpConfig.fromEnvironment();
  final service = DirectoryService(Store(file: File(env['DATA'] ?? 'directory.json')),
      smtp == null ? LogMailer() : SmtpMailer(smtp), base);

  final agent = await DirectoryAgent.start(
    service: service,
    identity: await loadOrCreateIdentity(args['identity'] as String),
    port: udp,
    publicAddress: args['public-address'] as String,
    queue: FileOutboundQueueStore(args['outbound-queue-file'] as String),
    state: FileAgentStateStore(args['state-file'] as String),
    program: args['program'] as String,
    rootSelf: args['root-self'] as String,
    rendezvous: rendezvous,
  );
  if (agent == null) {
    stderr.writeln('could not bind UDP port $udp');
    exit(1);
  }
  agent.onLine = (line) => stdout.writeln('AGENT $line');

  final server = await HttpServer.bind(host, port);
  service.onLog = stdout.writeln;
  server.listen(service.handle);
  // Expired links dropped, and buffers below their floor asked for again.
  final sweep = Timer.periodic(const Duration(minutes: 1), (_) => service.store.sweep());
  stdout.writeln('directory at $base/directory, listening on $host:$port, mail ${smtp == null ? 'logged, not sent' : 'through ${smtp.host}:${smtp.port}'}');
  stdout.writeln('READY ${agent.boundPort} ${agent.key.hex} $base/directory');

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
  sweep.cancel();
  await server.close(force: true);
  await agent.dispose();
  exit(0);
}
