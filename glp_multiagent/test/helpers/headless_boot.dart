/// A boot of madGLP agents over the real networking layer: each agent an
/// `AgentRuntime` over its own headless layer (`HeadlessGrassrootsNetwork`)
/// through GNA's adapter (`HeadlessGlpNetwork`), on UDP loopback, every agent
/// named by its key (IGLP, Remark "Agent Names": "Over a real network, the
/// name of an agent p is its public key"; Implementation Notes, "Networking
/// Seam", One interface: "transport integration is a backend swap").  The
/// pattern of agent_runtime_headless_test.dart (two agents, one program file)
/// over N agents and a program directory, in one process ([HeadlessBoot]) and
/// in one process per agent ([HeadlessBootProcesses], whose agents are
/// headless_boot_agent.dart).
///
/// What an agent is given is what the simulation's boot gives it (IGLP, "Boot";
/// glp_runtime/lib/multiagent/boot_loader.dart): its entry goal, the constant
/// arguments between its own name and the network input, and its program.
/// Here its own name is its key, and a constant argument is the text the
/// spec gives, or another agent's key where the spec names that agent by
/// [PeerKey] --- the test's wiring, as the pattern passes the callee's key
/// to the caller.  No directory from a symbolic name to a key exists over the
/// real layer: the Remark gives symbolic names to the simulation alone.
///
/// Order: every layer starts, so that every port is known; every agent is
/// given every other's key and address; then the agents are initialised in
/// the order given, each one's entry goal running before the next is bound.
/// A message to an agent not yet bound finds no host and is kept by the
/// sender's layer for redelivery, which is the layer's fair delivery
/// (Implementation Notes, "The contract"); an order in which the agents that
/// wait come first, as the callee before its callers, needs none.
library;

import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart' show Ed25519;
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:grassroots_networking_glue/headless_glp_network.dart';
import 'package:grassroots_networking_core/grassroots_networking_core.dart';
import 'package:sodium/sodium_sumo.dart';

/// Another agent's key, by that agent's id, as a constant argument.
class PeerKey {
  final String id;
  const PeerKey(this.id);
}

/// One agent of a boot: its id (the host's handle and the tag of its lines),
/// its entry goal, and the constant arguments between its own name and the
/// network input, each a [String] or a [PeerKey].
class HeadlessAgentSpec {
  final String id;
  final String goalLabel;
  final List<Object> extraArgs;
  const HeadlessAgentSpec(this.id, this.goalLabel,
      {this.extraArgs = const []});

  /// The specs of a boot file's directives, their constant arguments as the
  /// file has them, and the program directory beside the file: what `:boot`
  /// gives the simulation.
  static (List<HeadlessAgentSpec>, String) ofBootFile(String bootPath,
      {String? programDir}) {
    final config = BootLoader().load(File(bootPath).readAsStringSync());
    final dir = programDir ??
        (bootPath.endsWith('_boot.glp')
            ? bootPath.substring(0, bootPath.length - '_boot.glp'.length)
            : throw ArgumentError('$bootPath: no program directory given and '
                'the file is not named <dir>_boot.glp'));
    return (
      [
        for (final d in config.directives)
          HeadlessAgentSpec(d.agentId, '${d.goalFunctor}/${d.goalArity}',
              extraArgs: d.constantArgs),
      ],
      dir,
    );
  }
}

/// The 32-byte seed of agent number [index] of a boot: deterministic, so a
/// run's keys are the same run after run and across the processes of one.
Uint8List seedOf(int index) =>
    Uint8List.fromList(List.filled(32, 0x11 + index));

Future<GrassrootsIdentity> identityOfSeed(Uint8List seed) async =>
    GrassrootsIdentity.create(
        keyPair: await Ed25519().newKeyPairFromSeed(seed));

String hexOf(List<int> bytes) =>
    bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

Uint8List bytesOfHex(String hex) => Uint8List.fromList([
      for (var i = 0; i < hex.length; i += 2)
        int.parse(hex.substring(i, i + 2), radix: 16),
    ]);

/// The first line of [lines] that [test] accepts, waiting up to [wait] for it.
Future<String> lineOf(List<String> lines, bool Function(String) test,
    {Duration wait = const Duration(seconds: 90)}) async {
  final deadline = DateTime.now().add(wait);
  while (true) {
    for (final l in lines) {
      if (test(l)) return l;
    }
    if (DateTime.now().isAfter(deadline)) {
      throw TimeoutException('no line came: ${lines.join(' | ')}');
    }
    await Future<void>.delayed(const Duration(milliseconds: 50));
  }
}

/// The constant arguments of [spec] as texts: a [PeerKey] is that agent's key.
List<String> resolveArgs(HeadlessAgentSpec spec, Map<String, String> keyOf) => [
      for (final a in spec.extraArgs)
        a is PeerKey
            ? (keyOf[a.id] ??
                (throw ArgumentError(
                    '${spec.id}: no agent ${a.id} in this boot')))
            : a as String,
    ];

/// A boot in one process.
class HeadlessBoot {
  final List<HeadlessAgentSpec> specs;
  final Map<String, String> keyOf;
  final Map<String, AgentRuntime> hosts;
  final Map<String, HeadlessGrassrootsNetwork> layers;
  final Map<String, List<String>> outputs;

  HeadlessBoot._(this.specs, this.keyOf, this.hosts, this.layers, this.outputs);

  static Future<HeadlessBoot> start({
    required List<HeadlessAgentSpec> agents,
    required String program,
    required String rootSelfGlpPath,
    required SodiumSumo sodium,
    required String queueDir,
    void Function(String agentId, String line)? onOutput,
  }) async {
    final ids = <String, GrassrootsIdentity>{};
    final keyOf = <String, String>{};
    for (var i = 0; i < agents.length; i++) {
      final id = await identityOfSeed(seedOf(i));
      ids[agents[i].id] = id;
      keyOf[agents[i].id] = hexOf(id.publicKey);
    }
    final layers = <String, HeadlessGrassrootsNetwork>{};
    final nets = <String, HeadlessGlpNetwork>{};
    final hosts = <String, AgentRuntime>{};
    final outputs = <String, List<String>>{};
    for (final spec in agents) {
      final identity = ids[spec.id]!;
      final layer = HeadlessGrassrootsNetwork(
        identity: identity,
        sodium: sodium,
        outboundQueueStore:
            FileOutboundQueueStore('$queueDir/${spec.id}.json'),
      );
      final net = HeadlessGlpNetwork(layer);
      for (final other in agents) {
        if (other.id != spec.id) {
          net.putKnownPeer(glp.PubKey.fromList(ids[other.id]!.publicKey));
        }
      }
      final host = AgentRuntime(
        agentId: spec.id,
        program: program,
        rootSelfGlpPath: rootSelfGlpPath,
        goalLabel: spec.goalLabel,
        extraArgs: resolveArgs(spec, keyOf),
        keyPair: (
          pub: glp.PubKey.fromList(identity.publicKey),
          priv: Uint8List.fromList(identity.privateKey),
        ),
        network: net,
      );
      final lines = outputs[spec.id] = <String>[];
      host.onOutput = (line) {
        lines.add(line);
        onOutput?.call(spec.id, line);
      };
      layers[spec.id] = layer;
      nets[spec.id] = net;
      hosts[spec.id] = host;
    }
    // Every layer up, every address known to every other, then the agents.
    for (final spec in agents) {
      if (!await layers[spec.id]!.start()) {
        throw StateError('${spec.id}: the layer bound no socket');
      }
    }
    for (final spec in agents) {
      for (final other in agents) {
        if (other.id == spec.id) continue;
        nets[spec.id]!.putPeerAddress(
            glp.PubKey.fromList(ids[other.id]!.publicKey),
            '127.0.0.1:${layers[other.id]!.boundPort}');
      }
    }
    for (final spec in agents) {
      await hosts[spec.id]!.initialize();
    }
    return HeadlessBoot._(agents, keyOf, hosts, layers, outputs);
  }

  /// The first line agent [id] sent its person that [test] accepts.
  Future<String> line(String id, bool Function(String) test) =>
      lineOf(outputs[id]!, test);

  Future<void> dispose() async {
    for (final h in hosts.values) {
      h.dispose();
    }
    for (final l in layers.values) {
      await l.dispose();
    }
  }
}

/// A boot in one process per agent: each agent is headless_boot_agent.dart,
/// run by Dart; this process gives each the others' keys and addresses and
/// the order to start, and reads their lines.
class HeadlessBootProcesses {
  final List<HeadlessAgentSpec> specs;
  final Map<String, String> keyOf;
  final Map<String, Process> processes;
  final Map<String, List<String>> outputs;
  final Map<String, List<String>> transcripts;

  HeadlessBootProcesses._(
      this.specs, this.keyOf, this.processes, this.outputs, this.transcripts);

  static Future<HeadlessBootProcesses> start({
    required List<HeadlessAgentSpec> agents,
    required String program,
    required String rootSelfGlpPath,
    required String helper,
    void Function(String agentId, String line)? onOutput,
  }) async {
    final keyOf = <String, String>{};
    for (var i = 0; i < agents.length; i++) {
      keyOf[agents[i].id] = hexOf((await identityOfSeed(seedOf(i))).publicKey);
    }
    final processes = <String, Process>{};
    final outputs = <String, List<String>>{};
    final transcripts = <String, List<String>>{};
    final ports = <String, String>{};
    final ready = <String, Completer<void>>{};
    final inited = <String, Completer<void>>{};
    for (var i = 0; i < agents.length; i++) {
      final spec = agents[i];
      final peers = [
        for (final other in agents)
          if (other.id != spec.id) keyOf[other.id]!,
      ];
      final p = await Process.start(dartExecutable(), [
        'run',
        helper,
        hexOf(seedOf(i)),
        rootSelfGlpPath,
        program,
        spec.goalLabel,
        json.encode(resolveArgs(spec, keyOf)),
        peers.join(','),
      ]);
      processes[spec.id] = p;
      final lines = outputs[spec.id] = <String>[];
      final all = transcripts[spec.id] = <String>[];
      ready[spec.id] = Completer<void>();
      inited[spec.id] = Completer<void>();
      p.stdout
          .transform(utf8.decoder)
          .transform(const LineSplitter())
          .listen((l) {
        all.add(l);
        if (l.startsWith('READY ')) {
          final parts = l.split(' ');
          ports[spec.id] = parts[1];
          if (parts[2] != keyOf[spec.id]) {
            throw StateError('${spec.id} is named ${parts[2]}, not its key');
          }
          ready[spec.id]!.complete();
        } else if (l == 'INIT') {
          inited[spec.id]!.complete();
        } else if (l.startsWith('OUT ')) {
          final line = l.substring(4);
          lines.add(line);
          onOutput?.call(spec.id, line);
        }
      });
      p.stderr.transform(utf8.decoder).listen((s) => all.add('stderr: $s'));
    }
    final waiting = const Duration(seconds: 90);
    for (final spec in agents) {
      await ready[spec.id]!.future.timeout(waiting,
          onTimeout: () => throw TimeoutException(
              '${spec.id} was not ready: ${transcripts[spec.id]!.join(' | ')}'));
    }
    for (final spec in agents) {
      for (final other in agents) {
        if (other.id == spec.id) continue;
        processes[spec.id]!
            .stdin
            .writeln('PEER ${keyOf[other.id]} 127.0.0.1:${ports[other.id]}');
      }
    }
    for (final spec in agents) {
      processes[spec.id]!.stdin.writeln('GO');
      await inited[spec.id]!.future.timeout(waiting,
          onTimeout: () => throw TimeoutException(
              '${spec.id} did not start: ${transcripts[spec.id]!.join(' | ')}'));
    }
    return HeadlessBootProcesses._(agents, keyOf, processes, outputs, transcripts);
  }

  Future<String> line(String id, bool Function(String) test) =>
      lineOf(outputs[id]!, test);

  Future<void> dispose() async {
    for (final p in processes.values) {
      await p.stdin.close();
      await p.exitCode.timeout(const Duration(seconds: 20), onTimeout: () {
        p.kill();
        return -1;
      });
    }
  }
}

/// The Dart that runs an agent's process: under `flutter test` the executable
/// is flutter_tester's, not Dart's, so the Flutter SDK's own Dart is used,
/// and failing that the Dart on the path.
String dartExecutable() {
  final root = Platform.environment['FLUTTER_ROOT'];
  if (root != null) {
    final dart = File('$root/bin/dart');
    if (dart.existsSync()) return dart.path;
  }
  return 'dart';
}
