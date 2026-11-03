/// A boot of madGLP agents over the real networking layer: each agent an
/// `AgentRuntime` over its own headless layer (`HeadlessGrassrootsNetwork`)
/// through GNA's adapter (`HeadlessGlpNetwork`), on UDP loopback, every agent
/// named by its key (IGLP 670d36a, Remark "Agent Names": "An agent p is its
/// public key ... and has no other name.  A readable name such as alice or
/// bob ... is a variable standing for a public key"; Implementation Notes,
/// "Networking Seam", One interface: "transport integration is a backend
/// swap").  In one process ([HeadlessBoot]) and in one process per agent
/// ([HeadlessBootProcesses], whose agents are headless_boot_agent.dart).
///
/// Two entry forms, the host's two (`AgentRuntime`):
///
/// - The boot form (IGLP, Implementation Notes, "Boot"): a boot file's
///   directives, `agent(ch(_?,_), ch(_?,_))@p` with the person goal `P@p`,
///   read by `BootLoader` as `:boot` reads them ([HeadlessAgentSpec.ofBootFile]).
///   The agent's key is made at its spawn from the clock, the device and the
///   persona's name, as the simulation makes it (`PersonIdentity.forPersona`).
///   "An agent learns the keys of its peers from its person: the ground
///   arguments of P carry the keys the person knows": over the real layer the
///   person goal's arguments name the peers by their keys, so every readable
///   name among them, a variable standing for a key, is given that agent's
///   key, as a text constant ([personOf]); the simulation routes the names
///   as atoms instead.
/// - The entry form, `name(Me, extras..., NetIn)`, of the pattern test
///   agent_runtime_headless_test.dart: the constant arguments are the spec's
///   texts, or another agent's key where the spec names that agent by
///   [PeerKey].
///
/// Order: every layer starts, so that every port is known; every agent is
/// given every other's key and address; then the agents are initialised in
/// the order given, each one's entry goal running before the next is bound.
library;

import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart' show Ed25519;
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:glp_runtime/multiagent/identity.dart' show PersonIdentity;
import 'package:grassroots_networking_glue/headless_glp_network.dart';
import 'package:grassroots_networking_core/grassroots_networking_core.dart';
import 'package:sodium/sodium_sumo.dart';

/// Another agent's key, by that agent's id, as a constant argument of the
/// entry form.
class PeerKey {
  final String id;
  const PeerKey(this.id);
}

/// One agent of a boot: its id (the persona's name, the host's handle and
/// the tag of its lines); in the boot form its person goal, [person], as the
/// boot file has it; in the entry form its entry goal and the constant
/// arguments between its own name and the network input, each a [String] or
/// a [PeerKey].
class HeadlessAgentSpec {
  final String id;
  final String goalLabel;
  final List<Object> extraArgs;
  final PersonGoal? person;
  const HeadlessAgentSpec(this.id, this.goalLabel,
      {this.extraArgs = const [], this.person});

  bool get bootForm => person != null;

  /// The specs of a boot file's directives and the program directory beside
  /// the file (`<dir>_boot.glp`), as `:boot` gives the simulation: the boot
  /// form, each agent with its person goal; a directive without a person is
  /// refused, the harness having no person of its own to put there.
  static HeadlessBootFile ofBootFile(String bootPath, {String? programDir}) {
    final config = BootLoader().load(File(bootPath).readAsStringSync());
    final dir = programDir ??
        (bootPath.endsWith('_boot.glp')
            ? bootPath.substring(0, bootPath.length - '_boot.glp'.length)
            : throw ArgumentError('$bootPath: no program directory given and '
                'the file is not named <dir>_boot.glp'));
    return HeadlessBootFile(
      [
        for (final d in config.directives)
          HeadlessAgentSpec(d.agentId, 'agent/2',
              person: d.person ??
                  (throw ArgumentError('$bootPath: agent ${d.agentId} has '
                      'no person goal, and the boot over the real layer '
                      'has no person of its own to script'))),
      ],
      dir,
      config.source,
      bootPath,
    );
  }
}

/// A boot file read for the real layer: its agents, the program directory
/// beside it, and its own module (the source with the boot clause stripped)
/// with its path, which every agent loads on top of the program in the
/// file's scope.
class HeadlessBootFile {
  final List<HeadlessAgentSpec> agents;
  final String programDir;
  final String source;
  final String path;
  const HeadlessBootFile(this.agents, this.programDir, this.source, this.path);
}

String hexOf(List<int> bytes) =>
    bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

Uint8List bytesOfHex(String hex) => Uint8List.fromList([
      for (var i = 0; i < hex.length; i += 2)
        int.parse(hex.substring(i, i + 2), radix: 16),
    ]);

/// The key pair of agent [persona], made as the simulation makes it at the
/// agent's spawn (IGLP, Implementation Notes, "Boot": "from a seed drawn
/// from outside the program --- the SHA-256 of the clock, the device's
/// hardware identifiers and the persona's name"), as the layer's identity.
Future<GrassrootsIdentity> identityOfPersona(String persona) async {
  final seed = Uint8List.sublistView(
      PersonIdentity.forPersona(persona).priv, 0, 32);
  return GrassrootsIdentity.create(
      keyPair: await Ed25519().newKeyPairFromSeed(seed));
}

/// The lines an agent sends its person, as they come.  [first] waits for a
/// line on the event of its arrival, with no clock and no polling (IGLP,
/// Implementation Notes, "Event-driven execution": "there is no clock and no
/// polling"), and fails once no further line can come ([end]: the agent's
/// process exited or wrote an error), naming the agent, as IsolateManager's
/// boot fails on an agent's fault or its isolate's exit.  A run that never
/// ends is the suite's watchdog's to stop (test/run_all_tests.sh, Section Q).
class AgentLines {
  final String agentId;
  final List<String> _lines = [];
  final List<(bool Function(String), Completer<String>)> _waiting = [];
  Object? _ended;
  AgentLines(this.agentId);

  /// Every line so far, oldest first.
  List<String> get lines => List.unmodifiable(_lines);

  void add(String line) {
    _lines.add(line);
    for (final w in List.of(_waiting)) {
      if (w.$1(line)) {
        _waiting.remove(w);
        w.$2.complete(line);
      }
    }
  }

  /// No further line will come, for [why]; every wait still pending fails.
  void end(Object why) {
    if (_ended != null) return;
    _ended = why;
    for (final w in _waiting) {
      w.$2.completeError(why);
    }
    _waiting.clear();
  }

  /// The first line that [test] accepts: one already come, or the first to
  /// come that it accepts.  [test] may read [lines], which holds the line it
  /// is given.
  Future<String> first(bool Function(String) test) {
    for (final l in _lines) {
      if (test(l)) return Future.value(l);
    }
    if (_ended != null) return Future.error(_ended!);
    final c = Completer<String>();
    _waiting.add((test, c));
    return c.future;
  }
}

/// The constant arguments of [spec]'s entry form as texts: a [PeerKey] is
/// that agent's key.
List<String> resolveArgs(HeadlessAgentSpec spec, Map<String, String> keyOf) => [
      for (final a in spec.extraArgs)
        a is PeerKey
            ? (keyOf[a.id] ??
                (throw ArgumentError(
                    '${spec.id}: no agent ${a.id} in this boot')))
            : a as String,
    ];

/// The person goal of [spec]'s boot form over the real layer: every agent id
/// of the boot among its ground arguments, a readable name standing for a
/// key, replaced by that agent's key as a quoted text constant, whole words
/// only (`alice` in `[bob, alice]`, not in `alice_init`).
PersonGoal personOf(HeadlessAgentSpec spec, Map<String, String> keyOf) {
  final p = spec.person!;
  final args = [
    for (var i = 0; i < p.args.length; i++)
      i == p.channelIndex ? p.args[i] : _keysForNames(p.args[i], keyOf),
  ];
  return PersonGoal(p.functor, args, p.channelIndex);
}

String _keysForNames(String text, Map<String, String> keyOf) {
  var out = text;
  for (final MapEntry(key: id, value: key) in keyOf.entries) {
    out = out.replaceAllMapped(
        RegExp('(?<![A-Za-z0-9_])${RegExp.escape(id)}(?![A-Za-z0-9_])'),
        (_) => "'$key'");
  }
  return out;
}

/// The sender's key of a message delivered to an agent, where [log] is the
/// line its runtime logs on the delivery, `MAD_RECV from <key> over ...`
/// (AgentRuntime, the network it is given); null for any other line.
String? senderOfReceipt(String log) {
  final m = RegExp(r'^MAD_RECV from ([0-9a-f]+) ').firstMatch(log);
  return m?.group(1);
}

/// A boot in one process.
class HeadlessBoot {
  final List<HeadlessAgentSpec> specs;
  final Map<String, String> keyOf;
  final Map<String, AgentRuntime> hosts;
  final Map<String, HeadlessGrassrootsNetwork> layers;
  final Map<String, AgentLines> outputs;

  /// The key of the sender of each message the layer delivered to each agent,
  /// in order of delivery, as the agent's runtime logs it (`MAD_RECV from`).
  final Map<String, AgentLines> received;

  HeadlessBoot._(this.specs, this.keyOf, this.hosts, this.layers, this.outputs,
      this.received);

  /// A boot file's agents over its program directory.
  static Future<HeadlessBoot> ofBootFile(HeadlessBootFile f,
          {required String rootSelfGlpPath,
          required SodiumSumo sodium,
          required String queueDir,
          void Function(String agentId, String line)? onOutput,
          void Function(String agentId, String tag, String message)?
              onLog}) =>
      start(
          agents: f.agents,
          program: f.programDir,
          rootSelfGlpPath: rootSelfGlpPath,
          sodium: sodium,
          queueDir: queueDir,
          bootFile: f,
          onOutput: onOutput,
          onLog: onLog);

  static Future<HeadlessBoot> start({
    required List<HeadlessAgentSpec> agents,
    required String program,
    required String rootSelfGlpPath,
    required SodiumSumo sodium,
    required String queueDir,
    HeadlessBootFile? bootFile,
    void Function(String agentId, String line)? onOutput,
    void Function(String agentId, String tag, String message)? onLog,
  }) async {
    final ids = <String, GrassrootsIdentity>{};
    final keyOf = <String, String>{};
    for (final spec in agents) {
      final id = await identityOfPersona(spec.id);
      ids[spec.id] = id;
      keyOf[spec.id] = hexOf(id.publicKey);
    }
    final layers = <String, HeadlessGrassrootsNetwork>{};
    final nets = <String, HeadlessGlpNetwork>{};
    final hosts = <String, AgentRuntime>{};
    final outputs = <String, AgentLines>{};
    final received = <String, AgentLines>{};
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
        extraArgs: spec.bootForm ? const [] : resolveArgs(spec, keyOf),
        person: spec.bootForm ? personOf(spec, keyOf) : null,
        bootSource: bootFile?.source,
        bootPath: bootFile?.path,
        keyPair: (
          pub: glp.PubKey.fromList(identity.publicKey),
          priv: Uint8List.fromList(identity.privateKey),
        ),
        network: net,
      );
      final lines = outputs[spec.id] = AgentLines(spec.id);
      host.onOutput = (line) {
        lines.add(line);
        onOutput?.call(spec.id, line);
      };
      final from = received[spec.id] = AgentLines(spec.id);
      host.onLog = (tag, message) {
        final sender = senderOfReceipt(message);
        if (sender != null) from.add(sender);
        onLog?.call(spec.id, tag, message);
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
    return HeadlessBoot._(agents, keyOf, hosts, layers, outputs, received);
  }

  /// The first line agent [id] sent its person that [test] accepts.
  Future<String> line(String id, bool Function(String) test) =>
      outputs[id]!.first(test);

  Future<void> dispose() async {
    for (final h in hosts.values) {
      h.dispose();
    }
    for (final l in layers.values) {
      await l.dispose();
    }
  }
}

/// The agent program of [HeadlessBootProcesses], headless_boot_agent.dart,
/// compiled once to a kernel file before any agent process starts, which
/// every agent process then runs.  So no agent process builds the package:
/// `dart run` in each process built the package's native assets in its one
/// `.dart_tool` at once, and on macOS the processes raced on
/// `objective_c.dylib` and failed (`Failed to codesign dylib`, `Failed to set
/// install names`; Section QF on main 8e4e185a, 2026-11-03).
class HeadlessAgentProgram {
  /// The kernel file.
  final String path;
  final Directory _dir;
  HeadlessAgentProgram._(this.path, this._dir);

  /// [helper], a path relative to the package root, which is the working
  /// directory of `flutter test`, compiled to a kernel file in a directory
  /// of its own.
  static Future<HeadlessAgentProgram> compile(
      {String helper = 'test/helpers/headless_boot_agent.dart'}) async {
    final dir = Directory.systemTemp.createTempSync('glp_headless_agent_');
    final out = '${dir.path}/headless_boot_agent.dill';
    final r = await Process.run(
        dartExecutable(), ['compile', 'kernel', helper, '-o', out]);
    if (r.exitCode != 0 || !File(out).existsSync()) {
      dir.deleteSync(recursive: true);
      throw StateError('$helper did not compile: exit ${r.exitCode}: '
          '${r.stdout}${r.stderr}');
    }
    return HeadlessAgentProgram._(out, dir);
  }

  void dispose() => _dir.deleteSync(recursive: true);
}

/// A boot in one process per agent: each agent is headless_boot_agent.dart,
/// compiled once ([HeadlessAgentProgram]) and run by Dart; this process gives
/// each the others' keys and addresses, its goal's arguments with the peers'
/// keys in them, and the order to start, and reads their lines.
///
/// Every wait is on an event, with no clock (IGLP, Implementation Notes,
/// "Event-driven execution"): the boot waits for each agent's `READY` and
/// then, in the order given, its `INIT`, and fails, naming the agent, when
/// its process exits or writes an error before it, as IsolateManager's boot
/// fails on an agent's fault or its isolate's exit before Ready; after the
/// boot, a wait for a line fails likewise ([AgentLines]).
class HeadlessBootProcesses {
  final List<HeadlessAgentSpec> specs;
  final Map<String, String> keyOf;
  final Map<String, Process> processes;
  final Map<String, AgentLines> outputs;
  final Map<String, List<String>> transcripts;

  /// The key of the sender of each message the layer delivered to each agent,
  /// in order of delivery, as the agent's process prints it (`RECV <key>`).
  final Map<String, AgentLines> received;
  bool _disposing = false;

  HeadlessBootProcesses._(this.specs, this.keyOf, this.processes, this.outputs,
      this.transcripts, this.received);

  /// A boot file's agents over its program directory, one process each.
  static Future<HeadlessBootProcesses> ofBootFile(HeadlessBootFile f,
          {required String rootSelfGlpPath,
          required HeadlessAgentProgram agent,
          void Function(String agentId, String line)? onOutput}) =>
      start(
          agents: f.agents,
          program: f.programDir,
          rootSelfGlpPath: rootSelfGlpPath,
          agent: agent,
          bootFile: f,
          onOutput: onOutput);

  static Future<HeadlessBootProcesses> start({
    required List<HeadlessAgentSpec> agents,
    required String program,
    required String rootSelfGlpPath,
    required HeadlessAgentProgram agent,
    HeadlessBootFile? bootFile,
    void Function(String agentId, String line)? onOutput,
  }) async {
    final keyOf = <String, String>{};
    final processes = <String, Process>{};
    final outputs = <String, AgentLines>{};
    final transcripts = <String, List<String>>{};
    final received = <String, AgentLines>{};
    final ports = <String, String>{};
    final ready = <String, Completer<void>>{};
    final inited = <String, Completer<void>>{};
    late final HeadlessBootProcesses boot;
    var booted = false;

    // Why [id] can go no further: its process exited, or wrote an error.
    void fault(String id, String what) {
      final when = !ready[id]!.isCompleted
          ? ' before it was ready'
          : !inited[id]!.isCompleted
              ? ' before it started'
              : '';
      final why =
          StateError('$id $what$when: ${transcripts[id]!.join(' | ')}');
      for (final c in [ready[id]!, inited[id]!]) {
        if (!c.isCompleted) c.completeError(why);
      }
      if (!(booted && boot._disposing)) {
        outputs[id]!.end(why);
        received[id]!.end(why);
      }
    }

    try {
      for (final spec in agents) {
        final id = spec.id;
        final lines = outputs[id] = AgentLines(id);
        final from = received[id] = AgentLines(id);
        final all = transcripts[id] = <String>[];
        // A failure can come while another agent is awaited; marked handled
        // here, it reaches its await and not the zone as an uncaught error.
        ready[id] = Completer<void>()..future.ignore();
        inited[id] = Completer<void>()..future.ignore();
        final p = await Process.start(dartExecutable(), [
          agent.path,
          id,
          rootSelfGlpPath,
          program,
          spec.goalLabel,
          if (bootFile != null) bootFile.path,
        ]);
        processes[id] = p;
        // A write to a process that has exited fails here; the exit itself is
        // the fault, reported below.
        p.stdin.done.ignore();
        p.stdout
            .transform(utf8.decoder)
            .transform(const LineSplitter())
            .listen((l) {
          all.add(l);
          if (l.startsWith('READY ')) {
            final parts = l.split(' ');
            ports[id] = parts[1];
            keyOf[id] = parts[2];
            if (!ready[id]!.isCompleted) ready[id]!.complete();
          } else if (l == 'INIT') {
            if (!inited[id]!.isCompleted) inited[id]!.complete();
          } else if (l.startsWith('OUT ')) {
            final line = l.substring(4);
            lines.add(line);
            onOutput?.call(id, line);
          } else if (l.startsWith('RECV ')) {
            from.add(l.substring(5));
          }
        });
        p.stderr.transform(utf8.decoder).listen((s) {
          all.add('stderr: $s');
          fault(id, 'wrote an error');
        });
        p.exitCode.then((code) => fault(id, 'exited with code $code'));
      }
      await Future.wait([for (final spec in agents) ready[spec.id]!.future],
          eagerError: true);
      for (final spec in agents) {
        for (final other in agents) {
          if (other.id == spec.id) continue;
          processes[spec.id]!
              .stdin
              .writeln('PEER ${keyOf[other.id]} 127.0.0.1:${ports[other.id]}');
        }
      }
      for (final spec in agents) {
        final person = spec.bootForm ? personOf(spec, keyOf) : null;
        processes[spec.id]!.stdin.writeln('GO ${json.encode({
              'extraArgs': spec.bootForm ? const [] : resolveArgs(spec, keyOf),
              'person': person == null
                  ? null
                  : {
                      'functor': person.functor,
                      'args': person.args,
                      'channelIndex': person.channelIndex,
                    },
            })}');
        await inited[spec.id]!.future;
      }
    } catch (_) {
      for (final p in processes.values) {
        p.kill();
      }
      rethrow;
    }
    boot = HeadlessBootProcesses._(
        agents, keyOf, processes, outputs, transcripts, received);
    booted = true;
    return boot;
  }

  Future<String> line(String id, bool Function(String) test) =>
      outputs[id]!.first(test);

  /// Each agent ends when its standard input closes.
  Future<void> dispose() async {
    _disposing = true;
    for (final p in processes.values) {
      p.stdin.close().ignore();
      await p.exitCode;
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
