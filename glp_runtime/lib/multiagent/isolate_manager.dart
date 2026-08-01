/// Isolate Manager for madGLP
///
/// Spawns agent isolates based on BootConfig and routes messages between them.
/// Execution is event-driven: agents drain+flush on Start and on each incoming
/// NetworkMsg. There is no tick loop or external clock.
///
/// Termination is external: the caller shuts down isolates when done.
///
/// See: docs/ma/agent-runtime-spec.md

import 'dart:async';
import 'dart:isolate';
import 'dart:typed_data';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/engine_v2/interp.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/multiagent/payload_serializer.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/simulation_network.dart';

/// Message types for inter-isolate communication
sealed class IsolateMessage {}

/// Agent is ready, provides its SendPort
class Ready extends IsolateMessage {
  final String agentId;
  final SendPort sendPort;
  Ready(this.agentId, this.sendPort);
}

/// Signal to start execution
class Start extends IsolateMessage {}

/// Agent → router: an outbound send. Per seam spec v0.2 §4/§6 the wire carries
/// opaque payload bytes only — no MessageType.
class RouterSend extends IsolateMessage {
  final String fromId;
  final String toId;
  final List<int> payload;

  RouterSend(this.fromId, this.toId, this.payload);

  @override
  String toString() => 'RouterSend($fromId->$toId, ${payload.length}B)';
}

/// Router → agent: a delivered message, with the authenticated sender id and the
/// router-assigned messageId.
class Deliver extends IsolateMessage {
  final String fromId;
  final List<int> payload;
  final String messageId;

  Deliver(this.fromId, this.payload, this.messageId);

  @override
  String toString() => 'Deliver(from=$fromId, id=$messageId, ${payload.length}B)';
}

/// UI event from window to agent
class UIEvent extends IsolateMessage {
  final String agentId;
  final List<int> payload;
  UIEvent(this.agentId, this.payload);
}

/// An agent reports that it has finished one unit of work — a `Start` or a
/// `Deliver` — having drained its queue and flushed everything the drain
/// produced. It is sent after the flush, so the traffic that work generated is
/// already on its way to the manager when this arrives: a port delivers in
/// order, so the manager never sees an agent idle while its own output is still
/// unaccounted for.
class AgentIdle extends IsolateMessage {
  final String agentId;
  AgentIdle(this.agentId);
}

/// Sent by an agent isolate when handling a message throws.
///
/// Without this an uncaught exception ends the isolate silently: the agent stops
/// answering, no AgentIdle ever arrives, and the only symptom is [settle]
/// timing out — which is indistinguishable from a system that is merely slow.
/// A throw and a timing change then look identical, and a diagnosis cannot
/// separate them. This makes the throw visible at the moment it happens.
class AgentFaulted extends IsolateMessage {
  final String agentId;
  final String error;
  AgentFaulted(this.agentId, this.error);
}

/// Sent by an agent isolate when initialization fails (e.g. a load/type-check
/// error or a missing goal label) so the manager fails fast instead of hanging
/// forever waiting for a `Ready` that will never arrive (known-issues Issue 19).
class AgentInitFailed extends IsolateMessage {
  final String agentId;
  final String error;
  AgentInitFailed(this.agentId, this.error);
}

/// Trace configuration for multi-agent tracing
class TraceConfig {
  /// Enable GLP-level trace (reductions, suspensions, failures)
  final bool glp;
  /// Enable MAD infrastructure trace (send, globalize, localize, message routing)
  final bool mad;
  /// Only trace these agents (null = all agents)
  final Set<String>? agents;

  const TraceConfig({this.glp = false, this.mad = false, this.agents});
  static const off = TraceConfig();
}

/// Configuration passed to agent isolate
class AgentConfig {
  final String agentId;
  final String goalFunctor;
  final int goalArity; // Arity of the goal (e.g., 2, 3, 4)
  final List<String> goalConstantArgs; // Constant args between agentId and netIn
  final String programSource;
  final List<String>? sharedSources; // Optional shared code files (e.g., social_agent.glp)
  final String? programDir; // Optional program directory for static linking
  final String rootSelfGlpPath; // Absolute path to programs/self.glp
  final SendPort mainPort;
  final SendPort? uiPort; // null for headless
  final TraceConfig traceConfig;

  /// This agent's Ed25519 key pair (seam spec §4). The agent installs it on its
  /// GlpNetwork via putIdentity.
  final ({PubKey pub, Uint8List priv}) keyPair;

  /// The shared identifier–key directory, published to every adapter (§4).
  final NetworkDirectory directory;

  AgentConfig({
    required this.agentId,
    required this.goalFunctor,
    this.goalArity = 2,
    this.goalConstantArgs = const [],
    required this.programSource,
    this.sharedSources,
    this.programDir,
    required this.rootSelfGlpPath,
    required this.mainPort,
    required this.keyPair,
    required this.directory,
    this.uiPort,
    this.traceConfig = const TraceConfig(),
  });
}

/// Manages agent isolates and message routing.
///
/// Event-driven: agents execute on Start and on each incoming NetworkMsg.
/// Termination is external — the caller calls shutdown() when done.
class IsolateManager {
  final Map<String, SendPort> _agentPorts = {};
  final ReceivePort _mainPort = ReceivePort();

  /// The simulation router: owns the directory, adjacency, trust, queues, and
  /// messageId assignment, and routes all inter-agent traffic (seam spec §3).
  final SimulationRouter _router = SimulationRouter();

  /// Trace configuration (set via boot)
  TraceConfig _traceConfig = TraceConfig.off;

  /// Callback for UI output from agents (for Flutter integration)
  void Function(String agentId, Term message)? onUIOutput;

  /// Units of work handed to each agent — one per `Start`, one per `Deliver`.
  final Map<String, int> _workSent = {};

  /// Units of work each agent has reported finished, via [AgentIdle].
  final Map<String, int> _workDone = {};

  /// Awaiting [settle]; completed when the system is quiescent.
  Completer<void>? _settle;

  /// Faults reported by agents, newest last. A non-empty list means at least one
  /// agent threw while handling a message.
  final List<String> _faults = [];

  /// What the agents have thrown, if anything.
  List<String> get faults => List.unmodifiable(_faults);

  /// Whether every agent has finished every unit of work handed to it.
  ///
  /// A message parked in the router (a pair under [cut] or [holdDelivery]) was
  /// never handed to anyone, so it does not keep the system busy — parking is
  /// the harness's own doing and releasing it is the harness's to time.
  bool get _quiescent {
    for (final id in _workSent.keys) {
      if ((_workDone[id] ?? 0) < _workSent[id]!) return false;
    }
    return true;
  }

  void _noteWorkSent(String agentId) {
    _workSent[agentId] = (_workSent[agentId] ?? 0) + 1;
  }

  void _noteWorkDone(String agentId) {
    _workDone[agentId] = (_workDone[agentId] ?? 0) + 1;
    final s = _settle;
    if (s != null && !s.isCompleted && _quiescent) {
      _settle = null;
      s.complete();
    }
  }

  /// Wait until every agent has drained and flushed everything it was given,
  /// and the cascade of traffic that produced has itself settled.
  ///
  /// This is the condition the plays actually wait for. It replaces a fixed
  /// wall-clock delay, which is a guess: too short and the play is judged before
  /// it has run, too long and every run pays for the worst machine. [timeout] is
  /// a backstop for a system that never settles — it throws rather than passing
  /// quietly, since a play that cannot settle is a failure and not a slow pass.
  Future<void> settle(
      {Duration timeout = const Duration(seconds: 30)}) async {
    if (_faults.isNotEmpty) {
      throw StateError('an agent faulted before settling: ${_faults.first}');
    }
    if (_quiescent) return;
    final c = Completer<void>();
    _settle = c;
    await c.future.timeout(timeout, onTimeout: () {
      _settle = null;
      final outstanding = <String>[];
      for (final id in _workSent.keys) {
        final owed = _workSent[id]! - (_workDone[id] ?? 0);
        if (owed > 0) outstanding.add('$id owes $owed');
      }
      throw StateError(
          'agents did not settle within $timeout: ${outstanding.join(', ')}');
    });
  }

  /// Infrastructure log: only prints when MAD tracing is on.
  void _log(String msg) {
    if (_traceConfig.mad) print('[IsolateManager] $msg');
  }

  /// Boot all agents from configuration.
  ///
  /// Returns when all agents are ready (but not yet started).
  Future<void> boot(BootConfig config, {TraceConfig traceConfig = TraceConfig.off}) async {
    _traceConfig = traceConfig;
    final readyCompleter = Completer<void>();
    var readyCount = 0;
    final expectedCount = config.directives.length;

    // 1. Generate an Ed25519 key pair per agent and populate the directory
    //    (seam spec §3 Boot). The boot harness sets trust Open for the plays.
    final keyPairs = <String, ({PubKey pub, Uint8List priv})>{};
    for (final directive in config.directives) {
      final kp = generateKeyPair();
      keyPairs[directive.agentId] = kp;
      _router.register(directive.agentId, kp.pub);
      _router.setTrustLevel(directive.agentId, TrustLevel.open);
    }

    // 2. The router delivers to the destination agent's isolate port — the
    //    seam's simulation transport.
    _router.onDeliver = (toId, fromPk, payload, messageId, t) {
      final port = _agentPorts[toId];
      if (port == null) {
        print('[IsolateManager] WARNING: Unknown destination $toId');
        return;
      }
      final fromId = _router.directory.idOf(fromPk) ?? '?';
      _noteWorkSent(toId);
      port.send(Deliver(fromId, payload, messageId));
    };

    // Single listener for all messages
    _mainPort.listen((msg) {
      // Handle Ready messages for boot completion
      if (msg is Ready && !readyCompleter.isCompleted) {
        _agentPorts[msg.agentId] = msg.sendPort;
        readyCount++;
        if (readyCount == expectedCount) {
          readyCompleter.complete();
        }
      }
      // An isolate that died during init reports it here so boot() fails fast
      // instead of awaiting a Ready that will never come (Issue 19).
      if (msg is AgentInitFailed && !readyCompleter.isCompleted) {
        readyCompleter.completeError(
          StateError('Agent ${msg.agentId} failed to initialize: ${msg.error}'),
        );
        return;
      }
      // Always handle messages via _handleMessage
      _handleMessage(msg);
    });

    // 3. Spawn isolates with the key pair and the complete directory.
    for (final directive in config.directives) {
      final agentConfig = AgentConfig(
        agentId: directive.agentId,
        goalFunctor: directive.goalFunctor,
        goalArity: directive.goalArity,
        goalConstantArgs: directive.constantArgs,
        programSource: config.source,
        sharedSources: config.sharedSources,
        programDir: config.programDir,
        rootSelfGlpPath: config.rootSelfGlpPath,
        mainPort: _mainPort.sendPort,
        keyPair: keyPairs[directive.agentId]!,
        directory: _router.directory,
        traceConfig: traceConfig,
      );

      await Isolate.spawn(_agentIsolateEntry, agentConfig);
    }

    // Wait for all agents to be ready
    await readyCompleter.future;
  }

  /// Harness control: visible disconnection of a pair (seam spec §3, §7.2).
  void cut(String a, String b) => _router.cut(a, b);

  /// Harness control: reverse a [cut], flushing queued messages in order.
  void restore(String a, String b) => _router.restore(a, b);

  /// Harness control: invisible delay of a pair's delivery (seam spec §3).
  void holdDelivery(String a, String b) => _router.holdDelivery(a, b);

  /// Harness control: release a [holdDelivery], flushing in reverse order.
  void releaseDelivery(String a, String b) => _router.releaseDelivery(a, b);

  /// Start all agents.
  void start() {
    for (final entry in _agentPorts.entries) {
      _noteWorkSent(entry.key);
      entry.value.send(Start());
    }
  }

  /// Inject a UI event to an agent (for testing).
  void injectUIEvent(String agentId, Term message) {
    final port = _agentPorts[agentId];
    if (port == null) {
      print('[IsolateManager] WARNING: Unknown agent $agentId');
      return;
    }

    // Serialize the message
    final serializer = PayloadSerializer(agentId);
    final payload = serializer.serializeAgentMessage(message);
    port.send(UIEvent(agentId, payload));
  }

  /// Shutdown all isolates.
  Future<void> shutdown() async {
    _mainPort.close();
    _agentPorts.clear();
  }

  /// Handle messages from agent isolates.
  void _handleMessage(dynamic msg) {
    if (msg is Ready) {
      _log('${msg.agentId} ready');
      _agentPorts[msg.agentId] = msg.sendPort;

    } else if (msg is AgentIdle) {
      _noteWorkDone(msg.agentId);

    } else if (msg is AgentFaulted) {
      // Surface it at once rather than letting the wait time out: the fault is
      // the diagnosis, and a timeout thirty seconds later is not.
      _faults.add('${msg.agentId}: ${msg.error}');
      print('[IsolateManager] agent ${msg.agentId} faulted: ${msg.error}');
      final s = _settle;
      if (s != null && !s.isCompleted) {
        _settle = null;
        s.completeError(StateError('agent ${msg.agentId} faulted: ${msg.error}'));
      }

    } else if (msg is RouterSend) {
      if (_traceConfig.glp && _isTracingAgent(msg.fromId)) {
        print('[${msg.fromId}] → send to ${msg.toId}');
      }
      _router.routeSend(msg.fromId, msg.toId, Uint8List.fromList(msg.payload));
    }
  }

  /// Check if an agent should be traced.
  bool _isTracingAgent(String agentId) {
    if (!_traceConfig.glp && !_traceConfig.mad) return false;
    if (_traceConfig.agents != null && !_traceConfig.agents!.contains(agentId)) return false;
    return true;
  }
}

/// Agent isolate entry point.
///
/// This runs in a separate isolate for each agent.
/// Uses GlpEngine - the ONE way to run GLP programs.
///
/// Event-driven execution: drain+flush on Start and on each incoming NetworkMsg.
void _agentIsolateEntry(AgentConfig config) async {
  final agentId = config.agentId;
  final receivePort = ReceivePort();
  final tc = config.traceConfig;

  // Infrastructure log: only prints when MAD tracing is on
  void log(String msg) {
    if (tc.mad) print('[$agentId] $msg');
  }

  log('Starting isolate');

  // Create GlpEngine — the ONE way to run GLP programs.
  // Non-strict types: actor code may have type warnings that shouldn't be fatal.
  final engine = GlpEngine(rootSelfGlpPath: config.rootSelfGlpPath)..strictTypes = false;

  // Enable madGLP mode (loads madPredicates + creates MadContext)
  engine.enableMadGLP(agentId: agentId);

  // Load program code: either via program linking or individual file loading.
  // A load/type-check failure here (e.g. UnknownTypeError) must be reported to
  // the manager, not left to kill the isolate silently — otherwise boot() hangs
  // forever waiting for Ready (Issue 19).
  try {
    if (config.programDir != null) {
      // Program-directory mode: static-link the program, then load boot source on top.
      engine.loadProgram(config.programDir!);
      engine.loadSource(config.programSource, filename: 'program');
      log('Program loaded via program linking (${config.programDir}) + boot source');
    } else {
      // Legacy mode: load shared source files and boot program sequentially.
      // Each file is loaded separately to preserve per-file -mode() directives.
      if (config.sharedSources != null) {
        for (var i = 0; i < config.sharedSources!.length; i++) {
          engine.loadSource(config.sharedSources![i], filename: 'shared_$i');
        }
      }
      engine.loadSource(config.programSource, filename: 'program');
      log('Program loaded via GlpEngine (stdlib + madPredicates + user code)');
    }
  } catch (e, st) {
    print('[$agentId] ERROR: init failed during load: $e');
    config.mainPort.send(AgentInitFailed(agentId, '$e\n$st'));
    return;
  }
  engine.debugTrace = tc.glp;  // Enable GLP trace only when requested
  final ctx = engine.madContext!;
  final runtime = engine.runtime;

  // Initialize the permanent index-0 serializer entry for network input
  // Spec Section 4.1: "At boot time, each agent p creates a permanent entry
  // at index 0 mapping `_r(p, 0)` to the local writer N_p for p's network
  // input stream."
  final (netInWriter, netInReader) = runtime.heap.allocateVariable();
  ctx.wp.initializeSerializerEntry(netInWriter);
  log('Serializer entry initialized at index 0, netIn=($netInWriter,$netInReader)');

  // Networking seam (spec §3–4): the agent talks to a GlpNetwork, not the
  // mainPort directly. In simulation the client forwards sends to the router
  // (main isolate) over the existing port; deliveries arrive as Deliver
  // messages and fire onMessageReceived.
  final network = SimulationNetworkClient(
    selfId: agentId,
    directory: config.directory,
    sendToRouter: (toId, payload) =>
        config.mainPort.send(RouterSend(agentId, toId, payload)),
  );
  network.putIdentity(config.keyPair.pub, config.keyPair.priv);
  // Back the sign/2 kernel and the valid_attestation/4 guard (seam spec §4).
  ctx.network = network;

  // Outgoing (spec §4): ctx.onMessageReady(destId, msg) → network.send.
  ctx.onMessageReady = (destId, msg) {
    final pk = config.directory.pkOf(destId);
    if (pk == null) {
      print('[$agentId] ERROR: unknown destination $destId');  // Always print errors
      return;
    }
    log('Sending to $destId (${msg.payload.length}B)');
    network.send(pk, Uint8List.fromList(msg.payload));
  };

  // Incoming (spec §4): dispatch on the message kind byte (value/request/
  // acknowledgement) via the MadContext receive entry point.
  network.onMessageReceived = (senderPk, payload, messageId, transport) {
    try {
      final fromId = config.directory.idOf(senderPk) ?? '?';
      log('Incoming payload from $fromId (${payload.length}B)');
      ctx.handleIncomingPayload(payload: payload, fromAgent: fromId);
    } catch (e) {
      print('[$agentId] ERROR handling delivery: $e');  // Always print errors
    }
  };

  log('Network input ready: writer=$netInWriter, reader=$netInReader');

  // Find goal entry point with the actual arity from the boot directive.
  // Arity 2: agent_init(agentId, netIn)
  // Arity 3: child_init(agentId, playNum, netIn)
  // Arity 4: parent_init(agentId, childName, playNum, netIn)
  final program = engine.combinedProgram;
  final arity = config.goalArity;
  final goalLabel = '${config.goalFunctor}/$arity';
  // Run the isolate's agent on the byte interpreter over a CodeImage of the
  // program, entry as a byte offset.
  final image = codeImageFromProgram(program);
  final goalPC = image.entryOffsetOf(goalLabel);
  if (goalPC == null) {
    print('[$agentId] ERROR: Goal $goalLabel not found');  // Always print errors
    config.mainPort.send(AgentInitFailed(agentId, 'Goal $goalLabel not found'));
    return;
  }

  // Build argument map: arg 0 = agent ID, args 1..n-2 = constants, arg n-1 = netIn
  final args = <int, Term>{};

  // Arg 0: agent ID (constant)
  final (idArgWriter, idArgReader) = runtime.heap.allocateVariable();
  runtime.heap.bindVariable(idArgWriter, ConstTerm(agentId));
  args[0] = VarRef(idArgReader);

  // Args 1..n-2: additional constant arguments from boot directive
  for (var i = 0; i < config.goalConstantArgs.length; i++) {
    final constVal = config.goalConstantArgs[i];
    final (cw, cr) = runtime.heap.allocateVariable();
    // Try to parse as integer, otherwise treat as atom
    final intVal = int.tryParse(constVal);
    if (intVal != null) {
      runtime.heap.bindVariable(cw, ConstTerm(intVal));
    } else {
      runtime.heap.bindVariable(cw, ConstTerm(constVal));
    }
    args[i + 1] = VarRef(cr);
  }

  // Last arg: network input reader
  final (netInArgWriter, netInArgReader) = runtime.heap.allocateVariable();
  runtime.heap.bindVariable(netInArgWriter, VarRef(netInReader));
  args[arity - 1] = VarRef(netInArgReader);

  // Spawn main goal
  runtime.setGoalEnv(1, CallEnv(args: args));
  runtime.setGoalProgram(1, 'main');
  runtime.gq.enqueue(GoalRef(1, goalPC));
  log('Spawned ${config.goalFunctor}/$arity');

  // Create scheduler for this engine
  final GoalRunner runner = ByteRunner(image);
  final scheduler = Scheduler(rt: runtime, runners: {'main': runner});

  // Set up tracing: lines print directly (no buffering needed without ticks)
  if (tc.glp) {
    scheduler.traceSink = (String line) {
      print('[$agentId] $line');
    };
  }

  if (tc.mad) {
    ctx.traceSink = (String line) {
      // MAD traces already include [MAD agentId] prefix
      print(line);
    };
  }

  // Signal ready
  config.mainPort.send(Ready(agentId, receivePort.sendPort));

  // Event-driven message handling loop
  await for (final msg in receivePort) {
    try {
    if (msg is Start) {
      // Initial drain+flush: kicks off the agent's goal
      scheduler.drainWithStatus(debug: engine.debugTrace);
      ctx.flushMessages();
      config.mainPort.send(AgentIdle(agentId));

    } else if (msg is Deliver) {
      log('Received delivery from ${msg.fromId} (id=${msg.messageId})');
      final senderPk = config.directory.pkOf(msg.fromId);
      if (senderPk != null) {
        network.onMessageReceived?.call(
          senderPk,
          Uint8List.fromList(msg.payload),
          msg.messageId,
          Transport.ble,
        );
      } else {
        print('[$agentId] ERROR: delivery from unknown ${msg.fromId}');  // Always print errors
      }

      // Drain activated goals and flush any response messages
      scheduler.drainWithStatus(debug: engine.debugTrace);
      ctx.flushMessages();
      config.mainPort.send(AgentIdle(agentId));

    } else if (msg is UIEvent) {
      // UIEvent handling is for external Flutter UI integration.
      // With the current architecture (actors internal to GLP), this is not used.
      // The actor communicates directly with the agent via channels in GLP code.
      log('Received UI event (not processed - actors are internal)');
    }
    } catch (e, st) {
      // An uncaught throw used to end the isolate here, silently. Report it, and
      // still account the work unit, so a waiter fails on the fault rather than
      // on a timeout that says nothing about what went wrong.
      print('[$agentId] ERROR: uncaught while handling ${msg.runtimeType}: $e');
      print(st);
      config.mainPort.send(AgentFaulted(agentId, '$e'));
      config.mainPort.send(AgentIdle(agentId));
    }
  }
}
