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

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';
import 'package:glp_runtime/multiagent/payload_serializer.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';

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

/// Network message to route between agents (madGLP assignment)
class NetworkMsg extends IsolateMessage {
  final String from;
  final String to;
  final List<int> payload;
  final MessageType type;

  /// Global name for routing (madGLP)
  final String? globalNameAgent;
  final int? globalNameIndex;
  final bool? globalNameIsWriter;

  NetworkMsg(this.from, this.to, this.payload, this.type, {
    this.globalNameAgent,
    this.globalNameIndex,
    this.globalNameIsWriter,
  });

  @override
  String toString() => 'NetworkMsg($from->$to, $type)';
}

/// UI event from window to agent
class UIEvent extends IsolateMessage {
  final String agentId;
  final List<int> payload;
  UIEvent(this.agentId, this.payload);
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
  final String? projectDir; // Optional project directory for static linking
  final String rootSelfGlpPath; // Absolute path to programs/self.glp
  final SendPort mainPort;
  final SendPort? uiPort; // null for headless
  final TraceConfig traceConfig;

  AgentConfig({
    required this.agentId,
    required this.goalFunctor,
    this.goalArity = 2,
    this.goalConstantArgs = const [],
    required this.programSource,
    this.sharedSources,
    this.projectDir,
    required this.rootSelfGlpPath,
    required this.mainPort,
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

  /// Trace configuration (set via boot)
  TraceConfig _traceConfig = TraceConfig.off;

  /// Callback for UI output from agents (for Flutter integration)
  void Function(String agentId, Term message)? onUIOutput;

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
      // Always handle messages via _handleMessage
      _handleMessage(msg);
    });

    // Spawn isolates
    for (final directive in config.directives) {
      final agentConfig = AgentConfig(
        agentId: directive.agentId,
        goalFunctor: directive.goalFunctor,
        goalArity: directive.goalArity,
        goalConstantArgs: directive.constantArgs,
        programSource: config.source,
        sharedSources: config.sharedSources,
        projectDir: config.projectDir,
        rootSelfGlpPath: config.rootSelfGlpPath,
        mainPort: _mainPort.sendPort,
        traceConfig: traceConfig,
      );

      await Isolate.spawn(_agentIsolateEntry, agentConfig);
    }

    // Wait for all agents to be ready
    await readyCompleter.future;
  }

  /// Start all agents.
  void start() {
    for (final port in _agentPorts.values) {
      port.send(Start());
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

    } else if (msg is NetworkMsg) {
      _routeNetworkMessage(msg);
    }
  }

  /// Route a network message to its destination.
  void _routeNetworkMessage(NetworkMsg msg) {
    // Trace message events
    if (_traceConfig.glp) {
      if (_isTracingAgent(msg.from)) {
        print('[${msg.from}] → msg to ${msg.to}: ${msg.type}');
      }
      if (_isTracingAgent(msg.to)) {
        print('[${msg.to}] ← msg from ${msg.from}: ${msg.type}');
      }
    }

    final targetPort = _agentPorts[msg.to];
    if (targetPort == null) {
      print('[IsolateManager] WARNING: Unknown destination ${msg.to}');
      return;
    }

    targetPort.send(msg);
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

  // Load program code: either via project linking or individual file loading.
  if (config.projectDir != null) {
    // Project-directory mode: static-link the project, then load boot source on top.
    engine.loadProject(config.projectDir!);
    engine.loadSource(config.programSource, filename: 'program');
    log('Program loaded via project linking (${config.projectDir}) + boot source');
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

  // Message routing to main isolate (madGLP: push-based via onMessageReady)
  ctx.onMessageReady = (dest, msg) {
    log('Sending ${msg.type} to $dest');
    config.mainPort.send(NetworkMsg(agentId, dest, msg.payload, msg.type));
  };

  log('Network input ready: writer=$netInWriter, reader=$netInReader');

  // Find goal entry point with the actual arity from the boot directive.
  // Arity 2: agent_init(agentId, netIn)
  // Arity 3: child_init(agentId, playNum, netIn)
  // Arity 4: parent_init(agentId, childName, playNum, netIn)
  final program = engine.combinedProgram;
  final arity = config.goalArity;
  final goalLabel = '${config.goalFunctor}/$arity';
  final goalPC = program.labels[goalLabel];
  if (goalPC == null) {
    print('[$agentId] ERROR: Goal $goalLabel not found');  // Always print errors
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
  final runner = BytecodeRunner(program);
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
    if (msg is Start) {
      // Initial drain+flush: kicks off the agent's goal
      scheduler.drainWithStatus(debug: engine.debugTrace);
      ctx.flushMessages();

    } else if (msg is NetworkMsg) {
      log('Received ${msg.type} from ${msg.from}');

      if (msg.type == MessageType.assignment) {
        // madGLP: Handle assignment message
        final serializer = PayloadSerializer(agentId);

        try {
          final (globalName, value) = serializer.deserializeGlobalSendPayload(
            msg.payload,
            (isReader) {
              final (w, r) = runtime.heap.allocateVariable();
              return isReader ? r : w;
            },
          );

          log('Assignment: $globalName := $value');

          ctx.handleMadAssignment(
            globalName: globalName,
            value: value,
            fromAgent: msg.from,
          );
        } catch (e) {
          print('[$agentId] ERROR handling assignment: $e');  // Always print errors
        }
      } else if (msg.type == MessageType.agentMessage) {
        log('Received agent message from ${msg.from}');
      }

      // Drain activated goals and flush any response messages
      scheduler.drainWithStatus(debug: engine.debugTrace);
      ctx.flushMessages();

    } else if (msg is UIEvent) {
      // UIEvent handling is for external Flutter UI integration.
      // With the current architecture (actors internal to GLP), this is not used.
      // The actor communicates directly with the agent via channels in GLP code.
      log('Received UI event (not processed - actors are internal)');
    }
  }
}
