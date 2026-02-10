/// Isolate Manager for madGLP
///
/// Spawns agent isolates based on BootConfig and routes messages between them.
/// Implements the Dart-level routing described in isolate-boot-spec.md (v0.4).
///
/// Updated for madGLP push-based communication model (madGLP-spec.md v4.2).
/// Refactored to use GlpEngine - the ONE way to run GLP programs.
///
/// See: docs/ma/isolate-boot-spec.md, docs/ma/madGLP-spec.md

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

/// Tick to drive scheduler (for testing/headless mode)
class Tick extends IsolateMessage {}

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

/// Agent status report
class Status extends IsolateMessage {
  final String agentId;
  final String status; // 'running', 'suspended', 'completed'
  final int goalsRemaining;
  final List<String> traceLines; // Collected trace output for this tick
  Status(this.agentId, this.status, this.goalsRemaining, [this.traceLines = const []]);
}

/// Agent completed
class Done extends IsolateMessage {
  final String agentId;
  final bool success;
  final String? error;
  Done(this.agentId, this.success, {this.error});
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
  final String programSource;
  final String? sharedSource; // Optional shared code (e.g., social_agent.glp)
  final SendPort mainPort;
  final SendPort? uiPort; // null for headless
  final TraceConfig traceConfig;

  AgentConfig({
    required this.agentId,
    required this.goalFunctor,
    required this.programSource,
    this.sharedSource,
    required this.mainPort,
    this.uiPort,
    this.traceConfig = const TraceConfig(),
  });
}

/// Manages agent isolates and message routing.
class IsolateManager {
  final Map<String, SendPort> _agentPorts = {};
  final ReceivePort _mainPort = ReceivePort();
  final Set<String> _completed = {};

  Completer<void>? _allCompletedCompleter;
  Timer? _tickTimer;

  /// Trace configuration (set via boot)
  TraceConfig _traceConfig = TraceConfig.off;
  int _tickCount = 0;
  bool _tickHeaderPrinted = false;

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
    _allCompletedCompleter = Completer<void>();

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
        programSource: config.source,
        sharedSource: config.sharedSource,
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

  /// Send a tick to all agents (for headless testing).
  void tick() {
    _tickCount++;
    _tickHeaderPrinted = false;
    for (final port in _agentPorts.values) {
      port.send(Tick());
    }
  }

  /// Start automatic ticking at given interval.
  void startTicking({Duration interval = const Duration(milliseconds: 50)}) {
    _tickTimer?.cancel();
    _tickTimer = Timer.periodic(interval, (_) => tick());
  }

  /// Stop automatic ticking.
  void stopTicking() {
    _tickTimer?.cancel();
    _tickTimer = null;
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

  /// Wait for all agents to complete.
  Future<void> waitForCompletion({Duration? timeout}) async {
    if (_completed.length == _agentPorts.length) {
      return;
    }

    final future = _allCompletedCompleter?.future ?? Future.value();

    if (timeout != null) {
      await future.timeout(timeout, onTimeout: () {
        throw TimeoutException(
          'Agents did not complete within $timeout. '
          'Completed: $_completed, Expected: ${_agentPorts.keys}',
        );
      });
    } else {
      await future;
    }
  }

  /// Check if all agents have completed.
  bool get allCompleted => _completed.length == _agentPorts.length;

  /// Get list of completed agent IDs.
  Set<String> get completedAgents => Set.unmodifiable(_completed);

  /// Shutdown all isolates.
  Future<void> shutdown() async {
    stopTicking();
    _mainPort.close();
    _agentPorts.clear();
    _completed.clear();
  }

  /// Handle messages from agent isolates.
  void _handleMessage(dynamic msg) {
    if (msg is Ready) {
      _log('${msg.agentId} ready');
      _agentPorts[msg.agentId] = msg.sendPort;

    } else if (msg is NetworkMsg) {
      _routeNetworkMessage(msg);

    } else if (msg is Status) {
      // Render trace lines if tracing is enabled
      if (msg.traceLines.isNotEmpty && _isTracingAgent(msg.agentId)) {
        if (!_tickHeaderPrinted) {
          print('--- tick $_tickCount ---');
          _tickHeaderPrinted = true;
        }
        for (final line in msg.traceLines) {
          print(line);
        }
      }

    } else if (msg is Done) {
      _log('${msg.agentId} done: success=${msg.success}');
      _completed.add(msg.agentId);

      if (_completed.length == _agentPorts.length) {
        final completer = _allCompletedCompleter;
        if (completer != null && !completer.isCompleted) {
          completer.complete();
        }
      }
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

/// madGLP system predicates (embedded)
///
/// These are loaded before the user program to provide:
/// - send_to_net/1: processes network output stream
/// - global_send/3: send via global link (handles both cold-calls and established links)
const String _madPredicatesSource = r'''
-mode(system).  %% Uses reserved constants like '_w' and '_send'

%% madGLP System Predicates (embedded in isolate_manager.dart)
%% See: madGLP-spec.md Section 4 and Section 12

%% send_to_net/1 - Process network output stream
%% Uses global_send/3 with serializer address _w(Q,0) for cold-calls
%% Sends msg(Q, T) wrapper to match dGLP network3 format
%% Note: Q is used twice after ground guard - SRSW checker allows this for ground vars
procedure send_to_net(Stream?).
send_to_net([msg(Q, T) | In]) :- ground(Q?) | global_send(msg(Q?, T?), '_w'(Q?, 0), Q?), send_to_net(In?).
send_to_net([]).

%% global_send/3 - Send via global link
%% Handles both cold-calls (index 0) and established links (index > 0)
procedure global_send(_?, _?, _?).
global_send(T, G, Q) :- known(T?) | '_send'(T?, G?, Q?).
''';

/// Agent isolate entry point.
///
/// This runs in a separate isolate for each agent.
/// Uses GlpEngine - the ONE way to run GLP programs.
/// Updated for madGLP push-based communication.
void _agentIsolateEntry(AgentConfig config) async {
  final agentId = config.agentId;
  final receivePort = ReceivePort();
  var doneSent = false;
  final tc = config.traceConfig;

  // Infrastructure log: only prints when MAD tracing is on
  void log(String msg) {
    if (tc.mad) print('[$agentId] $msg');
  }

  log('Starting isolate');

  // Create GlpEngine (same as REPL would)
  // Non-strict types: actor code may have type warnings that shouldn't be fatal
  final engine = GlpEngine()..strictTypes = false;

  // Build combined source: mad_predicates + shared source (if any) + boot file
  // This ensures procedures like send_to_net/1 and agent/4 are visible to user code
  // Strip any -mode(system) directives since it's already in mad_predicates
  final sharedSource = config.sharedSource?.replaceAll(RegExp(r'-mode\s*\(\s*system\s*\)\s*\.'), '') ?? '';
  final userSource = config.programSource.replaceAll(RegExp(r'-mode\s*\(\s*system\s*\)\s*\.'), '');
  final combinedSource = '$_madPredicatesSource\n$sharedSource\n$userSource';
  engine.loadSource(combinedSource);
  engine.debugTrace = tc.glp;  // Enable GLP trace only when requested
  log('Program loaded via GlpEngine (with mad_predicates)');

  // Enable madGLP mode
  engine.enableMadGLP(agentId: agentId);
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

  // Find goal entry point (arity 2: goal(agentId, netIn))
  final program = engine.combinedProgram;
  final goalLabel = '${config.goalFunctor}/2';
  final goalPC = program.labels[goalLabel];
  if (goalPC == null) {
    print('[$agentId] ERROR: Goal $goalLabel not found');  // Always print errors
    config.mainPort.send(Done(agentId, false, error: 'Goal $goalLabel not found'));
    return;
  }

  // Build argument map: arg 0 = agent ID, arg 1 = network input reader
  final args = <int, Term>{};

  // Arg 0: agent ID (constant)
  final (idArgWriter, idArgReader) = runtime.heap.allocateVariable();
  runtime.heap.bindVariable(idArgWriter, ConstTerm(agentId));
  args[0] = VarRef(idArgReader);

  // Arg 1: network input reader
  final (netInArgWriter, netInArgReader) = runtime.heap.allocateVariable();
  runtime.heap.bindVariable(netInArgWriter, VarRef(netInReader));
  args[1] = VarRef(netInArgReader);

  // Spawn main goal
  runtime.setGoalEnv(1, CallEnv(args: args));
  runtime.setGoalProgram(1, 'main');
  runtime.gq.enqueue(GoalRef(1, goalPC));
  log('Spawned ${config.goalFunctor}/2');

  // Create scheduler for this engine
  final runner = BytecodeRunner(program);
  final scheduler = Scheduler(rt: runtime, runners: {'main': runner});

  // Set up trace collection: lines are buffered and sent back in Status messages
  final traceLines = <String>[];

  if (tc.glp) {
    scheduler.traceSink = (String line) {
      traceLines.add('[$agentId] $line');
    };
  }

  if (tc.mad) {
    ctx.traceSink = (String line) {
      // MAD traces already include [MAD agentId] prefix, no need to add [$agentId]
      traceLines.add(line);
    };
  }

  // Signal ready
  config.mainPort.send(Ready(agentId, receivePort.sendPort));

  // Message handling loop
  await for (final msg in receivePort) {
    if (msg is Start || msg is Tick) {
      // Clear trace buffer for this tick
      traceLines.clear();

      // Run scheduler with debug tracing
      final result = scheduler.drainWithStatus(debug: engine.debugTrace);

      // Flush messages (triggers global_send goals to send)
      ctx.flushMessages();

      // Report status with collected trace lines
      final hasSuspendedGoals = result.status == ExecutionStatus.suspended ||
                                runtime.suspended.isNotEmpty;
      final status = hasSuspendedGoals
          ? 'suspended'
          : (runtime.gq.isEmpty ? 'completed' : 'running');
      config.mainPort.send(Status(agentId, status, runtime.gq.length, List.from(traceLines)));

      // Only report done when gq is empty AND no goals are suspended
      if (runtime.gq.isEmpty && !hasSuspendedGoals && !doneSent) {
        doneSent = true;
        log('All goals completed');
        config.mainPort.send(Done(agentId, true));
      }

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

      // Flush any response messages
      ctx.flushMessages();

    } else if (msg is UIEvent) {
      // UIEvent handling is for external Flutter UI integration.
      // With the current architecture (actors internal to GLP), this is not used.
      // The actor communicates directly with the agent via channels in GLP code.
      log('Received UI event (not processed - actors are internal)');
    }
  }
}
