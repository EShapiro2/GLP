/// Isolate Manager for maGLP
///
/// Spawns agent isolates based on BootConfig and routes messages between them.
/// Implements the Dart-level routing described in isolate-boot-spec.md (v0.4).
///
/// See: docs/ma/isolate-boot-spec.md

import 'dart:async';
import 'dart:isolate';

import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/multiagent/irma_context.dart';
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

/// Network message to route between agents
class NetworkMsg extends IsolateMessage {
  final String from;
  final String to;
  final List<int> payload;
  final MessageType type;
  NetworkMsg(this.from, this.to, this.payload, this.type);

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
  Status(this.agentId, this.status, this.goalsRemaining);
}

/// Agent completed
class Done extends IsolateMessage {
  final String agentId;
  final bool success;
  final String? error;
  Done(this.agentId, this.success, {this.error});
}

/// Configuration passed to agent isolate
class AgentConfig {
  final String agentId;
  final String goalFunctor;
  final String programSource;
  final SendPort mainPort;
  final SendPort? uiPort; // null for headless

  AgentConfig({
    required this.agentId,
    required this.goalFunctor,
    required this.programSource,
    required this.mainPort,
    this.uiPort,
  });
}

/// Manages agent isolates and message routing.
class IsolateManager {
  final Map<String, SendPort> _agentPorts = {};
  final ReceivePort _mainPort = ReceivePort();
  final Set<String> _completed = {};
  
  Completer<void>? _allCompletedCompleter;
  Timer? _tickTimer;
  
  /// Callback for UI output from agents (for Flutter integration)
  void Function(String agentId, Term message)? onUIOutput;
  
  /// Boot all agents from configuration.
  ///
  /// Returns when all agents are ready (but not yet started).
  Future<void> boot(BootConfig config) async {
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
        mainPort: _mainPort.sendPort,
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
      print('[IsolateManager] ${msg.agentId} ready');
      _agentPorts[msg.agentId] = msg.sendPort;

    } else if (msg is NetworkMsg) {
      _routeNetworkMessage(msg);

    } else if (msg is Status) {
      // Could expose via callback if needed
      print('[IsolateManager] ${msg.agentId}: ${msg.status}, goals=${msg.goalsRemaining}');

    } else if (msg is Done) {
      print('[IsolateManager] ${msg.agentId} done: success=${msg.success}');
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
    print('[IsolateManager] Routing ${msg.type} from ${msg.from} to ${msg.to}');
    
    final targetPort = _agentPorts[msg.to];
    if (targetPort == null) {
      print('[IsolateManager] WARNING: Unknown destination ${msg.to}');
      return;
    }
    
    targetPort.send(msg);
  }
}

/// Agent isolate entry point.
///
/// This runs in a separate isolate for each agent.
void _agentIsolateEntry(AgentConfig config) async {
  final agentId = config.agentId;
  final receivePort = ReceivePort();
  var doneSent = false;

  print('[$agentId] Starting isolate');

  // Compile program
  final compiler = GlpCompiler();
  final program = compiler.compile(config.programSource);
  print('[$agentId] Program compiled: ${program.ops.length} ops');

  // Create runtime
  final runtime = GlpRuntime();
  final runner = BytecodeRunner(program);
  final scheduler = Scheduler(rt: runtime, runners: {'main': runner});
  final ctx = IrmaContext(agentId: agentId, runtime: runtime);

  // Message routing to main isolate
  ctx.onMessageReady = (dest, msg) {
    print('[$agentId] Sending ${msg.type} to $dest');
    config.mainPort.send(NetworkMsg(agentId, dest, msg.payload, msg.type));
  };

  // Allocate UI channel (second argument)
  final (uiInWriter, uiInReader) = runtime.heap.allocateVariable();
  final (uiOutWriter, uiOutReader) = runtime.heap.allocateVariable();
  final uiCh = StructTerm('ch', [VarRef(uiInReader), VarRef(uiOutWriter)]);

  // Allocate network channel (third argument)
  final (netInWriter, netInReader) = runtime.heap.allocateVariable();
  final (netOutWriter, netOutReader) = runtime.heap.allocateVariable();
  final netCh = StructTerm('ch', [VarRef(netInReader), VarRef(netOutWriter)]);

  // Register IRMA network streams
  ctx.registerNetworkInput(netInWriter);
  ctx.registerNetworkOutput(netOutWriter);

  print('[$agentId] Channels allocated');
  print('[$agentId]   UICh: in=($uiInWriter,$uiInReader), out=($uiOutWriter,$uiOutReader)');
  print('[$agentId]   NetCh: in=($netInWriter,$netInReader), out=($netOutWriter,$netOutReader)');

  // Create argument cells with proper reader references
  final (idArgWriter, idArgReader) = runtime.heap.allocateVariable();
  final (uiChArgWriter, uiChArgReader) = runtime.heap.allocateVariable();
  final (netChArgWriter, netChArgReader) = runtime.heap.allocateVariable();

  // Bind argument writers to their values
  runtime.heap.bindVariable(idArgWriter, ConstTerm(agentId));
  runtime.heap.bindVariable(uiChArgWriter, uiCh);
  runtime.heap.bindVariable(netChArgWriter, netCh);

  // Find goal entry point
  final goalLabel = '${config.goalFunctor}/3';
  final goalPC = program.labels[goalLabel];
  if (goalPC == null) {
    print('[$agentId] ERROR: Goal $goalLabel not found');
    config.mainPort.send(Done(agentId, false, error: 'Goal $goalLabel not found'));
    return;
  }

  // Spawn goal with reader references
  runtime.setGoalEnv(1, CallEnv(args: {
    0: VarRef(idArgReader),
    1: VarRef(uiChArgReader),
    2: VarRef(netChArgReader),
  }));
  runtime.setGoalProgram(1, 'main');
  runtime.gq.enqueue(GoalRef(1, goalPC));
  print('[$agentId] Spawned ${config.goalFunctor}/3');

  // Signal ready
  config.mainPort.send(Ready(agentId, receivePort.sendPort));

  // Message handling loop
  await for (final msg in receivePort) {
    if (msg is Start || msg is Tick) {
      // Run scheduler
      final result = scheduler.drainWithStatus();

      // Process suspensions
      if (result.status == ExecutionStatus.suspended) {
        ctx.processSuspension(result.blockingReaders);
      }

      // Flush messages
      ctx.flushMessages();

      // Report status
      // IMPORTANT: Check BOTH the drain result AND runtime.suspended (global suspension map)
      // Goals suspended in previous drains won't show in result.status but ARE in runtime.suspended
      final hasSuspendedGoals = result.status == ExecutionStatus.suspended ||
                                runtime.suspended.isNotEmpty;
      final status = hasSuspendedGoals
          ? 'suspended'
          : (runtime.gq.isEmpty ? 'completed' : 'running');
      config.mainPort.send(Status(agentId, status, runtime.gq.length));

      // Only report done when gq is empty AND no goals are suspended (either from this drain or previous)
      if (runtime.gq.isEmpty && !hasSuspendedGoals && !doneSent) {
        doneSent = true;
        print('[$agentId] All goals completed');
        config.mainPort.send(Done(agentId, true));
      }

    } else if (msg is NetworkMsg) {
      print('[$agentId] Received ${msg.type} from ${msg.from}');

      if (msg.type == MessageType.agentMessage) {
        ctx.handleNetworkMessage(msg.from, msg.payload);
      } else if (msg.type == MessageType.assignment) {
        final serializer = PayloadSerializer('');
        final (globalId, value) = serializer.deserializeAssignmentPayload(
          msg.payload,
          (isReader) => isReader
              ? runtime.heap.allocateImportedReader()
              : runtime.heap.allocateImportedWriter(),
          onVariableImported: (localAddr, isReader, globalId, pairedReaderCreatorLocalId) {
            ctx.attachImportedVariableEntry(localAddr, isReader, globalId, msg.from,
                pairedReaderCreatorLocalId: pairedReaderCreatorLocalId);
          },
        );
        print('[$agentId] Assignment: ${globalId.creator}:${globalId.localId} := $value');
        ctx.handleAssignment(globalId.creator, globalId.localId, value);
      } else if (msg.type == MessageType.readRequest) {
        final serializer = PayloadSerializer('');
        final varId = serializer.deserializeReadRequestPayload(msg.payload);
        print('[$agentId] Read request for $varId from ${msg.from}');
        ctx.handleReadRequest(varId, msg.from);
      }

      // Flush any response messages
      ctx.flushMessages();

    } else if (msg is UIEvent) {
      print('[$agentId] Received UI event');
      // Deserialize and bind to uiInWriter
      final serializer = PayloadSerializer('');
      final term = serializer.deserializeAgentMessagePayload(
        msg.payload,
        (isReader) => isReader
            ? runtime.heap.allocateImportedReader()
            : runtime.heap.allocateImportedWriter(),
      );
      
      // Bind as next element in UI input stream
      final (tailWriter, tailReader) = runtime.heap.allocateVariable();
      final consCell = StructTerm('.', [term, VarRef(tailReader)]);
      runtime.heap.bindVariable(uiInWriter, consCell);
      // Note: would need to track tail for subsequent messages
      
      ctx.flushMessages();
    }
  }
}
