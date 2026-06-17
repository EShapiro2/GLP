/// Isolate protocol for single-window multiagent architecture.
///
/// Defines message types for communication between the main Flutter isolate
/// (which owns the UI) and agent isolates (which run AgentRuntime).
///
/// ## Agent lifecycle
///
/// Each agent isolate follows one of two lifecycles:
///
/// **Immediate start** (`deferStart: false`, the default):
///   1. Main spawns isolate with [InitAgent].
///   2. Isolate creates [AgentRuntime], wires callbacks, sends [AgentReady].
///   3. Isolate immediately runs GLP initialization (`agent.initialize()`).
///   4. Isolate enters command loop (handles [UserInput], [DeliverMad], etc.).
///
///   Used by interactive apps where agents are spawned one at a time and
///   don't need to communicate during initialization.
///
/// **Deferred start** (`deferStart: true`):
///   1. Main spawns isolate with [InitAgent] (`deferStart: true`).
///   2. Isolate creates [AgentRuntime], wires callbacks, sends [AgentReady].
///   3. Isolate enters command loop **without** running GLP initialization.
///   4. Main collects all [AgentReady] messages and registers all ports.
///   5. Main sends [StartAgent] to each isolate.
///   6. Isolate receives [StartAgent], runs GLP initialization, then
///      continues the command loop.
///
///   Used by scripted-play apps (SG madGLP, CSSG madGLP) where all agents
///   are spawned at once.  By deferring GLP execution until all ports are
///   registered, network messages sent during initialization are guaranteed
///   to reach their destination — no race conditions.
library;

import 'dart:async';
import 'dart:isolate';
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/agent_runtime.dart';

// =============================================================================
// Messages: Main isolate → Agent isolate
// =============================================================================

sealed class ToAgentMsg {}

/// Initialization message — sent as the spawn argument.
class InitAgent extends ToAgentMsg {
  final String agentId;
  final List<String> glpSources;

  /// Optional real paths for [glpSources] (same order), so the type checker's
  /// self.glp ancestor-scope resolves shared types at load time.
  final List<String> glpSourcePaths;

  final String rootSelfGlpPath;
  final List<String> friends;
  final SendPort replyPort;

  /// Entry-point goal label, e.g. 'agent_init/3', 'agent_init_play/3'.
  final String goalLabel;

  /// Extra arguments inserted between Id and NetIn.
  final List<String> extraArgs;

  /// Optional project directory for static linking.
  /// When set, each isolate loads the project via loadProject() before
  /// loading glpSources (typically just the madGLP boot source) on top.
  final String? projectDir;

  /// If true, the isolate waits for a [StartAgent] command before running
  /// GLP initialization.  This allows the main isolate to register all agent
  /// ports first, eliminating message-routing race conditions.
  final bool deferStart;

  InitAgent({
    required this.agentId,
    required this.glpSources,
    this.glpSourcePaths = const [],
    required this.rootSelfGlpPath,
    required this.friends,
    required this.replyPort,
    this.goalLabel = 'agent_init/3',
    this.extraArgs = const [],
    this.projectDir,
    this.deferStart = false,
  });
}

/// User typed input in the agent's text field.
class UserInput extends ToAgentMsg {
  final String text;
  UserInput(this.text);
}

/// Incoming MAD message routed from another agent.
class DeliverMad extends ToAgentMsg {
  final String from;
  final Uint8List payload;
  DeliverMad(this.from, this.payload);
}

/// Begin GLP initialization (only used with `deferStart: true`).
class StartAgent extends ToAgentMsg {}

/// Request graceful shutdown.
class DisposeAgent extends ToAgentMsg {}

// =============================================================================
// Messages: Agent isolate → Main isolate
// =============================================================================

sealed class FromAgentMsg {}

/// Agent isolate is up and its command port is ready.  Carries the [SendPort]
/// for sending [ToAgentMsg] instances to this agent.
///
/// With `deferStart: false`, GLP initialization follows immediately.
/// With `deferStart: true`, the isolate waits for [StartAgent] before
/// running GLP initialization.
class AgentReady extends FromAgentMsg {
  final String agentId;
  final SendPort commandPort;
  AgentReady(this.agentId, this.commandPort);
}

/// Output line for display in the agent's panel.
class AgentOutput extends FromAgentMsg {
  final String agentId;
  final String line;
  AgentOutput(this.agentId, this.line);
}

/// Trace log entry.
class AgentLog extends FromAgentMsg {
  final String agentId;
  final String tag;
  final String message;
  AgentLog(this.agentId, this.tag, this.message);
}

/// Outbound MAD message to be routed to another agent.
class AgentSendMad extends FromAgentMsg {
  final String agentId;
  final String to;
  final Uint8List payload;
  AgentSendMad(this.agentId, this.to, this.payload);
}

/// Stats update for the agent's status bar.
class AgentStats extends FromAgentMsg {
  final String agentId;
  final int goals;
  final int heap;
  final int wp;
  final int mp;
  AgentStats(this.agentId, {
    required this.goals,
    required this.heap,
    required this.wp,
    required this.mp,
  });
}

/// Error during initialization or runtime.
class AgentError extends FromAgentMsg {
  final String agentId;
  final String error;
  AgentError(this.agentId, this.error);
}

// =============================================================================
// Agent Isolate Entry Point
// =============================================================================

/// Top-level entry point for agent isolates.
/// Must be top-level (not a closure) for Isolate.spawn().
void agentIsolateEntry(InitAgent init) {
  _runAgent(init);
}

Future<void> _runAgent(InitAgent init) async {
  final commandPort = ReceivePort();
  final agentId = init.agentId;

  final agent = AgentRuntime(
    agentId: agentId,
    glpSources: init.glpSources,
    glpSourcePaths: init.glpSourcePaths,
    rootSelfGlpPath: init.rootSelfGlpPath,
    friends: init.friends,
    goalLabel: init.goalLabel,
    extraArgs: init.extraArgs,
    projectDir: init.projectDir,
  );

  // Wire callbacks to send messages back to the main isolate.
  agent.onOutput = (line) {
    init.replyPort.send(AgentOutput(agentId, line));
  };

  agent.onLog = (tag, message) {
    init.replyPort.send(AgentLog(agentId, tag, message));
  };

  agent.onSendMadMessage = (to, payload) async {
    init.replyPort.send(AgentSendMad(agentId, to, Uint8List.fromList(payload)));
  };

  // Signal ready with our command port.
  init.replyPort.send(AgentReady(agentId, commandPort.sendPort));

  // Immediate start: initialize GLP now (before entering command loop).
  if (!init.deferStart) {
    try {
      await agent.initialize();
      _sendStats(agent, init.replyPort);
    } catch (e) {
      init.replyPort.send(AgentError(agentId, e.toString()));
      commandPort.close();
      return;
    }
  }

  // Listen for commands. Each message is fully awaited before the next,
  // preventing concurrent _runUntilQuiescent() calls.
  await for (final msg in commandPort) {
    if (msg is StartAgent) {
      // Deferred start: initialize GLP now (all ports are registered).
      try {
        await agent.initialize();
        _sendStats(agent, init.replyPort);
      } catch (e) {
        init.replyPort.send(AgentError(agentId, e.toString()));
        commandPort.close();
        break;
      }
    } else if (msg is UserInput) {
      await agent.injectUserInput(msg.text);
      _sendStats(agent, init.replyPort);
    } else if (msg is DeliverMad) {
      await agent.onMadMessageReceived(msg.from, msg.payload);
      _sendStats(agent, init.replyPort);
    } else if (msg is DisposeAgent) {
      agent.dispose();
      commandPort.close();
      break;
    }
  }
}

void _sendStats(AgentRuntime agent, SendPort replyPort) {
  agent.updateStats();
  replyPort.send(AgentStats(
    agent.agentId,
    goals: agent.goalCount,
    heap: agent.heapVars,
    wp: agent.wpSize,
    mp: agent.mpSize,
  ));
}
