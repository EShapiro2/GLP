/// Isolate protocol for single-window multiagent architecture.
///
/// Defines message types for communication between the main Flutter isolate
/// (which owns the UI) and agent isolates (which run AgentRuntime).
///
/// The main isolate spawns one Dart isolate per agent. Each agent isolate
/// creates an AgentRuntime, wires its callbacks to send messages back via
/// SendPort, and listens for commands (user input, network messages).
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
  final String glpSource;
  final List<String> friends;
  final SendPort replyPort;

  InitAgent({
    required this.agentId,
    required this.glpSource,
    required this.friends,
    required this.replyPort,
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

/// Request graceful shutdown.
class DisposeAgent extends ToAgentMsg {}

// =============================================================================
// Messages: Agent isolate → Main isolate
// =============================================================================

sealed class FromAgentMsg {}

/// Agent has initialized and is ready. Carries the command port for sending
/// ToAgentMsg instances to this agent.
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
    glpSource: init.glpSource,
    friends: init.friends,
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

  // Initialize runtime.
  try {
    await agent.initialize();
    _sendStats(agent, init.replyPort);
  } catch (e) {
    init.replyPort.send(AgentError(agentId, e.toString()));
    commandPort.close();
    return;
  }

  // Listen for commands. Each message is fully awaited before the next,
  // preventing concurrent _runUntilQuiescent() calls.
  await for (final msg in commandPort) {
    if (msg is UserInput) {
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
