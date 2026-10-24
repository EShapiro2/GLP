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
import 'package:glp_runtime/runtime/terms.dart' as rt;

import 'ui_runtime/term.dart';

// =============================================================================
// Messages: Main isolate → Agent isolate
// =============================================================================

sealed class ToAgentMsg {}

/// Initialization message — sent as the spawn argument.
class InitAgent extends ToAgentMsg {
  final String agentId;

  /// The path of the one program the agent runs: a directory with a
  /// self.glp, linked whole, or a self-contained module file (TGLP,
  /// def:program).  A program is one compiled value, and no sources are
  /// co-loaded beside it.
  final String program;

  final String rootSelfGlpPath;
  final SendPort replyPort;

  /// Entry-point goal label, e.g. 'agent_init/3', 'agent_init_play/3'.
  final String goalLabel;

  /// Extra arguments inserted between Id and NetIn.
  final List<String> extraArgs;

  /// If true, the isolate waits for a [StartAgent] command before running
  /// GLP initialization.  This allows the main isolate to register all agent
  /// ports first, eliminating message-routing race conditions.
  final bool deferStart;

  InitAgent({
    required this.agentId,
    required this.program,
    required this.rootSelfGlpPath,
    required this.replyPort,
    this.goalLabel = 'agent_init/3',
    this.extraArgs = const [],
    this.deferStart = false,
  });
}

/// The person's act on the agent's screen: a ground term, never text (GSG,
/// Appendix "The Prototype's Screens", and Section 3, "What the super-app
/// grants a mini-app", (ib)).
class UserInput extends ToAgentMsg {
  final GTerm act;
  UserInput(this.act);
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

/// A line of the agent's transcript (AgentRuntime.onOutput): its own notes
/// and, marked `< `, what its program emits to the person, as a terminal
/// shows it.  No screen reads it; a screen takes [AgentNotify].
class AgentOutput extends FromAgentMsg {
  final String agentId;
  final String line;
  AgentOutput(this.agentId, this.line);
}

/// A term the agent's program emits to its person by send_to_user/1, whole
/// (GLP-Spec appendix-guards.tex, "Output to the person": send_to_user(Stream)
/// "emits each element of Stream to the person operating the agent"), as the
/// screen holds a ground term ([screenTermOf]) --- carried as [UserInput]
/// carries the person's act the other way, and handed to the person's bridge
/// (ui_runtime/runtime.dart, [UiRuntime.handle]), which parses no text.
/// Until 2026-10-11 the screen took the transcript's `< ` line and parsed it,
/// and a term whose print did not parse never arrived.
class AgentNotify extends FromAgentMsg {
  final String agentId;
  final GTerm term;
  AgentNotify(this.agentId, this.term);
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
    program: init.program,
    rootSelfGlpPath: init.rootSelfGlpPath,
    goalLabel: init.goalLabel,
    extraArgs: init.extraArgs,
  );

  // Wire callbacks to send messages back to the main isolate: what the
  // program emits to the person as the term it is, and the transcript.
  agent.onUserOutput = (term) {
    init.replyPort.send(AgentNotify(agentId, screenTermOf(term)));
  };

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
  // so no two event cycles of the agent run at once.
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
      await agent.injectUserInput(runtimeTermOf(msg.act));
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

/// The runtime term of a ground term from the screen, as the agent's heap
/// holds it: an atom, an integer or a real a constant, a string the constant
/// of its text --- the one value of an atom of that text, GLP having one kind
/// of constant besides numbers and [] (GLP-Spec appendix-lp.tex, Definition
/// "Logic Programs Syntax"); until 2026-10-10 it was the text in double
/// quotes --- a compound a structure, and a list its '.' cells ending in [],
/// the runtime's [rt.nil].
rt.Term runtimeTermOf(GTerm t) => switch (t) {
      GAtom(:final name) => rt.ConstTerm(name),
      GInt(:final value) => rt.ConstTerm(value),
      GReal(:final value) => rt.ConstTerm(value),
      GString(:final value) => rt.ConstTerm(value),
      GStruct(:final functor, :final args) =>
        rt.StructTerm(functor, [for (final a in args) runtimeTermOf(a)]),
      GList(:final items) => items.reversed.fold<rt.Term>(rt.ConstTerm(rt.nil),
          (tail, item) => rt.StructTerm('.', [runtimeTermOf(item), tail])),
    };

/// The screen's term of a ground term the agent's program emits to its person
/// (send_to_user/1), the inverse of [runtimeTermOf]: an integer a [GInt], a
/// real a [GReal], the empty list a [GList] of nothing, a string the [GAtom]
/// of its characters --- GLP has one string constant (GLP-Spec
/// appendix-lp.tex, Definition "Logic Programs Syntax"), and the screen's
/// [GAtom] and [GString] of one text are that one value --- a list ending in
/// [] a [GList], and any other structure a [GStruct], a list cell ending
/// elsewhere among them, `'.'(H, T)`.  The term arrives whole and no text is
/// read: `form('.', [...])` and `'Hi there'` arrive as written (GLP #3
/// Cowork, 2026-10-11 12:59 UTC, "21:40 AND 03:16, (iii)").  A list's spine is
/// walked, not recursed on.  Throws on what is not a ground term of these
/// kinds.
GTerm screenTermOf(rt.Term t) {
  if (t is rt.ConstTerm) {
    final v = t.value;
    if (v == null || v == rt.nil) return const GList([]);
    if (v is int) return GInt(v);
    if (v is double) return GReal(v);
    if (v is String) return GAtom(v);
    throw ArgumentError('no screen term for the constant $v');
  }
  if (t is rt.StructTerm) {
    if (t.functor == '.' && t.args.length == 2) {
      final items = <GTerm>[];
      rt.Term rest = t;
      while (rest is rt.StructTerm && rest.functor == '.' && rest.args.length == 2) {
        items.add(screenTermOf(rest.args[0]));
        rest = rest.args[1];
      }
      if (rest is rt.ConstTerm && (rest.value == null || rest.value == rt.nil)) {
        return GList(items);
      }
      var cell = screenTermOf(rest);
      for (final item in items.reversed) {
        cell = GStruct('.', [item, cell]);
      }
      return cell;
    }
    return GStruct(t.functor, [for (final a in t.args) screenTermOf(a)]);
  }
  throw ArgumentError('no screen term for $t, which is no ground constant '
      'or structure');
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
