/// Agents in isolates, as the app hosts them (`lib/isolate_protocol.dart`), for the tests that run them: each agent's command port, its transcript and its notes, the MAD messages routed between them, and the waits, on events alone and on no clock.
///
/// An agent works on each command it is sent --- its start, a person's input, a delivered MAD message --- one at a time, and reports [AgentStats] when it has done one (IGLP, Implementation Notes, "Event-driven execution": on each event the agent "reduces until quiescent ... and then performs its Sends"); the Sends it makes reach this port before the stats do, since a port delivers in order.  So the agents are quiescent when every command sent has been reported done: none will act again until it is sent something.  An agent's start that fails reports [AgentError] and ends; a throw it does not catch, and an exit, reach its isolate's `onError` and `onExit`.  Each is a fault, and fails every wait.
///
/// [until] waits for a condition on what the agents reported, and ends on the event that makes it hold, on a fault, or on quiescence with it not holding, which nothing that is not sent will change; [settle] waits for quiescence.  Until 2026-11-02 each test polled its condition every 50 ms until a deadline of 12 to 20 seconds and then checked it, so its verdict depended on how loaded the machine was: `mad_w_clean_test.dart` failed in the full suite on main 8ff18057 and passed alone (IGLP-coop).  A run that never settles is the suite's watchdog's to report (`test/run_all_tests.sh`, Section Q).
library;

import 'dart:async';
import 'dart:isolate';

import 'package:glp_multiagent/isolate_protocol.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

class IsolateAgents {
  final ReceivePort _reply = ReceivePort();
  final Map<String, ReceivePort> _events = {};
  final Map<String, Isolate> _isolates = {};

  /// Each agent's command port, once it is ready.
  final Map<String, SendPort> ports = {};

  /// Each agent's transcript ([AgentOutput]), its notes to its person ([AgentNotify]) and its log ([AgentLog]), in order.
  final Map<String, List<String>> out = {};
  final Map<String, List<GTerm>> notes = {};
  final Map<String, List<String>> logs = {};

  /// The agents that have reported their first stats: started, the initial run included.
  final Set<String> started = {};

  /// Called with each note an agent sends its person, as it arrives: a test's bridge takes it there.
  void Function(String id, GTerm term)? onNote;

  /// What the agents reported as faults, in order.
  final List<String> faults = [];

  final Map<String, int> _owed = {};
  final Map<String, int> _done = {};
  final List<(String, bool Function(), Completer<void>)> _waits = [];

  IsolateAgents() {
    _reply.listen(_take);
  }

  /// Spawn [id] running [program] under the root [rootSelf].  Its readiness is owed, and, without [deferStart], its start, which it makes at once.
  Future<void> spawn(String id, String program, String rootSelf,
      {bool deferStart = true}) async {
    out[id] = [];
    notes[id] = [];
    logs[id] = [];
    _owe(id, deferStart ? 1 : 2);
    final events = ReceivePort();
    _events[id] = events;
    events.listen((e) => _fault(id,
        e is List ? 'its isolate threw: ${e.first}' : 'its isolate exited'));
    _isolates[id] = await Isolate.spawn(
      agentIsolateEntry,
      InitAgent(
        agentId: id,
        program: program,
        rootSelfGlpPath: rootSelf,
        replyPort: _reply.sendPort,
        deferStart: deferStart,
      ),
      onError: events.sendPort,
      onExit: events.sendPort,
    );
  }

  /// Start [id], spawned with its start deferred.
  void start(String id) => _send(id, StartAgent());

  /// Hand [id] its person's act [act].
  void input(String id, GTerm act) => _send(id, UserInput(act));

  /// Whether [id]'s transcript has a line containing [needle].
  bool has(String id, String needle) =>
      out[id]!.any((l) => l.contains(needle));

  /// Wait until [holds] does, which [what] names: it ends when it holds, and fails on a fault, or on quiescence with it not holding.
  Future<void> until(String what, bool Function() holds) {
    final c = Completer<void>();
    _waits.add((what, holds, c));
    _check();
    return c.future;
  }

  /// Wait until every agent is ready.
  Future<void> ready() =>
      until('every agent ready', () => ports.length == _owed.length);

  /// Wait until the agents are quiescent: every command sent them is done.
  Future<void> settle() => until('quiescence', () => _quiescent);

  /// Dispose of every agent and close the ports.
  void dispose() {
    for (final e in _events.values) {
      e.close();
    }
    for (final p in ports.values) {
      p.send(DisposeAgent());
    }
    for (final i in _isolates.values) {
      i.kill(priority: Isolate.beforeNextEvent);
    }
    _reply.close();
  }

  void _send(String id, Object msg) {
    final port = ports[id];
    if (port == null) {
      _fault(id, 'sent a command before it was ready');
      return;
    }
    _owe(id, 1);
    port.send(msg);
  }

  void _owe(String id, int n) => _owed[id] = (_owed[id] ?? 0) + n;

  void _did(String id) => _done[id] = (_done[id] ?? 0) + 1;

  bool get _quiescent =>
      _owed.keys.every((id) => (_done[id] ?? 0) >= _owed[id]!);

  void _take(dynamic msg) {
    if (msg is AgentReady) {
      ports[msg.agentId] = msg.commandPort;
      _did(msg.agentId);
    } else if (msg is AgentOutput) {
      out[msg.agentId]?.add(msg.line);
    } else if (msg is AgentNotify) {
      notes[msg.agentId]?.add(msg.term);
      onNote?.call(msg.agentId, msg.term);
    } else if (msg is AgentLog) {
      logs[msg.agentId]?.add('[${msg.tag}] ${msg.message}');
    } else if (msg is AgentSendMad) {
      final to = ports[msg.to];
      if (to == null) {
        _fault(msg.agentId, 'sent a MAD message to ${msg.to}, which is no agent here');
      } else {
        _owe(msg.to, 1);
        to.send(DeliverMad(msg.agentId, msg.payload));
      }
    } else if (msg is AgentStats) {
      started.add(msg.agentId);
      _did(msg.agentId);
    } else if (msg is AgentError) {
      out[msg.agentId]?.add('[ERROR] ${msg.error}');
      _did(msg.agentId);
      _fault(msg.agentId, msg.error);
    }
    _check();
  }

  void _fault(String id, String error) {
    faults.add('$id: $error');
    _check();
  }

  void _check() {
    _waits.removeWhere((w) {
      final (what, holds, c) = w;
      if (faults.isNotEmpty) {
        c.completeError(StateError('waiting for $what, an agent faulted: '
            '${faults.first}'));
        return true;
      }
      if (holds()) {
        c.complete();
        return true;
      }
      if (_quiescent) {
        c.completeError(StateError('the agents are quiescent and $what '
            'does not hold'));
        return true;
      }
      return false;
    });
  }
}
