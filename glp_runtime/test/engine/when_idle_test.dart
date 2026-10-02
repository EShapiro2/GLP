/// when_idle (GLP-Spec appendix-guards.tex at e3a8d52, the time guards):
/// "when_idle suspends while the machine has a Reduce or a Communicate to
/// make, and succeeds when it has none."  The machine's idleness decides it:
/// its goal queue empty, the goal asking having been taken from it, and in
/// madGLP its outbox too (IGLP eadadcd, Implementation Notes, "The when_idle
/// Guard": "a queued outbound message is a Communicate still to make").  A
/// goal that suspends on it is re-tried whenever the machine is idle, one at
/// a time, the one that has waited longest first.
///
/// Fixture: programs/tests/typed/when_idle.glp; the madGLP agents' program
/// is [_mad] below.
library;

import 'dart:async';
import 'dart:io';
import 'dart:typed_data';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';
import 'package:glp_runtime/multiagent/mad_context.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

/// The madGLP agents.  a_init/2 cold-calls b and leaves report/0 waiting on
/// when_idle; c_init/2 cold-calls b and leaves sender/1 waiting on when_idle,
/// which cold-calls b again when it passes, and waiter/1, which waits on
/// sender/1's assignment and then on when_idle.  b_init/2 reads the first
/// message on its network input and sends nothing back.
const String _mad = r'''
Go ::= go.

procedure a_init(_?, _?).
a_init(_, _) :- send_to_net([msg(b, ping)]), report.

procedure report.
report :- when_idle | send_to_user([idle]).

procedure c_init(_?, _?).
c_init(_, _) :- send_to_net([msg(b, ping)]), sender(X), waiter(X?).

procedure sender(Go).
sender(go) :- when_idle | send_to_net([msg(b, pong)]), send_to_user([sender]).

procedure waiter(Go?).
waiter(go) :- when_idle | send_to_user([waiter]).

procedure b_init(_?, _?).
b_init(_, [msg(_, M) | _]) :- send_to_user([got(M?)]).
''';

/// An agent of [_mad] started at [goal], what it sends to its person and
/// its Sends recorded in [events] in the order they happen --- `out <line>`
/// and `send <destination>` --- and its messages kept in [sent], delivered to
/// no one unless the test delivers them.  No message reaches it from outside.
AgentRuntime _agent(String id, String goal, List<String> events,
    List<(String, Uint8List)> sent) {
  final agent = AgentRuntime(
    agentId: id,
    glpSources: const [_mad],
    rootSelfGlpPath: File('../programs/self.glp').absolute.path,
    goalLabel: goal,
  );
  agent.onOutput = (line) {
    if (line.startsWith('< ')) events.add('out ${line.substring(2)}');
  };
  agent.onSendMadMessage = (to, payload) async {
    events.add('send $to');
    sent.add((to, Uint8List.fromList(payload)));
  };
  return agent;
}

GlpEngine _engine() {
  final engine =
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  expect(
      engine.loadFile(
          File('../programs/tests/typed/when_idle.glp').absolute.path),
      isTrue);
  return engine;
}

/// The value a binding prints as: `idle` for `Const(idle)`.
String _v(Object? term) =>
    '$term'.replaceAllMapped(RegExp(r'^Const\((.*)\)$'), (m) => m[1]!);

/// Run [goal] with the scheduler's trace on, and the reductions it prints,
/// one line each, in the order of the run.
Future<(ExecutionResult, List<String>)> _traced(
    GlpEngine engine, String goal) async {
  final lines = <String>[];
  engine.debugTrace = true;
  final result = await runZoned(() => engine.runGoal(goal),
      zoneSpecification: ZoneSpecification(
          print: (self, parent, zone, line) => lines.add(line)));
  return (result, lines.where((l) => l.contains(' :- ')).toList());
}

void main() {
  group('when_idle succeeds only when the machine has no Reduce to make', () {
    test('m/1 reduces after every other goal of its conjunction', () async {
      final r =
          await _engine().runGoal('m(X), count(1000, Z), probe(X?, Z?, O)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['X']), 'idle');
      expect(_v(r.bindings['Z']), '0');
      // probe/3, woken by X, finds count/2 at rest.
      expect(_v(r.bindings['O']), 'after');
    });

    test('the control: the same clause without the guard reduces at once',
        () async {
      final r =
          await _engine().runGoal('n(X), count(1000, Z), probe(X?, Z?, O)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['O']), 'before');
    });

    test('in the order of the run, m/1 is reduced after the last count/2',
        () async {
      final (r, reductions) =
          await _traced(_engine(), 'm(X), count(1000, Z), probe(X?, Z?, O)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      final counts = [
        for (var i = 0; i < reductions.length; i++)
          if (reductions[i].startsWith('count(')) i
      ];
      final ms = [
        for (var i = 0; i < reductions.length; i++)
          if (reductions[i].startsWith('m(')) i
      ];
      final probes = [
        for (var i = 0; i < reductions.length; i++)
          if (reductions[i].startsWith('probe(')) i
      ];
      expect(counts, hasLength(1001), reason: reductions.join('\n'));
      expect(ms, hasLength(1), reason: reductions.join('\n'));
      expect(probes, hasLength(1), reason: reductions.join('\n'));
      expect(ms.single, greaterThan(counts.last));
      expect(probes.single, greaterThan(ms.single));
    });

    test('alone in the machine, it succeeds at once', () async {
      final r = await _engine().runGoal('m(X)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['X']), 'idle');
    });

    test('placed after the work, it still waits for the work to rest',
        () async {
      final r =
          await _engine().runGoal('probe(X?, Z?, O), count(1000, Z), m(X)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['O']), 'after');
    });

    test('a goal suspended on a reader has no Reduce to make', () async {
      // probe/3 waits on X, and Z is never assigned: the queue empties with
      // probe/3 suspended, m/1 is re-tried and assigns X, and probe/3 finds
      // Z unassigned.
      final r = await _engine().runGoal('m(X), probe(X?, Z?, O)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['X']), 'idle');
      expect(_v(r.bindings['O']), 'before');
    });
  });

  group('two goals waiting on when_idle', () {
    test('are re-tried one at a time, the one that waited longest first',
        () async {
      // m/1 and later/2 both wait.  Re-tried first, m/1 assigns X; later/2 is
      // re-tried only when the queue empties again, and finds X assigned.
      // Were both put back at once, m/1 would find later/2 in the queue and
      // wait again, and later/2 would find X unassigned.
      final r = await _engine().runGoal('m(X), later(X?, O), count(10, W)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['X']), 'idle');
      expect(_v(r.bindings['W']), '0');
      expect(_v(r.bindings['O']), 'after');
    });
  });

  group('in madGLP, a queued outbound message is a Communicate still to make',
      () {
    test(
        'a goal waiting on it does not pass while a message is in the '
        'outbox, and passes after the Sends with no message arriving',
        () async {
      // One event, a's start: the drain leaves report/0 waiting with the
      // cold call to b in the outbox, the flush sends it, and the drain after
      // the flush re-tries report/0, which passes.  Nothing reaches a.  It
      // passed before the flush when the queue alone decided, and after a's
      // next incoming message when nothing re-drained after the flush.
      final events = <String>[];
      final sent = <(String, Uint8List)>[];
      await _agent('a', 'a_init/2', events, sent).initialize();
      expect(events, ['send b', 'out idle']);

      // The message a sent is b's, and b reads it.
      final bEvents = <String>[];
      final b = _agent('b', 'b_init/2', bEvents, <(String, Uint8List)>[]);
      await b.initialize();
      expect(sent.map((s) => s.$1), ['b']);
      await b.onMadMessageReceived('a', sent.single.$2);
      expect(bEvents, ['out got(ping)']);
    });

    test(
        'goals waiting on it are re-tried one at a time, each after the '
        'messages before it are sent', () async {
      // sender/1 passes after the cold call is sent, and cold-calls b
      // again; waiter/1, woken by its assignment, waits on when_idle until
      // that second message is sent too, the same event's third drain.
      final events = <String>[];
      final sent = <(String, Uint8List)>[];
      await _agent('c', 'c_init/2', events, sent).initialize();
      expect(events, ['send b', 'out sender', 'send b', 'out waiter']);
    });

    test(
        'a held message is no Communicate to make: Send is not enabled for '
        'it until authorise_link/2 releases it', () {
      // IGLP, Definition madGLP Send: "enabled when (m, q) ∈ M_p is unsent
      // and not held".
      final rt = GlpRuntime();
      final ctx = MadContext(agentId: 'a', runtime: rt);
      rt.madContext = ctx;
      expect(rt.isIdle, isTrue);
      final held = OutboundMessage(
          destination: 'b',
          type: MessageType.assignment,
          payload: const [1],
          held: true);
      ctx.mp.add(held);
      expect(rt.isIdle, isTrue);
      ctx.mp.add(OutboundMessage(
          destination: 'b', type: MessageType.assignment, payload: const [2]));
      expect(rt.isIdle, isFalse);
      expect(ctx.mp.poll('b')!.payload, [2]);
      expect(rt.isIdle, isTrue);
      held.held = false; // released
      expect(rt.isIdle, isFalse);
    });

    test('in an agent isolate too: the goal passes after the Sends',
        () async {
      // a's start is the only event a has: b sends nothing back.
      final config = BootLoader().load(
          'procedure boot.\nboot :- a_init(a, _)@a, b_init(b, _)@b.\n$_mad');
      config.rootSelfGlpPath = File('../programs/self.glp').absolute.path;
      final manager = IsolateManager();
      try {
        await manager.boot(config);
        manager.start();
        await manager.settle();
        expect(manager.outputOf('a'), ['idle']);
        expect(manager.outputOf('b'), ['got(ping)']);
      } finally {
        await manager.shutdown();
      }
    }, timeout: Timeout(Duration(seconds: 60)));
  });

  group('the declaration', () {
    test('when_idle takes no argument: when_idle/1 is not declared', () {
      final engine =
          GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
      expect(
          () => engine.loadSource('''
procedure w(Integer).
w(X?) :- when_idle(1) | X = 1.
'''),
          throwsA(predicate(
              (e) => '$e'.contains('Undefined procedure: when_idle/1'))));
    });
  });
}
