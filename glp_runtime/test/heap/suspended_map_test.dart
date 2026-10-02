/// The suspended map, [GlpRuntime.suspended]: each reader a goal waits on and
/// the goals waiting on it, the runtime's copy of the dGLP configuration's S
/// (IGLP dglp.tex, Definitions "dGLP Configuration" and "dGLP Transition
/// System"), whose Reduce takes out the goals it reactivates, S' = S \
/// {(G, W) : G ∈ R}.  Its one reader is the drain's report of the readers a
/// suspended run waits on ([DrainResult.blockingReaders]).
///
/// A goal woken leaves the map at the cost of its own readers, which an index
/// by goal holds; until 2026-10-02 every wakeup through
/// [GlpRuntime.enqueueReactivatedGoal] --- when_idle, wait/1, the madGLP
/// deliveries --- visited every entry of the map (GLP #3 Cowork, 2026-10-02
/// 08:40 UTC, H).  A goal a commit wakes, which the runner puts in the queue
/// itself, leaves it when the scheduler takes it from the queue; until then it
/// stayed in the map for the rest of the run, and after 30 days of sGLP's
/// social graph the map held 464,267 goals, 631 of them suspended.
///
/// The queue's order, which the heap's activations decide, and what every
/// run computes are unchanged.
library;

import 'dart:async';
import 'dart:collection';
import 'dart:io';
import 'dart:math';

import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/engine_v2/interp.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:test/test.dart';

/// The map as the code before the change kept it: a goal leaves it by a visit
/// to every entry.
class _Reference {
  final Map<int, Set<GoalRef>> map = {};

  void suspend(GoalRef goal, Set<int> readers) {
    for (final r in readers) {
      map.putIfAbsent(r, () => <GoalRef>{}).add(goal);
    }
  }

  void remove(GoalRef goal) {
    final empty = <int>[];
    for (final entry in map.entries) {
      entry.value.remove(goal);
      if (entry.value.isEmpty) empty.add(entry.key);
    }
    for (final k in empty) {
      map.remove(k);
    }
  }
}

/// The map's readers in order, each with its goals in order, a goal written
/// `id@pc`.
List<List<Object>> _entries(Map<int, Set<GoalRef>> map) => [
      for (final e in map.entries)
        [e.key, for (final g in e.value) '${g.id}@${g.pc}']
    ];

final _root = File('../programs/self.glp').absolute.path;

/// gen/2 writes a stream of N integers down to 1; merge/3 merges two streams;
/// total/3 sums one; tick/2 counts down by when_idle, one step each time the
/// machine is idle.
const String _streams = r'''
procedure gen(Integer?, Stream(Integer)).
gen(0, []).
gen(N, [N?|Xs?]) :- N? > 0 | N1 := N? - 1, gen(N1?, Xs).

procedure merge(Stream(Integer)?, Stream(Integer)?, Stream(Integer)).
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
merge(Xs, [], Xs?).
merge([], Ys, Ys?).

procedure total(Stream(Integer)?, Integer?, Integer).
total([X|Xs], A, S?) :- A1 := A? + X?, total(Xs?, A1?, S).
total([], A, A?).

procedure tick(Integer?, Integer).
tick(0, 0).
tick(N, Z?) :- N? > 0, when_idle | N1 := N? - 1, tick(N1?, Z).
''';

GlpEngine _engine() => GlpEngine(rootSelfGlpPath: _root)..loadSource(_streams);

/// Run [goal] traced: its result, and the procedure of each reduction of the
/// program's procedures in the order the run made them.
Future<(ExecutionResult, List<String>)> _traced(String goal) async {
  final engine = _engine()..debugTrace = true;
  final lines = <String>[];
  final r = await runZoned(() => engine.runGoal(goal),
      zoneSpecification: ZoneSpecification(
          print: (self, parent, zone, line) => lines.add(line)));
  const procedures = {'gen', 'merge', 'total', 'tick'};
  return (
    r,
    [
      for (final l in lines)
        if (l.contains(' :- '))
          if (RegExp(r'^[a-z][A-Za-z0-9_]*').stringMatch(l) case final p?)
            if (procedures.contains(p)) p
    ]
  );
}

String _v(Object? term) =>
    '$term'.replaceAllMapped(RegExp(r'^Const\((.*)\)$'), (m) => m[1]!);

void main() {
  test(
      'a goal woken leaves the map, every other entry as it was: the map '
      'as visiting every entry left it, step by step', () {
    final rt = GlpRuntime();
    final ref = _Reference();
    final refQueue = Queue<GoalRef>();
    final rnd = Random(20261002);
    // A pool of variables, some readers shared between goals.
    final pool = <(int, int)>[
      for (var i = 0; i < 24; i++) rt.heap.allocateVariable()
    ];
    var nextId = 1;
    var wakes = 0, commitWakes = 0, taken = 0;

    void suspend(GoalRef goal) {
      final readers = <int>{
        for (var k = 0; k <= rnd.nextInt(3); k++)
          pool[rnd.nextInt(pool.length)].$2
      };
      rt.suspendGoalFCP(
          goalId: goal.id, kappa: goal.pc, readerVarIds: readers);
      ref.suspend(goal, readers);
    }

    for (var step = 0; step < 3000; step++) {
      final op = rnd.nextInt(10);
      if (op < 4) {
        suspend(GoalRef(nextId++, rnd.nextInt(3) * 100));
      } else if (op < 8) {
        // Bind a writer, and put the goals it wakes in the queue: through
        // enqueueReactivatedGoal, or as a commit does, by the queue alone.
        final i = rnd.nextInt(pool.length);
        final acts = rt.heap.bindWriterConst(pool[i].$1, 0);
        final byCommit = op >= 6;
        for (final a in acts) {
          refQueue.add(a);
          if (byCommit) {
            rt.gq.enqueue(a);
          } else {
            rt.enqueueReactivatedGoal(a);
            ref.remove(a);
          }
        }
        if (byCommit) {
          commitWakes += acts.length;
        } else {
          wakes += acts.length;
        }
        pool[i] = rt.heap.allocateVariable();
      } else if (rt.gq.length > 0) {
        // The scheduler takes a goal from the queue; it may wait again, under
        // the same identity.
        final goal = rt.gq.dequeue()!;
        expect(goal, refQueue.removeFirst(), reason: 'step $step');
        rt.goalTaken(goal);
        ref.remove(goal);
        taken++;
        if (rnd.nextBool()) suspend(goal);
      }
      expect(_entries(rt.suspended), _entries(ref.map), reason: 'step $step');
      expect(rt.gq.items.toList(), refQueue.toList(), reason: 'step $step');
    }
    // The run exercised every path.
    expect(wakes, greaterThan(100));
    expect(commitWakes, greaterThan(100));
    expect(taken, greaterThan(100));
  });

  test('a goal waiting on two readers and woken through one leaves both', () {
    final rt = GlpRuntime();
    final (w1, r1) = rt.heap.allocateVariable();
    final (_, r2) = rt.heap.allocateVariable();
    final (_, r3) = rt.heap.allocateVariable();
    rt.suspendGoalFCP(goalId: 1, kappa: 7, readerVarIds: {r1, r2});
    rt.suspendGoalFCP(goalId: 2, kappa: 7, readerVarIds: {r2, r3});
    for (final a in rt.heap.bindWriterConst(w1, 0)) {
      rt.enqueueReactivatedGoal(a);
    }
    expect(rt.gq.items.toList(), [const GoalRef(1, 7)]);
    expect(_entries(rt.suspended), [
      [r2, '2@7'],
      [r3, '2@7'],
    ]);
  });

  group('the order of the reductions and what the run computes', () {
    // The expectations are the runs before the change, at fc0cb914.
    test('consumers posted before their producers', () async {
      final (r, heads) = await _traced(
          'total(C?, 0, S), merge(A?, B?, C), tick(2, T), gen(3, A), gen(2, B)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['S']), '9');
      expect(_v(r.bindings['T']), '0');
      expect(heads, _consumersFirst);
    });

    test('producers posted before their consumers', () async {
      final (r, heads) = await _traced(
          'gen(3, A), gen(2, B), merge(A?, B?, C), total(C?, 0, S), tick(2, T)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['S']), '9');
      expect(_v(r.bindings['T']), '0');
      expect(heads, _producersFirst);
    });

    test('a goal still waiting at the end', () async {
      final (r, heads) = await _traced('tick(2, T), total(C?, 0, S), '
          'merge(A?, B?, C), total(D?, 0, U), gen(2, B), gen(3, A)');
      expect(r.status, ExecutionStatus.suspended, reason: '${r.error}');
      expect(_v(r.bindings['S']), '9');
      expect(_v(r.bindings['T']), '0');
      expect(r.bindings['U'], isNull);
      expect(heads, _oneWaiting);
    });
  });

  group('the map holds the goals suspended, and only them', () {
    test('a run that ends with no goal suspended leaves it empty', () async {
      final engine = _engine();
      final r = await engine.runGoal('total(C?, 0, S), gen(1000, C)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['S']), '500500');
      expect(engine.runtime.suspended, isEmpty,
          reason: 'total/3 waited on the stream again and again, and each '
              'time gen/2 woke it by a commit');
    });

    test(
        'a suspended run reports the readers its goals wait on, and not '
        'those of a goal woken since', () {
      // w/2 waits on X; s/1 assigns X by a commit, which wakes w/2; a second
      // w/2 waits on Y, which nothing assigns.
      final rt = GlpRuntime();
      final image = codeImageFromProgram(GlpCompiler().compile('''
w(a, _).
s(a).
'''));
      final sched = Scheduler(rt: rt, runner: ByteRunner(image));
      Term bound(Object v) {
        final (w, r) = rt.heap.allocateVariable();
        rt.heap.bindWriter(w, ConstTerm(v));
        return VarRef(r);
      }

      void post(String proc, List<Term> args) {
        final id = rt.nextGoalId++;
        rt.setGoalEnv(id,
            CallEnv(args: {for (var i = 0; i < args.length; i++) i: args[i]}));
        rt.gq.enqueue(GoalRef(id, image.entryOffsetOf(proc)!));
      }

      final (wx, rx) = rt.heap.allocateVariable();
      final (_, ry) = rt.heap.allocateVariable();
      post('w/2', [VarRef(rx), bound('c')]);
      post('s/1', [VarRef(wx)]);
      post('w/2', [VarRef(ry), bound('c')]);
      final r = sched.drainWithStatus();
      expect(r.status, ExecutionStatus.suspended);
      expect(r.suspendedGoals, ['w(X1?, c)']);
      expect(r.blockingReaders, {ry},
          reason: 'before the change also X?, whose goal had reduced');
      expect(rt.suspended.keys, [ry]);
    });
  });
}

// The order of the reductions of the program's procedures in each run before
// the change, at fc0cb914.
const _consumersFirst = [
  'gen', 'gen', 'merge', 'gen', 'gen', 'total', 'merge', 'gen', 'gen', //
  'total', 'merge', 'gen', 'total', 'merge', 'total', 'merge', 'total',
  'merge', 'total', 'tick', 'tick', 'tick',
];
const _producersFirst = [
  'gen', 'gen', 'merge', 'total', 'gen', 'gen', 'merge', 'total', 'gen', //
  'gen', 'merge', 'total', 'gen', 'merge', 'total', 'merge', 'total',
  'merge', 'total', 'tick', 'tick', 'tick',
];
const _oneWaiting = _consumersFirst;
