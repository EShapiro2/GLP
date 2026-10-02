/// A goal that fails FAILS alone; it does not end the computation.
///
/// IGLP gives a reduction exactly three outcomes — succeeds, suspends with a
/// suspension set, or fails — and the dGLP and madGLP Reduce transactions each
/// put a failed goal in F and continue with the remainder of the queue. No
/// transaction ends a computation.
///
/// The first group reaches a failure through a body kernel that aborts: "A body
/// kernel whose precondition fails --- a zero divisor, an argument out of its
/// domain --- aborts" (GLP-Spec appendix-guards.tex, 22a8e17), and the goal
/// whose clause called it fails.  `X := sqrt(-1)` reaches '_sqrt', which
/// aborts: the root self.glp guards no domain of sqrt, ln, log, asin or acos
/// (GLP #3 Cowork, 2026-10-02 17:12 UTC, stop 3).  Until 2026-10-02 the root
/// kept a domain-error clause for each of the five, calling abort/1, which is
/// undefined, so that a domain error failed as a goal with no procedure; such
/// a goal fails as it did, and the group's last test posts one.
library;

import 'dart:io';
import 'dart:math' as math;

import 'package:glp_runtime/bytecode/opcodes.dart' as op;
import 'package:glp_runtime/bytecode/runner.dart' show BytecodeProgram, CallEnv;
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/engine_v2/interp.dart'
    show ByteRunner, codeImageFromProgram;
import 'package:glp_runtime/runtime/machine_state.dart' show GoalRef;
import 'package:glp_runtime/runtime/runtime.dart' show GlpRuntime;
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' show ConstTerm;
import 'package:test/test.dart';

void main() {
  GlpEngine fresh() =>
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);

  group('a goal that fails', () {
    test('fails without dropping its siblings in the resolvent', () async {
      final engine = fresh();
      // One clause, two body goals: the first aborts in '_sqrt', the second
      // is ordinary arithmetic.
      engine.loadSource('''
procedure probe(Integer, Integer).
probe(X?, Y?) :- X := sqrt(-1), Y := 2+2.
''');

      final result = await engine.runGoal('probe(A, B)');

      expect(result.status, ExecutionStatus.failed,
          reason: 'the goal whose kernel aborted joins F');
      expect(result.bindings['A'], isNull,
          reason: 'the failed square root binds nothing');
      expect(result.bindings['B'].toString(), contains('4'),
          reason: 'the sibling goal kept reducing — F does not end the agent');
    });

    test('joins the runtime failed set, carrying the call', () async {
      final engine = fresh();
      engine.loadSource('''
procedure probe(Integer).
probe(X?) :- X := sqrt(-1).
''');

      await engine.runGoal('probe(A)');

      expect(engine.runtime.failedGoals, hasLength(1));
      expect(engine.runtime.failedGoals.single, startsWith(':='),
          reason: 'the goal that failed is the := whose kernel aborted');
      expect(engine.runtime.failedGoals.single, contains('sqrt(-1)'),
          reason: 'the call carries its arguments, the whole of the fault');
    });

    test('ordinary clause-selection failure also leaves siblings running',
        () async {
      final engine = fresh();
      // No undefined procedure here: pick/2 is declared and defined, and no
      // clause matches `zzz`. Fail is Fail whatever produced it — the queue
      // advances, F takes the goal, and the run goes on.
      engine.loadSource('''
procedure pick(Constant?, Constant).
pick(a, one).
pick(b, two).
''');

      final result = await engine.runGoal('pick(zzz, X), Y := 2+2');

      expect(result.status, ExecutionStatus.failed);
      expect(result.bindings['X'], isNull);
      expect(result.bindings['Y'].toString(), contains('4'),
          reason: 'the conjunct after the failed one still reduced');
    });

    test('a failed conjunct does not drop the conjuncts after it', () async {
      final engine = fresh();
      engine.loadSource('''
procedure probe(Integer).
probe(X?) :- X := sqrt(-1).
''');

      final result = await engine.runGoal('probe(A), B := 2+2, C := 3+3');

      expect(result.status, ExecutionStatus.failed);
      expect(result.bindings['A'], isNull);
      expect(result.bindings['B'].toString(), contains('4'));
      expect(result.bindings['C'].toString(), contains('6'),
          reason: 'every conjunct after the failure ran, not just the next');
    });

    test('a later goal still runs after one has failed', () async {
      final engine = fresh();
      engine.loadSource('''
procedure bad(Integer).
bad(X?) :- X := sqrt(-1).

procedure good(Integer).
good(Y?) :- Y := 2+2.
''');

      final failed = await engine.runGoal('bad(A)');
      expect(failed.status, ExecutionStatus.failed);

      final ok = await engine.runGoal('good(B)');
      expect(ok.succeeded, isTrue,
          reason: 'the agent goes on after a failed goal');
      expect(ok.bindings['B'].toString(), contains('4'));
    });

    test('a spawned goal with no procedure fails, and its siblings run', () {
      // p/0 spawns nope/0, which nothing defines --- a codeless name of the
      // artefact with no kernel and no root runner --- and q/0, which
      // reduces.  Posted directly: no clause of the root self.glp calls an
      // undefined procedure now, and the checker refuses a program that does.
      final rt = GlpRuntime();
      final image = codeImageFromProgram(BytecodeProgram([
        op.Label('p/0'),
        op.ClauseTry(),
        op.Commit(),
        op.Spawn('nope/0', 0),
        op.Spawn('q/0', 0),
        op.Proceed(),
        op.Label('q/0'),
        op.ClauseTry(),
        op.Commit(),
        op.Proceed(),
      ]));
      final sched = Scheduler(rt: rt, runner: ByteRunner(image));
      final id = rt.nextGoalId++;
      rt.setGoalEnv(id, CallEnv());
      rt.gq.enqueue(GoalRef(id, image.entryOffsetOf('p/0')!));

      final result = sched.drainWithStatus();

      expect(result.status, ExecutionStatus.failed);
      expect(rt.failedGoals, ['nope/0'],
          reason: 'the goal with no procedure joins F');
      expect(result.goalsRun, 2,
          reason: 'p, and q after the failed spawn: the parent went on');
    });
  });

  // A body kernel either succeeds or aborts (GLP-Spec appendix-guards), and the
  // root self.glp no longer guards a divisor against zero: `X := 1/0` reaches
  // '_div', which aborts, and the goal that called it fails (round three, B3;
  // GLP #3 Cowork, 2026-10-02 15:31 UTC).
  group('a body kernel that aborts', () {
    test('a zero divisor aborts in the kernel, and the siblings run on',
        () async {
      final engine = fresh();
      engine.loadSource('''
procedure probe(Number, Integer, Integer, Integer).
probe(A?, B?, C?, D?) :- A := 1/0, B := 7 // 0, C := 7 mod 0, D := 2+2.
''');

      final result = await engine.runGoal('probe(A, B, C, D)');

      expect(result.status, ExecutionStatus.failed);
      expect(result.bindings['A'], isNull);
      expect(result.bindings['B'], isNull);
      expect(result.bindings['C'], isNull);
      expect(result.bindings['D'].toString(), contains('4'),
          reason: 'the conjunct after the aborted ones still reduced');
      expect(engine.runtime.failedGoals, hasLength(3));
      expect(engine.runtime.failedGoals.where((g) => g.startsWith('abort(')),
          isEmpty,
          reason: 'no domain-error clause calls abort/1 for a zero divisor');
      expect(engine.runtime.failedGoals.every((g) => g.startsWith(':=')),
          isTrue,
          reason: 'the goal that failed is the := whose kernel aborted');
    });

    test('=.. on the empty list aborts in the kernel', () async {
      final engine = fresh();
      engine.loadSource('''
procedure comp(_).
comp(T?) :- T =.. [].
''');

      final result = await engine.runGoal('comp(T)');

      expect(result.status, ExecutionStatus.failed);
      expect(result.bindings['T'], isNull);
      expect(engine.runtime.failedGoals.single, startsWith('=..'),
          reason: "=..'s [] clause hands [] to '_list_to_tuple', which aborts");
    });

    // The five math functions with a domain: the root's clause guards only
    // that the argument is a number and calls the kernel, which aborts out
    // of the domain (GLP #3 Cowork, 2026-10-02 17:12 UTC, stop 3).  Until
    // 2026-10-02 each kept its domain in the kernel clause's guard and a
    // clause `_ := f(X) :- number(X?), <out of domain> | abort(...)` beside
    // it, abort/1 undefined.  Deleting those clauses alone would have left a
    // number out of the domain to the `otherwise` clause, which evaluates
    // the number and posts the same := again, for ever: `X := sqrt(-1)` ran
    // to the cycle limit.
    test('each out of its domain aborts in its kernel, nothing re-posting',
        () async {
      const cases = {
        'sqrt(-1)': '_sqrt',
        'sqrt(-0.5)': '_sqrt',
        'ln(0)': '_ln',
        'ln(-1)': '_ln',
        'log(0)': '_log10',
        'log(-5)': '_log10',
        'asin(2)': '_asin',
        'asin(-2)': '_asin',
        'acos(1.5)': '_acos',
        'acos(-3)': '_acos',
      };
      for (final e in cases.keys) {
        final engine = fresh();
        final result = await engine.runGoal('X := $e');
        expect(result.status, ExecutionStatus.failed,
            reason: '$e fails, and is not capped by a re-posting otherwise');
        expect(result.bindings['X'], isNull, reason: e);
        expect(engine.runtime.failedGoals, hasLength(1), reason: e);
        expect(engine.runtime.failedGoals.single, startsWith(':='),
            reason: '$e: the := whose kernel ${cases[e]} aborted');
        expect(engine.runtime.failedGoals.single, contains(e), reason: e);
      }
    });

    test('an argument that is an expression is evaluated first, then aborts',
        () async {
      final engine = fresh();
      final result = await engine.runGoal('X := sqrt(2 - 6)');
      expect(result.status, ExecutionStatus.failed);
      expect(result.bindings['X'], isNull);
      expect(engine.runtime.failedGoals, hasLength(1));
    });

    test('in its domain each gives its value, the boundaries included',
        () async {
      final cases = <String, double>{
        'sqrt(4)': 2.0,
        'sqrt(0)': 0.0,
        'ln(1)': 0.0,
        'log(100)': 2.0,
        'asin(1)': math.pi / 2,
        'asin(-1)': -math.pi / 2,
        'asin(0.5)': math.asin(0.5),
        'acos(1)': 0.0,
        'acos(-1)': math.pi,
      };
      for (final e in cases.entries) {
        final engine = fresh();
        final result = await engine.runGoal('X := ${e.key}');
        expect(result.succeeded, isTrue, reason: e.key);
        final v = engine.runtime.heap.dereference(result.bindings['X']!);
        expect(v, isA<ConstTerm>(), reason: e.key);
        expect(((v as ConstTerm).value as num).toDouble(),
            closeTo(e.value, 1e-12),
            reason: e.key);
      }
    });
  });
}
