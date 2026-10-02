/// A goal with no procedure FAILS; it does not end the computation.
///
/// IGLP gives a reduction exactly three outcomes — succeeds, suspends with a
/// suspension set, or fails — and the dGLP and madGLP Reduce transactions each
/// put a failed goal in F and continue with the remainder of the queue. No
/// transaction ends a computation.
///
/// The case that reaches this is `abort/1`: it is undefined, so each of the
/// seven `:=` domain errors the root self.glp keeps --- sqrt's, ln's, log's,
/// asin's and acos's --- calls a procedure that does not exist. Its argument
/// names the fault, so the diagnostic carries the call and not the signature
/// alone. A zero divisor no longer reaches it: `'_div'`, `'_idiv'` and `'_mod'`
/// abort on one themselves (GLP-Spec appendix-guards: a body kernel "either
/// succeed[s] or abort[s]"), which the last group tests.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

void main() {
  GlpEngine fresh() =>
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);

  group('a goal with no procedure', () {
    test('fails without dropping its siblings in the resolvent', () async {
      final engine = fresh();
      // One clause, two body goals: the first calls abort/1 through the
      // domain-error clause of sqrt, the second is ordinary arithmetic.
      engine.loadSource('''
procedure probe(Integer, Integer).
probe(X?, Y?) :- X := sqrt(-1), Y := 2+2.
''');

      final result = await engine.runGoal('probe(A, B)');

      expect(result.status, ExecutionStatus.failed,
          reason: 'the goal that has no procedure joins F');
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
      expect(engine.runtime.failedGoals.single, startsWith('abort('));
      expect(engine.runtime.failedGoals.single,
          contains('Square root of negative number'),
          reason: 'the argument is the whole content of the fault');
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
  });
}
