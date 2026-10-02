import 'package:test/test.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/engine_v2/interp.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart';

/// What the drains report --- the run's status, how many goals it ran, the
/// goals left suspended and the readers they wait on, the goals that failed
/// --- is what it was when they kept a list of the goals they ran (GLP #3
/// Cowork, 2026-10-02 08:40 UTC, H).  [DrainResult.goalsRun] counts them,
/// where `goalsRan` held the id of every goal tried, which
/// [Scheduler.drainWithStatus] kept for the whole of a REPL goal's run and
/// [Scheduler.drainAsyncWithStatus] copied: eight bytes a try, gigabytes
/// over a long run.  Every expectation below is what the drains reported
/// before the change, at fc0cb914.
void main() {
  /// A queue of [n] goals of `p :- true.`, each reducing once and ending.
  (Scheduler, GlpRuntime) queued(int n) {
    final rt = GlpRuntime();
    final image = codeImageFromProgram(
        BytecodeProgram([Label('p/0'), ClauseTry(), Commit(), Proceed()]));
    final sched = Scheduler(rt: rt, runner: ByteRunner(image));
    final entry = image.entryOffsetOf('p/0')!;
    for (var i = 1; i <= n; i++) {
      rt.setGoalEnv(i, CallEnv());
      rt.gq.enqueue(GoalRef(i, entry));
    }
    return (sched, rt);
  }

  /// A queue of [n] goals of `loop :- true | loop.`, which never end.
  (Scheduler, GlpRuntime) loops(int n) {
    final rt = GlpRuntime();
    final image = codeImageFromProgram(BytecodeProgram(
        [Label('loop/0'), ClauseTry(), Commit(), Requeue('loop/0', 0)]));
    final sched = Scheduler(rt: rt, runner: ByteRunner(image));
    for (var i = 1; i <= n; i++) {
      rt.gq.enqueue(GoalRef(i, image.entryOffsetOf('loop/0')!));
    }
    return (sched, rt);
  }

  /// w/2 waits on an unbound first argument and fails on anything but a; p/2
  /// spawns q and r, which reduce.  [post] queues a goal; [bound] is the
  /// reader of a writer assigned [value].
  ({
    Scheduler sched,
    GlpRuntime rt,
    Term Function(Object value) bound,
    void Function(String proc, List<Term> args) post,
  }) program() {
    final rt = GlpRuntime();
    final image = codeImageFromProgram(GlpCompiler().compile('''
p(X, Y) :- q(X?), r(Y?).
q(_).
r(_).
w(a, _).
'''));
    final sched = Scheduler(rt: rt, runner: ByteRunner(image));
    var next = 1;
    Term bound(Object value) {
      final (writer, reader) = rt.heap.allocateVariable();
      rt.heap.bindWriter(writer, ConstTerm(value));
      return VarRef(reader);
    }

    void post(String proc, List<Term> args) {
      rt.setGoalEnv(next,
          CallEnv(args: {for (var i = 0; i < args.length; i++) i: args[i]}));
      rt.gq.enqueue(GoalRef(next++, image.entryOffsetOf(proc)!));
    }

    return (sched: sched, rt: rt, bound: bound, post: post);
  }

  group('the goals run are counted, and the status is the same', () {
    test('drainWithStatus: capped at its cap, then the rest', () {
      final (sched, rt) = queued(2500);
      final first = sched.drainWithStatus();
      expect(first.status, ExecutionStatus.capped);
      expect(first.goalsRun, 1000);
      expect(rt.gq.length, 1500);
      final rest = sched.drainWithStatus(maxCycles: 5000);
      expect(rest.status, ExecutionStatus.succeeded);
      expect(rest.goalsRun, 1500);
      expect(rt.gq.length, 0);
    });

    test('drainToQuiescence: the goals of all its drains', () {
      for (final chunk in [1000, 7]) {
        final (sched, rt) = queued(2500);
        final r = sched.drainToQuiescence(chunk: chunk);
        expect(r.status, ExecutionStatus.succeeded, reason: 'chunk $chunk');
        expect(r.goalsRun, 2500, reason: 'chunk $chunk');
        expect(rt.gq.length, 0, reason: 'chunk $chunk');
      }
    });

    test('drainToQuiescence: its net, and not a goal more', () {
      final (sched, rt) = loops(1);
      final r = sched.drainToQuiescence(maxCycles: 100);
      expect(r.status, ExecutionStatus.capped);
      expect(r.goalsRun, 100);
      expect(rt.gq.length, 1);

      final (sched2, rt2) = loops(2);
      final r2 = sched2.drainToQuiescence(maxCycles: 100, chunk: 30);
      expect(r2.status, ExecutionStatus.capped);
      expect(r2.goalsRun, 100);
      expect(rt2.gq.length, 2);
    });

    test('drainAndSend: the goals of its drains, and its Sends', () {
      final (sched, rt) = queued(1234);
      var sends = 0;
      final r = sched.drainAndSend(() => sends++);
      expect(r.status, ExecutionStatus.succeeded);
      expect(r.goalsRun, 1234);
      expect(rt.gq.length, 0);
      expect(sends, 1);

      final (sched2, rt2) = loops(1);
      var sends2 = 0;
      final r2 = sched2.drainAndSend(() => sends2++, maxCycles: 77);
      expect(r2.status, ExecutionStatus.capped);
      expect(r2.goalsRun, 77);
      expect(rt2.gq.length, 1);
      expect(sends2, 1);
    });

    test('drainAsyncWithStatus: the goals of its drains', () async {
      final (sched, rt) = queued(2500);
      final r = await sched.drainAsyncWithStatus(maxCycles: 100000);
      expect(r.status, ExecutionStatus.succeeded);
      expect(r.goalsRun, 2500);
      expect(rt.gq.length, 0);

      final (sched2, rt2) = loops(1);
      final r2 = await sched2.drainAsyncWithStatus(maxCycles: 50);
      expect(r2.status, ExecutionStatus.capped);
      expect(r2.goalsRun, 50);
      expect(rt2.gq.length, 1);
    });
  });

  group('the goals run, by id, for a caller that asks for them', () {
    test('drain returns them in the order they ran', () {
      final (sched, _) = loops(2);
      expect(sched.drain(maxCycles: 2), [1, 2]);
    });

    test('drainAsync too', () async {
      final (sched, _) = loops(3);
      expect(await sched.drainAsync(maxCycles: 5), [1, 2, 3, 1, 2]);
    });

    test('drainWithStatus appends them to the list it is given', () {
      final (sched, _) = queued(3);
      final ids = <int>[];
      final r = sched.drainWithStatus(goalIds: ids);
      expect(ids, [1, 2, 3]);
      expect(r.goalsRun, 3);
    });
  });

  group('suspended and failed goals are reported as before', () {
    test('a run that ends with a goal waiting: the goal and its reader', () {
      final run = program();
      final (_, reader) = run.rt.heap.allocateVariable();
      run.post('w/2', [VarRef(reader), run.bound('c')]);
      run.post('p/2', [run.bound(1), run.bound(2)]);
      final r = run.sched.drainWithStatus();
      expect(r.status, ExecutionStatus.suspended);
      expect(r.goalsRun, 4);
      expect(r.suspendedGoals, ['w(X1?, c)']);
      expect(r.blockingReaders, {reader});
      expect(run.rt.failedGoals, isEmpty);
    });

    test('a run in which a goal fails: failed, and the waiting goal listed',
        () {
      final run = program();
      final (_, reader) = run.rt.heap.allocateVariable();
      run.post('w/2', [run.bound('b'), run.bound('c')]);
      run.post('w/2', [VarRef(reader), run.bound('c')]);
      run.post('p/2', [run.bound(1), run.bound(2)]);
      final r = run.sched.drainWithStatus();
      expect(r.status, ExecutionStatus.failed);
      expect(r.goalsRun, 5);
      expect(r.suspendedGoals, ['w(X1?, c)']);
      expect(r.blockingReaders, isEmpty);
      expect(run.rt.failedGoals, ['w/2(b, c)']);
    });

    test('to quiescence, then a drain with nothing queued', () async {
      final run = program();
      final (_, reader) = run.rt.heap.allocateVariable();
      run.post('w/2', [VarRef(reader), run.bound('c')]);
      final r = run.sched.drainToQuiescence();
      expect(r.status, ExecutionStatus.suspended);
      expect(r.goalsRun, 1);
      expect(r.suspendedGoals, ['w(X1?, c)']);
      expect(r.blockingReaders, {reader});
      final again = await run.sched.drainAsyncWithStatus();
      expect(again.status, ExecutionStatus.succeeded);
      expect(again.goalsRun, 0);
      expect(again.suspendedGoals, isEmpty);
      expect(again.blockingReaders, isEmpty);
    });
  });
}
