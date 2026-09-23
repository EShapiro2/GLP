import 'package:test/test.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/engine_v2/interp.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';

/// Reducing until quiescent, and the cap that is not quiescence.
///
/// One [Scheduler.drainWithStatus] stops at its cycle cap with the queue
/// non-empty, which is not a settled run: the goals left standing there are
/// runnable. That exit is [ExecutionStatus.capped] and not
/// [ExecutionStatus.suspended], and [Scheduler.drainToQuiescence] is the drain
/// that runs on until the queue is empty. Its own cap is a safety net against
/// a program that never quiesces, and reaching it is reported, never passed
/// over.
void main() {
  /// A queue of [n] goals of `p :- true.`, each reducing once and ending.
  (Scheduler, GlpRuntime) queued(int n) {
    final rt = GlpRuntime();
    final image = codeImageFromProgram(BytecodeProgram([
      Label('p/0'),
      ClauseTry(),
      Commit(),
      Proceed(),
    ]));
    final sched = Scheduler(rt: rt, runner: ByteRunner(image));
    final entry = image.entryOffsetOf('p/0')!;
    for (var i = 1; i <= n; i++) {
      rt.setGoalEnv(i, CallEnv());
      rt.gq.enqueue(GoalRef(i, entry));
    }
    return (sched, rt);
  }

  test('a drain that exits with goals still queued reports capped, not suspended',
      () {
    final (sched, rt) = queued(2500);
    final result = sched.drainWithStatus();
    expect(result.goalsRan.length, 1000, reason: 'the drain stops at its cap');
    expect(rt.gq.length, 1500, reason: 'and what is left is runnable');
    expect(result.status, ExecutionStatus.capped);
    expect(result.status, isNot(ExecutionStatus.suspended),
        reason: 'a half-run is not a settled one');
  });

  test('drainToQuiescence runs the queue out, however deep', () {
    final (sched, rt) = queued(2500);
    final result = sched.drainToQuiescence();
    expect(result.goalsRan.length, 2500);
    expect(rt.gq.length, 0, reason: 'quiescent: the queue is empty');
    expect(result.status, ExecutionStatus.succeeded);
  });

  test('its cap is a net, and a program that never quiesces is caught by it',
      () {
    final rt = GlpRuntime();
    // loop :- true | loop.  — a goal that tail-loops and never ends.
    final image = codeImageFromProgram(BytecodeProgram([
      Label('loop/0'),
      ClauseTry(),
      Commit(),
      Requeue('loop/0', 0),
    ]));
    final sched = Scheduler(rt: rt, runner: ByteRunner(image));
    rt.gq.enqueue(GoalRef(1, image.entryOffsetOf('loop/0')!));

    final result = sched.drainToQuiescence(maxCycles: 100);
    expect(result.goalsRan.length, 100, reason: 'the net, and not a step more');
    expect(rt.gq.length, greaterThan(0));
    expect(result.status, ExecutionStatus.capped,
        reason: 'the caller is told the run did not finish');
  });
}
