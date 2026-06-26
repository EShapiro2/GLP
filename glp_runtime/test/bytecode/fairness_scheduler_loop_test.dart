import 'package:test/test.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/engine_v2/interp.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';

/// Tail-recursion fairness on the live tail-call op (ISA §9.2).
///
/// A goal that tail-loops via `requeue` must not starve its isolate: the
/// requeue handler applies the 26-step tail-recursion budget, yielding and
/// re-enqueueing so a ready sibling on the same scheduler progresses. Without
/// the budget, goal 1 would loop forever inside one run() call and goal 2 would
/// never run.
void main() {
  test('a tail-recursive requeue loop yields, letting a ready sibling run', () {
    final rt = GlpRuntime();

    // loop :- true | loop.  — a clause whose body tail-calls itself.
    final p = BytecodeProgram([
      Label('loop/0'),
      ClauseTry(),
      Commit(),
      Requeue('loop/0', 0),
    ]);
    final image = codeImageFromProgram(p);
    final runner = ByteRunner(image);
    final sched = Scheduler(rt: rt, runner: runner);

    rt.gq.enqueue(GoalRef(1, image.entryOffsetOf('loop/0')!));
    rt.gq.enqueue(GoalRef(2, image.entryOffsetOf('loop/0')!));

    final ran = sched.drain(maxCycles: 2);
    expect(ran, [1, 2],
        reason: 'goal 1 yields after its tail budget, then goal 2 runs');
  });
}
