import 'package:test/test.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';

void main() {
  test('Two goals alternate due to 26-step tail yield', () {
    final rt = GlpRuntime();

    final p = BytecodeProgram([
      Label('LOOP'),
      TailStep('LOOP'),
    ]);
    final runner = BytecodeRunner(p);
    final sched = Scheduler(rt: rt, runner: runner);

    rt.gq.enqueue(GoalRef(1, 123));
    rt.gq.enqueue(GoalRef(2, 456));

    final ran1 = sched.drain(maxCycles: 2);
    expect(ran1, [1, 2], reason: 'each goal runs until its first yield');

    final ran2 = sched.drain(maxCycles: 2);
    expect(ran2, [1, 2], reason: 'after re-enqueue, order remains FIFO');
  });
}
