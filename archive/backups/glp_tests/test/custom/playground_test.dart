import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/asm.dart';

void main() {
  test('Playground: write your own goal here', () {
    print('\n=== My Custom Goal ===\n');

    final rt = GlpRuntime();

    // Example: Create a writer for result X
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    // Example bytecode program: p(a).
    // This binds X to constant 'a'
    final prog = BC.prog([
      BC.L('p/1_start'),
      BC.TRY(),
      BC.headConst('a', 0),  // Match arg 0 with constant 'a'
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('p/1_end'),
      BC.SUSP(),
    ]);

    print('Program: p(a).');
    print('Goal: p(X) where X is unbound writer\n');

    // Create scheduler and runner
    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Set up goal environment and enqueue goal
    const goalId = 100;
    final env = CallEnv(writers: {0: wX});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, 0));

    // Run the scheduler
    print('--- Running goal ---\n');
    final ran = sched.drain(maxCycles: 100);
    print('Goals executed: $ran');

    print('\n=== Results ===');
    print('X (writer $wX) bound? ${rt.heap.isWriterBound(wX)}');
    if (rt.heap.isWriterBound(wX)) {
      final val = rt.heap.valueOfWriter(wX);
      print('X value: $val');
    }

    print('\n✓ Done!');
  });
}
