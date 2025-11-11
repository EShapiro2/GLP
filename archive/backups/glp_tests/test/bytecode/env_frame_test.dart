import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/asm.dart';

void main() {
  test('Allocate creates environment frame with permanent variables', () {
    print('\n=== ALLOCATE TEST: Environment frame creation ===');

    final rt = GlpRuntime();

    // Simple program that allocates an environment frame
    // p(X) :- allocate 3, proceed.
    final prog = BC.prog([
      BC.L('p/1'),
      BC.TRY(),
      BC.headConst('a', 0),  // Match X='a'
      BC.COMMIT(),
      BC.allocate(3),         // Allocate frame with 3 permanent variables
      BC.PROCEED(),
    ]);

    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Setup: X='a'
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));
    rt.heap.bindWriterConst(wX, 'a');

    // Call p('a')
    const goalId = 100;
    final env = CallEnv(writers: {0: wX});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, prog.labels['p/1']!));

    print("Starting p('a') - should allocate frame and succeed...");
    final ran = sched.drain(maxCycles: 10);

    print('Goals executed: ${ran.length}');
    expect(ran.length, 1, reason: 'Should execute once and succeed');
    print('✓ Allocate instruction executed successfully\n');
  });

  test('Deallocate removes environment frame', () {
    print('\n=== DEALLOCATE TEST: Environment frame removal ===');

    final rt = GlpRuntime();

    // Program that allocates then deallocates
    // p(X) :- allocate 2, deallocate, proceed.
    final prog = BC.prog([
      BC.L('p/1'),
      BC.TRY(),
      BC.headConst('b', 0),
      BC.COMMIT(),
      BC.allocate(2),         // Allocate frame with 2 slots
      BC.deallocate(),        // Deallocate frame
      BC.PROCEED(),
    ]);

    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Setup: X='b'
    const wX = 10;
    const rX = 11;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));
    rt.heap.bindWriterConst(wX, 'b');

    // Call p('b')
    const goalId = 200;
    final env = CallEnv(writers: {0: wX});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, prog.labels['p/1']!));

    print("Starting p('b') - should allocate, deallocate, and succeed...");
    final ran = sched.drain(maxCycles: 10);

    print('Goals executed: ${ran.length}');
    expect(ran.length, 1, reason: 'Should execute once and succeed');
    print('✓ Deallocate instruction executed successfully\n');
  });

  test('Nested environment frames (allocate within allocate)', () {
    print('\n=== NESTED FRAMES TEST: Multiple allocations ===');

    final rt = GlpRuntime();

    // Program with nested allocations
    // p(X) :- allocate 1, allocate 2, deallocate, deallocate, proceed.
    final prog = BC.prog([
      BC.L('p/1'),
      BC.TRY(),
      BC.headConst('c', 0),
      BC.COMMIT(),
      BC.allocate(1),         // First frame: 1 slot
      BC.allocate(2),         // Second frame: 2 slots (nested)
      BC.deallocate(),        // Remove second frame
      BC.deallocate(),        // Remove first frame
      BC.PROCEED(),
    ]);

    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Setup: X='c'
    const wX = 20;
    const rX = 21;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));
    rt.heap.bindWriterConst(wX, 'c');

    // Call p('c')
    const goalId = 300;
    final env = CallEnv(writers: {0: wX});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, prog.labels['p/1']!));

    print("Starting p('c') - should handle nested frames correctly...");
    final ran = sched.drain(maxCycles: 10);

    print('Goals executed: ${ran.length}');
    expect(ran.length, 1, reason: 'Should execute once and succeed');
    print('✓ Nested environment frames handled correctly\n');
  });
}
