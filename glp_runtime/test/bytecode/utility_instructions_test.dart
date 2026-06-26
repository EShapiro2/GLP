import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/engine_v2/interp.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/asm.dart';

void main() {
  test('Nop: no operation, just advances PC', () {
    print('\n=== NOP TEST: No operation ===');

    final rt = GlpRuntime();

    // Program: p :- nop, nop, nop, proceed.
    final prog = BC.prog([
      BC.L('p/0'),
      BC.TRY(),
      BC.COMMIT(),
      BC.nop(),              // Do nothing
      BC.nop(),              // Do nothing
      BC.nop(),              // Do nothing
      BC.PROCEED(),
    ]);

    final image = codeImageFromProgram(prog);
    final runner = ByteRunner(image);
    final sched = Scheduler(rt: rt, runner: runner);

    // Call p
    const goalId = 100;
    final env = CallEnv();
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, image.entryOffsetOf('p/0')!));

    print('Starting p with 3 nops');
    final ran = sched.drain(maxCycles: 10);

    print('Goals executed: ${ran.length}');
    expect(ran.length, 1, reason: 'Should execute through all nops and succeed');
    print('✓ Nop instruction works correctly\n');
  });

  test('Halt: terminates execution', () {
    print('\n=== HALT TEST: Terminate execution ===');

    final rt = GlpRuntime();

    // Program: p :- halt.
    final prog = BC.prog([
      BC.L('p/0'),
      BC.TRY(),
      BC.COMMIT(),
      BC.halt(),             // Terminate immediately
    ]);

    final image = codeImageFromProgram(prog);
    final runner = ByteRunner(image);
    final sched = Scheduler(rt: rt, runner: runner);

    // Call p
    const goalId = 100;
    final env = CallEnv();
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, image.entryOffsetOf('p/0')!));

    print('Starting p with halt');
    final ran = sched.drain(maxCycles: 10);

    print('Goals executed: ${ran.length}');
    expect(ran.length, 1, reason: 'Should halt and terminate');
    print('✓ Halt instruction terminates correctly\n');
  });

  test('Halt vs Proceed: both terminate', () {
    print('\n=== HALT VS PROCEED TEST ===');

    final rt = GlpRuntime();

    // Two programs: one uses halt, one uses proceed
    final progHalt = BC.prog([
      BC.L('p/0'),
      BC.TRY(),
      BC.COMMIT(),
      BC.halt(),
    ]);

    final progProceed = BC.prog([
      BC.L('p/0'),
      BC.TRY(),
      BC.COMMIT(),
      BC.PROCEED(),
    ]);

    final imageHalt = codeImageFromProgram(progHalt);
    final imageProceed = codeImageFromProgram(progProceed);
    final runnerHalt = ByteRunner(imageHalt);
    final runnerProceed = ByteRunner(imageProceed);

    // Test with halt
    final schedHalt = Scheduler(rt: rt, runner: runnerHalt);
    const goalId1 = 100;
    final env1 = CallEnv();
    rt.setGoalEnv(goalId1, env1);
    rt.gq.enqueue(GoalRef(goalId1, imageHalt.entryOffsetOf('p/0')!));

    print('Testing halt...');
    final ranHalt = schedHalt.drain(maxCycles: 10);
    expect(ranHalt.length, 1);
    print('✓ Halt terminated after 1 execution');

    // Test with proceed (need fresh runtime)
    final rt2 = GlpRuntime();
    final schedProceed = Scheduler(rt: rt2, runner: runnerProceed);
    const goalId2 = 200;
    final env2 = CallEnv();
    rt2.setGoalEnv(goalId2, env2);
    rt2.gq.enqueue(GoalRef(goalId2, imageProceed.entryOffsetOf('p/0')!));

    print('Testing proceed...');
    final ranProceed = schedProceed.drain(maxCycles: 10);
    expect(ranProceed.length, 1);
    print('✓ Proceed terminated after 1 execution');

    print('✓ Both Halt and Proceed terminate execution\n');
  });
}
