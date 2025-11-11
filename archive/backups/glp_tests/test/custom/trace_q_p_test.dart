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
  test('Program: p(a). p(b). q(b). Goal: q(X?), p(X) with full trace', () {
    print('\n========================================');
    print('=== PROGRAM ===');
    print('p(a).');
    print('p(b).');
    print('q(b).');
    print('\n=== GOAL ===');
    print('q(X?), p(X)');
    print('========================================\n');

    final rt = GlpRuntime();

    // Create writer/reader pair for X/X?
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    print('HEAP SETUP:');
    print('  Writer $wX (X) - unbound');
    print('  Reader $rX (X?) - paired with Writer $wX\n');

    // Bytecode for q/1: q(b).
    // This takes a READER argument and checks if it's bound to 'b'
    // According to v2.16 spec:
    // - headConst on unbound reader adds readerId to Si
    // - COMMIT checks Si: if non-empty, soft-fails to next clause
    // - Since there are no more clauses, SUSP will suspend on U
    final progQ = BC.prog([
      BC.L('q/1_start'),
      BC.TRY(),
      BC.headConst('b', 0),  // If reader unbound: adds to Si. If bound to 'b': success. If bound to other: soft-fail.
      BC.COMMIT(),           // If Si non-empty: soft-fails to next TRY. If Si empty: commits and proceeds.
      BC.PROCEED(),

      BC.L('q/1_end'),
      BC.SUSP(),             // Suspend if U is non-empty, otherwise FAIL
    ]);

    // Bytecode for p/1: p(a). p(b).
    // This takes a WRITER argument and binds it
    final progP = BC.prog([
      BC.L('p/1_start'),

      // Clause 1: p(a)
      BC.TRY(),
      BC.headConst('a', 0),  // Try to match/bind arg0 with 'a'
      BC.COMMIT(),
      BC.PROCEED(),

      // Clause 2: p(b)
      BC.L('p/1_clause2'),
      BC.TRY(),
      BC.headConst('b', 0),  // Try to match/bind arg0 with 'b'
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('p/1_end'),
      BC.SUSP(),
    ]);

    // Create runners for both programs
    final runnerQ = BytecodeRunner(progQ);
    final runnerP = BytecodeRunner(progP);

    // Create scheduler with both runners registered
    // We'll use string keys 'q' and 'p' to identify which program each goal uses
    final sched = Scheduler(rt: rt, runners: {
      'q': runnerQ,
      'p': runnerP,
    });

    print('========================================');
    print('GOAL 1: q(X?) where X is UNBOUND');
    print('Expected: SUSPEND (reader X? is unbound)');
    print('========================================\n');

    const goalId1 = 100;
    final env1 = CallEnv(readers: {0: rX});  // q takes reader argument
    rt.setGoalEnv(goalId1, env1);
    rt.setGoalProgram(goalId1, 'q');  // Associate goal with program 'q'
    rt.gq.enqueue(GoalRef(goalId1, 0));

    // Run goal 1 through scheduler
    final ran1 = sched.drain(maxCycles: 10);

    print('\nRESULT OF GOAL 1:');
    print('  Goals executed: $ran1');
    print('  X bound: ${rt.heap.isWriterBound(wX)}');
    print('  Suspended on R$rX: ${rt.roq.queue(rX)?.length ?? 0} goals\n');

    expect(rt.heap.isWriterBound(wX), false, reason: 'X should still be unbound');
    expect(rt.roq.queue(rX)?.length ?? 0, 1, reason: 'Goal should be suspended on rX');

    print('========================================');
    print('GOAL 2: p(X) where X is UNBOUND WRITER');
    print('Expected: BIND X to \'a\' (first clause)');
    print('Then: ACTIVATE suspended goal q(X?)');
    print('========================================\n');

    const goalId2 = 200;
    final env2 = CallEnv(writers: {0: wX});  // p takes writer argument
    rt.setGoalEnv(goalId2, env2);
    rt.setGoalProgram(goalId2, 'p');  // Associate goal with program 'p'
    rt.gq.enqueue(GoalRef(goalId2, 0));

    // Run goal 2 - when it commits, goal 1 will be reactivated with correct program
    final ran2 = sched.drain(maxCycles: 10);

    print('\nRESULT OF GOAL 2:');
    print('  Goals executed: $ran2');
    print('  X bound: ${rt.heap.isWriterBound(wX)}');
    if (rt.heap.isWriterBound(wX)) {
      print('  X value: ${rt.heap.valueOfWriter(wX)}');
    }
    print('  Goals in queue: ${rt.gq.length}\n');

    expect(rt.heap.isWriterBound(wX), true, reason: 'X should be bound');
    final val = rt.heap.valueOfWriter(wX);
    expect((val as ConstTerm).value, 'a', reason: 'X should be bound to a');
    // Note: scheduler already ran the activated goal, so queue is empty
    expect(ran2.length, 2, reason: 'Should have run both goal 200 and activated goal 100');

    print('========================================');
    print('GOAL 1 REACTIVATED: q(X?) where X is now bound to \'a\'');
    print('Expected: FAIL (q only has clause q(b), not q(a))');
    print('========================================\n');

    // Run reactivated goal
    final ran3 = sched.drain(maxCycles: 10);

    print('\nRESULT OF REACTIVATED GOAL:');
    print('  Goals executed: $ran3');

    print('========================================');
    print('=== EXECUTION SUMMARY ===');
    print('========================================');
    print('1. GOAL q(X?): Suspended (X unbound)');
    print('2. GOAL p(X): Bound X to \'a\' via first clause p(a)');
    print('3. ACTIVATION: q(X?) woke up');
    print('4. REACTIVATED q(X?): FAILED (q(b) does not match q(a))');
    print('\nFINAL STATE: X = ${rt.heap.valueOfWriter(wX)}');
    print('========================================\n');
  });
}
