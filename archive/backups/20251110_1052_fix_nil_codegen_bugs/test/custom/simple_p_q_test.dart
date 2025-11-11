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
  test('Simple p/q: p(a). q(a). Goal: q(X?), p(X) succeeds with X=a', () {
    print('\n=== SIMPLE P/Q TEST ===');
    print('Program: p(a). q(a).');
    print('Goal: q(X?), p(X)');
    print('Expected: X binds to a, q(a) succeeds\n');

    final rt = GlpRuntime();

    // Create writer/reader pair for X/X?
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    print('Heap: Writer $wX (X) paired with Reader $rX (X?)\n');

    // Program q: q(a).
    final progQ = BC.prog([
      BC.L('q/1'),
      BC.TRY(),
      BC.headConst('a', 0),  // arg0 must be 'a'
      BC.COMMIT(),
      BC.PROCEED(),
      BC.L('q_end'),
      BC.SUSP(),
    ]);

    // Program p: p(a).
    final progP = BC.prog([
      BC.L('p/1'),
      BC.TRY(),
      BC.headConst('a', 0),  // bind arg0 to 'a'
      BC.COMMIT(),
      BC.PROCEED(),
      BC.L('p_end'),
      BC.SUSP(),
    ]);

    // Create scheduler with both programs
    final runnerQ = BytecodeRunner(progQ);
    final runnerP = BytecodeRunner(progP);
    final sched = Scheduler(rt: rt, runners: {
      'q': runnerQ,
      'p': runnerP,
    });

    // Goal 1: q(X?) - will suspend because X is unbound
    print('Step 1: Run q(X?) where X is unbound');
    const goalQ = 100;
    rt.setGoalEnv(goalQ, CallEnv(readers: {0: rX}));
    rt.setGoalProgram(goalQ, 'q');
    rt.gq.enqueue(GoalRef(goalQ, 0));

    var ran = sched.drain(maxCycles: 10);
    print('  Ran goals: $ran');
    print('  X bound? ${rt.heap.isWriterBound(wX)}');
    print('  Suspended on R$rX? ${rt.roq.queue(rX).length} goals\n');

    expect(rt.heap.isWriterBound(wX), false, reason: 'X should be unbound');
    expect(rt.roq.queue(rX).length, 1, reason: 'q(X?) should be suspended');

    // Goal 2: p(X) - will bind X to 'a' and reactivate q(X?)
    print('Step 2: Run p(X) where X is unbound writer');
    const goalP = 200;
    rt.setGoalEnv(goalP, CallEnv(writers: {0: wX}));
    rt.setGoalProgram(goalP, 'p');
    rt.gq.enqueue(GoalRef(goalP, 0));

    ran = sched.drain(maxCycles: 10);
    print('  Ran goals: $ran');
    print('  X bound? ${rt.heap.isWriterBound(wX)}');
    if (rt.heap.isWriterBound(wX)) {
      print('  X value: ${rt.heap.valueOfWriter(wX)}');
    }
    print('');

    expect(rt.heap.isWriterBound(wX), true, reason: 'X should be bound');
    final val = rt.heap.valueOfWriter(wX);
    expect((val as ConstTerm).value, 'a', reason: 'X should be bound to a');
    expect(ran.length, 2, reason: 'Should run p(X) then reactivated q(X?)');
    expect(ran, [200, 100], reason: 'p runs first, then q is reactivated');

    print('✓ Success! X = a, both goals succeeded');
    print('✓ Goal q(X?) correctly ran with program Q after reactivation\n');
  });

  test('Simple p/q: p(b). q(a). Goal: q(X?), p(X) fails because p(b)≠q(a)', () {
    print('\n=== SIMPLE P/Q TEST (MISMATCH) ===');
    print('Program: p(b). q(a).');
    print('Goal: q(X?), p(X)');
    print('Expected: X binds to b, q(b) fails\n');

    final rt = GlpRuntime();

    // Create writer/reader pair
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    // Program q: q(a).
    final progQ = BC.prog([
      BC.L('q/1'),
      BC.TRY(),
      BC.headConst('a', 0),
      BC.COMMIT(),
      BC.PROCEED(),
      BC.L('q_end'),
      BC.SUSP(),
    ]);

    // Program p: p(b).
    final progP = BC.prog([
      BC.L('p/1'),
      BC.TRY(),
      BC.headConst('b', 0),  // bind to 'b' instead
      BC.COMMIT(),
      BC.PROCEED(),
      BC.L('p_end'),
      BC.SUSP(),
    ]);

    final runnerQ = BytecodeRunner(progQ);
    final runnerP = BytecodeRunner(progP);
    final sched = Scheduler(rt: rt, runners: {
      'q': runnerQ,
      'p': runnerP,
    });

    // Goal 1: q(X?) - suspends
    print('Step 1: Run q(X?)');
    const goalQ = 100;
    rt.setGoalEnv(goalQ, CallEnv(readers: {0: rX}));
    rt.setGoalProgram(goalQ, 'q');
    rt.gq.enqueue(GoalRef(goalQ, 0));
    sched.drain(maxCycles: 10);

    expect(rt.heap.isWriterBound(wX), false);
    expect(rt.roq.queue(rX).length, 1);

    // Goal 2: p(X) - binds X to 'b', reactivates q(X?) which fails
    print('Step 2: Run p(X) - binds X to b');
    const goalP = 200;
    rt.setGoalEnv(goalP, CallEnv(writers: {0: wX}));
    rt.setGoalProgram(goalP, 'p');
    rt.gq.enqueue(GoalRef(goalP, 0));

    final ran = sched.drain(maxCycles: 10);
    print('  Ran goals: $ran');
    print('  X value: ${rt.heap.valueOfWriter(wX)}');
    print('');

    expect(rt.heap.isWriterBound(wX), true);
    final val = rt.heap.valueOfWriter(wX);
    expect((val as ConstTerm).value, 'b', reason: 'X should be bound to b');
    expect(ran, [200, 100], reason: 'p runs, then q fails on reactivation');

    print('✓ Correct behavior: X = b, q(a) failed as expected');
    print('✓ Goal q(X?) correctly ran with program Q and failed\n');
  });
}
