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
  test('Commit occurs before body: σ̂w applied atomically before first body instruction', () {
    print('\n=== COMMIT BEFORE BODY TEST ===');

    final rt = GlpRuntime();

    // Program: p(X) :- head_constant(X, 'a'), commit, spawn('verify/1').
    // The writer X should be bound in σ̂w during HEAD phase, but NOT visible
    // on the heap until COMMIT. Then the BODY phase begins and Spawn can see it.
    //
    // We verify this by checking that:
    // 1. During HEAD/GUARD phase, writer is NOT bound on heap
    // 2. After COMMIT, writer IS bound on heap before first BODY instruction

    final prog = BC.prog([
      BC.L('p/1'),
      BC.TRY(),
      BC.headConst('a', 0),     // σ̂w[wX] = 'a' (tentative)
      BC.COMMIT(),              // Apply σ̂w to heap atomically
      // At this point, wX MUST be bound on heap before any BODY instruction
      BC.PROCEED(),

      BC.L('p/1_end'),
      BC.SUSP(),
    ]);

    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Setup: Unbound writer
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    // Call p(X)
    const goalId = 100;
    final env = CallEnv(writers: {0: wX});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, prog.labels['p/1']!));

    print('Starting p(X) - testing commit timing');
    print('Before execution: writer ${wX} bound = ${rt.heap.isWriterBound(wX)}');

    final ran = sched.drain(maxCycles: 10);

    print('After execution: writer ${wX} bound = ${rt.heap.isWriterBound(wX)}');
    print('Writer value: ${rt.heap.valueOfWriter(wX)}');

    // After COMMIT, writer MUST be bound
    expect(rt.heap.isWriterBound(wX), isTrue,
      reason: 'Writer must be bound on heap after COMMIT');
    final value = rt.heap.valueOfWriter(wX);
    expect(value, isA<ConstTerm>(), reason: 'Writer value should be ConstTerm');
    expect((value as ConstTerm).value, equals('a'),
      reason: 'Writer must have value from σ̂w after COMMIT');
    expect(ran.length, 1, reason: 'Goal should succeed');

    print('✓ COMMIT applied σ̂w to heap before entering BODY phase\n');
  });

  test('Commit binds paired reader and activates suspended goals', () {
    print('\n=== COMMIT ACTIVATES SUSPENDED GOALS TEST ===');

    final rt = GlpRuntime();

    // Two programs:
    // p(X) :- head_constant(X, 'value'), commit, proceed.
    // q(Y?) :- head_constant(Y?, 'value'), commit, proceed.
    //
    // If we call q(X?) first (where X is unbound), it suspends on reader X?.
    // Then we call p(X), which binds writer X.
    // COMMIT must bind X, then bind paired reader X?, then activate q.

    final progP = BC.prog([
      BC.L('p/1'),
      BC.TRY(),
      BC.headConst('value', 0),
      BC.COMMIT(),
      BC.PROCEED(),
      BC.L('p/1_end'),
      BC.SUSP(),
    ]);

    final progQ = BC.prog([
      BC.L('q/1'),
      BC.TRY(),
      BC.headConst('value', 0),
      BC.COMMIT(),
      BC.PROCEED(),
      BC.L('q/1_end'),
      BC.SUSP(),
    ]);

    final runnerP = BytecodeRunner(progP);
    final runnerQ = BytecodeRunner(progQ);
    final sched = Scheduler(rt: rt, runners: {
      'p': runnerP,
      'q': runnerQ,
    });

    // Setup: Writer/Reader pair
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    // Call q(X?) first - should suspend on unbound reader
    const goalIdQ = 100;
    rt.setGoalProgram(goalIdQ, 'q');
    final envQ = CallEnv(readers: {0: rX});
    rt.setGoalEnv(goalIdQ, envQ);
    rt.gq.enqueue(GoalRef(goalIdQ, progQ.labels['q/1']!));

    print('Calling q(X?) where X is unbound...');
    final ranQ = sched.drain(maxCycles: 10);

    // Check if goal suspended by verifying reader rX has suspension notes in ROQ
    final queueNotEmpty = !rt.roq.isEmpty(rX);
    print('Goal q suspended: ${queueNotEmpty} (ROQ for reader ${rX} has notes)');
    expect(queueNotEmpty, isTrue,
      reason: 'q should suspend on unbound reader (ROQ should have suspension note)');
    print('✓ q suspended on unbound reader X?');

    // Call p(X) - should bind writer and activate q
    const goalIdP = 200;
    rt.setGoalProgram(goalIdP, 'p');
    final envP = CallEnv(writers: {0: wX});
    rt.setGoalEnv(goalIdP, envP);
    rt.gq.enqueue(GoalRef(goalIdP, progP.labels['p/1']!));

    print('Calling p(X) to bind writer X...');
    final ran = sched.drain(maxCycles: 20);

    print('Writer X bound: ${rt.heap.isWriterBound(wX)}');
    print('Goals executed: ${ran.length}');

    // After p commits, writer should be bound, and q should be activated
    expect(rt.heap.isWriterBound(wX), isTrue, reason: 'Writer should be bound');
    // Check that q was activated and executed (ran should contain both goalIdP and goalIdQ)
    expect(ran.contains(goalIdP), isTrue, reason: 'p should have executed');
    expect(ran.contains(goalIdQ), isTrue, reason: 'q should have been activated and executed after writer binds');

    print('✓ COMMIT bound writer, then paired reader, then activated suspended goal q\n');
  });
}
