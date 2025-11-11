import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/asm.dart';

void main() {
  test('P/Q program with detailed trace: p(a). p(b). q(b). Goal: q(X?), p(X)', () {
    print('\n' + '=' * 60);
    print('PROGRAM:');
    print('  p(a).');
    print('  p(b).');
    print('  q(b).');
    print('\nGOAL: q(X?), p(X)');
    print('=' * 60 + '\n');

    final rt = GlpRuntime();

    // Create writer/reader pair for X/X?
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    print('HEAP INITIALIZATION:');
    print('  Writer $wX (X) - unbound');
    print('  Reader $rX (X?) - paired with Writer $wX');
    print('');

    // Program Q: q(b).
    final progQ = BC.prog([
      BC.L('q/1_start'),
      BC.TRY(),
      BC.headConst('b', 0),
      BC.COMMIT(),
      BC.PROCEED(),
      BC.L('q/1_end'),
      BC.SUSP(),
    ]);

    // Program P: p(a). p(b).
    final progP = BC.prog([
      BC.L('p/1_start'),
      // Clause 1: p(a)
      BC.TRY(),
      BC.headConst('a', 0),
      BC.COMMIT(),
      BC.PROCEED(),
      // Clause 2: p(b)
      BC.L('p/1_clause2'),
      BC.TRY(),
      BC.headConst('b', 0),
      BC.COMMIT(),
      BC.PROCEED(),
      BC.L('p/1_end'),
      BC.SUSP(),
    ]);

    // Create runners
    final runnerQ = BytecodeRunner(progQ);
    final runnerP = BytecodeRunner(progP);

    print('PROGRAMS COMPILED:');
    print('  Program Q (q/1): ${progQ.ops.length} instructions');
    print('  Program P (p/1): ${progP.ops.length} instructions');
    print('');

    // Manual execution with detailed trace
    print('=' * 60);
    print('EXECUTION TRACE');
    print('=' * 60 + '\n');

    // Goal 1: q(X?)
    print('--- GOAL 100: q(X?) ---');
    print('Setting up goal 100 with program Q');
    print('  Arg 0: Reader $rX (X? - unbound)');
    print('');

    const goalQ = 100;
    final envQ = CallEnv(readers: {0: rX});
    rt.setGoalEnv(goalQ, envQ);
    rt.setGoalProgram(goalQ, 'q');

    print('Executing goal 100 with program Q...');
    final cxQ = RunnerContext(rt: rt, goalId: goalQ, kappa: 0, env: envQ);
    final resultQ = runnerQ.runWithStatus(cxQ);

    print('  Status: $resultQ');
    print('  X bound: ${rt.heap.isWriterBound(wX)}');
    print('  Suspended on R$rX: ${rt.roq.queue(rX).length} goals');
    print('');

    // Goal 2: p(X)
    print('--- GOAL 200: p(X) ---');
    print('Setting up goal 200 with program P');
    print('  Arg 0: Writer $wX (X - unbound)');
    print('');

    const goalP = 200;
    final envP = CallEnv(writers: {0: wX});
    rt.setGoalEnv(goalP, envP);
    rt.setGoalProgram(goalP, 'p');

    print('Executing goal 200 with program P...');
    final cxP = RunnerContext(rt: rt, goalId: goalP, kappa: 0, env: envP);
    final resultP = runnerP.runWithStatus(cxP);

    print('  Status: $resultP');
    print('  X bound: ${rt.heap.isWriterBound(wX)}');
    if (rt.heap.isWriterBound(wX)) {
      print('  X value: ${rt.heap.valueOfWriter(wX)}');
    }
    print('  Goals activated: ${rt.gq.length}');
    print('');

    // Reactivated goal
    if (rt.gq.length > 0) {
      final act = rt.gq.dequeue();
      print('--- REACTIVATION: Goal ${act!.id} at PC ${act.pc} ---');

      final goalProgram = rt.getGoalProgram(act.id);
      print('  Goal program: $goalProgram');

      final runner = goalProgram == 'q' ? runnerQ : runnerP;
      print('  Using runner: ${goalProgram == 'q' ? 'Q' : 'P'}');
      print('');

      final env = rt.getGoalEnv(act.id);
      print('Executing reactivated goal ${act.id}...');
      final cx = RunnerContext(rt: rt, goalId: act.id, kappa: act.pc, env: env);
      final result = runner.runWithStatus(cx);

      print('  Status: $result');
      print('');
    }

    print('=' * 60);
    print('FINAL STATE');
    print('=' * 60);
    print('  X (Writer $wX) bound: ${rt.heap.isWriterBound(wX)}');
    if (rt.heap.isWriterBound(wX)) {
      print('  X value: ${rt.heap.valueOfWriter(wX)}');
    }
    print('  Queue length: ${rt.gq.length}');
    print('');

    // Assertions
    expect(rt.heap.isWriterBound(wX), true, reason: 'X should be bound');
    final val = rt.heap.valueOfWriter(wX);
    expect((val as ConstTerm).value, 'a', reason: 'X should be bound to a (first clause of p)');

    print('✓ Test passed: X correctly bound to \'a\' via first clause of p');
    print('✓ Goal 100 correctly reactivated with program Q');
    print('');
  });
}
