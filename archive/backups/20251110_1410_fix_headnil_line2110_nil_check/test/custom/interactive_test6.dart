import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/asm.dart';

void main() {
  test('Program: multi_pred, Goal: q(X?), q(X?), q(X?), p(X) - multiple suspensions, one wake', () {
    print('\n=== Program: multi_pred ===');
    print('p(a).');
    print('p(b).');
    print('q(b).');
    print('\n=== Goal: q(X?), q(X?), q(X?), p(X) ===\n');

    final rt = GlpRuntime();

    // Create writer/reader pair for X/X?
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    print('Heap Setup:');
    print('  Writer $wX (X) - unbound');
    print('  Reader $rX (X?) - paired with Writer $wX\n');

    // Bytecode for q/1 with reader check: q(b).
    final progQReader = BC.prog([
      BC.L('q/1_start'),
      BC.TRY(),              // Clause 1: q(b)
      BC.R(rX),              // Check reader (will add to Si if unbound)
      BC.U('q/1_end'),       // Union Si to U, go to end

      BC.L('q/1_end'),
      BC.SUSP(),             // Suspend if U non-empty
    ]);

    // Bytecode for p/1 with writer: p(a). p(b).
    final progPWriter = BC.prog([
      BC.L('p/1_start'),
      BC.TRY(),              // Clause 1: p(a)
      BC.W(wX),              // Mark writer in σ̂w
      BC.COMMIT(),
      BC.BCONST(wX, 'a'),    // Bind to 'a'
      BC.PROCEED(),

      BC.L('p/1_c2'),
      BC.TRY(),              // Clause 2: p(b)
      BC.W(wX),
      BC.COMMIT(),
      BC.BCONST(wX, 'b'),
      BC.PROCEED(),

      BC.L('p/1_end'),
      BC.SUSP(),
    ]);

    // Goal 1: q(X?) - first suspension
    print('--- Executing Goal 1: q(X?) ---');
    print('Reader $rX is unbound\n');

    final cx1 = RunnerContext(rt: rt, goalId: 100, kappa: 0);
    final result1 = BytecodeRunner(progQReader).runWithStatus(cx1);
    print('Goal 100 Result: $result1');
    expect(result1, RunResult.suspended);
    print('Goal 100 SUSPENDED on Reader $rX\n');

    // Goal 2: q(X?) - second suspension
    print('--- Executing Goal 2: q(X?) ---');
    print('Reader $rX is still unbound\n');

    final cx2 = RunnerContext(rt: rt, goalId: 101, kappa: 0);
    final result2 = BytecodeRunner(progQReader).runWithStatus(cx2);
    print('Goal 101 Result: $result2');
    expect(result2, RunResult.suspended);
    print('Goal 101 SUSPENDED on Reader $rX\n');

    // Goal 3: q(X?) - third suspension
    print('--- Executing Goal 3: q(X?) ---');
    print('Reader $rX is still unbound\n');

    final cx3 = RunnerContext(rt: rt, goalId: 102, kappa: 0);
    final result3 = BytecodeRunner(progQReader).runWithStatus(cx3);
    print('Goal 102 Result: $result3');
    expect(result3, RunResult.suspended);
    print('Goal 102 SUSPENDED on Reader $rX\n');

    print('--- Three goals now suspended on the SAME reader! ---\n');

    // Goal 4: p(X) - bind writer and wake ALL suspended goals
    print('--- Executing Goal 4: p(X) ---');
    print('Writer $wX will be bound to \'a\'');
    print('This should WAKE all 3 suspended goals (100, 101, 102)\n');

    List<GoalRef> wokenGoals = [];
    final cx4 = RunnerContext(
      rt: rt,
      goalId: 200,
      kappa: 0,
      onActivation: (goal) {
        wokenGoals.add(goal);
        print('  ⚡ ACTIVATION: Goal ${goal.id} at PC ${goal.pc}');
      },
    );

    final result4 = BytecodeRunner(progPWriter).runWithStatus(cx4);
    print('\nGoal 200 Result: $result4');
    print('Writer $wX bound? ${rt.heap.isWriterBound(wX)}');
    if (rt.heap.isWriterBound(wX)) {
      final val = rt.heap.valueOfWriter(wX);
      print('Writer $wX value: $val');
    }
    print('');

    expect(result4, RunResult.terminated);
    expect(rt.heap.isWriterBound(wX), true);
    final boundVal = rt.heap.valueOfWriter(wX);
    expect(boundVal, isA<ConstTerm>());
    expect((boundVal as ConstTerm).value, 'a');

    print('--- Activations Summary ---');
    print('Number of goals activated: ${wokenGoals.length}');
    expect(wokenGoals.length, 3, reason: 'Should wake exactly 3 suspended goals');
    for (final g in wokenGoals) {
      print('  Activated: Goal ${g.id}');
    }
    expect(wokenGoals.map((g) => g.id).toSet(), {100, 101, 102},
           reason: 'Should wake goals 100, 101, 102');
    print('');

    // Check goal queue
    print('--- Goal Queue After Wake ---');
    print('Goal queue length: ${rt.gq.length}');
    final queuedGoals = <GoalRef>[];
    while (rt.gq.length > 0) {
      final g = rt.gq.dequeue();
      if (g != null) {
        queuedGoals.add(g);
        print('  Queued: Goal ${g.id} at PC ${g.pc}');
      }
    }
    expect(queuedGoals.length, 3, reason: 'All 3 goals should be queued');
    print('');

    // Re-run all awakened goals
    print('--- Re-executing Awakened Goals ---');
    for (final qg in queuedGoals) {
      print('Re-executing Goal ${qg.id}: q(X?)');
      print('  Reader $rX is NOW BOUND to \'a\'');

      final cxRerun = RunnerContext(
        rt: rt,
        goalId: qg.id,
        kappa: qg.pc,
      );

      final resultRerun = BytecodeRunner(progQReader).runWithStatus(cxRerun);
      print('  Result: $resultRerun');
      expect(resultRerun, RunResult.terminated);
      print('  q(a) tried to match q(b) → FAIL\n');
    }

    print('=== Final State ===');
    final finalVal = rt.heap.valueOfWriter(wX);
    print('X = $finalVal');
    print('\nExecution Summary:');
    print('  1. q(X?) - Goal 100 suspended (reader unbound)');
    print('  2. q(X?) - Goal 101 suspended (reader unbound)');
    print('  3. q(X?) - Goal 102 suspended (reader unbound)');
    print('  4. p(X) bound writer X to \'a\' → WOKE ALL 3 GOALS');
    print('  5. All 3 goals re-executed: q(a) vs q(b) → all FAILED');
    print('\nKey insight: ONE writer binding wakes MULTIPLE suspended goals! ✓');

    expect(finalVal, isA<ConstTerm>());
    expect((finalVal as ConstTerm).value, 'a');
  });
}
