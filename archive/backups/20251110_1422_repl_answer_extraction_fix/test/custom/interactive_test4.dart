import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/asm.dart';

void main() {
  test('Program: multi_pred, Goal: p(X?), q(X) - suspend then wake', () {
    print('\n=== Program: multi_pred ===');
    print('p(a).');
    print('p(b).');
    print('q(b).');
    print('\n=== Goal: p(X?), q(X) ===\n');

    final rt = GlpRuntime();

    // Create writer/reader pair for X/X?
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    print('Heap Setup:');
    print('  Writer $wX (X) - unbound');
    print('  Reader $rX (X?) - paired with Writer $wX\n');

    // Bytecode for p/1 with reader check: p(a). p(b).
    final progPReader = BC.prog([
      BC.L('p/1_start'),
      BC.TRY(),              // Clause 1: p(a)
      BC.R(rX),              // Check reader (will add to Si if unbound)
      BC.U('p/1_c2'),        // Union Si to U, try next clause

      BC.L('p/1_c2'),
      BC.TRY(),              // Clause 2: p(b)
      BC.R(rX),              // Check reader again
      BC.U('p/1_end'),       // Union Si to U, go to end

      BC.L('p/1_end'),
      BC.SUSP(),             // Suspend if U non-empty
    ]);

    // Bytecode for q/1: q(b).
    final progQ = BC.prog([
      BC.L('q/1_start'),
      BC.TRY(),
      BC.W(wX),  // Mark writer in σ̂w
      BC.COMMIT(),
      BC.BCONST(wX, 'b'), // Bind to 'b'
      BC.PROCEED(),

      BC.L('q/1_end'),
      BC.SUSP(),
    ]);

    // Goal 1: p(X?) where X? is reader (writer is unbound)
    print('--- Executing Goal 1: p(X?) ---');
    print('Reader $rX is unbound (Writer $wX not bound yet)');
    print('Expected: Try both clauses, accumulate suspensions, SUSPEND\n');

    final cx1 = RunnerContext(
      rt: rt,
      goalId: 100,
      kappa: 0,
    );

    final result1 = BytecodeRunner(progPReader).runWithStatus(cx1);
    print('Result: $result1');
    print('Expected: SUSPENDED (reader unbound in both clauses)\n');

    expect(result1, RunResult.suspended, reason: 'Goal should suspend on unbound reader');
    print('Goal 100 is now SUSPENDED on Reader $rX\n');

    // Goal 2: q(X) where X is writer
    print('--- Executing Goal 2: q(X) ---');
    print('Writer $wX will be bound to \'b\'');
    print('This should WAKE Goal 100 (suspended on Reader $rX)\n');

    List<GoalRef> wokenGoals = [];
    final cx2 = RunnerContext(
      rt: rt,
      goalId: 200,
      kappa: 0,
      onActivation: (goal) {
        wokenGoals.add(goal);
        print('  ⚡ ACTIVATION: Goal ${goal.id} at PC ${goal.pc}');
      },
    );

    final result2 = BytecodeRunner(progQ).runWithStatus(cx2);
    print('Result: $result2');
    print('Writer $wX bound? ${rt.heap.isWriterBound(wX)}');
    if (rt.heap.isWriterBound(wX)) {
      final val = rt.heap.valueOfWriter(wX);
      print('Writer $wX value: $val');
    }
    print('');

    expect(result2, RunResult.terminated);
    expect(rt.heap.isWriterBound(wX), true);
    expect(wokenGoals.length, 1, reason: 'Should wake exactly 1 suspended goal');
    expect(wokenGoals.first.id, 100, reason: 'Should wake Goal 100');

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
    expect(queuedGoals.length, 1);
    expect(queuedGoals.first.id, 100);
    print('');

    // Now re-run the awakened goal
    print('--- Re-executing Goal 100: p(X?) (awakened) ---');
    print('Reader $rX is NOW BOUND to \'b\' (Writer $wX = \'b\')');
    print('Expected: Match clause 2 (p(b)) and succeed\n');

    final cx3 = RunnerContext(
      rt: rt,
      goalId: queuedGoals.first.id,
      kappa: queuedGoals.first.pc,
    );

    final result3 = BytecodeRunner(progPReader).runWithStatus(cx3);
    print('Result: $result3');
    print('Expected: TERMINATED (reader now bound to \'b\', matches p(b))\n');

    expect(result3, RunResult.terminated, reason: 'p(b) should succeed via clause 2');

    print('=== Final State ===');
    final finalVal = rt.heap.valueOfWriter(wX);
    print('X = $finalVal');
    print('SUCCESS ✓');
    print('\nExecution Summary:');
    print('  1. p(X?) suspended (reader unbound, tried both clauses)');
    print('  2. q(X) bound writer X to \'b\' and woke suspended goal');
    print('  3. p(X?) re-executed, reader now \'b\', matched clause 2 → success');

    expect(finalVal, isA<ConstTerm>());
    expect((finalVal as ConstTerm).value, 'b');
  });
}
