import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/asm.dart';

void main() {
  test('Program: facts, Goal: p(X?), p(X) - suspend then wake', () {
    print('\n=== Program: facts ===');
    print('p(a).');
    print('\n=== Goal: p(X?), p(X) ===\n');

    final rt = GlpRuntime();

    // Create writer/reader pair for X/X?
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    print('Heap Setup:');
    print('  Writer $wX (X) - unbound');
    print('  Reader $rX (X?) - paired with Writer $wX\n');

    // Bytecode for p/1 with reader: p(X?)
    final progReader = BC.prog([
      BC.L('p/1_start'),
      BC.TRY(),
      BC.R(rX), // Guard: need reader to be bound (will add to Si if unbound)
      BC.U('end'),
      BC.L('end'),
      BC.SUSP(), // Suspend if U non-empty
    ]);

    // Bytecode for p/1 with writer: p(X)
    final progWriter = BC.prog([
      BC.L('p/1_start'),
      BC.TRY(),
      BC.W(wX),  // Mark writer in σ̂w
      BC.COMMIT(), // Apply σ̂w and wake suspended goals
      BC.BCONST(wX, 'a'), // Bind to 'a'
      BC.PROCEED(),
    ]);

    // Goal 1: p(X?) where X? is reader (writer is unbound)
    print('--- Executing Goal 1: p(X?) ---');
    print('Reader $rX is unbound (Writer $wX not bound yet)');

    final cx1 = RunnerContext(
      rt: rt,
      goalId: 100,
      kappa: 0,
    );

    final result1 = BytecodeRunner(progReader).runWithStatus(cx1);
    print('Result: $result1');
    print('Expected: SUSPENDED because Writer $wX is unbound\n');

    expect(result1, RunResult.suspended, reason: 'Goal should suspend on unbound reader');
    print('Goal 100 is now SUSPENDED on Reader $rX\n');

    // Goal 2: p(X) where X is writer
    print('--- Executing Goal 2: p(X) ---');
    print('Writer $wX will be bound to \'a\'');
    print('This should WAKE Goal 100 (suspended on Reader $rX)');

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

    final result2 = BytecodeRunner(progWriter).runWithStatus(cx2);
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
    print('Reader $rX is NOW BOUND (Writer $wX = \'a\')');

    final cx3 = RunnerContext(
      rt: rt,
      goalId: queuedGoals.first.id,
      kappa: queuedGoals.first.pc,
    );

    final result3 = BytecodeRunner(progReader).runWithStatus(cx3);
    print('Result: $result3');
    print('Expected: TERMINATED (reader is now bound)\n');

    expect(result3, RunResult.terminated, reason: 'Awakened goal should terminate successfully');

    print('=== Final State ===');
    final finalVal = rt.heap.valueOfWriter(wX);
    print('X = $finalVal');
    print('SUCCESS ✓');
    print('\nExecution Summary:');
    print('  1. p(X?) suspended (reader unbound)');
    print('  2. p(X) bound writer to \'a\' and woke suspended goal');
    print('  3. p(X?) re-executed and succeeded (reader now bound)');

    expect(finalVal, isA<ConstTerm>());
    expect((finalVal as ConstTerm).value, 'a');
  });
}
