import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/asm.dart';

void main() {
  test('Program: multi_pred, Goal: q(X), p(X?)', () {
    print('\n=== Program: multi_pred ===');
    print('p(a).');
    print('p(b).');
    print('q(b).');
    print('\n=== Goal: q(X), p(X?) ===\n');

    final rt = GlpRuntime();

    // Create writer/reader pair for X/X?
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    print('Heap Setup:');
    print('  Writer $wX (X) - unbound');
    print('  Reader $rX (X?) - paired with Writer $wX\n');

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

    // Bytecode for p/1 with reader check: p(a). p(b).
    // We need to check if reader matches 'b' (clause 2)
    final progPReader = BC.prog([
      BC.L('p/1_start'),
      BC.TRY(),              // Clause 1: p(a)
      BC.R(rX),              // Check reader
      BC.U('p/1_c2'),        // If suspended, try next clause

      BC.L('p/1_c2'),
      BC.TRY(),              // Clause 2: p(b)
      BC.R(rX),              // Check reader (should be bound to 'b')
      BC.U('p/1_end'),       // If suspended, go to end
      BC.PROCEED(),          // Success!

      BC.L('p/1_end'),
      BC.SUSP(),
    ]);

    // Goal 1: q(X) where X is writer
    print('--- Executing Goal 1: q(X) ---');
    print('Expected: Bind X to \'b\'\n');

    final cx1 = RunnerContext(
      rt: rt,
      goalId: 100,
      kappa: 0,
    );

    final result1 = BytecodeRunner(progQ).runWithStatus(cx1);
    print('Result: $result1');
    print('Writer $wX bound? ${rt.heap.isWriterBound(wX)}');
    if (rt.heap.isWriterBound(wX)) {
      final val = rt.heap.valueOfWriter(wX);
      print('Writer $wX value: $val');
    }
    print('');

    expect(result1, RunResult.terminated);
    expect(rt.heap.isWriterBound(wX), true);
    final val1 = rt.heap.valueOfWriter(wX);
    expect(val1, isA<ConstTerm>());
    expect((val1 as ConstTerm).value, 'b');

    // Goal 2: p(X?) where X? is reader (now bound to 'b')
    print('--- Executing Goal 2: p(X?) ---');
    print('Reader $rX reads \'b\'');
    print('Expected: Match clause 2 of p (p(b)) and succeed\n');

    final cx2 = RunnerContext(
      rt: rt,
      goalId: 200,
      kappa: 0,
    );

    final result2 = BytecodeRunner(progPReader).runWithStatus(cx2);
    print('Result: $result2');
    print('Reader $rX bound? ${rt.heap.isWriterBound(wX)}');
    if (rt.heap.isWriterBound(wX)) {
      final val = rt.heap.valueOfWriter(wX);
      print('Reader $rX reads value: $val');
    }
    print('');

    expect(result2, RunResult.terminated, reason: 'p(b) should succeed via clause 2');

    print('=== Final State ===');
    final finalVal = rt.heap.valueOfWriter(wX);
    print('X = $finalVal');
    print('SUCCESS ✓');
    print('\nExecution Summary:');
    print('  1. q(X) bound writer X to \'b\'');
    print('  2. p(X?) checked reader (reads \'b\')');
    print('  3. p(b) matched via clause 2 → success');

    expect(finalVal, isA<ConstTerm>());
    expect((finalVal as ConstTerm).value, 'b');
  });
}
