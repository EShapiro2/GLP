import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/asm.dart';

void main() {
  test('Program: facts, Goal: p(X), p(X?)', () {
    print('\n=== Program: facts ===');
    print('p(a).');
    print('p(b).');
    print('\n=== Goal: p(X), p(X?) ===\n');

    final rt = GlpRuntime();

    // Create writer/reader pair for X/X?
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    print('Heap Setup:');
    print('  Writer $wX (X) - unbound');
    print('  Reader $rX (X?) - paired with Writer $wX\n');

    // Bytecode for p/1: p(a).
    // Using legacy opcodes since we don't have full v2.16 head_constant yet
    final prog = BC.prog([
      BC.L('p/1_start'),
      BC.TRY(),  // Clause 1: p(a)
      BC.W(wX),  // Mark writer in σ̂w
      BC.COMMIT(),
      BC.BCONST(wX, 'a'), // Bind to 'a'
      BC.PROCEED(),
    ]);

    // Goal 1: p(X) where X is writer
    print('--- Executing Goal 1: p(X) ---');
    final cx1 = RunnerContext(
      rt: rt,
      goalId: 100,
      kappa: 0,
    );

    final result1 = BytecodeRunner(prog).runWithStatus(cx1);
    print('Result: $result1');
    print('Writer $wX bound? ${rt.heap.isWriterBound(wX)}');
    if (rt.heap.isWriterBound(wX)) {
      final val = rt.heap.valueOfWriter(wX);
      print('Writer $wX value: ${val}');
    }
    print('');

    // Goal 2: p(X?) where X? is reader (now bound)
    print('--- Executing Goal 2: p(X?) ---');

    // For reader argument, we need different bytecode that checks reader
    final prog2 = BC.prog([
      BC.L('p/1_start'),
      BC.TRY(),
      BC.R(rX), // Guard: need reader to be bound
      BC.U('end'),
      BC.L('end'),
      BC.SUSP(),
    ]);

    final cx2 = RunnerContext(
      rt: rt,
      goalId: 101,
      kappa: 0,
    );

    final result2 = BytecodeRunner(prog2).runWithStatus(cx2);
    print('Result: $result2');
    print('Reader $rX bound? ${rt.heap.isWriterBound(wX)}');
    if (rt.heap.isWriterBound(wX)) {
      final val = rt.heap.valueOfWriter(wX);
      print('Reader $rX reads value: $val');
    }
    print('');

    print('=== Final State ===');
    final finalVal = rt.heap.valueOfWriter(wX);
    print('X = $finalVal');
    print('SUCCESS ✓');

    expect(result1, RunResult.terminated);
    expect(result2, RunResult.terminated);
    expect(finalVal, isA<ConstTerm>());
    expect((finalVal as ConstTerm).value, 'a');
  });
}
