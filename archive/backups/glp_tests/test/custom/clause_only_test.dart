import 'package:test/test.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/asm.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';

void main() {
  test('Simple clause/2: clause(p(a), true)', () {
    print('\n' + '=' * 70);
    print('SIMPLE CLAUSE TEST');
    print('Program: clause(p(a), true).');
    print('Goal: clause(p(X), Y)');
    print('Expected: X binds to a, Y binds to true');
    print('=' * 70 + '\n');

    final rt = GlpRuntime();

    // Setup: Writers X and Y
    const wX = 1;
    const rX = 2;
    const wY = 3;
    const rY = 4;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));
    rt.heap.addWriter(WriterCell(wY, rY));
    rt.heap.addReader(ReaderCell(rY));

    print('HEAP SETUP:');
    print('  Writer $wX (X) paired with Reader $rX (X?)');
    print('  Writer $wY (Y) paired with Reader $rY (Y?)');
    print('');

    // Build p(X) structure for first argument
    const wPX = 10;
    const rPX = 11;
    rt.heap.addWriter(WriterCell(wPX, rPX));
    rt.heap.addReader(ReaderCell(rPX));
    rt.heap.bindWriterStruct(wPX, 'p', [WriterTerm(wX)]);

    // Program: clause(p(a), true).
    final prog = BC.prog([
      BC.L('clause/2_start'),
      BC.TRY(),
      // Match first argument: p(a)
      BC.headStruct('p', 1, 0),      // arg0 must be p(_) structure
      BC.unifyConst('a'),             // first subterm must be 'a'
      // Match second argument: true
      BC.headConst('true', 1),        // arg1 must be 'true'
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('clause/2_end'),
      BC.SUSP(),
    ]);

    print('PROGRAM:');
    for (var i = 0; i < prog.ops.length; i++) {
      print('  PC $i: ${prog.ops[i]}');
    }
    print('');

    // Goal: clause(p(X), Y)
    // Arg 0: p(X) structure (as reader rPX)
    // Arg 1: Y (as writer wY)
    const goalId = 100;
    final env = CallEnv(readers: {0: rPX}, writers: {1: wY});
    rt.setGoalEnv(goalId, env);

    final cx = RunnerContext(rt: rt, goalId: goalId, kappa: prog.labels['clause/2_start']!, env: env);

    print('Goal: clause(p(X), Y) at PC ${prog.labels['clause/2_start']}');
    print('CallEnv: readers={${env.readerBySlot}}, writers={${env.writerBySlot}}');
    print('');

    print('Checking arguments:');
    print('  env.r(0) = ${env.r(0)} (should be rPX=$rPX)');
    print('  env.r(1) = ${env.r(1)} (should be null)');
    print('  env.w(0) = ${env.w(0)} (should be null)');
    print('  env.w(1) = ${env.w(1)} (should be wY=$wY)');
    print('');

    final runner = BytecodeRunner(prog);
    final result = runner.runWithStatus(cx);

    print('Result: $result');
    print('');

    print('=' * 70);
    print('RESULTS');
    print('=' * 70);
    print('X (W$wX) bound: ${rt.heap.isWriterBound(wX)}');
    print('Y (W$wY) bound: ${rt.heap.isWriterBound(wY)}');

    expect(rt.heap.isWriterBound(wX), true, reason: 'X should be bound');
    expect(rt.heap.isWriterBound(wY), true, reason: 'Y should be bound');

    if (rt.heap.isWriterBound(wX)) {
      final xValue = rt.heap.valueOfWriter(wX);
      print('X value: $xValue');
      expect(xValue, isA<ConstTerm>(), reason: 'X should be ConstTerm');
      if (xValue is ConstTerm) {
        expect(xValue.value, 'a', reason: 'X should be bound to a');
        print('✓ X correctly bound to a');
      }
    }

    if (rt.heap.isWriterBound(wY)) {
      final yValue = rt.heap.valueOfWriter(wY);
      print('Y value: $yValue');
      expect(yValue, isA<ConstTerm>(), reason: 'Y should be ConstTerm');
      if (yValue is ConstTerm) {
        expect(yValue.value, 'true', reason: 'Y should be bound to true');
        print('✓ Y correctly bound to true');
      }
    }
    print('');

    print('✓ clause/2 working correctly!');
  });
}
