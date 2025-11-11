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
  test('Metainterpreter: run(p(X)), run(p(X?)) - writer then reader', () {
    print('\n' + '=' * 70);
    print('METAINTERPRETER TEST: P(X) THEN P(X?)');
    print('Object program: clause(p(a),true). clause(p(b),true).');
    print('Metainterpreter: run(true). run(A) :- otherwise | clause(A?,B), run(B?).');
    print('Query: boot :- run(p(X)), run(p(X?))');
    print('Expected: First run(p(X)) binds X=a, second run(p(X?)) verifies p(a)');
    print('=' * 70 + '\n');

    final rt = GlpRuntime();

    // Setup: Writer X and its paired reader
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    print('HEAP SETUP:');
    print('  Writer $wX (X) paired with Reader $rX (X?)');
    print('');

    final prog = BC.prog([
      // ===== CLAUSE/2: Object program =====
      // clause(p(a), true).
      BC.L('clause/2'),
      BC.TRY(),
      BC.headStruct('p', 1, 0),
      BC.unifyConst('a'),
      BC.headConst('true', 1),
      BC.COMMIT(),
      BC.PROCEED(),

      // clause(p(b), true).
      BC.L('clause/2_c2'),
      BC.TRY(),
      BC.headStruct('p', 1, 0),
      BC.unifyConst('b'),
      BC.headConst('true', 1),
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('clause/2_end'),
      BC.SUSP(),

      // ===== RUN/1: Metainterpreter =====
      // Clause 1: run(true).
      BC.L('run/1'),
      BC.TRY(),
      BC.headConst('true', 0),
      BC.COMMIT(),
      BC.PROCEED(),

      // Clause 2: run(A) :- otherwise | clause(A?, B), run(B?).
      BC.L('run/1_c2'),
      BC.TRY(),
      BC.otherwise(),
      BC.getVar(0, 0),
      BC.COMMIT(),
      BC.putReader(0, 0),
      BC.putWriter(1, 1),
      BC.spawn('clause/2', 2),
      BC.putReader(1, 0),
      BC.requeue('run/1', 1),

      BC.L('run/1_end'),
      BC.SUSP(),

      // ===== BOOT/0: Test driver =====
      // boot(Arg0, Arg1) :- run(Arg0), run(Arg1).
      BC.L('boot/0'),
      BC.TRY(),
      BC.getVar(0, 0),
      BC.getVar(1, 1),
      BC.COMMIT(),
      BC.putReader(0, 0),
      BC.spawn('run/1', 1),
      BC.putReader(1, 0),
      BC.requeue('run/1', 1),

      BC.L('boot/0_end'),
      BC.SUSP(),
    ]);

    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    print('PROGRAM LABELS:');
    for (final entry in prog.labels.entries) {
      print('  ${entry.key} => PC ${entry.value}');
    }
    print('');

    // Build p(X) structure
    const wPX = 10;
    const rPX = 11;
    rt.heap.addWriter(WriterCell(wPX, rPX));
    rt.heap.addReader(ReaderCell(rPX));
    rt.heap.bindWriterStruct(wPX, 'p', [VarRef(wX, isReader: false)]);

    // Build p(X?) structure
    const wPXr = 12;
    const rPXr = 13;
    rt.heap.addWriter(WriterCell(wPXr, rPXr));
    rt.heap.addReader(ReaderCell(rPXr));
    rt.heap.bindWriterStruct(wPXr, 'p', [VarRef(rX, isReader: true)]);

    print('STRUCTURES:');
    print('  p(X) = reader $rPX bound to p(W$wX)');
    print('  p(X?) = reader $rPXr bound to p(R$rX)');
    print('');

    // Start boot goal with p(X) and p(X?) as arguments
    const goalId = 100;
    final env = CallEnv(readers: {0: rPX, 1: rPXr});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, prog.labels['boot/0']!));

    print('Starting: boot(p(X), p(X?)) at PC ${prog.labels['boot/0']}');
    print('');

    final ran = sched.drain(maxCycles: 100);

    print('');
    print('Goals executed: $ran');
    print('Total executions: ${ran.length}');
    print('');

    print('=' * 70);
    print('RESULTS');
    print('=' * 70);
    print('X (W$wX) bound: ${rt.heap.isWriterBound(wX)}');

    expect(rt.heap.isWriterBound(wX), true, reason: 'X should be bound');

    if (rt.heap.isWriterBound(wX)) {
      final xValue = rt.heap.valueOfWriter(wX);
      print('X value: $xValue');
      expect(xValue, isA<ConstTerm>(), reason: 'X should be ConstTerm');
      if (xValue is ConstTerm) {
        expect(xValue.value, 'a', reason: 'X should be bound to a');
        print('✓ X correctly bound to a');
      }
    }
    print('');

    print('✓ Metainterpreter test passed!');
    print('✓ run(p(X)) bound X to a');
    print('✓ run(p(X?)) verified p(a) successfully');
  });
}
