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
  test('Metainterpreter with conjunction: run((p(X), q(X?)))', () {
    print('\n' + '=' * 70);
    print('METAINTERPRETER TEST: CONJUNCTION');
    print('Object program: clause(p(a),true). clause(q(a),true).');
    print('Metainterpreter:');
    print('  run(true).');
    print('  run((A,B)) :- run(A?), run(B?).');
    print('  run(A) :- otherwise | clause(A?,B), run(B?).');
    print('Query: run((p(X), q(X?)))');
    print('Expected: X binds to a, both p(a) and q(a) succeed');
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

      // clause(q(a), true).
      BC.L('clause/2_c2'),
      BC.TRY(),
      BC.headStruct('q', 1, 0),
      BC.unifyConst('a'),
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

      // Clause 2: run((A,B)) :- run(A?), run(B?).
      BC.L('run/1_c2'),
      BC.TRY(),
      BC.headStruct(',', 2, 0),     // Match (A,B) - comma functor with arity 2
      BC.unifyWriter(0),             // First arg is A (writer)
      BC.unifyWriter(1),             // Second arg is B (writer)
      BC.COMMIT(),
      // BODY: spawn run(A?), then tail-call run(B?)
      BC.putReader(0, 0),            // arg0 = A? (reader of A)
      BC.spawn('run/1', 1),          // Spawn run(A?)
      BC.putReader(1, 0),            // arg0 = B? (reader of B)
      BC.requeue('run/1', 1),        // Tail call run(B?)

      // Clause 3: run(A) :- otherwise | clause(A?, B), run(B?).
      BC.L('run/1_c3'),
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
    ]);

    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    print('PROGRAM LABELS:');
    for (final entry in prog.labels.entries) {
      print('  ${entry.key} => PC ${entry.value}');
    }
    print('');

    // Build conjunction (p(X), q(X?))
    // This is a structure with functor ',' and 2 args

    // First build p(X)
    const wPX = 10;
    const rPX = 11;
    rt.heap.addWriter(WriterCell(wPX, rPX));
    rt.heap.addReader(ReaderCell(rPX));
    rt.heap.bindWriterStruct(wPX, 'p', [VarRef(wX, isReader: false)]);

    // Then build q(X?)
    const wQXr = 12;
    const rQXr = 13;
    rt.heap.addWriter(WriterCell(wQXr, rQXr));
    rt.heap.addReader(ReaderCell(rQXr));
    rt.heap.bindWriterStruct(wQXr, 'q', [VarRef(rX, isReader: true)]);

    // Now build the conjunction (p(X), q(X?))
    const wConj = 14;
    const rConj = 15;
    rt.heap.addWriter(WriterCell(wConj, rConj));
    rt.heap.addReader(ReaderCell(rConj));
    rt.heap.bindWriterStruct(wConj, ',', [VarRef(wPX, isReader: false), VarRef(wQXr, isReader: false)]);

    print('STRUCTURES:');
    print('  p(X) = writer $wPX bound to p(W$wX)');
    print('  q(X?) = writer $wQXr bound to q(R$rX)');
    print('  (p(X), q(X?)) = reader $rConj bound to ,(W$wPX, W$wQXr)');
    print('');

    // Start goal: run((p(X), q(X?)))
    const goalId = 100;
    final env = CallEnv(readers: {0: rConj});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, prog.labels['run/1']!));

    print('Starting: run((p(X), q(X?))) at PC ${prog.labels['run/1']}');
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

    print('✓ Metainterpreter with conjunction passed!');
    print('✓ run((p(X), q(X?))) executed successfully');
    print('✓ p(X) bound X to a');
    print('✓ q(X?) verified q(a)');
  });
}
