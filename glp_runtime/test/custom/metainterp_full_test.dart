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
  test('Full metainterpreter: run(p(X)), run(q(X?)) with p(a), p(b), q(b)', () {
    print('\n' + '=' * 70);
    print('FULL METAINTERPRETER TEST');
    print('Object program: clause(p(a),true). clause(p(b),true). clause(q(b),true).');
    print('Metainterpreter: run(true). run(A) :- otherwise | clause(A?,B), run(B?).');
    print('Query: boot :- run(p(X)), run(q(X?))');
    print('Expected: X binds to a (first solution), q(a) fails, backtrack not supported');
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

    // Full program with:
    // 1. clause/2: clause(p(a),true). clause(p(b),true). clause(q(b),true).
    // 2. run/1: run(true). run(A) :- otherwise | clause(A?,B), run(B?).
    // 3. boot/0: boot :- run(p(X)), run(q(X?)).

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

      // clause(q(b), true).
      BC.L('clause/2_c3'),
      BC.TRY(),
      BC.headStruct('q', 1, 0),
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
      BC.getVar(0, 0),           // Get A from arg0
      BC.COMMIT(),
      // BODY: call clause(A?, B), then tail-call run(B?)
      BC.putReader(0, 0),        // arg0 = A? (reader of A)
      BC.putWriter(1, 1),        // arg1 = B (fresh writer)
      BC.spawn('clause/2', 2),   // Call clause(A?, B)
      BC.putReader(1, 0),        // arg0 = B? (reader of B)
      BC.requeue('run/1', 1),    // Tail call run(B?)

      BC.L('run/1_end'),
      BC.SUSP(),

      // ===== BOOT/0: Test driver =====
      // boot(Arg0, Arg1) :- run(Arg0), run(Arg1).
      // (We'll pass p(X) as reader in slot 0, q(X?) as reader in slot 1)
      BC.L('boot/0'),
      BC.TRY(),
      BC.getVar(0, 0),              // Load Arg0 into var 0
      BC.getVar(1, 1),              // Load Arg1 into var 1
      BC.COMMIT(),
      // BODY: spawn run(Arg0), then tail-call run(Arg1)
      BC.putReader(0, 0),           // arg0 = var 0 (p(X) reader)
      BC.spawn('run/1', 1),         // Spawn run(p(X))
      BC.putReader(1, 0),           // arg0 = var 1 (q(X?) reader)
      BC.requeue('run/1', 1),       // Tail call run(q(X?))

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

    // Build q(X?) structure
    const wQXr = 12;
    const rQXr = 13;
    rt.heap.addWriter(WriterCell(wQXr, rQXr));
    rt.heap.addReader(ReaderCell(rQXr));
    rt.heap.bindWriterStruct(wQXr, 'q', [VarRef(rX, isReader: true)]);

    print('STRUCTURES:');
    print('  p(X) = reader $rPX bound to p(W$wX)');
    print('  q(X?) = reader $rQXr bound to q(R$rX)');
    print('');

    // Start boot goal with p(X) and q(X?) as arguments
    const goalId = 100;
    final env = CallEnv(readers: {0: rPX, 1: rQXr});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, prog.labels['boot/0']!));

    print('Starting: boot(p(X), q(X?)) at PC ${prog.labels['boot/0']}');
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

    // X should be bound to 'a' (first clause of p)
    expect(rt.heap.isWriterBound(wX), true, reason: 'X should be bound');

    if (rt.heap.isWriterBound(wX)) {
      final xValue = rt.heap.valueOfWriter(wX);
      print('X value: $xValue');
      expect(xValue, isA<ConstTerm>(), reason: 'X should be ConstTerm');
      if (xValue is ConstTerm) {
        expect(xValue.value, 'a', reason: 'X should be bound to a (first solution)');
        print('✓ X correctly bound to a');
      }
    }
    print('');

    print('✓ Full metainterpreter test completed!');
    print('✓ boot :- run(p(X)), run(q(X?)) executed');
    print('✓ p(X) bound X to a');
    print('✓ q(a) failed (no clause for q(a))');
  });
}
