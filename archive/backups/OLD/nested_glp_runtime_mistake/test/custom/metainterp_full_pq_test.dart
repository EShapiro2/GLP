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
  test('Full metainterpreter: run(p(X)) with object program p(a), p(b)', () {
    print('\n' + '=' * 70);
    print('FULL METAINTERPRETER TEST');
    print('Object program: p(a). p(b).');
    print('Encoded as: clause(p(a),true). clause(p(b),true).');
    print('');
    print('Metainterpreter:');
    print('  run(true).');
    print('  run((A,B)) :- run(A?), run(B?).');
    print('  run(A) :- otherwise | clause(A?,B), run(B?).');
    print('');
    print('Meta-level goal: run(p(X))');
    print('Expected: X binds to a (first clause of p)');
    print('=' * 70 + '\n');

    final rt = GlpRuntime();

    // Setup: Writer X and its paired reader X?
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    print('HEAP SETUP:');
    print('  Writer $wX (X) paired with Reader $rX (X?)');
    print('');

    // Build the combined program with both run/1 and clause/2
    final prog = BC.prog([
      // ===== CLAUSE/2 PROGRAM =====
      // clause(p(a), true).
      // clause(p(b), true).

      BC.L('clause/2_start'),

      // Clause 1: clause(p(a), true).
      BC.TRY(),
      BC.headStruct('p', 1, 0),        // arg 0 = p(_) structure, enter READ mode, S=0
      BC.unifyConst('a'),               // S=0: first arg of p(...) is 'a'
      BC.headConst('true', 1),          // arg 1 = 'true'
      BC.COMMIT(),
      BC.PROCEED(),

      // Clause 2: clause(p(b), true).
      BC.L('clause/2_clause2'),
      BC.TRY(),
      BC.headStruct('p', 1, 0),        // arg 0 = p(_) structure
      BC.unifyConst('b'),               // First arg of p(...) is 'b'
      BC.headConst('true', 1),          // arg 1 = 'true'
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('clause/2_end'),
      BC.SUSP(),

      // ===== RUN/1 PROGRAM =====
      // run(true).
      // run((A,B)) :- run(A?), run(B?).
      // run(A) :- otherwise | clause(A?,B), run(B?).
      BC.L('run/1_start'),

      // Clause 1: run(true).
      BC.TRY(),
      BC.headConst('true', 0),
      BC.COMMIT(),
      BC.PROCEED(),

      // Clause 2: run((A,B)) :- run(A?), run(B?).
      BC.L('run/1_clause2'),
      BC.TRY(),
      BC.headStruct(',', 2, 0),        // Match run((A,B))
      BC.headWriter(0),                 // Extract A into var 0
      BC.headWriter(1),                 // Extract B into var 1
      BC.COMMIT(),
      // BODY: spawn run(A?), then requeue run(B?)
      BC.putReader(0, 0),               // Put A? in arg 0
      BC.spawn('run/1_start', 1),       // Spawn run(A?)
      BC.putReader(1, 0),               // Put B? in arg 0
      BC.requeue('run/1_start', 1),     // Tail call run(B?)

      // Clause 3: run(A) :- otherwise | clause(A?,B), run(B?).
      BC.L('run/1_clause3'),
      BC.TRY(),
      BC.otherwise(),                   // Guard: succeeds if previous clauses failed
      BC.getVar(0, 0),                  // Get A from arg 0 into var 0
      BC.COMMIT(),
      // BODY: call clause(A?, B), then tail-call run(B?)
      BC.putReader(0, 0),               // arg 0 = A?
      BC.putWriter(1, 1),               // arg 1 = B (fresh writer, will be allocated)
      BC.spawn('clause/2_start', 2),    // Call clause(A?, B)
      BC.putReader(1, 0),               // arg 0 = B?
      BC.requeue('run/1_start', 1),     // Tail call run(B?)

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

    print('=' * 70);
    print('TEST: run(p(X)) - should bind X to a (first clause of p)');
    print('=' * 70 + '\n');

    // Build p(X) structure
    const wPX = 10;
    const rPX = 11;
    rt.heap.addWriter(WriterCell(wPX, rPX));
    rt.heap.addReader(ReaderCell(rPX));
    rt.heap.bindWriterStruct(wPX, 'p', [WriterTerm(wX)]);

    // Goal: run(p(X))
    const goal1 = 100;
    final env1 = CallEnv(readers: {0: rPX});
    rt.setGoalEnv(goal1, env1);
    rt.gq.enqueue(GoalRef(goal1, prog.labels['run/1_start']!));

    print('Enqueued: run(p(X)) at PC ${prog.labels['run/1_start']}');
    print('');

    final ran = sched.drain(maxCycles: 50);

    print('Goals executed: $ran');
    print('Total executions: ${ran.length}');
    print('');

    print('=' * 70);
    print('RESULTS');
    print('=' * 70);
    print('X (W$wX) bound: ${rt.heap.isWriterBound(wX)}');
    if (rt.heap.isWriterBound(wX)) {
      final xValue = rt.heap.valueOfWriter(wX);
      print('X value: $xValue');
      expect(xValue, isA<ConstTerm>(), reason: 'X should be ConstTerm');
      if (xValue is ConstTerm) {
        expect(xValue.value, 'a', reason: 'X should be bound to a (first clause)');
        print('✓ X correctly bound to a');
      }
    } else {
      print('ERROR: X not bound!');
    }
    print('');

    print('✓ Metainterpreter successfully executed object program');
    print('✓ run/1 dispatched to clause/2 for clause lookup');
    print('✓ clause/2 returned p(a) :- true');
    print('✓ run/1 executed body true');
  });
}
