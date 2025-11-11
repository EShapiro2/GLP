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
  test('Three-way circular merge (direct): merge([a|Xs?],Ys?,Zs), merge([b|Ys?],Zs?,Xs), merge([c|Zs?],Xs?,Ys)', () {
    print('\n' + '=' * 70);
    print('THREE-WAY CIRCULAR MERGE (DIRECT - NO METAINTERPRETER)');
    print('Goals:');
    print('  Goal 100: merge([a|Xs?], Ys?, Zs)');
    print('  Goal 200: merge([b|Ys?], Zs?, Xs)');
    print('  Goal 300: merge([c|Zs?], Xs?, Ys)');
    print('Expected: Creates infinite circular streams');
    print('  Xs = [b, c, b, c, ...]  (from merge 2 output)');
    print('  Ys = [c, a, c, a, ...]  (from merge 3 output)');
    print('  Zs = [a, b, a, b, ...]  (from merge 1 output)');
    print('=' * 70 + '\n');

    final rt = GlpRuntime();

    // Setup: Writers for Xs, Ys, Zs
    const varXs = 1;
    
    rt.heap.addWriter(WriterCell(varXs, varXs));
    rt.heap.addReader(ReaderCell(varXs));

    const varYs = 3;
    
    rt.heap.addWriter(WriterCell(varYs, varYs));
    rt.heap.addReader(ReaderCell(varYs));

    const varZs = 5;
    
    rt.heap.addWriter(WriterCell(varZs, varZs));
    rt.heap.addReader(ReaderCell(varZs));

    print('HEAP SETUP:');
    print('  Writer $varXs (Xs) paired with Reader $varXs (Xs?)');
    print('  Writer $varYs (Ys) paired with Reader $varYs (Ys?)');
    print('  Writer $varZs (Zs) paired with Reader $varZs (Zs?)');
    print('');

    // Merge program using the working pattern from merge_circular_pure_trace
    final prog = BC.prog([
      BC.L('merge/3_start'),

      // Clause 1: merge([X|Xs],Ys,[X?|Zs?]) :- merge(Ys?,Xs?,Zs).
      BC.TRY(),
      BC.headStruct('.', 2, 0),
      BC.headWriter(0),
      BC.headWriter(1),
      BC.getVar(2, 1),
      BC.headStruct('.', 2, 2),
      BC.headReader(0),
      BC.headWriter(3),
      BC.COMMIT(),
      BC.putReader(2, 0),
      BC.putReader(1, 1),
      BC.putWriter(3, 2),
      BC.requeue('merge/3_start', 3),

      // Clause 2: merge(Xs,[Y|Ys],[Y?|Zs?]) :- merge(Xs?,Ys?,Zs).
      BC.L('merge/3_clause2'),
      BC.TRY(),
      BC.getVar(0, 0),
      BC.headStruct('.', 2, 1),
      BC.headWriter(1),
      BC.headWriter(2),
      BC.headStruct('.', 2, 2),
      BC.headReader(1),
      BC.headWriter(3),
      BC.COMMIT(),
      BC.putReader(0, 0),
      BC.putReader(2, 1),
      BC.putWriter(3, 2),
      BC.requeue('merge/3_start', 3),

      // Clause 3: merge([],[],[]).
      BC.L('merge/3_clause3'),
      BC.TRY(),
      BC.headConst(null, 0),
      BC.headConst(null, 1),
      BC.headConst(null, 2),
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('merge/3_end'),
      BC.SUSP(),
    ]);

    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    print('PROGRAM LABELS:');
    for (final entry in prog.labels.entries) {
      print('  ${entry.key} => PC ${entry.value}');
    }
    print('');

    // Build [a|Xs?] using '.' functor (from working test)
    const varListA = 10;
    
    rt.heap.addWriter(WriterCell(varListA, varListA));
    rt.heap.addReader(ReaderCell(varListA));
    rt.heap.bindWriterStruct(varListA, '.', [ConstTerm('a'), VarRef(varXs, isReader: true)]);

    // Build [b|Ys?]
    const varListB = 12;
    
    rt.heap.addWriter(WriterCell(varListB, varListB));
    rt.heap.addReader(ReaderCell(varListB));
    rt.heap.bindWriterStruct(varListB, '.', [ConstTerm('b'), VarRef(varYs, isReader: true)]);

    // Build [c|Zs?]
    const varListC = 14;
    
    rt.heap.addWriter(WriterCell(varListC, varListC));
    rt.heap.addReader(ReaderCell(varListC));
    rt.heap.bindWriterStruct(varListC, '.', [ConstTerm('c'), VarRef(varZs, isReader: true)]);

    print('STRUCTURES:');
    print('  [a|Xs?] = writer $varListA');
    print('  [b|Ys?] = writer $varListB');
    print('  [c|Zs?] = writer $varListC');
    print('');

    // Goal 1: merge([a|Xs?], Ys?, Zs)
    const goal1 = 100;
    final env1 = CallEnv(
      writers: {0: varListA, 2: varZs},
      readers: {1: varYs},
    );
    rt.setGoalEnv(goal1, env1);
    rt.gq.enqueue(GoalRef(goal1, prog.labels['merge/3_start']!));

    // Goal 2: merge([b|Ys?], Zs?, Xs)
    const goal2 = 200;
    final env2 = CallEnv(
      writers: {0: varListB, 2: varXs},
      readers: {1: varZs},
    );
    rt.setGoalEnv(goal2, env2);
    rt.gq.enqueue(GoalRef(goal2, prog.labels['merge/3_start']!));

    // Goal 3: merge([c|Zs?], Xs?, Ys)
    const goal3 = 300;
    final env3 = CallEnv(
      writers: {0: varListC, 2: varYs},
      readers: {1: varXs},
    );
    rt.setGoalEnv(goal3, env3);
    rt.gq.enqueue(GoalRef(goal3, prog.labels['merge/3_start']!));

    print('Starting three goals:');
    print('  Goal 100: merge([a|Xs?], Ys?, Zs)');
    print('  Goal 200: merge([b|Ys?], Zs?, Xs)');
    print('  Goal 300: merge([c|Zs?], Xs?, Ys)');
    print('');

    final ran = sched.drain(maxCycles: 50);

    print('');
    print('Goals executed: ${ran.length} executions');
    print('');

    print('=' * 70);
    print('RESULTS');
    print('=' * 70);
    print('Xs (W$varXs) bound: ${rt.heap.isWriterBound(varXs)}');
    print('Ys (W$varYs) bound: ${rt.heap.isWriterBound(varYs)}');
    print('Zs (W$varZs) bound: ${rt.heap.isWriterBound(varZs)}');

    if (rt.heap.isWriterBound(varXs)) {
      final xsValue = rt.heap.valueOfWriter(varXs);
      print('Xs value: $xsValue');
    }

    if (rt.heap.isWriterBound(varYs)) {
      final ysValue = rt.heap.valueOfWriter(varYs);
      print('Ys value: $ysValue');
    }

    if (rt.heap.isWriterBound(varZs)) {
      final zsValue = rt.heap.valueOfWriter(varZs);
      print('Zs value: $zsValue');
    }

    print('');
    print('✓ Three-way circular merge (direct) test complete');
    print('Note: This creates infinite circular streams - execution limited by maxCycles');
  });
}
