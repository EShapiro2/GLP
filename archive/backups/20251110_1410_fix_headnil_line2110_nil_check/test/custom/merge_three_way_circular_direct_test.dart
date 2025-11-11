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
    const wXs = 1;
    const rXs = 2;
    rt.heap.addWriter(WriterCell(wXs, rXs));
    rt.heap.addReader(ReaderCell(rXs));

    const wYs = 3;
    const rYs = 4;
    rt.heap.addWriter(WriterCell(wYs, rYs));
    rt.heap.addReader(ReaderCell(rYs));

    const wZs = 5;
    const rZs = 6;
    rt.heap.addWriter(WriterCell(wZs, rZs));
    rt.heap.addReader(ReaderCell(rZs));

    print('HEAP SETUP:');
    print('  Writer $wXs (Xs) paired with Reader $rXs (Xs?)');
    print('  Writer $wYs (Ys) paired with Reader $rYs (Ys?)');
    print('  Writer $wZs (Zs) paired with Reader $rZs (Zs?)');
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
    const wListA = 10;
    const rListA = 11;
    rt.heap.addWriter(WriterCell(wListA, rListA));
    rt.heap.addReader(ReaderCell(rListA));
    rt.heap.bindWriterStruct(wListA, '.', [ConstTerm('a'), ReaderTerm(rXs)]);

    // Build [b|Ys?]
    const wListB = 12;
    const rListB = 13;
    rt.heap.addWriter(WriterCell(wListB, rListB));
    rt.heap.addReader(ReaderCell(rListB));
    rt.heap.bindWriterStruct(wListB, '.', [ConstTerm('b'), ReaderTerm(rYs)]);

    // Build [c|Zs?]
    const wListC = 14;
    const rListC = 15;
    rt.heap.addWriter(WriterCell(wListC, rListC));
    rt.heap.addReader(ReaderCell(rListC));
    rt.heap.bindWriterStruct(wListC, '.', [ConstTerm('c'), ReaderTerm(rZs)]);

    print('STRUCTURES:');
    print('  [a|Xs?] = writer $wListA');
    print('  [b|Ys?] = writer $wListB');
    print('  [c|Zs?] = writer $wListC');
    print('');

    // Goal 1: merge([a|Xs?], Ys?, Zs)
    const goal1 = 100;
    final env1 = CallEnv(
      writers: {0: wListA, 2: wZs},
      readers: {1: rYs},
    );
    rt.setGoalEnv(goal1, env1);
    rt.gq.enqueue(GoalRef(goal1, prog.labels['merge/3_start']!));

    // Goal 2: merge([b|Ys?], Zs?, Xs)
    const goal2 = 200;
    final env2 = CallEnv(
      writers: {0: wListB, 2: wXs},
      readers: {1: rZs},
    );
    rt.setGoalEnv(goal2, env2);
    rt.gq.enqueue(GoalRef(goal2, prog.labels['merge/3_start']!));

    // Goal 3: merge([c|Zs?], Xs?, Ys)
    const goal3 = 300;
    final env3 = CallEnv(
      writers: {0: wListC, 2: wYs},
      readers: {1: rXs},
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
    print('Xs (W$wXs) bound: ${rt.heap.isWriterBound(wXs)}');
    print('Ys (W$wYs) bound: ${rt.heap.isWriterBound(wYs)}');
    print('Zs (W$wZs) bound: ${rt.heap.isWriterBound(wZs)}');

    if (rt.heap.isWriterBound(wXs)) {
      final xsValue = rt.heap.valueOfWriter(wXs);
      print('Xs value: $xsValue');
    }

    if (rt.heap.isWriterBound(wYs)) {
      final ysValue = rt.heap.valueOfWriter(wYs);
      print('Ys value: $ysValue');
    }

    if (rt.heap.isWriterBound(wZs)) {
      final zsValue = rt.heap.valueOfWriter(wZs);
      print('Zs value: $zsValue');
    }

    print('');
    print('✓ Three-way circular merge (direct) test complete');
    print('Note: This creates infinite circular streams - execution limited by maxCycles');
  });
}
