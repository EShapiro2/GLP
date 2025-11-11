import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/asm.dart';

void main() {
  test('Circular merge with 100 reduction budget: merge(Xs?,[1],Ys), merge(Ys?,[a],Xs)', () {
    print('\n' + '=' * 70);
    print('CIRCULAR MERGE WITH REDUCTION BUDGET');
    print('Goal 100: merge(Xs?, [1], Ys)');
    print('Goal 200: merge(Ys?, [a], Xs)');
    print('Reduction budget: 100 per goal execution');
    print('=' * 70 + '\n');

    final rt = GlpRuntime();

    // Build list [1]
    const wList1 = 10;
    const rList1 = 11;
    rt.heap.addWriter(WriterCell(wList1, rList1));
    rt.heap.addReader(ReaderCell(rList1));
    rt.heap.bindWriterStruct(wList1, '.', [
      ConstTerm(1),
      ConstTerm(null),
    ]);

    // Build list [a]
    const wListA = 20;
    const rListA = 21;
    rt.heap.addWriter(WriterCell(wListA, rListA));
    rt.heap.addReader(ReaderCell(rListA));
    rt.heap.bindWriterStruct(wListA, '.', [
      ConstTerm('a'),
      ConstTerm(null),
    ]);

    // Result writers Xs and Ys
    const wXs = 30;
    const rXs = 31;
    rt.heap.addWriter(WriterCell(wXs, rXs));
    rt.heap.addReader(ReaderCell(rXs));

    const wYs = 40;
    const rYs = 41;
    rt.heap.addWriter(WriterCell(wYs, rYs));
    rt.heap.addReader(ReaderCell(rYs));

    print('HEAP SETUP:');
    print('  List1 (W$wList1/R$rList1) = [1]');
    print('  ListA (W$wListA/R$rListA) = [a]');
    print('  Result Xs (W$wXs/R$rXs) = unbound');
    print('  Result Ys (W$wYs/R$rYs) = unbound');
    print('');

    // Full merge program
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

    print('=' * 70);
    print('EXECUTION TRACE');
    print('=' * 70 + '\n');

    // Goal 100: merge(Xs?, [1], Ys)
    print('--- GOAL 100: merge(Xs?, [1], Ys) ---');
    const goal1 = 100;
    final env1 = CallEnv(readers: {0: rXs, 1: rList1}, writers: {2: wYs});
    rt.setGoalEnv(goal1, env1);

    print('Executing with 100 reduction budget...');
    final cx1 = RunnerContext(
      rt: rt,
      goalId: goal1,
      kappa: 0,
      env: env1,
      reductionBudget: 100,
    );
    final result1 = runner.runWithStatus(cx1);
    print('Result: $result1');
    print('Reductions used: ${cx1.reductionsUsed}');
    print('Xs bound: ${rt.heap.isWriterBound(wXs)}');
    print('Ys bound: ${rt.heap.isWriterBound(wYs)}');
    if (rt.heap.isWriterBound(wYs)) {
      print('Ys value: ${rt.heap.valueOfWriter(wYs)}');
    }
    print('Suspended on readers: ${cx1.U}');
    print('');

    // Goal 200: merge(Ys?, [a], Xs)
    print('--- GOAL 200: merge(Ys?, [a], Xs) ---');
    const goal2 = 200;
    final env2 = CallEnv(readers: {0: rYs, 1: rListA}, writers: {2: wXs});
    rt.setGoalEnv(goal2, env2);

    print('Executing with 100 reduction budget...');
    final cx2 = RunnerContext(
      rt: rt,
      goalId: goal2,
      kappa: 0,
      env: env2,
      reductionBudget: 100,
    );
    final result2 = runner.runWithStatus(cx2);
    print('Result: $result2');
    print('Reductions used: ${cx2.reductionsUsed}');
    print('Xs bound: ${rt.heap.isWriterBound(wXs)}');
    if (rt.heap.isWriterBound(wXs)) {
      print('Xs value: ${rt.heap.valueOfWriter(wXs)}');
    }
    print('Ys bound: ${rt.heap.isWriterBound(wYs)}');
    if (rt.heap.isWriterBound(wYs)) {
      print('Ys value: ${rt.heap.valueOfWriter(wYs)}');
    }
    print('');

    // Check if any goals were activated
    print('--- CHECKING ACTIVATIONS ---');
    print('Goals in queue: ${rt.gq.length}');

    // Show what's in the queue
    var queueSnapshot = <int>[];
    var tempQueue = <GoalRef>[];
    while (rt.gq.length > 0) {
      final ref = rt.gq.dequeue();
      if (ref != null) {
        queueSnapshot.add(ref.id);
        tempQueue.add(ref);
      }
    }
    for (final ref in tempQueue) {
      rt.gq.enqueue(ref);
    }
    print('Queue contents: $queueSnapshot');
    print('');

    print('--- SUSPENSION/ACTIVATION TRACE ---');
    print('');

    var cycle = 0;
    while (rt.gq.length > 0 && cycle < 10) {
      final act = rt.gq.dequeue();
      if (act == null) break;
      cycle++;

      print('CYCLE $cycle:');
      print('  Dequeued: Goal ${act.id} (PC ${act.pc})');
      print('  Queue before execution: ${rt.gq.length} goals');

      final env = rt.getGoalEnv(act.id);
      final cx = RunnerContext(
        rt: rt,
        goalId: act.id,
        kappa: act.pc,
        env: env,
        reductionBudget: 100,
      );
      final result = runner.runWithStatus(cx);

      print('  Execution result: $result');
      print('  Reductions used: ${cx.reductionsUsed}');
      print('  Queue after execution: ${rt.gq.length} goals');

      // Show what's in queue now
      queueSnapshot = <int>[];
      tempQueue = <GoalRef>[];
      while (rt.gq.length > 0) {
        final ref = rt.gq.dequeue();
        if (ref != null) {
          queueSnapshot.add(ref.id);
          tempQueue.add(ref);
        }
      }
      for (final ref in tempQueue) {
        rt.gq.enqueue(ref);
      }
      if (queueSnapshot.isNotEmpty) {
        print('  Queue now contains: $queueSnapshot');
      }
      print('');
    }

    print('');
    print('=' * 70);
    print('FINAL STATE AFTER $cycle CYCLES');
    print('=' * 70);
    print('Xs bound: ${rt.heap.isWriterBound(wXs)}');
    if (rt.heap.isWriterBound(wXs)) {
      print('Xs value: ${rt.heap.valueOfWriter(wXs)}');
    }
    print('Ys bound: ${rt.heap.isWriterBound(wYs)}');
    if (rt.heap.isWriterBound(wYs)) {
      print('Ys value: ${rt.heap.valueOfWriter(wYs)}');
    }
    print('Goals remaining in queue: ${rt.gq.length}');
    print('');

    print('✓ Circular merge with reduction budget test complete');
  });
}
