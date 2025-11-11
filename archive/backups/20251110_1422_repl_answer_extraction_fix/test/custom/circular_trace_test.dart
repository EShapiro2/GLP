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
  test('Circular trace: merge(Xs?,[1],Ys), merge(Ys?,[2],Xs)', () {
    print('\n=== Tracing Circular Execution ===\n');

    final rt = GlpRuntime();

    // Xs - unbound writer
    const wXs = 1;
    const rXs = 2;
    rt.heap.addWriter(WriterCell(wXs, rXs));
    rt.heap.addReader(ReaderCell(rXs));

    // Ys - unbound writer
    const wYs = 3;
    const rYs = 4;
    rt.heap.addWriter(WriterCell(wYs, rYs));
    rt.heap.addReader(ReaderCell(rYs));

    // Build list [1]
    const wList1 = 5;
    const rList1 = 6;
    rt.heap.addWriter(WriterCell(wList1, rList1));
    rt.heap.addReader(ReaderCell(rList1));
    rt.heap.bindWriterStruct(wList1, '.', [
      ConstTerm(1),
      ConstTerm(null)
    ]);

    // Build list [2]
    const wList2 = 7;
    const rList2 = 8;
    rt.heap.addWriter(WriterCell(wList2, rList2));
    rt.heap.addReader(ReaderCell(rList2));
    rt.heap.bindWriterStruct(wList2, '.', [
      ConstTerm(2),
      ConstTerm(null)
    ]);

    print('Initial Heap:');
    print('  Writer $wXs (Xs) - unbound, Reader $rXs');
    print('  Writer $wYs (Ys) - unbound, Reader $rYs');
    print('  Writer $wList1 - bound to [1], Reader $rList1');
    print('  Writer $wList2 - bound to [2], Reader $rList2\n');

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

    // Create scheduler and runner
    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Goal 1: merge(Xs?, [1], Ys)
    print('=== Goal 4000: merge(Xs?, [1], Ys) ===');
    print('  Arg 0 (slot 0): Reader $rXs (Xs? - unbound)');
    print('  Arg 1 (slot 1): Reader $rList1 ([1])');
    print('  Arg 2 (slot 2): Writer $wYs (Ys - unbound)\n');

    const goalId1 = 4000;
    final env1 = CallEnv(readers: {0: rXs, 1: rList1}, writers: {2: wYs});
    rt.setGoalEnv(goalId1, env1);
    rt.gq.enqueue(GoalRef(goalId1, 0));

    // Goal 2: merge(Ys?, [2], Xs)
    print('=== Goal 5000: merge(Ys?, [2], Xs) ===');
    print('  Arg 0 (slot 0): Reader $rYs (Ys? - unbound)');
    print('  Arg 1 (slot 1): Reader $rList2 ([2])');
    print('  Arg 2 (slot 2): Writer $wXs (Xs - unbound)\n');

    const goalId2 = 5000;
    final env2 = CallEnv(readers: {0: rYs, 1: rList2}, writers: {2: wXs});
    rt.setGoalEnv(goalId2, env2);
    rt.gq.enqueue(GoalRef(goalId2, 0));

    print('=== Starting Scheduler ===\n');

    // Run with manual stepping
    var cycle = 0;
    while (rt.gq.length > 0 && cycle < 10) {
      final act = rt.gq.dequeue();
      if (act == null) break;

      cycle++;
      print('--- Cycle $cycle: Running Goal ${act.id} from PC ${act.pc} ---');

      final env = rt.getGoalEnv(act.id);
      final cx = RunnerContext(rt: rt, goalId: act.id, kappa: act.pc, env: env);

      print('Before execution:');
      print('  Xs (W$wXs) bound: ${rt.heap.isWriterBound(wXs)}, value: ${rt.heap.isWriterBound(wXs) ? rt.heap.valueOfWriter(wXs) : "unbound"}');
      print('  Ys (W$wYs) bound: ${rt.heap.isWriterBound(wYs)}, value: ${rt.heap.isWriterBound(wYs) ? rt.heap.valueOfWriter(wYs) : "unbound"}');

      final result = runner.runWithStatus(cx);

      print('After execution:');
      print('  Result: $result');
      print('  Xs (W$wXs) bound: ${rt.heap.isWriterBound(wXs)}, value: ${rt.heap.isWriterBound(wXs) ? rt.heap.valueOfWriter(wXs) : "unbound"}');
      print('  Ys (W$wYs) bound: ${rt.heap.isWriterBound(wYs)}, value: ${rt.heap.isWriterBound(wYs) ? rt.heap.valueOfWriter(wYs) : "unbound"}');
      print('  ROQ R$rXs: ${rt.roq.queue(rXs)?.length ?? 0} suspended');
      print('  ROQ R$rYs: ${rt.roq.queue(rYs)?.length ?? 0} suspended');
      print('');
    }

    print('\n=== Final State ===');
    print('Total cycles: $cycle');
    print('Xs bound: ${rt.heap.isWriterBound(wXs)}');
    if (rt.heap.isWriterBound(wXs)) {
      print('Xs value: ${rt.heap.valueOfWriter(wXs)}');
    }
    print('Ys bound: ${rt.heap.isWriterBound(wYs)}');
    if (rt.heap.isWriterBound(wYs)) {
      print('Ys value: ${rt.heap.valueOfWriter(wYs)}');
    }
  });
}
