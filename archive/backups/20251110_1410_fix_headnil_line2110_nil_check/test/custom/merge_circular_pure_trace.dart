import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/asm.dart';

void main() {
  test('Pure VM trace: merge(Xs?,[1],Ys), merge(Ys?,[a],Xs)', () {
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

    // Goal 100: merge(Xs?, [1], Ys)
    const goal1 = 100;
    final env1 = CallEnv(readers: {0: rXs, 1: rList1}, writers: {2: wYs});
    rt.setGoalEnv(goal1, env1);

    final cx1 = RunnerContext(
      rt: rt,
      goalId: goal1,
      kappa: 0,
      env: env1,
      reductionBudget: 100,
    );
    runner.runWithStatus(cx1);

    // Goal 200: merge(Ys?, [a], Xs)
    const goal2 = 200;
    final env2 = CallEnv(readers: {0: rYs, 1: rListA}, writers: {2: wXs});
    rt.setGoalEnv(goal2, env2);

    final cx2 = RunnerContext(
      rt: rt,
      goalId: goal2,
      kappa: 0,
      env: env2,
      reductionBudget: 100,
    );
    runner.runWithStatus(cx2);

    // Run cycles
    var cycle = 0;
    while (rt.gq.length > 0 && cycle < 10) {
      final act = rt.gq.dequeue();
      if (act == null) break;
      cycle++;

      final env = rt.getGoalEnv(act.id);
      final cx = RunnerContext(
        rt: rt,
        goalId: act.id,
        kappa: act.pc,
        env: env,
        reductionBudget: 100,
      );
      runner.runWithStatus(cx);
    }

    expect(cycle, 10);
  });
}
