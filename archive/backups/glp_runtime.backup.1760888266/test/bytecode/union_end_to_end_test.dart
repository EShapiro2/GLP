import 'package:test/test.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';  // GoalRef
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/hanger.dart';
import 'package:glp_runtime/runtime/suspend.dart';

void main() {
  test('Union across two SUSPEND_READY clauses then single suspend at end', () {
    final rt = GlpRuntime();
    final r1 = 41, r2 = 42;

    final p = BytecodeProgram([
      Label('C1'),
      ClauseTry(),
      GuardNeedReader(r1),
      UnionSiAndGoto('C2'),

      Label('C2'),
      ClauseTry(),
      GuardNeedReader(r2),
      UnionSiAndGoto('END'),

      Label('END'),
      SuspendEnd(),
    ]);

    final cx = RunnerContext(rt: rt, goalId: 1000, kappa: 7);
    BytecodeRunner(p).run(cx);

    final acts1 = rt.roq.processOnBind(r1);
    expect(acts1.map((a) => a.id).toList(), [1000]);
    expect(acts1.map((a) => a.pc).toSet(), {7});

    final acts2 = rt.roq.processOnBind(r2);
    expect(acts2, isEmpty, reason: 'hanger single-reactivation prevents duplicate');
  });

  test('Commit applies σ̂w: wake suspended readers for those writers', () {
    final rt = GlpRuntime();
    const writerId = 1;
    const readerId = 1001;

    rt.heap.addWriter(WriterCell(writerId, readerId));
    rt.heap.addReader(ReaderCell(readerId));

    final p = BytecodeProgram([
      Label('C1'),
      ClauseTry(),
      HeadBindWriter(writerId),
      Commit(),
      Proceed(),
    ]);

    final cx = RunnerContext(rt: rt, goalId: 2000, kappa: 9);

    final h1 = Hanger(goalId: 3000, kappa: 9, armed: true);
    final h2 = Hanger(goalId: 4000, kappa: 9, armed: true);
    rt.roq.enqueue(readerId, SuspensionNote(readerId, h1));
    rt.roq.enqueue(readerId, SuspensionNote(readerId, h2));

    BytecodeRunner(p).run(cx);

    final List<GoalRef> acts = [];
    while (rt.gq.length > 0) {
      final a = rt.gq.dequeue();
      if (a != null) {
        acts.add(a);
      }
    }
    expect(acts.map((x) => x.id).toList(), [3000, 4000]);
    expect(acts.map((x) => x.pc).toSet(), {9});
  });
}
