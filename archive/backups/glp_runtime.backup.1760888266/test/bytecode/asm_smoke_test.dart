import 'package:test/test.dart';
import 'package:glp_runtime/bytecode/asm.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/hanger.dart';
import 'package:glp_runtime/runtime/suspend.dart';
import 'package:glp_runtime/runtime/cells.dart';

void main() {
  test('Assembler: union across clauses then suspend; wake FIFO once', () {
    final rt = GlpRuntime();
    final r1 = 501, r2 = 502;

    final program = BC.prog([
      BC.L('C1'),
      BC.TRY(),
      BC.R(r1),
      BC.U('C2'),

      BC.L('C2'),
      BC.TRY(),
      BC.R(r2),
      BC.U('END'),

      BC.L('END'),
      BC.SUSP(),
    ]);

    final cx = RunnerContext(rt: rt, goalId: 6000, kappa: 1);
    BytecodeRunner(program).run(cx);

    final acts1 = rt.roq.processOnBind(r1);
    expect(acts1.map((a) => a.id).toList(), [6000]);
    expect(acts1.map((a) => a.pc).toSet(), {1});

    final acts2 = rt.roq.processOnBind(r2);
    expect(acts2, isEmpty);
  });

  test('Assembler: commit wakes readers bound to writers in σ̂w', () {
    final rt = GlpRuntime();
    const w = 77;
    const r = 7077;
    rt.heap.addWriter(WriterCell(w, r));
    rt.heap.addReader(ReaderCell(r));

    final program = BC.prog([
      BC.L('C1'),
      BC.TRY(),
      BC.W(w),
      BC.COMMIT(),
      BC.PROCEED(),
    ]);

    final cx = RunnerContext(rt: rt, goalId: 7000, kappa: 9);

    final h1 = Hanger(goalId: 9001, kappa: 9, armed: true);
    final h2 = Hanger(goalId: 9002, kappa: 9, armed: true);
    rt.roq.enqueue(r, SuspensionNote(r, h1));
    rt.roq.enqueue(r, SuspensionNote(r, h2));

    BytecodeRunner(program).run(cx);

    final acts = <GoalRef>[];
    while (rt.gq.length > 0) {
      final a = rt.gq.dequeue();
      if (a != null) acts.add(a);
    }
    expect(acts.map((x) => x.id).toList(), [9001, 9002]);
    expect(acts.map((x) => x.pc).toSet(), {9});
  });
}
