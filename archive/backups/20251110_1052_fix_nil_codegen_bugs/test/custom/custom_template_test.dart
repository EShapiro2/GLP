import 'package:test/test.dart';
import 'package:glp_runtime/bytecode/asm.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/hanger.dart';
import 'package:glp_runtime/runtime/suspend.dart';

void main() {
  test('custom: commit wakes exactly one suspended goal', () {
    final rt = GlpRuntime();
    const w = 123;
    const r = 8123;
    rt.heap.addWriter(WriterCell(w, r));
    rt.heap.addReader(ReaderCell(r));

    final program = BC.prog([
      BC.L('C1'),
      BC.TRY(),
      BC.W(w),
      BC.COMMIT(),
      BC.PROCEED(),
    ]);
    final cx = RunnerContext(rt: rt, goalId: 999, kappa: 1);

    final h = Hanger(goalId: 42, kappa: 1, armed: true);
    rt.roq.enqueue(r, SuspensionNote(r, h));

    BytecodeRunner(program).run(cx);

    final activations = <GoalRef>[];
    while (rt.gq.length > 0) {
      final a = rt.gq.dequeue();
      if (a != null) activations.add(a);
    }
    expect(activations.map((a) => a.id).toList(), [42]);
    expect(activations.map((a) => a.pc).toSet(), {1});
  });
}
