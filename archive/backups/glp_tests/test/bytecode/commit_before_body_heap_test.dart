import 'package:test/test.dart';
import 'package:glp_runtime/bytecode/asm.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/cells.dart';

void main() {
  test('Pre-commit: body ops ignored; no heap mutation', () {
    final rt = GlpRuntime();
    const w = 5;
    const r = 5005;
    rt.heap.addWriter(WriterCell(w, r));
    rt.heap.addReader(ReaderCell(r));

    final program = BC.prog([
      BC.L('C1'),
      BC.TRY(),
      BC.W(w),
      BC.BCONST(w, 42),   // pre-commit → must NOT mutate heap
      BC.PROCEED(),
    ]);

    final cx = RunnerContext(rt: rt, goalId: 1, kappa: 0);
    BytecodeRunner(program).run(cx);

    expect(rt.heap.isWriterBound(w), isFalse, reason: 'no mutation pre-commit');
  });

  test('Post-commit: body ops mutate heap (const and struct)', () {
    final rt = GlpRuntime();
    const w1 = 6, w2 = 7;
    const r1 = 6006, r2 = 7007;
    rt.heap.addWriter(WriterCell(w1, r1));
    rt.heap.addReader(ReaderCell(r1));
    rt.heap.addWriter(WriterCell(w2, r2));
    rt.heap.addReader(ReaderCell(r2));

    final program = BC.prog([
      BC.L('C1'),
      BC.TRY(),
      BC.W(w1),
      BC.W(w2),
      BC.COMMIT(),              // enter body
      BC.BCONST(w1, 'ok'),
      BC.BSTRUCTC(w2, 'pair', [1, 2]),
      BC.PROCEED(),
    ]);

    final cx = RunnerContext(rt: rt, goalId: 2, kappa: 0);
    BytecodeRunner(program).run(cx);

    expect(rt.heap.isWriterBound(w1), isTrue);
    expect(rt.heap.valueOfWriter(w1), isA<ConstTerm>());
    expect((rt.heap.valueOfWriter(w1) as ConstTerm).value, 'ok');

    expect(rt.heap.isWriterBound(w2), isTrue);
    expect(rt.heap.valueOfWriter(w2), isA<StructTerm>());
    final s = rt.heap.valueOfWriter(w2) as StructTerm;
    expect(s.functor, 'pair');
    expect(s.args.length, 2);
    expect(s.args[0], isA<ConstTerm>());
    expect((s.args[0] as ConstTerm).value, 1);
    expect((s.args[1] as ConstTerm).value, 2);
  });
}
