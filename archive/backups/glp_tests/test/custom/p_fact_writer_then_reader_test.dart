import 'package:test/test.dart';
import 'package:glp_runtime/bytecode/asm.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';

void main() {
  const int W = 200;
  const int R = 1200;

  BytecodeProgram progWriter() => BC.prog([
        BC.L('C1'),
        BC.TRY(),
        BC.W(W),
        BC.COMMIT(),
        BC.BCONST(W, 'a'),
        BC.PROCEED(),
      ]);

  BytecodeProgram progReader() => BC.prog([
        BC.L('C1'),
        BC.TRY(),
        BC.R(R),      // our runner only blocks if R is unbound
        BC.U('END'),
        BC.L('END'),
        BC.SUSP(),   // suspends only if U ≠ ∅
      ]);

  test('p(X), then p(X?) does not suspend and produces no activations', () {
    final rt = GlpRuntime();
    rt.heap.addWriter(WriterCell(W, R));
    rt.heap.addReader(ReaderCell(R));

    final cxW = RunnerContext(rt: rt, goalId: 1, kappa: 1);
    BytecodeRunner(progWriter()).run(cxW);

    expect(rt.heap.isWriterBound(W), isTrue);
    expect((rt.heap.valueOfWriter(W) as ConstTerm).value, 'a');

    final cxR = RunnerContext(rt: rt, goalId: 2, kappa: 1);
    BytecodeRunner(progReader()).run(cxR);

    expect(rt.roq.isEmpty(R), isTrue, reason: 'reader already bound → no suspension');
    expect(rt.gq.length, 0, reason: 'no activations enqueued');
  });
}
