import 'package:test/test.dart';
import 'package:glp_runtime/bytecode/asm.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';

void main() {
  // Common variable pair for all three scenarios:
  const int W = 100;   // writer id
  const int R = 1100;  // paired reader id

  // Program p(X) for writer call: bind X = 'a'
  BytecodeProgram progWriter() => BC.prog([
    BC.L('C1'),
    BC.TRY(),
    BC.W(W),
    BC.COMMIT(),
    BC.BCONST(W, 'a'),
    BC.PROCEED(),
  ]);

  // Program p(X?) for reader call: suspend if R is unbound, else succeed
  BytecodeProgram progReader() => BC.prog([
    BC.L('C1'),
    BC.TRY(),
    BC.R(R),           // only blocks if unbound (runner checks boundness)
    BC.U('END'),
    BC.L('END'),
    BC.SUSP(),         // suspends if union U ≠ ∅
  ]);

  setUp(() {
    // nothing global
  });

  test('p(X) — succeeds and binds X = a', () {
    final rt = GlpRuntime();
    // set up heap with WR/RO pair
    rt.heap.addWriter(WriterCell(W, R));
    rt.heap.addReader(ReaderCell(R));

    final p = progWriter();
    final cx = RunnerContext(rt: rt, goalId: 1, kappa: 1);
    BytecodeRunner(p).run(cx);

    expect(rt.heap.isWriterBound(W), isTrue);
    final v = rt.heap.valueOfWriter(W);
    expect(v, isA<ConstTerm>());
    expect((v as ConstTerm).value, 'a');
  });

  test('p(X?) — suspends on unbound reader', () {
    final rt = GlpRuntime();
    rt.heap.addWriter(WriterCell(W, R));
    rt.heap.addReader(ReaderCell(R));

    final p = progReader();
    final cx = RunnerContext(rt: rt, goalId: 2, kappa: 1);
    BytecodeRunner(p).run(cx);

    // Suspended: ROQ must have an entry for R (one hanger note).
    // We can't access internal queue here easily, but we can simulate a bind
    // and see that an activation appears.
    final acts = rt.roq.processOnBind(R);
    expect(acts.map((a) => a.id).toList(), [2]);
    expect(acts.map((a) => a.pc).toSet(), {1});
  });

  test('p(X?), p(X) — suspend, bind, activate, terminate', () {
    final rt = GlpRuntime();
    rt.heap.addWriter(WriterCell(W, R));
    rt.heap.addReader(ReaderCell(R));

    // First: run p(X?) → suspends
    {
      final pR = progReader();
      final cxR = RunnerContext(rt: rt, goalId: 10, kappa: 1);
      BytecodeRunner(pR).run(cxR);
    }

    // Then: run p(X) → binds W='a' and enqueues activation for goalId=10
    {
      final pW = progWriter();
      final cxW = RunnerContext(rt: rt, goalId: 20, kappa: 1);
      BytecodeRunner(pW).run(cxW);
    }

    // Drain activations: should contain ⟨10,1⟩
    final activations = <GoalRef>[];
    while (rt.gq.length > 0) {
      final a = rt.gq.dequeue();
      if (a != null) activations.add(a);
    }
    expect(activations.map((a) => a.id).toList(), [10]);
    expect(activations.map((a) => a.pc).toSet(), {1});

    // Run the awakened goal (p(X?) again). Now R is bound, so it should *not* suspend.
    {
      final pR = progReader();
      final cxR2 = RunnerContext(rt: rt, goalId: activations.first.id, kappa: activations.first.pc);
      BytecodeRunner(pR).run(cxR2);
    }

    // After second run, no more activations created (goal terminates).
    expect(rt.gq.length, 0);
  });
}
