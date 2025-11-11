import 'package:test/test.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';

/// Program shape:
///   C1: TRY; GuardNeedReader(r); U->END
///   END: SUSPEND_END
/// On wake, the scheduler runs from kappa (clause 1).
BytecodeProgram mkSuspendProg(int r) => BytecodeProgram([
  Label('C1'),
  ClauseTry(),
  GuardNeedReader(r),
  UnionSiAndGoto('END'),
  Label('END'),
  SuspendEnd(),
]);

void main() {
  test('Scheduler drains FIFO activations after a single reader bind', () {
    final rt = GlpRuntime();
    final int r = 4200;

    final prog = mkSuspendProg(r);
    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Two goals will suspend on the same reader r in order g1 then g2.
    final g1 = GoalRef(101, 1);
    final g2 = GoalRef(202, 1);

    // Run g1 to suspension.
    rt.gq.enqueue(g1);
    var ran = sched.drain(maxCycles: 10);
    expect(ran, [101], reason: 'g1 runs and suspends');

    // Run g2 to suspension.
    rt.gq.enqueue(g2);
    ran = sched.drain(maxCycles: 10);
    expect(ran, [202], reason: 'g2 runs and suspends');

    // Now bind r: this enqueues both activations in FIFO (g1 then g2).
    final acts = rt.roq.processOnBind(r);
    for (final a in acts) {
      rt.gq.enqueue(a);
    }

    // Drain: we should see g1 then g2 run (each will suspend again).
    final ranAfter = sched.drain(maxCycles: 10);
    expect(ranAfter, [101, 202], reason: 'FIFO activations executed in order');
  });
}
