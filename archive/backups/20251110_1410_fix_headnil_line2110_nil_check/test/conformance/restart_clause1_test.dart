import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/suspend_ops.dart';
import 'package:glp_runtime/runtime/roq.dart';

void main() {
  test('On wake, activation pc equals kappa (restart at clause 1)', () {
    final rt = GlpRuntime();
    final ROQueues roq = rt.roq;

    const GoalId g = 77;
    const Pc kappa = 1;
    const r = 9001;

    // Suspend goal g on reader r with restart point kappa.
    SuspendOps.suspendGoal(goalId: g, kappa: kappa, roq: roq, readers: [r]);

    // Binding r should produce a single activation ⟨g, kappa⟩.
    final acts = roq.processOnBind(r);
    expect(acts, hasLength(1));
    expect(acts.first.id, g);
    expect(acts.first.pc, kappa);
  });
}
