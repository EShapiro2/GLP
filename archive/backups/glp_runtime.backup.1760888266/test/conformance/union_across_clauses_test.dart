import 'package:test/test.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/roq.dart';
import 'package:glp_runtime/runtime/suspend_ops.dart';

void main() {
  test('Union across clauses uses one hanger; ANY-of wake activates once', () {
    final roq = ROQueues();
    final GoalId g = 999;
    final Pc kappa = 1;

    final r1 = 41;
    final r2 = 42;

    final h = SuspendOps.suspendGoal(
      goalId: g,
      kappa: kappa,
      roq: roq,
      readers: [r1, r2],
    );

    final acts1 = roq.processOnBind(r1);
    expect(acts1.map((a) => a.id).toList(), [g]);
    expect(acts1.map((a) => a.pc).toSet(), {kappa});
    expect(h.armed, isFalse);

    final acts2 = roq.processOnBind(r2);
    expect(acts2, isEmpty);
  });
}
