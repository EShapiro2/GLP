import 'machine_state.dart';
import 'suspend.dart';
import 'roq.dart';
import 'hanger.dart';

class SuspendOps {
  static Hanger suspendGoal({
    required GoalId goalId,
    required Pc kappa,
    required ROQueues roq,
    required Iterable<int> readers, // ReaderId values
  }) {
    final h = Hanger(goalId: goalId, kappa: kappa, armed: true);
    for (final r in readers) {
      roq.enqueue(r, SuspensionNote(r, h));
    }
    return h;
  }
}
