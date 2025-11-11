import 'package:collection/collection.dart';
import 'machine_state.dart';
import 'suspend.dart';

class ROQueues {
  final Map<ReaderId, QueueList<SuspensionNote>> _q =
      <ReaderId, QueueList<SuspensionNote>>{};

  void enqueue(ReaderId r, SuspensionNote note) {
    _q.putIfAbsent(r, () => QueueList<SuspensionNote>()).add(note);
  }

  QueueList<SuspensionNote> queue(ReaderId r) =>
      _q.putIfAbsent(r, () => QueueList<SuspensionNote>());

  bool isEmpty(ReaderId r) => queue(r).isEmpty;

  SuspensionNote? dequeue(ReaderId r) {
    final q = queue(r);
    return q.isEmpty ? null : q.removeFirst();
  }

  /// Process the suspension queue for reader r in FIFO order.
  /// Returns the list of GoalRef activations to enqueue to the active goal queue.
  ///
  /// Semantics:
  /// - FIFO: process notes in arrival order.
  /// - Single reactivation: if note.hanger.armed is true, flip to false and produce ⟨goalId, kappa⟩;
  ///   if already false, produce nothing.
  /// - Queue is emptied by this call.
  List<GoalRef> processOnBind(ReaderId r) {
    final q = queue(r);
    final activations = <GoalRef>[];
    while (q.isNotEmpty) {
      final note = q.removeFirst(); // FIFO
      final h = note.hanger;
      if (h.armed) {
        h.armed = false;
        activations.add(GoalRef(h.goalId, h.kappa));
      }
    }
    return activations;
  }
}
