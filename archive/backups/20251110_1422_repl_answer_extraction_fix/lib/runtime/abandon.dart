import 'heap.dart';
import 'roq.dart';
import 'machine_state.dart';

class AbandonOps {
  /// Mark the writer abandoned and immediately process its paired reader queue.
  /// Returns activations (GoalRef) in FIFO order. Hanger enforces single reactivation.
  static List<GoalRef> abandonWriter({
    required Heap heap,
    required ROQueues roq,
    required int writerId,
  }) {
    final w = heap.writer(writerId);
    if (w == null) return const <GoalRef>[];
    w.abandoned = true;
    final readerId = w.readerId;
    final acts = roq.processOnBind(readerId);
    return acts;
  }
}
