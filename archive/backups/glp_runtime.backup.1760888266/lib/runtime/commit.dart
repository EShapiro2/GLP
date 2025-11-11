import 'machine_state.dart';
import 'heap.dart';
import 'roq.dart';

class CommitOps {
  static List<GoalRef> applySigmaHat({
    required Heap heap,
    required ROQueues roq,
    required Iterable<int> writerIds,
  }) {
    final acts = <GoalRef>[];
    for (final w in writerIds) {
      final wc = heap.writer(w);
      if (wc == null) continue;
      final r = wc.readerId;
      final a = roq.processOnBind(r);
      if (a.isNotEmpty) acts.addAll(a);
    }
    return acts;
  }
}
