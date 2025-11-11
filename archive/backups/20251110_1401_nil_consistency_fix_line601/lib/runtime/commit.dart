import 'machine_state.dart';
import 'heap.dart';
import 'roq.dart';
import 'terms.dart';

class CommitOps {
  /// Apply tentative writer substitution σ̂w (v2.16 semantics)
  /// 1. Bind writers to their tentative values in σ̂w
  /// 2. Process ROQ for each writer (wake suspended goals)
  static List<GoalRef> applySigmaHatV216({
    required Heap heap,
    required ROQueues roq,
    required Map<int, Object?> sigmaHat,
  }) {
    final acts = <GoalRef>[];
    for (final entry in sigmaHat.entries) {
      final writerId = entry.key;
      final value = entry.value;

      // Apply binding to heap
      if (value is StructTerm) {
        // Structure binding
        heap.bindWriterStruct(writerId, value.functor, value.args);
      } else if (value is ConstTerm) {
        // ConstTerm - extract the actual value
        heap.bindWriterConst(writerId, value.value);
      } else if (value is String || value is int || value is bool || value == null) {
        // Primitive constant value
        heap.bindWriterConst(writerId, value);
      } else {
        // Unknown type - try to bind as constant
        heap.bindWriterConst(writerId, value);
      }

      // Process ROQ to wake goals suspended on this writer's reader
      final wc = heap.writer(writerId);
      if (wc != null) {
        final r = wc.readerId;
        final a = roq.processOnBind(r);
        if (a.isNotEmpty) acts.addAll(a);
      }
    }
    return acts;
  }

  /// Legacy version: only processes ROQs for writer IDs (no binding)
  /// Used by existing code that binds writers separately
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
