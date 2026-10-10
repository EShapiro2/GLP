import 'machine_state.dart';
import 'heap_fcp.dart';
import 'suspension.dart';
import 'terms.dart';

/// Suspension operations using FCP-exact shared suspension records
/// 
/// - Suspensions are stored on WRITER cells, not reader cells (IGLP
///   app:in-heap, Suspension)
class SuspendOps {
  /// FCP-exact suspension: create ONE shared record, add to each variable's writer
  /// 
  /// Parameters:
  /// - heap: The heap
  /// - goalId: Goal to suspend
  /// - kappa: Resume PC (restart at clause 1)
  /// - readerVarIds: Set of addresses to suspend on (can be writer or reader addresses)
  static void suspendGoalFCP({
    required HeapFCP heap,
    required int goalId,
    required int kappa,
    required Set<HeapCell> readerVarIds,
  }) {
    // Create ONE shared suspension record
    final sharedRecord = SuspensionRecord(goalId, kappa);

    // Add suspension to each variable
    for (final addr in readerVarIds) {
      _suspendOnVariable(heap, addr, sharedRecord);
    }
  }

  /// Add suspension to a variable (follows chain to find final unbound writer)
  static void _suspendOnVariable(HeapFCP heap, HeapCell addr, SuspensionRecord record) {
    // Dereference to find the final target
    final result = heap.derefAddr(addr);

    if (result is VarRef) {
      // Unbound local variable - result.addr is the writer address
      final writerAddr = result.addr;
      heap.suspendOnWriter(writerAddr, record);
      return;
    }

    // Already bound to ground - no suspension needed
    // (This shouldn't normally happen if we're suspending on unbound vars)
  }
}
