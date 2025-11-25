import 'machine_state.dart';
import 'heap_fcp.dart';
import 'suspension.dart';
import 'terms.dart';

/// Suspension operations using FCP-exact shared suspension records
/// Records stored in wrapper nodes in reader cells (no separate ROQ)
class SuspendOps {
  /// FCP-exact suspension: create ONE shared record, wrap in nodes for each reader
  /// Implements FCP emulate.h suspend_on lines 169-188
  static void suspendGoalFCP({
    required HeapFCP heap,
    required int goalId,
    required int kappa,
    required String moduleName,
    required Set<int> readerVarIds,  // Variable IDs (not reader IDs)
  }) {
    // print('[TRACE SuspendOps FCP] Suspending goal $goalId on ${readerVarIds.length} reader(s):');
    // print('  Readers: ${readerVarIds.toList()}');
    // print('  Resume PC: $kappa');

    // Create ONE shared suspension record
    final sharedRecord = SuspensionRecord(goalId, kappa, moduleName);

    // Create wrapper node for each reader cell (independent next pointers)
    for (final varId in readerVarIds) {
      var finalVarId = varId;
      var (_, rAddr) = heap.varTable[finalVarId]!;
      var cell = heap.cells[rAddr];

      // Follow variable chain if reader is bound to another variable
      while (cell.content is VarRef) {
        final nextVar = cell.content as VarRef;
        finalVarId = nextVar.varId;
        (_, rAddr) = heap.varTable[finalVarId]!;
        cell = heap.cells[rAddr];
      }

      // Create wrapper node pointing to shared record
      final node = SuspensionListNode(sharedRecord);

      // Prepend to existing list (or null if none)
      node.next = cell.content is SuspensionListNode
          ? cell.content as SuspensionListNode
          : null;

      // REPLACE reader cell content with suspension list
      cell.content = node;
    }
  }

  /// Legacy version using ROQ (for backward compatibility during migration)
  /// TODO: Remove after runner.dart updated to use FCP suspension
  static void suspendGoal({
    required int goalId,
    required int kappa,
    required Set<int> readerVarIds,
  }) {
    // Placeholder - should not be called after migration
    throw UnimplementedError('Legacy suspendGoal deprecated - use suspendGoalFCP');
  }
}
