import 'machine_state.dart';
import 'heap_fcp.dart';
import 'suspension.dart';
import 'terms.dart';
import 'package:glp_runtime/multiagent/variable_table.dart' show VariableEntry;

/// Suspension operations using FCP-exact shared suspension records
/// Records stored in wrapper nodes in reader cells (no separate ROQ)
class SuspendOps {
  /// FCP-exact suspension: create ONE shared record, wrap in nodes for each reader
  /// Implements FCP emulate.h suspend_on lines 169-188
  static void suspendGoalFCP({
    required HeapFCP heap,
    required int goalId,
    required int kappa,
    required Set<int> readerVarIds,  // Variable IDs (not reader IDs)
  }) {
    // print('[TRACE SuspendOps FCP] Suspending goal $goalId on ${readerVarIds.length} reader(s):');
    // print('  Readers: ${readerVarIds.toList()}');
    // print('  Resume PC: $kappa');

    // Create ONE shared suspension record
    final sharedRecord = SuspensionRecord(goalId, kappa);

    // Create wrapper node for each reader cell (independent next pointers)
    for (final varId in readerVarIds) {
      var finalVarId = varId;
      
      // Determine the reader cell address
      // For imported readers (single cell), varId IS the reader address
      // For normal variables (two-cell), reader is at varId + 1
      int rAddr;
      
      // Check if this is an imported reader (single cell with VariableEntry)
      if (varId < heap.cells.length && 
          heap.cells[varId].tag == CellTag.RoTag &&
          heap.cells[varId].content is VariableEntry) {
        // Imported reader - varId is the reader cell directly
        rAddr = varId;
      } else {
        // Normal two-cell variable - reader is at varId + 1
        rAddr = varId + 1;
      }
      
      var cell = heap.cells[rAddr];

      // Follow variable chain if reader is bound to another variable
      while (cell.content is VarRef) {
        final nextVar = cell.content as VarRef;
        finalVarId = nextVar.varId;
        // Phase 2: Use address arithmetic
        rAddr = finalVarId + 1;
        cell = heap.cells[rAddr];
      }

      // Create wrapper node pointing to shared record
      final node = SuspensionListNode(sharedRecord);

      // For imported readers, V_p entry serves as "virtual writer" for suspensions
      // Per irmaGLP spec Section 3.1.2: V_p contains 4-tuples (Y, q, s, Σ)
      if (cell.content is VariableEntry) {
        final entry = cell.content as VariableEntry;
        // Prepend to entry's suspension list
        node.next = entry.suspensions;
        entry.suspensions = node;
        // print('[TRACE SuspendOps] Added suspension to VariableEntry for imported reader $varId');
      } else {
        // Normal variable - prepend to existing list in cell
        node.next = cell.content is SuspensionListNode
            ? cell.content as SuspensionListNode
            : null;
        cell.content = node;
      }
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
