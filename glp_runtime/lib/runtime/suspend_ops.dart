import 'machine_state.dart';
import 'heap_fcp.dart';
import 'suspension.dart';

/// Suspension operations using FCP-exact shared suspension records
/// Records stored directly in reader cells (no separate ROQ)
class SuspendOps {
  /// FCP-exact suspension: create ONE shared record, prepend to all reader cells
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
    final record = SuspensionRecord(goalId, kappa);

    // Prepend to each reader cell's suspension list
    for (final varId in readerVarIds) {
      final (_, rAddr) = heap.varTable[varId]!;
      final cell = heap.cells[rAddr];

      // Prepend to existing list (or null if none)
      record.next = cell.content is SuspensionRecord ? cell.content as SuspensionRecord : null;

      // REPLACE reader cell content with suspension list
      cell.content = record;

      // print('  → Added to R$varId suspension list (addr=$rAddr)');
    }

    // print('  ✓ Goal $goalId suspended (shared record)');
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
