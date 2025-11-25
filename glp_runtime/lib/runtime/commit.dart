import 'machine_state.dart';
import 'heap_fcp.dart';
import 'suspension.dart';
import 'terms.dart';

class CommitOps {
  /// Apply tentative writer substitution σ̂w (FCP-exact two-cell semantics)
  /// Implements FCP emulate.h do_commit1 lines 217-258
  /// Critical: Address-based dereferencing (FCP line 233) prevents variable chains
  static List<GoalRef> applySigmaHatFCP({
    required HeapFCP heap,
    required Map<int, Object?> sigmaHat,
  }) {
    // print('[TRACE Commit FCP] Applying σ̂w to heap (${sigmaHat.length} bindings):');
    for (final entry in sigmaHat.entries) {
      // print('  W${entry.key} → ${entry.value}');
    }

    final activations = <GoalRef>[];

    // Defensive WxW validation before applying bindings
    for (final entry in sigmaHat.entries) {
      final value = entry.value;
      if (value is VarRef && !value.isReader) {
        final clauseWriterId = entry.key;
        final queryWriterId = value.varId;
        // Check if both are unbound (WxW violation)
        if (!heap.isWriterBound(clauseWriterId) && !heap.isWriterBound(queryWriterId)) {
          throw StateError('WxW violation in applySigmaHatFCP: W$clauseWriterId → W$queryWriterId (both unbound)');
        }
      }
    }

    for (final entry in sigmaHat.entries) {
      final varId = entry.key;
      var value = entry.value;

      // Skip null values (writer declared but not bound in HEAD)
      if (value == null) continue;

      // Convert varId to addresses
      final (wAddr, rAddr) = heap.varTable[varId]!;

      // FCP line 226: CRITICAL - dereference by ADDRESS before binding
      // This prevents W1009→R1014→nil chains
      // IMPORTANT: Use reader address if VarRef is reader, writer address if writer
      if (value is VarRef) {
        final (targetWAddr, targetRAddr) = heap.varTable[value.varId]!;
        final targetAddr = value.isReader ? targetRAddr : targetWAddr;
        value = heap.derefAddr(targetAddr);
        // print('[TRACE Commit FCP] Dereferenced VarRef(${(entry.value as VarRef).varId}) → $value');
      }

      // FCP line 301: Save suspension list before replacing reader content
      final oldContent = heap.cells[rAddr].content;

      // CRITICAL: If oldContent is a suspension list, activate NOW before overwriting
      // This handles X? → Y? bindings where X has suspended goals
      if (oldContent is SuspensionListNode) {
        _walkAndActivate(oldContent, activations);
      }

      // FCP lines 233/303: Bind BOTH cells to dereferenced value
      heap.cells[wAddr].content = value;
      heap.cells[wAddr].tag = CellTag.ValueTag;
      heap.cells[rAddr].content = value;
      heap.cells[rAddr].tag = CellTag.ValueTag;
      // print('[DEBUG COMMIT] Bound varId $varId: W$varId[addr=$wAddr] = $value, R$varId[addr=$rAddr] = $value');
    }

    // CRITICAL FIX: Re-dereference all bound cells that contain VarRef
    // This handles dependencies in σ̂w (e.g., W1002→V1005, W1005→nil)
    // After first pass: W1002 contains VarRef(1005), W1005 contains nil
    // After second pass: W1002 contains nil (dereferenced through chain)
    // print('[TRACE Commit FCP] Re-dereferencing cells with VarRef values...');
    for (final varId in sigmaHat.keys) {
      final (wAddr, rAddr) = heap.varTable[varId]!;
      final wContent = heap.cells[wAddr].content;

      if (wContent is VarRef) {
        // Dereference again (target may now be bound)
        final (targetWAddr, _) = heap.varTable[wContent.varId]!;
        final derefValue = heap.derefAddr(targetWAddr);

        if (derefValue is! VarRef) {
          // Target is now bound - update both cells
          heap.cells[wAddr].content = derefValue;
          heap.cells[rAddr].content = derefValue;
          // print('[TRACE Commit FCP]   W$varId: VarRef(${wContent.varId}) → $derefValue');
        }
      }
    }

    // print('[TRACE Commit FCP] Total goals reactivated: ${activations.length}');
    return activations;
  }

  /// Walk suspension list and activate armed records (FCP lines 247-253)
  static void _walkAndActivate(SuspensionListNode? list, List<GoalRef> acts) {
    var current = list;

    while (current != null) {
      if (current.armed) {
        acts.add(GoalRef(current.goalId!, current.resumePC));
        current.record.disarm();  // Disarm shared record - affects all nodes
      }
      current = current.next;
    }
  }

}
