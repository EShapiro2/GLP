import 'machine_state.dart';
import 'heap_fcp.dart';
import 'suspension.dart';
import 'terms.dart';

class CommitOps {
  /// Apply tentative writer substitution σ̂w (FCP-exact two-cell semantics)
  /// 
  /// Per heap-pointer-architecture-spec.md v3.0:
  /// - VarRef has only addr field
  /// - Use heap.isWriter/isReader to check cell type
  /// - Suspensions are on writer cells
  static List<GoalRef> applySigmaHatFCP({
    required HeapFCP heap,
    required Map<int, Object?> sigmaHat,
  }) {
    final activations = <GoalRef>[];

    // Defensive WxW validation before applying bindings
    for (final entry in sigmaHat.entries) {
      final value = entry.value;
      if (value is VarRef && heap.isWriter(value.addr)) {
        final clauseWriterId = entry.key;
        final queryWriterAddr = value.addr;
        // Check if both are unbound (WxW violation)
        if (!heap.isFullyBound(clauseWriterId) && !heap.isFullyBound(queryWriterAddr)) {
          throw StateError('WxW violation in applySigmaHatFCP: W$clauseWriterId → W$queryWriterAddr (both unbound)');
        }
      }
    }

    for (final entry in sigmaHat.entries) {
      final varId = entry.key;  // This is writerAddr
      var value = entry.value;

      // Skip null values (writer declared but not bound in HEAD)
      if (value == null) continue;

      // Dereference VarRef before binding
      if (value is VarRef) {
        final derefResult = heap.derefAddr(value.addr);
        if (derefResult is Term) {
          value = derefResult;
        }
        // If still VarRef or VariableEntry, keep as-is
      }

      // Use heap.bindVariable() to properly trigger callbacks and handle suspensions
      final valueAsTerm = value is Term ? value : ConstTerm(value);
      final acts = heap.bindVariable(varId, valueAsTerm);
      activations.addAll(acts);
    }

    // Re-dereference all bound cells that contain VarRef
    // This handles dependencies in σ̂w (e.g., W1002→V1005, W1005→value)
    for (final varId in sigmaHat.keys) {
      final wAddr = varId;
      final cell = heap.cells[wAddr];
      
      // Only process if still WrtTag with Pointer content (bound to another var)
      if (cell.tag == CellTag.WrtTag && cell.content is Pointer) {
        final targetAddr = (cell.content as Pointer).targetAddr;
        final derefResult = heap.derefAddr(targetAddr);

        if (derefResult is Term && derefResult is! VarRef) {
          // Target is now bound to ground - update this cell
          cell.content = derefResult;
          cell.tag = CellTag.ValueTag;
        }
      }
    }

    return activations;
  }

  /// Walk suspension list and activate armed records
  static void _walkAndActivate(SuspensionListNode? list, List<GoalRef> acts) {
    var current = list;

    while (current != null) {
      if (current.armed) {
        acts.add(GoalRef(current.goalId!, current.resumePC));
        current.record.disarm();
      }
      current = current.next;
    }
  }

  /// Forward suspension list to target writer
  /// 
  /// Per heap-pointer-architecture-spec.md v3.0:
  /// Suspensions are stored on writer cells
  static void _forwardSuspensions(HeapFCP heap, SuspensionListNode? list, int targetWriterAddr) {
    var current = list;

    while (current != null) {
      if (current.armed) {
        final newNode = SuspensionListNode(current.record);
        final targetCell = heap.cells[targetWriterAddr];
        if (targetCell.content is SuspensionListNode) {
          newNode.next = targetCell.content as SuspensionListNode;
        }
        targetCell.content = newNode;
      }
      current = current.next;
    }
  }
}
