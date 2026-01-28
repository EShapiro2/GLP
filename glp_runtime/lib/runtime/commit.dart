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

    // Collect writers that need callbacks fired after all bindings complete
    final writersWithCallbacks = <int>[];

    for (final entry in sigmaHat.entries) {
      final varId = entry.key;  // This is writerAddr
      var value = entry.value;

      // Skip null values (writer declared but not bound in HEAD)
      if (value == null) continue;

      // Handle VarRef per spec Section 5.3:
      // - If reader address: use bindWriterToReader directly (no deref)
      // - If writer address: deref to get bound value, or error if unbound
      if (value is VarRef) {
        if (heap.isReader(value.addr)) {
          // Bind writer to another variable via reader - per spec Section 5.3
          final acts = heap.bindWriterToReader(varId, value.addr);
          activations.addAll(acts);
          continue;
        } else if (heap.isWriter(value.addr)) {
          // σ̂w contains writer address - check if it's bound to ground
          final derefResult = heap.derefAddr(value.addr);
          if (derefResult is Term && derefResult is! VarRef) {
            value = derefResult;
          } else {
            // Unbound writer in σ̂w - this is a HEAD instruction bug
            throw StateError('σ̂w contains unbound writer address ${value.addr} - HEAD instruction bug');
          }
        }
      }

      // Bind writer to ground term WITHOUT firing callbacks
      // Callbacks are deferred until all bindings complete, ensuring nested VarRefs can be dereferenced
      final valueAsTerm = value is Term ? value : ConstTerm(value);
      final acts = heap.bindWriterNoCallback(varId, valueAsTerm);
      activations.addAll(acts);
      writersWithCallbacks.add(varId);
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

    // Now fire all deferred callbacks after ALL bindings are complete
    // This ensures nested VarRefs in bound structures can be fully dereferenced
    for (final writerAddr in writersWithCallbacks) {
      heap.firePendingCallback(writerAddr);
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
