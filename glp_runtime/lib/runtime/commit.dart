import 'machine_state.dart';
import 'heap.dart';
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
    print('[TRACE Commit FCP] Applying σ̂w to heap (${sigmaHat.length} bindings):');
    for (final entry in sigmaHat.entries) {
      print('  W${entry.key} → ${entry.value}');
    }

    final activations = <GoalRef>[];

    for (final entry in sigmaHat.entries) {
      final varId = entry.key;
      var value = entry.value;

      // Skip null values (writer declared but not bound in HEAD)
      if (value == null) continue;

      // Convert varId to addresses
      final (wAddr, rAddr) = heap.varTable[varId]!;

      // FCP line 226: CRITICAL - dereference by ADDRESS before binding
      // This prevents W1009→R1014→nil chains
      if (value is VarRef) {
        final (targetWAddr, _) = heap.varTable[value.varId]!;
        value = heap.derefAddr(targetWAddr);
        print('[TRACE Commit FCP] Dereferenced VarRef(${(entry.value as VarRef).varId}) → $value');
      }

      // FCP line 301: Save suspension list before replacing reader content
      final oldContent = heap.cells[rAddr].content;

      // FCP lines 233/303: Bind BOTH cells to dereferenced value
      heap.cells[wAddr].content = value;
      heap.cells[wAddr].tag = CellTag.ValueTag;
      heap.cells[rAddr].content = value;
      heap.cells[rAddr].tag = CellTag.ValueTag;

      // FCP lines 245-254: Walk saved suspension list and activate
      if (oldContent is SuspensionRecord) {
        print('[TRACE Commit FCP] Processing suspension list for R$varId:');
        _walkAndActivate(oldContent, activations);
      }
    }

    // CRITICAL FIX: Re-dereference all bound cells that contain VarRef
    // This handles dependencies in σ̂w (e.g., W1002→V1005, W1005→nil)
    // After first pass: W1002 contains VarRef(1005), W1005 contains nil
    // After second pass: W1002 contains nil (dereferenced through chain)
    print('[TRACE Commit FCP] Re-dereferencing cells with VarRef values...');
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
          print('[TRACE Commit FCP]   W$varId: VarRef(${wContent.varId}) → $derefValue');
        }
      }
    }

    print('[TRACE Commit FCP] Total goals reactivated: ${activations.length}');
    return activations;
  }

  /// Walk suspension list and activate armed records (FCP lines 247-253)
  static void _walkAndActivate(SuspensionRecord? list, List<GoalRef> acts) {
    var current = list;
    int count = 0;

    while (current != null) {
      if (current.armed) {
        acts.add(GoalRef(current.goalId!, current.resumePC));
        current.disarm();  // Prevent re-activation
        count++;
      }
      current = current.next;
    }

    print('[TRACE Commit FCP]   Activated $count goal(s)');
  }


  /// Apply tentative writer substitution σ̂w (v2.16 semantics - deprecated)
  /// TODO: Remove after full migration to FCP
  static List<GoalRef> applySigmaHatV216({
    required Heap heap,
    required Map<int, Object?> sigmaHat,
  }) {
    throw UnimplementedError('applySigmaHatV216 deprecated - use applySigmaHatFCP');
  }

  /// Legacy version (deprecated)
  /// TODO: Remove after full migration to FCP
  static List<GoalRef> applySigmaHat({
    required Heap heap,
    required Iterable<int> writerIds,
  }) {
    throw UnimplementedError('applySigmaHat deprecated - use applySigmaHatFCP');
  }
}
