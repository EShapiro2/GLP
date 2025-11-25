/// FCP Two-Cell Heap with Address-Based Dereferencing
/// Follows FCP AM design exactly: two cells per variable, suspension lists in reader cells
library;

import 'terms.dart';
import 'suspension.dart';
import 'machine_state.dart';

/// Cell tags matching FCP design
enum CellTag {
  WrtTag,   // Writer cell
  RoTag,    // Read-only (reader) cell
  ValueTag, // Bound to value
}

/// Heap cell - contains either Pointer, SuspensionListNode, or Term
class HeapCell {
  dynamic content;  // Pointer | SuspensionListNode | Term
  CellTag tag;

  HeapCell(this.content, this.tag);

  bool get hasValue => tag == CellTag.ValueTag;
  bool get hasSuspensions => content is SuspensionListNode;
}

/// Pointer to another cell (just an address - List index)
class Pointer {
  final int targetAddr;  // Index in cells List

  Pointer(this.targetAddr);

  @override
  String toString() => 'Ptr($targetAddr)';
}

/// FCP Two-Cell Heap
class HeapFCP {
  final List<HeapCell> cells = [];
  final Map<int, (int, int)> varTable = {};  // varId -> (writerAddr, readerAddr)
  int HP = 0;  // Heap pointer (next free address)
  int nextVarId = 1000;

  /// Allocate a fresh variable (FCP notify.c lines 194-195)
  /// Returns varId, stores (writerAddr, readerAddr) in varTable
  int allocateVariable() {
    final varId = nextVarId++;
    final wAddr = HP++;
    final rAddr = HP++;

    // Writer cell points to reader
    cells.add(HeapCell(Pointer(rAddr), CellTag.WrtTag));

    // Reader cell is initially unbound (null content, RoTag)
    // NOT pointing back to writer - that would create a cycle
    cells.add(HeapCell(null, CellTag.RoTag));

    varTable[varId] = (wAddr, rAddr);
    return varId;
  }

  /// Address-based dereferencing (FCP-exact)
  /// Follows addresses directly like FCP's pointer arithmetic: p = *p
  /// No reverse lookup during traversal - only at exit when constructing VarRef
  Term derefAddr(int addr) {
    var current = addr;
    Set<int> visited = {};

    while (true) {
      if (visited.contains(current)) {
        throw StateError('Cycle detected at address $current - SRSW violation!');
      }
      visited.add(current);

      final cell = cells[current];

      // Bound to value
      if (cell.tag == CellTag.ValueTag) {
        final content = cell.content;

        // If bound to VarRef, follow the chain (variable→variable binding)
        // IMPORTANT: Use reader address if VarRef is reader, writer address if writer
        if (content is VarRef) {
          final (targetWAddr, targetRAddr) = varTable[content.varId]!;
          current = content.isReader ? targetRAddr : targetWAddr;
          continue;
        }

        // Bound to ground term - return it
        return content as Term;
      }

      // Check if this is a writer cell pointing to its paired reader
      // Writer cells contain Pointer(readerAddr) but should NOT be dereferenced
      // Instead, return the writer VarRef
      if (cell.content is Pointer && cell.tag == CellTag.WrtTag) {
        // This is an unbound writer → return writer VarRef, don't follow to reader
        return _varRefFromAddr(current);
      }

      // Follow pointer to another cell (for other pointer types)
      if (cell.content is Pointer) {
        current = (cell.content as Pointer).targetAddr;
        continue;
      }

      // Unbound - construct VarRef from address (API boundary only)
      return _varRefFromAddr(current);
    }
  }

  /// Helper: Convert address to VarRef (only at API boundary)
  /// This is O(n) but only happens when returning unbound VarRef to external API
  VarRef _varRefFromAddr(int addr) {
    for (final entry in varTable.entries) {
      final (wAddr, rAddr) = entry.value;
      if (wAddr == addr) {
        return VarRef(entry.key, isReader: false);  // Writer
      }
      if (rAddr == addr) {
        return VarRef(entry.key, isReader: true);   // Reader
      }
    }
    throw StateError('Address $addr not found in varTable');
  }

  /// API: Check if variable is fully bound to ground term
  bool isFullyBound(int varId) {
    final (wAddr, _) = varTable[varId]!;
    final result = derefAddr(wAddr);
    return result is! VarRef;
  }

  /// API: Get variable value (dereferenced)
  Term? getValue(int varId) {
    final (wAddr, _) = varTable[varId]!;
    final result = derefAddr(wAddr);
    return result is VarRef ? null : result;
  }

  /// API: Bind variable to a term
  /// Returns list of goals to reactivate (FCP: ALL bindings process suspensions)
  List<GoalRef> bindVariable(int varId, Term value) {
    final (wAddr, rAddr) = varTable[varId]!;

    // Dereference value if it's a VarRef
    var finalValue = value;
    if (value is VarRef) {
      final (targetWAddr, _) = varTable[value.varId]!;
      finalValue = derefAddr(targetWAddr);
    }

    // Defensive WxW check: prevent writer-to-writer binding
    if (finalValue is VarRef && !finalValue.isReader) {
      // Attempting to bind writer varId to another unbound writer
      if (!isWriterBound(varId) && !isWriterBound(finalValue.varId)) {
        throw StateError('WxW violation in bindVariable: W$varId → W${finalValue.varId} (both unbound)');
      }
    }

    // Save suspension list BEFORE overwriting reader content
    final oldContent = cells[rAddr].content;

    // Bind both cells to the dereferenced value
    cells[wAddr].content = finalValue;
    cells[wAddr].tag = CellTag.ValueTag;
    cells[rAddr].content = finalValue;
    cells[rAddr].tag = CellTag.ValueTag;

    // Handle suspensions based on whether we're binding to ground or unbound
    final activations = <GoalRef>[];
    if (oldContent is SuspensionListNode) {
      if (finalValue is VarRef) {
        // Binding to another variable (still unbound) - FORWARD suspensions
        // Don't activate now; merge into target's suspension list
        _forwardSuspensions(oldContent, finalValue.varId);
      } else {
        // Binding to ground value - activate suspensions
        _walkAndActivate(oldContent, activations);
      }
    }
    return activations;
  }

  /// Forward suspension list to another variable (for reader chains)
  void _forwardSuspensions(SuspensionListNode? list, int targetVarId) {
    final (_, targetRAddr) = varTable[targetVarId]!;
    var current = list;

    while (current != null) {
      if (current.armed) {
        // Create a new node sharing the same SuspensionRecord
        // This ensures disarming one disarms all copies (FCP shared state)
        final newNode = SuspensionListNode(current.record);
        final targetContent = cells[targetRAddr].content;
        newNode.next = targetContent is SuspensionListNode ? targetContent : null;
        cells[targetRAddr].content = newNode;
      }
      current = current.next;
    }
  }

  /// Walk suspension list and activate armed records (from commit.dart)
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

  /// API: Bind variable to constant
  /// Returns list of goals to reactivate
  List<GoalRef> bindVariableConst(int varId, Object? v) {
    return bindVariable(varId, ConstTerm(v));
  }

  /// API: Bind variable to structure
  /// Returns list of goals to reactivate
  List<GoalRef> bindVariableStruct(int varId, String functor, List<Term> args) {
    return bindVariable(varId, StructTerm(functor, args));
  }

  /// Get suspension list from reader cell (if any)
  SuspensionListNode? getSuspensions(int varId) {
    final (_, rAddr) = varTable[varId]!;
    final cell = cells[rAddr];
    return cell.content is SuspensionListNode ? cell.content as SuspensionListNode : null;
  }

  /// Add suspension to reader cell (prepend to list)
  void addSuspension(int varId, SuspensionListNode node) {
    final (_, rAddr) = varTable[varId]!;
    final oldContent = cells[rAddr].content;

    // Prepend new node to existing list
    node.next = oldContent is SuspensionListNode ? oldContent : null;
    cells[rAddr].content = node;  // REPLACE content
  }

  /// Process suspensions after binding (FCP commit-like operation for BODY bindings)
  /// Returns list of goals to wake
  List<GoalRef> processBindSuspensions(int varId) {
    final (_, rAddr) = varTable[varId]!;
    final oldContent = cells[rAddr].content;

    final activations = <GoalRef>[];

    if (oldContent is SuspensionListNode) {
      SuspensionListNode? current = oldContent;
      while (current != null) {
        if (current.armed) {
          activations.add(GoalRef(current.goalId!, current.resumePC));
          current.record.disarm();
        }
        current = current.next;
      }
    }

    return activations;
  }

  /// Compatibility methods for existing code (can be removed later)

  bool isWriterBound(int writerId) => isFullyBound(writerId);

  Term? valueOfWriter(int writerId) => getValue(writerId);

  List<GoalRef> bindWriterConst(int writerId, Object? v) => bindVariableConst(writerId, v);

  List<GoalRef> bindWriterStruct(int writerId, String f, List<Term> args) {
    return bindVariableStruct(writerId, f, args);
  }

  /// Compatibility: Get (writerId, readerId) pair
  /// In two-cell FCP design, both are the same varId but map to different addresses
  (int, int) allocateFreshPair() {
    final varId = allocateVariable();
    return (varId, varId);  // Same ID for compatibility
  }

  /// Compatibility: writerIdForReader - in FCP two-cell, both map to same varId
  int? writerIdForReader(int readerId) => readerId;

  /// Compatibility: allocateFreshVar - allocates a fresh variable
  int allocateFreshVar() => allocateVariable();

  /// Compatibility: addVariable - no-op in FCP (vars allocated on demand)
  void addVariable(int varId) {
    // No-op - variables already in varTable from allocateFreshVar
  }

  /// Compatibility: writer - no-op stub (not used in FCP design)
  Object? writer(int writerId) => null;

  /// Compatibility: dereference - wraps derefAddr for API compatibility
  Term dereference(Term term) {
    if (term is VarRef) {
      final (wAddr, _) = varTable[term.varId]!;
      return derefAddr(wAddr);
    }
    return term;
  }

  /// Compatibility: isBound - checks if variable is bound
  bool isBound(int varId) => isFullyBound(varId);

  /// Compatibility: allVarIds - returns all variable IDs in varTable
  Iterable<int> get allVarIds => varTable.keys;
}
