/// FCP Two-Cell Heap with Address-Based Design
/// Variables are identified by heap addresses directly, with no separate ID namespace.
/// Each variable consists of two consecutive cells: writer at addr N, reader at addr N+1.
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

/// FCP Two-Cell Heap with Address-Based Variable Identity
/// 
/// In this design:
/// - Variables are identified by heap addresses, not separate IDs
/// - allocateVariable() returns writerAddr (the "varId" IS the address)
/// - Writer is at addr N, reader is at addr N+1
/// - varTable maps writerAddr -> (writerAddr, writerAddr+1) for compatibility
class HeapFCP {
  final List<HeapCell> cells = [];
  
  /// Compatibility: varTable maps writerAddr -> (writerAddr, readerAddr)
  /// This is an identity mapping that will be removed in Phase 4
  final Map<int, (int, int)> varTable = {};
  
  int HP = 0;  // Heap pointer (next free address)

  /// Callbacks for external observation (Phase 0 I/O)
  /// Keyed by writerAddr (not varId, since varId == writerAddr now)
  final Map<int, void Function(Term)> _bindCallbacks = {};

  /// Allocate a fresh variable
  /// Returns writerAddr, which serves as the variable's identity ("varId")
  /// Writer cell is at writerAddr, reader cell is at writerAddr + 1
  int allocateVariable() {
    final wAddr = HP++;
    final rAddr = HP++;

    // Writer cell points to reader
    cells.add(HeapCell(Pointer(rAddr), CellTag.WrtTag));

    // Reader cell is initially unbound (null content, RoTag)
    cells.add(HeapCell(null, CellTag.RoTag));

    // Compatibility: populate varTable with identity mapping
    // varId == wAddr, so varTable[wAddr] = (wAddr, rAddr)
    varTable[wAddr] = (wAddr, rAddr);
    
    return wAddr;  // Return writerAddr as "varId"
  }

  // ==========================================================================
  // Address Helper Methods (new in address-based design)
  // ==========================================================================

  /// Get paired reader address from writer address
  /// This is the allocation pair relationship: reader = writer + 1
  int readerAddrFor(int writerAddr) => writerAddr + 1;

  /// Get paired writer address from reader address  
  /// This is the allocation pair relationship: writer = reader - 1
  int writerAddrFor(int readerAddr) => readerAddr - 1;

  /// Check if address is a writer cell
  bool isWriter(int addr) => addr < cells.length && cells[addr].tag == CellTag.WrtTag;

  /// Check if address is a reader cell
  bool isReader(int addr) => addr < cells.length && cells[addr].tag == CellTag.RoTag;

  /// Get address from VarRef (accounts for isReader flag)
  int addrFromVarRef(VarRef ref) {
    // varId IS writerAddr, isReader determines +0 or +1
    return ref.isReader ? ref.varId + 1 : ref.varId;
  }

  // ==========================================================================
  // Core Operations
  // ==========================================================================

  /// Register callback for when variable is bound (Phase 0 I/O)
  /// Takes writerAddr (which is also the varId in current design)
  void onBind(int writerAddr, void Function(Term) callback) {
    // Check if already bound
    if (isFullyBound(writerAddr)) {
      final value = getValue(writerAddr);
      if (value != null) {
        callback(value);
      }
      return;
    }
    // Register for later invocation (keyed by writerAddr)
    _bindCallbacks[writerAddr] = callback;
  }

  /// Remove a registered callback (for cleanup)
  void removeBindCallback(int writerAddr) {
    _bindCallbacks.remove(writerAddr);
  }

  /// Address-based dereferencing
  /// Follows variable chains using address arithmetic, no varTable lookup
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

        // If bound to VarRef, follow the chain
        if (content is VarRef) {
          // varId IS writerAddr, isReader tells us +0 or +1
          current = content.isReader ? content.varId + 1 : content.varId;
          continue;
        }

        // Bound to ground term - return it
        return content as Term;
      }

      // Check if this is a writer cell pointing to its paired reader
      if (cell.content is Pointer && cell.tag == CellTag.WrtTag) {
        // Unbound writer → return writer VarRef
        // Since varId == writerAddr, we use current as varId
        return VarRef(current, isReader: false);
      }

      // Follow pointer to another cell (for other pointer types)
      if (cell.content is Pointer) {
        current = (cell.content as Pointer).targetAddr;
        continue;
      }

      // Unbound reader → return reader VarRef
      // For a reader at addr N+1, the varId (writerAddr) is N
      if (cell.tag == CellTag.RoTag) {
        final writerAddr = current - 1;
        return VarRef(writerAddr, isReader: true);
      }

      // Unbound (other case) - construct VarRef
      // This shouldn't normally happen with proper two-cell design
      return VarRef(current, isReader: false);
    }
  }

  /// API: Check if variable is fully bound to ground term
  /// Takes writerAddr (which is also varId)
  bool isFullyBound(int writerAddr) {
    final result = derefAddr(writerAddr);
    return result is! VarRef;
  }

  /// API: Get variable value (dereferenced)
  /// Takes writerAddr (which is also varId)
  Term? getValue(int writerAddr) {
    final result = derefAddr(writerAddr);
    return result is VarRef ? null : result;
  }

  /// API: Bind variable to a term
  /// Takes writerAddr (which is also varId)
  /// Returns list of goals to reactivate
  List<GoalRef> bindVariable(int writerAddr, Term value) {
    final rAddr = writerAddr + 1;  // Address arithmetic, no varTable lookup

    // Dereference value if it's a VarRef
    var finalValue = value;
    if (value is VarRef) {
      // varId IS writerAddr
      finalValue = derefAddr(value.varId);
    }

    // Save suspension list BEFORE overwriting reader content
    final oldContent = cells[rAddr].content;

    // Bind both cells to the dereferenced value
    cells[writerAddr].content = finalValue;
    cells[writerAddr].tag = CellTag.ValueTag;
    cells[rAddr].content = finalValue;
    cells[rAddr].tag = CellTag.ValueTag;

    // Handle suspensions based on whether we're binding to ground or unbound
    final activations = <GoalRef>[];
    if (oldContent is SuspensionListNode) {
      if (finalValue is VarRef) {
        // Binding to another variable - FORWARD suspensions
        _forwardSuspensionsByAddr(oldContent, finalValue.varId + 1);  // target reader addr
      } else {
        // Binding to ground value - activate suspensions
        _walkAndActivate(oldContent, activations);
      }
    }

    // Notify external observer if registered
    final callback = _bindCallbacks.remove(writerAddr);
    if (callback != null) {
      if (finalValue is VarRef) {
        // Binding to another variable - forward callback to target
        _bindCallbacks[finalValue.varId] = callback;  // varId IS writerAddr
      } else {
        // Binding to ground value - invoke callback now
        callback(finalValue);
      }
    }

    return activations;
  }

  /// Forward suspension list to another reader cell (by address)
  void _forwardSuspensionsByAddr(SuspensionListNode? list, int targetReaderAddr) {
    var current = list;

    while (current != null) {
      if (current.armed) {
        // Create a new node sharing the same SuspensionRecord
        final newNode = SuspensionListNode(current.record);
        final targetContent = cells[targetReaderAddr].content;
        newNode.next = targetContent is SuspensionListNode ? targetContent : null;
        cells[targetReaderAddr].content = newNode;
      }
      current = current.next;
    }
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

  /// API: Bind variable to constant
  List<GoalRef> bindVariableConst(int writerAddr, Object? v) {
    return bindVariable(writerAddr, ConstTerm(v));
  }

  /// API: Bind variable to structure
  List<GoalRef> bindVariableStruct(int writerAddr, String functor, List<Term> args) {
    return bindVariable(writerAddr, StructTerm(functor, args));
  }

  /// Get suspension list from reader cell (by writerAddr/varId)
  SuspensionListNode? getSuspensions(int writerAddr) {
    final rAddr = writerAddr + 1;
    final cell = cells[rAddr];
    return cell.content is SuspensionListNode ? cell.content as SuspensionListNode : null;
  }

  /// Add suspension to reader cell (by writerAddr/varId)
  void addSuspension(int writerAddr, SuspensionListNode node) {
    final rAddr = writerAddr + 1;
    final oldContent = cells[rAddr].content;

    // Prepend new node to existing list
    node.next = oldContent is SuspensionListNode ? oldContent : null;
    cells[rAddr].content = node;
  }

  /// Process suspensions after binding
  List<GoalRef> processBindSuspensions(int writerAddr) {
    final rAddr = writerAddr + 1;
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

  // ==========================================================================
  // Compatibility Methods (to be removed in Phase 4)
  // ==========================================================================

  /// Compatibility: Forward suspension list to another variable (by varId)
  /// varId IS writerAddr, so this just calls the address-based version
  void _forwardSuspensions(SuspensionListNode? list, int targetVarId) {
    _forwardSuspensionsByAddr(list, targetVarId + 1);  // +1 to get reader addr
  }

  bool isWriterBound(int writerId) => isFullyBound(writerId);

  Term? valueOfWriter(int writerId) => getValue(writerId);

  List<GoalRef> bindWriterConst(int writerId, Object? v) => bindVariableConst(writerId, v);

  List<GoalRef> bindWriterStruct(int writerId, String f, List<Term> args) {
    return bindVariableStruct(writerId, f, args);
  }

  /// Compatibility: Get (writerId, readerId) pair
  /// Both are the same value (writerAddr) since readerId doesn't exist separately
  (int, int) allocateFreshPair() {
    final writerAddr = allocateVariable();
    return (writerAddr, writerAddr);  // Same value for compatibility
  }

  /// Compatibility: writerIdForReader - returns the same value since varId == writerAddr
  int? writerIdForReader(int readerId) => readerId;

  /// Compatibility: allocateFreshVar
  int allocateFreshVar() => allocateVariable();

  /// Compatibility: addVariable - no-op
  void addVariable(int varId) {
    // No-op - variables already in varTable from allocateVariable
  }

  /// Compatibility: writer - no-op stub
  Object? writer(int writerId) => null;

  /// Compatibility: dereference term
  Term dereference(Term term) {
    if (term is VarRef) {
      // varId IS writerAddr
      return derefAddr(term.varId);
    }
    return term;
  }

  /// Compatibility: isBound
  bool isBound(int varId) => isFullyBound(varId);

  /// Compatibility: allVarIds - returns all writerAddrs
  Iterable<int> get allVarIds => varTable.keys;

  // ==========================================================================
  // Phase 1 Compatibility: varTable lookup emulation
  // These methods allow code that hasn't been migrated yet to continue working
  // ==========================================================================

  /// Compatibility: Emulate varTable lookup
  /// Returns (addr, addr+1) if addr is a valid writer, null otherwise
  (int, int)? varTableLookup(int addr) {
    if (addr < 0 || addr >= HP) return null;
    if (!isWriter(addr)) return null;
    return (addr, addr + 1);
  }
}
