/// FCP Two-Cell Heap with Pointer Architecture
///
/// Per heap-pointer-architecture-spec.md v3.0:
/// - Reader cells point TO writer cells
/// - Writer cells contain: null (unbound), SuspensionListNode (waiting), or Pointer (bound to var)
/// - Suspensions live on writer cells, not reader cells
/// - ValueTag indicates bound to ground value
library;

import 'terms.dart';
import 'suspension.dart';
import 'machine_state.dart';
import 'package:glp_runtime/multiagent/variable_table.dart' show VariableEntry;

/// Cell tags matching FCP design
enum CellTag {
  WrtTag,   // Writer cell
  RoTag,    // Read-only (reader) cell
  ValueTag, // Bound to ground value
}

/// Heap cell - contains either Pointer, SuspensionListNode, Term, or VariableEntry
class HeapCell {
  dynamic content;  // null | Pointer | SuspensionListNode | Term | VariableEntry
  CellTag tag;

  HeapCell(this.content, this.tag);

  bool get hasValue => tag == CellTag.ValueTag;
  bool get hasSuspensions => content is SuspensionListNode;
}

/// Pointer to another cell (heap address)
class Pointer {
  final int targetAddr;

  Pointer(this.targetAddr);

  @override
  String toString() => 'Ptr($targetAddr)';
}

/// FCP Two-Cell Heap with Pointer-Based Variable Identity
/// 
/// Per heap-pointer-architecture-spec.md v3.0:
/// - allocateVariable() returns (writerAddr, readerAddr) tuple
/// - Reader cell points TO writer cell
/// - Writer cell contains null (unbound), SuspensionListNode, or Pointer (chain)
/// - Suspensions are stored on writer cells
class HeapFCP {
  final List<HeapCell> cells = [];
  
  int HP = 0;  // Heap pointer (next free address)

  /// Callbacks for external observation (Phase 0 I/O)
  /// Keyed by writerAddr
  final Map<int, void Function(Term)> _bindCallbacks = {};

  // ==========================================================================
  // Variable Allocation (Section 3 of spec)
  // ==========================================================================

  /// Allocate a fresh local variable
  /// Returns (writerAddr, readerAddr) tuple
  /// 
  /// Per spec Section 3.1:
  /// - Writer cell: null content (unbound, no suspensions)
  /// - Reader cell: Pointer to writer
  (int, int) allocateVariable() {
    final writerAddr = HP++;
    final readerAddr = HP++;

    // Writer cell: initially unbound (null content)
    cells.add(HeapCell(null, CellTag.WrtTag));

    // Reader cell: points TO writer
    cells.add(HeapCell(Pointer(writerAddr), CellTag.RoTag));

    return (writerAddr, readerAddr);
  }

  /// Allocate a single reader cell for an imported variable (no local writer)
  /// 
  /// Per irmaGLP spec, imported readers have no local paired writer.
  /// The cell content will be set to a VariableEntry by the caller.
  int allocateImportedReader() {
    final readerAddr = HP++;
    cells.add(HeapCell(null, CellTag.RoTag));
    return readerAddr;
  }

  /// Allocate a single writer cell for an imported variable (no local reader)
  /// 
  /// Per irmaGLP spec, imported writers have no local paired reader.
  /// The cell content will be set to a VariableEntry by the caller.
  int allocateImportedWriter() {
    final writerAddr = HP++;
    cells.add(HeapCell(null, CellTag.WrtTag));
    return writerAddr;
  }

  // ==========================================================================
  // Cell Type Checking
  // ==========================================================================

  /// Check if address is a writer cell
  bool isWriter(int addr) => 
      addr >= 0 && addr < cells.length && cells[addr].tag == CellTag.WrtTag;

  /// Check if address is a reader cell
  bool isReader(int addr) => 
      addr >= 0 && addr < cells.length && cells[addr].tag == CellTag.RoTag;

  /// Check if address is a value cell (bound to ground)
  bool isValue(int addr) =>
      addr >= 0 && addr < cells.length && cells[addr].tag == CellTag.ValueTag;

  // ==========================================================================
  // Pointer Navigation (Section 7 of spec)
  // ==========================================================================

  /// Get writer address from reader address by following pointer
  ///
  /// Per spec Section 7.1: Follow the reader's pointer to get the writer.
  /// PRECONDITION: addr must be a reader address (RoTag cell)
  int writerForReader(int readerAddr) {
    final cell = cells[readerAddr];
    if (cell.tag != CellTag.RoTag) {
      throw StateError('writerForReader called on non-reader cell at $readerAddr (tag: ${cell.tag})');
    }
    if (cell.content is! Pointer) {
      throw StateError('Reader cell at $readerAddr has no pointer (content: ${cell.content})');
    }
    return (cell.content as Pointer).targetAddr;
  }

  /// Try to get writer address from reader, returns null for imported readers
  ///
  /// For local readers (with Pointer), returns the writer address.
  /// For imported readers (with VariableEntry), returns null.
  int? tryWriterForReader(int readerAddr) {
    final cell = cells[readerAddr];
    if (cell.tag != CellTag.RoTag) {
      return null;
    }
    if (cell.content is Pointer) {
      return (cell.content as Pointer).targetAddr;
    }
    return null; // Imported reader - no local writer
  }

  // ==========================================================================
  // Dereferencing (Section 4 of spec)
  // ==========================================================================

  /// Dereference an address to its final value
  /// 
  /// Per spec Section 4.2:
  /// - RoTag: follow Pointer to target
  /// - WrtTag with null/SuspensionListNode: unbound, return VarRef
  /// - WrtTag with Pointer: follow to target (variable chain)
  /// - ValueTag: return the Term content
  /// - VariableEntry: check state for value or return entry
  /// 
  /// Returns: Term (bound) | VarRef (unbound writer) | VariableEntry (imported unbound)
  Object derefAddr(int startAddr) {
    var current = startAddr;
    final visited = <int>{};

    while (true) {
      if (visited.contains(current)) {
        throw StateError('Cycle detected at address $current - SRSW violation!');
      }
      visited.add(current);

      final cell = cells[current];

      switch (cell.tag) {
        case CellTag.RoTag:
          // Reader cell
          if (cell.content is VariableEntry) {
            // Imported reader - check for value in entry
            final entry = cell.content as VariableEntry;
            if (entry.state is Term) {
              return entry.state as Term;
            }
            return entry;  // Unbound imported
          }
          if (cell.content is Pointer) {
            // Follow pointer to writer
            current = (cell.content as Pointer).targetAddr;
            continue;
          }
          throw StateError('Reader cell at $current has invalid content: ${cell.content}');

        case CellTag.WrtTag:
          // Writer cell
          if (cell.content is VariableEntry) {
            // Imported writer - check for value in entry
            final entry = cell.content as VariableEntry;
            if (entry.state is Term) {
              return entry.state as Term;
            }
            return entry;  // Unbound imported
          }
          if (cell.content == null || cell.content is SuspensionListNode) {
            // Unbound writer - return VarRef to this address
            return VarRef(current);
          }
          if (cell.content is Pointer) {
            // Bound to another variable - follow pointer
            current = (cell.content as Pointer).targetAddr;
            continue;
          }
          throw StateError('Writer cell at $current has invalid content: ${cell.content}');

        case CellTag.ValueTag:
          // Bound to ground value
          return cell.content as Term;
      }
    }
  }

  // ==========================================================================
  // Binding (Section 5 of spec)
  // ==========================================================================

  /// Bind a writer to a ground term value
  /// 
  /// Per spec Section 5.1:
  /// - Changes writer tag to ValueTag
  /// - Stores value as content
  /// - Activates any suspensions on the writer
  /// 
  /// Returns list of goals to reactivate
  List<GoalRef> bindWriter(int writerAddr, Term value) {
    final cell = cells[writerAddr];
    if (cell.tag != CellTag.WrtTag) {
      throw StateError('bindWriter called on non-writer cell at $writerAddr (tag: ${cell.tag})');
    }

    final activations = <GoalRef>[];

    // Save and process suspensions before overwriting
    if (cell.content is SuspensionListNode) {
      _walkAndActivate(cell.content as SuspensionListNode, activations);
    }

    // Bind to value
    cell.content = value;
    cell.tag = CellTag.ValueTag;

    // Notify external observer if registered
    final callback = _bindCallbacks.remove(writerAddr);
    if (callback != null) {
      callback(value);
    }

    return activations;
  }

  /// Bind a writer to another variable (via its reader)
  /// 
  /// Per spec Section 5.3:
  /// - Stores Pointer(readerAddr) in writer cell
  /// - Forwards suspensions to target writer
  /// - Tag remains WrtTag (not bound to ground)
  /// 
  /// Returns list of goals to reactivate (empty if target unbound)
  List<GoalRef> bindWriterToReader(int writerAddr, int readerAddr) {
    final writerCell = cells[writerAddr];
    if (writerCell.tag != CellTag.WrtTag) {
      throw StateError('bindWriterToReader called on non-writer at $writerAddr');
    }

    final readerCell = cells[readerAddr];
    if (readerCell.tag != CellTag.RoTag) {
      throw StateError('bindWriterToReader target is not a reader at $readerAddr');
    }

    final activations = <GoalRef>[];

    // Forward suspensions to target writer
    if (writerCell.content is SuspensionListNode) {
      final targetWriterAddr = writerForReader(readerAddr);
      _forwardSuspensions(writerCell.content as SuspensionListNode, targetWriterAddr);
    }

    // Store pointer to reader (creates variable chain)
    writerCell.content = Pointer(readerAddr);
    // Tag remains WrtTag

    // Forward external callback if registered
    final callback = _bindCallbacks.remove(writerAddr);
    if (callback != null) {
      final targetWriterAddr = writerForReader(readerAddr);
      _bindCallbacks[targetWriterAddr] = callback;
    }

    return activations;
  }

  /// Bind writer to writer (WxW violation)
  /// 
  /// Per spec Section 5.2: This is forbidden and should throw
  void bindWriterToWriter(int w1, int w2) {
    throw StateError('WxW violation: cannot bind writer $w1 to writer $w2');
  }

  // ==========================================================================
  // Suspension (Section 6 of spec)
  // ==========================================================================

  /// Add a suspension to a writer cell
  /// 
  /// Per spec Section 6.1: Suspensions are stored on writer cells
  void suspendOnWriter(int writerAddr, SuspensionRecord record) {
    final cell = cells[writerAddr];
    if (cell.tag != CellTag.WrtTag) {
      throw StateError('suspendOnWriter called on non-writer at $writerAddr');
    }

    final node = SuspensionListNode(record);

    // Prepend to existing suspension list
    if (cell.content is SuspensionListNode) {
      node.next = cell.content as SuspensionListNode;
    }
    cell.content = node;
  }

  /// Add a suspension via a reader (finds writer and adds there)
  /// 
  /// Per spec Section 6.1: Find the reader's writer and add suspension there
  void suspendOnReader(int readerAddr, SuspensionRecord record) {
    final cell = cells[readerAddr];
    
    if (cell.content is VariableEntry) {
      // Imported reader - store suspension in entry or separate mechanism
      // For now, we'll add a suspension list to the entry's state
      // This may need refinement based on irmaGLP requirements
      final entry = cell.content as VariableEntry;
      // TODO: Handle imported reader suspension properly
      // For now, just return - the caller should handle this case
      return;
    }

    if (cell.tag != CellTag.RoTag || cell.content is! Pointer) {
      throw StateError('suspendOnReader called on invalid reader at $readerAddr');
    }

    final writerAddr = (cell.content as Pointer).targetAddr;
    suspendOnWriter(writerAddr, record);
  }

  /// Forward suspensions from one writer to another
  void _forwardSuspensions(SuspensionListNode? list, int targetWriterAddr) {
    var current = list;
    while (current != null) {
      if (current.armed) {
        // Create new node sharing the same record
        final newNode = SuspensionListNode(current.record);
        final targetCell = cells[targetWriterAddr];
        if (targetCell.content is SuspensionListNode) {
          newNode.next = targetCell.content as SuspensionListNode;
        }
        targetCell.content = newNode;
      }
      current = current.next;
    }
  }

  /// Walk suspension list and activate armed records
  static void _walkAndActivate(SuspensionListNode? list, List<GoalRef> activations) {
    var current = list;
    while (current != null) {
      if (current.armed) {
        activations.add(GoalRef(current.goalId!, current.resumePC));
        current.record.disarm();
      }
      current = current.next;
    }
  }

  // ==========================================================================
  // High-Level API
  // ==========================================================================

  /// Check if variable is fully bound to ground term
  /// 
  /// Returns false for VarRef (unbound) or VariableEntry (imported unbound)
  bool isFullyBound(int writerAddr) {
    final result = derefAddr(writerAddr);
    return result is! VarRef && result is! VariableEntry;
  }

  /// Get variable value (dereferenced)
  /// 
  /// Returns null if unbound
  Term? getValue(int writerAddr) {
    final result = derefAddr(writerAddr);
    if (result is VarRef || result is VariableEntry) {
      return null;
    }
    return result as Term;
  }

  /// Dereference a term
  /// 
  /// If term is VarRef, dereferences it. Otherwise returns term unchanged.
  Term dereference(Term term) {
    if (term is VarRef) {
      final result = derefAddr(term.addr);
      if (result is VariableEntry) {
        return term;  // Imported unbound - return original
      }
      if (result is VarRef) {
        return result;  // Still unbound
      }
      return result as Term;
    }
    return term;
  }

  /// Register callback for when variable is bound
  void onBind(int writerAddr, void Function(Term) callback) {
    if (isFullyBound(writerAddr)) {
      final value = getValue(writerAddr);
      if (value != null) {
        callback(value);
      }
      return;
    }
    _bindCallbacks[writerAddr] = callback;
  }

  /// Remove a registered callback
  void removeBindCallback(int writerAddr) {
    _bindCallbacks.remove(writerAddr);
  }

  // ==========================================================================
  // Compatibility Methods (for gradual migration of callers)
  // ==========================================================================

  /// Bind variable to a term (compatibility wrapper)
  List<GoalRef> bindVariable(int writerAddr, Term value) {
    if (value is VarRef) {
      // Binding to another variable
      if (isReader(value.addr)) {
        return bindWriterToReader(writerAddr, value.addr);
      } else if (isWriter(value.addr)) {
        bindWriterToWriter(writerAddr, value.addr);  // Will throw
        return [];
      }
    }
    return bindWriter(writerAddr, value);
  }

  /// Bind variable to constant
  List<GoalRef> bindVariableConst(int writerAddr, Object? v) {
    return bindWriter(writerAddr, ConstTerm(v));
  }

  /// Bind variable to structure
  List<GoalRef> bindVariableStruct(int writerAddr, String functor, List<Term> args) {
    return bindWriter(writerAddr, StructTerm(functor, args));
  }

  /// Compatibility: isWriterBound
  bool isWriterBound(int writerAddr) => isFullyBound(writerAddr);

  /// Compatibility: valueOfWriter  
  Term? valueOfWriter(int writerAddr) => getValue(writerAddr);

  /// Compatibility: bindWriterConst
  List<GoalRef> bindWriterConst(int writerAddr, Object? v) => bindVariableConst(writerAddr, v);

  /// Compatibility: bindWriterStruct
  List<GoalRef> bindWriterStruct(int writerAddr, String f, List<Term> args) {
    return bindVariableStruct(writerAddr, f, args);
  }

  /// Compatibility: isBound
  bool isBound(int varId) => isFullyBound(varId);

  /// Legacy: Get suspension list (now on writer, not reader)
  SuspensionListNode? getSuspensions(int writerAddr) {
    final cell = cells[writerAddr];
    return cell.content is SuspensionListNode ? cell.content as SuspensionListNode : null;
  }

  /// Legacy: Add suspension (now on writer)
  void addSuspension(int writerAddr, SuspensionListNode node) {
    final cell = cells[writerAddr];
    node.next = cell.content is SuspensionListNode ? cell.content as SuspensionListNode : null;
    cell.content = node;
  }
}
