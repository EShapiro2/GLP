/// FCP Two-Cell Heap with Pointer Architecture
///
/// Per heap-pointer-architecture-spec.md v3.0:
/// - Reader cells point TO writer cells
/// - Writer cells contain: null (unbound), SuspensionListNode (waiting), or Pointer (bound to var)
/// - Suspensions live on writer cells, not reader cells
/// - ValueTag indicates bound to ground value
library;

import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/suspension.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
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
  bool get hasSuspensions => content is WriterContent && (content as WriterContent).suspensions != null;
}

/// Pointer to another cell (heap address)
class Pointer {
  final int targetAddr;

  Pointer(this.targetAddr);

  @override
  String toString() => 'Ptr($targetAddr)';
}

/// Compound content for unbound writer with suspensions.
///
/// Per spec v3.2 Section 2.3: When suspensions are added to an unbound writer,
/// the reader pointer is preserved in this compound structure.
/// This enables readerForWriter() to work even when suspensions are present.
class WriterContent {
  final int readerAddr;  // Pointer to paired reader (preserved)
  SuspensionListNode? suspensions;

  WriterContent(this.readerAddr, [this.suspensions]);

  @override
  String toString() => 'WriterContent(reader=$readerAddr, sus=$suspensions)';
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
  /// Per spec v3.2 Section 3.1 (FCP pattern):
  /// - Writer cell: Pointer to reader (bidirectional)
  /// - Reader cell: Pointer to writer
  /// Both cells point to each other enabling navigation without arithmetic.
  (int, int) allocateVariable() {
    final writerAddr = HP;
    final readerAddr = HP + 1;
    HP += 2;

    // Writer cell: points TO reader (FCP pattern)
    cells.add(HeapCell(Pointer(readerAddr), CellTag.WrtTag));

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
  /// Try to get writer address from a reader address.
  ///
  /// Per spec Section 7.1: Follow the reader's pointer to get the writer.
  ///
  /// **Returns:**
  /// - `int`: The writer address for local readers (cell.content is Pointer)
  /// - `null`: For imported readers (cell.content is VariableEntry) or non-readers
  ///
  /// **Callers MUST handle null appropriately:**
  ///
  /// 1. **Suspending operations** (most common): If the operation needs the writer
  ///    but gets null, the reader is imported and the goal should suspend:
  ///    ```dart
  ///    final wid = heap.tryWriterForReader(rid);
  ///    if (wid == null) {
  ///      // Imported reader - suspend on it
  ///      suspendOnReader(rid, ...);
  ///      return RunResult.suspended;
  ///    }
  ///    ```
  ///
  /// 2. **Read-only operations**: If just reading the value (not modifying),
  ///    use derefAddr() instead, which handles imported readers transparently.
  ///
  /// 3. **Binding operations**: Operations that need to bind the writer (e.g.,
  ///    unification) should check isImportedReader first and use the appropriate
  ///    binding method:
  ///    ```dart
  ///    if (heap.isImportedReader(rid)) {
  ///      // Imported - can't bind locally, must receive value from creator
  ///      suspendOnReader(rid, ...);
  ///    } else {
  ///      // Local - can bind the writer
  ///      heap.bindVariable(wid, value);
  ///    }
  ///    ```
  ///
  /// **Common mistakes to avoid:**
  /// - Using `wid!` without null check → crash on imported readers
  /// - Silently ignoring null → logic errors, lost bindings
  /// - Throwing errors → breaks multiagent scenarios
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

  /// Find the paired reader for an unbound writer (FCP pattern).
  ///
  /// Per spec v3.2 Section 7.2: Follow the writer's pointer to find its paired reader.
  /// Returns null if writer is bound (pointer no longer points to paired reader).
  ///
  /// For code that needs the reader address regardless of binding state,
  /// use pairedReaderAddr() instead.
  int? readerForWriter(int writerAddr) {
    final cell = cells[writerAddr];
    if (cell.tag != CellTag.WrtTag) {
      return null;
    }

    // Case 1: Unbound without suspensions - direct Pointer to reader
    if (cell.content is Pointer) {
      final target = (cell.content as Pointer).targetAddr;
      // Verify it's the paired reader (points back to this writer)
      if (target < cells.length && cells[target].tag == CellTag.RoTag) {
        final readerContent = cells[target].content;
        if (readerContent is Pointer && readerContent.targetAddr == writerAddr) {
          return target;  // Confirmed bidirectional - this is the paired reader
        }
      }
      // Writer is bound to something else, no direct reader access
      return null;
    }

    // Case 2: Unbound with suspensions - compound WriterContent preserves reader pointer
    if (cell.content is WriterContent) {
      return (cell.content as WriterContent).readerAddr;
    }

    // Case 3: Bound or invalid - no reader access
    return null;
  }

  /// Get the paired reader address for a writer (works for bound and unbound).
  ///
  /// Per allocation pattern, the reader is always at writerAddr + 1.
  /// This method should be used when you need the reader address regardless
  /// of whether the writer is currently bound.
  ///
  /// Note: For checking if a writer is unbound and getting its reader via
  /// the bidirectional pointer pattern, use readerForWriter() instead.
  int pairedReaderAddr(int writerAddr) {
    // Try the FCP pattern first (works for unbound writers)
    final reader = readerForWriter(writerAddr);
    if (reader != null) return reader;

    // Fallback: by allocation, reader is at writerAddr + 1
    return writerAddr + 1;
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
    CellTag? previousTag;  // Track previous tag for WxW detection

    while (true) {
      if (visited.contains(current)) {
        throw StateError('Cycle detected at address $current - SRSW violation!');
      }
      visited.add(current);

      final cell = cells[current];

      // Per spec Section 4.5: WxW detection during deref
      // If we followed a pointer from a writer and landed on another writer, that's a violation
      if (previousTag == CellTag.WrtTag && cell.tag == CellTag.WrtTag) {
        throw StateError('SRSW violation: writer at ${visited.elementAt(visited.length - 2)} points to writer at $current');
      }

      switch (cell.tag) {
        case CellTag.RoTag:
          // Reader cell
          if (cell.content is VariableEntry) {
            // Imported reader - check for cached bound value in entry
            final entry = cell.content as VariableEntry;
            if (entry.boundValue != null) {
              return entry.boundValue!;
            }
            return entry;  // Unbound imported
          }
          if (cell.content is Pointer) {
            // Follow pointer to writer
            previousTag = cell.tag;
            current = (cell.content as Pointer).targetAddr;
            continue;
          }
          throw StateError('Reader cell at $current has invalid content: ${cell.content}');

        case CellTag.WrtTag:
          // Writer cell
          if (cell.content is VariableEntry) {
            // Imported writer - check for cached bound value in entry
            final entry = cell.content as VariableEntry;
            if (entry.boundValue != null) {
              return entry.boundValue!;
            }
            return entry;  // Unbound imported
          }
          // Case 1: WriterContent - unbound with suspensions (FCP pattern)
          if (cell.content is WriterContent) {
            // Unbound writer with suspensions - return VarRef to this address
            return VarRef(current);
          }
          // Case 2: Pointer - check if bidirectional (unbound) or chain (bound)
          if (cell.content is Pointer) {
            final target = (cell.content as Pointer).targetAddr;
            // Check if pointer is to paired reader (unbound) or to bound value
            if (target < cells.length && cells[target].tag == CellTag.RoTag) {
              final readerContent = cells[target].content;
              if (readerContent is Pointer && readerContent.targetAddr == current) {
                // Bidirectional - points to paired reader which points back
                // This is an unbound variable
                return VarRef(current);
              }
            }
            // Bound to another cell - follow the pointer
            previousTag = cell.tag;
            current = target;
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
    return bindWriterWithCallbackControl(writerAddr, value, fireCallback: true);
  }

  /// Bind a writer without firing callbacks
  ///
  /// Used by applySigmaHatFCP to defer callbacks until all bindings complete.
  /// This ensures nested VarRefs in structures can be dereferenced correctly.
  List<GoalRef> bindWriterNoCallback(int writerAddr, Term value) {
    return bindWriterWithCallbackControl(writerAddr, value, fireCallback: false);
  }

  /// Internal: bind with callback control
  List<GoalRef> bindWriterWithCallbackControl(int writerAddr, Term value, {required bool fireCallback}) {
    final cell = cells[writerAddr];
    if (cell.tag != CellTag.WrtTag) {
      throw StateError('bindWriter called on non-writer cell at $writerAddr (tag: ${cell.tag})');
    }

    final activations = <GoalRef>[];

    // Save and process suspensions before overwriting (FCP pattern: check WriterContent)
    if (cell.content is WriterContent) {
      final wc = cell.content as WriterContent;
      _walkAndActivate(wc.suspensions, activations);
    }

    // Bind to value
    cell.content = value;
    cell.tag = CellTag.ValueTag;

    // Notify external observer if registered
    if (fireCallback) {
      final callback = _bindCallbacks.remove(writerAddr);
      if (callback != null) {
        callback(value);
      }
    }

    return activations;
  }

  /// Fire pending callback for a writer (if any)
  ///
  /// Used after all bindings complete to fire deferred callbacks.
  void firePendingCallback(int writerAddr) {
    final callback = _bindCallbacks.remove(writerAddr);
    if (callback != null) {
      final value = getValue(writerAddr);
      if (value != null) {
        callback(value);
      }
    }
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

    // bindWriterToReader only works with LOCAL readers (must have paired writer)
    // Imported readers cannot be targets of writer-to-reader binding
    final targetWriterAddr = tryWriterForReader(readerAddr);
    if (targetWriterAddr == null) {
      throw StateError('bindWriterToReader target at $readerAddr is an imported reader (no local writer)');
    }

    final activations = <GoalRef>[];

    // Forward suspensions to target writer (FCP pattern: check WriterContent)
    if (writerCell.content is WriterContent) {
      final wc = writerCell.content as WriterContent;
      _forwardSuspensions(wc.suspensions, targetWriterAddr);
    }

    // Store pointer to reader (creates variable chain)
    writerCell.content = Pointer(readerAddr);
    // Tag remains WrtTag

    // Forward external callback if registered
    final callback = _bindCallbacks.remove(writerAddr);
    if (callback != null) {
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
  /// Per spec v3.2 Section 6.1: Suspensions are stored on writer cells using
  /// WriterContent to preserve the reader pointer.
  void suspendOnWriter(int writerAddr, SuspensionRecord record) {
    final cell = cells[writerAddr];
    if (cell.tag != CellTag.WrtTag) {
      throw StateError('suspendOnWriter called on non-writer at $writerAddr');
    }

    final node = SuspensionListNode(record);

    // FCP pattern: preserve reader pointer using WriterContent
    if (cell.content is WriterContent) {
      // Already has WriterContent - add to suspension list
      final wc = cell.content as WriterContent;
      node.next = wc.suspensions;
      wc.suspensions = node;
    } else if (cell.content is Pointer) {
      // First suspension: convert Pointer to WriterContent
      final readerAddr = (cell.content as Pointer).targetAddr;
      cell.content = WriterContent(readerAddr, node);
    } else {
      throw StateError('suspendOnWriter: unexpected content ${cell.content} at $writerAddr');
    }
  }

  /// Add a suspension via a reader (finds writer and adds there)
  /// 
  /// Per spec Section 6.1: Find the reader's writer and add suspension there
  void suspendOnReader(int readerAddr, SuspensionRecord record) {
    final cell = cells[readerAddr];
    
    if (cell.content is VariableEntry) {
      // Imported reader - store suspension in VariableEntry.suspensions
      // Per spec Section 3.1.2: For imported readers, V_p serves as the
      // "virtual writer" that holds suspensions. When an assignment arrives,
      // goals are resumed from VariableEntry.suspensions.
      final entry = cell.content as VariableEntry;
      final node = SuspensionListNode(record);
      node.next = entry.suspensions;
      entry.suspensions = node;
      return;
    }

    if (cell.tag != CellTag.RoTag || cell.content is! Pointer) {
      throw StateError('suspendOnReader called on invalid reader at $readerAddr');
    }

    final writerAddr = (cell.content as Pointer).targetAddr;
    suspendOnWriter(writerAddr, record);
  }

  /// Forward suspensions from one writer to another
  ///
  /// Per spec v3.2: Target writer uses WriterContent to preserve reader pointer.
  void _forwardSuspensions(SuspensionListNode? list, int targetWriterAddr) {
    var current = list;
    while (current != null) {
      if (current.armed) {
        // Create new node sharing the same record
        final newNode = SuspensionListNode(current.record);
        final targetCell = cells[targetWriterAddr];

        if (targetCell.content is WriterContent) {
          // Target already has WriterContent - add to its suspension list
          final wc = targetCell.content as WriterContent;
          newNode.next = wc.suspensions;
          wc.suspensions = newNode;
        } else if (targetCell.content is Pointer) {
          // Target is unbound with no suspensions - create WriterContent
          final readerAddr = (targetCell.content as Pointer).targetAddr;
          targetCell.content = WriterContent(readerAddr, newNode);
        }
        // Ignore other cases (e.g., bound targets)
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
  // Imported Reader Binding (Multiagent)
  // ==========================================================================

  /// Bind an imported reader to a received value
  ///
  /// Per irmaGLP spec Section 5.3 (imported reader case):
  /// - Imported readers have no local writer, just a reader cell with VariableEntry
  /// - When assignment arrives, the reader cell is updated to point to the value
  /// - Activations are extracted from VariableEntry.suspensions
  ///
  /// Heap structure transformation:
  ///
  /// BEFORE (unbound imported reader):
  /// ```
  /// cells[readerAddr] = HeapCell(VariableEntry(...), CellTag.RoTag)
  /// ```
  ///
  /// AFTER (bound imported reader):
  /// ```
  /// cells[readerAddr] = HeapCell(Pointer(valueCellAddr), CellTag.RoTag)
  /// cells[valueCellAddr] = HeapCell(value, CellTag.ValueTag)
  /// ```
  ///
  /// Note: Unlike local readers (which point to their paired writer), imported
  /// readers point directly to a ValueTag cell. This distinction is used by
  /// isImportedReader() to detect bound imported readers.
  ///
  /// Returns list of goals to reactivate (from VariableEntry suspensions)
  List<GoalRef> bindImportedReader(int readerAddr, Term value, VariableEntry entry) {
    final cell = cells[readerAddr];
    if (cell.tag != CellTag.RoTag) {
      throw StateError('bindImportedReader called on non-reader cell at $readerAddr (tag: ${cell.tag})');
    }
    if (cell.content is! VariableEntry) {
      throw StateError('bindImportedReader called on reader without VariableEntry at $readerAddr');
    }

    final activations = <GoalRef>[];

    // Extract activations from VariableEntry suspensions (linked list)
    if (entry.suspensions != null) {
      _walkAndActivate(entry.suspensions!, activations);
    }

    // Allocate a value cell for the term and point reader to it
    // IMPORTANT: Use HP++ to keep HP in sync with cells.length
    final valueCellAddr = HP++;
    cells.add(HeapCell(value, CellTag.ValueTag));
    cell.content = Pointer(valueCellAddr);

    return activations;
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

  // ==========================================================================
  // Reader abstraction methods (work for local AND imported readers)
  // ==========================================================================

  /// Check if a reader is bound (local or imported)
  ///
  /// For local readers: checks if paired writer is fully bound
  /// For imported readers: checks if cell content is Pointer (bound by bindImportedReader)
  bool isReaderBound(int readerAddr) {
    final cell = cells[readerAddr];
    if (cell.tag != CellTag.RoTag) return false;

    if (cell.content is Pointer) {
      final targetAddr = (cell.content as Pointer).targetAddr;
      final targetCell = cells[targetAddr];
      if (targetCell.tag == CellTag.WrtTag) {
        // Local reader - check if writer is fully bound
        return isFullyBound(targetAddr);
      } else if (targetCell.tag == CellTag.ValueTag) {
        // Imported reader, bound via bindImportedReader
        return true;
      }
    }
    // VariableEntry = unbound imported reader
    return false;
  }

  /// Get value for a bound reader (local or imported)
  ///
  /// Returns null if reader is unbound
  Term? getReaderValue(int readerAddr) {
    final cell = cells[readerAddr];
    if (cell.tag != CellTag.RoTag) return null;

    if (cell.content is Pointer) {
      final targetAddr = (cell.content as Pointer).targetAddr;
      final targetCell = cells[targetAddr];
      if (targetCell.tag == CellTag.WrtTag) {
        // Local reader - get writer value
        return getValue(targetAddr);
      } else if (targetCell.tag == CellTag.ValueTag) {
        // Imported reader, bound via bindImportedReader - value is in the cell
        return targetCell.content as Term;
      }
    }
    return null;
  }

  /// Check if reader is an imported reader (no local writer)
  ///
  /// Returns true for both bound and unbound imported readers, identified by
  /// cell structure rather than V_p presence. This is intentional:
  ///
  /// **Semantics of "imported reader":**
  /// An imported reader is one that was received from another agent (creator != self).
  /// The heap structure permanently marks this:
  /// - Unbound imported reader: cell.content is VariableEntry (suspensions stored here)
  /// - Bound imported reader: cell.content is Pointer -> ValueTag cell
  ///
  /// **Contrast with local readers:**
  /// - Local readers have cell.content as Pointer -> RwTag cell (the paired writer)
  ///
  /// **Why this matters:**
  /// After bindImportedReader(), the VariableEntry is removed from V_p, but the
  /// heap structure (Pointer -> ValueTag) still identifies it as imported. This
  /// allows derefAddr() to correctly retrieve the bound value without needing
  /// V_p lookup.
  ///
  /// **Cell structure summary:**
  /// | State | cell.content | Target cell |
  /// |-------|--------------|-------------|
  /// | Unbound imported | VariableEntry | N/A |
  /// | Bound imported | Pointer | ValueTag |
  /// | Local (any) | Pointer | RwTag (writer) |
  bool isImportedReader(int readerAddr) {
    final cell = cells[readerAddr];
    if (cell.tag != CellTag.RoTag) return false;

    if (cell.content is VariableEntry) {
      // Unbound imported reader
      return true;
    }
    if (cell.content is Pointer) {
      // Could be local reader (points to writer) or bound imported reader (points to ValueTag)
      final targetAddr = (cell.content as Pointer).targetAddr;
      final targetCell = cells[targetAddr];
      // If target is ValueTag, it was bound via bindImportedReader
      return targetCell.tag == CellTag.ValueTag;
    }
    return false;
  }

  /// Get writer address for local reader, null for imported reader
  ///
  /// This is the safe version - use this instead of writerForReader when
  /// the reader might be imported
  int? getWriterForReader(int readerAddr) => tryWriterForReader(readerAddr);

  /// Legacy: Get suspension list (now on writer via WriterContent)
  SuspensionListNode? getSuspensions(int writerAddr) {
    final cell = cells[writerAddr];
    if (cell.content is WriterContent) {
      return (cell.content as WriterContent).suspensions;
    }
    return null;
  }

  /// Legacy: Add suspension (now on writer via WriterContent)
  void addSuspension(int writerAddr, SuspensionListNode node) {
    final cell = cells[writerAddr];
    if (cell.content is WriterContent) {
      final wc = cell.content as WriterContent;
      node.next = wc.suspensions;
      wc.suspensions = node;
    } else if (cell.content is Pointer) {
      final readerAddr = (cell.content as Pointer).targetAddr;
      cell.content = WriterContent(readerAddr, node);
    }
  }

  // ==========================================================================
  // Term Storage Helper (for Heap-Only Argument Registers per spec v2.16.3)
  // ==========================================================================

  /// Store a Term on the heap and return the cell address.
  ///
  /// Per spec Section 1.1 (Heap-Only Requirement):
  /// All data passed through argument registers MUST be heap-allocated.
  /// Direct ConstTerm and StructTerm objects are NOT permitted in CallEnv.
  ///
  /// This helper converts any Term to a heap-stored VarRef:
  /// - VarRef: already on heap, return the address
  /// - ConstTerm: allocate a ValueTag cell containing the constant
  /// - StructTerm: recursively store args, allocate ValueTag cell with VarRef args
  ///
  /// Returns the heap address suitable for use in CallEnv via VarRef(addr).
  int storeTermOnHeap(Term term) {
    if (term is VarRef) {
      // Already on heap
      return term.addr;
    }

    if (term is ConstTerm) {
      // Allocate a ValueTag cell containing the constant
      final addr = HP++;
      cells.add(HeapCell(term, CellTag.ValueTag));
      return addr;
    }

    if (term is StructTerm) {
      // Recursively store all args on heap, creating VarRef args
      final heapArgs = <Term>[];
      for (final arg in term.args) {
        final argAddr = storeTermOnHeap(arg);
        heapArgs.add(VarRef(argAddr));
      }
      // Allocate a ValueTag cell containing the StructTerm with VarRef args
      final addr = HP++;
      cells.add(HeapCell(StructTerm(term.functor, heapArgs), CellTag.ValueTag));
      return addr;
    }

    if (term is MutualRefTerm) {
      // MutualRefTerm contains a writer address for circular structures
      final addr = HP++;
      cells.add(HeapCell(term, CellTag.ValueTag));
      return addr;
    }

    if (term is ModuleTerm) {
      // ModuleTerm wraps a compiled module binary — stored as opaque value
      final addr = HP++;
      cells.add(HeapCell(term, CellTag.ValueTag));
      return addr;
    }

    throw ArgumentError('Unknown term type: ${term.runtimeType}');
  }
}
