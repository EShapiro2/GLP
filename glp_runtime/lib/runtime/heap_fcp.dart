/// FCP Two-Cell Heap with Pointer Architecture
///
/// The heap of IGLP app:in-heap (Heap: Variables, Dereferencing, Binding,
/// Suspension):
/// - A variable pair is a writer cell and a reader cell, each a tagged
///   reference; unbound, the two point to each other
/// - Suspensions live on writer cells, not reader cells
/// - A writer bound to a value becomes a value cell (ValueTag)
///
/// - A variable occurrence in a goal or term is the cell itself, a reference
///   and not an address: the heap is no array, and a cell lives while a goal,
///   a suspension, a wait or a table reaches it and is reclaimed by Dart's
///   collector when none does (IGLP app:in-heap, Variable pairs)
///
/// The names `addr`, `readerAddr`, `targetAddr` and `writerAddr` hold cells;
/// [HeapCell.id] is a cell's serial number, for display, messages and the
/// hash, and indexes nothing.
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

  /// The cell's serial number, from [HeapFCP.HP], in order of allocation.
  /// For display, messages and hashing; it indexes nothing.
  final int id;

  /// A writer's paired reader, recorded at allocation and kept when the writer
  /// binds, the writer's own pointer to it being overwritten then.  Null for a
  /// reader, a value cell and an imported writer.
  HeapCell? pairedReader;

  HeapCell(this.content, this.tag, this.id);

  /// A cell is itself and no other: identity, hashed by its [id].
  @override
  bool operator ==(Object other) => identical(this, other);

  @override
  int get hashCode => id;

  /// The cell's [id]: a message or a trace that interpolates a cell names it
  /// by its serial number.
  @override
  String toString() => '$id';

  bool get hasValue => tag == CellTag.ValueTag;
  bool get hasSuspensions => content is WriterContent && (content as WriterContent).suspensions != null;
}

/// Pointer to another cell (heap address)
class Pointer {
  final HeapCell targetAddr;

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
  final HeapCell readerAddr;  // Pointer to paired reader (preserved)
  SuspensionListNode? suspensions;

  WriterContent(this.readerAddr, [this.suspensions]);

  @override
  String toString() => 'WriterContent(reader=$readerAddr, sus=$suspensions)';
}

/// FCP Two-Cell Heap with Pointer-Based Variable Identity
/// 
/// Per IGLP app:in-heap:
/// - allocateVariable() returns (writerAddr, readerAddr) tuple
/// - Unbound, the writer and reader cells point to each other; a writer bound
///   to a reader holds a Pointer, extending the dereference chain
/// - Suspensions are stored on writer cells
/// The hops a dereference follows before it keeps the cells it passes to find
/// a cycle ([HeapFCP.derefAddr]).  A chain this short needs no set; a cycle,
/// which goes round for ever, is found within one lap of the set's start.
const int _derefShortChain = 16;

class HeapFCP {
  /// The serial counter: the [HeapCell.id] the next cell gets, and the number
  /// of cells allocated so far.  No cell is found by it.
  int HP = 0;

  /// Callbacks for external observation (Phase 0 I/O)
  /// Keyed by the writer cell
  final Map<HeapCell, void Function(Term)> _bindCallbacks = {};

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
  (HeapCell, HeapCell) allocateVariable() {
    final writer = HeapCell(null, CellTag.WrtTag, HP);
    final reader = HeapCell(Pointer(writer), CellTag.RoTag, HP + 1);
    HP += 2;

    // Writer cell: points TO reader (FCP pattern)
    writer.content = Pointer(reader);

    // Record the pair so the reader is recoverable after the writer binds.
    writer.pairedReader = reader;

    return (writer, reader);
  }

  /// Allocate a single reader cell for an imported variable (no local writer)
  ///
  /// Per irmaGLP spec, imported readers have no local paired writer.
  /// The cell content will be set to a VariableEntry by the caller.
  HeapCell allocateImportedReader() => HeapCell(null, CellTag.RoTag, HP++);

  /// Allocate a single writer cell for an imported variable (no local reader)
  ///
  /// Per irmaGLP spec, imported writers have no local paired reader.
  /// The cell content will be set to a VariableEntry by the caller.
  HeapCell allocateImportedWriter() => HeapCell(null, CellTag.WrtTag, HP++);

  /// A value cell holding [value].
  HeapCell allocateValue(Term value) =>
      HeapCell(value, CellTag.ValueTag, HP++);

  // ==========================================================================
  // Cell Type Checking
  // ==========================================================================

  /// Check if address is a writer cell
  bool isWriter(HeapCell addr) => addr.tag == CellTag.WrtTag;

  /// Check if address is a reader cell
  bool isReader(HeapCell addr) => addr.tag == CellTag.RoTag;

  /// Check if address is a value cell (bound to ground)
  bool isValue(HeapCell addr) => addr.tag == CellTag.ValueTag;

  // ==========================================================================
  // Pointer Navigation (Section 7 of spec)
  // ==========================================================================

  /// Get writer address from reader address by following pointer
  /// Try to get writer address from a reader address.
  ///
  /// Per spec Section 7.1: Follow the reader's pointer to get the writer.
  ///
  /// **Returns:**
  /// - the writer for local readers (cell.content is Pointer)
  /// - `null`: For imported readers (cell.content is VariableEntry) or non-readers
  HeapCell? tryWriterForReader(HeapCell readerAddr) {
    final cell = readerAddr;
    if (cell.tag != CellTag.RoTag) {
      return null;
    }
    final c = cell.content;
    if (c is Pointer) {
      return c.targetAddr;
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
  HeapCell? readerForWriter(HeapCell writerAddr) {
    final cell = writerAddr;
    if (cell.tag != CellTag.WrtTag) {
      return null;
    }
    final c = cell.content;

    // Case 1: Unbound without suspensions - direct Pointer to reader
    if (c is Pointer) {
      final target = c.targetAddr;
      // Verify it's the paired reader (points back to this writer)
      if (target.tag == CellTag.RoTag) {
        final readerContent = target.content;
        if (readerContent is Pointer && identical(readerContent.targetAddr, writerAddr)) {
          return target;  // Confirmed bidirectional - this is the paired reader
        }
      }
      // Writer is bound to something else, no direct reader access
      return null;
    }

    // Case 2: Unbound with suspensions - compound WriterContent preserves reader pointer
    if (c is WriterContent) {
      return c.readerAddr;
    }

    // Case 3: Bound or invalid - no reader access
    return null;
  }

  /// Get the paired reader address for a writer (works for bound and unbound).
  ///
  /// Recovered from [HeapCell.pairedReader] (recorded at allocation, survives
  /// binding) — no `reader = writer + 1` arithmetic (known-issues Issue 9 fix).
  /// Falls back to the bidirectional pointer for any writer without one;
  /// throws if neither yields a reader, rather than guessing.
  HeapCell pairedReaderAddr(HeapCell writerAddr) {
    final indexed = writerAddr.pairedReader;
    if (indexed != null) return indexed;

    final reader = readerForWriter(writerAddr);
    if (reader != null) return reader;

    throw StateError(
        'pairedReaderAddr: no recorded reader for writer @$writerAddr '
        '(not from allocateVariable, or no paired reader). The reader address '
        'must come from allocation, not arithmetic — see known-issues Issue 9.');
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
  Object derefAddr(HeapCell startAddr) {
    var current = startAddr;
    // The cells passed, for the cycle check: kept from the [_derefShortChain]th
    // hop on, a chain that long being the rare one.  Until 2026-10-02 every
    // dereference made the set, most of them following one or two pointers.
    Set<HeapCell>? visited;
    var hops = 0;
    HeapCell? previous;
    CellTag? previousTag;  // Track previous tag for WxW detection

    while (true) {
      if (++hops > _derefShortChain) {
        visited ??= <HeapCell>{};
        if (!visited.add(current)) {
          throw StateError('Cycle detected at address $current - SRSW violation!');
        }
      }

      final cell = current;

      // Per spec Section 4.5: WxW detection during deref
      // If we followed a pointer from a writer and landed on another writer, that's a violation
      if (previousTag == CellTag.WrtTag && cell.tag == CellTag.WrtTag) {
        throw StateError('SRSW violation: writer at $previous points to writer at $current');
      }

      final content = cell.content;
      switch (cell.tag) {
        case CellTag.RoTag:
          // Reader cell
          if (content is VariableEntry) {
            // Imported reader - check for cached bound value in entry
            if (content.boundValue != null) {
              return content.boundValue!;
            }
            return content;  // Unbound imported
          }
          if (content is Pointer) {
            // Follow pointer to writer
            previousTag = cell.tag;
            previous = current;
            current = content.targetAddr;
            continue;
          }
          throw StateError('Reader cell at $current has invalid content: $content');

        case CellTag.WrtTag:
          // Writer cell
          if (content is VariableEntry) {
            // Imported writer - check for cached bound value in entry
            if (content.boundValue != null) {
              return content.boundValue!;
            }
            return content;  // Unbound imported
          }
          // Case 1: WriterContent - unbound with suspensions (FCP pattern)
          if (content is WriterContent) {
            // Unbound writer with suspensions - return VarRef to this address
            return VarRef(current);
          }
          // Case 2: Pointer - check if bidirectional (unbound) or chain (bound)
          if (content is Pointer) {
            final target = content.targetAddr;
            // Check if pointer is to paired reader (unbound) or to bound value
            if (target.tag == CellTag.RoTag) {
              final readerContent = target.content;
              if (readerContent is Pointer && identical(readerContent.targetAddr, current)) {
                // Bidirectional - points to paired reader which points back
                // This is an unbound variable
                return VarRef(current);
              }
            }
            // Bound to another cell - follow the pointer
            previousTag = cell.tag;
            previous = current;
            current = target;
            continue;
          }
          throw StateError('Writer cell at $current has invalid content: $content');

        case CellTag.ValueTag:
          // Bound to ground value
          return content as Term;
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
  List<GoalRef> bindWriter(HeapCell writerAddr, Term value) {
    return bindWriterWithCallbackControl(writerAddr, value, fireCallback: true);
  }

  /// Bind a writer without firing callbacks
  ///
  /// Used by applySigmaHatFCP to defer callbacks until all bindings complete.
  /// This ensures nested VarRefs in structures can be dereferenced correctly.
  List<GoalRef> bindWriterNoCallback(HeapCell writerAddr, Term value) {
    return bindWriterWithCallbackControl(writerAddr, value, fireCallback: false);
  }

  /// Internal: bind with callback control
  List<GoalRef> bindWriterWithCallbackControl(HeapCell writerAddr, Term value, {required bool fireCallback}) {
    final cell = writerAddr;
    if (cell.tag != CellTag.WrtTag) {
      throw StateError('bindWriter called on non-writer cell at $writerAddr (tag: ${cell.tag})');
    }

    final activations = <GoalRef>[];

    // Save and process suspensions before overwriting (FCP pattern: check WriterContent)
    final c = cell.content;
    if (c is WriterContent) {
      _walkAndActivate(c.suspensions, activations);
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

  /// Fire every registered callback whose writer has become bound.
  ///
  /// An observer registered with [onBind] is fired by the bind that binds its
  /// writer. A writer can also become bound without such a call: a commit
  /// binds a writer to a reader, leaving a chain, and the chain is resolved to
  /// a value by rewriting cells (`applySigmaHatFCP`). Nothing fires the
  /// observers on the cells so rewritten, and they are never fired again.
  ///
  /// The observer that matters is a `global_send` goal, which the madGLP
  /// specification puts in the resolvent (Definition global_send) and which
  /// this runtime realises as a callback instead: as a goal it would be woken
  /// by the ordinary suspension machinery, which follows chains, and as a
  /// callback it is not. Calling this at the end of a commit — where suspended
  /// goals are activated — is that waking. Until 2026-09-08 it was absent, and
  /// a value written into the tail of a stream whose reader had already
  /// crossed a link was produced and never sent (SGSG's linkprobe10 and 12,
  /// and with them the warm call of their Section 5.2).
  ///
  /// The map holds one entry per open global link, so the pass is over a few
  /// entries and each fires at most once.
  void fireBoundCallbacks() {
    if (_bindCallbacks.isEmpty) return;
    for (final addr in _bindCallbacks.keys.toList()) {
      if (!isFullyBound(addr)) continue;
      final callback = _bindCallbacks.remove(addr);
      final value = getValue(addr);
      if (callback != null && value != null) {
        callback(value);
      }
    }
  }

  /// Fire pending callback for a writer (if any)
  ///
  /// Used after all bindings complete to fire deferred callbacks.
  void firePendingCallback(HeapCell writerAddr) {
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
  /// Per spec Section 5.3 (v3.5):
  /// - Stores Pointer(readerAddr) in writer cell
  /// - Suspensions go where dereferencing the reader leads: forwarded to the
  ///   final unbound writer of the chain; activated when the chain already
  ///   ends in a bound value (the binding determines the value now); stored
  ///   in the VariableEntry for an imported unbound writer
  /// - Tag remains WrtTag (not bound to ground)
  ///
  /// Returns list of goals to reactivate (non-empty iff the chain already
  /// ends in a bound value)
  List<GoalRef> bindWriterToReader(HeapCell writerAddr, HeapCell readerAddr) {
    final writerCell = writerAddr;
    if (writerCell.tag != CellTag.WrtTag) {
      throw StateError('bindWriterToReader called on non-writer at $writerAddr');
    }

    final readerCell = readerAddr;
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

    // Where the reader's dereference chain ends: an unbound local writer
    // (VarRef), an unbound imported writer (VariableEntry), or a bound value.
    final chainEnd = derefAddr(readerAddr);

    // Route this writer's suspensions to wherever the chain leads
    // (spec Section 5.3 v3.5). One-hop forwarding is wrong on both ends: the
    // one-hop writer may itself be chain-bound onward, and a chain that
    // already ends in a value must activate, not forward.
    final wcontent = writerCell.content;
    if (wcontent is WriterContent) {
      final wc = wcontent;
      if (chainEnd is VarRef) {
        // Chain ends at an unbound local writer — move suspensions there
        _forwardSuspensions(wc.suspensions, chainEnd.addr);
      } else if (chainEnd is VariableEntry) {
        // Chain ends at an unbound imported writer — suspensions wait in its
        // entry (as in suspendOnReader for imported readers)
        var current = wc.suspensions;
        while (current != null) {
          if (current.armed) {
            final newNode = SuspensionListNode(current.record);
            newNode.next = chainEnd.suspensions;
            chainEnd.suspensions = newNode;
          }
          current = current.next;
        }
      } else {
        // Chain already ends in a bound value — this binding determines the
        // suspended goals' variable; activate them
        _walkAndActivate(wc.suspensions, activations);
      }
    }

    // Store pointer to reader (creates variable chain)
    writerCell.content = Pointer(readerAddr);
    // Tag remains WrtTag

    // An external callback follows the chain like the suspensions do
    final callback = _bindCallbacks.remove(writerAddr);
    if (callback != null) {
      if (chainEnd is VarRef) {
        _bindCallbacks[chainEnd.addr] = callback;
      } else if (chainEnd is VariableEntry) {
        _bindCallbacks[targetWriterAddr] = callback;
      } else {
        callback(chainEnd as Term);
      }
    }

    return activations;
  }

  /// Bind writer to writer (WxW violation)
  ///
  /// Per spec Section 5.2: This is forbidden and should throw
  void bindWriterToWriter(HeapCell w1, HeapCell w2) {
    throw StateError('WxW violation: cannot bind writer $w1 to writer $w2');
  }

  // ==========================================================================
  // Suspension (Section 6 of spec)
  // ==========================================================================

  /// Add a suspension to a writer cell
  ///
  /// Per spec v3.2 Section 6.1: Suspensions are stored on writer cells using
  /// WriterContent to preserve the reader pointer.
  void suspendOnWriter(HeapCell writerAddr, SuspensionRecord record) {
    final cell = writerAddr;
    if (cell.tag != CellTag.WrtTag) {
      throw StateError('suspendOnWriter called on non-writer at $writerAddr');
    }

    final node = SuspensionListNode(record);

    // FCP pattern: preserve reader pointer using WriterContent
    final c = cell.content;
    if (c is WriterContent) {
      // Already has WriterContent - add to suspension list
      node.next = c.suspensions;
      c.suspensions = node;
    } else if (c is Pointer) {
      // First suspension: convert Pointer to WriterContent
      cell.content = WriterContent(c.targetAddr, node);
    } else {
      throw StateError('suspendOnWriter: unexpected content $c at $writerAddr');
    }
  }

  /// Add a suspension via a reader (finds writer and adds there)
  ///
  /// Per spec Section 6.1: Find the reader's writer and add suspension there
  void suspendOnReader(HeapCell readerAddr, SuspensionRecord record) {
    final cell = readerAddr;
    final c = cell.content;

    if (c is VariableEntry) {
      // Imported reader - store suspension in VariableEntry.suspensions
      // Per spec Section 3.1.2: For imported readers, V_p serves as the
      // "virtual writer" that holds suspensions. When an assignment arrives,
      // goals are resumed from VariableEntry.suspensions.
      final node = SuspensionListNode(record);
      node.next = c.suspensions;
      c.suspensions = node;
      return;
    }

    if (cell.tag != CellTag.RoTag || c is! Pointer) {
      throw StateError('suspendOnReader called on invalid reader at $readerAddr');
    }

    suspendOnWriter(c.targetAddr, record);
  }

  /// Forward suspensions from one writer to another
  ///
  /// Per spec v3.2: Target writer uses WriterContent to preserve reader pointer.
  void _forwardSuspensions(SuspensionListNode? list, HeapCell targetWriterAddr) {
    var current = list;
    while (current != null) {
      if (current.armed) {
        // Create new node sharing the same record
        final newNode = SuspensionListNode(current.record);
        final targetCell = targetWriterAddr;
        final tc = targetCell.content;

        if (tc is WriterContent) {
          // Target already has WriterContent - add to its suspension list
          newNode.next = tc.suspensions;
          tc.suspensions = newNode;
        } else if (tc is Pointer) {
          // Target is unbound with no suspensions - create WriterContent
          targetCell.content = WriterContent(tc.targetAddr, newNode);
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
  bool isFullyBound(HeapCell writerAddr) {
    final result = derefAddr(writerAddr);
    return result is! VarRef && result is! VariableEntry;
  }

  /// Get variable value (dereferenced)
  ///
  /// Returns null if unbound
  Term? getValue(HeapCell writerAddr) {
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
  void onBind(HeapCell writerAddr, void Function(Term) callback) {
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
  void removeBindCallback(HeapCell writerAddr) {
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
  /// readerAddr = HeapCell(VariableEntry(...), CellTag.RoTag)
  /// ```
  ///
  /// AFTER (bound imported reader):
  /// ```
  /// readerAddr = HeapCell(Pointer(valueCell), CellTag.RoTag)
  /// valueCell = HeapCell(value, CellTag.ValueTag)
  /// ```
  ///
  /// Note: Unlike local readers (which point to their paired writer), imported
  /// readers point directly to a ValueTag cell. This distinction is used by
  /// isImportedReader() to detect bound imported readers.
  ///
  /// Returns list of goals to reactivate (from VariableEntry suspensions)
  List<GoalRef> bindImportedReader(HeapCell readerAddr, Term value, VariableEntry entry) {
    final cell = readerAddr;
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
    final valueCell = allocateValue(value);
    cell.content = Pointer(valueCell);

    return activations;
  }

  // ==========================================================================
  // Compatibility Methods (for gradual migration of callers)
  // ==========================================================================

  /// Bind variable to a term (compatibility wrapper)
  List<GoalRef> bindVariable(HeapCell writerAddr, Term value) {
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
  List<GoalRef> bindVariableConst(HeapCell writerAddr, Object? v) {
    return bindWriter(writerAddr, ConstTerm(v));
  }

  /// Bind variable to structure
  List<GoalRef> bindVariableStruct(HeapCell writerAddr, String functor, List<Term> args) {
    return bindWriter(writerAddr, StructTerm(functor, args));
  }

  /// Compatibility: isWriterBound
  bool isWriterBound(HeapCell writerAddr) => isFullyBound(writerAddr);

  /// Compatibility: valueOfWriter
  Term? valueOfWriter(HeapCell writerAddr) => getValue(writerAddr);

  /// Compatibility: bindWriterConst
  List<GoalRef> bindWriterConst(HeapCell writerAddr, Object? v) => bindVariableConst(writerAddr, v);

  /// Compatibility: bindWriterStruct
  List<GoalRef> bindWriterStruct(HeapCell writerAddr, String f, List<Term> args) {
    return bindVariableStruct(writerAddr, f, args);
  }

  /// Compatibility: isBound
  bool isBound(HeapCell varId) => isFullyBound(varId);

  // ==========================================================================
  // Reader abstraction methods (work for local AND imported readers)
  // ==========================================================================

  /// Check if a reader is bound (local or imported)
  ///
  /// For local readers: checks if paired writer is fully bound
  /// For imported readers: checks if cell content is Pointer (bound by bindImportedReader)
  bool isReaderBound(HeapCell readerAddr) {
    final cell = readerAddr;
    if (cell.tag != CellTag.RoTag) return false;

    final c = cell.content;
    if (c is Pointer) {
      final targetCell = c.targetAddr;
      if (targetCell.tag == CellTag.WrtTag) {
        // Local reader - check if writer is fully bound
        return isFullyBound(targetCell);
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
  Term? getReaderValue(HeapCell readerAddr) {
    final cell = readerAddr;
    if (cell.tag != CellTag.RoTag) return null;

    final c = cell.content;
    if (c is Pointer) {
      final targetCell = c.targetAddr;
      if (targetCell.tag == CellTag.WrtTag) {
        // Local reader - get writer value
        return getValue(targetCell);
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
  /// cell structure rather than V_p presence:
  /// | State | cell.content | Target cell |
  /// |-------|--------------|-------------|
  /// | Unbound imported | VariableEntry | N/A |
  /// | Bound imported | Pointer | ValueTag |
  /// | Local (any) | Pointer | RwTag (writer) |
  bool isImportedReader(HeapCell readerAddr) {
    final cell = readerAddr;
    if (cell.tag != CellTag.RoTag) return false;

    final c = cell.content;
    if (c is VariableEntry) {
      // Unbound imported reader
      return true;
    }
    if (c is Pointer) {
      // Could be local reader (points to writer) or bound imported reader (points to ValueTag)
      // If target is ValueTag, it was bound via bindImportedReader
      return c.targetAddr.tag == CellTag.ValueTag;
    }
    return false;
  }

  /// Get writer address for local reader, null for imported reader
  ///
  /// This is the safe version - use this instead of writerForReader when
  /// the reader might be imported
  HeapCell? getWriterForReader(HeapCell readerAddr) => tryWriterForReader(readerAddr);

  /// Legacy: Get suspension list (now on writer via WriterContent)
  SuspensionListNode? getSuspensions(HeapCell writerAddr) {
    final c = writerAddr.content;
    if (c is WriterContent) {
      return c.suspensions;
    }
    return null;
  }

  /// Legacy: Add suspension (now on writer via WriterContent)
  void addSuspension(HeapCell writerAddr, SuspensionListNode node) {
    final cell = writerAddr;
    final c = cell.content;
    if (c is WriterContent) {
      node.next = c.suspensions;
      c.suspensions = node;
    } else if (c is Pointer) {
      cell.content = WriterContent(c.targetAddr, node);
    }
  }

  // ==========================================================================
  // Term Storage Helper (for Heap-Only Argument Registers per spec v2.16.3)
  // ==========================================================================

  /// Store a Term on the heap and return the cell holding it.
  ///
  /// Per spec Section 1.1 (Heap-Only Requirement):
  /// All data passed through argument registers MUST be heap-allocated.
  /// Direct ConstTerm and StructTerm objects are NOT permitted in CallEnv.
  ///
  /// This helper converts any Term to a heap-stored VarRef:
  /// - VarRef: already on heap, return its cell
  /// - ConstTerm: allocate a ValueTag cell containing the constant
  /// - StructTerm: store its args, then allocate a ValueTag cell with VarRef
  ///   args
  ///
  /// The walk keeps a stack of its own, a frame for each structure being
  /// stored, and allocates in the order the recursion it replaces did: each
  /// argument's cells, left to right, before its structure's own cell.  Until
  /// 2026-10-02 it recursed once a structure argument, and a list of 50,000
  /// elements overflowed the Dart stack (long_list_walks_test; GLP #3 Cowork,
  /// 2026-10-02 20:58 UTC, answering Integration's 19:05 UTC Q3:
  /// "storeTermOnHeap and termToWire walk with a stack of their own").
  ///
  /// Returns the cell, suitable for use in CallEnv via VarRef(addr).
  HeapCell storeTermOnHeap(Term term) {
    if (term is! StructTerm) return _storeLeafOnHeap(term);
    // Each frame: a structure, and the cells of its arguments stored so far,
    // as VarRefs.
    final frames = <(StructTerm, List<Term>)>[(term, <Term>[])];
    HeapCell? stored; // the cell of the structure just stored, for its parent
    while (true) {
      final (source, heapArgs) = frames.last;
      if (stored != null) {
        heapArgs.add(VarRef(stored));
        stored = null;
      }
      if (heapArgs.length == source.args.length) {
        frames.removeLast();
        // Allocate a ValueTag cell containing the StructTerm with VarRef args
        final cell = allocateValue(StructTerm(source.functor, heapArgs));
        if (frames.isEmpty) return cell;
        stored = cell;
        continue;
      }
      final arg = source.args[heapArgs.length];
      if (arg is StructTerm) {
        frames.add((arg, <Term>[]));
      } else {
        heapArgs.add(VarRef(_storeLeafOnHeap(arg)));
      }
    }
  }

  /// [storeTermOnHeap] of a term that is not a structure.
  HeapCell _storeLeafOnHeap(Term term) {
    if (term is VarRef) {
      // Already on heap
      return term.addr;
    }

    if (term is ConstTerm) {
      // Allocate a ValueTag cell containing the constant
      return allocateValue(term);
    }

    if (term is MutualRefTerm) {
      // MutualRefTerm contains a writer address for circular structures
      return allocateValue(term);
    }

    if (term is ModuleTerm) {
      // ModuleTerm wraps a compiled module binary — stored as opaque value
      return allocateValue(term);
    }

    throw ArgumentError('Unknown term type: ${term.runtimeType}');
  }
}
