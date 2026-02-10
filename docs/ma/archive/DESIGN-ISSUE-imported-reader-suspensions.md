# Design Issue: Suspension Storage for Imported Readers

**Date:** 2026-01-19  
**Status:** RESOLVED  
**Severity:** Fundamental design question

---

## Resolution

VariableEntry in V_p serves as the "virtual writer" for imported readers. It holds both protocol bookkeeping (creator, creatorLocalId, state) AND the local suspension list. This is the natural choice because V_p is already the local representation of the remote variable.

When a goal suspends on an imported reader, the suspension is added to the VariableEntry's suspension list. When an assignment message arrives, the runtime resumes goals from that list.

**Spec updated**: irmaGLP-spec.md Section 3.1.2 now defines V_p as containing 4-tuples (Y, q, s, Σ) where Σ is the suspension list.

---

## Problem Statement

In standard single-agent GLP, when a goal suspends on a reader, the suspension list is stored in the writer cell. This works because reader and writer share the same underlying two-cell structure, with the writer cell's `content` field holding either a Pointer (to the reader) or a SuspensionListNode.

For imported readers, there is no local writer. The cell's `content` field is used to store the VariableEntry reference for V_p lookup. This creates a conflict: the `content` field cannot simultaneously hold both a VariableEntry and a SuspensionListNode.

---

## Current Implementation

From `heap_fcp.dart`:

```dart
class HeapCell {
  dynamic content;  // Pointer | SuspensionListNode | Term
  CellTag tag;
}
```

The `content` field is overloaded to hold different types depending on state. For imported variables, we set `content` to a VariableEntry (per Phase 6 implementation). But if a goal suspends on this imported reader, where does the suspension go?

---

## The Question

Where should the suspension list for an imported reader be stored?

---

## Possible Approaches

### Option 1: VariableEntry holds suspensions

The VariableEntry (in V_p) could have a field for local suspensions. When a goal suspends on an imported reader, the suspension is added to the VariableEntry's suspension list.

**Pros:** Keeps V_p as the single source of truth for imported variable state.

**Cons:** Mixes heap-level concerns (suspensions) with protocol-level concerns (V_p). VariableEntry is currently a protocol/bookkeeping structure, not a runtime execution structure.

### Option 2: HeapCell gets a separate suspensions field

Add a dedicated `suspensions` field to HeapCell, separate from `content`:

```dart
class HeapCell {
  dynamic content;  // Pointer | VariableEntry | Term
  CellTag tag;
  SuspensionListNode? suspensions;  // New field
}
```

**Pros:** Clean separation. `content` holds the cell's "value" (pointer, entry, or term), `suspensions` holds waiting goals. Works uniformly for all cell types.

**Cons:** Changes the fundamental HeapCell structure. May have ripple effects throughout the runtime.

### Option 3: Separate suspension map keyed by address

Maintain a separate `Map<int, SuspensionListNode>` in the heap or machine state, keyed by cell address.

**Pros:** No change to HeapCell structure. Suspensions looked up by address when needed.

**Cons:** Indirection. Must keep map in sync with cell lifecycle. Two places to look for suspension information.

### Option 4: Current design is broken

If none of the above are acceptable, the Phase 6 single-cell design for imported variables may need to be reconsidered.

---

## Implementation Required

1. Add a `suspensions` field to VariableEntry (in `variable_table.dart`)
2. When suspending on an imported reader, add suspension to the VariableEntry's list (not the heap cell)
3. When processing an assignment message, resume goals from the VariableEntry's suspension list

---

## Related Files

- `glp_runtime/lib/runtime/heap_fcp.dart` - HeapCell definition
- `glp_runtime/lib/runtime/suspension.dart` - SuspensionListNode
- `glp_runtime/lib/multiagent/variable_table.dart` - VariableEntry
- `glp_runtime/lib/multiagent/irma_context.dart` - Imported variable handling
