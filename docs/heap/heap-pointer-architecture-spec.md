# GLP Heap Storage Specification - Pointer Architecture

**Version**: 3.2
**Date**: 2026-01-31
**Status**: DRAFT - FCP bidirectional pointers
**Branch**: pointer-architecture

This document specifies the pointer-based variable representation for GLP, replacing the arithmetic-based address scheme in v2.18. The design follows the original FCP implementation.

**Reference**: FCP source at `/Users/udi/Dropbox/Concurrent Prolog/FCP/Merged EMULATOR/` (see `DISCIPLINE.md` Section 1.11)

---

## 1. Overview

### 1.1 Design Principles

A **reference** is a tagged pointer to a heap object. The tag identifies the type (reader, writer, value, etc.), and the pointer indicates where to find the content.

Heap navigation follows pointers explicitly rather than computing addresses via arithmetic. There is no implicit relationship between adjacent heap addresses.

### 1.2 Key Differences from v2.18

| Aspect | v2.18 (Arithmetic) | v3.2 (Pointer/FCP) |
|--------|-------------------|----------------|
| Reader→Writer relationship | Implicit: reader at writerAddr+1 | Explicit: reader contains pointer to writer |
| Writer→Reader relationship | Implicit: writer at readerAddr-1 | Explicit: writer contains pointer to reader (when unbound) |
| Writer content (unbound) | Self-pointer or null | Pointer to paired reader |
| VarRef structure | `{varId, isReader}` | Single address; tag determines role |
| Finding paired cell | Arithmetic: addr ± 1 | Follow pointer (both directions) |

**FCP Evidence**: In `kernels.c:522-526` and `emulate.c:114-117`, FCP allocates variable pairs with bidirectional pointers:
```c
*HP = Var_Word((HP+1), WrtTag);   // Writer points to reader
HP++;
*HP = Var_Word((HP-1), RoTag);    // Reader points to writer
```

---

## 2. Cell Structure

### 2.1 Tags

```dart
enum CellTag {
  WrtTag,    // Writer cell (unbound or bound)
  RoTag,     // Reader cell (read-only view)
  ValueTag,  // Bound to ground value (optimization)
}
```

### 2.2 Cell Definition

```dart
class HeapCell {
  dynamic content;  // See content rules below
  CellTag tag;
}
```

### 2.3 Content Rules by Tag

**WrtTag (Writer Cell)**:
- `Pointer(readerAddr)` — unbound, no suspensions, points to paired reader (FCP pattern)
- `WriterContent(Pointer(readerAddr), SuspensionListNode)` — unbound with suspensions, preserves reader pointer
- `Pointer(valueAddr)` — bound to value at addr (or transitively to another variable via reader)

**RoTag (Reader Cell)**:
- `Pointer(writerAddr)` — points to paired writer (always)
- `Pointer(valueAddr)` — after path compression, may point directly to value

**ValueTag (Ground Value)**:
- `Term` — the bound ground term (ConstTerm or StructTerm)

**FCP Pattern**: Both cells point to each other. This enables navigation in both directions without address arithmetic. When suspensions are added to an unbound writer, the reader pointer is preserved in a compound `WriterContent` structure.

---

## 3. Variable Allocation

### 3.1 Local Variable (Writer/Reader Pair)

```dart
/// Allocate a fresh local variable.
/// Returns (writerAddr, readerAddr).
///
/// FCP pattern: Both cells point to each other (bidirectional).
(int, int) allocateVariable() {
  final writerAddr = HP;
  final readerAddr = HP + 1;
  HP += 2;

  // Writer cell: points to its reader (FCP pattern)
  cells.add(HeapCell(Pointer(readerAddr), CellTag.WrtTag));

  // Reader cell: points to its writer
  cells.add(HeapCell(Pointer(writerAddr), CellTag.RoTag));

  return (writerAddr, readerAddr);
}
```

**Key point (FCP pattern)**: Both cells point to each other:
- Reader points TO the writer (for dereferencing to find value)
- Writer points TO the reader (for finding paired reader without arithmetic)

This enables `readerForWriter(writerAddr)` to simply follow the pointer, eliminating all `+1` arithmetic.

### 3.2 Variable Reference

```dart
/// A variable reference is simply a heap address.
/// The cell's tag determines whether it's a reader or writer.
class VarRef extends Term {
  final int addr;
  VarRef(this.addr);
}
```

To determine the role:

```dart
bool isWriter(int addr) => cells[addr].tag == CellTag.WrtTag;
bool isReader(int addr) => cells[addr].tag == CellTag.RoTag;
```

---

## 4. Dereferencing

### 4.1 Definition

Dereferencing is the act of following a chain of references until reaching the final object which is not a reference. As part of dereferencing, the initial reference is updated to point directly to the final object (path compression). This is integral to the design, not an optional optimization.

### 4.2 Algorithm

```dart
/// Dereference an address to its final value.
/// Updates the starting cell to point directly to the final target (path compression).
/// Returns: Term (bound) | VarRef (unbound local writer) | VariableEntry (unbound imported reader)
Object derefAddr(int startAddr) {
  // Phase 1: Follow chain to find final target
  var current = startAddr;
  int? finalAddr;  // Address of final target (if not a value)
  Object? finalValue;  // Final value (if ground term)

  while (true) {
    final cell = cells[current];

    switch (cell.tag) {
      case CellTag.RoTag:
        // Reader: follow pointer to writer, or return VariableEntry for imported readers
        if (cell.content is Pointer) {
          current = (cell.content as Pointer).targetAddr;
          continue;
        }
        if (cell.content is VariableEntry) {
          // Imported reader - no local writer to follow
          // Return the VariableEntry; caller treats as "unbound"
          return cell.content;
        }
        throw StateError('Reader cell has invalid content: ${cell.content}');

      case CellTag.WrtTag:
        // Case 1: Unbound without suspensions - pointer to paired reader
        if (cell.content is Pointer) {
          final target = (cell.content as Pointer).targetAddr;
          // Check if pointer is to paired reader (unbound) or to bound value
          if (cells[target].tag == CellTag.RoTag &&
              cells[target].content is Pointer &&
              (cells[target].content as Pointer).targetAddr == current) {
            // Points to paired reader which points back - this is unbound
            finalAddr = current;
            finalValue = VarRef(current);
            break;
          }
          // Bound to another cell - follow
          current = target;
          continue;
        }
        // Case 2: Unbound with suspensions - compound content
        if (cell.content is WriterContent) {
          finalAddr = current;
          finalValue = VarRef(current);
          break;
        }
        throw StateError('Writer cell has invalid content: ${cell.content}');

      case CellTag.ValueTag:
        // Ground value - this is the final target
        finalValue = cell.content as Term;
        finalAddr = current;
        break;
    }
    break;  // Exit loop when final target found
  }

  // Phase 2: Path compression - update starting cell to point to final target
  if (startAddr != finalAddr) {
    final startCell = cells[startAddr];
    if (startCell.tag == CellTag.RoTag) {
      // Reader: update pointer to final target
      startCell.content = Pointer(finalAddr!);
    }
    // Note: Writers are not updated during read-only dereference
    // Writer compression happens during binding
  }

  return finalValue!;
}
```

### 4.3 Path Compression Semantics

Path compression updates references to point directly to the final target, bypassing intermediate cells. This ensures that repeated dereferences of the same variable are O(1) after the first access.

Compression is applied to the starting cell only. Full chain compression (updating all intermediate cells) may be implemented as a further optimization but is not required by this specification.

### 4.4 Implementation Staging

For implementation purposes, path compression may be deferred as a final step after the basic pointer-following logic is working correctly. The implementation stages are:

1. **Stage 1**: Implement pointer-following without compression (dereference is read-only)
2. **Stage 2**: Add path compression to complete the design

This staging is for implementation convenience only. The final implementation must include path compression as specified.

### 4.5 Invariant Check

During dereferencing, if we follow a pointer and land on a writer, the previous cell MUST have been a reader. This is because writer-to-writer bindings are forbidden. In debug mode, verify this invariant:

```dart
// In deref loop, after following a pointer:
if (cells[current].tag == CellTag.WrtTag && previousTag == CellTag.WrtTag) {
  throw StateError('SRSW violation: writer points to writer');
}
```

---

## 5. Binding

### 5.1 Binding a Writer to a Value

```dart
/// Bind writer at writerAddr to the given term.
/// Returns list of goals to reactivate.
List<GoalRef> bindWriter(int writerAddr, Term value) {
  final cell = cells[writerAddr];
  assert(cell.tag == CellTag.WrtTag, 'Can only bind writers');
  
  final activations = <GoalRef>[];
  
  // 1. Save suspension list (if any)
  if (cell.content is SuspensionListNode) {
    _walkAndActivate(cell.content as SuspensionListNode, activations);
  }
  
  // 2. Determine what to store
  if (value is VarRef) {
    // Binding to another variable - store pointer
    cell.content = Pointer(value.addr);
    // Tag remains WrtTag (bound to variable, not ground)
  } else {
    // Binding to ground value
    cell.content = value;
    cell.tag = CellTag.ValueTag;
  }
  
  return activations;
}
```

### 5.2 Writer-to-Writer Binding (Forbidden)

If binding would create a writer → writer chain, fail immediately:

```dart
if (value is VarRef && isWriter(value.addr)) {
  throw StateError('WxW violation: cannot bind writer to writer');
}
```

### 5.3 Binding to Another Variable (Variable-to-Variable)

When a writer W is bound to a reader R:
1. W's content becomes `Pointer(R.addr)`
2. Any suspensions on W are forwarded to R's writer

```dart
List<GoalRef> bindWriterToReader(int writerAddr, int readerAddr) {
  final writerCell = cells[writerAddr];
  final activations = <GoalRef>[];
  
  // Forward suspensions to target writer
  if (writerCell.content is SuspensionListNode) {
    final targetWriter = findWriter(readerAddr);  // Follow reader's pointer
    _forwardSuspensions(writerCell.content, targetWriter);
  }
  
  // Update writer to point to reader
  writerCell.content = Pointer(readerAddr);
  
  return activations;  // No activations yet - goals wait for target
}
```

---

## 6. Suspension

### 6.1 Adding a Suspension

When a goal suspends on a reader, find the reader's writer and add the suspension there:

```dart
void suspendOnReader(int readerAddr, SuspensionRecord record) {
  // Follow reader's pointer to find writer
  final cell = cells[readerAddr];
  assert(cell.tag == CellTag.RoTag);

  final writerAddr = (cell.content as Pointer).targetAddr;
  final writerCell = cells[writerAddr];

  // Create suspension node
  final node = SuspensionListNode(record);

  // Add to writer's suspension list, preserving reader pointer
  if (writerCell.content is WriterContent) {
    // Compound content: (readerPointer, suspensionList)
    final content = writerCell.content as WriterContent;
    node.next = content.suspensions;
    content.suspensions = node;
  } else if (writerCell.content is Pointer) {
    // First suspension: convert to compound content
    final readerPtr = writerCell.content as Pointer;
    writerCell.content = WriterContent(readerPtr, node);
  }
}

/// Compound content for unbound writer with suspensions
class WriterContent {
  final Pointer readerPointer;  // Preserved pointer to paired reader
  SuspensionListNode? suspensions;

  WriterContent(this.readerPointer, this.suspensions);
}
```

**Note**: The writer must preserve its pointer to the paired reader even when suspensions are added. This enables `readerForWriter()` to work at any time.

### 6.2 Suspension List Structure

```dart
class SuspensionRecord {
  int? goalId;        // null if disarmed
  final int resumePC;
  
  void disarm() => goalId = null;
  bool get armed => goalId != null;
}

class SuspensionListNode {
  final SuspensionRecord record;
  SuspensionListNode? next;
  
  SuspensionListNode(this.record);
}
```

### 6.3 Activating Suspensions

When a writer is bound to a ground value:

```dart
void _walkAndActivate(SuspensionListNode? list, List<GoalRef> activations) {
  var current = list;
  while (current != null) {
    if (current.armed) {
      activations.add(GoalRef(current.goalId!, current.resumePC));
      current.record.disarm();
    }
    current = current.next;
  }
}
```

### 6.4 Forwarding Suspensions

When a writer W1 is bound to another variable (via reader R2), forward W1's suspensions to R2's writer W2:

```dart
void _forwardSuspensions(SuspensionListNode? list, int targetWriterAddr) {
  final targetCell = cells[targetWriterAddr];
  var current = list;
  
  while (current != null) {
    if (current.armed) {
      // Create new node sharing the same record
      final newNode = SuspensionListNode(current.record);
      newNode.next = targetCell.content is SuspensionListNode 
          ? targetCell.content as SuspensionListNode 
          : null;
      targetCell.content = newNode;
    }
    current = current.next;
  }
}
```

---

## 7. Finding the Paired Cell

### 7.1 Reader → Writer

Follow the reader's pointer:

```dart
int writerForReader(int readerAddr) {
  final cell = cells[readerAddr];
  assert(cell.tag == CellTag.RoTag);
  return (cell.content as Pointer).targetAddr;
}
```

### 7.2 Writer → Reader (FCP Pattern)

Follow the writer's pointer to find its paired reader:

```dart
int? readerForWriter(int writerAddr) {
  final cell = cells[writerAddr];
  assert(cell.tag == CellTag.WrtTag);

  // Case 1: Unbound without suspensions - direct pointer to reader
  if (cell.content is Pointer) {
    final target = (cell.content as Pointer).targetAddr;
    // Verify it's the paired reader (points back to this writer)
    if (cells[target].tag == CellTag.RoTag) {
      return target;
    }
    // Writer is bound to something else, no direct reader access
    return null;
  }

  // Case 2: Unbound with suspensions - compound content preserves reader pointer
  if (cell.content is WriterContent) {
    return (cell.content as WriterContent).readerPointer.targetAddr;
  }

  // Case 3: Bound or invalid - no reader access
  return null;
}
```

**FCP Pattern**: The writer points to its paired reader. No address arithmetic (`+1`) is ever needed.

**Bound writers**: Once a writer is bound to a value (not its paired reader), the pointer changes. The paired reader can still be found by following the reader→writer→... chain backward, but this is rarely needed.

**Suspension handling**: When suspensions are added to an unbound writer, the reader pointer is preserved in a `WriterContent` compound structure (see Section 6.1).

---

## 8. Heap Diagram

### 8.1 Unbound Variable (FCP Pattern)

```
+-------+-------+
| WrtTag| Ptr(1)|  ← Writer (addr 0): points to reader (FCP pattern)
+-------+-------+
| RoTag | Ptr(0)|  ← Reader (addr 1): points to writer
+-------+-------+
```

Both cells point to each other. To check if unbound: follow writer's pointer, verify target is reader that points back.

### 8.2 Unbound Variable with Suspension

```
+-------+-----------+
| WrtTag| (Ptr(1),  |  ← Writer (addr 0): reader pointer + suspension queue
|       |  SusQ)    |
+-------+-----------+
| RoTag | Ptr(0)    |  ← Reader (addr 1): points to writer
+-------+-----------+
```

The writer preserves the pointer to reader even when suspensions are added.

### 8.3 Writer Bound to Ground Value

```
+--------+-------+
|ValueTag| 42    |  ← Was writer (addr 0): now holds value
+--------+-------+
| RoTag  | Ptr(0)|  ← Reader (addr 1): still points to addr 0
+--------+-------+
```

### 8.4 Writer Bound to Another Variable

```
Variable X (addrs 0,1):
+-------+-------+
| WrtTag| Ptr(3)|  ← Writer X: bound to reader Y?
+-------+-------+
| RoTag | Ptr(0)|  ← Reader X?: points to writer X
+-------+-------+

Variable Y (addrs 2,3):
+-------+-------+
| WrtTag| Ptr(3)|  ← Writer Y: unbound, points to reader Y?
+-------+-------+
| RoTag | Ptr(2)|  ← Reader Y?: points to writer Y
+-------+-------+
```

Dereferencing X (addr 0):
1. addr 0 is WrtTag with Ptr(3) → follow to addr 3
2. addr 3 is RoTag with Ptr(2) → follow to addr 2
3. addr 2 is WrtTag with Ptr(3) → check: addr 3 is RoTag pointing back to addr 2 → unbound, return VarRef(2)

---

## 9. Migration Notes

### 9.1 VarRef Changes

Old:
```dart
class VarRef extends Term {
  final int varId;      // Writer address
  final bool isReader;  // Flag
}
```

New:
```dart
class VarRef extends Term {
  final int addr;       // Cell address (could be reader or writer)
}
```

### 9.2 Address Arithmetic Removal

All instances of `writerAddr + 1` or `readerAddr - 1` must be replaced with explicit pointer following.

**CRITICAL**: Code must NEVER use address arithmetic to navigate between writer and reader. Always use:
- `writerForReader(readerAddr)` — follow reader's pointer
- `readerForWriter(writerAddr)` — follow writer's pointer (FCP pattern)

The `+1` pattern was a workaround for the old spec that had writers pointing to `null`. With the FCP bidirectional pointer pattern, this arithmetic is never needed.

### 9.3 Affected Code Locations

- `heap_fcp.dart`: Core heap operations
- `suspend_ops.dart`: Suspension management
- `runner.dart`: Bytecode execution
- `irma_context.dart`: Multiagent variable handling
- `variable_table.dart`: V_p management

---

## 10. Imported Variables (Multiagent)

For multiagent GLP, imported readers have no local writer. The reader cell contains a VariableEntry (virtual writer) instead of a Pointer:

```
+-------+-------------+
| RoTag | VarEntry    |  ← Imported reader: contains V_p entry
+-------+-------------+
```

The VariableEntry serves as the "virtual writer" and holds:
- Creator agent ID
- Creator's local address
- Suspension queue (for local goals waiting)
- Received value (after assignment arrives)

When `derefAddr` encounters an imported reader (cell content is VariableEntry), it returns the VariableEntry directly. Callers should treat this as "unbound" and suspend the goal, similar to encountering an unbound local writer.

---

## Document History

| Version | Date | Changes |
|---------|------|---------|
| 3.0 | 2026-01-20 | Pointer architecture specification (replaces arithmetic-based v2.18) |
| 3.1 | 2026-01-20 | Added VariableEntry as derefAddr return type for imported readers (Section 4.2, 10) |
| 3.2 | 2026-01-31 | FCP bidirectional pointers: writer points to reader (Sections 1.2, 2.3, 3.1, 7.2, 8.x, 9.2). Eliminates all `+1` arithmetic. |
