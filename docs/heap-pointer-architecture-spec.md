# GLP Heap Storage Specification - Pointer Architecture

**Version**: 3.0 DRAFT  
**Date**: 2026-01-20  
**Status**: DRAFT - for review before implementation  
**Branch**: pointer-architecture

This document specifies the pointer-based variable representation for GLP, replacing the arithmetic-based address scheme in v2.18. The design follows the original FCP implementation.

---

## 1. Overview

### 1.1 Design Principles

A **reference** is a tagged pointer to a heap object. The tag identifies the type (reader, writer, value, etc.), and the pointer indicates where to find the content.

Heap navigation follows pointers explicitly rather than computing addresses via arithmetic. There is no implicit relationship between adjacent heap addresses.

### 1.2 Key Differences from v2.18

| Aspect | v2.18 (Arithmetic) | v3.0 (Pointer) |
|--------|-------------------|----------------|
| Reader→Writer relationship | Implicit: reader at writerAddr+1 | Explicit: reader contains pointer to writer |
| Writer content (unbound) | Pointer to reader | NULL or suspension queue |
| VarRef structure | `{varId, isReader}` | Single address; tag determines role |
| Finding paired cell | Arithmetic: addr ± 1 | Follow pointer |

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
- `null` — unbound, no suspensions
- `SuspensionListNode` — unbound, with suspended goals waiting
- `Pointer(addr)` — bound to value at addr (or transitively to another variable)

**RoTag (Reader Cell)**:
- `Pointer(writerAddr)` — points to paired writer (always, unless bound)
- `Pointer(valueAddr)` — after path compression, may point directly to value

**ValueTag (Ground Value)**:
- `Term` — the bound ground term (ConstTerm or StructTerm)

---

## 3. Variable Allocation

### 3.1 Local Variable (Writer/Reader Pair)

```dart
/// Allocate a fresh local variable.
/// Returns (writerAddr, readerAddr).
(int, int) allocateVariable() {
  final writerAddr = HP;
  final readerAddr = HP + 1;
  HP += 2;

  // Writer cell: initially unbound (null content)
  cells.add(HeapCell(null, CellTag.WrtTag));

  // Reader cell: points to its writer
  cells.add(HeapCell(Pointer(writerAddr), CellTag.RoTag));

  return (writerAddr, readerAddr);
}
```

**Key point**: The reader points TO the writer. The writer does NOT point to the reader. This is the opposite of v2.18.

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
/// Returns: Term (bound) | VarRef (unbound writer)
Object derefAddr(int startAddr) {
  // Phase 1: Follow chain to find final target
  var current = startAddr;
  int? finalAddr;  // Address of final target (if not a value)
  Object? finalValue;  // Final value (if ground term)

  while (true) {
    final cell = cells[current];

    switch (cell.tag) {
      case CellTag.RoTag:
        // Reader: follow pointer to writer
        if (cell.content is Pointer) {
          current = (cell.content as Pointer).targetAddr;
          continue;
        }
        throw StateError('Reader cell has invalid content: ${cell.content}');

      case CellTag.WrtTag:
        if (cell.content == null || cell.content is SuspensionListNode) {
          // Unbound writer - this is the final target
          finalAddr = current;
          finalValue = VarRef(current);
          break;
        }
        if (cell.content is Pointer) {
          // Bound to another cell - follow
          current = (cell.content as Pointer).targetAddr;
          continue;
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
  
  // Prepend to writer's suspension list
  if (writerCell.content is SuspensionListNode) {
    node.next = writerCell.content as SuspensionListNode;
  }
  writerCell.content = node;
}
```

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

### 7.2 Writer → Reader

There is no direct pointer from writer to reader. To find the paired reader, you need the original allocation return value. Alternatively, if the allocation invariant holds (reader is at writerAddr + 1), you can compute it:

```dart
int readerForWriter(int writerAddr) {
  // Only valid for locally allocated pairs
  // For imported writers, there is no local reader
  return writerAddr + 1;
}
```

**Note**: This arithmetic is only valid at allocation time for local variables. Once variables are bound or chains are formed, the relationship may not hold. For general navigation, always follow pointers.

---

## 8. Heap Diagram

### 8.1 Unbound Variable

```
+-------+-------+
| WrtTag| null  |  ← Writer (addr 0): unbound, no suspensions
+-------+-------+
| RoTag | Ptr(0)|  ← Reader (addr 1): points to writer
+-------+-------+
```

### 8.2 Unbound Variable with Suspension

```
+-------+-------+
| WrtTag| SusQ  |  ← Writer (addr 0): unbound, with suspension queue
+-------+-------+
| RoTag | Ptr(0)|  ← Reader (addr 1): points to writer
+-------+-------+
```

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
| WrtTag| null  |  ← Writer Y: unbound
+-------+-------+
| RoTag | Ptr(2)|  ← Reader Y?: points to writer Y
+-------+-------+
```

Dereferencing X (addr 0):
1. addr 0 is WrtTag with Ptr(3) → follow to addr 3
2. addr 3 is RoTag with Ptr(2) → follow to addr 2
3. addr 2 is WrtTag with null → return VarRef(2)

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

### 9.3 Affected Code Locations

- `heap_fcp.dart`: Core heap operations
- `suspend_ops.dart`: Suspension management
- `runner.dart`: Bytecode execution
- `irma_context.dart`: Multiagent variable handling
- `variable_table.dart`: V_p management

---

## 10. Future: Imported Variables

For multiagent GLP, imported readers have no local writer. The reader cell points to a VariableEntry (virtual writer) instead of a writer cell:

```
+-------+-------------+
| RoTag | VarEntry    |  ← Imported reader: points to V_p entry
+-------+-------------+
```

The VariableEntry serves as the "virtual writer" and holds:
- Creator agent ID
- Creator's local address
- Suspension queue (for local goals waiting)
- Received value (after assignment arrives)

This extension is deferred until single-isolate GLP works correctly.

---

## Document History

| Version | Date | Changes |
|---------|------|---------|
| 3.0 | 2026-01-20 | Pointer architecture specification (replaces arithmetic-based v2.18) |
