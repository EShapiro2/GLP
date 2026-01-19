# Implementation Plan: Imported Variable Heap Representation

**Date**: 2026-01-19  
**Status**: DRAFT  
**Author**: Claude  
**Related Specs**: 
- `/docs/glp-runtime-spec.txt` v2.18 (Heap Storage, Dereferencing, Imported Variables)
- `/docs/ma/irmaGLP-spec.md` v2.3 (Variable Table V_p, request routine, Communicate Transaction)

---

## 1. Rationale

### 1.1 The Problem

The current implementation incorrectly handles imported readers in multiagent GLP. When an agent receives a term containing a reader from another agent, the deserializer allocates a full writer/reader cell pair using `allocateFreshPair()`. This is wrong because the writer for an imported reader is remote; it exists at the creating agent's heap, not locally.

The consequence is that imported readers have a meaningless local writer cell that serves no purpose. When the GLP program suspends on an imported reader, the suspension system reports this local variable, but there is no proper connection to the V_p entry that tracks the remote creator. The `request()` routine cannot reliably find the V_p entry to send a read request to the creator.

### 1.2 The Solution

Per the updated runtime spec (v2.18), imported readers should be represented by a single reader cell whose content is the V_p entry (VariableEntry) itself. The V_p entry represents the remote writer. When dereferenced, the runtime recognizes the VariableEntry and either returns the stored value (if an assignment has been received) or the entry itself (if still unbound).

This design unifies the heap representation with the V_p tracking mechanism specified in irmaGLP-spec.md Section 3.1.2. The reader cell's content directly points to the routing information, eliminating the lookup indirection that was prone to failure.

### 1.3 Specification Guidance

The runtime spec v2.18 defines the following behavior:

**Imported Variables (from Heap Storage section):**
- Imported readers: single reader cell allocated, content is V_p entry
- Imported writers: single writer cell allocated, content is V_p entry
- No paired local cell exists for imported variables

**Dereferencing (from Dereferencing section):**
- If cell content is VariableEntry and entry.state is Term, return the value
- If cell content is VariableEntry and entry.state is not Term, return the entry
- The caller uses the returned VariableEntry for routing (creator, creatorLocalId)

The irmaGLP spec v2.3 defines the V_p entry structure and the `request(X?)` routine that uses it.

---

## 2. Current Architecture

### 2.1 Relevant Files

**Heap and Terms:**
- `/lib/runtime/heap_fcp.dart` — HeapFCP class, allocateVariable(), derefAddr()
- `/lib/runtime/terms.dart` — VarRef, StructTerm, ConstTerm

**Multiagent:**
- `/lib/multiagent/variable_table.dart` — VariableEntry, VariableTable, VarKey
- `/lib/multiagent/payload_serializer.dart` — deserializeAgentMessagePayloadWithMapping()
- `/lib/multiagent/irma_context.dart` — importTerm(), processSuspension()
- `/lib/multiagent/helpers.dart` — request() routine

**Integration:**
- `/glp_multiagent/lib/main.dart` — scheduler integration, processSuspension call

### 2.2 Current Flow (Broken)

When Charlie receives `ch(CA?, AC)` from Bob:

1. `PayloadSerializer.deserializeAgentMessagePayloadWithMapping()` calls `allocateFreshPair()` for each variable
2. Returns `VarRef(localVarId, isReader: true)` for CA? where localVarId is the writer address
3. `importTerm()` creates V_p entry with `VarKey(localVarId, true)` 
4. The reader cell at `localVarId + 1` points to the local writer cell at `localVarId`
5. V_p entry exists separately, not connected to heap cell

When Charlie suspends on CA?:

1. Bytecode runner finds unbound reader at `localVarId + 1`
2. Reports `localVarId` (writer address) to suspension system
3. `processSuspension()` calls `request(localVarId, ...)`
4. `request()` looks up `VarKey(localVarId, true)` in V_p
5. **This should work**, but the indirection is fragile and the local writer cell is meaningless

---

## 3. Implementation Plan

### 3.1 Phase 1: Extend HeapFCP for Imported Variables

**File:** `/lib/runtime/heap_fcp.dart`

**3.1.1 Add allocateImportedReader method:**

```dart
/// Allocate a reader cell for an imported variable (no local writer)
/// Returns the reader cell address
/// The cell content will be set to the VariableEntry by the caller
int allocateImportedReader() {
  final readerAddr = HP++;
  cells.add(HeapCell(null, CellTag.RoTag));
  return readerAddr;
}
```

**3.1.2 Add allocateImportedWriter method:**

```dart
/// Allocate a writer cell for an imported variable (no local reader)
/// Returns the writer cell address
/// The cell content will be set to the VariableEntry by the caller
int allocateImportedWriter() {
  final writerAddr = HP++;
  cells.add(HeapCell(null, CellTag.WrtTag));
  return writerAddr;
}
```

**3.1.3 Update derefAddr to handle VariableEntry:**

Add handling for VariableEntry before existing checks:

```dart
Term derefAddr(int addr) {
  var current = addr;
  Set<int> visited = {};

  while (true) {
    if (visited.contains(current)) {
      throw StateError('Cycle detected at address $current');
    }
    visited.add(current);

    final cell = cells[current];

    // NEW: Imported reader - content is V_p entry
    if (cell.tag == CellTag.RoTag && cell.content is VariableEntry) {
      final entry = cell.content as VariableEntry;
      if (entry.state is Term) {
        return entry.state as Term;
      }
      return entry;  // Return entry for routing
    }

    // NEW: Imported writer - content is V_p entry  
    if (cell.tag == CellTag.WrtTag && cell.content is VariableEntry) {
      final entry = cell.content as VariableEntry;
      if (entry.state is Term) {
        return entry.state as Term;
      }
      return entry;  // Return entry for routing
    }

    // ... existing code unchanged ...
  }
}
```

**3.1.4 Add import for VariableEntry:**

```dart
import 'package:glp_runtime/multiagent/variable_table.dart' show VariableEntry;
```

**Note:** This creates a dependency from runtime to multiagent. If this is undesirable, we could define a marker interface or abstract class in runtime that VariableEntry implements.

### 3.2 Phase 2: Update Payload Deserializer

**File:** `/lib/multiagent/payload_serializer.dart`

**3.2.1 Change deserializeAgentMessagePayloadWithMapping signature:**

The deserializer needs to allocate different cell types based on whether the variable is a reader or writer. It also needs access to create VariableEntry objects.

```dart
static (Term, Map<int, GlobalVarId>) deserializeAgentMessagePayloadWithMapping(
  List<int> payload,
  int Function(bool isReader) allocateImportedVar,  // NEW: takes isReader flag
  VariableEntry Function(int addr, bool isReader, GlobalVarId globalId) createEntry,  // NEW
) {
  ...
}
```

**3.2.2 Update variable deserialization logic:**

```dart
case _tagVariable:
  // Decode global ID
  final (idLength, lengthSize) = _decodeLength(bytes, offset);
  offset += lengthSize;
  final idBytes = bytes.sublist(offset, offset + idLength);
  offset += idLength;
  final globalIdStr = utf8.decode(idBytes);
  final globalId = GlobalVarId.decode(globalIdStr);
  
  // Decode isReader flag
  final isReader = bytes[offset] == 1;
  offset++;
  
  // Allocate appropriate cell type and create entry
  int localAddr;
  if (varMapping.containsKey(globalIdStr)) {
    localAddr = varMapping[globalIdStr]!;
  } else {
    localAddr = allocateImportedVar(isReader);
    varMapping[globalIdStr] = localAddr;
    // Create and attach V_p entry to cell
    createEntry(localAddr, isReader, globalId);
  }
  
  return (VarRef(localAddr, isReader: isReader), offset - startOffset);
```

### 3.3 Phase 3: Update IrmaContext Import Logic

**File:** `/lib/multiagent/irma_context.dart`

**3.3.1 Update importTerm to use new deserializer API:**

```dart
Term importTerm(List<int> payload, String sender) {
  final (term, globalIdMapping) = PayloadSerializer.deserializeAgentMessagePayloadWithMapping(
    payload,
    // Allocator callback
    (bool isReader) {
      if (isReader) {
        return heap.allocateImportedReader();
      } else {
        return heap.allocateImportedWriter();
      }
    },
    // Entry creator callback
    (int addr, bool isReader, GlobalVarId globalId) {
      final entry = VariableEntry(
        varId: addr,
        isReader: isReader,
        creator: globalId.creator,
        creatorLocalId: globalId.localId,
        role: isReader ? VariableRole.importedReader : VariableRole.importedWriter,
        state: null,
      );
      // Attach entry to heap cell
      heap.cells[addr].content = entry;
      // Also register in V_p for lookup by VarKey
      vp.add(VarKey(addr, isReader), entry);
      return entry;
    },
  );
  
  return term;
}
```

### 3.4 Phase 4: Update Suspension Handling

**File:** `/lib/multiagent/helpers.dart`

**3.4.1 Update request() to handle VariableEntry from dereference:**

The current `request()` looks up the V_p entry by VarKey. With the new design, we can get the entry directly from dereferencing:

```dart
void request(int readerAddr, String agentId, VariableTable vp, MessageQueue mp, HeapFCP heap) {
  // Dereference to get the entry directly
  final derefResult = heap.derefAddr(readerAddr);
  
  if (derefResult is! VariableEntry) {
    // Not an imported reader, or already bound
    return;
  }
  
  final entry = derefResult;
  
  // Check conditions per spec
  if (entry.role != VariableRole.importedReader) return;
  if (entry.creator == agentId) return;  // Local, not imported
  if (entry.state != null) return;  // Already requested
  
  // Update state to mark request sent
  entry.state = entry.creator;
  
  // Queue read request message
  final creatorSerializer = PayloadSerializer(entry.creator);
  final payload = creatorSerializer.createReadRequestPayload(
    entry.creatorLocalId,
    agentId,
  );
  mp.add(OutboundMessage(
    destination: entry.creator,
    type: MessageType.readRequest,
    payload: payload,
  ));
}
```

### 3.5 Phase 5: Update Assignment Handling

**File:** `/lib/multiagent/irma_context.dart`

**3.5.1 Update handleAssignment to store value in entry:**

When an assignment message arrives for an imported reader, store the value in the entry's state field:

```dart
void handleAssignment(GlobalVarId globalId, Term value, String sender) {
  // Find the local reader cell for this variable
  final readerKey = VarKey.fromGlobal(globalId, isReader: true);
  final entry = vp.lookup(readerKey);
  
  if (entry == null) {
    // Variable not in V_p - might be local now
    // Handle accordingly...
    return;
  }
  
  // Store value in entry
  entry.state = value;
  
  // Process suspensions on the reader cell
  final readerAddr = entry.varId;
  final suspensions = heap.getSuspensionsFromCell(readerAddr);
  if (suspensions != null) {
    // Activate suspended goals
    for (final record in suspensions.armed) {
      scheduler.enqueue(GoalRef(record.goalId!, record.resumePC));
      record.disarm();
    }
  }
}
```

### 3.6 Phase 6: Update Binding Callback for Imported Writers

**File:** `/lib/multiagent/irma_context.dart`

**3.6.1 Detect imported writer binding:**

When a writer is bound, check if it's an imported writer and send assignment to creator:

```dart
void onWriterBound(int writerAddr, Term value) {
  final cell = heap.cells[writerAddr];
  
  if (cell.content is VariableEntry) {
    final entry = cell.content as VariableEntry;
    if (entry.role == VariableRole.importedWriter) {
      // Send assignment to creator
      final payload = serializer.createAssignmentPayload(
        entry.creatorLocalId,
        value,
      );
      mp.add(OutboundMessage(
        destination: entry.creator,
        type: MessageType.assignment,
        payload: payload,
      ));
    }
  }
}
```

---

## 4. Testing Strategy

### 4.1 Unit Tests

**New tests in `/test/multiagent/`:**

1. **heap_imported_test.dart** — Test allocateImportedReader/Writer, verify single cell allocation
2. **deref_imported_test.dart** — Test dereferencing imported readers returns VariableEntry or stored value
3. **import_term_test.dart** — Test importTerm creates proper cell/entry structure
4. **request_imported_test.dart** — Test request() with new dereference-based lookup

### 4.2 Integration Tests

**Existing tests to verify:**

Run the full multiagent test suite to ensure no regressions:

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test test/multiagent/
```

### 4.3 End-to-End Test

**Manual test scenario:**

1. Launch multiagent app: `cd /Users/udi/Grassroots/GLP/glp_multiagent && flutter run -d macos`
2. Click "Alice↔Bob↔Charlie"
3. Bob: `introduce(alice, charlie)`
4. Alice: `accept_intro(charlie)`
5. Charlie: `accept_intro(alice)`
6. Alice: `send(charlie, hello)`
7. Verify: Charlie sees `received(alice, hello)`

---

## 5. Migration Considerations

### 5.1 VarRef Representation

The current VarRef uses `(varId, isReader)` where varId is the writer address. For imported readers, there is no writer address. We have two options:

**Option A (Recommended):** VarRef.varId for imported readers is the reader cell address directly. The isReader flag indicates this is a reader. This is consistent with the spec's statement that VarRef holds a heap address.

**Option B:** Keep varId as a logical ID separate from address. This would require more changes.

### 5.2 Backward Compatibility

The changes affect only imported variables. Local variables continue to use the two-cell design unchanged. Existing single-agent tests should pass without modification.

### 5.3 V_p Dual Registration

The implementation registers imported variables both:
1. In the heap cell (cell.content = entry)
2. In the V_p map (vp.add(key, entry))

Both point to the same VariableEntry object. The heap cell provides direct access during dereferencing. The V_p map provides lookup by VarKey for message handling. This dual registration is intentional and correct.

---

## 6. Implementation Order

1. **Phase 1** (HeapFCP) — Foundation, no breaking changes
2. **Phase 2** (PayloadSerializer) — Update API, may temporarily break callers
3. **Phase 3** (IrmaContext.importTerm) — Connect new allocator/entry creator
4. **Phase 4** (helpers.request) — Update to use dereference result
5. **Phase 5** (handleAssignment) — Store value in entry
6. **Phase 6** (onWriterBound) — Detect imported writer binding

Phases 1-3 should be done together as they form a cohesive unit. Phases 4-6 can be done incrementally.

---

## 7. Risks and Mitigations

**Risk:** Circular dependency between runtime and multiagent packages.

**Mitigation:** If needed, define an abstract `RemoteVariableEntry` interface in runtime that `VariableEntry` implements. The heap only depends on the interface.

**Risk:** Existing code assumes all readers have paired local writers.

**Mitigation:** Audit all code that uses `writerAddr + 1` or similar arithmetic. Such code must check whether the variable is imported first.

**Risk:** Suspension list handling differs for imported readers.

**Mitigation:** Suspension lists are still attached to the reader cell. The difference is only in what the cell content points to when unbound. Suspension processing logic remains unchanged.

---

## Document History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 0.1 | 2026-01-19 | Claude | Initial draft |
