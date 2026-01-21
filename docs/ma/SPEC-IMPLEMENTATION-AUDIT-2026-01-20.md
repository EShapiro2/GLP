# irmaGLP Specification vs Implementation Audit

**Date:** 2026-01-20  
**Auditor:** Claude  
**Scope:** Complete review of irmaGLP spec (v3.0) against implementation in `/glp_runtime/lib/multiagent/`  
**Status:** Comprehensive audit identifying all discrepancies

---

## Executive Summary

This document provides an exhaustive analysis of discrepancies between the irmaGLP specification and its Dart implementation. The audit examined the following files against the specification in `/docs/ma/irmaGLP-spec.md`:

**Implementation files reviewed:**
- `lib/multiagent/irma_context.dart`
- `lib/multiagent/variable_table.dart`
- `lib/multiagent/helpers.dart`
- `lib/multiagent/payload_serializer.dart`
- `lib/runtime/heap_fcp.dart`
- `lib/runtime/terms.dart`
- `test/multiagent/bidirectional_stream_test.dart`

---

## Issue 1: VarRef Address Arithmetic Violates Spec Section 3.2.1

### Specification

Section 3.2.1 states: "A variable's reader/writer identity is determined by its heap cell tag (RoTag or WrtTag), NOT by address arithmetic."

**MUST NOT**: "Code must not assume `reader_addr == writer_addr + 1` or derive reader/writer identity from address parity."

### Implementation

`VarRef` in `terms.dart` computes `isReader` and `varId` from address parity:

```dart
int get varId => addr & ~1;            // Clear lowest bit to get "writer address"
bool get isReader => (addr & 1) == 1;  // Odd address = reader
```

The docstring explicitly documents the now-prohibited assumption:
```
/// Pointer architecture address layout:
/// - Writer at even address N, Reader at N+1 (odd)
/// - Both share the same logical varId = N (the writer address)
```

This docstring directly contradicts spec Section 3.2.1.

### Problem

`allocateImportedReader()` and `allocateImportedWriter()` in `heap_fcp.dart` simply use `HP++`:

```dart
int allocateImportedReader() {
  final readerAddr = HP++;
  cells.add(HeapCell(null, CellTag.RoTag));
  return readerAddr;
}
```

If HP is even when allocating an imported reader, the resulting address is even, and `VarRef.isReader` returns `false`. The entire multiagent system depends on correctly identifying readers vs writers for:
- Serialization (payload_serializer.dart stores isReader flag)
- V_p role determination (importedReader vs importedWriter)
- Message routing (assignments go to readers, requests come from readers)

### Consequences

When an imported reader happens to have an even address:
1. Serialization stores `isReader=false`
2. Deserialization at the receiver creates a VarRef with wrong `isReader`
3. V_p lookups fail because VarKey uses the wrong `isReader` value
4. Message handlers cannot find the correct entry

### Required Fix

The fix requires changes to `VarRef` itself. Either:
1. Store `isReader` explicitly as a field in VarRef (breaking change), or
2. Remove `isReader` and `varId` computed properties entirely; all callers must use `heap.isReader(addr)` and `heap.isWriter(addr)`

All code that uses `VarRef.isReader` or `VarRef.varId` is transitively violating the spec. Known usages include:
- `payload_serializer.dart`: `_serializeTermRecursive` uses `term.varId` and `term.isReader`
- `helpers.dart`: `_exportTermRecursive` uses `term.isReader`
- `irma_context.dart`: `_importTermRecursive` uses `term.isReader`
- `variable_table.dart`: `VarKey` constructor often called with `term.isReader`

---

## Issue 2: Suspensions Dropped for Imported Readers

### Specification

Section 3.1.2 states: "For imported readers, V_p serves as the 'virtual writer' that holds the suspension list. When an assignment message arrives for an imported reader, the runtime resumes goals from Σ."

Section 5.3 Type 1 (Assignment to imported reader): "Let R = reactivate(X?) for agent q (modifies S'_q)"

### Implementation

In `heap_fcp.dart`, `suspendOnReader` explicitly drops suspensions for imported readers:

```dart
void suspendOnReader(int readerAddr, SuspensionRecord record) {
  final cell = cells[readerAddr];
  
  if (cell.content is VariableEntry) {
    // Imported reader - store suspension in entry or separate mechanism
    // For now, we'll add a suspension list to the entry's state
    // This may need refinement based on irmaGLP requirements
    // TODO: Handle imported reader suspension properly
    // For now, just return - the caller should handle this case
    return;  // <-- SUSPENSION IS DISCARDED
  }
  ...
}
```

### Consequences

1. Goal suspends on imported reader
2. Suspension record is discarded (not stored anywhere)
3. Assignment message arrives via `handleAssignment`
4. `bindImportedReader` is called, which walks `entry.suspensions`
5. `entry.suspensions` is null because no suspension was ever stored
6. No goals are reactivated
7. Goals that should resume remain stuck

### Required Fix

Modify `suspendOnReader` to store suspensions in `VariableEntry.suspensions`:

```dart
if (cell.content is VariableEntry) {
  final entry = cell.content as VariableEntry;
  final node = SuspensionListNode(record);
  node.next = entry.suspensions;
  entry.suspensions = node;
  return;
}
```

---

## Issue 3: Scheduler Does Not Report Blocking Readers to IrmaContext

### Specification

Section 5.2 Case 2 (Suspend): "Call request(X?) for each X? ∈ W (modifies V'_p and M'_p)"

Section 8.4 (Scheduler-IRMA Integration): "The `DrainResult` structure should include the blocking reader set... After each drain operation, if `status == suspended`, the caller should invoke: `context.processSuspension(result.blockingReaders)`"

### Implementation

`IrmaContext` has a method `processSuspension(Set<int> blockingReaders)` that is designed to send read requests for imported readers. However, this method is never called.

In the bidirectional stream test:
```dart
final result1 = scheduler1.drainWithStatus();
// processSuspension is NOT called!
ctx1.flushMessages();
```

The `DrainResult` class (in scheduler.dart) does not include a `blockingReaders` field. The scheduler tracks suspended goals internally but does not expose which readers caused the suspension.

### Consequences

1. Goal attempts to reduce but suspends on imported reader X?
2. Scheduler records the suspension internally
3. No read request is sent to the creator of X?
4. Creator never learns that anyone wants X?'s value
5. Even when creator binds the variable, no assignment is sent (state=null means no requester)
6. Goal remains suspended indefinitely

### Required Fix

1. Modify `DrainResult` to include `Set<int> blockingReaders`
2. Modify scheduler to populate this set when goals suspend
3. Modify all call sites to invoke `context.processSuspension(result.blockingReaders)` after drain

---

## Issue 4: Legacy Arithmetic Addressing Violates Spec Section 3.2.1

### Specification

Section 3.2.1: "Code must not assume `reader_addr == writer_addr + 1`"

### Implementation

Multiple methods in `payload_serializer.dart` use prohibited arithmetic:

1. `deserializeTerm` (non-V2):
```dart
// Compute heap address: varId is writer address, reader is at varId + 1
final addr = isReader ? globalId.localId + 1 : globalId.localId;
```

2. `_deserializeTermWithMapping` (legacy):
```dart
// allocateFreshVar() returns writer address; compute correct address
final writerAddr = allocateFreshVar();
localVarId = isReader ? writerAddr + 1 : writerAddr;
```

3. `_serializeTermRecursive` uses `term.varId` and `term.isReader` which are computed via prohibited address arithmetic:
```dart
final globalId = GlobalVarId(agentId, term.varId);  // term.varId uses addr & ~1
builder.addByte(term.isReader ? 1 : 0);  // term.isReader uses (addr & 1) == 1
```

### Consequences

All serialization and legacy deserialization paths are broken for imported variables whose addresses do not follow the even/odd convention. The V2 deserializer `_deserializeTermWithMappingV2` correctly uses allocated addresses, but serialization still relies on the prohibited `term.varId` and `term.isReader` computed properties.

### Required Fix

Remove or deprecate all legacy deserializers. For serialization, pass explicit `isReader` flag rather than relying on `VarRef.isReader`. This requires either:
1. Storing `isReader` explicitly in VarRef, or
2. Looking up the cell tag via `heap.isReader(addr)` during serialization

---

## Issue 5: VarKey vs VarRef.varId Mismatch Due to Prohibited Arithmetic

### Specification

Section 3.2.1 prohibits deriving variable identity from address arithmetic.

### Implementation

`VarKey` in `variable_table.dart`:
```dart
class VarKey {
  final int varId;
  final bool isReader;
  
  const VarKey(this.varId, this.isReader);
}
```

`VarRef` in `terms.dart`:
```dart
int get varId => addr & ~1;  // Computed: clear lowest bit
```

For local variables (two-cell allocation), VarKey(writerAddr, false) and VarRef(writerAddr).varId both equal writerAddr. But for imported variables:

Example: Imported reader allocated at address 5
- Heap address = 5
- VarKey in V_p uses `VarKey(5, true)` (raw address)
- VarRef(5).varId = 4 (computed via `addr & ~1`)

### Consequences

Code that creates VarKey from VarRef.varId will use mismatched keys:
```dart
final key = VarKey(varRef.varId, varRef.isReader);
// For imported reader at addr 5: key = VarKey(4, true)
// But V_p entry was stored as VarKey(5, true)
// Lookup fails!
```

### Required Fix

Establish consistent addressing: either VarKey always uses raw address, or VarRef.varId returns raw address. Given that VarRef.isReader is already broken for imported variables (Issue 1), the cleanest fix is to make VarRef store explicit fields rather than computing them.

---

## Issue 6: Test Creates VarRef Without Explicit Reader Flag

### Specification

N/A (test code)

### Implementation

In `bidirectional_stream_test.dart`:
```dart
final xsReaderRef1 = VarRef(xs1ImportedAddr);  // Reader address (imported)
```

This relies on `xs1ImportedAddr` being odd for `VarRef.isReader` to return true.

### Consequences

If the imported reader allocation happens to return an even address, the test passes a VarRef that claims to be a writer when it is actually a reader. The merge program then receives incorrect input.

### Required Fix

Either fix VarRef per Issue 1, or explicitly verify address parity in tests:
```dart
assert(xs1ImportedAddr & 1 == 1, 'Imported reader should have odd address');
```

But the latter is a workaround, not a fix.

---

## Issue 7: handleAssignment Stores Value in entry.state But Binding Already Happened

### Specification

Section 5.3 Type 1 (Imported reader case): "Apply {X?:=T} to resolvent... Remove (X?, r, s) from V'_q"

### Implementation

In `irma_context.dart`:
```dart
void handleAssignment(String creator, int creatorLocalId, Term value) {
  ...
  if (entry.role == VariableRole.importedReader) {
    // Store value in entry.state for reference
    entry.state = value;

    // Bind imported reader: updates heap cell
    final activations = runtime.heap.bindImportedReader(readerAddr, value, entry);
    ...
    // Remove from V_p - variable is now bound
    vp.remove(entry.key);
  }
}
```

The sequence is:
1. Store value in entry.state
2. Call bindImportedReader (which uses entry.suspensions)
3. Remove entry from V_p

### Problem

`entry.state = value` happens before `bindImportedReader`. Inside `bindImportedReader`, the code checks `entry.suspensions` which is separate from `entry.state`. This is not necessarily wrong, but there is conceptual confusion about what `entry.state` represents:
- For created writers: requester agent ID (String)
- For imported readers: creator ID after request sent, or the value after assignment?

The dual use of `state` for both protocol bookkeeping and value storage is error-prone.

### Required Fix

Clarify the semantics of `VariableEntry.state` in the spec and implementation. Consider separate fields for `requester: String?` and `value: Term?` to avoid type confusion.

---

## Issue 8: _requestFromHeap Falls Back to Legacy Request Without Heap Cell

### Specification

Section 4.2 (request): "If (X?, q, ⊥) ∈ V'_p and q ≠ p then: Update to (X?, q, q) in V'_p, Add (request(X?, p), q) to M'_p"

### Implementation

In `irma_context.dart`:
```dart
void _requestFromHeap(int readerAddr) {
  // Check if this address exists in the heap
  if (readerAddr >= runtime.heap.cells.length) {
    print('[DEBUG IRMA $agentId] _requestFromHeap: addr $readerAddr out of bounds, using legacy request');
    helpers.request(readerAddr, agentId, vp, mp);
    return;
  }
  ...
}
```

The comment mentions "Legacy importTerm doesn't allocate heap cells" as the reason for the fallback.

### Problem

If there are code paths where imported variables exist in V_p but not in the heap, the system is in an inconsistent state. The spec requires that imported readers have heap cells (with VariableEntry attached).

### Required Fix

Ensure all imported variable registration creates heap cells. Remove the fallback or convert it to an assertion failure that indicates a bug.

---

## Issue 9: helpers.abandon() Takes readerId But Documentation Says "Variable Y"

### Specification

Section 4.1: "abandon(Y)" where Y can be either reader or writer depending on context.

### Implementation

In `helpers.dart`:
```dart
/// abandon(readerId) for agent p
/// 
/// CRITICAL: An agent can only abandon a READER, which causes its 
/// dual writer to be abandoned at the remote agent.
void abandon(
  int readerId, 
  VariableTable vp, 
  MessageQueue mp,
) {
  final readerKey = VarKey(readerId, true); // reader
  ...
}
```

The implementation explicitly takes `readerId` and creates a reader VarKey, but the spec defines abandon for any variable Y.

### Problem

The spec's abandon() definition handles multiple cases:
- "If (Y, q, s) ∈ V_p where q ≠ p": Y is imported (could be reader or writer)
- "If (Y, p, s) ∈ V_p and s ≠ ⊥": Y is created with requester

The implementation restricts this to only readers.

### Required Fix

Either update the spec to clarify that only readers can be abandoned locally (which may be correct per GLP semantics), or update the implementation to handle all cases per spec.

---

## Issue 10: export() Relay Setup Uses Wrong VarRef for Replacement

### Specification

Section 4.3: "create fresh pair (Z, Z?), replace Y with Z? in T'"

### Implementation

In `helpers.dart`:
```dart
// Callback allocates and returns [writerAddr, readerAddr]
final pair = allocateFreshPair(0, 0);
final relayWriter = pair[0];  // Writer address (even)
final relayReader = pair[1];  // Reader address (odd)

// Replace Y? with Z? in term - relayReader is already the reader address
final replacedTerm = VarRef(relayReader);
```

This creates `VarRef(relayReader)` where relayReader is the reader address. Per Issue 1, if relayReader is odd, `VarRef.isReader` will be true. But if the allocation happens to produce an even reader address (impossible with current allocateVariable but possible if allocateFreshPair is changed), this breaks.

### Problem

The code comment says "relayReader is already the reader address" and creates a VarRef assuming address parity determines type. This continues the problematic pattern from Issue 1.

### Required Fix

Address Issue 1 comprehensively so that VarRef type is explicit, not computed.

---

## Issue 11: bindImportedReader Heap Structure Now Specified in Section 3.2.4

### Specification

Section 3.2.4 now specifies the binding process:
"When an assignment message arrives for an imported reader:
1. Allocate a new ValueTag cell containing the term
2. Replace the reader cell's content with Pointer to the value cell
3. Resume goals from VariableEntry.suspensions"

### Implementation

In `heap_fcp.dart`:
```dart
List<GoalRef> bindImportedReader(int readerAddr, Term value, VariableEntry entry) {
  ...
  // Allocate a value cell for the term and point reader to it
  final valueCellAddr = cells.length;
  cells.add(HeapCell(value, CellTag.ValueTag));
  cell.content = Pointer(valueCellAddr);

  return activations;
}
```

After binding, the reader cell contains `Pointer(valueCellAddr)` where valueCellAddr is a ValueTag cell.

### Problem

This creates an unusual heap structure: a reader cell pointing to a value cell (not a writer cell). The `derefAddr` function handles this:
```dart
case CellTag.RoTag:
  if (cell.content is Pointer) {
    current = (cell.content as Pointer).targetAddr;
    continue;  // Will reach ValueTag case
  }
```

But other code that expects readers to point to writers may be confused. For example, `isImportedReader`:
```dart
if (cell.content is Pointer) {
  final targetAddr = (cell.content as Pointer).targetAddr;
  final targetCell = cells[targetAddr];
  // If target is ValueTag, it was bound via bindImportedReader
  return targetCell.tag == CellTag.ValueTag;
}
```

This works correctly but is fragile: the distinction between "local reader pointing to writer" and "bound imported reader pointing to value" depends on target cell tag.

### Required Fix

Document this heap layout explicitly in the spec. Add assertions or documentation in heap_fcp.dart to clarify the valid states for reader cells.

---

## Issue 12: VariableEntry.state Has Overloaded Semantics

### Specification

Section 3.1.2: "s ∈ 𝒯 ∪ Π ∪ {⊥}" where:
- For writers: s is the value or ⊥
- For created readers: s is the requester or ⊥
- For imported readers: s is the creator (after request sent) or ⊥

### Implementation

In `variable_table.dart`:
```dart
/// State depends on role:
/// - Writer: bound value (dynamic) or null if unbound
/// - Created reader: requester agent ID (String) or null if no request
/// - Imported reader: creator ID (String) if request sent, null otherwise
dynamic state;
```

In `irma_context.dart`, `handleAssignment` for imported readers:
```dart
// Store value in entry.state for reference
entry.state = value;
```

So `state` for imported readers holds:
- null: not requested
- String (creator ID): request sent
- Term (value): after assignment received

### Problem

The same field holds String or Term depending on lifecycle stage. Code that checks `entry.state is String` vs `entry.state is Term` is type-unsafe.

In `_onWriterBound`:
```dart
if (entry.role == VariableRole.createdWriter && entry.state != null) {
  final requester = entry.state as String;  // Assumes String, could be Term
```

If for any reason a Term ended up in state for a createdWriter (unlikely but possible bug), this would crash.

### Required Fix

Use typed fields instead of a single `dynamic state`:
```dart
String? requester;      // For created writers/readers: who requested
Term? boundValue;       // For all roles: the bound value (if any)
bool requestSent;       // For imported readers: whether request was sent
```

---

## Issue 13: PayloadSerializer Uses agentId for Global ID Creation

### Specification

Section 8.1: "creator:localId" where creator is the agent who allocated the variable.

### Implementation

In `payload_serializer.dart`:
```dart
void _serializeTermRecursive(Term term, String agentId, BytesBuilder builder) {
  ...
  if (term is VarRef) {
    // Encode as global ID: creator:localId
    final globalId = GlobalVarId(agentId, term.varId);
    ...
  }
}
```

The `agentId` parameter is assumed to be the creator of all variables in the term.

### Problem

When serializing a term that contains imported variables, the imported variable's creator is NOT the serializing agent. For example:
- Alice has imported variable X from Bob (creator=Bob, localId=42 at Bob)
- Alice's local heap has X at address 100
- When Alice serializes a term containing X, she should send `bob:42`, not `alice:100`

The current code would incorrectly send `alice:100` because it uses `agentId` (Alice) and `term.varId` (100, Alice's local address).

### Required Fix

When serializing terms for export, look up each variable in V_p to get the correct (creator, creatorLocalId) for imported variables. Only use local agentId for variables created locally.

---

## Issue 14: allocateFreshPair Callback Parameters Unused

### Specification

N/A (implementation detail)

### Implementation

In `helpers.dart`:
```dart
List<int> Function(int, int) allocateFreshPair,
...
final pair = allocateFreshPair(0, 0);
```

The callback takes two int parameters but they are always passed as (0, 0) and ignored.

### Problem

Dead parameters indicate incomplete design or vestigial code.

### Required Fix

Either remove the parameters from the callback signature, or document their intended purpose and implement proper usage.

---

## Issue 15: No Validation of Message Types in Routing Callbacks

### Specification

Section 5.3 defines three message types: Assignment, Read Request, Abandon.

### Implementation

In `irma_context.dart`:
```dart
typedef MessageDeliveryCallback = void Function(String destination, OutboundMessage message);
```

The callback receives the raw `OutboundMessage` and must switch on `message.type` to deserialize appropriately.

### Problem

There is no type-safe routing mechanism. Each callback implementation must manually handle all message types, duplicating deserialization logic. This is visible in the bidirectional stream test:

```dart
ctx1.onMessageReady = (destination, message) {
  if (message.type == MessageType.assignment) {
    final serializer = PayloadSerializer('isolate1');
    final (globalId, value) = serializer.deserializeAssignmentPayload(message.payload);
    ctx2.handleAssignment(globalId.creator, globalId.localId, value);
  } else if (message.type == MessageType.readRequest) {
    ...
  }
};
```

### Required Fix

Consider a more type-safe approach:
```dart
void onAssignment(String destination, GlobalVarId varId, Term value);
void onReadRequest(String destination, int varId, String requester);
void onAbandon(String destination, int varId);
```

---

## Issue 16: handleReadRequest Passes varId to findByCreatorLocalId Incorrectly

### Specification

Section 5.3 Type 2: Read request contains the variable's global ID from the requester's perspective.

### Implementation

In `irma_context.dart`:
```dart
void handleReadRequest(int varId, String requester) {
  // First check reader entry - use findByCreatorLocalId since varId is creatorLocalId
  // Note: For created readers, we are the creator, so use agentId
  final readerEntry = vp.findByCreatorLocalId(agentId, varId, isReader: true);
```

The comment says "varId is creatorLocalId" but this is the value extracted from the read request payload, which was created by the requester using THEIR creator's namespace.

### Problem

If Alice sends a read request to Bob for variable X (created by Bob):
- Alice's V_p has entry: VarKey(aliceLocalAddr, true), creator=bob, creatorLocalId=bobsVarId
- Alice sends request using creatorLocalId (bobsVarId)
- Bob receives varId=bobsVarId
- Bob calls `findByCreatorLocalId(bob, bobsVarId, isReader: true)`

This should find the created reader entry in Bob's V_p. But Bob's entry might be:
- VarKey(bobsVarId, true), creator=bob

Wait, this should work because for created variables, varId == creatorLocalId. Let me trace more carefully...

Actually, the issue is that `findByCreatorLocalId` searches for entries where `entry.creator == creator && entry.creatorLocalId == creatorLocalId`. For Bob's created reader:
- entry.creator = bob (correct)
- entry.creatorLocalId = bobsVarId (correct, defaults to varId)

So this should work. However, the logic is convoluted and relies on the default value of creatorLocalId.

### Required Fix

Add comments and assertions to clarify the expected relationships between varId, creatorLocalId, and the message protocol.

---

## Issue 17: export() Does Not Register Heap Callback for Newly Exported Writers

### Specification

Section 5.2 Case 1: "Update M'_p for created readers: add (X?:=T, r) for each {X?:=T} ∈ σ̂? where (X?, p, r) ∈ V'_p, r ≠ ⊥"

### Implementation

In `irma_context.dart`, `exportTerm` calls `helpers.export` which adds entries to V_p:
```dart
vp.add(varKey, VariableEntry(
  varId: varId,
  isReader: term.isReader,
  creator: agentId,
  role: role,
));
```

But for newly exported writers, no heap callback is registered.

### Problem

If a local writer is exported (added to V_p as createdWriter), and later a remote agent requests it, and then the writer is bound locally, the binding should trigger a message. But without a heap callback, the binding goes unnoticed.

Compare with `registerWriter`:
```dart
void registerWriter(int varId) {
  vp.add(key, VariableEntry(...));
  
  // Register heap callback to observe when this writer is bound
  runtime.heap.onBind(varId, (Term value) {
    _onWriterBound(varId, value);
  });
}
```

In `helpers.export`, there is no corresponding `onBind` registration.

### Required Fix

`exportTerm` should register heap callbacks for all newly exported writers, similar to `registerWriter`.

---

## Issue 18: isImportedReader Returns True for Bound Imported Readers

### Specification

Does not define when a reader stops being "imported."

### Implementation

In `heap_fcp.dart`:
```dart
bool isImportedReader(int readerAddr) {
  ...
  if (cell.content is Pointer) {
    final targetAddr = (cell.content as Pointer).targetAddr;
    final targetCell = cells[targetAddr];
    // If target is ValueTag, it was bound via bindImportedReader
    return targetCell.tag == CellTag.ValueTag;
  }
  return false;
}
```

After `bindImportedReader`, the reader cell points to a ValueTag cell, and `isImportedReader` returns true.

### Problem

The function name suggests it identifies imported readers, but it returns true for BOUND imported readers. For unbound imported readers (cell.content is VariableEntry), it returns true. This is consistent but the naming is confusing.

Consider: what does "imported" mean after binding? The variable is now bound and the entry was removed from V_p.

### Required Fix

Clarify the semantics of "imported reader" in the spec. Document that `isImportedReader` returns true for both bound and unbound imported readers (identified by cell structure rather than V_p presence).

---

## Issue 19: Missing Abandon Message Handling for Imported Writers

### Specification

Section 5.3 Type 3: "abandon(Y)" notification handling is not fully specified for imported writers.

### Implementation

In `irma_context.dart`:
```dart
void handleAbandon(int varId) {
  // Remove both reader and writer entries if present
  vp.remove(VarKey(varId, true));
  vp.remove(VarKey(varId, false));
  
  // Remove any pending bind callback
  runtime.heap.removeBindCallback(varId);
  
  // TODO: Reactivate any goals suspended on this variable
  // (They will fail since the remote counterpart is gone)
}
```

The TODO indicates incomplete implementation.

### Problem

Per spec Section 5.1 (Variable Abandonment): "Abandoned variables allow garbage-collection in shared variable tables and cause dependent suspended goals to fail rather than wait indefinitely."

Currently, goals suspended on an abandoned variable remain suspended forever.

### Required Fix

Implement goal failure for abandoned variables:
1. Find all goals suspended on this variable
2. Move them to the failed set F_p
3. Optionally: propagate abandonment to dependent variables

---

## Issue 20: tryWriterForReader Returns null for Imported Readers But No Guidance on Handling

### Specification

The spec describes imported readers as having no local writer, but does not detail error handling.

### Implementation

In `heap_fcp.dart`:
```dart
int? tryWriterForReader(int readerAddr) {
  ...
  if (cell.content is Pointer) {
    return (cell.content as Pointer).targetAddr;
  }
  return null; // Imported reader - no local writer
}
```

Callers receive `null` but guidance on handling varies.

### Problem

Different callers handle `null` differently:
- Some throw errors (e.g., `bindWriterToReader`)
- Some silently skip processing
- Some have no null check and would crash on `null!`

### Required Fix

Document the expected behavior when `tryWriterForReader` returns null. Consider whether callers should always check for imported readers explicitly before calling methods that assume local writers.

---

## Summary

This audit identified 20 distinct issues ranging from fundamental design mismatches (VarRef address arithmetic) to incomplete implementations (suspension handling, abandon notifications) to type safety concerns (VariableEntry.state overloading).

The most critical issues that explain the bidirectional stream test failure are:

1. **Issue 2**: Suspensions dropped for imported readers
2. **Issue 3**: Scheduler does not call processSuspension
3. **Issue 1**: VarRef.isReader broken for imported variables

These three issues form a cascade: even if suspensions were stored correctly (Issue 2), they would never be used because read requests are never sent (Issue 3), and even if requests were sent, address mismatches might prevent proper routing (Issue 1).

All 20 issues should be addressed to achieve a correct irmaGLP implementation.

---

## Document History

| Version | Date | Author | Changes |
|---------|------|--------|--------|
| 1.0 | 2026-01-20 | Claude | Initial comprehensive audit |
| 1.1 | 2026-01-20 | Claude | Updated to reference spec v3.0 Section 3.2.1; expanded Issue 1 and Issue 4 |
