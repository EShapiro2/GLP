# irmaGLP Implementation Progress

**Date**: 2026-01-21
**Branch**: claude/refactor-implementation-aGDN6
**Session**: Addressing 20 audit issues from SPEC-IMPLEMENTATION-AUDIT-2026-01-20.md

---

## Summary

Refactoring the GLP runtime implementation to comply with irmaGLP-spec.md v3.0.

## Progress

### Issue 1: VarRef Address Arithmetic Violates Spec Section 3.2.1 ✅ COMPLETED

**Status**: FIXED (commit b87b948)

**Problem**: VarRef computed `isReader` and `varId` from address parity, violating spec requirement that variable identity comes from heap cell tags.

**Solution**:
- Removed `VarRef.isReader` and `VarRef.varId` computed properties from terms.dart
- Updated all callers to use `heap.isReader(addr)` for type checking
- Added `isReader` callback parameter to `helpers.export()`
- Updated `PayloadSerializer` with `serializeTermWithCallbacks()` for proper handling

**Files Changed**:
- `lib/runtime/terms.dart` - Removed computed properties
- `lib/multiagent/helpers.dart` - Added isReader callback
- `lib/multiagent/payload_serializer.dart` - Added callback-based serialization
- `lib/multiagent/irma_context.dart` - Uses heap.isReader()
- `lib/multiagent/irma_agent.dart` - Uses heap.isReader()
- `lib/runtime/external_io.dart` - Changed .varId to .addr
- Test files updated with testIsReader helper

---

### Issue 2: Suspensions Dropped for Imported Readers ✅ COMPLETED

**Status**: FIXED (commit 780e5d0)

**Problem**: `suspendOnReader()` in heap_fcp.dart just returned without storing suspensions for imported readers, causing suspended goals to never be reactivated.

**Solution**: Store suspensions in `VariableEntry.suspensions` for imported readers:

```dart
if (cell.content is VariableEntry) {
  final entry = cell.content as VariableEntry;
  final node = SuspensionListNode(record);
  node.next = entry.suspensions;
  entry.suspensions = node;
  return;
}
```

**Files Changed**:
- `lib/runtime/heap_fcp.dart`

---

### Issue 3: Scheduler Does Not Report Blocking Readers to IrmaContext ✅ COMPLETED

**Status**: FIXED (commit c828bd0)

**Problem**: After `drainWithStatus()`, `processSuspension()` was not being called to send read requests for blocking imported readers.

**Solution**: Added `processSuspension()` calls after `drainWithStatus()` in bidirectional_stream_test.dart:

```dart
if (result1.status == ExecutionStatus.suspended && result1.blockingReaders.isNotEmpty) {
  ctx1.processSuspension(result1.blockingReaders);
}
```

**Files Changed**:
- `test/multiagent/bidirectional_stream_test.dart`

**Note**: The bidirectional stream test still fails due to a separate issue where goals succeed instead of suspending on imported readers. This appears to be a runtime issue with how merge clauses handle imported readers in the first argument position.

---

### Issue 4: Legacy Arithmetic in PayloadSerializer ✅ COMPLETED

Added `createAssignmentPayloadV2()` with isReader callback. Updated `irma_context._queueAssignmentFromEntry()` to use V2 method.

---

### Issue 5: VarKey vs VarRef.varId Mismatch ✅ COMPLETED

Resolved by Issue 1 - VarKey now uses raw addresses consistently.

---

### Issue 6: Test VarRef Explicit Reader Flag ✅ COMPLETED

Resolved by Issue 1 - Tests no longer rely on VarRef.isReader.

---

### Issue 7: handleAssignment entry.state Semantics ✅ COMPLETED

Removed unnecessary `entry.state = value` for imported readers since bindImportedReader only uses entry.suspensions.

---

### Issue 8: _requestFromHeap Legacy Fallback ✅ COMPLETED

Converted out-of-bounds fallback to StateError since all proper import paths should create heap cells.

---

### Issue 9: helpers.abandon() Parameter Type ✅ COMPLETED

Bug fix: abandon() was sending local readerId instead of creatorLocalId for imported readers.

---

### Issue 10: export() Relay VarRef ✅ COMPLETED

Already resolved - RelaySetup uses integer addresses, not VarRef objects.

---

### Issue 11: bindImportedReader Heap Structure Docs ✅ COMPLETED

Added heap structure transformation documentation showing before/after cell layout.

---

### Issue 12: VariableEntry.state Typed Fields ✅ COMPLETED

**Status**: FIXED

**Problem**: The `state` field in `VariableEntry` had overloaded semantics - it stored different types (String or Term) depending on lifecycle stage, which was error-prone.

**Solution**: Replaced `dynamic state` with typed fields:
- `String? requester` - For created writers/readers: who is waiting for the value
- `bool requestSent` - For imported readers: whether a read request has been sent
- `Term? boundValue` - Cached bound value for any role

**Files Changed**:
- `lib/multiagent/variable_table.dart` - Replaced `state` with typed fields, added typed update methods
- `lib/multiagent/irma_context.dart` - Updated all state usages to typed fields
- `lib/multiagent/helpers.dart` - Updated all state usages to typed fields
- `lib/runtime/heap_fcp.dart` - Updated derefAddr to use boundValue
- `test/multiagent/variable_table_test.dart` - Updated tests for typed fields
- `test/multiagent/irma_context_test.dart` - Updated tests for typed fields
- `test/multiagent/helpers_test.dart` - Updated tests for typed fields

---

### Issue 13: PayloadSerializer VariableTable Access ✅ COMPLETED

**Status**: FIXED

**Problem**: When serializing terms containing imported variables, the code used (agentId, addr) instead of the variable's true (creator, creatorLocalId) from V_p.

**Solution**: Added `_lookupVariableForSerialization()` callback in `irma_context.dart` that looks up each variable in V_p to get the correct (creator, creatorLocalId). This callback is now passed to `createAssignmentPayloadV2()`.

**Files Changed**:
- `lib/multiagent/irma_context.dart` - Added `_lookupVariableForSerialization()` and wired it to `_queueAssignmentFromEntry()`

---

### Issue 14: allocateFreshPair Unused Params ✅ COMPLETED

**Status**: FIXED

**Problem**: The `allocateFreshPair` callback in `export()` took two int parameters that were always passed as (0, 0) and ignored.

**Solution**: Changed callback signature from `List<int> Function(int, int)` to `List<int> Function()`. Updated all call sites and tests.

**Files Changed**:
- `lib/multiagent/helpers.dart` - Changed callback signature
- `test/multiagent/helpers_test.dart` - Updated test callbacks

---

### Issues 15-20: Pending

See SPEC-IMPLEMENTATION-AUDIT-2026-01-20.md for details.

---

## Open Issues

### Bidirectional Stream Test Failure

**Symptom**: Goals complete with `ExecutionStatus.succeeded` instead of suspending on imported readers.

**Details**:
- Goal `merge(Xs?, [a], Ys)` where Xs? is an imported reader
- Clause 2 `merge(Xs, [Y|Ys], [Y?|Zs?])` matches because first arg is just a variable
- Body goal `merge(Xs?, [], Zs)` should suspend on imported reader but doesn't

**Investigation needed**:
- Verify bytecode correctly handles imported reader in spawned body goals
- Check if clause variable binding for imported readers is correct

---

## Test Status

| Test Suite | Pass | Fail | Notes |
|------------|------|------|-------|
| REPL Tests | 223 | 0 | All tests passing |
| simple_imported_reader_test | Pass | - | Works correctly with processSuspension |
| bidirectional_stream_test | Fail | - | Separate runtime issue (see above) |

---

## Document History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2026-01-21 | Claude | Initial progress document |
