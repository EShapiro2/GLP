# Handover: madGLP OutputObserver Bug

**Date**: 2026-01-31
**Status**: Bug identified, fix pending
**Branch**: main (after FCP merge)

## Summary

madGLP multiagent communication is broken: Alice sends `connect(bob)` but Bob never receives the message. The root cause is a bug in `OutputObserver` that passes a reader address to `onBind()` which expects a writer address.

## Background

### What Works
- REPL execution is correct: `NetOut = [msg(bob, intro(alice, X50)) | X106]`
- FCP bidirectional pointers merged successfully
- madGLP agents initialize correctly (suspended on 3 blocking readers)
- Message injection wakes goals and execution proceeds

### What Fails
- Dart `OutputObserver` callback never fires when GLP binds the net output stream
- Bob's window never receives Alice's message

## Root Cause Analysis

### The Bug

In `external_io.dart`, `OutputObserver._observeNext()`:

```dart
void _observeNext() {
  if (_closed) return;

  // Register callback for when reader is bound
  heap.onBind(_currentReaderId, (Term value) {  // BUG: passes reader address
    ...
  });
}
```

But `HeapFCP.onBind()` expects a **writer** address:

```dart
void onBind(int writerAddr, void Function(Term) callback) {
  if (isFullyBound(writerAddr)) {
    ...
  }
  _bindCallbacks[writerAddr] = callback;  // Registers on writer address
}
```

The callback is registered on the reader address, but bindings happen on the writer address. So the callback never fires.

### Spec vs Implementation Mismatch

**Spec Section 7.1** defines:
```dart
int writerForReader(int readerAddr) {
  final cell = cells[readerAddr];
  assert(cell.tag == CellTag.RoTag);
  return (cell.content as Pointer).targetAddr;
}
```

**Implementation** has different names:
- `tryWriterForReader()` → returns `int?` (handles imported readers)
- `getWriterForReader()` → alias for `tryWriterForReader()`

The spec's `writerForReader()` with assert doesn't exist. The implementation diverged to handle imported readers gracefully.

### Spec Gap

The spec (Section 7.1) doesn't account for imported readers (Section 10). For imported readers, there's no local writer - the reader contains a `VariableEntry` not a `Pointer`. The spec's `writerForReader()` would crash.

## Required Fixes

### Fix 1: OutputObserver must convert reader to writer

```dart
void _observeNext() {
  if (_closed) return;

  // Convert reader to writer per spec Section 7.1
  final writerAddr = heap.getWriterForReader(_currentReaderId);
  if (writerAddr == null) {
    // Imported reader - no local writer to observe
    return;
  }

  heap.onBind(writerAddr, (Term value) {
    ...
  });
}
```

### Fix 2: Reconcile spec and implementation naming

Either:
1. Add `writerForReader()` to implementation (asserts local reader, returns `int`)
2. Or update spec Section 7.1 to show nullable return for imported readers

Recommendation: Have both methods:
- `writerForReader(int readerAddr)` → `int` (asserts local, per spec)
- `tryWriterForReader(int readerAddr)` → `int?` (safe for imported readers)

## Files Involved

- `/Users/udi/Grassroots/GLP/glp_runtime/lib/runtime/external_io.dart` - OutputObserver bug
- `/Users/udi/Grassroots/GLP/glp_runtime/lib/runtime/heap_fcp.dart` - missing `writerForReader()`
- `/Users/udi/Grassroots/GLP/docs/heap/heap-pointer-architecture-spec.md` - Section 7.1

## Test Verification

After fix, run madGLP:
1. Alice sends `connect(bob)`
2. Bob should receive `befriend(alice, Resp?)` message in his window
3. Bob can respond with `decision(yes, alice, Resp)`
4. Alice should see `connected(bob)`

## Related Context

- FCP merge completed: `dda7e11 feat(heap): implement FCP bidirectional pointers refactoring`
- REPL tests: 221/223 passing
- The scheduler `onReduction` callback bug was fixed earlier in this session
