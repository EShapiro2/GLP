# FCP Bidirectional Pointers Implementation Plan

**Date**: 2026-01-31
**Status**: READY FOR IMPLEMENTATION
**Spec**: `docs/heap/heap-pointer-architecture-spec.md` v3.2
**Branch**: `feature/fcp-bidirectional-pointers`

---

## Overview

This plan describes how to update the GLP runtime to match the FCP bidirectional pointer architecture specified in `heap-pointer-architecture-spec.md` v3.2.

**Key Change**: Unbound writers now point to their paired readers (FCP pattern), eliminating all `+1` address arithmetic.

---

## Prerequisites

Before starting:
1. Create branch from latest `main`: `git checkout -b feature/fcp-bidirectional-pointers`
2. Run baseline tests and record results:
   ```bash
   cd glp_runtime && dart test > /tmp/baseline-dart.txt 2>&1
   cd .. && bash test/full_run_repl_tests.sh > /tmp/baseline-repl.txt 2>&1
   ```
3. Read and understand:
   - `docs/heap/heap-pointer-architecture-spec.md` v3.2 (the target)
   - `docs/DISCIPLINE.md` Section 1.11 (FCP reference)
   - FCP source: `/Users/udi/Dropbox/Concurrent Prolog/FCP/Merged EMULATOR/`

---

## Phase 1: Core Heap Changes

### 1.1 Add WriterContent Class

**File**: `glp_runtime/lib/runtime/heap_fcp.dart`

Add the compound content type for unbound writers with suspensions:

```dart
/// Compound content for unbound writer with suspensions.
/// Preserves the reader pointer while holding suspension list.
class WriterContent {
  final int readerAddr;  // Pointer to paired reader
  SuspensionListNode? suspensions;

  WriterContent(this.readerAddr, [this.suspensions]);
}
```

### 1.2 Update Variable Allocation

**File**: `glp_runtime/lib/runtime/heap_fcp.dart`

Change `allocateVariable()` to create bidirectional pointers:

**Before**:
```dart
// Writer cell: null (unbound)
// Reader cell: points to writer
```

**After**:
```dart
// Writer cell: points to reader (FCP pattern)
// Reader cell: points to writer
```

### 1.3 Add readerForWriter() Method

**File**: `glp_runtime/lib/runtime/heap_fcp.dart`

Add method to find paired reader by following writer's pointer:

```dart
/// Find the paired reader for an unbound writer.
/// Returns null if writer is bound.
int? readerForWriter(int writerAddr) {
  final cell = cells[writerAddr];
  if (cell.content is int) {
    // Direct pointer to reader
    final target = cell.content as int;
    if (cells[target].tag == CellTag.RoTag) {
      return target;
    }
    return null;  // Bound to something else
  }
  if (cell.content is WriterContent) {
    return (cell.content as WriterContent).readerAddr;
  }
  return null;
}
```

### 1.4 Update Deref Logic

**File**: `glp_runtime/lib/runtime/heap_fcp.dart`

Update `deref()` to detect unbound writers via the bidirectional pointer pattern:

- If writer points to reader AND reader points back to writer → unbound
- If writer has `WriterContent` → unbound with suspensions
- Otherwise → bound, follow the pointer

### 1.5 Update Suspension Handling

**File**: `glp_runtime/lib/runtime/heap_fcp.dart` or `suspend_ops.dart`

When adding suspensions to unbound writer:
- If content is `int` (pointer to reader): wrap in `WriterContent`
- If content is `WriterContent`: add to suspension list

---

## Phase 2: Remove +1 Arithmetic

### 2.1 Find All Occurrences

Search for patterns:
```bash
grep -n "+ 1\|+1\|- 1\|-1" glp_runtime/lib/runtime/runner.dart
grep -n "+ 1\|+1\|- 1\|-1" glp_runtime/lib/runtime/heap_fcp.dart
grep -n "+ 1\|+1\|- 1\|-1" glp_runtime/lib/runtime/external_io.dart
grep -n "+ 1\|+1\|- 1\|-1" glp_runtime/lib/multiagent/*.dart
```

### 2.2 Categories of +1 Usage

**Category A: Finding reader from writer**
```dart
// OLD: int readerAddr = writerAddr + 1;
// NEW: int? readerAddr = heap.readerForWriter(writerAddr);
```

**Category B: Finding writer from reader** (already correct)
```dart
// Uses heap.writerForReader(readerAddr) - no change needed
```

**Category C: Allocation-time (acceptable)**
```dart
// In allocateVariable(): readerAddr = writerAddr + 1;
// This is OK - it's the allocation itself, not navigation
```

**Category D: Non-variable arithmetic** (ignore)
```dart
// Loop counters, array indices, etc. - not related to heap navigation
```

### 2.3 Files to Update

1. **`runner.dart`** (~30+ occurrences)
   - Focus on HEAD/BODY variable handling
   - Clause variable allocation
   - Unification operations

2. **`external_io.dart`** (already partially fixed)
   - `ExternalChannel` uses explicit addresses
   - `buildChannelTerm` may still have issues

3. **`glp_repl.dart`**
   - Answer extraction
   - Variable display

4. **`isolate_manager.dart`**
   - Variable serialization for multiagent

---

## Phase 3: Testing

### 3.1 Unit Tests

Run and fix:
```bash
cd glp_runtime && dart test
```

Focus on:
- `test/heap/pointer_architecture_test.dart`
- `test/heap/binding_pointer_test.dart`
- `test/heap/varref_pointer_test.dart`

### 3.2 REPL Tests

Run and fix:
```bash
bash test/full_run_repl_tests.sh
```

### 3.3 Multiagent Tests

Run and fix:
```bash
cd glp_runtime && dart test test/multiagent/
```

### 3.4 Flutter Build

Verify:
```bash
cd glp_multiagent && flutter build macos
```

---

## Phase 4: Cleanup

### 4.1 Update Bytecode Spec

**File**: `docs/glp-bytecode-v216-complete.md`

Update from old `VarRef(varId, isReader)` format to `VarRef(addr)` format.

### 4.2 Remove Dead Code

Remove any helper functions that computed reader/writer via arithmetic.

### 4.3 Add Assertions

Add debug assertions that catch any remaining arithmetic:
```dart
assert(!_usesArithmetic, 'Address arithmetic detected');
```

---

## Completion Checklist

- [ ] Phase 1: Core heap changes
  - [ ] WriterContent class added
  - [ ] allocateVariable() creates bidirectional pointers
  - [ ] readerForWriter() method added
  - [ ] deref() handles new patterns
  - [ ] Suspension handling preserves reader pointer

- [ ] Phase 2: Remove +1 arithmetic
  - [ ] runner.dart cleaned
  - [ ] external_io.dart cleaned
  - [ ] glp_repl.dart cleaned
  - [ ] multiagent files cleaned

- [ ] Phase 3: All tests pass
  - [ ] Dart unit tests
  - [ ] REPL tests
  - [ ] Multiagent tests
  - [ ] Flutter build

- [ ] Phase 4: Cleanup
  - [ ] Bytecode spec updated
  - [ ] Dead code removed
  - [ ] Debug assertions added

- [ ] Create PR to main

---

## Notes for Implementer

1. **Commit frequently** - after each small change that compiles
2. **Run tests after each phase** - don't batch test runs
3. **If stuck, check FCP source** - the reference implementation is at `/Users/udi/Dropbox/Concurrent Prolog/FCP/Merged EMULATOR/`
4. **Follow DISCIPLINE.md** - especially sections 1.9 (spec authority) and 1.11 (FCP reference)

---

## Document History

| Date | Changes |
|------|---------|
| 2026-01-31 | Initial plan created |
