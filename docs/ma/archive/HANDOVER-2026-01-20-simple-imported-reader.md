# Handover: Simple Imported Reader Test and addr+1 Bug

**Date:** 2026-01-20
**Status:** In Progress - Blocked on spec clarification
**Branch:** `claude/fix-stream-test-output-4Xlvr`

---

## 1. Summary

This session created a simpler test case for imported readers after the bidirectional stream test failed. The simpler test exposed a critical bug: **35+ places in runner.dart assume `reader addr = writer addr + 1`**, which is invalid for imported readers.

---

## 2. Tests Created/Executed

### 2.1 Simple Imported Reader Test

**File:** `glp_runtime/test/multiagent/simple_imported_reader_test.dart`

**Setup:**
- @1: Has writer X, runs `p(X)`
- @2: Has imported reader X?, runs `q(X?)`

**Program:**
```glp
p([a,b]).
q([_|Xs]) :- q(Xs?).
q([]).
```

**Expected Flow:**
1. @2 runs first, suspends on unbound X?
2. @2 sends READ_REQUEST to @1
3. @1 runs, binds X = [a,b], sends ASSIGNMENT to @2
4. @2 receives assignment, reactivates
5. @2 processes list recursively until empty

**Result:** PARTIAL SUCCESS
- READ_REQUEST/ASSIGNMENT protocol works correctly
- HP sync bug was found and fixed (see Section 3.1)
- Test crashes with serialization issue after HP fix

### 2.2 Shared Variable Test (Reference - Working)

**File:** `glp_runtime/test/multiagent/shared_variable_test.dart`

**Program:**
```glp
p(a).
q(a).
```

**Result:** PASSES - This simpler test with atoms (not lists) works correctly.

### 2.3 Bidirectional Stream Test (Original Failing Test)

**File:** `glp_runtime/test/multiagent/bidirectional_stream_test.dart`

**Program:**
```glp
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
merge([], [], []).
```

**Result:** FAILS - Both isolates complete immediately with wrong output `[_8, _9]`

---

## 3. Bugs Found

### 3.1 HP Sync Bug (FIXED)

**Location:** `glp_runtime/lib/runtime/heap_fcp.dart`, function `bindImportedReader`

**Problem:** Used `cells.length` instead of `HP++` to get address for new value cell, causing HP to get out of sync with actual cell count.

**Fix Applied:**
```dart
// BEFORE (buggy):
final valueCellAddr = cells.length;

// AFTER (fixed):
final valueCellAddr = HP++;
```

**Commit:** `3289bf5 test(ma): add simple imported reader test exposing HP sync bug`

### 3.2 addr+1 Assumption Bug (NOT FIXED - NEEDS SPEC)

**Location:** `glp_runtime/lib/bytecode/runner.dart` - 35+ places

**Problem:** Code assumes that for any writer at address N, its paired reader is at address N+1. This is true for locally allocated variables (`allocateVariable()` returns `(writerAddr, writerAddr+1)`), but **NOT true for imported readers** which are allocated with `allocateImportedReader()` as single cells.

**Examples of problematic code:**
```dart
// Line 617
struct.args[cx.S] = VarRef(existingValue + 1);  // reader addr = writer addr + 1

// Line 1293
cx.argSlots[targetSlot] = VarRef(targetWriterId + 1);  // reader addr = writer addr + 1

// Line 1420
struct.args[cx.S] = VarRef(addr + 1);  // reader addr
```

**All 35+ occurrences found by grep:**
```
287:      final readerAddr = isWriter ? finalAddr + 1 : finalAddr;
617:                struct.args[cx.S] = VarRef(existingValue + 1);
1293:                    cx.argSlots[targetSlot] = VarRef(targetWriterId + 1);
1418-1477: Multiple occurrences in HeadVariable handling
1536-1590: Multiple occurrences in nested structure handling
1630-1746: Multiple occurrences in sigmaHat handling
1835-1971: Multiple occurrences in guard/body processing
2049-2148: Multiple occurrences in Commit handling
2277-2278: In tentative structure conversion
2649-2688: Multiple occurrences in spawn handling
```

**Impact:** When runner.dart tries to compute a reader address from an imported reader's address using +1, it gets garbage (an unrelated cell or out of bounds).

**Status:** BLOCKED - Need spec clarification before implementing fix.

---

## 4. Questions for Next Session

1. What spec document covers the heap architecture for multiagent/imported variables?
   - Is it `docs/heap/heap-pointer-architecture-spec.md`?
   - Is there an irmaGLP-specific heap spec?

2. What is the correct approach to fix the addr+1 assumption?
   - Option A: Add `tryReaderForWriter(writerAddr)` method to heap that returns null for imported writers
   - Option B: Track writer→reader mapping explicitly
   - Option C: Something else per spec

3. Should all 35+ places be fixed, or only specific code paths that handle imported readers?

---

## 5. Baseline Test Status

As of session end:
- **REPL tests:** 222/223 pass (1 timing failure - pre-existing)
- **Unit tests:** All pass

---

## 6. Files Modified This Session

| File | Change |
|------|--------|
| `glp_runtime/lib/runtime/heap_fcp.dart` | HP sync fix in `bindImportedReader` |
| `glp_runtime/test/multiagent/simple_imported_reader_test.dart` | New test file |

---

## 7. How to Run Tests

```bash
# Baseline REPL tests
cd /home/user/GLP/glp_runtime
export PATH="/home/user/dart-sdk/bin:$PATH"
bash ../test/full_run_repl_tests.sh

# Simple imported reader test
cd /home/user/GLP/glp_runtime
dart test test/multiagent/simple_imported_reader_test.dart

# Shared variable test (working reference)
dart test test/multiagent/shared_variable_test.dart

# Bidirectional stream test (failing)
dart test test/multiagent/bidirectional_stream_test.dart
```

---

## 8. Key Insight

The fundamental issue is that **imported readers break the local paired-allocation invariant**. In single-agent GLP:
- `allocateVariable()` always creates writer at N, reader at N+1
- Code can safely assume reader = writer + 1

In multiagent GLP:
- Imported readers are allocated alone via `allocateImportedReader()`
- There is NO local writer for an imported reader
- The writer exists on a REMOTE heap
- Code CANNOT assume reader = writer + 1

The fix must either:
1. Check cell type before assuming +1 relationship
2. Use abstraction methods that handle both cases
3. Track relationships explicitly in a map

**DO NOT proceed with implementation until spec clarifies the correct approach.**

---

## 9. References

- Previous handover: `docs/ma/HANDOVER-2026-01-20-bidirectional-stream-v2.md`
- Heap spec: `docs/heap/heap-pointer-architecture-spec.md`
- irmaGLP spec: `docs/ma/irmaGLP-spec.md`
