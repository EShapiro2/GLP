# Handover Report: Heterogeneous Argument Registers Implementation
**Date**: 2025-11-18
**Task**: Implement heterogeneous argument registers (VarRef, ConstTerm, StructTerm)
**Status**: PARTIAL - Core implementation complete, 1 test regression vs baseline

---

## Summary

Implemented heterogeneous argument registers to support passing constants and structures as arguments, not just variable IDs. The specification required `A1-An` registers to hold heterogeneous `Term` types (VarRef, ConstTerm, StructTerm), but the code only supported `Map<int,int>` for variable IDs.

**Test Results:**
- **Baseline** (commit 22ce508): 24/29 REPL tests passing (82%)
- **After implementation** (commit 5d3d62b): 23/29 REPL tests passing (79%)
- **Regression**: 1 test (Test 26: Metainterpreter shared variable test)
- **Improvements**: Fixed 4 quicksort tests that broke during implementation

---

## What Was Done

### 1. Core Data Structure Changes

**File**: `glp_runtime/lib/bytecode/runner.dart`

#### CallEnv Class (lines 72-88)
Changed from separate reader/writer maps to unified heterogeneous Term map:

```dart
// BEFORE (baseline):
class CallEnv {
  final Map<int, int> writerBySlot;
  final Map<int, int> readerBySlot;
  CallEnv({Map<int, int>? writers, Map<int, int>? readers})
      : writerBySlot = writers ?? {},
        readerBySlot = readers ?? {};
  int? w(int slot) => writerBySlot[slot];
  int? r(int slot) => readerBySlot[slot];
}

// AFTER (current):
class CallEnv {
  final Map<int, Term> argBySlot;
  CallEnv({Map<int, Term>? args}) : argBySlot = args ?? <int, Term>{};
  Term? arg(int slot) => argBySlot[slot];
  void update(Map<int, Term> newArgs) {
    argBySlot.clear();
    argBySlot.addAll(newArgs);
  }
}
```

#### RunnerContext (line 133)
```dart
// BEFORE:
final Map<int, int> argWriters = {};
final Map<int, int> argReaders = {};

// AFTER:
final Map<int, Term> argSlots = {};
```

### 2. PUT Instructions Updated

All PUT instructions now store `Term` types in `argSlots`:

#### PutWriter (lines 1957-1985)
Stores `VarRef(varId, isReader: false)` instead of bare int.

#### PutReader (lines 1987-2021)
Stores `VarRef(varId, isReader: true)` instead of bare int.

#### PutConstant (lines 2038-2046)
**Critical implementation detail**: Constants are NOT stored directly as ConstTerm. Instead:
1. Allocate fresh variable
2. Bind it to the constant value
3. Store `VarRef(varId, isReader: true)` to the bound variable

This matches baseline behavior where constants are passed via bound variables.

#### PutStructure (lines 2049-2085)
Structures built incrementally, stored when complete.

### 3. GET Instructions Updated

All GET instructions (GetWriterVariable, GetReaderVariable, GetWriterValue, GetReaderValue) updated to use pattern matching on `Term` types:

```dart
final arg = _getArg(cx, op.slot);
if (arg is VarRef && !arg.isReader) {
  // Handle writer VarRef
} else if (arg is VarRef && arg.isReader) {
  // Handle reader VarRef
} else if (arg is ConstTerm) {
  // Handle constant
} else if (arg is StructTerm) {
  // Handle structure
}
```

### 4. Structure Completion Bug Fix

**Major bug discovered and fixed**: When structures were built using `UnifyReader`/`UnifyWriter` instructions (as opposed to `SetWriter`), the completion code was binding the structure to the heap but NOT storing it in `argSlots`.

This caused arguments to be dropped in calls like:
```prolog
qsort(Smaller?, Sorted, [X? | Sorted1?])
                        ^^^^^^^^^^^^^^^^ This argument was missing!
```

**Fix applied** (lines 1651-1664 and 1527-1532):
Added argSlots storage to both `UnifyReader` and `UnifyWriter` BODY phase structure completion:

```dart
// When structure is complete
if (cx.S >= struct.args.length) {
  // ... bind to heap ...

  // Store VarRef to bound writer in argSlots
  final targetSlot = cx.clauseVars[-2];
  if (targetSlot is int && targetSlot >= 0 && targetSlot < 10) {
    cx.argSlots[targetSlot] = VarRef(targetWriterId, isReader: true);
    cx.clauseVars.remove(-2);
  }
}
```

### 5. Spawn and Requeue Updated

Both instructions now copy `argSlots` instead of separate reader/writer maps:

```dart
// Spawn (line 2378-2381)
final newEnv = CallEnv(
  args: Map<int, Term>.from(cx.argSlots),
);

// Requeue (line 2450)
cx.env.update(Map<int, Term>.from(cx.argSlots));
```

### 6. Guard Evaluation Updated

Guard instruction (lines 2498-2565) already had partial support for argSlots but needed fixes to `_dereferenceWithTracking`.

#### _dereferenceWithTracking Bug Fix (lines 3277-3290)

**Bug**: When dereferencing a reader VarRef, code was calling `isWriterBound(readerId)` directly instead of first getting the paired writer.

**Fix**:
```dart
// BEFORE:
if (t.isReader) {
  final varId = t.varId;
  if (cx.rt.heap.isWriterBound(varId)) {  // WRONG! varId is reader ID
    ...
  }
}

// AFTER:
if (t.isReader) {
  final readerId = t.varId;
  final wid = cx.rt.heap.writerIdForReader(readerId);  // Get paired writer first
  if (wid != null && cx.rt.heap.isWriterBound(wid)) {
    final boundValue = cx.rt.heap.valueOfWriter(wid);
    ...
  }
}
```

### 7. Supporting Files Updated

**File**: `udi/glp_repl.dart` (lines 205-215, 320-374)
- Changed from `Map<int,int> readers/writers` to `Map<int,Term> argSlots`
- Updated `_setupArgument` function to store VarRef terms instead of bare IDs

**File**: `glp_runtime/lib/runtime/scheduler.dart` (lines 61-76)
- Updated `_formatGoal` to use `env.arg()` instead of separate `writerBySlot`/`readerBySlot`

**File**: `docs/glp-bytecode-v216-complete.md` (lines 62-91)
- Added implementation notes documenting the use of existing VarRef types

---

## Test Results Breakdown

### Passing Tests (23/29)

**Basic functionality:**
1. Hello World ✅
2. Simple Unification ✅
3. Merge [1,2,3] and [a,b] ✅
4. Merge Standalone ✅
6. Clause Lookup ✅
7. Simple Run ✅
8. Merge via Metainterpreter (SRSW fix) ✅

**Arithmetic guards:**
10. Addition 5+3 ✅
11. Multiplication 4*7 ✅
12. Compound (2*3)+4 ✅

**Direct sorting (NOT via metainterpreter):**
14. Quicksort empty list ✅
15. Quicksort single element ✅ (was broken, now fixed)
16. Insertion sort empty list ✅
17. Insertion sort single element ✅
18. Insertion sort two elements ✅
19. Quicksort two elements [1,2] ✅ (was broken, now fixed)
20. Quicksort larger list ✅ (was broken, now fixed)
21. Quicksort five elements ✅ (was broken, now fixed)
22. Quicksort with non-number fails ✅
23. Quicksort with unbound tail suspends ✅

**Metainterpreter tests (some passing):**
24. Metainterpreter: merge([a],[b],X) ✅
25. Metainterpreter: merge([a],[b,c,d],X) ✅
27. Metainterpreter: merge chain with shared vars ✅

### Failing Tests (6/29)

**Metainterpreter-specific failures:**
5. Merge with Reader ❌ (SRSW violation in test file)
9. Insertion Sort via Metainterpreter ❌ (goal explosion)
13. Structure Demo ❌ (unknown issue)
26. Metainterpreter: run2(X) - shared variable test ❌ **[REGRESSION from baseline]**
28. Metainterpreter: quicksort([],X) ❌
29. Metainterpreter: quicksort([1],X) ❌

---

## Known Issues

### 1. Test 26 Regression (Critical)

Test 26 was passing at baseline but now fails. This is the only regression introduced by the heterogeneous argument work.

**Test**: Metainterpreter shared variable test
**Status**: Unknown - needs investigation

### 2. Metainterpreter Quicksort Failures (Tests 28, 29)

**Symptom**: When running `run(quicksort([],X))` via metainterpreter, X remains unbound even though it should be bound to [].

**Analysis from debug trace**:
```
10001: run(qsort([],W1002,[])) :- clause(qsort([]?,W1002,[])?, W1007), run(R1007?)
10002: clause(qsort([],W1002,[]), W1007) :- true
10003: run(true) :- true
X = W1002, isBound=false  <-- WRONG! Should be bound to []
```

**Root cause**: The clause `qsort([],Rest?,Rest)` has Rest appearing twice (once as reader arg2, once as writer arg3). When `clause(qsort([],W1002,[]), Body)` matches this, it should unify:
- Arg2: W1002 (writer) with Rest? (reader)
- Arg3: [] (constant) with Rest (writer)

This should bind Rest to [], and propagate to W1002. But this isn't happening.

**Note**: This appears to be a metainterpreter-specific issue, as direct calls to `quicksort([],X)` work correctly.

### 3. Other Metainterpreter Issues (Tests 5, 9, 13)

These were failing at baseline and continue to fail. Not regressions from this work.

---

## Commits Made

1. **22ce508** - Checkpoint: before heterogeneous argument registers implementation
2. **a5d07a3** - WIP: Heterogeneous argument registers - partial implementation
3. **8d9c587** - Heterogeneous argument registers - major progress (68 compilation errors → 0)
4. **03cca92** - Complete heterogeneous argument registers implementation
5. **f1af585** - fix: Update glp_repl and scheduler for heterogeneous CallEnv
6. **5d3d62b** - fix: Store structures in argSlots for UnifyReader/UnifyWriter (current HEAD)

---

## Technical Decisions Made

### 1. VarRef vs Direct Storage

**Decision**: Store VarRef(varId, isReader) instead of bare variable IDs.

**Rationale**:
- Provides type safety
- Makes reader/writer mode explicit
- Consistent with existing Term hierarchy

### 2. Constants as Bound Variables

**Decision**: Constants are passed as VarRef to bound heap variables, not direct ConstTerm.

**Rationale**:
- Matches baseline behavior
- Allows consistent dereferencing logic
- Heap binding ensures proper value propagation

### 3. Structure Storage Timing

**Decision**: Structures are bound to heap immediately when complete, and VarRef(writerId, isReader:true) is stored in argSlots.

**Rationale**:
- Allows immediate activation of suspended goals
- VarRef provides consistent access pattern
- Matches baseline semantics for structure passing

### 4. Pattern Matching for Type Dispatch

**Decision**: Use Dart's `is` operator for type checking instead of property-based checks.

**Rationale**:
- More idiomatic Dart
- Type-safe
- Clearer code intent

---

## Next Steps / Recommendations

### Immediate Priority

1. **Investigate Test 26 regression** - This is the only test that regressed from baseline. Find root cause and fix.

2. **Debug metainterpreter quicksort** (Tests 28, 29) - The issue with `Rest?,Rest` pattern in clause heads needs investigation. This may be a fundamental issue with how shared variables are handled in HEAD matching when the same variable appears with different modes.

### Medium Priority

3. **Review other metainterpreter failures** (Tests 5, 9, 13) - These were failing before but should be understood and documented.

4. **Remove debug output** - Some debug print statements remain in the code (search for "DEBUG METAINT", "DEBUG MetaInterp").

5. **Clean up dead code** - The implementation left some commented code and old patterns that could be removed.

### Low Priority

6. **Performance testing** - The heterogeneous Term approach may have performance implications vs bare ints. Consider benchmarking.

7. **Documentation** - Update architecture docs to reflect the new argument passing model.

---

## Code Locations Reference

**Key files modified:**
- `glp_runtime/lib/bytecode/runner.dart` - Main VM implementation (2500+ lines)
- `udi/glp_repl.dart` - REPL implementation
- `glp_runtime/lib/runtime/scheduler.dart` - Goal scheduling
- `docs/glp-bytecode-v216-complete.md` - Specification updates

**Critical functions:**
- `CallEnv` class (runner.dart:72-88) - Argument environment
- PUT instructions (runner.dart:1957-2085) - Argument setup
- GET instructions (runner.dart:1069-1315) - Argument retrieval
- `UnifyReader`/`UnifyWriter` structure completion (runner.dart:1651-1664, 1527-1532) - Structure argument storage
- `_dereferenceWithTracking` (runner.dart:3277-3340) - Term dereferencing

**Test suite:**
- `udi/run_repl_tests.sh` - REPL test harness (29 tests total)

---

## Questions for Next Session

1. **Is the Test 26 regression acceptable?** Or should it be fixed before proceeding?

2. **Are the metainterpreter quicksort failures** (28, 29) acceptable as known limitations, or should they block other work?

3. **Should the heterogeneous argument work be considered complete**, or does it need to support the metainterpreter path fully?

4. **Is there spec clarification needed** for how shared variables like `Rest?,Rest` should work in clause heads?

---

## Final Status

✅ **Heterogeneous argument registers implemented and working** for direct predicate calls
⚠️ **Partial regression** in metainterpreter path (1 test worse than baseline, but 4 tests recovered)
📊 **Net result**: 23/29 tests passing (79%) vs baseline 24/29 (82%)
🔧 **Recommendation**: Investigate Test 26 regression before declaring complete

---

*End of handover report*
