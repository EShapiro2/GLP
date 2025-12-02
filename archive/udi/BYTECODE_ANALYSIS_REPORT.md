# Bytecode Analysis Report: Quicksort Bug Investigation

**Date:** 2025-11-16
**Session:** Claude Code bytecode investigation
**For:** Claude Web

---

## Summary

Successfully added `:bytecode` command to REPL and analyzed bytecode for qsort clauses. Found that the compiler is emitting correct opcodes per spec, but identified the likely bug location in the runtime's UnifyReader WRITE mode handler.

---

## Work Completed

### 1. Added `:bytecode` Command to REPL ✅

**File:** `/Users/udi/GLP/udi/glp_repl.dart`

**Command usage:**
```
GLP> filename.glp
GLP> :bytecode   (or :bc)
```

**Implementation:** Lines 88-103, displays all loaded programs' bytecode instructions

---

## Bytecode Analysis Results

### Test Files Created

1. **test_partition_base.glp** - `partition([], _, [], []).`
2. **test_qsort_base.glp** - `qsort([], Rest?, Rest).`
3. **test_qsort_recursive.glp** - Full qsort recursive clause with structure in body

### Partition Base Case Bytecode

**Source:** `partition([], _, [], []).`

```
Bytecode for test_partition_base.glp:
============================================================
     0: Instance of 'Label'
     1: Instance of 'ClauseTry'
     2: Instance of 'HeadNil'          ← First arg: []
     3: Instance of 'UnifyVoid'        ← Second arg: _
     4: Instance of 'HeadNil'          ← Third arg: []
     5: Instance of 'HeadNil'          ← Fourth arg: []
     6: Instance of 'Commit'
     7: Instance of 'Proceed'
     8: Instance of 'Label'
     9: Instance of 'NoMoreClauses'
```

**Analysis:** Correct - base case is just a fact with no body, uses HeadNil for all three `[]` arguments.

---

### Qsort Base Case Bytecode

**Source:** `qsort([], Rest?, Rest).`

```
Bytecode for test_qsort_base.glp:
============================================================
     0: Instance of 'Label'
     1: Instance of 'ClauseTry'
     2: Instance of 'HeadNil'          ← First arg: []
     3: Instance of 'GetReaderVariable' ← Second arg: Rest? (reader)
     4: Instance of 'GetWriterValue'   ← Third arg: Rest (writer, same variable)
     5: Instance of 'Commit'
     6: Instance of 'Proceed'
     7: Instance of 'Label'
     8: Instance of 'NoMoreClauses'
```

**Analysis:** Correct - matches `[]` with HeadNil, loads Rest? as reader, matches third arg with same variable's writer value.

---

### Qsort Recursive Case Bytecode (THE KEY FINDING)

**Source:**
```prolog
qsort([X | Unsorted], Sorted?, Rest?) :-
    number(X?) |
    partition(Unsorted?, X?, Smaller, Larger),
    qsort(Smaller?, Sorted, [X? | Sorted1?]),  ← Structure with shared variable
    qsort(Larger?, Sorted1, Rest).
```

```
Bytecode for test_qsort_recursive.glp:
============================================================
     0: Instance of 'Label'
     1: Instance of 'ClauseTry'
     2: Instance of 'HeadStructure'    ← HEAD: [X | Unsorted]
     3: unify_writer(0)                ← Extract X
     4: unify_writer(1)                ← Extract Unsorted
     5: Instance of 'GetReaderVariable' ← Load Sorted?
     6: Instance of 'GetReaderVariable' ← Load Rest?
     7: put_reader(X0, A0)             ← GUARD: number(X?)
     8: Instance of 'Guard'
     9: Instance of 'Commit'
    10: put_reader(X1, A0)             ← BODY: partition(Unsorted?, ...)
    11: put_reader(X0, A1)             ← partition(..., X?, ...)
    12: put_writer(X4, A2)             ← partition(..., Smaller, ...)
    13: put_writer(X5, A3)             ← partition(..., Larger)
    14: Instance of 'Spawn'            ← Spawn partition/4
    15: put_reader(X4, A0)             ← qsort(Smaller?, ...)
    16: put_writer(X2, A1)             ← qsort(..., Sorted, ...)
    17: Instance of 'PutStructure'     ← *** Building [X? | Sorted1?] ***
    18: unify_reader(0)                ← *** X? in structure HEAD ***
    19: unify_reader(6)                ← *** Sorted1? in tail ***
    20: Instance of 'Spawn'            ← Spawn qsort/3
    21: put_reader(X5, A0)             ← qsort(Larger?, ...)
    22: put_writer(X6, A1)             ← qsort(..., Sorted1, ...)
    23: put_writer(X3, A2)             ← qsort(..., Rest)
    24: Instance of 'Spawn'            ← Spawn qsort/3
    25: Instance of 'Proceed'
    26: Instance of 'Label'
    27: Instance of 'NoMoreClauses'
```

---

## Critical Finding: Lines 17-19

**The structure `[X? | Sorted1?]` in the BODY:**
```
17: Instance of 'PutStructure'     ← Enter WRITE mode to build structure
18: unify_reader(0)                ← Place X? (reader of X from HEAD)
19: unify_reader(6)                ← Place Sorted1? (reader of fresh writer)
```

### What This Means

1. **Variable X appears in THREE places:**
   - Line 3: HEAD structure extraction (`unify_writer(0)`) - X gets bound in HEAD
   - Line 11: BODY goal argument (`put_reader(X0, A1)`) - X? passed to partition
   - Line 18: BODY structure building (`unify_reader(0)`) - X? inside `[X? | Sorted1?]`

2. **All three use the SAME variable index (0)** - correct per SRSW

3. **Line 18 uses `unify_reader` not `set_reader`** - initially suspected this was wrong

---

## Spec Verification

### Checked: `/Users/udi/GLP/docs/glp-bytecode-v216-complete.md`

**Section 8: "These instructions fill in structure arguments after head_structure or put_structure."**

**Section 8.2 reader Xi** (lines 232-250):
- Specifies behavior **"In READ mode:"** and **"In WRITE mode:"**
- `unify_reader` works in BOTH modes
- **Not** separate opcodes for HEAD vs BODY

**WRITE mode behavior (lines 240-248):**
```
- If Xi is unbound (first use): allocate fresh variable ID,
  create VarRef(newId, isReader: true), store at H and in Xi
- If Xi contains VarRef (subsequent use): extract varId from existing VarRef,
  create VarRef(varId, isReader: true), store at H
- If Xi contains ground term (ConstTerm/StructTerm): allocate fresh variable ID,
  bind it to the ground term, create VarRef(newId, isReader: true), store at H
```

**Example from spec (lines 246-247):**
> "In `qsort([X|Xs], S, [X?|S1?])`, after HEAD matches X=1, BODY builds `[X?|S1?]`
> by creating a fresh variable V bound to 1, storing `VarRef(V, isReader: true)` in the list"

### Conclusion: Bytecode is CORRECT per spec

The compiler is correctly emitting `unify_reader` after `PutStructure`. The opcode checks the mode and behaves appropriately.

---

## The Actual Bug Location

**The bug is in the RUNTIME implementation of UnifyReader's WRITE mode**, not in the compiler.

### Hypothesis

When `unify_reader(0)` executes in WRITE mode (line 18):
1. It should check `cx.clauseVars[0]` (which contains X's writer ID from HEAD)
2. If X is bound to a constant (e.g., 1), it should allocate a fresh variable, bind it to 1, and store a reader of that fresh variable
3. **BUT** - if `clauseVars[0]` is not properly initialized or has wrong type, it might allocate a fresh unbound variable instead

This would create:
- Original X (from HEAD) → W1009
- Fresh variable (from UnifyReader fallback) → W1014 (unbound)
- Structure contains reader of W1014 instead of reader of fresh variable bound to X's value
- Result: W1009 → W1014 chain (writer-to-writer binding) and W1014 is unbound

---

## Next Steps for Claude Web

### 1. Investigate UnifyReader Handler

**File:** `/Users/udi/GLP/glp_runtime/lib/bytecode/runner.dart`

Search for `if (op is UnifyReader)` or similar handler.

**Check:**
- How does it handle WRITE mode?
- Does it have the same fallback allocation bug as SetReader (lines 2198-2207)?
- Does it properly:
  - Check `cx.clauseVars[varIndex]` for existing writer ID?
  - Handle ground terms (ConstTerm/StructTerm) per spec lines 245-247?
  - Create fresh variable bound to ground term when needed?

### 2. Compare with Spec Section 8.2

Ensure UnifyReader WRITE mode implements:
- Line 241: First use → allocate fresh VarRef
- Line 242: Subsequent use → extract varId, create VarRef with isReader: true
- Line 245-247: Ground term → allocate fresh, bind to term, create reader

### 3. Fix Similar to SetReader

If UnifyReader has the same fallback issue, apply similar fix but with proper ground term handling.

---

## Files Modified This Session

1. **`/Users/udi/GLP/udi/glp_repl.dart`** - Added `:bytecode` command (lines 88-103)
2. **`/Users/udi/GLP/udi/glp/test_partition_base.glp`** - Test file
3. **`/Users/udi/GLP/udi/glp/test_qsort_base.glp`** - Test file
4. **`/Users/udi/GLP/udi/glp/test_qsort_recursive.glp`** - Test file

---

## Test Results

**Unit tests:** 86/86 passing ✅
**REPL tests:** 18/23 passing (5 quicksort/insertion_sort failures remain)

**REPL build:** `2025-11-16T22:00:00Z (Add :bytecode command for investigation)`

---

## Key Takeaway

**The bytecode is CORRECT.** The bug is in the runtime's UnifyReader WRITE mode implementation, which likely has incorrect fallback allocation when `clauseVars[varIndex]` doesn't contain the expected VarRef from HEAD phase processing.

The fix should ensure UnifyReader properly handles:
1. Reusing existing writer IDs from clauseVars
2. Creating fresh variables bound to ground terms when needed (spec lines 245-247)
3. NOT allocating fresh unbound variables as a fallback
