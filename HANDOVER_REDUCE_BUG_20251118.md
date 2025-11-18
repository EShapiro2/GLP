# Handover Report: Metainterpreter Reduce Bug Investigation
**Date**: 2025-11-18
**Session**: Debugging metainterpreter execution suspension

## Executive Summary

**Bug Found**: Metainterpreter `reduce` clauses do not bind their input arguments after execution, causing subsequent goals to suspend on unbound variables.

**Root Cause**: Mode conversion during HEAD unification creates bindings to unbound reader variables instead of binding to actual values. The reduce clauses return `true` without executing the actual logic, so these reader variables never get bound.

**Status**: Bug isolated and root cause identified. Fix requires architectural decision.

---

## Bug Demonstration

### Isolated Test Case

**File**: `/Users/udi/GLP/udi/glp/test_reduce.glp`

```prolog
% Direct clause (for comparison)
partition([X|Xs], A, Smaller?, [X?|Larger?]) :-
    A? < X? | partition(Xs?, A?, Smaller, Larger).

% Reduce clause (exhibits bug)
reduce(partition([X|Xs], A, Smaller?, [X?|Larger?]), partition(Xs?, A?, Smaller, Larger)) :-
    A? < X? | true.
```

**Test Query**: `reduce(partition([3,4],1,A,B), R).`

**Expected**: After reduce commits, A should be bound (similar to direct partition execution)

**Actual**: A remains unbound

### Evidence

```
Query: reduce(partition([3,4],1,A,B), R).

REPL creates:
  queryVarWriters = {A: 1003, B: 1004, R: 1005}

Commit applies σ̂w:
  [DEBUG COMMIT] Bound varId 1003: W1003[addr=6] = R1006?, R1003[addr=7] = R1006?
  
Result:
  DEBUG DISPLAY: A = X3, isBound=false, rawValue=null
  A = <unbound>
```

---

## Root Cause Analysis

### The Binding Chain

1. **Query creates**: A = varId 1003 (writer/reader pair in heap)
2. **HEAD matching**: partition structure contains W1003 in position 3
3. **Mode conversion** (PC 214): Unbound writer W1004 at position 4 → creates `./2(R1007?, R1008?)`
4. **Output structure created**: Contains references to query variables
5. **Commit applies σ̂w**: `W1003 → R1006?` (binding to UNBOUND READER)
6. **Check isBound(1003)**: Returns FALSE because R1006 is unbound

### Why R1006 Stays Unbound

In **direct partition execution**:
- Clause body executes: `partition(Xs?, A?, Smaller, Larger)`
- Writers Smaller and Larger get bound by recursive execution
- Eventually binds to concrete values ([], [3,4], etc.)

In **reduce clause execution**:
- Clause body is just: `true`
- No recursive execution occurs
- R1006 is created during mode conversion but never bound
- W1003 points to R1006, but R1006 points nowhere (unbound)

### Code Flow

**Commit code** (`/Users/udi/GLP/glp_runtime/lib/runtime/commit.dart:56-61`):
```dart
// FCP lines 233/303: Bind BOTH cells to dereferenced value
heap.cells[wAddr].content = value;  // W1003 cell = R1006
heap.cells[wAddr].tag = CellTag.ValueTag;
heap.cells[rAddr].content = value;  // R1003 cell = R1006
heap.cells[rAddr].tag = CellTag.ValueTag;
```
✓ Correctly binds both cells to R1006

**isBound check** (`/Users/udi/GLP/glp_runtime/lib/runtime/heap_fcp.dart:119-123`):
```dart
bool isFullyBound(int varId) {
  final (wAddr, _) = varTable[varId]!;
  final result = derefAddr(wAddr);  // Follows W1003 → R1006 → (unbound)
  return result is! VarRef;  // Returns false (R1006 is VarRef)
}
```
✓ Correctly returns false (variable bound to unbound variable = not fully bound)

---

## Comparison: Direct vs Metainterpreter

### Direct Execution: `partition([3,4],1,A,B).`

```
Goal 1: partition([3,4], 1, X2, X3)
  → spawns partition([4], 1, X15, X17)
  → A (X2) eventually = []
  → B (X3) eventually = [3, 4]
  
✓ Result: A=[], B=[3,4]
```

### Metainterpreter: `reduce(partition([3,4],1,A,B), R).`

```
Goal 1: reduce(partition([3,4], 1, X3, X4), X5)
  HEAD: partition(R1001?, R1002?, W1003, W1004)
  MODE CONVERSION: W1004 → ./2(R1007?, R1008?)
  COMMIT σ̂w: W1003 → R1006?, W1004 → ./2(R1007?, R1008?), ...
  BODY: true (no execution!)
  
✗ Result: A=unbound, B=[3|X8?] (partially bound)
```

---

## Technical Details

### Mode Conversion Behavior

**Location**: `/Users/udi/GLP/glp_runtime/lib/bytecode/runner.dart:437-455`

When HeadStructure/UnifyStructure encounters an unbound writer where a structure is expected:

1. Creates fresh tentative structure: `_TentativeStruct(functor, arity, ...)`
2. Fills with fresh variables during WRITE mode traversal
3. Adds to σ̂w: `cx.sigmaHat[writerVarId] = structure`
4. At commit: binds writer to structure in heap

✓ **This is correct** for normal clauses where the body executes and binds the fresh variables

✗ **This fails** for reduce clauses where body=`true` and fresh variables never get bound

### Variable Creation in REPL

**Location**: `/Users/udi/GLP/udi/glp_repl.dart:388-410`

When building query structures:
- Checks `queryVarWriters` for existing variables
- Reuses if found, creates fresh pair if not
- Correctly registers writers in `queryVarWriters`

✓ **REPL code is correct** - creates and tracks variables properly

---

## Why This Matters

### Impact on Metainterpreters

The qsort.glp metainterpreter pattern:
```prolog
run(G) :- reduce(G, G'), run(G').
reduce(original_goal, transformed_goal) :- guards | true.
```

This pattern is **fundamental to GLP metainterpreters** and is used extensively in FCP AM literature. If it doesn't work, metainterpreters cannot be implemented in GLP!

### Full Quicksort Failure

```
> run(quicksort([1,3,4],X)).
  → Suspends at goal 10012: reduce(qsort(X12?,X9,[1|X15?]), X34)
  → X12 is unbound reader created by mode conversion
  → Never gets bound because reduce body is just `true`
```

---

## Possible Solutions

### Option 1: Change Reduce Clause Design

Instead of:
```prolog
reduce(partition([X|Xs], A, Smaller?, [X?|Larger?]), 
       partition(Xs?, A?, Smaller, Larger)) :-
    A? < X? | true.
```

Use actual execution (but this defeats the purpose of a metainterpreter):
```prolog
reduce(partition([X|Xs], A, Smaller?, [X?|Larger?]), 
       partition(Xs?, A?, Smaller, Larger)) :-
    A? < X? | partition(Xs?, A?, Smaller, Larger).  % Actually execute!
```
❌ **Not viable** - defeats metainterpreter purpose

### Option 2: Fix Mode Conversion for Metainterpreter Pattern

When mode conversion happens in HEAD of a clause with body=`true`:
- Don't create fresh unbound variables
- Instead, create writer-reader pairs that are PRE-UNIFIED
- OR: bind the input argument writer directly to the created structure (not to intermediate readers)

This requires detecting the metainterpreter pattern and special-casing it.

### Option 3: Post-Commit Unification

After reduce clause commits, unify the query argument variables with the clause-created variables:
- Query has W1003 (A)
- Clause creates structure with R1006
- After commit: explicitly unify W1003 with the position in the structure

This would require tracking which query args map to which clause positions.

### Option 4: Consult FCP AM Paper

Check how FCP AM actually handles metainterpreters:
- Read `/Users/udi/GLP/docs/1-s2.0-0743106689900113-main.pdf`
- Check FCP source: `/Users/udi/Dropbox/Concurrent Prolog/FCP/Savannah` or https://github.com/EShapiro2/FCP
- See if there's a special mechanism for this

---

## Files Modified (Debug Only)

### `/Users/udi/GLP/glp_runtime/lib/runtime/commit.dart`
- Line 61: Added debug output for commit bindings
- **Action**: Remove debug print before final commit

### `/Users/udi/GLP/glp_runtime/lib/bytecode/runner.dart`
- Lines 1961-1964: Added σ̂w debug output
- Lines 2053, 2058: Added commit application debug
- Lines 424-430: Added isBound debug output
- **Action**: Remove debug prints before final commit

### `/Users/udi/GLP/udi/glp_repl.dart`
- Line 230: Added queryVarWriters debug
- Lines 392, 396, 400, 405: Added structure building debug
- **Action**: Remove debug prints before final commit

### `/Users/udi/GLP/udi/glp/test_reduce.glp`
- Created isolated test case
- **Action**: Keep for testing

---

## Next Steps

1. **Architectural Decision Required**: Which solution approach to take?
   - Consult Claude Chat for FCP AM guidance
   - Check FCP implementation for metainterpreter handling

2. **If Option 4 (FCP AM research)**:
   - Read FCP paper section on metainterpreters
   - Check FCP source code for reduce/meta patterns
   - Identify if there's a special unification mechanism

3. **If Option 2 (Fix mode conversion)**:
   - Detect when clause body is just `true`
   - Modify mode conversion to create pre-unified structures
   - OR: bind input writers directly to output positions

4. **Testing Protocol**:
   - Isolated: `reduce(partition([3,4],1,A,B), R).` → A should be bound
   - Full: `run(quicksort([1,3,4],X)).` → should complete
   - Test suites: `dart test` + `bash run_repl_tests.sh`

5. **Clean up debug output** once fix is implemented

---

## References

- **Specs**: `/Users/udi/GLP/docs/glp-bytecode-v216-complete.md`
- **FCP Paper**: `/Users/udi/GLP/docs/1-s2.0-0743106689900113-main.pdf`
- **FCP Source**: `/Users/udi/Dropbox/Concurrent Prolog/FCP/Savannah`
- **Protocol**: `/Users/udi/GLP/CLAUDE.md` - Mandatory debugging protocol

---

## Key Insight

**The metainterpreter reduce pattern is NOT a runtime bug - it's a fundamental incompatibility between:**
1. Mode conversion creating unbound reader variables in HEAD
2. Reduce clauses not executing (body=`true`) to bind those variables
3. FCP AM expecting these patterns to work

Either:
- The runtime needs special handling for this pattern, OR
- The metainterpreter pattern needs to be different than we thought, OR
- FCP AM has a mechanism we haven't discovered yet

**Recommendation**: Research FCP AM metainterpreter implementation before attempting a fix.

