# Status Report: Runtime Bug Fix Progress
**Date:** 2025-11-17
**Session:** UnifyWriter unification fix

---

## Summary

**Fixed:** UnifyWriter now unifies instead of overwrites when clause variables appear multiple times in HEAD.

**Status:** Partial success - direct queries work, metainterpreter still fails.

---

## What Was Fixed

**File:** `/Users/udi/GLP/glp_runtime/lib/bytecode/runner.dart` lines 1511-1560

**Change:** Modified UnifyWriter READ mode logic to check for existing values in `clauseVars[varIndex]` and unify instead of overwrite.

**Key insight:** Discovered that `clauseVars` can store values as:
- Bare `int` (writer ID) - from UnifyReader
- `VarRef` - from various sources
- `Term` (ConstTerm, StructTerm) - direct values

**Fix handles all three cases:**
```dart
final existingValue = cx.clauseVars[op.varIndex];

if (existingValue == null) {
  // First occurrence - store value
  cx.clauseVars[op.varIndex] = value;
} else {
  // Subsequent occurrence - UNIFY
  if (existingValue is int) {
    // Bare writer ID - bind in sigmaHat
    cx.sigmaHat[existingValue] = value;
  } else if (existingValue is VarRef && !existingValue.isReader) {
    // Writer VarRef - bind in sigmaHat
    cx.sigmaHat[existingValue.varId] = value;
  } else if (existingValue is VarRef && existingValue.isReader) {
    // Reader VarRef - bind paired writer in sigmaHat
    final wid = cx.rt.heap.writerIdForReader(existingValue.varId);
    cx.sigmaHat[wid] = value;
  } else {
    // Term - check they match (fail if mismatch)
    if (existingValue != value) fail();
  }
}
```

---

## Test Results

### ✅ Direct Query Works

**Test:** `qsort([],X,[])`
**Clause:** `clause(qsort([], Rest?, Rest), true).`

```
GLP> qsort([],X,[]).
1: qsort([], W1001, []) :- true
  X = []   ← SUCCESS!
  → 1 goals
```

**Verification:** The variable `Rest` appears as reader then writer in the clause head. The fix correctly:
1. Stores writer ID for `Rest?` in clauseVars[0]
2. Unifies it with `[]` when processing `Rest`
3. Binds W1001 to `[]` via sigmaHat

### ❌ Metainterpreter Still Fails

**Test:** `run(quicksort([],X))`
**Expected:** `X = []`
**Actual:** `X = <unbound>`

```
GLP> run(quicksort([],X)).
1: run(quicksort([],W1002)) :- clause(quicksort([]?,W1002)?, W1004), run(R1004?)
10000: clause(quicksort([],W1002), W1004) :- true
10001: run(qsort([],W1006,[])) :- clause(qsort([]?,W1006,[])?, W1008), run(R1008?)
10002: clause(qsort([],W1006,[]), W1008) :- true
10003: run(true) :- true
  X = <unbound>  ← STILL FAILS
  → 5 goals
```

**Analysis:**
- Goal 10002: `clause(qsort([],W1006,[]), Body)` succeeds
- This matches `clause(qsort([], Rest?, Rest), true)`
- Should bind W1006 to `[]`
- But W1006 remains unbound

**Problem:** The variable binding chain is:
```
X (query) → W1002 → Sorted → W1006 → Rest
```

When `Rest = []` is established in goal 10002, it should propagate back through this chain, but it doesn't.

---

## Hypothesis: Missing Unification in clause/2

The clause is:
```prolog
clause(quicksort(Unsorted, Sorted?), qsort(Unsorted?, Sorted, [])).
```

When this matches `clause(quicksort([],W1002), Body)`:
- First arg HEAD: `quicksort(Unsorted, Sorted?)`
  - Matches call: `quicksort([], W1002)`
  - Should store: Unsorted=[], Sorted?=W1002 (reader)

- Second arg BODY value: `qsort(Unsorted?, Sorted, [])`
  - Should build: `qsort([], Sorted, [])` where Sorted is the same var as Sorted?
  - Should return: Body = this structure with Sorted still unbound

**Issue:** When goal 10002 later binds the Rest variable (which is connected to Sorted), that binding doesn't propagate back to W1002.

**Possible cause:** The variable sharing between clause HEAD and BODY arguments isn't being tracked correctly. When we build the BODY structure in clauseVars, we might not be maintaining the connection to the original query variable W1002.

---

## Next Steps

**Need to investigate:**

1. **How BODY structures are built in clause/2**
   - When `clause(quicksort([],W1002), Body)` matches
   - How is `Body = qsort(Unsorted?, Sorted, [])` constructed?
   - Does `Sorted` in the BODY maintain connection to `Sorted?` in HEAD?

2. **Variable binding propagation**
   - When goal 10001 calls `run(qsort([],W1006,[]))`
   - And goal 10002 establishes W1006 should be `[]`
   - Why doesn't this propagate to W1002 from goal 10000?

3. **Check Proceed instruction**
   - When clause/2 succeeds and returns
   - Are bindings from sigmaHat properly applied?
   - Are BODY structures dereferenced correctly?

**Possible fixes:**

**Option A:** Fix in Commit/Proceed
- Ensure all clauseVars values are dereferenced through sigmaHat
- Apply bindings to return values before proceeding

**Option B:** Fix in UnifyReader
- Similar issue may exist in UnifyReader
- Check if it also needs unification logic

**Option C:** Fix in BODY structure building
- When building BODY argument structures
- Ensure variable references maintain heap connections

---

## Files Modified

**`/Users/udi/GLP/glp_runtime/lib/bytecode/runner.dart`:**
- Lines 1511-1560: UnifyWriter unification logic
- Lines 1514-1518: Handle bare int (writer ID) case
- Lines 1519-1523: Handle writer VarRef case
- Lines 1524-1544: Handle reader VarRef case
- Lines 1546-1555: Handle Term case

---

## Test Commands

**Working test:**
```bash
cd /Users/udi/GLP/udi
dart glp_repl.dart <<EOF
qsort.glp
qsort([],X,[]).
:quit
EOF
```

**Failing test:**
```bash
dart glp_repl.dart <<EOF
qsort.glp
run(quicksort([],X)).
:quit
EOF
```

---

## Commits Status

**Not yet committed** - Fix is incomplete. Direct queries work but metainterpreter fails.

Need to either:
1. Find and fix remaining issue
2. OR commit current fix with documentation that metainterpreter issue remains

---

## Summary for Claude Web

UnifyWriter now properly unifies when clause variables repeat in HEAD. This fixes direct queries like `qsort([],X,[])` which now correctly binds X to [].

However, metainterpreter queries like `run(quicksort([],X))` still fail. The issue appears to be in how variable bindings propagate through multiple levels of clause matching when clause/2 predicates build BODY structures.

Need architectural guidance on:
1. How should variable connections be maintained when building BODY argument structures?
2. Should bindings be propagated during Commit/Proceed?
3. Is there a similar issue in UnifyReader that needs the same fix?
