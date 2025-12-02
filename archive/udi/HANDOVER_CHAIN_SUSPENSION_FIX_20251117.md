# Chain Suspension Fix - Final Handover
**Date:** 2025-11-17 (Evening Session)
**Status:** Partial fix implemented, goals now reactivate but execution still incomplete
**Test Results:** Still 11/23 passing (47%), but with correct reactivation behavior

---

## Summary

Implemented the "suspend on final variable in chain" fix as suggested by user. Goals now correctly suspend on the ultimate unbound variable when variable chains exist (W1009→R1014). Reactivation is now happening (1 goal reactivated in quicksort trace), but tests still don't pass - indicating a different issue beyond suspension.

---

## Changes Made

### 1. Added `_finalUnboundVar()` Helper Function

**Location:** `glp_runtime/lib/bytecode/runner.dart:200-216`

```dart
/// Find the final unbound variable in a chain (FCP: follow var→var bindings)
/// If readerId's writer is bound to another unbound variable, return that variable's ID
/// Otherwise return the original readerId
int _finalUnboundVar(RunnerContext cx, int readerId) {
  final wid = cx.rt.heap.writerIdForReader(readerId);
  if (wid == null) return readerId;

  final (wAddr, _) = cx.rt.heap.varTable[wid]!;
  final derefResult = cx.rt.heap.derefAddr(wAddr);

  if (derefResult is VarRef) {
    // Bound to another unbound variable - return that one
    return derefResult.varId;
  }

  return readerId;
}
```

**Purpose:** When a reader's writer is bound to another unbound variable (W1→R2 where R2 is unbound), return R2's ID so we suspend on the final variable in the chain.

### 2. Updated All HEAD Instruction Suspension Points

**Files Modified:** `glp_runtime/lib/bytecode/runner.dart`

**Locations Updated:**
- Line 441: HeadConstant
- Line 718-719: HeadStructure
- Line 1002-1003: UnifyReader (Si case)
- Line 1036-1037: GetVariable
- Line 1207-1208: GetReaderVariable
- Line 2955-2960: HeadNil (with debug output)
- Line 3027-3029: HeadList

**Pattern Applied:**
```dart
// BEFORE:
pc = _suspendAndFail(cx, arg.readerId!, pc);

// AFTER:
final suspendOnVar = _finalUnboundVar(cx, arg.readerId!);
pc = _suspendAndFail(cx, suspendOnVar, pc);
```

**Example (HeadNil with debugging):**
```dart
if (wid == null || !bound) {
  // Find the final unbound variable in the chain (FCP: suspend on ultimate target)
  final suspendOnVar = _finalUnboundVar(cx, arg.readerId!);
  if (suspendOnVar != arg.readerId) {
    print('[DEBUG HeadNil] → Chain detected: R${arg.readerId} → R$suspendOnVar, suspending on final');
  }
  print('[DEBUG HeadNil] → SUSPENDING on unbound reader R$suspendOnVar');
  pc = _suspendAndFail(cx, suspendOnVar, pc);
  continue;
}
```

---

## Verification: The Fix Works!

### Quicksort Trace Evidence

**Before Fix:**
```
[DEBUG HeadNil] → SUSPENDING on unbound reader R1009
[TRACE NoMoreClauses] Goal 10002 suspending:
  U (blocked readers): [1009]
```
Goal suspended on R1009 (which was bound to R1014).

**After Fix:**
```
[DEBUG HeadNil] → Chain detected: R1009 → R1014, suspending on final
[DEBUG HeadNil] → SUSPENDING on unbound reader R1014
[TRACE NoMoreClauses] Goal 10002 suspending:
  U (blocked readers): [1014]
  → Added to R1014 suspension list (addr=29)
```
Goal now suspends on R1014 (the final unbound variable).

**Reactivation Happens:**
```
[TRACE Commit FCP] Applying σ̂w to heap (2 bindings):
  W1014 → Const(nil)
  W1016 → Const(nil)
[TRACE Commit FCP] Processing suspension list for R1014:
[TRACE Commit FCP]   Activated 1 goal(s)
[TRACE Commit FCP] Total goals reactivated: 1
[TRACE Post-Commit] Enqueueing 1 reactivated goal(s):
```

✅ **Goal 10002 IS reactivated when W1014→nil happens!**

---

## Remaining Issue

Despite correct reactivation, quicksort still fails with `X = <unbound>, → 10 goals`.

**Possible Causes:**

1. **Reactivated goal fails again:** Goal 10002 wakes up, tries clauses again, but fails for a different reason

2. **Termination condition:** Execution stops before all goals complete (e.g., goal limit, infinite loop detection)

3. **Variable binding propagation:** Even though W1014 is bound to nil, the binding might not be propagating back through the chain correctly

4. **Different bug:** The reactivation fix revealed a different underlying issue

**Evidence Needed:**
- Full trace of goal 10002's execution after reactivation
- Check what happens when it retries clauses
- Verify variable binding after reactivation

---

## Test Results

### Same 11/23 Tests Passing

No regression, no improvement in test count. But behavior is more correct:

#### ✅ Still Passing (11):
1, 2, 3, 4, 6, 7, 8, 16, 20, 21, 23

#### ❌ Still Failing (12):
5, 9, 10, 11, 12, 13, 14, 15, 17, 18, **19**, 22

Test 19 (quicksort [1,2]) now shows proper reactivation in trace, but still doesn't complete.

---

## Files Modified

### Code Files

1. **glp_runtime/lib/bytecode/runner.dart**
   - Added `_finalUnboundVar()` helper (lines 200-216)
   - Updated 7 suspension call sites to use helper

2. **glp_runtime/lib/runtime/heap_fcp.dart** (from earlier session)
   - Added suspension processing to `bindVariable()`
   - Fixed cycle bug: reader cell initially null, not pointing back to writer

3. **glp_runtime/lib/runtime/commit.dart** (unchanged this session)
   - Already had correct suspension processing

### Specification Files

4. **docs/glp-runtime-spec.txt**
   - Added section "Variable Chain Suspension (FCP Wake-and-Retry)" at line 363
   - Documents `_finalUnboundVar()` helper and chain suspension semantics
   - Updated "Heap Storage (FCP Two-Cell Design)" section (lines 88-125)
   - Fixed reader cell initialization: null content instead of back-pointer to writer
   - Added "Critical Design Fix" explaining why reader→writer pointer creates cycle

---

## Key Insights

### 1. User's Solution Was Correct

The proposed fix to "suspend on final variable in chain" was exactly right. The implementation works as designed - goals now suspend on the correct variable and wake when that variable is bound.

### 2. FCP Wake-and-Retry Mechanism Works

The trace shows:
1. Goal suspends on R1014
2. W1014 gets bound to nil
3. Suspension list processed
4. Goal reactivated

This is textbook FCP behavior.

### 3. But Something Else is Wrong

The fact that reactivation happens but tests still fail means there's a different bug. Possibilities:
- Goal retries but matches wrong clause
- Variable binding state is corrupted
- Scheduler issue (goals not completing)
- Missing bindings somewhere

### 4. Chain Tracking is NOT Needed

Earlier I thought we'd need `Map<int, Set<int>> chainedVars` to track dependencies. The wake-and-retry mechanism makes this unnecessary - much simpler and more elegant.

---

## Next Steps (For Next Session)

### Immediate Investigation

1. **Get full trace of goal 10002 after reactivation**
   ```bash
   # Add more trace points in runner to see what happens after reactivation
   # Specifically: which clauses does it try? Why does it fail/suspend again?
   ```

2. **Check variable binding state**
   ```bash
   # After W1014→nil and goal reactivates, what is W1009's value?
   # Is the chain properly resolved?
   ```

3. **Simplify test case**
   ```glp
   % Minimal test for variable chain suspension
   test(X) :- p(X).
   p(Y?) :- Y = [], q(Y?).
   q([]).
   ```
   Test with: `test(X)` - should bind X to [].

### Potential Fixes

**Option A: Chain Resolution in Commit**

Maybe when we bind W1014→nil, we should also update any writers that point to R1014:
```dart
// In applySigmaHatFCP, after binding a variable:
// Check if any other writers point to this reader
// Update them transitively
```

**Option B: Dereferencing in HeadNil**

Maybe when checking a reader, we should fully dereference first:
```dart
// When checking R1009:
// Follow W1009 → R1014 → nil
// See that it's ultimately bound to nil
```

**Option C: Something Completely Different**

Maybe the issue is unrelated to suspension/reactivation. Could be:
- Clause selection logic
- σ̂w application
- BODY execution
- Scheduler fairness

---

## Code Quality Notes

### Good

- Helper function `_finalUnboundVar()` is clean and reusable
- Debug output shows exactly what's happening
- No code duplication (all suspension sites use same helper)
- FCP comments reference design

### Could Improve

- Many debug print statements (should be behind debug flag)
- Some suspension sites have slightly different patterns (inconsistent)
- No unit tests for chain suspension specifically

---

## Performance Impact

**Minimal:**
- `_finalUnboundVar()` is O(1) - just one dereference call
- Called only when suspending (rare compared to successful execution)
- No new data structures or allocations

---

## Git Status

### Modified (Not Committed):
```
M glp_runtime/lib/bytecode/runner.dart  (chain suspension fix)
M glp_runtime/lib/runtime/heap_fcp.dart (bindVariable suspension processing)
```

### Recommended Commit:
```
fix: Suspend on final variable in chain (FCP wake-and-retry)

When a reader's writer is bound to another unbound variable (W1→R2),
suspend on R2 instead of R1. This ensures goals wake when the ultimate
variable is bound, following FCP's wake-and-retry semantics.

Changes:
- Add _finalUnboundVar() helper to find ultimate unbound variable
- Update all HEAD instruction suspension points to use helper
- Add debug output showing chain detection

Test results: Goals now reactivate correctly (trace confirmed)
Still 11/23 passing - reactivation works but execution incomplete

Related to: #quicksort-bug
```

---

## Session Timeline

1. **Investigated commit.dart** - initially thought fix was needed there
2. **User corrected me** - pointed out the real issue is in HEAD instructions
3. **Discovered timeline** - W1009→R1014 happens BEFORE goal 10002 suspends
4. **Found HeadNil code** - checks isWriterBound correctly, but suspends on wrong variable
5. **Implemented fix** - `_finalUnboundVar()` helper and updated 7 call sites
6. **Compiled and tested** - reactivation now works, but tests still fail
7. **Analyzed traces** - confirmed reactivation happening, identified new mystery

---

## Questions for User

1. Should we investigate why the reactivated goal doesn't complete? Or is there a known issue?

2. The trace shows "→ 10 goals" at the end. Is this the expected number, or should it be fewer?

3. Are there any FCP-specific semantics for what happens after a goal is reactivated? Does it restart from the beginning of the procedure, or resume at a specific point?

4. Would a simpler test case help debug? E.g., just testing variable chain suspension without the complexity of quicksort?

---

## Comparison with Previous Analysis

### What I Got Wrong Earlier

I thought we needed:
- Variable chain tracking (`Map<int, Set<int>> chainedVars`)
- Post-commit chain resolution phase
- Complex transitive binding logic

**Actually needed:**
- Simple helper function to find final variable
- Update suspension call sites
- Let FCP wake-and-retry handle the rest

Much simpler!

### What User Got Right

- Identified the real problem (HEAD instruction suspension)
- Proposed the correct solution (suspend on final variable)
- Explained FCP wake-and-retry mechanism

Perfect diagnosis!

---

## Remaining Mystery

**Why do tests still fail even with correct reactivation?**

The chain suspension fix is working (reactivation confirmed in trace), but something prevents completion. This suggests the bug has multiple parts:

1. ✅ **Fixed:** Suspension on wrong variable
2. ❓ **Unknown:** Reactivated goal behavior

Need more investigation to solve part 2.

---

**End of Report**

**Total Session Time:** ~4 hours
**Lines Changed:** ~50 lines (mostly adding `_finalUnboundVar` calls)
**Key Achievement:** Reactivation now works correctly
**Next Challenge:** Figure out why reactivated goals don't complete
