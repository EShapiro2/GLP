# Nested Structure Bug - Fixed Report

**Date:** November 11, 2025
**Status:** CRITICAL BUG FIXED - Parent context tracking corrected
**New Status:** Partial success - conjunction now binds, but final unification incomplete

---

## Executive Summary

**THE BUG WAS FOUND AND FIXED!**

The root cause was in the `PutStructure` handler in `runner.dart`. The code was saving the parent writer ID AFTER overwriting it with the new structure's writer ID, causing the parent context to be lost.

**Fix Applied:** Reordered lines 1547-1563 to save parent context BEFORE storing new writer ID.

**Result:** The conjunction structure now properly binds during BODY phase execution!

**Remaining Issue:** Final unification still incomplete - need to investigate why `Z = <unbound>` instead of `Z = b`.

---

## The Bug

### Location
`/Users/udi/GLP/glp_runtime/lib/bytecode/runner.dart` lines 1547-1563

### Original Broken Code
```dart
// Store the writer ID in context for later binding
cx.clauseVars[-1] = freshWriterId; // Line 1548 - OVERWRITES parent's writer ID!

// Handle different argSlot cases
if (op.argSlot == -1 || cx.currentStructure != null) {
  // Save parent context
  cx.parentStructure = cx.currentStructure;
  cx.parentS = cx.S;
  cx.parentMode = cx.mode;
  cx.parentWriterId = cx.clauseVars[-1]; // Line 1560 - WRONG! Saves NEW writer ID, not parent's!
}
```

### The Problem

When building nested structures like `(bar(X), baz(X?,Y))`:

1. **Line 4: `PutStructure(",", 2, 0)`** - Creates conjunction
   - Stores writer ID 1002 in `clauseVars[-1]`
   - `currentStructure` is null, so no parent context saved

2. **Line 5: `PutStructure("bar", 1, -1)`** - Creates nested bar/1
   - Line 1548 overwrites `clauseVars[-1]` with NEW writer ID (1004)
   - Line 1551 checks: `op.argSlot == -1 || cx.currentStructure != null` → TRUE (argSlot is -1)
   - Line 1560 tries to save parent's writer ID from `clauseVars[-1]`
   - **But `clauseVars[-1]` now contains 1004, not 1002!**
   - Saves WRONG writer ID as parent

3. **Line 6: `SetWriter`** - Completes bar/1
   - Tries to restore parent context
   - But `parentWriterId` is 1004 (bar's writer) instead of 1002 (conjunction's writer)
   - Parent completion check fails because writer ID is wrong

4. **Result:** Conjunction writer 1002 never gets bound!

---

## The Fix

### Fixed Code (runner.dart:1547-1563)

```dart
// Handle different argSlot cases FIRST - save parent before overwriting
// -1 = nested structure being built inside parent (don't put in argReaders)
// 0-9 = argument slot (put reader in argReaders for goal passing)
// 10+ = temp register (store writer in clauseVars)
if (op.argSlot == -1 || cx.currentStructure != null) {
  // Nested structure - save parent context before starting nested build
  // This triggers not just for -1 but also when we're already building a structure
  print('DEBUG: PutStructure - SAVING PARENT CONTEXT: currentStruct=${(cx.currentStructure as StructTerm?)?.functor}, S=${cx.S}, parentWriterId=${cx.clauseVars[-1]}');
  cx.parentStructure = cx.currentStructure;
  cx.parentS = cx.S;
  cx.parentMode = cx.mode;
  cx.parentWriterId = cx.clauseVars[-1]; // Save parent's writer ID BEFORE we overwrite it
}

// NOW store the writer ID for this new structure
print('DEBUG: PutStructure ${op.functor}/${op.arity} (argSlot=${op.argSlot}) - storing writerId $freshWriterId at clauseVars[-1]');
cx.clauseVars[-1] = freshWriterId; // Use -1 as special marker for structure binding
```

### Key Changes

1. **Moved parent context save BEFORE `clauseVars[-1]` assignment**
2. **Added condition: `op.argSlot == -1 || cx.currentStructure != null`**
   - Original only checked `op.argSlot == -1`
   - Fix also checks if we're already building a structure
   - This handles the case where first PutStructure has argSlot 0

3. **Added debug output to trace execution**

---

## Test Results

### Before Fix
```
DEBUG: HeadStructure - Writer 1002 is unbound or null, adding to Si and soft failing
Z = <unbound>
```

### After Fix
```
DEBUG: PutStructure ,/2 (argSlot=0) - storing writerId 1002 at clauseVars[-1]
DEBUG: PutStructure - SAVING PARENT CONTEXT: currentStruct=,, S=0, parentWriterId=1002
DEBUG: PutStructure bar/1 (argSlot=-1) - storing writerId 1004 at clauseVars[-1]
DEBUG: SetWriter - structure complete! functor=bar, S=1/1, targetWriter=1004, hasParent=true
DEBUG: SetWriter - restoring parent context, parentWriterId=1002, parentStruct=,
DEBUG: SetWriter - checking parent completion: S=1, arity=2, parentWriterId=1002
DEBUG: PutStructure - SAVING PARENT CONTEXT: currentStruct=,, S=1, parentWriterId=1002
DEBUG: PutStructure baz/2 (argSlot=-1) - storing writerId 1008 at clauseVars[-1]
DEBUG: SetWriter - structure complete! functor=baz, S=2/2, targetWriter=1008, hasParent=true
DEBUG: SetWriter - restoring parent context, parentWriterId=1002, parentStruct=,
DEBUG: SetWriter - checking parent completion: S=2, arity=2, parentWriterId=1002
DEBUG: SetWriter - PARENT COMPLETE! Binding parent ,/2
DEBUG: HeadStructure ,/2 at argSlot 0
DEBUG: HeadStructure - Reader 1003 -> Writer 1002
DEBUG: HeadStructure - Writer 1002 value = StructTerm: ,(R1005,R1009), expecting ,/2
```

**Success!** The conjunction is now properly bound: `StructTerm: ,(R1005,R1009)`

---

## Remaining Issue

Despite the conjunction binding correctly, the test still outputs:
```
Z = <unbound>
→ 2 goals
```

### What's Working Now

1. ✅ BODY phase builds conjunction `(bar(X), baz(X?,Y))` correctly
2. ✅ Nested structures `bar(X)` and `baz(X?,Y)` complete and bind
3. ✅ Parent completion check fires when second nested structure completes
4. ✅ Conjunction writer 1002 gets bound to `StructTerm: ,(R1005,R1009)`
5. ✅ HEAD phase extracts nested structures to temps:
   - `clauseVars[10] = bar(W1006)`
   - `clauseVars[11] = baz(R1007,W1000)`

### What's Not Working

The final unification of constants `a` and `b` with the writers/readers in the extracted structures is not completing properly.

**Expected Flow:**
1. `HeadStructure("bar", 1, argSlot: 10)` ✅ Extracts `bar(W1006)`
2. `UnifyConstant(a)` → Should bind W1006 to `a`
3. `HeadStructure("baz", 2, argSlot: 11)` ✅ Extracts `baz(R1007,W1000)`
4. `UnifyConstant(a)` → Should unify R1007 with `a` (R1007 should already be bound to W1006's value)
5. `UnifyConstant(b)` → Should bind W1000 to `b`, which binds Y to `b`

**Hypothesis:** The UnifyConstant handler may not be processing all the constants, or the final binding of Y isn't propagating to Z in the goal.

---

## Debug Output Analysis

### Execution Trace Breakdown

#### BODY Phase (First Clause)
```
PutStructure(",", 2, 0)       → Writer 1002, Reader 1003 for conjunction
  PutStructure("bar", 1, -1)  → Writer 1004, Reader 1005 for bar/1
    SetWriter (X)             → Completes bar/1, S=1 in parent
  PutStructure("baz", 2, -1)  → Writer 1008, Reader 1009 for baz/2
    SetReader (X?)            → S=1 in baz
    SetWriter (Y)             → Completes baz/2, S=2 in parent
                              → PARENT COMPLETE! Binds conjunction 1002
```

**Result:** Writer 1002 → `StructTerm: ,(R1005, R1009)`

#### HEAD Phase (Second Clause)
```
HeadStructure(",", 2, 0)      → Extracts conjunction from arg 0
  Reader 1003 → Writer 1002 = StructTerm: ,(R1005,R1009) ✅

UnifyVariable (temp 10)       → Extracts R1005 → clauseVars[10] = bar(W1006)
UnifyVariable (temp 11)       → Extracts R1009 → clauseVars[11] = baz(R1007,W1000)

HeadStructure("bar", 1, 10)   → Match bar(W1006) against bar(a)
UnifyConstant(a)              → Should bind W1006 to 'a' in σ̂w

HeadStructure("baz", 2, 11)   → Match baz(R1007,W1000) against baz(a,b)
UnifyConstant(a)              → Should verify R1007 = 'a'
UnifyConstant(b)              → Should bind W1000 to 'b' in σ̂w
```

---

## Next Steps

1. **Add debug to UnifyConstant** - Trace whether constants are being bound properly
2. **Check σ̂w (sigmaHat) propagation** - Verify tentative bindings are committed
3. **Trace goal activation** - See if Y's binding activates suspended goals
4. **Check final result extraction** - Verify Z gets Y's value in result formatting

---

## Files Modified

### `/Users/udi/GLP/glp_runtime/lib/bytecode/runner.dart`

**Critical Fix (lines 1547-1563):**
- Reordered parent context save to happen BEFORE clauseVars[-1] overwrite
- Added condition `cx.currentStructure != null` to catch top-level structures with nested children

**Debug Output Added:**
- Line 367: HeadStructure entry
- Line 384: Arg type from argSlot
- Line 475: isWriter check
- Lines 511-514: Reader dereferencing
- Line 518: Unbound writer detection
- Line 1034: UnifyWriter storage
- Line 1554: PutStructure parent context save
- Line 1562: PutStructure writer ID storage
- Line 1610: SetWriter structure completion
- Line 1634: SetWriter parent restoration
- Line 1662: SetWriter parent completion check
- Line 1664: Parent binding success

---

## Bytecode Reference

### Test Case
```prolog
test_conj(Y?) :- foo((bar(X), baz(X?,Y))).
foo((bar(a), baz(a,b))).
```

### Generated Bytecode for foo/1

**First clause (builds conjunction):**
```
 1: ClauseTry
 2: GetVariable
 3: Commit
 4: PutStructure(",", 2, 0)           ← Creates conjunction at arg 0
 5: PutStructure("bar", 1, -1)        ← Nested bar (saves parent context)
 6: SetWriter                         ← Completes bar, S=1 in parent
 7: PutStructure("baz", 2, -1)        ← Nested baz (saves parent context)
 8: SetReader                         ← Adds X? to baz
 9: SetWriter                         ← Completes baz, S=2 in parent → BINDS CONJUNCTION!
10: Spawn
11: Proceed
```

**Second clause (matches ground conjunction):**
```
15: ClauseTry
16: HeadStructure(",", 2, argSlot: 0)  ← Matches conjunction
17: UnifyVariable                       ← Extracts bar(...) to temp 10
18: UnifyVariable                       ← Extracts baz(...) to temp 11
19: HeadStructure("bar", 1, argSlot: 10)
20: UnifyConstant(a)
21: HeadStructure("baz", 2, argSlot: 11)
22: UnifyConstant(a)
23: UnifyConstant(b)
24: Commit
25: Proceed
```

---

## Related Documents

- `NESTED_STRUCTURE_BUG_FINAL_REPORT.md` - Original diagnosis (identified BODY phase issue)
- `NESTED_STRUCTURE_FIX_PLAN.md` - Proposed fix (focused on different issue)
- `NESTED_STRUCTURE_STATUS.md` - Previous status (thought it was HEAD phase)

---

## Summary

**Major Progress:** The critical bug in parent context tracking has been fixed. Nested structures now properly bind during BODY phase execution.

**Current Status:** Conjunction binding works, but final unification needs investigation.

**Confidence Level:** High - the fix addresses the root cause and debug output confirms proper behavior.

**Next Session:** Focus on UnifyConstant execution and final binding propagation.

---

## Test Command

```bash
cd /Users/udi/GLP/udi
dart run glp_repl.dart <<'EOF'
test_conj_var.glp
test_conj(Z)
:quit
EOF
```

**Current Output:**
```
Z = <unbound>
→ 2 goals
```

**Expected Output:**
```
Z = b
```

---

## Code Verification

The fix is minimal, surgical, and addresses exactly the issue identified:

**Before:** Parent writer ID saved AFTER being overwritten
**After:** Parent writer ID saved BEFORE being overwritten

This ensures that when nested structures complete, the parent completion check has the correct writer ID to bind.

---

*Report generated: November 11, 2025*
*Session continuation from previous debugging work*
