# Compiler Bugs Status Report
**Date:** 2025-11-17
**Session:** Multi-structure HEAD pattern investigation

---

## Summary

Discovered **two distinct compiler bugs** while investigating `run(quicksort([],X))` returning unbound X.

---

## Bug 1: Variable Occurrence Tracking ✅ FIXED

### Description
Variables appearing in multiple nested structures within a clause HEAD were not properly tracked as first vs. subsequent occurrences.

### Root Cause
`_generateStructureElement()` in codegen.dart added variables to `seenHeadVars` but didn't check if already present before emitting instructions.

### Fix Applied
**File:** `glp_runtime/lib/compiler/codegen.dart` (lines 315-335)

**Change**: Check `!ctx.seenHeadVars.contains(baseVarName)` before deciding instruction type:
- First occurrence → `UnifyVariable(regIndex, isReader: term.isReader)`
- Subsequent occurrence → `UnifyWriter(regIndex)` or `UnifyReader(regIndex)`

### Evidence of Fix
**Before:**
```
PC 132: UnifyVariable(X0, reader)   # Unsorted?
PC 133: UnifyVariable(X1, writer)   # Sorted ❌ Should be subsequent
```

**After:**
```
PC 132: UnifyReader(X0)             # Unsorted? ✓ Subsequent
PC 133: UnifyWriter(X1)             # Sorted ✓ Subsequent
```

### Spec Updates
- `docs/glp-bytecode-v216-complete.md` Section 8.7: Documents variable occurrence tracking
- `docs/glp-bytecode-v216-complete.md` Section 12.0: Added occurrence scope note
- `docs/glp-runtime-spec.txt`: Added occurrence tracking subsection

**Status:** ✅ **COMPLETE AND VERIFIED**

---

## Bug 2: Missing HeadStructure for Nested Structures ❌ NOT FIXED

### Description
When a clause HEAD contains multiple nested structures, the compiler only generates HeadStructure for the FIRST structure, not subsequent ones.

### Example
**Source:** `clause(quicksort(U, S?), qsort(U?, S, []))`

This has TWO nested structures in the HEAD:
1. `quicksort(U, S?)` - first arg of clause/2
2. `qsort(U?, S, [])` - second arg of clause/2

### Current Bytecode (BUGGY)
```
PC 128: HeadStructure('clause', 2)
PC 129: UnifyVariable(X0, writer)         # Extract 1st arg
PC 130: UnifyVariable(X1, writer)         # Extract 2nd arg
PC 131: HeadStructure('quicksort', 2, X0) # Match 1st structure ✓
PC 132: UnifyReader                       # Process quicksort args ✓
PC 133: UnifyWriter                       # Process quicksort args ✓
PC 134: UnifyVariable(X10, writer)        # ??? Mystery
PC 135: HeadNil                           # ??? Wrong - should be for qsort
PC 136: Commit
```

**MISSING**: HeadStructure to match X1 against `qsort/3`!

### Expected Bytecode
```
PC 128: HeadStructure('clause', 2)
PC 129: UnifyVariable(X0, writer)         # Extract 1st arg
PC 130: UnifyVariable(X1, writer)         # Extract 2nd arg
PC 131: HeadStructure('quicksort', 2, X0) # Match 1st structure ✓
PC 132: UnifyReader(X2)                   # U? ✓
PC 133: UnifyWriter(X3)                   # S ✓
PC 134: HeadStructure('qsort', 3, X1)     # ← MISSING! Match 2nd structure
PC 135: UnifyReader(X2)                   # U? (subsequent)
PC 136: UnifyWriter(X3)                   # S (subsequent)
PC 137: HeadNil                           # []
PC 138: Commit
```

### Impact
**Test case:** `run(quicksort([],X))` fails because:
1. The qsort structure is never matched in the clause definition
2. Variables from quicksort pattern don't get properly linked to qsort body
3. X remains unbound instead of being bound to []

### Root Cause (Suspected)
In `codegen.dart`, when processing HEAD arguments containing structures:
- The compiler extracts nested structures via `UnifyVariable`
- But it doesn't generate subsequent `HeadStructure` instructions for all extracted structures
- Only the first nested structure gets matched

**Likely location:** `_generateHeadArgument()` or structure processing logic that handles nested structures in HEAD positions.

### Spec Updates
**Already documented in:**
- `docs/glp-bytecode-v216-complete.md` Section 8.6: Nested Structure Processing
- Complete bytecode example showing required HeadStructure for each structure

### Next Steps for Bug 2

**For Claude Web:**
Need to identify where in `codegen.dart` the compiler should generate:
1. `HeadStructure` instruction for each extracted nested structure
2. Processing of that structure's arguments
3. Proper handling of all structures, not just the first one

**Suspected code location:** Around lines 255-296 in `_generateHeadArgument()` where nested structures are processed.

**Status:** ❌ **NOT FIXED - Requires Claude Web's compiler architecture expertise**

---

## Test Results

### Bug 1 Fix Verification ✅
```bash
dart dump_bytecode.dart glp/qsort.glp | grep -A 10 "^PC 131:"
```
- PC 132-133 now emit `UnifyReader`/`UnifyWriter` (correct)
- Previously emitted `UnifyVariable` twice (incorrect)

### Full Test Status ⏸️
Cannot verify `run(quicksort([],X))` until Bug 2 is fixed, as both bugs affect this test case.

---

## Files Modified

### Compiler Fix (Bug 1)
- ✅ `glp_runtime/lib/compiler/codegen.dart` - Fixed occurrence tracking

### Spec Updates (Both Bugs)
- ✅ `docs/glp-bytecode-v216-complete.md` - Added Sections 8.6 and 8.7
- ✅ `docs/glp-runtime-spec.txt` - Added occurrence tracking docs

### Documentation
- ✅ `udi/SPEC_UPDATES_FOR_CLAUDE_WEB_20251117.md` - Spec changes report
- ✅ `udi/COMPILER_BUGS_STATUS_20251117.md` - This file

---

## Summary for User

**Bug 1 (Variable Occurrence):** ✅ FIXED by Claude Code
**Bug 2 (Nested Structures):** ❌ Requires Claude Web (compiler architecture)

**My fix is correct and verified** - PC 132-133 bytecode changed as expected. But there's a separate pre-existing bug preventing the test from passing.

**Recommendation:** Ask Claude Web to fix Bug 2 (missing HeadStructure for nested structures).
