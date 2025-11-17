# Handover Report: Variable Object Model Clarification

**Date:** 2025-11-17
**Session:** Claude Code - spec clarification and cleanup planning
**For:** Claude Web / Next Session

---

## Summary

Clarified the single-ID variable object model in the bytecode spec by adding a new "Variable Object Model" section. Investigated the old two-variable allocation scheme and created a complete elimination plan. No code changes made - spec documentation only.

---

## Changes Made

### 1. Spec Clarification (docs/glp-bytecode-v216-complete.md)

**Added "Variable Object Model" section to Section 0:**

Explains:
- **VariableCell**: Heap-allocated object with unique ID (varId)
  - Contains: varId, value (Term or null), dereferencedCache, abandoned flag
  - Created by: `heap.allocateFreshVar()` returns ID, then `heap.addVariable(varId)` creates object

- **VarRef**: Lightweight references pointing to variable objects
  - `VarRef(varId, isReader: true)` - reader reference
  - `VarRef(varId, isReader: false)` - writer reference
  - Multiple references can point to same variable with different access modes

- **Variable Lifecycle**:
  1. Allocate: `varId = heap.allocateFreshVar()`, then `heap.addVariable(varId)`
  2. Bind: `heap.bindVariable(varId, term)` or via σ̂w at commit
  3. Dereference: `heap.valueOfWriter(varId)` follows binding chain
  4. Wake: ROQ processes suspended goals when variable is bound

- **Key Principle**: ONE variable object on heap, MULTIPLE VarRef references with different access modes

- **Compatibility Layer**: Documents `allocateFreshPair()`, `addWriter()`, `addReader()` as thin wrappers for test convenience

---

## Investigation: Two-Variable Scheme Elimination

### Current State Analysis

**Old two-ID artifacts still present:**

1. **Classes** (lib/runtime/cells.dart):
   - `WriterCell` class (lines 11-19)
   - `ReaderCell` class (lines 22-28)

2. **Heap methods** (lib/runtime/heap.dart):
   - `allocateFreshPair()` - returns (varId, varId)
   - `addWriter(WriterCell)` - calls addVariable()
   - `addReader(ReaderCell)` - no-op
   - `writer(varId)` - returns WriterCell for compatibility
   - `_readerToWriter` / `_writerToReader` pairing maps

3. **Usage statistics:**
   - 233 lines in test/bin files using old API
   - 23 uses of `heap.writer()` in lib/ (runner.dart, commit.dart, abandon.dart)

### Elimination Plan (NOT YET EXECUTED)

**Phase 1: Eliminate WriterCell from lib/**
- Replace 23 `heap.writer(varId)` calls with direct varId access
- Add `heap.hasVariable(varId)` helper method
- Test: 86/86 should pass

**Phase 2: Migrate Tests & Demos**
- Create automated sed script to replace patterns:
  ```
  OLD: allocateFreshPair + addWriter + addReader
  NEW: allocateFreshVar + addVariable
  ```
- Apply to 233 lines across test/bin files
- Test: 86/86 should pass

**Phase 3: Remove Old Code**
- Delete `allocateFreshPair()`, `addWriter()`, `addReader()`, `writer()` from heap.dart
- Delete `_readerToWriter`, `_writerToReader` maps
- Test: 86/86 should pass

**Phase 4: Remove Classes**
- Delete `WriterCell`, `ReaderCell` from cells.dart
- Test: 86/86 should pass

**Estimated effort:** 2-3 hours total

---

## Test Results

### Unit Tests: ✅ 86/86 Passing
No code changes, spec clarification only - all tests pass.

### REPL Tests: 18/23 Passing (Baseline)

**Failing tests (5):**
- Test 5: Merge with Reader (SRSW violation)
- Test 9: Insertion Sort via Metainterpreter
- Test 13: Structure Demo
- Test 18: Insertion sort two elements
- Test 19: Quicksort two elements [1,2]

Same as baseline - no regression.

---

## Git Status

**Current commit:** 47b8f87 `docs: Clarify single-ID variable object model in bytecode spec`

**Recent commits:**
```
47b8f87 docs: Clarify single-ID variable object model in bytecode spec
8e57219 chore: Update REPL buildTime to reference commit 7ee6f95
7ee6f95 refactor: Remove Si, HEAD opcodes fail immediately on unbound readers
```

**Branch:** main

---

## Next Steps

### Option 1: Execute Two-Variable Elimination Plan

Pros:
- Cleaner codebase with single consistent API
- Removes confusion from having two ways to allocate variables
- Better code examples in tests

Cons:
- 2-3 hours of migration work
- Risk of breaking something during migration
- Delays fixing the actual ROQ wake bug

### Option 2: Fix ROQ Wake Bug First

Pros:
- Gets quicksort working (immediate user value)
- More important than cleanup
- Can do cleanup later

Cons:
- Old two-variable artifacts remain for now

**Recommendation:** Fix ROQ wake bug first (Option 2). The two-variable elimination is cleanup work that can wait. Getting quicksort working is more valuable.

---

## ROQ Wake Bug Investigation Plan (Pending Approval)

From HANDOVER_SI_REMOVAL_20251117.md, the issue remains:
- ✅ Goals properly suspend on unbound readers (Si removal fixed this)
- ❌ Suspended goals don't wake up when writers are bound

**Next steps:**
1. Add ROQ tracing to `no_more_clauses` (goal suspension)
2. Add writer binding tracing to `commit` (when writers get bound)
3. Add ROQ wake tracing to `processOnBind` (goal reactivation)
4. Run `quicksort([1,2], X)` with tracing
5. Identify why goals aren't reactivating

---

## Files Modified

1. **`/Users/udi/GLP/docs/glp-bytecode-v216-complete.md`**
   - Added "Variable Object Model" section to Section 0
   - 25 lines added

---

## Key Takeaways

### What Was Done ✅
1. Spec now clearly documents single-ID variable model
2. Explained VariableCell (heap object) vs VarRef (lightweight reference)
3. Documented variable lifecycle and ROQ interaction
4. Documented compatibility layer for tests
5. Investigated two-variable scheme elimination (plan ready, not executed)

### What Remains ❌
1. Two-variable scheme still exists (plan ready but not executed)
2. ROQ wake bug still unfixed (quicksort still fails)
3. 5 REPL tests still failing

### Decision Point 🤔

**User needs to choose:**
- Execute two-variable elimination now (2-3 hours cleanup)
- OR fix ROQ wake bug first (immediate functionality)

**Recommendation:** Fix ROQ wake bug first.

---

## Questions for Next Session

1. Should we execute the two-variable elimination plan, or defer it?
2. Should we proceed with ROQ wake bug investigation?
3. Any other cleanup priorities before debugging?
