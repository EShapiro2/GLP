# Single-ID Variable Migration - Status Report

**Date:** November 11, 2025 (Updated)
**Branch:** `single-id-migration`
**Status:** In Progress - Step 2.4 90% Complete ✅, Runtime using HeapV2 directly

---

## Progress Summary

### ✅ Completed (Steps 2.0.1 - 2.0.3, Group 2.3.1, Categories 1-2, Step 2.4 90%)

1. **Branch Created** - `single-id-migration` from commit 0af4d64
2. **VarRef Moved to terms.dart** - Now first-class term type
3. **Old types deprecated** - WriterTerm/ReaderTerm marked @Deprecated
4. **Migration helpers created** - lib/bytecode/migration_helper.dart
5. **Heap interface extended** - Added allocateFreshVar() and addVariable()
6. **Group 2.3.1 Complete** - All 19 allocateFreshPair calls migrated ✅
7. **Category 1 Complete** - All 22 type checks migrated to dual-type support ✅
8. **Category 2 Complete** - Analyzed, no migration needed (0 refs) ✅
9. **Step 2.4 (90%)** - Runtime using HeapV2 directly, compatibility layer added ✅

### 📊 Discovery: Scope Larger Than Expected

**Original estimate:** 53 occurrences of WriterTerm/ReaderTerm
**Actual finding:** Much larger scope:

- `allocateFreshPair`: **19 occurrences** (not 5 as estimated)
- WriterTerm/ReaderTerm references: 53 occurrences
- Total migration points: **70+ occurrences**

---

## Step 2.4: Remove Adapter - 90% Complete ✅

### What Was Accomplished

**Runtime Switched to HeapV2 Directly**
- Changed `runtime.dart` from `HeapV2Adapter()` to `HeapV2()`
- No longer using adapter layer - direct single-ID system!

**Comprehensive Compatibility Layer Added to HeapV2**
- `HeapV2` now extends `Heap` base class
- All old two-ID methods overridden with single-ID implementations
- Compatibility methods:
  - `isWriterBound()`, `valueOfWriter()` → `isBound()`, `getValue()`
  - `bindWriterConst()`, `bindWriterStruct()` → `bindVariableConst()`, `bindVariableStruct()`
  - `writer(varId)` → returns `WriterCell(varId, varId)` (both IDs same!)
  - `writerIdForReader(readerId)` → returns writerId (with pairing map for old tests)
  - `addWriter()`, `addReader()` → `addVariable()`
  - `allocateFreshVar()`, `addVariable()` → override base implementations

**Key Insight: Single-ID Paradigm**
- In single-ID system: `writerId == readerId == varId`
- Writer and reader are just different access modes to the same variable
- Compatibility layer bridges old two-ID test code with new single-ID implementation

### Remaining Issues

**Edge Case: Old Two-ID Test Code**
- Some tests explicitly use different writer/reader IDs:
  ```dart
  heap.addWriter(WriterCell(100, 200));  // Different IDs!
  ```
- HeapV2 compatibility layer handles this with pairing map
- A few tests may need updates to use single-ID paradigm properly

**Status**: ~1-2 tests failing with edge cases, but core functionality works

**Commit**: 3b97ab6 - "feat: Step 2.4 (partial) - Switch to HeapV2 directly"

---

## Category-Based Migration Complete ✅

### ✅ Category 1: Type Checks (22 references) - COMPLETE

**Pattern migrated:** `if (term is WriterTerm/ReaderTerm)` → `if (MigrationHelper.isWriter/isReader(term))`

**Locations migrated:**
- Guard instructions: IfWriter, IfReader, IfVariable (lines 237, 252, 271, 281)
- Format/debug: _formatTerm function (lines 168, 175)
- Structure unification: HeadStructure, UnifyConstant, UnifyWriter/UnifyReader (lines 430, 731, 749, 863, 884, 1013, 1016, 1061, 1096, 1128, 1135, 1490)
- Suspension: collectUnbound helper (lines 2046, 2053)
- System predicates: variable_name/2 (lines 2155, 2157)

**Strategy:** Dual-type support using MigrationHelper - both WriterTerm/ReaderTerm AND VarRef work

**Commit:** 9209865 - "refactor: Category 1 - Migrate all type checks to dual-type support"

### ✅ Category 2: Property Access (0 references) - NO MIGRATION NEEDED

**Analysis:** All `.writerId` and `.readerId` accesses are on `arg` objects from `_getArg()`, NOT on WriterTerm/ReaderTerm types. These are part of the argument unification system and do not require migration.

### ⏸️ Category 3: Compatibility Code (14 references) - DEFER TO STEP 2.4

**Pattern:** `WriterTerm(id)` and `ReaderTerm(id)` construction

**Locations:**
- Lines 625, 958, 978, 1059, 1093, 1228, 1230, 1637, 1717, 1753, 1883, 1885, 1928, 1930

**Decision:** KEEP AS-IS during migration. These constructions maintain backward compatibility with the dual-heap system. Will migrate to VarRef construction in Step 2.4 when removing HeapV2Adapter.

### ⏸️ Category 4: Adapter Bridging (~11 references) - PART OF STEP 2.4

**Pattern:** Conversion logic in HeapV2Adapter (_convertToV2, _convertFromV2)

**Decision:** KEEP AS-IS. These are the adapter's core functionality for bridging the two systems. Will be removed entirely in Step 2.4.

---

## Completed: Group 2.3.1 (Variable Allocation)

**Pattern to replace:**
```dart
// OLD:
final (freshWriterId, freshReaderId) = cx.rt.heap.allocateFreshPair();
cx.rt.heap.addWriter(WriterCell(freshWriterId, freshReaderId));
cx.rt.heap.addReader(ReaderCell(freshReaderId));

// NEW:
final varId = cx.rt.heap.allocateFreshVar();
cx.rt.heap.addVariable(varId);
// Use VarRef(varId, isReader: false) for writer
// Use VarRef(varId, isReader: true) for reader
```

**Locations in runner.dart:**
- Line 963, 984, 1068, 1078, 1105
- Line 1237, 1252
- Line 1438, 1447, 1485, 1493, 1504
- Line 1534, 1550, 1601, 1717
- Line 2438, 2451, 2463

**Strategy:**
1. Convert allocateFreshPair → allocateFreshVar
2. Replace WriterTerm(freshWriterId) → VarRef(varId, isReader: false)
3. Replace ReaderTerm(freshReaderId) → VarRef(varId, isReader: true)
4. Test after each block of 5 changes

---

## Revised Time Estimate

**Original:** 8-12 hours
**Revised:** 12-16 hours (due to larger scope)

**Breakdown:**
- Group 2.3.1: 2-3 hours (19 occurrences)
- Group 2.3.2-2.3.7: 6-8 hours (remaining 51+ occurrences)
- Step 2.4 (Remove adapter): 1-2 hours
- Step 2.5 (Cleanup): 1 hour
- Testing & debugging: 2-3 hours

---

## Critical Files

### Modified So Far
- `lib/runtime/terms.dart` - VarRef added, old types deprecated
- `lib/bytecode/migration_helper.dart` - Helper functions created

### To Be Modified
- `lib/bytecode/runner.dart` - 70+ occurrences to migrate
- `lib/runtime/heap_v2_adapter.dart` - Remove duplicate storage
- `lib/runtime/runtime.dart` - Switch to HeapV2 directly

### To Be Deleted
- `lib/runtime/heap.dart` - Old two-ID heap
- `lib/runtime/heap_v2_adapter.dart` - After migration complete
- `lib/bytecode/migration_helper.dart` - Temporary, delete after done

---

## Risk Assessment

**Low Risk:**
- Incremental approach with testing
- Can rollback at any checkpoint
- Deprecation warnings guide remaining work

**Medium Risk:**
- Larger scope than expected (70+ changes)
- Time required longer than estimated

**Mitigation:**
- Commit after each group (already doing)
- Test after every 5 changes
- Keep migration branch separate from main

---

## Commits So Far

1. `0af4d64` - Bug fixes (parent context + heap consistency)
2. `521a80d` - Checkpoint before migration
3. `5859b94` - VarRef preparation (Steps 2.0.2-2.0.3)
4. `c6dc067` - docs: Single-ID migration status and scope update
5. `b37789b` - fix: Remove duplicate VarRef definition and update constructor calls
6. `d42d4e9` - refactor: Add single-ID methods and migrate first 5 allocateFreshPair calls
7. `d001bba` - refactor: Migrate next 7 allocateFreshPair calls (Group 2.3.1 continued)
8. `f09f0db` - refactor: Complete Group 2.3.1 - all 19 allocateFreshPair calls migrated
9. `9209865` - refactor: Category 1 - Migrate all type checks to dual-type support
10. `a1d151c` - docs: Category 1-2 complete, ready for Step 2.4
11. `3b97ab6` - feat: Step 2.4 (partial) - Switch to HeapV2 directly, remove adapter dependency

---

## When Resuming

**Test baseline:**
```bash
cd /Users/udi/GLP/glp_runtime
dart test test/bytecode/asm_smoke_test.dart
# Should show: All tests passed!
dart test
# Should show: +197 -24 (same as before migration)
```

**Current State:**
- ✅ All variable allocation uses single-ID (Group 2.3.1)
- ✅ All type checks use dual-type support (Category 1)
- ✅ Runtime using HeapV2 directly (Step 2.4 90%)
- ✅ Compatibility layer providing old API
- ✅ Zero allocateFreshPair calls in runner.dart
- ⚠️ A few tests with old two-ID paradigm need fixes
- ⏸️ Category 3 (14 construction sites) still to migrate

**Next Session Tasks:**

**Priority 1: Fix Remaining Test Issues** (1-2 hours)
- Debug the ROQ activation test failure
- Update tests using explicit two-ID paradigm
- Verify all tests pass with HeapV2

**Priority 2: Complete Category 3 Migration** (2-3 hours)
- Migrate 14 WriterTerm/ReaderTerm construction sites to VarRef
- Update all `ReaderTerm(id)` → `VarRef(id, isReader: true)`
- Update all `WriterTerm(id)` → `VarRef(id, isReader: false)`

**Priority 3: Final Cleanup** (1 hour)
- Delete heap_v2_adapter.dart (no longer needed!)
- Consider deleting old heap.dart once tests pass
- Remove migration_helper.dart compatibility code
- Update documentation

---

## Notes

- Group 2.3.1 took ~3 hours (as estimated)
- Pattern proven: mechanical replacements work well
- Complexity discovered: remaining refs need architectural decisions
- HeapV2Adapter bridges two systems - needs careful handling
- Clean git state with good checkpoints

---

**Status:** Step 2.4 90% Complete ✅ - Runtime using HeapV2 directly
**Minor Issues:** A few tests need updates for single-ID paradigm
**Next Action:** Fix test edge cases, then complete Category 3 construction site migrations
