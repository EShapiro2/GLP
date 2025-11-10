# Single-ID Variable Migration - Status Report

**Date:** November 11, 2025 (Updated)
**Branch:** `single-id-migration`
**Status:** In Progress - Group 2.3.1 Complete ✅

---

## Progress Summary

### ✅ Completed (Steps 2.0.1 - 2.0.3, Group 2.3.1)

1. **Branch Created** - `single-id-migration` from commit 0af4d64
2. **VarRef Moved to terms.dart** - Now first-class term type
3. **Old types deprecated** - WriterTerm/ReaderTerm marked @Deprecated
4. **Migration helpers created** - lib/bytecode/migration_helper.dart
5. **Heap interface extended** - Added allocateFreshVar() and addVariable()
6. **Group 2.3.1 Complete** - All 19 allocateFreshPair calls migrated ✅

### 📊 Discovery: Scope Larger Than Expected

**Original estimate:** 53 occurrences of WriterTerm/ReaderTerm
**Actual finding:** Much larger scope:

- `allocateFreshPair`: **19 occurrences** (not 5 as estimated)
- WriterTerm/ReaderTerm references: 53 occurrences
- Total migration points: **70+ occurrences**

---

## Groups 2.3.2+ Status: Architectural Decisions Required

### Remaining Work: 41 WriterTerm/ReaderTerm References

After completing Group 2.3.1, analysis revealed that the remaining 41 references involve **architectural decisions** rather than simple mechanical replacements.

**Category 1: Type checks** (~10 references)
- Pattern: `if (term is WriterTerm)` or `if (term is ReaderTerm)`
- Challenge: Need strategy for VarRef-aware type checking
- Question: Add VarRef checks? Keep both during migration? Use helpers?

**Category 2: Property access** (~5 references)
- Pattern: `term.writerId` or `term.readerId`
- Challenge: VarRef has `varId` instead
- Question: Direct replacement or mapping through adapter?

**Category 3: Compatibility code** (~15 references)
- Pattern: `WriterTerm(id)` in formatters, debug output, etc.
- Challenge: Determine temporary vs permanent compatibility
- Question: Migrate now or keep for backward compatibility?

**Category 4: Adapter bridging** (~11 references)
- Pattern: References in/near HeapV2Adapter logic
- Challenge: Understanding system boundary during migration
- Question: What stays in adapter until Step 2.4?

### Key Architectural Questions for Next Session

1. **Type System Strategy**
   - How should VarRef coexist with WriterTerm/ReaderTerm during migration?
   - Should we use interface implementation or migration helpers?

2. **Compatibility Boundary**
   - What conversions must stay in HeapV2Adapter until Step 2.4?
   - Which references can safely migrate now in Groups 2.3.2+?

3. **Migration Sequence**
   - Continue with Groups 2.3.2-2.3.7 after resolving design?
   - Or skip to Step 2.4 (remove adapter) sooner?

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
- ✅ All variable allocation uses single-ID
- ✅ Zero allocateFreshPair calls in runner.dart
- ✅ Tests stable (197 passing, 24 pre-existing failures)
- ⏸️ 41 WriterTerm/ReaderTerm references require design decisions

**Next Session Tasks:**

1. **Resolve architectural questions** (see "Key Architectural Questions" above)
2. **Design migration strategy** for the 4 categories of remaining references
3. **Continue with Groups 2.3.2+** once strategy is clear

---

## Notes

- Group 2.3.1 took ~3 hours (as estimated)
- Pattern proven: mechanical replacements work well
- Complexity discovered: remaining refs need architectural decisions
- HeapV2Adapter bridges two systems - needs careful handling
- Clean git state with good checkpoints

---

**Status:** Group 2.3.1 Complete ✅ - Awaiting architectural decisions for Groups 2.3.2+
**Blocker:** Need design strategy for remaining WriterTerm/ReaderTerm references
**Next Action:** Consult on type system strategy and compatibility boundaries
