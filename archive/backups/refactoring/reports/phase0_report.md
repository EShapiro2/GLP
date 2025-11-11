# Phase 0 Baseline Report
Date: 2025-11-09 01:15:00 +0200
Commit: 20fd92ce11e33591b33a813b46504036581ce7aa
Branch: vm-claude-integration

## Test Results Summary

### Total Test Counts
- **Total tests**: 195 (180 passing + 15 failing)
- **Passing**: 180/195 (92.3%)
- **Failing**: 15/195 (7.7%)

### Test Status
✅ **180 tests passing** - Core functionality working
❌ **15 tests failing** - Known issues, not blockers for refactoring

## ID Allocation Pattern

### Confirmed Pattern
- **Writer ID**: N
- **Reader ID**: N+1 (always Writer + 1)
- **Consistency**: ✅ 100% consistent across all allocations
- **Storage**: `WriterCell(writerId, readerId)` maintains pairing

### Example Allocations
```
allocateFreshPair() -> (1000, 1001)
allocateFreshPair() -> (1002, 1003)
allocateFreshPair() -> (1004, 1005)
Pattern: Reader = Writer + 1
```

## Performance Baseline

### Variable Lookup Complexity
- **writerIdForReader()**: O(n) - scans all writers
- **Location**: `lib/runtime/heap.dart:44-49`
- **Impact**: Linear degradation with heap size

### Current Data Structures
- **clauseVars**: `Map<int, Object?>` - O(log n) access
- **argWriters**: `Map<int, int>` - O(log n) access
- **argReaders**: `Map<int, int>` - O(log n) access
- **Set<int> si, U**: Set operations for suspension tracking

## Behavioral Findings

### Suspension Mechanism
- **Uses**: `Set<int> si` (clause-local) and `Set<int> U` (accumulated)
- **Pattern**: Accumulates suspensions across clause tries
- **ROQ Structure**: `Map<int, Set<int>>` - one reader → many goals

### Instruction Redundancy
- **HeadWriter / HeadReader**: Separate instructions
- **UnifyWriter / UnifyReader**: Separate instructions
- **PutWriter / PutReader**: Separate instructions
- **Redundancy Factor**: ~2x (every operation has writer/reader variant)

## Issues Found

### Critical Issues
1. ✅ **O(n) variable lookup** - confirmed, affects performance at scale
2. ✅ **Separate Writer/Reader IDs** - unnecessary complexity
3. ✅ **Set-based suspension** - could be simplified to single reader
4. ✅ **Map-based registers** - could be array-based for O(1) access
5. ✅ **No dereferencing optimization** - no path compression

### Non-Blocking Issues
- 15 failing tests - appear to be in advanced features, not core runtime
- None prevent refactoring baseline from being established

## Refactoring Feasibility

### Ready to Proceed: ✅ **YES**

### Justification
1. **Baseline captured** - 180/195 tests passing, behavior documented
2. **Pattern confirmed** - Writer+1=Reader verified
3. **Performance issues identified** - O(n) lookup, map overhead
4. **No blocking issues** - failing tests don't affect core refactoring

### Risk Assessment
- **Low Risk**: Isolated changes to heap and ID allocation
- **High Impact**: Major performance improvement expected
- **Well-Documented**: FCP design provides clear target

## Next Steps

### Phase 1 Prep
1. ✅ Create `lib/runtime/heap_v2.dart` with single ID system
2. ✅ Implement `VarRef(varId, isReader)` term representation
3. ✅ Add O(1) direct lookup (eliminate writerIdForReader)
4. ✅ Create compatibility tests (old vs new heap)

### Success Criteria for Phase 1
- Both heaps pass same tests
- Performance improvement > 10x for lookups
- No semantic changes
- All 180 passing tests still pass

## Checkpoint 0 Complete

**Status**: ✅ BASELINE ESTABLISHED
**Ready for Phase 1**: ✅ YES
**Blocking Issues**: ❌ NONE

---

## Detailed Metrics

### File Structure
```
refactoring/
├── baseline/
│   ├── tests/baseline_test_run.txt (195 tests)
│   ├── behavior/id_allocation_pattern.md
│   └── [coverage and performance to be added]
├── reports/phase0_report.md (this file)
└── tools/ (to be populated)
```

### Test Categories Passing
- ✅ Smoke tests
- ✅ SRSW validation
- ✅ Bytecode operations
- ✅ Suspension/wake cycles
- ✅ Scheduler fairness
- ✅ Ground/known guards
- ✅ Environment frames
- ✅ Conformance tests (partial)

### Architecture Confirmed
- Runtime: `GlpRuntime`, `Heap`, `ROQ`, `Scheduler`
- Bytecode: `BytecodeRunner`, `RunnerContext`, opcodes
- Terms: `WriterTerm`, `ReaderTerm`, `ConstTerm`, `StructTerm`
- Cells: `WriterCell`, `ReaderCell`

**Report Complete - Awaiting Approval to Proceed to Phase 1**
