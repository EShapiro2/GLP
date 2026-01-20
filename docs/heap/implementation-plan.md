# Pointer Architecture Implementation Plan

**Version**: 1.3
**Date**: 2026-01-20
**Status**: Phase 5 COMPLETE for single-isolate (223/223 REPL tests, 259/271 unit tests)
**Spec**: `docs/heap/heap-pointer-architecture-spec.md` v3.0
**Branch**: `claude/pointer-architecture-migration-zFED8`
**Test Suite**: REPL tests primary (223/223 passing), unit tests (259/271 - 12 multiagent deferred)

---

## Overview

This document describes the implementation plan for the pointer-based heap architecture. The key change is reversing the pointer direction: readers point TO writers (not vice versa). This enables imported readers to naturally point to V_p entries instead of requiring a local paired writer.

---

## Current Progress (2026-01-20)

| Phase | Status | Notes |
|-------|--------|-------|
| Phase 1 | ✅ DONE | VarRef simplified, allocateVariable returns tuple |
| Phase 2 | ✅ DONE | All binding/suspension methods implemented |
| Phase 3 | ✅ DONE | derefAddr follows pointers correctly |
| Phase 4 | ✅ DONE | Compatibility methods updated |
| Phase 5 | ✅ DONE (single-isolate) | 223/223 REPL tests, 259/271 unit tests |
| Phase 6 | ⏳ PENDING | Path compression (optional) |
| Phase 7 | ⏳ PENDING | Final verification |

### Phase 5 File Status:
- ✅ `lib/runtime/suspend_ops.dart`
- ✅ `lib/runtime/commit.dart` - Fixed VarRef handling per spec Section 5.3
- ✅ `lib/runtime/scheduler.dart`
- ✅ `lib/runtime/body_kernels.dart`
- ✅ `lib/runtime/module_runtime.dart`
- ✅ `lib/compiler/codegen.dart`
- ✅ `lib/compiler/analyzer.dart` - Added wait_until/wait as grounding guards
- ✅ `lib/bytecode/runner.dart` - Fixed PutVariable for ValueTag cells
- ✅ `bin/glp_repl.dart` - display code migrated
- ✅ `lib/runtime/system_predicates_impl.dart` - fully migrated
- ⏸️ `lib/runtime/external_io.dart` - DEFERRED (multiagent-related)
- ⏸️ `lib/multiagent/irma_context.dart` - DEFERRED (focus on single-isolate first)
- ⏸️ `lib/multiagent/payload_serializer.dart` - DEFERRED
- ⏸️ `lib/multiagent/helpers.dart` - DEFERRED

### Test Status:
- **REPL tests**: 223/223 passing ✅
- **Unit tests**: 259/271 passing (12 failing are multiagent-related, DEFERRED)

---

## Phase 1: Core Data Structures

**Status**: ✅ DONE

**Goal**: Update `VarRef` and `allocateVariable()` to match spec.

**Files**:
- `lib/runtime/terms.dart`
- `lib/runtime/heap_fcp.dart`

**Changes**:

### 1.1 VarRef (terms.dart)

Current:
```dart
class VarRef extends Term {
  final int varId;
  final bool isReader;
  VarRef(this.varId, {required this.isReader});
}
```

Target:
```dart
class VarRef extends Term {
  final int addr;
  VarRef(this.addr);
}
```

### 1.2 allocateVariable (heap_fcp.dart)

Current:
```dart
int allocateVariable() {
  // Writer points to reader
  cells.add(HeapCell(Pointer(rAddr), CellTag.WrtTag));
  cells.add(HeapCell(null, CellTag.RoTag));
  return wAddr;
}
```

Target:
```dart
(int, int) allocateVariable() {
  final writerAddr = HP++;
  final readerAddr = HP++;
  // Writer: null content (unbound)
  cells.add(HeapCell(null, CellTag.WrtTag));
  // Reader: points TO writer
  cells.add(HeapCell(Pointer(writerAddr), CellTag.RoTag));
  return (writerAddr, readerAddr);
}
```

**Checkpoint**: `dart analyze lib/runtime/terms.dart lib/runtime/heap_fcp.dart`

---

## Phase 2: New Binding and Suspension Methods

**Goal**: Add methods expected by test suite.

**File**: `lib/runtime/heap_fcp.dart`

**New methods**:

### 2.1 bindWriter
```dart
List<GoalRef> bindWriter(int writerAddr, Term value)
```
Binds writer to ground value. Changes tag to `ValueTag`. Activates any suspensions on the writer cell.

### 2.2 bindWriterToReader
```dart
List<GoalRef> bindWriterToReader(int writerAddr, int readerAddr)
```
Binds writer to another variable (via its reader). Stores `Pointer(readerAddr)` in writer cell. Forwards suspensions to target writer.

### 2.3 bindWriterToWriter
```dart
void bindWriterToWriter(int w1, int w2)
```
Should throw `StateError` — WxW binding is forbidden.

### 2.4 suspendOnWriter
```dart
void suspendOnWriter(int writerAddr, SuspensionRecord record)
```
Adds suspension to writer cell. Prepends to existing suspension list if present.

### 2.5 suspendOnReader
```dart
void suspendOnReader(int readerAddr, SuspensionRecord record)
```
Follows reader's pointer to find writer, then adds suspension there.

### 2.6 writerForReader
```dart
int writerForReader(int readerAddr)
```
Follows reader's pointer to return the writer address.

**Checkpoint**: `dart test test/heap/pointer_architecture_test.dart`

---

## Phase 3: Update Dereferencing

**Goal**: `derefAddr` follows pointers correctly per spec Section 4.

**File**: `lib/runtime/heap_fcp.dart`

**Algorithm**:
1. Start at given address
2. If RoTag: follow Pointer to target, continue
3. If WrtTag with null or SuspensionListNode: return `VarRef(currentAddr)` (unbound)
4. If WrtTag with Pointer: follow to target, continue
5. If ValueTag: return the Term content
6. If VariableEntry: check state for value or return entry
7. Detect cycles via visited set, throw StateError

**Note**: Path compression is deferred to Phase 6.

**Checkpoint**: `dart test test/heap/`

---

## Phase 4: Update Compatibility Methods

**Goal**: Existing API methods work with new internals.

**File**: `lib/runtime/heap_fcp.dart`

**Methods to update**:
- `isFullyBound(int writerAddr)` — returns true if deref yields Term (not VarRef or VariableEntry)
- `getValue(int writerAddr)` — returns Term or null
- `bindVariable(int writerAddr, Term value)` — delegate to `bindWriter`
- `dereference(Term term)` — handle new VarRef format
- `isWriter(int addr)` / `isReader(int addr)` — check cell tag

**Checkpoint**: `dart test test/heap/`

---

## Phase 5: Update Callers Throughout Codebase

**Goal**: All code using the old API is updated.

**Files to update** (one at a time, test after each):

| File | Key Changes |
|------|-------------|
| `lib/runtime/suspend_ops.dart` | Use readerAddr directly, suspensions on writer |
| `lib/runtime/commit.dart` | sigmaHat keyed by writerAddr, use bindWriter |
| `lib/bytecode/runner.dart` | VarRef construction, allocateVariable tuple |
| `lib/runtime/external_io.dart` | Channel creation, callback registration |
| `lib/multiagent/irma_context.dart` | importTerm, registerWriter/Reader |
| `lib/multiagent/payload_serializer.dart` | VarRef serialization |
| `lib/compiler/*.dart` | Review for VarRef usage |

**Approach**: Fix compilation errors in each file, run tests, commit.

**Checkpoint**: `dart test` (full suite)

---

## Phase 6: Path Compression

**Goal**: Implement path compression per spec Section 4.4.

**File**: `lib/runtime/heap_fcp.dart`

**Description**: When dereferencing, update the starting cell's pointer to point directly to the final target, bypassing intermediate cells. This ensures repeated dereferences are O(1).

**Implementation**:
```dart
Object derefAddr(int startAddr) {
  // Phase 1: Follow chain to find final target
  var current = startAddr;
  int? finalAddr;
  Object? finalValue;
  // ... follow chain ...

  // Phase 2: Path compression
  if (startAddr != finalAddr && cells[startAddr].tag == CellTag.RoTag) {
    cells[startAddr].content = Pointer(finalAddr!);
  }

  return finalValue!;
}
```

**Checkpoint**: `dart test test/heap/pointer_architecture_test.dart` — path compression tests

---

## Phase 7: Final Verification

**Goal**: All tests pass — both new heap tests and existing baseline.

**Commands**:
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test > /private/tmp/glp-tests.txt 2>&1
cd /Users/udi/Grassroots/GLP && bash test/full_run_repl_tests.sh > /private/tmp/repl-tests.txt 2>&1
```

**Success criteria**: 
- All 7 heap test files pass
- Baseline test count matches or exceeds pre-refactor count
- No regressions in REPL tests

---

## Risk Assessment

| Phase | Risk Level | Concern | Mitigation |
|-------|------------|---------|------------|
| 1 | Medium | VarRef change breaks many files | Expected; fix in Phase 5 |
| 2 | Low | New methods are additive | Tests guide implementation |
| 3 | Medium | Deref logic is central to correctness | Comprehensive test coverage |
| 4 | Low | Compatibility wrappers | Straightforward delegation |
| 5 | High | Many files, subtle bugs possible | One file at a time, frequent commits |
| 6 | Low | Isolated optimization | Clear spec, targeted tests |
| 7 | Low | Verification only | — |

---

## Document History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2026-01-20 | Claude | Initial plan |
| 1.2 | 2026-01-20 | Claude | Updated status: 198/223 REPL tests, added migrated files |
