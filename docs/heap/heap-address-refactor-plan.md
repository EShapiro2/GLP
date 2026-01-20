# Heap Address Refactor Implementation Plan

**Version**: 1.2  
**Date**: 2026-01-19  
**Status**: PLANNED  
**Related**: glp-runtime-spec.txt v2.17.1, heap-address-refactor-rationale.md

## 1. Overview

This document describes the implementation plan for refactoring the GLP heap from a varId-based design to an address-based design. The goal is to eliminate unnecessary indirection where a separate variable ID namespace maps to heap addresses, and instead use heap addresses directly as variable identities.

### 1.1 Current State

The current implementation uses a `varTable` map that associates a single `varId` with a pair of heap addresses `(writerAddr, readerAddr)`. A `VarRef` contains both a `varId` and an `isReader` flag to distinguish which cell is being referenced. This design adds complexity without benefit.

### 1.2 Target State

Variables are identified by their heap addresses directly. The `allocateVariable()` function returns two distinct addresses. A `VarRef` contains only an address. The cell's tag (`WrtTag` or `RoTag`) determines whether it is a writer or reader. There is no `varTable` and no separate ID namespace.

### 1.3 Scope

This refactor affects the core runtime heap and all code that references variables. It does not change the GLP language semantics, bytecode format, or multiagent protocol. The multiagent layer (V_p, IrmaContext) will need updates to work with addresses instead of varIds, but the protocol semantics remain unchanged.

### 1.4 Address Stability Confirmation

Heap addresses are integer indices into a `List<HeapCell>`. Dart's garbage collector does not compact or relocate List elements, so these indices remain stable for the lifetime of the heap. This design is safe and does not require any special handling for GC.

### 1.5 Dart Version Confirmation

The project uses Dart SDK ^3.9.4 (per glp_runtime/pubspec.yaml). Dart records, required for the `(int, int)` return type of `allocateVariable()`, were introduced in Dart 3.0 and are fully supported.

## 2. Files to Modify

### 2.1 Core Runtime

| File | Changes |
|------|---------|
| `glp_runtime/lib/runtime/heap_fcp.dart` | Remove varTable, change allocateVariable signature, remove compatibility methods, update _bindCallbacks key |
| `glp_runtime/lib/runtime/terms.dart` | Change VarRef to hold only addr, remove isReader field, update MutualRefTerm |
| `glp_runtime/lib/runtime/suspend_ops.dart` | Update to use addresses, remove varTable lookups |
| `glp_runtime/lib/runtime/commit.dart` | Update applySigmaHatFCP to use addresses, remove varTable lookups |
| `glp_runtime/lib/runtime/runtime.dart` | Update suspended map keys, update _waitReaders values |

### 2.2 Bytecode Runner

| File | Changes |
|------|---------|
| `glp_runtime/lib/bytecode/runner.dart` | Update all VarRef usage, sigmaHat keys, suspension sets (Note: clauseVars is unaffected — see Section 3.4) |

### 2.3 External I/O

| File | Changes |
|------|---------|
| `glp_runtime/lib/runtime/external_io.dart` | Update channel creation, InputInjector._currentWriterId, OutputObserver._currentReaderId |

### 2.4 Multiagent Layer

| File | Changes |
|------|---------|
| `glp_runtime/lib/multiagent/irma_context.dart` | Update V_p to use addresses, update message handling |
| `glp_runtime/lib/multiagent/irma_agent.dart` | Update variable allocation and references |
| `glp_runtime/lib/multiagent/variable_table.dart` | Update VarKey and VarEntry to use addresses |
| `glp_runtime/lib/multiagent/payload_serializer.dart` | Update serialization to work with addresses |
| `glp_multiagent/lib/main.dart` | Update all variable allocation and references |

### 2.5 Compiler (if needed)

| File | Changes |
|------|---------|
| `glp_runtime/lib/compiler/*.dart` | Review for any varId assumptions |

## 3. Detailed Changes

### 3.1 heap_fcp.dart

**Remove:**
```dart
final Map<int, (int, int)> varTable = {};  // varId -> (writerAddr, readerAddr)
int nextVarId = 1000;
```

**Change allocateVariable:**
```dart
// BEFORE
int allocateVariable() {
  final varId = nextVarId++;
  final wAddr = HP++;
  final rAddr = HP++;
  cells.add(HeapCell(Pointer(rAddr), CellTag.WrtTag));
  cells.add(HeapCell(null, CellTag.RoTag));
  varTable[varId] = (wAddr, rAddr);
  return varId;
}

// AFTER
(int, int) allocateVariable() {
  final writerAddr = HP++;
  final readerAddr = HP++;
  cells.add(HeapCell(Pointer(readerAddr), CellTag.WrtTag));
  cells.add(HeapCell(null, CellTag.RoTag));
  return (writerAddr, readerAddr);
}
```

**Remove compatibility methods:**
- `allocateFreshPair()` — no longer needed
- `allocateFreshVar()` — no longer needed
- `writerIdForReader()` — replaced by address arithmetic (readerAddr - 1 = writerAddr)
- `_varRefFromAddr()` — the O(n) reverse lookup helper becomes unnecessary

**Update _bindCallbacks:**
```dart
// BEFORE
final Map<int, void Function(Term)> _bindCallbacks = {};  // varId → callback

void onBind(int varId, void Function(Term) callback) {
  if (isFullyBound(varId)) {
    // ...
  }
  _bindCallbacks[varId] = callback;
}

// AFTER
final Map<int, void Function(Term)> _bindCallbacks = {};  // writerAddr → callback

void onBind(int writerAddr, void Function(Term) callback) {
  if (isFullyBound(writerAddr)) {
    // ...
  }
  _bindCallbacks[writerAddr] = callback;
}
```

**Update all methods that use varTable:**
- `derefAddr()` — remove varTable lookup for VarRef handling
- `isFullyBound()` — take address directly
- `getValue()` — take address directly
- `bindVariable()` — take writerAddr directly, return type `List<GoalRef>` preserved
- `getSuspensions()` — take readerAddr directly
- `addSuspension()` — take readerAddr directly

**Add helper methods:**
```dart
/// Get paired reader address from writer address (allocation structure only)
int readerAddrFor(int writerAddr) => writerAddr + 1;

/// Get paired writer address from reader address (allocation structure only)
int writerAddrFor(int readerAddr) => readerAddr - 1;

/// Check if address is a writer
bool isWriter(int addr) => cells[addr].tag == CellTag.WrtTag;

/// Check if address is a reader
bool isReader(int addr) => cells[addr].tag == CellTag.RoTag;
```

### 3.2 terms.dart

**Change VarRef:**
```dart
// BEFORE
class VarRef extends Term {
  final int varId;
  final bool isReader;
  VarRef(this.varId, {this.isReader = false});
  
  @override
  bool operator ==(Object other) =>
      other is VarRef && other.varId == varId && other.isReader == isReader;
  
  @override
  int get hashCode => Object.hash(varId, isReader);
}

// AFTER
class VarRef extends Term {
  final int addr;
  VarRef(this.addr);
  
  @override
  bool operator ==(Object other) =>
      other is VarRef && other.addr == addr;
  
  @override
  int get hashCode => addr.hashCode;
}
```

**VarRef in Collections Audit:**
A codebase audit confirmed that `VarRef` is not used as a key in any `Map` or `Set` collections. The equality/hashing change is therefore low risk. The only collections that might contain VarRef are term structures (StructTerm args), which compare by identity or use deep term comparison, not VarRef-specific hashing.

**Change MutualRefTerm:**
```dart
// BEFORE
class MutualRefTerm implements Term {
  int _currentWriterId;  // varId of current unbound tail
  final int id;

  MutualRefTerm(this._currentWriterId) : id = _nextId++;

  int get currentWriterId => _currentWriterId;
  set currentWriterId(int varId) => _currentWriterId = varId;
}

// AFTER
class MutualRefTerm implements Term {
  int _currentWriterAddr;  // address of current unbound tail writer
  final int id;

  MutualRefTerm(this._currentWriterAddr) : id = _nextId++;

  int get currentWriterAddr => _currentWriterAddr;
  set currentWriterAddr(int addr) => _currentWriterAddr = addr;
}
```

All code that creates or updates MutualRefTerm must change from using varId to using writerAddr. This includes stream append operations and mutual_ref predicates.

### 3.3 suspend_ops.dart

**Current implementation uses varTable:**
```dart
static void suspendGoalFCP({
  required HeapFCP heap,
  required int goalId,
  required int kappa,
  required Set<int> readerVarIds,  // Variable IDs (not reader IDs)
}) {
  final sharedRecord = SuspensionRecord(goalId, kappa);

  for (final varId in readerVarIds) {
    var finalVarId = varId;
    var (_, rAddr) = heap.varTable[finalVarId]!;  // <-- varTable lookup
    var cell = heap.cells[rAddr];

    // Follow variable chain if reader is bound to another variable
    while (cell.content is VarRef) {
      final nextVar = cell.content as VarRef;
      finalVarId = nextVar.varId;  // <-- uses varId
      (_, rAddr) = heap.varTable[finalVarId]!;  // <-- varTable lookup
      cell = heap.cells[rAddr];
    }
    // ... rest of suspension logic
  }
}
```

**After refactor:**
```dart
static void suspendGoalFCP({
  required HeapFCP heap,
  required int goalId,
  required int kappa,
  required Set<int> readerAddrs,  // Reader addresses directly
}) {
  final sharedRecord = SuspensionRecord(goalId, kappa);

  for (final readerAddr in readerAddrs) {
    var currentAddr = readerAddr;
    var cell = heap.cells[currentAddr];

    // Follow variable chain if reader is bound to another variable
    while (cell.content is VarRef) {
      final nextVar = cell.content as VarRef;
      currentAddr = nextVar.addr;  // <-- direct address, no lookup
      cell = heap.cells[currentAddr];
    }
    // ... rest of suspension logic (unchanged)
  }
}
```

The key changes are: parameter type becomes `Set<int> readerAddrs`, varTable lookups are removed, and VarRef chain following uses `addr` directly. The logic is simpler because addresses can be used directly without translation.

### 3.4 runner.dart

**Update sigmaHat:**
The tentative substitution `sigmaHat` is keyed by writer addresses:
```dart
final Map<int, Object?> sigmaHat = {};  // writerAddr -> tentative value
```

**Compatibility layer transition:** During Phase 1, the compatibility method returns writerAddr as the "varId". Since sigmaHat is keyed by whatever allocateVariable returns, the keys become addresses automatically when callers are updated. No special transition logic is needed.

**Update suspension sets:**
The sets `si` and `U` contain reader addresses:
```dart
final Set<int> si = {};  // reader addresses for clause-local suspension
final Set<int> U = {};   // reader addresses for goal-level suspension
```

**clauseVars is unaffected:**
```dart
final Map<int, Object?> clauseVars = {};  // varIndex → value
```

This maps bytecode register indices (varIndex) to values, NOT varIds. This is an instruction-level concept that refers to clause-local variable slots, not heap variables. This map is unaffected by the refactor.

**Update all VarRef construction:**
```dart
// BEFORE
VarRef(varId, isReader: true)
VarRef(varId, isReader: false)

// AFTER  
VarRef(readerAddr)
VarRef(writerAddr)
```

**Update all VarRef inspection:**
```dart
// BEFORE
if (term is VarRef && term.isReader) { ... }
final varId = term.varId;

// AFTER
if (term is VarRef && heap.isReader(term.addr)) { ... }
final addr = term.addr;
```

### 3.5 commit.dart

**Current implementation uses varTable:**
```dart
static List<GoalRef> applySigmaHatFCP({
  required HeapFCP heap,
  required Map<int, Object?> sigmaHat,  // varId → value
}) {
  final activations = <GoalRef>[];
  
  for (final entry in sigmaHat.entries) {
    final varId = entry.key;
    final value = entry.value;
    
    final (wAddr, rAddr) = heap.varTable[varId]!;  // varTable lookup
    
    // ... binding logic using wAddr, rAddr
  }
  
  return activations;
}
```

**After refactor:**
```dart
static List<GoalRef> applySigmaHatFCP({
  required HeapFCP heap,
  required Map<int, Object?> sigmaHat,  // writerAddr → value
}) {
  final activations = <GoalRef>[];
  
  for (final entry in sigmaHat.entries) {
    final writerAddr = entry.key;
    final value = entry.value;
    
    final readerAddr = writerAddr + 1;  // Simple arithmetic, no lookup
    
    // ... binding logic using writerAddr, readerAddr (unchanged)
  }
  
  return activations;
}
```

The return type `List<GoalRef>` is preserved. The only change is how addresses are obtained: directly from sigmaHat keys instead of via varTable lookup.

### 3.6 runtime.dart

**Update suspended map:**
```dart
// BEFORE
final Map<int, Set<GoalRef>> suspended = <int, Set<GoalRef>>{};
// Keyed by reader varId

// AFTER
final Map<int, Set<GoalRef>> suspended = <int, Set<GoalRef>>{};
// Keyed by reader address
```

The type signature is unchanged, but the semantics of the key changes from varId to readerAddr.

**Update _waitReaders:**
```dart
// BEFORE
final Map<int, int> _waitReaders = <int, int>{};
// Maps goalId → reader varId

// AFTER
final Map<int, int> _waitReaders = <int, int>{};
// Maps goalId → reader address
```

Again, type signature unchanged, but key semantics change.

**Update methods that use these maps:**
- `checkWaitState(int goalId)` — uses _waitReaders[goalId] to get reader, then checks if bound
- `setWaitReader(int goalId, int readerId)` — second parameter becomes readerAddr
- `suspendGoalFCP(...)` — updates suspended map with reader addresses

### 3.7 external_io.dart

**Update ExternalChannel:**
```dart
class ExternalChannel {
  final String name;
  final int inputWriterAddr;   // was: inputVarId
  final int inputReaderAddr;
  final int outputWriterAddr;  // was: outputVarId
  final int outputReaderAddr;
}
```

**Update createExternalChannel:**
```dart
ExternalChannel createExternalChannel(HeapFCP heap, String name) {
  final (inputWriterAddr, inputReaderAddr) = heap.allocateVariable();
  final (outputWriterAddr, outputReaderAddr) = heap.allocateVariable();
  return ExternalChannel(
    name: name,
    inputWriterAddr: inputWriterAddr,
    inputReaderAddr: inputReaderAddr,
    outputWriterAddr: outputWriterAddr,
    outputReaderAddr: outputReaderAddr,
  );
}
```

**Update InputInjector:**
```dart
// BEFORE
class InputInjector {
  int _currentWriterId;
  
  List<GoalRef> inject(Term term) {
    final tailId = heap.allocateFreshVar();
    // ...
    _currentWriterId = tailId;
  }
}

// AFTER
class InputInjector {
  int _currentWriterAddr;
  
  List<GoalRef> inject(Term term) {
    final (tailWriterAddr, _) = heap.allocateVariable();
    // ...
    _currentWriterAddr = tailWriterAddr;
  }
}
```

**Update OutputObserver:**
```dart
// BEFORE
class OutputObserver {
  int _currentReaderId;
  
  void _observeNext() {
    heap.onBind(_currentReaderId, (Term value) {
      // ...
      if (tail is VarRef) {
        _currentReaderId = tail.varId;  // <-- uses varId
        _observeNext();
      }
    });
  }
}

// AFTER
class OutputObserver {
  int _currentReaderAddr;
  
  void _observeNext() {
    final writerAddr = _currentReaderAddr - 1;  // Get paired writer
    heap.onBind(writerAddr, (Term value) {
      // ...
      if (tail is VarRef) {
        _currentReaderAddr = tail.addr;  // <-- uses addr directly
        _observeNext();
      }
    });
  }
}
```

Note: The `onBind` callback is registered on the writer address (since binding happens to writers), but the observer tracks the reader address for stream traversal.

### 3.8 Multiagent Layer

**Current VarKey structure (from variable_table.dart):**
```dart
class VarKey {
  final int varId;
  final bool isReader;
}

class VariableEntry {
  final int varId;
  final bool isReader;
  final String creator;
  final int creatorLocalId;
  final VariableRole role;
  dynamic state;
}
```

The VarKey does NOT embed creator information — creator is stored in VariableEntry. The table is keyed by (varId, isReader) locally, with creator information in the entry value. The `findByCreatorLocalId` method scans entries to find by global identity.

**After refactor:**
```dart
class VarKey {
  final int addr;  // The address uniquely identifies writer vs reader
}

class VariableEntry {
  final int addr;              // Local heap address
  final String creator;        // Agent who created this variable
  final int creatorLocalAddr;  // Creator's heap address (was creatorLocalId)
  final VariableRole role;
  dynamic state;
}
```

Since the address itself distinguishes writer from reader (via cell tag), `isReader` is no longer needed in the key. The `findByCreatorLocalId` method becomes `findByCreatorLocalAddr`.

**Migration complexity:** The migration is straightforward because:
1. VarKey simply drops `isReader` and renames `varId` to `addr`
2. VariableEntry similarly updates field names
3. The lookup logic remains the same (keyed by local address, search for global identity)
4. Creator information stays in VariableEntry, not in the key

### 3.9 Global Variable Identity

For multiagent communication, variables need globally unique identifiers. The format is:

```
globalId = "creatorAgentId:creatorLocalAddr"
```

Where `creatorLocalAddr` is the heap address at the creating agent. When deserializing at the receiving agent, a fresh local address pair is allocated and the mapping is stored in V_p.

## 4. Migration Strategy

### 4.1 Phase 1: Core Heap (Isolated)

Modify `heap_fcp.dart` and `terms.dart` in isolation. Create a temporary compatibility layer that maintains the old API signatures while internally using the new design. This allows incremental migration of callers.

**Compatibility layer methods (temporary):**

| Method | Purpose | Removed In |
|--------|---------|------------|
| `allocateVariableCompat()` | Returns writerAddr as "varId" for unmigrated code | Phase 4 |
| `varTableLookup(int addr)` | Emulates varTable: returns (addr, addr+1) if addr is valid writer | Phase 4 |
| `bindVariableByVarId(int varId, Term value)` | Wrapper that treats varId as writerAddr | Phase 4 |

```dart
// Temporary bridge during migration - returns writerAddr as "varId"
int allocateVariableCompat() {
  final (writerAddr, _) = allocateVariable();
  return writerAddr;
}

// Temporary varTable emulation for code not yet migrated
(int, int)? varTableLookup(int addr) {
  if (addr >= HP || addr < 0) return null;
  if (cells[addr].tag != CellTag.WrtTag) return null;
  return (addr, addr + 1);
}

// Wrapper for unmigrated binding calls
List<GoalRef> bindVariableByVarId(int varId, Term value) {
  return bindVariable(varId, value);  // varId IS writerAddr
}
```

**Checkpoint:** Run full test suite. All tests should pass. Create git commit.

### 4.2 Phase 2: Update Callers

Update each caller file one at a time, running tests after each change:

1. `commit.dart`
2. `suspend_ops.dart`
3. `runtime.dart`
4. `runner.dart`
5. `external_io.dart`

**Checkpoint:** Run full test suite. All tests should pass. Create git commit.

### 4.3 Phase 3: Multiagent Layer

Update the multiagent layer after the core runtime is stable:

1. `variable_table.dart`
2. `irma_context.dart`
3. `irma_agent.dart`
4. `payload_serializer.dart`
5. `main.dart` (glp_multiagent)

**Checkpoint:** Run full test suite including multiagent tests. All tests should pass. Create git commit.

### 4.4 Phase 4: Remove Compatibility Layer

Once all callers are updated, remove the temporary compatibility methods from heap_fcp.dart:
- `allocateVariableCompat()`
- `varTableLookup()`
- `bindVariableByVarId()`
- Any remaining `varTable` references

**Final checkpoint:** Run full test suite. All tests should pass. Create final git commit.

## 5. Testing Strategy

### 5.1 Baseline Tests

Before making any changes, run all existing tests and record results:

```bash
cd glp_runtime && dart test 2>&1 | tee /tmp/baseline-runtime-tests.txt
cd glp_multiagent && dart test 2>&1 | tee /tmp/baseline-multiagent-tests.txt
```

Document the exact number of passing/failing tests as the acceptance baseline.

### 5.2 Critical Checkpoint Tests

The following tests specifically exercise the varId→address boundary and must pass at each phase:

**Phase 1 (Core Heap):**
- `test/srsw_test.dart` — tests variable allocation and binding
- `test/circular_term_test.dart` — tests dereferencing with cycles
- `test/glp_runtime_test.dart` — general runtime tests

**Phase 2 (Callers):**
- `test/bytecode/arithmetic_test.dart` — tests runner with sigmaHat
- `test/conformance/fairness_26_test.dart` — tests suspension/reactivation
- `test/conformance/restart_clause1_test.dart` — tests clause retry with suspension

**Phase 3 (Multiagent):**
- `test/multiagent/variable_table_test.dart` — tests V_p operations
- `test/multiagent/irma_context_test.dart` — tests message handling
- `test/multiagent/payload_serializer_test.dart` — tests serialization
- `test/multiagent/social_agent_integration_test.dart` — end-to-end test

### 5.3 Incremental Testing

After each file modification, run tests to catch regressions immediately:

```bash
dart test
```

If any test fails that was passing at baseline, stop and fix before proceeding.

### 5.4 Phase Checkpoints

At each phase boundary (end of Phase 1, 2, 3, 4):
1. Run full test suite for both glp_runtime and glp_multiagent
2. Verify all baseline-passing tests still pass
3. Run the critical checkpoint tests for that phase explicitly
4. Create a git commit with clear message describing the phase
5. The codebase should be in a working state suitable for handover

This ensures work can be safely paused at any phase boundary.

### 5.5 Integration Testing

After completing the core runtime changes, run the full multiagent integration test:

1. Launch coordinator
2. Spawn Alice, Bob, Charlie
3. Execute introduction protocol
4. Verify message delivery

### 5.6 New Tests

Add specific tests for the new design:

```dart
test('allocateVariable returns distinct writer and reader addresses', () {
  final heap = HeapFCP();
  final (writerAddr, readerAddr) = heap.allocateVariable();
  expect(writerAddr, isNot(equals(readerAddr)));
  expect(readerAddr, equals(writerAddr + 1));
});

test('cell tags correctly identify writer vs reader', () {
  final heap = HeapFCP();
  final (writerAddr, readerAddr) = heap.allocateVariable();
  expect(heap.isWriter(writerAddr), isTrue);
  expect(heap.isReader(readerAddr), isTrue);
  expect(heap.isWriter(readerAddr), isFalse);
  expect(heap.isReader(writerAddr), isFalse);
});

test('VarRef equality uses address only', () {
  final ref1 = VarRef(100);
  final ref2 = VarRef(100);
  final ref3 = VarRef(101);
  expect(ref1, equals(ref2));
  expect(ref1, isNot(equals(ref3)));
});
```

## 6. Rollback Plan

If critical issues are discovered during migration:

1. All changes are made in a feature branch
2. The baseline test results serve as the acceptance criteria
3. If tests fail after a change, revert that specific change and investigate
4. The compatibility layer allows partial rollback if needed
5. Phase checkpoints provide known-good states to return to

## 7. Risk Assessment

### 7.1 High Risk Areas

**Bytecode runner:** The runner has extensive VarRef handling throughout. Careful review needed to ensure all cases are updated.

**commit.dart:** Direct varTable access and sigmaHat processing. Central to the binding mechanism.

**Multiagent serialization:** The payload serializer converts between local and global variable identities. This is conceptually unchanged but the representation changes.

**Suspension tracking:** The runtime's `suspended` map and the heap's suspension lists must consistently use addresses.

### 7.2 Medium Risk Areas

**External I/O callbacks:** The `_bindCallbacks` map and `onBind` mechanism interact with both heap binding and external observers. Key semantics must be consistent.

**MutualRefTerm:** Used for O(1) stream append. The `_currentWriterAddr` field must be updated consistently wherever MutualRefs are created or modified.

### 7.3 Low Risk Areas (Confirmed)

**VarRef in collections:** A codebase audit confirmed VarRef is not used as a Map or Set key anywhere. The equality/hashing change does not affect collection semantics.

**clauseVars:** Maps bytecode register indices to values, not varIds. Unaffected by refactor.

## 8. Success Criteria

1. All baseline tests pass after refactor
2. No new test failures introduced
3. `varTable` and `varId` concepts completely removed from codebase
4. `VarRef` contains only `addr` field
5. `allocateVariable()` returns `(writerAddr, readerAddr)`
6. Multiagent introduction protocol works end-to-end

## 9. Timeline Estimate

| Phase | Estimated Duration | Checkpoint |
|-------|-------------------|------------|
| Phase 1: Core Heap | 2-3 hours | Full test run, git commit |
| Phase 2: Update Callers | 3-4 hours | Full test run, git commit |
| Phase 3: Multiagent Layer | 3-4 hours | Full test run, git commit |
| Phase 4: Cleanup | 30 minutes | Full test run, git commit |
| **Total** | **9-12 hours** | |

**Note:** The timeline accounts for the scope of changes and the importance of careful testing at each phase. Each phase boundary is a safe stopping point if work needs to be paused.

## 10. Open Questions

### 10.1 resumePC vs kappa in SuspensionRecord

The spec states that suspended goals resume at PC = kappa (procedure entry point), implementing wake-and-retry semantics. The SuspensionRecord stores both `goalId` and `resumePC`. If resumePC always equals kappa, storing both may be redundant. This is not a blocker for the refactor but could be simplified in a future cleanup.

## 11. Document History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2026-01-19 | Claude | Initial plan |
| 1.1 | 2026-01-19 | Claude | Addressed reviewer feedback: added address stability confirmation, Dart version confirmation, VarRef collection audit, detailed suspend_ops.dart changes, clarified VarKey structure, added phase checkpoints, revised timeline estimate |
| 1.2 | 2026-01-19 | Claude | Addressed second review: added commit.dart (Section 3.5), added MutualRefTerm (Section 3.2), added runtime.dart details (Section 3.6), added _bindCallbacks (Section 3.1), specified compatibility layer methods (Section 4.1), added critical checkpoint tests (Section 5.2), noted clauseVars unaffected (Section 3.4), updated risk assessment (Section 7) |
