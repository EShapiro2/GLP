# Single-ID Variable System Migration

**Status**: Complete (November 2025)
**Branch**: `single-id-migration` → merged to `main`

## Overview

GLP has completed migration from a **two-ID variable system** to a **single-ID variable system**, aligning with the true FCP (Flat Concurrent Prolog) design.

### What Changed

**Before (Two-ID System)**:
- Each variable had TWO IDs: `writerId` and `readerId`
- Separate types: `WriterTerm(writerId)` and `ReaderTerm(readerId)`
- Pairing tracked in heap: writer 77 ↔ reader 7077
- Complex pairing lookups required

**After (Single-ID System)**:
- Each variable has ONE ID: `varId`
- Unified type: `VarRef(varId, isReader: bool)`
- Access mode indicated by `isReader` flag:
  - `isReader: false` → writer (can bind)
  - `isReader: true` → reader (can read/suspend)
- Simplified heap operations

## Core Design Principles

### 1. One Variable, Two Access Modes

```dart
// Same variable accessed as writer and reader
final varId = heap.allocateFreshVar();

final writer = VarRef(varId, isReader: false);  // Can bind
final reader = VarRef(varId, isReader: true);   // Can read/suspend
```

### 2. True FCP Single-ID

The key insight from FCP: **writerId == readerId == varId**

```dart
// OLD two-ID: Different IDs for same variable
(writerId: 77, readerId: 7077)

// NEW single-ID: Same ID, different access modes
(varId: 1000, isReader: false)  // Writer
(varId: 1000, isReader: true)   // Reader
```

### 3. Heap Operations Simplified

```dart
// Single-ID heap operations use varId directly
heap.isBound(varId)          // Check if bound
heap.getValue(varId)         // Get value (null if unbound)
heap.bindVariable(varId, term)  // Bind to term
heap.addSuspension(varId, goalId)  // Suspend on reader
```

## API Changes

### Heap API

**Old Two-ID API**:
```dart
heap.isWriterBound(writerId)
heap.valueOfWriter(writerId)
heap.writerIdForReader(readerId)  // Lookup pairing
```

**New Single-ID API**:
```dart
heap.isBound(varId)          // Replaces isWriterBound
heap.getValue(varId)         // Replaces valueOfWriter
// No pairing lookup needed - readerId == writerId == varId
```

### Term Construction

**Old**:
```dart
WriterTerm(writerId)
ReaderTerm(readerId)
```

**New**:
```dart
VarRef(varId, isReader: false)  // Writer
VarRef(varId, isReader: true)   // Reader
```

### Fresh Variable Allocation

**Old**:
```dart
final (writerId, readerId) = heap.allocateFreshPair();
// writerId != readerId (e.g., 77 and 7077)
```

**New (FCP-aligned)**:
```dart
final (writerId, readerId) = heap.allocateFreshPair();
// writerId == readerId (e.g., 1000 and 1000)
```

## Implementation Details

### VarRef Class

```dart
class VarRef implements Term {
  final int varId;
  final bool isReader;

  VarRef(this.varId, {required this.isReader});

  @override
  String toString() => isReader ? 'R$varId?' : 'W$varId';
}
```

### Heap Structure

```dart
class Heap {
  // Single ID per variable
  final Map<int, VariableCell> _vars = {};

  // ROQ: reader suspensions (varId -> Set<goalId>)
  final Map<int, Set<int>> _roq = {};

  // Compatibility: Track old two-ID pairings during migration
  final Map<int, int> _readerToWriter = {};  // For old tests
  final Map<int, int> _writerToReader = {};  // For old tests
}

class VariableCell {
  final int varId;
  Term? value;
  Term? dereferencedCache;  // Path compression optimization
}
```

### Key Operations

**Binding**:
```dart
void bindVariable(int varId, Term value) {
  final cell = _vars[varId];
  if (cell != null && cell.value == null) {
    cell.value = value;
    _processROQ(varId);  // Wake suspended goals
  }
}
```

**Dereferencing**:
```dart
Term dereference(Term term) {
  if (term is! VarRef) return term;

  Set<int> visited = {};
  Term current = term;

  while (current is VarRef) {
    if (!isBound(current.varId)) return current;
    if (visited.contains(current.varId)) {
      throw StateError('Cycle detected - SRSW violation!');
    }
    visited.add(current.varId);
    current = getValue(current.varId)!;
  }
  return current;
}
```

## Migration Process

The migration was completed in phases:

### Phase 1: Prepare (Commits: 3b97ab6 - 2a2cbbe)
- Created HeapV2 with single-ID design
- Implemented compatibility layer
- Runtime switched to use HeapV2 directly

### Phase 2: Migrate Construction Sites (Commits: da00037 - bb09d51)
- Updated all `WriterTerm()` → `VarRef(id, isReader: false)`
- Updated all `ReaderTerm()` → `VarRef(id, isReader: true)`
- Fixed 14 construction sites in runner.dart
- Updated test files

### Phase 3: True Single-ID (Commits: f9df891 - 1002444)
- Changed `allocateFreshPair()` to return `(varId, varId)`
- Simplified `writerIdForReader()` (just returns varId)
- Fixed REPL display and variable handling

### Phase 4: Cleanup (Commits: ec97408 - 7b155c8)
- Deleted legacy files: heap_v2_adapter.dart, migration_helper.dart
- Removed WriterTerm and ReaderTerm classes
- Renamed heap_v2.dart → heap.dart
- Fixed REPL and system predicates
- Disabled DEBUG output

### Phase 5: Documentation (Commit: 72cc192)
- Reorganized directory structure
- Updated CLAUDE.md with structure guide
- Created helper scripts for Claude Web

## Testing Status

**As of November 2025**:
- 134 tests passing
- 35 tests failing (old two-ID test fixtures)
- REPL fully functional
- Core system working correctly

**Failing tests** use old two-ID setup code:
```dart
// OLD: Creates variables with different IDs
final wc = WriterCell(77, 7077);  // writerId != readerId
```

**To fix**, update to single-ID:
```dart
// NEW: Same ID for both writer and reader
final varId = heap.allocateFreshVar();
heap.addVariable(varId);
```

## Benefits of Single-ID System

1. **Theoretical correctness**: Aligns with FCP formal semantics
2. **Simplified implementation**: No pairing lookups needed
3. **Clearer semantics**: One variable, two access modes
4. **Reduced complexity**: Fewer maps, simpler logic
5. **Better performance**: Fewer indirections
6. **Easier debugging**: varId directly identifies variable

## Compatibility Layer

For gradual migration, Heap maintains compatibility methods:

```dart
// Compatibility: Old two-ID API during migration
bool isWriterBound(int writerId) => isBound(writerId);
Term? valueOfWriter(int writerId) => getValue(writerId);

int? writerIdForReader(int readerId) {
  // In single-ID: readerId == writerId
  return _vars.containsKey(readerId) ? readerId : null;
}
```

These are **deprecated** and will be removed once all tests are updated.

## Migration Guide for Remaining Code

If you find old two-ID code:

### 1. Update Term Construction
```dart
// OLD
WriterTerm(wid)
ReaderTerm(rid)

// NEW
VarRef(varId, isReader: false)
VarRef(varId, isReader: true)
```

### 2. Update Type Checks
```dart
// OLD
if (term is WriterTerm) { ... }
if (term is ReaderTerm) { ... }

// NEW
if (term is VarRef && !term.isReader) { ... }  // Writer
if (term is VarRef && term.isReader) { ... }   // Reader
```

### 3. Update Property Access
```dart
// OLD
term.writerId
term.readerId

// NEW
term.varId  // Same property for both
```

### 4. Update Heap Calls
```dart
// OLD
heap.isWriterBound(writerId)
heap.valueOfWriter(writerId)
heap.writerIdForReader(readerId)

// NEW
heap.isBound(varId)
heap.getValue(varId)
// No pairing lookup needed
```

### 5. Update Test Fixtures
```dart
// OLD
final wc = WriterCell(77, 7077);
heap.addWriter(wc);

// NEW
final varId = heap.allocateFreshVar();
heap.addVariable(varId);
```

## References

- **FCP Paper**: Houri & Shapiro (1989) - `/Users/udi/GLP/docs/1-s2.0-0743106689900113-main.pdf`
- **FCP Implementation**: https://github.com/EShapiro2/FCP
- **GLP Spec**: `/Users/udi/GLP/docs/glp_spec.pdf`
- **Runtime Spec**: `/Users/udi/GLP/docs/glp-runtime-spec.txt`

## Commits

Key commits in the single-ID migration:

1. `3b97ab6` - feat: Step 2.4 (partial) - Switch to HeapV2 directly
2. `2a2cbbe` - docs: Step 2.4 progress - Runtime using HeapV2 directly (90% complete)
3. `da00037` - fix: Add bidirectional writer<->reader pairing maps for old two-ID tests
4. `bb09d51` - refactor: Category 3 complete - all construction sites migrated to VarRef
5. `1002444` - refactor: Update test files to handle VarRef instead of ReaderTerm
6. `f9df891` - fix: Implement true single-ID in HeapV2 (FCP-aligned)
7. `846780d` - fix: REPL display now dereferencing VarRef properly
8. `ec97408` - cleanup: Remove two-ID legacy code, single-ID migration complete
9. `7b155c8` - fix: Complete REPL and system predicates migration to single-ID
10. `72cc192` - chore: Reorganize directory structure for clarity

## Future Work

- Update remaining 35 test fixtures to use single-ID paradigm
- Remove compatibility layer from Heap
- Add performance benchmarks comparing old vs new system
- Document best practices for writing new tests
