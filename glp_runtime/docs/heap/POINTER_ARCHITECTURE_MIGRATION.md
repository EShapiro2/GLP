# Pointer Architecture Migration - Phase 5 Continuation

## Current Status

Branch: `pointer-architecture`

The pointer architecture migration is partially complete. Phases 1-4 are done. Phase 5 (updating callers) is in progress with several files completed and others still needing updates.

### Completed Files
- `lib/runtime/terms.dart` - VarRef simplified to single `addr` field
- `lib/runtime/heap_fcp.dart` - New pointer architecture with derefAddr, allocateVariable returns tuple
- `lib/runtime/suspend_ops.dart` - Updated for pointer architecture
- `lib/runtime/commit.dart` - Updated for pointer architecture  
- `lib/runtime/scheduler.dart` - Updated for pointer architecture
- `lib/runtime/body_kernels.dart` - Updated for pointer architecture
- `lib/runtime/module_runtime.dart` - Updated for pointer architecture
- `lib/compiler/codegen.dart` - Updated for pointer architecture

### Files Still Needing Updates
- `lib/bytecode/runner.dart` - **MAIN FILE** - has 50+ occurrences needing transformation
- Possibly: `lib/runtime/external_io.dart`, `lib/runtime/irma_context.dart`, `lib/runtime/payload_serializer.dart`

## Transformation Patterns

Apply these mechanical transformations throughout the codebase:

### 1. VarRef Field Access
```dart
// OLD
varRef.varId
varRef.isReader

// NEW  
varRef.addr
rt.heap.isReader(varRef.addr)  // or heap.isReader(addr)
rt.heap.isWriter(varRef.addr)  // inverted from isReader
```

### 2. VarRef Constructor
```dart
// OLD
VarRef(id, isReader: true)
VarRef(id, isReader: false)

// NEW
VarRef(addr)  // Just the address, no isReader parameter
```

### 3. Heap Method Renames
```dart
// OLD
heap.writerIdForReader(readerId)
heap.isWriterBound(writerId)
heap.allocateVariable()  // returned single int

// NEW
heap.writerForReader(readerAddr)
heap.isFullyBound(addr)
heap.allocateVariable()  // returns (writerAddr, readerAddr) tuple
```

### 4. Tuple Destructuring for allocateVariable
```dart
// OLD
final varId = heap.allocateVariable();

// NEW
final (writerAddr, readerAddr) = heap.allocateVariable();
// Then use whichever address you need
```

### 5. MutualRefTerm Field
```dart
// OLD
mutualRef.currentWriterId

// NEW
mutualRef.currentWriterAddr
```

### 6. Reader/Writer Logic Inversion
When checking if something is a reader, the logic often needs inversion:
```dart
// OLD (VarRef had isReader field)
if (varRef.isReader) { ... }
if (!varRef.isReader) { ... }  // writer check

// NEW (query heap for cell type)
if (heap.isReader(varRef.addr)) { ... }
if (heap.isWriter(varRef.addr)) { ... }  // direct writer check
```

## Testing

Run heap tests to verify changes:
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart test test/heap/
```

Current baseline: 78/83 tests passing (5 failing due to remaining compilation errors)

Target: All 83 tests passing after migration complete.

## Key Files Reference

### VarRef Definition (lib/runtime/terms.dart)
```dart
class VarRef extends Term {
  final int addr;  // Single field - heap cell address
  VarRef(this.addr);
  // No isReader field - query heap.isReader(addr) or heap.isWriter(addr)
}
```

### Heap API (lib/runtime/heap_fcp.dart)
Key methods:
- `allocateVariable()` → `(int writerAddr, int readerAddr)`
- `derefAddr(int addr)` → `Term` (follows chain, returns value or VarRef to final unbound writer)
- `isWriter(int addr)` → `bool`
- `isReader(int addr)` → `bool`
- `writerForReader(int readerAddr)` → `int?`
- `isFullyBound(int addr)` → `bool`
- `getValue(int addr)` → `Term?`
- `bindVariable(int writerAddr, Term value)`

## Strategy for runner.dart

The runner.dart file is large (~4800 lines). Use grep to find all occurrences:

```bash
grep -n "\.varId\|\.isReader\|writerIdForReader\|VarRef.*isReader:" lib/bytecode/runner.dart
```

Key areas in runner.dart that need updates:
1. `_finalUnboundVar()` - already updated
2. `_formatTerm()` - needs updating (uses varId, isReader, writerIdForReader)
3. Various instruction handlers that create or inspect VarRef instances
4. `_getVarDisplayName()` - likely needs updating
5. Any `visited` sets that track varId need to use addr instead

## Notes

- The spec document for the pointer architecture is at `docs/heap/pointer-architecture-spec.md`
- Tests for the new architecture are in `test/heap/pointer_architecture_test.dart` and `test/heap/varref_pointer_test.dart`
- The architecture separates "where" (addr) from "what kind" (heap cell tag)
