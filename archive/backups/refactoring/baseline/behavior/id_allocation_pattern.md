# ID Allocation Pattern Analysis
Date: 2025-11-09
Commit: 20fd92ce

## Current Implementation

### allocateFreshPair() Behavior
Location: `lib/runtime/heap.dart:23-27`

```dart
(int, int) allocateFreshPair() {
  final writerId = _nextId++;
  final readerId = _nextId++;
  return (writerId, readerId);
}
```

### Pattern Confirmed
- **Writer ID**: N (even when starting from 1000)
- **Reader ID**: N+1 (always Writer + 1)
- **Consistent**: Yes, every pair follows this pattern
- **Pairing**: Stored in `WriterCell(writerId, readerId)`

### Lookup Complexity
- **writerIdForReader()**: O(n) scan through all writers
- **Location**: `lib/runtime/heap.dart:44-49`
- **Impact**: Performance degrades linearly with heap size

### Example Allocations
```
Pair 1: Writer=1000, Reader=1001 (diff=1)
Pair 2: Writer=1002, Reader=1003 (diff=1)
Pair 3: Writer=1004, Reader=1005 (diff=1)
...
```

## Issues Identified
1. **O(n) lookup** - no direct reader→writer mapping
2. **Separate IDs** - unnecessary complexity vs single ID per variable
3. **No optimization** - no caching or path compression for dereferencing

## Refactoring Target
Single ID system where:
- Variable ID = N
- WriterTerm(N) and ReaderTerm(N) both reference same ID
- Direct O(1) lookup
