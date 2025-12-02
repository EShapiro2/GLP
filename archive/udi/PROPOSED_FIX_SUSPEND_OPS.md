# Proposed Fix for suspend_ops.dart Bug

## Problem Summary

The current implementation creates ONE shared `SuspensionRecord` and tries to prepend it to multiple reader cells' suspension lists. Each iteration modifies the shared record's `next` pointer, corrupting all previously updated lists.

## FCP Design vs Our Implementation

After checking FCP source (`/Users/udi/Dropbox/Concurrent Prolog/FCP/Savannah/Logix/EMULATOR/macros.h`):

**FCP uses a suspension TABLE (STP)**, not linked lists in cells:
```c
#define sus_tbl_add(Address) \
{ \
  if (Address < HB) { \
    *(++STP) = (heapT) Address; \
  } \
  ...
}
```

FCP stores addresses in a separate table, then processes them at suspension time.

**Our implementation stores linked lists IN reader cells**, which is a different design that's closer to standard implementations but requires proper list management.

## Proposed Fix: Separate Records

Since we're storing suspension lists in cells (not a separate table), we need **separate records** for each reader to maintain list independence.

### Fixed Code

```dart
/// Suspension operations using FCP-inspired design
/// Records stored directly in reader cells (separate record per reader)
class SuspendOps {
  /// Suspend goal on multiple readers
  /// Creates separate record for each reader to maintain list independence
  static void suspendGoalFCP({
    required HeapFCP heap,
    required int goalId,
    required int kappa,
    required Set<int> readerVarIds,
  }) {
    // Create SEPARATE record for each reader (not shared)
    // This maintains list independence while preserving disarm semantics
    for (final varId in readerVarIds) {
      final (_, rAddr) = heap.varTable[varId]!;
      final cell = heap.cells[rAddr];

      // Create NEW record for this reader
      final record = SuspensionRecord(goalId, kappa);

      // Prepend to existing list
      record.next = cell.content is SuspensionRecord
          ? cell.content as SuspensionRecord
          : null;

      // Store in reader cell
      cell.content = record;
    }
  }

  /// Legacy version - removed
}
```

## Why This Works

1. **List Independence**: Each reader cell has its own suspension list with its own chain
2. **Correct Prepending**: Each record's `next` pointer only affects that reader's list
3. **Disarming Still Works**: All records have the same `goalId`/`resumePC`, so when walking ANY list:
   ```dart
   if (current.armed) {
     acts.add(GoalRef(current.goalId!, current.resumePC));
     current.disarm();  // Sets goalId = null
   }
   ```
   Once disarmed, subsequent records with same goalId in other lists will also be skipped (they're separate objects but have same goalId value)

Wait - **this has a problem**: separate records means separate `armed` flags, so disarming one doesn't disarm the others!

## Better Fix: Track Already Activated Goals

Instead of relying on shared record disarming, track which goals have already been activated:

### Option A: Track in Commit/Heap

Modify `_walkAndActivate` to track activated goals:

```dart
static void _walkAndActivate(SuspensionRecord? list, List<GoalRef> acts, Set<int> alreadyActivated) {
  var current = list;
  while (current != null) {
    if (current.armed && !alreadyActivated.contains(current.goalId)) {
      acts.add(GoalRef(current.goalId!, current.resumePC));
      alreadyActivated.add(current.goalId!);  // Track, don't disarm
      current.disarm();  // Still disarm for this list
    }
    current = current.next;
  }
}
```

But this requires passing `alreadyActivated` through all the call sites...

### Option B: Simplest - Deduplicate at Enqueue

The scheduler/runtime already has a goal queue. Just deduplicate when enqueuing:

```dart
// In commit.dart or wherever reactivations are enqueued
for (final g in reactivations) {
  if (!gq.contains(g.id)) {  // Only enqueue if not already in queue
    gq.enqueue(g);
  }
}
```

But this doesn't prevent the same goal being enqueued multiple times if it runs between bindings...

### Option C: Use Shared Record Correctly (RECOMMENDED)

The real issue is we're trying to use a linked list node as a shared object. Instead:

**Keep ONE shared record, but DON'T use it as a list node directly.**

Store the shared record separately and have each cell point to it:

```dart
// In SuspensionRecord
class SuspensionRecord {
  int? goalId;
  final int resumePC;
  // Remove 'next' - this record is not a list node

  SuspensionRecord(this.goalId, this.resumePC);

  void disarm() { goalId = null; }
  bool get armed => goalId != null;
}

// New wrapper class for list nodes
class SuspensionListNode {
  final SuspensionRecord record;  // Points to shared record
  SuspensionListNode? next;       // List chain

  SuspensionListNode(this.record);

  bool get armed => record.armed;  // Delegate to shared record
  int? get goalId => record.goalId;
  int get resumePC => record.resumePC;
}
```

Then:

```dart
static void suspendGoalFCP({
  required HeapFCP heap,
  required int goalId,
  required int kappa,
  required Set<int> readerVarIds,
}) {
  // Create ONE shared record
  final sharedRecord = SuspensionRecord(goalId, kappa);

  // Create separate list node for each reader
  for (final varId in readerVarIds) {
    final (_, rAddr) = heap.varTable[varId]!;
    final cell = heap.cells[rAddr];

    // Create wrapper node pointing to shared record
    final node = SuspensionListNode(sharedRecord);

    // Prepend to existing list
    node.next = cell.content is SuspensionListNode
        ? cell.content as SuspensionListNode
        : null;

    // Store in reader cell
    cell.content = node;
  }
}
```

Walking the list:

```dart
static void _walkAndActivate(SuspensionListNode? list, List<GoalRef> acts) {
  var current = list;
  while (current != null) {
    if (current.armed) {  // Checks shared record's armed flag
      acts.add(GoalRef(current.goalId!, current.resumePC));
      current.record.disarm();  // Disarms shared record - affects ALL nodes
    }
    current = current.next;
  }
}
```

**This preserves FCP semantics**: one record shared across lists, disarming once prevents all reactivations.

## Recommendation

Implement **Option C** - the wrapper node approach:

1. Keep `SuspensionRecord` as shared state (goalId, resumePC, armed flag)
2. Add `SuspensionListNode` to wrap records in linked lists
3. Each reader cell stores a `SuspensionListNode` pointing to the shared record
4. Disarming the shared record disables ALL nodes that point to it

This is the most faithful to FCP's "shared record" design while working correctly with linked lists.

## Files to Modify

1. `/Users/udi/GLP/glp_runtime/lib/runtime/suspension.dart`
   - Keep `SuspensionRecord` but remove `next` field
   - Add `SuspensionListNode` class

2. `/Users/udi/GLP/glp_runtime/lib/runtime/suspend_ops.dart`
   - Modify `suspendGoalFCP` to create wrapper nodes

3. `/Users/udi/GLP/glp_runtime/lib/runtime/heap_fcp.dart`
   - Update `_walkAndActivate` to walk `SuspensionListNode`
   - Update type checks from `SuspensionRecord` to `SuspensionListNode`

## Testing

After fix:
1. Run baseline tests: `dart test`
2. Run REPL tests: `bash run_repl_tests.sh`
3. Test `run(quicksort([1,2,3],X))` - should succeed
4. Verify no double-activation with debug traces
