# ROQ Critical Bug Report - 2025-11-19

## Bug Location

**File**: `/Users/udi/GLP/glp_runtime/lib/runtime/suspend_ops.dart`
**Lines**: 20-35
**Function**: `suspendGoalFCP()`

## The Bug

The code attempts to create ONE shared `SuspensionRecord` and prepend it to multiple reader cells' suspension lists. But each iteration modifies the shared record's `next` pointer, **corrupting ALL previously updated lists**.

### Buggy Code

```dart
// Create ONE shared suspension record
final record = SuspensionRecord(goalId, kappa);  // Line 21

// Prepend to each reader cell's suspension list
for (final varId in readerVarIds) {              // Line 24
  final (_, rAddr) = heap.varTable[varId]!;
  final cell = heap.cells[rAddr];

  // Prepend to existing list (or null if none)
  record.next = cell.content is SuspensionRecord ? cell.content as SuspensionRecord : null;  // Line 29 - BUG!

  // REPLACE reader cell content with suspension list
  cell.content = record;  // Line 32
}
```

### Why It's Broken

When suspending on multiple readers (e.g., `U = {R1012, R1024, R1037}`):

**Iteration 1** - R1012:
```
record.next = null           // R1012 has no existing suspensions
cells[rAddr1012].content = record
// Result: R1012 → [record → null]  ✓
```

**Iteration 2** - R1024:
```
record.next = S1             // R1024 has existing suspension S1
cells[rAddr1024].content = record
// Result: R1024 → [record → S1]  ✓
// BUT: R1012 now also → [record → S1]  ❌ CORRUPTED!
```

**Iteration 3** - R1037:
```
record.next = S2             // R1037 has existing suspension S2
cells[rAddr1037].content = record
// Result: R1037 → [record → S2]  ✓
// BUT: R1012 now → [record → S2]  ❌ CORRUPTED!
// AND: R1024 now → [record → S2]  ❌ CORRUPTED!
```

### Final State

All three reader cells point to the SAME record, and that record's `next` pointer is whatever the LAST iteration set it to (S2). The previous lists for R1012 and R1024 are LOST.

```
R1012 → record → S2  (should be: record → null)
R1024 → record → S2  (should be: record → S1)
R1037 → record → S2  ✓ (correct by accident)
```

## Root Cause: Impossible Data Structure

The spec says:
> "ONE SuspensionRecord created - That same record prepended to R1's list, R2's list, R3's list"
> (`glp-runtime-spec.txt` lines 334-335)

**This is structurally impossible** with singly-linked lists where each node has only ONE `next` pointer.

You **cannot** prepend the same node object to multiple independent lists without corrupting them.

## Impact on quicksort([1,2,3]) Bug

This bug explains why the suspension/reactivation mechanism isn't working:

1. Goal 10122 suspends on multiple readers in its U set
2. The shared record's `next` pointers get corrupted
3. Suspension lists are broken
4. When variables bind, the wrong (or no) goals get reactivated
5. The program hangs with suspended goals that never wake up

## Possible Solutions

### Option 1: Separate Records (Simplest Fix)

Create a **separate** SuspensionRecord for each reader, instead of trying to share one:

```dart
static void suspendGoalFCP({
  required HeapFCP heap,
  required int goalId,
  required int kappa,
  required Set<int> readerVarIds,
}) {
  // Create SEPARATE record for each reader
  for (final varId in readerVarIds) {
    final (_, rAddr) = heap.varTable[varId]!;
    final cell = heap.cells[rAddr];

    // Create NEW record
    final record = SuspensionRecord(goalId, kappa);

    // Prepend to existing list
    record.next = cell.content is SuspensionRecord ? cell.content as SuspensionRecord : null;

    // Store in reader cell
    cell.content = record;
  }
}
```

**Pros**:
- Simple fix
- Correct behavior
- Each list maintains independence

**Cons**:
- Not "FCP-exact" shared record design
- Uses more memory (N records instead of 1)
- Need to verify disarming still prevents double-activation

### Option 2: Shared Record with Wrapper Nodes

Keep ONE shared record but create wrapper nodes for each list:

```dart
class SuspensionWrapper {
  final SuspensionRecord record;  // Shared record
  SuspensionWrapper? next;        // Individual next pointer

  SuspensionWrapper(this.record);
}
```

Then each reader cell gets its own wrapper pointing to the shared record.

**Pros**:
- Preserves "shared record" semantics
- Correct list independence

**Cons**:
- More complex
- Complicates disarming logic
- More memory overhead

### Option 3: Verify FCP Source Code

Check the actual FCP implementation to see how they handle this:

```
/Users/udi/Dropbox/Concurrent Prolog/FCP/Savannah/Logix/EMULATOR/
```

Look at `emulate.h` or `notify.c` to see the actual suspension record structure and prepending logic.

**If FCP uses separate records**: Fix is Option 1
**If FCP has a different data structure**: Adapt that structure

## Recommendation

**Immediate action**: Implement Option 1 (separate records) as it's the simplest fix and will unblock testing.

**Follow-up action**: Review FCP source code to verify the correct "shared record" semantics and whether our disarming mechanism needs adjustment.

## Testing After Fix

After implementing the fix, test:

1. **Baseline REPL tests**: `bash run_repl_tests.sh`
2. **Quicksort [1,2,3]**: Should now succeed instead of suspending
3. **Multiple suspension**: Verify goals suspended on multiple readers wake correctly
4. **No double-activation**: Ensure shared record concept (via same goalId/PC) prevents duplicate wakeups

## Files to Modify

1. `/Users/udi/GLP/glp_runtime/lib/runtime/suspend_ops.dart` (lines 20-35)
2. Possibly `/Users/udi/GLP/glp_runtime/lib/runtime/suspension.dart` if wrapper approach needed
3. Update `/Users/udi/GLP/docs/glp-runtime-spec.txt` to clarify "shared record" semantics

## Related Files

- `/Users/udi/GLP/glp_runtime/lib/runtime/heap_fcp.dart` - `bindVariable()` processes suspensions
- `/Users/udi/GLP/glp_runtime/lib/bytecode/runner.dart` - Calls `suspendGoalFCP()`
- `/Users/udi/GLP/udi/SUSPENSION_REACTIVATION_ANALYSIS_20251119.md` - Previous analysis

## Connection to Previous Analysis

This bug was **not** identified in the previous analysis (`SUSPENSION_REACTIVATION_ANALYSIS_20251119.md`) because that analysis focused on:
- Reactivation semantics (direct σ̂w membership vs transitive chains)
- GLP paper Definition 3.4.2
- Variable chain dereferencing

But the **real problem** is simpler and more fundamental: the suspension lists themselves are being corrupted during suspension creation, so the reactivation mechanism never even gets correct data to work with!

## Timeline

**2025-11-19 (earlier)**: Focused on variable chains and reactivation semantics
**2025-11-19 (now)**: User asked "can you look at suspend_ops.dart suspension.dart they have not been modified for a while"
**Discovery**: The shared record prepending logic is fundamentally broken

This is a **critical bug** that must be fixed before any other suspension/reactivation analysis can proceed.
