# Handover Report: Si Removal and HEAD Opcode Suspension Fix

**Date:** 2025-11-17
**Session:** Claude Code continuation from HANDOVER_GETWRITERVALUE_FIX_20251117.md
**For:** Claude Web / Next Session

---

## Summary

Simplified the GLP suspension mechanism by removing the clause-local suspension set (Si) and having HEAD opcodes fail immediately when encountering unbound readers. Created helper methods to enforce atomic "add to U and fail to next clause" semantics. All unit tests pass, but quicksort still fails due to separate ROQ wake issue.

---

## Changes Made

### 1. Spec Simplification (docs/glp-bytecode-v216-complete.md)

**Removed Si (clause-local suspension set):**
- Section 0: Removed Si definition, kept only U (goal-level suspension set)
- Section 2.1 (try_clause): Removed Si initialization
- Section 2.2 (clause_next): Removed Si→U union operation
- Section 6 (Commit): Removed Si check before commit

**Updated HEAD opcode semantics:**
- All HEAD opcodes (head_nil, head_list, head_constant, head_structure) now specify:
  - "If reader variable: add reader to U and fail to next clause"
- This is ONE atomic operation, not two separate steps

**Key principle added:**
> When a HEAD instruction encounters an unbound reader, it adds the reader to U and immediately fails to the next clause. This is a single atomic operation.

### 2. Implementation Changes (glp_runtime/lib/bytecode/runner.dart)

**Removed Si from RunnerContext (line 117):**
```dart
// REMOVED:
// final Set<int> si = <int>{};  // clause-local blockers

// KEPT:
final Set<int> U = <int>{};      // goal-level suspension set
```

**Updated clearClause() (line 164-171):**
- Removed `si.clear()` call
- Only clears clauseVars, sigmaHat now

**Added helper methods (lines 200-213):**
```dart
/// Suspend on unbound reader: add to U and fail to next clause atomically
int _suspendAndFail(RunnerContext cx, int readerId, int currentPc) {
  cx.U.add(readerId);
  _softFailToNextClause(cx, currentPc);
  return _findNextClauseTry(currentPc);
}

/// Suspend on multiple unbound readers
int _suspendAndFailMulti(RunnerContext cx, Set<int> readerIds, int currentPc) {
  cx.U.addAll(readerIds);
  _softFailToNextClause(cx, currentPc);
  return _findNextClauseTry(currentPc);
}
```

**Updated HEAD opcodes to use helpers:**

- **HeadNil** (~line 3013):
  ```dart
  if (wid == null || !cx.rt.heap.isWriterBound(wid)) {
    pc = _suspendAndFail(cx, arg.readerId!, pc);
    continue;
  }
  ```

- **HeadList** (~line 3073): Same pattern

- **HeadConstant** (~line 416): Same pattern

- **HeadStructure** (~lines 556, 683, 390, 646): Same pattern

- **GUARD evaluation** (~line 2636):
  ```dart
  if (unboundReaders.isNotEmpty) {
    pc = _suspendAndFailMulti(cx, unboundReaders, pc);
    continue;
  }
  ```

**Removed Si references:**
- ClauseNext (line 1792): Removed Si→U union
- Commit check (lines 1651-1656): Removed Si emptiness check

### 3. REPL Update (udi/glp_repl.dart)

**Updated buildTime:**
```dart
final buildTime = '2025-11-17T14:00:00Z (FIX: Remove Si, HEAD opcodes fail immediately on unbound readers)';
```

---

## Implementation Process

### Design Evolution

**Old design:**
1. HEAD opcodes add unbound readers to Si (clause-local set)
2. At clause_next, Si is merged into U
3. At commit, check if Si is empty before proceeding

**New design:**
1. HEAD opcodes add unbound readers directly to U
2. HEAD opcodes fail immediately to next clause
3. No Si tracking needed

**Rationale:**
- Simpler: One suspension set instead of two
- Correct: Matches fail-fast semantics from spec
- Safer: Helper methods make atomic operation impossible to forget

### Bug Fix Process

1. **Initial attempt:** Just replaced `cx.si.add()` with `cx.U.add()` (11 locations)
   - **Problem:** Forgot to add fail-to-next-clause logic
   - **User caught this:** "does cx.U.add softfail the clause?"

2. **User suggestion:** Create helper method to enforce atomicity
   - "why can't cx.U.add() add the variable AND fail the clause, why another routine?"

3. **Final solution:** Created `_suspendAndFail()` and `_suspendAndFailMulti()` helpers
   - Makes atomic "add to U and fail" operation explicit
   - Impossible to forget the fail logic
   - Clear intent in code

4. **Batch update:** Used sed to replace patterns, then manually fixed helper itself
   - Sed accidentally modified the helper method body (line 203)
   - Fixed by restoring correct implementation

---

## Test Results

### Unit Tests: ✅ 86/86 Passing
No regressions. All existing tests pass.

### REPL Tests: 18/23 Passing (Same as Baseline)

**Passing tests (18):**
- Tests 1-4, 6-8, 10-12, 14-17, 20-23

**Failing tests (5):**
- Test 5: Merge with Reader (SRSW violation - test file issue)
- Test 9: Insertion Sort via Metainterpreter
- Test 13: Structure Demo
- Test 18: Insertion sort two elements
- Test 19: Quicksort two elements [1,2]

**No change from baseline** - these were already failing.

### Manual Test: quicksort([1,2], X)

**Result:**
```
X = R1005?
→ 9 goals
```

**Status:** Still fails (same as before the fix)

**Trace shows:**
- Goals are spawning with reader arguments
- Dependencies eventually get bound via Commit operations
- But suspended goals are NOT waking up

---

## Remaining Issue: ROQ Wake Mechanism

### The Problem

The Si removal and HEAD opcode changes are **correct and working**:
- ✅ HEAD opcodes properly suspend on unbound readers
- ✅ Goals add readers to U and fail to next clause atomically
- ✅ All unit tests pass

But quicksort still fails because **suspended goals don't wake up** when their dependencies are bound.

### Why Suspended Goals Aren't Waking

From the handover HANDOVER_GETWRITERVALUE_FIX_20251117.md:

> **Expected behavior:** Suspended goals should:
> 1. Suspend when encountering unbound readers during HEAD matching ✅ NOW WORKING
> 2. Be added to ROQ (Read-Only Queue) for those readers ❓ UNKNOWN
> 3. Wake up when the writers are bound ❌ NOT HAPPENING
> 4. Re-execute and succeed ❌ NOT HAPPENING

**Root cause:** Either:
1. Goals are not being added to ROQ when they suspend, OR
2. Goals are being added to ROQ but not woken up when writers are bound, OR
3. Goals are waking up but in an invalid state that causes immediate failure

### Next Steps for Investigation

From the previous handover recommendations:

1. **Add ROQ tracing:**
   - NoMoreClauses - Show when goal is being added to ROQ
   - ROQ wake mechanism - Show when goals are reactivated
   - Heap.bindWriter - Show when writers are bound and which ROQs are triggered

2. **Focus on specific goal:**
   - Identify which spawned goal should complete (e.g., goal for `qsort([],X,[1|Y?])`)
   - Trace why it's not waking when dependencies are bound

3. **Check ROQ registration:**
   - Are goals being added to the correct reader ROQs?
   - Are the ROQs being checked when writers are bound?

---

## Files Modified

1. **`/Users/udi/GLP/docs/glp-bytecode-v216-complete.md`**
   - Removed all Si references
   - Updated HEAD opcode semantics to specify atomic "add to U and fail"
   - Simplified section 0, 2.1, 2.2, 6.1-6.6

2. **`/Users/udi/GLP/glp_runtime/lib/bytecode/runner.dart`**
   - Line 117: Removed Si field from RunnerContext
   - Lines 164-171: Updated clearClause() to not clear Si
   - Lines 200-213: Added _suspendAndFail() and _suspendAndFailMulti() helpers
   - Multiple locations: Updated HEAD opcodes to use helpers
   - Line 1792: Removed Si→U union from ClauseNext
   - Lines 1651-1656: Removed Si check from Commit

3. **`/Users/udi/GLP/udi/glp_repl.dart`**
   - Line 25: Updated buildTime to reflect changes

---

## Git Status

**Branch:** main
**Last commit:** b026e45 `fix: GetWriterValue isReader flag + add tracing`

**Uncommitted changes:**
- Modified: docs/glp-bytecode-v216-complete.md
- Modified: glp_runtime/lib/bytecode/runner.dart
- Modified: udi/glp_repl.dart
- Untracked: udi/HANDOVER_SI_REMOVAL_20251117.md (this file)

**Ready to commit with message:**
```
refactor: Remove Si, HEAD opcodes fail immediately on unbound readers

- Removed clause-local suspension set (Si)
- HEAD opcodes now add to U and fail to next clause atomically
- Added _suspendAndFail() helpers to enforce atomic semantics
- Updated bytecode spec to reflect simplified suspension model

Tests: 86/86 unit tests passing
REPL: 18/23 tests (same as baseline)
Note: Quicksort still fails due to separate ROQ wake issue
```

---

## Key Takeaways

### What Worked ✅
1. Spec simplification - removing Si makes semantics clearer
2. Helper methods - enforce atomic "add to U and fail" operation
3. All unit tests continue to pass
4. Code is cleaner and more maintainable

### What Didn't Change ❌
1. Quicksort still fails with 9 suspended goals
2. REPL test results unchanged (18/23)
3. The core wake mechanism issue remains

### Design Insight 💡

The user's suggestion to create helper methods was crucial:
> "why can't cx.U.add() add the variable AND fail the clause, why another routine?"

This led to `_suspendAndFail()` which:
- Makes atomic semantics explicit in code
- Prevents bugs from forgetting the fail step
- Matches spec language "add to U and fail to next clause"
- Self-documenting code

---

## Recommendations

1. **Commit these changes** - They are correct and improve code quality
2. **Next investigation:** Focus on ROQ wake mechanism, not suspension
3. **Add targeted tracing:** NoMoreClauses and Heap.bindWriter operations
4. **Compare execution paths:** Why does direct REPL call work but spawned goals fail?

The Si removal is a successful refactoring. The quicksort issue is a separate problem in the goal reactivation system.
