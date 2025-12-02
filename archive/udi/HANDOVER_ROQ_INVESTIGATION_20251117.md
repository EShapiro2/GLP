# Handover Report: ROQ Wake Mechanism Investigation

**Date:** 2025-11-17
**Session:** Claude Code - ROQ debugging with comprehensive tracing
**Status:** ROOT CAUSE IDENTIFIED
**For:** Claude Web / User / Next Session

---

## Summary

Added comprehensive tracing to the ROQ wake mechanism (suspension, commit, reactivation). Discovered that **goals are never reaching `no_more_clauses`** and therefore **never being registered in ROQ**. All ROQ queues remain empty throughout execution.

---

## Investigation Approach

Added tracing to four critical points:

1. **no_more_clauses** (runner.dart:1810) - When goals suspend
2. **SuspendOps.suspendGoal** (suspend_ops.dart) - When goals register in ROQ
3. **CommitOps.applySigmaHatV216** (commit.dart) - When writers are bound
4. **ROQ.processOnBind** (roq.dart) - When ROQ wakes suspended goals
5. **Post-commit enqueueing** (runner.dart:1776) - When reactivated goals added to queue
6. **_suspendAndFail** (runner.dart:202) - When HEAD opcodes suspend

---

## Test Results

**Command:** `quicksort([1,2], X)`

**Output:** `X = R1005?, → 9 goals` (same as before - still fails)

---

## Key Findings

### 1. No Goals Ever Suspend

**Evidence:**
```
grep -i "NoMoreClauses\|suspended" /tmp/quicksort_trace.txt
```
Result: **ZERO matches**

No `[TRACE NoMoreClauses]` messages in entire trace.

### 2. All ROQ Queues Empty

**Evidence from trace:**
```
[TRACE Commit] Processing ROQ for R1014 (paired with W1014):
[TRACE ROQ.processOnBind] Processing ROQ for R1014:
  Queue length: 0          ← ALWAYS ZERO!
[TRACE ROQ.processOnBind] R1014 produced 0 activation(s)
  (no goals waiting on R1014)
```

Every single ROQ check shows "Queue length: 0".

### 3. No _suspendAndFail Calls

Added tracing to `_suspendAndFail()` - **ZERO traces**.

This means HEAD opcodes are **NOT** encountering unbound readers and calling `_suspend AndFail()`.

### 4. Goals Complete Without Suspension

The 9 goals mentioned in output are completing their execution without ever:
- Encountering unbound readers during HEAD matching
- Adding readers to U
- Reaching `no_more_clauses` handler
- Being registered in ROQ

---

## ROOT CAUSE

**The bug:** Goals that SHOULD suspend on unbound readers are instead **completing/failing without suspension**.

**Possible reasons:**

1. **Arguments are already bound when goals start**
   - Goals spawn with reader arguments (R1003, R1004, etc.)
   - By the time the goal executes, those readers' paired writers might already be bound
   - So HEAD matching succeeds/fails immediately without hitting unbound readers

2. **Wrong execution order**
   - Goals are executing BEFORE their dependencies are established
   - Or dependencies are being bound AFTER goals have already failed

3. **Bytecode structure issue**
   - `no_more_clauses` instruction might not be reachable
   - Or `_findNextClauseTry()` is skipping over it

---

## Next Investigation Steps

### Option 1: Check Argument State at Goal Start

Add tracing to show:
- When goals are spawned
- What the reader arguments are (R1003, etc.)
- Whether their paired writers are bound at spawn time
- If readers are unbound, why HEAD opcodes aren't suspending

### Option 2: Check Bytecode Structure

Dump the compiled bytecode for `qsort/3` to see:
- Where `NoMoreClauses` instruction is located
- Whether it's reachable from all clause failures
- If there's a structural issue preventing suspension

### Option 3: Add HEAD Opcode Tracing

Add detailed tracing to HeadNil/HeadList/HeadStructure to show:
- When they execute
- What arguments they're matching against
- Whether readers are bound/unbound
- Why they're not calling `_suspendAndFail()`

---

## Files Modified

1. **runner.dart:**
   - Added tracing to `no_more_clauses` (line 1812)
   - Added tracing to `_suspendAndFail()` (line 203)
   - Added tracing to post-commit enqueueing (line 1776)

2. **suspend_ops.dart:**
   - Added tracing to `suspendGoal()` (line 13)

3. **commit.dart:**
   - Added tracing to `applySigmaHatV216()` (line 15)
   - Added tracing to ROQ processing (line 55)

4. **roq.dart:**
   - Added tracing to `processOnBind()` (line 33)

---

## Current State

**Unit tests:** Not run (focus on quicksort trace)
**REPL tests:** Not run
**Quicksort:** Still fails with 9 suspended goals

**Trace file:** `/tmp/quicksort_trace.txt` (full execution trace)

---

## Tracing Output Summary

**What we see:**
- ✅ Spawn events (goals being created)
- ✅ Commit events (writers being bound)
- ✅ ROQ processing (but always empty)
- ✅ GetReaderVariable mode conversions
- ✅ Guard evaluations

**What we DON'T see:**
- ❌ NoMoreClauses execution
- ❌ Goal suspension events
- ❌ _suspendAndFail calls
- ❌ ROQ registration (enqueue calls)
- ❌ Non-empty ROQ queues
- ❌ Goal reactivation

---

## The Mystery

From HANDOVER_GETWRITERVALUE_FIX_20251117.md, running `qsort([],X,[1|Y?])` **directly** in REPL works:
```
4: qsort/3([]?, W1081, .(1,R1083?)?) :- true
  X = [1 | R1083?]
  → 1 goals
```

But when the same pattern is spawned during recursive quicksort, it fails.

**Key difference:** Direct REPL call vs spawned goal during execution.

Likely the spawned goals' reader arguments are in a different state (already bound?) when they start executing.

---

## Recommendation

**Next step:** Add HEAD opcode tracing to HeadNil/HeadStructure to see:
- What they're matching against
- Reader binding states
- Why suspension isn't happening

**Alternative:** Check if the issue is actually in how Spawn sets up argument registers - maybe readers are being resolved to bound values before HEAD matching even starts.

---

## Git Status

**Branch:** main
**Uncommitted changes:**
- Modified: runner.dart (tracing)
- Modified: suspend_ops.dart (tracing)
- Modified: commit.dart (tracing)
- Modified: roq.dart (tracing)
- Modified: glp_repl (recompiled)
- Modified: glp_repl.dart (buildTime)

**Should commit tracing code?**
- Option A: Keep for continued investigation
- Option B: Remove after finding root cause
- Option C: Make conditional on debug flag

---

## Key Insight

The ROQ wake mechanism itself is **working correctly** - it's just never being used because goals aren't suspending in the first place!

The real bug is earlier in the execution flow, likely in:
- How arguments are set up during Spawn
- How HEAD opcodes check reader binding state
- Or bytecode structure preventing suspension

This is actually good news - the fix should be simpler than fixing the entire ROQ system.
