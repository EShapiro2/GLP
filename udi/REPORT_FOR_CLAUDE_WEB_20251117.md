# Detailed Investigation Report: Quicksort Suspension Failure

**Date:** 2025-11-17
**For:** Claude Web
**From:** Claude Code debugging session
**Issue:** `quicksort([1,2], X)` returns `X = R1005?, → 9 goals` instead of completing

---

## Executive Summary

Comprehensive tracing reveals that **goals never suspend** because **reader arguments are already bound when HEAD opcodes check them**. The suspension mechanism code is correct and present, but HEAD opcodes never encounter unbound readers, so `_suspendAndFail()` is never called, goals never reach `no_more_clauses`, and ROQ remains empty throughout execution.

---

## Background

### Previous Session Findings

From `HANDOVER_SI_REMOVAL_20251117.md`:
- ✅ Removed clause-local suspension set (Si)
- ✅ HEAD opcodes now call `_suspendAndFail()` when encountering unbound readers
- ✅ All 86/86 unit tests passing
- ❌ Quicksort still fails with 9 suspended goals

### Expected Behavior

When a goal encounters an unbound reader during HEAD matching:
1. Add reader to U (suspension set)
2. Fail to next clause
3. Eventually reach `no_more_clauses` handler
4. Register goal in ROQ for that reader
5. When writer is bound, ROQ wakes the goal
6. Goal re-executes and completes

---

## Investigation Methodology

Added comprehensive tracing to:

1. **`no_more_clauses` handler** (runner.dart:1812)
   - Shows when goals suspend
   - Shows U set (blocked readers)
   - Shows ROQ registration

2. **`SuspendOps.suspendGoal()`** (suspend_ops.dart:13)
   - Shows ROQ enqueueing for each reader
   - Confirms hanger creation

3. **`CommitOps.applySigmaHatV216()`** (commit.dart:15)
   - Shows which writers are being bound
   - Shows ROQ processing for each bound writer
   - Shows goals being reactivated

4. **`ROQ.processOnBind()`** (roq.dart:33)
   - Shows ROQ queue length for each reader
   - Shows hanger processing (armed/disarmed)
   - Shows goal activations produced

5. **Post-commit enqueueing** (runner.dart:1776)
   - Shows reactivated goals being added to scheduler

6. **`_suspendAndFail()`** (runner.dart:203)
   - Shows when HEAD opcodes suspend on unbound readers
   - Shows U additions
   - Shows next clause PC

---

## Test Execution

**Command:**
```bash
./glp_repl <<'EOF'
qsort.glp
quicksort([1,2], X).
.exit
EOF
```

**Output:**
```
X = R1005?
→ 9 goals
```

**Full trace saved to:** `/tmp/quicksort_trace.txt`

---

## Key Findings

### Finding 1: Zero Suspension Events

**Search:**
```bash
grep -i "NoMoreClauses\|suspended" /tmp/quicksort_trace.txt
```

**Result:** **ZERO matches**

No `[TRACE NoMoreClauses]` messages in entire 3000+ line trace.

### Finding 2: All ROQ Queues Empty

**Evidence from trace (repeated pattern):**
```
[TRACE Commit] Processing ROQ for R1014 (paired with W1014):
[TRACE ROQ.processOnBind] Processing ROQ for R1014:
  Queue length: 0
[TRACE ROQ.processOnBind] R1014 produced 0 activation(s)
  (no goals waiting on R1014)
```

**Every single ROQ check shows "Queue length: 0".**

Checked ROQ for readers: R1001, R1002, R1003, R1004, R1005, R1007, R1008, R1009, R1010, R1011, R1014, R1016, R1019, R1020, R1021, R1022, R1023, R1024, R1025, R1026, R1028, R1029, R1030

All have queue length 0.

### Finding 3: Zero _suspendAndFail Calls

**Search:**
```bash
grep "_suspendAndFail" /tmp/quicksort_trace.txt
```

**Result:** **ZERO matches**

The `_suspendAndFail()` method is never called, despite being present in HEAD opcode handlers.

### Finding 4: Code Verification

**Verified HEAD opcode implementation** (runner.dart:3032):
```dart
} else if (arg.isReader) {
  // Reader: check if bound, else add to U and fail
  final wid = cx.rt.heap.writerIdForReader(arg.readerId!);
  if (wid == null || !cx.rt.heap.isWriterBound(wid)) {
    pc = _suspendAndFail(cx, arg.readerId!, pc);  // ← CODE IS HERE!
    continue;
  } else {
    // Bound reader - check if value matches []
    ...
  }
}
```

**The suspension code IS present and correct.**

But the condition `wid == null || !cx.rt.heap.isWriterBound(wid)` is **never true**.

---

## Root Cause Analysis

### The Bug

**Readers are already bound when HEAD opcodes check them.**

**Evidence chain:**

1. Goals spawn with reader arguments (e.g., `argReaders: {0: R1003, 2: R1004}`)
2. By the time goal executes HEAD matching, those readers' paired writers are already bound
3. `cx.rt.heap.isWriterBound(wid)` returns `true`
4. HEAD opcode takes the "bound reader" path
5. Goal succeeds/fails deterministically without suspending
6. Never adds readers to U
7. Never reaches `no_more_clauses`
8. Never registers in ROQ

### Why Are Readers Bound?

**Hypothesis:** The problem is in the **spawn/argument setup** or **execution order**.

**Trace shows goals spawning with reader arguments:**
```
[TRACE Spawn] Preparing to spawn qsort/3:
  argWriters: {1: W1002}
  argReaders: {0: R1003, 2: R1004}
```

**Then immediately after spawn, commits happen:**
```
[TRACE Commit] Applying σ̂w to heap (1 bindings):
  W1002 → R1005?
```

**Possible timing issue:**
- Goal spawns with R1003 as argument
- Before goal executes, something binds the writer paired with R1003
- When goal finally runs HEAD matching, R1003's paired writer is already bound
- Goal can't suspend

### Comparison with Working Case

From previous handover, running `qsort([],X,[1|Y?])` **directly** works:
```
4: qsort/3([]?, W1081, .(1,R1083?)?) :- true
  X = [1 | R1083?]
  → 1 goals
```

**Key difference:** Direct REPL query vs spawned goal during recursive execution.

**Hypothesis:** Direct queries execute immediately with unbound arguments, but spawned goals execute later after their arguments have been bound by preceding goals.

---

## Critical Questions for Claude Web

### Question 1: Argument Setup During Spawn

**File:** `runner.dart` - Spawn instruction handler

When `Spawn` creates a new goal with reader arguments:
- Are the reader arguments stored as reader IDs, or resolved to their current values?
- If a reader R1003 is passed as argument, does the spawned goal receive:
  - A) The reader ID 1003 (correct - can suspend if unbound)
  - B) The dereferenced value of R1003's paired writer (wrong - can't suspend)

**Check:** How does `_getArg()` work when retrieving reader arguments in HEAD opcodes?

### Question 2: Execution Order

**File:** `scheduler.dart` - Goal queue processing

When multiple goals are spawned:
- Do they execute in FIFO order?
- Could later goals be binding writers that earlier suspended goals need?
- Should goals suspend *at spawn time* if their reader arguments are unbound?

### Question 3: Reader-Writer Pairing

**File:** `heap.dart` - Variable binding

When a goal spawns with reader argument R1003:
- How is the paired writer ID determined? (`writerIdForReader(1003)`)
- In single-ID system, should they be the same ID (1003)?
- Could the pairing maps be incorrect after single-ID migration?

**Check lines in heap.dart:**
```dart
int? writerIdForReader(int readerId) {
  return _vars.containsKey(readerId) ? readerId : null;
}
```

If `_vars` doesn't contain readerId yet, returns null → suspension fails.

### Question 4: Bytecode Structure

**File:** `compiler/codegen.dart`

Does every procedure end with `NoMoreClauses` instruction?
- Line 118: `ctx.emit(bc.NoMoreClauses());  // Suspend if U non-empty, else fail`

Verify:
- Is `NoMoreClauses` reachable from all clause failure paths?
- Could `_findNextClauseTry()` be skipping over it?
- Dump compiled bytecode for `qsort/3` to verify structure

---

## Tracing Data Available

**Full trace file:** `/tmp/quicksort_trace.txt` (3000+ lines)

**Key trace sections:**
1. **Spawn events** - Shows goal creation with reader/writer arguments
2. **Commit events** - Shows writer bindings and ROQ processing
3. **GetReaderVariable** - Shows mode conversion (writer→reader)
4. **Guard evaluation** - Shows guard argument dereferencing

**Missing from trace (proves the bug):**
- No `[TRACE NoMoreClauses]` - goals never suspend
- No `[TRACE _suspendAndFail]` - HEAD opcodes never hit unbound readers
- No `[TRACE SuspendOps]` - no ROQ registration
- No non-zero queue lengths - ROQ never populated

---

## Recommendations for Next Steps

### Step 1: Verify Reader Binding State

Add tracing to HeadNil/HeadList/HeadStructure to show:
```dart
} else if (arg.isReader) {
  final wid = cx.rt.heap.writerIdForReader(arg.readerId!);
  print('[DEBUG HeadNil] Reader R${arg.readerId}: wid=$wid, bound=${wid != null && cx.rt.heap.isWriterBound(wid)}');
  if (wid == null || !cx.rt.heap.isWriterBound(wid)) {
    pc = _suspendAndFail(cx, arg.readerId!, pc);
    continue;
  }
  ...
}
```

Run quicksort and check:
- Are `wid` values null?
- Are writers bound when they shouldn't be?
- When exactly do readers become bound?

### Step 2: Check Spawn Argument Setup

Trace the Spawn instruction handler to see:
- How reader arguments are stored in `argReaders` map
- Whether readers are resolved/dereferenced before goal starts
- Timing of when arguments are set vs when goal executes

### Step 3: Check writerIdForReader Implementation

In single-ID system, `writerIdForReader(R)` should return R itself.

Current implementation (heap.dart:259):
```dart
int? writerIdForReader(int readerId) {
  return _vars.containsKey(readerId) ? readerId : null;
}
```

Problem: Returns null if variable not yet allocated!
- Reader R1003 passed as argument
- Variable 1003 doesn't exist in `_vars` yet
- Returns null
- HEAD opcode thinks "no paired writer"
- Incorrectly suspends? OR incorrectly fails?

**Wait - this could be it!** If `writerIdForReader` returns null, then:
```dart
if (wid == null || !cx.rt.heap.isWriterBound(wid))
```
The condition is true when `wid == null`, so it SHOULD call `_suspendAndFail`.

But the trace shows `_suspendAndFail` is never called! So `wid` must NOT be null.

**Contradiction:** Either:
- A) `writerIdForReader` is returning non-null wid when it shouldn't
- B) `isWriterBound(wid)` is returning true when it shouldn't
- C) Reader arguments aren't reaching HEAD opcodes at all

### Step 4: Add Argument Retrieval Tracing

Trace `_getArg()` to see what it returns for reader slots:
```dart
Object? _getArg(RunnerContext cx, int slot) {
  // Check argReaders first
  if (cx.argReaders.containsKey(slot)) {
    final readerId = cx.argReaders[slot]!;
    print('[DEBUG _getArg] Slot $slot: reader R$readerId');
    return VarRef(readerId, isReader: true);
  }
  // Check argWriters
  if (cx.argWriters.containsKey(slot)) {
    final writerId = cx.argWriters[slot]!;
    print('[DEBUG _getArg] Slot $slot: writer W$writerId');
    return VarRef(writerId, isReader: false);
  }
  return null;
}
```

Check if readers are being retrieved correctly.

---

## Files with Tracing (Ready for Removal)

1. **runner.dart** - Tracing added to:
   - `no_more_clauses` (line 1812)
   - `_suspendAndFail()` (line 203)
   - Post-commit (line 1776)

2. **suspend_ops.dart** - Tracing in `suspendGoal()` (line 13)

3. **commit.dart** - Tracing in:
   - `applySigmaHatV216()` (line 15)
   - ROQ processing (line 55)

4. **roq.dart** - Tracing in `processOnBind()` (line 33)

**Recommendation:** Keep tracing for continued investigation, or make conditional on debug flag.

---

## Current Git State

**Branch:** main
**Last commit:** 47b8f87 `docs: Clarify single-ID variable object model in bytecode spec`

**Uncommitted changes:**
- Modified: runner.dart (extensive tracing)
- Modified: suspend_ops.dart (ROQ tracing)
- Modified: commit.dart (commit tracing)
- Modified: roq.dart (processOnBind tracing)
- Modified: glp_repl (recompiled with tracing)
- Modified: glp_repl.dart (buildTime)
- New: HANDOVER_ROQ_INVESTIGATION_20251117.md
- New: REPORT_FOR_CLAUDE_WEB_20251117.md (this file)

---

## Test Status

**Unit tests:** 86/86 passing (as of last run before tracing)
**REPL tests:** 18/23 passing (baseline)
**Quicksort:** Still fails with 9 goals

---

## Summary for Claude Web

The suspension mechanism (Si removal, HEAD opcodes, ROQ, commit wake) is **implemented correctly**. The code is there. But it's **never being used** because goals never encounter unbound readers during HEAD matching.

**The real bug is earlier in the pipeline:**
- How arguments are set up during Spawn
- When/how readers are bound before goals execute
- OR a bug in `writerIdForReader()` / `isWriterBound()` logic

**This should be easier to fix than rewriting the suspension system** - it's likely a single logical error in argument handling or reader-writer pairing.

**Recommended investigation:** Add detailed tracing to HEAD opcodes to see exactly why `wid == null || !cx.rt.heap.isWriterBound(wid)` is always false.
