# Handover: Bidirectional Stream Test Debugging

**Date:** 2026-01-20
**Status:** In Progress
**Test File:** `glp_runtime/test/multiagent/bidirectional_stream_test.dart`

---

## 1. Test Overview

### 1.1 Purpose

The bidirectional stream test demonstrates **circular data flow** between two isolates using the `merge/3` predicate. This is a key maGLP scenario where:
- Each isolate produces output that the other isolate consumes
- The pattern creates an infinite interleaved stream
- The test validates that the irmaGLP message-passing infrastructure correctly handles this circular dependency

### 1.2 Test Scenario

**Program:**
```prolog
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
merge([], [], []).
```

**Goals:**
- Isolate 1: `merge(Xs?, [a], Ys)` — reads Xs from isolate 2, writes Ys
- Isolate 2: `merge(Ys?, [b], Xs)` — reads Ys from isolate 1, writes Xs

**Expected Behavior:**
1. Clause 2 can fire even when the first argument is unbound (it just stores the reader)
2. Isolate 1 produces `Ys = [a | Zs?]` where Zs is a recursive call
3. The assignment `Ys := [a | ...]` should be sent to isolate 2
4. Isolate 2 receives the value, its goal can now proceed
5. This creates the infinite alternating pattern: `[a, b, a, b, ...]`

### 1.3 Variable Setup

```
Isolate 1:
  - Writer Ys at addr=0 (local, registered in V_p as createdWriter)
  - Imported reader Xs? at addr=2 (from isolate 2's writer at addr=0)
    - VariableEntry in heap cell content
    - Registered in V_p with role=importedReader, creator='isolate2'

Isolate 2:
  - Writer Xs at addr=0 (local, registered in V_p as createdWriter)
  - Imported reader Ys? at addr=2 (from isolate 1's writer at addr=0)
    - VariableEntry in heap cell content
    - Registered in V_p with role=importedReader, creator='isolate1'
```

---

## 2. Current Failure Mode

### 2.1 Observed Behavior

When the test runs:
1. Both isolates execute and return `ExecutionStatus.succeeded` immediately
2. **Zero messages are sent** between isolates
3. Results show fresh unbound variables instead of the expected values:
   - `Ys = [_8, _9]` (should be `[a | Zs?]`)
   - `Xs = [_8, _9]` (should be `[b | Zs?]`)

### 2.2 Key Trace Output

```
[DEBUG IRMA isolate1] _onWriterBound: writerId=0, value=.(Var@8,Var@10)
[DEBUG IRMA isolate1] _onWriterBound: entry.role=VariableRole.createdWriter, entry.state=null
[DEBUG IRMA isolate1] _onWriterBound: NO ACTION (role=VariableRole.createdWriter, state=null)
```

The "NO ACTION" message is critical — it means:
- The writer WAS bound to a value `.(Var@8, Var@10)`
- But `state=null` indicates **no requester** has asked for this variable
- Per irmaGLP spec, without a pending request, no assignment message is sent

---

## 3. Root Cause Analysis

### 3.1 The Core Issue: Missing Read Requests

Per the irmaGLP spec (Section 5.2, Reduce Transaction):
- When a goal **suspends** on an imported reader, it should trigger a **read request** to the creator
- The creator stores the requester in `V_p` entry state
- When the writer is later bound, the stored requester receives an assignment

**What's NOT happening:**
- Isolate 2's goal uses imported reader `Ys?` in position 0
- But isolate 2 never sends a read request to isolate 1
- So when isolate 1 binds `Ys`, there's no requester to notify

### 3.2 Why No Read Request?

The test expects clause 2 to fire without suspending:
```prolog
merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
```

Clause 2's first argument is just `Xs` (a writer variable) — it accepts ANY value including an unbound reader. So:
1. Clause 2 matches successfully
2. The goal doesn't suspend on the imported reader
3. No read request is sent
4. The body `merge(Xs?, Ys?, Zs)` is spawned where `Xs?` is the imported reader

The read request should be triggered when the **body's recursive call** eventually needs `Xs?` to be bound.

### 3.3 Why Is the Result Wrong?

The result `[_8, _9]` instead of `[a | Zs?]` suggests:
1. Clause 2's HeadStructure for `[Y?|Zs?]` in WRITE mode is creating fresh variables
2. But `Y` should be bound to `'a'` (extracted from the second argument `[a]`)
3. So `Y?` should be a reader pointing to `'a'`, not a fresh unbound variable

This indicates a problem in how the bytecode execution handles:
- Extracting values in READ mode (from `[Y|Ys]` matching `[a]`)
- Building structures in WRITE mode (for `[Y?|Zs?]`)
- Connecting the extracted value to the output structure

---

## 4. Previous Investigation Attempts

### 4.1 `_finalUnboundVar` Fix (Reverted)

**Problem Identified:** `_finalUnboundVar` in runner.dart treated `VariableEntry` (imported reader with `state=null`) as a "ground term" instead of recognizing it as unbound.

**Fix Attempted:** Added check for `VariableEntry` with `state==null` to return the reader address.

**Status:** This fix is **maGLP-related** and should be re-applied. It was reverted along with unrelated changes.

### 4.2 `skipSubterms` Mechanism (Reverted - Not maGLP)

**Problem Identified:** When HeadStructure encounters an unbound reader, it adds to Si but doesn't set up mode/currentStructure. The following UnifyVariable instructions then execute with undefined state.

**Fix Attempted:** Added `skipSubterms` counter to skip Unify instructions when HeadStructure couldn't process a structure.

**Status:** This is a **general bytecode issue**, not maGLP-specific. Reverted per user request to focus on maGLP.

### 4.3 HeadList `isReaderBound` Fix (Reverted)

**Problem Identified:** HeadList used deprecated `writerForReader` which doesn't work for imported readers.

**Fix Attempted:** Changed to use `isReaderBound` and `getReaderValue` abstraction methods.

**Status:** This is **maGLP-related** and should be re-applied.

---

## 5. Key Files and Components

### 5.1 Test Infrastructure

| File | Purpose |
|------|---------|
| `test/multiagent/bidirectional_stream_test.dart` | The failing test |
| `test/multiagent/shared_variable_test.dart` | Simpler test (p(X)@1, q(X?)@2) - PASSES |

### 5.2 irmaGLP Implementation

| File | Purpose |
|------|---------|
| `lib/multiagent/irma_context.dart` | IrmaContext with V_p, M_p, message handlers |
| `lib/multiagent/variable_table.dart` | VariableEntry, VariableRole, VarKey |
| `lib/multiagent/message_queue.dart` | OutboundMessage, MessageType |

### 5.3 Bytecode Runner

| File | Purpose |
|------|---------|
| `lib/bytecode/runner.dart` | BytecodeRunner, RunnerContext |
| `lib/runtime/heap_fcp.dart` | HeapFCP with pointer architecture |

---

## 6. Specification References

### 6.1 irmaGLP Spec (docs/ma/irmaGLP-spec.md)

**Section 5.2 - Reduce Transaction:**
> When a goal suspends on readers W, for each imported reader X? in W:
> - Send request(X?, p) to creator

**Section 5.3 - Communicate Transaction:**
> For assignment m = (X?:=T) to imported reader:
> - Reactivate suspended goals
> - Apply {X?:=T} to resolvent

### 6.2 Key Invariants

1. **Imported readers have no local writer** — they're represented by VariableEntry in heap cell
2. **VariableEntry.state** tracks:
   - For created writer: requester agent ID (or null)
   - For imported reader: creator ID after request sent (or null)
3. **Read requests** are sent when goal suspends on imported reader
4. **Assignments** are sent when writer is bound AND requester is recorded

---

## 7. Recommended Next Steps

### 7.1 Immediate Actions

1. **Re-apply `_finalUnboundVar` fix** — Correctly identify imported readers as unbound
2. **Re-apply HeadList `isReaderBound` fix** — Use abstraction methods for imported readers
3. **Run multiagent tests** to see new baseline

### 7.2 Investigation Focus

1. **Trace clause 2 execution** — Why does `[Y?|Zs?]` produce `[_8, _9]` instead of `[a | Zs?]`?
2. **Check σ̂w at Commit** — What bindings are being applied?
3. **Verify Y extraction** — Is `Y` being correctly bound to `'a'` from `[Y|Ys]` matching `[a]`?

### 7.3 Hypothesis to Test

The result `Var@8, Var@10` in σ̂w suggests:
- W7 → Const(a) (Y bound to 'a')
- Structure contains Var@8 which should be R8 (reader of W7)

If W7 = 7, then R8 = 8. But derefing Var@8 should give 'a'. The display `[_8, _9]` suggests either:
- The binding wasn't applied to heap
- Or the display function doesn't follow VarRef chains

---

## 8. Baseline Test Status

As of 2026-01-20 (after revert):
- **Unit tests:** 363 passing, 13 failing (pre-existing failures)
- **REPL tests:** 223 passing, 0 failing

The 13 unit test failures include:
- `restart_clause1_test.dart` — Type errors in test itself
- Several multiagent tests — Expected failures being investigated
- `shared_variable_pointer_test.dart` — maGLP-related tests

---

## 9. Questions for Discussion

1. **Should clause 2 suspend on imported reader?** Per GLP semantics, clause 2's head `merge(Xs, ...)` accepts any value for Xs including readers. The suspension should happen in the body when `Xs?` is actually needed.

2. **When should read requests be sent?** Per irmaGLP spec, when a goal suspends. But if clause 2 doesn't suspend (it commits successfully), when does the request get sent?

3. **Is the bytecode correct for `[Y?|Zs?]` construction?** Need to verify that UnifyVariable properly connects the extracted Y value to the output structure's first element.

---

## 10. Contact

For questions about this handover, refer to:
- irmaGLP spec: `docs/ma/irmaGLP-spec.md`
- Test design doc: `docs/ma/shared-variable-test-design.md`
- Implementation README: `docs/ma/README.md`
