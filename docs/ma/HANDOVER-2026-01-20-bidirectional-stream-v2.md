# Handover: Bidirectional Stream Test Debugging (Session 2)

**Date:** 2026-01-20
**Status:** In Progress
**Branch:** `claude/setup-bidirectional-stream-WzM2n`
**Test File:** `glp_runtime/test/multiagent/bidirectional_stream_test.dart`

---

## 1. Summary

This session continued debugging the bidirectional stream test for irmaGLP. The previous session's `writerForReader` deprecation work was completed and merged to main. The test still fails with incorrect output values.

---

## 2. Completed Work This Session

### 2.1 Environment Setup
- Dart SDK 3.10.1 confirmed working
- Branch is up to date with main

### 2.2 Test Execution
- Ran bidirectional stream test
- Confirmed the failure mode matches previous handover

---

## 3. Current Failure Mode

### 3.1 Observed Behavior
```
@1: Spawned goal merge(Xs?, [a], Ys) at PC=0
@2: Spawned goal merge(Ys?, [b], Xs) at PC=0

--- Iteration 1 (elements: 0) ---

@1 running...
[DEBUG IRMA isolate1] _onWriterBound: writerId=0, value=.(Var@8,Var@10)
[DEBUG IRMA isolate1] _onWriterBound: NO ACTION (role=VariableRole.createdWriter, state=null)
@1: status=ExecutionStatus.succeeded, GQ=0

@2 running...
[DEBUG IRMA isolate2] _onWriterBound: writerId=0, value=.(Var@8,Var@10)
[DEBUG IRMA isolate2] _onWriterBound: NO ACTION (role=VariableRole.createdWriter, state=null)
@2: status=ExecutionStatus.succeeded, GQ=0

Both isolates completed

Ys = [_8, _9]
Xs = [_8, _9]
```

### 3.2 Key Observations

1. **Both isolates succeed immediately** with `ExecutionStatus.succeeded`
2. **Zero messages sent** between isolates (`Elements produced: 0`)
3. **Incorrect output values**: `[_8, _9]` instead of expected `[a | Zs?]`
4. **`state=null`** in writer binding callback means no requester recorded

---

## 4. Root Cause Analysis

### 4.1 User Clarification
The user stated: "the code is correct, compiler is correct, single isolate behavior is correct. the only problem is with an imported reader."

This means:
- The general bytecode execution is correct
- The issue is specifically in how **imported readers** are handled in maGLP

### 4.2 Expected Behavior (per irmaGLP spec)

For clause 2 `merge(Xs, [Y|Ys], [Y?|Zs?])`:
1. First arg `Xs` (writer) receives the imported reader VarRef
2. Second arg `[Y|Ys]` matches `[a]`, extracting Y='a'
3. Third arg `[Y?|Zs?]` writes `[a | Zs?]` to goal's Ys writer

The output should be `[a | Zs?]` but we see `[_8, _9]` (fresh variables).

### 4.3 Hypothesis

The wrong output (`Var@8, Var@10` instead of `ConstTerm('a'), VarRef(reader)`) suggests:
- Either clauseVars[1] doesn't contain `ConstTerm('a')` when `unify_reader(1)` runs
- Or the tentative structure conversion at Commit is creating fresh variables incorrectly

---

## 5. Investigation Notes

### 5.1 Bytecode for Clause 2 (merge)
```
17: get_writer_variable(X0, A0)   // Xs = arg 0 (imported reader)
18: HeadStructure('.', 2, A1)     // [Y|Ys] matches arg 1 ([a])
19: unify_writer(1)               // Y - extract head
20: unify_writer(2)               // Ys - extract tail
21: HeadStructure('.', 2, A2)     // [Y?|Zs?] writes to arg 2
22: unify_reader(1)               // Y? - should use clauseVars[1]
23: unify_reader(3)               // Zs? - creates _ClauseVar(3)
24: Commit
25: put_reader(X0, A0)            // Body call arg 0 = Xs?
26: put_reader(X2, A1)            // Body call arg 1 = Ys?
27: put_writer(X3, A2)            // Body call arg 2 = Zs
28: Spawn
29: Proceed
```

### 5.2 Key Code Paths

**HeadVariable in WRITE mode** (lines 605-629 of runner.dart):
- When `unify_reader(1)` runs in WRITE mode, it should:
  1. Get `existingValue = clauseVars[1]` (should be `ConstTerm('a')`)
  2. Store it in `struct.args[cx.S]`

**Commit tentative→StructTerm conversion** (lines 2243-2314):
- For each arg in tentative struct:
  - If arg is Term → use as-is (expected for Y)
  - If arg is _ClauseVar → resolve or create fresh variable

### 5.3 Files Examined

| File | Lines | Purpose |
|------|-------|---------|
| `runner.dart` | 274-295 | `_finalUnboundVar` - handles imported reader deref |
| `runner.dart` | 605-665 | `HeadVariable` - unify_writer/unify_reader |
| `runner.dart` | 780-1080 | `HeadStructure` - structure matching/writing |
| `runner.dart` | 2211-2360 | `Commit` - sigmaHat application |
| `heap_fcp.dart` | 154-182 | `derefAddr` - returns VariableEntry for imported |

---

## 6. What Was NOT Done

- Full trace with debug output enabled
- Step-by-step verification of clauseVars state
- Adding instrumentation to pinpoint exact divergence point

---

## 7. Recommended Next Steps

### 7.1 Immediate Actions

1. **Add trace output** to unify_reader in WRITE mode:
   - Print `clauseVars[varIndex]` value
   - Print `struct.args` after assignment
   - Verify ConstTerm('a') is actually present

2. **Add trace output** to Commit:
   - Print tentative structure args before conversion
   - Print converted StructTerm args after conversion

### 7.2 Hypothesis to Verify

The wrong output suggests one of:
1. clauseVars[1] is NOT `ConstTerm('a')` at PC 22
2. HeadVariable WRITE mode is not using clauseVars correctly
3. Commit conversion is replacing ConstTerm with VarRef somehow

### 7.3 Single-Isolate Baseline

Run `merge([1,2,3], [a,b,c], Xs)` in REPL to verify:
- Single isolate produces correct output
- clauseVars flow is correct when no imported readers involved

---

## 8. Test Status

As of session end:
- **REPL tests:** 222/223 pass (1 timing failure)
- **Unit tests:** 363/376 pass (13 pre-existing failures)
- **Bidirectional stream:** FAILING

---

## 9. Files Changed This Session

None - investigation only.

---

## 10. References

- Previous handover: `docs/ma/HANDOVER-2026-01-20-bidirectional-stream.md`
- irmaGLP spec: `docs/ma/irmaGLP-spec.md` (Section 5.2 - Reduce Transaction)
- Test file: `glp_runtime/test/multiagent/bidirectional_stream_test.dart`
- Bytecode dump: `glp_runtime/bin/test_merge.dart`
