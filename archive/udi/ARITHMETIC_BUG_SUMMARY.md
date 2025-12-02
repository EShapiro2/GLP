# Arithmetic Bug Summary for Claude Web

**Date**: 2025-11-12
**Context**: Continuation of arithmetic implementation work
**Current Issue**: Execute instruction receives unbound VarRefs instead of actual values

---

## Quick Context

We're implementing arithmetic expressions in GLP. The parser, lexer, and codegen fixes are complete. The issue is at runtime in the Execute instruction.

---

## The Bug

When executing:
```prolog
add(X, Y, Z?) :- execute('evaluate', [X? + Y?, Z]).
```

With query: `add(5, 3, Result).`

**Expected**: evaluate/2 receives `+(5, 3)` and computes 8
**Actual**: evaluate/2 receives `+(R0?, R1?)` with R0 and R1 **unbound**

---

## Debug Evidence

```
Query: add(5, 3, X).
Trace shows: add/3(5?, 3?, W1002)  ← Goal has bound args

[EXECUTE] ClauseVars: {0: +(R0?,R1?), 1: W2, 2: 1002}
[DEREF] VarRef: R0? (id=0, isReader=true)
[DEREF] isWriterBound(0) = false  ← UNBOUND!
[DEREF] VarRef: R1? (id=1, isReader=true)
[DEREF] isWriterBound(1) = false  ← UNBOUND!

Result: X = <unbound>
```

---

## Root Cause

Clause variables contain **VarRef objects** instead of actual values:
- `ClauseVars = {0: +(R0?,R1?), 1: W2, 2: 1002}`

Should contain:
- `ClauseVars = {0: +(5, 3), 1: W2, 2: 1002}`

The HEAD phase unified the goal `add(5?, 3?, W1002)` with clause `add(X, Y, Z?)` but:
- Writers X (id=0) and Y (id=1) were **never bound** to 5 and 3
- `isWriterBound(0) = false` and `isWriterBound(1) = false`

---

## FCP Validation

Checked FCP authoritative implementation (`emulate.c` lines 2430-2437):

```c
case plusnum_reg_reg:
    Va = pc_reg();
    Vb = pc_reg();
    deref(Va, Pa);     // ← Explicit dereference
    deref(Vb, Pb);     // ← Before arithmetic
    if (IsInt(Va)) {
      if (IsInt(Vb)) {
        *Ref_Val(pc_reg()) = ((heapT) ((int) Va + (int) Off_Tag(Vb)));
```

**FCP always dereferences before operations** - validates our approach.

---

## Two Possible Root Causes

### Hypothesis A: HEAD Phase Doesn't Bind Writers

The HEAD phase unification might not be binding clause writers to goal argument values. Sigma-hat (σ̂w) might not contain bindings for writers 0 and 1.

**Check needed**: Does `cx.sigmaHat` contain `{0: 5, 1: 3}` during execute?

### Hypothesis B: Clause Variables Not Updated from Sigma-Hat

After COMMIT, sigma-hat bindings should be reflected in clause variables so body references can read them. The COMMIT might apply bindings to heap but not update clause variables.

**Check needed**: After COMMIT, should clause variables be updated?

---

## Work Done So Far

### ✅ Completed
1. **Documentation**: Guards vs execute, SRSW relaxation (5 files updated)
2. **Codegen fix**: Line 485 - support readers in execute arguments
3. **Parser/Lexer**: Infix arithmetic expressions work
4. **FCP Investigation**: Validated dereferencing approach
5. **Dereference implementation**: Added `_dereferenceForExecute()` helper

### ❌ Issue
6. **Execute instruction**: Dereference function works but variables are never bound

---

## The Question

**Which is the correct fix?**

**Option A**: Make Execute dereference from sigma-hat first:
```dart
// In _dereferenceForExecute()
if (term is VarRef) {
  final varId = term.varId;
  // Check sigma-hat first
  if (cx.sigmaHat.containsKey(varId)) {
    return _dereferenceForExecute(cx.sigmaHat[varId], cx);
  }
  // Then check heap
  if (cx.rt.heap.isWriterBound(varId)) {
    return _dereferenceForExecute(cx.rt.heap.getValue(varId), cx);
  }
  return term; // Unbound
}
```

**Option B**: Make COMMIT update clause variables:
```dart
// In COMMIT instruction
for (final entry in cx.sigmaHat.entries) {
  cx.rt.heap.bindWriter(entry.key, entry.value);
  // Also update clause variables?
  if (cx.clauseVars.containsKey(entry.key)) {
    cx.clauseVars[entry.key] = entry.value;
  }
}
```

**Option C**: Fix codegen - body variable references shouldn't create VarRefs in clause variables, they should reference actual values from unification.

---

## Key Files

1. **runner.dart** line 2196-2240: Execute instruction (with debug logging)
2. **runner.dart** line 2574-2613: `_dereferenceForExecute()` helper (with debug logging)
3. **system_predicates_impl.dart** line 73-82: evaluate/2 (with debug logging)
4. **codegen.dart** line 485: Reader support fix

---

## Next Step

Need to determine:
1. Does `cx.sigmaHat` contain bindings `{0: 5, 1: 3}` during execute?
2. If yes → Fix A (dereference from sigma-hat)
3. If no → Need to understand why HEAD phase didn't bind writers

---

## Test Case

Simple reproduction:
```prolog
% File: simple_test.glp
test_simple(X, Y, Z?) :- execute('write', [X?]), execute('write', [Y?]).
```

Query: `test_simple(5, 3, Result).`

Shows: `ClauseVars = {0: R0?, 1: 1001, 2: 1002}` with `isWriterBound(0) = false`

Expected: Should print "5" and "3", actually prints nothing (unbound).

---

**Status**: Waiting for architectural decision on correct fix approach.
