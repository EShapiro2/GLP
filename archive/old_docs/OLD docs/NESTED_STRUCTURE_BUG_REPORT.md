# Nested Structure Compilation Bug Report

**Date**: 2025-11-09
**Status**: Bug Identified, Fix Needed
**Context**: Continuation of COMPILER_BUG_FIX_ATTEMPT.md

---

## Summary

The compiler incorrectly generates bytecode for nested structures (lists and structures) when they appear as arguments within other structures. This causes two different failure modes:
1. Lists are "flattened" - elements emitted directly instead of building list structure
2. Nested structures use temp registers incorrectly with `PutStructure`

---

## Test Cases

### Working Case
```prolog
run4(Ys?) :- run(merge([],[],Ys)).
```
**Result**: ✅ Works perfectly
```
1: run4/1(W1000) :- run/1(merge([],[],W1000)?)
10000: run/1(merge([],[],W1000)?) :- clause/2(merge([],[],W1000)?, W1004), run/1(R1005?)
10001: clause/2(merge([],[],W1000)?, W1004) :- true
10002: run/1(true?) :- true
  X = []
```

### Failing Case 1: Non-empty List
```prolog
run6(Ys?) :- run(merge([a],[],Ys)).
```
**Result**: ❌ No reductions at all
```
GLP> run6(X)
  X = <unbound>
```

### Failing Case 2: Nested Structure
```prolog
run7(Ys?) :- run(merge(f(a),[],Ys)).
```
**Result**: ❌ No reductions at all
```
GLP> run7(X)
  X = <unbound>
```

### Additional Failing Cases
```prolog
run8(Ys?) :- run(merge(a,[],Ys)).
```
**Result**: ❌ Incorrect suspension
```
1: run8/1(W1000) :- run/1(merge(a,[],W1000)?)
10000: run/1(merge(a,[],W1000)?) :- clause/2(merge(a,[],W1000)?, W1004), run/1(R1005?)
10002: run/1(R1005?) → suspended
  X = <unbound>
```

---

## Bytecode Analysis

### run4 (WORKS) - `merge([],[],Ys)`
```
4: PutStructure(merge, 3, slot=0)   ← Start merge structure
5: UnifyConstant(null)              ← First arg: []
6: UnifyConstant(null)              ← Second arg: []
7: unify_writer(0)                  ← Third arg: Ys
8: Spawn
9: Proceed
```
**Why it works**: Both `[]` are atomic (null), emitted as `UnifyConstant`

### run6 (FAILS) - `merge([a],[],Ys)`
```
16: PutStructure(merge, 3, slot=0)  ← Start merge structure
17: UnifyConstant(a)                ← BUG: List flattened! Should be list structure
18: UnifyConstant(null)             ← BUG: List tail emitted directly
19: UnifyConstant(null)             ← Third arg: []
20: unify_writer(0)                 ← Fourth arg: Ys
21: Spawn
22: Proceed
```
**Bug**: The list `[a]` (which is `.`(a, null)) is being **flattened** - its head and tail are emitted as separate `UnifyConstant` instructions instead of building a list structure. This makes `merge` have 4 arguments instead of 3!

### run7 (FAILS) - `merge(f(a),[],Ys)`
```
29: PutStructure(merge, 3, slot=0)   ← Start merge structure
30: PutStructure(f, 1, slot=10)      ← BUG: Using temp register 10!
31: UnifyConstant(a)                 ← Add 'a' to f structure
32: unify_writer(10)                 ← BUG: Try to add from slot 10
33: UnifyConstant(null)              ← Second arg: []
34: unify_writer(0)                  ← Third arg: Ys
35: Spawn
36: Proceed
```
**Bug**: The nested structure `f(a)` uses `PutStructure(f, 1, slot=10)` where 10 is a temp register, not an argument slot. The runtime's `PutStructure` handler stores the reader in `argReaders[10]`, which is wrong.

---

## Root Cause

The compiler function `_generateArgumentStructureElement` in `codegen.dart` is responsible for emitting elements that go INSIDE a structure being built.

### Current Buggy Code (lines 599-628)

```dart
} else if (term is ListTerm) {
  if (term.isNil) {
    ctx.emit(bc.UnifyConstant(null));  // Empty list
  } else {
    // Nested list structure - recursively just emit the elements
    // The list structure itself was already created by parent PutStructure
    if (term.head != null) {
      _generateArgumentStructureElement(term.head!, varTable, ctx);
    } else {
      ctx.emit(bc.UnifyConstant(null));
    }
    if (term.tail != null) {
      _generateArgumentStructureElement(term.tail!, varTable, ctx);
    } else {
      ctx.emit(bc.UnifyConstant(null));
    }
  }

} else if (term is StructTerm) {
  // Nested structure - build in temp, then unify
  final tempReg = ctx.allocateTemp();
  ctx.emit(bc.PutStructure(term.functor, term.arity, tempReg));
  for (final arg in term.args) {
    _generateArgumentStructureElement(arg, varTable, ctx);
  }
  ctx.emit(bcv2.UnifyVariable(tempReg, isReader: false));
```

### Problem 1: List Flattening
Lines 605-614 emit the list's head and tail as separate elements, instead of building the list structure first. The comment "The list structure itself was already created by parent PutStructure" is **incorrect** - the parent created a `merge` structure, not a list structure!

### Problem 2: PutStructure with Temp Registers
Line 620: `ctx.emit(bc.PutStructure(term.functor, term.arity, tempReg));` uses a temp register (e.g., 10).

The runtime's `PutStructure` handler (runner.dart:1527) does:
```dart
cx.argReaders[op.argSlot] = freshReaderId;
```

When `op.argSlot = 10` (temp register), this stores the reader in `argReaders[10]`, which is NOT an argument slot (only 0-9 are valid argument slots).

---

## Why No Reductions?

The key observation: **run6 and run7 produce NO reductions at all**, not even goal 1.

This means the bytecode execution **fails silently during the BODY phase** before it can spawn the `run/1` goal. The most likely causes:

1. **Structure arity mismatch**: run6 creates a 4-argument merge instead of 3-argument
2. **Missing reader**: run7's `unify_writer(10)` tries to access `argReaders[10]` which doesn't exist
3. **Silent exception**: The runtime may be catching an exception and failing the goal silently

---

## What We Know

1. **Empty lists work**: `[]` emitted as `UnifyConstant(null)` is correct
2. **Top-level structures work**: `PutStructure` at the top level (argument slots 0-9) works fine
3. **Nested structures fail**: Any non-empty list or structure AS AN ARGUMENT to another structure fails
4. **Post-commit guarantee**: Since this is BODY phase (post-commit), the code SHOULD execute and produce reductions even if buggy

---

## Questions for Resolution

1. **How should nested structures be built?**
   - Should we build them BEFORE the parent `PutStructure`?
   - Do we need a `SetStructure` or similar instruction?
   - Should we use a different register mechanism?

2. **What is the correct bytecode sequence for `merge([a],[],Ys)`?**
   - Option A: Build list first in a temp, then reference it?
   - Option B: Use nested `PutStructure` with different semantics?
   - Option C: Use a completely different approach?

3. **Why the silent failure?**
   - Should we add error logging to the runtime to catch this?
   - Is there exception handling that's swallowing the error?

4. **WAM Precedent?**
   - How does the Warren Abstract Machine handle nested structures in BODY phase?
   - Are there `set_value` or `set_local_value` instructions we should use?

---

## Attempted Fixes (All Failed)

### Attempt 1: Separate Helper Function
Created `_generateArgumentStructureElement` separate from `_generateStructureElement` - no effect.

### Attempt 2: Ground Term Optimization
Detected ground terms and emitted as constants - bytecode improved but runtime representation mismatch.

### Attempt 3: Remove List PutStructure
Removed nested `PutStructure` for lists, thinking parent already created structure - caused list flattening bug.

---

## Recommended Next Steps

1. **Study WAM structure creation** - Read wam.pdf sections on `put_structure`, `set_value`, `set_local_value`
2. **Check runtime PutStructure semantics** - Verify what happens when arity doesn't match
3. **Add debug logging** - Add prints to see exactly where run6/run7 fail
4. **Design correct instruction sequence** - Determine proper bytecode for nested structures
5. **Implement and test** - Fix compiler, verify run6 and run7 work

---

**Files Modified**:
- `glp_runtime/lib/compiler/codegen.dart` (lines 583-628)

**Test Files Created**:
- `/Users/udi/GLP/udi/debug_run6_run7.dart`

**Status**: Awaiting design decision on correct bytecode structure for nested terms

---

**Report Generated**: 2025-11-09
