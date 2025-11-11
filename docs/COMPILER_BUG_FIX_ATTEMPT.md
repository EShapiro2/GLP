# Compiler Bug Fix Attempt Report

**Date**: 2025-11-09
**Status**: Partial Progress - Ground Term Optimization Implemented, Still Failing
**Context**: Continuation of COMPILER_BUG_REPORT.md

---

## Summary

Attempted to fix the compiler bug where complex structures (non-empty lists) as arguments to spawned goals cause suspension. Made progress identifying the root cause and implemented a partial fix (ground term optimization), but the core issue remains.

---

## Root Cause Analysis

### The Core Problem

`PutStructure` instruction is designed to work with **argument slots (A0-A9)**, not arbitrary temp registers. When building nested structures, the compiler tries to use temp registers, but:

1. `PutStructure(functor, arity, tempReg)` executes and does:
   ```dart
   // Creates writer/reader pair
   final (freshWriterId, freshReaderId) = cx.rt.heap.allocateFreshPair();

   // Stores READER in argReaders[op.argSlot]
   cx.argReaders[op.argSlot] = freshReaderId;  // ← tempReg used as argSlot!
   ```

2. When `tempReg` is passed as `argSlot`, it puts the reader in `argReaders[tempReg]`
3. But temp registers (10, 11, etc.) are NOT argument slots (0-9)!
4. The structure gets built but isn't properly connected to the spawned goal's environment

### Key Insight from User

**"in the body we are post commit, so building is permanent into the heap not temporary"**

This is critical - BODY phase builds structures permanently in the heap. The issue isn't about temporary vs permanent, it's about how we reference these heap structures when passing them as arguments.

---

## Fix Attempt 1: Separate Helper Function

**Approach**: Created `_generateArgumentStructureElement` separate from `_generateStructureElement`

**Rationale**: `_generateStructureElement` is designed for HEAD/GUARD phase unification (S register traversal), not for BODY phase argument construction.

**Result**: ❌ No effect - bytecode unchanged

**Why it failed**: The new function still used the same approach of building nested structures in temp registers and using `UnifyVariable` to add them.

---

## Fix Attempt 2: Ground Term Optimization

**Approach**: Detect fully ground terms (no variables) and emit them as constants instead of building structures.

**Implementation**:

```dart
// In _generateArgumentStructureElement:
if (_isGroundTerm(term)) {
  // Ground list - emit as constant
  final listValue = _groundTermToValue(term);
  ctx.emit(bc.UnifyConstant(listValue));
} else {
  // Non-ground - build structure (old way)
  ...
}
```

**Helper functions added**:
- `_isGroundTerm(Term)` - checks if term contains no variables
- `_groundTermToValue(Term)` - converts ground term to Dart value

**Bytecode Result**: ✅ **Success** - bytecode simplified!

### Before:
```
4: PutStructure('merge', 3, slot=0)
5: UnifyConstant([])
6: PutStructure('.', 2, tempReg)     ← Extra structure building
7: UnifyConstant(a)
8: UnifyConstant([])
9: unify_writer(10)                  ← Wrong writer
10: unify_writer(0)
```

### After:
```
4: PutStructure('merge', 3, slot=0)
5: UnifyConstant([])
6: UnifyConstant([a, null])          ← Single constant!
7: unify_writer(0)
```

**Runtime Result**: ❌ **Still fails** - but for a different reason

### New Problem

The list `[a]` is being represented as Dart list `[a, null]` (cons cell: head=a, tail=null), but the runtime doesn't interpret this correctly as a GLP list structure.

**Trace shows**:
```
1: run3/1(W1000) :- run/1(merge([],[a, null],W1000)?)
10000: run/1(merge([],[a, null],W1000)?) :- clause/2(merge([],[a, null],W1000)?, W1004), run/1(R1005?)
10002: run/1(R1005?) → suspended
```

The structure is being passed, but `clause/2` can't match it properly or the body returned is wrong.

---

## Current State

### What Works
- ✅ Empty lists `[]`: `run4(Ys?) :- run(merge([],[],Ys)).`
- ✅ Simple variables: `test3 :- fetch(a, Body), execute(Body?).`
- ✅ Facts through metainterpreter: `run(p(a))` where `p(a).`
- ✅ Bytecode generation for ground terms simplified

### What Still Fails
- ❌ Non-empty lists: `run3(Ys?) :- run(merge([],[a],Ys)).`
- ❌ Nested structures as arguments
- ❌ Metainterpreter with rules: `run(step1)` where `step1 :- step2.`
- ❌ Circular merge through metainterpreter

---

## The Real Issue

There are actually TWO problems:

### Problem 1: Non-Ground Nested Structures (Not Fixed)

When a nested structure contains variables, it can't be emitted as a constant. Example:
```prolog
helper(f(X, a))  % X is a variable, structure is not ground
```

The compiler still tries to build this with nested `PutStructure` in a temp register, which doesn't work correctly.

### Problem 2: Constant Representation (New)

Even when we emit ground structures as constants, the Dart representation doesn't match what the runtime expects.

**Current**: `[a, null]` (Dart list with 2 elements)
**Expected**: Proper GLP list structure that `clause/2` can match

---

## Possible Solutions

### Option A: Fix Constant Representation

Make `_groundTermToValue` return values in a format the runtime understands:
- Use a special wrapper object for lists
- Or use the actual StructTerm/ConstTerm objects
- Or match the format that `clause/2` database uses

### Option B: Fix Nested Structure Building

Abandon the constant approach and fix the real problem:
- Don't use `PutStructure` with temp registers
- Instead, build structures differently in BODY phase
- Possibly use a new instruction or different approach

### Option C: Hybrid Approach

- Use constants for ground terms (fix the representation)
- Create a proper solution for non-ground nested structures
- This would handle both cases correctly

---

## Recommended Next Steps

1. **Investigate runtime structure representation**
   - How does `clause/2` represent clause bodies?
   - What format does `UnifyConstant` expect for complex terms?
   - Should we use StructTerm objects instead of Dart values?

2. **Test ground term constant with correct format**
   - Try different representations for the list
   - See what `clause/2` actually receives

3. **If constants won't work, investigate alternative instruction**
   - Is there a way to build structures without `PutStructure`?
   - Can we bind structures to temp variables differently?
   - Should we use `PutWriter` + heap binding instead?

---

## Code Changes Made

### File: `/Users/udi/GLP/glp_runtime/lib/compiler/codegen.dart`

**Added functions**:
- `_generateArgumentStructureElement` (lines 583-628) - Separate helper for argument structures
- `_isGroundTerm` (lines 548-561) - Check if term is fully ground
- `_groundTermToValue` (lines 563-581) - Convert ground term to Dart value

**Modified function**:
- `_generatePutArgument` (lines 508-546) - Now calls `_generateArgumentStructureElement`

**Key change in `_generateArgumentStructureElement`** (lines 603-608):
```dart
if (_isGroundTerm(term)) {
  // Ground list - emit as constant
  final listValue = _groundTermToValue(term);
  ctx.emit(bc.UnifyConstant(listValue));
} else {
  // Non-ground nested list - build in temp, then unify
  ...
}
```

---

## Test Cases

### Minimal failing case:
```prolog
run3(Ys?) :- run(merge([],[a],Ys)).
```

### Minimal working case:
```prolog
run4(Ys?) :- run(merge([],[],Ys)).
```

### Difference:
- `[a]` vs `[]`
- One is ground non-empty list, other is empty list
- Empty list uses `UnifyConstant(null)` which works
- Non-empty list now uses `UnifyConstant([a, null])` which doesn't work correctly

---

## Questions for Investigation

1. **How should lists be represented in UnifyConstant?**
   - Current: `[a, null]` (Dart list)
   - Should it be: StructTerm? Map? Special object?

2. **Can we use StructTerm directly?**
   ```dart
   ctx.emit(bc.UnifyConstant(StructTerm('.', [ConstTerm('a'), ConstTerm(null)])));
   ```

3. **What does clause/2 return as clause body?**
   - Is it a StructTerm?
   - Is it bound as a constant or as a structure?

4. **Should we abandon PutStructure for nested structures?**
   - Use different approach entirely?
   - Build in heap manually?

---

**Status**: Partial fix implemented (ground term optimization), but representation issue discovered
**Recommendation**: Investigate runtime structure representation before proceeding
**Alternative**: May need to abandon constant approach and fix nested PutStructure properly

---

**Report Generated**: 2025-11-09
**Files Modified**: `glp_runtime/lib/compiler/codegen.dart`
**Test Status**: Bytecode improved, runtime still fails

