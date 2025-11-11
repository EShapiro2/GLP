# Compiler Bug Report - Complex Structure Arguments

**Date**: 2025-11-09
**Status**: Root Cause Identified - Compiler Code Generation Bug
**Priority**: Critical - Blocks metainterpreter and complex argument passing

---

## Executive Summary

The metainterpreter bug has been isolated to a **compiler code generation bug** when passing complex structures (non-empty lists, nested structures) as arguments to spawned goals.

**Smoking Gun Test**:
- ✅ **WORKS**: `run4(Ys?) :- run(merge([],[],Ys)).` - empty list
- ❌ **FAILS**: `run3(Ys?) :- run(merge([],[a],Ys)).` - non-empty list `[a]`

The ONLY difference is `[]` vs `[a]` in the argument to `run(...)`.

---

## Minimal Reproduction

### Working Case

```prolog
run4(Ys?) :- run(merge([],[],Ys)).
```

**Execution**:
```
1: run4/1(W1000) :- run/1(merge([],[],W1000)?)
10000: run/1(merge([],[],W1000)?) :- clause/2(...), run/1(R1005?)
10001: clause/2(...) :- true
10002: run/1(true?) :- true
  X = []
```

✅ **SUCCESS**

### Failing Case

```prolog
run3(Ys?) :- run(merge([],[a],Ys)).
```

**Execution**:
```
1: run3/1(W1000) :- run/1(R1003?)
10000: run/1(R1003?) → suspended
  X = <unbound>
```

❌ **SUSPENSION** - `run/1(R1003?)` suspends immediately!

---

## Bytecode Comparison

### run4 (WORKS):
```
19: PutStructure('merge', 3, slot=0)
20: UnifyConstant([])              # First arg: []
21: UnifyConstant([])              # Second arg: []
22: unify_writer(0)                # Third arg: Ys (writer 0)
23: Spawn('run/1', 1)
24: Proceed
```

**Argument A0**: Structure `merge([],[],W0)` correctly built in slot 0

### run3 (FAILS):
```
4: PutStructure('merge', 3, slot=0)
5: UnifyConstant([])               # First arg: []
6: PutStructure('.', 2, ???)       # ← BUG: builds [a] but WHERE?
7: UnifyConstant(a)
8: UnifyConstant([])
9: unify_writer(11)                # ← BUG: uses writer 11 (inside structure!)
10: unify_writer(0)                # Third arg: Ys
11: Spawn('run/1', 1)
12: Proceed
```

**Problem**:
- Line 6: `PutStructure('.',2)` - builds list `[a]` but doesn't specify target slot!
- Line 9: `unify_writer(11)` - references writer 11 which is never set up as an argument
- The spawned `run/1` goal receives a **reader** (R1003) instead of the structure!

---

## Root Cause

**File**: `/Users/udi/GLP/glp_runtime/lib/compiler/codegen.dart:524-532`

```dart
} else if (term is ListTerm) {
  if (term.isNil) {
    ctx.emit(bc.PutNil(argSlot));
  } else {
    // Build list structure as '.'(H, T)
    ctx.emit(bc.PutStructure('.', 2, argSlot));      // ← Line 529
    if (term.head != null) _generateStructureElement(term.head!, varTable, ctx, inHead: false);
    if (term.tail != null) _generateStructureElement(term.tail!, varTable, ctx, inHead: false);
  }
```

**The bug**:
1. `PutStructure('.', 2, argSlot)` starts building a list in argument slot
2. `_generateStructureElement` is called to fill in the list elements
3. `_generateStructureElement` uses `unify_writer/unify_reader` instructions
4. These instructions operate on the **S register** (structure traversal), not argument registers!
5. The structure is partially built in the wrong location
6. Writers/readers inside the structure are not properly connected to argument registers

**Result**: The argument slot doesn't contain the complete structure, it contains a reader to an incomplete/incorrectly built structure.

---

## What Should Happen

When building a complex structure as an argument for a spawned goal:

1. **Allocate a temporary register** for the complete structure
2. **Build the entire structure** in that temporary register
3. **Move/copy the structure** to the argument slot
4. **OR**: Build structure inline but ensure all writers/readers are properly registered

The current code mixes structure building (which uses S register traversal) with argument setup (which uses A register slots).

---

## Evidence Trail

### Test Progression

1. ✅ **test3**: `fetch(a, Body), execute(Body?)` - WORKS (fixed by removing Requeue)
2. ✅ **test_middle**: `exec(go) :- next_step(go, Step), exec(Step?)` - WORKS (simple recursion)
3. ✅ **run(p(a))**: WORKS - metainterpreter with fact (clause body is `true`)
4. ❌ **run(step1)**: FAILS - metainterpreter with rule (clause body is `step2`)
5. ✅ **run4**: `run(merge([],[],Ys))` - WORKS (empty list argument)
6. ❌ **run3**: `run(merge([],[a],Ys))` - FAILS (non-empty list argument)

### Key Insight

The pattern that works: **Simple terms, constants, variables, empty lists**
The pattern that fails: **Non-empty lists, nested structures** as arguments to spawned goals

---

## Affected Code

### Main Issue

**File**: `glp_runtime/lib/compiler/codegen.dart`
**Function**: `_generatePutArgument` (lines 508-546)
**Specific**: ListTerm handling (lines 524-532)
**Also**: StructTerm handling (lines 534-539) - likely has same bug

### Helper Function

**Function**: `_generateStructureElement` (called at lines 530-531, 538)
- This function generates `unify_*` instructions
- These operate on S register during structure traversal
- This is correct for HEAD phase unification
- This is WRONG for BODY phase argument setup

---

## Fix Direction

### Option 1: Two-Phase Structure Building

For BODY arguments, don't build structures inline:

1. Build the complete structure first (allocate temp, build, bind)
2. Then use `PutReader/PutWriter` to pass it as an argument

### Option 2: Inline with Proper Registration

Keep inline building but ensure:
1. PutStructure correctly sets up the argument slot
2. All writers/readers inside the structure are registered with the spawned goal's environment
3. Unify instructions properly populate the argument structure

### Option 3: Use PutConstant for Ground Terms

For fully ground structures like `[a]`:
1. Recognize they're ground at compile time
2. Use `PutConstant` to pass the entire structure as a constant
3. Avoid structure building instructions entirely

---

## Test Cases for Fix

After fix, these should all work:

```prolog
% Currently working
run4(Ys?) :- run(merge([],[],Ys)).

% Currently failing - should work after fix
run3(Ys?) :- run(merge([],[a],Ys)).

% Complex structures
test_struct(X) :- helper(f(a,b,c)), process(X?).

% Nested lists
test_nested(X) :- helper([[a],[b]]), process(X?).

% Metainterpreter
run(step1) where step1 :- step2.
run((merge(Xs?,[a],Ys),merge(Ys?,[b],Xs))).
```

---

## Related Files

### Test Files
- `/Users/udi/GLP/udi/glp/run1.glp` - Contains run3 and run4
- `/Users/udi/GLP/udi/glp/test_meta_simple.glp` - Minimal metainterpreter tests
- `/Users/udi/GLP/udi/glp/test_working.glp` - Working test cases

### Compiler Files
- `/Users/udi/GLP/glp_runtime/lib/compiler/codegen.dart` - Code generation (BUG HERE)
- `/Users/udi/GLP/glp_runtime/lib/compiler/ast.dart` - AST definitions

### Debug Scripts
- `/Users/udi/GLP/udi/debug_run3_run4.dart` - Bytecode comparison

---

## Success Criteria

Fix is successful when:

```
GLP> run3(X)
1: run3/1(W1000) :- run/1(merge([],[a],W1002)?)
10000: run/1(merge([],[a],W1002)?) :- clause/2(...), run/1(R1005?)
10001: clause/2(...) :- true
10002: run/1(...) :- ...
  X = [a]   ← NOT <unbound>
```

And metainterpreter works:

```
GLP> run((merge(Xs?,[a],Ys),merge(Ys?,[b],Xs)))
  Ys = [a, b, a, b, ...]
  Xs = [b, a, b, a, ...]
  → many goals (not 13)
```

---

**Report Status**: Root cause identified, ready for fix
**Complexity**: Moderate - requires understanding WAM structure building vs argument passing
**Recommendation**: Claude Web should review `_generatePutArgument` and `_generateStructureElement` interaction

**Alternative**: If fix is complex, consider implementing Option 3 (PutConstant for ground terms) as a quick workaround, then fix general case later.

---

**Report Generated**: 2025-11-09
**Investigation**: Complete bytecode analysis and test isolation
**Next**: Code generation fix in `_generatePutArgument`

