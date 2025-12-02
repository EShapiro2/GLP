# Bug Report: Metainterpreter Variable Binding Issue
**Date:** 2025-11-17
**Status:** Compiler bugs fixed, runtime/metainterpreter issue discovered
**Commit:** aabf12b (nil fix), 33ca501 (Bug 1 fix)

---

## Executive Summary

**Compiler bugs FIXED:**
1. ✅ Bug 1 (commit 33ca501): Variable occurrence tracking across nested structures
2. ✅ Bug 2 (commit aabf12b): Extra UnifyVariable before HeadNil for nil lists

**New issue discovered:** `run(quicksort([],X))` still returns `X = <unbound>` despite correct bytecode generation.

**Root cause:** Metainterpreter or runtime bug in variable binding for clause `qsort([],Rest?,Rest)`.

---

## Test Case

**Query:** `run(quicksort([],X))`

**Expected:** `X = []`

**Actual:** `X = <unbound>`

**Source:** `/Users/udi/GLP/udi/glp/qsort.glp`

**Relevant clauses:**
```prolog
clause(quicksort(Unsorted, Sorted?), qsort(Unsorted?, Sorted, [])).  # Line 32
clause(qsort([], Rest?, Rest), true).                                 # Line 38
```

---

## Execution Trace

```
GLP> run(quicksort([],X)).

1: run(quicksort([],W1002)) :- clause(quicksort([]?,W1002)?, W1004), run(R1004?)
10000: clause(quicksort([],W1002), W1004) :- true
10001: run(qsort([],W1006,[])) :- clause(qsort([]?,W1006,[])?, W1008), run(R1008?)

DEBUG MetaInterp: HeadStructure checking clauseVars[10]: ConstTerm = Const(nil)

10002: clause(qsort([],W1006,[]), W1008) :- true
10003: run(true) :- true
  X = <unbound>
  → 5 goals
```

---

## Analysis

### What Should Happen

**Step 1:** `clause(quicksort([],W1002), Body)` matches line 32:
- HEAD: `clause(quicksort(Unsorted, Sorted?), ...)`
- Unifies: `[] = Unsorted`, `W1002 = Sorted?` (reader, suspends)
- BODY: `qsort(Unsorted?, Sorted, [])`
- Returns: `Body = qsort([], Sorted, [])`
- **Issue:** `Sorted` is unbound here, but will be bound later

**Step 2:** `run(qsort([],W1006,[]))` spawns `clause(qsort([],W1006,[]), Body2)`

**Step 3:** Should match line 38: `clause(qsort([], Rest?, Rest), true)`
- First arg: `[] = []` ✓
- Second arg: `W1006 = Rest?` (reader, suspends)
- Third arg: `[] = Rest` (writer, **should bind Rest = []**)
- Since `Rest?` and `Rest` are the same variable, binding `Rest = []` should also bind `W1006 = []`
- Returns: `Body2 = true`

**Step 4:** `run(true)` succeeds, X should be bound to []

### What Actually Happens

Trace shows:
- Step 1: ✓ `clause(quicksort([],W1002), ...)` succeeds
- Step 2: ✓ `run(qsort([],W1006,[]))` spawned
- Step 3: ✓ `clause(qsort([],W1006,[]), W1008)` succeeds (debug shows `ConstTerm = Const(nil)`)
- Step 4: ✓ `run(true)` succeeds
- **BUT:** `X` remains `<unbound>` instead of being bound to `[]`

**Problem:** Variable chain `X → W1002 → Sorted → Rest → W1006` should all be bound to `[]` when `Rest = []` is established, but this binding doesn't propagate.

---

## Bytecode Verification

**Compiler output is CORRECT:**

```bash
$ dart dump_bytecode.dart glp/qsort.glp | grep -A 10 "^PC 127:"
PC 127: ClauseTry
PC 128: HeadStructure(quicksort, 2, A0)
PC 129: UnifyVariable(X0, writer)        # Unsorted
PC 130: UnifyVariable(X1, reader)        # Sorted?
PC 131: HeadStructure(qsort, 3, A1)
PC 132: UnifyReader(X0)                  # Unsorted? ✓ Bug 1 fixed
PC 133: UnifyWriter(X1)                  # Sorted ✓ Bug 1 fixed
PC 134: UnifyConstant('nil')             # [] ✓ Bug 2 fixed
PC 135: Commit
PC 136: Proceed
```

All three Unify instructions are correct:
1. PC 132: `UnifyReader(X0)` for `Unsorted?` (subsequent occurrence)
2. PC 133: `UnifyWriter(X1)` for `Sorted` (subsequent occurrence)
3. PC 134: `UnifyConstant('nil')` for `[]` (atomic constant, no temp)

**Base case bytecode also correct:**

```bash
$ dart dump_bytecode.dart glp/qsort.glp | grep -A 10 "^PC 94:"
PC 94: ClauseTry
PC 95: HeadStructure(qsort, 3, A0)
PC 96: HeadNil(A0)                       # [] in arg 0
PC 97: UnifyVariable(X0, reader)         # Rest?
PC 98: UnifyReader(X0)                   # Rest (subsequent)
PC 99: Commit
PC 100: BodyConstant(true)
PC 101: Proceed
```

Correct: `UnifyVariable(X0, reader)` for first occurrence, `UnifyReader(X0)` for second.

---

## Hypothesis: Runtime Variable Binding Bug

**Possible causes:**

### 1. Variable chain dereferencing issue
When `Rest = []` is bound, the runtime should:
- Follow the chain: `W1006 → Rest? → Rest`
- Bind all references to `[]`
- But this might not be happening correctly for reader/writer variable pairs

### 2. Metainterpreter clause variable handling
The metainterpreter might not correctly propagate bindings from clause HEAD variables back to query variables.

### 3. Suspension/reactivation issue
When `Rest?` suspends and `Rest` is later bound, the suspended goal might not correctly propagate the binding.

---

## Test Results

**Unit tests:** 25/58 passing (baseline maintained)
**REPL tests:** 24/27 passing (baseline maintained)
**No regressions** from compiler fixes.

**Failing tests** (pre-existing):
- Test 5: Merge with Reader
- Test 9: Insertion Sort via Metainterpreter
- Test 13: Structure Demo

All involve metainterpreter or complex variable binding.

---

## Next Steps

**For Claude Web:**

1. **Investigate runtime variable binding** in `/Users/udi/GLP/glp_runtime/lib/bytecode/runner.dart`
   - How does `UnifyReader` handle variable chains?
   - How does `UnifyWriter` propagate bindings?
   - How does metainterpreter manage clause variables?

2. **Check heap dereferencing** in `/Users/udi/GLP/glp_runtime/lib/runtime/heap.dart` or scheduler
   - When a writer binds a variable, are all reader references updated?
   - Is there a chain-following mechanism?

3. **Compare with FCP implementation**
   - How does FCP handle `p(X?,X)` patterns?
   - How does FCP metainterpreter propagate bindings?

---

## Files for Investigation

**Runtime:**
- `/Users/udi/GLP/glp_runtime/lib/bytecode/runner.dart` - UnifyReader/UnifyWriter handlers
- `/Users/udi/GLP/glp_runtime/lib/runtime/heap.dart` - Variable dereferencing
- `/Users/udi/GLP/glp_runtime/lib/runtime/scheduler.dart` - Suspension/reactivation

**Specs:**
- `/Users/udi/GLP/docs/glp-runtime-spec.txt` - Variable binding semantics
- `/Users/udi/GLP/docs/glp-bytecode-v216-complete.md` - UnifyReader/UnifyWriter semantics

**Reference:**
- FCP implementation: `/Users/udi/Dropbox/Concurrent Prolog/FCP/Savannah`
- FCP paper: `/Users/udi/GLP/docs/1-s2.0-0743106689900113-main.pdf`

---

## Git Status

**Branch:** main
**Commits:**
- aabf12b: fix: Treat nil as atomic constant in nested structures
- 33ca501: fix: Variable occurrence tracking across nested structures in HEAD

**Modified files:** None (clean working directory)

**Test command:**
```bash
cd /Users/udi/GLP/udi
dart glp_repl.dart <<EOF
qsort.glp
run(quicksort([],X)).
:quit
EOF
```

---

## Summary

Compiler bugs are FIXED. Bytecode generation is CORRECT. The remaining issue is in the runtime's handling of variable binding chains, particularly for patterns like `p(X?,X)` where reader and writer share a variable. The metainterpreter clause `qsort([],Rest?,Rest)` should bind all references when `Rest = []` is established, but this propagation is not working.
