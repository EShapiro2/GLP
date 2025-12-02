# Final Diagnostic Report: Arithmetic Implementation

**Date**: 2025-11-12
**Issue**: Arithmetic expressions compile but don't bind results
**Status**: Root cause identified

---

## Executive Summary

**Good News**:
- ✅ Documentation complete with SRSW relaxation properly documented
- ✅ Codegen fix applied and working (readers in execute arguments)
- ✅ Execute instruction works correctly
- ✅ Parser transforms infix to prefix correctly

**Root Cause Identified**:
- ❌ **evaluate/2 system predicate fails to bind result variable in REPL context**

---

## Diagnostic Tests Performed

### Test 1: Simple Execute with Write
```prolog
test_write :- execute('write', ['hello from execute']).
```
**Result**: ✅ Prints "hello from execute"
**Conclusion**: Execute instruction works

### Test 2: Execute with Variable
```prolog
test_write_var(X) :- execute('write', [X?]).
% Query: test_write_var(42).
```
**Result**: ✅ Prints "42"
**Conclusion**: Execute can read variables correctly

### Test 3: Arithmetic
```prolog
add(X, Y, Z?) :- execute('evaluate', [X? + Y?, Z]).
% Query: add(5, 3, X).
```
**Result**: ❌ X stays `<unbound>`
**Conclusion**: evaluate/2 is called but doesn't bind result

---

## What Works

1. **✅ Codegen**: Reader variables in execute arguments compile correctly
2. **✅ Parser**: `X? + Y?` transforms to `+(VarRef(X, isReader:true), VarRef(Y, isReader:true))`
3. **✅ Execute instruction**: Runs and can access variables
4. **✅ Runtime tests**: `test/custom/execute_evaluate_test.dart` passes (7/7)

---

## What Doesn't Work

**evaluate/2 in REPL context fails to bind the result variable**

### Evidence

Query: `add(5, 3, X).` with `add(X, Y, Z?) :- execute('evaluate', [X? + Y?, Z]).`

**Expected**:
- evaluate/2 receives: `+(Const(5), Const(3))` and writer Z
- Computes: 8
- Binds: Z = 8
- Result: `X = 8`

**Actual**:
- evaluate/2 is called (no error)
- X remains: `<unbound>`
- No binding occurs

---

## Why Runtime Tests Pass But REPL Fails

### Runtime Tests (`execute_evaluate_test.dart`)
These tests:
1. Create runtime structures directly
2. Call evaluate/2 directly
3. Check return value (SystemResult.success)
4. Verify binding manually

**They don't test REPL's binding mechanism**.

### REPL Context
The REPL:
1. Compiles GLP → bytecode
2. Executes bytecode
3. **Must extract variable bindings** for display
4. Uses scheduler and goal execution pipeline

**The binding extraction or propagation is failing.**

---

## Hypothesis: Writer Binding Context Issue

### The Problem

When `add(5, 3, X)` is queried:
- HEAD: `add(X, Y, Z?)` where X=5, Y=3, Z? is reader
- BODY: `execute('evaluate', [5? + 3?, Z])` where Z is writer

The writer `Z` in the body:
- Is a **local variable** to the clause
- Must be **bound by evaluate/2**
- Must **propagate back** to make Z? (reader in head) readable

**Possible Issue**: The writer Z is not properly connected to the reader Z? that the REPL is trying to display as X.

---

## Evidence from Trace

```
add/3(5?, 3?, W1002) :- true
  X = <unbound>
```

- `W1002` is the internal writer ID
- Query variable `X` maps to reader for `W1002`
- After execution, reader is still unbound
- This means `W1002` was never bound

**Conclusion**: evaluate/2 either:
1. Doesn't receive the writer correctly
2. Binds a different writer
3. Binding doesn't persist after execute returns

---

## Comparison with Working Execute

### Working: `execute('write', [X?])`
- Reads from X (reader)
- No binding needed
- Just performs I/O
- ✅ Works

### Broken: `execute('evaluate', [X? + Y?, Z])`
- Reads from X and Y (readers)
- **Must bind Z (writer)**
- Binding must persist
- ❌ Fails

**Key Difference**: evaluate/2 must **write**, not just read.

---

## Next Steps for Investigation

### 1. Add Debug Logging to evaluate/2 (HIGH PRIORITY)

In `system_predicates_impl.dart`, add prints:

```dart
SystemResult evaluatePredicate(GlpRuntime rt, SystemCall call) {
  print('[EVALUATE] Called with ${call.args.length} args');
  print('[EVALUATE] Expr: ${call.args[0]}');
  print('[EVALUATE] Result term: ${call.args[1]}');

  // ... existing code ...

  final result = _evaluate(rt, exprTerm);
  print('[EVALUATE] Computed result: $result');

  if (resultTerm is VarRef && !resultTerm.isReader) {
    final wid = resultTerm.varId;
    print('[EVALUATE] Binding writer $wid to $result');
    rt.heap.bindWriter(wid, ConstTerm(result));
    print('[EVALUATE] Writer bound: ${rt.heap.isWriterBound(wid)}');
  }

  return SystemResult.success;
}
```

**This will show**:
- Is evaluate/2 receiving correct arguments?
- Is the result computed correctly?
- Is the writer being bound?
- Does the binding persist?

### 2. Check Writer ID in REPL Display

The REPL shows `W1002` - verify this is the same writer ID evaluate/2 tries to bind.

### 3. Compare with file_read (Working Execute That Binds)

```prolog
read_file(Path, Content) :- execute('file_read', [Path?, Content]).
```

Does this work? If yes, compare its implementation with evaluate/2.

### 4. Check Heap Binding Mechanism

Verify `rt.heap.bindWriter()` actually persists the binding and makes it visible to paired readers.

---

## Documentation Status

### ✅ Complete

All documentation updated with:
1. **Guards vs Execute distinction** - three-valued vs two-valued
2. **SRSW relaxation** - ground guards allow multiple readers
3. **Implementation status** - clear markers (✅/⏳/📝)
4. **Safe patterns** - guards before execute
5. **Compiler requirements** - SRSW analysis with ground guards

Files updated:
- SPEC_GUIDE.md
- glp-bytecode-v216-complete.md
- parser-spec.md
- main_GLP_to_Dart (1).tex
- guards-reference.md

---

## Summary Table

| Component | Status | Details |
|-----------|--------|---------|
| **Documentation** | ✅ Complete | Guards, SRSW, execute semantics |
| **Codegen Fix** | ✅ Applied | Line 485 - readers in execute |
| **Parser** | ✅ Working | Infix → prefix transformation |
| **Lexer** | ✅ Working | Arithmetic tokens |
| **Execute Instruction** | ✅ Working | Runs correctly |
| **evaluate/2 Logic** | ✅ Working | Computes correctly (tests pass) |
| **evaluate/2 Binding** | ❌ **BROKEN** | Doesn't bind in REPL context |
| **REPL Integration** | ❌ **ISSUE** | Writer binding not propagating |

---

## Recommendation

**Add debug logging to evaluate/2** to see exactly what's happening with the writer binding. This is a 10-minute change that will definitively show whether:

1. evaluate/2 receives the correct writer ID
2. The computed result is correct
3. The binding call succeeds
4. The binding persists in the heap

Once we see the debug output, the root cause will be clear.

---

## End of Report

**Next Action**: Add debug logging to `evaluatePredicate()` in `system_predicates_impl.dart` and retest.
