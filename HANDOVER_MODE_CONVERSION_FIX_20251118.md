# Handover: Mode Conversion Bug Fixes - 2025-11-18

## Summary
Fixed three critical bugs in variable mode conversion and dereferencing that were preventing the metainterpreter from correctly binding variables through reader VarRef chains.

## Bugs Fixed

### Bug 1: UnifyWriter READ Mode - VarRef Type Check
**Location:** `glp_runtime/lib/bytecode/runner.dart` lines 1709-1744

**Problem:** After earlier fixes that changed `clauseVars` to store `VarRef(varId, isReader: false)` instead of bare `int`, the UnifyWriter READ mode was still checking `if (existingValue is int)`, causing it to skip binding when the clause variable was a VarRef.

**Fix:**
```dart
// Changed line 1710 from:
if (existingValue is int) {

// To:
if (existingValue is int || (existingValue is VarRef && !existingValue.isReader)) {
  // Extract the varId
  final clauseVarId = existingValue is int ? existingValue : (existingValue as VarRef).varId;
  // Then use clauseVarId instead of existingValue
```

**Result:** `qsort([],Rest?,Rest)` now correctly binds Rest to `[]` during direct execution.

### Bug 2: Commit - VarRef Dereferencing
**Location:** `glp_runtime/lib/runtime/commit.dart` lines 47-53

**Problem:** When σ̂w contained a binding to a reader VarRef (e.g., `W1012 → R1024?`), commit was always dereferencing from the **writer** address, ignoring the `isReader` flag. This caused bindings to reader VarRefs to be lost.

**Fix:**
```dart
// Changed lines 48-51 from:
if (value is VarRef) {
  final (targetWAddr, _) = varTable[value.varId]!;
  value = heap.derefAddr(targetWAddr);
}

// To:
if (value is VarRef) {
  final (targetWAddr, targetRAddr) = varTable[value.varId]!;
  final targetAddr = value.isReader ? targetRAddr : targetWAddr;
  value = heap.derefAddr(targetAddr);
}
```

**Result:** Writer→reader bindings now correctly dereference from the reader address.

### Bug 3: HeapFCP derefAddr - VarRef Chain Following
**Location:** `glp_runtime/lib/runtime/heap_fcp.dart` lines 81-87

**Problem:** Same issue as Bug 2 - when a cell was bound to a VarRef (variable→variable binding), `derefAddr` always followed the writer address, breaking reader VarRef chains.

**Fix:**
```dart
// Changed lines 83-85 from:
if (content is VarRef) {
  final (targetWAddr, _) = varTable[content.varId]!;
  current = targetWAddr;
  continue;
}

// To:
if (content is VarRef) {
  final (targetWAddr, targetRAddr) = varTable[content.varId]!;
  current = content.isReader ? targetRAddr : targetWAddr;
  continue;
}
```

**Result:** Variable chains like `W1012 → R1024 → W1024 → R1037 → []` now correctly dereference.

## Test Results

### Working Cases
- ✅ Direct execution: `qsort([],X,[])` → `X = []`
- ✅ Metainterpreter: `run(quicksort([],X))` → `X = []`
- ✅ Metainterpreter: `run(quicksort([1],X))` → `X = [1]`

### Failing Case
- ❌ Metainterpreter: `run(quicksort([1,2,3],X))` → `X = <unbound>`

**Symptom:** Goal suspends waiting for `X12` to be bound, but the partition base case that should bind it never executes or doesn't propagate the binding.

**Trace Evidence:**
```
Goal 10006: W1012 → R1024? (from partition recursive case)
Goal 10010: W1024 → R1037? (from partition recursive case)
Goal 10011: run(partition([],1,X37,X39)) - should bind W1037 to []
Goal 10012: reduce(qsort(X12?,X9,[1 | X15?]), X34) → suspended on R1012
```

The chain is: `W1012 → R1024 ← W1024 → R1037 ← W1037`, where W1037 should be bound to `[]` by the partition base case `partition([],A,[],[])`.

## Current State

**Files Modified:**
1. `glp_runtime/lib/bytecode/runner.dart` - UnifyWriter READ mode fix
2. `glp_runtime/lib/runtime/commit.dart` - VarRef dereferencing fix
3. `glp_runtime/lib/runtime/heap_fcp.dart` - derefAddr VarRef chain fix

**REPL Compiled:** `/Users/udi/GLP/udi/glp_repl` (as of 2025-11-18 18:54:13Z)

**Git Status:** Modified files not yet committed

## Next Steps

### Investigation Needed
The `run(quicksort([1,2,3],X))` case is suspending because of a complex variable chain. Possible causes:

1. **Missing execution:** The `partition([],1,X37,X39)` goal may not be executing due to scheduling/suspension issues
2. **Binding propagation:** The binding from `partition([],A,[],[])` may not be propagating through the reader VarRef chain
3. **Suspension loop:** Multiple goals may be waiting on each other, creating a deadlock

### Debug Approach
1. Add targeted trace for `partition([],1,X37,X39)` execution
2. Trace the binding chain: W1037 → [] and verify it propagates to R1037 → R1024 → R1012
3. Check if the suspended goal at PC 147 (UnifyStructure checking R1012) ever re-activates
4. Verify the suspension/reactivation mechanism is working for reader VarRef chains

### Files to Review
- Suspension mechanism in `runner.dart` (NoMoreClauses, U tracking)
- Goal reactivation after variable binding in `commit.dart`
- Reader cell suspension list handling in `heap_fcp.dart`

## Baseline Tests
Before attempting further fixes, establish baseline:
```bash
cd /Users/udi/GLP/glp_runtime && dart test
cd /Users/udi/GLP/udi && bash run_repl_tests.sh
```

Current baseline (approximate):
- Unit tests: ~86/89 passing
- REPL tests: To be determined after fixes stabilize

## Notes
- All three fixes follow the same pattern: respect the `isReader` flag when dereferencing VarRefs
- The fixes align with GLP's SRSW semantics where writers can be bound to readers to avoid WxW violations
- The metainterpreter stress-tests variable binding chains more than direct execution
