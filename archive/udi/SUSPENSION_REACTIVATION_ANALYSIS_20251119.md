# Suspension/Reactivation Analysis - 2025-11-19

## Executive Summary

Investigation into `run(quicksort([1,2,3],X))` suspension bug reveals a fundamental discrepancy between implementation expectations and GLP paper semantics. The bug may not be a bug at all - the current suspension behavior appears **semantically correct** according to the GLP paper, raising the question of why simpler cases work.

## Problem Statement

**Failing Case**: `run(quicksort([1,2,3],X))` suspends indefinitely
**Working Cases**: `run(quicksort([1],X))` and `run(quicksort([1,2],X))` succeed

**Suspension Point**:
```
Goal 10122: reduce(qsort(X12?,X9,[1 | X15?]), X34) → suspended on R1012
```

**Variable Chain at Suspension**:
```
W1012 → R1024 → W1024 → R1037 → W1037 → []
```

**Later Binding**:
```
Goal 10132 commits: σ̂w = {W1037 := []}
```

**Expected**: Goal 10122 should reactivate
**Actual**: Goal 10122 remains suspended

## Critical Discovery from GLP Paper

Reading `/Users/udi/Downloads/main_GLP_to_Dart (3).tex` Definition 3.4.2 (line 349):

### Reactivation Semantics

A goal G suspended on reader set W is reactivated if:
```
∃X? ∈ W such that X?σ̂? ≠ X?
```

Where the reader counterpart substitution σ̂? is defined as:
```
X?σ̂? = Xσ̂  for every X ∈ V_σ̂
```

### Key Implications

1. **σ̂? is a mathematical substitution**, NOT heap dereferencing
2. **Only direct membership** in σ̂w triggers reactivation
3. **Variable chains do NOT cause transitive reactivation**

### Application to Failing Case

For goal suspended on R1012 when σ̂w = {W1037 := []}:
- W1012 ∉ σ̂w (not directly in substitution)
- R1012 σ̂? = R1012 (unchanged by σ̂?)
- Condition `R1012 σ̂? ≠ R1012` is **FALSE**
- **Goal should NOT reactivate** ✓ (matches observed behavior)

## Current Implementation Status

### Files Modified During Session

#### `/Users/udi/GLP/udi/glp_repl.dart`
**Purpose**: Add trace control commands

**Changes**:
- Added state variables (lines 51-54):
  - `debugTrace` - toggles trace output
  - `showBindings` - toggles σ̂w bindings display
  - `debugOutput` - toggles DEBUG prints
  - `maxCycles` - goal reduction limit

- Added commands (lines 91-117):
  - `:binding` / `:b` - toggle bindings display
  - `:debug` / `:d` - toggle DEBUG output
  - `:limit <n>` - set reduction limit

- Wrapped DEBUG REPL prints with `if (debugOutput)` conditionals
- Updated `:help` text

**Status**: ✅ Working correctly

#### `/Users/udi/GLP/glp_runtime/lib/runtime/scheduler.dart`
**Purpose**: Pass trace flags to RunnerContext

**Changes** (line 129):
```dart
List<int> drain({int maxCycles = 1000, bool debug = false,
                 bool showBindings = true, bool debugOutput = false})
```

Pass flags to RunnerContext (lines 201-202):
```dart
showBindings: showBindings,
debugOutput: debugOutput,
```

**Status**: ✅ Working correctly

#### `/Users/udi/GLP/glp_runtime/lib/bytecode/runner.dart`
**Purpose**: Control DEBUG output and implement variable chain suspension

**Changes**:
- Added RunnerContext fields (lines 153-154):
  ```dart
  final bool showBindings;
  final bool debugOutput;
  ```

- Wrapped all DEBUG prints with `if (cx.debugOutput)` checks

- Modified COMMIT bindings display (lines 1976-1989):
  ```dart
  if (cx.showBindings && cx.sigmaHat.isNotEmpty) {
    cx.sigmaHat.forEach((writerId, value) {
      print('  W$writerId → $value');
    });
  }
  ```

- **ATTEMPTED FIX** to `_finalUnboundVar` (lines 209-228):
  ```dart
  int _finalUnboundVar(RunnerContext cx, int readerId) {
    final wid = cx.rt.heap.writerIdForReader(readerId);
    if (wid == null) return readerId;

    final (wAddr, _) = cx.rt.heap.varTable[wid]!;
    final derefResult = cx.rt.heap.derefAddr(wAddr);

    if (cx.debugOutput) print('[DEBUG _finalUnboundVar] R$readerId -> W$wid -> derefResult=$derefResult');

    if (derefResult is VarRef) {
      if (cx.debugOutput) print('[DEBUG _finalUnboundVar] Suspending on final var: ${derefResult.varId}');
      return derefResult.varId;
    }

    if (cx.debugOutput) print('[DEBUG _finalUnboundVar] Writer bound to ground term, returning original: $readerId');
    return readerId;
  }
  ```

**Status**: ⚠️ Fix appears correct for FCP wake-and-retry semantics but doesn't solve [1,2,3] suspension

### Key Implementation Code

#### `heap_fcp.dart` - bindVariable (lines 143-176)

```dart
List<GoalRef> bindVariable(int varId, Term value) {
  final (wAddr, rAddr) = varTable[varId]!;

  // Dereference value if it's a VarRef
  var finalValue = value;
  if (value is VarRef) {
    final (targetWAddr, _) = varTable[value.varId]!;
    finalValue = derefAddr(targetWAddr);  // ← Dereferences chain
  }

  // Save suspension list BEFORE overwriting reader content
  final oldContent = cells[rAddr].content;

  // Bind both cells to the dereferenced value
  cells[wAddr].content = finalValue;
  cells[wAddr].tag = CellTag.ValueTag;
  cells[rAddr].content = finalValue;
  cells[rAddr].tag = CellTag.ValueTag;

  // Process suspensions (FCP: every binding wakes goals)
  final activations = <GoalRef>[];
  if (oldContent is SuspensionRecord) {
    _walkAndActivate(oldContent, activations);
  }
  return activations;
}
```

**Issue**: This dereferences the value before binding, so chain W1012 → R1024 becomes W1012 → [] immediately when W1024 binds to []. But goal suspended on R1012 was already processed - no new activation occurs.

## The Paradox

### According to GLP Paper Semantics
- Goal suspended on R1012 should NOT wake when W1037 binds
- W1012 must appear directly in σ̂w to trigger reactivation
- Current behavior is **CORRECT** ✓

### According to Test Expectations
- Goal should eventually succeed for [1,2,3] like it does for [1,2]
- Suggests there should be a mechanism to create W1012 binding
- Current behavior is a **BUG** ✗

## Critical Questions for Claude Web

### Question 1: Why Does [1,2] Work?

If the same variable chain semantics apply, why does `run(quicksort([1,2],X))` succeed but [1,2,3] doesn't?

**Hypothesis**: The partition recursion for [1,2] may create fewer intermediate goals, allowing W1012 to appear directly in some σ̂w. Need to compare traces.

**Action Item**: Generate side-by-side traces of [1,2] vs [1,2,3] to identify where σ̂w structures diverge.

### Question 2: Is bindVariable Dereferencing Correct?

`heap_fcp.dart:146-149` dereferences the value before binding:

```dart
if (value is VarRef) {
  final (targetWAddr, _) = varTable[value.varId]!;
  finalValue = derefAddr(targetWAddr);
}
```

This causes bindings like `W1012 := R1024?` to immediately become `W1012 := []` when W1024 is already bound.

**According to GLP Paper**: Should σ̂w contain intermediate bindings or final dereferenced values?

**FCP Implementation**: Need to check `/Users/udi/Dropbox/Concurrent Prolog/FCP/Savannah` code to see if FCP dereferences before binding.

**Action Item**: Verify against FCP source whether bindVariable should dereference or preserve VarRef chains.

### Question 3: Is _finalUnboundVar Implementation Correct?

Current implementation (runner.dart:209-228) follows variable chains to find the ultimate unbound variable for suspension.

**According to glp-runtime-spec.txt:377-417**: This is the documented FCP wake-and-retry semantics.

**According to GLP paper Definition 3.4.2**: Reactivation is based on σ̂w membership, not heap chains.

**Conflict**: Spec says follow chains for suspension, paper says don't follow chains for reactivation.

**Action Item**: Reconcile `glp-runtime-spec.txt` with GLP paper semantics. Are these describing different aspects (suspension point vs reactivation condition)?

### Question 4: Should σ̂? Perform Heap Dereferencing?

GLP paper Definition 3.4.2 defines σ̂? as a **mathematical substitution**:
```
X?σ̂? = Xσ̂ for every X ∈ V_σ̂
```

Current implementation in Commit (`commit.dart`) performs heap dereferencing when applying σ̂w.

**Action Item**: Check if Commit implementation matches paper's mathematical substitution semantics or if it should avoid dereferencing.

## Code Sections to Review

### 1. `/Users/udi/GLP/glp_runtime/lib/runtime/commit.dart`

**Current Implementation** (lines 23-84):
```dart
void applyCommit(int goalId, Map<int, Term> sigmaHat, HeapFCP heap, ...) {
  final goalEnv = envTable[goalId];
  if (goalEnv == null) return;

  final varTable = goalEnv.varTable;
  final reactivations = <GoalRef>{};

  // Apply bindings
  sigmaHat.forEach((writerId, value) {
    var finalValue = value;

    // Dereference VarRef values
    if (value is VarRef) {
      final (targetWAddr, targetRAddr) = varTable[value.varId]!;
      final targetAddr = value.isReader ? targetRAddr : targetWAddr;
      finalValue = heap.derefAddr(targetAddr);
    }

    // Apply binding and collect reactivations
    final activatedGoals = heap.bindVariable(writerId, finalValue);
    reactivations.addAll(activatedGoals);
  });

  // Enqueue reactivated goals
  for (final g in reactivations) {
    gq.enqueue(g);
  }
}
```

**Question**: Should this dereference VarRef values, or should it preserve them as-is per paper's mathematical σ̂?

**Lines to review**: 47-53 (VarRef dereferencing), 58-61 (bindVariable call)

### 2. `/Users/udi/GLP/glp_runtime/lib/runtime/heap_fcp.dart`

**bindVariable Implementation** (lines 143-176)

**Question**: Should value dereferencing at lines 146-149 be removed to preserve VarRef chains in heap?

**Lines to review**: 146-149 (value dereferencing), 153-156 (cell binding), 159-175 (suspension processing)

**derefAddr Implementation** (lines 73-92)

**Question**: Is VarRef chain following (lines 83-87) correct, especially the fix from HANDOVER_MODE_CONVERSION_FIX_20251118.md?

**Lines to review**: 83-87 (VarRef chain following with isReader flag)

### 3. `/Users/udi/GLP/glp_runtime/lib/bytecode/runner.dart`

**_finalUnboundVar Implementation** (lines 209-228)

**Question**: Is following chains for suspension point correct if reactivation doesn't follow chains?

**Lines to review**: 215-219 (derefAddr call and VarRef check)

**HEAD Instruction Suspension** - Multiple locations:
- UnifyStructure: lines 1452-1464 (suspension on unbound reader)
- UnifyConstant: lines 1388-1398 (suspension on unbound reader)
- UnifyNil: lines 1496-1506 (suspension on unbound reader)

**Question**: Do these correctly call _finalUnboundVar and add to U set?

**Lines to review**: All calls to `_finalUnboundVar` and `cx.U.add()`

### 4. Specification Documents

**`/Users/udi/GLP/docs/glp-runtime-spec.txt`** (lines 377-417)
- Documents FCP wake-and-retry semantics
- Specifies _finalUnboundVar behavior
- **May conflict with GLP paper semantics**

**`/Users/udi/Downloads/main_GLP_to_Dart (3).tex`** (Definition 3.4.2, line 349)
- Defines formal reactivation semantics
- Specifies σ̂? as mathematical substitution
- **Authoritative source for correct semantics**

**Action Item**: Reconcile these two specifications. Update `glp-runtime-spec.txt` if it contradicts the paper.

## Trace Evidence

### Suspension Event (from user's trace)

```
Goal 10122 at PC 147 (HEAD phase):
  Instruction: UnifyStructure '.' 2
  Checking: R1012 (first arg of reduce/3 - the term being reduced)
  Status: R1012 → W1012 → unbound
  Result: Goal suspended, added to R1012's suspension list
```

### Later Binding Event

```
Goal 10132 commits:
  σ̂w = {W1037 := []}
  This eventually propagates: W1037 → [] causes W1024 → [] causes W1012 → []
  But goal 10122 remains suspended
```

### Debug Output from Modified Code

```
[DEBUG _finalUnboundVar] R1012 -> W1012 -> derefResult=W1034
[DEBUG _finalUnboundVar] Suspending on final var: 1034
```

**Observation**: derefAddr returns W1034 (another unbound writer), suggesting the chain exists but final variable is different from expected W1037.

**Question**: Is W1034 the correct final variable, or is there an error in variable chain tracking?

## Attempted Fixes During Session

### Fix Attempt 1: Recursive Chain Following
**Code**:
```dart
if (derefResult is VarRef) {
  return _finalUnboundVar(cx, derefResult.varId);
}
```
**Result**: Stack Overflow due to infinite recursion
**Error**: Recursive call without base case for VarRef → VarRef chains

### Fix Attempt 2: Iterative Chain Following
**Code**:
```dart
while (derefResult is VarRef && !derefResult.isReader) {
  final (nextWAddr, _) = cx.rt.heap.varTable[derefResult.varId]!;
  derefResult = cx.rt.heap.derefAddr(nextWAddr);
}
```
**Result**: Compiled but created nested loops
**Error**: derefAddr already loops internally, creating loop within loop

### Fix Attempt 3: Trust derefAddr (Current)
**Code**:
```dart
final derefResult = cx.rt.heap.derefAddr(wAddr);
if (derefResult is VarRef) {
  return derefResult.varId;
}
```
**Result**: Compiles and runs, still suspends on [1,2,3]
**Status**: May be correct implementation but doesn't solve the problem

**Insight**: The fix may be semantically correct (finding the right suspension variable) but the problem lies elsewhere - in how reactivation works or how σ̂w is constructed.

## Recommended Investigation Path

### Step 1: Verify Specification Consistency
1. Read FCP implementation at `/Users/udi/Dropbox/Concurrent Prolog/FCP/Savannah`
2. Compare FCP's bindVariable with current implementation
3. Check if FCP dereferences values before binding
4. Reconcile `glp-runtime-spec.txt` with GLP paper semantics

### Step 2: Compare Working vs Failing Traces
1. Generate detailed trace of `run(quicksort([1,2],X))` (working)
2. Generate detailed trace of `run(quicksort([1,2,3],X))` (failing)
3. Identify where σ̂w structures diverge
4. Determine why W1012 appears in σ̂w for [1,2] but not [1,2,3]

### Step 3: Verify Heap Operations
1. Check derefAddr implementation against FCP
2. Verify VarRef chain following uses isReader flag correctly (fix from HANDOVER_MODE_CONVERSION_FIX_20251118.md)
3. Test derefAddr with manual variable chains

### Step 4: Decide on Correct Semantics
Based on Steps 1-3, determine:
- Should σ̂? perform heap dereferencing or mathematical substitution?
- Should bindVariable preserve VarRef chains or dereference values?
- Should reactivation follow transitive chains or only direct membership?
- How should _finalUnboundVar work in relation to reactivation?

### Step 5: Implement Corrected Semantics
Once semantics are clarified, implement consistently across:
- `commit.dart` - σ̂w application
- `heap_fcp.dart` - bindVariable and derefAddr
- `runner.dart` - _finalUnboundVar and suspension logic

## Test Suite Status

### Baseline Tests (before this session's changes)
```bash
cd /Users/udi/GLP/udi
bash run_repl_tests.sh
```
**Result**: 21/29 passing

### Modified Files Not Yet Committed
- `glp_runtime/lib/bytecode/runner.dart` - Debug output, _finalUnboundVar
- `glp_runtime/lib/runtime/scheduler.dart` - Trace flags
- `udi/glp_repl.dart` - New commands
- `udi/glp_repl` - Compiled executable

**Git Status**: Modified files not committed

**Recommendation**: Before any fixes, commit current state and establish new baseline

## Summary for Claude Web Team

The [1,2,3] suspension bug investigation has revealed a fundamental question about GLP's suspension/reactivation semantics:

**The current implementation appears semantically correct** according to the GLP paper - goals should NOT wake up from transitive variable chain bindings, only from direct σ̂w membership.

**But this doesn't explain** why [1,2] works while [1,2,3] doesn't if both create similar variable chains.

**The core issue** likely involves one of:
1. **σ̂w construction**: How partition clauses build σ̂w during reduction
2. **Value dereferencing**: Whether Commit and bindVariable should dereference VarRef values
3. **Specification conflict**: `glp-runtime-spec.txt` vs GLP paper semantics
4. **Missing mechanism**: Some other way W1012 should appear in σ̂w for [1,2,3]

**Critical action**: Compare [1,2] and [1,2,3] traces to identify where σ̂w diverges, then verify current implementation against FCP source code and GLP paper.

## Files to Provide to Claude Web

1. This analysis document
2. `/Users/udi/GLP/docs/glp-runtime-spec.txt` (lines 377-417)
3. `/Users/udi/Downloads/main_GLP_to_Dart (3).tex` (Definition 3.4.2)
4. Trace outputs from user's session (working [1,2] vs failing [1,2,3])
5. Current implementation:
   - `glp_runtime/lib/runtime/commit.dart` (lines 23-84)
   - `glp_runtime/lib/runtime/heap_fcp.dart` (lines 143-176, 73-92)
   - `glp_runtime/lib/bytecode/runner.dart` (lines 209-228)
