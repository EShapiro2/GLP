# Suspension Reactivation Investigation - Handover Report
**Date:** 2025-11-17
**Session:** Investigation of why goals don't reactivate after FCP migration
**Status:** Root cause identified - architectural issue with multi-commit variable chains

---

## Executive Summary

After fixing the cycle bug (11/23 tests passing), I implemented suspension processing in `bindVariable()` to wake goals when variables are bound. However, quicksort still fails with the same symptom: `X = <unbound>, → 9 goals`.

**Root Cause Identified:** Variables bound to other unbound variables in one commit don't get their suspensions processed when the target variable is bound in a later commit.

**Impact:** All tests requiring suspension/reactivation across multiple commits fail (12 tests).

**Solution Required:** Either fix `applySigmaHatFCP` to handle chains, or add post-commit chain resolution.

---

## Work Completed This Session

### 1. Added Suspension Processing to bindVariable() ✅

**Files Modified:**
- `glp_runtime/lib/runtime/heap_fcp.dart`
- `glp_runtime/lib/bytecode/runner.dart`

**Changes:**

#### heap_fcp.dart
```dart
/// API: Bind variable to a term
/// Returns list of goals to reactivate (FCP: ALL bindings process suspensions)
List<GoalRef> bindVariable(int varId, Term value) {
  final (wAddr, rAddr) = varTable[varId]!;

  // Dereference value if it's a VarRef
  var finalValue = value;
  if (value is VarRef) {
    final (targetWAddr, _) = varTable[value.varId]!;
    finalValue = derefAddr(targetWAddr);
  }

  // Save suspension list BEFORE overwriting reader content
  final oldContent = cells[rAddr].content;

  // Bind both cells to the dereferenced value
  cells[wAddr].content = finalValue;
  cells[wAddr].tag = CellTag.ValueTag;
  cells[rAddr].content = finalValue;
  cells[rAddr].tag = CellTag.ValueTag;

  // Process suspensions and return activated goals (FCP: every binding wakes goals)
  final activations = <GoalRef>[];
  if (oldContent is SuspensionRecord) {
    print('[TRACE HeapFCP bindVariable] Processing suspensions for V$varId:');
    _walkAndActivate(oldContent, activations);
  }
  return activations;
}

/// Walk suspension list and activate armed records (from commit.dart)
static void _walkAndActivate(SuspensionRecord? list, List<GoalRef> acts) {
  var current = list;
  int count = 0;

  while (current != null) {
    if (current.armed) {
      acts.add(GoalRef(current.goalId!, current.resumePC));
      current.disarm();  // Prevent re-activation
      count++;
    }
    current = current.next;
  }

  print('[TRACE HeapFCP bindVariable]   Activated $count goal(s)');
}
```

Updated wrapper methods to return activations:
```dart
List<GoalRef> bindVariableConst(int varId, Object? v) {
  return bindVariable(varId, ConstTerm(v));
}

List<GoalRef> bindVariableStruct(int varId, String functor, List<Term> args) {
  return bindVariable(varId, StructTerm(functor, args));
}

List<GoalRef> bindWriterConst(int writerId, Object? v) => bindVariableConst(writerId, v);

List<GoalRef> bindWriterStruct(int writerId, String f, List<Term> args) {
  return bindVariableStruct(writerId, f, args);
}
```

#### runner.dart
Simplified BODY instruction handlers (already had `processBindSuspensions`, now just use return value):

```dart
// BodySetConst
if (cx.inBody) {
  // bindWriterConst now returns activations (FCP: all bindings wake goals)
  final acts = cx.rt.heap.bindWriterConst(op.writerId, op.value);
  for (final a in acts) {
    cx.rt.gq.enqueue(a);
    if (cx.onActivation != null) cx.onActivation!(a);
  }
}

// BodySetStructConstArgs
if (cx.inBody) {
  final args = <Term>[
    for (final v in op.constArgs)
      v is Term ? v : ConstTerm(v)
  ];
  // bindWriterStruct now returns activations (FCP: all bindings wake goals)
  final acts = cx.rt.heap.bindWriterStruct(op.writerId, op.functor, args);
  for (final a in acts) {
    cx.rt.gq.enqueue(a);
    if (cx.onActivation != null) cx.onActivation!(a);
  }
}

// BodySetConstArg
final wid = cx.env.w(op.slot);
if (cx.inBody && wid != null) {
  // bindWriterConst now returns activations (FCP: all bindings wake goals)
  final acts = cx.rt.heap.bindWriterConst(wid, op.value);
  for (final a in acts) {
    cx.rt.gq.enqueue(a);
    if (cx.onActivation != null) cx.onActivation!(a);
  }
}
```

**Result:**
- ✅ Compiles successfully
- ✅ No regressions (still 11/23 tests passing)
- ❌ Doesn't fix quicksort - bindVariable isn't being called where needed

---

## Root Cause Analysis

### The Quicksort Trace

Traced execution of `quicksort([1,2], X)`:

**Goal 10002** (qsort on smaller partition):
1. Spawned with reader R1009 for the sorted output
2. Tries to match clauses for qsort/3
3. All clauses fail because R1009 is unbound
4. Suspends on R1009, waiting for it to be bound

**Goal 10003** (partition/4 - first call):
1. HEAD phase creates σ̂w with binding: **W1009 → R1014**
2. Commit applies this binding:
   ```
   [TRACE Commit FCP] Applying σ̂w to heap (3 bindings):
     W1009 → R1014?
   ```
3. At this point, R1014 is UNBOUND (another variable)
4. No suspensions processed because W1009 is bound to an unbound variable

**Goal 10004** (partition/4 - second call):
1. HEAD phase creates σ̂w with binding: **W1014 → Const(nil)**
2. Commit applies this binding:
   ```
   [TRACE Commit FCP] Applying σ̂w to heap (2 bindings):
     W1014 → Const(nil)
   ```
3. This makes R1014 bound to nil
4. Suspensions on R1014 are processed (none exist)
5. **Goal 10002 is NOT reactivated** because its suspension is on R1009, not R1014

**The Chain:**
- Goal 10002 suspended on R1009
- W1009 → R1014 (commit 1)
- W1014 → nil (commit 2)
- Transitively: W1009 → R1014 → nil
- But R1009's suspension list never gets processed!

### Why bindVariable() Didn't Help

The `bindVariable()` changes I made process suspensions for BODY-phase bindings. But in this case:
1. W1009 → R1014 happens during HEAD commit (applySigmaHatFCP)
2. W1014 → nil happens during HEAD commit (applySigmaHatFCP)
3. Neither calls `bindVariable()` - they directly modify heap cells

### The Fundamental Issue

**FCP Assumption:** Variables are bound to values (or other unbound variables) atomically within a single commit. The commit processes suspensions on the variable being bound.

**GLP Reality:** Multi-goal concurrent execution means:
1. Goal A binds W1 → R2 (R2 unbound)
2. Goal B later binds W2 → value
3. Now W1 is transitively bound, but we never revisited W1's suspensions

This is a **multi-commit variable chain resolution problem**.

---

## Detailed Code Analysis

### applySigmaHatFCP (commit.dart)

Current implementation:
```dart
static List<GoalRef> applySigmaHatFCP({
  required HeapFCP heap,
  required Map<int, Object?> sigmaHat,
}) {
  final activations = <GoalRef>[];

  for (final entry in sigmaHat.entries) {
    final varId = entry.key;
    var value = entry.value;

    if (value == null) continue;

    final (wAddr, rAddr) = heap.varTable[varId]!;

    // FCP line 226: CRITICAL - dereference by ADDRESS before binding
    if (value is VarRef) {
      final (targetWAddr, _) = heap.varTable[value.varId]!;
      value = heap.derefAddr(targetWAddr);  // ← Dereferences target
      print('[TRACE Commit FCP] Dereferenced VarRef(${(entry.value as VarRef).varId}) → $value');
    }

    // FCP line 301: Save suspension list before replacing reader content
    final oldContent = heap.cells[rAddr].content;

    // FCP lines 233/303: Bind BOTH cells to dereferenced value
    heap.cells[wAddr].content = value;
    heap.cells[wAddr].tag = CellTag.ValueTag;
    heap.cells[rAddr].content = value;
    heap.cells[rAddr].tag = CellTag.ValueTag;

    // FCP lines 245-254: Walk saved suspension list and activate
    if (oldContent is SuspensionRecord) {
      print('[TRACE Commit FCP] Processing suspension list for R$varId:');
      _walkAndActivate(oldContent, activations);  // ← Only processes THIS variable's suspensions
    }
  }

  return activations;
}
```

**Problem:** When we bind W1009 → R1014, we:
1. Dereference R1014 (gets R1014? because it's unbound)
2. Bind W1009 and R1009 to R1014?
3. Process suspensions on R1009 (none at this point? Or are there?)

Wait - let me check if the suspension is added BEFORE or AFTER the commit...

### Suspension Timeline

From trace:
```
[TRACE Commit FCP] Applying σ̂w to heap (3 bindings):
  W1009 → R1014?
[TRACE Commit FCP] Total goals reactivated: 0

[TRACE Spawn] Preparing to spawn qsort/3:
  argWriters: {1: W1005}
  argReaders: {0: R1009, 2: R1011}

[TRACE _suspendAndFail] Goal 10002 adding R1009 to U, failing to next clause
[TRACE NoMoreClauses] Goal 10002 suspending:
  U (blocked readers): [1009]
```

**Sequence:**
1. W1009 → R1014 commit happens FIRST
2. Goal 10002 spawned with R1009 as reader AFTER
3. Goal 10002 suspends on R1009 AFTER the commit

So the suspension is added AFTER W1009 is already bound to R1014!

This means:
- When goal 10002 suspends on R1009, the reader cell R1009 contains R1014? (a VarRef)
- The suspension record gets added to R1009
- Later when W1014 → nil, R1014 gets bound to nil
- But we never revisit R1009 to see that its content (R1014) is now bound

### The Real Problem

**At suspension time:**
- R1009's cell content: R1014? (VarRef - unbound)
- Suspension added to R1009's cell
- Cell now has: SuspensionRecord (content replaced)

**At W1014 → nil commit:**
- R1014 and W1014 cells get bound to nil
- Suspensions on R1014 processed (none)
- R1009's cell still has suspension record
- R1009's content was R1014?, but we replaced it with SuspensionRecord
- So we lost the link to R1014!

**AH!** The suspension mechanism REPLACES the reader cell content. So when we add a suspension to R1009:
```dart
// From heap_fcp.dart:163
cells[rAddr].content = record;  // REPLACE content
```

We REPLACE R1014? with the SuspensionRecord. So the link from R1009 to R1014 is LOST!

---

## The Actual Root Cause

**The suspension mechanism destroys variable chains!**

When we:
1. Bind W1009 → R1014 (R1009's content becomes R1014? - a VarRef)
2. Suspend goal on R1009 (R1009's content becomes SuspensionRecord, REPLACING R1014?)
3. Bind W1014 → nil (R1014 becomes nil, but R1009 doesn't know anymore)

The suspension list mechanism uses the reader cell's `content` field, which overwrites the variable reference.

### FCP Design Check

In FCP C code, the reader cell is a union:
```c
union {
  Pointer pointer;        // Points to another cell
  SuspensionRecord *list; // Suspension list
  Value value;            // Bound value
}
```

When unbound: pointer to writer
When suspended: suspension list
When bound: value

**In FCP:** Before suspending, the cell contains a pointer. The suspension mechanism saves/restores this? No, in FCP the pointer is from reader→writer initially, not to another variable.

The issue is: **GLP creates variable-to-variable bindings (W1→R2) that FCP doesn't have.**

In pure FCP:
- Unbound variable: reader points to writer, writer points to reader
- Suspended: reader has suspension list
- Bound: both have value

In GLP after σ̂w with W1→R2:
- W1 and R1 both contain R2? (VarRef to variable 2)
- This breaks the FCP invariant!

---

## Solution Options

### Option 1: Save Variable Chain Before Suspension

When suspending on R1009, if its content is a VarRef, save that reference and check it later.

**Problem:** Where to save it? The suspension record doesn't have a field for this.

### Option 2: Don't Bind to VarRefs in applySigmaHatFCP

When committing W1→R2, if R2 is unbound:
- Don't bind W1 to R2
- Instead, create a "delayed binding" or "equality constraint"
- Only bind when R2 gets bound

**Problem:** Complex, requires new data structure

### Option 3: Post-Commit Chain Resolution

After every commit, scan all suspended goals:
- For each suspension on Rvar
- Check if Rvar is now transitively bound
- If yes, reactivate the goal

**Problem:** O(n) scan after every commit, expensive

### Option 4: Hybrid Approach - Keep Chain Info in Suspension

Modify SuspensionRecord to include:
```dart
class SuspensionRecord {
  int? goalId;
  final int resumePC;
  SuspensionRecord? next;

  // NEW: If suspended on a variable that was bound to another variable
  VarRef? chainTarget;  // The variable this one is waiting for
}
```

When suspending:
1. Check if reader cell content is VarRef
2. Save it in suspension.chainTarget
3. Replace content with suspension

When binding a variable:
1. Process direct suspensions
2. Walk all suspensions in the system looking for chainTarget == this variable
3. Reactivate those too

**Problem:** Still O(n) scan, but only when binding (not every commit)

### Option 5: Reverse Mapping

Maintain a reverse map:
```dart
Map<int, Set<int>> variableChains;  // targetVar -> [vars waiting on it]
```

When binding W1→R2:
- Add entry: variableChains[2].add(1)

When binding W2→value:
- Look up variableChains[2]
- For each varId in that set, process suspensions on Rvar

**This seems most efficient!**

---

## Recommended Solution

### Implement Variable Chain Tracking

**Data Structure:**
```dart
// In HeapFCP or Runtime
Map<int, Set<int>> chainedVars = {};  // targetVarId -> {varIds waiting on it}
```

**During Commit (applySigmaHatFCP):**
```dart
for (final entry in sigmaHat.entries) {
  final varId = entry.key;
  var value = entry.value;

  if (value == null) continue;

  // If binding to another variable, track the chain
  if (value is VarRef && !heap.isFullyBound(value.varId)) {
    // Add to chain: when value.varId gets bound, revisit varId
    heap.chainedVars.putIfAbsent(value.varId, () => {}).add(varId);
  }

  // ... existing binding logic ...

  // After processing suspensions on this variable
  // Check if any other variables are chained to this one
  if (heap.chainedVars.containsKey(varId)) {
    for (final chainedVarId in heap.chainedVars[varId]!) {
      final (_, chainedRAddr) = heap.varTable[chainedVarId]!;
      final chainedContent = heap.cells[chainedRAddr].content;

      if (chainedContent is SuspensionRecord) {
        _walkAndActivate(chainedContent, activations);
      }
    }
    // Clear the chain entries
    heap.chainedVars.remove(varId);
  }
}
```

**Advantages:**
- O(1) to add chain entry
- O(k) to process chains when binding (k = number of chained vars)
- No global scanning
- Fits FCP model

**Disadvantages:**
- Extra memory for chain map
- Complexity in commit logic

---

## Test Status

### Current Results: 11/23 passing (47%)

#### ✅ Passing (11):
1. Hello World
2. Simple Unification
3. Merge [1,2,3] and [a,b]
4. Merge Standalone
6. Clause Lookup
7. Simple Run
8. Merge via Metainterpreter (SRSW fix)
16. Insertion sort empty list
20. Metainterpreter: merge([a],[b],X)
21. Metainterpreter: merge([a],[b,c,d],X)
23. Metainterpreter: merge chain with shared vars

#### ❌ Failing (12):
5. Merge with Reader (SRSW violation in source)
9. Insertion Sort via Metainterpreter
10. Addition 5+3
11. Multiplication 4*7
12. Compound (2*3)+4
13. Structure Demo
14. Quicksort empty list
15. Quicksort single element
17. Insertion sort single element
18. Insertion sort two elements
**19. Quicksort two elements [1,2]** ← Critical test
22. Metainterpreter: run2(X)

**No regressions** from bindVariable() changes.

---

## Files Modified This Session

1. **glp_runtime/lib/runtime/heap_fcp.dart**
   - Added `_walkAndActivate` static method
   - Changed `bindVariable()` to return `List<GoalRef>`
   - Changed `bindVariableConst()` to return `List<GoalRef>`
   - Changed `bindVariableStruct()` to return `List<GoalRef>`
   - Changed `bindWriterConst()` to return `List<GoalRef>`
   - Changed `bindWriterStruct()` to return `List<GoalRef>`
   - Added suspension processing to all bind methods

2. **glp_runtime/lib/bytecode/runner.dart**
   - Simplified BodySetConst handler (line 1835)
   - Simplified BodySetStructConstArgs handler (line 1850)
   - Simplified BodySetConstArg handler (line 1863)
   - Removed redundant `processBindSuspensions()` calls

3. **udi/HANDOVER_FCP_MIGRATION_20251117.md** (created earlier)
   - Comprehensive report on cycle fix work

4. **udi/HANDOVER_SUSPENSION_INVESTIGATION_20251117.md** (this file)

---

## Next Steps

### Immediate (High Priority)

1. **Implement Variable Chain Tracking** (~2-3 hours)
   - Add `Map<int, Set<int>> chainedVars` to HeapFCP
   - Modify `applySigmaHatFCP` to track chains when binding var→var
   - Process chained variable suspensions when target gets bound
   - Add tracing for chain operations

2. **Test quicksort([1,2], X)** (~30 min)
   - Should see chain tracking in trace
   - Should see goal 10002 reactivated when W1014→nil happens
   - Should complete with X = [1, 2]

3. **Run full test suite** (~15 min)
   - Expect 18-20/23 passing (most chain-dependent tests should work)

### Secondary (Medium Priority)

4. **Update unit tests** (~2-3 hours)
   - 33 unit tests need heap API updates
   - Similar changes as REPL updates

5. **Clean up and optimize** (~1-2 hours)
   - Remove deprecated code
   - Add chain cleanup (prevent memory leaks)
   - Optimize chain lookup

### Documentation

6. **Update specs** (~1 hour)
   - Document variable chain semantics
   - Add examples to bytecode spec
   - Explain why GLP needs this vs pure FCP

---

## Key Insights

1. **Suspension mechanism breaks variable chains:** The FCP design replaces reader cell content with suspension lists, losing the VarRef link.

2. **Multi-commit execution creates chains:** GLP's concurrent execution means variables can be bound in stages across different goal commits.

3. **bindVariable() was correct but insufficient:** BODY-phase bindings do need suspension processing, but most bindings happen during HEAD commits.

4. **Variable chain tracking is necessary:** Need explicit tracking of var→var dependencies to handle transitive bindings.

5. **FCP assumption violated:** FCP assumes atomic binding within single commit, GLP has multi-commit staged bindings.

---

## Performance Considerations

### Chain Tracking Overhead

**Per commit:**
- Adding chain entry: O(1) per var→var binding
- Processing chains: O(k) where k = vars chained to this var
- Typical k: 0-3 (few variables chain together)

**Memory:**
- Map overhead: ~24 bytes per entry
- Set overhead: ~16 bytes + 8 bytes per element
- Typical: 50-100 entries max during complex computation
- Total: ~2-4 KB

**Acceptable overhead for correctness.**

### Potential Optimizations

1. **Lazy cleanup:** Don't remove chain entries immediately, batch cleanup
2. **Weak references:** Use weak maps if supported (Dart doesn't have this)
3. **Pooling:** Reuse Set objects to reduce allocations

---

## Comparison with Claude Web's Assessment

Claude Web said:
> "The issue is that `bindVariable()` in HeapFCP doesn't call `processBindSuspensions()`"

**My finding:** That was part of the issue, but the deeper problem is variable chains across commits. The `bindVariable()` fix helps for BODY-phase bindings but doesn't solve the HEAD-commit chain problem.

Claude Web's solution was correct for BODY-phase, but the quicksort failure requires the additional chain tracking mechanism.

---

## Lessons Learned

1. **Trace-driven debugging is essential:** The detailed trace showed exactly when each binding happened and why reactivation failed.

2. **FCP isn't a drop-in solution:** GLP's semantics differ from FCP in subtle ways that require extensions to the basic model.

3. **Concurrent execution complicates binding:** Single-threaded FCP doesn't deal with multi-commit variable chains.

4. **Data structure design matters:** The suspension mechanism's use of cell content field has implications for variable chains.

5. **Test incrementally:** Each fix (cycle, bindVariable, chain tracking) builds on the previous and can be tested independently.

---

## Git Status

### Modified (Not Committed):
```
M glp_runtime/lib/runtime/heap_fcp.dart
M glp_runtime/lib/bytecode/runner.dart
```

### New Files:
```
?? udi/HANDOVER_FCP_MIGRATION_20251117.md
?? udi/HANDOVER_SUSPENSION_INVESTIGATION_20251117.md
```

---

## Recommended Commit Strategy

**Commit 1:** FCP cycle fix (already done, in working tree)
- heap_fcp.dart: allocateVariable() reader cell null
- Test improvement: 2/23 → 11/23

**Commit 2:** bindVariable suspension processing (current state)
- heap_fcp.dart: bindVariable() returns activations
- runner.dart: simplified BODY handlers
- No test improvement (11/23 still) but architecturally correct

**Commit 3:** Variable chain tracking (TODO)
- heap_fcp.dart: add chainedVars map
- commit.dart: track and process chains
- Expected: 11/23 → 18-20/23

---

## References

- Previous handover: `HANDOVER_FCP_MIGRATION_20251117.md`
- FCP source: `/Users/udi/Dropbox/Concurrent Prolog/FCP/Savannah`
- Bytecode spec: `docs/glp-bytecode-v216-complete.md`
- Runtime spec: `docs/glp-runtime-spec.txt`

---

**Report Generated:** 2025-11-17T18:30:00Z
**Investigation Duration:** ~1.5 hours
**Lines of Code Changed:** ~80 lines (heap_fcp.dart + runner.dart)
**Root Cause:** Variable chain tracking needed for multi-commit bindings
**Estimated Fix Time:** 2-3 hours for chain tracking implementation
