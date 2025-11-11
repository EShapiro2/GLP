# Writer-Reader Suspension Bug Report

**Date**: 2025-11-09
**Status**: Bug Isolated, Fix Needed
**Priority**: Critical - Blocks metainterpreter and inter-goal communication

---

## Executive Summary

A critical bug prevents goals from reading values written by sibling goals in the same clause. When a clause spawns multiple body goals where one produces data and another consumes it, the parent goal suspends incorrectly and never completes, even though both child goals succeed.

**Impact**: This bug breaks:
- Metainterpreter (`run/1` with `clause/2`)
- Any pattern where body goals communicate via writer/reader pairs
- Dataflow between concurrent goals in the same clause

---

## Minimal Reproduction Case

**Test File**: `/Users/udi/GLP/udi/glp/test_binding.glp`

```prolog
% Simple predicate that writes to its second argument
fetch(a, p(q)).
fetch(b, r(s,t)).

% Test: Fetch data, then process it
test3 :- fetch(a, Body), execute(Body?).

% Predicate that reads its argument
execute(p(q)).
execute(r(s,t)).
```

**Expected Behavior**: `test3` should succeed
- Spawn `fetch(a, Body)` which binds `Body = p(q)`
- Spawn `execute(Body?)` which reads `Body?` and matches `p(q)`
- Parent `test3` terminates successfully

**Actual Behavior**: `test3` suspends

```
GLP> test3
1: test3/0 :- fetch/2(a?, W1002), execute/1(R1003?)
1: test3/0 → suspended
10000: fetch/2(a?, W1002) :- true
1: execute/1(p(q)?) :- true
```

**Observations**:
1. Parent `test3` spawns both goals
2. Parent **suspends** (should terminate)
3. `fetch(a, W1002)` executes and succeeds - binds W1002 to `p(q)`
4. `execute(p(q)?)` executes and succeeds - successfully reads the value
5. Parent NEVER resumes or completes

---

## Alternative Test Case

```prolog
% Test 1: Sequential bind-then-read pattern
test1(X) :- bind_term(a, Y), process(Y?).

bind_term(a, f(x)).
process(f(x)).
```

**Same behavior**:
```
GLP> test1(X)
1: test1/1(W1000) :- bind_term/2(a?, W1004), process/1(R1005?)
1: test1/1(W1000) → suspended
10000: bind_term/2(a?, W1004) :- true
1: process/1(f(x)?) :- true
  X = <unbound>
```

Both spawned goals succeed, parent suspends and never completes.

---

## Key Pattern

The bug occurs when:
1. **Parent clause** has multiple body goals: `goal1(..., Writer), goal2(Reader?, ...)`
2. **First goal** binds a writer variable
3. **Second goal** reads via the paired reader
4. Both child goals execute and **succeed**
5. Parent goal **suspends** instead of completing

**Critical**: The child goals CAN communicate - `execute(p(q)?)` successfully reads the value written by `fetch`. The bug is that the **parent doesn't complete** even though both children succeeded.

---

## Why This Matters

### Metainterpreter Broken

The metainterpreter relies on this exact pattern:

```prolog
run(A) :- otherwise | clause(A?, B), run(B?).
```

- `clause(A?, B)` writes clause body to `B`
- `run(B?)` should read and execute it
- But parent `run(A)` suspends instead of spawning both goals

This is why circular merge via metainterpreter fails:
```
run((merge(Xs?,[a],Ys),merge(Ys?,[b],Xs)))
```

Every `run/1` call suspends waiting for clause bodies to be bound, even though `clause/2` succeeds in binding them.

### Breaks Dataflow

Any clause that implements producer-consumer pattern:
```prolog
pipeline :- generate(Data), process(Data?), output(Data?).
```

The parent will suspend instead of letting the goals execute concurrently.

---

## What Works

**Direct circular merge** (without metainterpreter):
```prolog
merge2 :- merge(Xs?,[a],Ys), merge(Ys?,[b],Xs).
```

This WORKS correctly - produces infinite stream as expected.

**Why it works**: `merge2` is a single clause that spawns two `merge/3` goals. The difference is... (unknown - need investigation).

---

## Investigation Points

### 1. Why Does Parent Suspend?

Trace shows parent suspends AFTER spawning both goals:
```
1: test3/0 :- fetch/2(a?, W1002), execute/1(R1003?)
1: test3/0 → suspended
```

**Question**: What causes the parent to suspend? It should:
- Spawn both body goals
- Execute Proceed instruction
- Terminate successfully

### 2. Why Doesn't Parent Resume?

Both child goals succeed:
```
10000: fetch/2(a?, W1002) :- true
1: execute/1(p(q)?) :- true
```

When `fetch` binds W1002, it should activate any goals suspended on R1003. But the parent doesn't resume.

**Question**: Is the parent registering suspension on the reader? Should it be?

### 3. Spawn vs Body Execution

During body execution, the parent:
1. Executes PutWriter/PutReader instructions to set up arguments
2. Executes Spawn instructions to create child goals
3. Should execute Proceed to terminate

**Question**: Does Spawn check if arguments are bound? Does it suspend the parent if reader arguments are unbound?

### 4. Difference from merge2

`merge2` spawns two merge goals with circular readers and works fine. `test3` spawns two goals with linear dataflow and suspends.

**Question**: What's different about how these clauses are compiled/executed?

---

## Code Locations

### Relevant Files

1. **Test case**: `/Users/udi/GLP/udi/glp/test_binding.glp`
2. **Spawn instruction**: `/Users/udi/GLP/glp_runtime/lib/bytecode/runner.dart:1669`
3. **PutReader instruction**: `/Users/udi/GLP/glp_runtime/lib/bytecode/runner.dart:1432`
4. **Proceed instruction**: `/Users/udi/GLP/glp_runtime/lib/bytecode/runner.dart:2355`
5. **Suspension logic**: `/Users/udi/GLP/glp_runtime/lib/runtime/suspend_ops.dart`

### Spawn Instruction (line 1669)

```dart
if (op is Spawn) {
  if (cx.inBody) {
    // Create CallEnv from current argument registers
    final newEnv = CallEnv(
      writers: Map.from(cx.argWriters),
      readers: Map.from(cx.argReaders),
    );

    // Get entry point, create goal, enqueue
    final entryPc = prog.labels[op.procedureLabel];
    final newGoalId = cx.rt.nextGoalId++;
    final newGoalRef = GoalRef(newGoalId, entryPc);

    // ... format and track spawned goal ...

    cx.rt.gq.enqueue(newGoalRef);

    // Clear argument registers for next spawn
    cx.argWriters.clear();
    cx.argReaders.clear();
  }
  pc++; continue;
}
```

**Question**: Is there any suspension logic in Spawn? Or does suspension happen elsewhere?

### PutReader Instruction (line 1432)

Handles setting up reader arguments. Has special case for null (first occurrence):

```dart
if (value == null) {
  // First occurrence - create fresh unbound pair
  final (freshWriterId, freshReaderId) = cx.rt.heap.allocateFreshPair();
  cx.rt.heap.addWriter(WriterCell(freshWriterId, freshReaderId));
  cx.rt.heap.addReader(ReaderCell(freshReaderId));
  cx.clauseVars[op.varIndex] = freshWriterId;
  cx.argReaders[op.argSlot] = freshReaderId;
}
```

**Question**: Should PutReader check if the writer is bound? Should it suspend if unbound?

---

## Hypotheses

### Hypothesis 1: Spawn Suspends Parent

Maybe Spawn instruction checks if reader arguments are bound and suspends the parent if not.

**Evidence Against**: Spawn code doesn't show suspension logic. It just creates and enqueues the goal.

### Hypothesis 2: Goal Setup Suspends

Maybe when creating the child goal's environment, it checks reader bindings and suspends the parent.

**Evidence For**: Trace shows parent suspends RIGHT after spawning.

### Hypothesis 3: Parent Waits for Children

Maybe the parent is designed to wait for child goals to complete before proceeding.

**Evidence Against**: GLP is concurrent - parent should spawn and terminate, not wait.

### Hypothesis 4: Reader Registration

Maybe during body setup, encountering an unbound reader causes the parent to register itself as suspended on that reader.

**Evidence For**: This would explain why parent suspends after setup but before Proceed.

---

## What We Need

### Immediate

1. **Trace Spawn instruction execution**
   - Add debug output showing when Spawn executes
   - Show argument register contents
   - Show if any suspension occurs

2. **Trace PutReader execution**
   - Show when reader arguments are set up
   - Show if writer is bound or unbound
   - Show if suspension is triggered

3. **Trace Proceed execution**
   - Does it even reach Proceed?
   - Or does goal suspend before reaching it?

### Root Cause Analysis

Need to answer:
- **WHERE** does the parent goal suspend? (which instruction/phase)
- **WHY** does it suspend? (what condition triggers suspension)
- **SHOULD** it suspend? (is this by design or a bug)

### Fix Direction

Once we know where/why it suspends:

**If suspension is intentional but wrong**:
- Remove suspension logic from parent during body spawning
- Parent should spawn children and proceed, not wait

**If suspension is accidental**:
- Fix the condition that incorrectly triggers suspension
- Ensure parent only suspends if IT needs unbound data, not its children

**If coordination is needed**:
- Implement proper parent-child coordination
- Parent should track child completion if needed
- But standard GLP semantics say parent spawns and proceeds

---

## Test Commands

```bash
cd /Users/udi/GLP/udi

# Run minimal test
dart run glp_repl.dart
GLP> test_binding.glp
GLP> :trace
GLP> test3

# Run alternative test
GLP> test1(X)

# Compare with working case
GLP> circular_merge.glp
GLP> merge2
```

---

## Success Criteria

Fix is successful when:

```
GLP> test3
1: test3/0 :- fetch/2(a?, W1002), execute/1(R1003?)
10000: fetch/2(a?, W1002) :- true
1: execute/1(p(q)?) :- true
✓ Success  [NOT: → suspended]
```

And metainterpreter works:

```
GLP> run((merge(Xs?,[a],Ys),merge(Ys?,[b],Xs)))
[Shows alternating a/b pattern, not suspension]
```

---

## Related Issues

- Session report: `/Users/udi/GLP/docs/SESSION_REPORT_2025_11_09.md`
- Original issue identified during metainterpreter testing
- Isolated to minimal test case per user request
- Critical blocker for GLP dataflow semantics

---

**Report Status**: Ready for investigation
**Next Step**: Systematic tracing to identify where/why parent suspends
**Assigned To**: Claude Web (fresh investigation needed)
