# Bidirectional Stream Test - Handover Report

**Date:** 2026-01-21  
**Author:** Claude (Opus 4.5)  
**Status:** In Progress  
**Related Spec:** `/docs/ma/irmaGLP-spec.md`

---

## Summary

Investigation of the `bidirectional_stream_test` revealed multiple bugs in the GLP/maGLP stack. Several bugs have been fixed, but one critical bug remains: **HeadList/HeadNil do not properly handle unbound imported readers**, causing goals to fail instead of suspend when encountering nested variables from cross-agent assignments.

---

## Completed Work

### Bug 1: Scheduler suspendedGoals Tracking (FIXED)

**Location:** `lib/runtime/scheduler.dart`

**Problem:** `suspendedGoals` was only tracked when debug mode was enabled.

**Fix:** Now tracks suspended goals regardless of debug mode.

### Bug 2: PutVariable Heap Violation (FIXED)

**Location:** `lib/bytecode/runner.dart`

**Problem:** When `clauseVars` contained a ground value (StructTerm), PutVariable put it directly into `argSlots` instead of wrapping in a VarRef. This violated spec v2.16.3 Section 1.1 (heap-only argument registers).

**Fix:** Store the value on heap first, then put VarRef to that address in argSlots.

### Bug 3: VarRef→ValueTag Dereference in HEAD Phase (FIXED)

**Location:** `lib/bytecode/runner.dart` - UnifyVariable WRITE mode

**Problem:** When building tentative structures in HEAD phase (like `[Y?|Zs?]`), if `clauseVars[Y]` contains a VarRef pointing to a ValueTag cell (ground value like `'a'`), the code didn't dereference it.

**Fix:** Added dereferencing logic to extract actual ground value from VarRef→ValueTag cells.

**Result:** Output now shows `Ys = [a, _12]` instead of `[_11, _12]`.

### Bug 4: Nested Variable Serialization (FIXED)

**Location:** `lib/multiagent/payload_serializer.dart`, `lib/multiagent/irma_context.dart`

**Problem:** When sending assignments containing nested variables (e.g., `[a | Zs?]`), the serializer wasn't properly allocating heap cells for nested VarRefs on the receiving side.

**Fix:** 
- Updated `_queueAssignmentFromEntry` to call `exportTerm(value)` before serialization per spec Section 4.3
- This registers new local variables in V_p with proper `onBind` callbacks

### Discipline Update: No Deprecated Code (ADDED)

**Location:** `docs/DISCIPLINE.md` - Section 1.8

**Content:** Added rule that deprecated code must be removed immediately. Code marked `@Deprecated` is not allowed; all callers must be updated and the deprecated code deleted.

---

## Current State

### Test Behavior

After the fixes, `bidirectional_stream_test` shows:

1. ✅ Initial message exchange works (element count reaches 2)
2. ✅ Nested variables are allocated correctly (`Var@20`)
3. ❌ Reactivated goals fail instead of suspending on nested imported readers

### Debug Output Pattern

```
[isolate2 -> isolate1] ASSIGNMENT: isolate2:0 := .(Const(b),Var@20)
...
@1: status=ExecutionStatus.failed, GQ=0
```

The goal fails when it should suspend waiting for `Var@20` (an imported reader) to be bound.

---

## Remaining Bug: HeadList/HeadNil Imported Reader Handling

### Root Cause

When a reactivated goal encounters an unbound imported reader (heap cell contains `VariableEntry`), HeadList and HeadNil do not properly detect this case. They should:

1. Recognize `VariableEntry` as an unbound imported reader
2. Add the reader address to suspension set `Si`
3. Soft-fail to next clause

Instead, they either succeed incorrectly or fail outright, causing the goal to fail instead of suspend.

### Location

`lib/bytecode/runner.dart` - HeadList handler (around line 3900-3950) and HeadNil handler

### Fix Required

After dereferencing the argument, add a check:

```dart
// In HeadList/HeadNil handler, after getting the argument:
final arg = env.arg(argSlot);
if (arg is VarRef) {
  final deref = cx.rt.heap.derefAddr(arg.addr);
  
  if (deref is VariableEntry) {
    // Unbound imported reader - add to Si and fail to next clause
    cx.Si.add(arg.addr);
    _softFailToNextClause(cx, pc);
    pc = _findNextClauseTry(pc);
    continue;
  }
  
  // ... rest of handling for bound values
}
```

Apply this pattern to:
- HeadList (matching `[X|Xs]` patterns)
- HeadNil (matching `[]` patterns)
- Potentially other HEAD instructions that access argument values

---

## Deprecated Code to Remove

Per new DISCIPLINE.md Section 1.8, the following deprecated methods in `lib/multiagent/payload_serializer.dart` must be removed:

| Method | Replacement |
|--------|-------------|
| `createAssignmentPayload` | `createAssignmentPayloadV2` |
| `serializeTerm` | `serializeTermWithCallbacks` |
| `_serializeTermRecursiveLegacy` | `_serializeTermRecursiveV2` |
| `deserializeTerm` | `deserializeAgentMessagePayload` |
| `deserializeAgentMessagePayloadWithMappingLegacy` | `deserializeAgentMessagePayloadWithMapping` |
| `deserializeAgentMessagePayloadLegacy` | `deserializeAgentMessagePayload` |
| `_deserializeTermWithMapping` | `_deserializeTermWithMappingV2` |

The test file `test/multiagent/payload_serializer_test.dart` must be rewritten to use V2 methods with proper `isReader` callbacks and heap allocation.

---

## Files Modified

| File | Changes |
|------|---------|
| `lib/bytecode/runner.dart` | PutVariable fix, UnifyVariable dereference fix |
| `lib/runtime/scheduler.dart` | suspendedGoals tracking fix |
| `lib/multiagent/irma_context.dart` | Added `exportTerm()` call before serialization |
| `docs/DISCIPLINE.md` | Added Section 1.8: No Deprecated Code |

---

## Test Status

| Suite | Status |
|-------|--------|
| Dart Unit Tests | ~236 passing (verify after changes) |
| REPL Tests | 222/223 passing |
| bidirectional_stream_test | FAILING (goals fail instead of suspend) |

---

## Next Steps

1. **Fix HeadList/HeadNil** - Add `VariableEntry` check to properly handle unbound imported readers
2. **Remove deprecated serialization methods** - Per DISCIPLINE.md Section 1.8
3. **Rewrite payload_serializer_test.dart** - Use V2 methods with proper callbacks
4. **Run full test suite** - Verify no regressions
5. **Continue bidirectional stream debugging** - After HeadList/HeadNil fix, verify goals suspend correctly

---

## Key Insights

### Why REPL Tests Didn't Catch These Bugs

REPL tests are single-agent. The bugs manifest when:
1. Imported readers from another isolate get bound
2. The bound value (containing nested variables) flows through PutVariable
3. Nested imported variables are encountered during HEAD matching

Without multiagent communication, these paths aren't exercised.

### Spec-Correct Solution for Nested Writers

Per irmaGLP spec Section 4.3, variables are added to V_p **when exported** (sent to another agent), not at allocation time. The fix uses `exportTerm()` before serialization to:
- Scan the term for local variables not yet in V_p
- Add them to V_p with entries
- Register `onBind` callbacks for new writers

This is the spec-correct solution, not hooks in the bytecode runner or registering at allocation time.

---

## Notes for Next Session

1. The HeadList/HeadNil fix is mechanical but requires finding the exact location in the 200KB runner.dart file. Search for "HeadList" opcode handling.

2. The `VariableEntry` type is imported from `multiagent/variable_table.dart`. Ensure it's imported in runner.dart.

3. Test the fix with a minimal case first: a goal that directly matches an imported reader argument, before testing the full bidirectional stream.

4. The test output shows `Var@20` being allocated - verify this address is indeed an imported reader with `VariableEntry` content by adding debug logging.

---

## Document History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2026-01-21 | Initial handover report |
