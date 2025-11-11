# Nested Structure Bug - Final Diagnosis

**Date:** November 10, 2025
**Status:** ROOT CAUSE IDENTIFIED
**Bug Location:** BODY phase structure building - conjunction writer never binds

---

## Executive Summary

**The bug is NOT in HEAD phase matching - it's in BODY phase structure building!**

The conjunction `(bar(X), baz(X?,Y))` is never bound to its writer during BODY phase execution. When the HEAD phase tries to match, the writer is unbound, causing the goal to suspend.

---

## Debug Trace Analysis

### Test Case
```prolog
test_conj(Y?) :- foo((bar(X), baz(X?,Y))).
foo((bar(a), baz(a,b))).
```

### Actual Execution

**Query:** `test_conj(Z)` calls `foo((bar(X), baz(X?,Y)))`

**BODY Phase (First Clause - builds conjunction):**
```
DEBUG: HeadStructure ,/2 at argSlot 0
DEBUG: HeadStructure - got arg from argSlot 0: _ArgInfo
DEBUG: HeadStructure - about to check arg.isWriter, arg=_ArgInfo
DEBUG: HeadStructure - arg is Reader 1003
DEBUG: HeadStructure - Reader 1003 -> Writer 1002
DEBUG: HeadStructure - Writer 1002 is unbound or null, adding to Si and soft failing
```

**Key Finding:** Writer 1002 (which should hold the conjunction structure) is **UNBOUND** when HEAD tries to match!

---

## The Root Cause

### What Should Happen (BODY Phase)

Looking at bytecode for first clause (lines 1-11):
```
1: ClauseTry
2: GetVariable
3: Commit
4: PutStructure(",", 2, 0)           // Start building conjunction at arg0
5: PutStructure("bar", 1, -1)        // Nested bar/1 (parent context saved)
6: SetWriter                         // Add X to bar
7: PutStructure("baz", 2, -1)        // Nested baz/2 (parent context saved)
8: SetReader                         // Add X? to baz
9: SetWriter                         // Add Y to baz
10: Spawn
11: Proceed
```

**The problem:** After instructions 5-9 build the nested structures and complete them:
1. `bar(X)` completes and binds its writer
2. `baz(X?,Y)` completes and binds its writer
3. **Parent completion check** should fire and bind the conjunction writer (1002)
4. But it doesn't! Writer 1002 remains unbound!

### Why Parent Completion Check Fails

Looking at the parent completion check code (runner.dart lines 1607-1660):

```dart
if (cx.parentStructure != null && targetWriterId is int) {
  // ... restore parent context ...

  // Check if parent is now complete
  if (cx.currentStructure is StructTerm) {
    final parentStruct = cx.currentStructure as StructTerm;
    if (cx.S >= parentStruct.args.length && parentWriterId is int) {
      // Bind the parent structure
      cx.rt.heap.bindWriterStruct(parentWriterId as int,
                                   parentStruct.functor,
                                   parentStruct.args);
      // ...
    }
  }
}
```

**The issue:** After restoring parent context and incrementing S, we check `cx.S >= parentStruct.args.length`.

For conjunction with 2 arguments:
- After first nested structure completes: S becomes 1
- After second nested structure completes: S becomes 2
- Check: `2 >= 2` → TRUE, should bind!

**But it's not binding!** This means either:
1. The parent context isn't being restored properly
2. The S value isn't being incremented correctly
3. The `parentWriterId` is null or wrong type

---

## The Actual Bug

The parent completion check happens in **SetWriter** and **SetReader** handlers, but only for structures built with `PutStructure(..., -1)`.

**Looking at bytecode line 4:**
```
PutStructure(",", 2, 0)    // argSlot = 0, NOT -1!
```

The conjunction is built with `argSlot: 0` (target argument), not `-1` (nested structure marker).

**This means:**
- `bar(X)` (line 5) with argSlot `-1` → triggers parent context save ✓
- `baz(X?,Y)` (line 7) with argSlot `-1` → triggers parent context save ✓
- But when they complete, parent context restoration happens
- **Parent is the conjunction with argSlot 0, not -1**
- The parent completion check doesn't recognize it needs to bind!

---

## The Fix

The parent completion check in `SetWriter`/`SetReader` needs to handle the case where the parent structure was created with a **non-negative argSlot** (target argument), not just nested structures with argSlot `-1`.

### Option 1: Track Parent Writer ID Differently

When `PutStructure` is called with argSlot >= 0, it should still set up parent tracking if we're building nested structures inside it.

**Modify runner.dart PutStructure handler** (around line 1537):

```dart
if (op is PutStructure) {
  if (cx.inBody) {
    if (op.argSlot == -1) {
      // Nested structure - save parent context
      cx.parentStructure = cx.currentStructure;
      cx.parentS = cx.S;
      cx.parentMode = cx.mode;
      cx.parentWriterId = cx.clauseVars[-1];
    } else {
      // Top-level structure for argument
      // But if we're already building a structure, this might be nested!
      if (cx.currentStructure != null) {
        // We're nested - save parent context
        cx.parentStructure = cx.currentStructure;
        cx.parentS = cx.S;
        cx.parentMode = cx.mode;
        cx.parentWriterId = cx.clauseVars[-1];
      }
    }

    // Create fresh writer/reader pair for this structure
    // ...
```

### Option 2: Always Check for Completion

After any structure completes (when `cx.S >= struct.args.length`), check if there's a parent structure waiting and complete it if all its arguments are filled.

This is more robust but requires tracking the parent-child relationship more carefully.

---

## Test to Verify Fix

After implementing the fix:

```bash
dart run glp_repl.dart <<'EOF'
test_conj_var.glp
test_conj(Z)
:quit
EOF
```

**Expected output:**
```
Z = b
```

**Currently outputs:**
```
Z = <unbound>
```

---

## Summary

**Bug Location:** BODY phase structure building in `runner.dart`

**Problem:** Conjunction structure at top level (argSlot 0) doesn't get its writer bound when nested structures complete

**Root Cause:** Parent completion check only triggers for structures with argSlot `-1`, not structures at argSlot >= 0 that contain nested structures

**Solution:** Extend parent context tracking to handle top-level structures that contain nested structures

---

## Files Involved

1. **glp_runtime/lib/bytecode/runner.dart**
   - Line ~1537: `PutStructure` handler - needs to save parent context for argSlot >= 0 when already building
   - Line ~1607-1660: `SetWriter` parent completion check - may need adjustment
   - Line ~1722-1776: `SetReader` parent completion check - may need adjustment

2. **glp_runtime/lib/compiler/codegen.dart**
   - Compiler is generating correct bytecode - no changes needed

---

## Related Documents

- `NESTED_STRUCTURE_STATUS.md` - Previous status thinking it was HEAD phase issue
- `NESTED_STRUCTURE_INVESTIGATION_REPORT.md` - Investigation assuming HEAD phase bug
- `FCP_NESTED_STRUCTURE_ANALYSIS.md` - FCP analysis (still valuable for understanding architecture)

---

## Next Steps

1. Implement fix in `PutStructure` handler to recognize when a non-nested structure contains nested children
2. Test with `test_conj(Z)` and `run2(X)`
3. Verify all existing tests still pass
4. Update status documents with successful resolution

---

## Bytecode Reference

**First clause (builds conjunction):**
```
4: PutStructure(",", 2, 0)           ← Creates conjunction, stores writer ID in clauseVars[-1]
5: PutStructure("bar", 1, -1)        ← Saves parent (conjunction) context
6: SetWriter                         ← Completes bar/1, restores parent, S=1
7: PutStructure("baz", 2, -1)        ← Saves parent (conjunction) context
8: SetReader
9: SetWriter                         ← Completes baz/2, restores parent, S=2
                                       Should bind conjunction here but doesn't!
10: Spawn
```

**Second clause (fact with ground conjunction):**
```
16: HeadStructure(",", 2, argSlot: 0)  ← Tries to match, finds writer 1002 unbound
17: UnifyVariable                       ← Would extract if structure was bound
18: UnifyVariable
19: HeadStructure("bar", 1, argSlot: 10)
20: UnifyConstant(a)
21: HeadStructure("baz", 2, argSlot: 11)
22: UnifyConstant(a)
23: UnifyConstant(b)
```

The HEAD phase matching logic is perfect - the bug is that the structure was never built in the first place!
