# Handover Report: Extra UnifyVariable Bug for Nil Lists
**Date:** 2025-11-17
**Session:** Bug 2 - Extra UnifyVariable before HeadNil
**For:** Claude Web (new session)

---

## Executive Summary

**Bug:** `run(quicksort([],X))` returns X as unbound instead of binding it to `[]`

**Status:** Bug 1 (variable occurrence tracking) is FIXED and committed. Bug 2 discovered: compiler generates extra `UnifyVariable` instruction before `HeadNil` for empty lists in HEAD structures.

**Root Cause:** Lines 341-351 in `/Users/udi/GLP/glp_runtime/lib/compiler/codegen.dart`

**Your Task:** Determine correct fix for nil list handling in `_generateStructureElement()` for HEAD mode.

---

## The Bug

### Test Case
**Source:** `/Users/udi/GLP/udi/glp/qsort.glp` line 32:
```prolog
clause(quicksort(Unsorted, Sorted?), qsort(Unsorted?, Sorted, [])).
```

**Query:** `run(quicksort([],X))`

**Expected:** `X = []`

**Actual:** `X = <unbound>` (pattern match fails)

### Bytecode Analysis

**Expected bytecode** for `clause(quicksort(U, S?), qsort(U?, S, []))`:
```
PC 127: ClauseTry
PC 128: HeadStructure(quicksort, 2, A0)
PC 129: UnifyVariable(X0, writer)        # U (first)
PC 130: UnifyVariable(X1, reader)        # S? (first)
PC 131: HeadStructure(qsort, 3, A1)
PC 132: UnifyReader(X0)                  # U? (subsequent) ✓
PC 133: UnifyWriter(X1)                  # S (subsequent) ✓
PC 134: (should be UnifyConstant('nil') or nothing)
PC 135: (Commit follows)
```

**Actual bytecode** (from `dart dump_bytecode.dart glp/qsort.glp`):
```
PC 127: ClauseTry
PC 128: HeadStructure(quicksort, 2, A0)
PC 129: UnifyVariable(X0, writer)
PC 130: UnifyVariable(X1, reader)
PC 131: HeadStructure(qsort, 3, A1)
PC 132: UnifyReader(X0)                  # ✓ Bug 1 fixed
PC 133: UnifyWriter(X1)                  # ✓ Bug 1 fixed
PC 134: UnifyVariable(X10, writer)       # ← EXTRA! BUG!
PC 135: HeadNil(X10)                     # ← Should be earlier?
PC 136: Commit
```

**Problem:** The extra `UnifyVariable(X10, writer)` at PC 134 extracts a value to temp register X10, then HeadNil matches X10 against nil. This creates a 4th Unify* instruction where only 3 should exist (for U?, S, and []).

---

## Root Cause

### File: `/Users/udi/GLP/glp_runtime/lib/compiler/codegen.dart`
**Lines 341-372**: `_generateStructureElement()` - ListTerm case

```dart
} else if (term is ListTerm) {
  // Nested list: need to extract into temp, then match
  final tempReg = ctx.allocateTemp();  // ← Line 343: ALWAYS allocates temp

  if (inHead) {
    // READ mode: extract value at S into temp
    ctx.emit(bcv2.UnifyVariable(tempReg, isReader: false));  // ← Line 347: EXTRA!

    // Then match temp against nested structure
    if (term.isNil) {
      ctx.emit(bc.HeadNil(tempReg));  // ← Line 351: Then HeadNil
    } else {
      ctx.emit(bc.HeadStructure('.', 2, tempReg));
      if (term.head != null) _generateStructureElement(term.head!, varTable, ctx, inHead: inHead);
      if (term.tail != null) _generateStructureElement(term.tail!, varTable, ctx, inHead: inHead);
    }
  } else {
    // WRITE mode (BODY): building nested structure within argument structure
    if (term.isNil) {
      ctx.emit(bc.UnifyConstant('nil'));  // ← Line 362: BODY does this correctly!
    } else {
      // For non-empty list, we need nested structure building
      ctx.emit(bc.PutStructure('.', 2, tempReg));
      if (term.head != null) _generateStructureElement(term.head!, varTable, ctx, inHead: inHead);
      if (term.tail != null) _generateStructureElement(term.tail!, varTable, ctx, inHead: inHead);
      ctx.emit(bcv2.UnifyVariable(tempReg, isReader: false));
    }
  }
```

**Issue:** For nil lists, the code:
1. Allocates temp register (line 343) - unnecessary
2. Emits `UnifyVariable(tempReg, writer)` to extract to temp (line 347) - EXTRA instruction!
3. Emits `HeadNil(tempReg)` to match temp against nil (line 351)

**Note:** BODY mode (line 362) correctly emits just `UnifyConstant('nil')` for nil lists.

---

## Questions for Claude Web

### 1. What is the correct bytecode for nil in HEAD structures?

**Option A:** Just `UnifyConstant('nil')` (same as BODY mode)
```dart
if (term.isNil) {
  ctx.emit(bc.UnifyConstant('nil'));
}
```

**Option B:** Extract to temp then HeadNil (current behavior)
```dart
final tempReg = ctx.allocateTemp();
ctx.emit(bcv2.UnifyVariable(tempReg, isReader: false));
ctx.emit(bc.HeadNil(tempReg));
```

**Option C:** Just `HeadNil` without temp extraction
```dart
if (term.isNil) {
  ctx.emit(bc.HeadNil(argSlot));  // But what is argSlot here?
}
```

### 2. Does HeadNil need a register parameter?

Looking at `/Users/udi/GLP/glp_runtime/lib/bytecode/opcodes.dart`:
```dart
class HeadNil extends Opcode {
  final int argSlot;
  HeadNil(this.argSlot);
  // ...
}
```

Yes, `HeadNil` requires an `argSlot` parameter. But what should it be when nil appears as a nested structure element (not a top-level HEAD argument)?

### 3. Is extract-then-match required for ALL nested structures?

**Context:** Non-empty lists `[H|T]` and nested structures both use extract-then-match:
```dart
final tempReg = ctx.allocateTemp();
ctx.emit(bcv2.UnifyVariable(tempReg, isReader: false));  // Extract
ctx.emit(bc.HeadStructure('.', 2, tempReg));             // Match
```

**Question:** Does nil need the same pattern, or is it special because it's atomic (like a constant)?

### 4. What does WAM/FCP do for nil in structure arguments?

**Request:** Check WAM paper and FCP implementation for how they handle:
```prolog
p(f(a, [], b)).  % nil as 2nd argument of structure f/3
```

Do they:
- Emit a constant instruction?
- Emit a special nil instruction?
- Treat it like any other atomic value?

---

## Context: What Works

### Bug 1 Fix (COMMITTED) ✅

**File:** Same file, lines 315-335
**Problem:** Variables appearing in multiple nested structures weren't tracked correctly
**Fix:** Track first vs subsequent occurrences in `ctx.seenHeadVars`

**Verification:** PC 132-133 now correctly show:
```
PC 132: UnifyReader(X0)   # U? (subsequent occurrence)
PC 133: UnifyWriter(X1)   # S (subsequent occurrence)
```

This fix is working and committed. No regressions.

### Current Test Status

**Baseline:**
- Unit tests: 25/58 passing
- REPL tests: 24/27 passing

**After Bug 1 fix:**
- Unit tests: 25/58 passing (no regression)
- REPL tests: 24/27 passing (no regression)
- `run(quicksort([],X))` STILL FAILS (Bug 2 remains)

---

## Files to Read

### 1. Specifications (MANDATORY)

**`/Users/udi/GLP/docs/glp-bytecode-v216-complete.md`**
- Section 6.3: HeadNil instruction semantics
- Section 6: All HEAD phase instructions
- Section 12: Mode-aware opcodes

**`/Users/udi/GLP/docs/glp-runtime-spec.txt`**
- HeadNil execution semantics
- How runtime handles nil vs constants

### 2. Current Code

**`/Users/udi/GLP/glp_runtime/lib/compiler/codegen.dart`**
- Lines 341-372: The buggy ListTerm case (shown above)
- Lines 297-372: Full `_generateStructureElement()` function
- Lines 201-295: `_generateHeadArgument()` for comparison

**`/Users/udi/GLP/glp_runtime/lib/bytecode/opcodes.dart`**
- HeadNil class definition
- UnifyConstant class definition

**`/Users/udi/GLP/glp_runtime/lib/bytecode/runner.dart`**
- HeadNil execution handler
- UnifyConstant execution handler in HEAD mode

### 3. Reference Implementations

**WAM Paper:** `/Users/udi/GLP/docs/wam.pdf`
**FCP Code:** `/Users/udi/Dropbox/Concurrent Prolog/FCP/Savannah`
**FCP Paper:** `/Users/udi/GLP/docs/1-s2.0-0743106689900113-main.pdf`

---

## What NOT to Do

1. **Don't suggest extract-then-match for main HEAD atom** - Previous session confirmed this is wrong
2. **Don't change Bug 1 fix** - It's working and committed
3. **Don't suggest algorithmic changes** - Follow WAM/FCP patterns exactly
4. **Don't remove code without understanding** - Current structure may be intentional

---

## Expected Deliverable

**Concrete answer to:** What should `_generateStructureElement()` emit for nil lists in HEAD mode?

**Include:**
1. **Exact code change** - Which lines to modify, what to emit
2. **Why it's correct** - Reference to spec sections or WAM/FCP behavior
3. **Expected bytecode** - What PC 134-136 should show after fix
4. **Any caveats** - Edge cases or related issues to watch

**Format:**
```
CHANGE: File lib/compiler/codegen.dart, lines 341-372
LOGIC: For term.isNil in HEAD mode, emit [specific instruction]
REASON: [spec reference or WAM/FCP pattern]
EXPECTED BYTECODE:
  PC 132: UnifyReader(X0)
  PC 133: UnifyWriter(X1)
  PC 134: [what should be here]
  PC 135: Commit
```

---

## Summary

The compiler generates an extra `UnifyVariable` instruction before `HeadNil` for empty lists in HEAD structures. Need to determine whether nil should:
- Emit `UnifyConstant('nil')` like BODY mode
- Emit `HeadNil` without temp extraction
- Keep extract-then-match pattern (but then why does it fail?)

Check WAM/FCP for nil handling in structure arguments, verify spec semantics for HeadNil vs UnifyConstant in HEAD mode, and provide specific fix instructions.
