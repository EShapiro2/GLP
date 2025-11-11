# Nested Structure Fix Plan - Final Solution

**Date:** November 10, 2025
**Root Cause Identified:** `UnifyConstant` doesn't handle nested `StructTerm` values in READ mode

---

## The Problem (Confirmed by Claude Web + Code Analysis)

### Test Case
```prolog
test_conj(Y?) :- foo((bar(X), baz(X?,Y))).
foo((bar(a), baz(a,b))).
```

### What Happens

1. **Compilation of `foo((bar(a), baz(a,b)))`:**
   - Compiler sees fully ground nested structure
   - Two-pass approach (codegen.dart lines 220-261) processes it
   - But when it hits the innermost constants...
   - **Problem:** Nested ground `StructTerm`s might be emitted as `UnifyConstant(StructTerm(...))`

2. **Runtime Execution:**
   - Goal `foo((bar(X), baz(X?,Y)))` builds the structure with variables
   - Tries to match against fact
   - `UnifyConstant` receives a `StructTerm` as `op.value`
   - **Runtime bug:** `UnifyConstant` READ mode (runner.dart lines 836-894) doesn't handle `StructTerm` values!
   - It only handles: `ConstTerm`, `WriterTerm`, `ReaderTerm`
   - Falls through to "Mismatch - soft fail" at line 890

---

## The Solution: Two-Part Fix

### Part 1: Runtime - Handle StructTerm in UnifyConstant

Add to runner.dart line 889 (before the final "Mismatch" case):

```dart
} else if (value is StructTerm && op.value is StructTerm) {
  // Nested structure matching - need to recursively unify
  final incomingStruct = value as StructTerm;
  final patternStruct = op.value as StructTerm;

  if (incomingStruct.functor == patternStruct.functor &&
      incomingStruct.args.length == patternStruct.args.length) {
    // Structures match - need to recursively unify arguments
    // Save current state
    final savedStruct = cx.currentStructure;
    final savedS = cx.S;

    // Set up for matching pattern structure
    cx.currentStructure = incomingStruct;
    cx.S = 0;

    // Match each argument of pattern against incoming
    bool allMatch = true;
    for (int i = 0; i < patternStruct.args.length; i++) {
      final patternArg = patternStruct.args[i];
      final incomingArg = incomingStruct.args[i];

      if (patternArg is ConstTerm) {
        // Pattern expects constant
        if (incomingArg is ConstTerm) {
          if (incomingArg.value != patternArg.value) {
            allMatch = false;
            break;
          }
        } else if (incomingArg is WriterTerm) {
          // Bind writer to constant
          cx.sigmaHat[incomingArg.writerId] = patternArg;
        } else if (incomingArg is ReaderTerm) {
          // Check if bound
          final wid = cx.rt.heap.writerIdForReader(incomingArg.readerId);
          if (wid != null && cx.rt.heap.isWriterBound(wid)) {
            final boundValue = cx.rt.heap.valueOfWriter(wid);
            if (boundValue is ConstTerm && boundValue.value != patternArg.value) {
              allMatch = false;
              break;
            }
          } else {
            // Suspend on unbound reader
            cx.si.add(incomingArg.readerId);
          }
        } else {
          allMatch = false;
          break;
        }
      } else if (patternArg is StructTerm) {
        // Recursively match nested structure (would need deeper recursion)
        // For now, fail - this case shouldn't happen in practice
        allMatch = false;
        break;
      }
    }

    // Restore state
    cx.currentStructure = savedStruct;
    cx.S = savedS;

    if (allMatch) {
      cx.S++; // Advance past this nested structure
    } else {
      _softFailToNextClause(cx, pc);
      pc = _findNextClauseTry(pc);
      continue;
    }
  } else {
    // Functor/arity mismatch
    _softFailToNextClause(cx, pc);
    pc = _findNextClauseTry(pc);
    continue;
  }
}
```

### Part 2: Better Approach - Don't Emit StructTerm Constants in HEAD

**The cleaner fix:** Modify the compiler to NEVER emit `UnifyConstant(StructTerm(...))` in HEAD phase.

Instead, always use the two-pass destructuring approach:

**Modify codegen.dart `_generateArgumentStructureElement()` around line 631:**

```dart
} else if (term is StructTerm) {
  // NEVER emit nested structures as constants in HEAD
  // Always use two-pass destructuring approach

  // This is tricky - we're inside a parent structure's arg list
  // We need to defer this to the caller
  // Actually, this case should never be hit if _generateHeadArg handles it correctly
  throw CompileError(
    'Internal error: nested StructTerm in _generateArgumentStructureElement. '
    'Should be handled by _generateHeadArg two-pass approach.',
    term.line, term.column, phase: 'codegen'
  );
}
```

**The real issue:** The two-pass approach in `_generateHeadArg` (lines 220-261) **should** already handle this. Let me check if there's a path where nested StructTerms slip through as constants...

---

## Investigation: Where Do StructTerm Constants Come From?

Looking at the code:

1. **_generateHeadArg (lines 197-267):**
   - Line 220: `if (term is StructTerm)` → Two-pass approach
   - Line 233-237: Nested `StructTerm` → Extract to temp, defer processing
   - Line 254-260: Process deferred nested `StructTerm`

2. **_generateStructureElement (lines 269-311):**
   - Called for each argument during structure matching
   - Line 290: `if (term is StructTerm)` → **Calls `_generateArgumentStructureElement`!**

3. **_generateArgumentStructureElement (lines 585-644):**
   - Line 631: `if (term is StructTerm)` → Emits `PutStructure(..., -1)` for BODY
   - **This is for BODY phase structure building, not HEAD matching!**

**Ah!** I found it. In `_generateStructureElement` line 290, when `inHead: true`, it calls:

```dart
_generateArgumentStructureElement(term, varTable, ctx);
```

But `_generateArgumentStructureElement` is designed for BODY phase (building structures), not HEAD phase (matching structures)!

---

## The Actual Bug

In **`_generateStructureElement`** at line 290:

```dart
void _generateStructureElement(Term term, VariableTable varTable, CodeGenContext ctx, {required bool inHead}) {
  // ...
  } else if (term is StructTerm) {
    // Nested structure as argument
    _generateArgumentStructureElement(term, varTable, ctx);  // WRONG!
  }
}
```

When `inHead: true`, this calls `_generateArgumentStructureElement`, which at line 631-639 does:

```dart
} else if (term is StructTerm) {
  // Nested structure in BODY - need to handle both ground and non-ground
  ctx.emit(bc.PutStructure(term.functor, term.arity, -1));
  for (final arg in term.args) {
    _generateStructureElementInBody(arg, varTable, ctx);
  }
}
```

This emits `PutStructure` which is for **BODY phase structure building**, not HEAD phase matching!

---

## The Fix (Correct)

### Fix: _generateStructureElement Should Handle HEAD Differently

Modify **codegen.dart line 290** to not call `_generateArgumentStructureElement` when in HEAD:

```dart
} else if (term is StructTerm) {
  if (inHead) {
    // In HEAD: nested structures should NOT reach here
    // They should be handled by _generateHeadArg's two-pass approach
    // If we get here, it means we're trying to match a nested structure
    // as part of an already-extracted parent structure

    // This is the case where we have something like:
    // foo(p(q(X))) and we've already extracted p(...) to a temp
    // Now we're processing the arguments of p, and q(X) is one of them

    // We need to recursively apply the two-pass approach
    // Extract nested structure to temp
    final tempReg = ctx.allocateTemp();
    ctx.emit(bcv2.UnifyVariable(tempReg, isReader: false));

    // Then process it with HeadStructure
    ctx.emit(bc.HeadStructure(term.functor, term.arity, tempReg));
    for (final arg in term.args) {
      _generateStructureElement(arg, varTable, ctx, inHead: true);
    }
  } else {
    // In BODY: use structure building approach
    _generateArgumentStructureElement(term, varTable, ctx);
  }
}
```

---

## Summary

**Root cause:** `_generateStructureElement` calls `_generateArgumentStructureElement` (BODY-phase logic) even when `inHead: true`.

**Fix:** When `inHead: true` and encountering nested `StructTerm`:
1. Extract to temp with `UnifyVariable`
2. Process with `HeadStructure`
3. Recursively process its arguments

This ensures nested structures in HEAD are always **destructured and matched**, never emitted as constants.

---

## Files to Modify

1. **glp_runtime/lib/compiler/codegen.dart** (line 290)
   - Add `if (inHead)` branch for nested StructTerm
   - Extract to temp + HeadStructure + recursive processing

---

## Testing

After fix, these should work:
```
test_conj(Z) → Z = b   ✅
run2(X)      → X = []  ✅
```

---

## Alternative: Runtime Fix

If we want to also handle `StructTerm` constants in the runtime (defensive programming), add the handler at runner.dart line 889 as shown in Part 1.

But the **correct fix** is at the compiler level - don't generate code that requires runtime structure matching via constants.
