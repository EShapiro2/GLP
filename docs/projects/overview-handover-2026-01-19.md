# Overview GLP Handover — Warnings to Errors Conversion

**Date**: 2026-01-19  
**Author**: Claude (Overview GLP)  
**Status**: In Progress

---

## Summary

This handover documents the work to eliminate warnings from the GLP codebase. Per discipline section 1.6, there should be no warnings, only errors. Two issues were identified: (1) the type checker uses a `TypeWarning` class that should not exist, and (2) the prelude is missing a procedure declaration for `=/2`, causing type errors when unification appears in body position.

---

## Issues Identified

### Issue 1: TypeWarning Class in Type Checker

**Location**: `glp_runtime/lib/analysis/type_checker/type_checker.dart`

**Problem**: The type checker defines a `TypeWarning` class (lines 65-73) and reports certain conditions as warnings instead of errors:

1. "Procedure X declared but not defined" (line ~155)
2. "Procedure X has no type declaration" (line ~172)

Per discipline section 1.6: "When code does not conform to the spec, use precise language... Do not soften errors with euphemisms." Warnings are a form of softening — if something is wrong enough to report, it should stop compilation.

**Current Code**:
```dart
class TypeWarning {
  final String message;
  final int line;
  final int column;

  TypeWarning(this.message, this.line, this.column);

  @override
  String toString() => '$message at line $line, column $column';
}
```

**Decision Needed**: Should these become errors, or should they be removed entirely? Specifically:

1. "Procedure declared but not defined" — Could be acceptable for builtins. The code already skips builtins (line ~151: `if (!procDecl.isBuiltin)`), so this warning only fires for non-builtins. A declared non-builtin without clauses is likely an error.

2. "Procedure has no type declaration" — In typed GLP, every procedure should have a type declaration. This should be an error.

### Issue 2: Missing `=/2` Procedure Declaration

**Location**: `glp_runtime/lib/analysis/type_checker/prelude.dart`

**Problem**: The prelude contains the clause `X? = X.` but has no procedure declaration for `=/2`. A misleading comment states these predicates don't need declarations because they're unfolded at compile time. This is only partially correct — when `=/2` appears in body position (after `|`), it is NOT unfolded and the type checker reports "Undefined procedure: =/2".

**Current Prelude Comment** (incorrect):
```dart
// The following are defined guards (unit clauses) - no procedure declarations needed.
// They are unfolded at compile time by partial evaluation.
```

**Specification**: `docs/type system/prelude-stdlib-consolidation.md` already documents this fix but it was not applied. The fix is to add:

```glp
procedure =(_?, _).
```

### Issue 3: 3-Tuple Syntax (Mentioned but Not Yet Investigated)

The user mentioned that 3-tuple syntax needs to be nested. This requires investigation to understand the current behavior and what the correct behavior should be.

---

## Proposed Changes

### Change 1: Convert TypeWarning to TypeError

Remove the `TypeWarning` class and convert both warning cases to errors:

1. Remove `TypeWarning` class definition
2. Remove `warnings` field from `TypeCheckResult`
3. Change "Procedure declared but not defined" from warning to error (for non-builtins)
4. Change "Procedure has no type declaration" from warning to error

### Change 2: Add `=/2` Procedure Declaration

In `prelude.dart`, add to the PROCEDURE DECLARATIONS section:

```glp
% Unification (can appear in body position after partial evaluation)
procedure =(_?, _).
```

Update the misleading comment to:

```dart
// The following predicates have clauses below. They are unfolded when used in
// guard position (before |) but execute as normal goals in body position (after |).
// Procedure declarations are provided so body uses can be type-checked.
```

---

## Current State

### Test Status

Not yet run — changes not yet implemented.

### Known Issues

1. `TypeWarning` class exists and is used in type checker
2. `=/2` has no procedure declaration in prelude
3. 3-tuple syntax issue not yet investigated

---

## Next Steps

1. **Discuss**: Confirm the proposed changes with Udi before implementation
2. **Implement**: Convert warnings to errors in type_checker.dart
3. **Implement**: Add `=/2` procedure declaration to prelude.dart
4. **Investigate**: Understand the 3-tuple syntax issue
5. **Test**: Run baseline tests after each change
6. **Commit**: Commit after each successful change

---

## Files to Modify

| File | Change |
|------|--------|
| `glp_runtime/lib/analysis/type_checker/type_checker.dart` | Remove TypeWarning, convert to errors |
| `glp_runtime/lib/analysis/type_checker/prelude.dart` | Add `procedure =(_?, _).` |

---

## Notes for Next Session

The consolidation spec (`docs/type system/prelude-stdlib-consolidation.md`) was marked as "COMPLETED" but the `=/2` fix was not actually applied to prelude.dart. This is a discrepancy that should be resolved.

The discipline document (section 1.6) is clear: no warnings, only errors. However, the specific semantics of "procedure declared but not defined" need discussion — is this always an error, or are there legitimate cases where a procedure might be declared without clauses?
