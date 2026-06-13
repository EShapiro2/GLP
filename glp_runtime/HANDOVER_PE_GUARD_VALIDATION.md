# Handover: Partial Evaluator Guard Validation

**Date**: 2026-01-27  
**From**: Claude Web  
**To**: Claude Code  
**Status**: Tests created, need verification

---

## Summary

Added validation to the partial evaluator that rejects non-unit-clause procedures in guard position. The spec now clearly defines that guards must be either:

1. **Builtin guards** — implemented in Dart runtime with NO GLP clauses (e.g., `integer/1`, `ground/1`, `</2`)
2. **Single-unit-clause procedures** — exactly one clause, no guards, no body

Any other user-defined procedure called in guard position is a compile-time error.

---

## Files Modified

### Implementation
- `lib/compiler/partial_evaluator.dart`
  - Added import of `builtinProcedures` from prelude
  - Added `_collectAllProcedures()` method
  - Updated `_transformClause()` to validate guards against builtins and all procedures
  - Throws `CompileError` for non-unit-clause procedures in guard position

### Tests
- `test/compiler/partial_evaluator_test.dart` (NEW)
  - Tests for accepting single-unit-clause procedures
  - Tests for accepting various builtin guards
  - Tests for rejecting multi-clause procedures
  - Tests for rejecting procedures with body
  - Tests for rejecting procedures with guards
  - Tests for error message content

### Documentation
- `docs/guards-reference.md`
  - Added new section "What Can Appear in Guard Position"
  - Documents guard classification and validation rule
  - Includes error example

- `docs/typed-glp-manual.md`
  - Section 8 documents single-unit-clause procedures

---

## Verification Tasks

1. **Run tests with verbose output**:
   ```bash
   cd /Users/udi/Grassroots/GLP/glp_runtime
   dart test test/compiler/partial_evaluator_test.dart -r expanded
   ```

2. **If tests fail**, debug the partial evaluator logic in `_transformClause()`:
   - Check that `builtinProcedures` contains the correct keys (format: `name/arity`)
   - Check that `allProcedures` is correctly populated
   - Verify the error is thrown at the right condition

3. **Run full test suite** to ensure no regressions:
   ```bash
   dart test
   ```

4. **Test with actual GLP program** that uses guards:
   ```bash
   echo "../programs/book/social_graph/play_alice_bob_carol.glp" | dart run bin/glp_repl.dart 2>&1 | head -20
   ```

---

## Key Code Location

The validation logic is in `_transformClause()` around line 455:

```dart
} else {
  // Not a unit clause - check if it's a builtin or an error
  if (builtinProcedures.contains(key)) {
    // Builtin guard (like integer/1, ground/1) - keep it
    remainingGuards.add(guard);
  } else if (allProcedures.contains(key)) {
    // Procedure exists but is NOT a single unit clause
    throw CompileError(
      'Cannot call "${guard.predicate}/${guard.args.length}" in guard position.\n'
      '  Only builtin guards and single-unit-clause procedures can appear in guards.\n'
      '  The procedure "${guard.predicate}" has multiple clauses or non-unit clauses.',
      guard.line,
      guard.column,
      phase: 'partial_evaluator'
    );
  } else {
    // Unknown guard - let later phases handle it
    remainingGuards.add(guard);
  }
}
```

---

## Spec References

- `docs/guards-reference.md` — "What Can Appear in Guard Position" section
- `docs/typed-glp-manual.md` — Section 8 "Single-Unit-Clause Procedures"
- `lib/analysis/type_checker/prelude.dart` — `builtinProcedures` set defines all builtins
