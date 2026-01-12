# Type Checker Bug Fix Plan

**Version**: 1.0  
**Date**: 2025-01-12  
**Status**: Ready for implementation

## Summary of Findings

After careful analysis of the specs and test results:

### Spec Truth

1. **SRSW is checked by the parser** (compile-time), NOT the type checker
   - See `parser-spec.md`, `SPEC_GUIDE.md`, `well-typed-clause.md` v0.8
   - Type checker **assumes** all clauses satisfy SRSW

2. **Mode auto-correction is correct** per Definition 4.8 Step 2
   - `_ensureVariablesMatchModes()` transforms variables to match structural modes
   - This is a transformation, NOT an error check

3. **Complementarity only checks paired variables** per well-typed-clause.md
   - Only verifies types are complementary when BOTH X and X? appear
   - Does NOT flag unpaired variables (SRSW guarantees pairs exist)

### Mode Semantics (Correct)

| Position Type | Structural Mode | Expected Variable Form |
|---------------|-----------------|------------------------|
| Output (Type) | ↑ produce | Writer X |
| Input (Type?) | ↓ consume | Reader X? |

## Test Classification Issue

The tests `reader_at_input.glp` and `writer_at_output.glp` are misclassified:

| Test | Actual Issue | Who Should Reject |
|------|--------------|-------------------|
| `reader_at_input.glp` | SRSW violation (Y? without Y) | **Parser** |
| `writer_at_output.glp` | SRSW violation (X without X?) | **Parser** |

These are NOT type checker tests - they're SRSW tests that should fail at parsing.

The test comments are wrong - they say "mode-incorrect but SRSW-compliant" when it's actually "mode-correct but SRSW-violating".

## Current Behavior Analysis

From test results:
- `writer_at_output.glp` → rejected (shows "Type errors")
- `reader_at_input.glp` → NOT rejected

This asymmetry suggests the type checker is doing something unexpected.

## Investigation Results (2025-01-12)

### SRSW Checking VERIFIED

Parser correctly rejects SRSW violations:

```
=== reader_at_input.glp ===
Error loading: SRSW violations found:
  • bar/1: Line 11: Variable "Y" has no writer (must have exactly one)

=== writer_at_output.glp ===  
Error loading: SRSW violations found:
  • foo/1: Line 11: Variable "X" has no reader
```

**Both files are rejected BEFORE type checking.**

### Test Script Bug

The test script `run_typechecker_repl_tests.sh` only looks for `"Type errors"`:

```bash
if echo "$output" | grep -q "Type errors"; then
    echo "PASS: $name (correctly rejected)"
```

But SRSW violations show as `"SRSW violations found"`, so the script incorrectly reports these as failures.

## Action Plan

### Immediate: Fix Test Script

Update the negative test detection to recognize both rejection types:
- "Type errors" → type checker rejection
- "SRSW violations" → parser rejection
- "Error loading" → general rejection

### Next: Focus on Genuine Type Checker Tests

Tests that pass SRSW but should fail type checking:
- `double_involution_error.glp` - nested mode error
- Coverage tests in subdirectories
- Other mode error tests

### Reclassify SRSW Tests

These are NOT type checker tests:
- `reader_at_input.glp` 
- `writer_at_output.glp`

They test SRSW enforcement (parser), not type checking.

## Files Updated

| File | Change |
|------|--------|
| `well-typed-clause.md` v0.8 | Added SRSW precondition section |
| `SPEC_GUIDE.md` | Added note SRSW checked by parser before type checking |
| `parser-spec.md` v1.1 | Added SRSW checking section |

## Next Steps

Execute Step 1 above to determine which hypothesis is correct, then proceed accordingly.

## Test Misclassification Findings (2025-01-12)

### Double Involution Tests - Both VALID

Tested both `double_involution_error.glp` and `double_involution.glp`:
- Both pass type checking ✓
- This is **correct behavior per spec**

**Mode trace for `foo(outer(inner(X?)))`:**
1. `foo(Outer?)` → arg mode ↓ (input)
2. Inside `Outer?`, `outer(Inner?)` → combineMode(↓, ↓) = ↑
3. Inside `Inner`, `inner(Number?)` → combineMode(↑, ↓) = ↓
4. At X? position: mode ↓, type Number?

Per spec: mode ↓ expects **reader**. X? IS reader → **CORRECT**

### Test Comment Errors

Many test comments have **inverted mode semantics**:
- Comments say: "input position expects WRITER"
- Spec says: Input (↓) expects **READER**

**Correct semantics:**
| Position Type | Mode | Expected Variable |
|---------------|------|-------------------|
| Input (Type?) | ↓ consume | Reader X? |
| Output (Type) | ↑ produce | Writer X |

### Auto-Correction Behavior

Per Definition 4.8, `modedHead()` transforms variables to match structural modes:
- Writer X at mode ↓ → flipped to reader X?
- Reader X? at mode ↑ → flipped to writer X

This means programs with "wrong" variable forms still pass type checking after transformation.
This is **correct per spec**, not a bug.

### Tests to Reclassify

| Test | Current Location | Should Be | Reason |
|------|------------------|-----------|--------|
| `double_involution_error.glp` | invalid/ | **valid/** | X? at mode ↓ is correct |
| `reader_at_input.glp` | invalid/ | Keep (but for SRSW) | Rejected by parser, not type checker |
| `writer_at_output.glp` | invalid/ | Keep (but for SRSW) | Rejected by parser, not type checker |

## Action Items

1. ✅ Fix test script to recognize SRSW rejections (instructions given to other Claude)
2. **Fix test comments** - other Claude is handling this
3. **Reclassify tests** - move `double_involution_error.glp` to valid/
4. **Find genuine type checker bugs** - tests that have actual type errors but pass
