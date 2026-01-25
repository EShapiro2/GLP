# Typechecker REPL Test Diagnostic Report

**Date**: 2026-01-23  
**Baseline**: 139/222 passing (83 failures)  
**Source**: Claude Code diagnostic analysis

---

## 1. Failure Breakdown by Error Type

| Error Type | Count | Description |
|------------|-------|-------------|
| SRSW violations | ~35 | Parser rejects before type checking (loading error) |
| Type errors (mode mismatch) | ~30 | Writer/reader mode conflicts |
| Uncovered alternatives | ~5 | Missing type coverage |
| Unknown failure | 3 | File not found or other |
| File not found | 1 | positive/paper/merge.glp missing |

**Total**: 83 failures out of 222 tests

---

## 2. Failure Categories

### Category A: SRSW Violations (Loading Errors) - ~35 tests

Programs violate Single-Reader/Single-Writer constraint. Parser rejects them before type checking runs.

**Common patterns:**
- Writer variable occurs 2+ times
- Variable has no reader
- Reader variable occurs 2+ times without ground guard

### Category B: Mode Mismatch Type Errors - ~30 tests

Type checker detects writer↑/reader↓ conflicts.

**Pattern:** writer requires ↑ (produce), got ↓ (consume) or vice versa

Often in body atoms with `=` unification or stream operations.

### Category C: Coverage Gaps - ~5 tests

Type checker detects uncovered alternatives.

**Pattern:** uncovered alternative "[]" at path: Stream → []

---

## 3. Sample Failing Tests

### Sample 1: SRSW Violation (factorial.glp)

```
Error: SRSW violations found:
  • reduce/2: Line 20: Variable "T" has no reader (guard occurrences only count if grounded)
```

### Sample 2: Mode Mismatch (agent.glp)

```
✗ Body atom 3 (social_graph) is not well-typed:
  Variable mode mismatch: writer requires ↑ (produce), got ↓ (consume)
  Path: ([|]/2, 0, input) → (,/2, 1, input) → (UserOut, 2, input)
```

### Sample 3: Mode Mismatch with = (certainty_meta.glp)

```
✗ Body atom 3 (=) is not well-typed:
  Variable mode mismatch: writer requires ↑ (produce), got ↓ (consume)
  Path: (Z, 0, input)
  Variable mode mismatch: reader requires ↓ (consume), got ↑ (produce)
  Path: (X?, 0, output)
```

### Sample 4: Coverage Gap (bounded_buffer.glp)

```
✗ receive_unbounded argument 2: uncovered alternative "[]" at path: Stream → []
✗ close argument 1: uncovered alternative "[]" at path: Stream → []
```

### Sample 5: Complex SRSW (channels.glp)

```
• read/3: Line 27: Writer variable "Left2" occurs 2 times
• serialize/2: Line 55: Reader variable "Channel2?" occurs 2 times without ground guard
• subset/2: Line 67: Variable "Message" has no writer (must have exactly one)
```

---

## 4. Test Script Format

From `run_typechecker_repl_tests.sh`:

```bash
POSITIVE_FILES=(
    "$TEST_DIR/positive/merge_basic.glp"
    "$MODED_DIR/valid/append.glp"
    "$BOOK_DIR/recursive/arithmetic_trees/factorial.glp"
    ...
)
```

Tests are organized as:
- **Positive tests**: Should load successfully (183 files)
- **Negative tests**: Should be rejected by type checker (37 files)
- **SRSW tests**: Should be rejected by parser (2 files)

A test passes if:
- **Positive**: Loads without "Error loading" or "Type errors"
- **Negative**: Produces "Error" or "Type errors" (expected rejection)

---

## 5. Root Cause Analysis

Most failures fall into two categories:

### Program Issues (~60%)

The typed book programs contain SRSW violations or mode errors that need fixing in the .glp source files.

### Type Checker Limitations (~40%)

The type checker may have:
- Overly strict mode checking for `=` unification
- Missing support for certain type constructs (e.g., Any type in body positions)
- Path analysis issues with complex nested structures

---

## 6. Remediation Categories

Based on the analysis, fixes fall into these work streams:

| Category | Failures | Fix Location |
|----------|----------|--------------|
| A: SRSW violations | ~35 | .glp program files |
| B: Mode mismatch | ~30 | Type checker OR .glp files |
| C: Coverage gaps | ~5 | Type definitions OR coverage checker |
| D: Missing files | 1 | Create missing test file |
| E: Unknown | 3 | Requires investigation |

**Total**: 83 failures to fix
