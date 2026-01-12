# Type Checker Testing Infrastructure Plan

**Version**: 0.3  
**Date**: 2026-01-12  
**Status**: INFRASTRUCTURE COMPLETE

## Goal

Integrate type checking into the REPL and create a comprehensive test suite using the existing REPL test infrastructure.

## Design Decisions

1. **Typed vs Untyped Programs**: Programs without `procedure` declarations skip type checking (vacuously well-typed)
2. **Test Format**: Pure .glp files tested via REPL, same as existing runtime tests
3. **Infrastructure**: Shell script, not separate Dart test framework

## Completed Work

### Phase 1: REPL with Type Checking ✓

**Created:** `bin/glp_repl_typed.dart`

- Copy of `glp_repl.dart` with type checker integration
- In `loadProgram()`: if `module.procDeclarations.isNotEmpty`, calls `checkModule()`
- Rejects files with error messages if ill-typed
- Shows "Type check passed" message for well-typed programs

### Phase 2: Test Programs ✓

**Typechecker directory:** `test/programs/typechecker/`

| Category | Count | Status |
|----------|-------|--------|
| positive/ | 10 | Fixed and working |
| negative/coverage/ | 3 | Tests created |
| negative/head/ | 4 | Tests created |
| negative/body/ | 2 | Tests created |
| negative/complementarity/ | 2 | Tests created |
| negative/type_def/ | 1 | Tests created |
| negative/ (top level) | 7 | Tests created |

**Moded_types directory:** `test/programs/moded_types/`

| Category | Count | Status |
|----------|-------|--------|
| valid/ | 4 | Existing tests |
| valid/embedded/ | 6 | Existing tests |
| valid/universal/ | 4 | Existing tests |
| invalid/ | 4 | Existing tests |
| invalid/embedded/ | 4 | Existing tests |
| invalid/deep/ | 10 | Existing tests |
| invalid/universal/ | 8 | Existing tests |

**Total: 69 test programs**

### Phase 3: Test Script ✓

**Created:** `test/run_typechecker_repl_tests.sh`

- Tests both typechecker/ and moded_types/ directories
- Positive tests: load file, check for success
- Negative tests: load file, expect type error
- Summary with pass/fail counts
- Organized output by category

### Phase 4: Baseline Results

**First run (before fixes):** 14/29 passing (48%) - typechecker/ only

**Issues identified:**
1. 3 positive tests had parse issues (fixed)
2. 12 negative tests not rejected (type checker bugs)
3. 7 negative tests correctly rejected

## Files Created/Modified

| File | Status |
|------|--------|
| `glp_runtime/bin/glp_repl_typed.dart` | Created |
| `test/run_typechecker_repl_tests.sh` | Created |
| `test/programs/typechecker/positive/monitor.glp` | Fixed |
| `test/programs/typechecker/positive/int_list_sum.glp` | Fixed |
| `test/programs/typechecker/positive/paper/merge.glp` | Fixed |

## How to Run Tests

```bash
cd /Users/udi/GLP
bash test/run_typechecker_repl_tests.sh
```

Or to save output:

```bash
bash test/run_typechecker_repl_tests.sh 2>&1 | tee typechecker_results.txt
```

## Files to Clean Up (Optional)

These can be deleted when no longer needed:

| File | Purpose |
|------|---------|
| `glp_runtime/test/golden/` | Unused Dart infrastructure |
| `glp_runtime/test/golden_test.dart` | Unused |
| `run_baseline_tests.sh` | Temporary script |
| `test_compile_typed_repl.sh` | Temporary script |
| `baseline_test_output.txt` | Test output |

## Integration (Future Phase 5)

Once type checker is stable:
1. Merge type checking into main `glp_repl.dart`
2. Merge test script into `full_run_repl_tests.sh`
3. Delete `glp_repl_typed.dart`
4. Consider consolidating moded_types/ into typechecker/

## Test Categories Reference

### Positive (Well-Typed)
- Basic stream operations (merge, append, copy)
- Difference lists
- Channels
- Universal types (Any)
- Embedded mode types

### Negative (Ill-Typed)
- **Coverage**: Missing clauses for type alternatives
- **Head Mode**: Wrong reader/writer annotations in head
- **Body Mode**: Wrong modes in body goals, undefined procedures
- **Complementarity**: Type/mode mismatches between variable occurrences
- **Type Definition**: Undefined types
- **Embedded**: Incorrect embedded mode inversions
- **Deep**: Errors in deeply nested structures
- **Universal**: Incorrect use of Any type
