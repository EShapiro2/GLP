# GLP Type Checker Test Infrastructure

**Location:** `/Users/udi/Grassroots/GLP/test/`
**Last Updated:** 2025-01-16

## Test Scripts

### Main Test Suites

**`run_typechecker_repl_tests.sh`**
- Primary type checker test suite
- Runs 183 positive + 37 negative + 2 SRSW tests in a single REPL session
- Reports PASS/FAIL summary for each file
- Usage: `cd /Users/udi/Grassroots/GLP && ./test/run_typechecker_repl_tests.sh`
- Output to file: `./test/run_typechecker_repl_tests.sh > /private/tmp/test_output.txt 2>&1`

**`get_detailed_errors.sh`**
- Captures detailed error messages for all 96 failing tests
- Uses `check_types.dart` which provides full error details
- Filters out builtin procedure warnings for cleaner output
- Usage: `chmod +x /Users/udi/Grassroots/GLP/test/get_detailed_errors.sh && /Users/udi/Grassroots/GLP/test/get_detailed_errors.sh > /private/tmp/detailed_errors.txt 2>&1`

**`run_book_tests.sh`**
- Tests book examples in the untyped REPL
- Verifies programs compile without syntax errors
- Usage: `cd /Users/udi/Grassroots/GLP && ./test/run_book_tests.sh`

**`run_type_checker_tests.sh`**
- Runs Dart unit tests for the type checker
- Usage: `cd /Users/udi/Grassroots/GLP/glp_runtime && dart test test/analysis/type_checker/`

### Batch Test Scripts

**`test_batch1_arithmetic.sh`** - Arithmetic tree programs
**`test_batch2_list.sh`** - List processing programs
**`full_run_repl_tests.sh`** - Complete REPL test run

## Key Directories

**Test Programs:**
- `/Users/udi/Grassroots/GLP/glp_runtime/test/programs/typechecker/` - Type checker test cases
- `/Users/udi/Grassroots/GLP/glp_runtime/test/programs/moded_types/` - Mode checking test cases
- `/Users/udi/Grassroots/GLP/programs/typed_book/` - Typed book examples (main test corpus)

**Implementation:**
- `/Users/udi/Grassroots/GLP/glp_runtime/lib/analysis/type_checker/` - Type checker source

**Specifications:**
- `/Users/udi/Grassroots/GLP/docs/modules/` - Spec files (authoritative for implementation)

## Command-Line Tools

**Type check a single file:**
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart run bin/check_types.dart <file.glp>
```

**Load file in typed REPL (interactive):**
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && echo "<absolute-path.glp>" | dart run bin/glp_repl_typed.dart
```

**Run Dart unit tests:**
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test test/analysis/type_checker/
```

## Output Locations

Scripts should write output to `/private/tmp/` for Claude to read:
- `/private/tmp/test_output.txt` - General test output
- `/private/tmp/detailed_errors.txt` - Detailed error messages
- `/private/tmp/all_errors.txt` - Raw REPL output

## Current Test Status (2025-01-16)

- **Total:** 222 tests
- **Passing:** 126
- **Failing:** 96

### Failure Categories

1. **SRSW violations** - Programs use `_` in body or have multiple writer occurrences
2. **Type errors** - Structural type mismatches
3. **Loading errors** - Parse/syntax issues or missing definitions
4. **Coverage errors** - Missing clause alternatives

### Already Marked as Ill-Typed

Files with `STATUS: ILL-TYPED` comments:
- `sum_list.glp` - SRSW violations in reduce/2 meta-interpreter clauses
- `bubble_sort.glp` - Uses `_` in body
- `cooperative.glp` - Uses `_` in body (per handover)

## Workflow for Debugging

1. Run full test suite: `./test/run_typechecker_repl_tests.sh > /private/tmp/test_output.txt 2>&1`
2. Get detailed errors: `./test/get_detailed_errors.sh > /private/tmp/detailed_errors.txt 2>&1`
3. Analyze specific file: `dart run bin/check_types.dart <file.glp>`
4. Read source to determine if program bug or type checker bug
5. Either fix program, mark as ill-typed, or fix type checker implementation
