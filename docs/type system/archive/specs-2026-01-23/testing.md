# Type Checker Testing Infrastructure

**Version**: 1.2  
**Date**: 2026-01-12

## For Claude Code: Verify Your Fixes

After making changes to the type checker, run:

```bash
cd /Users/udi/GLP && bash test/run_typechecker_repl_tests.sh 2>&1 | tee typechecker_results.txt
```

Current baseline: **45/69 passing (65%)**. Your goal is to increase this.

## Overview

This document describes the testing infrastructure for the GLP moded type system. It enables testing both well-typed (positive) and ill-typed (negative) programs through a single REPL session.

## Testing Philosophy: GLP Tests Only

**Decision (2026-01-12):** The type checker is tested exclusively through GLP program files, not Dart unit tests.

### Rationale

1. **GLP tests are the spec** — If a GLP program should pass or fail type checking, that's what matters. The user experience is defined by GLP programs, not internal function behavior.

2. **End-to-end coverage** — GLP tests exercise the complete chain: parser → type environment → DFA → well-typed clause checking → coverage checking. This catches integration issues that unit tests miss.

3. **Self-documenting** — Test files are valid GLP programs with comments explaining what they test. Anyone can read them without knowing Dart.

4. **Avoid double maintenance** — Dart unit tests require keeping expectations synchronized with the spec. When the spec changes, only GLP tests need updating.

5. **Historical issues** — Previous Dart unit tests had inverted mode expectations (10+ tests), causing confusion about what behavior was correct. GLP tests tied to spec examples avoid this.

### When to Add Dart Unit Tests

Dart unit tests may be added for:
- **Algorithm verification** — Testing a specific algorithm in isolation (e.g., DFA construction edge cases)
- **Performance regression** — Benchmarking specific operations
- **Debugging aids** — Temporary tests while investigating issues

New Dart unit tests should be placed in `glp_runtime/test/analysis/type_checker/` and documented in this file.

### Dart Unit Tests: Removed (2026-01-12)

The following Dart test files were removed from `glp_runtime/test/analysis/type_checker/`:

| Removed File | Reason |
|--------------|--------|
| `channel_types_test.dart` | Covered by GLP channel tests |
| `defined_guards_test.dart` | Covered by GLP guard tests |
| `guard_types_test.dart` | Covered by GLP guard tests |
| `moded_head_test.dart` | Covered by GLP mode tests |
| `moded_term_test.dart` | Covered by GLP mode tests |
| `moded_type_parser_test.dart` | Parser tested via GLP file loading |
| `nested_any_coverage_test.dart` | Empty file (was broken) |
| `predefined_operations_test.dart` | Covered by GLP prelude tests |
| `prelude_test.dart` | Covered by GLP prelude tests |
| `primitive_mode_coverage_test.dart` | Had inverted mode expectations |
| `primitive_mode_test.dart` | Covered by GLP mode tests |
| `primitive_state_modes_test.dart` | Had inverted mode expectations |
| `program_dfa_test.dart` | DFA tested via type checking GLP programs |
| `system_types_test.dart` | Covered by GLP type tests |
| `test_helpers.dart` | No longer needed |
| `type_checker_new_test.dart` | Covered by GLP tests |
| `type_environment_test.dart` | Covered by GLP type definition tests |
| `well_typed_clause_test.dart` | Had inverted mode expectations |
| `well_typed_term_test.dart` | Covered by GLP term tests |
| `debug_dl.dart` | Debugging utility, no longer needed |
| `debug_single_mode_test.dart` | Debugging utility, no longer needed |

## Quick Start

```bash
cd /Users/udi/GLP
bash test/run_typechecker_repl_tests.sh
```

Or to save output:

```bash
bash test/run_typechecker_repl_tests.sh 2>&1 | tee typechecker_results.txt
```

## Components

### 1. Typed REPL

**File:** `glp_runtime/bin/glp_repl_typed.dart`

A copy of `glp_repl.dart` with type checking integrated. When loading a `.glp` file:

- If the file has `procedure` declarations → runs type checker
- If type errors found → rejects file, prints errors
- If well-typed → prints "Type check passed", proceeds to compile
- If no `procedure` declarations → skips type checking (untyped program)

### 2. Test Script

**File:** `test/run_typechecker_repl_tests.sh` (v2.0)

Runs all test files in a single REPL session for speed. Categories:
- **POSITIVE_FILES** — Well-typed programs that should load successfully
- **NEGATIVE_FILES** — Ill-typed programs that should be rejected (type errors)
- **SRSW_FILES** — Programs rejected by parser for SRSW violations (not type errors)

### 3. Test Programs

**Location:** `glp_runtime/test/programs/`

| Directory | Purpose |
|-----------|---------|
| `typechecker/positive/` | Well-typed programs |
| `typechecker/negative/` | Ill-typed programs by category |
| `moded_types/valid/` | Valid mode configurations |
| `moded_types/invalid/` | Invalid mode configurations |

## Test File Format

Each test file is a valid GLP program with comments describing the test:

```glp
%% VALID: Basic merge program from paper
%% 
%% Mode trace:
%%   - merge(Stream?, Stream?, Stream)
%%   - Args 1,2 are inputs (↓), arg 3 is output (↑)
%%
%% SRSW: All variables properly paired ✓

Stream ::= [] ; [_|Stream].

procedure merge(Stream?, Stream?, Stream).

merge([], Ys, Ys?).
merge(Xs, [], Xs?).
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
```

## Adding New Tests

### Positive Test (should pass)

1. Create file in `typechecker/positive/` or `moded_types/valid/`
2. Include type definitions and procedure declarations
3. Add comments explaining mode trace and why it's valid
4. Add file path to `POSITIVE_FILES` array in test script

### Negative Test (should fail)

1. Create file in appropriate subdirectory:
   - `typechecker/negative/coverage/` — Missing clause coverage
   - `typechecker/negative/head/` — Head type errors
   - `typechecker/negative/body/` — Body type errors
   - `typechecker/negative/complementarity/` — Complementarity violations
   - `moded_types/invalid/` — Mode errors
2. Add comments explaining what error should be detected
3. Add file path to `NEGATIVE_FILES` array in test script

### SRSW Test (parser rejection)

1. Create file with SRSW violation (unpaired reader/writer)
2. Add to `SRSW_FILES` array (not `NEGATIVE_FILES`)
3. These test parser behavior, not type checker

## Debugging a Test

To test a single file interactively:

```bash
cd /Users/udi/GLP/glp_runtime
dart run bin/glp_repl_typed.dart
```

Then in REPL:
```
GLP> /full/path/to/test/file.glp
```

## File Locations Summary

| File | Purpose |
|------|---------|
| `glp_runtime/bin/glp_repl_typed.dart` | REPL with type checking |
| `test/run_typechecker_repl_tests.sh` | Test runner script (v2.0) |
| `glp_runtime/test/programs/typechecker/` | Organized test programs |
| `glp_runtime/test/programs/moded_types/` | Feature-based test programs |
| `docs/type system/testing.md` | This document |

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2026-01-12 | Initial version with GLP test infrastructure |
| 1.1 | 2026-01-12 | Added "GLP Tests Only" policy; listed Dart tests for removal |
| 1.2 | 2026-01-12 | Dart unit tests removed; updated test script to v2.0 with SRSW category |
