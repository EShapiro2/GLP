# Type Checker Testing Infrastructure

**Version**: 1.0  
**Date**: 2026-01-12

## For Claude Code: Verify Your Fixes

After making changes to the type checker, run:

```bash
cd /Users/udi/GLP && bash test/run_typechecker_repl_tests.sh 2>&1 | tee typechecker_results.txt
```

Current baseline: **36/69 passing (52%)**. Your goal is to increase this.

## Overview

This document describes the testing infrastructure for the GLP moded type system. It enables testing both well-typed (positive) and ill-typed (negative) programs through a single REPL session.

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

**Key code in `loadProgram()`:**
```dart
if (module.procDeclarations.isNotEmpty) {
  final typeResult = checkModule(module);
  if (!typeResult.isWellTyped) {
    print('Type errors in $filename:');
    for (final error in typeResult.errors) {
      print('  ✗ $error');
    }
    return false;
  }
}
```

### 2. Test Script

**File:** `test/run_typechecker_repl_tests.sh`

Runs all test files in a single REPL session for speed. Loads 69 test files:
- 24 positive (well-typed, should load)
- 45 negative (ill-typed, should be rejected)

### 3. Test Programs

**Location:** `glp_runtime/test/programs/`

Two directories with test files:

| Directory | Purpose |
|-----------|---------|
| `typechecker/` | Organized by error category |
| `moded_types/` | Organized by feature (embedded, deep, universal) |

## Test Program Structure

### Positive Tests (Should Pass)

```
typechecker/positive/
├── merge_basic.glp
├── append_list.glp
├── copy_stream.glp
├── dl_append.glp
├── new_channel.glp
├── monitor.glp
├── int_list_sum.glp
├── nat_operations.glp
├── process_complete.glp
└── paper/
    └── merge.glp

moded_types/valid/
├── merge.glp
├── append.glp
├── counter.glp
├── simple_io.glp
├── embedded/
│   ├── counter_show.glp
│   ├── double_involution.glp
│   └── ... (6 files)
└── universal/
    ├── any_copy.glp
    └── ... (4 files)
```

### Negative Tests (Should Be Rejected)

```
typechecker/negative/
├── coverage/           # Missing clauses for type alternatives
│   ├── merge_missing_both_nil.glp
│   ├── merge_missing_first_nil.glp
│   └── merge_missing_cons.glp
├── head/               # Wrong modes in clause head
│   ├── merge_reader_at_input.glp
│   ├── merge_writer_at_output.glp
│   ├── merge_wrong_constant.glp
│   └── merge_wrong_functor.glp
├── body/               # Wrong modes in clause body
│   ├── merge_undefined_proc.glp
│   └── merge_wrong_mode.glp
├── complementarity/    # Type/mode mismatches
│   ├── merge_type_mismatch.glp
│   └── merge_swapped_vars.glp
├── type_def/           # Type definition errors
│   └── merge_undefined_type.glp
└── *.glp               # Top-level negative tests

moded_types/invalid/
├── reader_at_input.glp
├── writer_at_output.glp
├── embedded/           # Embedded mode errors
├── deep/               # Deeply nested structure errors
└── universal/          # Universal type (Any) errors
```

## Test File Format

Each test file is a valid GLP program with comments describing the test:

```glp
% test: merge_basic
% expected: pass
% description: Basic merge program from paper

Stream ::= [] ; [_|Stream].

procedure merge(Stream?, Stream?, Stream).

merge([], Ys, Ys?).
merge(Xs, [], Xs?).
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
```

## Current Results (Baseline)

```
Total: 69 | Passed: 36 | Failed: 33 (52%)
```

### Known Issues

| Category | Problem |
|----------|---------|
| Universal types | All `any_*` tests fail (not implemented?) |
| Coverage (subdirs) | `merge_missing_*` not detected |
| Head/body modes (subdirs) | Not detected |
| Some valid embedded | `simple_io`, `counter_show`, `double_involution` wrongly rejected |

## Adding New Tests

### Positive Test

1. Create file in `typechecker/positive/` or `moded_types/valid/`
2. Include type definitions and procedure declarations
3. Add file path to `POSITIVE_FILES` array in test script

### Negative Test

1. Create file in appropriate subdirectory under `typechecker/negative/` or `moded_types/invalid/`
2. Include the specific error you want to test
3. Add file path to `NEGATIVE_FILES` array in test script

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

## Integration with Main REPL

Once the type checker is stable:

1. Merge type checking code into `bin/glp_repl.dart`
2. Merge test script into `test/full_run_repl_tests.sh`
3. Delete `bin/glp_repl_typed.dart`

## File Locations Summary

| File | Purpose |
|------|---------|
| `glp_runtime/bin/glp_repl_typed.dart` | REPL with type checking |
| `test/run_typechecker_repl_tests.sh` | Test runner script |
| `glp_runtime/test/programs/typechecker/` | Organized test programs |
| `glp_runtime/test/programs/moded_types/` | Feature-based test programs |
| `docs/typechecker-testing.md` | This document |
