# Type Checker Status

**Last Updated**: 2025-01-14
**Test Output**: `/Users/udi/GLP/test_output/typed_repl_output.txt`

## Current State

Run `bash /Users/udi/GLP/run_typed_repl_tests.sh` to get current numbers.

## Test Infrastructure

| Script | Output |
|--------|--------|
| `run_typed_repl_tests.sh` | `test_output/typed_repl_output.txt` |
| `run_main_repl_tests.sh` | `test_output/main_repl_output.txt` |

## Directories

| Directory | Purpose |
|-----------|---------|
| `programs/typed_book/` | Book programs with type annotations |
| `programs/book/` | Original book programs (untyped) |
| `glp_runtime/test/programs/typechecker/` | Type checker unit tests |
| `glp_runtime/test/programs/moded_types/` | Mode checking unit tests |

## Known Issues

The test output file is authoritative. Check failed tests there.

## Specs

All specification files are in `/Users/udi/GLP/docs/modules/` (not in `docs/type system/`).
