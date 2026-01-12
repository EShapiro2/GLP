# Type Checker Testing Infrastructure Plan

**Version**: 0.2  
**Date**: 2026-01-12  
**Status**: IN PROGRESS

## Goal

Integrate type checking into the REPL and create a comprehensive test suite using the existing REPL test infrastructure.

## Design Decisions

1. **Typed vs Untyped Programs**: Programs without `procedure` declarations skip type checking (vacuously well-typed)
2. **Test Format**: Pure .glp files tested via REPL, same as existing runtime tests
3. **Infrastructure**: Shell script, not separate Dart test framework

## Phase 1: REPL with Type Checking

**Create:** `bin/glp_repl_typed.dart`

Copy of `glp_repl.dart` with type checker integration in `loadProgram()`:
- If `module.procDeclarations.isNotEmpty`, call `checkModule()`
- Reject file with error messages if ill-typed
- Otherwise proceed to compile and run

## Phase 2: Positive Test Programs

**Location:** `test/programs/typechecker/positive/`

| File | Description |
|------|-------------|
| `merge.glp` | Stream merge (Paper A.1) |
| `append.glp` | List append |
| `dl_append.glp` | Difference list (Paper A.5) |
| `channel.glp` | Bidirectional channel (Paper A.6) |
| `copy.glp` | Stream copy |
| `counter.glp` | Nat operations |

Each file has type definitions, procedure declarations, is well-typed, and includes a query with expected output.

## Phase 3: Negative Test Programs

**Location:** `test/programs/typechecker/negative/`

| Category | File | Expected Error |
|----------|------|----------------|
| Coverage | `merge_missing_nil.glp` | Missing [] clause |
| Coverage | `merge_missing_cons.glp` | Missing [|] clause |
| Head Mode | `reader_at_input.glp` | X? where X expected |
| Head Mode | `writer_at_output.glp` | X where X? expected |
| Head Mode | `wrong_constant.glp` | 42 at Stream position |
| Head Mode | `wrong_functor.glp` | cons/2 instead of [|] |
| Body Mode | `undefined_proc.glp` | Undeclared procedure |
| Body Mode | `body_wrong_mode.glp` | Wrong mode in body |
| Complementarity | `type_mismatch.glp` | X:Stream vs X?:_? |
| Type Definition | `undefined_type.glp` | Unknown type in procedure |

## Phase 4: Test Script

**Create:** `test/run_typechecker_repl_tests.sh`

- Positive tests: load file, run query, check output pattern
- Negative tests: try to load file, expect rejection with error pattern
- Summary with pass/fail counts

## Phase 5: Cleanup

**Delete:**
- `test/golden/` directory (unused Dart infrastructure)
- `test/golden_test.dart` (unused)
- `test/programs/moded_types/` directory (consolidate into typechecker/)

**Keep/Update:**
- `test/programs/typechecker/positive/` — merge valuable existing files
- `test/programs/typechecker/negative/` — merge valuable existing files

## Phase 6: Integration (Future)

Once stable:
1. Merge type checking into main `glp_repl.dart`
2. Merge test script into `full_run_repl_tests.sh`
3. Delete `glp_repl_typed.dart`

## Progress Log

| Date | Step | Status | Notes |
|------|------|--------|-------|
| 2026-01-12 | Plan v0.1 | Done | Initial complex plan |
| 2026-01-12 | Plan v0.2 | Done | Simplified to REPL-based |
| 2026-01-12 | Phase 1 | Pending | Create glp_repl_typed.dart |
| 2026-01-12 | Phase 2-4 | Pending | Test files and script |
| 2026-01-12 | Phase 5 | Pending | Cleanup |
