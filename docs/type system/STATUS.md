# Type Checker Status

**Last Updated**: 2026-01-18
**Test Output**: `/Users/udi/Grassroots/GLP/test_output/typed_repl_output.txt`

## Current State

**Test Results (2026-01-18):** 141/222 passing (63.5%)

Baseline verified after DFA state creation fix — no regressions.

Run `bash /Users/udi/Grassroots/GLP/test/run_typechecker_repl_tests.sh` to get current numbers.

## Test Infrastructure

| Script | Output |
|--------|--------|
| `test/run_typechecker_repl_tests.sh` | `test_output/typed_repl_output.txt` |
| `test/run_book_tests.sh` | `test_output/main_repl_output.txt` |

## Directories

| Directory | Purpose |
|-----------|---------|
| `programs/typed_book/` | Book programs with type annotations |
| `programs/book/` | Original book programs (untyped) |
| `glp_runtime/test/programs/typechecker/` | Type checker unit tests |
| `glp_runtime/test/programs/moded_types/` | Mode checking unit tests |

## Specifications

All specification files are in `/Users/udi/Grassroots/GLP/docs/type system/`:

| File | Purpose |
|------|---------|
| `compilation-pipeline.md` | **Pipeline architecture: SRSW vs typing distinction** |
| `mode.md` | Mode definitions |
| `type-environment.md` | Type environment spec |
| `type-conversion.md` | Term to TypeExpr conversion |
| `clause-validation.md` | Clause term validation |
| `moded-term.md` | Moded term spec |
| `moded-head.md` | Moded head spec |
| `type-dfa.md` | Type DFA spec |
| `well-typed-term.md` | Well-typed term spec |
| `well-typed-clause.md` | Well-typed clause spec |
| `well-typed-program.md` | Well-typed program spec |
| `testing.md` | Testing strategy |

## Known Errors

1. **SRSW display bug** — SRSW violations not shown when type errors present (bug-report-2026-01-17.md, Bug 7).

2. **81 failing typed tests** — See test output for details. Categories include social_graph, social_networks, meta, cryptocurrencies, and constitutional_consensus programs.

## Remaining Diary Files

| File | Purpose |
|------|---------|
| `docs/diary/book-fixes-tracker.md` | Tracks SRSW violations in original `book/` programs |
| `docs/diary/glp-programming-knowledge.md` | Accumulated GLP programming wisdom |
