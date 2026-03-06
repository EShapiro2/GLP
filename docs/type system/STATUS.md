# Type System Specifications

**Version**: 2.0  
**Date**: 2026-01-23  
**Paper**: Types_for_GLP_34.pdf

## Specification Files

| File | Paper Definition | Description |
|------|------------------|-------------|
| `typed-program.md` | Definition 4.1 | Typed GLP program structure |
| `moded-term.md` | Definition 5.1 | Moded terms and dual operation |
| `consistent-paths.md` | Definition 5.2 | Path consistency rules |
| `well-typed-term.md` | Definition 5.4 | Well-typed moded term |
| `moded-head.md` | Definition 5.5 | Moded head construction |
| `well-typed-clause.md` | Definition 5.7 | Well-typed clause |
| `well-typed-program.md` | Definition 5.10 | Well-typed program |
| `type-automaton.md` | Definitions 5.11-5.13 | Type automaton |
| `subtyping.md` | Definitions 5.16-5.20 | Subtyping |

`typed-program.md` also covers **parameterized types** (Paper Section 8, Definition 8.1): parameterized type definitions, instantiation, the expansion algorithm, parameterized procedure declarations, and interaction with modules. The expansion is a preprocessing step; all other specs are unchanged.

## Archived

Previous specifications (2026-01-23) moved to `archive/specs-2026-01-23/`.

## Test Status

Run `bash test/run_typechecker_repl_tests.sh` to get current test results.
