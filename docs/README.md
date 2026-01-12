# GLP Documentation Index

## Quick Start

- **[SPEC_GUIDE.md](SPEC_GUIDE.md)** - Overview of the GLP execution model
- **[DEVELOPMENT_DISCIPLINE_v1.0.md](DEVELOPMENT_DISCIPLINE_v1.0.md)** - Required development methodology

## Core Specifications

| Document | Description |
|----------|-------------|
| [glp-bytecode-v216-complete.md](glp-bytecode-v216-complete.md) | Complete instruction set specification (v2.16) |
| [glp-runtime-spec.txt](glp-runtime-spec.txt) | Dart runtime architecture |
| [glp-compiler-spec.md](glp-compiler-spec.md) | Compiler design and implementation |
| [glp-arithmetic-spec.md](glp-arithmetic-spec.md) | Arithmetic operations via `:=` operator |
| [glp-io-spec.md](glp-io-spec.md) | I/O operations |
| [glp-module-system-v1-spec.md](glp-module-system-v1-spec.md) | Module system specification |
| [parser-spec.md](parser-spec.md) | Parser specification |

## Type System (`type system/`)

Moded type system specifications and implementation plans:

| Document | Description |
|----------|-------------|
| [mode.md](type%20system/mode.md) | Mode (input/output) definitions |
| [type-environment.md](type%20system/type-environment.md) | Type and procedure declarations |
| [type-dfa.md](type%20system/type-dfa.md) | DFA construction for types |
| [moded-term.md](type%20system/moded-term.md) | Moded term representation |
| [moded-head.md](type%20system/moded-head.md) | Moded head construction |
| [well-typed-term.md](type%20system/well-typed-term.md) | Path consistency checking |
| [well-typed-clause.md](type%20system/well-typed-clause.md) | Clause well-typing |
| [well-typed-program.md](type%20system/well-typed-program.md) | Program well-typing |
| [SPEC_IMPLEMENTATION_GAPS.md](type%20system/SPEC_IMPLEMENTATION_GAPS.md) | Gap analysis (11 issues) |
| [COMPLETION_PLAN.md](type%20system/COMPLETION_PLAN.md) | Implementation plan |
| [testing.md](type%20system/testing.md) | Test infrastructure (69 tests) |

## Reference Documents

| Document | Description |
|----------|-------------|
| [guards-reference.md](guards-reference.md) | Guard predicates reference |
| [glp-predicate-taxonomy.md](glp-predicate-taxonomy.md) | Classification of predicates |
| [naming-conventions.md](naming-conventions.md) | System primitive naming (`'_name'`) |
| [equators-spec.md](equators-spec.md) | Equator extension for many-to-one signaling |
| [mutual-ref-spec.md](mutual-ref-spec.md) | Mutual reference specification |
| [grassroots-testing-framework.md](grassroots-testing-framework.md) | Testing framework documentation |

## Future Work (`future/`)

| Document | Description |
|----------|-------------|
| [glp-modules-spec.md](future/glp-modules-spec.md) | Module system design |
| [module-implementation-plan.md](future/module-implementation-plan.md) | Module implementation plan |
| [FCP_NESTED_STRUCTURE_ANALYSIS.md](future/FCP_NESTED_STRUCTURE_ANALYSIS.md) | FCP nested structure analysis |

## Session Logs (`diary/`)

Recent session handovers and status reports.

## Archive (`archive/`)

Historical documents and completed work.

## External References

- **Moded Types Paper**: `/Users/udi/Moded-Types/`
- **GLP Language Spec**: `GLP_2025.pdf` in project knowledge
