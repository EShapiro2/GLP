# GLP Documentation Index

**Last updated:** 2026-05-18

## Start here

- [`DISCIPLINE.md`](DISCIPLINE.md) — development discipline (spec-first, baseline-before-commit, no workarounds)
- [`typed-glp-manual.md`](typed-glp-manual.md) — typed GLP programming guide
- [`glp-cheat-sheet.md`](glp-cheat-sheet.md) — patterns and idioms ("GLP is NOT Prolog")
- [`known-issues.md`](known-issues.md) — outstanding known issues
- [`Mandatory protocol for debugging the GLP implementation with GLP programs.txt`](./Mandatory%20protocol%20for%20debugging%20the%20GLP%20implementation%20with%20GLP%20programs.txt) — debugging protocol

## Core specifications

| Document | Description |
|----------|-------------|
| [`glp-bytecode-v216-complete.md`](glp-bytecode-v216-complete.md) | Instruction set (v2.16) |
| [`glp-runtime-spec.txt`](glp-runtime-spec.txt) | Dart runtime architecture |
| [`glp-compiler-spec.md`](glp-compiler-spec.md) | Compiler design |
| [`glp-arithmetic-spec.md`](glp-arithmetic-spec.md) | Arithmetic via `:=` |
| [`glp-io-spec.md`](glp-io-spec.md) | I/O |
| [`parser-spec.md`](parser-spec.md) | Parser |
| [`mutual-ref-spec.md`](mutual-ref-spec.md) | Mutual references |
| [`heap/heap-pointer-architecture-spec.md`](heap/heap-pointer-architecture-spec.md) | Heap pointer architecture (FCP-style) |
| [`modules/glp-module-system-spec.md`](modules/glp-module-system-spec.md) | Module system |
| [`modules/glp-project-compilation-spec.md`](modules/glp-project-compilation-spec.md) | Project compilation |
| [`ma/madGLP-spec.md`](ma/madGLP-spec.md) | Multi-agent GLP |
| [`ma/agent-runtime-spec.md`](ma/agent-runtime-spec.md) | Agent runtime |
| [`ma/isolate-boot-spec.md`](ma/isolate-boot-spec.md) | Multi-isolate boot |
| [`ma/multi-agent-trace-spec.md`](ma/multi-agent-trace-spec.md) | Trace format |
| [`ma/ui-io-spec.md`](ma/ui-io-spec.md) | UI I/O |

## Type system (`type system/`)

| Document | Description |
|----------|-------------|
| [`STATUS.md`](type%20system/STATUS.md) | Type system implementation status |
| [`moded-term.md`](type%20system/moded-term.md) | Moded term representation |
| [`moded-head.md`](type%20system/moded-head.md) | Moded head construction |
| [`well-typed-term.md`](type%20system/well-typed-term.md) | Path consistency checking |
| [`well-typed-clause.md`](type%20system/well-typed-clause.md) | Clause well-typing |
| [`well-typed-program.md`](type%20system/well-typed-program.md) | Program well-typing |
| [`typed-program.md`](type%20system/typed-program.md) | Typed program structure |
| [`type-automaton.md`](type%20system/type-automaton.md) | Type automaton |
| [`subtyping.md`](type%20system/subtyping.md) | Subtyping |
| [`parameterized-types-plan.md`](type%20system/parameterized-types-plan.md) | Parameterized types |
| [`dynamic-module-dispatch.md`](type%20system/dynamic-module-dispatch.md) | Dynamic dispatch |
| [`friends-list-moding-pattern.md`](type%20system/friends-list-moding-pattern.md) | Friends-list moding |

## References

| Document | Description |
|----------|-------------|
| [`guards-reference.md`](guards-reference.md) | Guard predicates (success/suspend/fail) |
| [`body-kernels-reference.md`](body-kernels-reference.md) | Body kernels |
| [`glp-predicate-taxonomy.md`](glp-predicate-taxonomy.md) | Predicate classification |
| [`naming-conventions.md`](naming-conventions.md) | Naming (`'_name'` for system) |
| [`grassroots-testing-framework.md`](grassroots-testing-framework.md) | Theatre-style play tests |
| [`village-market-scenario.md`](village-market-scenario.md) | Village-market reference scenario |

## Subdirectories

| Path | Purpose |
|------|---------|
| `bugs/` | Active bug reports |
| `analysis/` | Code/test analysis (currently empty) |
| `archive/` | Historical documents and completed work |
| `diary/` | Accumulated programming knowledge |
| `future/` | Designs not yet implemented |
| `handover/` | (empty after 2026-05 cleanup; will collect future handovers) |
| `heap/` | Heap architecture |
| `infra/` | Infrastructure rules and module boundary |
| `ma/` | Multi-agent (madGLP) |
| `modules/` | Module system |
| `projects/` | Sub-project overviews |
| `type system/` | Type system specs |

## External references

- CSSN GLP implementation spec: `/Users/udi/Grassroots/CSSN/docs/cssn-glp-implementation-spec.md`
- FCP reference (GitHub): https://github.com/EShapiro2/FCP
- GLP arXiv paper: `/Users/udi/Grassroots/GLP-arXiv/`
- ICLP 2026 camera-ready: `/Users/udi/Grassroots/GLP-ICLP-2026/`
