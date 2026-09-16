# GLP documentation index

Rewritten 2026-09-16 with the tree reorganisation; every file named here exists.  Ownership is Coordination Appendix B; the rules for working in this repository are `/Grassroots/GLP/CLAUDE.md`.

## Start here

- [`DISCIPLINE.md`](DISCIPLINE.md) — development discipline (spec-first, baseline-before-commit, no workarounds)
- [`typed-glp-manual.md`](typed-glp-manual.md) — typed GLP programming guide
- [`glp-cheat-sheet.md`](glp-cheat-sheet.md) — patterns and idioms ("GLP is NOT Prolog")
- [`known-issues.md`](known-issues.md) — the running issue record
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
| [`glp-wire-format-spec.md`](glp-wire-format-spec.md) | Wire format |
| [`heap/heap-pointer-architecture-spec.md`](heap/heap-pointer-architecture-spec.md) | Heap pointer architecture (FCP-style) |
| [`ma/madGLP-spec.md`](ma/madGLP-spec.md) | Multi-agent GLP |
| [`ma/agent-runtime-spec.md`](ma/agent-runtime-spec.md) | Agent runtime |
| [`ma/isolate-boot-spec.md`](ma/isolate-boot-spec.md) | Multi-isolate boot |
| [`ma/multi-agent-trace-spec.md`](ma/multi-agent-trace-spec.md) | Trace format |
| [`ma/HOW-TO-RUN.md`](ma/HOW-TO-RUN.md) | Running the multi-agent routes by hand |

## References

| Document | Description |
|----------|-------------|
| [`guards-reference.md`](guards-reference.md) | Guard predicates (success/suspend/fail) |
| [`body-kernels-reference.md`](body-kernels-reference.md) | Body kernels |
| [`glp-predicate-taxonomy.md`](glp-predicate-taxonomy.md) | Predicate classification |
| [`naming-conventions.md`](naming-conventions.md) | Naming (`'_name'` for system) |

## Open bug reports

The closed reports and their index were deleted on 2026-09-16; these three are the ones still open, and `known-issues.md` carries their status.

| Document | Owner |
|----------|-------|
| [`agent-netin-wakeup-stall-bug.md`](agent-netin-wakeup-stall-bug.md) | madGLP/IGLP — a suspended agent goal is not re-awoken when `NetIn` is bound after suspension |
| [`bug-report-lib-routing-bare-only-params-2026-07-22.md`](bug-report-lib-routing-bare-only-params-2026-07-22.md) | IGLP — checker enforcement parked; two paper decisions pending |
| [`glp-type-limitation-variable-pair-subtyping-2026-07-23.md`](glp-type-limitation-variable-pair-subtyping-2026-07-23.md) | TGLP (GLP-Spec) — variable-pair base-type limitation |

## Subdirectories

| Path | Purpose |
|------|---------|
| `heap/` | Heap architecture |
| `ma/` | Multi-agent (madGLP) |

## External references

- CSSN GLP implementation spec: `/Users/udi/Grassroots/CSSN/docs/cssn-glp-implementation-spec.md`
- FCP reference (GitHub): https://github.com/EShapiro2/FCP
