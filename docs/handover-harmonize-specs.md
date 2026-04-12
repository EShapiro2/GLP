# Handover: Harmonize Spec Files with Paper

## Context

The CGLP paper (just pushed to GitHub/Overleaf) now defines the authoritative architecture for system predicates and body kernels. Two spec files in the GLP repo need to be harmonized with it.

**Authoritative source of truth**: `~/Grassroots/GLP-ICLP-2026/sections/appendix-guards.tex`

**Authoritative stdlib implementations** (verify against these too):
- `~/Grassroots/GLP/programs/stdlib/assign.glp` — `:=` definition
- `~/Grassroots/GLP/programs/stdlib/univ.glp` — `=..` definition  
- `~/Grassroots/GLP/programs/stdlib/unify.glp` — `=` definition
- `~/Grassroots/GLP/programs/stdlib/time.glp` — `now/1` definition

## Files to Modify

### 1. `~/Grassroots/GLP/docs/glp-predicate-taxonomy.md`

This file is ~500 lines and already at v1.1 (partially harmonized). Remaining issues:

**a) Section 3 comparison table**: The Context column for System Predicates was just changed from "Any" to "Body" — verify this is correct. System predicates are called in body position (after `|`), spawned like any other goal.

**b) `=` is misclassified**: `=` should NOT appear anywhere as a system predicate. It is a **defined guard predicate** (unit clause `X? = X.`). Verify it is correctly categorized. If it appears under system predicates, move it.

**c) `..=` should not exist**: Only `=..` exists, and it's bidirectional. Remove any mention of `..=` as a separate predicate.

**d) `execute/2` references**: The bytecode spec (section 18) and possibly the taxonomy mention `execute/2` as the mechanism for system predicates. This is WRONG — system predicates are regular GLP clauses, not invoked via execute. Remove or correct any `execute/2` references that describe it as the system predicate mechanism.

**e) Guard kernel naming**: The taxonomy uses `guard_add`, `guard_sub` etc. Verify these are consistent with the bytecode spec. These are compiler-internal and NOT in the paper, so they just need internal consistency.

**f) Version bump**: Update version to 1.2, add revision history entry noting harmonization with paper appendix.

### 2. `~/Grassroots/GLP/docs/glp-bytecode-v216-complete.md`

This is a LARGE file (~2000+ lines). Key sections to fix:

**a) Section 18 (System Predicates / execute)**: This section likely describes system predicates as invoked via an `execute` instruction. The paper now makes clear that system predicates are regular GLP clauses whose bodies call body kernels. Section 18 needs to reflect this: system predicates are compiled and executed as normal GLP procedures, not via a special execute opcode. The `execute` mechanism (if it exists as a bytecode instruction) may still be useful for other purposes, but it is NOT how `:=`, `=..`, and `now/1` work.

**b) Section 19.8 (Guards vs System Predicates table)**: The table says system predicates have "Two-valued (SUCCESS/ABORT)" semantics and are "External function calls". WRONG. System predicates are GLP clauses with three-valued semantics. Body kernels (which they call) are two-valued. Fix the table to distinguish system predicates from body kernels, or add a row for body kernels.

**c) Examples referencing `evaluate/2`**: Any examples using `evaluate/2` or `execute/2` for arithmetic should be updated to show `:=` as a regular GLP procedure call (spawn).

**d) Version bump**: Update to v2.16.3 or similar, add revision history entry.

### 3. Bug report update: `~/Grassroots/GLP/docs/bug-execute-varref-resolution.md`

This documents a VarRef resolution bug in `execute/2`. The bug report itself is still valid (the execute mechanism has that bug), but add a note that the paper now specifies system predicates as GLP clauses using body kernels, and the `execute/2` workaround (`'_now'(T)` body kernel) is actually the correct architecture, not a workaround.

## Principles

1. **Paper is authoritative** — if spec disagrees with paper, spec is wrong
2. **Stdlib is ground truth for clause definitions** — read the actual .glp files
3. **Don't invent** — if something isn't in the paper or stdlib, don't add it to specs
4. **Preserve valid content** — much of both specs is correct; surgical edits only
5. **Bump versions and document changes** — revision history matters for these normative docs
