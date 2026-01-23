# Type Checker Implementation Plan

**Created**: 2026-01-23  
**Status**: Phase 1 complete, Phase 2 ready for execution  
**Paper**: sections/well-typing.tex (Definition `moded-head` = Definition 5.5)

## Overview

The paper underwent major mathematical simplification. Specifications have been rewritten from scratch (see archive/specs-2026-01-23/ for old versions). This plan tracks implementation alignment with the new specs.

## Phase Summary

| Phase | Description | Status | Notes |
|-------|-------------|--------|-------|
| 1 | Terminology: complement → dual | ✅ COMPLETE | 218/302 tests, no regression |
| 2 | Clean up moded_head.dart | READY | Remove dead code, update refs |
| 3 | Review well_typed_term.dart | NOT STARTED | Pending Phase 2 |
| 4 | Review well_typed_clause.dart | NOT STARTED | Pending Phase 2 |
| 5 | Full test suite validation | NOT STARTED | Fix regressions |

---

## Phase 1: Terminology Update ✅ COMPLETE

**Completed**: 2026-01-23  
**Commit**: 2e5dfa5  
**Test Results**: 218/302 passing (identical to baseline)  
**Changes**: 7 files, 146 insertions, 146 deletions

---

## Phase 2: Clean up moded_head.dart

### Paper Analysis (from LaTeX source)

**Definition `moded-head`** in `sections/well-typing.tex`:

> Given a head H for procedure p with type declaration p(T₁, …, Tₙ), the moded head H' is constructed as follows:
> 1. **Assign structural modes:** For each argument position i, assign mode ↓ if Tᵢ is an input type and mode ↑ if Tᵢ is an output type. **Modes propagate through nested term structure according to the type definition, inverting at embedded `?` annotations.**
> 2. **Adjust variables:** For each variable, if its form does not match the structural mode at its position, replace it with its paired variable.

**Key insight**: The paper explicitly requires "modes propagate through nested term structure according to the type definition." This means the type lookup machinery (`_getSubtermModes`, `_getListSubtermModes`) is **necessary**, not over-engineering.

### Implementation Assessment

| Function | Purpose | Verdict |
|----------|---------|---------|
| `modedHead()` | Main entry point | ✅ Correct |
| `producedTerm()` | Body atom entry | ✅ Correct |
| `_buildIOModedTerm()` | Step 1: build moded term | ✅ Correct |
| `_buildModedSubterm()` | Recursive builder | ✅ Correct |
| `_getSubtermModes()` | Type lookup for structs | ✅ Required by paper |
| `_getListSubtermModes()` | Type lookup for lists | ✅ Required by paper |
| `_getEmbeddedMode()` | Extract mode from type | ✅ Correct |
| `_dualType()` | Flip type mode | ✅ Correct |
| `_flipAllVariables()` | Unconditional flip | ❌ **DEAD CODE** - never called |
| `_buildOpaqueModedTerm()` | Wildcard handling | ✅ Correct |
| `_ensureVariablesMatchModes()` | Step 2: adjust variables | ✅ Correct |

### Changes to Make

1. **Remove dead code**: Delete `_flipAllVariables()` (~20 lines) - never called
2. **Update paper reference**: Change "Definition 4.8" → "Definition 5.5" in comments
3. **Simplify errors** (optional): Convert error classes to simple string exceptions
4. **Trim comments**: Remove redundant documentation

### Expected Result

350 lines → ~310 lines (modest cleanup, not radical rewrite)

**Rationale**: The implementation correctly implements what the paper requires. The complexity is inherent in "modes propagate through nested term structure according to the type definition."

### Task for Claude Code

```
Pull latest: cd ~/GLP && git pull

Read the implementation plan: ~/GLP/docs/type\ system/IMPLEMENTATION-PLAN.md

In ~/GLP/glp_runtime/lib/analysis/type_checker/moded_head.dart:

1. DELETE the function `_flipAllVariables()` (around lines 270-290) - it's dead code, never called

2. UPDATE the file header comment: change "Definition 4.8" to "Definition 5.5"

3. UPDATE the doc comment for `modedHead()`: change "Definition 4.6" to "Definition 5.5"

4. Run tests: cd ~/GLP && bash test/run_typechecker_repl_tests.sh 2>&1 | tail -20

5. If tests pass (should be 218/302), commit:
   git add -A
   git commit -m "refactor(moded_head): remove dead code, update paper references to Definition 5.5"
   git push
```

---

## Phase 3: Review well_typed_term.dart

**Status**: Pending Phase 2 completion

Will analyze against Definition `well-typed-moded-term` in paper.

---

## Phase 4: Review well_typed_clause.dart

**Status**: Pending Phase 2 completion

Will analyze against Definition `well-typed-clause` in paper.

---

## Phase 5: Full Test Suite Validation

**Baseline**: 218/302 tests passing (72.2%)

---

## Progress Log

### 2026-01-23 (continued)

- **Phase 2 design REVISED**: Read actual LaTeX source of paper
  - Paper explicitly requires type-driven mode propagation
  - Implementation complexity is justified, not over-engineering
  - Only cleanup: remove `_flipAllVariables()` (dead code), update refs
  - Original ~100 line estimate was wrong; ~310 lines is appropriate

### 2026-01-23

- Specs archived and rewritten from scratch
- Implementation plan created
- **Phase 1 complete**: terminology rename (complement → dual)

---

## Notes

- Always read LaTeX source of paper, not PDF assumptions
- String errors are sufficient (no elaborate error class hierarchies needed)
- Not all code can be radically simplified - some complexity is inherent in the problem
