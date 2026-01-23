# Type Checker Implementation Plan

**Created**: 2026-01-23  
**Status**: Phase 1 complete, Phase 2 ready  
**Paper**: Types_for_GLP_34.pdf (Definition 5.1–5.20)

## Overview

The paper underwent major mathematical simplification. Specifications have been rewritten from scratch (see archive/specs-2026-01-23/ for old versions). This plan tracks implementation alignment with the new specs.

## Phase Summary

| Phase | Description | Status | Notes |
|-------|-------------|--------|-------|
| 1 | Terminology: complement → dual | ✅ COMPLETE | 218/302 tests, no regression |
| 2 | Simplify moded_head.dart | NOT STARTED | 350 → ~100 lines |
| 3 | Simplify well_typed_term.dart | NOT STARTED | 350 → ~80 lines |
| 4 | Simplify well_typed_clause.dart | NOT STARTED | 500 → ~120 lines |
| 5 | Full test suite validation | NOT STARTED | Fix regressions |

---

## Phase 1: Terminology Update ✅ COMPLETE

**Goal**: Rename "complement" to "dual" throughout the type checker to align with paper Definition 5.1.

**Completed**: 2026-01-23  
**Commit**: 2e5dfa5 (merged from claude/revise-type-system-cfIgs)  
**Test Results**: 218/302 passing (identical to baseline)  
**Changes**: 7 files, 146 insertions, 146 deletions

### Files Updated

| File | Changes | Status |
|------|---------|--------|
| mode.dart | `Mode.complement` → `Mode.dual` | ✅ |
| moded_term.dart | `complement()` → `dual()`, `_ComplementVisitor` → `_DualVisitor` | ✅ |
| type_ast.dart | `TypeRef.complement()` → `TypeRef.dual()`, `isComplement` → `isDual` | ✅ |
| program_dfa.dart | `DFAState.isComplement/complement` → `isDual/dual`, TransitionLabel, Automaton | ✅ |
| moded_head.dart | `_complementType()` → `_dualType()`, `isComplement` → `isDual` | ✅ |
| well_typed_term.dart | `_checkComplementarity` → `_checkDuality`, `NonComplementaryError` → `NonDualError` | ✅ |
| well_typed_clause.dart | `_areComplementaryTypes` → `_areDualTypes`, `ClauseComplementaryError` → `ClauseDualityError` | ✅ |

---

## Phase 2: Simplify moded_head.dart

**Goal**: Rewrite to match Definition 5.5 (3 sentences in paper → ~100 lines of code)

**Paper Definition 5.5** (Moded Head):
> Given a clause head H with type τH, the moded head for H is obtained by:
> (1) Building an I/O-moded term from τH
> (2) For each variable V, if V's form ≠ V's structural mode, replace V with V?

**Current state**: 350 lines with `_buildIOModedTerm`, `_buildModedSubterm`, `_getSubtermModes`, `_getListSubtermModes`, `_getEmbeddedMode`, `_dualType`, `_buildOpaqueModedTerm`, `_ensureVariablesMatchModes`, `_flipAllVariables`, plus anonymous variable management.

**Target**: ~100-120 lines implementing the two-step process directly.

### Status

- [ ] Design new implementation
- [ ] Write replacement code
- [ ] Verify tests pass
- [ ] Remove dead code

---

## Phase 3: Simplify well_typed_term.dart

**Goal**: Rewrite to match Definition 5.4 (2 sentences in paper → ~80 lines of code)

**Paper Definition 5.4** (Well-Typed Moded Term):
> A moded term T with type τ is well-typed if:
> (1) Each path in T has a consistent type path in τ
> (2) Every variable pair (V, V?) has dual types

**Current state**: 350 lines with `WellTypedResult`, `VariableTypeInfo`, multiple error classes (`InconsistentPathError`, `InconsistentVariableError`, `NonDualError`), elaborate traversal.

**Target**: ~80 lines with simple path enumeration and consistency check.

### Status

- [ ] Design new implementation
- [ ] Write replacement code
- [ ] Verify tests pass
- [ ] Remove dead code

---

## Phase 4: Simplify well_typed_clause.dart

**Goal**: Rewrite to match Definition 5.7 (~8 lines in paper → ~120 lines of code)

**Paper Definition 5.7** (Well-Typed Clause):
> A clause H :- B₁, ..., Bₙ is well-typed if:
> (1) The moded head is well-typed
> (2) Each body atom is well-typed
> (3) For each variable pair (V, V?), their types across head and body are dual or identical

**Current state**: 500 lines with `ClauseCheckResult`, `HeadError`, `BodyAtomError`, `ClauseDualityError`, `UndefinedProcedureError`, `ArityMismatchClauseError`, complex variable location tracking.

**Target**: ~120 lines with straightforward three-condition check.

### Status

- [ ] Design new implementation
- [ ] Write replacement code
- [ ] Verify tests pass
- [ ] Remove dead code

---

## Phase 5: Full Test Suite Validation

**Goal**: Ensure all tests pass after simplification

### Current Test Status

**Baseline (Phase 1)**: 218/302 tests passing (72.2%)

Note: Previous session reported 145/222 (65.3%). The test suite appears to have expanded or been reconfigured.

### Status

- [x] Run baseline tests before Phase 1: 218/302
- [ ] Track test counts after each phase
- [ ] Investigate systematic failures
- [ ] Fix implementation bugs

---

## Files Kept Unchanged

These files are already well-aligned with the paper or are infrastructure:

| File | Reason |
|------|--------|
| type_ast.dart | Solid type definition infrastructure |
| program_dfa.dart | Correctly implements Definitions 5.11-5.13 |
| type_checker.dart | Good orchestration layer |
| prelude.dart | Standard library definitions |
| type_conversion.dart | AST conversion utilities |

---

## Progress Log

### 2026-01-23

- Specs archived and rewritten from scratch
- Implementation plan created
- **Phase 1 complete**: terminology rename (complement → dual)
  - 7 files changed, 146 insertions, 146 deletions
  - Tests: 218/302 (no regression)
  - Commit: 2e5dfa5

---

## Notes

- String errors are sufficient (no elaborate error class hierarchies needed)
- Each spec quotes paper definition verbatim as authoritative source
- Implementation should be minimal: if paper says 3 sentences, code should be ~100 lines, not 350
