# Type Checker Implementation Plan

**Created**: 2026-01-23  
**Status**: Phase 1 complete, Phase 2 under design review  
**Paper**: Types_for_GLP_34.pdf (Definition 5.1–5.20)

## Overview

The paper underwent major mathematical simplification. Specifications have been rewritten from scratch (see archive/specs-2026-01-23/ for old versions). This plan tracks implementation alignment with the new specs.

## Phase Summary

| Phase | Description | Status | Notes |
|-------|-------------|--------|-------|
| 1 | Terminology: complement → dual | ✅ COMPLETE | 218/302 tests, no regression |
| 2 | Review moded_head.dart | ⚠️ DESIGN REVIEW | See analysis below |
| 3 | Review well_typed_term.dart | NOT STARTED | Pending Phase 2 decision |
| 4 | Review well_typed_clause.dart | NOT STARTED | Pending Phase 2 decision |
| 5 | Full test suite validation | NOT STARTED | Fix regressions |

---

## Phase 1: Terminology Update ✅ COMPLETE

**Goal**: Rename "complement" to "dual" throughout the type checker to align with paper Definition 5.1.

**Completed**: 2026-01-23  
**Commit**: 2e5dfa5 (merged from claude/revise-type-system-cfIgs)  
**Test Results**: 218/302 passing (identical to baseline)  
**Changes**: 7 files, 146 insertions, 146 deletions

---

## Phase 2: Review moded_head.dart ⚠️ DESIGN REVIEW NEEDED

### Original Estimate vs. Reality

**Original estimate**: Simplify from 350 → ~100 lines  
**Revised assessment**: The original estimate was too optimistic. The code complexity is largely inherent in what it needs to do.

### Paper Definition 5.5

> Given a head H, a moded head H' is obtained by:
> 1. Constructing an I/O-moded term corresponding to H, then
> 2. For each variable, if its form does not match its position's structural mode, replacing it with its paired variable.

This appears simple, but "constructing an I/O-moded term" requires:
- Looking up procedure declarations for argument modes
- Looking up type definitions for nested structure modes
- Implementing mode involution (parent ⊕ embedded modes)
- Handling special cases: lists, diff-lists, wildcards, anonymous variables

### Current Code Analysis (350 lines)

| Function | Lines | Purpose | Can Simplify? |
|----------|-------|---------|---------------|
| `modedHead()` | 20 | Main entry for heads | No |
| `producedTerm()` | 15 | Entry for body atoms | No |
| `_buildIOModedTerm()` | 15 | Build from goal | No |
| `_buildModedSubterm()` | 60 | Recursive term builder | Minor |
| `_getSubtermModes()` | 45 | Type lookup for structs | No |
| `_getListSubtermModes()` | 40 | Type lookup for lists | No |
| `_getEmbeddedMode()` | 12 | Extract mode from type | No |
| `_dualType()` | 12 | Flip type mode | No |
| `_flipAllVariables()` | 20 | **DEAD CODE** | Remove |
| `_buildOpaqueModedTerm()` | 30 | Wildcard handling | No |
| `_ensureVariablesMatchModes()` | 20 | Step 2 of definition | No |
| Anonymous var management | 15 | Fresh _#N names | No |
| Error classes | 15 | ArityMismatch, InvalidHead | Simplify to strings |
| Comments/docs | 50 | Documentation | Trim |

### Proposed Changes (Realistic)

**Remove:**
- `_flipAllVariables()` — dead code, never called (-20 lines)

**Simplify:**
- Error classes → simple Exception with string message (-10 lines)
- Trim excessive comments and redundant doc strings (-30 lines)

**Update:**
- All references from "Definition 4.x" to "Definition 5.5"
- Spec reference from "v0.8" to current spec

**Realistic result**: ~290 lines (not 100 lines)

### Discussion Point

The paper's Definition 5.5 is deceptively simple. The spec says:
> Nested modes propagate according to type structure, flipping at each `?`

Implementing this correctly REQUIRES the type lookup machinery (`_getSubtermModes`, `_getListSubtermModes`). This is not over-engineering — it's the minimum needed to implement the spec.

**Question for Udi**: Should we:
1. Proceed with modest cleanup (remove dead code, simplify errors) → ~290 lines
2. Accept the current implementation as appropriately complex for what it does
3. Re-examine the paper/spec to see if a simpler algorithm exists

---

## Phase 3: Review well_typed_term.dart (PENDING)

Will analyze after Phase 2 decision is made.

---

## Phase 4: Review well_typed_clause.dart (PENDING)

Will analyze after Phase 2 decision is made.

---

## Phase 5: Full Test Suite Validation

**Goal**: Ensure all tests pass after simplification

### Current Test Status

**Baseline (Phase 1)**: 218/302 tests passing (72.2%)

---

## Files Kept Unchanged

| File | Reason |
|------|--------|
| type_ast.dart | Solid type definition infrastructure |
| program_dfa.dart | Correctly implements Definitions 5.11-5.13 |
| type_checker.dart | Good orchestration layer |
| prelude.dart | Standard library definitions |
| type_conversion.dart | AST conversion utilities |

---

## Progress Log

### 2026-01-23 (continued)

- **Phase 2 design review**: Analyzed moded_head.dart
  - Found `_flipAllVariables()` is dead code — can remove
  - Original ~100 line estimate was too optimistic
  - Most complexity is inherent in type-driven mode propagation
  - Realistic target: ~290 lines (from 350)
  - **Decision needed**: proceed with modest cleanup or re-examine approach

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
- **Revised**: Not all code can be radically simplified. Some complexity is inherent in the type-driven mode propagation required by Definition 5.5.
