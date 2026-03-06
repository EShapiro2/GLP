# Type Checker Implementation Plan

**Created**: 2026-01-23
**Status**: Phase 7 NOT STARTED - Parameterized type expansion
**Paper**: sections/well-typing.tex (Definition `moded-head` = Definition 5.5)

## Overview

The paper underwent major mathematical simplification. Specifications have been rewritten from scratch (see archive/specs-2026-01-23/ for old versions). This plan tracks implementation alignment with the new specs.

## Phase Summary

| Phase | Description | Status | Notes |
|-------|-------------|--------|-------|
| 1 | Terminology: complement → dual | ✅ COMPLETE | 218/302 tests, no regression |
| 2 | Clean up moded_head.dart | ✅ COMPLETE | Dead code removed, refs updated |
| 3 | Unit tests for moded_head.dart | ✅ COMPLETE | 10/10 tests passing |
| 4 | Review well_typed_term.dart | ✅ COMPLETE | Refs updated, 7 tests added |
| 5 | Review well_typed_clause.dart | ✅ COMPLETE | Refs updated, 6 tests added |
| 6 | Full test suite validation | ✅ COMPLETE | No regressions, 23 unit tests |
| 7 | Parameterized type expansion | NOT STARTED | Preprocessing before automaton construction |

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

## Phase 3: Unit Tests for moded_head.dart

**Status**: READY  
**Spec**: docs/type system/moded-head.md  
**Paper**: Definition 5.5

### Gap Analysis

The directory `glp_runtime/test/analysis/type_checker/` is **empty**. Per discipline §2.4, unit tests must exist for all implementations.

### Required Tests

Create `glp_runtime/test/analysis/type_checker/moded_head_test.dart` with:

1. **modedHead() basic construction**
   - Positive: merge head with correct type → correct moded head
   - Positive: monitor head with interactive type → correct mode flips
   - Negative: arity mismatch → ArityMismatchError

2. **producedTerm() for body atoms**
   - Positive: body atom has mode ↑ throughout (no variable flip)
   - Negative: arity mismatch → ArityMismatchError

3. **Variable replacement (Step 2)**
   - Positive: writer at ↓ position → replaced with reader
   - Positive: reader at ↑ position → replaced with writer
   - Positive: reader at ↓ position → unchanged
   - Positive: writer at ↑ position → unchanged

4. **Nested mode propagation**
   - Positive: Stream? element has ↓ mode
   - Positive: Stream element has ↑ mode
   - Positive: Interactive type flips correctly (e.g., CounterCall)

5. **Anonymous variables**
   - Positive: each _ generates unique fresh writer name
   - Positive: _ at ↓ position becomes fresh reader in moded head

### Task for Claude Code

```
Create file: glp_runtime/test/analysis/type_checker/moded_head_test.dart

Spec: docs/type system/moded-head.md

Test the modedHead() and producedTerm() functions from
lib/analysis/type_checker/moded_head.dart

Cover:
1. Basic moded head construction for merge procedure
2. Body atom construction (producedTerm)
3. Variable replacement based on structural mode
4. Nested mode propagation through type structure
5. Anonymous variable uniqueness

Use examples from the spec and paper Appendix A.

Run: cd glp_runtime && dart test test/analysis/type_checker/
```

---

## Phase 4: Review well_typed_term.dart

**Status**: READY  
**Spec**: docs/type system/well-typed-term.md  
**Paper**: Definition 5.4

### Analysis

The implementation correctly implements Definition 5.4:
1. Checks each term path against automaton for consistency
2. Records variable types during path traversal
3. Verifies variable pairs have dual types

### Changes Needed

1. **Update paper reference** in file header:
   - Change: "Definition 4.5 (Consistent Paths), Definition 4.7 (Well-Typed Moded Term)"
   - To: "Definition 5.4 (Well-Typed Moded Term)"

2. **Add unit tests** per discipline §2.4

### Task for Claude Code (Part A: Update Reference)

```
In glp_runtime/lib/analysis/type_checker/well_typed_term.dart:

Change the file header comment from:
// Paper Reference: Definition 4.5 (Consistent Paths), Definition 4.7 (Well-Typed Moded Term)

To:
// Paper Reference: Definition 5.4 (Well-Typed Moded Term)

Run: cd glp_runtime && dart analyze lib/analysis/type_checker/well_typed_term.dart

Commit:
git add -A
git commit -m "docs(well_typed_term): update paper reference to Definition 5.4"
git push
```

### Task for Claude Code (Part B: Add Unit Tests)

Create file: glp_runtime/test/analysis/type_checker/well_typed_term_test.dart

Spec: docs/type system/well-typed-term.md

Tests should cover:
1. Simple well-typed term (constant at correct type position)
2. Variable at wildcard position (should be well-typed)
3. Variable pair with dual types (should be well-typed)
4. Variable pair with non-dual types (should fail)
5. Path with no matching transition (should fail)
6. Mode mismatch at variable position (should fail)

Run: cd glp_runtime && dart test test/analysis/type_checker/

Commit when tests pass.

---

## Phase 5: Review well_typed_clause.dart

**Status**: Pending Phase 3 completion

Will analyze against Definition `well-typed-clause` in paper.

---

## Phase 6: Full Test Suite Validation ✅ COMPLETE

**Completed**: 2026-01-23

### Final Test Results

| Suite | Passed | Failed | Notes |
|-------|--------|--------|-------|
| Dart unit tests | 328 | 18 | Type checker tests (23) all pass |
| Full REPL | 222 | 1 | Pre-existing "Time advances" failure |
| Typechecker REPL | 139 | 83 | Consistent with baseline |

**No regressions from Phases 1-5.** All 23 type checker unit tests pass:
- moded_head_test.dart: 10 tests
- well_typed_term_test.dart: 7 tests
- well_typed_clause_test.dart: 6 tests

The 18 Dart unit test failures and 1 REPL test failure are pre-existing issues unrelated to the type system revision work.

---

## Progress Log

### 2026-01-23 (Phase 6)

- **Phase 6 complete**: Full test suite validation
  - Dart unit tests: 328/346 (23 type checker tests all pass)
  - Full REPL: 222/223 (pre-existing failure)
  - Typechecker REPL: 139/222 (consistent with baseline)
  - No regressions from Phases 1-5

### 2026-01-23 (Phases 4-5)

- **Phase 4 complete**: Updated well_typed_term.dart reference, added 7 unit tests
- **Phase 5 complete**: Updated well_typed_clause.dart reference, added 6 unit tests

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

---

## Phase 7: Parameterized Type Expansion

**Status**: NOT STARTED
**Paper Reference**: Section 8, Definition 8.1
**Spec**: `docs/type system/typed-program.md`, section "Parameterized Types"

### Overview

Parameterized types are syntactic sugar. A preprocessing step expands all parameterized type definitions and references into monomorphic equivalents before type automaton construction. After expansion, all existing machinery (automaton, well-typing, subtyping) applies without modification.

### Implementation Location

New file: `glp_runtime/lib/analysis/type_checker/param_expansion.dart`

This runs after parsing and before type automaton construction. The entry point should accept the parsed program (type definitions + procedure declarations) and return a transformed program with only monomorphic types.

### Algorithm (5 steps)

1. **Collect templates.** Scan type definitions. A definition with parameters (e.g., `Stream(X)`) is a template. Record it; do not add to type environment.

2. **Collect instantiations.** Scan all type definitions (including bodies), procedure declarations (`procedure`, `imported procedure`, `exported procedure`) for parameterized type references like `Stream(Integer)`. Record each distinct instantiation.

3. **Expand.** For each instantiation, generate a fresh monomorphic type definition by substituting parameters. Recursive self-references become the fresh name. Nested instantiations expand inside-out.

4. **Replace references.** In all type definitions and procedure declarations, replace `T(S₁,...,Sₖ)` with the expanded name `T<S₁,...,Sₖ>`.

5. **Remove templates.** Remove parameterized type definitions. Only expanded monomorphic definitions remain.

### Parameterized Procedure Declarations

When a procedure is declared with type parameters (e.g., `merge(Stream(X)?, Stream(X)?, Stream(X))`), the type checker must infer the instantiation at each call site by structural matching of the declared parameterized types against the concrete types from the call context. Conflicting bindings are an error.

### What Does NOT Change

- Type automaton construction (`type-automaton.md`)
- Moded head construction (`moded-head.md`)
- Well-typed term checking (`well-typed-term.md`)
- Well-typed clause checking (`well-typed-clause.md`)
- Well-typed program checking (`well-typed-program.md`)
- Subtyping (`subtyping.md`)
- Consistent paths (`consistent-paths.md`)

### Test Strategy

New test programs in `programs/tests/typed/`:

1. **Positive**: `Stream(Integer)` with merge — basic expansion
2. **Positive**: `Channel(Msg, Msg)` — mode annotations preserved
3. **Positive**: Nested `Stream(Pair(Integer, String))` — inside-out expansion
4. **Positive**: Parameterized procedure declaration with inference
5. **Negative**: Conflicting type parameter bindings at call site
6. **Negative**: Wrong arity in instantiation (e.g., `Stream(A, B)`)
7. **Positive**: Module interaction — imported procedure with instantiated parameters

---

## Notes

- Always read LaTeX source of paper, not PDF assumptions
- String errors are sufficient (no elaborate error class hierarchies needed)
- Not all code can be radically simplified - some complexity is inherent in the problem
