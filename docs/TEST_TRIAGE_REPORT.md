# Test Triage Report

**Version**: 1.0  
**Date**: 2025-01-11

## Summary

| Category | Count | Action |
|----------|-------|--------|
| A: Translate to GLP | 5 files | Extract valid test cases to `.glp` files |
| B1: Internal API tests | 5 files | Keep as Dart unit tests (test internal modules) |
| B2: Discard | 6 files | Delete (SRSW violations, debug files, obsolete) |
| B3: Skipped tests | 3 files | Delete (all tests skipped, not useful) |

---

## Category A: Translate to GLP

These files contain valid integration tests that should be converted to `.glp` file tests.

### 1. type_checker_new_test.dart

**What it tests**: End-to-end type checking via `checkTypes()` helper.

**Valid positive tests to extract**:
- `simple list append` — MyList append with Stream?
- `merge example from paper` — Paper merge example
- `fact clause only` — nat(0) with no body
- `single output argument` — yes(mytrue)
- `binary tree mirror` — Tree mirror
- `both arguments covered` — MyBool and operation
- `variable at input covers all alternatives` — copy(X, X?)

**Valid negative tests to extract**:
- `wrong constant at output position` — 42 at Stream position
- `wrong functor in head` — foo(X) at Nat position
- `missing nil case for list` — merge without [] case (contravariance)
- `missing cons case for list` — length without [|] case
- `missing zero case for Nat` — pred without 0 case
- `missing successor case for Nat` — iszero without s case
- `first argument incomplete` — myand missing myfalse cases

### 2. predefined_operations_test.dart

**What it tests**: DiffList and Channel operations, primitive modes.

**Valid positive tests to extract**:
- `Output position with reader` — procedure produce(_). produce(X?).
- `Input position with writer` — procedure consume(_?). consume(X).
- `List copy` — MyList copy with _ elements
- `dl_append is well-moded` — DiffList append
- `dl_to_list is well-moded` — DiffList to list conversion

**Valid negative tests to extract**:
- `dl_append with wrong modes` — Inverted modes fail

### 3. system_types_test.dart

**What it tests**: System types (List, Stream).

**Valid positive tests to extract**:
- `Complete list procedure` — MyList length with both cases
- `Stream handler with complete coverage` — MyList process

**Valid negative tests to extract**:
- `Missing [] case` — length without base case
- `Missing cons case` — length without recursive case

### 4. primitive_mode_coverage_test.dart

**What it tests**: Primitive mode coverage (_, _?).

**Valid positive tests to extract**:
- `Single _ position` — produce(X?)
- `Single _? position` — consume(X)
- `List with _ elements` — copy with two clauses
- `Nested struct with single-mode primitives` — Pair swap

**Valid negative tests to extract**:
- `Wrong mode at output position` — produce(X) [wrong]
- `Wrong mode at input position` — consume(X?) [wrong]

### 5. well_typed_clause_test.dart

**What it tests**: Clause well-typing conditions via internal API.

**Test scenarios that can be converted to GLP**:
- `reader at output position` — foo(X?) with procedure foo(_).
- `writer at input position` — bar(X) with procedure bar(_?).
- `append with head and body` — Full append example
- `X? and X at complementary positions in Pair` — pair(X?, X)
- `X and X? at different types NOT complementary` — mismatch(X?, X) with Nat, Stream

---

## Category B1: Internal API Tests (Keep as Dart Unit Tests)

These test internal module behavior. They are valid and useful for testing implementation correctness.

### 1. moded_term_test.dart
Tests `ModedVariable`, `ModedConstant`, `ModedCompound`, `paths()`, `complement()`, `isConsumed()`, `isProduced()`, `isIO()`.

**Action**: Keep. Tests moded term data structures per spec.

### 2. moded_head_test.dart
Tests `modedHead()` and `producedTerm()` functions.

**Action**: Keep. Tests moded head construction per spec.

### 3. well_typed_term_test.dart
Tests `checkModedTerm()` and `checkPathAgainstAutomaton()`.

**Action**: Keep. Tests path consistency checking per spec.

### 4. program_dfa_test.dart
Tests `DFAState`, `TransitionLabel`, `buildProgramDFA()`, `checkLeafConsistency()`.

**Action**: Keep. Tests DFA construction per spec.

### 5. prelude_test.dart
Tests prelude parsing and predefined types.

**Action**: Keep. Infrastructure tests for prelude correctness.

---

## Category B2: Discard

### 1. primitive_state_modes_test.dart

**Problem**: SRSW violations throughout. Uses single-variable clauses:
- `copy(X?)` — no paired X
- `echo(X)` — no paired X?
- `bad_copy(X)` — no paired X?
- `bad_echo(X?)` — no paired X

These cannot be converted to valid GLP programs.

**Action**: Delete entirely.

### 2. debug_dl.dart
Debug file.

**Action**: Delete.

### 3. debug_single_mode_test.dart
Debug file.

**Action**: Delete.

### 4. moded_type_parser_test.dart
Tests obsolete type_parser.dart which is replaced by main parser integration.

**Action**: Delete.

### 5. nested_any_coverage_test.dart
Tests obsolete Any/Every types that no longer exist.

**Action**: Delete.

### 6. primitive_mode_test.dart
Likely overlaps with primitive_mode_coverage_test.dart or has SRSW issues.

**Action**: Review and delete if redundant.

---

## Category B3: Skipped Tests (Delete)

These files have all or most tests skipped. They provide no current value.

### 1. channel_types_test.dart
All tests skipped: "Nested type mode handling not yet implemented"

**Action**: Delete. Recreate when feature is implemented.

### 2. guard_types_test.dart
Most tests skipped: Various "not yet implemented" reasons.

**Action**: Delete. Recreate when feature is implemented.

### 3. defined_guards_test.dart
Most tests skipped: "Defined guard type checking not yet implemented"

**Action**: Delete. Recreate when feature is implemented.

---

## Files to Keep

- test_helpers.dart (needed by remaining Dart tests)
- moded_term_test.dart
- moded_head_test.dart
- well_typed_term_test.dart
- program_dfa_test.dart
- prelude_test.dart

---

## Test Count Summary

**GLP file tests to create**:
- Positive: ~15 tests
- Negative: ~12 tests

**Dart unit tests to keep**: 6 files

**Files to delete**: ~13 files
