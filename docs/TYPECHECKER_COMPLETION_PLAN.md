# Type Checker Completion Plan

**Version**: 1.0  
**Date**: 2025-01-11  
**Status**: DRAFT

---

## 1. Current State Assessment

### 1.1 What Works

The parser integration is complete. The main parser now handles type definitions, procedure declarations, and clauses. The prelude is parsed correctly with all declarations before clauses.

The core type checking modules exist and have specifications:
- `type-environment.md` v0.5 — Type and procedure declarations
- `type-dfa.md` v0.9 — DFA construction with complement states
- `moded-term.md` v0.6 — Moded term representation
- `moded-head.md` v0.7 — Moded head construction with conditional variable adjustment
- `well-typed-term.md` v0.5 — Path consistency checking
- `well-typed-clause.md` v0.6 — Clause well-typing (three conditions)
- `well-typed-program.md` v0.5 — Program well-typing (covariance + contravariance)

### 1.2 What Is Broken

The Dart unit tests in `test/analysis/type_checker/` are fundamentally flawed:
- Many tests use programs that violate SRSW (unpaired variables)
- Tests were written with incorrect assumptions about spec behavior
- Tests mix concerns (testing internal functions vs. testing type checker behavior)
- No clear traceability to spec or paper

The test failures we observe are not implementation bugs but invalid test cases.

### 1.3 Root Cause

Tests were written before the specification was stable, or were written based on assumptions rather than the spec. Per the development discipline, tests must be derived from specifications.

---

## 2. Testing Strategy

### 2.1 Abandon Dart Unit Tests

Delete all Dart unit tests in `test/analysis/type_checker/`. These test internal implementation details using invalid programs.

### 2.2 GLP File Tests

Replace with `.glp` file tests that are:
- **Valid GLP programs** that pass SRSW checking
- **Organized as positive/negative pairs** for each feature
- **REPL-style** — each test is a standalone `.glp` file
- **Traceable** — filename indicates what is being tested

### 2.3 Test Directory Structure

```
programs/tests/typechecker/
  positive/                    # Programs that should type-check successfully
    merge_basic.glp           # Paper example: merge with Stream
    merge_weak_types.glp      # merge with _? weaker types
    new_channel.glp           # Interactive type example
    dl_append.glp             # Difference list operations
    monitor.glp               # Hollow message example
    bounded_buffer.glp        # HollowStream example
    coop_stream.glp           # Cooperative stream construction
    ...
  negative/                    # Programs that should fail type-checking
    merge_wrong_modes.glp     # merge with all output modes
    merge_missing_clause.glp  # merge missing nil case (contravariance)
    type_mismatch.glp         # Integer at Stream position (covariance)
    non_complementary.glp     # Variable pair with non-complementary types
    ...
```

### 2.4 Test File Format

Each `.glp` test file is a complete, self-contained program:

```
% test: merge_basic
% expected: pass
% description: Basic merge program from paper is well-typed

Stream ::= [] ; [_|Stream].

procedure merge(Stream?, Stream?, Stream).

merge([], Ys, Ys?).
merge(Xs, [], Xs?).
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
```

For negative tests:

```
% test: merge_missing_clause
% expected: fail
% expected_error: coverage
% description: Missing nil case for first argument fails contravariance

Stream ::= [] ; [_|Stream].

procedure merge(Stream?, Stream?, Stream).

merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
```

### 2.5 Test Harness

Create a shell script that:
1. Runs each `.glp` file through the type checker
2. Compares actual result (pass/fail) against expected
3. Reports successes and failures
4. Provides verbose output for debugging

```bash
#!/bin/bash
# run_typechecker_tests.sh

POSITIVE_DIR="programs/tests/typechecker/positive"
NEGATIVE_DIR="programs/tests/typechecker/negative"

pass=0
fail=0

for f in "$POSITIVE_DIR"/*.glp; do
  result=$(dart run bin/check_types.dart "$f" 2>&1)
  if echo "$result" | grep -q "well-typed"; then
    ((pass++))
  else
    echo "FAIL (expected pass): $f"
    echo "$result"
    ((fail++))
  fi
done

for f in "$NEGATIVE_DIR"/*.glp; do
  result=$(dart run bin/check_types.dart "$f" 2>&1)
  if echo "$result" | grep -q "Type Error\|Coverage Error"; then
    ((pass++))
  else
    echo "FAIL (expected fail): $f"
    echo "$result"
    ((fail++))
  fi
done

echo "Passed: $pass, Failed: $fail"
```

---

## 3. Implementation Tasks

### 3.1 Phase 1: Triage Existing Dart Tests

Review each Dart test file and categorize:

**Category A: Translate to GLP** — Test logic is valid, but expressed using invalid SRSW programs or internal APIs. Rewrite as valid `.glp` file tests.

**Category B: Discard** — Test is fundamentally flawed (tests internal implementation details, based on incorrect spec understanding, or cannot be expressed as valid GLP).

For each test file in `test/analysis/type_checker/`:
1. Read the test
2. Identify what behavior it is testing
3. Determine if that behavior can be tested via a valid GLP program
4. If yes → write the equivalent `.glp` test (positive and negative pair)
5. If no → document why and discard

After triage:
1. **Delete triaged Dart test files**
2. **Delete obsolete files**:
   - `type_parser.dart` (replaced by main parser integration)
   - `type_lexer.dart` (if exists)
3. **Keep implementation files** — they are needed
4. **Verify parser integration** — run existing book programs to confirm parsing works

### 3.2 Phase 2: Create Test Infrastructure

1. **Create directory structure**:
   ```
   programs/tests/typechecker/positive/
   programs/tests/typechecker/negative/
   ```

2. **Create test harness script**: `bin/run_typechecker_tests.sh`

3. **Create initial test files** from paper examples:
   - `positive/merge_basic.glp` — paper merge example
   - `negative/merge_missing_clause.glp` — merge without nil cases

### 3.3 Phase 3: Verify Implementation Against Spec

Work through each spec module and verify the implementation matches:

**3.3.1 type-dfa.md** — Verify `program_dfa.dart`:
- DFA states are created correctly (complement pairs)
- Transitions encode functor/arity/argIndex/mode
- Complement automata have flipped modes

**3.3.2 moded-head.md** — Verify `moded_head.dart`:
- `modedHead()` builds I/O moded term correctly
- Step 2 adjusts variables conditionally (not unconditionally)
- `producedTerm()` does not adjust variables

**3.3.3 well-typed-term.md** — Verify `well_typed_term.dart`:
- Path consistency uses Mode Correspondence Property
- Reader at ↓ position: consistent
- Writer at ↑ position: consistent
- Complementarity check uses `DFAState.baseName` and `isComplement`

**3.3.4 well-typed-clause.md** — Verify `well_typed_clause.dart`:
- Condition 1: Head checked per argument against declared type automaton
- Condition 2: Body atoms checked as produced terms
- Condition 3: Variable pairs complementary across entire clause

**3.3.5 well-typed-program.md** — Verify `type_checker.dart`:
- Covariance: All clauses well-typed
- Contravariance: All input paths covered by some clause

### 3.4 Phase 4: Build Test Suite

For each feature, create paired positive/negative tests:

**Covariance tests:**
- Positive: Well-typed clause (all three conditions)
- Negative: Wrong type at argument position
- Negative: Non-complementary variable types

**Contravariance tests:**
- Positive: All input alternatives covered
- Negative: Missing clause for some alternative

**Interactive types tests:**
- Positive: new_channel with correct modes
- Negative: new_channel with incorrect modes

**Predefined operations tests:**
- Positive: dl_append well-typed
- Negative: dl_append with wrong modes

**System types tests:**
- Positive: Integer constant at Integer position
- Negative: String constant at Integer position

### 3.5 Phase 5: Fix Bugs

As tests reveal bugs:
1. Stop and investigate
2. Trace through spec to understand expected behavior
3. If implementation differs from spec, fix implementation
4. If spec is unclear, clarify spec first
5. Add the test to permanent suite

---

## 4. Test Cases from Paper

### 4.1 Positive Cases (Should Pass)

From the paper's examples:

1. **merge** (lines 12-15, 170-174): Stream merging with `Stream?`, `Stream?`, `Stream`
2. **monitor** (lines 177-184): CounterCall with hollow message `read(Integer?)`
3. **bounded_buffer** (lines 193-205): HollowIntegers consumer/producer
4. **coop_stream** (lines 208-225): CoopStream with `[switch|CoopStream]?`
5. **dl_append** (lines 228-232): DiffList operations
6. **new_channel** (lines 234-245): Bidirectional channels

### 4.2 Negative Cases (Should Fail)

From the paper's invalid examples:

1. **merge with wrong declaration** (line 160): `procedure merge(Stream,Stream,Stream)` — all output modes
2. **merge with all inputs** (line 161): `procedure merge(Stream?,Stream?,Stream?)` — all input modes
3. **merge with primitives wrong** (line 162): `procedure merge(_,_,_)` — primitives need correct orientation
4. **Overlapping alternatives** (lines 86-89): `Any ::= _ ; _?` — ambiguous
5. **Type aliases** (lines 91-95): `Output ::= _` — illegal alias

---

## 5. Success Criteria

The type checker is complete when:

1. **All positive tests pass** — well-typed programs are accepted
2. **All negative tests fail appropriately** — ill-typed programs are rejected with correct error type
3. **Paper examples work** — all examples from typed-glp.tex behave as described
4. **Book programs** — 82%+ of typed_book programs pass (pre-existing SRSW violations excluded)
5. **No workarounds** — implementation follows spec exactly

---

## 6. Non-Goals

The following are explicitly out of scope for this plan:

- Parametric types (polymorphism) — mentioned in paper but not implemented
- Type inference — programmer must provide declarations
- Defined guards type checking — guards treated as procedure calls
- Performance optimization — correctness first

---

## 7. Execution Order

1. Delete Dart unit tests (Phase 1)
2. Create test infrastructure (Phase 2)
3. Create initial test files from paper (Phase 2)
4. Run tests, observe failures
5. For each failure:
   - Read relevant spec section
   - Trace through implementation
   - Fix implementation to match spec (Phase 3)
   - Verify test passes
6. Add more tests incrementally (Phase 4)
7. Continue until all tests pass (Phase 5)

---

## 8. Files to Modify

### Delete

```
test/analysis/type_checker/*.dart (all files)
lib/analysis/type_checker/type_parser.dart (obsolete)
lib/analysis/type_checker/type_lexer.dart (if exists)
```

### Create

```
programs/tests/typechecker/positive/*.glp
programs/tests/typechecker/negative/*.glp
bin/run_typechecker_tests.sh
```

### Potentially Modify (based on test results)

```
lib/analysis/type_checker/type_checker.dart
lib/analysis/type_checker/well_typed_clause.dart
lib/analysis/type_checker/well_typed_term.dart
lib/analysis/type_checker/moded_head.dart
lib/analysis/type_checker/program_dfa.dart
```

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2025-01-11 | Initial plan |
