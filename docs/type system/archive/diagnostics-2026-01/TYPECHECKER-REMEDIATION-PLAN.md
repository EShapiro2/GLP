# Type Checker Remediation Plan

**Created**: 2026-01-23  
**Baseline**: 139/222 passing (83 failures)  
**Target**: 222/222 passing  

## Overview

The 83 failing tests fall into four categories requiring different remediation approaches:

| Category | Count | Root Cause | Remediation |
|----------|-------|------------|-------------|
| A: SRSW Violations | ~35 | Source programs violate SRSW | Fix .glp source files |
| B: Mode Mismatch | ~30 | Type checker bugs | Fix type_checker implementation |
| C: Coverage Gaps | ~5 | Missing type alternatives | Fix programs or checker |
| D: Missing/Unknown | ~4 | Missing files, unclear errors | Investigate and fix |

---

## Phase 1: Quick Fixes (Category D)

**Target**: Fix 4 tests  
**Effort**: Low

### 1.1 Missing File: positive/paper/merge.glp

The test references a file that doesn't exist:
```
/home/user/GLP/glp_runtime/test/programs/typechecker/positive/paper/merge.glp
```

**Action**: Create the file or remove from test list. Check if this should reference an existing merge.glp.

### 1.2 Unknown Failures: ancestor.glp, heapify.glp

These show "unknown failure" without clear error messages.

**Action**: Run manually with verbose output to diagnose:
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart run bin/typed_glp_repl.dart programs/typed_book/recursive/structure_processing/ancestor.glp 2>&1
dart run bin/typed_glp_repl.dart programs/typed_book/recursive/structure_processing/heapify.glp 2>&1
```

---

## Phase 2: Type Checker Bugs (Categories B & C)

**Target**: Fix ~35 tests  
**Effort**: Medium-High

### 2.1 Critical Bug: `=` Procedure Mode Declaration

**Symptom**: All programs using `X = Y?` fail with mode mismatch errors like:
```
Variable mode mismatch: writer requires ↑ (produce), got ↓ (consume)
Path: (Z, 0, input)
Variable mode mismatch: reader requires ↓ (consume), got ↑ (produce)
Path: (X?, 0, output)
```

**Analysis**: The error indicates argument 0 is treated as input (↓) and argument 1 as output (↑). But the correct declaration per paper section B.4 is:
```
procedure =(_, _?).
```
Which means: argument 0 is output (↑), argument 1 is input (↓).

**Action**: Check `prelude.dart` for the `=` procedure declaration. If incorrect, fix it. If correct, the bug is in how body atoms apply argument modes.

**Files to check**:
- `/Users/udi/Grassroots/GLP/glp_runtime/lib/analysis/type_checker/prelude.dart`
- `/Users/udi/Grassroots/GLP/glp_runtime/lib/analysis/type_checker/well_typed_clause.dart`

### 2.2 Body Atom Mode Application

**Symptom**: Multiple body atoms fail mode checking even when modes appear correct.

**Analysis**: Per Definition 5.7 (condition 2):
> For each atom A ∈ B, the produced moded term A' corresponding to A is well-typed by D.

Body atoms have root mode ↑ (produce). But argument modes must follow the procedure declaration - input arguments get ↓, output arguments get ↑.

**Action**: Review `producedTerm()` in `moded_head.dart` to verify it correctly applies:
- Root mode ↑ (produce) for the atom itself
- Argument modes per the procedure declaration (↓ for input, ↑ for output)

### 2.3 Coverage Gap Errors

**Symptom**: 
```
uncovered alternative "[]" at path: Stream → []
```

**Analysis**: Some procedures don't have clauses covering all type alternatives. This is a valid type error if the program is incomplete, but may indicate overly strict checking if the program handles the case indirectly.

**Action**: For each coverage gap error:
1. Check if the program genuinely lacks coverage (fix the program)
2. Or if the checker is wrong (fix the checker)

**Files affected**: bounded_buffer.glp, close procedure

---

## Phase 3: SRSW Source Program Fixes (Category A)

**Target**: Fix ~35 tests  
**Effort**: High (many files)

### 3.1 Understanding SRSW Violations

Per paper Definition 3.2, valid GLP clauses must satisfy:
1. **Single Occurrence (SO)**: Every variable occurs at most once
2. **SRSW**: A variable occurs in a clause iff its paired variable also occurs

Relaxation: If a guard proves a variable is ground (e.g., `number(X?)`), multiple reader occurrences are allowed.

### 3.2 Common Violation Patterns

**Pattern 1: Multiple Writer Occurrences**
```
WRONG:  foo(X, X) :- ...           % X appears twice
RIGHT:  foo(X, X?) :- ...          % Writer and reader paired
```

**Pattern 2: Writer Without Reader**
```
WRONG:  foo(X) :- bar(Y).          % X has no reader, Y has no writer
RIGHT:  foo(X) :- bar(X?).         % Proper pairing
```

**Pattern 3: Multiple Readers Without Ground Guard**
```
WRONG:  foo(X, R?) :- R = f(X?, X?).  % X? appears twice
RIGHT:  foo(X, R?) :- ground(X?) | R = f(X?, X?).  % Ground guard allows multiple readers
```

### 3.3 Files Requiring SRSW Fixes

**Group 1: Arithmetic Trees (7 files)**
- `factorial.glp` - Line 20: Variable "T" has no reader
- `fibonacci.glp` - SRSW violation (details TBD)
- `lesseq.glp` - SRSW violation
- `hanoi.glp` - SRSW violation
- `min.glp` - SRSW violation
- `primes.glp` - SRSW violation
- `length.glp` - SRSW violation

**Group 2: Streams (10 files)**
- `channels.glp` - Multiple violations (Left2, Channel2, Message)
- `cooperative_producers.glp` - SRSW violation
- `dynamic_merger.glp` - SRSW violation
- `parallel_table.glp` - SRSW violation
- `bounded_buffer_original.glp` - SRSW violation
- `network_switch.glp` - SRSW violation
- `network_switch_3way.glp` - SRSW violation
- `observed_monitor.glp` - SRSW violation
- `play_absolute.glp` - SRSW violation

**Group 3: Social Graph (13 files)**
- `play_4agent.glp` - SRSW violation
- `play_4agents.glp` - SRSW violation
- `play_alice_bob.glp` - SRSW violation
- `play_cold_call.glp` - SRSW violation
- `play_introduction.glp` - SRSW violation
- `plays/play01_cold_call/alice.glp` - SRSW violation
- `plays/play01_cold_call/bob.glp` - SRSW violation
- `plays/play01_cold_call/main.glp` - SRSW violation
- Plus files that have both SRSW and type errors

**Group 4: Social Networks (9 files)**
- `dm_simple.glp` - SRSW violation
- `feed.glp` - SRSW violation
- `feed_server.glp` - SRSW violation
- `follower_mgmt.glp` - SRSW violation
- `group_formation.glp` - SRSW violation
- `group_messaging.glp` - SRSW violation
- `interlaced_streams.glp` - SRSW violation
- `replicate.glp` - SRSW violation

**Group 5: Cryptocurrencies (1 file)**
- `gc.glp` - SRSW violation

---

## Phase 4: Combined SRSW + Type Error Files

**Target**: Fix files with both SRSW violations AND type errors  
**Approach**: Fix SRSW first (enables loading), then address any remaining type errors

These files appear in both Category A and B:
- `agent.glp`
- `bounded_buffer.glp`
- Several social_graph files

---

## Execution Order

### Step 1: Diagnose and Fix Quick Issues (Phase 1)
- [ ] Create or fix `positive/paper/merge.glp`
- [ ] Diagnose `ancestor.glp` and `heapify.glp`

### Step 2: Fix Type Checker Bugs (Phase 2)
- [ ] Verify/fix `=` procedure declaration in prelude.dart
- [ ] Verify body atom mode application in well_typed_clause.dart
- [ ] Run type checker unit tests to ensure no regressions
- [ ] Re-run typechecker REPL tests to measure progress

### Step 3: Fix SRSW-Only Programs (Phase 3)
- [ ] Fix arithmetic_trees programs (7 files)
- [ ] Fix streams programs (10 files)
- [ ] Fix social_graph SRSW-only programs
- [ ] Fix social_networks programs (9 files)
- [ ] Fix cryptocurrencies/gc.glp

### Step 4: Fix Combined Programs (Phase 4)
- [ ] Fix remaining files with both issues

### Step 5: Final Validation
- [ ] Run full typechecker REPL test suite
- [ ] Verify 222/222 passing
- [ ] Update implementation plan

---

## Success Criteria

- All 222 typechecker REPL tests pass
- No regressions in other test suites (dart tests, REPL tests)
- All changes traced to spec sections

---

## Files Reference

**Type Checker Implementation**:
- `glp_runtime/lib/analysis/type_checker/prelude.dart` - Builtin declarations
- `glp_runtime/lib/analysis/type_checker/well_typed_clause.dart` - Clause checking
- `glp_runtime/lib/analysis/type_checker/moded_head.dart` - Moded term construction

**Test Programs**:
- `glp_runtime/test/programs/typechecker/` - Type checker test files
- `glp_runtime/test/programs/moded_types/` - Moded type test files  
- `programs/typed_book/` - Typed book programs

**Test Script**:
- `test/run_typechecker_repl_tests.sh`
