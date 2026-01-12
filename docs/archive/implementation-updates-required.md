# Required Implementation Updates

**Created**: 2025-12-18
**Status**: Blocking book program fixes
**Priority**: High

## Summary

During systematic fixing of book programs, parser limitations and test system gaps were discovered that block progress on 19+ files.

---

## Parser Enhancements Required

### P1: Equality Test Operator `=?=`

**Status**: Not implemented
**Spec Reference**: docs/SPEC_GUIDE.md defines `'=?=' → ARITH_EQUAL` at precedence 700
**Files Blocked**: biased_merge.glp, consensus.glp
**Usage Example**:
```prolog
Agent? =?= Leader? | true.
```

### P2: Negation Wrapper `~()`

**Status**: Not implemented
**Spec Reference**: docs/SPEC_GUIDE.md mentions negated equality guard `~(X? =?= Z?)`
**Files Blocked**: biased_merge.glp, consensus.glp
**Usage Example**:
```prolog
~(M? =?= started), ~(M? =?= halted) | ...
```

### P3: Not-Unifiable Operator `\=`

**Status**: Not implemented (lexer rejects backslash)
**Spec Reference**: docs/SPEC_GUIDE.md defines `'\\=' → NOT_UNIFIABLE`
**Files Blocked**: agent_full.glp
**Usage Example**:
```prolog
Target? \= Other? | ...
```

### P4: Negation-as-Failure `\+`

**Status**: Not implemented (lexer rejects backslash)
**Spec Reference**: Standard Prolog operator
**Files Blocked**: attestation_guards.glp
**Usage Example**:
```prolog
\+ trusted_module(Module?) | ...
```
**Note**: Semantics of negation-as-failure in committed-choice language needs discussion.

### P5: Include Directive `:- include(file).`

**Status**: Not implemented
**Spec Reference**: Standard Prolog/FCP directive for textual inclusion
**Files Blocked**: 10 files
- cryptocurrencies/: play_mutual_credit.glp, play_payment.glp, play_redemption.glp, test_balance.glp, test_repayments.glp
- constitutional_consensus/: play_agents.glp, play_high_throughput.glp, play_low_throughput.glp, test_blocklace.glp, test_waves.glp

**Usage Example**:
```prolog
:- include(gc).
:- include(consensus).
```

### P6: Tuple-in-List Syntax

**Status**: Possibly not implemented
**Files Blocked**: group_formation.glp
**Usage Example**:
```prolog
[(Friend,Ch1?)|Fs2?]
```
**Error**: "Expected ) after arguments"

---

## Test System Enhancement Required

### T1: Multiple-Arity Predicate Support

**Status**: Test system rejects files with same predicate name at different arities
**Files Blocked**: 4 files (sum_list.glp, maxlist.glp, inner_product_iter.glp, monitor.glp)
**Pattern**: Standard accumulator pattern with wrapper predicate

```prolog
% Wrapper (external interface)
sum(Xs, S?) :- sum(Xs?, 0, S).

% Accumulator (internal)
sum([], Acc?, Acc?).
sum([X?|Xs], Acc, S?) :- Acc1 := Acc? + X?, sum(Xs?, Acc1?, S).
```

**Error**: "Clause for sum/3 found, expected sum/2"

**Impact**: This is a fundamental GLP pattern used throughout the book. The test system should recognize that predicates can have multiple arities.

---

## Implementation Priority

Based on file count and pattern importance:

1. **T1: Multiple-arity support** — Unblocks 4 files, fundamental pattern
2. **P5: Include directive** — Unblocks 10 files
3. **P1 + P2: `=?=` and `~()`** — Work together, unblock 2+ files
4. **P3: `\=`** — Unblocks 1 file
5. **P6: Tuple-in-list** — Needs investigation
6. **P4: `\+`** — Unblocks 1 file, needs semantic discussion

---

## Current Blocking Summary

| Blocker | Files Affected |
|---------|----------------|
| P5: Include directive | 10 |
| T1: Multiple-arity | 4 |
| P1+P2: =?= and ~() | 2 |
| P3: \= | 1 |
| P4: \+ | 1 |
| P6: Tuple-in-list | 1 |
| **Total Blocked** | **19** |

---

## GLP Programming Patterns Needed

### GP1: Helper Predicate Pattern for Arithmetic Results

**Status**: Pattern not yet documented
**Files Affected**: gc.glp (partially fixed, needs helper predicate refactoring)
**Issue**: Variables bound by `:=` arithmetic used multiple times in body goals violate SRSW

**Current pattern (SRSW violation)**:
```prolog
handle(issue(...), Balances, BalancesOut?, Block?) :-
    ground(...) |
    NewBal := OldBal? - Amount?,
    set_balance(Currency?, NewBal?, Balances?, BalancesOut),
    make_issue_block(Currency?, To?, Amount?, NewBal?, Block).
```

Error: "Reader variable 'NewBal?' occurs 2 times without ground guard"

**Needed solution**: Extract arithmetic to helper predicate or use ground guard pattern.

**Status**: gc.glp has partial fixes (Output Parameter Pattern applied to BalancesOut?, Block?), but still needs NewBal? fix in 5+ handle clauses.

---

## Change Log

- 2025-12-18: Initial document created from book program analysis
- 2025-12-18: Added GP1 (Helper Predicate Pattern) based on gc.glp analysis
