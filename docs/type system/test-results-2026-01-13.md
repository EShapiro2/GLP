# Type Checker Test Results Analysis - 2026-01-13 (Updated)

## Summary
- **Total:** 222 tests
- **Passed:** 130
- **Failed:** 92 (all in positive tests that should have passed)
- **All negative tests:** PASSED (37/37)
- **All SRSW tests:** PASSED (2/2)

## Good News
- `tuple` → `compound` fix WORKED: times.glp, min.glp, exp.glp, ackermann.glp all pass now!
- All originally passing tests still pass

---

## Root Cause Analysis

### Issue Category 1: SRSW Annotation Errors in Book Programs
Many files have **writers at input positions** which violates SRSW.

**Example: sum_list.glp**
```glp
procedure sum(NumList?, Number?, Number).  % positions: input, input, output
sum([], Acc, Acc?).  % Acc (writer) at position 2 which is Number? (input) - WRONG!
```
Should be: `sum([], Acc?, Acc).` or the type declaration needs fixing.

**Example: biased_merge.glp**
```glp
procedure bmerge(Number?, Number?, Number?, ...).  % all Number? are inputs
bmerge(Bx, By, 0, X, Y, Z?) :- ...  % Bx, By are writers at input positions - WRONG!
```

**Files affected:** sum_list, biased_merge, many social_graph files, etc.

### Issue Category 2: Guard Arithmetic Expression Parsing
The type checker may not handle arithmetic expressions in guards.

**Example: primes.glp, filter_even.glp**
```glp
filter([X|Xs], P, Ys?) :- X? mod P? =:= 0 | ...
```
The guard `X? mod P? =:= 0` requires:
1. Evaluating `mod(X?, P?)` to get a Number
2. Then checking `=:=(result, 0)`

The prelude declares `procedure =:=(Number?, Number?).` but `X? mod P?` is an expression, not a Number?.

### Issue Category 3: Conflicting Procedure Declarations
Some files redefine procedures that exist in prelude.

**Example: flatten.glp**
```glp
procedure is_list(_?).  % File declares is_list(_?)
```
But prelude has: `procedure is_list(List?).`

This may cause conflicts.

### Issue Category 4: Multiple Arities Same Name
Some files declare same procedure name with different arities.

**Example: sum_list.glp**
```glp
procedure sum(NumList?, Number).           % arity 2
procedure sum(NumList?, Number?, Number).  % arity 3
```
This is valid in GLP but may cause type checker issues.

---

## Action Items

### For Type Checker Implementation (Claude's job):
1. **BUG: Arithmetic in guards** - The type checker needs to handle arithmetic expressions (`mod`, `+`, `-`, etc.) in guard arguments for comparison guards like `=:=`, `=\=`, `<`, `>`, etc.

### For Book Program Annotations (Other Claude's job):
1. **Fix SRSW violations** - Many files have writers at input positions
2. **Review type declarations** - Ensure procedure modes match clause heads

---

## Detailed Failure List

### Loading Errors (45 files) - Likely SRSW violations
- sum_list.glp - SRSW: writer at input
- flatten.glp - conflicts with prelude is_list
- polygon_area.glp - TBD
- Most social_graph/* files - SRSW violations
- Most social_networks/* files - SRSW violations
- Most constitutional_consensus/* files - TBD
- Most cryptocurrencies/* files - TBD

### Type Errors (47 files) - Mix of issues
- primes.glp - arithmetic in guards (mod)
- filter_even.glp - arithmetic in guards (mod)
- member.glp - using =:= for non-numeric equality
- merge_ordered.glp - TBD
- Most meta/* files - likely complex patterns
- biased_merge.glp - SRSW violations
