# Arithmetic Guard Groundness - Spec Clarification Needed

**Date:** 2025-11-14
**Issue:** Contradiction in specification regarding arithmetic guards and groundness

---

## Problem Statement

The current specification has a **contradiction** regarding whether arithmetic comparison guards imply groundness.

### Contradiction in guards-reference.md

**Lines 144-148** say arithmetic guards DON'T imply groundness:
```prolog
% ❌ WRONG - comparison guards don't imply groundness
bad_compare(X, Y1, Y2) :- X > 0 |
    send(X?, Y1),    % SRSW VIOLATION!
    send(X?, Y2).    % X > 0 doesn't guarantee X is ground
```

**But lines 213-216** define arithmetic comparison semantics as:
```
Semantics:
- Success: Both X and Y bound to numbers AND condition holds
- Suspend: Either X or Y is unbound reader
- Fail: Both bound to numbers AND condition false
```

### Contradiction in bytecode spec

**glp-bytecode-v216-complete.md Section 19.3.1** (lines 824-831):
```
#### 19.3.1 guard_less Xi, Xj
**Source**: `X < Y` in guard position
**Operation**: Evaluate Xi < Xj
**Behavior**:
- Evaluate expressions in Xi and Xj
- If both ground numbers: succeed if Xi < Xj, else fail
- If unbound readers in either: suspend, add readers to Si
- Type error (non-numeric): fail
```

---

## Analysis

### What the Semantics Actually Mean

When a guard like `X? < Y?` **succeeds**:
1. Both X and Y MUST be bound to numbers (otherwise it would have suspended or failed)
2. The comparison condition (X < Y) holds
3. **Therefore: X and Y are guaranteed to be ground numbers**

### Why This Matters for SRSW

If `X? < Y?` succeeds, then:
- X is ground (bound to a number)
- Ground values contain no unbound writers
- Multiple readers of X don't violate SRSW (no writer to expose)
- **X? can appear multiple times in the body**

### Real-World Example (from sort.glp)

```prolog
partition([X | Xs], A, [X? | Smaller?], Larger?) :-
    A? >= X? | partition(Xs?, A?, Smaller, Larger).
```

**Current compiler behavior:** Flags SRSW violation (X? appears twice)

**Correct behavior:** Should be allowed because:
1. Guard `A? >= X?` succeeds
2. This guarantees X is ground (bound to a number)
3. Multiple occurrences of X? in body are safe

---

## Proposed Solution

### Option 1: Arithmetic Guards Imply Groundness (RECOMMENDED)

**Change guards-reference.md** to reflect actual semantics:

```markdown
### Guards That Imply Groundness

| Guard | Implies Ground | Allows Multiple Readers |
|-------|----------------|-------------------------|
| ✅ `ground(X)` | Yes | ✅ Yes |
| ⏳ `integer(X)` | Yes (when implemented) | ✅ Yes |
| ⏳ `number(X)` | Yes (when implemented) | ✅ Yes |
| ✅ `X < Y` (when succeeds) | Yes (both operands) | ✅ Yes |
| ✅ `X =< Y` (when succeeds) | Yes (both operands) | ✅ Yes |
| ✅ `X > Y` (when succeeds) | Yes (both operands) | ✅ Yes |
| ✅ `X >= Y` (when succeeds) | Yes (both operands) | ✅ Yes |
| ✅ `X =:= Y` (when succeeds) | Yes (both operands) | ✅ Yes |
| ✅ `X =\= Y` (when succeeds) | Yes (both operands) | ✅ Yes |
| ✅ `known(X)` | **NO** | ❌ No |
| ✅ `otherwise` | No | ❌ No |
```

**Update example from lines 144-148:**

```prolog
% ✅ CORRECT - comparison guard implies groundness
good_compare(X, Y1, Y2) :- X > 0 |
    send(X?, Y1),    % OK! X > 0 guarantees X is ground
    send(X?, Y2).    % Multiple readers allowed
```

### Option 2: Require Explicit Ground Guards (NOT RECOMMENDED)

Force programmers to write:
```prolog
partition([X | Xs], A, [X? | Smaller?], Larger?) :-
    number(X?), A? >= X? |  % Redundant!
    partition(Xs?, A?, Smaller, Larger).
```

**Problems:**
- Redundant (arithmetic guard already tests this)
- Less readable
- Inconsistent with semantics (guard ALREADY ensures groundness)
- Would require changes to many existing programs

---

## Recommendation

**Update the specification** to clarify that:

1. **Arithmetic comparison guards** (`<`, `=<`, `>`, `>=`, `=:=`, `=\=`) **imply groundness** of their operands when they succeed

2. **Compiler/Analyzer** should recognize this and allow multiple reader occurrences of variables that appear in successful arithmetic guards

3. **Remove the contradictory example** at guards-reference.md lines 144-148

4. **Add correct example** showing that arithmetic guards DO allow multiple readers

---

## Impact on Existing Code

### Files Affected

- `/Users/udi/GLP/docs/guards-reference.md` - Update table and examples
- `/Users/udi/GLP/glp_runtime/lib/compiler/analyzer.dart` - Update SRSW analysis to recognize arithmetic guards

### Test Cases to Update

- `udi/glp/sort.glp` - Remove workaround `number(X?)` guards (they're redundant)
- Any other programs using arithmetic guards

### Benefits

- More natural code (no redundant guards)
- Consistent with actual semantics
- Matches programmer intuition (if X > 0 succeeds, X must be a number)
- Enables common patterns in sorting, partitioning, etc.

---

## Implementation Steps

1. **Update guards-reference.md**:
   - Fix table at lines 101-108
   - Fix example at lines 144-148
   - Add correct examples showing arithmetic guards allow multiple readers

2. **Update glp-bytecode-v216-complete.md** (if needed):
   - Add explicit note that arithmetic guards imply groundness
   - Clarify SRSW implications

3. **Update compiler/analyzer.dart**:
   - Recognize arithmetic comparison guards in SRSW analysis
   - Mark variables as "ground-certified" when they appear in arithmetic guards
   - Allow multiple reader occurrences for ground-certified variables

4. **Update tests**:
   - Remove redundant `number(X?)` guards from sort.glp
   - Add tests specifically for this feature

5. **Document in CLAUDE.md**:
   - Add note about arithmetic guards and SRSW relaxation
   - Update compiler requirements section

---

## Questions for User

1. **Do arithmetic comparison guards imply groundness?** (My answer: Yes, based on semantics)

2. **Should the compiler automatically recognize this?** (My answer: Yes, makes code more natural)

3. **Any edge cases or concerns?** (e.g., what about complex arithmetic expressions in guards?)
