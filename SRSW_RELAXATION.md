# SRSW Relaxation: Allow `_` Everywhere

**Date:** 2026-01-16
**Status:** Approved for implementation
**Impact:** Parser, Specs, Documentation

---

## Current Rule (Inconsistent)

- `_` allowed in unit clauses (facts)
- `_` forbidden in regular clauses (heads and bodies)
- **This is inconsistent**

## New Rule (Simple & Consistent)

**Allow `_` everywhere**

### Rationale:
- Already allowed in unit clauses
- Natural programming style
- Simple, consistent rule
- Programmer's choice to use or ignore values

### Examples Now Valid:
```glp
% Head position - procedure doesn't bind output
process(X, _) :- compute(X).

% Body position - caller ignores output  
bubble([X?|Xs?], Ys, _).

% Already valid - unit clause
fact(_, _).
```

---

## Implementation Changes

### 1. Parser (`glp_runtime/lib/compiler/`)
**File to find and modify:** Search for SRSW validation, underscore checks
- Remove check that forbids `_` in clause bodies
- Remove check that forbids `_` in clause heads
- Keep allowing `_` in unit clauses (no change needed)

### 2. Specs (`docs/`)
**Update:** Any mention of SRSW forbidding `_`
- Change to: "`_` is allowed in all positions"
- Simplify SRSW description

### 3. Tests
**Update:** Programs marked as ill-typed due to `_`
- `bubble_sort.glp` - remove ILL-TYPED status
- Any other programs marked for this reason

---

## Files to Modify

Need to identify:
1. Parser code that validates/rejects `_`
2. Spec files mentioning SRSW and `_`
3. Test programs marked ill-typed for this reason

---

## Next Step

Search codebase for actual SRSW validation code that needs modification.
