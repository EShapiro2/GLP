# Module: well-typed-program

**Version**: 0.1  
**Date**: 2025-01-07  
**Status**: DRAFT  
**Paper References**: Definition 4.7 (lines 351-357)

## Purpose

Determines when a typed GLP program P = (Cs, D) is well-typed. This is the top-level type checking predicate.

## Dependencies

- `well-typed-clause` — checkClause, accepts
- `type-paths` — inputPaths(D)

## Paper Definition

### Definition 4.7: Well-typed GLP Program (lines 351-357)

> A typed GLP program P = (Cs, D) is **well-typed** if:
>
> 1. **Covariance:** Every clause C ∈ Cs is well-typed by D.
> 2. **Contravariance:** Every input path in every procedure type in D has a clause C ∈ Cs that accepts it.

## Two Conditions

### Covariance (Condition 1)

Every clause must satisfy all three conditions of Definition 4.6:
- Head is well-typed
- Body atoms are well-typed
- Variable pairs have complementary types

This ensures that clauses **produce** terms within the declared types.

### Contravariance (Condition 2)

For every procedure with input arguments, every possible input path must be accepted by at least one clause.

This ensures that the program can **consume** all values allowed by the input types.

**Input paths** are paths in paths(D) that:
- Correspond to input argument positions (Type?, not Type)
- Start with mode ↓ (consume)

A clause **accepts** a path if its moded head has a consistent path.

## Example

### Program

```
Stream ::= [] ; [_|Stream].
procedure merge(Stream?, Stream?, Stream).

merge([], Ys, Ys?).
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
```

### Covariance Check

Both clauses must be well-typed (per Definition 4.6).

### Contravariance Check

Input paths for `merge` (arguments 1 and 2, both `Stream?`):

**Argument 1 paths:**
```
(0,↓) → merge --(1,↓)--> Stream? --(1,↓)--> []
(0,↓) → merge --(1,↓)--> Stream? --(1,↓)--> [|] --(1,↓)--> _?
(0,↓) → merge --(1,↓)--> Stream? --(1,↓)--> [|] --(2,↓)--> Stream? → ...
```

**Checking acceptance:**
- Path ending in `[]`: Accepted by clause 1 (pattern `[]`)
- Path ending in `_?`: Accepted by clause 2 (pattern `[X|Xs]` with X at that position)
- Recursive paths: Accepted by clause 2

**Argument 2 paths:** Similar analysis.

All input paths are accepted → program is well-typed.

## Interface

### `ProgramCheckResult checkProgram(List<Clause> cs, TypeEnv d)`

Checks if program (Cs, D) is well-typed.

**Returns:**
- `isWellTyped`: true if both conditions hold
- `clauseResults`: individual clause check results
- `uncoveredPaths`: input paths not accepted by any clause (if any)

### Algorithm

```
checkProgram(cs, d):
  errors = []
  clauseResults = []
  
  // Condition 1: Covariance
  for c in cs:
    result = checkClause(c, d)
    clauseResults.add(result)
    if not result.isWellTyped:
      errors.add("Clause not well-typed: " + c)
  
  // Condition 2: Contravariance
  for proc in d.procedures:
    for argIndex, argType in enumerate(proc.argTypes):
      if argType.isInput:  // Type?, not Type
        inputPaths = extractInputPaths(d, proc, argIndex)
        for path in inputPaths:
          if not anyClauseAccepts(cs, path, d):
            errors.add("Uncovered input path: " + path)
  
  return ProgramCheckResult(errors.isEmpty, clauseResults, errors)

anyClauseAccepts(cs, path, d):
  for c in cs:
    if c.head.functor == path.procedure:
      if accepts(c, path, d):
        return true
  return false
```

## Implementation Note: Finite Representation

Input paths form a regular language (represented by the type DFA). Since this language is infinite for recursive types, contravariance checking must work with the DFA representation rather than enumerating paths.

**Approach:** For each input argument position, verify that the union of clause contributions covers the declared type. This is equivalent to checking that every path is accepted.

Specifically, for input argument i of procedure p:
1. Compute the DFA of patterns at position i across all clauses for p
2. Check that this DFA is a superset of (or equal to) the declared input type DFA

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft from paper |
