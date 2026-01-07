# Module: well-typed-program

**Version**: 0.1  
**Date**: 2025-01-07  
**Status**: DRAFT  
**Paper References**: Definition 4.10 (lines 351-357)

## Purpose

Determines when a typed GLP program P = (Cs, D) is well-typed. This is the top-level type checking predicate.

## Dependencies

- `well-typed-clause` — checkClause, accepts
- `type-paths` — inputPaths(D)

## Paper Definition

### Definition 4.10: Well-typed GLP Program (lines 351-357)

> A typed GLP program P = (Cs, D) is **well-typed** if:
>
> 1. **Covariance:** Every clause C ∈ Cs is well-typed by D.
> 2. **Contravariance:** Every input path in every procedure type in D has a clause C ∈ Cs that accepts it.

## Two Conditions

### Covariance (Condition 1)

Every clause must satisfy all three conditions of Definition 4.8:
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

Both clauses must be well-typed (per Definition 4.8).

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

  // Condition 2: Contravariance (using DFA operations)
  for proc in d.procedures:
    for argIndex, argType in enumerate(proc.argTypes):
      if argType.isInput:  // Type?, not Type
        // Get the declared input type DFA
        inputTypeDFA = compileType(argType, d)

        // Compute union of clause contributions at this position
        clausePatternsDFA = emptyDFA()
        for c in cs where c.head.functor == proc.name:
          patternDFA = extractHeadPatternDFA(c.head, argIndex)
          clausePatternsDFA = union(clausePatternsDFA, patternDFA)

        // Check coverage: inputTypeDFA ⊆ clausePatternsDFA
        if not isSubsetOf(inputTypeDFA, clausePatternsDFA):
          // Find witness path in the difference
          uncoveredDFA = intersect(inputTypeDFA, complement(clausePatternsDFA))
          witness = findAcceptingPath(uncoveredDFA)
          errors.add("Uncovered input path at arg " + argIndex + ": " + witness)

  return ProgramCheckResult(errors.isEmpty, clauseResults, errors)
```

## Implementation Note: DFA-Based Contravariance

The algorithm above uses DFA operations rather than path enumeration because:
1. Input paths form an infinite regular language for recursive types
2. DFA subset checking (`isSubsetOf`) decides inclusion in finite time
3. When coverage fails, DFA intersection with complement provides a witness path

Required DFA operations (from `type-dfa` module):
- `compileType(typeName, env)` — compile type to DFA
- `union(dfa1, dfa2)` — language union
- `intersect(dfa1, dfa2)` — language intersection
- `complement(dfa)` — language complement
- `isSubsetOf(dfa1, dfa2)` — check L(dfa1) ⊆ L(dfa2)
- `findAcceptingPath(dfa)` — find a path accepted by DFA (for error reporting)

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft from paper |
| 0.2 | 2025-01-07 | Replace path enumeration with DFA operations |
| 0.3 | 2025-01-07 | Fix definition numbers |
