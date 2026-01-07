# Module: clause-contribution

**Version**: 0.1
**Date**: 2025-01-07
**Status**: DRAFT
**Paper References**: Definition 4.8 (accepts predicate), Definition 4.10 (contravariance)

## Dependencies

- `moded-head` — modedHead(H, decl)
- `moded-term` — paths(T), ModedPath
- `type-dfa` — TypeDFA, DFAState, ModedLabel

## Purpose

Computes the DFA representing the set of input paths that a clause head can accept at a given argument position. This is used for contravariance checking: verifying that the union of all clause contributions covers the declared input type.

## Paper Context

From Definition 4.8 (lines 311-321):
> C **accepts** an input path x ∈ paths(D) if H' has a path consistent with x.

From Definition 4.10 (lines 351-357):
> **Contravariance:** Every input path in every procedure type in D has a clause C ∈ Cs that accepts it.

The contravariance check requires computing, for each input argument position, the union of paths accepted by all clauses. This module provides the DFA representation of what a single clause head accepts.

## Definitions

### Clause Contribution

Given a clause C with head H and a procedure declaration, the **clause contribution** at argument position i is the DFA representing all input paths that the moded head H' can match at position i.

### Head Pattern DFA

The head pattern at position i is derived from:
1. The argument term at position i in the clause head
2. The mode information from the procedure declaration
3. The variable flip operation (Definition 4.6)

The resulting DFA accepts exactly those type paths that are consistent with the moded head's paths at that position.

## Public Interface

### Functions

#### `TypeDFA extractHeadPatternDFA(Head head, int argIndex, ProcDecl decl, TypeEnvironment env)`

Extracts the DFA representing input paths accepted by the clause head at the given argument position.

**Preconditions:**
- `head` is a valid clause head
- `argIndex` is 1-based and within the head's arity
- `decl` provides the procedure type declaration
- `decl.argTypes[argIndex-1].isInput` is true (only input positions need coverage)

**Postconditions:** Returns a TypeDFA where:
- The language is the set of type paths consistent with the head's moded paths at this position
- For variable arguments, the DFA accepts all paths (universal acceptance at that position)
- For constant arguments, the DFA accepts only the matching constant path
- For compound arguments, the DFA recursively represents the structure

**Errors:**
- Throws `ArgumentOutOfRangeError` if argIndex is invalid
- Throws `NotInputPositionError` if the argument is not an input position

### Algorithm

```
extractHeadPatternDFA(head, argIndex, decl, env):
  // Step 1: Get the argument term at this position
  argTerm = head.args[argIndex - 1]

  // Step 2: Get the declared type for this position
  declaredType = decl.argTypes[argIndex - 1]

  // Step 3: Build the contribution DFA from the term structure
  return buildContributionDFA(argTerm, declaredType, env)

buildContributionDFA(term, typeExpr, env):
  match term:
    Variable(name, isReader):
      // After variable flip: writer becomes reader (input), reader becomes writer (output)
      // A variable at an input position accepts ALL paths of the declared type
      // This is the "wildcard" case - the variable can match anything
      flippedIsReader = !isReader
      if flippedIsReader:
        // Reader in moded head = accepts any value of declared type
        return compileType(typeExpr.typeName, env)
      else:
        // Writer in moded head at input position = type error (caught elsewhere)
        return emptyDFA()

    Constant(value):
      // Constant accepts only the path to that exact constant
      return singletonDFA(value)

    Compound(functor, arity, args):
      // Build DFA that accepts paths through this structure
      dfa = newDFA()
      startState = dfa.startState

      for i in 1..arity:
        childType = getChildType(typeExpr, functor, i, env)
        childDFA = buildContributionDFA(args[i-1], childType, env)

        // Add transitions for this argument position
        label = ModedLabel("$functor($arity,$i)", mode: modeOf(childType))
        addSubDFA(dfa, startState, label, childDFA)

      return dfa

singletonDFA(value):
  // DFA that accepts only the path ending in this constant
  dfa = newDFA()
  finalState = DFAState("_final_$value", isFinal: true)
  dfa.addTransition(dfa.startState, ModedLabel(value.toString()), finalState)
  return dfa
```

## Examples

### Example: Variable Argument (Wildcard)

Clause head: `merge(Xs, Ys, Zs)`

At argument 1 with type `Stream?`:
- Term is variable `Xs`
- After flip: `Xs?` (reader)
- Reader at input position → accepts all paths of `Stream?`
- Result: DFA equivalent to compiled `Stream?`

### Example: Constant Argument

Clause head: `merge([], Ys, Ys?)`

At argument 1 with type `Stream?`:
- Term is constant `[]`
- Result: DFA accepting only the path `[] → _final_nil`

### Example: Compound Argument

Clause head: `merge([X|Xs], Ys, [X?|Zs?])`

At argument 1 with type `Stream?`:
- Term is `[X|Xs]` (list cons)
- After flip: `[X?|Xs?]`
- X? is reader → accepts any `_?` at head position
- Xs? is reader → accepts any `Stream?` at tail position
- Result: DFA with transitions:
  - `Stream? --[[|](2,1), ↓]--> _prim` (accepts any element)
  - `Stream? --[[|](2,2), ↓]--> Stream?` (recursive)

### Example: Union for Coverage Check

For procedure `merge(Stream?, Stream?, Stream)` with clauses:
```
merge([], Ys, Ys?).
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
```

At argument 1:
- Clause 1 contributes: DFA for `[]` only
- Clause 2 contributes: DFA for `[_?|Stream?]`
- Union: DFA for `[] | [_?|Stream?]` = full `Stream?`

Coverage check: `Stream? ⊆ union` → ✓ covered

## Error Conditions

| Condition | Exception |
|-----------|-----------|
| Argument index out of range | `ArgumentOutOfRangeError` |
| Position is not an input (T?, not T) | `NotInputPositionError` |

## Notes

### Relationship to Contravariance

The contravariance algorithm in `well-typed-program` works as follows:
1. For each input argument position, compile the declared type to a DFA
2. For each clause, extract the head pattern DFA at that position
3. Compute the union of all clause pattern DFAs
4. Check: declared type DFA ⊆ union of clause patterns

If the subset check fails, there exist input paths not accepted by any clause.

### Variable Handling

Variables in clause heads are the "wildcards" of coverage checking. A variable at an input position means "this clause accepts any value of the declared type at this position." This is why variables compile to the full type DFA rather than an empty or singleton DFA.

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft |
