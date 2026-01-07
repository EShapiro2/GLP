# Module: well-typed-program

**Version**: 0.5  
**Date**: 2025-01-08  
**Status**: DRAFT  
**Paper References**: Definition 4.10 (lines 351-357)

## Purpose

Determines when a typed GLP program P = (Cs, D) is well-typed. This is the top-level type checking entry point.

## Dependencies

- `well-typed-clause` — checkClause(), getAcceptedLabels()
- `type-dfa` — compileType(), complementDFA(), getTransitions(), isLeafState()
- `type-environment` — TypeEnvironment, ProcDecl

## Definitions

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

For every procedure with input arguments, every possible input must be accepted by at least one clause.

This ensures that the program can **consume** all values allowed by the input types.

**Structural coverage checking:** Rather than DFA set operations, we traverse the input type DFA and verify that each transition (alternative) is covered by some clause head.

## Public Interface

### Types

#### `class ProgramCheckResult`

```dart
class ProgramCheckResult {
  final bool isWellTyped;
  final List<ClauseCheckResult> clauseResults;
  final List<CoverageError> coverageErrors;
}

class CoverageError {
  final String procedure;
  final int argIndex;
  final DFALabel uncoveredLabel;
  final String path;  // Human-readable path to uncovered position
}
```

### Functions

#### `ProgramCheckResult checkProgram(List<Clause> clauses, TypeEnvironment env)`

Checks if program (Cs, D) is well-typed per Definition 4.10.

**Preconditions:**
- `clauses` is a non-empty list of valid GLP clauses
- `env` contains type definitions and procedure declarations for all types and predicates

**Postconditions:** Returns ProgramCheckResult where:
- `isWellTyped` is true iff both covariance and contravariance hold
- `clauseResults` contains individual check results for each clause
- `coverageErrors` lists uncovered input paths (empty if all covered)

**Errors:**
- Throws `UndeclaredProcedureError` if any clause uses an undeclared procedure
- Throws `UndefinedTypeError` if any procedure references an undefined type

## Algorithms

### Algorithm: Program Well-Typing Check

```
checkProgram(clauses, env):
  clauseResults = []
  coverageErrors = []
  
  // Condition 1: Covariance — check each clause
  for clause in clauses:
    result = checkClause(clause, env)
    clauseResults.add(result)
  
  // Condition 2: Contravariance — check coverage for each input argument
  for proc in env.procedures:
    procClauses = clauses.filter(c => c.head.functor == proc.name 
                                   && c.head.arity == proc.arity)
    
    for argIndex in 1..proc.arity:
      if proc.argTypes[argIndex - 1].isInput:
        errors = checkCoverage(procClauses, proc, argIndex, env)
        coverageErrors.addAll(errors)
  
  isWellTyped = clauseResults.all(r => r.isWellTyped) && coverageErrors.isEmpty
  
  return ProgramCheckResult(isWellTyped, clauseResults, coverageErrors)
```

### Algorithm: Structural Coverage Check

```
checkCoverage(clauses, proc, argIndex, env):
  // Get the input type DFA (already complemented for T?)
  inputType = proc.argTypes[argIndex - 1]
  baseDFA = compileType(inputType.baseName, env)
  inputDFA = complementDFA(baseDFA)  // T? needs complement
  
  errors = []
  visited = {}
  
  checkStateCoverage(inputDFA.startState, clauses, argIndex, "", visited, errors, inputDFA)
  
  return errors

checkStateCoverage(state, clauses, argIndex, pathPrefix, visited, errors, dfa):
  // Prevent infinite recursion on recursive types
  if state in visited:
    return  // Already checked this state
  visited.add(state)
  
  // Leaf states don't need coverage - they're reached by matching structure
  if isLeafState(state):
    return
  
  // Get all transitions (alternatives) from this state
  transitions = getTransitions(state, dfa)
  
  for (label, targetState) in transitions:
    // Check if some clause accepts this transition at this argument position
    if clauseAcceptsLabel(clauses, argIndex, label, pathPrefix):
      // Recursively check the target state
      newPath = pathPrefix + " → " + label.toString()
      checkStateCoverage(targetState, clauses, argIndex, newPath, visited, errors, dfa)
    else:
      // No clause covers this alternative
      errors.add(CoverageError(
        procedure: proc.name,
        argIndex: argIndex,
        uncoveredLabel: label,
        path: pathPrefix + " → " + label.toString()
      ))

clauseAcceptsLabel(clauses, argIndex, label, pathPrefix):
  for clause in clauses:
    acceptedLabels = getAcceptedLabels(clause, argIndex, env)
    
    if acceptedLabels == ALL_LABELS:
      // Variable at this position - accepts anything
      return true
    
    if label in acceptedLabels:
      // Clause explicitly matches this label
      return true
  
  return false
```

### Algorithm: Deep Coverage Check

For nested structures, we need to check coverage at each level:

```
checkDeepCoverage(clauses, argIndex, dfaState, termPath, env):
  // termPath tracks the path through the argument structure
  // e.g., for [X|Xs], termPath might be "head" or "tail"
  
  if isLeafState(dfaState):
    return []  // Leaf covered by variable matching
  
  errors = []
  transitions = getTransitions(dfaState)
  
  for (label, targetState) in transitions:
    // Find clauses that match at this position in the structure
    matchingClauses = findClausesMatching(clauses, argIndex, termPath, label)
    
    if matchingClauses.isEmpty:
      errors.add(CoverageError(...))
    else:
      // Check deeper coverage for matching clauses
      for nestedPath in getNestedPaths(label):
        nestedErrors = checkDeepCoverage(
          matchingClauses, argIndex, targetState, 
          termPath + "." + nestedPath, env
        )
        errors.addAll(nestedErrors)
  
  return errors
```

## Examples

### Example: Well-Typed merge Program

```
Stream ::= [] ; [_|Stream].
procedure merge(Stream?, Stream?, Stream).

merge([], Ys, Ys?).
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
```

**Covariance:** Both clauses are well-typed ✓

**Contravariance for argument 1 (Stream?):**

Input type DFA transitions from Stream state:
- `[]` → nil (leaf)
- `[|](2,1)` → _ (primitive leaf)
- `[|](2,2)` → Stream (recursive)

Coverage:
- `[]`: Clause 1 matches with `[]` ✓
- `[|]`: Clause 2 matches with `[X|Xs]` ✓
  - Head position: Variable X accepts any _ ✓
  - Tail position: Variable Xs accepts any Stream ✓

**Contravariance for argument 2 (Stream?):** Similar analysis ✓

**Result: Well-typed program**

### Example: INVALID — Covariance Failure

```
Stream ::= [] ; [_|Stream].
procedure merge(Stream?, Stream?, Stream).

merge([], Ys, 42).  // Integer at Stream position!
```

**Problem:** Clause 1 produces integer `42` at output position expecting `Stream`.

**Error:** `ClauseCheckResult.errors = [HeadNotWellTypedError(...)]`

### Example: INVALID — Contravariance Failure

```
Stream ::= [] ; [_|Stream].
procedure merge(Stream?, Stream?, Stream).

merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
// Missing: merge([], Ys, Ys?).
```

**Problem:** Input type `Stream?` includes `[]` alternative, but no clause accepts nil at argument 1.

**Error:** 
```
CoverageError(
  procedure: "merge",
  argIndex: 1,
  uncoveredLabel: DFALabel("[]", 0, 0, null),
  path: "Stream? → []"
)
```

### Example: Deep Coverage

```
Tree ::= leaf(Integer) ; node(Tree, Tree).
procedure sum(Tree?, Integer).

sum(leaf(N), N?).
sum(node(L, R), S) :- sum(L?, X), sum(R?, Y), add(X?, Y?, S).
```

**Contravariance for argument 1 (Tree?):**

- `leaf`: Clause 1 matches with `leaf(N)` 
  - Inside leaf: Variable N accepts any Integer ✓
- `node`: Clause 2 matches with `node(L, R)`
  - Left subtree: Variable L accepts any Tree ✓
  - Right subtree: Variable R accepts any Tree ✓

**Result: Well-typed**

## Error Conditions

| Condition | Error |
|-----------|-------|
| Clause not well-typed | In `clauseResults` with errors |
| Input alternative not covered | `CoverageError` |

## Notes

### Why Structural Coverage Instead of DFA Operations?

Previous approaches used DFA union/intersection/subset:
```
inputDFA ⊆ union(clausePatternDFAs)
```

Structural coverage is simpler:
1. No need to build clause pattern DFAs
2. No need for DFA set operations
3. Directly answers "which alternative is missing?"
4. Naturally handles recursive types via visited set

### Variable Coverage

A variable at an argument position is a **wildcard** — it accepts all values of the declared type. This is why `getAcceptedLabels` returns `ALL_LABELS` for variables.

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft |
| 0.5 | 2025-01-08 | Replace DFA set operations with structural coverage; simplified algorithms |
