# Module: well-typed-term

**Version**: 0.1  
**Date**: 2025-01-07  
**Status**: DRAFT  
**Paper References**: Definition 4.4 (Well-Typed Moded Term), lines 275-277

## Purpose

Determines when a moded term is well-typed by a GLP type. This is the foundation for clause and program well-typing.

## Dependencies

- `moded-term` — ModedTerm, paths(T)
- `type-dfa` — TypeDFA, paths(D)
- `path-consistency` — areConsistent

## Definitions

### Well-Typed Moded Term (Paper Definition 4.4, lines 275-277)

> A moded term T is **well-typed** by a GLP type D if:
> 1. For each term path x ∈ paths(T) there is a consistent path y ∈ paths(D), and
> 2. For every pair of variables in T, their types as determined by D are complementary.

### Variable Type Determination

When checking path consistency (Definition 4.3), variables are assigned types:
- A reader `X?` at a position with type `T` and mode `↓` has type `T` (consumed form)
- A writer `X` at a position with type `T` and mode `↑` has type `T` (produced form)

### Complementary Types

Two types are complementary if one is the mode-complement of the other:
- `T` and `T?` are complements
- `_` and `_?` are complements
- A type is its own complement under double complementation: `(T?)? = T`

For paired variables `X` and `X?`:
- If `X` has type `T` (produced)
- Then `X?` must have type `T?` (consumed)

## Public Interface

### Functions

#### `WellTypedResult checkWellTyped(ModedTerm term, TypeDFA typeDfa)`
Checks if a moded term is well-typed by a type DFA.

**Preconditions:**
- `term` is a valid moded term
- `typeDfa` is a compiled type DFA

**Postconditions:** Returns result indicating:
- Whether term is well-typed
- Variable type assignments
- Any inconsistencies found

#### `bool isWellTyped(ModedTerm term, TypeDFA typeDfa)`
Simplified check returning only boolean.

### Types

#### `class WellTypedResult`
```dart
class WellTypedResult {
  final bool isWellTyped;
  final Map<String, VariableTypeInfo> variableTypes;
  final List<WellTypingError> errors;
}

class VariableTypeInfo {
  final String typeName;
  final Mode mode;           // consume for readers, produce for writers
  final bool isReader;
}

abstract class WellTypingError {}

class InconsistentPathError extends WellTypingError {
  final ModedPath termPath;
  final String message;
}

class NonComplementaryVariablesError extends WellTypingError {
  final String variableName;
  final String writerType;
  final String readerType;
}
```

## Algorithms

### Algorithm: Well-Typed Moded Term Check

```
checkWellTyped(term, typeDfa):
  errors = []
  variableTypes = {}
  
  // Step 1: Extract all paths from the moded term
  termPaths = paths(term)
  
  // Step 2: For each term path, find a consistent type path
  for termPath in termPaths:
    result = findConsistentTypePath(termPath, typeDfa)
    
    if not result.found:
      errors.add(InconsistentPathError(termPath, 
        "No consistent type path found"))
    else:
      // Record variable type assignments
      if result.variableAssignment != null:
        recordVariableType(variableTypes, result.variableAssignment)
  
  // Step 3: Check that variable pairs have complementary types
  complementErrors = checkComplementaryVariables(variableTypes)
  errors.addAll(complementErrors)
  
  return WellTypedResult(
    isWellTyped: errors.isEmpty,
    variableTypes: variableTypes,
    errors: errors
  )

findConsistentTypePath(termPath, typeDfa):
  // Traverse the DFA following the term path structure
  // Check consistency at each step
  
  currentState = typeDfa.startState
  typePathSteps = []
  
  for i, termStep in enumerate(termPath.steps):
    if isPrimitiveState(currentState, typeDfa):
      // Type path is prefix of term path (Case 3)
      return checkCase3Consistency(termPath, typePathSteps, i, typeDfa)
    
    // Find matching transition in DFA
    transition = findMatchingTransition(currentState, termStep, typeDfa)
    
    if transition == null:
      // If term step is a variable, check Case 2
      if isVariable(termStep):
        return checkCase2Consistency(termPath, typePathSteps, termStep, 
                                     currentState, typeDfa)
      else:
        return ConsistencySearchResult(found: false)
    
    typePathSteps.add(transition.label)
    currentState = transition.target
  
  // Reached end of term path
  if isPrimitiveState(currentState, typeDfa):
    // Case 1 or Case 2: check final consistency
    return checkFinalConsistency(termPath, typePathSteps, currentState, typeDfa)
  
  return ConsistencySearchResult(found: false)

checkCase2Consistency(termPath, typePathSteps, variableStep, currentState, typeDfa):
  // Term path ends in variable, type path continues
  varName = variableStep.symbol
  isReader = isReaderVariable(variableStep)
  
  // Get mode at current type position
  typeMode = getModeAtState(currentState, typeDfa)
  
  // Case 2(a): reader at consumed position
  if isReader and typeMode == Mode.consume:
    typeName = getTypeNameAtState(currentState, typeDfa)
    return ConsistencySearchResult(
      found: true,
      variableAssignment: VariableAssignment(varName, typeName, Mode.consume, true)
    )
  
  // Case 2(b): writer at produced position
  if not isReader and typeMode == Mode.produce:
    typeName = getTypeNameAtState(currentState, typeDfa)
    return ConsistencySearchResult(
      found: true,
      variableAssignment: VariableAssignment(varName, typeName, Mode.produce, false)
    )
  
  return ConsistencySearchResult(found: false)

checkComplementaryVariables(variableTypes):
  errors = []
  
  // Group by base variable name (X and X? share name "X")
  baseNames = getUniqueBaseNames(variableTypes)
  
  for baseName in baseNames:
    writerInfo = variableTypes[baseName]      // Writer X
    readerInfo = variableTypes[baseName + "?"] // Reader X?
    
    if writerInfo != null and readerInfo != null:
      // Both forms appear - check complementarity
      if not areComplementaryTypes(writerInfo.typeName, readerInfo.typeName):
        errors.add(NonComplementaryVariablesError(
          baseName,
          writerInfo.typeName,
          readerInfo.typeName
        ))
  
  return errors

areComplementaryTypes(writerType, readerType):
  // T and T? are complements
  if readerType == writerType + "?":
    return true
  // _ and _? are complements
  if writerType == "_" and readerType == "_?":
    return true
  // Check involution: (T?)? = T
  if writerType.endsWith("?") and readerType == writerType.dropLast(1):
    return true
  return false
```

## Examples

### Example: Well-Typed merge Head

Moded head (from paper):
```
H' = ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
```

Type: `merge(Stream?, Stream?, Stream)`

**Paths and consistency:**

1. Path to `X?`:
   - Term: `(0,↓) --> merge --(1,↓)--> "."/2 --(1,↓)--> X?`
   - Type: `(0,↓) --> merge --(1,↓)--> Stream? --(1,↓)--> "."/2 --(1,↓)--> _?`
   - Case 2(a): reader at ↓ position ✓
   - Assignment: `X? : _?`

2. Path to `X`:
   - Term: `(0,↓) --> merge --(3,↑)--> "."/2 --(1,↑)--> X`
   - Type: `(0,↓) --> merge --(3,↑)--> Stream --(1,↑)--> "."/2 --(1,↑)--> _`
   - Case 2(b): writer at ↑ position ✓
   - Assignment: `X : _`

3. Variable complementarity:
   - `X : _` (produced)
   - `X? : _?` (consumed)
   - `_` and `_?` are complements ✓

**Result: Well-typed**

### Example: Ill-Typed — Wrong Mode

Moded term:
```
↓foo(↓X)   // Writer at consumed position!
```

Type: `foo(T?)` where `T?` has mode consume.

**Analysis:**
- Path: `(0,↓) --> foo --(1,↓)--> X`
- X is a writer, but position has mode ↓
- Case 2(b) requires ↑, not ↓
- **No consistent type path found**

**Result: Not well-typed**

### Example: Ill-Typed — Non-Complementary Variables

Moded term:
```
↓bar(↓X?, ↑X)
```

Type: `bar(T?, S)` where T ≠ S.

**Analysis:**
- `X?` at position 1 gets type `T` (consumed)
- `X` at position 2 gets type `S` (produced)
- If T ≠ S, types are not complementary

**Result: Not well-typed**

## Error Conditions

| Condition | Error Type |
|-----------|------------|
| Term path has no consistent type path | `InconsistentPathError` |
| Variable pair has non-complementary types | `NonComplementaryVariablesError` |

## Notes

### Checking Against DFA vs Path Set

The algorithm checks term paths against the type DFA directly rather than enumerating all type paths (which would be infinite for recursive types). The DFA structure allows efficient path-by-path consistency checking.

### Variable Occurrence Tracking

A variable may occur multiple times in a term. Each occurrence must yield the same type assignment. If occurrences yield different types, this indicates a type error (the term structure is inconsistent with the type).

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft |
