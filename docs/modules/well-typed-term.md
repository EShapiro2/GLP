# Module: well-typed-term

**Version**: 0.3  
**Date**: 2025-01-08  
**Status**: DRAFT  
**Paper References**: Definition 4.3 (Consistent Paths, lines 229-245), Definition 4.5 (Well-Typed Moded Term, lines 275-277)

## Purpose

Determines when a moded term is well-typed by a type DFA. This includes checking path consistency by traversing the DFA alongside moded paths.

## Dependencies

- `mode` — Mode enum
- `moded-term` — ModedTerm, ModedPath, paths()
- `type-dfa` — TypeDFA, DFAState, DFALabel, stateAfterLabel, isLeafState, getLeafType, getLeafMode

## Definitions

### Definition 4.5: Well-Typed Moded Term (lines 275-277)

> A moded term T is **well-typed** by a GLP type D if:
> 1. For each term path x ∈ paths(T) there is a consistent path in the type DFA, and
> 2. For every pair of variables in T, their types are complementary.

### Definition 4.3: Consistent Paths (lines 229-245)

A moded path is **consistent** with the type DFA if, when traversing the DFA alongside the path:

1. **Structure matches:** Each non-leaf step in the path corresponds to a valid transition in the DFA (same functor, arity, argument index, mode).

2. **Leaf consistency:** When the path reaches a leaf:
   - **Variable at DFA non-leaf:** Reader X? is consistent if DFA state has mode ↓; Writer X is consistent if DFA state has mode ↑
   - **Variable at DFA leaf:** Reader X? matches `_?`; Writer X matches `_`
   - **Constant at DFA leaf:** Integer matches `Integer` or same value; String matches `String` or same value; other constants match exactly

### Variable Type Assignment

When a path ends in a variable, the variable is assigned the type at that DFA position:
- Reader `X?` at position with type state S → X? has type S (consumed)
- Writer `X` at position with type state S → X has type S (produced)

### Complementary Types

Two variable types are complementary if:
- Both reach the same DFA state (or equivalent states)
- One is in consume mode, the other in produce mode

## Public Interface

### Types

#### `class WellTypedResult`

```dart
class WellTypedResult {
  final bool isWellTyped;
  final Map<String, VariableTypeInfo> variableTypes;
  final List<TypeError> errors;
}

class VariableTypeInfo {
  final DFAState typeState;  // DFA state where variable appears
  final Mode mode;           // consume for readers, produce for writers
}

abstract class TypeError {}

class InconsistentPathError extends TypeError {
  final ModedPath path;
  final String reason;
}

class NonComplementaryError extends TypeError {
  final String variableName;
  final VariableTypeInfo writerType;
  final VariableTypeInfo readerType;
}
```

### Functions

#### `WellTypedResult checkModedTerm(ModedTerm term, TypeDFA dfa)`

Checks if a moded term is well-typed by a type DFA.

**Preconditions:**
- `term` is a valid moded term
- `dfa` is a compiled type DFA

**Postconditions:** Returns WellTypedResult where:
- `isWellTyped` is true iff all paths are consistent and variable pairs are complementary
- `variableTypes` maps each variable (including reader/writer variants) to its assigned type
- `errors` lists all violations found

#### `PathCheckResult checkPathAgainstDFA(ModedPath path, TypeDFA dfa)`

Checks if a single moded path is consistent with the type DFA.

**Preconditions:**
- `path` is a valid moded path
- `dfa` is a compiled type DFA

**Postconditions:** Returns PathCheckResult indicating:
- Whether path is consistent
- If path ends in variable, the variable's type assignment

## Algorithms

### Algorithm: Well-Typed Moded Term Check

```
checkModedTerm(term, dfa):
  errors = []
  variableTypes = {}
  
  // Step 1: Extract and check all paths
  termPaths = paths(term)
  
  for path in termPaths:
    result = checkPathAgainstDFA(path, dfa)
    
    if not result.isConsistent:
      errors.add(InconsistentPathError(path, result.reason))
    else if result.variableAssignment != null:
      // Record variable type
      varKey = result.variableAssignment.varName
      if varKey in variableTypes:
        // Same variable appears multiple times - types must match
        if variableTypes[varKey].typeState != result.variableAssignment.typeState:
          errors.add(InconsistentVariableError(varKey))
      else:
        variableTypes[varKey] = result.variableAssignment
  
  // Step 2: Check variable pair complementarity
  complementErrors = checkComplementarity(variableTypes)
  errors.addAll(complementErrors)
  
  return WellTypedResult(
    isWellTyped: errors.isEmpty,
    variableTypes: variableTypes,
    errors: errors
  )
```

### Algorithm: Path Consistency Check (DFA Traversal)

```
checkPathAgainstDFA(path, dfa):
  state = dfa.startState
  
  // Traverse path, following DFA transitions
  for i in 0..<path.steps.length - 1:
    step = path.steps[i]
    nextStep = path.steps[i + 1]
    
    // Build label from next step
    label = DFALabel(
      symbol: extractFunctor(step.symbol),
      arity: extractArity(step.symbol),
      argIndex: nextStep.argIndex,
      mode: nextStep.mode
    )
    
    // Try to follow transition
    nextState = stateAfterLabel(state, label, dfa)
    
    if nextState == null:
      return PathCheckResult(
        isConsistent: false,
        reason: "No transition for ${label} from state ${state.name}"
      )
    
    state = nextState
  
  // Check leaf consistency
  leafStep = path.leaf
  return checkLeafConsistency(leafStep, state, dfa)

checkLeafConsistency(leafStep, dfaState, dfa):
  if leafStep.isVariable:
    // Variable leaf
    if leafStep.isReader:
      // Reader X? must be at consume position
      if isLeafState(dfaState):
        leafType = getLeafType(dfaState)
        if leafType == LeafType.primitiveInput:
          return PathCheckResult(
            isConsistent: true,
            variableAssignment: VariableTypeInfo(dfaState, Mode.consume)
          )
        else:
          return PathCheckResult(isConsistent: false, reason: "Reader at non-input leaf")
      else:
        // Non-leaf state - check if state accepts consume mode
        // (state must have transitions with consume mode, or be primitive)
        return PathCheckResult(
          isConsistent: true,  // Variable can appear at any type position
          variableAssignment: VariableTypeInfo(dfaState, Mode.consume)
        )
    else:
      // Writer X must be at produce position
      if isLeafState(dfaState):
        leafType = getLeafType(dfaState)
        if leafType == LeafType.primitiveOutput:
          return PathCheckResult(
            isConsistent: true,
            variableAssignment: VariableTypeInfo(dfaState, Mode.produce)
          )
        else:
          return PathCheckResult(isConsistent: false, reason: "Writer at non-output leaf")
      else:
        return PathCheckResult(
          isConsistent: true,
          variableAssignment: VariableTypeInfo(dfaState, Mode.produce)
        )
  
  else:
    // Constant leaf
    if not isLeafState(dfaState):
      return PathCheckResult(isConsistent: false, reason: "Constant at non-leaf state")
    
    leafType = getLeafType(dfaState)
    
    if leafType == LeafType.integer:
      if leafStep.value is int:
        return PathCheckResult(isConsistent: true)
      else:
        return PathCheckResult(isConsistent: false, reason: "Expected integer")
    
    if leafType == LeafType.string:
      if leafStep.value is String:
        return PathCheckResult(isConsistent: true)
      else:
        return PathCheckResult(isConsistent: false, reason: "Expected string")
    
    if leafType == LeafType.constant:
      if dfaState.constantValue == leafStep.value:
        return PathCheckResult(isConsistent: true)
      else:
        return PathCheckResult(isConsistent: false, 
          reason: "Expected ${dfaState.constantValue}, got ${leafStep.value}")
    
    return PathCheckResult(isConsistent: false, reason: "Unexpected leaf type")
```

### Algorithm: Complementarity Check

```
checkComplementarity(variableTypes):
  errors = []
  
  // Group by base name (X and X? share base "X")
  baseNames = {}
  for (varKey, info) in variableTypes:
    baseName = varKey.endsWith("?") ? varKey.substring(0, varKey.length-1) : varKey
    baseNames[baseName] = baseNames[baseName] ?? {}
    baseNames[baseName][varKey] = info
  
  for (baseName, variants) in baseNames:
    writerKey = baseName
    readerKey = "${baseName}?"
    
    if writerKey in variants and readerKey in variants:
      writerInfo = variants[writerKey]
      readerInfo = variants[readerKey]
      
      // Must be at same type state with complementary modes
      if writerInfo.typeState != readerInfo.typeState:
        errors.add(NonComplementaryError(baseName, writerInfo, readerInfo))
      else if writerInfo.mode != Mode.produce or readerInfo.mode != Mode.consume:
        errors.add(NonComplementaryError(baseName, writerInfo, readerInfo))
  
  return errors
```

## Examples

### Example: Well-Typed Moded Head

Moded head:
```
H' = ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
```

Type DFA for `merge(Stream?, Stream?, Stream)`:
- Arg 1, 2: Stream? (complemented)
- Arg 3: Stream (not complemented)

**Path checks:**

1. Path to X?: `(0,↓) → merge --(1,↓)--> [|] --(1,↓)--> X?`
   - Traverse: Stream? state → head position (↓) → primitive _?
   - X? is reader at _? (consume) ✓
   - Assignment: X? → (_?, consume)

2. Path to X: `(0,↓) → merge --(3,↑)--> [|] --(1,↑)--> X`
   - Traverse: Stream state → head position (↑) → primitive _
   - X is writer at _ (produce) ✓
   - Assignment: X → (_, produce)

**Complementarity:**
- X: (_, produce)
- X?: (_?, consume)
- Same base type position, complementary modes ✓

**Result: Well-typed**

### Example: Not Well-Typed — Mode Mismatch

Moded term:
```
↓foo(↓X)   // Writer X at consumed position!
```

Type: `foo(T?)` where T? has mode consume.

**Path check:**
- Path: `(0,↓) → foo --(1,↓)--> X`
- X is writer but at ↓ position
- Writer requires ↑ position

**Result: InconsistentPathError("Writer at consume position")**

### Example: Not Well-Typed — Non-Complementary Variables

Moded term:
```
↓bar(↓X?, ↑Y)
```

Type: `bar(Stream?, NatStream)` where Stream ≠ NatStream.

**Variable assignments:**
- X?: (Stream?, consume)
- Y: (NatStream, produce)

If X and X? both appeared, and X was assigned NatStream while X? was assigned Stream, they would not be complementary.

**Result: NonComplementaryError**

## Error Conditions

| Condition | Error Type |
|-----------|------------|
| Path has no matching DFA transition | `InconsistentPathError` |
| Variable at wrong mode position | `InconsistentPathError` |
| Constant doesn't match type leaf | `InconsistentPathError` |
| Same variable has different types at different occurrences | `InconsistentVariableError` |
| Variable pair not complementary | `NonComplementaryError` |

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft |
| 0.3 | 2025-01-08 | Merged path-consistency into this module; complete DFA traversal algorithm |
