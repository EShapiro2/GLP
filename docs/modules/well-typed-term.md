# Module: well-typed-term

**Version**: 0.4
**Date**: 2025-01-10
**Status**: DRAFT
**Paper References**: Definition 4.3 (Consistent Paths, lines 283-298), Definition 4.5 (Well-Typed Moded Term, lines 330-332)

## Purpose

Determines when a moded term is well-typed by an automaton. This includes checking path consistency by traversing the automaton alongside moded paths.

## Dependencies

- `mode` — Mode enum
- `moded-term` — ModedTerm, ModedPath, paths()
- `type-dfa` — ProgramDFA, Automaton, DFAState, TransitionLabel, checkLeafConsistency

## Definitions

### Definition 4.5: Well-Typed Moded Term (lines 330-332)

> A moded term T is **well-typed** by a GLP type D if:
> 1. For each term path x ∈ paths(T) there is a consistent path in the type automaton, and
> 2. For every pair of variables in T, their types are complementary.

### Definition 4.3: Consistent Paths (lines 283-298)

A moded path is **consistent** with the type automaton if, when traversing the automaton alongside the path:

1. **Structure matches:** Each non-leaf step in the path corresponds to a valid transition in the automaton (same functor, arity, argument index, mode).

2. **Leaf consistency:** When the path reaches a leaf:
   - **Variable at non-complement state:** Writer X with mode ↑ is consistent
   - **Variable at complement state:** Reader X? with mode ↓ is consistent
   - **Variable at _ state:** Writer X with mode ↑
   - **Variable at _? state:** Reader X? with mode ↓
   - **Integer literal at Integer/Integer? state:** Consistent (reaches _FINAL_)
   - **String literal at String/String? state:** Consistent (reaches _FINAL_)
   - **Constant matching transition:** Consistent if transition exists to _FINAL_

### Variable Type Assignment

When a path ends in a variable, the variable is assigned the type at that DFA position:
- Reader `X?` at complement state S? → X? has type S? (consumed)
- Writer `X` at non-complement state S → X has type S (produced)

### Complementary Types (Paper lines 342-348)

Two variable types are complementary if their states are complements:
- `_` and `_?` are complements
- `Stream` and `Stream?` are complements
- `Integer` and `Integer?` are complements

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
  final DFAState typeState;  // DFA state where variable appears (includes isComplement)
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

#### `WellTypedResult checkModedTerm(ModedTerm term, Automaton automaton, ProgramDFA dfa)`

Checks if a moded term is well-typed by an automaton.

**Preconditions:**
- `term` is a valid moded term
- `automaton` is the automaton for the expected type
- `dfa` provides access to all states

**Postconditions:** Returns WellTypedResult where:
- `isWellTyped` is true iff all paths are consistent and variable pairs are complementary
- `variableTypes` maps each variable (including reader/writer variants) to its assigned type
- `errors` lists all violations found

#### `PathCheckResult checkPathAgainstAutomaton(ModedPath path, Automaton automaton, ProgramDFA dfa)`

Checks if a single moded path is consistent with the automaton.

**Preconditions:**
- `path` is a valid moded path
- `automaton` is a compiled type automaton

**Postconditions:** Returns PathCheckResult indicating:
- Whether path is consistent
- If path ends in variable, the variable's type assignment

## Algorithms

### Algorithm: Well-Typed Moded Term Check

```
checkModedTerm(term, automaton, dfa):
  errors = []
  variableTypes = {}

  // Step 1: Extract and check all paths
  termPaths = paths(term)

  for path in termPaths:
    result = checkPathAgainstAutomaton(path, automaton, dfa)

    if not result.isConsistent:
      errors.add(InconsistentPathError(path, result.reason))
    else if result.variableAssignment != null:
      varKey = result.variableAssignment.varName
      if varKey in variableTypes:
        // Same variable appears multiple times - types must match
        if variableTypes[varKey].typeState.name != result.variableAssignment.typeState.name:
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

### Algorithm: Path Consistency Check (Automaton Traversal)

```
checkPathAgainstAutomaton(path, automaton, dfa):
  state = automaton.startState

  // Traverse path, following automaton transitions
  for i in 0..<path.steps.length - 1:
    step = path.steps[i]
    nextStep = path.steps[i + 1]

    // Build transition label from path step
    label = TransitionLabel.functor(
      extractFunctor(step.symbol),
      extractArity(step.symbol),
      nextStep.argIndex,
      mode: nextStep.mode
    )

    // Try to follow transition
    nextState = automaton.transition(state, label)

    if nextState == null:
      return PathCheckResult(
        isConsistent: false,
        reason: "No transition for ${label} from state ${state.name}"
      )

    state = nextState

  // Check leaf consistency
  leafStep = path.leaf
  return checkLeafConsistency(leafStep, state, dfa)
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

      // Check modes
      if writerInfo.mode != Mode.produce:
        errors.add(NonComplementaryError(baseName, writerInfo, readerInfo,
          "Writer must have produce mode"))
        continue
      if readerInfo.mode != Mode.consume:
        errors.add(NonComplementaryError(baseName, writerInfo, readerInfo,
          "Reader must have consume mode"))
        continue

      // Check states are complements (same baseName, opposite isComplement)
      if writerInfo.typeState.baseName != readerInfo.typeState.baseName:
        errors.add(NonComplementaryError(baseName, writerInfo, readerInfo,
          "Types must have same base: ${writerInfo.typeState.name} vs ${readerInfo.typeState.name}"))
        continue

      if writerInfo.typeState.isComplement == readerInfo.typeState.isComplement:
        errors.add(NonComplementaryError(baseName, writerInfo, readerInfo,
          "One must be complement, other not: ${writerInfo.typeState.name} vs ${readerInfo.typeState.name}"))

  return errors
```

## Examples

### Example: Well-Typed Moded Head (Paper lines 306-328)

Moded head:
```
H' = ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
```

For `merge(Stream?, Stream?, Stream)`, use automata:
- Arg 1: `Stream?` automaton (states have `isComplement: true`)
- Arg 2: `Stream?` automaton
- Arg 3: `Stream` automaton (states have `isComplement: false`)

**Path to X? (arg 1):**
```
(0,↓) → merge --(1,↓)--> [|] --(1,↓)--> X?
```

Traversing `Stream?` automaton:
1. Start at `Stream?` (isComplement: true)
2. Follow `[|](2,1):↓` → arrive at `_?` (isComplement: true)
3. Leaf: X? is reader, state `_?` is complement with consume mode ✓

Assignment: X? → (`_?`, consume)

**Path to X (arg 3):**
```
(0,↓) → merge --(3,↑)--> [|] --(1,↑)--> X
```

Traversing `Stream` automaton:
1. Start at `Stream` (isComplement: false)
2. Follow `[|](2,1):↑` → arrive at `_` (isComplement: false)
3. Leaf: X is writer, state `_` is non-complement with produce mode ✓

Assignment: X → (`_`, produce)

**Complementarity:**
- X: (`_`, produce) — baseName="_", isComplement=false
- X?: (`_?`, consume) — baseName="_", isComplement=true
- Same baseName, opposite isComplement ✓

**Result: Well-typed**

### Example: NEGATIVE — Mode Mismatch

Moded term:
```
↓foo(↓X)   // Writer X at consumed position!
```

Using `Stream?` automaton (complement):
- State `Stream?` expects consume mode for variables
- X is writer with produce mode
- Mismatch!

**Result: InconsistentPathError("Writer at complement state expects produce, got consume position")**

## Error Conditions

| Condition | Error Type |
|-----------|------------|
| Path has no matching automaton transition | `InconsistentPathError` |
| Variable mode doesn't match state's expectation | `InconsistentPathError` |
| Constant doesn't match type leaf | `InconsistentPathError` |
| Same variable has different types at different occurrences | `InconsistentVariableError` |
| Variable pair not complementary | `NonComplementaryError` |

## Changes from v0.3

- `TypeDFA` → `Automaton` and `ProgramDFA`
- Removed `complementDFA()` — use correct automaton directly
- Complementarity check uses `DFAState.baseName` and `isComplement`
- Leaf consistency determined by `state.isComplement`, not separate leaf types

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft |
| 0.3 | 2025-01-08 | Merged path-consistency; complete DFA traversal algorithm |
| 0.4 | 2025-01-10 | Update for ProgramDFA v0.8: Automaton, complement states |
