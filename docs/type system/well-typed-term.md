# Module: well-typed-term

**Version**: 0.6
**Date**: 2025-01-12
**Status**: DRAFT
**Paper References**: Definition 4.5 (Consistent Paths), Definition 4.7 (Well-Typed Moded Term)

## Purpose

Determines when a moded term is well-typed by an automaton. This includes checking path consistency by traversing the automaton alongside moded paths.

## Dependencies

- `mode` — Mode enum
- `moded-term` — ModedTerm, ModedPath, paths(), variableMatchesStructuralMode()
- `type-dfa` — ProgramDFA, Automaton, DFAState, TransitionLabel

## Definitions

### Definition 4.7: Well-Typed Moded Term

> A moded term T is **well-typed** by a GLP type D if:
> 1. For each term path x ∈ paths(T) there is a consistent path in the type automaton, and
> 2. For every pair of variables in T, their types are complementary.

### Definition 4.5: Consistent Paths

Let x be a moded term path and y a GLP type path. Then x and y are **consistent** if:

**Case 1 (Equal length):** They are of equal length and identical except for their last symbols, which are consistent:
- Integer literal at Integer/Integer? state → consistent
- Real literal at Real/Real? state → consistent
- Numeric literal at Number/Number? state → consistent
- String literal at String/String? state → consistent
- Constant c at state with transition on c → consistent

**Case 2 (Term path is prefix):** x is a prefix of y except for its last symbol that is:
- **(a)** a reader X? and the mode of the corresponding type symbol is consume ↓, or
- **(b)** a writer X and the mode of the corresponding type symbol is produce ↑

**Case 3 (Type path is prefix):** y is a prefix of x except for its last symbol that is:
- **(a)** _? (consumed wildcard) and the mode of the corresponding term symbol is consume ↓, or
- **(b)** _ (produced wildcard) and the mode of the corresponding term symbol is produce ↑

### Primitive Term to Type Correspondence

| Term | State | Complement | Interpretation |
|------|-------|------------|----------------|
| X (writer) | _ | _? | any produced term |
| X? (reader) | _? | _ | any consumed term |
| 42 (integer) | Integer | Integer? | any integer literal |
| 3.14 (real) | Real | Real? | any real literal |
| numeric literal | Number | Number? | any numeric literal |
| "foo" (string) | String | String? | any string literal |
| [] (constant) | — | — | exact match required |

**Note:** The wildcard states `_` and `_?` accept any term of the appropriate mode, including literals and constants—they subsume all specific primitive types listed above.

### Variable Type Assignment

When a path ends in a variable, the variable is assigned the type at that DFA position:
- Reader `X?` at complement state S? → X? has type S? (consumed)
- Writer `X` at non-complement state S → X has type S (produced)

### Complementary Types

Two variable types are complementary if their states are complements:
- `_` and `_?` are complements
- `Stream` and `Stream?` are complements
- `Integer` and `Integer?` are complements

More formally, types T and U are complementary iff:
- T.baseName == U.baseName, AND
- T.isComplement != U.isComplement

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
  final String varName;       // e.g., "X" or "X?"
  final DFAState typeState;   // DFA state where variable appears
  final Mode mode;            // consume for readers, produce for writers
  
  String get baseName => varName.endsWith('?') ? 
    varName.substring(0, varName.length - 1) : varName;
}

abstract class TypeError {}

class InconsistentPathError extends TypeError {
  final ModedPath path;
  final String reason;
  final DFAState? lastState;  // State where inconsistency was detected
}

class NonComplementaryError extends TypeError {
  final String variableBaseName;
  final VariableTypeInfo writerType;
  final VariableTypeInfo readerType;
  final String reason;
}

class InconsistentVariableError extends TypeError {
  final String varName;
  final DFAState firstType;
  final DFAState secondType;
}
```

#### `class PathCheckResult`

```dart
class PathCheckResult {
  final bool isConsistent;
  final String? reason;           // If inconsistent, why
  final VariableTypeInfo? variableAssignment;  // If path ends in variable
  final DFAState? finalState;     // State at end of traversal
}
```

### Functions

#### `WellTypedResult checkModedTerm(ModedTerm term, Automaton automaton, ProgramDFA dfa)`

Checks if a moded term is well-typed by an automaton.

**Preconditions:**
- `term` is a valid moded term
- `automaton` is the automaton for the expected type
- `dfa` provides access to all automata (for type boundary crossing)

**Postconditions:** Returns WellTypedResult where:
- `isWellTyped` is true iff all paths are consistent and variable pairs are complementary
- `variableTypes` maps each variable (including reader/writer variants) to its assigned type
- `errors` lists all violations found

#### `PathCheckResult checkPathAgainstAutomaton(ModedPath path, Automaton automaton, ProgramDFA dfa)`

Checks if a single moded path is consistent with the automaton per Definition 4.5.

**Preconditions:**
- `path` is a valid moded path
- `automaton` is a compiled type automaton

**Postconditions:** Returns PathCheckResult indicating:
- Whether path is consistent
- If path ends in variable, the variable's type assignment
- The final state reached

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
      errors.add(InconsistentPathError(path, result.reason, result.finalState))
    else if result.variableAssignment != null:
      varKey = result.variableAssignment.varName
      if varKey in variableTypes:
        // Same variable appears multiple times - types must match
        if variableTypes[varKey].typeState.name != result.variableAssignment.typeState.name:
          errors.add(InconsistentVariableError(varKey, 
            variableTypes[varKey].typeState,
            result.variableAssignment.typeState))
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

### Algorithm: Path Consistency Check (Definition 4.5)

```
checkPathAgainstAutomaton(path, automaton, dfa):
  state = automaton.startState
  currentAutomaton = automaton

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
    nextState = currentAutomaton.transition(state, label)

    if nextState == null:
      return PathCheckResult(
        isConsistent: false,
        reason: "No transition for ${label} from state ${state.name}",
        finalState: state
      )

    // If we cross into a different user-defined type, switch automata
    if nextState.isUserDefinedType && nextState.baseName != state.baseName:
      currentAutomaton = dfa.getAutomaton(nextState.name)

    state = nextState

  // Check leaf consistency per Definition 4.5
  leafStep = path.leaf
  return checkLeafConsistency(leafStep, state, currentAutomaton, dfa)
```

### Algorithm: Leaf Consistency Check (Definition 4.5 Cases)

```
checkLeafConsistency(leaf, state, automaton, dfa):
  // Get structural mode at this position
  structuralMode = leaf.mode

  // Case 3(b): Produced wildcard state (_)
  // Per paper: "_ accepts any produced term (a writer or ground term with mode ↑)"
  if state.isProducedWildcard:
    if leaf.isVariable:
      if !leaf.isReader:
        // Writer at produce position - consistent
        return PathCheckResult(
          isConsistent: true,
          variableAssignment: VariableTypeInfo(leaf.symbol, state, Mode.produce),
          finalState: state
        )
      // Reader at produce position - mode mismatch
      return PathCheckResult(
        isConsistent: false,
        reason: "_ expects produced term, got reader ${leaf.symbol}",
        finalState: state
      )
    // Constant/literal - consistent if structural mode is produce
    if structuralMode == Mode.produce:
      return PathCheckResult(isConsistent: true, finalState: state)
    return PathCheckResult(
      isConsistent: false,
      reason: "_ expects produced term, got consumed ${describeLeaf(leaf)}",
      finalState: state
    )

  // Case 3(a): Consumed wildcard state (_?)
  // Per paper: "_? accepts any consumed term (a reader or ground term with mode ↓)"
  if state.isConsumedWildcard:
    if leaf.isVariable:
      if leaf.isReader:
        // Reader at consume position - consistent
        return PathCheckResult(
          isConsistent: true,
          variableAssignment: VariableTypeInfo(leaf.symbol, state, Mode.consume),
          finalState: state
        )
      // Writer at consume position - mode mismatch
      return PathCheckResult(
        isConsistent: false,
        reason: "_? expects consumed term, got writer ${leaf.symbol}",
        finalState: state
      )
    // Constant/literal - consistent if structural mode is consume
    if structuralMode == Mode.consume:
      return PathCheckResult(isConsistent: true, finalState: state)
    return PathCheckResult(
      isConsistent: false,
      reason: "_? expects consumed term, got produced ${describeLeaf(leaf)}",
      finalState: state
    )

  // Case 1: Primitive type states with literal
  if state.isIntegerType:
    if leaf.isConstant && leaf.value is int:
      return PathCheckResult(isConsistent: true, finalState: dfa.finalState)
    if leaf.isVariable:
      return checkVariableAtState(leaf, state, structuralMode)
    return PathCheckResult(
      isConsistent: false,
      reason: "Integer type expects integer literal or variable",
      finalState: state
    )

  if state.isRealType:
    if leaf.isConstant && leaf.value is double:
      return PathCheckResult(isConsistent: true, finalState: dfa.finalState)
    if leaf.isVariable:
      return checkVariableAtState(leaf, state, structuralMode)
    return PathCheckResult(
      isConsistent: false,
      reason: "Real type expects real literal or variable",
      finalState: state
    )

  if state.isNumberType:
    if leaf.isConstant && leaf.value is num:
      return PathCheckResult(isConsistent: true, finalState: dfa.finalState)
    if leaf.isVariable:
      return checkVariableAtState(leaf, state, structuralMode)
    return PathCheckResult(
      isConsistent: false,
      reason: "Number type expects numeric literal or variable",
      finalState: state
    )

  if state.isStringType:
    if leaf.isConstant && leaf.value is String:
      return PathCheckResult(isConsistent: true, finalState: dfa.finalState)
    if leaf.isVariable:
      return checkVariableAtState(leaf, state, structuralMode)
    return PathCheckResult(
      isConsistent: false,
      reason: "String type expects string literal or variable",
      finalState: state
    )

  // Anonymous final state (from constant match)
  if state.isAnonymousFinal:
    return PathCheckResult(isConsistent: true, finalState: state)

  // Case 2: User-defined type state with variable
  if state.isUserDefinedType && leaf.isVariable:
    return checkVariableAtState(leaf, state, structuralMode)

  // Case 1: Constant at type state - check for matching transition
  if leaf.isConstant:
    constMode = state.isComplement ? Mode.consume : Mode.produce
    constLabel = TransitionLabel.constant(leaf.value, mode: constMode)
    if automaton.transition(state, constLabel) != null:
      return PathCheckResult(isConsistent: true, finalState: dfa.finalState)
    return PathCheckResult(
      isConsistent: false,
      reason: "Constant ${leaf.value} has no transition from ${state.name}",
      finalState: state
    )

  return PathCheckResult(
    isConsistent: false,
    reason: "Unexpected leaf ${describeLeaf(leaf)} at state ${state.name}",
    finalState: state
  )

checkVariableAtState(leaf, state, structuralMode):
  // Case 2(a): Reader at consume position
  if leaf.isReader && state.isComplement:
    return PathCheckResult(
      isConsistent: true,
      variableAssignment: VariableTypeInfo(leaf.symbol, state, Mode.consume),
      finalState: state
    )
  
  // Case 2(b): Writer at produce position
  if !leaf.isReader && !state.isComplement:
    return PathCheckResult(
      isConsistent: true,
      variableAssignment: VariableTypeInfo(leaf.symbol, state, Mode.produce),
      finalState: state
    )
  
  // Mismatch
  expectedForm = state.isComplement ? "reader" : "writer"
  actualForm = leaf.isReader ? "reader" : "writer"
  return PathCheckResult(
    isConsistent: false,
    reason: "Variable mode mismatch: ${state.name} expects $expectedForm, got $actualForm ${leaf.symbol}",
    finalState: state
  )

describeLeaf(leaf):
  if leaf.isVariable:
    return leaf.isReader ? "reader ${leaf.symbol}" : "writer ${leaf.symbol}"
  if leaf.isConstant:
    return "constant ${leaf.value}"
  return "unknown"
```

### Algorithm: Complementarity Check

```
checkComplementarity(variableTypes):
  errors = []

  // Group by base name (X and X? share base "X")
  baseNames = {}
  for (varKey, info) in variableTypes:
    baseName = info.baseName
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
          "Writer must have produce mode, got ${writerInfo.mode}"))
        continue
        
      if readerInfo.mode != Mode.consume:
        errors.add(NonComplementaryError(baseName, writerInfo, readerInfo,
          "Reader must have consume mode, got ${readerInfo.mode}"))
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

### Example 1: Well-Typed Moded Head (Paper Example)

Moded head:
```
H' = ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
```

For `merge(Stream?, Stream?, Stream)`, check each argument:

**Path to X? (arg 1):**
```
(0,↓) → merge/3 --(1,↓)--> Stream? --(1,↓)--> [|]/2 --(1,↓)--> _?
```

Traversing `Stream?` automaton:
1. Start at `Stream?` (isComplement: true)
2. Follow `[|](2,1):↓` → arrive at `_?` (isComplement: true)
3. Leaf: X? is reader, state `_?` is complement → Case 2(a) ✓

Assignment: X? → (`_?`, consume)

**Path to X (arg 3):**
```
(0,↓) → merge/3 --(3,↑)--> Stream --(1,↑)--> [|]/2 --(1,↑)--> _
```

Traversing `Stream` automaton:
1. Start at `Stream` (isComplement: false)
2. Follow `[|](2,1):↑` → arrive at `_` (isComplement: false)
3. Leaf: X is writer, state `_` is non-complement → Case 2(b) ✓

Assignment: X → (`_`, produce)

**Complementarity:**
- X: (`_`, produce) — baseName="_", isComplement=false
- X?: (`_?`, consume) — baseName="_", isComplement=true
- Same baseName, opposite isComplement ✓

**Result: Well-typed**

### Example 2: Interactive Type (CounterCall)

Moded head for `monitor(N, [read(N?)|In])`:
```
H' = ↓monitor(↓N?, ↓[↓read(↑N)|↓In?])
```

**Path to N (inside read):**
```
(0,↓) → monitor/2 --(2,↓)--> Stream(CounterCall)? --(1,↓)--> CounterCall? --(1,↑)--> Integer
```

At `Integer` (non-complement), leaf N is writer → Case 2(b) ✓

Assignment: N → (`Integer`, produce)

**Path to N? (arg 1):**
At `Integer?` (complement), leaf N? is reader → Case 2(a) ✓

Assignment: N? → (`Integer?`, consume)

**Complementarity:** Integer and Integer? are complements ✓

### Example 3: NEGATIVE — Mode Mismatch

Moded term:
```
↓foo(↓X)   // Writer X at consumed position!
```

Using `Stream?` automaton (complement):
- State `Stream?` expects reader (consume mode for variables)
- X is writer with produce mode
- Case 2(a) requires reader, but got writer

**Error:** `InconsistentPathError("Variable mode mismatch: Stream? expects reader, got writer X")`

### Example 4: NEGATIVE — Non-Complementary Types

Given `convert(Stream?, NatStream)` where:
- `Stream ::= [] ; [_|Stream]`
- `NatStream ::= [] ; [Integer|NatStream]`

Clause:
```
convert([X|Xs], [X?|Ys]) :- ...
```

Variable types:
- X from `Stream?` arg gets type `_?`
- X? from `NatStream` arg gets type `Integer`

Complementarity check:
- X: baseName="_"
- X?: baseName="Integer"
- Different baseNames!

**Error:** `NonComplementaryError("X", "Types must have same base: _? vs Integer")`

## Error Conditions

| Condition | Error Type |
|-----------|------------|
| No matching automaton transition | `InconsistentPathError` |
| Variable at wrong state (mode mismatch) | `InconsistentPathError` |
| Constant without matching transition | `InconsistentPathError` |
| Same variable has different types | `InconsistentVariableError` |
| Variable pair not complementary | `NonComplementaryError` |

## Changes from v0.4

- Added Definition 4.5 with three cases for path consistency
- Added automaton switching at type boundaries
- Added `PathCheckResult.finalState`
- Added detailed error messages with state information
- Added interactive type example (CounterCall)
- Updated algorithms to explicitly reference Definition 4.5 cases

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft |
| 0.3 | 2025-01-08 | Merged path-consistency; complete DFA traversal algorithm |
| 0.4 | 2025-01-10 | Update for ProgramDFA v0.8: Automaton, complement states |
| 0.5 | 2025-01-12 | Add Definition 4.5 three cases; automaton switching; interactive type examples |
| 0.6 | 2025-01-12 | Fix wildcard states to accept any term of appropriate mode (not just variables) |
