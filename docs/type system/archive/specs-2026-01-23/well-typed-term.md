# Module: well-typed-term

**Version**: 0.8
**Date**: 2026-01-23
**Status**: DRAFT
**Paper References**: Definition 5.2 (Consistent Paths), Definition 5.4 (Well-Typed Moded Term)

## Purpose

Determines when a moded term is well-typed by an automaton. This includes checking path consistency by traversing the automaton alongside moded paths.

## Dependencies

- `mode` — Mode enum
- `moded-term` — ModedTerm, ModedPath, paths(), variableMatchesStructuralMode()
- `type-dfa` — ProgramDFA, Automaton, DFAState, TransitionLabel

## Definitions

### Definition 5.4: Well-Typed Moded Term

> A moded term T is **well-typed** by a GLP type D if:
> 1. For each term path x ∈ paths(T) there is a consistent path in the type automaton, and
> 2. For every pair of variables in T, their types as determined by D are dual.

### Definition 5.2: Consistent Paths

Let x be a moded term path and y a GLP type path, with lengths |x| and |y| respectively. Then x and y are **consistent** if one of the following holds:

**Case 1 (Equal length):** |x| = |y| and x and y are identical except for their last symbols, which are consistent:
- Integer literal at Integer/Integer? state → consistent
- Real literal at Real/Real? state → consistent
- Numeric literal at Number/Number? state → consistent
- String literal at String/String? state → consistent
- Constant c at state with transition on c → consistent

**Case 2 (Term path shorter — variable at leaf):** |x| < |y| and x is a prefix of y except for its last symbol, which is:
- **(a)** a reader X? and the structural mode at that position is consume ↓, or
- **(b)** a writer X and the structural mode at that position is produce ↑

**Case 3 (Type path shorter — wildcard in type):** |y| < |x| and y ends at a wildcard state (`_` or `_?`), and:
- **(a)** if `_?`, the structural mode at position |y| in the term path is consume ↓, or
- **(b)** if `_`, the structural mode at position |y| in the term path is produce ↑

**The remainder of the term path beyond position |y| is not examined; the wildcard accepts the entire subterm at that position.**

### Primitive Term to Type Correspondence

| Term | State | Dual | Interpretation |
|------|-------|------|----------------|
| X (writer) | _ | _? | any produced term |
| X? (reader) | _? | _ | any consumed term |
| 42 (integer) | Integer | Integer? | any integer literal |
| 3.14 (real) | Real | Real? | any real literal |
| numeric literal | Number | Number? | any numeric literal |
| "foo" (string) | String | String? | any string literal |
| [] (constant) | — | — | exact match required |

**Note:** The wildcard states `_` and `_?` accept any term of the same mode, including literals and constants—they subsume all specific primitive types listed above.

### Variable Type Assignment

When a path ends in a variable, the variable is assigned the type at that DFA position:
- Reader `X?` at dual state S? → X? has type S? (consumed)
- Writer `X` at non-dual state S → X has type S (produced)

### Dual Types

Two variable types are dual if their states are duals:
- `_` and `_?` are duals
- `Stream` and `Stream?` are duals
- `Integer` and `Integer?` are duals

More formally, types T and U are dual iff:
- T.baseName == U.baseName, AND
- T.isDual != U.isDual

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

class NonDualError extends TypeError {
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
- `isWellTyped` is true iff all paths are consistent and variable pairs are dual
- `variableTypes` maps each variable (including reader/writer variants) to its assigned type
- `errors` lists all violations found

#### `PathCheckResult checkPathAgainstAutomaton(ModedPath path, Automaton automaton, ProgramDFA dfa)`

Checks if a single moded path is consistent with the automaton per Definition 5.2.

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

  // Step 2: Check variable pair duality
  dualityErrors = checkDuality(variableTypes)
  errors.addAll(dualityErrors)

  return WellTypedResult(
    isWellTyped: errors.isEmpty,
    variableTypes: variableTypes,
    errors: errors
  )
```

### Algorithm: Path Consistency Check (Definition 5.2)

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

  // Check leaf consistency per Definition 5.2
  leafStep = path.leaf
  return checkLeafConsistency(leafStep, state, currentAutomaton, dfa)
```

### Algorithm: Leaf Consistency Check (Definition 5.2 Cases)

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
    constMode = state.isDual ? Mode.consume : Mode.produce
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
  if leaf.isReader && state.isDual:
    return PathCheckResult(
      isConsistent: true,
      variableAssignment: VariableTypeInfo(leaf.symbol, state, Mode.consume),
      finalState: state
    )
  
  // Case 2(b): Writer at produce position
  if !leaf.isReader && !state.isDual:
    return PathCheckResult(
      isConsistent: true,
      variableAssignment: VariableTypeInfo(leaf.symbol, state, Mode.produce),
      finalState: state
    )
  
  // Mismatch
  expectedForm = state.isDual ? "reader" : "writer"
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

### Algorithm: Duality Check

```
checkDuality(variableTypes):
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
        errors.add(NonDualError(baseName, writerInfo, readerInfo,
          "Writer must have produce mode, got ${writerInfo.mode}"))
        continue
        
      if readerInfo.mode != Mode.consume:
        errors.add(NonDualError(baseName, writerInfo, readerInfo,
          "Reader must have consume mode, got ${readerInfo.mode}"))
        continue

      // Check states are duals (same baseName, opposite isDual)
      if writerInfo.typeState.baseName != readerInfo.typeState.baseName:
        errors.add(NonDualError(baseName, writerInfo, readerInfo,
          "Types must have same base: ${writerInfo.typeState.name} vs ${readerInfo.typeState.name}"))
        continue

      if writerInfo.typeState.isDual == readerInfo.typeState.isDual:
        errors.add(NonDualError(baseName, writerInfo, readerInfo,
          "One must be dual, other not: ${writerInfo.typeState.name} vs ${readerInfo.typeState.name}"))

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
1. Start at `Stream?` (isDual: true)
2. Follow `[|](2,1):↓` → arrive at `_?` (isDual: true)
3. Leaf: X? is reader, state `_?` is dual → Case 2(a) ✓

Assignment: X? → (`_?`, consume)

**Path to X (arg 3):**
```
(0,↓) → merge/3 --(3,↑)--> Stream --(1,↑)--> [|]/2 --(1,↑)--> _
```

Traversing `Stream` automaton:
1. Start at `Stream` (isDual: false)
2. Follow `[|](2,1):↑` → arrive at `_` (isDual: false)
3. Leaf: X is writer, state `_` is non-dual → Case 2(b) ✓

Assignment: X → (`_`, produce)

**Duality:**
- X: (`_`, produce) — baseName="_", isDual=false
- X?: (`_?`, consume) — baseName="_", isDual=true
- Same baseName, opposite isDual ✓

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

At `Integer` (non-dual), leaf N is writer → Case 2(b) ✓

Assignment: N → (`Integer`, produce)

**Path to N? (arg 1):**
At `Integer?` (dual), leaf N? is reader → Case 2(a) ✓

Assignment: N? → (`Integer?`, consume)

**Duality:** Integer and Integer? are duals ✓

### Example 3: NEGATIVE — Mode Mismatch

Moded term:
```
↓foo(↓X)   // Writer X at consumed position!
```

Using `Stream?` automaton (dual):
- State `Stream?` expects reader (consume mode for variables)
- X is writer with produce mode
- Case 2(a) requires reader, but got writer

**Error:** `InconsistentPathError("Variable mode mismatch: Stream? expects reader, got writer X")`

### Example 4: NEGATIVE — Non-Dual Types

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

Duality check:
- X: baseName="_"
- X?: baseName="Integer"
- Different baseNames!

**Error:** `NonDualError("X", "Types must have same base: _? vs Integer")`

## Error Conditions

| Condition | Error Type |
|-----------|------------|
| No matching automaton transition | `InconsistentPathError` |
| Variable at wrong state (mode mismatch) | `InconsistentPathError` |
| Constant without matching transition | `InconsistentPathError` |
| Same variable has different types | `InconsistentVariableError` |
| Variable pair not dual | `NonDualError` |

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft |
| 0.3 | 2025-01-08 | Merged path-consistency; complete DFA traversal algorithm |
| 0.4 | 2025-01-10 | Update for ProgramDFA v0.8: Automaton, complement states |
| 0.5 | 2025-01-12 | Add Definition 4.5 three cases; automaton switching; interactive type examples |
| 0.6 | 2025-01-12 | Fix wildcard states to accept any term of appropriate mode (not just variables) |
| 0.7 | 2026-01-18 | Clarify Definition 4.5 Case 3: wildcards accept entire subterm at position, no further traversal |
| 0.8 | 2026-01-23 | **Paper alignment**: Updated to Definitions 5.2, 5.4; "complement" → "dual" throughout; `isComplement` → `isDual`; `NonComplementaryError` → `NonDualError`; `checkComplementarity` → `checkDuality` |
