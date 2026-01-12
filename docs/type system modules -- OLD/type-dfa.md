# Module: type-dfa

**Version**: 1.0
**Date**: 2025-01-11
**Status**: DRAFT
**Paper References**: Section 4.1 (lines 19-24, 47-53), Example 4.1 (lines 55-80), Definition 4.3 (lines 283-298)

## Purpose

Represents the DFA for a typed GLP program. Each type T has a complement type T? with a corresponding complement automaton. The complement automaton is derived by flipping states and modes.

## Dependencies

- `mode` — Mode enum
- `type-environment` — TypeEnvironment, TypeDef, ProcDecl

## DFA Structure (Paper lines 47-53)

A typed GLP program P = (Cs, D) has a DFA where:

**States come in complement pairs:**
- For each defined type `T`: states `T` and `T?`
- System states: `Integer` and `Integer?`, `Real` and `Real?`, `Number` and `Number?`, `String` and `String?`
- Primitive final states: `_` and `_?`
- Anonymous final state: `_FINAL_` (for constant/literal matches)
- Procedure states: `merge/3`, etc. (no complement—procedures are not types)

**Automata come in complement pairs:**
- For each type `T`: automaton for `T` (producer view) and automaton for `T?` (consumer view)
- The automaton for `T?` is derived from `T` by flipping states and modes

**Transitions:**
- From procedure states: to declared argument type states directly (`Stream?` or `Stream`)
- From type states: based on BNF alternatives, with modes from producer view
- From complement type states: same structure with flipped states and modes

## Complementation (Paper lines 19-24)

For each type T, the complement automaton T? is obtained by:
1. Replacing each state S with its complement state S?
2. Replacing each mode: ↑ becomes ↓, and ↓ becomes ↑

This defines complementation as an involution: (T?)? = T.

**Example (Paper lines 55-80):**

For `Stream ::= [] ; [_|Stream]`:

**Stream automaton** (producer view):
- State `Stream` with transitions:
  - `[]` → `_FINAL_`
  - `[|](2,1):↑` → `_`
  - `[|](2,2):↑` → `Stream`

**Stream? automaton** (consumer view):
- State `Stream?` with transitions:
  - `[]` → `_FINAL_`
  - `[|](2,1):↓` → `_?`
  - `[|](2,2):↓` → `Stream?`

## Public Interface

### Types

#### `class DFAState`

```dart
class DFAState {
  final String baseName;      // e.g., "Stream", "Integer", "_"
  final bool isComplement;    // true for Stream?, Integer?, _?
  final bool isFinal;

  String get name => isComplement ? '$baseName?' : baseName;

  DFAState get complement => DFAState(baseName, isComplement: !isComplement, isFinal: isFinal);

  bool get isWildcard => baseName == '_';
  bool get isProducedWildcard => baseName == '_' && !isComplement;
  bool get isConsumedWildcard => baseName == '_' && isComplement;
  bool get isIntegerType => baseName == 'Integer';
  bool get isRealType => baseName == 'Real';
  bool get isNumberType => baseName == 'Number';
  bool get isStringType => baseName == 'String';
  bool get isAnonymousFinal => baseName == '_FINAL_';
}
```

#### `class TransitionLabel`

```dart
class TransitionLabel {
  final String symbol;    // functor name or constant value
  final int arity;        // 0 for constants
  final int argIndex;     // 1-based, 0 for constants
  final Mode? mode;       // null for procedure arg transitions and constants

  factory TransitionLabel.functor(String name, int arity, int argIndex, {Mode? mode});
  factory TransitionLabel.constant(Object value);

  TransitionLabel get complement => TransitionLabel(symbol, arity, argIndex, mode: mode?.flip);
}
```

#### `class Automaton`

An automaton for a single type (either T or T?).

```dart
class Automaton {
  final DFAState startState;
  final Map<(DFAState, TransitionLabel), DFAState> transitions;

  DFAState? transition(DFAState from, TransitionLabel label);

  /// Create complement automaton by flipping all states and modes
  Automaton get complement;
}
```

#### `class ProgramDFA`

The complete DFA for a typed GLP program.

```dart
class ProgramDFA {
  final Map<String, DFAState> states;       // All states including complements
  final Map<String, Automaton> automata;    // One per type name (T and T? are separate entries)

  DFAState getState(String name);           // e.g., "Stream" or "Stream?"
  Automaton getAutomaton(String typeName);  // e.g., "Stream" or "Stream?"
}
```

### Functions

#### `ProgramDFA buildProgramDFA(TypeEnvironment env)`

Builds the complete DFA from the type environment.

## Algorithm: Build Program DFA

```
buildProgramDFA(env):
  states = {}
  automata = {}

  // Create system states (complement pairs)
  states['_'] = DFAState('_', isComplement: false, isFinal: true)
  states['_?'] = DFAState('_', isComplement: true, isFinal: true)
  states['Integer'] = DFAState('Integer', isComplement: false, isFinal: false)
  states['Integer?'] = DFAState('Integer', isComplement: true, isFinal: false)
  states['Real'] = DFAState('Real', isComplement: false, isFinal: false)
  states['Real?'] = DFAState('Real', isComplement: true, isFinal: false)
  states['Number'] = DFAState('Number', isComplement: false, isFinal: false)
  states['Number?'] = DFAState('Number', isComplement: true, isFinal: false)
  states['String'] = DFAState('String', isComplement: false, isFinal: false)
  states['String?'] = DFAState('String', isComplement: true, isFinal: false)
  states['_FINAL_'] = DFAState('_FINAL_', isComplement: false, isFinal: true)

  // Create automata for system types
  automata['_'] = finalAutomaton(states['_'])
  automata['_?'] = finalAutomaton(states['_?'])
  automata['Integer'] = integerAutomaton(states['Integer'], states['_FINAL_'])
  automata['Integer?'] = integerAutomaton(states['Integer?'], states['_FINAL_'])
  automata['Real'] = realAutomaton(states['Real'], states['_FINAL_'])
  automata['Real?'] = realAutomaton(states['Real?'], states['_FINAL_'])
  automata['Number'] = numberAutomaton(states['Number'], states['_FINAL_'])
  automata['Number?'] = numberAutomaton(states['Number?'], states['_FINAL_'])
  automata['String'] = stringAutomaton(states['String'], states['_FINAL_'])
  automata['String?'] = stringAutomaton(states['String?'], states['_FINAL_'])

  // Create states and automata for defined types
  for (typeName, typeDef) in env.types:
    // Create state pair
    states[typeName] = DFAState(typeName, isComplement: false, isFinal: false)
    states[typeName + '?'] = DFAState(typeName, isComplement: true, isFinal: false)

    // Build producer automaton
    automata[typeName] = buildTypeAutomaton(typeDef, states, isComplement: false)

    // Build consumer automaton (complement)
    automata[typeName + '?'] = buildTypeAutomaton(typeDef, states, isComplement: true)

  // Create procedure states (no complement)
  for (procKey, procDecl) in env.procedures:
    states[procKey] = DFAState(procKey, isComplement: false, isFinal: false)

  // Build procedure automata
  for (procKey, procDecl) in env.procedures:
    automata[procKey] = buildProcedureAutomaton(procDecl, states, automata)

  return ProgramDFA(states, automata)
```

## Algorithm: Build Type Automaton

```
buildTypeAutomaton(typeDef, states, isComplement):
  typeName = typeDef.name
  startStateName = isComplement ? typeName + '?' : typeName
  startState = states[startStateName]

  transitions = {}

  for alt in typeDef.alternatives:
    addTypeTransitions(startState, alt, Mode.produce, states, transitions, isComplement)

  return Automaton(startState, transitions)
```

## Algorithm: Add Type Transitions

```
addTypeTransitions(fromState, alt, contextMode, states, transitions, isComplement):
  // Apply complement to mode if building complement automaton
  effectiveMode = isComplement ? contextMode.flip : contextMode

  match alt:
    ConstantAlt(value):
      label = TransitionLabel.constant(value)
      transitions[(fromState, label)] = states['_FINAL_']

    ListNilAlt:
      label = TransitionLabel.constant('[]')
      transitions[(fromState, label)] = states['_FINAL_']

    ListConsAlt(headType, tailType):
      headMode = modeOf(headType, contextMode)
      tailMode = modeOf(tailType, contextMode)

      // Apply complement to modes
      if isComplement:
        headMode = headMode.flip
        tailMode = tailMode.flip

      headLabel = TransitionLabel.functor('[|]', 2, 1, mode: headMode)
      tailLabel = TransitionLabel.functor('[|]', 2, 2, mode: tailMode)

      transitions[(fromState, headLabel)] = resolveTypeExpr(headType, states, isComplement)
      transitions[(fromState, tailLabel)] = resolveTypeExpr(tailType, states, isComplement)

    StructAlt(functor, args):
      for i in 0..<args.length:
        argType = args[i]
        argMode = modeOf(argType, contextMode)
        if isComplement:
          argMode = argMode.flip
        label = TransitionLabel.functor(functor, args.length, i+1, mode: argMode)
        transitions[(fromState, label)] = resolveTypeExpr(argType, states, isComplement)

    DiffListAlt(content, hole):
      contentMode = modeOf(content, contextMode)
      holeMode = modeOf(hole, contextMode)
      if isComplement:
        contentMode = contentMode.flip
        holeMode = holeMode.flip

      contentLabel = TransitionLabel.functor('\\', 2, 1, mode: contentMode)
      holeLabel = TransitionLabel.functor('\\', 2, 2, mode: holeMode)

      transitions[(fromState, contentLabel)] = resolveTypeExpr(content, states, isComplement)
      transitions[(fromState, holeLabel)] = resolveTypeExpr(hole, states, isComplement)
```

## Algorithm: Resolve Type Expression

```
resolveTypeExpr(typeExpr, states, isComplement):
  match typeExpr:
    PrimitiveModeAlt(isInput):
      // Determine base state
      baseIsComplement = isInput
      // XOR with automaton complement flag
      finalIsComplement = baseIsComplement XOR isComplement
      return finalIsComplement ? states['_?'] : states['_']

    TypeRef(name, isInput):
      // Determine base state
      baseIsComplement = isInput
      // XOR with automaton complement flag
      finalIsComplement = baseIsComplement XOR isComplement

      if name == 'Integer':
        return finalIsComplement ? states['Integer?'] : states['Integer']
      if name == 'Real':
        return finalIsComplement ? states['Real?'] : states['Real']
      if name == 'Number':
        return finalIsComplement ? states['Number?'] : states['Number']
      if name == 'String':
        return finalIsComplement ? states['String?'] : states['String']

      return finalIsComplement ? states[name + '?'] : states[name]
```

## Algorithm: Mode Computation

```
modeOf(typeExpr, contextMode):
  // T? flips mode, T keeps mode (before complement is applied)
  if typeExpr is TypeRef && typeExpr.isInput:
    return contextMode.flip
  if typeExpr is PrimitiveModeAlt && typeExpr.isInput:
    return contextMode.flip
  return contextMode
```

## Algorithm: Build Procedure Automaton

```
buildProcedureAutomaton(procDecl, states, automata):
  procState = states[procDecl.key]
  transitions = {}

  for i in 0..<procDecl.arity:
    argType = procDecl.argTypes[i]
    label = TransitionLabel.functor(procDecl.name, procDecl.arity, i+1, mode: null)

    // Target is the declared type directly (Stream? or Stream)
    targetStateName = getFullTypeName(argType)
    transitions[(procState, label)] = states[targetStateName]

  return Automaton(procState, transitions)

getFullTypeName(typeExpr):
  match typeExpr:
    PrimitiveModeAlt(isInput):
      return isInput ? '_?' : '_'
    TypeRef(name, isInput):
      return isInput ? name + '?' : name
```

## Path Consistency Checking (Definition 4.3)

### Mode Correspondence Property

When a moded head H' is constructed per the moded-head module (Definition 4.6) using type declaration D, the structural mode at each position in paths(H') equals the mode at the corresponding position in paths(D). This follows directly from the construction: modes are assigned based on the type declaration.

**Consequence:** For variable leaves, path consistency can be checked using only the term path's structural mode—no separate type path traversal or DFA state inspection is required. Specifically:
- Reader X? is consistent iff the path's structural mode at that position is ↓ (consume)
- Writer X is consistent iff the path's structural mode at that position is ↑ (produce)

The DFA is still needed for:
1. Building the moded term (determining argument modes from type declaration)
2. Checking that constants are valid at type positions
3. Assigning types to variables (the DFA state reached)
4. Contravariance/coverage checking

### Path Consistency Algorithm

Path consistency is checked by traversing the appropriate automaton directly. No complement flag needed.

```
checkPathConsistency(termPath, automaton):
  state = automaton.startState

  for i in 0..<termPath.length - 1:
    step = termPath[i]
    nextStep = termPath[i + 1]

    label = TransitionLabel.functor(step.functor, step.arity, nextStep.argIndex, mode: nextStep.mode)

    nextState = automaton.transition(state, label)
    if nextState == null:
      return PathCheckResult.inconsistent("No transition for $label from $state")

    state = nextState

  return checkLeafConsistency(termPath.leaf, state)
```

## Algorithm: Leaf Consistency (Definition 4.3 cases)

Leaf consistency checks whether the term path leaf is consistent with the DFA state reached. By the Mode Correspondence Property, variable mode checking uses only the path's structural mode.

```
checkLeafConsistency(leaf, state, automaton):
  // Case 1: Variable leaf — check path mode matches variable's implicit mode
  if leaf.isVariable:
    if leaf.isReader && leaf.mode == Mode.consume:
      return consistent(type: state)
    if !leaf.isReader && leaf.mode == Mode.produce:
      return consistent(type: state)
    return inconsistent("Variable mode mismatch: reader requires ↓, writer requires ↑")

  // Case 2: Constant leaf — check type accepts this constant

  // Case 2a: At anonymous final state (already matched a constant transition)
  if state.isAnonymousFinal:
    return consistent(type: state)

  // Case 2b: At primitive type state (Integer, Real, Number, String)
  if state.isIntegerType:
    if leaf.isInteger:
      return consistent(type: states['_FINAL_'])
    return inconsistent("Integer type requires integer literal")

  if state.isRealType:
    if leaf.isReal:
      return consistent(type: states['_FINAL_'])
    return inconsistent("Real type requires real literal")

  if state.isNumberType:
    if leaf.isInteger || leaf.isReal:
      return consistent(type: states['_FINAL_'])
    return inconsistent("Number type requires numeric literal")

  if state.isStringType:
    if leaf.isString:
      return consistent(type: states['_FINAL_'])
    return inconsistent("String type requires string literal")

  // Case 2c: At wildcard state — wildcards accept any value
  if state.isWildcard:
    return consistent(type: state)

  // Case 2d: At user-defined type state — check for matching transition
  constLabel = TransitionLabel.constant(leaf.value)
  if automaton.transition(state, constLabel) != null:
    return consistent(type: states['_FINAL_'])

  return inconsistent("Constant does not match any alternative at this type position")
```

## Checking a Procedure Argument

```
checkProcedureArg(procDecl, argIndex, termPath, dfa):
  argType = procDecl.argTypes[argIndex]
  argTypeName = getFullTypeName(argType)  // e.g., "Stream?" or "Stream"

  automaton = dfa.getAutomaton(argTypeName)

  return checkPathConsistency(termPath, automaton)
```

## Error Conditions

| Error | Condition |
|-------|-----------|
| `NoTransitionError` | No matching transition from current state |
| `ModeMismatchError` | Variable mode doesn't match state's expected mode |
| `TypeMismatchError` | Constant doesn't match expected type (Integer/String) |
| `UnknownTypeError` | Type name not found in environment |
| `UnknownProcedureError` | Procedure key not found in environment |

## Examples

### Example 1: Stream Type Automata

```
Stream ::= [] ; [_|Stream].
```

**Stream automaton:**
- Start state: `Stream`
- Transitions:
  - `(Stream, []) → _FINAL_`
  - `(Stream, [|](2,1):↑) → _`
  - `(Stream, [|](2,2):↑) → Stream`

**Stream? automaton:**
- Start state: `Stream?`
- Transitions:
  - `(Stream?, []) → _FINAL_`
  - `(Stream?, [|](2,1):↓) → _?`
  - `(Stream?, [|](2,2):↓) → Stream?`

### Example 2: merge Procedure

```
procedure merge(Stream?, Stream?, Stream).
```

**merge/3 automaton:**
- Start state: `merge/3`
- Transitions:
  - `(merge/3, merge(3,1)) → Stream?`
  - `(merge/3, merge(3,2)) → Stream?`
  - `(merge/3, merge(3,3)) → Stream`

Arguments 1 and 2 use the `Stream?` automaton. Argument 3 uses the `Stream` automaton.

### Example 3: Path Consistency (Paper lines 306-314)

For the first `merge` clause:
```
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
```

**Term path to X?:**
```
(0,↓) --> merge/3 --(1,↓)--> [|]/2 --(1,↓)--> X?
```

**Type path (using Stream? automaton):**
```
(0,↓) --> merge --(1,↓)--> Stream? --(1,↓)--> [|]/2 --(1,↓)--> _?
```

Since arg 1 is declared `Stream?`, we use the `Stream?` automaton which already has:
- Modes: all ↓
- States: `Stream?`, `_?`

The term path has mode ↓ and ends in reader `X?`. The type path has mode ↓ and reaches `_?`. These are consistent by Definition 4.3 case 2(a): reader at consume position.

## Removed from Previous Spec

- `complement` parameter in path checking functions
- Mode flipping during path checking
- Single automaton with flag approach

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.5 | 2025-01-08 | Per-type DFAs with merging |
| 0.6 | 2025-01-10 | Single program DFA; complement flag during checking |
| 0.7 | 2025-01-10 | Integer/String as type states; _FINAL_ for literals |
| 0.8 | 2025-01-10 | Complement automata model: two automata per type, no runtime flag |
| 0.9 | 2025-01-10 | Added Real, Number system types |
| 1.0 | 2025-01-11 | Simplified leaf consistency: Mode Correspondence Property; variable checks use path mode only |
