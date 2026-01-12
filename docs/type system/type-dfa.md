# Module: type-dfa

**Version**: 1.0
**Date**: 2025-01-12
**Status**: DRAFT
**Paper References**: Section 4.2 (Type Automaton Definition), Definition 4.X (Type Automaton), Definition 4.X (Transition Function), Definition 4.X (Complementation as Involution)

## Purpose

Represents the type automaton for a typed GLP program. The automaton formally defines well-typing via path acceptance. Each type T has a complement type T? with a corresponding complement automaton derived by flipping states and modes.

## Dependencies

- `mode` — Mode enum
- `type-environment` — TypeEnvironment, TypeDef, ProcDecl, TypeAlternative

## Formal Definition: Type Automaton (Paper Section 4.2)

### Definition: Type Automaton

Given a typed GLP program P = (Cs, D), the **type automaton** is a tuple A_D = (Q, Σ, δ, q₀, F) where:

**Q (States):** The set of states consists of:
- **User-defined type states:** For each type T defined in D, states T and T?
- **Procedure states:** For each procedure p/n declared in D, state p/n
- **Primitive type states:** Integer, Integer?, Real, Real?, Number, Number?, String, String?
- **Wildcard states:** _ and _? (final states for any term)
- **Final state:** ✓ (for matched constants/literals)

**Σ (Alphabet):** Transition labels are tuples (f, n, i, m) where:
- f is a functor name or constant value
- n is the arity (0 for constants)
- i is the argument position (1-based, 0 for constants)
- m ∈ {↑, ↓} is the mode (null for procedure transitions)

**δ (Transition Function):** See Definition below.

**q₀ (Initial States):** Each procedure state p/n serves as an initial state for checking arguments of that procedure.

**F (Final States):** The set {_, _?, ✓} plus Integer, Integer?, Real, Real?, Number, Number?, String, String? when matching literals.

### Definition: Transition Function

The transition function δ is defined by cases:

**1. Procedure Transitions:**
For procedure p/n with declared type (T₁^m₁, ..., Tₙ^mₙ):
```
δ(p/n, (p, n, i, mᵢ)) = Tᵢ? if mᵢ = ↓ (input)
δ(p/n, (p, n, i, mᵢ)) = Tᵢ  if mᵢ = ↑ (output)
```
where mᵢ is ↓ for input types (T?) and ↑ for output types (T).

**2. Type Definition Transitions:**
For type T with alternative f(S₁^m₁, ..., Sₖ^mₖ):
```
δ(T, (f, k, i, mᵢ)) = resolve(Sᵢ, mᵢ, false)
```
where mᵢ is computed from the type expression Sᵢ:
- If Sᵢ is T' (non-complement): mᵢ = ↑ (produce)
- If Sᵢ is T'? (complement): mᵢ = ↓ (consume)

**3. Complement Type Transitions:**
For complement type T?:
```
δ(T?, (f, k, i, m̄ᵢ)) = resolve(Sᵢ, mᵢ, true)
```
where m̄ᵢ is the flipped mode.

**4. Resolution Function:**
```
resolve(typeExpr, baseMode, inComplement):
  if typeExpr is PrimitiveType(name, isInput):
    effectiveComplement = isInput XOR inComplement
    return effectiveComplement ? name + '?' : name
  
  if typeExpr is TypeRef(name, isInput):
    effectiveComplement = isInput XOR inComplement
    return effectiveComplement ? name + '?' : name
```

**5. Constant Transitions:**
For any state with a constant alternative c:
```
δ(S, (c, 0, ↑)) = ✓  for state S
δ(S?, (c, 0, ↓)) = ✓  for complement state S?
```

**6. Primitive Type Transitions:**
```
δ(Integer, (k, 0, ↑)) = ✓   for any integer literal k
δ(Integer?, (k, 0, ↓)) = ✓  for any integer literal k
δ(Real, (r, 0, ↑)) = ✓      for any real literal r
δ(Real?, (r, 0, ↓)) = ✓     for any real literal r
δ(Number, (n, 0, ↑)) = ✓    for any numeric literal n
δ(Number?, (n, 0, ↓)) = ✓   for any numeric literal n
δ(String, (s, 0, ↑)) = ✓    for any string literal s
δ(String?, (s, 0, ↓)) = ✓   for any string literal s
```

**7. Wildcard States:**
The states _ and _? are final states that accept any term:
- _ accepts any writer variable at mode ↑
- _? accepts any reader variable at mode ↓

### Definition: Complementation as Involution

For each type T, the automaton for T? is obtained from the automaton for T by:
1. Replacing each state S with its complement state S?
2. Replacing each mode: ↑ becomes ↓, and ↓ becomes ↑

This defines complementation as an involution: (T?)? = T, since flipping states and modes twice returns to the original automaton.

## Public Interface

### Types

#### `class DFAState`

```dart
class DFAState {
  final String baseName;      // e.g., "Stream", "Integer", "_", "merge/3"
  final bool isComplement;    // true for Stream?, Integer?, _?
  final bool isFinal;         // true for _, _?, ✓
  final bool isProcedure;     // true for procedure states

  String get name => isComplement ? '$baseName?' : baseName;

  DFAState get complement => DFAState(
    baseName, 
    isComplement: !isComplement, 
    isFinal: isFinal,
    isProcedure: isProcedure
  );

  // State classification
  bool get isWildcard => baseName == '_';
  bool get isProducedWildcard => baseName == '_' && !isComplement;
  bool get isConsumedWildcard => baseName == '_' && isComplement;
  bool get isIntegerType => baseName == 'Integer';
  bool get isRealType => baseName == 'Real';
  bool get isNumberType => baseName == 'Number';
  bool get isStringType => baseName == 'String';
  bool get isNumericType => isIntegerType || isRealType || isNumberType;
  bool get isPrimitiveType => isWildcard || isIntegerType || isRealType || 
                              isNumberType || isStringType;
  bool get isAnonymousFinal => baseName == '✓';
  bool get isUserDefinedType => !isPrimitiveType && !isProcedure && !isAnonymousFinal;
}
```

#### `class TransitionLabel`

```dart
class TransitionLabel {
  final String symbol;    // functor name or constant value
  final int arity;        // 0 for constants
  final int argIndex;     // 1-based, 0 for constants
  final Mode? mode;       // null for procedure arg transitions

  factory TransitionLabel.functor(String name, int arity, int argIndex, {Mode? mode});
  factory TransitionLabel.constant(Object value);
  factory TransitionLabel.procedureArg(String procName, int procArity, int argIndex);

  TransitionLabel get complement => TransitionLabel(
    symbol, arity, argIndex, 
    mode: mode?.flip
  );
  
  @override
  bool operator ==(Object other);
  
  @override
  int get hashCode;
}
```

#### `class Automaton`

An automaton for a single type (either T or T?) or procedure.

```dart
class Automaton {
  final DFAState startState;
  final Map<(DFAState, TransitionLabel), DFAState> transitions;
  final Set<DFAState> finalStates;

  /// Follow a transition, returning null if no transition exists
  DFAState? transition(DFAState from, TransitionLabel label);
  
  /// Get all outgoing transitions from a state
  List<(TransitionLabel, DFAState)> getTransitions(DFAState from);

  /// Create complement automaton by flipping all states and modes
  Automaton get complement;
}
```

#### `class ProgramDFA`

The complete type automaton for a typed GLP program.

```dart
class ProgramDFA {
  final Map<String, DFAState> states;       // All states by name
  final Map<String, Automaton> automata;    // One per type/procedure name
  final DFAState finalState;                // The ✓ state

  DFAState getState(String name);           // e.g., "Stream" or "Stream?"
  Automaton getAutomaton(String typeName);  // e.g., "Stream" or "Stream?"
  
  /// Get automaton for a procedure
  Automaton getProcedureAutomaton(String procKey);  // e.g., "merge/3"
}
```

### Functions

#### `ProgramDFA buildProgramDFA(TypeEnvironment env)`

Builds the complete type automaton from the type environment.

**Preconditions:**
- `env` contains valid type definitions and procedure declarations
- All types pass determinism and alias checks

**Postconditions:**
- Returns a ProgramDFA with automata for all types and procedures
- Each type T has automata for both T and T?
- Complement automata satisfy the involution property

## Algorithm: Build Program DFA

```
buildProgramDFA(env):
  states = {}
  automata = {}

  // Create anonymous final state
  states['✓'] = DFAState('✓', isComplement: false, isFinal: true)

  // Create wildcard states (final states)
  states['_'] = DFAState('_', isComplement: false, isFinal: true)
  states['_?'] = DFAState('_', isComplement: true, isFinal: true)
  automata['_'] = wildcardAutomaton(states['_'])
  automata['_?'] = wildcardAutomaton(states['_?'])

  // Create primitive type states
  for baseName in ['Integer', 'Real', 'Number', 'String']:
    states[baseName] = DFAState(baseName, isComplement: false, isFinal: false)
    states[baseName + '?'] = DFAState(baseName, isComplement: true, isFinal: false)
    automata[baseName] = primitiveAutomaton(states[baseName], baseName, states['✓'])
    automata[baseName + '?'] = primitiveAutomaton(states[baseName + '?'], baseName, states['✓'])

  // Create states for user-defined types
  for (typeName, typeDef) in env.types:
    states[typeName] = DFAState(typeName, isComplement: false, isFinal: false)
    states[typeName + '?'] = DFAState(typeName, isComplement: true, isFinal: false)

  // Build automata for user-defined types
  for (typeName, typeDef) in env.types:
    automata[typeName] = buildTypeAutomaton(typeDef, states, env, isComplement: false)
    automata[typeName + '?'] = buildTypeAutomaton(typeDef, states, env, isComplement: true)

  // Create procedure states
  for (procKey, procDecl) in env.procedures:
    states[procKey] = DFAState(procKey, isComplement: false, isFinal: false, isProcedure: true)

  // Build procedure automata
  for (procKey, procDecl) in env.procedures:
    automata[procKey] = buildProcedureAutomaton(procDecl, states)

  return ProgramDFA(states, automata, states['✓'])
```

## Algorithm: Build Type Automaton

```
buildTypeAutomaton(typeDef, states, env, isComplement):
  typeName = typeDef.name
  startStateName = isComplement ? typeName + '?' : typeName
  startState = states[startStateName]

  transitions = {}
  finalStates = {states['✓'], states['_'], states['_?']}

  for alt in typeDef.alternatives:
    addTypeTransitions(startState, alt, states, transitions, env, isComplement)

  return Automaton(startState, transitions, finalStates)
```

## Algorithm: Add Type Transitions

```
addTypeTransitions(fromState, alt, states, transitions, env, isComplement):
  match alt:
    ConstantAlt(value):
      // Constant transition to final state
      mode = isComplement ? Mode.consume : Mode.produce
      label = TransitionLabel.constant(value, mode: mode)
      transitions[(fromState, label)] = states['✓']

    ListNilAlt:
      mode = isComplement ? Mode.consume : Mode.produce
      label = TransitionLabel.constant('[]', mode: mode)
      transitions[(fromState, label)] = states['✓']

    ListConsAlt(headType, tailType):
      // Head transition
      headBaseMode = headType.isInput ? Mode.consume : Mode.produce
      headMode = isComplement ? headBaseMode.flip : headBaseMode
      headLabel = TransitionLabel.functor('[|]', 2, 1, mode: headMode)
      headTarget = resolveTargetState(headType, states, isComplement)
      transitions[(fromState, headLabel)] = headTarget
      
      // Tail transition
      tailBaseMode = tailType.isInput ? Mode.consume : Mode.produce
      tailMode = isComplement ? tailBaseMode.flip : tailBaseMode
      tailLabel = TransitionLabel.functor('[|]', 2, 2, mode: tailMode)
      tailTarget = resolveTargetState(tailType, states, isComplement)
      transitions[(fromState, tailLabel)] = tailTarget

    StructAlt(functor, argTypes):
      for i in 0..<argTypes.length:
        argType = argTypes[i]
        argBaseMode = argType.isInput ? Mode.consume : Mode.produce
        argMode = isComplement ? argBaseMode.flip : argBaseMode
        label = TransitionLabel.functor(functor, argTypes.length, i+1, mode: argMode)
        target = resolveTargetState(argType, states, isComplement)
        transitions[(fromState, label)] = target

    DiffListAlt(contentType, holeType):
      // Content transition (first arg of \)
      contentBaseMode = contentType.isInput ? Mode.consume : Mode.produce
      contentMode = isComplement ? contentBaseMode.flip : contentBaseMode
      contentLabel = TransitionLabel.functor('\\', 2, 1, mode: contentMode)
      contentTarget = resolveTargetState(contentType, states, isComplement)
      transitions[(fromState, contentLabel)] = contentTarget
      
      // Hole transition (second arg of \)
      holeBaseMode = holeType.isInput ? Mode.consume : Mode.produce
      holeMode = isComplement ? holeBaseMode.flip : holeBaseMode
      holeLabel = TransitionLabel.functor('\\', 2, 2, mode: holeMode)
      holeTarget = resolveTargetState(holeType, states, isComplement)
      transitions[(fromState, holeLabel)] = holeTarget
```

## Algorithm: Resolve Target State

```
resolveTargetState(typeExpr, states, inComplement):
  match typeExpr:
    PrimitiveType(baseName, isInput):
      // XOR: input type in complement context = non-complement state
      effectiveComplement = isInput XOR inComplement
      stateName = effectiveComplement ? baseName + '?' : baseName
      return states[stateName]
    
    TypeRef(typeName, isInput, _):
      effectiveComplement = isInput XOR inComplement
      stateName = effectiveComplement ? typeName + '?' : typeName
      return states[stateName]
```

## Algorithm: Build Procedure Automaton

```
buildProcedureAutomaton(procDecl, states):
  procState = states[procDecl.key]
  transitions = {}

  for i in 0..<procDecl.arity:
    argType = procDecl.argTypes[i]
    
    // Procedure transitions don't have mode in the label
    label = TransitionLabel.procedureArg(procDecl.name, procDecl.arity, i+1)
    
    // Target is the declared type directly
    targetStateName = argType.isInput ? 
      argType.baseName + '?' : 
      argType.baseName
    transitions[(procState, label)] = states[targetStateName]

  return Automaton(procState, transitions, {})
```

## Path Consistency Checking

Path consistency is checked by traversing the appropriate automaton. The automaton for the declared argument type is used directly (no runtime complement flag needed).

```
checkPathConsistency(termPath, automaton, dfa):
  state = automaton.startState

  // Traverse non-leaf steps
  for i in 0..<termPath.length - 1:
    step = termPath[i]
    nextStep = termPath[i + 1]

    label = TransitionLabel.functor(
      step.functor, 
      step.arity, 
      nextStep.argIndex, 
      mode: nextStep.mode
    )

    nextState = automaton.transition(state, label)
    if nextState == null:
      return PathCheckResult.inconsistent(
        "No transition for $label from ${state.name}"
      )

    // IMPORTANT: If we transition to a different type, switch automata
    if nextState.isUserDefinedType && nextState.baseName != state.baseName:
      automaton = dfa.getAutomaton(nextState.name)

    state = nextState

  return checkLeafConsistency(termPath.leaf, state, dfa)
```

## Algorithm: Leaf Consistency

```
checkLeafConsistency(leaf, state, dfa):
  // Case: Wildcard final states
  if state.isProducedWildcard:
    if leaf.isVariable && !leaf.isReader:
      return consistent(type: state)
    return inconsistent("_ expects writer variable")

  if state.isConsumedWildcard:
    if leaf.isVariable && leaf.isReader:
      return consistent(type: state)
    return inconsistent("_? expects reader variable")

  // Case: Primitive type states with literal
  if state.isIntegerType:
    if leaf.isInteger:
      return consistent(type: dfa.finalState)
    if leaf.isVariable:
      return checkVariableAtPrimitiveState(leaf, state)
    return inconsistent("Integer type expects integer literal or variable")

  if state.isRealType:
    if leaf.isReal:
      return consistent(type: dfa.finalState)
    if leaf.isVariable:
      return checkVariableAtPrimitiveState(leaf, state)
    return inconsistent("Real type expects real literal or variable")

  if state.isNumberType:
    if leaf.isInteger || leaf.isReal:
      return consistent(type: dfa.finalState)
    if leaf.isVariable:
      return checkVariableAtPrimitiveState(leaf, state)
    return inconsistent("Number type expects numeric literal or variable")

  if state.isStringType:
    if leaf.isString:
      return consistent(type: dfa.finalState)
    if leaf.isVariable:
      return checkVariableAtPrimitiveState(leaf, state)
    return inconsistent("String type expects string literal or variable")

  // Case: Anonymous final state
  if state.isAnonymousFinal:
    return consistent(type: state)

  // Case: User-defined type state with variable
  if state.isUserDefinedType && leaf.isVariable:
    return checkVariableAtTypeState(leaf, state)

  // Case: Constant at type state - check for matching transition
  if leaf.isConstant:
    mode = state.isComplement ? Mode.consume : Mode.produce
    constLabel = TransitionLabel.constant(leaf.value, mode: mode)
    if automaton.transition(state, constLabel) != null:
      return consistent(type: dfa.finalState)
    return inconsistent("Constant ${leaf.value} has no transition from ${state.name}")

  return inconsistent("Unexpected leaf at state ${state.name}")

checkVariableAtPrimitiveState(leaf, state):
  expectedMode = state.isComplement ? Mode.consume : Mode.produce
  if leaf.isReader && state.isComplement:
    return consistent(type: state)
  if !leaf.isReader && !state.isComplement:
    return consistent(type: state)
  return inconsistent("Variable mode mismatch at ${state.name}")

checkVariableAtTypeState(leaf, state):
  if leaf.isReader && state.isComplement:
    return consistent(type: state)
  if !leaf.isReader && !state.isComplement:
    return consistent(type: state)
  return inconsistent("Variable mode mismatch at ${state.name}")
```

## Examples

### Example 1: Stream Type Automata

```
Stream ::= [] ; [_|Stream].
```

**Stream automaton (producer view):**
- Start state: `Stream`
- Transitions:
  - δ(Stream, ([], 0, ↑)) → ✓
  - δ(Stream, ([|], 2, 1, ↑)) → _
  - δ(Stream, ([|], 2, 2, ↑)) → Stream

**Stream? automaton (consumer view):**
- Start state: `Stream?`
- Transitions:
  - δ(Stream?, ([], 0, ↓)) → ✓
  - δ(Stream?, ([|], 2, 1, ↓)) → _?
  - δ(Stream?, ([|], 2, 2, ↓)) → Stream?

### Example 2: Interactive Type (HollowIntegers)

```
HollowIntegers ::= [] ; [Integer?|HollowIntegers].
```

**HollowIntegers automaton:**
- Start state: `HollowIntegers`
- Transitions:
  - δ(HollowIntegers, ([], 0, ↑)) → ✓
  - δ(HollowIntegers, ([|], 2, 1, ↓)) → Integer?  (mode flipped due to Integer?)
  - δ(HollowIntegers, ([|], 2, 2, ↑)) → HollowIntegers

**HollowIntegers? automaton:**
- Start state: `HollowIntegers?`
- Transitions:
  - δ(HollowIntegers?, ([], 0, ↓)) → ✓
  - δ(HollowIntegers?, ([|], 2, 1, ↑)) → Integer  (double flip = no flip)
  - δ(HollowIntegers?, ([|], 2, 2, ↓)) → HollowIntegers?

### Example 3: CounterCall (Multiple Alternatives)

```
CounterCall ::= add ; clear ; read(Integer?).
```

**CounterCall automaton:**
- Start state: `CounterCall`
- Transitions:
  - δ(CounterCall, (add, 0, ↑)) → ✓
  - δ(CounterCall, (clear, 0, ↑)) → ✓
  - δ(CounterCall, (read, 1, 1, ↓)) → Integer?

**CounterCall? automaton:**
- Start state: `CounterCall?`
- Transitions:
  - δ(CounterCall?, (add, 0, ↓)) → ✓
  - δ(CounterCall?, (clear, 0, ↓)) → ✓
  - δ(CounterCall?, (read, 1, 1, ↑)) → Integer

### Example 4: merge Procedure

```
procedure merge(Stream?, Stream?, Stream).
```

**merge/3 automaton:**
- Start state: `merge/3`
- Transitions:
  - δ(merge/3, (merge, 3, 1)) → Stream?
  - δ(merge/3, (merge, 3, 2)) → Stream?
  - δ(merge/3, (merge, 3, 3)) → Stream

### Example 5: Channel (Parametric Type)

```
Channel(X) ::= ch(Stream(X)?, Stream(X)).
```

**Channel(X) automaton:**
- Start state: `Channel(X)`
- Transitions:
  - δ(Channel(X), (ch, 2, 1, ↓)) → Stream(X)?
  - δ(Channel(X), (ch, 2, 2, ↑)) → Stream(X)

**Channel(X)? automaton:**
- Start state: `Channel(X)?`
- Transitions:
  - δ(Channel(X)?, (ch, 2, 1, ↑)) → Stream(X)
  - δ(Channel(X)?, (ch, 2, 2, ↓)) → Stream(X)?

### Example 6: Path Consistency with Type Boundary Crossing

For `monitor(N, [read(N?)|In])` with type `monitor(Integer?, Stream(CounterCall)?)`:

Path to N? inside read:
```
(0,↓) → monitor/2 --(2,↓)--> Stream(CounterCall)? --(1,↓)--> CounterCall? --(1,↑)--> Integer
```

Traversal:
1. Start at `monitor/2`
2. Follow (monitor, 2, 2) → `Stream(CounterCall)?`
3. Switch to `Stream(CounterCall)?` automaton
4. Follow ([|], 2, 1, ↓) → `CounterCall?`
5. Switch to `CounterCall?` automaton
6. Follow (read, 1, 1, ↑) → `Integer`
7. Leaf N? is reader, but position mode is ↑ → need writer N

This demonstrates the automaton boundary crossing that handles nested compound types.

## Error Conditions

| Error | Condition |
|-------|-----------|
| `NoTransitionError` | No matching transition from current state |
| `ModeMismatchError` | Variable mode doesn't match state's expected mode |
| `TypeMismatchError` | Literal doesn't match expected primitive type |
| `UnknownTypeError` | Type name not found in states |
| `UnknownProcedureError` | Procedure key not found in states |

## Changes from v0.9

- Added formal Type Automaton definition (Definition 4.X)
- Added formal Transition Function definition
- Added Complementation as Involution definition
- Added `isProcedure` and `isUserDefinedType` to DFAState
- Added automaton switching at type boundaries in path checking
- Added parametric type example (Channel)
- Restructured algorithms to match formal definitions

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.5 | 2025-01-08 | Per-type DFAs with merging |
| 0.6 | 2025-01-10 | Single program DFA; complement flag during checking |
| 0.7 | 2025-01-10 | Integer/String as type states; ✓ for literals |
| 0.8 | 2025-01-10 | Complement automata model: two automata per type |
| 0.9 | 2025-01-10 | Added Real, Number system types |
| 1.0 | 2025-01-12 | Formal automaton definition from paper Section 4.2; automaton switching at type boundaries |
