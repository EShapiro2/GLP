# Module: type-dfa

**Version**: 0.7
**Date**: 2025-01-10
**Status**: DRAFT
**Paper References**: Section 4.1 (lines 32-44), Definition 4.3 (lines 247-262)

## Purpose

Represents the single DFA for a typed GLP program. The DFA is built once from the type environment. Path consistency checking uses a complement flag to flip mode expectations for `T?` positions.

## Dependencies

- `mode` — Mode enum
- `type-environment` — TypeEnvironment, TypeDef, ProcDecl

## DFA Structure (Paper lines 42-44)

A typed GLP program P = (Cs, D) has a **single DFA** where:

**States:**
- One state per defined type name (e.g., `Stream`, `CounterCall`)
- One state per procedure (e.g., `merge/3`, `sum/2`)
- System type states: `Integer`, `String` (NOT final; have conceptual transitions to `_FINAL_`)
- Final states: `_` (any produced term), `_?` (any consumed term), `_FINAL_` (anonymous final for constant/literal matches)

**Transitions:**
- From procedure states: labeled (procName, arity, argIndex), leading to argument type states
- From defined type states: labeled (functor, arity, argIndex, mode), leading to type states or final states
- From `Integer`/`String`: conceptually one per literal value, all leading to `_FINAL_`; implemented as type membership check rather than explicit transitions
- Constant alternatives (e.g., `[]`): transition labeled with the constant, leading to `_FINAL_`

**No separate DFAs per type. No merging. No `withSuffix`.**

## Complementation (Paper line 19)

Complementation does **not** create additional states. When checking paths at a `T?` position, mode expectations are flipped during traversal:
- Where `T` expects produce ↑, check expects consume ↓
- Where `T` expects consume ↓, check expects produce ↑

This preserves the involution property: flipping twice returns to original expectations.

## Public Interface

### Types

#### `class ProgramDFA`

The single DFA for a typed GLP program.

```dart
class ProgramDFA {
  final Map<String, DFAState> states;
  final Map<(DFAState, TransitionLabel), DFAState> transitions;

  DFAState getState(String name);  // Type name or procedure key
  DFAState? transition(DFAState from, TransitionLabel label);
}
```

#### `class DFAState`

```dart
class DFAState {
  final String name;
  final bool isFinal;

  bool get isWildcard => name == '_' || name == '_?';
  bool get isProducedWildcard => name == '_';
  bool get isConsumedWildcard => name == '_?';
  bool get isIntegerType => name == 'Integer';
  bool get isStringType => name == 'String';
  bool get isAnonymousFinal => name == '_FINAL_';
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
}
```

### Functions

#### `ProgramDFA buildProgramDFA(TypeEnvironment env)`

Builds the single DFA from the type environment.

## Algorithm: Build Program DFA

```
buildProgramDFA(env):
  states = {}
  transitions = {}

  // Create final states (only _ and _? are true finals accepting variables)
  states['_'] = DFAState('_', isFinal: true)
  states['_?'] = DFAState('_?', isFinal: true)
  states['_FINAL_'] = DFAState('_FINAL_', isFinal: true)  // anonymous final for constants/literals

  // Create system type states (NOT final - they have conceptual transitions to _FINAL_)
  states['Integer'] = DFAState('Integer', isFinal: false)
  states['String'] = DFAState('String', isFinal: false)

  // Integer and String have conceptual transitions to _FINAL_ for any literal.
  // These are not explicitly enumerated; instead, checkLeafConsistency performs
  // a membership check when the current state is Integer or String.

  // Create states for defined types
  for typeName in env.types.keys:
    states[typeName] = DFAState(typeName, isFinal: false)

  // Create states for procedures
  for procKey in env.procedures.keys:
    states[procKey] = DFAState(procKey, isFinal: false)

  // Add transitions from type definitions
  for (typeName, typeDef) in env.types:
    fromState = states[typeName]
    for alt in typeDef.alternatives:
      addTypeTransitions(fromState, alt, Mode.produce, states, transitions)

  // Add transitions from procedure declarations
  for (procKey, procDecl) in env.procedures:
    fromState = states[procKey]
    for i in 0..<procDecl.arity:
      argType = procDecl.argTypes[i]
      label = TransitionLabel.functor(procDecl.name, procDecl.arity, i+1, mode: null)
      targetState = resolveTypeExpr(argType, states)
      transitions[(fromState, label)] = targetState

  return ProgramDFA(states, transitions)
```

## Algorithm: Add Type Transitions

```
addTypeTransitions(fromState, alt, contextMode, states, transitions):
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

      headLabel = TransitionLabel.functor('[|]', 2, 1, mode: headMode)
      tailLabel = TransitionLabel.functor('[|]', 2, 2, mode: tailMode)

      transitions[(fromState, headLabel)] = resolveTypeExpr(headType, states)
      transitions[(fromState, tailLabel)] = resolveTypeExpr(tailType, states)

    StructAlt(functor, args):
      for i in 0..<args.length:
        argType = args[i]
        argMode = modeOf(argType, contextMode)
        label = TransitionLabel.functor(functor, args.length, i+1, mode: argMode)
        transitions[(fromState, label)] = resolveTypeExpr(argType, states)

    DiffListAlt(content, hole):
      contentMode = modeOf(content, contextMode)
      holeMode = modeOf(hole, contextMode)

      contentLabel = TransitionLabel.functor('\\', 2, 1, mode: contentMode)
      holeLabel = TransitionLabel.functor('\\', 2, 2, mode: holeMode)

      transitions[(fromState, contentLabel)] = resolveTypeExpr(content, states)
      transitions[(fromState, holeLabel)] = resolveTypeExpr(hole, states)
```

## Algorithm: Resolve Type Expression

```
resolveTypeExpr(typeExpr, states):
  match typeExpr:
    PrimitiveModeAlt(isInput):
      // _ and _? are final states
      return isInput ? states['_?'] : states['_']

    TypeRef(name, isInput):
      // Integer and String are type states (not final)
      // They have conceptual transitions to _FINAL_ for each literal
      if name == 'Integer': return states['Integer']
      if name == 'String': return states['String']
      // Note: isInput flag is NOT used here - complementation happens during path checking
      return states[name]
```

## Algorithm: Mode Computation

```
modeOf(typeExpr, contextMode):
  // T? flips mode, T keeps mode
  if typeExpr is TypeRef && typeExpr.isInput:
    return contextMode.flip
  if typeExpr is PrimitiveModeAlt && typeExpr.isInput:
    return contextMode.flip
  return contextMode
```

## Path Consistency Checking (Definition 4.3)

Path consistency is checked by traversing the DFA alongside a moded term path. The `complement` flag indicates whether modes should be flipped (for `T?` positions).

```
checkPathConsistency(termPath, dfa, startState, complement):
  state = startState

  for i in 0..<termPath.length - 1:
    step = termPath[i]
    nextStep = termPath[i + 1]

    // Build expected label with mode adjustment for complement
    expectedMode = nextStep.mode
    if complement:
      expectedMode = expectedMode.flip

    label = TransitionLabel.functor(step.functor, step.arity, nextStep.argIndex, mode: expectedMode)

    // Follow transition
    nextState = dfa.transition(state, label)
    if nextState == null:
      return PathCheckResult.inconsistent("No transition for $label from $state")

    state = nextState

  // Check leaf consistency
  return checkLeafConsistency(termPath.leaf, state, dfa, complement)
```

## Algorithm: Leaf Consistency (Definition 4.3 cases)

```
checkLeafConsistency(leaf, state, dfa, complement):
  expectedMode = leaf.mode
  if complement:
    expectedMode = expectedMode.flip

  // Case: Produced wildcard final state (_)
  if state.isProducedWildcard:
    // Definition 4.3 case 3(b): type path ends in _ and term has produce mode
    if leaf.isVariable && !leaf.isReader && expectedMode == Mode.produce:
      return consistent(type: state)
    return inconsistent("_ expects writer at produce position")

  // Case: Consumed wildcard final state (_?)
  if state.isConsumedWildcard:
    // Definition 4.3 case 3(a): type path ends in _? and term has consume mode
    if leaf.isVariable && leaf.isReader && expectedMode == Mode.consume:
      return consistent(type: state)
    return inconsistent("_? expects reader at consume position")

  // Case: Integer type state (conceptual infinite transitions)
  if state.isIntegerType:
    if leaf.isInteger:
      // Conceptually: follow transition labeled with this integer to _FINAL_
      return consistent(type: dfa.states['_FINAL_'])
    if leaf.isVariable:
      // Definition 4.3 case 2: term path is prefix ending in variable
      if leaf.isReader && expectedMode == Mode.consume:
        return consistent(type: state)
      if !leaf.isReader && expectedMode == Mode.produce:
        return consistent(type: state)
      return inconsistent("Variable mode mismatch at Integer")
    return inconsistent("Integer type requires integer literal or variable")

  // Case: String type state (conceptual infinite transitions)
  if state.isStringType:
    if leaf.isString:
      // Conceptually: follow transition labeled with this string to _FINAL_
      return consistent(type: dfa.states['_FINAL_'])
    if leaf.isVariable:
      if leaf.isReader && expectedMode == Mode.consume:
        return consistent(type: state)
      if !leaf.isReader && expectedMode == Mode.produce:
        return consistent(type: state)
      return inconsistent("Variable mode mismatch at String")
    return inconsistent("String type requires string literal or variable")

  // Case: Anonymous final state (reached via exact constant match)
  if state.isAnonymousFinal:
    // Definition 4.3 case 1: equal length, last symbols consistent
    // We only reach here if a constant transition was followed, so it matched
    return consistent(type: state)

  // Case: Non-final type state with variable
  // Definition 4.3 case 2: term path is prefix ending in reader/writer
  if leaf.isVariable:
    if leaf.isReader && expectedMode == Mode.consume:
      return consistent(type: state)  // Case 2(a)
    if !leaf.isReader && expectedMode == Mode.produce:
      return consistent(type: state)  // Case 2(b)
    return inconsistent("Variable mode mismatch at type position")

  // Case: Non-final type state with constant - must follow transition
  // Definition 4.3 case 1: check if constant matches a transition
  constLabel = TransitionLabel.constant(leaf.value)
  nextState = dfa.transition(state, constLabel)
  if nextState != null:
    return consistent(type: nextState)

  return inconsistent("Constant at type state without matching transition")
```

## Procedure Argument Complement Flag

When checking a clause head or body atom against a procedure declaration, each argument may need the complement flag:

```
getArgumentComplement(procDecl, argIndex):
  argType = procDecl.argTypes[argIndex]

  if argType is TypeRef:
    return argType.isInput  // T? means complement=true

  if argType is PrimitiveModeAlt:
    return argType.isInput  // _? means complement=true

  return false
```

For heads (callee's view), an additional global complement is applied due to the caller/callee perspective flip. This is detailed in the well-typed-clause module.

## Error Conditions

| Error | Condition |
|-------|-----------|
| `NoTransitionError` | No matching transition from current state |
| `ModeMismatchError` | Variable mode doesn't match expected mode at position |
| `TypeMismatchError` | Constant doesn't match expected type (Integer/String) |
| `UnknownTypeError` | Type name not found in environment |
| `UnknownProcedureError` | Procedure key not found in environment |

## Examples

### Example 1: Stream Type DFA

```
Stream ::= [] ; [_|Stream].
```

**States:** `Stream`, `_`, `_FINAL_`

**Transitions:**
- `(Stream, []) → _FINAL_`
- `(Stream, [|](2,1):↑) → _`
- `(Stream, [|](2,2):↑) → Stream`

### Example 2: merge Procedure

```
procedure merge(Stream?, Stream?, Stream).
```

**Additional state:** `merge/3`

**Transitions:**
- `(merge/3, merge(3,1)) → Stream`
- `(merge/3, merge(3,2)) → Stream`
- `(merge/3, merge(3,3)) → Stream`

Note: All three arguments point to the same `Stream` state. The complement flag is:
- Arg 1: complement=true (declared as `Stream?`)
- Arg 2: complement=true (declared as `Stream?`)
- Arg 3: complement=false (declared as `Stream`)

### Example 3: Path Consistency Check

For the first `merge` clause:
```
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
```
with type `merge(Stream?, Stream?, Stream)`.

**Step 1: Construct moded head** (after variable flip per Definition 4.7):
```
H' = ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
```

**Step 2: Extract term path** to first element of first argument:
```
Term path: (0,↓) --> merge/3 --(1,↓)--> [|]/2 --(1,↓)--> X?
```

**Step 3: Build type path** by traversing DFA with complement:

The DFA has:
- `(merge/3, merge(3,1)) → Stream`
- `(Stream, [|](2,1):↑) → _`

Since arg 1 is declared `Stream?`, we set complement=true. This means when we record the type path, we flip the DFA's modes:

```
Type path: (0,↓) --> merge/3 --(1,↓)--> Stream? --(1,↓)--> [|]/2 --(1,↓)--> _?
```

Note: The DFA transition has mode ↑, but with complement=true, the type path records mode ↓.

**Step 4: Check consistency** (Definition 4.3 case 2a):
- Term path ends in reader `X?`
- Type path at corresponding position has mode ↓ (consume)
- ✓ Consistent! Variable `X?` is assigned type `_?`

---

**Key clarification:**

| Concept | Source | Modes |
|---------|--------|-------|
| Term path | Extracted from clause/moded head | Based on reader/writer annotations and procedure type |
| DFA transitions | Type definitions | Always from producer's view (↑ by default) |
| Type path | Traversal of DFA | DFA modes, flipped if complement=true |
| Consistency check | Compare term path vs type path | Per Definition 4.3 |

## Removed from Previous Spec

- `withSuffix()` — no longer needed
- `applyModeComplement()` — replaced by `complement` flag during checking
- `primitiveStateModes` map — replaced by checking state names (`_` vs `_?`)
- Separate per-type DFA compilation — single program DFA
- DFA merging for procedures — procedures are states in the same DFA
- `TypeDFA` class — replaced by `ProgramDFA`

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.5 | 2025-01-08 | Previous version with per-type DFAs |
| 0.6 | 2025-01-10 | Single program DFA; complement during checking not construction |
| 0.7 | 2025-01-10 | Integer/String as type states (not final); _FINAL_ for literals |
