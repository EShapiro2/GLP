# Module: type-dfa

**Version**: 0.1  
**Date**: 2025-01-07  
**Status**: DRAFT  
**Paper References**: Section 4.1 (lines 7-35), lines 213-226

## Purpose

Compiles GLP type definitions directly to DFAs and provides operations for working with moded type paths. Types are deterministic by construction—each state has at most one transition per symbol.

## Dependencies

- `moded-term` — Mode enum

## Definitions

### DFA Correspondence (Paper line 30)

> "We impose standard restrictions on GLP type definitions so that they correspond to a DFA in which every state is a defined type, except for final states which are primitive types."

GLP type definitions compile **directly** to a DFA:
- Each defined type name is a DFA state
- Each alternative adds transitions from that state
- Primitive types (`_`, `_?`) mark accepting states
- Constants (`[]`, `1`, etc.) lead to accepting states

### Determinism Requirement (Paper lines 31-35)

Type definitions must be deterministic. Alternatives must have **distinct leading symbols**:

```
Stream ::= [] ; [_|Stream].    % LEGAL: [] vs [|] are different symbols
```

The following are **illegal** (same symbol, different modes):
```
Any ::= _ ; _?.                % ILLEGAL: same position, different modes
AnyOne ::= 1 ; 1?.             % ILLEGAL: same constant, different modes
```

### Type Paths (Paper line 213)

A defined GLP type D defines a regular set of moded paths, denoted **paths(D)**:
- Every intermediate symbol is a type name
- The last symbol is a primitive type (`_`, `_?`, `Integer`, `String`, or constant)
- Edges are labelled with `(argIndex, mode)`

### Primitive Types (Paper page 6)

| Primitive Type | Meaning |
|----------------|---------|
| `_` | Any produced term |
| `_?` | Any consumed term |
| `Integer` | Any integer constant |
| `String` | Any string constant |
| constant (e.g., `[]`, `1`) | Exact value |

Note: `_` has intrinsic mode produce (↑), `_?` has intrinsic mode consume (↓). Constants and `Integer`/`String` take their mode from context.

### Mode Propagation (Paper lines 9-17)

Modes propagate through type structure via complementation:
- Uncomplemented type `T` preserves parent mode
- Complemented type `T?` flips parent mode
- Complementation is an involution: `(T?)? = T`

## Public Interface

### Types

#### `class TypeDFA`
A DFA representing a compiled type definition.

```dart
class TypeDFA {
  final Set<DFAState> states;
  final DFAState startState;
  final Set<DFAState> finalStates;         // Constant-accepting states
  final Map<(DFAState, ModedLabel), DFAState> transitions;
  final Map<DFAState, Set<Mode>> primitiveStateModes;  // For _ and _? states
}
```

#### `class DFAState`
A state in the type DFA.

```dart
class DFAState {
  final String name;       // Type name or generated name
  final bool isFinal;      // True for constant-accepting states
}
```

#### `class ModedLabel`
A transition label with optional mode annotation.

```dart
class ModedLabel {
  final String symbol;     // e.g., "[|](2,1)", "[]", "ch(2,1)"
  final Mode? mode;        // Mode at this position (null for constants)
}
```

### Functions

#### `TypeDFA compileType(String typeName, TypeEnvironment env)`
Compiles a type definition to a DFA.

**Preconditions:** 
- `typeName` is defined in `env`
- Type definition is deterministic

**Postconditions:** Returns a DFA where:
- Start state corresponds to `typeName`
- Each reachable type has a corresponding state
- Primitive positions are marked in `primitiveStateModes`

**Errors:** Throws `NonDeterministicTypeError` if alternatives have conflicting symbols.

#### `TypeDFA applyModeComplement(TypeDFA dfa)`
Applies mode complementation to all transitions and primitive states.

**Postconditions:** Returns DFA with:
- All transition modes flipped (↑ ↔ ↓)
- All primitive state modes flipped

#### `bool isPrimitiveState(DFAState state, TypeDFA dfa)`
Returns true if state corresponds to a primitive type position.

#### `Set<Mode> getModesAt(DFAState state, TypeDFA dfa)`
Returns the mode set at a primitive state (empty for non-primitive).

#### DFA Operations

The following standard DFA operations are required:

```dart
TypeDFA intersect(TypeDFA a, TypeDFA b);      // L(A) ∩ L(B)
TypeDFA complement(TypeDFA dfa);               // L̄(A)
TypeDFA union(TypeDFA a, TypeDFA b);           // L(A) ∪ L(B)
bool isSubsetOf(TypeDFA a, TypeDFA b);         // L(A) ⊆ L(B)
bool isEmpty(TypeDFA dfa);                     // L(A) = ∅
TypeDFA complete(TypeDFA dfa, Set<ModedLabel> alphabet);  // Add sink state
```

These operations must correctly handle `primitiveStateModes`:
- **Intersection**: mode sets are intersected
- **Complement**: mode sets are complemented ({↑,↓} - modes)
- **Union**: mode sets are unioned

## Algorithms

### Algorithm: Direct DFA Compilation

Type definitions compile directly to DFA (no NFA intermediate):

```
compileType(typeName, env):
  states = {}
  transitions = {}
  finalStates = {}
  primitiveStateModes = {}
  compiled = {}
  
  startState = getOrCreateState(typeName, states)
  compileTypeDef(typeName, env, states, transitions, finalStates, 
                 primitiveStateModes, compiled)
  
  return TypeDFA(states, startState, finalStates, transitions, primitiveStateModes)

compileTypeDef(typeName, env, states, transitions, finalStates, primitiveStateModes, compiled):
  if typeName in compiled: return
  compiled.add(typeName)
  
  fromState = states[typeName]
  typeDef = env.getType(typeName)
  
  for alt in typeDef.alternatives:
    compileAlternative(fromState, alt, ...)

compileAlternative(fromState, alt, states, transitions, finalStates, primitiveStateModes, env, compiled):
  match alt:
    PrimitiveModeAlt(isInput):
      mode = isInput ? Mode.consume : Mode.produce
      primitiveStateModes[fromState] = 
        (primitiveStateModes[fromState] ?? {}).add(mode)
    
    ConstantAlt(value):
      label = ModedLabel(value.toString())
      finalState = DFAState("_final_$value", isFinal: true)
      finalStates.add(finalState)
      states.add(finalState)
      transitions[(fromState, label)] = finalState
    
    ListNilAlt:
      label = ModedLabel("[]")
      finalState = DFAState("_final_nil", isFinal: true)
      finalStates.add(finalState)
      states.add(finalState)
      transitions[(fromState, label)] = finalState
    
    ListConsAlt(head, tail):
      // Head transition
      headMode = modeOf(head)
      headLabel = ModedLabel("[|](2,1)", mode: headMode)
      headState = resolveTarget(head, states, env, compiled, ...)
      transitions[(fromState, headLabel)] = headState
      
      // Tail transition  
      tailMode = modeOf(tail)
      tailLabel = ModedLabel("[|](2,2)", mode: tailMode)
      tailState = resolveTarget(tail, states, env, compiled, ...)
      transitions[(fromState, tailLabel)] = tailState
    
    StructAlt(functor, arity, args):
      for i in 0..<args.length:
        argMode = modeOf(args[i])
        label = ModedLabel("$functor($arity,${i+1})", mode: argMode)
        targetState = resolveTarget(args[i], states, env, compiled, ...)
        transitions[(fromState, label)] = targetState

modeOf(typeExpr):
  match typeExpr:
    TypeRef(_, isInput): return isInput ? Mode.consume : Mode.produce
    PrimitiveModeAlt(isInput): return isInput ? Mode.consume : Mode.produce
    _: return null

resolveTarget(typeExpr, states, env, compiled, ...):
  match typeExpr:
    TypeRef(name, _):
      targetState = getOrCreateState(name, states)
      compileTypeDef(name, env, ...)  // Recursive compilation
      return targetState
    PrimitiveModeAlt(isInput):
      state = DFAState("_prim_${counter++}")
      states.add(state)
      mode = isInput ? Mode.consume : Mode.produce
      primitiveStateModes[state] = {mode}
      return state
    ConstantAlt(value):
      // Create intermediate state that accepts the constant
      state = DFAState("_const_$value")
      states.add(state)
      label = ModedLabel(value.toString())
      finalState = DFAState("_final_$value", isFinal: true)
      finalStates.add(finalState)
      states.add(finalState)
      transitions[(state, label)] = finalState
      return state
    ListNilAlt:
      // Similar to ConstantAlt for []
      ...
```

### Algorithm: Mode Complement Application

For `T?` arguments, apply mode complement to the compiled DFA:

```
applyModeComplement(dfa):
  newTransitions = {}
  for ((from, label), to) in dfa.transitions:
    flippedMode = label.mode?.flip  // null stays null
    newLabel = ModedLabel(label.symbol, mode: flippedMode)
    newTransitions[(from, newLabel)] = to
  
  newPrimitiveModes = {}
  for (state, modes) in dfa.primitiveStateModes:
    flippedModes = modes.map(m => m.flip).toSet()
    newPrimitiveModes[state] = flippedModes
  
  return TypeDFA(
    states: dfa.states,
    startState: dfa.startState,
    finalStates: dfa.finalStates,
    transitions: newTransitions,
    primitiveStateModes: newPrimitiveModes
  )
```

## Examples

### Example: Stream Type Compilation

```
Stream ::= [] ; [_|Stream].
```

Compiles to DFA:
```
States: {Stream, _prim_0, _final_nil}
Start: Stream
Final: {_final_nil}
Primitive: {_prim_0: {↑}}

Transitions:
  Stream --[[]]---------> _final_nil
  Stream --[[|](2,1), ↑]--> _prim_0
  Stream --[[|](2,2), ↑]--> Stream
```

### Example: Procedure Type for merge

```
procedure merge(Stream?, Stream?, Stream).
```

Argument DFAs:
- Args 1, 2: Compile `Stream`, then apply `applyModeComplement()`
- Arg 3: Compile `Stream` as-is

After complement for args 1, 2:
```
Transitions:
  Stream --[[]]---------> _final_nil
  Stream --[[|](2,1), ↓]--> _prim_0    // ↑ flipped to ↓
  Stream --[[|](2,2), ↓]--> Stream     // ↑ flipped to ↓

Primitive: {_prim_0: {↓}}              // ↑ flipped to ↓
```

### Example: Interactive Type (HollowStream)

```
HollowStream ::= [] ; [_?|HollowStream].
```

Compiles to:
```
States: {HollowStream, _prim_0, _final_nil}
Start: HollowStream
Final: {_final_nil}
Primitive: {_prim_0: {↓}}              // _? has consume mode

Transitions:
  HollowStream --[[]]---------> _final_nil
  HollowStream --[[|](2,1), ↓]--> _prim_0
  HollowStream --[[|](2,2), ↑]--> HollowStream  // No ? on recursive ref
```

## Error Conditions

| Condition | Exception |
|-----------|-----------|
| Type not defined | `UndefinedTypeError` |
| Non-deterministic alternatives (same symbol) | `NonDeterministicTypeError` |
| Redefinition of predefined type | `PredefinedTypeError` |

## Notes

### Why Not NFA?

The paper requires deterministic types (line 30-35). Union (`;`) in type definitions is NOT nondeterminism—alternatives have different leading symbols. This allows direct DFA construction, which is simpler and more efficient than NFA→DFA conversion.

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft — direct DFA compilation, no NFA |
| 0.2 | 2025-01-07 | Remove implementation notes, clarify primitive types |
