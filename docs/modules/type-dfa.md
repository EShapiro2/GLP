# Module: type-dfa

**Version**: 0.5  
**Date**: 2025-01-08  
**Status**: DRAFT  
**Paper References**: Section 4.1 (lines 7-35), lines 30-35 (DFA correspondence)

## Purpose

Compiles GLP type definitions to DFAs and provides traversal operations. Types are deterministic by construction—each state has at most one transition per symbol.

## Dependencies

- `mode` — Mode enum
- `type-environment` — TypeDef, TypeEnvironment

## Definitions

### DFA Correspondence (Paper line 30)

> "We impose standard restrictions on GLP type definitions so that they correspond to a DFA in which every state is a defined type, except for final states which are primitive types."

GLP type definitions compile **directly** to a DFA:
- Each defined type name is a DFA state
- Each alternative adds transitions from that state
- Leaf types mark accepting states

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

### No Type Aliasing (Epsilon Transitions)

Type definitions must have **concrete alternatives only**. Each alternative must be one of:
- A constant (`[]`, `0`, `foo`)
- A primitive mode (`_`, `_?`)
- A structure with arguments (`s(Nat)`, `[_|List]`, `ch(Stream?, Stream)`)

Type aliasing (referencing another type directly) is **illegal** because it creates epsilon transitions, converting the DFA to an NFA:

```
Stream ::= List.               % ILLEGAL: type alias creates epsilon transition
Combined ::= List1 ; List2.    % ILLEGAL: merging types creates NFA
Wrapper ::= inner(Other).      % LEGAL: Other appears inside a constructor
```

**Rationale:** Each DFA state must have explicit transitions labeled with symbols (constants or functor/arity/position). A bare type reference `A ::= B` would require an epsilon transition from state A to state B, violating the DFA structure.

**Error:** `TypeAliasError("A: type alias not allowed; use constructor wrapper")`

### Leaf Types

A **leaf type** terminates a type path. The DFA has leaf states for:

| Leaf Type | Description | Intrinsic Mode |
|-----------|-------------|----------------|
| `_` | Any produced term | produce (↑) |
| `_?` | Any consumed term | consume (↓) |
| `Integer` | Any integer constant | from context |
| `String` | Any string constant | from context |
| constant (e.g., `[]`, `1`) | Exact value | from context |

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
  final Map<String, DFAState> states;
  final DFAState startState;
  final Map<(DFAState, DFALabel), DFAState> transitions;
}
```

#### `class DFAState`

A state in the type DFA.

```dart
class DFAState {
  final String name;           // Type name or generated name
  final bool isLeaf;           // True for leaf type states
  final LeafType? leafType;    // If isLeaf, what kind
  final Mode? intrinsicMode;   // For _ and _?, their intrinsic mode
  final Object? constantValue; // For constant leaves, the value
}

enum LeafType {
  primitiveOutput,   // _
  primitiveInput,    // _?
  integer,           // Integer
  string,            // String  
  constant,          // Specific constant value ([], 1, foo, etc.)
}
```

#### `class DFALabel`

A transition label encoding functor, arity, argument position, and mode.

```dart
class DFALabel {
  final String symbol;     // e.g., "[|]", "[]", "ch", "foo"
  final int arity;         // Number of arguments (0 for constants)
  final int argIndex;      // 1-based argument position (0 for constants)
  final Mode? mode;        // Mode at this position (null for constants)
}
```

### Functions

#### `TypeDFA compileType(String typeName, TypeEnvironment env)`

Compiles a type definition to a DFA.

**Preconditions:** 
- `typeName` is defined in `env` or is a predefined type
- Type definition is deterministic (distinct leading symbols per alternative)

**Postconditions:** Returns a DFA where:
- Start state corresponds to `typeName`
- Each reachable type has a corresponding state
- Leaf types have appropriate leaf states

**Errors:** 
- Throws `UndefinedTypeError` if type not defined
- Throws `NonDeterministicTypeError` if alternatives have conflicting symbols

#### `TypeDFA complementDFA(TypeDFA dfa)`

Applies mode complementation to all transitions and leaf states.

**Postconditions:** Returns DFA with:
- All transition modes flipped (↑ ↔ ↓)
- Primitive leaf intrinsic modes flipped (`_` ↔ `_?`)

**Use case:** For procedure argument `T?`, compile `T` then call `complementDFA`.

#### `DFAState? stateAfterLabel(DFAState from, DFALabel label, TypeDFA dfa)`

Returns the target state for a transition, or null if no such transition exists.

**Preconditions:**
- `from` is a state in `dfa`

**Postconditions:** Returns the target state if transition exists, null otherwise.

#### `List<(DFALabel, DFAState)> getTransitions(DFAState state, TypeDFA dfa)`

Returns all outgoing transitions from a state.

**Preconditions:**
- `state` is a state in `dfa`

**Postconditions:** Returns list of (label, targetState) pairs. Empty list for leaf states.

#### `bool isLeafState(DFAState state)`

Returns true if state is a leaf type state.

#### `LeafType? getLeafType(DFAState state)`

Returns the leaf type kind if state is a leaf, null otherwise.

#### `Mode? getLeafMode(DFAState state)`

Returns the intrinsic mode of a primitive leaf state (`_` → produce, `_?` → consume), or null for non-primitive leaves (Integer, String, constants).

## Algorithms

### Algorithm: Direct DFA Compilation

```
compileType(typeName, env):
  states = {}
  transitions = {}
  compiled = {}
  
  startState = compileTypeRec(typeName, Mode.produce, env, states, transitions, compiled)
  
  return TypeDFA(states, startState, transitions)

compileTypeRec(typeName, contextMode, env, states, transitions, compiled):
  key = typeName
  if key in compiled:
    return states[key]
  
  // Handle predefined leaf types
  if typeName == "_":
    state = DFAState(
      name: "_", 
      isLeaf: true, 
      leafType: LeafType.primitiveOutput,
      intrinsicMode: Mode.produce
    )
    states[key] = state
    compiled.add(key)
    return state
  
  if typeName == "_?":
    state = DFAState(
      name: "_?", 
      isLeaf: true, 
      leafType: LeafType.primitiveInput,
      intrinsicMode: Mode.consume
    )
    states[key] = state
    compiled.add(key)
    return state
  
  if typeName == "Integer":
    state = DFAState(name: "Integer", isLeaf: true, leafType: LeafType.integer)
    states[key] = state
    compiled.add(key)
    return state
  
  if typeName == "String":
    state = DFAState(name: "String", isLeaf: true, leafType: LeafType.string)
    states[key] = state
    compiled.add(key)
    return state
  
  // Create state for user-defined type
  state = DFAState(name: typeName, isLeaf: false)
  states[key] = state
  compiled.add(key)
  
  typeDef = env.getType(typeName)
  if typeDef is null:
    throw UndefinedTypeError(typeName)
  
  for alt in typeDef.alternatives:
    compileAlternative(state, alt, contextMode, env, states, transitions, compiled)
  
  return state

compileAlternative(fromState, alt, contextMode, env, states, transitions, compiled):
  match alt:
    PrimitiveAlt(isInput):
      // _ or _? as an alternative
      // This makes fromState also act as a leaf
      fromState.isLeaf = true
      fromState.leafType = isInput ? LeafType.primitiveInput : LeafType.primitiveOutput
      fromState.intrinsicMode = isInput ? Mode.consume : Mode.produce
    
    ConstantAlt(value):
      label = DFALabel(symbol: value.toString(), arity: 0, argIndex: 0, mode: null)
      leafState = DFAState(
        name: "_const_$value",
        isLeaf: true,
        leafType: LeafType.constant,
        constantValue: value
      )
      states["_const_$value"] = leafState
      transitions[(fromState, label)] = leafState
    
    ListNilAlt:
      label = DFALabel(symbol: "[]", arity: 0, argIndex: 0, mode: null)
      leafState = DFAState(
        name: "_nil",
        isLeaf: true,
        leafType: LeafType.constant,
        constantValue: []
      )
      states["_nil"] = leafState
      transitions[(fromState, label)] = leafState
    
    ListConsAlt(headTypeExpr, tailTypeExpr):
      // Head transition
      headMode = modeOfTypeExpr(headTypeExpr, contextMode)
      headLabel = DFALabel(symbol: "[|]", arity: 2, argIndex: 1, mode: headMode)
      headTarget = resolveTypeExpr(headTypeExpr, headMode, env, states, transitions, compiled)
      transitions[(fromState, headLabel)] = headTarget
      
      // Tail transition
      tailMode = modeOfTypeExpr(tailTypeExpr, contextMode)
      tailLabel = DFALabel(symbol: "[|]", arity: 2, argIndex: 2, mode: tailMode)
      tailTarget = resolveTypeExpr(tailTypeExpr, tailMode, env, states, transitions, compiled)
      transitions[(fromState, tailLabel)] = tailTarget
    
    StructAlt(functor, argTypeExprs):
      arity = argTypeExprs.length
      for i in 1..arity:
        argTypeExpr = argTypeExprs[i-1]
        argMode = modeOfTypeExpr(argTypeExpr, contextMode)
        label = DFALabel(symbol: functor, arity: arity, argIndex: i, mode: argMode)
        target = resolveTypeExpr(argTypeExpr, argMode, env, states, transitions, compiled)
        transitions[(fromState, label)] = target

modeOfTypeExpr(typeExpr, parentMode):
  match typeExpr:
    TypeRef(_, isComplement):
      return isComplement ? parentMode.flip : parentMode
    PrimitiveAlt(isInput):
      return isInput ? Mode.consume : Mode.produce

resolveTypeExpr(typeExpr, contextMode, env, states, transitions, compiled):
  match typeExpr:
    TypeRef(name, isComplement):
      return compileTypeRec(name, contextMode, env, states, transitions, compiled)
    PrimitiveAlt(isInput):
      primName = isInput ? "_?" : "_"
      return compileTypeRec(primName, contextMode, env, states, transitions, compiled)
```

### Algorithm: DFA Mode Complementation

```
complementDFA(dfa):
  newStates = {}
  newTransitions = {}
  
  // Complement all states
  for (name, state) in dfa.states:
    if state.isLeaf and state.leafType in {primitiveOutput, primitiveInput}:
      // Flip primitive type
      newLeafType = state.leafType == primitiveOutput ? primitiveInput : primitiveOutput
      newMode = state.intrinsicMode.flip
      newName = newMode == Mode.consume ? "_?" : "_"
      newStates[name] = DFAState(
        name: newName,
        isLeaf: true,
        leafType: newLeafType,
        intrinsicMode: newMode
      )
    else:
      newStates[name] = state
  
  // Complement all transition modes
  for ((fromState, label), toState) in dfa.transitions:
    newLabel = DFALabel(
      symbol: label.symbol,
      arity: label.arity,
      argIndex: label.argIndex,
      mode: label.mode?.flip
    )
    newTransitions[(newStates[fromState.name], newLabel)] = newStates[toState.name]
  
  return TypeDFA(newStates, newStates[dfa.startState.name], newTransitions)
```

## Examples

### Example: Stream Type Compilation

```
Stream ::= [] ; [_|Stream].
```

Compiles to DFA:
```
States: 
  Stream (non-leaf)
  _nil (leaf: constant [])
  _ (leaf: primitiveOutput, intrinsicMode=produce)

Start: Stream

Transitions:
  Stream --[[], 0, 0, null]--> _nil
  Stream --[[|], 2, 1, ↑]--> _
  Stream --[[|], 2, 2, ↑]--> Stream
```

### Example: Stream? (Complemented)

After `complementDFA(StreamDFA)`:
```
States:
  Stream (non-leaf)
  _nil (leaf: constant [])
  _? (leaf: primitiveInput, intrinsicMode=consume)  // Flipped from _

Transitions:
  Stream --[[], 0, 0, null]--> _nil
  Stream --[[|], 2, 1, ↓]--> _?     // Mode flipped ↑→↓
  Stream --[[|], 2, 2, ↓]--> Stream // Mode flipped ↑→↓
```

### Example: HollowStream (Interactive Type)

```
HollowStream ::= [] ; [_?|HollowStream].
```

Compiles to:
```
States:
  HollowStream (non-leaf)
  _nil (leaf: constant [])
  _? (leaf: primitiveInput, intrinsicMode=consume)

Transitions:
  HollowStream --[[], 0, 0, null]--> _nil
  HollowStream --[[|], 2, 1, ↓]--> _?           // _? has consume mode
  HollowStream --[[|], 2, 2, ↑]--> HollowStream // No complement on recursive ref
```

### Example: Procedure Type for merge

```
procedure merge(Stream?, Stream?, Stream).
```

Argument DFAs:
- Args 1, 2: Compile `Stream`, then call `complementDFA()` 
- Arg 3: Compile `Stream` as-is (no complement)

### Example: INVALID — Non-deterministic Type

```
BadTree ::= leaf(Integer) ; leaf(String).   % ILLEGAL
```

**Problem:** Both alternatives start with `leaf/1`. The DFA would need two transitions from `BadTree` state with label `leaf(1,1)` — this violates determinism.

**Error:** `NonDeterministicTypeError("BadTree: multiple transitions for leaf/1")`

**Fix:** Use distinct constructors:
```
Tree ::= int_leaf(Integer) ; str_leaf(String).   % LEGAL
```

## Error Conditions

| Condition | Exception |
|-----------|-----------|
| Type not defined | `UndefinedTypeError` |
| Non-deterministic alternatives (same leading symbol) | `NonDeterministicTypeError` |

## Notes

### Why No DFA Set Operations?

Previous versions of this spec included union, intersection, complement (set), and subset operations. These are **not needed** because:

1. **Covariance checking** traverses the DFA alongside moded paths — no set operations needed
2. **Contravariance checking** uses structural coverage — traverse the type DFA and verify each transition is accepted by some clause head

The only DFA operation needed beyond compilation is `complementDFA` for handling `T?` procedure arguments.

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft |
| 0.5 | 2025-01-08 | Major simplification: removed DFA set operations (union, intersect, subset, isEmpty); clarified leaf types; improved algorithms |
