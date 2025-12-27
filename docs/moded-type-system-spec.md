# GLP Moded Type System Specification (v1.11)

**Updated:** 2025-12-27
**Changes:**
- Section 1.1: Changed to asymmetric well-typing (output containment + input coverage)
- Section 2.2: Removed ::< subtype syntax (only ::= exists now)
- Section 2.5.2: Replaced Every/Any with single Any type
- Section 6: Rewrote algorithm for asymmetric conditions
- Section 6.4-6.5: Coverage applies only to input positions

## 1. Overview

This document extends the GLP Type System Specification to include **moded types**. Moded types add input/output mode annotations to structural types, capturing the distinction between values produced by the program and values consumed from the environment.

### 1.1 Relationship to Existing Type System

| Aspect | Unmoded Types (Current) | Moded Types (Extension) |
|--------|------------------------|-------------------------|
| Semantics | Model-theoretic (success set) | Trace semantics (produced/consumed) |
| What it checks | Structural correctness | Structural + directional correctness |
| Abstraction | T_P^α (tuple-distributive closure) | T_M^{α,m} (moded tuple-distributive closure) |
| Well-Typing | T_P^α(S) = S | Asymmetric: output containment |
 | (fixpoint equality) | + input coverage |
| Correctness | Types ⊇ ⟦P⟧ | Outputs ⊆ S, Inputs ⊇ S |

**Asymmetric Well-Typing Conditions:**
- **Output Containment:** `T_M^{α,m}(S)|↓ ⊆ S|↓` — produced values must be within declared type
- **Input Coverage:** `S|↑ ⊆ T_M^{α,m}(S)|↑` — all declared input alternatives must be handled

Moded types **subsume** unmoded types: every unmoded type is equivalent to a moded type where all positions have the universal mode `Any = _ | _?`.

### 1.2 Key Concepts

**Mode Complementation**: The `?` operator is a type-level involution that inverts modes:
- `T` (output mode) → callee writes, caller reads
- `T?` (input mode) → callee reads, caller writes
- `(T?)? = T` (involution property)

**Mode Checking**: At leaf positions during term/type traversal:
- Writer variable `X` must occur at input position `_?` in type
- Reader variable `X?` must occur at output position `_` in type

**Call Boundary Complementation**: When checking body goals, modes are complemented:
- What caller produces (output), callee consumes (input)
- What caller consumes (input), callee produces (output)

---

## 2. Syntax

### 2.1 Primitive Mode Types

The primitive mode types are the atomic building blocks:

```
_    output mode only (program produces value)
_?   input mode only (program consumes value)
```

### 2.2 Moded Type Expressions

Type definitions remain as before (using `::=` syntax):
```
Nat ::= 0 ; s(Nat).
List ::= [] ; [_ | List].
```

Mode annotations appear in **procedure declarations**:
```
procedure merge(List?, List?, List).
```

This declares:
- Arguments 1, 2: input mode (`List?`) — caller provides readers
- Argument 3: output mode (`List`) — caller provides writer

### 2.3 Grammar Extension

```
proc_decl     ::= 'procedure' atom '(' moded_type_refs ')' '.'
moded_type_refs ::= moded_type_ref (',' moded_type_ref)*
moded_type_ref  ::= type_ref '?'?
```

The `?` suffix on a type reference indicates input mode.

### 2.4 Predefined Types

The following types and procedures are predefined by prepending their definitions to every module. A module cannot redefine a predefined type or procedure.

#### 2.4.1 Primitive Types

```prolog
Number.   % numeric values (built-in)
String.   % string values (built-in)
```

#### 2.4.2 Universal Type

```prolog
Any ::= _ ; _?.
```

**Self-Duality:**
Since `Any ::= _ ; _?` contains both primitive modes, it is *self-dual*:

```
(Any)? = (_ ; _?)? = _? ; _ = Any
```

Complementing `Any` yields itself. This self-duality makes `Any` the true universal type—any value at any mode.

**Coverage Implications:**
At **input** positions, the asymmetric well-typing condition requires coverage of both mode alternatives: clauses must collectively handle both the `_` case (writer) and the `_?` case (reader). At **output** positions, output containment is automatically satisfied since any produced value is within `Any`.

Mode annotations on `Any` positions are semantically irrelevant. Writing `Any` or `Any?` in a procedure declaration has the same meaning—both writer and reader variables are acceptable at such positions.

#### 2.4.3 Output and Input Types

```prolog
Output.                % all variable positions have mode _ (writers)
Input ::= Output?.     % all variable positions have mode _? (readers)
```

`Output` is the type of terms where all variable positions have mode `_` (writer variables). `Input` is defined as `Output?`, the type of terms where all variable positions have mode `_?` (reader variables).

Neither `Output` nor `Input` contains mode complementations. The difference is whether the term's variables are all writers (`Output`) or all readers (`Input`).

**Output Intersection Property:**

For any type T:
- If T has no mode complementations, then `Output ∩ T = T`
- If T has mode complementations, then `Output ∩ T ≠ T`

**Example:**
```glp
List ::= [] ; [_ | List].        % no mode complementations
DiffList ::= List \ List?.       % has mode complementation (List?)
```

`Output ∩ List = List` (List has no `?` in its definition).

`Output ∩ DiffList ≠ DiffList` (DiffList contains `List?`).

#### 2.4.4 Collections

```prolog
List ::= [Any | List] ; [].
Stream ::= [Any | Stream].
DiffList ::= List \ List?.     % difference list with hole
```

**List** uses `Any` for elements. At input positions, `Any` requires mode coverage; at output positions, no coverage required.

**Stream** is defined without the nil case `[]`. Procedures operating on streams need not handle empty streams—the type simply doesn't include that alternative.

**DiffList** represents a list with a hole at the end. The structure `List \ List?` pairs:
- `List` (output): the content produced so far
- `List?` (input): the hole where more content can be appended

#### 2.4.5 Channels

```prolog
Channel ::= ch(Stream?, Stream).
```

A channel pairs two streams with complementary modes:
- First stream (`Stream?`): input—messages received
- Second stream (`Stream`): output—messages sent

The `new_channel` operation creates two complementary endpoints by swapping the streams.

#### 2.4.6 Predefined Procedures

These unit clauses are predefined and can be used as defined guards:

```prolog
%% Difference List Operations
procedure dl_append(DiffList?, DiffList?, DiffList).
procedure dl_to_list(DiffList?, List).

dl_append(A\B?, B\C?, A?\C).
dl_to_list(L\[], L?).

%% Channel Operations
procedure new_channel(Channel, Channel).
procedure send(Any, Channel?, Channel).
procedure receive(Any, Channel?, Channel).

new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).
send(X, ch(In, [X?|Out?]), ch(In?, Out)).
receive(X?, ch([X|In], Out?), ch(In?, Out)).
```

**dl_append** concatenates two difference lists in O(1) time by unifying the first list's hole with the second list's content.

**dl_to_list** closes a difference list by unifying its hole with `[]`.

**new_channel** creates two complementary channel endpoints. What one side sends, the other receives.

**send** adds a message to the channel's output stream.

**receive** takes a message from the channel's input stream.

#### 2.4.7 Usage as Defined Guards

Since the predefined procedures are unit clauses, they can be used in guard position:

```prolog
% Append in guard position
process(DL1, DL2, Result) :- dl_append(DL1?, DL2?, Result) |
    continue(Result?).

% Receive in guard position (suspends until message available)
handler(Ch) :- receive(Msg, Ch?, Ch2) |
    process(Msg?),
    handler(Ch2?).
```

#### 2.4.8 Any List and Mode Coverage

Lists with `Any` at element positions require mode coverage at input positions:

```glp
List ::= [] ; [Any | List].

procedure copy(List?, List).

copy([], []).
copy([H? | In], [H | Out?]) :- copy(In?, Out).
copy([H | In], [H? | Out?]) :- copy(In?, Out).
```

The two non-base clauses collectively cover both mode combinations at the `Any` position:
- First clause: reader at input head (`_?`), writer at output head (`_`)
- Second clause: writer at input head (`_`), reader at output head (`_?`)

This is required because at **input** positions, `Any ::= _ ; _?` demands coverage of both alternatives.

For **output** positions, a single clause suffices:

```glp
procedure generate(List).
generate([X | Xs?]) :- generate(Xs).
generate([]).
```

Output containment (`T_M^{α,m}(S)|↓ ⊆ S|↓`) only requires that produced values be within `Any`—no coverage obligation.

### 2.5 Embedded Modes in Type Definitions

Types can embed mode information for complex data structures:
```
% CounterMsg: show constructor has embedded input for response slot
CounterMsg ::= clear ; up ; down ; show(Number?).

% DiffList: head is output (content), tail is input (hole)
DiffList ::= List \ List?.

% Request: get has response slot (input), put has value slot (output)
Request ::= get(Value?) ; put(Value).

% QueueMsg: dequeue has response slot
QueueMsg ::= enqueue(Any) ; dequeue(Any?).
```

In `show(Number?)`, the `Number?` marks an **input position in the type definition**. When the counter receives `CounterMsg?` (input stream), involution applies: `show(Number?)` → `show(Number)` — so the counter WRITES the response.

### 2.6 Examples

```glp
% Simple moded procedure
Nat ::= 0 ; s(Nat).
procedure add(Nat?, Nat?, Nat).

add(0, Y, Y?).
add(s(X), Y, s(Z)?) :- add(X?, Y?, Z).

% Stream merge with all modes explicit
List ::= [] ; [_ | List].
procedure merge(List?, List?, List).

merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
merge([], [], []).

% Counter with embedded response mode
CounterMsg ::= clear ; up ; down ; show(Number?).
CounterStream ::= [] ; [CounterMsg | CounterStream].
procedure counter(CounterStream?, Number).

counter([clear|S], _) :- counter(S?, 0).
counter([up|S], State) :-
    NewState := State? + 1,
    counter(S?, NewState?).
counter([down|S], State) :-
    NewState := State? - 1,
    counter(S?, NewState?).
counter([show(State?)|S], State) :-
    number(State?) |
    counter(S?, State?).
counter([], _).
```

---

## 3. AST Representation (Dart)

### 3.1 Extended Type AST

```dart
/// A reference to a type, optionally with input mode
class TypeRef extends TypeExpr {
  final String name;        // "Nat", "List", "Number", etc.
  final bool isInput;       // true if Type?, false if Type

  TypeRef(this.name, {this.isInput = false});

  /// Mode complementation operator (·)?
  TypeRef complement() => TypeRef(name, isInput: !isInput);

  @override
  String toString() => isInput ? '$name?' : name;
}

/// Procedure declaration with moded argument types
class ProcDecl {
  final String name;
  final int arity;
  final List<TypeRef> argTypes;  // Each TypeRef carries its mode

  ProcDecl(this.name, this.argTypes) : arity = argTypes.length;

  String get signature => '$name/$arity';

  /// Get the mode for argument at index i
  bool isInputArg(int i) => argTypes[i].isInput;

  /// Get complemented view (callee's perspective)
  List<TypeRef> get calleeView =>
      argTypes.map((t) => t.complement()).toList();
}
```

### 3.2 Mode Enum (for internal representation)

```dart
enum Mode {
  output,  // Program produces (writer position)
  input,   // Environment provides (reader position)
}

extension ModeOps on Mode {
  Mode get complement => this == Mode.output ? Mode.input : Mode.output;
  bool get isOutput => this == Mode.output;
  bool get isInput => this == Mode.input;
}
```

### 3.3 Moded Path Representation

A **moded path** is a sequence of moded labels where each position carries mode information. Mode is tracked via two mechanisms (see Section 5):
- `ModedLabel.mode` for TypeRef positions (e.g., `T?` in `ch(T?, T)`)
- `primitiveStateModes` for primitive positions (e.g., `_` or `_?`)

```dart
/// A moded path: sequence of moded labels with mode at every position.
class ModedPath {
  final List<ModedLabel> steps;  // Each ModedLabel may carry mode

  ModedPath(this.steps);

  @override
  String toString() => steps.map((s) => s.toString()).join('·');
}

/// Extract moded paths from a term given its expected type
Set<ModedPath> extractModedPaths(Term term, TypeRef expectedType, TypeEnvironment env);
```

---

## 4. Mode Complementation Semantics

### 4.1 The `(·)?` Operator

Mode complementation is defined recursively on type expressions:

```
(c)?                          = c           (constants are modeless)
(T)?                          = T?          (apply mode to type ref)
(T?)?                         = T           (involution)
(f(τ₁, ..., τₙ))?             = f((τ₁)?, ..., (τₙ)?)
((τ₁ ; ... ; τₙ))?            = (τ₁)? ; ... ; (τₙ)?
```

**Primitive Complementation:**
```
(_)?  = _?          (output primitive → input primitive)
(_?)? = _           (input primitive → output primitive)
```

These follow the involution property: `((_)?)? = (_?)? = _`.

### 4.2 Mode Combination at Nested Positions

When traversing into a type structure, modes combine:

```dart
/// Combine parent mode with embedded mode
Mode combineMode(Mode parentMode, Mode embeddedMode) {
  // If parent is input, invert embedded mode
  if (parentMode == Mode.input) {
    return embeddedMode.complement;
  }
  return embeddedMode;
}
```

**Example**: For `DiffList ::= dl(List?, List)` with procedure `foo(DiffList?)`:
- The `DiffList?` argument is input mode
- Inside `dl(...)`:
  - First `List?`: input (parent) + input (embedded) = output
  - Second `List`: input (parent) + output (embedded) = input

### 4.3 Call Boundary Complementation

When checking a body goal against a procedure declaration:

```dart
/// Get the type expectation for a body goal
List<TypeRef> getBodyGoalTypes(ProcDecl decl) {
  // Complement all types: caller's output = callee's input
  return decl.argTypes.map((t) => t.complement()).toList();
}
```

### 4.4 Mode Coverage for Any Type (Asymmetric)

With asymmetric well-typing, mode coverage applies **only to input positions**. The `Any ::= _ ; _?` type contains both mode alternatives.

**Asymmetric Mode Coverage:**
- At **input** positions (`Type?`): clauses must cover both `_` and `_?` alternatives
- At **output** positions (`Type`): no coverage required—output containment only checks subset

#### Example: List Copy with Any Elements

```glp
List ::= [] ; [Any | List].

procedure copy(List?, List).

copy([], []).
copy([H? | In], [H | Out?]) :- copy(In?, Out).
copy([H | In], [H? | Out?]) :- copy(In?, Out).
```

The first argument `List?` is an **input position**. At the `Any` head position:
- First non-base clause: `H?` (reader) covers `_?`
- Second non-base clause: `H` (writer) covers `_`

Both mode alternatives are covered. **Well-moded-typed.**

#### Output Positions Need No Mode Coverage

For output-only procedures, a single clause suffices:

```glp
procedure generate(List).
generate([]).
generate([X | Xs?]) :- generate(Xs).
```

The argument is an **output position**. Output containment only requires that produced values be within `List`—no obligation to produce both modes.

#### Restricting to Single Mode

If coverage overhead is undesirable, restrict element type to single mode:

```glp
List1 ::= [] ; [_ | List1].    % output-mode elements only

procedure copy(List1?, List1).
copy([], []).
copy([H? | In], [H | Out?]) :- copy(In?, Out).
```

With `_` instead of `Any`, input position only needs to cover output mode. **Well-moded-typed.**

#### Design Principle

The asymmetric well-typing condition reduces coverage burden:
- Use `Any` when both modes genuinely need handling at input positions
- Use `_` or `_?` to restrict to single mode when appropriate
- Output positions never require mode coverage

---

## 5. Moded Type Automata: NFA Construction and DFA Conversion

This section describes how moded types are compiled to finite automata following the Yardeni-Shapiro approach. The compilation has two stages:

1. **NFA Construction** (§5.1-5.3): Type definitions with union (`T ::= A ; B`) are compiled to NFAs with ε-transitions representing nondeterministic choice
2. **DFA Conversion** (§5.4): The NFA is converted to a DFA using subset construction

### 5.1 Moded Transition Labels

A **moded label** is a transition label in the type automaton that encodes both structural information (functor name, argument position) and optional mode annotation.

```dart
/// Transition label in moded type automata
class ModedLabel {
  final String symbol;   // Structural: "f(n,i)" for functor f/n position i
  final Mode? mode;      // Optional mode annotation

  ModedLabel(this.symbol, {this.mode});

  factory ModedLabel.functor(String name, int arity, int argIndex, {Mode? mode}) {
    return ModedLabel('$name($arity,$argIndex)', mode: mode);
  }

  factory ModedLabel.listHead({Mode? mode}) => ModedLabel('[|](2,1)', mode: mode);
  factory ModedLabel.listTail({Mode? mode}) => ModedLabel('[|](2,2)', mode: mode);

  @override
  bool operator ==(Object other) =>
      other is ModedLabel && symbol == other.symbol && mode == other.mode;

  @override
  int get hashCode => Object.hash(symbol, mode);

  @override
  String toString() => mode != null ? '$symbol${mode == Mode.input ? "?" : ""}' : symbol;
}
```

**Key property:** Labels with different modes are distinct:
- `ModedLabel('f(2,1)', mode: Mode.input)` ≠ `ModedLabel('f(2,1)', mode: Mode.output)`
- `ModedLabel('f(2,1)')` (no mode) ≠ `ModedLabel('f(2,1)', mode: Mode.output)`

**Epsilon transition:** A special label `ModedLabel.epsilon()` represents ε-transitions in the NFA.

### 5.2 Moded Type NFA

A **moded type NFA** is a nondeterministic finite automaton with moded labels on transitions and mode sets on states.

```dart
class TypeNFA {
  final Set<NFAState> states;
  final NFAState startState;
  final Set<NFAState> finalStates;

  /// Transition relation: state × (label ∪ {ε}) → Set<NFAState>
  /// Maps (from-state, label) to set of possible next states
  final Map<(NFAState, ModedLabel), Set<NFAState>> transitions;

  /// Mode information at primitive type states
  /// Maps NFAState to set of accepted modes {output, input}
  /// Only primitive states (_, _?) appear in this map
  final Map<NFAState, Set<Mode>> primitiveStateModes;

  TypeNFA({
    required this.states,
    required this.startState,
    required this.finalStates,
    required this.transitions,
    required this.primitiveStateModes,
  });

  /// Check if state is primitive (appears in primitiveStateModes)
  bool isPrimitiveState(NFAState state) =>
      primitiveStateModes.containsKey(state);

  /// Get accepted modes at primitive state (empty if non-primitive)
  Set<Mode> getModesAt(NFAState state) =>
      primitiveStateModes[state] ?? {};

  /// Add ε-transition from -> to
  void addEpsilonTransition(NFAState from, NFAState to) {
    final epsilonLabel = ModedLabel.epsilon();
    transitions.putIfAbsent((from, epsilonLabel), () => {}).add(to);
  }

  /// Add labeled transition from --[label]--> to
  void addTransition(NFAState from, ModedLabel label, NFAState to) {
    transitions.putIfAbsent((from, label), () => {}).add(to);
  }
}
```

### 5.3 NFA Construction from Type Definitions

Type definitions are compiled to NFAs using the following rules:

#### 5.3.1 Union Types (Disjunction)

For a type definition `T ::= A ; B`, create an NFA that accepts paths accepted by either A or B:

```
    ε         ε
q₀ ---> A ---> qf
  \            /
   ε          ε
    ---> B --->
```

```dart
TypeNFA compileUnion(TypeExpr typeA, TypeExpr typeB) {
  final nfaA = compileTypeExpr(typeA);
  final nfaB = compileTypeExpr(typeB);

  final startState = NFAState.fresh();
  final finalState = NFAState.fresh();

  final nfa = TypeNFA(
    states: {startState, finalState, ...nfaA.states, ...nfaB.states},
    startState: startState,
    finalStates: {finalState},
    transitions: {},
    primitiveStateModes: {...nfaA.primitiveStateModes, ...nfaB.primitiveStateModes},
  );

  // ε-transitions to alternatives
  nfa.addEpsilonTransition(startState, nfaA.startState);
  nfa.addEpsilonTransition(startState, nfaB.startState);

  // ε-transitions from alternative finals to combined final
  for (final finalA in nfaA.finalStates) {
    nfa.addEpsilonTransition(finalA, finalState);
  }
  for (final finalB in nfaB.finalStates) {
    nfa.addEpsilonTransition(finalB, finalState);
  }

  // Merge transitions from both NFAs
  nfa.transitions.addAll(nfaA.transitions);
  nfa.transitions.addAll(nfaB.transitions);

  return nfa;
}
```

#### 5.3.2 Primitive Types

For primitive types `_` and `_?`, create a single-state NFA marked with the appropriate mode:

```dart
TypeNFA compilePrimitive(bool isInput) {
  final state = NFAState.fresh();
  final mode = isInput ? Mode.input : Mode.output;

  return TypeNFA(
    states: {state},
    startState: state,
    finalStates: {state},
    transitions: {},
    primitiveStateModes: {state: {mode}},
  );
}
```

For `Any ::= _ ; _?`, the union construction creates:

```
    ε         ε
q₀ ---> q_out ---> qf
  \            /
   ε          ε
    ---> q_in -->
```

where `primitiveStateModes[q_out] = {Mode.output}` and `primitiveStateModes[q_in] = {Mode.input}`.

After ε-removal and minimization (§5.4), this becomes a single state with `primitiveStateModes[q] = {Mode.output, Mode.input}`.

#### 5.3.3 Structure Types

For a structure `f(T₁, T₂, ..., Tₙ)`, compile each argument type and connect with labeled transitions:

```dart
TypeNFA compileStruct(String functor, int arity, List<TypeExpr> args) {
  final startState = NFAState.fresh();
  final finalState = NFAState.fresh();
  final nfa = TypeNFA(
    states: {startState, finalState},
    startState: startState,
    finalStates: {finalState},
    transitions: {},
    primitiveStateModes: {},
  );

  for (int i = 0; i < args.length; i++) {
    final argType = args[i];
    final argIndex = i + 1;
    final argNFA = compileTypeExpr(argType);

    // Determine mode for this position
    final Mode? labelMode;
    if (argType is TypeRef) {
      // Moded TypeRef: encode mode in label
      labelMode = argType.isInput ? Mode.input : Mode.output;
    } else {
      // PrimitiveModeAlt: mode in primitiveStateModes, not label
      labelMode = null;
    }

    final label = ModedLabel.functor(functor, arity, argIndex, mode: labelMode);

    // Add transition: startState --[label]--> argNFA.start
    nfa.addTransition(startState, label, argNFA.startState);

    // Add ε-transitions: argNFA.finals --[ε]--> finalState
    for (final argFinal in argNFA.finalStates) {
      nfa.addEpsilonTransition(argFinal, finalState);
    }

    // Merge argument NFA
    nfa.states.addAll(argNFA.states);
    nfa.transitions.addAll(argNFA.transitions);
    nfa.primitiveStateModes.addAll(argNFA.primitiveStateModes);
  }

  return nfa;
}
```

#### 5.3.4 Type References

For a type reference `T` or `T?`, recursively compile the referenced type and apply mode transformation if needed:

```dart
TypeNFA compileTypeRef(String typeName, bool isInput) {
  final typeDef = typeEnvironment.getTypeDef(typeName);
  final nfa = compileTypeDef(typeDef);

  if (isInput) {
    // Apply mode complementation: swap all modes in primitiveStateModes
    return nfa.withComplementedModes();
  }

  return nfa;
}
```

**Mode complementation** for `T?`:
```dart
TypeNFA withComplementedModes() {
  final newPrimitiveModes = <NFAState, Set<Mode>>{};

  for (final entry in primitiveStateModes.entries) {
    final state = entry.key;
    final modes = entry.value;
    final complemented = modes.map((m) =>
      m == Mode.output ? Mode.input : Mode.output
    ).toSet();
    newPrimitiveModes[state] = complemented;
  }

  return TypeNFA(
    states: states,
    startState: startState,
    finalStates: finalStates,
    transitions: transitions,
    primitiveStateModes: newPrimitiveModes,
  );
}
```

#### 5.3.5 Example: Channel Type

For `MyChannel ::= ch(MyStream?, MyStream) ; ch(MyStream, MyStream?)`:

```
NFA before ε-removal:

              ch(2,1)?          ε
    ε    ┌──────────────> S₁ ──────┐
q₀ ────> q₁                         ├──> qf
    ε    │   ch(2,2)         ε     │
    ────> q₂ ──────────> S₂ ──────┘
         │   ch(2,1)         ε
         └──────────────> S₃ ──────┤
             ch(2,2)?         ε     │
         └──────────────> S₄ ──────┘

where S₁, S₂, S₃, S₄ are the MyStream start states for each argument
```

After ε-removal and determinization, this becomes a DFA with four distinct transitions from the start state.

### 5.4 Clause Contribution via NFA Construction

Following Yardeni-Shapiro and the paper (Section 7.4), clause contributions are computed via NFA construction and determinization—the **same pipeline** used for type definitions.

> "Each type definition alternative contributes states and transitions to a nondeterministic finite automaton (NFA). Union types introduce nondeterminism... **Each clause of a procedure similarly contributes to the NFA** representing the inferred type." — Paper, Section 7.4

#### 5.4.1 Principle

A clause head pattern is semantically a type expression where:
- Ground positions (constants) contribute constant alternatives
- Variable positions contribute the variable's inferred type

The NFA for a clause contribution is built using the same `TypeNFACompiler` infrastructure as type definitions, but with pattern terms instead of type AST.

#### 5.4.2 Pattern-to-Type Correspondence

Each pattern construct corresponds to a type construct:

| Pattern | Equivalent Type Expression |
|---------|---------------------------|
| Constant `c` | `ConstantAlt(c)` |
| Variable `X` (writer) | `varTypes[X]` with mode output |
| Variable `X?` (reader) | `varTypes[X]` with mode input |
| Structure `f(t₁,...,tₙ)` | `StructAlt(f, [T₁,...,Tₙ])` where Tᵢ = patternToType(tᵢ) |
| List `[H\|T]` | `ListConsAlt(patternToType(H), patternToType(T))` |
| List `[]` | `ListNilAlt` |

#### 5.4.3 NFA Construction Algorithm

```
ClauseContributionNFA(pattern, varTypes, declaredArgMode):
  // Convert pattern to type expression
  typeExpr = patternToTypeExpr(pattern, varTypes, declaredArgMode)

  // Use standard NFA compiler
  nfaCompiler = TypeNFACompiler(typeEnv)
  nfa = nfaCompiler.compileExpr(typeExpr)

  return nfa

patternToTypeExpr(term, varTypes, contextMode):

  if term is VarTerm:
    // Variable: reference to inferred type with variable's mode
    varTypeName = varTypes[term.name].typeName
    varMode = term.isReader ? Mode.input : Mode.output

    // Create TypeRef with appropriate mode annotation
    // The mode is encoded in the TypeRef's isInput flag
    return TypeRef(varTypeName, isInput: varMode == Mode.input)

  elif term is ConstTerm:
    return ConstantAlt(term.value)

  elif term is StructTerm:
    argExprs = []
    for i in 0..term.arity:
      argExpr = patternToTypeExpr(term.args[i], varTypes, contextMode)
      argExprs.add(argExpr)
    return StructAlt(term.functor, argExprs)

  elif term is ListTerm:
    if term.isNil:
      return ListNilAlt()
    else:
      headExpr = patternToTypeExpr(term.head, varTypes, contextMode)
      tailExpr = patternToTypeExpr(term.tail, varTypes, contextMode)
      return ListConsAlt(headExpr, tailExpr)

  elif term is UnderscoreTerm:
    // Anonymous variable: accepts any value at any mode
    return TypeRef("Any")
```

#### 5.4.4 Variable Type as TypeRef

When a pattern contains variable `X` with inferred type `T`:
- Writer `X` → `TypeRef(T, isInput: false)` (output mode)
- Reader `X?` → `TypeRef(T, isInput: true)` (input mode)

The NFA compiler creates a transition with the mode encoded in `ModedLabel`:

```
For pattern [X | Xs] where type(X) = Any, type(Xs) = List:

patternToTypeExpr produces:
  ListConsAlt(
    TypeRef("Any", isInput: false),   // X is writer → output
    TypeRef("List", isInput: false)   // Xs is writer → output
  )

NFA compiler produces transitions:
  start --[[|](2,1), output]--> Any_state
  start --[[|](2,2), output]--> List_state
```

#### 5.4.5 Correspondence with Paper

| Paper (Section 7.4) | Spec |
|---------------------|------|
| "Each clause similarly contributes to the NFA" | `patternToTypeExpr` + `TypeNFACompiler` |
| "NFA converted to DFA via subset construction" | Section 5.5 `NFAToDFAConverter.convert()` |
| "Union of clause contributions" | `TypeDFA.union()` (Section 5.8) |
| "Fixpoint check T_M^{α,m}(S) = S" | Section 6.1 with subset check |

**Key insight**: By converting patterns to type expressions, we reuse the existing NFA→DFA pipeline rather than building a parallel infrastructure. This ensures:
1. **Uniform representation**: Both type definitions and clause contributions use same NFA→DFA
2. **Correct mode encoding**: ModedLabel modes set from variable modes via TypeRef.isInput
3. **Primitive mode preservation**: Subset construction correctly merges bi-moded states

### 5.5 NFA to DFA Conversion (Subset Construction)

The NFA is converted to a DFA using standard subset construction with mode set tracking.

#### 5.5.1 ε-Closure

Compute the ε-closure of a set of NFA states:

```dart
Set<NFAState> epsilonClosure(Set<NFAState> states, TypeNFA nfa) {
  final closure = <NFAState>{...states};
  final worklist = [...states];

  while (worklist.isNotEmpty) {
    final state = worklist.removeLast();
    final epsilonLabel = ModedLabel.epsilon();
    final epsilonTargets = nfa.transitions[(state, epsilonLabel)] ?? {};

    for (final target in epsilonTargets) {
      if (!closure.contains(target)) {
        closure.add(target);
        worklist.add(target);
      }
    }
  }

  return closure;
}
```

#### 5.5.2 Subset Construction Algorithm

```dart
TypeDFA nfaToDfa(TypeNFA nfa) {
  final dfaStateMap = <Set<NFAState>, DFAState>{};
  final dfaTransitions = <(DFAState, ModedLabel), DFAState>{};
  final dfaFinalStates = <DFAState>{};
  final dfaPrimitiveModes = <DFAState, Set<Mode>>{};

  int stateCounter = 0;
  DFAState getDFAState(Set<NFAState> nfaStates) {
    return dfaStateMap.putIfAbsent(nfaStates, () {
      final dfaState = DFAState('q${stateCounter++}');

      // Check if any NFA state is final
      final hasFinal = nfaStates.any((s) => nfa.finalStates.contains(s));

      // Compute mode set for this DFA state
      final modes = <Mode>{};
      var hasPrimitive = false;

      for (final nfaState in nfaStates) {
        if (nfa.isPrimitiveState(nfaState)) {
          hasPrimitive = true;
          modes.addAll(nfa.getModesAt(nfaState));
        }
      }

      if (hasPrimitive) {
        // Primitive DFA state: mark with union of modes
        dfaPrimitiveModes[dfaState] = modes;
      } else if (hasFinal) {
        // Structural accepting state
        dfaFinalStates.add(dfaState);
      }

      return dfaState;
    });
  }

  // Start state: ε-closure of NFA start
  final startNFAStates = epsilonClosure({nfa.startState}, nfa);
  final startDFAState = getDFAState(startNFAStates);

  final worklist = [startNFAStates];
  final visited = <Set<NFAState>>{};

  while (worklist.isNotEmpty) {
    final currentNFAStates = worklist.removeLast();
    if (visited.contains(currentNFAStates)) continue;
    visited.add(currentNFAStates);

    final currentDFAState = getDFAState(currentNFAStates);

    // Collect all non-ε transitions from current NFA state set
    final labelMap = <ModedLabel, Set<NFAState>>{};

    for (final nfaState in currentNFAStates) {
      for (final entry in nfa.transitions.entries) {
        final (from, label) = entry.key;
        if (from != nfaState) continue;
        if (label.symbol == 'ε') continue;  // Skip ε-transitions

        final targets = entry.value;
        labelMap.putIfAbsent(label, () => {}).addAll(targets);
      }
    }

    // Create DFA transitions
    for (final entry in labelMap.entries) {
      final label = entry.key;
      final targetNFAStates = epsilonClosure(entry.value, nfa);
      final targetDFAState = getDFAState(targetNFAStates);

      dfaTransitions[(currentDFAState, label)] = targetDFAState;

      if (!visited.contains(targetNFAStates)) {
        worklist.add(targetNFAStates);
      }
    }
  }

  return TypeDFA(
    states: dfaStateMap.values.toSet(),
    startState: startDFAState,
    finalStates: dfaFinalStates,
    transitions: dfaTransitions,
    primitiveStateModes: dfaPrimitiveModes,
  );
}
```

#### 5.5.3 Mode Set Union in Subset Construction

**Key property:** When multiple NFA states with different primitive modes are merged into a single DFA state, the DFA state accepts the **union** of their mode sets.

Example: `Any ::= _ ; _?`

- NFA has two states: `q_out` with `{Mode.output}` and `q_in` with `{Mode.input}`
- After ε-removal, both are in the ε-closure of the start state
- DFA state `q₀ = {q_out, q_in}` gets `primitiveStateModes[q₀] = {Mode.output, Mode.input}`

This is how bi-moded types arise naturally from subset construction.

### 5.6 DFA Representation

The final DFA representation is the same as the NFA structure, but with deterministic transitions:

```dart
class TypeDFA {
  final Set<DFAState> states;
  final DFAState startState;
  final Set<DFAState> finalStates;

  /// Deterministic transitions: state × label → state
  final Map<(DFAState, ModedLabel), DFAState> transitions;

  /// Mode information at primitive states
  final Map<DFAState, Set<Mode>> primitiveStateModes;

  TypeDFA({
    required this.states,
    required this.startState,
    required this.finalStates,
    required this.transitions,
    required this.primitiveStateModes,
  });

  bool isPrimitiveState(DFAState state) =>
      primitiveStateModes.containsKey(state);

  Set<Mode> getModesAt(DFAState state) =>
      primitiveStateModes[state] ?? {};
}
```

**Invariant:** For all `(state, label)`, there is at most one entry in `transitions`. This is the determinism property.

### 5.7 Accepting Moded Paths

A moded type DFA accepts a moded path ξ = π₁·π₂···πₙ where each πᵢ is a moded label, iff:

1. There exist states q₀, q₁, ..., qₙ such that:
   - q₀ is the start state
   - For each i ∈ [1..n]: transitions[(qᵢ₋₁, πᵢ)] = qᵢ
2. Either:
   - (a) qₙ ∈ finalStates and primitiveStateModes[qₙ] = ∅ (**structural acceptance**)
   - (b) qₙ ∈ primitiveStateModes.keys and the mode m ∈ primitiveStateModes[qₙ] (**primitive acceptance**)

```dart
bool acceptsModedPath(List<ModedLabel> path, Mode leafMode) {
  var current = startState;

  for (final label in path) {
    final next = transitions[(current, label)];
    if (next == null) return false;
    current = next;
  }

  // Check acceptance
  if (isPrimitiveState(current)) {
    // Primitive acceptance: mode must be in mode set
    return getModesAt(current).contains(leafMode);
  } else {
    // Structural acceptance: must be final state
    return finalStates.contains(current);
  }
}
```

### 5.8 Operations on Moded Type DFA

This section specifies DFA operations required for type checking: containment, intersection, complement, and emptiness.

#### 5.8.1 Moded Type Containment

**Definition:** For moded type DFAs A and B:
```
A ⊆ᵐ B  iff  Lᵐ(A) ⊆ Lᵐ(B)
```

**Proposition (Bi-Moded Start State):** If B has a bi-moded start state (primitiveStateModes[q₀] = {output, input}), then A ⊆ᵐ B for any A.

**Algorithm:** A ⊆ᵐ B iff Lᵐ(A ∩ᵐ B̄) = ∅

```dart
bool isSubsetOf(TypeDFA other) {
  // Optimization: bi-moded start accepts everything
  if (other.isPrimitiveState(other.startState) &&
      other.getModesAt(other.startState).length == 2) {
    return true;
  }

  final otherComplement = other.modedComplement();
  final intersection = this.intersect(otherComplement);
  return intersection.isModedEmpty;
}
```

#### 5.8.2 Moded Type Intersection

Product construction with mode set intersection:

```dart
TypeDFA intersect(TypeDFA other) {
  final productStates = <(DFAState, DFAState), DFAState>{};
  final newTransitions = <(DFAState, ModedLabel), DFAState>{};
  final newFinalStates = <DFAState>{};
  final newPrimitiveModes = <DFAState, Set<Mode>>{};

  int counter = 0;
  DFAState getProduct(DFAState qA, DFAState qB) {
    return productStates.putIfAbsent((qA, qB), () {
      final product = DFAState('p${counter++}');

      // Compute mode set for product
      final isPrimA = isPrimitiveState(qA);
      final isPrimB = other.isPrimitiveState(qB);

      if (isPrimA && isPrimB) {
        // Both primitive: intersect modes
        final modesA = getModesAt(qA);
        final modesB = other.getModesAt(qB);
        final intersection = modesA.intersection(modesB);
        // Always add, even if empty, to preserve primitive status
        newPrimitiveModes[product] = intersection;
      } else if (isPrimA && !isPrimB) {
        // Only A primitive: keep A's modes
        newPrimitiveModes[product] = getModesAt(qA);
      } else if (!isPrimA && isPrimB) {
        // Only B primitive: keep B's modes
        newPrimitiveModes[product] = other.getModesAt(qB);
      } else {
        // Both structural: check if both final
        if (finalStates.contains(qA) && other.finalStates.contains(qB)) {
          newFinalStates.add(product);
        }
      }

      return product;
    });
  }

  final start = getProduct(startState, other.startState);
  final worklist = [(startState, other.startState)];
  final visited = <(DFAState, DFAState)>{};

  while (worklist.isNotEmpty) {
    final (qA, qB) = worklist.removeLast();
    if (visited.contains((qA, qB))) continue;
    visited.add((qA, qB));

    final product = getProduct(qA, qB);

    // Find common transitions
    for (final entryA in transitions.entries) {
      final (fromA, label) = entryA.key;
      if (fromA != qA) continue;

      final toA = entryA.value;
      final toB = other.transitions[(qB, label)];
      if (toB == null) continue;

      final toProduct = getProduct(toA, toB);
      newTransitions[(product, label)] = toProduct;

      if (!visited.contains((toA, toB))) {
        worklist.add((toA, toB));
      }
    }
  }

  return TypeDFA(
    states: productStates.values.toSet(),
    startState: start,
    finalStates: newFinalStates,
    transitions: newTransitions,
    primitiveStateModes: newPrimitiveModes,
  );
}
```

#### 5.8.3 Moded Type Complement

For a complete DFA A, the complement Āᵐ swaps final/non-final states and complements primitive mode sets:

```dart
TypeDFA modedComplement() {
  final completed = complete();  // Add sink state if needed

  final newFinalStates = completed.states.difference(completed.finalStates);
  final newPrimitiveModes = <DFAState, Set<Mode>>{};

  for (final state in completed.states) {
    final modes = completed.primitiveStateModes[state];
    if (modes != null && modes.isNotEmpty) {
      final complement = {Mode.output, Mode.input}.difference(modes);
      // Always add to preserve primitive status, even if complement is empty
      newPrimitiveModes[state] = complement;
    }
  }

  return TypeDFA(
    states: completed.states,
    startState: completed.startState,
    finalStates: newFinalStates,
    transitions: completed.transitions,
    primitiveStateModes: newPrimitiveModes,
  );
}
```

#### 5.8.4 Moded Emptiness Check

A moded DFA is empty iff no reachable state is accepting:

```dart
bool get isModedEmpty {
  final visited = <DFAState>{};
  final worklist = [startState];

  while (worklist.isNotEmpty) {
    final state = worklist.removeLast();
    if (visited.contains(state)) continue;
    visited.add(state);

    // Check if accepting
    if (isPrimitiveState(state)) {
      if (getModesAt(state).isNotEmpty) return false;
    } else if (finalStates.contains(state)) {
      return false;
    }

    // Add successors
    for (final entry in transitions.entries) {
      final (from, _) = entry.key;
      if (from == state) worklist.add(entry.value);
    }
  }

  return true;
}
```

#### 5.8.5 DFA Completion

Add a sink state for missing transitions:

```dart
TypeDFA complete([Set<ModedLabel>? alphabet]) {
  final allLabels = alphabet ?? _computeAlphabet();
  final sinkState = DFAState('sink');
  var needsSink = false;

  final newTransitions = Map<(DFAState, ModedLabel), DFAState>.from(transitions);

  for (final state in states) {
    for (final label in allLabels) {
      if (!transitions.containsKey((state, label))) {
        newTransitions[(state, label)] = sinkState;
        needsSink = true;
      }
    }
  }

  if (needsSink) {
    // Add self-loops on sink
    for (final label in allLabels) {
      newTransitions[(sinkState, label)] = sinkState;
    }

    return TypeDFA(
      states: {...states, sinkState},
      startState: startState,
      finalStates: finalStates,  // sink is not final
      transitions: newTransitions,
      primitiveStateModes: primitiveStateModes,  // sink not primitive
    );
  }

  return this;
}

Set<ModedLabel> _computeAlphabet() {
  return transitions.keys.map((pair) => pair.$2).toSet();
}
```

### 5.9 Mode Restriction Notation

For a set P of moded paths, we define mode restrictions:

```
P|↓ = {(ξ, m) ∈ P | m = ↓}    (paths ending at output mode)
P|↑ = {(ξ, m) ∈ P | m = ↑}    (paths ending at input mode)
```

For a moded type S (represented as a DFA), we extend this notation via paths:

```
S|↓ = paths^m(S)|↓
S|↑ = paths^m(S)|↑
```

The well-moded-typing conditions compare these path sets:
- **Output Containment:** `T_M^{α,m}(S)|↓ ⊆ S|↓`
- **Input Coverage:** `S|↑ ⊆ T_M^{α,m}(S)|↑`

```dart
/// Extract paths at output mode from a moded type DFA
Set<ModedPath> outputPaths(TypeDFA dfa) {
  return extractModedPaths(dfa).where((p) => p.mode == Mode.output).toSet();
}

/// Extract paths at input mode from a moded type DFA
Set<ModedPath> inputPaths(TypeDFA dfa) {
  return extractModedPaths(dfa).where((p) => p.mode == Mode.input).toSet();
}
```

### 5.10 Correspondence with Paper

| Paper Section | Spec Section | Notes |
|---------------|--------------|-------|
| Definition (Moded Paths) | 5.7 | Paths are sequences of moded labels |
| Definition (Moded Type Automaton) | 5.2 (NFA), 5.6 (DFA) | NFA with ε-transitions, then DFA |
| Construction Algorithm | 5.3 | Union creates ε-transitions |
| Clause Contribution to NFA | 5.4 | Pattern-to-type conversion + NFA compilation |
| Subset Construction | 5.5 | Standard NFA→DFA with mode set union |
| Bi-Moded Types | 5.5.3 | Arise from mode set union in subset construction |
| Containment | 5.8.1 | A ⊆ B iff L(A ∩ B̄) = ∅ |
| Intersection | 5.8.2 | Product construction with mode intersection |
| Complement | 5.8.3 | Swap final states, complement mode sets |
| Emptiness | 5.8.4 | Reachability to accepting states |

---

## 6. Moded Type Checking Algorithm

### 6.1 Overview

The moded type checker extends the Yardeni-Shapiro algorithm with mode tracking:

```
For each procedure p/n with declared moded type (T₁^m₁, ..., Tₙ^mₙ):
  Let S = moded product type as DFA

  For each clause C = H :- B₁, ..., Bₘ defining p/n:

    // Step 1: Check ground paths with modes
    For every ground moded path ξ in body of C:
      If ξ ∉ paths^m(S):
        Report error: "ground path ξ not in moded type"
        Mark C as useless

    // Step 2: Infer variable moded types
    For each variable Y in C:
      Determine if Y is writer (X) or reader (X?)
      occurrenceTypes := []
      For each occurrence of Y in body:
        expectedMode := mode at that position from type
        actualMode := writer→output, reader→input
        If expectedMode ≠ actualMode:
          Report error: "mode mismatch for Y"
        occurrenceTypes.add(type at position)
      varTypes[Y] := intersect(occurrenceTypes)

    // Step 2.5: Apply guard constraints
    For each guard G in C:
      Extract type constraints from G's signature
      For each variable X constrained by G:
        varTypes[X] := varTypes[X] ∩ guardType(G, X)
        If varTypes[X] = ∅:
          Report error: "Guard inconsistent with head type"

      If G implies groundness for variable X:
        Mark X as recursively ground

    // Step 3: Check head variable modes
    For each variable Y in head:
      expectedMode := mode at head position from declaration
      actualMode := writer→output, reader→input
      If expectedMode.complement ≠ actualMode:
        Report error: "head mode mismatch for Y"

    // Step 4: Compute clause contribution via NFA
    typeExpr_C := headToTypeExpr(head, varTypes)
    nfa_C := TypeNFACompiler.compileExpr(typeExpr_C)
    T_C^{α,m}(S) := NFAToDFAConverter.convert(nfa_C)

  // Step 5: Check asymmetric well-moded-typing conditions
  inferred := union(T_C^{α,m}(S) for all clauses C)

  // Output Containment: T_M^{α,m}(S)|↓ ⊆ S|↓
  If NOT inferred|↓ ⊆ declared|↓:
    Report error: "output containment violated - clause produces value outside declared type"

  // Input Coverage: S|↑ ⊆ T_M^{α,m}(S)|↑
  If NOT declared|↑ ⊆ inferred|↑:
    Report error: "input coverage violated - declared input alternative not handled by any clause"
```

### 6.2 Mode Checking at Leaf Positions

The core mode check at variable positions:

```dart
class ModeChecker {
  /// Check that variable mode matches type expectation
  ModeError? checkVariableMode(
    VarTerm variable,      // The variable (with isReader flag)
    TypeRef expectedType,  // Type at this position
    bool isHeadPosition,   // Head vs body
  ) {
    // Variable's intrinsic mode
    final varMode = variable.isReader ? Mode.input : Mode.output;

    // Expected mode from type (complemented for head positions)
    var expectedMode = expectedType.isInput ? Mode.input : Mode.output;
    if (isHeadPosition) {
      expectedMode = expectedMode.complement;
    }

    // Check match
    if (varMode != expectedMode) {
      return ModeError(
        'Variable ${variable.name} has ${varMode.name} mode, '
        'expected ${expectedMode.name} at this position',
        variable.line,
        variable.column,
      );
    }
    return null;
  }
}
```

### 6.3 Body Goal Mode Complementation

When checking body goals:

```dart
/// Check a body goal against procedure declaration
List<ModeError> checkBodyGoal(Goal goal, ProcDecl decl) {
  final errors = <ModeError>[];

  // Get complemented types (caller→callee perspective shift)
  final calleeTypes = decl.calleeView;

  for (int i = 0; i < goal.args.length; i++) {
    final arg = goal.args[i];
    final expectedType = calleeTypes[i];

    // Recursively check argument against complemented type
    errors.addAll(checkTermAgainstType(arg, expectedType, isHead: false));
  }

  return errors;
}
```

### 6.4 Mode Coverage Check (Input Positions Only)

Mode coverage applies **only to input positions**. At input positions, clauses must collectively handle all mode alternatives in the declared type.

```dart
/// Check mode coverage at input positions
List<ModeError> checkModeCoverage(
  List<Clause> clauses,
  ProcDecl decl,
  TypeEnvironment env,
) {
  final errors = <ModeError>[];

  for (int argIndex = 0; argIndex < decl.arity; argIndex++) {
    final argType = decl.argTypes[argIndex];

    // Mode coverage applies only to INPUT positions
    if (!argType.isInput) continue;

    // Find positions with Any type (requires both mode alternatives)
    final anyPositions = findAnyPositions(argType, env);

    for (final position in anyPositions) {
      final coveredModes = <Mode>{};

      for (final clause in clauses) {
        final termAtPosition = extractTermAtPosition(clause.head, argIndex, position);
        if (termAtPosition is VarTerm) {
          // Head position: mode is complemented from declaration
          // Declaration says Type? (input) → head expects writer (output)
          // Declaration says Type (output) → head expects reader (input)
          final mode = termAtPosition.isReader ? Mode.input : Mode.output;
          coveredModes.add(mode);
        }
        // Ground terms cover all modes at that position
        if (termAtPosition != null && isGround(termAtPosition)) {
          coveredModes.add(Mode.output);
          coveredModes.add(Mode.input);
        }
      }

      if (coveredModes.length < 2) {
        final missing = coveredModes.contains(Mode.output) ? 'input' : 'output';
        errors.add(ModeError(
          'Input coverage violated: mode $missing not covered at position $position '
          'of argument ${argIndex + 1}',
          decl.line,
          decl.column,
        ));
      }
    }
  }

  return errors;
}
```

**Key Point:** At output positions, no mode coverage is required. Output containment only checks that produced values are within the declared type, not that all alternatives are covered.

### 6.5 Structural Coverage (Input Positions Only)

Structural coverage, like mode coverage, applies **only to input positions**. At input positions, clauses must collectively handle all constructor alternatives in the declared type.

#### 6.5.1 Output Containment Permits Subtyping

At **output** positions, the well-moded-typing condition is output containment:

```
T_M^{α,m}(S)|↓ ⊆ S|↓
```

This is a **subset** condition, not equality. A procedure may produce only a subset of its declared output type.

**Example (Well-Moded-Typed):**

```glp
Nat ::= 0 ; s(Nat).

procedure succ(Nat?, Nat).
succ(N, s(N?)).
```

The clause produces only terms of the form `s(Nat)`—it never produces `0`. Computing the clause contribution at output positions:

```
T_{succ}^{α,m}(S)|↓ = s(Nat)
```

The declared output type is `Nat = 0 | s(Nat)`, and `s(Nat) ⊆ Nat`. Output containment is satisfied.

**This program IS well-moded-typed.**

For input coverage, the clause head `succ(N, ...)` with writer `N` handles all `Nat` values at input position 1.

If a more precise output type is desired:

```glp
PosNat ::= s(Nat).

procedure succ(Nat?, PosNat).
succ(N, s(N?)).
```

Both declarations are well-moded-typed. The choice is a matter of precision in the interface specification.

#### 6.5.2 Input Coverage Requires Completeness

At **input** positions, the well-moded-typing condition is input coverage:

```
S|↑ ⊆ T_M^{α,m}(S)|↑
```

This is a **superset** condition. Clauses must handle all alternatives in the declared input type.

**Example (NOT Well-Moded-Typed):**

```glp
Nat ::= 0 ; s(Nat).

procedure pred(Nat?, Nat).
pred(s(N), N?).
```

The clause only handles `s(N)` at input position 1—it doesn't handle `0`. The declared input type is `Nat = 0 | s(Nat)`, but only `s(Nat)` is covered.

**This program is NOT well-moded-typed** — input coverage violated.

**Solution (Precise Input Type):**

```glp
PosNat ::= s(Nat).

procedure pred(PosNat?, Nat).
pred(s(N), N?).
```

Now the declared input type is `PosNat = s(Nat)`, which is fully covered.

#### 6.5.3 Summary: Asymmetric Coverage Requirements

| Position | Condition | Requirement |
|----------|-----------|-------------|
| Output (↓) | Output Containment | `inferred ⊆ declared` — no full coverage needed |
| Input (↑) | Input Coverage | `declared ⊆ inferred` — all alternatives must be handled |

This asymmetry reflects the compositional semantics:
- Writers (outputs) only need to produce valid values
- Readers (inputs) must accept all possible values

#### 6.5.4 Implementation

```dart
/// Check structural coverage at input positions
List<TypeError> checkStructuralCoverage(
  ProcDecl decl,
  List<TypeDFA> declaredDFAs,
  List<TypeDFA> inferredDFAs,
) {
  final errors = <TypeError>[];

  for (int i = 0; i < decl.arity; i++) {
    final argType = decl.argTypes[i];

    // Structural coverage applies only to INPUT positions
    if (!argType.isInput) continue;

    final declaredDFA = declaredDFAs[i];
    final inferredDFA = inferredDFAs[i];

    // Input Coverage: declared|↑ ⊆ inferred|↑
    if (!inputPaths(declaredDFA).isSubsetOf(inputPaths(inferredDFA))) {
      errors.add(TypeError(
        'Input coverage error: argument ${i + 1} of ${decl.name}/${decl.arity}\n'
        'Declared input type not fully covered by clauses.\n'
        'Hint: Add clauses for missing constructors, or use a more precise input type.',
        decl.line,
        decl.column,
      ));
    }
  }

  return errors;
}

/// Check output containment at output positions
List<TypeError> checkOutputContainment(
  ProcDecl decl,
  List<TypeDFA> declaredDFAs,
  List<TypeDFA> inferredDFAs,
) {
  final errors = <TypeError>[];

  for (int i = 0; i < decl.arity; i++) {
    final argType = decl.argTypes[i];

    // Output containment applies only to OUTPUT positions
    if (argType.isInput) continue;

    final declaredDFA = declaredDFAs[i];
    final inferredDFA = inferredDFAs[i];

    // Output Containment: inferred|↓ ⊆ declared|↓
    if (!outputPaths(inferredDFA).isSubsetOf(outputPaths(declaredDFA))) {
      errors.add(TypeError(
        'Output containment error: argument ${i + 1} of ${decl.name}/${decl.arity}\n'
        'Clause produces value outside declared output type.',
        decl.line,
        decl.column,
      ));
    }
  }

  return errors;
}
```

#### 6.5.5 Correspondence with Paper

This section implements:
- Paper Definition 6.12 (Well-Moded-Typing) with asymmetric conditions
- Paper Example (Output Containment Permits Subtyping)
- Paper Remark (Asymmetric Coverage Requirements)

---

## 7. Guards and Type Inference

Guards provide type and mode information that constrains variable types within a clause.

### 7.1 Built-in Guard Signatures

Built-in guards have known type signatures:

| Guard | Argument Types | Implies Ground |
|-------|---------------|----------------|
| `number(X?)` | (Number) | Yes |
| `integer(X?)` | (Number) | Yes |
| `string(X?)` | (String) | Yes |
| `ground(X?)` | (Input) | Yes (recursively) |
| `known(X?)` | (Any) | No |
| `unknown(X?)` | (Any) | No |
| `X? < Y?` | (Number, Number) | Yes |
| `X? > Y?` | (Number, Number) | Yes |
| `X? =< Y?` | (Number, Number) | Yes |
| `X? >= Y?` | (Number, Number) | Yes |
| `X? =:= Y?` | (Number, Number) | Yes |
| `X? =\= Y?` | (Number, Number) | Yes |
| `X? =?= Y?` | (Any, Any) | Yes |

### 7.2 Type Constraint Extraction

When a guard succeeds, it constrains the types of its arguments. These constraints are intersected with types inferred from head patterns:

```
For each guard G in clause C:
  For each argument position i of G:
    Let T_guard = declared type for position i of G
    Let X = variable at position i (if any)
    varTypes[X] := varTypes[X] ∩ T_guard
    If varTypes[X] = ∅:
      Report error: "Guard type inconsistent with pattern type"
```

### 7.3 Ground Guards

The ground guard is typed using the `Input` type (Section 2.5.3):

```prolog
procedure ground(Input).
```

#### 7.3.1 Well-Moded-Typed Ground Guard

A call `ground(X?)` is **well-moded-typed (WMT)** iff `Input ∩ T_X = T_X`, where `T_X` is the type of X.

By the Output Intersection Property (Section 2.5.3) and `Input ::= Output?`, this holds iff `T_X` has no mode complementations (no `?` in type definition).

**Example - WMT ground guard:**
```glp
List ::= [] ; [_ | List].

procedure broadcast(List?, List, List).
broadcast(X, Y, Z) :- ground(X?) |
    send(X?, Y),
    send(X?, Z).
```

The call `ground(X?)` is WMT: `Input ∩ List = List` (List has no mode complementations).

**Example - Ill-typed ground guard:**
```glp
DiffList ::= List \ List?.

procedure bad(DiffList?).
bad(X) :- ground(X?) | process(X?).
```

The call `ground(X?)` is **not** WMT: `Input ∩ DiffList ≠ DiffList` (DiffList contains `List?`).

#### 7.3.2 Implementation

The WMT check for ground guards reduces to checking that the variable's type has no mode complementations:

```dart
/// Check if a moded type has no mode complementations
bool hasNoModeComplementations(String typeName, TypeEnvironment env, Set<String> visited) {
  // Prevent infinite recursion on recursive types
  if (visited.contains(typeName)) return true;

  // Built-in types have no complementations
  if (typeName == 'Number' || typeName == 'String') return true;
  if (typeName == 'Output' || typeName == 'Input') return true;

  // Primitive modes
  if (typeName == '_') return true;
  if (typeName == '_?') return false;  // Has complementation

  final typeDef = env.getType(typeName);
  if (typeDef == null) return true;

  visited.add(typeName);

  for (final alt in typeDef.alternatives) {
    if (_containsModeComplementation(alt, env, visited)) return false;
  }
  return true;
}

bool _containsModeComplementation(TypeExpr expr, TypeEnvironment env, Set<String> visited) {
  if (expr is TypeRef) {
    if (expr.isInput) return true;  // T? has complementation
    return !hasNoModeComplementations(expr.name, env, visited);
  }
  if (expr is StructAlt) {
    return expr.args.any((a) => _containsModeComplementation(a, env, visited));
  }
  if (expr is ListConsAlt) {
    return _containsModeComplementation(expr.head, env, visited) ||
           _containsModeComplementation(expr.tail, env, visited);
  }
  if (expr is DiffListAlt) {
    return _containsModeComplementation(expr.head, env, visited) ||
           _containsModeComplementation(expr.tail, env, visited);
  }
  return false;  // Constants, primitives without ? have no complementation
}

/// Check ground guard well-moded-typing via Input ∩ T = T
bool isGroundGuardWMT(ast.Goal guard, Map<String, TypeDFA> varTypes, TypeEnvironment env) {
  if (guard.functor != 'ground') return true;
  if (guard.args.isEmpty) return true;

  final arg = guard.args[0];
  if (arg is! ast.VarTerm) return true;

  final varType = varTypes[arg.name];
  if (varType == null) return true;

  return hasNoModeComplementations(varType.startState.name, env, <String>{});
}
```

### 7.4 Defined Guards

Defined guards (user-defined predicates callable in guard position) require procedure declarations with moded types, just like any other procedure. No special treatment is needed.

```
% Unit clause defining a type test
channel(ch(_, _)).

% Requires procedure declaration:
procedure channel(Channel?).
```

The defined guard is type-checked as a body goal with call-boundary complementation applied.

### 7.5 Example: Bidirectional Channels

Channels pair two streams with complementary modes:

```prolog
% Stream has no nil case - may remain open
Stream ::= [Any | Stream].

% Channel pairs two streams with complementary modes
Channel ::= ch(Stream?, Stream) ; ch(Stream, Stream?).

procedure create_channel(Channel, Channel).
create_channel(ch(AtoB?, BtoA), ch(BtoA?, AtoB)).
```

The `Channel` type has two alternatives capturing endpoint duality:
- `ch(Stream?, Stream)` — reads from first stream, writes to second
- `ch(Stream, Stream?)` — writes to first stream, reads from second

Since `Stream ::= [Any | Stream]` has no `[]` alternative, there is no requirement that streams close.

**Bounded Buffer Example:**

A bounded buffer uses an inverted stream of empty slots:

```prolog
InvStream ::= [] ; [_? | InvStream].

procedure bounded_buffer(Stream?, InvStream?, Stream).
bounded_buffer(In, Slots, Out) :-
    % Takes values from In, consumes slots, produces Out
    ...
```

The `InvStream` of slots has `_?` elements (input mode), representing empty positions the buffer can fill.

---

## 8. Error Messages

### 8.1 Mode Errors

| Situation | Message |
|-----------|---------|
| Writer at output position | `Writer variable 'X' at line 5 occurs at output position; expected input position (_?)` |
| Reader at input position | `Reader variable 'X?' at line 7 occurs at input position; expected output position (_)` |
| Mode mismatch in call | `Argument 2 of merge/3 at line 10: expected input mode, found output` |
| Incomplete mode coverage | `No clause covers output mode (_) at Any position in list head` |

### 8.2 Example Error Output

```
[MODE ERROR] Writer variable 'Result' at line 8, column 15 occurs at output
position in type 'Number'. In moded types, writers must occur at input
positions (_?) where the caller provides a slot for the callee to fill.

Hint: The procedure declaration is:
  procedure compute(Number?, Number).
         argument 2 ────────────┘ (output mode)

At output positions, use a reader variable (Result?) to receive the value.
```

### 8.3 Mode Coverage Error

```
[MODE ERROR] Incomplete mode coverage at Any position in argument 1.

The type 'List ::= [] ; [Any | List]' has Any at the head position.
At input positions, clauses must cover BOTH mode alternatives:
  - _ (output): requires writer variable
  - _? (input): requires reader variable

Current clauses only cover: _? (input)

Solutions:
  1. Add clause with writer at head position
  2. Change type to use single mode: [_ | List] (output only)
```

---

## 9. Implementation Plan

### Phase 1: Parser Extension (1 day)
- [ ] Extend `type_parser.dart` to parse `Type?` in procedure declarations
- [ ] Store `isInput` flag in `TypeRef`
- [ ] Update `ProcDecl` to carry moded types
- [ ] Add tests for moded procedure parsing

### Phase 2: Mode Checker Core (2 days)
- [ ] Create `mode_checker.dart` with `ModeChecker` class
- [ ] Implement `checkVariableMode()` for leaf position checking
- [ ] Implement mode combination for nested positions
- [ ] Implement call boundary complementation
- [ ] Implement mode coverage check for Any positions
- [ ] Add comprehensive tests

### Phase 2.5: Guard Type Checking (1 day)
- [ ] Create GuardTypeRegistry with built-in guard signatures
- [ ] Implement guard constraint extraction
- [ ] Integrate guard constraints into variable type inference
- [ ] Track recursively-ground variables from ground-implying guards
- [ ] Update mode coverage to recognize ground-protected variables
- [ ] Tests for guard type constraints
- [ ] Tests for ground guards bypassing mode coverage

### Phase 3: Integrate with Type Checker (2 days)
- [ ] Extend `type_checker.dart` to invoke mode checker
- [ ] Moded DFA compilation in `type_compiler.dart`
- [ ] Moded path extraction
- [ ] Unified error reporting

### Phase 4: Upgrade Book Programs (3 days)
- [ ] Add mode annotations to all 152 procedure declarations
- [ ] Verify all programs pass moded type checking
- [ ] Document any necessary program changes
- [ ] Create test suite for mode checking

### Phase 5: Documentation & Polish (1 day)
- [ ] Update `--type-check` help text
- [ ] Add mode checking examples to docs
- [ ] Update SPEC_GUIDE.md

**Total estimate: 10 days**

---

## 10. File Organization

```
lib/
  analysis/
    type_checker/
      type_ast.dart          # Extended with isInput on TypeRef
      type_parser.dart       # Extended to parse Type?
      type_dfa.dart          # Extended with mode annotations
      type_compiler.dart     # Extended for moded compilation
      type_checker.dart      # Integrated mode checking
      mode_checker.dart      # NEW: Core mode checking logic
      mode_error.dart        # NEW: Mode-specific errors
```

---

## 11. CLI Integration

The `--type-check` flag performs both structural type checking and mode checking:

```bash
# Type check with modes (default behavior)
dart bin/glpc.dart --type-check file.glp

# Strict mode: abort on any type or mode error
dart bin/glpc.dart -t -s file.glp
```

Mode checking is **not** a separate phase — it's integrated into type checking as the moded extension of the Yardeni-Shapiro algorithm.

---

## 12. Theoretical Foundation

This implementation follows the theory developed in "Moded Types for Grassroots Logic Programs" (2024), which establishes:

1. **Moded types as distributive abstraction of trace semantics** (Theorem 6.18)
2. **Partial correctness guarantee**: Well-moded-typed programs have produced/consumed assignments conforming to declared types
3. **EXPTIME-complete complexity** for moded type checking
4. **Mode complementation `(·)?`** as the uniform mechanism for producer/consumer duality
5. **Mode coverage requirement**: Under `::=` semantics, union types require coverage of all alternatives

---

## 13. References

- Yardeni & Shapiro, "A Type System for Logic Programs", JLP 1991
- Frühwirth, Shapiro, Vardi & Yardeni, "Logic Programs as Types for Logic Programs", LICS 1991
- "Moded Types for Grassroots Logic Programs", 2024 (this project)

---

## Appendix A: Changelog

### v1.10 (2025-12-25)
- **NEW** Section 5.4: Clause Contribution via NFA Construction
  - 5.4.1: Principle - Clause patterns as type expressions
  - 5.4.2: Pattern-to-Type Correspondence - Mapping pattern constructs to type constructs
  - 5.4.3: NFA Construction Algorithm - `patternToTypeExpr` + `TypeNFACompiler`
  - 5.4.4: Variable Type as TypeRef - Mode encoding via `isInput` flag
  - 5.4.5: Correspondence with Paper - Alignment with Section 7.4
- **UPDATED** Section 6.1: Algorithm Steps 4-5 to reference NFA-based clause contribution
- **RENUMBERED** Sections 5.4-5.8:
  - Old 5.4 → 5.5 (NFA to DFA Conversion)
  - Old 5.5 → 5.6 (DFA Representation)
  - Old 5.6 → 5.7 (Accepting Moded Paths)
  - Old 5.7 → 5.8 (Operations on Moded Type DFA)
  - Old 5.8 → 5.9 (Correspondence with Paper)
- **UPDATED** Section 5.9: Correspondence table with new section numbers and clause contribution row
- **RATIONALE:** Aligns spec with paper Section 7.4: "Each clause of a procedure similarly contributes to the NFA representing the inferred type." By converting patterns to type expressions, we reuse the existing NFA→DFA pipeline rather than building parallel infrastructure. This ensures uniform representation, correct mode encoding, and primitive mode preservation.

### v1.9 (2025-12-25)
- **COMPLETE REWRITE** Section 5: Moded Type Automata - NFA Construction and DFA Conversion
  - 5.1: Moded Transition Labels - Renamed PathElement to ModedLabel, added epsilon transitions
  - 5.2: Moded Type NFA - Full NFA representation with ε-transitions and mode sets
  - 5.3: NFA Construction from Type Definitions
    - 5.3.1: Union Types (Disjunction) - ε-transitions for nondeterministic choice
    - 5.3.2: Primitive Types - Single-state NFAs with mode annotations
    - 5.3.3: Structure Types - Labeled transitions with mode encoding
    - 5.3.4: Type References - Mode complementation for `T?`
    - 5.3.5: Example: Channel Type - Complete NFA construction walkthrough
  - 5.4: NFA to DFA Conversion (Subset Construction)
    - 5.4.1: ε-Closure algorithm
    - 5.4.2: Subset Construction Algorithm with mode set union
    - 5.4.3: Mode Set Union in Subset Construction - How bi-moded types arise naturally
  - 5.5: DFA Representation - Final deterministic automaton structure
  - 5.6: Accepting Moded Paths - Acceptance criteria for moded paths
  - 5.7: Operations on Moded Type DFA
    - 5.7.1: Moded Type Containment (A ⊆ᵐ B)
    - 5.7.2: Moded Type Intersection (product construction)
    - 5.7.3: Moded Type Complement (mode set complementation)
    - 5.7.4: Moded Emptiness Check
    - 5.7.5: DFA Completion (sink state addition)
  - 5.8: Correspondence with Paper - Complete mapping to paper definitions
- **RENAMED** PathElement → ModedLabel throughout entire specification
- **UPDATED** Section 3.3: Moded Path Representation to reference ModedLabel
- **RATIONALE:** This rewrite properly implements the Yardeni-Shapiro NFA→DFA algorithm. Union types (`T ::= A ; B`) create ε-transitions representing nondeterministic choice. Bi-moded types like `Every ::= _ ; _?` arise naturally from subset construction merging NFA states with different modes. The previous v1.8 approach attempted to encode modes directly in the DFA, which could not correctly handle union types and led to undebuggable implementation issues.

### v1.8 (2025-12-24)
- **REVISED** Section 5.4: Accepting Moded Paths
  - Clarified two-case acceptance: structural (qₙ ∈ F, non-primitive) vs primitive (m ∈ μ(qₙ))
  - Added moded language definition L^m(A)
  - Updated code to match paper formulation
- **NEW** Section 5.8: Operations on Moded Type DFA
  - 5.8.1: Primitive State Classification (outputOnly, inputOnly, biModed)
  - 5.8.2: Moded Type Containment (isSubsetOf) with bi-moded optimization
  - 5.8.3: Moded Type Intersection (product construction with mode intersection)
  - 5.8.4: Moded Type Emptiness (reachability to accepting states)
  - 5.8.5: Moded Type Complement (mode set complementation)
  - 5.8.6: Correspondence with Paper Section 7.5.1
  - Purpose: Provide operational definitions for DFA operations required by type checking

### v1.7 (2025-12-24)
- **NEW** Section 5.7: Moded Path Elements for TypeRef Positions
  - 5.7.1: The Problem - Structural ambiguity in channel types with mode-distinguished alternatives
  - 5.7.2: Two Mode Tracking Mechanisms - `primitiveStateModes` vs `mode` field on ModedLabel
  - 5.7.3: ModedLabel with Optional Mode - Extended ModedLabel class specification
  - 5.7.4: Type Compilation Rules - How to encode mode in ModedLabel based on argument syntax
  - 5.7.5: Clause Contribution - Matching declared type's mode encoding in contribution computation
  - 5.7.6: Correspondence with Paper - Static DFA encoding vs dynamic mode traversal
  - 5.7.7: ListConsAlt Handling - Same rules apply to list syntax
  - 5.7.8: Summary Table - Complete examples of mode encoding by position type
  - Purpose: Close gap between paper's dynamic mode traversal and spec's static DFA encoding

### v1.6 (2025-12-22)
- **FIXED** Section 2.1: Removed incorrect `Any ::= _ ; _?` definition
- **FIXED** Section 4.4: Renamed to "Mode Coverage for Exact Union Types (::=)"
  - Clarified: `Every ::= _ ; _?` requires coverage, `Any ::< Every` does not
  - Renamed examples from AnyList to EveryList
  - Added "Why Standard List Has No Coverage Requirement" subsection
- **FIXED** Section 6.4: Renamed to "Mode Coverage Check for Every Positions"
- **NEW** Section 2.5.3: Output and Input Types
  - `Output` = type where all variable positions have mode `_` (writers)
  - `Input ::= Output?` = type where all variable positions have mode `_?` (readers)
  - Output Intersection Property: `Output ∩ T = T` iff T has no mode complementations
  - Renumbered subsequent subsections (2.5.4-2.5.8)
- **REWRITTEN** Section 7.3: Ground Guards
  - Ground guard typed as `procedure ground(Input).`
  - WMT condition: `ground(X?)` is WMT iff `Input ∩ T_X = T_X`
  - Equivalently: WMT iff T_X has no mode complementations (no `?` in type definition)
  - Added implementation pseudocode for `hasNoModeComplementations()` and `isGroundGuardWMT()`
  - Removed previous "moded type without mode complementations" approach in favor of type intersection formalization
- **Clarification:** Mode coverage applies only to `::=` types with primitive mode alternatives, not to `::< ` subtypes

### v1.5 (2025-12-22)
- **REVISED Section 5: Moded Type DFA** to match paper specification
  - Replaced `Map<String, Mode> stateModes` with `Map<DFAState, Set<Mode>> primitiveStateModes`
  - Removed incorrect default `Mode.output` for non-primitive states
  - Added `isPrimitiveState()` to distinguish primitive from structural positions
  - Added table mapping type alternatives to mode sets
  - Added `acceptsModedPath()` specification
  - Added Section 5.5: Mode Computation During Traversal
  - Added Section 5.6: Correspondence with Paper
  - Emphasized integration: mode checking happens during type traversal, not as separate pass

### v1.4 (2025-12-22)
- **MAJOR REVISION** of Section 2.5: Predefined Types
  - Introduced `Every ::= _ ; _?` and `Any ::< Every` distinction
  - Added comprehensive explanation of self-duality: `(Every)? = Every` and `(Any)? = Any`
  - Consequence: Mode annotations on `Any` positions are semantically irrelevant
  - Changed `List` to use `Any` elements: `List ::= [Any | List] ; []`
  - Added `DiffList ::= List \ List?` for difference lists with holes
  - Added predefined procedures: `dl_append`, `dl_to_list`, `new_channel`, `send`, `receive`
  - Added Section 2.5.6: Usage as Defined Guards
  - Added Section 2.5.7: EveryList theoretical example (requires full mode coverage)
  - Removed `InvStream` (no longer needed with new `Any` semantics)
- Updated Section 2.5.4: Channels now use single constructor `ch(Stream?, Stream)`
- Throughout document: replaced references to old List type with context-appropriate `List` (Any elements) or `EveryList` (Every elements)

### v1.3 (2025-12-22)
- Updated Section 2.1: Clarified that `_` and `_?` are primitive modes, distinct from `Any ::= _ ; _?`
- Added Section 2.5: System Type Definitions
  - Defined `List ::= [] ; [_ | List]` (standard list with output-mode elements)
  - Defined `Stream ::< List` (open-ended stream, subtype)
  - Defined `InvStream ::= [] ; [_? | InvStream]` (input-mode elements, for bounded buffers)
  - Introduced `AnyList ::= [] ; [Any | AnyList]` as theoretical type
- Renamed Section 2.5 → 2.7, Section 2.6 → 2.8 (due to new Section 2.5)
- Updated Section 4.4: Changed "List Copy" example to "AnyList Copy" to distinguish from standard List
- Updated all `List ::= [] ; [Any | List]` occurrences to use `AnyList` when demonstrating mode coverage with Any
- Updated Section 7.5: Changed Channel example to use `Stream ::< List`, added bounded buffer example with InvStream
- Updated error message examples to use AnyList for Any-based lists

### v1.2 (2025-12-22)
- Added Section 7: Guards and Type Inference
  - Section 7.1: Built-in Guard Signatures
  - Section 7.2: Type Constraint Extraction
  - Section 7.3: Ground Guards and Mode Coverage
  - Section 7.4: Defined Guards
  - Section 7.5: Bidirectional Channels Example
- Added Step 2.5 to Section 6.1: Apply guard constraints in type checking algorithm
- Added Phase 2.5 to Implementation Plan: Guard Type Checking (1 day)
- Updated total implementation estimate to 10 days
- Renumbered sections 7-12 to 8-13

### v1.1 (2025-12-21)
- Added Section 2.2: Type Definitions vs. Subtype Declarations (`::=` vs `::<`)
- Added Section 4.4: Mode Coverage for Union Types with List Copy example
- Added Section 6.4: Mode Coverage Check for Any Positions
- Added Section 7.3: Mode Coverage Error messages
- Updated theoretical foundation with mode coverage requirement
