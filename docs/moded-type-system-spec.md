# GLP Moded Type System Specification (v1.6)

**Updated:** 2025-12-22
**Change:** Fixed mode coverage - Every requires coverage, Any does not

## 1. Overview

This document extends the GLP Type System Specification to include **moded types**. Moded types add input/output mode annotations to structural types, capturing the distinction between values produced by the program and values consumed from the environment.

### 1.1 Relationship to Existing Type System

| Aspect | Unmoded Types (Current) | Moded Types (Extension) |
|--------|------------------------|-------------------------|
| Semantics | Model-theoretic (success set) | Trace semantics (produced/consumed) |
| What it checks | Structural correctness | Structural + directional correctness |
| Abstraction | T_P^α (tuple-distributive closure) | T_M^{α,m} (moded tuple-distributive closure) |
| Fixpoint | T_P^α(S) = S | T_M^{α,m}(S) = S |

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

### 2.2 Type Definitions vs. Subtype Declarations

Following Yardeni-Shapiro, we distinguish two declaration forms:

| Syntax | Semantics | Fixpoint Condition | Coverage |
|--------|-----------|-------------------|----------|
| `T ::= S` | Type definition | T_M^{α,m}(S) = S | Complete (equality) |
| `T ::< S` | Subtype declaration | T_M^{α,m}(S') ⊆ S | Partial (subset) |

**Type definition (`::=`)**: Clauses must collectively cover ALL alternatives of S.

**Subtype declaration (`::<`)**: Clauses need only cover SOME SUBSET of S. This is an escape hatch for partial implementations.

**GLP Implementation Status:** GLP currently implements only `::=`. The `::< ` form is reserved for future Polymorphic Moded Types (PMT).

### 2.3 Moded Type Expressions

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

### 2.4 Grammar Extension

```
proc_decl     ::= 'procedure' atom '(' moded_type_refs ')' '.'
moded_type_refs ::= moded_type_ref (',' moded_type_ref)*
moded_type_ref  ::= type_ref '?'?
```

The `?` suffix on a type reference indicates input mode.

### 2.5 Predefined Types

The following types and procedures are predefined by prepending their definitions to every module. A module cannot redefine a predefined type or procedure.

#### 2.5.1 Primitive Types

```prolog
Number.   % numeric values (built-in)
String.   % string values (built-in)
```

#### 2.5.2 Universal Types

```prolog
Every ::= _ ; _?.      % exact: requires both mode alternatives covered
Any ::< Every.         % subtype: no coverage requirement
```

**Self-Duality of Every and Any:**

Since `Every ::= _ ; _?` contains both output and input modes as alternatives, the type is *self-dual*:

```
(Every)? = Every
```

Complementing `Every` yields `Every`. The same holds for `Any ::< Every`:

```
(Any)? = Any
```

**Consequence:** Mode annotations on `Any` positions are semantically irrelevant. Writing `Any` or `Any?` in a procedure declaration has the same meaning—both writer and reader variables are acceptable at such positions. Since `Any` uses subtype semantics (`::< `), there is no coverage requirement either.

This self-duality means `Any` truly represents "any value with any mode"—the universal type for positions where mode is unconstrained.

#### 2.5.3 Collections

```prolog
List ::= [Any | List] ; [].
Stream ::< List.               % may remain open (no [] case required)
DiffList ::= List \ List?.     % difference list with hole
```

**List** uses `Any` for elements. Since `Any` has no coverage requirement, a two-clause copy suffices:

```prolog
procedure copy(List?, List).
copy([], []).
copy([X | In], [X? | Out]) :- copy(In?, Out).
```

**Stream** uses subtype semantics, so procedures need not handle the `[]` case.

**DiffList** represents a list with a hole at the end. The structure `List \ List?` pairs:
- `List` (output): the content produced so far
- `List?` (input): the hole where more content can be appended

#### 2.5.4 Channels

```prolog
Channel ::= ch(Stream?, Stream).
```

A channel pairs two streams with complementary modes:
- First stream (`Stream?`): input—messages received
- Second stream (`Stream`): output—messages sent

The `new_channel` operation creates two complementary endpoints by swapping the streams.

#### 2.5.5 Predefined Procedures

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

#### 2.5.6 Usage as Defined Guards

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

#### 2.5.7 EveryList (Theoretical Example)

For theoretical analysis, one may define a list requiring full mode coverage at element positions:

```prolog
EveryList ::= [Every | EveryList] ; [].
```

Unlike `List` (which uses `Any`), `EveryList` requires three clauses for copy:

```prolog
procedure copy(EveryList?, EveryList).
copy([], []).
copy([X | In], [X? | Out]) :- copy(In?, Out).   % element flows in→out
copy([X? | In], [X | Out]) :- copy(In?, Out).   % element flows out→in
```

The third clause covers the `_?` alternative of `Every`. This is primarily of theoretical interest; practical programs use `List` with `Any` elements.

### 2.7 Embedded Modes in Type Definitions

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

### 2.8 Examples

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

```dart
/// A path with mode annotation at the leaf
class ModedPath {
  final List<PathStep> steps;  // e.g., [f(2,1), g(3,2)]
  final Mode mode;             // Mode at leaf position

  ModedPath(this.steps, this.mode);

  @override
  String toString() => '${steps.join('·')}:${mode.name}';
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

### 4.4 Mode Coverage for Exact Union Types (::=)

Under `::=` semantics, union types require coverage of **all** alternatives. This has critical implications for `Every ::= _ | _?`:

**Important Distinction:**
- `Every ::= _ ; _?` — exact definition, REQUIRES mode coverage
- `Any ::< Every` — subtype declaration, NO coverage requirement

Since `Any` uses `::< ` (subtype), not `::=` (exact), **`Any` positions have NO coverage requirement**. Procedures using `Any` need not cover both modes.

**Mode Coverage Requirement:** If a type position has type `Every ::= _ | _?` under `::=` semantics, clauses must collectively cover **both** mode alternatives:
- Some clause(s) must handle the `_` case (writer at that position)
- Some clause(s) must handle the `_?` case (reader at that position)

A single clause typically covers only one mode combination.

#### Example: EveryList Copy (Why Every Requires Mode Coverage)

Consider a list type with `Every` at the head position:
```glp
EveryList ::= [] ; [Every | EveryList].

procedure copy(EveryList?, EveryList).

copy([], []).
copy([H? | In], [H | Out?]) :- copy(In?, Out).
```

**SRSW check:** Each variable has exactly one writer and one reader ✓

**Question:** Is this program well-moded-typed?

**Analysis:** At the head position of `[H? | In]` and `[H | Out?]`, the type is `Every ::= _ | _?`. Under `::=` semantics, clauses must collectively cover both alternatives:
- `_` requires a writer variable at that position
- `_?` requires a reader variable at that position

The single clause `copy([H? | In], [H | Out?])` covers only one mode combination:
- Input head: `H?` (reader) → matches `_?`
- Output head: `H` (writer) → matches `_`

The opposite combination (writer at input head, reader at output head) is **not covered**.

**Verdict: This program is NOT well-moded-typed.**

#### Solution 1: Restrict to Output-Mode Heads

```glp
List1 ::= [] ; [_ | List1].

procedure copy(List1?, List1).

copy([], []).
copy([H? | In], [H | Out?]) :- copy(In?, Out).
```

Head type is `_` (not `Any`), so only one mode needs coverage. **Well-moded-typed.**

#### Solution 2: Restrict to Input-Mode Heads

```glp
List2 ::= [] ; [_? | List2].

procedure copy(List2?, List2).

copy([], []).
copy([H | In], [H? | Out?]) :- copy(In?, Out).
```

Head type is `_?` (not `Any`), so only one mode needs coverage. **Well-moded-typed.**

#### Solution 3: Cover Both Modes with Multiple Clauses

```glp
EveryList ::= [] ; [Every | EveryList].

procedure copy(EveryList?, EveryList).

copy([], []).
copy([H? | In], [H | Out?]) :- copy(In?, Out).
copy([H | In], [H? | Out?]) :- copy(In?, Out).
```

The two non-base clauses collectively cover both mode combinations:
- First clause: reader at input head (`_?`), writer at output head (`_`)
- Second clause: writer at input head (`_`), reader at output head (`_?`)

**Well-moded-typed.**

#### Why Standard List Has No Coverage Requirement

The standard `List` type uses `Any` for elements:

```glp
List ::= [] ; [Any | List].
```

Since `Any ::< Every` (subtype, not exact), there is **NO mode coverage requirement** at the head position. A two-clause copy suffices:

```glp
procedure copy(List?, List).
copy([], []).
copy([H? | In], [H | Out?]) :- copy(In?, Out).
```

This is well-moded-typed because:
1. `Any` has no coverage obligation (subtype semantics)
2. The single clause's mode combination is valid (reader at input, writer at output)

Compare with `EveryList ::= [Every | EveryList]` which WOULD require three clauses to cover all mode combinations.

#### Design Principle

Under `::=` semantics, `Every` positions in type definitions impose coverage obligations that typically require:
1. **Restricting the type** to a single mode (`List1`, `List2`)
2. **Multiple clauses** covering each mode alternative
3. **Using `::< S`** to permit partial coverage (escape hatch, future PMT)

The choice depends on intended semantics: does the procedure genuinely need to handle both modes at that position?

---

## 5. Moded Type DFA

### 5.1 Moded Paths

A **moded path** (as defined in Section 3.3) is a path together with the mode annotation at its leaf. A moded type is characterized by its set of moded paths: `paths^m(S)`.

Following the paper (Definition 6.15):
- A path describes a position in a term (sequence of functor/argument-index steps)
- A moded path pairs this with the mode at that leaf position
- The DFA accepts moded paths, not just structural paths

### 5.2 Primitive State Modes

The type DFA tracks mode information at **primitive type positions** (`_` and `_?`). Non-primitive positions are purely structural.

```dart
class TypeDFA {
  final Set<DFAState> states;
  final DFAState startState;
  final Set<DFAState> finalStates;
  final Map<(DFAState, PathElement), DFAState> transitions;
  
  /// Mode information at primitive type states.
  /// 
  /// A state appears in this map iff it corresponds to a primitive type
  /// position (_ or _?) in a type definition:
  /// - {Mode.output} for _ (program produces value)
  /// - {Mode.input} for _? (program consumes value)
  /// - {Mode.output, Mode.input} for Every ::= _ ; _?
  ///
  /// States not in this map are structural (non-primitive) positions.
  final Map<DFAState, Set<Mode>> primitiveStateModes;
  
  /// Check if state is a primitive type position
  bool isPrimitiveState(DFAState state) => 
      primitiveStateModes.containsKey(state);
  
  /// Get accepted modes at a primitive state (empty for non-primitive)
  Set<Mode> getModesAt(DFAState state) => 
      primitiveStateModes[state] ?? {};
}
```

### 5.3 Compiling Primitive Types

When compiling a type definition to DFA, primitive types map to mode sets:

| Type alternative | `primitiveStateModes` entry |
|-----------------|----------------------------|
| `_` | `{Mode.output}` |
| `_?` | `{Mode.input}` |
| `Every ::= _ ; _?` | `{Mode.output, Mode.input}` |
| `Any ::< Every` | `{Mode.output, Mode.input}` (inherited) |
| Non-primitive (constructors, type refs) | Not in map |

```dart
void _compileAlternative(DFAState state, TypeExpr alt) {
  if (alt is PrimitiveModeAlt) {
    // Primitive type: mark state with its mode
    final mode = alt.isInput ? Mode.input : Mode.output;
    primitiveStateModes[state] = 
        (primitiveStateModes[state] ?? <Mode>{})..add(mode);
    finalStates.add(state);  // Primitive positions are accepting
    return;
  }
  // ... handle constructors, type references, etc.
}
```

### 5.4 Accepting Moded Paths

A DFA accepts moded path `(ξ, m)` iff:
1. Path `ξ` leads from start state to a final state `q`, AND
2. Either `q` is not primitive, OR `m ∈ primitiveStateModes[q]`

```dart
bool acceptsModedPath(ModedPath path) {
  var current = startState;
  for (final elem in path.steps) {
    final next = transitions[(current, elem)];
    if (next == null) return false;
    current = next;
  }
  
  if (!finalStates.contains(current)) return false;
  
  // At primitive positions, verify mode is accepted
  if (isPrimitiveState(current)) {
    return getModesAt(current).contains(path.mode);
  }
  return true;
}
```

### 5.5 Mode Computation During Traversal

During type checking, mode is computed while traversing term and type in parallel. This integrates mode checking with type checking—they are **not** separate passes.

1. Start with declared mode from procedure declaration
2. At each type reference `T` or `T?`, apply `combineMode` (Section 4.2)
3. At primitive positions, verify variable mode ∈ accepted modes

```dart
/// During type checking traversal
void checkVariableAtPosition(
  Variable variable,
  DFAState typeState,
  Mode currentMode,  // Accumulated mode through traversal
) {
  if (!isPrimitiveState(typeState)) {
    // Non-primitive: structural check only
    return;
  }
  
  // Primitive position: check mode
  final acceptedModes = getModesAt(typeState);
  final variableMode = variable.isReader ? Mode.input : Mode.output;
  
  if (!acceptedModes.contains(variableMode)) {
    reportModeError(variable, variableMode, acceptedModes);
  }
}
```

### 5.6 Correspondence with Paper

This specification corresponds to the paper as follows:

| Paper | Spec |
|-------|------|
| "moded path = path + mode at leaf" (Def 6.15) | `ModedPath(steps, mode)` |
| `_` and `_?` are distinct primitives | Different entries in `primitiveStateModes` |
| "alternating tree automata with mode annotations" (§7.4) | `primitiveStateModes: Map<DFAState, Set<Mode>>` |
| `Every ::= _ ; _?` accepts both | `{Mode.output, Mode.input}` |
| Mode combines via involution (§7.7) | `combineMode()` during traversal |

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
          Report error: "Guard inconsistent with pattern type"

      If G implies groundness for variable X:
        Mark X as recursively ground (covers all mode alternatives)

    // Step 3: Check head variable modes
    For each variable Y in head:
      expectedMode := mode at head position from declaration
      actualMode := writer→output, reader→input
      If expectedMode.complement ≠ actualMode:
        Report error: "head mode mismatch for Y"

    // Step 4: Compute clause contribution
    T_C^{α,m} := compute moded contribution

  // Step 5: Check fixpoint (for ::= semantics)
  inferred := modedTupleDistributiveClosure(union(contributions))
  If inferred ≠ S:
    Report error: "inferred moded type ≠ declared type"
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

### 6.4 Mode Coverage Check for Every Positions (::= Types)

For positions typed with `::=` union types containing primitive modes (e.g., `Every ::= _ | _?`), verify collective mode coverage:

```dart
/// Check that all mode alternatives are covered across clauses
List<ModeError> checkModeCoverage(
  List<Clause> clauses,
  ProcDecl decl,
  TypeEnvironment env,
) {
  final errors = <ModeError>[];

  for (int argIndex = 0; argIndex < decl.arity; argIndex++) {
    final argType = decl.argTypes[argIndex];

    // Find positions with ::= union types containing primitive modes
    final everyPositions = findEveryPositions(argType, env);

    for (final position in everyPositions) {
      final coveredModes = <Mode>{};

      for (final clause in clauses) {
        final termAtPosition = extractTermAtPosition(clause.head, argIndex, position);
        if (termAtPosition is VarTerm) {
          final mode = termAtPosition.isReader ? Mode.input : Mode.output;
          coveredModes.add(mode);
        }
      }

      // Check both modes are covered
      if (!coveredModes.contains(Mode.output)) {
        errors.add(ModeError(
          'No clause covers output mode (_) at Every position $position in argument $argIndex',
        ));
      }
      if (!coveredModes.contains(Mode.input)) {
        errors.add(ModeError(
          'No clause covers input mode (_?) at Every position $position in argument $argIndex',
        ));
      }
    }
  }

  return errors;
}
```

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
| `ground(X?)` | (Any) | Yes (recursively) |
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

### 7.3 Ground Guards and Mode Coverage

The `ground(X?)` guard has special significance for mode checking. When `ground(X?)` succeeds:

1. X contains no unbound variables
2. All nested positions within X are fully determined
3. No mode inversions can occur within X's structure

**Consequence:** Variables protected by `ground/1` (or other ground-implying guards) satisfy all mode coverage requirements. A clause with `ground(X?)` in its guard contributes both writer and reader coverage for all nested positions within X.

```
groundVars := variables occurring in ground-implying guards

For mode coverage at position P:
  If term at P is variable V and V ∈ groundVars:
    hasWriter := true
    hasReader := true  // Ground covers both modes
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
% Stream may not close (subtype of List)
Stream ::< List.

% Channel pairs two streams with complementary modes
Channel ::= ch(Stream?, Stream) ; ch(Stream, Stream?).

procedure create_channel(Channel, Channel).
create_channel(ch(AtoB?, BtoA), ch(BtoA?, AtoB)).
```

The `Channel` type has two alternatives capturing endpoint duality:
- `ch(Stream?, Stream)` — reads from first stream, writes to second
- `ch(Stream, Stream?)` — writes to first stream, reads from second

Since `Stream ::< List` uses subtype semantics, there is no requirement that streams close (the `[]` alternative need not be covered).

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

The type 'AnyList ::= [] ; [Any | AnyList]' has Any at the head position.
Under ::= semantics, clauses must cover BOTH mode alternatives:
  - _ (output): requires writer variable
  - _? (input): requires reader variable

Current clauses only cover: _? (input)

Solutions:
  1. Add clause with writer at head position
  2. Change type to List ::= [] ; [_ | List] (single mode, standard)
  3. Use ::< AnyList for partial coverage (future PMT feature)
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

### v1.6 (2025-12-22)
- **FIXED** Section 2.1: Removed incorrect `Any ::= _ ; _?` definition
- **FIXED** Section 4.4: Renamed to "Mode Coverage for Exact Union Types (::=)"
  - Clarified: `Every ::= _ ; _?` requires coverage, `Any ::< Every` does not
  - Renamed examples from AnyList to EveryList
  - Added "Why Standard List Has No Coverage Requirement" subsection
- **FIXED** Section 6.4: Renamed to "Mode Coverage Check for Every Positions"
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
