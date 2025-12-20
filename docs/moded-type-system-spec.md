# GLP Moded Type System Specification (v1.0)

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

```
_    output mode (program produces value)
_?   input mode (environment provides value)
```

The universal type is self-dual:
```
Any ::= _ ; _?.
(Any)? = Any
```

### 2.2 Moded Type Expressions

Type definitions remain as before (using `::=` syntax):
```
Nat ::= 0 ; s(Nat).
List ::= [] ; [Any | List].
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

### 2.4 Embedded Modes in Type Definitions

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

### 2.5 Examples

```glp
% Simple moded procedure
Nat ::= 0 ; s(Nat).
procedure add(Nat?, Nat?, Nat).

add(0, Y, Y?).
add(s(X), Y, s(Z)?) :- add(X?, Y?, Z).

% Stream merge with all modes explicit
List ::= [] ; [Any | List].
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

% Request/Response server
Request ::= get(Value?) ; put(Value).
RequestStream ::= [] ; [Request | RequestStream].
procedure server(RequestStream?, State).

% After complementation at server receiving RequestStream?:
% - get(Value?) → get(Value) : server WRITES response
% - put(Value) → put(Value?) : server READS provided value
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

---

## 5. Moded Type DFA

### 5.1 Extension to TypeDFA

```dart
class ModedTypeDFA extends TypeDFA {
  /// Mode annotation for each state
  final Map<String, Mode> stateModes;

  ModedTypeDFA({
    required super.states,
    required super.startState,
    required super.finalStates,
    required super.transitions,
    required this.stateModes,
  });

  /// Get mode at a given state
  Mode getModeAt(String state) => stateModes[state] ?? Mode.output;

  /// Check if a term matches with correct modes
  bool acceptsWithModes(Term t, Map<String, Mode> varModes);

  /// Extract moded paths from this DFA
  Set<ModedPath> modedPaths();
}
```

### 5.2 Compiling Moded Types to DFA

The compilation extends the unmoded case:

1. **Parse type definition** → TypeDef with alternatives
2. **Build state machine** → states for each type name + constructors
3. **Annotate states with modes** → track mode at each position
4. **Handle mode complementation** → when following `Type?` reference

```dart
class ModedTypeCompiler {
  final TypeEnvironment env;

  ModedTypeDFA compile(TypeRef typeRef) {
    final baseDFA = compileUnmodedType(typeRef.name);
    return annotateWithModes(baseDFA, typeRef.isInput);
  }

  ModedTypeDFA annotateWithModes(TypeDFA dfa, bool isInput) {
    final stateModes = <String, Mode>{};
    // Traverse DFA, tracking mode through transitions
    // Apply complementation when entering Type? references
    ...
  }
}
```

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

    // Step 3: Check head variable modes
    For each variable Y in head:
      expectedMode := mode at head position from declaration
      actualMode := writer→output, reader→input
      If expectedMode.complement ≠ actualMode:
        Report error: "head mode mismatch for Y"

    // Step 4: Compute clause contribution
    T_C^{α,m} := compute moded contribution

  // Step 5: Check fixpoint
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

---

## 7. Error Messages

### 7.1 Mode Errors

| Situation | Message |
|-----------|---------|
| Writer at output position | `Writer variable 'X' at line 5 occurs at output position; expected input position (_?)` |
| Reader at input position | `Reader variable 'X?' at line 7 occurs at input position; expected output position (_)` |
| Mode mismatch in call | `Argument 2 of merge/3 at line 10: expected input mode, found output` |

### 7.2 Example Error Output

```
[MODE ERROR] Writer variable 'Result' at line 8, column 15 occurs at output
position in type 'Number'. In moded types, writers must occur at input
positions (_?) where the caller provides a slot for the callee to fill.

Hint: The procedure declaration is:
  procedure compute(Number?, Number).
         argument 2 ────────────┘ (output mode)

At output positions, use a reader variable (Result?) to receive the value.
```

---

## 8. Implementation Plan

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
- [ ] Add comprehensive tests

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

**Total estimate: 9 days**

---

## 9. File Organization

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

## 10. CLI Integration

The `--type-check` flag performs both structural type checking and mode checking:

```bash
# Type check with modes (default behavior)
dart bin/glpc.dart --type-check file.glp

# Strict mode: abort on any type or mode error
dart bin/glpc.dart -t -s file.glp
```

Mode checking is **not** a separate phase — it's integrated into type checking as the moded extension of the Yardeni-Shapiro algorithm.

---

## 11. Theoretical Foundation

This implementation follows the theory developed in "Moded Types for Grassroots Logic Programs" (2024), which establishes:

1. **Moded types as distributive abstraction of trace semantics** (Theorem 6.18)
2. **Partial correctness guarantee**: Well-moded-typed programs have produced/consumed assignments conforming to declared types
3. **EXPTIME-complete complexity** for moded type checking
4. **Mode complementation `(·)?`** as the uniform mechanism for producer/consumer duality

---

## 12. References

- Yardeni & Shapiro, "A Type System for Logic Programs", JLP 1991
- Frühwirth, Shapiro, Vardi & Yardeni, "Logic Programs as Types for Logic Programs", LICS 1991
- "Moded Types for Grassroots Logic Programs", 2024 (this project)
