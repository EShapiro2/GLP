# GLP Type System Specification (v0.2)

## 1. Overview

We implement the Yardeni-Shapiro type system for GLP, following Section 2 of the Moded Types paper. This is a **structural type system** for logic programs based on:

- **Types** = tuple-distributive sets of ground terms
- **Regular types** = types specifiable by RUL programs (equivalently, tree automata)
- **Well-typing** = type is a fixpoint of the abstract consequence operator T_P^α

**No modes** in this phase — purely structural typing.

## 2. Type Declaration Syntax

Type declarations are **first-class syntactic elements**, like clauses. They can appear anywhere in a module. The type environment of a module is the union of all type definitions in that module.

### 2.1 Grammar

```
type_decl     ::= type_name '::=' type_expr '.'
type_expr     ::= type_alt (';' type_alt)*
type_alt      ::= constant
                | functor '(' type_args ')'
                | '[' ']'                        % empty list
                | '[' type_ref '|' type_ref ']'  % list cons
type_args     ::= type_ref (',' type_ref)*
type_ref      ::= type_name
type_name     ::= UppercaseIdentifier

proc_decl     ::= 'procedure' atom '(' type_refs ')' '.'
type_refs     ::= type_ref (',' type_ref)*
```

### 2.2 Built-in Types

| Type | Meaning | Guard |
|------|---------|-------|
| `Number` | Dart int/double | `number(X)` |
| `String` | Dart string | `string(X)` |
| `Any` | Herbrand universe | (none) |

### 2.3 Standard Library Types

```prolog
Constant ::= Number ; String.
```

### 2.4 Examples

```prolog
Nat ::= 0 ; s(Nat).
NatList ::= [] ; [Nat | NatList].

procedure append(NatList, NatList, NatList).

append([], Ys, Ys?).
append([X|Xs], Ys, [X?|Zs?]) :- append(Xs?, Ys?, Zs).
```

```prolog
CounterMsg ::= clear ; up ; down ; show(Number).
CounterMsgList ::= [] ; [CounterMsg | CounterMsgList].

procedure counter(CounterMsgList, Number).

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

## 3. Type Representation (Dart)

### 3.1 AST

```dart
abstract class TypeExpr {}

class TypeRef extends TypeExpr {
  final String name;  // "Nat", "Number", "Any", etc.
}

class ConstantAlt extends TypeExpr {
  final Object value;  // atom, number, or string
}

class StructAlt extends TypeExpr {
  final String functor;
  final List<TypeExpr> args;
}

class ListNilAlt extends TypeExpr {}

class ListConsAlt extends TypeExpr {
  final TypeExpr head;
  final TypeExpr tail;
}

class TypeDef {
  final String name;
  final List<TypeExpr> alternatives;  // separated by ;
}

class ProcDecl {
  final String name;
  final int arity;
  final List<TypeRef> argTypes;
}

class TypeEnvironment {
  final Map<String, TypeDef> types;
  final Map<String, ProcDecl> procedures;  // keyed by name/arity
}
```

### 3.2 Type as DFA

Following Theorem 2.4 (RUL ↔ Regular), each type compiles to a DFA:

```dart
class TypeDFA {
  final Set<String> states;
  final String startState;
  final Set<String> finalStates;
  final Map<(String, String), String> transitions;  // (state, symbol) → state

  bool accepts(Term t);
  Set<String> pathsOf(Term t);
  TypeDFA intersect(TypeDFA other);
  bool isEquivalent(TypeDFA other);
  bool isSubset(TypeDFA other);
}
```

## 4. Type Checking Algorithm

Following Section 2.11 of the Moded Types paper:

### 4.1 Input

- Program P (set of clauses)
- Type environment E (type definitions + procedure declarations)

### 4.2 Algorithm

```
errors := []

For each procedure p/n with declared type (T₁, ..., Tₙ):
  Let S = product type as DFA
  clauseContributions := []

  For each clause C = H :- B₁, ..., Bₘ defining p/n:

    // Step 1: Check ground paths in body
    For every ground path ξ in body of C:
      If ξ ∉ paths(S):
        errors.add("Clause C: ground path ξ not in type")
        mark C as useless
        continue to next clause

    // Step 2: Infer variable types
    varTypes := {}
    For each variable Y in C:
      occurrenceTypes := []
      For each occurrence of Y in body:
        Run path to occurrence through DFA → state q
        occurrenceTypes.add(L_q(DFA))  // language from q
      varTypes[Y] := intersect(occurrenceTypes)
      If varTypes[Y] is empty:
        errors.add("Clause C: variable Y has empty type")
        mark C as useless

    // Step 3: Compute clause contribution
    If C not useless:
      T_C := compute T_{C}^α(S) using varTypes
      clauseContributions.add(T_C)
    Else:
      errors.add("Warning: clause C is useless")

  // Step 4: Check fixpoint
  inferred := tupleDistributiveClosure(union(clauseContributions))
  If inferred ≠ S:
    errors.add("Procedure p/n: inferred type ≠ declared type")
    // Report specific difference

Return errors
```

### 4.3 Output

- Empty list = well-typed
- Non-empty list = all type errors and warnings

## 5. Compiler Integration

### 5.1 Analysis Phases

The compiler has multiple analysis phases that can run independently or together:

```
Source (.glp)
    ↓
[Lexer] → Tokens
    ↓
[Parser] → AST
    ↓
[Analysis Phases] ← can run any subset
    ├── [Type Checker] → type errors
    ├── [SRSW Checker] → SRSW violations
    └── [Defined Guards] → guard expansion
    ↓
[Code Generator] → Bytecode (optional)
```

### 5.2 Phase Interface

```dart
abstract class AnalysisPhase {
  String get name;
  List<AnalysisError> analyze(Program ast, AnalysisContext ctx);
}

class TypeChecker implements AnalysisPhase {
  @override String get name => 'type';
  @override List<AnalysisError> analyze(Program ast, AnalysisContext ctx);
}

class SRSWChecker implements AnalysisPhase {
  @override String get name => 'srsw';
  @override List<AnalysisError> analyze(Program ast, AnalysisContext ctx);
}

class DefinedGuardExpander implements AnalysisPhase {
  @override String get name => 'guards';
  @override List<AnalysisError> analyze(Program ast, AnalysisContext ctx);
}
```

### 5.3 Analysis Runner

```dart
class AnalysisRunner {
  final List<AnalysisPhase> phases;

  AnalysisResult run(Program ast, {bool stopOnError = false}) {
    final ctx = AnalysisContext();
    final allErrors = <AnalysisError>[];

    for (final phase in phases) {
      final errors = phase.analyze(ast, ctx);
      allErrors.addAll(errors);
      if (stopOnError && errors.isNotEmpty) break;
    }

    return AnalysisResult(allErrors, ctx);
  }
}

// Usage:
final runner = AnalysisRunner([
  TypeChecker(),
  SRSWChecker(),
]);

// Run standalone (no compilation)
final result = runner.run(ast);

// Or integrate with compiler
final compiler = GlpCompiler(analysisRunner: runner);
```

## 6. Implementation Plan

### Phase 1: Extend Parser (2 days)

- Add tokens: `::=`, `;` (in type context)
- Parse type definitions as top-level elements
- Parse procedure declarations
- Build TypeEnvironment from AST

### Phase 2: Type-to-DFA Compiler (3 days)

- Convert TypeDef to DFA (Theorem 2.4)
- Handle built-ins: Number, String, Any
- Implement DFA operations: intersection, union, equivalence
- Implement paths() function

### Phase 3: Type Checker Core (4 days)

- Path extraction from GLP terms
- Variable type inference (DFA state traversal)
- T_C^α(S) computation per clause
- Fixpoint check with detailed error reporting

### Phase 4: Analysis Framework (1 day)

- Create AnalysisPhase interface
- Wrap existing SRSW checker
- Create AnalysisRunner
- Standalone CLI mode

### Phase 5: Add Types to Programs (3 days)

- Add type declarations to book/ programs (~147 files)
- Add type declarations to tests/repl/ programs (~72 files)
- Verify all pass type checking
- Fix any programs that fail

**Total estimate: 13 days**

## 7. File Organization

```
lib/
  analysis/
    analysis_phase.dart      # Interface
    analysis_runner.dart     # Runner
    analysis_context.dart    # Shared context
    type_checker/
      type_ast.dart          # Type AST nodes
      type_parser.dart       # Parse type declarations
      type_dfa.dart          # DFA representation
      type_compiler.dart     # TypeDef → DFA
      type_checker.dart      # Main checker
    srsw_checker/
      srsw_checker.dart      # Existing, wrapped
    defined_guards/
      guard_expander.dart    # Existing, wrapped
```

## 8. Relationship to Existing PMT Work

### 8.1 PMT vs Pure Types

The existing PMT (Polymorphic Moded Types) implementation in `lib/compiler/pmt/` mixed two concerns:

1. **Structural types** (this spec) - Yardeni-Shapiro regular types
2. **Mode annotations** (separate) - reader/writer data flow

This spec implements **only structural types**. Mode checking will be added later as a separate analysis phase.

### 8.2 Migration Path

**Current PMT code** (`lib/compiler/pmt/`):
- `occurrence.dart` - Now uses syntactic `?` for SRSW checking ✓
- `checker.dart` - SRSW checker (will be wrapped as AnalysisPhase)
- `mode_table.dart` - Mode declarations (separate from types)
- `type_*.dart` - Old type checking code (will be replaced)

**New structure** (`lib/analysis/`):
- `type_checker/` - Pure structural type checking (this spec)
- `srsw_checker/` - Wraps existing SRSW checker
- `mode_checker/` - Future: validates mode consistency

### 8.3 Syntax Changes

**Old MT syntax** (being replaced):
```prolog
List := [] | [_ | List].               % Type definition
Merge := merge(List?, List?, List).    % Mode declaration (conflated with type)
```

**New syntax** (this spec):
```prolog
List ::= [] | [List | List].           % Type definition (::= not :=)
procedure merge(List, List, List).     % Type declaration
```

Mode annotations (`?`) remain in clause bodies for SRSW checking, but are not part of type declarations.

## 9. References

- Yardeni & Shapiro, "A Type System for Logic Programs", JLP 1991
- Frühwirth, Shapiro, Vardi & Yardeni, "Logic Programs as Types for Logic Programs", LICS 1991
- Section 2 of Moded Types paper (this project)
