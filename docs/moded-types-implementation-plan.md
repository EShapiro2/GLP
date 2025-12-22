# Moded Type System Implementation Plan

**Version:** 2.0
**Date:** 2025-12-22
**Status:** Ready for Implementation

---

## 1. Overview

This document describes the implementation plan for the complete moded type system for GLP, extending the Yardeni-Shapiro type checking algorithm with mode tracking.

### 1.1 Goals

1. **Full YS Fixpoint Checking**: Verify T_P^α(S) = S (clauses collectively cover declared type)
2. **Mode Coverage**: Verify clauses cover required mode alternatives under `::=` semantics
3. **Guard Integration**: Extract type constraints from guards; handle ground guards
4. **Predefined Types**: Every, Any, List, Stream, DiffList, Channel with operations

### 1.2 Current State

| Component | Status | Notes |
|-----------|--------|-------|
| Type Parser | ✓ Complete | Parses `::=`, `::< `, `?` suffix |
| Type Compiler | ✓ Complete | Compiles types to DFAs |
| Type DFA | Partial | Has `intersect`; missing `union`, `complement`, `isEquivalent` |
| Ground Path Checking | ✓ Complete | Checks constructors match type |
| Variable Type Inference | ✓ Complete | Infers types from head patterns |
| Mode Checking (per-clause) | ✓ Complete | Checks reader/writer at positions |
| Mode Coverage | Partial | Bug with nested positions being fixed |
| Guard Type Constraints | ✗ Missing | TODO comment in code |
| Clause Contribution | ✗ Missing | Does not compute T_{C}^α(S) |
| Fixpoint Check | ✗ Missing | Comment admits "simplified check" |
| Predefined Types | ✗ Missing | Not yet implemented |

---

## 2. Predefined Types and Operations

The following definitions are prepended to every module. Redefinition is an error.

### 2.1 Type Definitions

```prolog
% Primitives (built-in, not user-definable)
Number.
String.

% Universal types
Every ::= _ ; _?.      % exact: requires both mode alternatives covered
Any ::< Every.         % subtype: no coverage requirement

% Collections
List ::= [Any | List] ; [].
Stream ::< List.               % may remain open
DiffList ::= List \ List?.     % difference list with hole

% Communication
Channel ::= ch(Stream?, Stream).
```

### 2.2 Predefined Procedures

```prolog
% Difference list operations
procedure dl_append(DiffList?, DiffList?, DiffList).
procedure dl_to_list(DiffList?, List).

dl_append(A\B?, B\C?, A?\C).
dl_to_list(L\[], L?).

% Channel operations
procedure new_channel(Channel, Channel).
procedure send(Any, Channel?, Channel).
procedure receive(Any, Channel?, Channel).

new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).
send(X, ch(In, [X?|Out?]), ch(In?, Out)).
receive(X?, ch([X|In], Out?), ch(In?, Out)).
```

### 2.3 Key Properties

**Self-Duality of Every and Any:**

Since `Every ::= _ ; _?` contains both modes:
```
(Every)? = (_)? ; (_?)? = _? ; _ = Every
```

Self-duality means `Any` and `Any?` are equivalent—mode annotations on `Any` positions are vacuous.

**Every vs Any:**

| Type | Definition | Coverage Requirement |
|------|------------|---------------------|
| Every | `::= _ ; _?` | Must cover both modes |
| Any | `::< Every` | No requirement (subtype) |

---

## 3. Implementation Phases

### Phase 1: DFA Operations
**Effort:** 2-3 days
**Dependencies:** None
**Blocking:** Phases 4, 5

#### 3.1.1 Description

Extend `lib/analysis/type_checker/type_dfa.dart` with operations required for fixpoint checking.

#### 3.1.2 New Methods

```dart
class TypeDFA {
  // === Existing ===
  bool acceptsPath(TermPath path);
  DFAState? stateAfterPath(TermPath path);
  TypeDFA intersect(TypeDFA other);

  // === New: Required for fixpoint checking ===

  /// Union: accepts strings accepted by either DFA
  /// Uses NFA union + subset construction for determinization
  TypeDFA union(TypeDFA other);

  /// Complement: accepts strings this DFA rejects
  /// Requires complete DFA (sink state for missing transitions)
  TypeDFA complement();

  /// Complete DFA by adding sink state for missing transitions
  TypeDFA complete();

  /// Check if L(this) ⊆ L(other)
  /// Implementation: L(this) ∩ L(complement(other)) = ∅
  bool isSubsetOf(TypeDFA other);

  /// Check if L(this) = L(other)
  /// Implementation: isSubsetOf in both directions, or minimize + isomorphism
  bool isEquivalent(TypeDFA other);

  /// Minimize DFA using Hopcroft's algorithm
  TypeDFA minimize();

  /// Check if language is empty (no accepting paths)
  bool get isEmpty;

  /// DFA accepting exactly one constant
  static TypeDFA singleton(String constant);

  /// DFA accepting empty language
  static TypeDFA empty();
}
```

#### 3.1.3 Tests

Create `test/analysis/type_checker/type_dfa_operations_test.dart`:

- Union of disjoint types (Nat ∪ Bool)
- Union of overlapping types
- Complement of finite type
- Complement of recursive type
- isSubsetOf: Nat ⊆ Any
- isSubsetOf: List ⊄ Nat
- isEquivalent: same type defined differently
- isEmpty: empty vs non-empty
- singleton: accepts only given constant

---

### Phase 2: Predefined Types Prelude
**Effort:** 1-2 days
**Dependencies:** None
**Blocking:** Phase 7 (tests need predefined types)

#### 3.2.1 Description

Create prelude containing predefined type definitions and procedures. Prepend to every module before parsing.

#### 3.2.2 Implementation

Create `lib/analysis/type_checker/prelude.dart`:

```dart
/// Predefined type and procedure definitions
/// Prepended to every module before parsing
const String typePrelude = r'''
% Universal types
Every ::= _ ; _?.
Any ::< Every.

% Collections
List ::= [Any | List] ; [].
Stream ::< List.
DiffList ::= List \ List?.

% Communication
Channel ::= ch(Stream?, Stream).

% Difference list operations
procedure dl_append(DiffList?, DiffList?, DiffList).
procedure dl_to_list(DiffList?, List).

dl_append(A\B?, B\C?, A?\C).
dl_to_list(L\[], L?).

% Channel operations
procedure new_channel(Channel, Channel).
procedure send(Any, Channel?, Channel).
procedure receive(Any, Channel?, Channel).

new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).
send(X, ch(In, [X?|Out?]), ch(In?, Out)).
receive(X?, ch([X|In], Out?), ch(In?, Out)).
''';

/// Names of predefined types (cannot be redefined)
const Set<String> predefinedTypeNames = {
  'Number', 'String', 'Every', 'Any', 'List', 'Stream', 'DiffList', 'Channel',
};

/// Names of predefined procedures (cannot be redefined)
const Set<String> predefinedProcedureNames = {
  'dl_append', 'dl_to_list', 'new_channel', 'send', 'receive',
};
```

Update `lib/analysis/type_checker/type_parser.dart`:

```dart
TypeEnvironment parseTypes(String source) {
  // Prepend prelude
  final fullSource = '$typePrelude\n$source';

  // Parse combined source
  final env = _parseTypeDeclarations(fullSource);

  // Verify no redefinition of predefined types
  // (error if user source redefines Any, List, etc.)

  return env;
}
```

#### 3.2.3 Parser Verification

Verify parser correctly handles:
- `\` operator in `DiffList ::= List \ List?`
- Nested type refs in `ch(Stream?, Stream)`
- `[Any | List]` syntax (already working)

#### 3.2.4 Tests

Create `test/analysis/type_checker/prelude_test.dart`:

- Predefined types available without declaration
- Redefinition of predefined type is error
- Predefined procedures available as defined guards
- dl_append is well-moded
- Channel operations are well-moded

---

### Phase 3: Guard Type Checking
**Effort:** 1-2 days
**Dependencies:** None
**Blocking:** Phase 7

#### 3.3.1 Description

Extract type constraints from guards and integrate with variable type inference.

#### 3.3.2 Implementation

Create `lib/analysis/type_checker/guard_types.dart`:

```dart
/// Signature of a built-in guard
class GuardSignature {
  final List<TypeRef> argTypes;
  final bool impliesGround;
  final bool recursivelyGround;

  const GuardSignature({
    required this.argTypes,
    this.impliesGround = false,
    this.recursivelyGround = false,
  });
}

/// Registry of built-in guard signatures
class GuardTypeRegistry {
  static const Map<String, GuardSignature> signatures = {
    // Type guards
    'number': GuardSignature(
      argTypes: [TypeRef('Number')],
      impliesGround: true,
    ),
    'integer': GuardSignature(
      argTypes: [TypeRef('Number')],
      impliesGround: true,
    ),
    'string': GuardSignature(
      argTypes: [TypeRef('String')],
      impliesGround: true,
    ),
    'ground': GuardSignature(
      argTypes: [TypeRef('Any')],
      impliesGround: true,
      recursivelyGround: true,
    ),
    'known': GuardSignature(
      argTypes: [TypeRef('Any')],
      impliesGround: false,
    ),
    'unknown': GuardSignature(
      argTypes: [TypeRef('Any')],
      impliesGround: false,
    ),

    // Arithmetic comparisons
    '<': GuardSignature(
      argTypes: [TypeRef('Number'), TypeRef('Number')],
      impliesGround: true,
    ),
    '>': GuardSignature(
      argTypes: [TypeRef('Number'), TypeRef('Number')],
      impliesGround: true,
    ),
    '=<': GuardSignature(
      argTypes: [TypeRef('Number'), TypeRef('Number')],
      impliesGround: true,
    ),
    '>=': GuardSignature(
      argTypes: [TypeRef('Number'), TypeRef('Number')],
      impliesGround: true,
    ),
    '=:=': GuardSignature(
      argTypes: [TypeRef('Number'), TypeRef('Number')],
      impliesGround: true,
    ),
    '=\\=': GuardSignature(
      argTypes: [TypeRef('Number'), TypeRef('Number')],
      impliesGround: true,
    ),

    // Equality
    '=?=': GuardSignature(
      argTypes: [TypeRef('Any'), TypeRef('Any')],
      impliesGround: true,
    ),
  };

  /// Get signature for guard, or null if not a built-in
  static GuardSignature? getSignature(String functor) => signatures[functor];
}

/// Extract type constraints from clause guards
Map<String, TypeDFA> extractGuardConstraints(
  List<Guard>? guards,
  TypeEnvironment typeEnv,
  TypeCompiler compiler,
) {
  final constraints = <String, TypeDFA>{};
  if (guards == null) return constraints;

  for (final guard in guards) {
    // Check built-in guards
    final signature = GuardTypeRegistry.getSignature(guard.functor);
    if (signature != null) {
      for (int i = 0; i < guard.args.length && i < signature.argTypes.length; i++) {
        final arg = guard.args[i];
        if (arg is VarTerm) {
          final typeDFA = compiler.compile(signature.argTypes[i].name);
          final varName = arg.name;

          if (constraints.containsKey(varName)) {
            constraints[varName] = constraints[varName]!.intersect(typeDFA);
          } else {
            constraints[varName] = typeDFA;
          }
        }
      }
      continue;
    }

    // Check defined guards (user procedures used as guards)
    final procDecl = typeEnv.getProcedure(guard.functor, guard.args.length);
    if (procDecl != null) {
      for (int i = 0; i < guard.args.length && i < procDecl.argTypes.length; i++) {
        final arg = guard.args[i];
        if (arg is VarTerm) {
          final typeDFA = compiler.compile(procDecl.argTypes[i].name);
          final varName = arg.name;

          if (constraints.containsKey(varName)) {
            constraints[varName] = constraints[varName]!.intersect(typeDFA);
          } else {
            constraints[varName] = typeDFA;
          }
        }
      }
    }
  }

  return constraints;
}

/// Get variables that are recursively ground due to guards
Set<String> getRecursivelyGroundVars(List<Guard>? guards) {
  if (guards == null) return {};

  final result = <String>{};

  for (final guard in guards) {
    final signature = GuardTypeRegistry.getSignature(guard.functor);
    if (signature == null) continue;

    if (signature.recursivelyGround || signature.impliesGround) {
      for (final arg in guard.args) {
        if (arg is VarTerm) {
          result.add(arg.name);
        }
      }
    }
  }

  return result;
}
```

#### 3.3.3 Integration

Update `type_checker.dart` `_checkClause`:

```dart
// After variable type inference from head patterns...

// Apply guard constraints
if (clause.guards != null) {
  final guardConstraints = extractGuardConstraints(
    clause.guards, typeEnv, compiler);

  for (final entry in guardConstraints.entries) {
    final varName = entry.key;
    final guardType = entry.value;

    if (varTypes.containsKey(varName)) {
      final intersected = varTypes[varName]!.intersect(guardType);
      if (intersected.isEmpty) {
        errors.add(TypeError(
          'Guard type inconsistent with pattern type for variable $varName',
          clause.line, clause.column,
        ));
      }
      varTypes[varName] = intersected;
    } else {
      varTypes[varName] = guardType;
    }
  }
}
```

Update `mode_checker.dart` `_checkModeCoverage`:

```dart
// Get recursively ground variables from guards
final groundVars = getRecursivelyGroundVars(clause.guards);

// When checking coverage, ground-protected variables cover all modes
if (termAtPosition is VarTerm && groundVars.contains(termAtPosition.name)) {
  hasWriter = true;
  hasReader = true;  // Ground covers both modes
}
```

#### 3.3.4 Tests

Create `test/analysis/type_checker/guard_types_test.dart`:

- number(X?) constrains X to Number
- string(X?) constrains X to String
- Guard inconsistent with head type → error
- ground(X?) allows multiple readers
- ground(X?) covers all mode alternatives
- Defined guard constrains type
- known(X?) does NOT imply ground

---

### Phase 4: Clause Contribution Computation
**Effort:** 2 days
**Dependencies:** Phase 1 (DFA operations)
**Blocking:** Phase 5

#### 3.4.1 Description

Compute T_{C}^α(S) for each clause—the DFA of all ground head instances the clause can produce given inferred variable types.

#### 3.4.2 Implementation

Create `lib/analysis/type_checker/clause_contribution.dart`:

```dart
/// Computes clause contributions for fixpoint checking
class ClauseContributionComputer {
  final TypeEnvironment typeEnv;
  final TypeCompiler compiler;

  ClauseContributionComputer(this.typeEnv, this.compiler);

  /// Compute DFA for all ground terms matching a pattern
  /// with variables replaced by their inferred types
  TypeDFA computeArgContribution(
    Term pattern,
    Map<String, TypeDFA> varTypes,
  ) {
    if (pattern is VarTerm) {
      return varTypes[pattern.name] ?? TypeDFA.empty();
    }

    if (pattern is ConstTerm) {
      return TypeDFA.singleton(pattern.value.toString());
    }

    if (pattern is StructTerm) {
      final argDFAs = pattern.args
          .map((arg) => computeArgContribution(arg, varTypes))
          .toList();
      return _buildStructDFA(pattern.functor, pattern.arity, argDFAs);
    }

    if (pattern is ListTerm) {
      if (pattern.isNil) {
        return TypeDFA.singleton('[]');
      }
      final headDFA = computeArgContribution(pattern.head!, varTypes);
      final tailDFA = computeArgContribution(pattern.tail!, varTypes);
      return _buildListConsDFA(headDFA, tailDFA);
    }

    return TypeDFA.empty();
  }

  /// Build DFA accepting f(v1,...,vn) where vi ∈ L(argDFAs[i])
  TypeDFA _buildStructDFA(String functor, int arity, List<TypeDFA> argDFAs) {
    // Create DFA with:
    // - Start state
    // - Transition on functor/arity to intermediate state
    // - For each argument position, transitions from argDFAs
    // Implementation depends on DFA representation
    throw UnimplementedError('TODO: implement struct DFA construction');
  }

  /// Build DFA accepting [h|t] where h ∈ L(head) and t ∈ L(tail)
  TypeDFA _buildListConsDFA(TypeDFA head, TypeDFA tail) {
    // Similar to struct but for list cons
    throw UnimplementedError('TODO: implement list cons DFA construction');
  }
}
```

#### 3.4.3 Tests

Create `test/analysis/type_checker/clause_contribution_test.dart`:

- Constant pattern → singleton DFA
- Variable pattern → variable's inferred type
- Struct pattern → product DFA
- List pattern → cons DFA
- Nested patterns

---

### Phase 5: Fixpoint Check
**Effort:** 1-2 days
**Dependencies:** Phase 1 (DFA ops), Phase 4 (contribution)
**Blocking:** Phase 7

#### 3.5.1 Description

Check that T_P^α(S) = S: the tuple-distributive closure of clause contributions equals the declared type.

#### 3.5.2 Implementation

Update `type_checker.dart` `_checkProcedure`:

```dart
TypeCheckResult _checkProcedure(ProcDecl decl, List<ast.Clause> clauses) {
  final errors = <TypeError>[];
  final warnings = <TypeWarning>[];

  // Compile declared argument types
  final declaredDFAs = <TypeDFA>[];
  for (final argType in decl.argTypes) {
    declaredDFAs.add(compiler.compile(argType.name));
  }

  // Collect contributions from non-useless clauses
  final contributionComputer = ClauseContributionComputer(typeEnv, compiler);
  final perClauseContributions = <List<TypeDFA>>[];

  for (final clause in clauses) {
    // ... existing ground path checking ...
    // ... existing variable type inference ...
    // ... existing guard constraint application ...

    if (clauseIsUseless) {
      warnings.add(TypeWarning('Clause is useless', clause.line, clause.column));
      continue;
    }

    // Compute clause contribution
    final clauseContrib = <TypeDFA>[];
    for (int i = 0; i < decl.arity; i++) {
      clauseContrib.add(contributionComputer.computeArgContribution(
        clause.head.args[i],
        varTypes,
      ));
    }
    perClauseContributions.add(clauseContrib);
  }

  // Tuple-distributive closure: union of contributions per position
  for (int i = 0; i < decl.arity; i++) {
    var inferredDFA = TypeDFA.empty();
    for (final contrib in perClauseContributions) {
      inferredDFA = inferredDFA.union(contrib[i]);
    }

    final declaredDFA = declaredDFAs[i];

    // Check fixpoint: inferred should equal declared
    if (!inferredDFA.isEquivalent(declaredDFA)) {
      if (inferredDFA.isSubsetOf(declaredDFA)) {
        // Inferred ⊂ Declared: incomplete definition
        errors.add(TypeError(
          'Procedure ${decl.name}/${decl.arity} argument ${i + 1}: '
          'clauses do not cover full declared type (incomplete definition)',
          decl.line, decl.column,
        ));
      } else if (declaredDFA.isSubsetOf(inferredDFA)) {
        // Declared ⊂ Inferred: over-broad definition
        errors.add(TypeError(
          'Procedure ${decl.name}/${decl.arity} argument ${i + 1}: '
          'clauses produce values outside declared type',
          decl.line, decl.column,
        ));
      } else {
        // Neither subset: both incomplete and over-broad
        errors.add(TypeError(
          'Procedure ${decl.name}/${decl.arity} argument ${i + 1}: '
          'inferred type does not match declared type',
          decl.line, decl.column,
        ));
      }
    }
  }

  // Mode checking
  final modeErrors = modeChecker.checkProcedure(decl.name, decl.arity, clauses);
  for (final modeError in modeErrors) {
    errors.add(TypeError(modeError.message, modeError.line, modeError.column));
  }

  return TypeCheckResult(errors, warnings);
}
```

#### 3.5.3 Tests

Create `test/analysis/type_checker/fixpoint_check_test.dart`:

**Positive (should pass):**
- Complete Nat definition (both 0 and s(N) cases)
- Complete List definition (both [] and [_|_] cases)
- Complete binary tree definition

**Negative - Incomplete (should fail):**
- Missing base case (no 0 clause for Nat)
- Missing recursive case (no s(N) clause for Nat)
- Missing middle alternative (red/blue but no green for Color)

**Negative - Over-broad (should fail):**
- Clause produces value outside type (foo in Nat)
- Wrong constructor (p(N) instead of s(N))

---

### Phase 6: Nested Mode Coverage Fix
**Effort:** 0.5 days
**Dependencies:** None (in progress)
**Blocking:** Phase 7

#### 3.6.1 Description

Fix bug where mode coverage only checks top-level arguments, not nested positions.

#### 3.6.2 Implementation

Already provided to Claude Code. Key changes to `mode_checker.dart`:

- `_findPrimitiveTypePositions`: recurse into type definitions to find nested Every/Any
- Track `::< ` entry to skip coverage requirements for subtypes
- `_getTermAtPath`: navigate into clause head patterns following paths

#### 3.6.3 Parser Fix

Support `[_ | List]` syntax in type definitions (primitive mode as list head).

---

### Phase 7: Comprehensive Tests
**Effort:** 2 days
**Dependencies:** All previous phases
**Blocking:** None

#### 3.7.1 Test Files

| File | Coverage |
|------|----------|
| `type_dfa_operations_test.dart` | DFA union, complement, equivalence |
| `prelude_test.dart` | Predefined types and procedures |
| `guard_types_test.dart` | Guard constraints, ground guards |
| `clause_contribution_test.dart` | T_{C}^α(S) computation |
| `fixpoint_check_test.dart` | Complete, incomplete, over-broad |
| `predefined_types_test.dart` | List, Stream, DiffList, Channel |
| `predefined_operations_test.dart` | dl_append, send, receive |
| `primitive_mode_coverage_test.dart` | _, _?, Every coverage |
| `self_duality_test.dart` | Every/Any equivalence |
| `nested_mode_coverage_test.dart` | Nested Any positions |

#### 3.7.2 Erroneous Pass Markers

Mark tests that currently pass but should fail after fixes:

```dart
test('ERRONEOUS: Should fail after fixpoint check implemented', () {
  // Current: passes (no fixpoint check)
  // Expected: fails (incomplete definition)
});
```

---

## 4. Dependency Graph

```
Phase 1 (DFA Ops)
    │
    ├──────────────────┐
    ▼                  ▼
Phase 4 (Contribution)
    │
    ▼
Phase 5 (Fixpoint) ────────────────────┐
                                       │
Phase 2 (Prelude) ─────────────────────┤
                                       │
Phase 3 (Guards) ──────────────────────┤
                                       │
Phase 6 (Nested Fix) ──────────────────┤
                                       ▼
                               Phase 7 (Tests)
```

**Critical Path:** Phase 1 → Phase 4 → Phase 5 → Phase 7

**Parallelizable:** Phases 1, 2, 3, 6 can proceed simultaneously.

---

## 5. Effort Summary

| Phase | Description | Days |
|-------|-------------|------|
| 1 | DFA Operations | 2-3 |
| 2 | Predefined Types Prelude | 1-2 |
| 3 | Guard Type Checking | 1-2 |
| 4 | Clause Contribution | 2 |
| 5 | Fixpoint Check | 1-2 |
| 6 | Nested Mode Coverage Fix | 0.5 |
| 7 | Comprehensive Tests | 2 |
| **Total** | | **9-13 days** |

---

## 6. Success Criteria

### 6.1 Functional Requirements

1. All predefined types parse correctly
2. Predefined procedures usable as defined guards
3. `::=` types require full coverage; `::< ` types do not
4. Guards constrain variable types
5. `ground(X?)` satisfies all mode alternatives
6. Fixpoint check detects incomplete definitions
7. Fixpoint check detects over-broad definitions
8. Nested mode coverage correctly checked

### 6.2 Test Requirements

1. All existing tests continue to pass
2. New tests cover all phases
3. "Erroneous pass" tests fail after fixes
4. Book programs (82%) still pass

### 6.3 Documentation Requirements

1. Spec document updated with predefined types
2. Paper updated with examples
3. Implementation plan kept current

---

## 7. References

- Yardeni & Shapiro, "A Type System for Logic Programs", JLP 1991
- Frühwirth, Shapiro, Vardi & Yardeni, "Logic Programs as Types", LICS 1991
- GLP Moded Type System Specification (docs/moded-type-system-spec.md)

---
