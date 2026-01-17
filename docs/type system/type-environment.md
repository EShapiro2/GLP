# Module: type-environment

**Version**: 0.8
**Date**: 2026-01-17
**Status**: DRAFT
**Paper References**: Definition 4.1 (Typed GLP Program), Section 4.1 (Type Classification), Section 4.2 (Type Automaton - Determinism, Type Aliases)

## Dependencies

None (leaf module).

## Purpose

Stores and provides lookup for type definitions and procedure declarations in a typed GLP program. Enforces validity constraints on type definitions including determinism. Resolves type aliases during preprocessing.

## Paper Definitions

### Definition 4.1: Typed GLP Program

> A typed GLP program P = (Cs, D) has GLP clauses Cs and a GLP type D defining the type of every procedure in Cs.

The type environment represents "D" — the collection of type definitions and procedure declarations.

### Type Classification (Paper Section 4.1)

Types are classified by their mode structure:

| Classification | Definition | Example |
|----------------|------------|---------|
| **Output type** | No complementation in definition | `Stream ::= [] ; [_\|Stream]` |
| **Input type** | Complement of an output type | `Stream?` (all modes flipped) |
| **Interactive type** | Contains internal complementation | `HollowStream ::= [] ; [_?\|HollowStream]` |

**Key insight:** Interactive types like `HollowStream` have some positions that consume (`_?`) while the overall structure is produced. This enables bidirectional communication.

### Predefined Types (Paper Section 4.1)

The following types are predefined and cannot be redefined:

| Type | Complement | Description |
|------|------------|-------------|
| `_` | `_?` | Any produced/consumed term (wildcard) |
| `Integer` | `Integer?` | Any integer literal |
| `Real` | `Real?` | Any real (floating-point) literal |
| `Number` | `Number?` | Any numeric literal (Integer or Real) |
| `String` | `String?` | Any string literal |
| `Exp` | `Exp?` | Arithmetic expression evaluating to Number |

**Numeric type hierarchy:**
- `Integer` literals: no decimal point (e.g., `42`, `-1`, `0`)
- `Real` literals: with decimal point or exponent (e.g., `3.14`, `2.5e10`)
- `Number` accepts both Integer and Real literals

**Arithmetic expression type:**
- `Exp` accepts arithmetic expressions built from:
  - Numeric literals (Integer, Real)
  - Numeric variables (Number?)
  - Binary operators: `+`, `-`, `*`, `/`, `//`, `mod`
  - Unary operator: `-` (negation)
- `Exp` is used in comparison guards (`<`, `>`, `=<`, `>=`, `=:=`, `=\=`) which expect `Exp?` arguments
- At runtime, `Exp` evaluates to `Number`

### Determinism Requirement (Paper Section 4.2)

Type definitions must be **deterministic**: alternatives must be distinguishable by their top-level functor or, for primitive types, by disjoint type membership.

**Illegal overlapping definitions:**
```
Any ::= _ ; _?.           % overlapping: both accept all terms
AnyOne ::= 1 ; 1?.        % overlapping: 1 matches both alternatives  
Ambiguous ::= _ ; Integer. % overlapping: integers match both
```

### Type Aliases (Paper Section 4.2)

For documentation and readability, type aliases are permitted in source programs.

**Simple aliases** have a single alternative that is a type reference:
```
AgentId ::= Constant.       % alias for primitive type
MyStream ::= Stream.        % alias for defined type
ConsumedStream ::= Stream?. % alias for dual of defined type
```

**Union aliases** have multiple alternatives, each of which is a type reference:
```
Msg ::= NetMsg ; UserMsg.   % union of two message types
```

Union aliases are expanded by collecting all alternatives from the referenced types. For example:
```
NetMsg ::= msg(_, _, _).
UserMsg ::= sent(_, _) ; received(_, _).
Msg ::= NetMsg ; UserMsg.
```
Expands `Msg` to: `Msg ::= msg(_, _, _) ; sent(_, _) ; received(_, _).`

The referenced types must be defined types (not simple aliases, union aliases, or primitives), and the expanded alternatives must satisfy the determinism requirement.

All aliases are fully resolved during preprocessing: every occurrence of an alias name is replaced by its expansion before type automaton construction and well-typing analysis. The type checker therefore never encounters alias definitions—it operates only on resolved types.

**Circular alias chains are prohibited:**
```
A ::= B.
B ::= A.    % Error: circular alias chain
```

### Type Alternative Syntax

Type alternatives use the same term syntax as program terms. This ensures consistency between type definitions and the terms they describe.

**Syntactic forms:**

| Syntax | Meaning | TypeAlternative Class |
|--------|---------|----------------------|
| `atom` | Constant atom | `ConstantAlt` |
| `42`, `3.14` | Numeric constant | `ConstantAlt` |
| `[]` | Empty list | `ListNilAlt` |
| `[H \| T]` | List cons | `ListConsAlt` |
| `functor(A1, ..., An)` | Structure | `StructAlt` |
| `(T1, T2)` | Conjunction (shorthand for `','(T1, T2)`) | `StructAlt` with functor `,` |
| `Content \ Hole` | Difference list | `DiffListAlt` |
| `_` | Output wildcard | `PrimitiveModeAlt(isInput: false)` |
| `_?` | Input wildcard | `PrimitiveModeAlt(isInput: true)` |
| `TypeName` | Type reference (output) | `TypeRef(isInput: false)` |
| `TypeName?` | Type reference (input) | `TypeRef(isInput: true)` |

**Conjunction shorthand:** The parenthesized syntax `(T1, T2, T3)` is right-associative shorthand for `','(T1, ','(T2, T3))`. This matches the term parser behavior for conjunction in program clauses.

**Example:**
```
Pair ::= (_, _).                    % Equivalent to: Pair ::= ','(_, _).
Triple ::= (_, _, _).               % Equivalent to: Triple ::= ','(_, ','(_, _)).
FriendEntry ::= (String, Channel).  % Conjunction with named types
```

## Public Interface

### Types

#### `class TypeEnvironment`

```dart
class TypeEnvironment {
  /// All type definitions (e.g., Stream ::= [] ; [_|Stream])
  final Map<String, TypeDef> types;

  /// All procedure declarations (e.g., procedure merge(Stream?, Stream?, Stream))
  final Map<String, ProcDecl> procedures;

  /// Predefined type names (cannot be redefined)
  static final Set<String> predefinedTypes = {
    'Integer', 'Integer?',
    'Real', 'Real?', 
    'Number', 'Number?',
    'String', 'String?',
    'Exp', 'Exp?',
    '_', '_?'
  };
  
  /// Base predefined type names (without complement marker)
  static final Set<String> predefinedBaseTypes = {
    'Integer', 'Real', 'Number', 'String', 'Exp', '_'
  };
}
```

#### `class TypeDef`

```dart
class TypeDef {
  final String name;
  final List<TypeAlternative> alternatives;
  final List<String>? typeParameters;  // For parametric types like Stream(X)
  
  /// Classification based on mode structure
  TypeClassification get classification;
}

enum TypeClassification {
  output,      // No complementation in definition
  input,       // Would be complement of output (not directly defined)
  interactive  // Contains internal complementation
}
```

#### `class TypeAlternative`

```dart
abstract class TypeAlternative {
  /// The top-level functor (for determinism checking)
  String? get functor;
  
  /// Arity of this alternative
  int get arity;
}

class ConstantAlt extends TypeAlternative {
  final Object value;  // int, double, String
  String? get functor => null;  // Constants have no functor
  int get arity => 0;
}

class ListNilAlt extends TypeAlternative {
  String? get functor => '[]';
  int get arity => 0;
}

class ListConsAlt extends TypeAlternative {
  final TypeExpr headType;
  final TypeExpr tailType;
  String? get functor => '[|]';
  int get arity => 2;
}

class StructAlt extends TypeAlternative {
  final String name;
  final List<TypeExpr> argTypes;
  String? get functor => name;
  int get arity => argTypes.length;
}

class DiffListAlt extends TypeAlternative {
  final TypeExpr contentType;
  final TypeExpr holeType;
  String? get functor => '\\';
  int get arity => 2;
}
```

#### `class ProcDecl`

```dart
class ProcDecl {
  final String name;
  final int arity;
  final List<TypeExpr> argTypes;  // Each is Type or Type?
  
  String get key => '$name/$arity';
}
```

#### `class TypeExpr`

```dart
abstract class TypeExpr {
  bool get isInput;  // true for T?, false for T
  String get baseName;  // Type name without complement marker
}

class TypeRef extends TypeExpr {
  final String typeName;
  final bool isComplement;  // true for T?
  final List<TypeExpr>? typeArgs;  // For parametric types
  
  bool get isInput => isComplement;
  String get baseName => typeName;
}

class PrimitiveType extends TypeExpr {
  final String name;  // "Integer", "Real", "Number", "String", "_"
  final bool isComplement;  // true for Integer?, etc.
  
  bool get isInput => isComplement;
  String get baseName => name;
}
```

### Functions

#### `TypeDef? getType(String name)`

Returns the type definition for `name`, or `null` if not defined.

**Note:** Returns `null` for predefined types (`Integer`, `Real`, `Number`, `String`, `_` and their complements).

#### `ProcDecl? getProcedure(String name, int arity)`

Returns the procedure declaration, or `null` if not declared.

#### `bool isTypeDefined(String name)`

Returns `true` if `name` is a defined type or a predefined type.

#### `bool isPredefinedType(String name)`

Returns `true` if `name` is a predefined type (including complements).

#### `void addType(TypeDef def)`

Adds a type definition after validation. 

**Throws:**
- `PredefinedTypeError` if redefining a predefined type
- `RedefinitionError` if type already defined
- `NonDeterministicTypeError` if alternatives overlap
- `CircularAliasError` if alias chain is circular

#### `void addProcedure(ProcDecl decl)`

Adds a procedure declaration.

**Throws:**
- `RedefinitionError` if already declared
- `UndefinedTypeError` if referencing undefined type

## Algorithms

### Algorithm: Type Lookup

```
getType(name):
  // Strip complement marker for lookup
  baseName = name.endsWith('?') ? name.substring(0, name.length-1) : name
  
  if baseName in predefinedBaseTypes:
    return null  // Predefined types don't have definitions
  return types[baseName]  // Returns null if not found
```

### Algorithm: Procedure Lookup

```
getProcedure(name, arity):
  key = "$name/$arity"
  return procedures[key]  // Returns null if not found
```

### Algorithm: Add Type Definition

```
addType(def):
  // Check 1: Not a predefined type
  if def.name in predefinedBaseTypes:
    throw PredefinedTypeError("Cannot redefine predefined type: ${def.name}")
  
  // Check 2: Not already defined
  if def.name in types:
    throw RedefinitionError("Type already defined: ${def.name}")
  
  // Check 3: Deterministic (no overlapping alternatives) - only for non-aliases
  if not isTypeAlias(def) and not isDeterministic(def):
    throw NonDeterministicTypeError("Type alternatives must be distinguishable: ${def.name}")
  
  types[def.name] = def
```

### Algorithm: Type Alias Check

```
isSimpleAlias(def):
  // A simple alias has exactly one alternative that is a TypeRef or PrimitiveModeAlt
  
  if def.alternatives.length != 1:
    return false
  
  alt = def.alternatives[0]
  
  // Single TypeRef (T or T?) = simple alias
  if alt is TypeRef:
    return true
  
  // Single PrimitiveModeAlt (_ or _?) = simple alias  
  if alt is PrimitiveModeAlt:
    return true
  
  return false

isUnionAlias(def):
  // A union alias has multiple alternatives, all of which are TypeRefs (not PrimitiveModeAlt)
  
  if def.alternatives.length < 2:
    return false
  
  for alt in def.alternatives:
    if not (alt is TypeRef):
      return false
  
  return true

isTypeAlias(def):
  return isSimpleAlias(def) or isUnionAlias(def)
```

### Algorithm: Alias Resolution (Preprocessing)

Alias resolution happens before type checking, transforming all alias references to their target types.

```
resolveAliases(types, procedures):
  // Step 1: Identify simple and union aliases
  simpleAliases = {}   // name -> single TypeExpr
  unionAliases = {}    // name -> list of TypeRef
  
  for def in types:
    if isSimpleAlias(def):
      simpleAliases[def.name] = def.alternatives[0]
    else if isUnionAlias(def):
      unionAliases[def.name] = def.alternatives
  
  // Step 2: Resolve simple aliases (with cycle detection)
  visiting = {}
  resolvedSimple = {}
  
  for aliasName in simpleAliases:
    if aliasName not in resolvedSimple:
      resolveSimpleAlias(aliasName, simpleAliases, visiting, resolvedSimple)
  
  // Step 3: Expand union aliases
  //   For each union alias, collect alternatives from referenced types
  //   Referenced types must be non-alias defined types
  for (name, refs) in unionAliases:
    expandedAlts = []
    for ref in refs:
      if ref.name in simpleAliases or ref.name in unionAliases:
        throw AliasExpansionError("Union alias cannot reference another alias: ${ref.name}")
      targetDef = types[ref.name]
      if targetDef is null:
        throw UndefinedTypeError("Union alias references undefined type: ${ref.name}")
      // Collect alternatives, applying complement if ref.isInput
      for alt in targetDef.alternatives:
        expandedAlts.add(applyComplementToAlt(alt, ref.isInput))
    // Check determinism of expanded type
    if not isDeterministic(TypeDef(name, expandedAlts)):
      throw NonDeterministicTypeError("Expanded union alias has overlapping alternatives: ${name}")
    // Replace union alias definition with expanded definition
    types[name] = TypeDef(name, expandedAlts, ...)
  
  // Step 4: Replace simple alias usages in all type definitions
  for def in types:
    if def.name not in simpleAliases:  // Skip simple alias definitions
      for alt in def.alternatives:
        replaceAliasReferences(alt, resolvedSimple)
  
  // Step 5: Replace alias usages in procedure declarations
  for proc in procedures:
    for argType in proc.argTypes:
      replaceAliasReferences(argType, resolvedSimple)
  
  // Step 6: Remove simple alias definitions from types map
  //   (Union aliases are already expanded in place, so keep them)
  for aliasName in simpleAliases:
    types.remove(aliasName)

resolveSimpleAlias(name, simpleAliases, visiting, resolved):
  if name in resolved:
    return resolved[name]
  
  if name in visiting:
    throw CircularAliasError("Circular alias chain detected: ${name}")
  
  visiting.add(name)
  
  target = simpleAliases[name]
  if target is TypeRef and target.name in simpleAliases:
    // Alias to another alias - resolve transitively
    finalTarget = resolveSimpleAlias(target.name, simpleAliases, visiting, resolved)
    // Preserve complement marker: if target was T? and finalTarget is S, result is S?
    resolved[name] = applyComplement(finalTarget, target.isInput)
  else:
    resolved[name] = target
  
  visiting.remove(name)
  return resolved[name]

replaceAliasReferences(typeExpr, resolved):
  if typeExpr is TypeRef and typeExpr.name in resolved:
    // Replace with resolved target, preserving complement
    target = resolved[typeExpr.name]
    return applyComplement(target, typeExpr.isInput)
  
  // Recursively process nested type expressions
  if typeExpr has nested types (e.g., StructAlt, ListConsAlt):
    for nested in typeExpr.nestedTypes:
      replaceAliasReferences(nested, resolved)
  
  return typeExpr

applyComplementToAlt(alt, applyComplement):
  // Apply complement to all TypeRef/PrimitiveModeAlt within an alternative
  if not applyComplement:
    return alt  // No change
  
  // Recursively flip isInput on all type references in the alternative
  return transformAlt(alt, flipModes: true)
```

### Algorithm: Determinism Check

```
isDeterministic(def):
  // Group alternatives by their distinguishing feature
  functorGroups = {}
  primitiveTypes = {}
  
  for alt in def.alternatives:
    match alt:
      ConstantAlt(value):
        // Constants are distinguishable by exact value
        key = "const:${value}"
        if key in functorGroups:
          return false
        functorGroups[key] = alt
      
      ListNilAlt:
        if '[]' in functorGroups:
          return false
        functorGroups['[]'] = alt
      
      ListConsAlt:
        if '[|]' in functorGroups:
          return false
        functorGroups['[|]'] = alt
      
      StructAlt(name, args):
        key = "$name/${args.length}"
        if key in functorGroups:
          return false
        functorGroups[key] = alt
      
      DiffListAlt:
        if '\\' in functorGroups:
          return false
        functorGroups['\\'] = alt
      
      // Primitive type alternatives need special handling
      PrimitiveAlt(type):
        if type in primitiveTypes:
          return false
        // Check for overlap with existing primitive types
        if hasOverlap(type, primitiveTypes):
          return false
        primitiveTypes[type] = alt
  
  // Check for overlap between _ and specific types
  if '_' in primitiveTypes or '_?' in primitiveTypes:
    if primitiveTypes.length > 1:
      return false  // _ overlaps with everything
  
  // Check Number vs Integer/Real overlap
  if 'Number' in primitiveTypes:
    if 'Integer' in primitiveTypes or 'Real' in primitiveTypes:
      return false
  
  return true
```

### Algorithm: Add Procedure Declaration

```
addProcedure(decl):
  key = "$decl.name/$decl.arity"
  if key in procedures:
    throw RedefinitionError("Procedure already declared: $key")

  // Validate all referenced types exist
  for argType in decl.argTypes:
    if not isTypeDefined(argType.baseName):
      throw UndefinedTypeError("Undefined type: ${argType.baseName}")

  procedures[key] = decl
```

### Algorithm: Type Classification

```
classifyType(def):
  hasComplement = false
  
  for alt in def.alternatives:
    if containsComplement(alt):
      hasComplement = true
      break
  
  if hasComplement:
    return TypeClassification.interactive
  return TypeClassification.output

containsComplement(alt):
  // Check if any type expression in the alternative uses complement
  for typeExpr in getTypeExprs(alt):
    if typeExpr.isInput:
      return true
    if typeExpr is TypeRef and typeExpr.typeArgs != null:
      for arg in typeExpr.typeArgs:
        if containsComplement(arg):
          return true
  return false
```

## Construction

A `TypeEnvironment` is constructed by parsing type declarations from source:

```
Stream ::= [] ; [_|Stream].
HollowIntegers ::= [] ; [Integer?|HollowIntegers].
procedure merge(Stream?, Stream?, Stream).
procedure consumer(HollowIntegers).
```

Produces:
```dart
TypeEnvironment(
  types: {
    'Stream': TypeDef('Stream', [
      ListNilAlt(),
      ListConsAlt(PrimitiveType('_', false), TypeRef('Stream', false))
    ], classification: output),
    'HollowIntegers': TypeDef('HollowIntegers', [
      ListNilAlt(),
      ListConsAlt(PrimitiveType('Integer', true), TypeRef('HollowIntegers', false))
    ], classification: interactive)
  },
  procedures: {
    'merge/3': ProcDecl('merge', 3, [
      TypeRef('Stream', isComplement: true),
      TypeRef('Stream', isComplement: true),
      TypeRef('Stream', isComplement: false)
    ]),
    'consumer/1': ProcDecl('consumer', 1, [
      TypeRef('HollowIntegers', isComplement: false)
    ])
  }
)
```

## Examples

### Example: Valid Environment with All Type Categories

```
% Output types
Stream ::= [] ; [_|Stream].
NatStream ::= [] ; [Integer|NatStream].

% Interactive types  
HollowStream ::= [] ; [_?|HollowStream].
CounterCall ::= add ; clear ; read(Integer?).

% Procedures
procedure merge(Stream?, Stream?, Stream).
procedure sum(NatStream?, Integer).
procedure monitor(Stream(CounterCall)?).
procedure consumer(HollowStream).
```

### Example: INVALID — Redefine Predefined Type

```
Integer ::= 0 ; succ(Integer).
```

**Error:** `PredefinedTypeError("Cannot redefine predefined type: Integer")`

### Example: VALID — Type Aliases

```
AgentId ::= Constant.
MyStream ::= Stream.
ConsumedStream ::= Stream?.
```

These are resolved during preprocessing:
- `AgentId` → `Constant`
- `MyStream` → `Stream`  
- `ConsumedStream` → `Stream?`

After resolution, the type checker operates on the target types.

### Example: VALID — Transitive Alias Chain

```
Id ::= Constant.
AgentId ::= Id.
UserId ::= AgentId.
```

Resolved: `UserId` → `AgentId` → `Id` → `Constant`

### Example: VALID — Union Alias

```
NetMsg ::= msg(Constant, Constant, _).
UserMsg ::= sent(Constant, _) ; received(Constant, _).
Msg ::= NetMsg ; UserMsg.
```

`Msg` is expanded to: `Msg ::= msg(Constant, Constant, _) ; sent(Constant, _) ; received(Constant, _).`

After expansion, the type checker operates on the expanded definition.

### Example: INVALID — Union Alias with Overlapping Alternatives

```
TypeA ::= foo(Integer).
TypeB ::= foo(String).
BadUnion ::= TypeA ; TypeB.
```

**Error:** `NonDeterministicTypeError("Expanded union alias has overlapping alternatives: BadUnion")`

(Both types have `foo/1` alternative, which violates determinism)

### Example: INVALID — Union Alias References Another Alias

```
BaseType ::= x.
AliasToBase ::= BaseType.
BadUnion ::= AliasToBase ; OtherType.
```

**Error:** `AliasExpansionError("Union alias cannot reference another alias: AliasToBase")`

### Example: INVALID — Circular Alias Chain

```
A ::= B.
B ::= A.
```

**Error:** `CircularAliasError("Circular alias chain detected: A")`

### Example: INVALID — Non-Deterministic Type

```
Any ::= _ ; _?.
```

**Error:** `NonDeterministicTypeError("Type alternatives must be distinguishable: Any")`

### Example: INVALID — Overlapping Primitives

```
Ambiguous ::= _ ; Integer.
```

**Error:** `NonDeterministicTypeError("Type alternatives must be distinguishable: Ambiguous")`

### Example: INVALID — Duplicate Functor

```
BadTree ::= leaf(Integer) ; leaf(String).
```

**Error:** `NonDeterministicTypeError("Type alternatives must be distinguishable: BadTree")`

### Example: VALID — Different Arities

```
Tree ::= leaf ; node(Tree, Tree).
```

This is valid: `leaf` (arity 0) is distinguishable from `node` (arity 2).

### Example: VALID — Disjoint Primitive Types

```
Constant ::= Integer ; String.
```

This is valid: integers and strings are syntactically disjoint.

### Example: INVALID — Number Overlaps Integer

```
BadNumeric ::= Number ; Integer.
```

**Error:** `NonDeterministicTypeError("Type alternatives must be distinguishable: BadNumeric")`

(Number includes Integer, so they overlap)

## Error Conditions

| Condition | Exception |
|-----------|-----------|
| Redefining predefined type | `PredefinedTypeError` |
| Redefining existing type | `RedefinitionError` |
| Circular alias chain | `CircularAliasError` |
| Union alias references another alias | `AliasExpansionError` |
| Type alternatives overlap | `NonDeterministicTypeError` |
| Expanded union alias alternatives overlap | `NonDeterministicTypeError` |
| Redeclaring procedure | `RedefinitionError` |
| Reference to undefined type | `UndefinedTypeError` |

## Changes from v0.7

- Added union alias support (e.g., `Msg ::= NetMsg ; UserMsg.`)
- Union aliases are expanded by collecting alternatives from referenced types
- Added `isSimpleAlias()` and `isUnionAlias()` helper functions
- Added `AliasExpansionError` for union alias validation failures
- Updated alias resolution algorithm for two-phase processing
- Added examples for valid and invalid union aliases

## Changes from v0.6

- Changed type aliases from prohibited to permitted (resolved at preprocessing)
- Added alias resolution algorithm
- Replaced `TypeAliasError` with `CircularAliasError` (only circular chains are errors)
- Updated examples to show valid aliases and circular chain error

## Changes from v0.5

- Added "Type Alternative Syntax" section clarifying that type definitions use the same term syntax as program terms
- Documented conjunction shorthand: `(T1, T2)` is `','(T1, T2)`

## Changes from v0.4

- Added `Real`, `Number` to predefined types
- Added determinism requirement (new validation)  
- Added `TypeClassification` enum
- Added `TypeAlternative` class hierarchy
- Updated algorithms with validation checks
- Added examples for new error conditions

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.8 | 2026-01-17 | Add union alias support; expand by collecting alternatives from referenced types |
| 0.1 | 2025-01-07 | Initial draft |
| 0.2 | 2025-01-07 | Add Dependencies section |
| 0.3 | 2025-01-07 | Add algorithms, positive and negative examples |
| 0.4 | 2025-01-09 | Add Type Classification section |
| 0.5 | 2025-01-12 | Add Real/Number types; type alias prohibition; determinism requirement |
| 0.6 | 2025-01-14 | Add Type Alternative Syntax section; document compound term shorthand |
| 0.7 | 2025-01-17 | Allow type aliases (resolved at preprocessing); add alias resolution algorithm |
