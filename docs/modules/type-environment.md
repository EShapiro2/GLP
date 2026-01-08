# Module: type-environment

**Version**: 0.4
**Date**: 2025-01-09
**Status**: DRAFT
**Paper References**: Definition 4.1 (page 5)

## Dependencies

None (leaf module).

## Purpose

Stores and provides lookup for type definitions and procedure declarations in a typed GLP program.

## Paper Definition

### Definition 4.1: Typed GLP Program (page 5)

> A typed GLP program P = (Cs, D) has GLP clauses Cs and a GLP type D defining the type of every procedure in Cs.

The type environment represents "D" — the collection of type definitions and procedure declarations.

### Type Classification (Paper lines 9-17)

Types are classified by their mode structure:

| Classification | Definition | Example |
|----------------|------------|---------|
| **Output type** | No complementation in definition | `Stream ::= [] ; [_\|Stream]` |
| **Input type** | Complement of an output type | `Stream?` (all modes flipped) |
| **Interactive type** | Contains internal complementation | `HollowStream ::= [] ; [_?\|HollowStream]` |

**Key insight:** Interactive types like `HollowStream` have some positions that consume (`_?`) while the overall structure is produced. This enables bidirectional communication patterns.

## Public Interface

### Types

#### `class TypeEnvironment`

```dart
class TypeEnvironment {
  /// All type definitions (e.g., Stream ::= [] ; [_|Stream])
  final Map<String, TypeDef> types;

  /// All procedure declarations (e.g., procedure merge(Stream?, Stream?, Stream))
  final Map<String, ProcDecl> procedures;

  /// Predefined types (Integer, String, _, _?)
  static final Set<String> predefinedTypes = {'Integer', 'String', '_', '_?'};
}
```

#### `class TypeDef`

```dart
class TypeDef {
  final String name;
  final List<TypeAlternative> alternatives;
  final List<String>? typeParameters;  // For parametric types like Stream(X)
}
```

#### `class ProcDecl`

```dart
class ProcDecl {
  final String name;
  final int arity;
  final List<TypeExpr> argTypes;  // Each is Type or Type?
}
```

#### `class TypeExpr`

```dart
abstract class TypeExpr {
  bool get isInput;  // true for T?, false for T
}

class TypeRef extends TypeExpr {
  final String typeName;
  final bool isComplement;  // true for T?
  bool get isInput => isComplement;
}

class PrimitiveType extends TypeExpr {
  final String name;  // "_", "_?", "Integer", "String"
  bool get isInput => name == "_?";
}
```

### Functions

#### `TypeDef? getType(String name)`

Returns the type definition for `name`, or `null` if not defined.

**Note:** Returns `null` for predefined types (`Integer`, `String`, `_`, `_?`).

#### `ProcDecl? getProcedure(String name, int arity)`

Returns the procedure declaration, or `null` if not declared.

#### `bool isTypeDefined(String name)`

Returns `true` if `name` is a defined type or a predefined type.

#### `bool isPredefinedType(String name)`

Returns `true` if `name` is a predefined type.

#### `void addType(TypeDef def)`

Adds a type definition. Throws `RedefinitionError` if already defined or is predefined.

#### `void addProcedure(ProcDecl decl)`

Adds a procedure declaration. Throws `RedefinitionError` if already declared.

## Algorithms

### Algorithm: Type Lookup

```
getType(name):
  if name in predefinedTypes:
    return null  // Predefined types don't have definitions
  return types[name]  // Returns null if not found
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
  if def.name in predefinedTypes:
    throw PredefinedTypeError("Cannot redefine predefined type: " + def.name)
  if def.name in types:
    throw RedefinitionError("Type already defined: " + def.name)
  types[def.name] = def
```

### Algorithm: Add Procedure Declaration

```
addProcedure(decl):
  key = "$decl.name/$decl.arity"
  if key in procedures:
    throw RedefinitionError("Procedure already declared: " + key)

  // Validate all referenced types exist
  for argType in decl.argTypes:
    if not isTypeDefined(argType.baseName):
      throw UndefinedTypeError("Undefined type: " + argType.baseName)

  procedures[key] = decl
```

## Construction

A `TypeEnvironment` is constructed by parsing type declarations from source:

```
Stream ::= [] ; [_|Stream].
procedure merge(Stream?, Stream?, Stream).
```

Produces:
```dart
TypeEnvironment(
  types: {
    'Stream': TypeDef('Stream', [
      ListNilAlt(),
      ListConsAlt(PrimitiveType('_'), TypeRef('Stream'))
    ])
  },
  procedures: {
    'merge/3': ProcDecl('merge', 3, [
      TypeRef('Stream', isComplement: true),
      TypeRef('Stream', isComplement: true),
      TypeRef('Stream', isComplement: false)
    ])
  }
)
```

## Examples

### Example: Valid Environment

```
Stream ::= [] ; [_|Stream].
NatStream ::= [] ; [Integer|NatStream].
procedure merge(Stream?, Stream?, Stream).
procedure sum(NatStream?, Integer).
```

### Example: INVALID — Redefine Predefined Type

```
Integer ::= 0 ; succ(Integer).
```

**Error:** `PredefinedTypeError("Cannot redefine predefined type: Integer")`

Predefined types (`Integer`, `String`, `_`, `_?`) cannot be redefined.

### Example: INVALID — Duplicate Type Definition

```
Stream ::= [] ; [_|Stream].
Stream ::= nil ; cons(_, Stream).  % Duplicate!
```

**Error:** `RedefinitionError("Type already defined: Stream")`

### Example: INVALID — Undefined Type in Procedure

```
procedure foo(UndefinedType?, Integer).
```

**Error:** `UndefinedTypeError("Undefined type: UndefinedType")`

All types referenced in procedure declarations must be defined.

## Error Conditions

| Condition | Exception |
|-----------|-----------|
| Redefining existing type | `RedefinitionError` |
| Redefining predefined type | `PredefinedTypeError` |
| Redeclaring procedure | `RedefinitionError` |
| Reference to undefined type | `UndefinedTypeError` |

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft |
| 0.2 | 2025-01-07 | Add Dependencies section |
| 0.3 | 2025-01-07 | Add algorithms, positive and negative examples |
| 0.4 | 2025-01-09 | Add Type Classification section (Paper lines 9-17) |
