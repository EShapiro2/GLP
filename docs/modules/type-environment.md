# Module: type-environment

**Version**: 0.1
**Date**: 2025-01-07
**Status**: DRAFT
**Paper References**: Definition 4.1 (page 5)

## Purpose

Stores and provides lookup for type definitions and procedure declarations in a typed GLP program.

## Paper Definition

### Definition 4.1: Typed GLP Program (page 5)

> A typed GLP program P = (Cs, D) has GLP clauses Cs and a GLP type D defining the type of every procedure in Cs.

The type environment represents "D" — the collection of type definitions and procedure declarations.

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
