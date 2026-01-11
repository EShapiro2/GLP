# Module: type-environment

**Version**: 0.5
**Date**: 2025-01-11
**Status**: DRAFT
**Paper References**: Definition 4.1, Section 4.1 (Type Definition Constraints)

## Dependencies

None (leaf module).

## Purpose

Stores and provides lookup for type definitions and procedure declarations in a typed GLP program.

## Paper Definition

### Definition 4.1: Typed GLP Program

> A typed GLP program P = (Cs, D) has GLP clauses Cs and a GLP type D defining the type of every procedure in Cs.

The type environment represents "D" — the collection of type definitions and procedure declarations.

### Type Classification

Types are classified by their mode structure:

| Classification | Definition | Example |
|----------------|------------|---------|
| **Output type** | No complementation in definition | `Stream ::= [] ; [_\|Stream]` |
| **Input type** | Complement of an output type | `Stream?` (all modes flipped) |
| **Interactive type** | Contains internal complementation | `HollowStream ::= [] ; [_?\|HollowStream]` |

**Key insight:** Interactive types like `HollowStream` have some positions that consume (`_?`) while the overall structure is produced. This enables bidirectional communication patterns.

### Valid Type Alternatives

Each type definition must introduce new structure. A **valid type alternative** is one of:

| Alternative | Syntax | Example |
|-------------|--------|---------|
| Constant | `atom` or `number` | `0`, `nil`, `add` |
| Empty list | `[]` | `[]` |
| List cons | `[H \| T]` | `[_ \| Stream]`, `[Integer \| List]` |
| Structure | `functor(args)` | `s(Nat)`, `pair(_, _?)`, `ch(Stream?, Stream)` |
| Difference list | `T \ T?` | `List \ List?` |

**Primitives** (`_`, `_?`) and **type references** (`T`, `T?`) may only appear as arguments within structured alternatives, not as top-level alternatives.

### Prohibited: Type Aliases

Type aliases are **prohibited**. A type definition must not merely rename an existing type or primitive.

**Illegal examples:**

```
Output ::= _.            % alias for primitive — ILLEGAL
Input ::= _?.            % alias for primitive — ILLEGAL
Any ::= _.               % alias for primitive — ILLEGAL
MyList ::= List.         % alias for defined type — ILLEGAL
MyStream ::= Stream?.    % alias for complement — ILLEGAL
```

**Rationale:** Each type definition must introduce structure (functors, constants, or list constructors) that can be matched during type checking. An alias provides no new structure and would create ambiguity in the DFA.

### Prohibited: Overlapping Alternatives

Alternatives must be distinguishable by their top-level functor or by disjoint type membership.

**Illegal examples:**

```
Any ::= _ ; _?.          % overlapping: both accept all terms — ILLEGAL
AnyOne ::= 1 ; 1?.       % overlapping: 1 matches both — ILLEGAL  
Ambiguous ::= _ ; Integer.  % overlapping: integers match both — ILLEGAL
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

  /// Predefined types (Number, String)
  static final Set<String> predefinedTypes = {'Number', 'String'};
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

**Note:** Returns `null` for predefined types (`Number`, `String`).

#### `ProcDecl? getProcedure(String name, int arity)`

Returns the procedure declaration, or `null` if not declared.

#### `bool isTypeDefined(String name)`

Returns `true` if `name` is a defined type or a predefined type.

#### `bool isPredefinedType(String name)`

Returns `true` if `name` is a predefined type.

#### `void addType(TypeDef def)`

Adds a type definition. Validates the definition and throws on error.

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

### Algorithm: Validate Type Alternative

```
isValidAlternative(alt):
  match alt:
    ConstantAlt(_):      return true   // 0, nil, add, etc.
    ListNilAlt:          return true   // []
    ListConsAlt(_, _):   return true   // [H|T]
    StructAlt(_, args):  return true   // functor(args)
    DiffListAlt(_, _):   return true   // T \ T?
    PrimitiveModeAlt(_): return false  // _ or _? alone — ALIAS
    TypeRef(_):          return false  // T or T? alone — ALIAS
```

### Algorithm: Add Type Definition

```
addType(def):
  if def.name in predefinedTypes:
    throw PredefinedTypeError("Cannot redefine predefined type: " + def.name)
  if def.name in types:
    throw RedefinitionError("Type already defined: " + def.name)
  
  // Check for aliases: single alternative that is just a primitive or type reference
  if def.alternatives.length == 1:
    alt = def.alternatives[0]
    if alt is PrimitiveModeAlt:
      throw AliasError("Type alias prohibited: " + def.name + " ::= " + alt)
    if alt is TypeRef:
      throw AliasError("Type alias prohibited: " + def.name + " ::= " + alt)
  
  // Validate each alternative is a valid top-level form
  for alt in def.alternatives:
    if not isValidAlternative(alt):
      throw InvalidAlternativeError("Invalid top-level alternative in " + def.name + ": " + alt)
  
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
    if argType is TypeRef and not isTypeDefined(argType.name):
      throw UndefinedTypeError("Undefined type: " + argType.name)

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

### Example: Valid Type Definitions

```
Stream ::= [] ; [_|Stream].
Nat ::= 0 ; s(Nat).
Pair ::= pair(_, _?).
DiffList ::= List \ List?.
CounterCall ::= add ; clear ; read(Integer?).
```

### Example: INVALID — Alias to Primitive

```
Output ::= _.
```

**Error:** `AliasError("Type alias prohibited: Output ::= _")`

### Example: INVALID — Alias to Primitive (Input)

```
Input ::= _?.
```

**Error:** `AliasError("Type alias prohibited: Input ::= _?")`

### Example: INVALID — Alias to Type Reference

```
MyList ::= List.
```

**Error:** `AliasError("Type alias prohibited: MyList ::= List")`

### Example: INVALID — Alias to Complement

```
MyStream ::= Stream?.
```

**Error:** `AliasError("Type alias prohibited: MyStream ::= Stream?")`

### Example: INVALID — Redefine Predefined Type

```
Number ::= 0 ; succ(Number).
```

**Error:** `PredefinedTypeError("Cannot redefine predefined type: Number")`

### Example: INVALID — Duplicate Type Definition

```
Stream ::= [] ; [_|Stream].
Stream ::= nil ; cons(_, Stream).
```

**Error:** `RedefinitionError("Type already defined: Stream")`

### Example: INVALID — Undefined Type in Procedure

```
procedure foo(UndefinedType?, Number).
```

**Error:** `UndefinedTypeError("Undefined type: UndefinedType")`

## Error Conditions

| Condition | Exception |
|-----------|-----------|
| Single primitive alternative (`_ ` or `_?`) | `AliasError` |
| Single type reference alternative (`T` or `T?`) | `AliasError` |
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
| 0.4 | 2025-01-09 | Add Type Classification section |
| 0.5 | 2025-01-11 | Add Valid Type Alternatives section; prohibit aliases; add AliasError |
