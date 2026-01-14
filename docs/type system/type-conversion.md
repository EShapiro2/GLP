# Module: type-conversion

**Version**: 1.0
**Date**: 2025-01-14
**Status**: DRAFT
**Paper References**: Section 4.1 (Type Syntax)

## Purpose

Converts `Term` AST (produced by the unified parser) to `TypeExpr` AST for type definitions and procedure declarations.

## Dependencies

- `compiler/ast` — Term AST nodes
- `type_ast` — TypeExpr AST nodes

## Conversion Rules

### Term to TypeExpr Mapping

| Term AST | TypeExpr AST | Notes |
|----------|--------------|-------|
| `VarTerm(name, isReader: false)` | `TypeRef(name, isInput: false)` | Type reference |
| `VarTerm(name, isReader: true)` | `TypeRef(name, isInput: true)` | Input type reference |
| `UnderscoreTerm(isReader: false)` | `PrimitiveModeAlt(isInput: false)` | Output wildcard `_` |
| `UnderscoreTerm(isReader: true)` | `PrimitiveModeAlt(isInput: true)` | Input wildcard `_?` |
| `ConstTerm(value)` | `ConstantAlt(value)` | Constant (atom, number) |
| `ListTerm(null, null)` | `ListNilAlt()` | Empty list `[]` |
| `ListTerm(head, tail)` | `ListConsAlt(convert(head), convert(tail))` | List cons `[H\|T]` |
| `StructTerm('\\', [a, b])` | `DiffListAlt(convert(a), convert(b))` | Difference list `A \ B` |
| `StructTerm(functor, args)` | `StructAlt(functor, args.map(convert))` | Structure |

### Special Cases

**Conjunction functor:** `StructTerm(',', [A, B])` converts to `StructAlt(',', [convert(A), convert(B)])`. This handles `(A, B)` syntax in type definitions.

**Difference list detection:** `StructTerm('\\', [content, hole])` is recognized as difference list syntax and converts to `DiffListAlt`.

## Algorithm

```
termToTypeExpr(term, line, column):
  match term:
    VarTerm(name, isReader):
      return TypeRef(name, line, column, isInput: isReader)
    
    UnderscoreTerm(isReader):
      return PrimitiveModeAlt(isReader, line, column)
    
    ConstTerm(value):
      return ConstantAlt(value, line, column)
    
    ListTerm(null, null):
      return ListNilAlt(line, column)
    
    ListTerm(head, tail):
      return ListConsAlt(
        termToTypeExpr(head, head.line, head.column),
        termToTypeExpr(tail, tail.line, tail.column),
        line, column
      )
    
    StructTerm('\\', [content, hole]):
      return DiffListAlt(
        termToTypeExpr(content, content.line, content.column),
        termToTypeExpr(hole, hole.line, hole.column),
        line, column
      )
    
    StructTerm(functor, args):
      return StructAlt(
        functor,
        args.map(arg => termToTypeExpr(arg, arg.line, arg.column)),
        line, column
      )
```

## Public Interface

### Functions

#### `TypeExpr termToTypeExpr(Term term)`

Converts a Term AST node to a TypeExpr AST node.

**Preconditions:**
- `term` is a valid Term AST node from the parser

**Postconditions:**
- Returns corresponding TypeExpr
- Preserves line/column information for error reporting

**Errors:**
- None. All valid Term nodes have corresponding TypeExpr representations.

## Examples

### Example 1: Simple Type Reference

**Input (Term):**
```dart
VarTerm('Stream', isReader: false, line: 1, column: 1)
```

**Output (TypeExpr):**
```dart
TypeRef('Stream', line: 1, column: 1, isInput: false)
```

### Example 2: Input Type Reference

**Input (Term):**
```dart
VarTerm('Stream', isReader: true, line: 1, column: 1)
```

**Output (TypeExpr):**
```dart
TypeRef('Stream', line: 1, column: 1, isInput: true)
```

### Example 3: List Type

**Input (Term):** `[_ | Stream]`
```dart
ListTerm(
  UnderscoreTerm(isReader: false),
  VarTerm('Stream', isReader: false)
)
```

**Output (TypeExpr):**
```dart
ListConsAlt(
  PrimitiveModeAlt(isInput: false),
  TypeRef('Stream', isInput: false)
)
```

### Example 4: Conjunction Type

**Input (Term):** `(_, _)`
```dart
StructTerm(',', [
  UnderscoreTerm(isReader: false),
  UnderscoreTerm(isReader: false)
])
```

**Output (TypeExpr):**
```dart
StructAlt(',', [
  PrimitiveModeAlt(isInput: false),
  PrimitiveModeAlt(isInput: false)
])
```

### Example 5: Difference List Type

**Input (Term):** `List \ List?`
```dart
StructTerm('\\', [
  VarTerm('List', isReader: false),
  VarTerm('List', isReader: true)
])
```

**Output (TypeExpr):**
```dart
DiffListAlt(
  TypeRef('List', isInput: false),
  TypeRef('List', isInput: true)
)
```

## Implementation Notes

This conversion is purely structural. Semantic validation (determinism, alias prohibition, undefined type references) happens in the type environment module after conversion.

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2025-01-14 | Initial specification |
