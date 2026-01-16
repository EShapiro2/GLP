# Module: clause-validation

**Version**: 1.0
**Date**: 2025-01-14
**Status**: DRAFT
**Paper References**: Section 3 (GLP), SRSW Restriction

## Purpose

Validates Term AST nodes in program clause contexts, enforcing restrictions on anonymous variables that are allowed syntactically but forbidden semantically.

## Dependencies

- `compiler/ast` — Term AST nodes
- `compiler/error` — CompileError

## Restrictions

### Anonymous Variable Restrictions

The parser accepts `_` and `_?` uniformly (see parser-spec.md). This module enforces semantic restrictions:

| Context | `_` | `_?` |
|---------|-----|------|
| Clause head | ✓ Allowed (discard input) | ✗ Forbidden |
| Clause body | ✗ Forbidden | ✗ Forbidden |
| Guard | ✓ Allowed (discard input) | ✗ Forbidden |

**Rationale:**
- `_` in head/guard: Discards incoming values at input positions (valid)
- `_` in body: SRSW violation - writer with no paired reader
- `_?` anywhere: No use case — cannot write into an anonymous position

See moded-term.md, "Anonymous Variables in Programs" for full specification.

## Public Interface

### Functions

#### `void validateClauseHead(Term term)`

Validates a term in clause head context.

**Preconditions:**
- `term` is a valid Term AST from parser

**Postconditions:**
- Returns normally if valid
- Throws `AnonymousReaderError` if `_?` found

**Errors:**
- `AnonymousReaderError`: `_?` not permitted in program clauses

#### `void validateClauseBody(Term term)`

Validates a term in clause body context.

**Preconditions:**
- `term` is a valid Term AST from parser

**Postconditions:**
- Returns normally if valid
- Throws error if `_` or `_?` found

**Errors:**
- `AnonymousReaderError`: `_?` not permitted in program clauses
- `AnonymousInBodyError`: `_` not permitted in clause bodies (SRSW violation)

#### `void validateGuard(Term term)`

Validates a term in guard context.

**Preconditions:**
- `term` is a valid Term AST from parser

**Postconditions:**
- Returns normally if valid
- Throws `AnonymousReaderError` if `_?` found

**Errors:**
- `AnonymousReaderError`: `_?` not permitted in program clauses

## Algorithms

### Algorithm: Validate Clause Head

```
validateClauseHead(term):
  traverse term recursively:
    if node is UnderscoreTerm(isReader: true):
      throw AnonymousReaderError(
        "_? (anonymous reader) not permitted in program clauses",
        node.line, node.column
      )
```

### Algorithm: Validate Clause Body

```
validateClauseBody(term):
  traverse term recursively:
    if node is UnderscoreTerm(isReader: true):
      throw AnonymousReaderError(
        "_? (anonymous reader) not permitted in program clauses",
        node.line, node.column
      )
    if node is UnderscoreTerm(isReader: false):
      throw AnonymousInBodyError(
        "Anonymous variable _ not permitted in clause body (violates SRSW: no paired reader)",
        node.line, node.column
      )
```

### Algorithm: Validate Guard

```
validateGuard(term):
  traverse term recursively:
    if node is UnderscoreTerm(isReader: true):
      throw AnonymousReaderError(
        "_? (anonymous reader) not permitted in program clauses",
        node.line, node.column
      )
```

## Error Types

```dart
class AnonymousReaderError extends CompileError {
  AnonymousReaderError(String message, int line, int column)
      : super(message, line, column, phase: 'validation');
}

class AnonymousInBodyError extends CompileError {
  AnonymousInBodyError(String message, int line, int column)
      : super(message, line, column, phase: 'validation');
}
```

## Examples

### Example 1: Valid Head with Anonymous Variable

**Input:**
```prolog
project_first([X | _], X).
```

**Term:**
```dart
Atom('project_first', [
  ListTerm(VarTerm('X'), UnderscoreTerm(isReader: false)),
  VarTerm('X')
])
```

**Result:** Valid ✓

### Example 2: Invalid — Anonymous Reader in Head

**Input:**
```prolog
bad(_?, X).
```

**Term:**
```dart
Atom('bad', [
  UnderscoreTerm(isReader: true),
  VarTerm('X')
])
```

**Result:** `AnonymousReaderError: "_? (anonymous reader) not permitted in program clauses"`

### Example 3: Invalid — Anonymous Variable in Body

**Input:**
```prolog
bad(X) :- | foo(X, _).
```

**Body term:**
```dart
Atom('foo', [VarTerm('X'), UnderscoreTerm(isReader: false)])
```

**Result:** `AnonymousInBodyError: "Anonymous variable _ not permitted in clause body (violates SRSW: no paired reader)"`

### Example 4: Valid Guard with Anonymous Variable

**Input:**
```prolog
pred(X, _) :- known(X) | body(X).
```

**Head term:** Contains `UnderscoreTerm(isReader: false)` — valid in head

**Guard term:** `known(X)` — no anonymous variables

**Result:** Valid ✓

## Integration

This validation is called after parsing, before SRSW checking and type checking:

```
Source → Lexer → Parser → Clause Validation → SRSW Check → Type Check
                              ↓
                     Reject _? anywhere
                     Reject _ in body
```

## Relationship to SRSW

SRSW checking happens **after** clause validation. Anonymous `_` is an exception to SRSW:
- Each `_` is a fresh variable with no paired counterpart
- SRSW checker should ignore `_` occurrences

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2025-01-14 | Initial specification |
