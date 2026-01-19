# GLP Parser Specification

**Version**: 2.1
**Date**: 2026-01-19
**Status**: DRAFT

## Overview

The GLP parser performs **uniform term parsing** for all syntactic contexts:
- Program clause heads and bodies
- Type definition alternatives
- Procedure declaration argument types

The parser produces a single `Term` AST. Semantic validation (what is allowed where) happens in separate compliance checking phases.

## Goal Syntax Principle

**The syntax of body goals and queries is identical.**

Any term that can appear as a top-level query can also appear as a goal in a clause body, and vice versa.

## Uniform Term Syntax

The parser accepts the following term forms:

| Syntax | AST Node | Description |
|--------|----------|-------------|
| `foo` | `ConstTerm('foo')` | Atom constant |
| `42`, `3.14` | `ConstTerm(42)`, `ConstTerm(3.14)` | Numeric constant |
| `"hello"` | `ConstTerm('"hello"')` | String constant |
| `X` | `VarTerm('X', isReader: false)` | Writer variable |
| `X?` | `VarTerm('X', isReader: true)` | Reader variable |
| `_` | `UnderscoreTerm(isReader: false)` | Anonymous variable |
| `_?` | `UnderscoreTerm(isReader: true)` | Anonymous reader |
| `[]` | `ListTerm(null, null)` | Empty list |
| `[H \| T]` | `ListTerm(H, T)` | List cons |
| `[a, b, c]` | `ListTerm(a, ListTerm(b, ListTerm(c, [])))` | List sugar |
| `foo(A, B)` | `StructTerm('foo', [A, B])` | Structure |
| `(A, B)` | `StructTerm(',', [A, B])` | Conjunction |
| `(A, B, C)` | `StructTerm(',', [A, StructTerm(',', [B, C])])` | Right-associative |
| `A + B` | `StructTerm('+', [A, B])` | Infix operator |
| `A \ B` | `StructTerm('\\', [A, B])` | Difference list |

### Anonymous Variables

The parser accepts both `_` and `_?` syntactically. Semantic restrictions (e.g., `_?` forbidden in programs) are enforced by separate validation phases, not by the parser.

### Conjunction Syntax

Parenthesized comma-separated terms are parsed as right-associative conjunction:
- `(A, B)` → `StructTerm(',', [A, B])`
- `(A, B, C)` → `StructTerm(',', [A, StructTerm(',', [B, C])])`

This matches standard Prolog conjunction syntax.

## Parsing Contexts

The same `_parseTerm()` method is used in all contexts:

| Context | Post-parsing Phase |
|---------|-------------------|
| Type definition alternatives | `termToTypeExpr()` conversion |
| Procedure declaration args | `termToTypeExpr()` conversion |
| Clause heads | `validateClauseTerm()` + SRSW check |
| Clause bodies | `validateClauseTerm()` + SRSW check |
| Guard expressions | `validateClauseTerm()` |

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                         PARSER LAYER                            │
│                                                                 │
│  _parseTerm() → Term AST                                        │
│    - VarTerm(name, isReader)                                    │
│    - StructTerm(functor, args)   includes ','(A,B) for (A,B)   │
│    - ListTerm(head, tail)                                       │
│    - ConstTerm(value)                                           │
│    - UnderscoreTerm(isReader)    _ or _?                        │
│                                                                 │
│  Uniform parsing - no semantic checks                           │
└─────────────────────────────────────────────────────────────────┘
                              │
              ┌───────────────┴───────────────┐
              ▼                               ▼
┌─────────────────────────────┐   ┌─────────────────────────────┐
│   TYPE DEFINITION PATH      │   │   PROGRAM CLAUSE PATH       │
│                             │   │                             │
│  termToTypeExpr(Term)       │   │  validateClauseTerm(Term)   │
│    → TypeExpr AST           │   │    - Reject _? anywhere     │
│                             │   │    - Reject _ in body       │
│  See: type-conversion.md    │   │                             │
│                             │   │  See: clause-validation.md  │
└─────────────────────────────┘   └─────────────────────────────┘
```

## Arithmetic Expression Syntax

**User-facing syntax**: Programmers write natural infix arithmetic:
```prolog
add(X, Y, Z) :- execute('evaluate', [X? + Y?, Z]).
compute(Z) :- execute('evaluate', [(2 + 3) * 4, Z]).
```

**Internal representation**: Parser transforms to prefix structures:
```
+(VarRef(X, isReader:true), VarRef(Y, isReader:true))
*(+(2, 3), 4)
```

### Grammar

```
expr ::= number                    % integer literal
       | variable                  % writer or reader variable
       | '-' expr                  % unary negation
       | expr '+' expr             % addition
       | expr '-' expr             % subtraction
       | expr '*' expr             % multiplication
       | expr '/' expr             % integer division
       | expr 'mod' expr           % modulo
       | '(' expr ')'              % grouping
```

### Operator Precedence (highest to lowest)

1. **Parentheses**: `()`
2. **Unary negation**: `-` (prefix)
3. **Multiplicative**: `*`, `/`, `mod` (left-associative)
4. **Additive**: `+`, `-` (left-associative)

### Operator Associativity

All binary operators are **left-associative**:
- `X + Y + Z` parses as `(X + Y) + Z`
- `X * Y / Z` parses as `(X * Y) / Z`

### Transformation Rules

| Source (Infix)  | AST (Prefix Structure) |
|-----------------|------------------------|
| `X + Y`         | `+(X, Y)`              |
| `X - Y`         | `-(X, Y)`              |
| `X * Y`         | `*(X, Y)`              |
| `X / Y`         | `/(X, Y)`              |
| `X mod Y`       | `mod(X, Y)`            |
| `-X`            | `neg(X)`               |

**Note**: Unary `-` uses functor `neg` to avoid ambiguity with binary subtraction.

### Lexer Token Additions

```dart
enum TokenType {
  // ... existing tokens ...

  // Arithmetic operators
  PLUS,           // +
  MINUS,          // - (can be binary or unary)
  STAR,           // *
  SLASH,          // /
  MOD,            // mod (keyword)
}
```

## Guard Expression Syntax

Guards are pure tests that appear before `|` in clauses: `Head :- Guard1, Guard2, ... | Body.`

**See `guards-reference.md` for the authoritative specification of:**
- All builtin guards (`ground`, `known`, `integer`, `number`, `writer`, `reader`, `otherwise`, `=?=`, arithmetic comparisons)
- Defined guards via unit clauses (e.g., `X = X.` defines the equality guard)
- Guard negation (`~G`)
- SRSW relaxation rules for guards that imply groundness

This section covers only the **parser syntax** for guards.

### Infix Comparison Guards

The parser transforms infix comparison syntax to prefix form:

| Source Syntax | AST Representation |
|---------------|-------------------|
| `X < Y` | `Atom('<', [X, Y])` |
| `X =< Y` | `Atom('=<', [X, Y])` |
| `X > Y` | `Atom('>', [X, Y])` |
| `X >= Y` | `Atom('>=', [X, Y])` |
| `X =:= Y` | `Atom('=:=', [X, Y])` |
| `X =\= Y` | `Atom('=\\=', [X, Y])` |
| `X =?= Y` | `Atom('=?=', [X, Y])` |

### Lexer Tokens for Comparison Guards

```dart
enum TokenType {
  // ... existing tokens ...

  // Comparison operators (precedence 700, non-associative)
  LESS,           // <
  LESS_EQUAL,     // =< (Prolog convention, not <=)
  GREATER,        // >
  GREATER_EQUAL,  // >=
  ARITH_EQUAL,    // =:=
  ARITH_NOT_EQUAL,// =\=
  GROUND_EQUAL,   // =?=
}
```

### Operator Precedence (Extended)

```
1200  :- (rule separator)
1100  | (guard separator)
 700  < =< > >= =:= =\= =?= (comparison, non-associative)
 500  + - (additive, left-associative)
 400  * / mod (multiplicative, left-associative)
 200  - (unary minus, non-associative)
```

**Key property**: Comparison operators are **non-associative** (expressions like `X < Y < Z` are rejected).

### Implementation Status

- ✅ Parser recognizes guard separator `|` correctly
- ✅ Parses predicates before `|` as `Guard` AST nodes
- ✅ Guard execution infrastructure (AST, codegen, runner) ready
- ⏳ Some comparison operator tokens not yet in lexer

## SRSW Checking

The parser is responsible for checking the **Single-Reader/Single-Writer (SRSW) syntactic restriction** on all clauses:

1. Every variable X that occurs in a clause must have its paired variable X? also occur in the clause
2. Each variable (reader or writer) occurs exactly once
3. Exception: Multiple readers allowed if guarded by `ground(X?)` (see `guards-reference.md`)

**SRSW is checked before type checking.** The type checker assumes all input clauses satisfy SRSW and does not verify variable pairing.

See `SPEC_GUIDE.md` for detailed SRSW semantics.

## Error Handling

### Lexer Errors

- **Unexpected character**: If `+`, `-`, `*`, `/` appear in invalid contexts
- **Invalid number**: Malformed numeric literals

### Parser Errors

- **Missing operand**: `X + ` (no right operand)
- **Mismatched parentheses**: `(X + Y`
- **Invalid primary**: `+ + X` (two consecutive operators)

### Runtime Errors

- **Type error**: Operand is non-integer (e.g., `3 + a` where `a` is atom)
- **Division by zero**: `X / 0`
- **Unbound reader**: Suspends goal, not an error

## References

- **guards-reference.md**: Authoritative guard specification
- **SPEC_GUIDE.md**: SRSW semantics
- **glp-bytecode-v216-complete.md**: Execute instruction and system predicates
- **Pratt Parsing**: [https://en.wikipedia.org/wiki/Operator-precedence_parser#Pratt_parsing](https://en.wikipedia.org/wiki/Operator-precedence_parser#Pratt_parsing)

## Version History

- **v1.0 (2025-11-12)**: Initial specification for arithmetic expression parsing
- **v1.1 (2025-01-12)**: Added SRSW checking section documenting parser responsibility
- **v2.0 (2025-01-14)**: Unified term parsing architecture; separate compliance checks for types vs clauses
- **v2.1 (2026-01-19)**: Consolidated guard section to reference guards-reference.md; removed duplicated guard semantics
