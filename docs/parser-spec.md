# GLP Parser Specification

**Version**: 2.0
**Date**: 2025-01-14
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

## Motivation

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

This approach keeps the VM simple (no special arithmetic instructions) while providing convenient syntax.

## Arithmetic Expression Syntax

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

## Transformation Rules

### Binary Operators

| Source (Infix)  | AST (Prefix Structure) |
|-----------------|------------------------|
| `X + Y`         | `+(X, Y)`              |
| `X - Y`         | `-(X, Y)`              |
| `X * Y`         | `*(X, Y)`              |
| `X / Y`         | `/(X, Y)`              |
| `X mod Y`       | `mod(X, Y)`            |

### Unary Operator

| Source          | AST (Prefix Structure) |
|-----------------|------------------------|
| `-X`            | `neg(X)`               |

**Note**: Unary `-` uses functor `neg` to avoid ambiguity with binary subtraction.

### Precedence Examples

| Source              | AST (respects precedence)    |
|---------------------|------------------------------|
| `X + Y * Z`         | `+(X, *(Y, Z))`              |
| `(X + Y) * Z`       | `*(+(X, Y), Z)`              |
| `X + Y + Z`         | `+(+(X, Y), Z)`              |
| `X * Y / Z`         | `/(*(X, Y), Z)`              |
| `2 + 3 * 4`         | `+(2, *(3, 4))` = 14         |
| `(2 + 3) * 4`       | `*(+(2, 3), 4)` = 20         |
| `-X + Y`            | `+(neg(X), Y)`               |
| `-(X + Y)`          | `neg(+(X, Y))`               |

### Reader Variables in Expressions

Readers (`X?`) are preserved in the AST:

| Source              | AST                                      |
|---------------------|------------------------------------------|
| `X? + Y?`           | `+(VarRef(X, isReader:true), VarRef(Y, isReader:true))` |
| `X? + 5`            | `+(VarRef(X, isReader:true), ConstTerm(5))` |
| `-X?`               | `neg(VarRef(X, isReader:true))`          |

## Lexer Token Additions

The lexer must recognize these new tokens:

```dart
enum TokenType {
  // ... existing tokens ...

  // Arithmetic operators (NEW)
  PLUS,           // +
  MINUS,          // - (can be binary or unary)
  STAR,           // *
  SLASH,          // /
  MOD,            // mod (keyword)
}
```

### Lexer Changes

1. **Operator tokens**: Add cases for `+`, `*`, `/` in `_scanToken()`
2. **Minus handling**: `-` already handled for negative number literals; add MINUS token for non-numeric context
3. **MOD keyword**: Recognize `mod` as keyword (not atom) during identifier scanning

## Parser Implementation Strategy

### Expression Parsing Algorithm

Use **Pratt parsing** (precedence climbing) for expressions:

1. **Primary expression**: number, variable, `(expr)`, `-expr`
2. **Binary operators**: Handle precedence and associativity
3. **Transform to AST**: Build StructTerm with operator as functor

### Parser Method Additions

```dart
// In Parser class:

/// Parse arithmetic expression with precedence
Term parseExpression([int minPrecedence = 0]) {
  var left = parsePrimary();

  while (current is operator && precedence(current) >= minPrecedence) {
    final op = advance();
    final right = parseExpression(precedence(op) + 1);
    left = StructTerm(op.functor, [left, right]);
  }

  return left;
}

/// Parse primary expression: number | variable | (expr) | -expr
Term parsePrimary() {
  if (match(NUMBER)) return ConstTerm(previous.value);
  if (match(VARIABLE)) return VarTerm(previous.lexeme, isReader: false);
  if (match(READER)) return VarTerm(previous.lexeme, isReader: true);
  if (match(LPAREN)) {
    final expr = parseExpression();
    consume(RPAREN);
    return expr;
  }
  if (match(MINUS)) {
    final operand = parsePrimary();
    return StructTerm('neg', [operand]);
  }
  throw ParseError("Expected expression");
}

/// Operator precedence table
int precedence(Token op) {
  switch (op.type) {
    case TokenType.STAR:
    case TokenType.SLASH:
    case TokenType.MOD:
      return 20;  // multiplicative
    case TokenType.PLUS:
    case TokenType.MINUS:
      return 10;  // additive
    default:
      return 0;
  }
}
```

### Integration with Existing Parser

Arithmetic expressions are parsed in contexts where terms are expected:
- **Execute arguments**: `execute('evaluate', [Expr, Result])`
- **List elements**: `[X + Y, A * B]`
- **Structure arguments**: `foo(X + Y, Z)`

**Detection heuristic**: If we see a term followed by an operator token (`+`, `-`, `*`, `/`, `mod`), enter expression parsing mode.

## Type System

### Supported Types

- **Integers only**: No floating point support in this phase
- **Division**: Integer division (truncates toward zero)
- **Modulo**: Standard modulo operation

### Type Checking

Type checking happens at **runtime** during `evaluate/2` execution:

```dart
// In evaluatePredicate():
final value = _evaluate(runtime, expr, call);
if (value is! int) {
  return SystemResult.failure;  // non-integer operand
}
```

### Three-Valued Semantics

The `evaluate/2` system predicate (called via execute) follows **two-valued semantics** (success/abort):

1. **Success**: All operands bound to numbers → compute result
2. **Abort**: Operand is unbound reader OR non-numeric OR division by zero

**Safe Pattern**: Use guards to ensure preconditions:
```prolog
safe_add(X, Y, Z) :-
  number(X), number(Y) |
  execute('evaluate', [X? + Y?, Z]).
```

## Guard Expressions

Guards are pure tests that check runtime conditions without side effects. They use three-valued semantics (success/suspend/fail) and execute during the HEAD/GUARDS phase before commit.

### Guard Categories

#### Type Guards

Function-call syntax (already working):
- `ground(X)` - test if X contains no unbound variables
- `known(X)` - test if X is bound (not unbound variable)
- `integer(X)` - test if X is integer (planned)
- `number(X)` - test if X is number (planned)
- `writer(X)` - test if X is writer variable (planned)
- `reader(X)` - test if X is reader variable (planned)

#### Comparison Guards

Infix syntax (requires parser extension):
- `X < Y` - less than
- `X =< Y` - less than or equal (Prolog convention, NOT `<=`)
- `X > Y` - greater than
- `X >= Y` - greater than or equal
- `X =:= Y` - arithmetic equality
- `X =\= Y` - arithmetic inequality

#### Unification Guards (planned)

- `X = Y` - unification guard
- `X \= Y` - non-unification guard

#### Control Guards

- `otherwise` - succeeds if all previous clauses failed (not suspended)
- `true` - always succeeds

### Lexer Token Additions for Guards

Add to `token.dart`:

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

  // Unification guards (precedence 700)
  UNIFY,          // =
  NOT_UNIFIABLE,  // \=
}
```

**Lexer Implementation**:
```dart
// In Lexer._scanToken():
case '<':
  addToken(_peek() == '=' ? LESS_EQUAL : LESS);
  if (_peek() == '=') _advance();
  break;
case '>':
  addToken(_peek() == '=' ? GREATER_EQUAL : GREATER);
  if (_peek() == '=') _advance();
  break;
case '=':
  if (_peek() == ':' && _peekNext() == '=') {
    _advance(); _advance();
    addToken(ARITH_EQUAL);  // =:=
  } else if (_peek() == '\\' && _peekNext() == '=') {
    _advance(); _advance();
    addToken(ARITH_NOT_EQUAL);  // =\=
  } else {
    addToken(UNIFY);  // =
  }
  break;
case '\\':
  if (_peek() == '=') {
    _advance();
    addToken(NOT_UNIFIABLE);  // \=
  } else {
    error("Unexpected character");
  }
  break;
```

### Operator Precedence (Extended)

```
1200  :- (rule separator)
1100  | (guard separator)
 700  < =< > >= =:= =\= = \= (comparison/test, non-associative)
 500  + - (additive, left-associative)
 400  * / mod (multiplicative, left-associative)
 200  - (unary minus, non-associative)
```

**Key property**: Comparison operators are **non-associative** (expressions like `X < Y < Z` are rejected).

### Parser Extension for Guard Expressions

Update `_parseGoalOrGuard()` to handle infix comparison syntax:

```dart
Goal _parseGoalOrGuard() {
  // Start parsing as term
  final left = _parseTerm();

  // Check for comparison operators
  if (_isComparisonOp(_current)) {
    final op = _advance();
    final right = _parseTerm();

    // Transform infix to prefix: X < Y → <(X, Y)
    return Atom(op.lexeme, [left, right]);
  }

  // Otherwise, it's a regular function-call guard
  return left as Atom;
}

bool _isComparisonOp(Token token) {
  return token.type == TokenType.LESS ||
         token.type == TokenType.LESS_EQUAL ||
         token.type == TokenType.GREATER ||
         token.type == TokenType.GREATER_EQUAL ||
         token.type == TokenType.ARITH_EQUAL ||
         token.type == TokenType.ARITH_NOT_EQUAL ||
         token.type == TokenType.UNIFY ||
         token.type == TokenType.NOT_UNIFIABLE;
}
```

### Transformation Examples

| Source Syntax      | AST Representation       | Guard Predicate |
|--------------------|--------------------------|-----------------|
| `X < Y`            | `Atom('<', [X, Y])`      | `guard_less`    |
| `X =< Y`           | `Atom('=<', [X, Y])`     | `guard_less_equal` |
| `X > Y`            | `Atom('>', [X, Y])`      | `guard_greater` |
| `X >= Y`           | `Atom('>=', [X, Y])`     | `guard_greater_equal` |
| `X =:= Y`          | `Atom('=:=', [X, Y])`    | `guard_arith_equal` |
| `X =\= Y`          | `Atom('=\\=', [X, Y])`   | `guard_arith_not_equal` |
| `X = Y`            | `Atom('=', [X, Y])`      | `guard_unify` |
| `X \= Y`           | `Atom('\\=', [X, Y])`    | `guard_not_unifiable` |

### Guard Examples

**Type Guards** (working):
```prolog
pred(X, Y) :- known(X), ground(Y) | body.
factorial(N, F) :- integer(N) | compute_fact(N?, F).
```

**Comparison Guards** (requires parser extension):
```prolog
% Quicksort with comparison guards
partition(Pivot, [], [], []).
partition(Pivot, [X | Xs?], [X | Smaller], Greater) :-
    X? < Pivot? |
    partition(Pivot?, Xs?, Smaller, Greater).
partition(Pivot, [X | Xs?], Smaller, [X | Greater]) :-
    X? >= Pivot? |
    partition(Pivot?, Xs?, Smaller, Greater).

% Arithmetic equality
equal_results(X, Y) :-
    X? =:= Y? |
    execute('write', ['Results are equal']).
```

**Unification Guards** (planned):
```prolog
% Unification without binding
unifiable(X, Y) :- X = Y | body.  % succeeds if X and Y can unify
different(X, Y) :- X \= Y | body. % succeeds if X and Y cannot unify
```

**Implementation Status**:
- ✅ Parser recognizes guard separator `|` correctly
- ✅ Parses predicates before `|` as `Guard` AST nodes
- ✅ Guard execution infrastructure (AST, codegen, runner) ready
- ⏳ Comparison operator tokens not yet in lexer
- ⏳ Infix syntax in guard position not yet parsed

**Required for Full Guard Support**:
1. Add comparison tokens to `token.dart` (see above)
2. Update lexer to recognize multi-character operators (`=<`, `>=`, `=:=`, `=\=`)
3. Extend `_parseGoalOrGuard()` to handle infix comparison syntax
4. Transform infix to prefix: `X < Y` → `Atom('<', [X, Y])`

### Three-Valued Semantics

Guards return one of three outcomes:
1. **Success**: Condition holds, continue with clause
2. **Suspend**: Unbound reader encountered, add to Si suspension set
3. **Fail**: Condition does not hold, try next clause

**Example**:
```prolog
safe_divide(X, Y, Z) :-
    number(X), number(Y), Y =\= 0 |  % guards ensure preconditions
    execute('evaluate', [X? / Y?, Z]).
```

If `Y` is unbound, the `number(Y)` guard suspends (adds Y's reader to Si).
If `Y` is bound to 0, the `Y =\= 0` guard fails (tries next clause).
If both guards succeed, execute proceeds safely.

## Examples

### Simple Arithmetic

**Source**:
```prolog
add(X, Y, Z) :- execute('evaluate', [X? + Y?, Z]).
```

**AST**:
```dart
Clause(
  head: Atom('add', [VarTerm('X'), VarTerm('Y'), VarTerm('Z')]),
  body: [
    Atom('execute', [
      ConstTerm('evaluate'),
      ListTerm([
        StructTerm('+', [
          VarTerm('X', isReader: true),
          VarTerm('Y', isReader: true)
        ]),
        VarTerm('Z', isReader: false)
      ])
    ])
  ]
)
```

### Complex Expression

**Source**:
```prolog
compute(X, Y, Z) :- execute('evaluate', [(X? + Y?) * 2, Z]).
```

**AST**:
```dart
StructTerm('*', [
  StructTerm('+', [
    VarTerm('X', isReader: true),
    VarTerm('Y', isReader: true)
  ]),
  ConstTerm(2)
])
```

### Unary Negation

**Source**:
```prolog
negate(X, Y) :- execute('evaluate', [-X?, Y]).
```

**AST**:
```dart
StructTerm('neg', [VarTerm('X', isReader: true)])
```

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

## Implementation Checklist

### Phase 1: Lexer
- [ ] Add PLUS, MINUS, STAR, SLASH tokens
- [ ] Add MOD keyword token
- [ ] Update `_scanToken()` switch statement
- [ ] Handle `-` in numeric vs operator context

### Phase 2: Parser
- [ ] Implement `parseExpression()` with precedence climbing
- [ ] Implement `parsePrimary()` for base cases
- [ ] Add precedence table for operators
- [ ] Integrate with term parsing
- [ ] Transform to prefix StructTerm

### Phase 3: Testing
- [ ] Test simple operations: `X + Y`, `X * Y`
- [ ] Test precedence: `X + Y * Z` = `+(X, *(Y, Z))`
- [ ] Test parentheses: `(X + Y) * Z` = `*(+(X, Y), Z)`
- [ ] Test unary minus: `-X` = `neg(X)`
- [ ] Test with readers: `X? + Y?`
- [ ] Test nested expressions: `(X + Y) * (Z - W)`

### Phase 4: Integration
- [ ] Run REPL test suite with arithmetic programs
- [ ] Verify `evaluate/2` receives correct prefix structures
- [ ] Test three-valued semantics (success/suspend/fail)
- [ ] Verify error messages are clear

## SRSW Checking

The parser is responsible for checking the **Single-Reader/Single-Writer (SRSW) syntactic restriction** on all clauses:

1. Every variable X that occurs in a clause must have its paired variable X? also occur in the clause
2. Each variable (reader or writer) occurs exactly once
3. Exception: Multiple readers allowed if guarded by `ground(X?)`

**SRSW is checked before type checking.** The type checker assumes all input clauses satisfy SRSW and does not verify variable pairing.

See `SPEC_GUIDE.md` for detailed SRSW semantics.

## References

- **WAM Paper** (Section 8): Structure building and traversal
- **glp-bytecode-v216-complete.md**: Execute instruction and system predicates
- **Pratt Parsing**: [https://en.wikipedia.org/wiki/Operator-precedence_parser#Pratt_parsing](https://en.wikipedia.org/wiki/Operator-precedence_parser#Pratt_parsing)

## Version History

- **v1.0 (2025-11-12)**: Initial specification for arithmetic expression parsing
- **v1.1 (2025-01-12)**: Added SRSW checking section documenting parser responsibility
- **v2.0 (2025-01-14)**: Unified term parsing architecture; separate compliance checks for types vs clauses
