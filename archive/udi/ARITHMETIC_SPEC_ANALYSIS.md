# GLP Arithmetic: Spec vs Implementation Analysis

**Date**: November 12, 2025
**Issue**: Parser fails on infix operators (`+`, `-`, `*`, `/`, `mod`) in expressions

---

## Summary

✅ **The evaluate/2 system predicate is fully implemented and working**
❌ **The parser doesn't recognize infix notation for arithmetic operators**

---

## Specification

From `docs/glp-bytecode-v216-complete.md`:

### System Predicate: evaluate/2

```
Usage: execute('evaluate', [ExpressionVar, ResultVar])

Supports:
- Operators: +, -, *, /, mod
- Nested expressions
- Three-valued semantics (success/suspend/failure)

Example:
  evaluate(+(2, *(3, 4)), R)  % R = 14
  evaluate(+(X?, 5), R)       % Suspend on X
```

### Implementation Status

**File**: `glp_runtime/lib/runtime/system_predicates_impl.dart`

✅ **Fully Implemented**:
- Lines 72-196: `evaluatePredicate()` function
- Lines 198-300: `_evaluate()` helper
- Handles all operators: `+`, `-`, `*`, `/`, `mod`
- Proper suspension on unbound readers
- Division by zero detection
- Type error handling
- Three-valued semantics working

**Test Status**: All 22 system predicates working, including `evaluate/2`

---

## The Problem: Parser vs Runtime

### What Works (Runtime) ✅

The **runtime** expects **prefix notation** with structures:

```dart
StructTerm('+', [ConstTerm(2), ConstTerm(3)])  // Represents: +(2, 3)
```

Example that works in tests:
```dart
evaluate(+(2, 3), R)  // R = 5
evaluate(*(X?, 10), R)  // Suspend on X, then compute X * 10
```

### What Doesn't Work (Parser) ❌

The **parser** doesn't recognize **infix notation**:

```glp
add(X, Y, Z) :- execute('evaluate', [X? + Y?, Z]).
                                         ^
                                    ERROR: Unexpected character: +
```

The parser sees `X? + Y?` and:
1. Parses `X?` as a reader variable ✅
2. Encounters `+` as a character (not an operator) ❌
3. Fails with "Unexpected character: +"

---

## Two Solutions

### Solution 1: Fix the Parser (Add Infix Operators)

**Pros**:
- Natural syntax: `X? + Y?`
- Matches mathematical notation
- User-friendly

**Cons**:
- Complex parser changes
- Need operator precedence rules
- Need to handle associativity

**Implementation**:
1. Add infix operator tokenization
2. Build expression trees with precedence
3. Convert infix → prefix structures

### Solution 2: Rewrite GLP Files (Use Prefix Notation)

**Pros**:
- No parser changes needed
- Works with current implementation
- Matches Lisp/Scheme notation

**Cons**:
- Unnatural syntax for users
- Different from most languages

**Example Rewrite**:
```glp
% BEFORE (infix - doesn't parse)
add(X, Y, Z) :- execute('evaluate', [X? + Y?, Z]).

% AFTER (prefix - works now)
add(X, Y, Z) :- execute('evaluate', [+(X?, Y?), Z]).
```

---

## Detailed Examples

### Current Working Syntax (Prefix)

```glp
% Addition
add(X, Y, Z) :- execute('evaluate', [+(X?, Y?), Z]).

% Nested expression: (2 + 3) * 4
compute(R) :- execute('evaluate', [*(+(2, 3), 4), R]).

% Factorial: N * F1
fact(N, F) :-
  known(N), ground(N) |
  execute('evaluate', [-(N, one), N1]),
  fact(N1?, F1),
  execute('evaluate', [*(N, F1?), F]).
```

### Desired Syntax (Infix - Not Supported Yet)

```glp
% Addition with infix
add(X, Y, Z) :- execute('evaluate', [X? + Y?, Z]).

% Nested expression: (2 + 3) * 4
compute(R) :- execute('evaluate', [(2 + 3) * 4, R]).

% Factorial: N * F1
fact(N, F) :-
  known(N), ground(N) |
  execute('evaluate', [N - one, N1]),
  fact(N1?, F1),
  execute('evaluate', [N * F1?, F]).
```

---

## What the Runtime Expects

From the implementation (`system_predicates_impl.dart:232-296`):

```dart
switch (functor) {
  case '+':  // StructTerm('+', [left, right])
    return left + right;

  case '-':  // StructTerm('-', [left, right])
    return left - right;

  case '*':  // StructTerm('*', [left, right])
    return left * right;

  case '/':  // StructTerm('/', [left, right])
    return left / right;

  case 'mod':  // StructTerm('mod', [left, right])
    return left % right;
}
```

The runtime expects **structures with operator functors**, not infix syntax.

---

## Current Workaround: Rewrite Files

To make the existing programs work immediately:

### File: arithmetic.glp

```glp
% FIXED VERSION - Uses prefix notation

% Addition
add(X, Y, Z) :- execute('evaluate', [+(X?, Y?), Z]).

% Multiplication
multiply(X, Y, Z) :- execute('evaluate', [*(X?, Y?), Z]).

% Subtraction
subtract(X, Y, Z) :- execute('evaluate', [-(X?, Y?), Z]).

% Division
divide(X, Y, Z) :- execute('evaluate', [/(X?, Y?), Z]).

% Modulo
modulo(X, Y, Z) :- execute('evaluate', [mod(X?, Y?), Z]).

% Compound example
compute(X) :- execute('evaluate', [+(*(2, 3), 4), X]).  % (2*3)+4 = 10
```

### File: factorial.glp

```glp
% FIXED VERSION - Uses prefix notation

% Base case
fact(zero, one).

% Recursive case with guard
fact(N, F) :-
  known(N), ground(N) |
  execute('evaluate', [-(N, one), N1]),
  fact(N1?, F1),
  execute('evaluate', [*(N, F1?), F]).
```

---

## Testing the Fix

After rewriting to prefix notation, these should work:

```bash
dart glp_repl.dart <<EOF
arithmetic.glp
add(5, 3, X).
:quit
EOF
# Expected: X = 8

dart glp_repl.dart <<EOF
factorial.glp
fact(5, F).
:quit
EOF
# Expected: F = 120 (5! = 5*4*3*2*1)
```

---

## Recommendation

**Short-term**: Use Solution 2 (rewrite to prefix notation)
- Gets arithmetic working immediately
- No parser changes needed
- Can test factorial and other programs

**Long-term**: Consider Solution 1 (parser enhancement)
- Better user experience
- More natural syntax
- Requires significant parser work

**Immediate Action**: Create fixed versions of `arithmetic.glp` and `factorial.glp` with prefix notation.

---

## Parser Enhancement Design (Future)

If we decide to enhance the parser:

### Phase 1: Tokenization
```
X? + Y?  →  [VAR_READER(X), INFIX_OP(+), VAR_READER(Y)]
```

### Phase 2: Expression Parsing
```
[VAR_READER(X), INFIX_OP(+), VAR_READER(Y)]
  →  StructTerm('+', [VarRef(X, isReader: true), VarRef(Y, isReader: true)])
```

### Phase 3: Precedence
```
2 + 3 * 4  →  +(2, *(3, 4))   # * has higher precedence
(2 + 3) * 4  →  *(+(2, 3), 4)  # Parentheses override
```

This would require changes to:
- `lib/compiler/parser.dart` - Add infix operator parsing
- `lib/compiler/lexer.dart` - Tokenize operators in expressions
- `lib/compiler/expression_parser.dart` - Build expression trees

---

## Conclusion

**The evaluate/2 system predicate is fully working** ✅

**The parser needs enhancement OR the GLP programs need rewriting** ❌

For now, **rewrite to prefix notation** is the fastest path to working arithmetic programs.

**End of Analysis**
