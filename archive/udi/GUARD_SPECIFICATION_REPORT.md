# Guard Predicate Specification - Complete Documentation Update

**Date**: 2025-11-12
**Status**: Complete
**Purpose**: Comprehensive guard predicate specification added to all documentation files

## Summary

This report documents the addition of complete guard predicate specifications to all GLP documentation files, including:
- Type guards (ground, known, integer, number, writer, reader)
- Arithmetic comparison guards (<, =<, >, >=, =:=, =\=)
- Unification guards (=, \=)
- Control guards (otherwise, true)
- Lexer/parser integration requirements
- Operator precedence and associativity
- Implementation examples

## Files Updated

### 1. `/Users/udi/GLP/docs/glp-bytecode-v216-complete.md`

**Section Added**: Section 19 - Guard Predicates

**Content Added**:
- **19.1 Guard Execution Model**: Three-valued semantics, execution during HEAD/GUARDS phase
- **19.2 Arithmetic Expression Evaluation in Guards**: How expressions are evaluated
- **19.3 Arithmetic Comparison Guards**:
  - `guard_less` (X < Y)
  - `guard_greater` (X > Y)
  - `guard_less_equal` (X =< Y)
  - `guard_greater_equal` (X >= Y)
  - `guard_arith_equal` (X =:= Y)
  - `guard_arith_not_equal` (X =\= Y)
- **19.4 Type Guards**:
  - `guard_ground(X)` - test if X contains no unbound variables
  - `guard_known(X)` - test if X is bound
  - `guard_integer(X)` - test if X is integer
  - `guard_number(X)` - test if X is number
  - `guard_writer(X)` - test if X is writer variable
  - `guard_reader(X)` - test if X is reader variable
- **19.5 Control Guards**:
  - `guard_true` - always succeeds
  - `guard_otherwise` - succeeds if previous clauses failed
- **19.6 Unification Guards**:
  - `guard_unify(X, Y)` - test if X and Y can unify
  - `guard_not_unifiable(X, Y)` - test if X and Y cannot unify
- **19.7 Lexer/Parser Integration**: Complete token table with operator precedence
- **19.8 Guards vs. System Predicates**: Comparison table showing differences
- **19.9 Compilation Example**: Complete qsort program with guard compilation
- **19.10 Implementation Requirements**: Detailed requirements for implementation
- **19.11 Implementation Status**: Current status of each guard type

**Lines Added**: ~450 lines

### 2. `/Users/udi/GLP/docs/SPEC_GUIDE.md`

**Sections Updated**:

**Guards vs System Predicates Section** (lines 125-303):
- Reorganized guard categories with subsections
- **Type Guards**: Added writer(X), reader(X) to planned guards
- **Arithmetic Comparison Guards**: Complete list with semantics
  - Precedence and associativity table
  - Example quicksort with comparison guards
- **Control Guards**: otherwise (implemented), true (planned)
- **Unification Guards**: X = Y, X \= Y (planned)
- **Lexer/Parser Integration**:
  - Token definitions for all comparison operators
  - Operator precedence table (1200 down to 200)
  - Parser rules for guard production
  - Precedence handling notes (non-associative comparisons)

**Key Additions**:
- Comprehensive operator precedence table
- Parser rule extensions for guard expressions
- Token definitions with Dart code examples
- Implementation requirements for lexer/parser

**Lines Added**: ~160 lines

### 3. `/Users/udi/GLP/docs/parser-spec.md`

**Section Updated**: Guard Expressions (lines 222-433)

**Content Replaced and Expanded**:
- Complete guard categorization (Type, Comparison, Unification, Control)
- **Lexer Token Additions**: Full TokenType enum with all comparison operators
- **Lexer Implementation**: Complete code for multi-character operator scanning
- **Operator Precedence (Extended)**: Complete table from 1200 to 200
- **Parser Extension**: `_parseGoalOrGuard()` method with infix handling
- **Transformation Examples**: Table showing Source → AST → Bytecode mapping
- **Guard Examples**:
  - Type guards (working)
  - Comparison guards (requires parser extension) - quicksort example
  - Unification guards (planned)
- **Implementation Status**: Clear checklist of what's done vs. what's needed
- **Three-Valued Semantics**: Detailed explanation with safe_divide example

**Lines Added**: ~210 lines

### 4. `/Users/udi/GLP/docs/main_GLP_to_Dart (1).tex`

**Sections Updated**:

**Main Text** (line 352):
- Updated guard summary to include all planned guard categories
- Comprehensive listing: type, comparison, unification, control guards

**Appendix Section** (lines 884-947):
- **Planned Type Guards**: number, integer, writer, reader
- **Planned Arithmetic Comparison Guards**:
  - Complete list with semantics
  - Parser extension requirements noted
- **Planned Unification Guards**: =, \=
- **Planned Control Guards**: true
- **Guard Syntax and Semantics**:
  - Transformation table (Source → AST → Bytecode)
  - Complete for all comparison operators
- **Operator Precedence**: Table in LaTeX verbatim format
- **Implementation Requirements**: Lexer, parser, and semantic requirements
- **Example with Comparison Guards**: Complete quicksort implementation

**Lines Added**: ~65 lines

## Guard Categories Specified

### Type Guards

| Guard | Semantics | Status |
|-------|-----------|--------|
| `ground(X)` | Tests if X contains no unbound variables | ✅ Implemented |
| `known(X)` | Tests if X is bound (not unbound variable) | ✅ Implemented |
| `integer(X)` | Tests if X is bound to integer | ⏳ Planned |
| `number(X)` | Tests if X is bound to number | ⏳ Planned |
| `writer(X)` | Tests if X is writer variable | ⏳ Planned |
| `reader(X)` | Tests if X is reader variable | ⏳ Planned |

### Arithmetic Comparison Guards

| Guard | Operator | Semantics | Status |
|-------|----------|-----------|--------|
| `guard_less` | `<` | Less than | ⏳ Planned |
| `guard_less_equal` | `=<` | Less than or equal (Prolog convention) | ⏳ Planned |
| `guard_greater` | `>` | Greater than | ⏳ Planned |
| `guard_greater_equal` | `>=` | Greater than or equal | ⏳ Planned |
| `guard_arith_equal` | `=:=` | Arithmetic equality | ⏳ Planned |
| `guard_arith_not_equal` | `=\=` | Arithmetic inequality | ⏳ Planned |

### Unification Guards

| Guard | Operator | Semantics | Status |
|-------|----------|-----------|--------|
| `guard_unify` | `=` | Test if terms can unify | ⏳ Planned |
| `guard_not_unifiable` | `\=` | Test if terms cannot unify | ⏳ Planned |

### Control Guards

| Guard | Semantics | Status |
|-------|-----------|--------|
| `otherwise` | Succeeds if previous clauses failed | ✅ Implemented |
| `true` | Always succeeds | ⏳ Planned |

## Operator Precedence Specification

Complete precedence table specified across all documents:

```
1200  :- (rule separator)
1100  | (guard separator)
 700  < =< > >= =:= =\= = \= (comparison/test, non-associative)
 500  + - (additive, left-associative)
 400  * / mod (multiplicative, left-associative)
 200  - (unary minus, non-associative)
```

**Key Properties**:
- Comparison operators are **non-associative** (expressions like `X < Y < Z` are rejected)
- Guard separator `|` binds less tightly than all comparison operators
- Arithmetic operators have standard precedence (PEMDAS)

## Lexer/Parser Integration Requirements

### Lexer Token Additions

New tokens required:
```dart
enum TokenType {
  // Comparison operators (precedence 700, non-associative)
  LESS,           // <
  LESS_EQUAL,     // =< (Prolog convention, NOT <=)
  GREATER,        // >
  GREATER_EQUAL,  // >=
  ARITH_EQUAL,    // =:=
  ARITH_NOT_EQUAL,// =\=

  // Unification guards (precedence 700)
  UNIFY,          // =
  NOT_UNIFIABLE,  // \=
}
```

### Multi-Character Operator Scanning

Lexer must handle:
- `=<` (not `<=`)
- `>=`
- `=:=` (three characters)
- `=\=` (three characters)
- `\=` (two characters)

### Parser Extension

Required method updates:
```dart
Goal _parseGoalOrGuard() {
  final left = _parseTerm();

  if (_isComparisonOp(_current)) {
    final op = _advance();
    final right = _parseTerm();
    return Atom(op.lexeme, [left, right]);  // Transform infix to prefix
  }

  return left as Atom;
}
```

## Transformation Pipeline

Complete transformation pipeline specified:

**Source Code**:
```prolog
partition(Pivot, [X | Xs?], [X | Smaller], Greater) :-
    X? < Pivot? |
    partition(Pivot?, Xs?, Smaller, Greater).
```

**Parser AST**:
```dart
Clause(
  head: Atom('partition', [...]),
  guards: [Atom('<', [VarRef(X, isReader:true), VarRef(Pivot, isReader:true)])],
  body: [...]
)
```

**Bytecode**:
```
clause_try partition_clause_2
<HEAD instructions>
guard_less 0 1  // Compare clauseVars[0] < clauseVars[1]
commit
<BODY instructions>
```

## Implementation Examples

### Quicksort with Comparison Guards

Complete example provided in all documentation:

```prolog
% Quicksort with comparison guards
quick_sort([], []).
quick_sort([Pivot | Rest?], Sorted) :-
    partition(Pivot?, Rest?, Smaller, Greater),
    quick_sort(Smaller?, SortedSmaller),
    quick_sort(Greater?, SortedGreater),
    append(SortedSmaller?, [Pivot | SortedGreater?]?, Sorted).

partition(Pivot, [], [], []).
partition(Pivot, [X | Xs?], [X | Smaller], Greater) :-
    X? < Pivot? |
    partition(Pivot?, Xs?, Smaller, Greater).
partition(Pivot, [X | Xs?], Smaller, [X | Greater]) :-
    X? >= Pivot? |
    partition(Pivot?, Xs?, Smaller, Greater).
```

### Safe Division Pattern

Example showing guards ensuring preconditions:

```prolog
safe_divide(X, Y, Z) :-
    number(X), number(Y), Y =\= 0 |
    execute('evaluate', [X? / Y?, Z]).
```

## Three-Valued Semantics

Detailed semantics specified across all documents:

**Success**: Condition holds, continue with clause body
- Example: `5 < 10` succeeds

**Suspend**: Unbound reader encountered, add to Si suspension set
- Example: `X? < 10` suspends if X unbound

**Fail**: Condition does not hold, try next clause
- Example: `10 < 5` fails

## Guards vs. System Predicates

Complete comparison table added to bytecode spec:

| Aspect | Guards | System Predicates (via execute) |
|--------|--------|----------------------------------|
| **Semantics** | Three-valued (success/suspend/fail) | Two-valued (success/abort) |
| **Execution Phase** | HEAD/GUARDS (before commit) | BODY (after commit) |
| **Side Effects** | Never | May have (I/O, etc.) |
| **Unbound Readers** | Suspend (patient) | Abort (impatient) |
| **Syntax** | After head, before `\|` | In body via `execute/2` |

## Documentation Consistency

All four documentation files now have:
- ✅ Consistent guard categorization (Type, Comparison, Unification, Control)
- ✅ Consistent operator precedence table
- ✅ Consistent transformation pipeline (Source → AST → Bytecode)
- ✅ Consistent lexer/parser requirements
- ✅ Consistent implementation status markers (✅, ⏳)
- ✅ Consistent quicksort example
- ✅ Consistent three-valued semantics explanation

## Implementation Status

### Currently Implemented
- ✅ `ground(X)` - ground test guard
- ✅ `known(X)` - bound test guard
- ✅ `otherwise` - default clause guard
- ✅ Guard infrastructure (AST, codegen, runner)
- ✅ Guard separator `|` parsing

### Requires Implementation
- ⏳ Lexer: Multi-character comparison operator tokens
- ⏳ Parser: Infix syntax in guard position
- ⏳ Type guards: integer, number, writer, reader
- ⏳ Comparison guards: <, =<, >, >=, =:=, =\=
- ⏳ Unification guards: =, \=
- ⏳ Control guards: true

## Next Steps

To implement comparison guards:

1. **Lexer Changes** (1-2 hours):
   - Add tokens: LESS, LESS_EQUAL, GREATER, GREATER_EQUAL, ARITH_EQUAL, ARITH_NOT_EQUAL, UNIFY, NOT_UNIFIABLE
   - Implement multi-character operator scanning
   - Test with guard expressions

2. **Parser Changes** (2-3 hours):
   - Extend `_parseGoalOrGuard()` to handle infix comparison
   - Add `_isComparisonOp()` helper
   - Transform infix to prefix: `X < Y` → `Atom('<', [X, Y])`
   - Test with guard clauses

3. **Codegen Changes** (1 hour):
   - Map guard atoms to guard instructions
   - `Atom('<', [X, Y])` → `guard_less(X_slot, Y_slot)`
   - Handle all comparison operators

4. **Runner Implementation** (3-4 hours):
   - Implement `guard_less`, `guard_greater`, etc.
   - Evaluate arithmetic expressions in guards
   - Return three-valued result (SUCCESS/SUSPEND/FAIL)
   - Add to Si on suspend

5. **Testing** (2-3 hours):
   - Unit tests for each guard type
   - Integration tests with quicksort, merge sort, etc.
   - REPL tests with comparison guards
   - Edge cases: unbound readers, type errors

**Total Estimated Effort**: 10-14 hours

## References

All documentation now references:
- WAM paper (structure traversal, precedence)
- FCP implementation (guard evaluation, suspension)
- Prolog convention (=< not <=)
- glp_spec.pdf (three-valued semantics)

## Conclusion

The guard predicate specification is now comprehensive and consistent across all GLP documentation:
- **glp-bytecode-v216-complete.md**: Complete instruction set for guards
- **SPEC_GUIDE.md**: Runtime architecture and guard execution model
- **parser-spec.md**: Lexer/parser implementation requirements
- **main_GLP_to_Dart.tex**: Formal specification and examples

All planned guard types are now documented with:
- Clear semantics (three-valued: success/suspend/fail)
- Transformation pipeline (source → AST → bytecode)
- Implementation requirements (lexer, parser, codegen, runner)
- Complete examples (quicksort, safe division)
- Operator precedence and associativity

The specification is **ready for implementation** with clear guidance for each phase.

---

**Report Generated**: 2025-11-12
**Files Modified**: 4
**Lines Added**: ~885
**Status**: ✅ Complete
