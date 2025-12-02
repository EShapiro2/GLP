# GLP Arithmetic and Comparison Implementation Status Report

**Date**: 2025-11-12
**Commit**: 7d6c04c (after adding comparison operator support)

## Executive Summary

Arithmetic and comparison operations in GLP are **partially implemented** across the compilation pipeline. The lexer, parser, and analyzer are complete. The `evaluate/2` system predicate for expression evaluation is fully functional. **The critical missing piece is guard instruction execution in the bytecode runner** - comparison guards compile successfully but always fail at runtime.

---

## 1. Language-Level Support (COMPLETE ✅)

### 1.1 Lexer (lib/compiler/lexer.dart)

**Status**: ✅ **COMPLETE**

All arithmetic and comparison operators are tokenized correctly:

| Operator | Token Type | Status | Notes |
|----------|-----------|--------|-------|
| `+` | `PLUS` | ✅ Complete | Arithmetic addition |
| `-` | `MINUS` | ✅ Complete | Arithmetic subtraction / unary negation |
| `*` | `STAR` | ✅ Complete | Arithmetic multiplication |
| `/` | `SLASH` | ✅ Complete | Arithmetic division |
| `mod` | `MOD` | ✅ Complete | Modulo operator (keyword) |
| `<` | `LESS` | ✅ Complete | Less-than comparison |
| `>` | `GREATER` | ✅ Complete | Greater-than comparison |
| `=<` | `LESS_EQUAL` | ✅ Complete | Less-than-or-equal (Prolog convention) |
| `>=` | `GREATER_EQUAL` | ✅ Complete | Greater-than-or-equal |
| `=` | `EQUALS` | ✅ Complete | Equality test (in guards) |

**Implementation Details**:
- Multi-character operators (`>=`, `=<`) use lookahead scanning (lines 56-66)
- Negative number literals handled correctly (line 86-92)
- Unary minus for expressions handled in parser

### 1.2 Parser (lib/compiler/parser.dart)

**Status**: ✅ **COMPLETE**

Arithmetic expressions and comparison guards parse correctly with proper precedence:

**Expression Parsing** (Pratt parsing with precedence climbing):
- Precedence levels (line 355-373):
  - **Multiplicative** (`*`, `/`, `mod`): 20
  - **Additive** (`+`, `-`): 10
  - **Comparison** (`<`, `>`, `=<`, `>=`, `=`): 5 (lower than arithmetic)
- Unary minus: transforms `-X` to `neg(X)` structure (line 234-238)
- Infix expressions: `X + Y * Z` correctly parses as `+(X, *(Y, Z))`

**Guard Parsing** (special handling for infix comparisons):
- Function `_parseGoalOrGuard()` (line 104-149) handles both prefix and infix syntax
- Infix comparison syntax: `X? < Y?` transforms to `Goal('<', [X?, Y?])` (line 126-140)
- Uses `_parsePrimary()` to avoid expression parser consuming operators (critical fix)

**Examples of correct parsing**:
```prolog
X + Y * Z           → +(X, *(Y, Z))
A? < B?             → <(A?, B?)
evaluate(+(2,3), R) → evaluate(+(2,3), R)
```

### 1.3 Semantic Analyzer (lib/compiler/analyzer.dart)

**Status**: ✅ **COMPLETE**

SRSW (Single-Reader/Single-Writer) analysis correctly handles groundness from guards:

**Groundness Recognition** (line 212-254):

1. **Explicit groundness** (line 214-220):
   - `ground(X?)` - marks X as grounded

2. **Type-checking guards** (line 222-234) - **NEW**:
   - Predicates: `number`, `integer`, `float`, `atom`, `string`, `list`, `compound`, `var`, `nonvar`
   - Rationale: Type tests require bound values, which are ground by definition
   - Effect: Allows multiple reader occurrences after type check

3. **Comparison guards** (line 236-248):
   - Operators: `<`, `>`, `=<`, `>=`, `=:=`, `=\=`
   - Rationale: Comparisons require both operands to be bound numeric values
   - Effect: **Both operands marked as grounded**, allowing multiple reader occurrences

**Example SRSW Analysis**:
```prolog
% VALID - comparison guard makes X grounded
partition([X | Xs], A, [X? | Smaller?], Larger?) :-
    A? < X? |           % X and A marked grounded
    partition(Xs?, A?, Smaller, Larger).  % X? and A? allowed

% VALID - type check makes X grounded
qsort([X | Unsorted], Sorted?, Rest?) :-
    number(X?) |        % X marked grounded
    partition(Unsorted?, X?, Smaller, Larger),  % X? allowed
    qsort(Smaller?, Sorted, [X? | Sorted1?]).   % X? allowed again
```

---

## 2. Code Generation (COMPLETE ✅)

### 2.1 Guard Compilation (lib/compiler/codegen.dart)

**Status**: ✅ **COMPLETE**

Guards compile to bytecode instructions correctly:

**Special Built-ins** (line 371-396):
- `ground(X)` → `Ground(varIndex)` opcode
- `known(X)` → `Known(varIndex)` opcode
- `otherwise` → `Otherwise()` opcode

**Generic Guards** (line 398-405):
- Comparison guards (`<`, `>`, etc.) compile as:
  1. `PutArgument` instructions to set up arg registers
  2. `Guard(predicate, arity)` instruction

**Example bytecode for `A? < X?`**:
```
PutReader(varIndex_A, slot=0)    % Load A? into arg register 0
PutReader(varIndex_X, slot=1)    % Load X? into arg register 1
Guard('<', 2)                     % Call < guard with 2 arguments
```

### 2.2 Bytecode Opcodes (lib/bytecode/opcodes.dart)

**Status**: ✅ **COMPLETE**

All necessary opcodes are defined:

| Opcode | Purpose | Parameters | Status |
|--------|---------|-----------|--------|
| `Guard` | Generic guard call | `procedureLabel`, `arity` | ✅ Defined |
| `Ground` | Ground test | `varIndex` | ✅ Defined |
| `Known` | Known test | `varIndex` | ✅ Defined |
| `Execute` | System predicate call | `predicateName`, `args` | ✅ Defined |

---

## 3. Runtime Evaluation (PARTIAL ⚠️)

### 3.1 System Predicate: evaluate/2 (lib/runtime/system_predicates_impl.dart)

**Status**: ✅ **COMPLETE AND FUNCTIONAL**

The `evaluate/2` predicate correctly evaluates arithmetic expressions with three-valued semantics:

**Signature**: `evaluate(Expression, Result)`

**Three-valued semantics** (line 52-71):
1. **SUCCESS**: Expression is ground → evaluate and bind Result
2. **SUSPEND**: Expression contains unbound readers → suspend goal
3. **FAIL**: Expression contains unbound writers → fail (SRSW violation)

**Supported Operators** (line 256-366):

| Operator | Implementation | Return Type | Status |
|----------|---------------|-------------|--------|
| `+` | Binary addition | `num` | ✅ Complete |
| `-` | Binary subtraction | `num` | ✅ Complete |
| `*` | Binary multiplication | `num` | ✅ Complete |
| `/` | Binary division | `num` (double) | ✅ Complete (handles div-by-zero) |
| `mod` | Modulo | `int` | ✅ Complete (handles div-by-zero) |
| `<` | Less-than | `bool` | ✅ Complete |
| `>` | Greater-than | `bool` | ✅ Complete |
| `=<` | Less-or-equal | `bool` | ✅ Complete |
| `>=` | Greater-or-equal | `bool` | ✅ Complete |
| `=` | Equality | `bool` | ✅ Complete |

**Evaluation Logic** (line 219-368):
- Dereferences writers and readers recursively
- Handles `VarRef`, `ConstTerm`, `StructTerm` types
- Returns `null` for unbound variables or errors
- Deep evaluation: `+(*(2,3), 4)` correctly evaluates to `10`

**Example Usage**:
```prolog
% Works in BODY phase via execute/2:
evaluate(+(2, *(3, 4)), R).     % R = 14
evaluate(+(X?, 5), R).          % Suspends on X
```

### 3.2 Bytecode Runner: Guard Execution (lib/bytecode/runner.dart)

**Status**: ❌ **NOT IMPLEMENTED** (Critical Missing Piece)

**Current Behavior** (line 2012-2019):
```dart
if (op is Guard) {
  // Call guard predicate - not yet implemented
  // For now, soft-fail to next clause
  print('[WARN] Guard instruction not fully implemented - failing');
  _softFailToNextClause(cx, pc);
  pc = _findNextClauseTry(pc);
  continue;
}
```

**What This Means**:
- **ALL comparison guards fail immediately**, regardless of operand values
- Sort algorithms cannot work (insertion_sort, merge_sort, quicksort all fail)
- Any predicate using `<`, `>`, `=<`, `>=` guards will fail

**Example Failure**:
```prolog
% This clause ALWAYS fails, even when 1 < 2 is true:
insert(X, [Y | Ys], [X?, Y? | Ys?]) :-
    X? < Y? |    % ← Always fails with "[WARN] Guard instruction not fully implemented"
    true.
```

**Test Evidence**:
```
GLP> insertion_sort([2,1],X)
[WARN] Guard instruction not fully implemented - failing
[WARN] Guard instruction not fully implemented - failing
  X = <unbound>
  → 6 goals
```

### 3.3 Implemented Guard Instructions

**Status**: ✅ **PARTIAL** - Built-ins work, comparisons don't

| Instruction | Status | Notes |
|------------|--------|-------|
| `Ground` | ✅ Working | Three-valued: success/suspend/fail (line 2021-2100) |
| `Known` | ✅ Working | Tests if variable is bound |
| `Otherwise` | ✅ Working | Always succeeds |
| `Guard` (generic) | ❌ **NOT IMPLEMENTED** | **Critical missing piece** |

---

## 4. What Works vs. What Doesn't

### ✅ **WORKS** (Functional)

1. **Parsing arithmetic expressions**:
   ```prolog
   evaluate(+(2, *(3, 4)), R).   % Parses correctly
   ```

2. **Parsing comparison guards**:
   ```prolog
   insert(X, [Y|Ys], [X?|Ys?]) :- X? < Y? | true.  % Parses correctly
   ```

3. **SRSW analysis with groundness**:
   ```prolog
   % Accepted - comparison makes X grounded:
   partition([X|Xs], A, [X?|Smaller?], Larger?) :- A? < X? | body.
   ```

4. **evaluate/2 system predicate** (in BODY phase):
   ```prolog
   test(X) :- evaluate(+(2,3), X).  % X = 5 ✅
   test(X) :- evaluate(+(Y?,3), X). % Suspends on Y ✅
   ```

5. **Built-in guard predicates**:
   ```prolog
   test(X) :- ground(X?) | body.    % ✅ Works
   test(X) :- known(X?) | body.     % ✅ Works
   ```

### ❌ **DOESN'T WORK** (Broken)

1. **Comparison guards in clauses**:
   ```prolog
   % ALWAYS FAILS - guard not implemented:
   insert(X, [Y|Ys], [X?|Ys?]) :- X? < Y? | true.
   ```

2. **All sorting algorithms**:
   ```prolog
   % FAILS - relies on < guard:
   merge([X|Xs], [Y|Ys], [X?|Zs?]) :- X? < Y? | merge(Xs?, [Y?|Ys?], Zs).

   % FAILS - relies on < guard:
   insertion_sort([X|Xs], Sorted?) :- ...

   % FAILS - relies on number/1 type guard and < comparison:
   qsort([X|Unsorted], Sorted?, Rest?) :- number(X?) | ...
   ```

3. **Any custom predicate with comparison guards**:
   ```prolog
   % FAILS - guard not implemented:
   max(X, Y, X?) :- X? >= Y? | true.
   max(X, Y, Y?) :- X? < Y? | true.
   ```

---

## 5. Implementation Gaps and Recommendations

### 5.1 Critical Missing Implementation

**File**: `lib/bytecode/runner.dart`
**Location**: Lines 2012-2019
**Issue**: Guard instruction handler immediately fails

**What Needs to be Implemented**:

```dart
if (op is Guard) {
  // Extract guard predicate name and arity
  final predicateName = op.procedureLabel;
  final arity = op.arity;

  // Comparison guards should be handled specially
  final comparisonOps = ['<', '>', '=<', '>=', '=:=', '=\\='];

  if (comparisonOps.contains(predicateName) && arity == 2) {
    // 1. Read arguments from arg registers (A0, A1)
    final left = cx.argRegs[0];
    final right = cx.argRegs[1];

    // 2. Dereference to get actual values
    final leftValue = _derefValue(cx, left);
    final rightValue = _derefValue(cx, right);

    // 3. Check for unbound variables (three-valued semantics)
    final unboundReaders = _collectUnboundReaders(cx, [left, right]);

    if (unboundReaders.isNotEmpty) {
      // SUSPEND - add readers to Si, continue to next guard/body
      cx.si.addAll(unboundReaders);
      pc++;
      continue;
    }

    if (_hasUnboundWriters(cx, [left, right])) {
      // FAIL - soft-fail to next clause
      _softFailToNextClause(cx, pc);
      pc = _findNextClauseTry(pc);
      continue;
    }

    // 4. Both operands ground - perform comparison
    if (leftValue is num && rightValue is num) {
      bool result = false;
      switch (predicateName) {
        case '<':  result = leftValue < rightValue; break;
        case '>':  result = leftValue > rightValue; break;
        case '=<': result = leftValue <= rightValue; break;
        case '>=': result = leftValue >= rightValue; break;
        case '=:=': result = leftValue == rightValue; break;
        case '=\\=': result = leftValue != rightValue; break;
      }

      if (result) {
        // SUCCESS - continue to next instruction
        pc++;
      } else {
        // FAIL - soft-fail to next clause
        _softFailToNextClause(cx, pc);
        pc = _findNextClauseTry(pc);
      }
      continue;
    }

    // Type error - fail
    _softFailToNextClause(cx, pc);
    pc = _findNextClauseTry(pc);
    continue;
  }

  // Other guards (type checks, user-defined) - TODO
  // For now, fail
  print('[WARN] Non-comparison guard not implemented: $predicateName');
  _softFailToNextClause(cx, pc);
  pc = _findNextClauseTry(pc);
  continue;
}
```

### 5.2 Additional Guards to Implement

Once comparison guards work, implement type-checking guards:

| Guard | Implementation | Priority |
|-------|---------------|----------|
| `number(X)` | Check `is num` | HIGH (used in qsort) |
| `integer(X)` | Check `is int` | MEDIUM |
| `float(X)` | Check `is double` | MEDIUM |
| `atom(X)` | Check `is String` (non-numeric) | MEDIUM |
| `string(X)` | Check `is String` | LOW |
| `list(X)` | Check `is List` or `ListTerm` | LOW |
| `compound(X)` | Check `is StructTerm` | LOW |

**Implementation Pattern** (same three-valued semantics):
1. Check for unbound readers → SUSPEND (add to Si)
2. Check for unbound writers → FAIL (soft-fail)
3. Perform type test → SUCCESS/FAIL based on result

### 5.3 Testing Strategy

**Step 1**: Implement comparison guards, test with:
```prolog
% Simple comparison test
test_less :-
    X = 1, Y = 2,
    (X? < Y? | write('SUCCESS') ; write('FAIL')).

% Should print: SUCCESS
```

**Step 2**: Test with insertion sort (simplest algorithm):
```prolog
insertion_sort([3,1,2], Sorted).
% Expected: Sorted = [1,2,3]
```

**Step 3**: Test with merge and quicksort:
```prolog
merge_sort([3,1,4,1,5,9,2,6], Sorted).
quicksort([3,1,4,1,5,9,2,6], Sorted).
```

**Step 4**: Test three-valued semantics:
```prolog
% Should suspend on X:
test_suspend(X, Result) :-
    (3 < X? | Result = yes ; Result = no).

% Query: test_suspend(X, R).
% Expected: Goal suspended on readers: {X}
```

---

## 6. Dependency on Other Features

### 6.1 Prerequisites (Already Complete ✅)

- ✅ Argument registers (A0, A1, ...) for passing guard arguments
- ✅ Clause variables (cx.clauseVars) for storing dereferenced values
- ✅ Suspension set (cx.si) for tracking unbound readers
- ✅ Soft-fail mechanism (_softFailToNextClause) for guard failures
- ✅ Value dereferencing for writers and readers

### 6.2 No Blockers

**Guard execution can be implemented immediately** - all infrastructure is in place.

---

## 7. Summary and Next Steps

### Current State

| Component | Status | Completeness |
|-----------|--------|--------------|
| Lexer | ✅ Complete | 100% |
| Parser | ✅ Complete | 100% |
| Analyzer | ✅ Complete | 100% |
| Code Generation | ✅ Complete | 100% |
| Opcodes | ✅ Complete | 100% |
| evaluate/2 predicate | ✅ Complete | 100% |
| **Guard execution** | ❌ **Missing** | **0%** |

**Overall Arithmetic Support**: ~85% complete (missing only runtime guard execution)

### Immediate Next Step

**Implement Guard instruction handler in bytecode runner** (runner.dart:2012-2019):
1. Handle comparison operators (`<`, `>`, `=<`, `>=`) with three-valued semantics
2. Test with sort.glp sorting algorithms
3. Add type-checking guards (`number`, `atom`, etc.)
4. Test with more complex predicates

**Estimated Effort**: 2-4 hours (straightforward implementation, infrastructure exists)

**Impact**: Unlocks ALL sorting algorithms and any predicate using comparisons (high-value feature)

### Long-term Roadmap

1. **Phase 1** (Critical): Comparison guards → enables sorting ✅
2. **Phase 2** (Important): Type-checking guards → enables type-safe code
3. **Phase 3** (Nice-to-have): User-defined guards (if needed)
4. **Phase 4** (Future): Arithmetic constraints (advanced feature)

---

## 8. Code References

### Key Files

- **Lexer**: `/Users/udi/GLP/glp_runtime/lib/compiler/lexer.dart` (lines 53-66, 86-92)
- **Parser**: `/Users/udi/GLP/glp_runtime/lib/compiler/parser.dart` (lines 104-149, 217-373)
- **Analyzer**: `/Users/udi/GLP/glp_runtime/lib/compiler/analyzer.dart` (lines 212-254)
- **Codegen**: `/Users/udi/GLP/glp_runtime/lib/compiler/codegen.dart` (lines 369-405)
- **Opcodes**: `/Users/udi/GLP/glp_runtime/lib/bytecode/opcodes.dart` (lines 253-288)
- **System Predicates**: `/Users/udi/GLP/glp_runtime/lib/runtime/system_predicates_impl.dart` (lines 52-368)
- **Runner** (NEEDS WORK): `/Users/udi/GLP/glp_runtime/lib/bytecode/runner.dart` (lines 2011-2019)

### Test Files

- **Sort algorithms**: `/Users/udi/GLP/udi/glp/sort.glp`
- **Arithmetic tests**: (TODO - create comprehensive test suite)

---

**Report Generated**: 2025-11-12
**Author**: Claude Code
**Status**: DRAFT - Ready for implementation
