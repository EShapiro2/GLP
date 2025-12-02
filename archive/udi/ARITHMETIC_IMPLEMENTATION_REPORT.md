# Arithmetic Implementation Report for Claude Web

**Date**: 2025-11-12
**Context**: Implementation of infix arithmetic expression parsing for GLP
**Status**: Parser complete, codegen blocker identified

---

## Summary

We have successfully implemented infix arithmetic expression parsing (lexer + parser) according to the specification. The parser correctly transforms infix notation like `X? + Y?` into prefix structures `+(X?, Y?)`. However, we've hit a **codegen limitation** that prevents compilation of reader variables inside `execute()` argument expressions.

---

## What Was Completed

### 1. Specification Documents (✅ COMPLETE)

Updated 4 specification documents with comprehensive arithmetic support:

#### `docs/main_GLP_to_Dart (1).tex`
- Added "Arithmetic Expressions" section (33 lines) with full grammar
- Updated "Guards and system predicates" section with clear distinction
- Expanded "System Predicates" appendix with detailed evaluate/2 spec
- Added parser transformation rules and examples

#### `docs/glp-bytecode-v216-complete.md`
- Expanded evaluate/2 specification from 4 to 38 lines
- Added parser transformation rules table
- Added three-valued semantics details
- Added complete compilation examples

#### `docs/SPEC_GUIDE.md`
- Added major "Guards vs System Predicates" section (75 lines)
- Added "Arithmetic Expressions" subsection with full syntax
- Added "Migration Note for Existing Programs" explaining backward compatibility

#### `docs/parser-spec.md` (NEW - 260 lines)
- Complete grammar and precedence rules
- Transformation rules with examples
- Lexer token additions
- Parser implementation strategy (Pratt parsing)
- Type system and error handling
- Implementation checklist

### 2. Lexer Implementation (✅ COMPLETE)

**File**: `lib/compiler/token.dart`, `lib/compiler/lexer.dart`

**Added tokens**:
- `PLUS` (+)
- `MINUS` (-)
- `STAR` (*)
- `SLASH` (/)
- `MOD` (keyword)

**Key implementation details**:
- `-` handled contextually: negative number literal vs. binary/unary operator
- `mod` recognized as keyword, not atom
- All operators properly tokenized

### 3. Parser Implementation (✅ COMPLETE)

**File**: `lib/compiler/parser.dart`

**Implemented Pratt parsing** (precedence climbing):
- `_parseExpression()` - handles binary operators with precedence
- `_parsePrimary()` - handles unary minus, variables, numbers, parentheses
- `_isOperator()` - checks if token is arithmetic operator
- `_precedence()` - returns operator precedence (multiplicative=20, additive=10)
- `_operatorFunctor()` - maps token to functor name

**Precedence**:
1. Parentheses: `()`
2. Multiplicative: `*`, `/`, `mod`
3. Additive: `+`, `-`

**Transformation examples**:
- `X? + Y?` → `+(X?, Y?)`
- `(2 + 3) * 4` → `*(+(2, 3), 4)`
- `-X?` → `neg(X?)`

### 4. Test Programs Created

**File**: `udi/glp/arithmetic_fixed.glp`

Current version (following correct GLP pattern):
```prolog
% Addition
add(X, Y, Z?) :- execute('evaluate', [X? + Y?, Z]).

% Multiplication
multiply(X, Y, Z?) :- execute('evaluate', [X? * Y?, Z]).

% Compound example
compute(X?) :- execute('evaluate', [(2 * 3) + 4, X]).
```

**Pattern explanation**:
- `X, Y` - writers in head (inputs)
- `Z?` - reader in head (output parameter)
- `X?, Y?` - readers in body (read from head writers)
- `Z` - writer in body (bound by evaluate)

---

## Current Blocker: Codegen Limitation

### Error Message

```
Error loading arithmetic_fixed.glp: [codegen] Reader variables in execute() not yet supported: X?
Line 7, Column 39:
add(X, Y, Z?) :- execute('evaluate', [X? + Y?, Z]).
                                      ^
```

### Source Location

**File**: `lib/compiler/codegen.dart`
**Lines**: 482-485

```dart
if (term.isReader) {
  // Return reader term - but we need the writer ID to get the reader
  // This is tricky - for now, just note this limitation
  throw CompileError('Reader variables in execute() not yet supported: ${term.name}?',
                     term.line, term.column, phase: 'codegen');
}
```

### Root Cause

The `_termToValue()` method in codegen is used to convert AST terms to runtime values for `execute()` arguments. When it encounters a reader variable (`VarTerm` with `isReader: true`):

1. **Current behavior**: Throws error
2. **Comment indicates**: "we need the writer ID to get the reader" - "This is tricky"
3. **What's needed**: Return `rt.VarRef(varInfo.registerIndex!, isReader: true)`

### Why This is Needed

Looking at the structure traversal in line 508-519, the codegen already handles `StructTerm` (which is what `+(X?, Y?)` becomes after parsing). It recursively calls `_termToValue()` on the structure arguments. When it hits the reader variables `X?` and `Y?`, it fails.

### Correct Pattern (from working examples)

From `file_demo.glp` (which DOES work):
```prolog
read_file(Path, Content) :- execute('file_read', [Path?, Content]).
```

This works because `Path?` is NOT inside a nested structure - it's a direct argument to execute.

Our arithmetic case has:
```prolog
add(X, Y, Z?) :- execute('evaluate', [X? + Y?, Z]).
```

The `X? + Y?` is parsed to `+(X?, Y?)`, which is a **StructTerm** containing reader **VarTerms**. When codegen processes the struct arguments, it hits the readers and fails.

---

## Test Results

**REPL Test Suite**: 6/11 tests passing (54%)

**Passing** (6):
1. ✅ Hello World (hello.glp)
2. ✅ Simple Unification (p.glp)
3. ✅ Merge [1,2,3] and [a,b] (merge.glp)
4. ✅ Merge Standalone (merge_standalone.glp)
5. ✅ Clause Lookup (clause.glp)
6. ✅ Simple Run (run.glp)

**Failing** (5):
1. ❌ Merge with Reader (merge_with_reader.glp) - SRSW violation in test file
2. ❌ Addition 5+3 (arithmetic_fixed.glp) - **codegen blocker**
3. ❌ Multiplication 4*7 (arithmetic_fixed.glp) - **codegen blocker**
4. ❌ Compound (2*3)+4 (arithmetic_fixed.glp) - **codegen blocker**
5. ❌ Structure Demo (struct_demo.glp) - parser issue with numeric literals in structures

**Note**: 3 arithmetic tests fail with the same codegen error.

---

## Technical Analysis

### Parser Output (Verified Working)

The parser correctly transforms:
```
Input:  X? + Y?
Tokens: READER(X) PLUS READER(Y)
AST:    StructTerm('+', [VarTerm('X', isReader:true), VarTerm('Y', isReader:true)])
```

This is exactly the correct prefix representation per the spec.

### Codegen Flow

1. `_emitExecute()` processes execute goals
2. Extracts predicate name: `'evaluate'`
3. Calls `_termToValue()` on each argument in the list
4. For `+(X?, Y?)`:
   - Recognizes `StructTerm` (line 508)
   - Recursively calls `_termToValue()` on args `[X?, Y?]`
   - Hits `VarTerm` with `isReader: true` (line 482)
   - **THROWS ERROR** (line 485)

### Fix Required

Change line 485 from:
```dart
throw CompileError('Reader variables in execute() not yet supported: ${term.name}?', ...);
```

To:
```dart
return rt.VarRef(varInfo.registerIndex!, isReader: true);
```

**Why this should work**:
- The register index is already assigned during analysis
- `VarRef` with `isReader: true` is the correct runtime representation
- This is consistent with how writers are handled (line 488)
- The runtime `evaluate/2` predicate already handles reader VarRefs correctly (verified in `test/custom/execute_evaluate_test.dart`)

---

## Specification Discrepancy: Integer vs. Float Support

### Issue Discovered

There is a **discrepancy between the original GLP spec and the arithmetic implementation spec** regarding numeric type support:

**Original GLP Specification** (glp_spec.tex, main_GLP_to_Dart.tex line 720):
```
A number is a numeric string, which may include a decimal point,
e.g. 0, 1, 103.65.
```
This indicates **floating point support**.

**Arithmetic Specs Written by Claude Code** (all 4 updated docs):
- parser-spec.md: "**Integers only**: No floating point support in this phase"
- glp-bytecode-v216-complete.md: "**Type System**: Only integers supported (no floating point)"
- main_GLP_to_Dart (1).tex: "Only integers are supported (no floating point)"
- SPEC_GUIDE.md: "Type system: integers only (no floats)"

**Current Implementation** (system_predicates_impl.dart, original):
```dart
/// evaluate/2: Arithmetic evaluation
/// The expression can contain:
/// - Constants (integers, floats)
/// - Arithmetic operators (+, -, *, /, mod)
```
The original `evaluate/2` implementation comment says "integers, floats".

**Current Lexer** (lexer.dart line 115):
```dart
final value = text.contains('.') ? double.parse(text) : int.parse(text);
```
The lexer **already parses both integers and floats**.

### Decision Needed

**Which specification is correct?**

1. **Integers only** (as Claude Code specified in arithmetic docs)
   - Simpler semantics
   - Avoids floating point precision issues
   - Division is clearly integer division
   - Modulo only makes sense for integers

2. **Integers and floats** (as original GLP spec and current implementation suggest)
   - More general
   - Lexer already supports it
   - Original evaluate/2 comment mentions floats
   - Matches standard mathematical expectations

### Current State

- **Lexer**: Supports both (returns int or double)
- **Parser**: Type-agnostic (works with any numeric literal)
- **Codegen**: Type-agnostic (passes through numeric values)
- **Runtime evaluate/2**: Uses Dart's `num` type (supports both int and double)
- **Specs written by Claude Code**: Say "integers only"

### Recommendation

**Clarify the intended type system** before finalizing the arithmetic implementation:
- If **integers only**: Update evaluate/2 to reject floats, document why
- If **integers and floats**: Update the 4 spec docs to remove "integers only" restriction

This affects:
- Division semantics (integer division vs. float division)
- Modulo semantics (only defined for integers)
- Type checking in evaluate/2
- User expectations

---

## Questions for Claude Web

### 1. Is the codegen fix straightforward?

Can we simply change line 485 to:
```dart
return rt.VarRef(varInfo.registerIndex!, isReader: true);
```

Or is there a deeper issue the comment "we need the writer ID to get the reader" is referring to?

### 2. Single-ID system consideration

In the single-ID variable system:
- Writer X has ID N
- Reader X? also has ID N (with isReader flag)

So `varInfo.registerIndex!` for a reader should already be the correct ID. Is this assumption correct?

### 3. Alternative pattern?

Is there a different GLP coding pattern we should use for arithmetic that avoids readers in execute() arguments? For example:

```prolog
% Current (blocked):
add(X, Y, Z?) :- execute('evaluate', [X? + Y?, Z]).

% Alternative?
add(X, Y, Z?) :-
  Expr = X? + Y?,
  execute('evaluate', [Expr?, Z]).
```

Would this help, or would it have the same issue?

---

## Recommendation

**Implement the codegen fix** to support readers in execute() argument expressions:

1. **Change**: Line 485 in `lib/compiler/codegen.dart`
2. **From**: `throw CompileError(...)`
3. **To**: `return rt.VarRef(varInfo.registerIndex!, isReader: true);`
4. **Test**: Run `dart test test/custom/execute_evaluate_test.dart` (should still pass)
5. **Test**: Run `./run_repl_tests.sh` (should reach 9/11 or 10/11 passing)

This is a simple, one-line fix that aligns with how the rest of the codegen handles readers and with how the runtime already processes VarRefs.

---

## Files to Share with Claude Web

**Minimal context**:
1. This report (ARITHMETIC_IMPLEMENTATION_REPORT.md)
2. `lib/compiler/codegen.dart` (lines 470-530)
3. `udi/glp/arithmetic_fixed.glp`

**Full context**:
1. All spec documents in `docs/` (updated with arithmetic)
2. `lib/compiler/lexer.dart`, `lib/compiler/parser.dart`, `lib/compiler/token.dart`
3. `lib/compiler/codegen.dart`
4. `test/custom/execute_evaluate_test.dart` (shows evaluate works)
5. `udi/glp/arithmetic_fixed.glp`
6. This report

---

## Implementation Timeline

- ✅ Specs written (2 hours)
- ✅ Lexer implementation (30 min)
- ✅ Parser implementation (1 hour)
- ⏸️ **Blocked on codegen** (awaiting architectural guidance)
- ⏳ Testing (30 min after unblock)
- ⏳ Commit and baseline (15 min after tests pass)

**Estimated time to complete after codegen fix**: 45 minutes

---

## End of Report

**Next action**: Await Claude Web's guidance on codegen fix approach.
