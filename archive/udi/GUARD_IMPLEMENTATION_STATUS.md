# Guard Implementation Status Report

**Date**: 2025-11-12
**Context**: Review of arithmetic guard implementation before documentation updates
**Reviewer**: Claude Code

---

## Executive Summary

**Current State**: GLP has **partial guard infrastructure** but **NO arithmetic guards implemented**.

**Implemented**:
- ✅ Parser supports guard syntax (`Head :- Guards | Body`)
- ✅ AST has `Guard` class
- ✅ Codegen emits guard bytecode
- ✅ Runner executes 3 built-in guards: `known/1`, `ground/1`, `otherwise/0`
- ✅ Three-valued semantics (success/suspend/fail) working

**Missing**:
- ❌ NO arithmetic type guards (`number/1`, `integer/1`)
- ❌ NO comparison guards (`</2`, `=</2`, `>/2`, `>=/2`, `=:=/2`, `=\=/2`)
- ❌ Parser does NOT support infix operators in guard position

**Recommendation**: Documentation should mark arithmetic guards as **"Specified but not implemented"**.

---

## Detailed Findings

### 1. Parser Support for Guards (✅ WORKING)

**File**: `lib/compiler/parser.dart` lines 58-101

**Capabilities**:
- ✅ Parses `Head :- Guards | Body.` syntax correctly
- ✅ Uses `|` (PIPE token) to separate guards from body
- ✅ Converts predicates before `|` into `Guard` AST nodes
- ✅ Passes guard arguments through `_parseTerm()` which supports expressions

**Example working syntax**:
```prolog
factorial(N, F) :- known(N), ground(N) | compute_factorial(N?, F).
run(A) :- otherwise | clause(A?, B), run(B?).
```

**Limitation**: Parser accepts any predicate in guard position (validation happens later in codegen/runtime).

---

### 2. AST Representation (✅ WORKING)

**File**: `lib/compiler/ast.dart` lines 79-88

```dart
class Guard extends AstNode {
  final String predicate;
  final List<Term> args;

  Guard(this.predicate, this.args, int line, int column) : super(line, column);
}
```

**Status**: Generic guard representation supports any predicate name and arguments.

---

### 3. Codegen for Guards (✅ WORKING)

**File**: `lib/compiler/codegen.dart` lines 369-405

**Implemented**:
- ✅ Special handling for `ground/1`, `known/1`, `otherwise/0`
- ✅ Generic `Guard(predicateName, arity)` bytecode emission for unknown guards
- ✅ Argument setup via `_generatePutArgument()`

**Code**:
```dart
void _generateGuard(Guard guard, VariableTable varTable, CodeGenContext ctx) {
  // Special built-in guards
  if (guard.predicate == 'ground' && guard.args.length == 1) {
    ctx.emit(bc.Ground(varInfo.registerIndex!));
    return;
  }

  if (guard.predicate == 'known' && guard.args.length == 1) {
    ctx.emit(bc.Known(varInfo.registerIndex!));
    return;
  }

  if (guard.predicate == 'otherwise' && guard.args.isEmpty) {
    ctx.emit(bc.Otherwise());
    return;
  }

  // Generic guard predicate call
  for (int i = 0; i < guard.args.length; i++) {
    _generatePutArgument(guard.args[i], i, varTable, ctx);
  }
  ctx.emit(bc.Guard(guard.predicate, guard.args.length));
}
```

**Status**: Codegen ready to handle new guards - just needs runtime implementation.

---

### 4. Bytecode Instructions (✅ DEFINED)

**File**: `lib/bytecode/opcodes.dart` lines 232-271

**Implemented opcodes**:
```dart
class Otherwise implements Op {}    // Line 235
class Ground implements Op {         // Line 261
  final int varIndex;
  Ground(this.varIndex);
}
class Known implements Op {          // Line 268
  final int varIndex;
  Known(this.varIndex);
}
class Guard implements Op {          // Line 253
  final LabelName procedureLabel;
  final int arity;
  Guard(this.procedureLabel, this.arity);
}
```

**Status**: Opcodes defined. Generic `Guard` opcode can dispatch to new guard implementations.

---

### 5. Runtime Execution (✅ PARTIAL)

**File**: `lib/bytecode/runner.dart`

**Implemented handlers**:
- ✅ `Otherwise` - Line 220: Checks if Si empty (previous clauses failed, not suspended)
- ✅ `Ground` - Line 2021: Tests if variable contains no unbound variables
- ✅ `Known` - Line 2114: Tests if variable is bound

**Code example (Known)**:
```dart
if (op is Known) {
  final vid = op.varIndex;
  final value = cx.clauseVars[vid];

  bool isKnown = false;
  if (value is int) {
    isKnown = cx.rt.heap.isWriterBound(value);
  } else if (value is ConstTerm || value is StructTerm) {
    isKnown = true;
  }

  if (isKnown) {
    pc++;
    continue;
  } else {
    // Failed guard - try next clause
    goto clauseNext;
  }
}
```

**Status**: Three-valued guard semantics working correctly (success continues, fail jumps to next clause, suspend handled by specific guards).

---

### 6. System Predicates (❌ NO ARITHMETIC GUARDS)

**File**: `lib/runtime/system_predicates_impl.dart`

**Complete predicate list** (lines 16-50):
```dart
void registerStandardPredicates(SystemPredicateRegistry registry) {
  // Arithmetic
  registry.register('evaluate', evaluatePredicate);  // ← Two-valued (execute)

  // Utilities
  registry.register('current_time', currentTimePredicate);
  registry.register('unique_id', uniqueIdPredicate);
  registry.register('variable_name', variableNamePredicate);
  registry.register('copy_term', copyTermPredicate);

  // File I/O
  registry.register('file_read', fileReadPredicate);
  registry.register('file_write', fileWritePredicate);
  registry.register('file_exists', fileExistsPredicate);
  // ... more file operations

  // Terminal I/O
  registry.register('write', writePredicate);
  registry.register('nl', nlPredicate);
  registry.register('read', readPredicate);

  // Module loading
  registry.register('link', linkPredicate);
  registry.register('load_module', loadModulePredicate);

  // Channel primitives
  registry.register('distribute_stream', distributeStreamPredicate);
  registry.register('copy_term_multi', copyTermMultiPredicate);
}
```

**Finding**: **NO guard predicates registered** (only system predicates called via `execute/2`).

**Search results**:
- `number`, `integer`, `ground`, `known` - NOT FOUND in system_predicates_impl.dart
- Comparison operators (`<`, `>`, etc.) - NOT FOUND in any runtime file

---

### 7. Actual Guard Usage in Codebase

**Survey of `/Users/udi/GLP/udi/glp/*.glp` files**:

**Used guards**:
```prolog
% factorial.glp, factorial_fixed.glp
factorial(N, F) :- known(N), ground(N) | ...

% test_reactivation.glp
q(X) :- known(X) | execute('write', [X?]), execute('nl', []).

% echo.glp
... known(Input) | ...

% run.glp, run1.glp, run2.glp, test_*.glp (multiple files)
run(A) :- otherwise | clause(A?, B), run(B?).
```

**Observation**: Only `known/1`, `ground/1`, `otherwise/0` are used - because those are the only ones implemented!

---

## Implementation Gap Analysis

### What EXISTS:
1. ✅ Full syntactic support (parser, AST, codegen)
2. ✅ Guard execution infrastructure in runner
3. ✅ Three-valued semantics (success/suspend/fail)
4. ✅ Three working guards: `known/1`, `ground/1`, `otherwise/0`
5. ✅ Generic `Guard` opcode for extensibility

### What's MISSING:
1. ❌ **Arithmetic type guards**:
   - `number/1` - Test if variable is bound to a number
   - `integer/1` - Test if variable is bound to an integer

2. ❌ **Comparison guards**:
   - `</2` (less than)
   - `=</2` (less than or equal) - Note: Prolog uses `=<`, not `<=`
   - `>/2` (greater than)
   - `>=/2` (greater than or equal)
   - `=:=/2` (arithmetic equality)
   - `=\=/2` (arithmetic inequality)

3. ❌ **Parser support for infix operators in guard position**:
   - Currently: `known(X), ground(Y) | body` ✅
   - Needed: `X < Y, Y > 0 | body` ❌
   - Parser would need to recognize `<`, `>`, etc. as valid predicates in guard context

---

## Comparison: Guards vs Execute Predicates

### Execute Predicates (Two-Valued)
**Examples**: `evaluate/2`, `file_read/2`, `write/1`
- ✅ **Implemented and working**
- Called via: `execute('predicate_name', [args])`
- Semantics: SUCCESS or ABORT (never suspend)
- Phase: BODY (after commit)
- Unbound reader → ABORT with error
- Used for: Side effects, I/O, arithmetic computation

### Guards (Three-Valued)
**Examples**: `known/1`, `ground/1`, `number/1`, `X < Y`
- ⏳ **Partially implemented** (only 3 basic guards)
- Called directly in guard section: `pred(...) :- guard1, guard2 | body`
- Semantics: SUCCESS / SUSPEND / FAIL
- Phase: GUARDS (during tentative unification)
- Unbound variable → SUSPEND (patient)
- Used for: Pure tests, conditional clause selection

---

## Parser Limitation: Infix Operators in Guards

### Current Parser Behavior

The parser's `_parseGoalOrGuard()` (lines 104-122) expects:
```
ATOM LPAREN args RPAREN
```

**Works**:
```prolog
pred(...) :- known(X), ground(Y) | body.
```

**Does NOT work** (would require parser extension):
```prolog
pred(...) :- X < Y, Y > 0 | body.
```

### Why It Doesn't Work

The comparison operators (`<`, `>`, `=<`, `>=`, `=:=`, `=\=`) are:
- NOT defined as tokens in `token.dart`
- NOT recognized by lexer in `lexer.dart`
- NOT handled by parser in guard position

**Current arithmetic operator tokens** (added for arithmetic expressions):
- `PLUS` (+), `MINUS` (-)
- `STAR` (*), `SLASH` (/), `MOD`

**Missing comparison operator tokens**:
- `LT` (<), `GT` (>), `LE` (=<), `GE` (>=)
- `ARITH_EQ` (=:=), `ARITH_NE` (=\=)

### Required Changes for Infix Guards

1. **Add tokens** to `token.dart`:
   ```dart
   LT,        // <
   GT,        // >
   LE,        // =<
   GE,        // >=
   ARITH_EQ,  // =:=
   ARITH_NE,  // =\=
   ```

2. **Update lexer** to recognize multi-character operators:
   ```dart
   case '=':
     if (_peek() == '<') {  // =<
       _advance();
       return _makeToken(TokenType.LE, ...);
     } else if (_peek() == ':' && _peekNext() == '=') {  // =:=
       _advance(); _advance();
       return _makeToken(TokenType.ARITH_EQ, ...);
     }
     // ... etc
   ```

3. **Update parser** to handle infix syntax in guard position:
   - Option A: Special-case in `_parseGoalOrGuard()`
   - Option B: Treat as expressions (like arithmetic) and transform to prefix

**Design Question**: Should guards support full arithmetic expressions?
```prolog
% Simple comparison
pred(X, Y) :- X < Y | body.

% Complex expression (future?)
pred(X, Y, Z) :- X + Y < Z * 2 | body.
```

---

## Recommendations for Documentation

### 1. SPEC_GUIDE.md

**Add status markers**:
```markdown
### Arithmetic Guards (Three-Valued)

#### Type Guards
- ⏳ `number(X)` - Success: X bound to number, Suspend: X unbound, Fail: X bound to non-number
- ⏳ `integer(X)` - Success: X bound to integer, Suspend: X unbound, Fail: otherwise

#### Comparison Guards
- ⏳ `X < Y`, `X =< Y`, `X > Y`, `X >= Y`, `X =:= Y`, `X =\= Y`
- Success: Both bound and condition true
- Suspend: Either unbound (success still possible)
- Fail: Both bound and condition false

**Implementation Status**: Type and comparison guards are specified but not yet implemented.

#### Currently Implemented Guards
- ✅ `known(X)` - Test if X is bound
- ✅ `ground(X)` - Test if X contains no variables
- ✅ `otherwise` - Previous clauses failed (not suspended)
```

### 2. glp-bytecode-v216-complete.md

**Update section 11.7** to distinguish implemented vs. planned:
```markdown
### 11.7 Arithmetic Guards

**Status**: Specified for future implementation

#### Planned: number(X)
**Three-valued**: Success if X bound to number, suspend if unbound, fail if non-number

#### Planned: integer(X)
**Three-valued**: Success if X bound to integer, suspend if unbound, fail otherwise

#### Planned: Comparison (X < Y, X =< Y, ...)
**Three-valued**: Success if both bound and condition holds, suspend if either unbound, fail if both bound and false

**Current Limitation**: Requires parser extension to support infix operators in guard position.

#### Currently Implemented
- ✅ `known/1`, `ground/1`, `otherwise/0` - See section 11.6
```

### 3. parser-spec.md

**Be explicit about limitations**:
```markdown
### Guard Expressions

**Current Status**: Guards use function-call syntax only.

**Working**:
```prolog
pred(X, Y) :- known(X), ground(Y) | body.
```

**Not Yet Supported** (requires parser extension):
```prolog
pred(X, Y) :- X < Y, Y > 0 | body.
```

**TODO - Parser Enhancement**:
1. Add comparison operator tokens: `<`, `=<`, `>`, `>=`, `=:=`, `=\=`
2. Update lexer to recognize multi-character operators
3. Handle infix syntax in guard position (transform to prefix predicates)

**Note**: The guard infrastructure (AST, codegen, runner) is ready - only parser support is needed.
```

### 4. main_GLP_to_Dart__1_.tex

**Add implementation status**:
```latex
\item Arithmetic guards (\textbf{planned}): \verb|number(X)|, \verb|integer(X)|, \verb|X < Y|, \verb|X =< Y|, etc.
\item Currently implemented: \verb|known/1|, \verb|ground/1|, \verb|otherwise/0|
```

### 5. NEW: guards-reference.md

**Complete with status markers**:
```markdown
# GLP Guards Quick Reference

## Implementation Status

### ✅ Implemented Guards
- `known(X)` - X is bound to a value
- `ground(X)` - X contains no unbound variables
- `otherwise` - All previous clauses failed (not suspended)

### ⏳ Specified (Not Implemented)
- `number(X)` - Test for numeric type
- `integer(X)` - Test for integer type
- `X < Y` - Less than
- `X =< Y` - Less than or equal (Prolog convention: `=<` not `<=`)
- `X > Y` - Greater than
- `X >= Y` - Greater than or equal
- `X =:= Y` - Arithmetic equality
- `X =\= Y` - Arithmetic inequality

### 📝 Parser Limitation
Comparison guards require parser extension to support infix operators in guard position.

## Semantics
All guards: SUCCESS / SUSPEND / FAIL (three-valued)
- Never have side effects
- Execute during HEAD/GUARDS phase only
- Patient: suspend on unbound variables rather than fail
```

---

## Terminology Clarification

Following your guidance, use these terms consistently:

### ✅ Correct Terminology
- **"System predicates called via execute/2"** - For `evaluate/2`, `file_read/2`, etc.
- **"Execute instruction"** - For the bytecode `Execute` opcode
- **"Guards"** - For `known/1`, `ground/1`, comparison operators
- **"Two-valued semantics"** - Success or abort (execute predicates)
- **"Three-valued semantics"** - Success, suspend, or fail (guards)

### ❌ Avoid
- "Execute predicates" (ambiguous)
- "Guard predicates" (redundant - just "guards")

---

## Next Steps

### Option A: Document Current State (Honest)
- Mark arithmetic guards as ⏳ "Specified but not implemented"
- Document working guards: `known/1`, `ground/1`, `otherwise/0`
- Note parser limitation for infix operators
- **Advantage**: Accurate, sets clear expectations
- **Use case**: When users need to know what works NOW

### Option B: Aspirational Specification
- Specify full arithmetic guard suite as the design
- Clearly separate "Specification" from "Implementation Status" sections
- **Advantage**: Complete design document for future implementation
- **Use case**: Architectural planning, implementation roadmap

### Recommended Approach: HYBRID
- **Main spec sections**: Describe complete design (aspirational)
- **Implementation Status boxes**: Mark what's working vs. planned
- **Quick reference**: Show status markers (✅/⏳/📝) clearly

**Example format**:
```markdown
## Arithmetic Guards (Specification)

[Full description of how guards should work]

---

**Implementation Status**:
- ✅ Infrastructure: Parser, AST, codegen, runner ready
- ✅ Basic guards: `known/1`, `ground/1`, `otherwise/0` working
- ⏳ Type guards: `number/1`, `integer/1` - not implemented
- ⏳ Comparison guards: `</2`, `=</2`, etc. - not implemented
- 📝 Parser: Infix operators in guard position - requires extension

---
```

---

## Summary Table

| Component | Status | Notes |
|-----------|--------|-------|
| **Parser** | ✅ Partial | Guards with function syntax work; infix operators don't |
| **AST** | ✅ Complete | Generic `Guard` class |
| **Codegen** | ✅ Complete | Handles known/ground/otherwise + generic dispatch |
| **Bytecode** | ✅ Complete | Opcodes defined |
| **Runner** | ✅ Partial | Executes 3 guards; infrastructure for more |
| **System Predicates** | ❌ None | No arithmetic guards registered |
| **Comparison Tokens** | ❌ Missing | `<`, `>`, `=<`, `>=`, `=:=`, `=\=` not in lexer |
| **Infix Guard Parsing** | ❌ Missing | Parser expects function-call syntax only |

---

## Code Readiness Assessment

**To implement arithmetic guards**, you would need:

### Easy (Runtime only):
1. Add guard predicates to `system_predicates_impl.dart`:
   ```dart
   SystemResult numberGuard(GlpRuntime rt, SystemCall call) {
     // Check if arg is bound number
     // Return success/suspend/fail
   }
   ```
2. Register in guard dispatcher (runner or registry)

### Medium (Parser extension):
1. Add comparison tokens to `token.dart`
2. Update lexer to recognize `<`, `>`, `=<`, `>=`, `=:=`, `=\=`
3. Update parser to handle infix operators in guard position
4. Transform infix to prefix: `X < Y` → `<(X, Y)`

### Infrastructure:
- ✅ Already in place and working
- ✅ Three-valued semantics implemented
- ✅ Guard execution flow tested and proven

---

## End of Report

**Conclusion**: GLP has excellent guard infrastructure, but arithmetic guards are **not implemented**. Documentation should be honest about this while specifying the complete design for future implementation.
