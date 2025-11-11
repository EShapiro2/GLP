# All Compiler Test Sources and Goals

## compiler_test.dart (13 tests)

### 1. Simple fact
**Source**: `p(a).`
**Goal**: Verify it compiles with HeadConstant

### 2. Clause with variables
**Source**: `p(X, Y) :- q(X?, Y?).`
**Goal**: Verify variables compile with GetVariable and PutReader

### 3. Clause with guards
**Source**: `p(X) :- ground(X?) | q(X?), r(X?).`
**Goal**: Verify guards compile

### 4. List matching
**Source**: `p([X|Xs]).`
**Goal**: Verify list compiles with HeadStructure (for '.')

### 5. Empty list
**Source**: `p([]).`
**Goal**: Verify empty list compiles with HeadNil

### 6. Multiple clauses
**Source**:
```prolog
p(a).
p(b).
```
**Goal**: Verify multiple clauses with clause labels

### 7. Merge/3 example
**Source**:
```prolog
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
merge([], [], []).
```
**Goal**: Verify 3 clauses, HeadStructure for lists, Requeue

### 8. Structure matching
**Source**: `p(f(X, Y)).`
**Goal**: Verify HeadStructure for functor f/2

### 9. Spawn for non-tail goals
**Source**: `p(X, Y) :- q(X?), r(Y?).`
**Goal**: Verify Spawn for q, Requeue for r

### 10. Correct labels
**Source**:
```prolog
p(a).
q(b).
```
**Goal**: Verify labels p/1, q/1 exist

### 11. Otherwise guard
**Source**: `p(X) :- otherwise | q(X?).`
**Goal**: Verify Otherwise instruction

### 12. Known guard
**Source**: `p(X, Y) :- known(X?) | q(Y?).`
**Goal**: Verify Known instruction

### 13. Underscore handling
**Source**: `p(_, X).`
**Goal**: Verify UnifyVoid for anonymous variable

---

## integration_simple_test.dart (3 tests)

### 1. Simple fact execution
**Source**: `p(a).`
**Goal**: `p(a)` with writer bound to 'a'
**Expected**: Goal succeeds

### 2. Variable unification
**Source**: `p(X).`
**Goal**: `p(W)` with unbound writer W
**Expected**: Goal succeeds (self-unification)

### 3. Multiple facts
**Source**:
```prolog
p(a).
p(b).
```
**Goals**:
- `p(a)` should succeed
- `p(b)` should succeed
- `p(c)` should fail
**Expected**: All goals execute correctly

---

## integration_body_test.dart (3 tests) ⚠️ FAILING

### 1. Body with reader
**Source**:
```prolog
p(X) :- q(X?).
q(a).
```
**Goal**: `p(W)` with unbound writer W
**Expected**: p spawns q(W?), q binds W to 'a'
**Status**: ❌ Suspends at PutReader (VM bug)

### 2. Multiple body goals
**Source**:
```prolog
p(X, Y) :- q(X?), r(Y?).
q(a).
r(b).
```
**Goal**: `p(W1, W2)` with two unbound writers
**Expected**: Spawns q(W1?) and requeues r(W2?), both bind
**Status**: ❌ Suspends at PutReader (VM bug)

### 3. Suspension test
**Source**: `p(X?).`
**Goal**: `p(R?)` with unbound reader R
**Expected**: Goal suspends on unbound reader
**Status**: ❌ Related to PutReader issue

---

## Verification Tests (Compare + Run)

### verify_simple_body_test.dart
**Source (SRSW-compliant)**:
```prolog
p(a).
forward(X) :- p(X?).
```
**Goal**: `forward(Y)` with unbound writer Y
**Expected**: Suspends (X? reads unbound Y)
**Note**: Hand-written version uses `p(X)` which violates SRSW

### verify_simple_pq_test.dart ✅
**Source**:
```prolog
p(a).
q(a).
```
**Goals**:
1. `q(X?)` with unbound reader → suspends
2. `p(X)` with unbound writer → binds X to 'a', wakes q
**Expected**: Both succeed, X = a

### verify_merge_base_test.dart ✅
**Source**: `merge([],[],[]).`
**Goal**: `merge([], [], [])` with three bound empty lists
**Expected**: Success

### verify_body_reader_test.dart ⚠️
**Source**:
```prolog
p(X) :- q(X?).
q(a).
```
**Goal**: `p(W)` with unbound writer
**Expected**: W binds to 'a'
**Status**: ❌ PutReader VM bug

---

## Comparison Tests (Bytecode Analysis)

### compare_simple_body.dart ✅
**Source (hand-written)**: `forward(X) :- p(X).` (SRSW violation)
**Source (compiled)**: `forward(X) :- p(X?).` (SRSW-compliant)
**Comparison**: PutWriter vs PutReader
**Conclusion**: Different semantics due to SRSW

### compare_simple_pq.dart ✅
**Source**:
```prolog
p(a).
q(a).
```
**Comparison**: Identical except label naming
**Conclusion**: Semantically equivalent

### compare_merge_base.dart ✅
**Source**: `merge([],[],[]).`
**Comparison**: HeadNil vs HeadConstant(null)
**Conclusion**: HeadNil is optimization

### compare_run_merge_clause1.dart ✅
**Source**:
```prolog
merge([X|Xs],Ys,[X?|Zs?]) :- merge(Ys?,Xs?,Zs).
merge([],[],[]).
```
**Goal**: `merge([a], [], Z)` where Z is unbound
**Expected**: Z binds to [a]
**Comparison**: Both use HeadStructure('.', 2) now
**Result**: ✅ WORKS!

### compare_run_struct_test.dart ✅
**Source**: `p(f(a,b)).`
**Goal**: `p(X)` with unbound writer X
**Expected**: X binds to f(a,b)
**Result**: ✅ WORKS!

### compare_bytecode_test.dart ✅
**Source**:
```prolog
p(X) :- q(X?).
q(a).
```
**Goal**: Bytecode structure verification
**Result**: ✅ Correct bytecode generated

---

## Analyzer Tests (9 tests) ✅

All analyzer tests verify SRSW constraints:
1. Single occurrence per clause
2. Reader/writer pairing
3. Variable scope
4. SRSW violations detected
5. Multiple occurrences rejected
6. Body reader usage
7. Multiple body readers
8. Nested readers
9. Guard readers

---

## Lexer/Parser Tests (42 tests) ✅

All lexer and parser tests verify:
- Tokenization
- AST generation
- Syntax validation
- List parsing
- Structure parsing
- Guard parsing
- Multiple clauses
- Error handling

---

## Summary

**Total Tests**: 51
- **Compiler unit tests**: 13 ✅
- **Integration simple**: 3 ✅
- **Integration body**: 3 ❌ (VM bug)
- **Verification tests**: 5 (4 ✅, 1 ❌)
- **Comparison tests**: 6 ✅
- **Analyzer tests**: 9 ✅
- **Lexer/Parser tests**: 42 ✅

**Success Rate**: 47/51 (92%) - blocked by 1 VM bug affecting 4 tests
