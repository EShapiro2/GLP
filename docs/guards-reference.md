# GLP Guards Quick Reference

**Last Updated**: 2026-03-06

---

## WxW (Writer-to-Writer Matching Fails)

GLP term matching fails on writer-to-writer to ensure no readers are abandoned:
- If writers X and Y were to match, their readers X? and Y? would have no writer to provide values
- Runtime must FAIL immediately on writer-to-writer term matching attempts
- This is NOT a suspension case - it's a definitive failure

---

## Overview

Guards are pure tests with **three-valued semantics** (success/suspend/fail) that appear before the `|` separator in GLP clauses. Guards are **patient**—they suspend on unbound variables rather than fail.

**Syntax**: `Head :- Guard1, Guard2, ... | Body.`

**Semantics**:
- **Success**: Guard condition definitively true → continue to next guard or body
- **Suspend**: Unbound variables present, success possible → add to suspension set Si
- **Fail**: Guard condition definitively false → try next clause

**Key Property**: Guards never have side effects and execute during HEAD/GUARDS phase (before commit).

---

## Guard Negation (`~G`)

**Syntax**: `~G` where G is an atomic built-in guard

**Semantics**: `~G` succeeds iff G fails. Suspension behavior follows from the standard guard definition (a guard suspends if there exists an assignment to its readers that makes it succeed).

**Restrictions**:
- Only atomic built-in guards can be negated
- Defined guards (unit clauses) cannot be negated
- Compound guards cannot be negated (no `~(A, B)`)
- Double negation `~~G` is syntactically forbidden (formally equivalent to G, but forbidden in syntax)

### Negatable Guards

These guards can be negated with `~`:

| Guard | Description | `~` Negation |
|-------|-------------|--------------|
| `ground(X?)` | Test if X contains no variables | `~ground(X?)` succeeds if X is not ground |
| `known(X?)` | Test if X is bound | `~known(X?)` succeeds if X is unbound |
| `unknown(X?)` | Test if X is unbound | `~unknown(X?)` succeeds if X is bound |
| `integer(X?)` | Test for integer type | `~integer(X?)` succeeds if X is not an integer |
| `number(X?)` | Test for numeric type | `~number(X?)` succeeds if X is not a number |
| `string(X?)` | Test for string type | `~string(X?)` succeeds if X is not a string |
| `constant(X?)` | Test for constant | `~constant(X?)` succeeds if X is not a constant |
| `compound(X?)` | Test for compound term | `~compound(X?)` succeeds if X is not compound |
| `list(X?)` | Test for list type | `~list(X?)` succeeds if X is not a list |
| `module(X?)` | Test for module term | `~module(X?)` succeeds if X is not a module |
| `is_mutual_ref(X?)` | Test for mutual reference | `~is_mutual_ref(X?)` succeeds if X is not a mutual ref |
| `no_readers(X?)` | Test for no readers in term | `~no_readers(X?)` succeeds if X contains readers |
| `X =?= Y` | Ground equality test | `~(X =?= Y)` succeeds if X and Y are not equal |

### Non-Negatable Guards

These guards cannot be negated (due to type-error semantics or special behavior):

| Guard | Reason |
|-------|--------|
| `<`, `>`, `=<`, `>=` | Type error on non-numeric operands |
| `=:=`, `=\=` | Type error on non-numeric operands |
| `@<` | Type error on non-constant operands |
| `otherwise` | Special clause-ordering semantics |
| `wait`, `wait_until` | Time-based control flow |

### Examples

```prolog
% Negation of type guards
handle(X, Y) :- ~integer(X?) | handle_non_integer(X?, Y).
handle(X, Y) :- integer(X?) | handle_integer(X?, Y).

% Negation of ground
process(X, Y) :- ~ground(X?) | wait_for_binding(X?, Y).
process(X, Y) :- ground(X?) | process_ground(X?, Y).

% Negation of equality
lookup(Key, [(K,V)|_], V?) :- Key =?= K? | true.
lookup(Key, [(K,_)|Rest], V?) :- ~(Key =?= K?) | lookup(Key?, Rest?, Value).
```

### Design Rationale

In GLP, guards have **input-only variables** - they test but don't bind. This makes success and failure symmetric definitive outcomes. Neither produces bindings, both are final decisions. This symmetry enables clean negation semantics where `~G` simply inverts the success/fail outcome while preserving suspension behavior.

---

## Implementation Status Legend

- ✅ **Implemented** - Working in current runtime
- ⏳ **Specified** - Documented but not yet implemented
- 📝 **Requires Parser** - Needs parser extension for infix syntax

---

## Currently Implemented Guards

### ✅ `known(X)`
**Test if X is bound to a constant or compound term**

**Semantics**:
- Success: X bound to constant (number/string) or compound term (may contain unbound subterms)
- Suspend: X is unbound reader
- Fail: X is unbound writer

**Logical Definition**: `known(X)` ≡ `constant(X) ∨ compound(X)`

**Example**:
```prolog
% Safe read from variable
echo(Input, Output) :- known(Input) | Output = Input?.
```

**Difference from ground**: `known(f(Y))` succeeds even if Y is unbound, because the structure f(Y) itself is a compound term. `ground(f(Y))` would suspend waiting for Y to be bound.

---

### ✅ `constant(X?)`
**Test for atomic constant (number or string)**

**Semantics**:
- Success: X? bound to a number (integer or real) or string atom
- Suspend: X? is unbound reader
- Fail: X? bound to compound term

**Example**:
```prolog
% Safe copying of constants
copy(X, Y, Z) :- constant(X?) | Y = X?, Z = X?.
```

**Note**: Constants are ground by definition. See "Ground Guards - SRSW Relaxation" below for details on multiple occurrences.

---

### ✅ `compound(X?)`
**Test for compound term (structure with functor and arguments)**

**Semantics**:
- Success: X? bound to compound term f(T₁, ..., Tₙ) where n > 0
- Suspend: X? is unbound reader
- Fail: X? bound to constant (number or string)

**Example**:
```prolog
% Process only compound terms
copy(X, Y, Z) :- compound(X?) |
    X? =.. [F|Args],
    copy_list(Args?, Args1, Args2),
    Y =.. [F?|Args1?],
    Z =.. [F?|Args2?].
```

**Relationship to known**: The semantics of `known(X)` can be understood as `constant(X) ∨ compound(X)` — a variable is bound to a value if it's bound to either a constant or a compound term.

**Lists are compound**: In GLP, the list cons `[X|Xs]` is syntactic sugar for the compound term `'.'(X, Xs)`, so lists are compound terms and `compound([a,b,c])` succeeds.

---

### ✅ `ground(X?)`
**Test if X? contains no unbound variables**

**Semantics**:
- Success: X? is ground (no unbound variables anywhere)
- Suspend: X? contains unbound readers (waiting for values)
- Fail: X? contains unbound writers

**Why the argument must be a reader**: Guards use three-valued semantics where unbound variables cause suspension (waiting for a value). If the argument were a writer, an unbound variable would cause immediate failure rather than suspension, defeating the purpose of patient synchronization.

**Example**:
```prolog
% Enable multiple occurrences with ground guard
replicate(X, [X?, X?, X?]) :- ground(X?) | true.
```

**Key Property**: See "Ground Guards - SRSW Relaxation" below for details on how ground guards enable multiple occurrences.

**Circular Term Behavior**: When `X?` is bound to a circular term (e.g., `f(f(f(...)))`), `ground(X?)` succeeds if the term contains no unbound variables on any branch. The cycle itself does not make a term non-ground.

---

### ✅ `otherwise`
**Succeeds if all previous clauses failed (not suspended)**

**Semantics**:
- Success: All previous clauses for this procedure definitively failed
- Fail: At least one previous clause suspended (may still succeed)

**Example**:
```prolog
% Metainterpreter catch-all
run(Goal) :- clause(Goal?, Body) | run(Body?).
run(Goal) :- otherwise | send_to_user(no_clauses(Goal?)).
```

**Usage**: Common in metainterpreters and default case handling.

---

### ✅ `no_readers(X?)`
**Test if X contains no readers (only ground terms or writers)**

**Semantics**:
- Success: X? is bound to a term containing no readers (ground terms and/or writers only)
- Suspend: X? contains any readers (waiting for them to be instantiated)
- Fail: Never fails

**Key Property**: This guard **never fails**—it either succeeds (no readers) or suspends (has readers). This is because any term with readers will eventually either have those readers bound (at which point the guard is re-evaluated) or remain suspended indefinitely.

**Use Case**: Ensuring a term is safe for external output (e.g., to a UI). Terms sent to external systems should not contain readers, as the external system cannot wait for them to be instantiated.

**Example**:
```prolog
% UI agent validates output before sending to Dart
ui_output(Term, DartOut) :- no_readers(Term?) |
    send_to_dart(Term?, DartOut).
```

**Difference from ground**:
- `ground(X?)` succeeds only if X contains no variables at all (neither readers nor writers)
- `no_readers(X?)` succeeds if X contains no readers but may contain writers

For example:
- `no_readers(f(Y))` where Y is a writer: **succeeds** (writers are OK)
- `no_readers(f(Y?))` where Y? is an unbound reader: **suspends**
- `ground(f(Y))` where Y is a writer: **fails** (writers are unbound variables)

**SRSW Relaxation**: No. Success of `no_readers(X?)` does not imply groundness (X may contain writers), so multiple occurrences are not permitted.

---

## Guard Arguments: Why Readers?

Guards that test variable values (`ground`, `known`, `integer`, `number`, `string`) take **reader** arguments. This follows from GLP's three-valued guard semantics:

| Argument Type | If Unbound | Behavior |
|---------------|------------|----------|
| Reader (`X?`) | Suspend | Wait for paired writer to provide value |
| Writer (`X`) | Fail | No paired reader to wait for |

Using a reader enables patient synchronization: the clause waits until data arrives before testing it. Using a writer would cause immediate failure on unbound variables, defeating the purpose of concurrent synchronization.

**Example**:
```prolog
% ✅ CORRECT - reader suspends until value available
process(X, Y?) :- ground(X?) | Y = computed(X?).

% ❌ WRONG - writer fails immediately if unbound
process(X, Y?) :- ground(X) | Y = computed(X?).  % Would fail, not suspend
```

---

## Ground Guards - SRSW Relaxation

Per the formal definition, variables occur as reader/writer pairs with exactly one of each. The ONLY exception: when guards guarantee groundness, multiple occurrences of both the writer and reader are permitted because ground terms contain no unbound writers.

### The Rule

When a guard ensures a variable is ground (contains no unbound variables), both the writer and its paired reader may appear **multiple times** in the clause without violating SRSW. This is fundamental to GLP's concurrent programming model.

### Why This Works

Ground terms contain no unbound writers. Multiple occurrences of a ground variable's writer and reader do not create single-writer violations because there's no exposed writer that could be bound multiple times.

### Guard Arguments Count as Reader Occurrences

Guards take reader arguments. These reader occurrences count toward satisfying the SRSW syntactic restriction that each writer has a paired reader. A clause such as:

```glp
check(X) :- known(X?) | true.
```

is valid because X appears as writer in the head and X? appears as reader in the guard, satisfying SRSW with one writer and one reader.

This is distinct from the multiple-occurrence relaxation below. Guard reader counting ensures guards participate in SRSW validation. The relaxation below determines which guards permit both the writer and reader of a variable to appear multiple times.

### Guards That Imply Groundness

| Guard | Implies Ground | Allows Multiple Occurrences |
|-------|----------------|-------------------------|
| ✅ `ground(X?)` | Yes | ✅ Yes |
| ✅ `constant(X?)` | Yes | ✅ Yes |
| ✅ `integer(X?)` | Yes | ✅ Yes |
| ✅ `number(X?)` | Yes | ✅ Yes |
| ✅ `string(X?)` | Yes | ✅ Yes |
| ✅ `module(X?)` | Yes | ✅ Yes |
| ✅ `X? < Y?` | Yes (both operands, when succeeds) | ✅ Yes |
| ✅ `X? =< Y?` | Yes (both operands, when succeeds) | ✅ Yes |
| ✅ `X? > Y?` | Yes (both operands, when succeeds) | ✅ Yes |
| ✅ `X? >= Y?` | Yes (both operands, when succeeds) | ✅ Yes |
| ✅ `X? =:= Y?` | Yes (both operands, when succeeds) | ✅ Yes |
| ✅ `X? =\= Y?` | Yes (both operands, when succeeds) | ✅ Yes |
| ✅ `compound(X?)` | **NO** | ❌ No |
| ✅ `known(X?)` | **NO** | ❌ No |
| ✅ `no_readers(X?)` | **NO** | ❌ No |
| ✅ `otherwise` | No | ❌ No |

**Note**: Arithmetic comparison guards suspend if operands are unbound and only succeed if both operands are bound to numbers. Therefore, when they succeed, both operands are guaranteed to be ground.

**Critical Difference**: `known(X)` tests if X is bound to either a constant or compound term (i.e., `constant(X) ∨ compound(X)`), but does not require groundness. A compound term like `f(Y)` is known but not ground if Y is unbound. Similarly, `compound(X)` succeeds on `f(Y)` even when Y is unbound.

### Correct Patterns

```prolog
% ✅ Broadcasting with ground guard
broadcast(Msg, Out1, Out2, Out3) :- ground(Msg?) |
    send(Msg?, Out1),    % Msg? appears 3 times - OK!
    send(Msg?, Out2),
    send(Msg?, Out3).

% ✅ Multiple computations with ground value
compute_twice(X, Y1, Y2) :- ground(X?) |
    execute('evaluate', [X? + 1, Y1]),   % X? appears twice - OK!
    execute('evaluate', [X? * 2, Y2]).

% ✅ Integer guard implies groundness
distribute(N, R1, R2) :- integer(N?) |
    execute('evaluate', [N? * 2, R1]),
    execute('evaluate', [N? + 5, R2]).

% ✅ Arithmetic comparison guard implies groundness
partition(X, Pivot, Small, Large) :- X? < Pivot? |
    Small = [X? | RestSmall?],           % X? appears twice - OK!
    partition_rest(RestSmall?, Pivot?, Large).
```

### Incorrect Patterns

```prolog
% ❌ WRONG - no ground guard, SRSW violation
bad_broadcast(X, Y1, Y2) :-
    send(X?, Y1),    % SRSW VIOLATION!
    send(X?, Y2).    % X? appears twice without ground guard

% ❌ WRONG - known(X?) does NOT imply ground
bad_example(X, Y1, Y2) :- known(X?) |
    send(X?, Y1),    % SRSW VIOLATION!
    send(X?, Y2).    % X? could be f(Z) where Z is unbound
```

### Compiler Requirements

The SRSW analyzer must:
1. Track guards in HEAD/GUARDS phase
2. Recognize guards that imply groundness:
   - Type guards: `ground/1`, `integer/1`, `number/1`, `string/1`, `constant/1`
   - Arithmetic comparisons: `<`, `=<`, `>`, `>=`, `=:=`, `=\=`
3. For variables with ground-guaranteeing guards:
   - Mark variable as "ground-certified" for this clause
   - Allow multiple occurrences of both writer and reader in clause
4. For variables without such guards:
   - Enforce strict single-occurrence constraint

### Use Cases

This feature enables essential concurrent patterns:
- **Broadcasting**: One value to multiple consumers
- **Replication**: Copying ground data structures
- **Multi-computation**: Using same input for multiple calculations
- **Fan-out**: Distributing work to multiple goals

**Key Insight**: Without this relaxation, GLP would be severely limited for concurrent programming. The ground guard is what makes safe, concurrent data distribution possible.

---

## Type Guards

### ✅ `string(X?)`
**Test for string constant (non-numeric, non-nil)**

**Semantics**:
- Success: X? bound to a string constant (e.g., `hello`, `foo`)
- Suspend: X? is unbound reader
- Fail: X? bound to number, compound term, or empty list

**Note**: The empty list `[]` is represented as `nil` internally and is NOT a string. `string([])` fails.

**SRSW Relaxation**: Yes. Strings are ground by definition, so `string(X?)` implies groundness and permits multiple occurrences.

**Example**:
```prolog
% Process string messages
handle(X, Y) :- string(X?) | process_message(X?, Y).
```

---

### ✅ `number(X?)`
**Test for numeric type**

**Semantics**:
- Success: X? bound to number (int or double)
- Suspend: X? is unbound reader
- Fail: X? bound to non-number

**Example**:
```prolog
safe_compute(X, Y) :- number(X?) | execute('evaluate', [X? * 2, Y]).
```

---

### ✅ `integer(X?)`
**Test for integer type**

**Semantics**:
- Success: X? bound to integer
- Suspend: X? is unbound reader
- Fail: X? bound to non-integer (including floats)

**Example**:
```prolog
safe_divide(X, Y, Z) :- integer(X?), integer(Y?), Y? =\= 0 |
                        execute('evaluate', [X? / Y?, Z]).
```

---

### ✅ `list(X?)`
**Test for list (empty or cons cell)**

**Semantics**:
- Success: X? bound to `[]` (empty list) or `[H|T]` (cons cell)
- Suspend: X? is unbound reader
- Fail: X? bound to non-list (number, string, or non-list compound term)

**Note**: This tests that the top-level term is a list constructor. It does not check whether the list is proper (i.e., terminated by `[]`). `list([a|b])` succeeds even though the tail `b` is not a list.

**SRSW Relaxation**: No. A list may contain unbound subterms, so `list(X?)` does not imply groundness.

**Example**:
```prolog
% Process only list inputs
handle(X, Y) :- list(X?) | process_list(X?, Y).
handle(X, Y) :- otherwise | process_other(X?, Y).
```

---

### ✅ `module(X?)`
**Test if X is a module term**

**Semantics**:
- Success: X? bound to a `ModuleTerm` (compiled module binary)
- Suspend: X? is unbound reader
- Fail: X? bound to any other value

**SRSW Relaxation**: Yes. Module terms are ground (opaque compiled values with no unbound variables), so `module(X?)` implies groundness and permits multiple occurrences.

**Use Case**: Guards in module-based code that need to verify a term is a module before dispatching goals to it via `_activate/2`.

---

### ✅ `unknown(X?)`
**Test if X is unbound (inverse of `known`)**

**Semantics**:
- Success: X? is an unbound variable (reader or writer)
- Fail: X? is bound to any value (constant, compound, list)

**Logical Definition**: `unknown(X)` ≡ `~known(X)`. The guard succeeds when dereferencing X leads to an unbound variable.

**Note**: Unlike most guards, `unknown(X?)` does NOT suspend — it either succeeds (unbound) or fails (bound). An unbound reader succeeds immediately rather than suspending, because the purpose is to test for unboundness.

**SRSW Relaxation**: No.

**Example**:
```prolog
% Default value for unbound variables
provide_default(X, X?, _) :- known(X?) | true.
provide_default(X, _, Default?) :- unknown(X?) | true.
```

---

## Equality Guard

### ✅ `X =?= Y`
**Ground equality test**

Tests whether two terms are ground and equal.

**Semantics** (three-valued):

| X | Y | Result |
|---|---|--------|
| ground | ground, X = Y | succeed |
| ground | ground, X ≠ Y | fail |
| unbound reader | any | suspend |
| any | unbound reader | suspend |
| unbound writer | any | fail |
| any | unbound writer | fail |

**Usage**: Pattern matching where equality must be tested explicitly.

```prolog
% Lookup in association list
lookup(Key, [(K, Value)|_], Value?) :- Key =?= K? | true.
lookup(Key, [_|Rest], Value?) :- otherwise | lookup(Key?, Rest?, Value).
```

The guard `Key =?= K?` succeeds when `Key` and `K` are both ground and equal. If `K` is unbound (reader), it suspends. If `Key` is unbound writer, it fails.

**Why not multiple head writers**: GLP maintains the SO invariant via SRSW syntactic restriction (one writer per variable). Instead of implicit equality via multiple head occurrences, use `=?=` for explicit, visible equality testing.

---

## What Can Appear in Guard Position

### Guard Classification

The partial evaluator validates all guards at compile time. Guards fall into exactly two categories:

1. **Builtin guards** — Implemented in the Dart runtime with NO GLP clauses. These include type guards (`integer/1`, `number/1`, `ground/1`, etc.), comparison guards (`</2`, `>/2`, etc.), and equality guards (`=?=/2`). Builtin guards are kept as-is by the partial evaluator.

2. **Single-unit-clause procedures** — User-defined procedures with exactly one clause, no guards, and no body. These are unfolded at compile time by the partial evaluator.

### Validation Rule

**Any procedure called in guard position that is NOT a builtin guard MUST be a single-unit-clause procedure.** If a user-defined procedure has multiple clauses, or has guards or body goals, calling it in guard position is a compile-time error.

### Error Example

```prolog
%% COMPILE ERROR: multi/1 has multiple clauses
procedure multi(_).
multi(a).
multi(b).

procedure test(_).
test(X?) :- multi(X?) | process(X?).  %% ERROR!
```

The partial evaluator reports:
```
Cannot call "multi/1" in guard position.
  Only builtin guards and single-unit-clause procedures can appear in guards.
  The procedure "multi" has multiple clauses or non-unit clauses.
```

---

## Single-Unit-Clause Procedures in Guards

### ✅ Regular procedures can be called in guard position

A **single-unit-clause procedure** is a regular procedure that happens to be defined by exactly one clause with no guards and no body. These procedures have **no special status** — they are ordinary procedures with procedure declarations.

When such a procedure is called in guard position, the partial evaluator unfolds it at compile time.

**Example from root self.glp:**
```prolog
procedure new_channel(Channel, Channel).
new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).
```

This can be called in either position:

```prolog
%% Guard position - PE unfolds at compile time
play :- new_channel(AliceCh, BobCh) | alice(AliceCh?), bob(BobCh?).

%% Body position - executes at runtime  
setup(Ch1?, Ch2?) :- new_channel(Ch1, Ch2).
```

**Pure pattern guards** (for type testing) use anonymous variables:

```prolog
procedure channel(_?).
channel(ch(_, _)).

%% Usage:
process(X, Y) :- channel(X?) | handle_channel(X?, Y).
```

When `channel(X?)` is unfolded, it becomes pattern matching against `ch(_, _)`.

**Semantics** (three-valued, like all guards):
- **Success**: Arguments unify with the clause head pattern
- **Suspend**: Arguments contain unbound readers
- **Fail**: Arguments don't match pattern

**Requirements:**
1. **Procedure declaration** — required for type checking
2. **SRSW compliance** — the clause must satisfy SRSW
3. **Single unit clause** — for guard-position calls (PE validates this)

**Why anonymous variables for pattern guards**: Use `_` for positions that don't need to produce bindings. Named variables like `channel(ch(In?, Out)).` would violate SRSW (reader with no paired writer).

**See also**: `docs/typed-glp-manual.md` Section 8 for single-unit-clause procedure details.

---

## Comparison Guards

### ✅ `X < Y`, `X =< Y`, `X > Y`, `X >= Y`
**Arithmetic comparison**

**Note**: Prolog uses `=<` (not `<=`) for "less than or equal"

**Semantics**:
- Success: Both X and Y bound to numbers AND condition holds
- Suspend: Either X or Y is unbound reader
- Fail: Both bound to numbers AND condition false

**Example**:
```prolog
factorial(N, F) :- integer(N?), N? > 0 |
                   execute('evaluate', [N? - 1, N1]),
                   factorial(N1?, F1),
                   execute('evaluate', [N? * F1?, F]).
factorial(N, 1) :- integer(N?), N? =< 0 | true.
```

---

### ✅ `X =:= Y`
**Arithmetic equality**

**Semantics**:
- Success: Both bound and numerically equal
- Suspend: Either operand is unbound reader
- Fail: Both bound and not numerically equal

**Note on `=\=`**: The arithmetic inequality guard `=\=` is **redundant** once guard negation (`~`) is implemented. It becomes equivalent to `~(X =:= Y)`. Use `~(X? =:= Y?)` for arithmetic inequality.

---

### ✅ `X @< Y`
**Lexicographic less-than on ground constants**

**Semantics**:
- Success: Both X and Y bound to ground constants AND X's name lexicographically precedes Y's name
- Suspend: Either operand is an unbound reader
- Fail: Both bound and not (X precedes Y), or either operand is bound to a non-constant (type error)

**Procedure signature**: `procedure @<(Constant?, Constant?).`

**Use case**: Symmetry-breaking on participant identity, e.g. choosing a canonical channel when two agents attempt to establish a friendship concurrently.

**Example**:
```prolog
befriend_commit(Id, Other, ...) :- Id? @< Other? | ...   % smaller-named side
befriend_commit(Id, Other, ...) :- otherwise | ...        % larger-named side
```

**Negation**: Non-negatable (same rationale as arithmetic comparisons — `~(X @< Y)` would conflate "X is not lex-smaller" with "type error", so negation is forbidden).

**Implementation in tables**:
- `root_scope.dart`'s `predefinedProcedureNames` and `builtinProcedures` sets include `@<` and `@</2`.
- `analyzer.dart`'s `_nonNegatableGuards` includes `@<`.
- `analyzer.dart`'s `comparisonOps` (groundness inference) includes `@<`.
- `runner.dart`'s guard switch implements the lex comparison via the local `evalConst` helper.
- `lexer.dart` tokenizes `@<` as `TokenType.AT_LESS` (distinct from `@` followed by `<`).
- `parser.dart`'s infix-comparison check includes `AT_LESS`.
- `self.glp` declares `procedure @<(Constant?, Constant?).`.

---

## Guards vs System Predicates (Critical Distinction)

| Aspect | Guards | System Predicates (via execute) |
|--------|--------|----------------------------------|
| **Semantics** | Three-valued (success/suspend/fail) | Two-valued (success/abort) |
| **Unbound Input** | Suspend (patient) | Abort (impatient) |
| **Syntax** | `Head :- Guard \| Body` | `execute('name', [Args])` |
| **Phase** | HEAD/GUARDS (before commit) | BODY (after commit) |
| **Side Effects** | Never | May have (I/O, mutations) |
| **Examples** | `known(X?)`, `ground(X?)`, `number(X?)` | `evaluate/2`, `file_read/2` |

---

## Safe Programming Pattern

**Always use guards to ensure preconditions before execute:**

```prolog
% ❌ UNSAFE - aborts if X unbound or non-numeric
unsafe_double(X, Y) :-
  execute('evaluate', [X? * 2, Y]).

% ✅ SAFE - guard ensures X is bound number
safe_double(X, Y) :-
  number(X?) |
  execute('evaluate', [X? * 2, Y]).

% ✅ SAFE - multiple guards ensure preconditions
safe_divide(X, Y, Z) :-
  number(X?), number(Y?), Y? =\= 0 |
  execute('evaluate', [X? / Y?, Z]).
```

---

## Common Usage Patterns

### Pattern 1: Type Checking Before Execute
```prolog
process(X, Result) :-
  integer(X?), X? > 0 |
  execute('evaluate', [X? * X?, Result]).
```

### Pattern 2: Conditional Clause Selection
```prolog
compute(N, Result) :-
  integer(N?), N? > 10 |
  execute('evaluate', [N? * 2, Result]).
compute(N, Result) :-
  integer(N?), N? =< 10 |
  execute('evaluate', [N? + 10, Result]).
```

### Pattern 3: Default Case with Otherwise
```prolog
handle(X, done) :- integer(X?) | process_int(X?).
handle(X, done) :- ground(X?) | process_ground(X?).
handle(_, error) :- otherwise | true.
```

### Pattern 4: Safe Multiple Readers
```prolog
broadcast(Msg, [Msg?, Msg?, Msg?]) :- ground(Msg?) | true.
```

---

## Implementation Checklist

**For Adding New Guards**:

1. **Runtime** (`system_predicates_impl.dart`):
   - [ ] Implement guard predicate with three-valued return
   - [ ] Handle unbound readers (return suspend)
   - [ ] Handle bound values (return success/fail)

2. **Codegen** (`codegen.dart`):
   - [x] Already handles generic guards via `Guard` opcode
   - [ ] Optional: Add special case for optimized bytecode

3. **Runner** (`runner.dart`):
   - [x] Generic guard execution infrastructure exists
   - [ ] Add handler for specific guard opcode if optimized

4. **Parser** (for infix guards only):
   - [ ] Add tokens to `token.dart`
   - [ ] Update lexer in `lexer.dart`
   - [ ] Handle infix syntax in `_parseGoalOrGuard()`
   - [ ] Transform infix to prefix: `X < Y` → `<(X, Y)`

---

## Testing Guards

**Test all three outcomes**:

```prolog
% Test success
test_known_success :-
  X = 42,
  known(X) |  % Should succeed
  send_to_user(known_42_succeeded).

% Test suspension (requires runtime trace)
test_known_suspend :-
  known(X) |  % Should suspend on unbound X
  send_to_user(should_not_reach_here).

% Test failure (writer case)
test_known_fail :-
  % Would need internal writer representation to test fail case
  true.
```

---

## References

- **SPEC_GUIDE.md** - Overview of guards vs execute predicates
- **glp-bytecode-v216-complete.md** - Complete guard instruction specifications
- **parser-spec.md** - Parser implementation for guard expressions
- **main_GLP_to_Dart (1).tex** - Formal specification

---

### ✅ `is_mutual_ref(X?)`
**Test if X is a mutual reference term**

**Semantics**:
- Success: X? bound to a `MutualRefTerm` (an internal runtime term that enables SRSW-safe multiple reads)
- Suspend: X? is unbound reader
- Fail: X? bound to any other value

**SRSW Relaxation**: No.

---

## Time Guards

### ✅ `wait(Duration)`
**Suspend for a specified duration in milliseconds**

**Semantics**:
- Duration ≤ 0: succeed immediately
- Duration > 0: suspend the goal, start a timer, and resume when the timer fires
- Duration is non-number: fail
- Duration is unbound reader: suspend (handled by caller)

**Mechanism**: On the first call, `wait` allocates a reader/writer pair, starts a timer, and adds the reader to the suspension set. When the timer fires, it binds the writer, which reactivates the goal via the ROQ. On resume, the guard checks if the timer has fired and succeeds.

**Non-Negatable**: `wait` is a control flow guard, not a pure test. Negation is not meaningful.

**Example**:
```prolog
% Wait 100ms before proceeding
delayed_action(Result?) :- wait(100) | Result = done.
```

---

### ✅ `wait_until(Timestamp)`
**Suspend until absolute time has passed**

**Semantics**:
- Success: current time (milliseconds since epoch) ≥ Timestamp
- Suspend: current time < Timestamp — starts a timer for the remaining duration, suspends until the timer fires, then succeeds
- Timestamp is non-number: fail
- Timestamp is unbound reader: suspend (handled by caller)

**Mechanism**: Like `wait`, uses a reader/writer pair and a Dart timer. Computes `remaining = timestamp - now`, starts a timer for that duration, and suspends the goal on the reader. When the timer fires, the writer is bound, reactivating the goal via the ROQ. On resume, the guard re-checks `now >= timestamp` and succeeds.

**Non-Negatable**: Time-based control flow guard.

**Example**:
```prolog
% Suspend until a given timestamp, then proceed
after_deadline(T, Result?) :- wait_until(T?) | Result = done.
```

---

## Frequently Asked Questions

**Q: When should I use guards vs system predicates?**

A: Use guards for **pure tests** that check properties without side effects. Use system predicates (via execute) for **operations** that compute results or perform I/O.

**Q: Why do guards suspend instead of fail?**

A: Guards are **patient** (suspend on unbound variables) to enable concurrent programming. An unbound variable may become bound later through message passing, at which point the suspended goal can resume.

**Q: Can I use arithmetic in guards?**

A: Yes. Comparison guards (`X? < Y?`, `X? > Y?`, `X? =< Y?`, `X? >= Y?`, `X? =:= Y?`, `X? =\= Y?`) are implemented. They suspend on unbound readers and succeed or fail based on the numeric comparison. Type guards `number(X?)` and `integer(X?)` test for numeric types.

**Q: What's the difference between `known` and `ground`?**

A: `known(f(X))` succeeds if the structure f(X) is bound (even if X inside is unbound). `ground(f(X))` only succeeds if X is also bound (no unbound variables anywhere).

**Q: Why does `otherwise` check for failure, not suspension?**

A: If a previous clause suspended, it may still succeed when its readers are bound. `otherwise` only succeeds when all previous attempts **definitively failed**, not when they're waiting for data.

---

**End of Quick Reference**
