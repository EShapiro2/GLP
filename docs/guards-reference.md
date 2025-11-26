# GLP Guards Quick Reference

**Last Updated**: 2025-11-12

---

## WxW (No Writer-to-Writer Binding) Restriction

GLP prohibits writer-to-writer binding to ensure no readers are abandoned:
- If writers X and Y unified, their readers X? and Y? would have no writer to provide values
- Runtime must FAIL immediately on writer-to-writer unification attempts
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

## Implementation Status Legend

- ✅ **Implemented** - Working in current runtime
- ⏳ **Specified** - Documented but not yet implemented
- 📝 **Requires Parser** - Needs parser extension for infix syntax

---

## Currently Implemented Guards

### ✅ `known(X)`
**Test if X is bound to a value**

**Semantics**:
- Success: X bound to any value (including structures with variables)
- Suspend: X is unbound reader
- Fail: X is unbound writer

**Example**:
```prolog
% Safe read from variable
echo(Input, Output) :- known(Input) | Output = Input?.
```

**Difference from ground**: `known(f(Y))` succeeds even if Y is unbound, because the structure f(Y) itself is bound. `ground(f(Y))` would suspend on unbound Y.

---

### ✅ `ground(X?)`
**Test if X contains no unbound variables**

**IMPORTANT: Must use reader form `ground(X?)`, not writer form `ground(X)`.**

This is stricter than the GLP 2025 paper, which allows either form. We require reader form as a discipline to ensure the variable is being read, not written.

**Semantics**:
- Success: X is ground (no unbound variables anywhere)
- Suspend: X contains unbound readers
- Fail: X contains unbound writers

**Example**:
```prolog
% Enable multiple reader occurrences
replicate(X, [X?, X?, X?]) :- ground(X?) | true.
```

**Key Property**: With `ground(X?)` guard, multiple occurrences of `X?` in the body don't violate single-writer, as ground terms don't expose writers.

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
run(Goal) :- otherwise | execute('write', ['No clauses for: ']),
                         execute('write', [Goal?]).
```

**Usage**: Common in metainterpreters and default case handling.

---

## CRITICAL: Ground Guards - The ONLY Exception to SRSW Syntactic Restriction

Per the formal definition, variables occur as reader/writer pairs with exactly one of each. The ONLY exception: when guards guarantee groundness, multiple reader occurrences are permitted because ground terms cannot expose writers.

### The Rule

When a guard ensures a variable is ground (contains no unbound variables), that variable may appear **multiple times as a reader** in the clause body without violating SRSW. This is fundamental to GLP's concurrent programming model.

### Why This Works

Ground terms contain no unbound writers. Multiple readers of a ground term do not create single-writer violations because there's no writer to expose.

### Guards That Imply Groundness

| Guard | Implies Ground | Allows Multiple Readers |
|-------|----------------|-------------------------|
| ✅ `ground(X)` | Yes | ✅ Yes |
| ⏳ `integer(X)` | Yes (when implemented) | ✅ Yes |
| ⏳ `number(X)` | Yes (when implemented) | ✅ Yes |
| 📝 `X < Y` | Yes (both operands, when succeeds) | ✅ Yes |
| 📝 `X =< Y` | Yes (both operands, when succeeds) | ✅ Yes |
| 📝 `X > Y` | Yes (both operands, when succeeds) | ✅ Yes |
| 📝 `X >= Y` | Yes (both operands, when succeeds) | ✅ Yes |
| 📝 `X =:= Y` | Yes (both operands, when succeeds) | ✅ Yes |
| 📝 `X =\= Y` | Yes (both operands, when succeeds) | ✅ Yes |
| ✅ `known(X)` | **NO** | ❌ No |
| ✅ `otherwise` | No | ❌ No |

**Note**: Arithmetic comparison guards suspend if operands are unbound and only succeed if both operands are bound to numbers. Therefore, when they succeed, both operands are guaranteed to be ground.

**Critical Difference**: `known(X)` only tests if X is bound, not if it's ground. A structure like `f(Y)` is known (bound) but not ground (Y may be unbound).

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

% ✅ Integer guard implies groundness (when implemented)
distribute(N, R1, R2) :- integer(N) |
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

% ❌ WRONG - known(X) does NOT imply ground
bad_pattern(X, Y1, Y2) :- known(X) |
    send(X?, Y1),    % SRSW VIOLATION!
    send(X?, Y2).    % X could be f(Z) where Z is unbound
```

### Compiler Requirements

The SRSW analyzer must:
1. Track guards in HEAD/GUARDS phase
2. Recognize guards that imply groundness:
   - Type guards: `ground/1`, `integer/1`, `number/1`
   - Arithmetic comparisons: `<`, `=<`, `>`, `>=`, `=:=`, `=\=`
3. For variables with ground-guaranteeing guards:
   - Mark variable as "ground-certified" for this clause
   - Allow multiple reader occurrences in body
4. For variables without such guards:
   - Enforce strict single-reader constraint

### Use Cases

This feature enables essential concurrent patterns:
- **Broadcasting**: One value to multiple consumers
- **Replication**: Copying ground data structures
- **Multi-computation**: Using same input for multiple calculations
- **Fan-out**: Distributing work to multiple goals

**Key Insight**: Without this relaxation, GLP would be severely limited for concurrent programming. The ground guard is what makes safe, concurrent data distribution possible.

---

## Planned Guards (Not Yet Implemented)

### ⏳ `number(X)`
**Test for numeric type**

**Semantics**:
- Success: X bound to number (int or double)
- Suspend: X is unbound reader
- Fail: X bound to non-number

**Example** (future):
```prolog
safe_compute(X, Y) :- number(X) | execute('evaluate', [X? * 2, Y]).
```

---

### ⏳ `integer(X)`
**Test for integer type**

**Semantics**:
- Success: X bound to integer
- Suspend: X is unbound reader
- Fail: X bound to non-integer (including floats)

**Example** (future):
```prolog
safe_divide(X, Y, Z) :- integer(X), integer(Y), Y =\= 0 |
                        execute('evaluate', [X? / Y?, Z]).
```

---

## Planned Comparison Guards (Require Parser Extension)

### 📝 `X < Y`, `X =< Y`, `X > Y`, `X >= Y`
**Arithmetic comparison**

**Note**: Prolog uses `=<` (not `<=`) for "less than or equal"

**Semantics**:
- Success: Both X and Y bound to numbers AND condition holds
- Suspend: Either X or Y is unbound reader
- Fail: Both bound to numbers AND condition false

**Example** (future):
```prolog
factorial(N, F) :- integer(N), N > 0 |
                   execute('evaluate', [N? - 1, N1]),
                   factorial(N1?, F1),
                   execute('evaluate', [N? * F1?, F]).
factorial(N, 1) :- integer(N), N =< 0 | true.
```

**Parser Status**: Requires adding comparison tokens (`LT`, `GT`, `LE`, `GE`) to lexer and handling infix syntax in guard position.

---

### 📝 `X =:= Y`, `X =\= Y`
**Arithmetic equality and inequality**

**Semantics**:
- `=:=` (equality): Success if both bound and numerically equal
- `=\=` (inequality): Success if both bound and numerically different
- Both suspend if either operand unbound
- Both fail if condition doesn't hold

**Example** (future):
```prolog
safe_divide(X, Y, Z) :- number(X), number(Y), Y =\= 0 |
                        execute('evaluate', [X? / Y?, Z]).
```

**Parser Status**: Requires lexer support for multi-character operators `=:=` and `=\=`.

---

## Guards vs System Predicates (Critical Distinction)

| Aspect | Guards | System Predicates (via execute) |
|--------|--------|----------------------------------|
| **Semantics** | Three-valued (success/suspend/fail) | Two-valued (success/abort) |
| **Unbound Input** | Suspend (patient) | Abort (impatient) |
| **Syntax** | `Head :- Guard \| Body` | `execute('name', [Args])` |
| **Phase** | HEAD/GUARDS (before commit) | BODY (after commit) |
| **Side Effects** | Never | May have (I/O, mutations) |
| **Examples** | `known(X)`, `ground(X)`, `number(X)` | `evaluate/2`, `write/1`, `file_read/2` |

---

## Safe Programming Pattern

**Always use guards to ensure preconditions before execute:**

```prolog
% ❌ UNSAFE - aborts if X unbound or non-numeric
unsafe_double(X, Y) :-
  execute('evaluate', [X? * 2, Y]).

% ✅ SAFE - guard ensures X is bound number
safe_double(X, Y) :-
  number(X) |
  execute('evaluate', [X? * 2, Y]).

% ✅ SAFE - multiple guards ensure preconditions
safe_divide(X, Y, Z) :-
  number(X), number(Y), Y =\= 0 |
  execute('evaluate', [X? / Y?, Z]).
```

---

## Common Usage Patterns

### Pattern 1: Type Checking Before Execute
```prolog
process(X, Result) :-
  integer(X), X > 0 |
  execute('evaluate', [X? * X?, Result]).
```

### Pattern 2: Conditional Clause Selection
```prolog
compute(N, Result) :-
  integer(N), N > 10 |
  execute('evaluate', [N? * 2, Result]).
compute(N, Result) :-
  integer(N), N =< 10 |
  execute('evaluate', [N? + 10, Result]).
```

### Pattern 3: Default Case with Otherwise
```prolog
handle(X, done) :- integer(X) | process_int(X?).
handle(X, done) :- ground(X) | process_ground(X?).
handle(_, error) :- otherwise | true.
```

### Pattern 4: Safe Multiple Readers
```prolog
broadcast(Msg, [Msg?, Msg?, Msg?]) :- ground(Msg) | true.
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
  execute('write', ['known(42) succeeded']).

% Test suspension (requires runtime trace)
test_known_suspend :-
  known(X) |  % Should suspend on unbound X
  execute('write', ['Should not reach here']).

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

## Frequently Asked Questions

**Q: When should I use guards vs system predicates?**

A: Use guards for **pure tests** that check properties without side effects. Use system predicates (via execute) for **operations** that compute results or perform I/O.

**Q: Why do guards suspend instead of fail?**

A: Guards are **patient** (suspend on unbound variables) to enable concurrent programming. An unbound variable may become bound later through message passing, at which point the suspended goal can resume.

**Q: Can I use arithmetic in guards?**

A: ⏳ Not yet - `number(X)` and `X < Y` are planned but not implemented. Currently, use `known(X)` and `ground(X)`, then perform arithmetic via `execute('evaluate', ...)` in the body.

**Q: What's the difference between `known` and `ground`?**

A: `known(f(X))` succeeds if the structure f(X) is bound (even if X inside is unbound). `ground(f(X))` only succeeds if X is also bound (no unbound variables anywhere).

**Q: Why does `otherwise` check for failure, not suspension?**

A: If a previous clause suspended, it may still succeed when its readers are bound. `otherwise` only succeeds when all previous attempts **definitively failed**, not when they're waiting for data.

---

**End of Quick Reference**
