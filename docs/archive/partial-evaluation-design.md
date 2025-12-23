# GLP Partial Evaluator Design

## Status: STAGE 1 IMPLEMENTED

Stage 1 (Defined Guards) is fully implemented and tested.

**Implementation**: `lib/compiler/analyzer.dart` - `PartialEvaluator` class

**Tests**:
- `glp/test_defined_guards.glp` - Basic channel/1 guard
- `glp/test_relay_send.glp` - send/3 guard with nested structures
- `glp/test_channel_guards.glp` - Full channel operations (send, receive, new_channel)

**REPL test coverage**: 145 tests passing including defined guard tests.

---

This document specifies the design of the GLP partial evaluator. Stage 2 (Meta-interpreter Specialization) is future work.

## Overview

### What is Partial Evaluation?

Partial evaluation (also called partial deduction in logic programming) is a program transformation technique that specializes a program with respect to known inputs. In essence, it performs computation at compile time that would otherwise happen at runtime.

The core operations are:
- **Unfold**: Replace a goal with its definition (resolve against a clause)
- **Fold**: Recognize that goals are an instance of a definition

### Why Partial Evaluation for GLP?

1. **Defined Guards**: User-defined guard predicates (unit clauses) are reduced at compile time, enabling abstract data types and cleaner code organization.

2. **Meta-interpreter Specialization** (future): Remove interpretation overhead by specializing a meta-interpreter for a specific object program.

### Key References

- Burstall & Darlington (1977): Unfold/fold for functional programming
- Tamaki & Sato (1984): Unfold/fold for logic programming
- Futamura (1971): Compiling away interpretation levels
- Safra & Shapiro (1988): "Meta interpreters for real"
- Sterling & Shapiro, "The Art of Prolog", Chapter 18

---

## Stage 1: Defined Guards

### Specification

A **defined guard** is a predicate defined by a **unit clause** that is called in guard position.

A **unit clause** is a clause with:
- Exactly one clause for the predicate
- No guards
- No body (or body is `true`)

**Example unit clauses:**
```prolog
channel(ch(_, _)).                              % Type test
pair(p(_, _)).                                  % Type test
new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).          % Constructor with cross-linking
```

### Compile-Time Behavior

When a defined guard `p(S₁,...,Sₙ)` appears in a clause, and `p(T₁,...,Tₙ)` is a unit clause:

1. **Rename** unit clause variables to fresh names
2. **Unify** call arguments with unit clause arguments using GLP unification
3. **Apply** resulting substitution to clause head, remaining guards, and body
4. **Remove** the guard from the clause

### Compilation Outcomes

| Unification Result | Meaning | Action |
|-------------------|---------|--------|
| **Success(σ)** | Guard reducible at compile time | Apply σ to clause, remove guard |
| **Suspend(readers)** | Cannot determine at compile time | **Compilation error** |
| **Fail** | Guard can never succeed | **Compilation error** |

Both suspension and failure are compilation errors. This catches bugs early:
- Suspension means the guard requires runtime information unavailable at compile time
- Failure means the clause is unreachable (dead code, likely a bug)

### Transformation Examples

#### Example 1: Simple Pattern Test

```prolog
% Unit clause
channel(ch(_, _)).

% Original clause
test(X, ok) :- channel(X?) | true.

% After transformation:
test(ch(_, _), ok) :- true.
```

**Steps:**
1. Guard `channel(X?)` matches unit clause `channel(ch(_, _))`
2. Unify: `X?` (reader) against `ch(_, _)` (structure)
3. Result: `{X → ch(_, _)}`
4. Apply to head: `test(X, ok)` → `test(ch(_, _), ok)`
5. Remove guard

#### Example 2: Binding Construction

```prolog
% Unit clause
q(X, f(X?)).

% Original clause
p(X) :- q(X?, Y) | r(Y?).

% After transformation:
p(X) :- r(f(X?)).
```

**Steps:**
1. Rename unit clause: `q(X₁, f(X₁?))`
2. Unify `(X?, Y)` with `(X₁, f(X₁?))`
   - `X?` vs `X₁`: reader vs writer → alias `{X₁ → X}`
   - `Y` vs `f(X₁?)`: writer vs structure → bind `{Y → f(X₁?)}`
3. Resolve: `{X₁ → X, Y → f(X?)}`
4. Apply to body: `r(Y?)` → `r(f(X?))`
5. Remove guard

#### Example 3: Head Binding

```prolog
% Unit clause
q(X, f(X?)).

% Original clause
p(X, Y?) :- q(X?, Y) | true.

% After transformation:
p(X, f(X?)).
```

**Steps:**
1. Same unification as Example 2: `{X₁ → X, Y → f(X?)}`
2. Apply to head: `p(X, Y?)` → `p(X, f(X?))`
3. Body is `true`, result is a fact

#### Example 4: Cross-Linked Channels

```prolog
% Unit clause
new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).

% Original clause
make_pair(C1, C2?) :- new_channel(C1?, C2) | true.

% After transformation:
make_pair(ch(X?, Y), ch(Y?, X)).
```

This creates the cross-linked channel structure at compile time.

#### Example 5: Compilation Error - Suspension

```prolog
% Unit clause
channel(ch(_, _)).

% This clause CANNOT be compiled:
test(X) :- channel(X?) | process(X?).
%          ^^^^^^^^^^^
% Error: X? is unbound reader, cannot match against ch(_, _) at compile time
```

#### Example 6: Compilation Error - Failure

```prolog
% Unit clause
channel(ch(_, _)).

% This clause CANNOT be compiled:
test(R?) :- channel(foo) | R = matched.
%           ^^^^^^^^^^^^
% Error: Constant 'foo' does not match pattern 'ch(_, _)'
%        This clause is unreachable
```

#### Example 7: Nested Defined Guards

```prolog
% Unit clauses
wrapper(w(X?)).
inner(i(_)).

% Original clause
test(X, ok) :- wrapper(X?), inner(X?) | true.

% After first guard (wrapper):
test(w(Y?), ok) :- inner(w(Y?)?) | true.
%                  Note: X became w(Y?), now inner sees w(Y?)

% After second guard (inner):
% inner(w(Y?)?) must match inner(i(_))
% w(Y?) ≠ i(_) → Compilation error: guard can never succeed
```

---

## GLP Unification for Partial Evaluation

### Context

This is **compile-time** unification between call arguments and unit clause arguments. Variables are symbolic - we compute a substitution to transform the clause, not runtime bindings.

### Two-Phase Algorithm

**Phase 1: Collection**

Traverse argument pairs, accumulating:
- Tentative substitution σ̂
- Preliminary suspension set S (readers that might need to suspend)

**Phase 2: Resolution**

Compute final suspension set:
```
S' = {X? ∈ S : X ∉ dom(σ̂)}
```
- If S' = ∅: **Success** with σ̂
- If S' ≠ ∅: **Suspend** on S'

### Unification Cases

For each pair (call argument, unit clause argument after renaming):

| Call Arg | Unit Arg | Action |
|----------|----------|--------|
| Writer X | Writer Y | Alias: add `{Y → X}` to σ̂ |
| Writer X | Reader Y? | Should not occur in well-formed unit clause |
| Writer X | Constant c | Bind: add `{X → c}` to σ̂ |
| Writer X | Structure f(...) | Bind: add `{X → f(...)}` to σ̂, recurse into structure |
| Reader X? | Writer Y | Alias: add `{Y → X}` to σ̂ (Y becomes alias for X) |
| Reader X? | Constant c | Add X? to S; if X ∈ dom(σ̂) later, resolved; else suspend |
| Reader X? | Structure f(...) | Add X? to S; if X ∈ dom(σ̂) later with matching structure, recurse; else suspend |
| Constant c | Constant c | OK (match) |
| Constant c | Constant d | **Fail** (c ≠ d) |
| Constant c | Writer Y | Bind: add `{Y → c}` to σ̂ |
| Constant c | Structure f(...) | **Fail** |
| Structure f(a₁..aₙ) | Structure f(b₁..bₙ) | Recurse on arguments |
| Structure f(...) | Structure g(...) | **Fail** (functor mismatch) |
| `_` (underscore) | anything | OK (wildcard matches anything, stays `_`) |
| anything | `_` (underscore) | OK (wildcard matches anything, stays `_`) |

### Underscore Handling

Underscores (`_`) in unit clauses are **not renamed** to fresh variables. They remain as underscores in the output, matching any value without binding.

```prolog
channel(ch(_, _)).
test(X, ok) :- channel(X?) | true.
% Result: test(ch(_, _), ok) :- true.
% NOT: test(ch(_G1, _G2), ok) :- true.
```

### Substitution Resolution

After collection, resolve substitution chains:
```
If σ = {X → Y, Y → f(Z)}
Then resolved σ = {X → f(Z), Y → f(Z)}
```

Apply resolved substitution to transform the clause.

---

## Error Messages

### Suspension Error

```
Error at <file>:<line>: Cannot reduce defined guard at compile time.
  Guard: channel(X?)
  Unit clause: channel(ch(_, _))
  Unbound reader 'X?' cannot be matched against pattern 'ch(_, _)'.

  Defined guards must be fully reducible at compile time.
  Ensure the guard argument is bound by the clause head or a preceding guard.
```

### Failure Error

```
Error at <file>:<line>: Defined guard can never succeed.
  Guard: channel(foo)
  Unit clause: channel(ch(_, _))
  Constant 'foo' does not match pattern 'ch(_, _)'.

  This clause is unreachable and likely indicates a bug.
```

---

## Implementation

### Location

All partial evaluation code resides in `lib/compiler/analyzer.dart`.

Runs **after parsing**, **before** SRSW analysis (`_analyzeClause`).

### Pipeline Position

```
Source → Parser → [Partial Evaluator] → SRSW Analysis → Codegen → Bytecode
                         ↑
                   Transform AST here
```

### Functions

```dart
/// Entry point: transform all defined guards in program
Program transformDefinedGuards(Program program)

/// Phase 1: Collect unit clauses from program
/// Returns map from "name/arity" to list of head arguments
Map<String, List<Term>> _collectUnitClauses(Program program)

/// Phase 2: Transform a single clause
/// Returns transformed clause, or throws CompileError
Clause _transformClause(Clause clause, Map<String, List<Term>> unitClauses)

/// Core: GLP unification for partial evaluation
/// Returns UnifyResult (Success/Fail/Suspend)
UnifyResult _glpUnifyForPE(List<Term> callArgs, List<Term> unitArgs)

/// Rename variables in unit clause to fresh names (except underscores)
List<Term> _renameUnitClauseVars(List<Term> args, int counter)

/// Apply substitution to a term
Term _applySubstitution(Term term, Map<String, Term> subst)

/// Resolve substitution chains
Map<String, Term> _resolveSubstitution(Map<String, Term> subst)
```

### Data Types

```dart
/// Result of compile-time GLP unification
sealed class UnifyResult {}

class UnifySuccess extends UnifyResult {
  final Map<String, Term> substitution;
  UnifySuccess(this.substitution);
}

class UnifyFail extends UnifyResult {
  final String reason;  // For error message
  UnifyFail(this.reason);
}

class UnifySuspend extends UnifyResult {
  final Set<String> unboundReaders;  // Variable names
  UnifySuspend(this.unboundReaders);
}
```

---

## Test Cases

### Must Pass (after implementation)

Located in `glp/test_defined_guards.glp`:

```prolog
% Unit clause
channel(ch(_, _)).

% Test 1: Bound argument - should transform and succeed
test(ch(A?, B), R) :- channel(...) | R = ok.
% Query: test(ch(X?, Y), R). → R = ok

% Test 2: Non-matching constant - should fail at runtime (no matching clause)
% Query: test(foo, R). → fails (no clause matches)

% Test 3: Unbound reader - should be compilation error
% test(X, R) :- channel(X?) | R = ok.  % Cannot compile this
```

### Additional Test Cases to Add

```prolog
% Binding construction
q(X, f(X?)).
bind_test(X, Y?) :- q(X?, Y) | true.
% Query: bind_test(a, R). → R = f(a)

% Cross-linked channels
new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).
channel_test(C1, C2?) :- new_channel(C1?, C2) | true.
% Query: channel_test(ch(A?, B), R). → R = ch(B?, A)

% Nested guards (should fail - types incompatible)
wrapper(w(_)).
inner(i(_)).
% nested_test(X) :- wrapper(X?), inner(X?) | ok.  % Compilation error
```

---

## Stage 2: Meta-interpreter Specialization (Future)

### Overview

Specialize a meta-interpreter with respect to an object program, removing interpretation overhead.

### GLP Meta-interpreter

```prolog
run(true).
run((A,B)) :- run(A?), run(B?).
run(A) :- tuple(A?) | reduce(A?, B), run(B?).
```

### Object Program Encoding

```prolog
reduce(merge([X|Xs],Ys,[X?|Zs?]), merge(Xs?,Ys?,Zs)).
reduce(merge(Xs,[Y|Ys],[Y?|Zs?]), merge(Xs?,Ys?,Zs)).
reduce(merge([],[],[]), true).
```

### Specialization Goal

Transform `run(merge(A,B,C))` into direct `merge` clauses:

```prolog
run_merge([X|Xs],Ys,[X?|Zs?]) :- run_merge(Xs?,Ys?,Zs).
run_merge(Xs,[Y|Ys],[Y?|Zs?]) :- run_merge(Xs?,Ys?,Zs).
run_merge([],[],[]).
```

### Additional Considerations

- Termination control (unfolding depth limits)
- Generalization strategies
- `should_unfold` / `should_fold` declarations
- Preserving concurrent fork semantics

Details to be specified when Stage 1 is complete.

---

## References

- `docs/defined-guards-partial-evaluation.md` - Original design notes
- `chapters/glp_core.tex` - GLP unification specification
- Art of Prolog, Chapter 18 - Program Transformation
- Concurrent Prolog: Collected Papers - Meta interpreters for real
