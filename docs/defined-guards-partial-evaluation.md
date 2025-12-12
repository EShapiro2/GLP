# Defined Guards: Partial Evaluation Design

## Status: NOT IMPLEMENTED (Design Document)

This document describes the intended implementation of defined guards via partial evaluation at compile time. The current implementation uses bytecode-level pattern matching which is incorrect - it only tests patterns, it doesn't construct bindings.

## Overview

Defined guards are user-defined guard predicates defined as unit clauses. A **unit clause** is a clause with:
- Exactly one clause for the predicate
- No guards
- No body

Example:
```prolog
channel(ch(_, _)).           % Unit clause defining channel/1 guard
new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).  % Unit clause defining new_channel/2
```

When a defined guard is called in a guard position, it should be **reduced at compile time** using partial evaluation, not evaluated at runtime.

## Specification

From `docs/drafts/main_GLP_to_Dart (3).tex`:

> **Defined guard predicates**
> To support abstract data types and cleaner code organization, GLP provides for user-defined guards, defined unit clauses `p(T1,...,Tn)`. The call `p(S1,...,Sn)` in the guard is **folded to the equalities `T1=S1,...,Tn=Sn`** for each unit goal.

However, this description is simplified. The actual transformation is **partial evaluation** - compile-time goal reduction using GLP unification.

## Examples

### Example 1: Simple Pattern Test

```prolog
% Unit clause
channel(ch(_, _)).

% Original clause
test(X, ok) :- channel(X?) | true.
test(_, not_channel) :- otherwise | true.

% After partial evaluation:
test(ch(_, _), ok) :- true.
test(_, not_channel) :- otherwise | true.
```

Transformation:
1. Guard `channel(X?)` matches unit clause `channel(ch(_, _))`
2. GLP unification: `X?` (reader) with `ch(_, _)` (pattern)
   - Reader `X?` reads from writer `X`
   - Pattern `ch(_, _)` binds to `X`
   - Substitution: `{X → ch(_, _)}`
3. Apply substitution to clause head: `test(X, ok)` → `test(ch(_, _), ok)`
4. Remove guard, result: `test(ch(_, _), ok) :- true.`

### Example 2: Binding Construction

```prolog
% Unit clause
q(X, f(X?)).

% Original clause
p(X) :- q(X?, Y) | r(Y?).

% After partial evaluation:
p(X) :- r(f(X?)).
```

Transformation:
1. Guard `q(X?, Y)` matches unit clause `q(X1, f(X1?))` (renamed)
2. GLP unification:
   - Position 1: `X?` (reader in call) with `X1` (writer in unit clause)
     - Reader `X?` reads from writer `X`
     - Unit clause writer `X1` aliases to `X`: `{X1 → X}`
   - Position 2: `Y` (writer in call) with `f(X1?)` (term in unit clause)
     - Writer `Y` binds to `f(X1?)`: `{Y → f(X1?)}`
   - After resolving aliases: `{X1 → X, Y → f(X?)}`
3. Apply substitution to body: `r(Y?)` → `r(f(X?))`
4. Remove guard, result: `p(X) :- r(f(X?)).`

### Example 3: Head Binding

```prolog
% Unit clause
q(X, f(X?)).

% Original clause
p(X, Y?) :- q(X?, Y) | true.

% After partial evaluation:
p(X, f(X?)).
```

Transformation:
1. Same unification as Example 2: `{X1 → X, Y → f(X?)}`
2. Apply substitution to head: `p(X, Y?)` → `p(X, f(X?))`
3. Body is `true`, so result is a fact: `p(X, f(X?)).`

### Example 4: Cross-Linked Channels

```prolog
% Unit clause
new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).

% Original clause
make_pair(FCh1, FCh2?) :- new_channel(FCh1?, FCh2) | true.

% After partial evaluation:
make_pair(ch(X?, Y), ch(Y?, X)).
```

This creates the cross-linked channel structure at compile time.

## Algorithm

### Phase 1: Identify Unit Clauses

Scan all procedures and collect unit clauses:
```
unitClauses = {}
for each procedure P:
    if P has exactly 1 clause C:
        if C has no guards AND no body:
            unitClauses[P.name/P.arity] = C.head.args
```

### Phase 2: Transform Clauses

For each clause with guards:
```
repeat until fixpoint:
    for each guard G in clause.guards:
        if G.predicate/G.arity in unitClauses:
            unitArgs = unitClauses[G.predicate/G.arity]
            renamedArgs = rename_variables(unitArgs)  // Fresh names

            subst = glp_unify(G.args, renamedArgs)

            if subst is FAIL:
                // Dead clause - remove entirely
                remove clause from procedure
                break

            // Apply substitution to entire clause
            clause.head = apply_subst(clause.head, subst)
            clause.guards = apply_subst(clause.guards, subst)
            clause.body = apply_subst(clause.body, subst)

            // Remove the processed guard
            remove G from clause.guards
```

### Phase 3: GLP Unification

Two-phase unification between call args and unit clause args:

```
glp_unify(callArgs, unitArgs):
    subst = {}

    // Phase 1: Writer Assignment (σw)
    for i in 0..length:
        callArg = callArgs[i]
        unitArg = unitArgs[i]

        if callArg is Writer:
            // Writer in call binds to unit clause term
            subst[callArg.name] = unitArg

        else if callArg is Reader:
            if unitArg is Writer:
                // Reader reads from its paired writer
                // Unit clause writer aliases to call's writer
                subst[unitArg.name] = Writer(callArg.name)
            else:
                // Reader's paired writer binds to pattern
                subst[callArg.name] = unitArg

        else:  // Constant or structure
            if not structural_match(callArg, unitArg, subst):
                return FAIL

    // Resolve substitution chains
    resolve_chains(subst)

    return subst
```

### Phase 4: Apply Substitution

```
apply_subst(term, subst):
    if term is VarTerm:
        if term.name in subst:
            replacement = subst[term.name]
            if term.isReader and replacement is VarTerm and not replacement.isReader:
                // Preserve reader status
                return VarTerm(replacement.name, isReader=true)
            return apply_subst(replacement, subst)
        return term

    if term is StructTerm:
        return StructTerm(term.functor, [apply_subst(arg, subst) for arg in term.args])

    if term is ListTerm:
        return ListTerm(apply_subst(term.head, subst), apply_subst(term.tail, subst))

    return term  // Constants, underscores
```

## Special Cases

### Underscores in Unit Clauses

Underscores (`_`) in unit clause patterns should remain as underscores in the transformed clause. They are wildcards that match anything.

```prolog
channel(ch(_, _)).
test(X, ok) :- channel(X?) | true.
% Transforms to:
test(ch(_, _), ok) :- true.
```

Do NOT rename underscores to fresh variables - this would cause SRSW violations.

### The `=` Predicate

`=` (unification) is also a defined guard with unit clause:
```prolog
=(X, X?).
```

When `T1 = T2` appears in a guard, it's a call to `=/2` and should be processed as a defined guard, producing unification bindings.

### Nested Defined Guards

After processing one defined guard, others may become processable. Iterate until fixpoint.

### Dead Clauses

If writer assignment fails during GLP unification, the clause is statically dead (can never succeed). Remove it from the procedure.

## Implementation Location

The partial evaluator should run:
1. **After parsing** - needs complete AST
2. **Before SRSW checking** - transformed code must pass SRSW
3. **Before code generation** - codegen sees transformed AST

Suggested location: `analyzer.dart`, as a transformation pass before `_analyzeClause`.

## Current Implementation

**Status: NOT IMPLEMENTED**

The bytecode-level approach that was previously in `codegen.dart` has been removed. That approach was incorrect because:
1. It only tested patterns, didn't construct bindings
2. HEAD-style opcodes are for tentative unification, not binding construction
3. The guard disappeared but bindings weren't applied to the rest of the clause

Proper partial evaluation as described in this document is pending implementation.

## Testing Plan

Test cases needed:

1. **Simple pattern test**: `channel(ch(_, _))` with `channel(X?)`
2. **Binding construction**: `q(X, f(X?))` with `q(X?, Y)`
3. **Head binding**: `q(X, f(X?))` with head `p(X, Y?)`
4. **Cross-linked structures**: `new_channel/2` example
5. **Nested defined guards**: Multiple defined guards in sequence
6. **Dead clause elimination**: Guard that always fails
7. **Unification guard**: `X = Y` as defined guard
8. **Complex patterns**: Lists, nested structures

## References

- `docs/drafts/main_GLP_to_Dart (3).tex` - Defined guard specification
- Partial deduction / unfold-fold transformation in logic programming
- GLP two-phase unification (writer assignment σw, reader assignment σr)

## Future Work

1. Implement proper partial evaluation as described
2. Remove bytecode-level defined guard code from `codegen.dart`
3. Add comprehensive test suite
4. Consider optimization: cache transformed clauses
5. Consider error messages for dead clauses
