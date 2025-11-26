# GLP Structure Manipulation Predicates

**Version**: 1.0 (Draft)
**Date**: 2025-11-26
**Status**: Proposed

## Overview

These predicates enable decomposition and construction of compound terms (structures), essential for implementing recursive observers and generic term manipulation.

Based on FCP's `Savannah/Logix/utils.cp` implementation.

---

## Guard Predicates

### `arity(Term?, Arity?)`

**Purpose**: Get the arity (number of arguments) of a term.

**Semantics**:
- If Term? is a structure f(a1,...,an): Arity = n, succeed
- If Term? is an atom: Arity = 0, succeed
- If Term? is a number: Arity = 0, succeed
- If Term? is a list [H|T]: Arity = 2, succeed (list is './2')
- If Term? is unbound: suspend
- If Term? is a writer: fail

**Example**:
```prolog
get_arity(T, A?) :- arity(T?, A?) | true.
% get_arity(foo(a,b,c), A) -> A = 3
% get_arity(hello, A) -> A = 0
```

---

### `functor(Term?, Name?, Arity?)`

**Purpose**: Decompose a term into its functor name and arity.

**Semantics**:
- If Term? is a structure f(a1,...,an): Name = f, Arity = n, succeed
- If Term? is an atom: Name = Term, Arity = 0, succeed
- If Term? is unbound: suspend
- If Term? is a writer: fail

**Example**:
```prolog
decompose(T, F?, A?) :- functor(T?, F?, A?) | true.
% decompose(foo(a,b), F, A) -> F = foo, A = 2
```

**Note**: This is a guard that reads Term? and outputs to Name and Arity writers.

---

### `arg(N?, Term?, Arg?)`

**Purpose**: Get the Nth argument of a term (1-indexed).

**Semantics**:
- If N? is bound to integer, Term? is structure with arity >= N: Arg = Nth argument, succeed
- If N? is out of range (N < 1 or N > arity): fail
- If N? or Term? is unbound: suspend
- If Term? is not a structure: fail

**Example**:
```prolog
get_arg(N, T, A?) :- arg(N?, T?, A?) | true.
% get_arg(2, foo(a,b,c), A) -> A = b
```

**Note**: Arguments are 1-indexed per Prolog convention.

---

### `struct(Term?)`

**Purpose**: Test if Term is a compound structure (not atom, number, or list).

**Semantics**:
- If Term? is a structure f(a1,...,an) where n > 0: succeed
- If Term? is an atom, number, or list: fail
- If Term? is unbound: suspend

**Example**:
```prolog
is_struct(T) :- struct(T?) | true.
% is_struct(foo(a)) -> succeed
% is_struct(hello) -> fail
% is_struct([1,2]) -> fail
```

---

## Body Kernel Predicates

### `make_struct(Name?, Arity?, Struct)`

**Purpose**: Create a new structure with given functor and arity.

**Semantics**:
- Creates structure Name(V1, V2, ..., Vn) where Vi are fresh writers
- Name? must be an atom
- Arity? must be a positive integer
- Struct is bound to the new structure

**Example**:
```prolog
% Used internally by system predicates
build(F, N, S) :- atom(F?), integer(N?) | make_struct(F?, N?, S).
% build(foo, 3, S) -> S = foo(W1, W2, W3) where Wi are fresh writers
```

---

### `set_arg(N?, Struct, Value?)`

**Purpose**: Set the Nth argument of a structure.

**Semantics**:
- Binds the Nth argument (writer) of Struct to Value?
- N? must be in range [1, arity]
- The argument must be an unbound writer

**Note**: May not be needed if `make_struct` returns a structure with accessible writers.

---

## System Predicates (GLP Code)

### `struct_to_list(Term?, List?)`

**Purpose**: Convert a structure's arguments to a list.

**Implementation**:
```prolog
struct_to_list(T?, L?) :-
    arity(T?, N?) |
    struct_to_list_loop(T, N, L).

struct_to_list_loop(T, N, L?) :-
    N? > 0,
    arg(N?, T?, Arg?) |
    L = [Arg? | Rest],
    N1 := N? - 1,
    struct_to_list_loop(T?, N1?, Rest).

struct_to_list_loop(T, N, L?) :-
    N? =:= 0 |
    L = [].
```

**Example**:
```prolog
% struct_to_list(foo(a,b,c), L) -> L = [a,b,c]
```

---

### `list_to_struct(Name?, Args?, Struct)`

**Purpose**: Construct a structure from functor name and argument list.

**Implementation**:
```prolog
list_to_struct(Name?, Args?, S) :-
    atom(Name?), list(Args?) |
    length(Args?, N?),
    make_struct(Name?, N?, S1),
    fill_args(Args?, S1?, 1, S).

fill_args(Args, S, N, Result) :-
    Args? = [A | Rest] |
    set_arg(N?, S?, A?),
    N1 := N? + 1,
    fill_args(Rest?, S?, N1?, Result).

fill_args(Args, S, N, Result) :-
    Args? = [] |
    Result = S?.
```

**Example**:
```prolog
% list_to_struct(foo, [a,b,c], S) -> S = foo(a,b,c)
```

---

## Use Case: Recursive Structure Observer

With these predicates, we can implement a recursive observer that handles arbitrary structures:

```prolog
% Observe any term, creating two copies
observe(X, Y?, Z?) :- ground(X?) | Y = X?, Z = X?.

observe(X, Y?, Z?) :-
    struct(X?),
    functor(X?, F?, N?) |
    make_struct(F?, N?, Y1),
    make_struct(F?, N?, Z1),
    observe_args(X?, Y1?, Z1?, 1, N?),
    Y = Y1?, Z = Z1?.

observe(X, Y?, Z?) :-
    X? = [H|T] |
    observe(H?, H1, H2),
    observe(T?, T1, T2),
    Y = [H1? | T1?],
    Z = [H2? | T2?].

observe_args(X, Y, Z, I, N) :-
    I? =< N?,
    arg(I?, X?, Arg?) |
    arg(I?, Y?, YArg),
    arg(I?, Z?, ZArg),
    observe(Arg?, YArg, ZArg),
    I1 := I? + 1,
    observe_args(X?, Y?, Z?, I1?, N?).

observe_args(X, Y, Z, I, N) :-
    I? > N? |
    true.
```

---

## Implementation Priority

1. **Guard predicates**: `arity/2`, `functor/3`, `arg/3`, `struct/1`
2. **Body kernel**: `make_struct/3`
3. **System predicates**: `struct_to_list/2`, `list_to_struct/3`

---

## References

- FCP implementation: `/tmp/FCP/Savannah/Logix/utils.cp`
- FCP emulator: `/tmp/FCP/Savannah/Logix/EMULATOR/kernels.c` (do_make_tuple)

---

**End of Specification**
