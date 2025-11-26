# GLP Structure Manipulation Predicates

**Version**: 1.1
**Date**: 2025-11-26
**Status**: Proposed

## Overview

These predicates enable decomposition and construction of compound terms (structures), essential for implementing recursive observers and generic term manipulation.

Based on FCP's `Savannah/Logix/utils.cp` implementation.

---

## Design Principles

1. **Guards are 3-valued** (success/suspend/fail)
2. **Body kernels are 2-valued** (success/abort) - they cannot suspend or fail
3. **Minimal primitive set** - only guards and body kernels that cannot be defined in GLP
4. **=.. defined as system predicate** using guards and body kernels

---

## Guard Predicates

### `list(L?)`

**Purpose**: Test if L is a complete (proper) list.

**Semantics**:
- If L? is `[]`: succeed
- If L? is `[H|T]` and T is a complete list: succeed
- If L? is `[H|T]` and T is unbound: suspend
- If L? is not a list (atom, number, struct): fail
- If L? is unbound: suspend

**Example**:
```prolog
process_list(L?, Result?) :- list(L?) | do_something(L?, Result).
```

**Note**: This tests for a *complete* list, not just list structure. `[1,2|X]` with unbound X suspends.

---

### `struct(T?)`

**Purpose**: Test if T is a compound structure (tuple), not atom, number, or list.

**Semantics**:
- If T? is a structure f(a1,...,an) where n > 0: succeed
- If T? is an atom, number, or list: fail
- If T? is unbound: suspend

**Example**:
```prolog
is_compound(T?) :- struct(T?) | true.
% is_compound(foo(a,b)) -> succeed
% is_compound(hello) -> fail
% is_compound([1,2]) -> fail
```

**FCP Note**: FCP uses `tuple/1` for this guard.

---

## Body Kernel Predicates

**Important**: Body kernels are **not accessible to user code**. They can only be called by system predicates (GLP code shipped with the runtime). User code must use the system predicate `=../2` which wraps these body kernels safely.

### `struct_to_list(T?, L)`

**Purpose**: Convert a structure to a list containing functor and arguments.

**Visibility**: Internal only (used by `=..` system predicate)

**Semantics**:
- T? must be bound to a structure f(a1,...,an)
- L is bound to `[f, a1, ..., an]`
- If T? is unbound: **abort** (body kernels don't suspend)
- If T? is not a structure: **abort**

**Example** (internal use only):
```prolog
% Used inside =.. system predicate
% struct_to_list(foo(a,b,c), L) -> L = [foo, a, b, c]
```

**FCP Note**: FCP's `tuple_to_dlist/3` is similar but uses difference lists.

---

### `list_to_struct(L?, T)`

**Purpose**: Construct a structure from a list containing functor and arguments.

**Visibility**: Internal only (used by `=..` system predicate)

**Semantics**:
- L? must be bound to a complete list `[f, a1, ..., an]` where f is an atom and n >= 1
- T is bound to the structure `f(a1, ..., an)`
- If L? is unbound or incomplete: **abort**
- If L? is empty or first element is not an atom: **abort**

**Example** (internal use only):
```prolog
% Used inside =.. system predicate
% list_to_struct([foo, a, b, c], T) -> T = foo(a,b,c)
```

**FCP Note**: FCP's `list_to_tuple/2` is similar.

---

## System Predicates (GLP Code)

### `=../2` (univ)

**Purpose**: Bidirectional conversion between structures and lists.

**Implementation**:
```prolog
% Clause 1: Decompose - Struct is bound
T? =.. L? :- struct(T?) | struct_to_list(T?, L).

% Clause 2: Compose - List is bound
T? =.. L? :- list(L?) | list_to_struct(L?, T).
```

**Usage**:
```prolog
% Decomposition
foo(a, b, c) =.. L.   % L = [foo, a, b, c]

% Composition
T =.. [bar, 1, 2].    % T = bar(1, 2)
```

**Note**: If both T and L are unbound, both clauses suspend.

---

## Use Case: Recursive Structure Observer

With these predicates, we can implement a recursive observer that handles arbitrary structures:

```prolog
% Observe any term, creating two copies Y and Z
observe(X?, Y?, Z?) :- ground(X?) | Y = X?, Z = X?.

observe(X?, Y?, Z?) :-
    struct(X?) |
    X? =.. [F?|Args?],
    observe_list(Args?, YArgs, ZArgs),
    Y =.. [F?|YArgs?],
    Z =.. [F?|ZArgs?].

observe(X?, Y?, Z?) :-
    X? = [H?|T?] |
    observe(H?, H1, H2),
    observe(T?, T1, T2),
    Y = [H1? | T1?],
    Z = [H2? | T2?].

observe([], [], []).

observe_list([], [], []).
observe_list([A?|As?], [B?|Bs?], [C?|Cs?]) :-
    observe(A?, B, C),
    observe_list(As?, Bs, Cs).
```

---

## Implementation Priority

1. **Guard predicates**: `list/1`, `struct/1`
2. **Body kernels**: `struct_to_list/2`, `list_to_struct/2`
3. **System predicate**: `=../2`

---

## On `system(X)` Guard for Metainterpreters

The user suggested a `system(X)` guard that succeeds if X is an atomic goal with a system predicate. This would be useful in metainterpreters to distinguish system calls from user-defined predicates.

**FCP Approach**: FCP does **not** have a `system(X)` guard. Instead, FCP metainterpreters use:
1. Explicit pattern matching for known system predicates
2. `otherwise` guard as a catch-all for unknown goals

**Example FCP pattern** (from control/self.cp):
```prolog
% Explicit cases for each known goal type
_select({reduce, Goal}, Controls) :- ... | ...
_select({_, Goal}, Controls) :- otherwise | Controls = unknown.
```

**GLP Options**:
1. **Follow FCP**: Use `otherwise` as fallback (simpler, no new guard needed)
2. **Add `system(X)`**: Would require runtime to track which predicates are "system" vs "user"

**Recommendation**: Start with FCP approach (otherwise), consider system guard if needed later.

---

## References

- FCP implementation: `/tmp/FCP/Savannah/Logix/utils.cp`
- FCP guard table: `/tmp/FCP/Savannah/Logix/system/guardtable.cp`
- FCP emulator: `/tmp/FCP/Savannah/Logix/EMULATOR/kernels.c`

---

**End of Specification**
