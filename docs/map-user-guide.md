# GLP Maps — User Guide

Maps provide O(1) key-value storage for GLP programs. They are backed by Dart's `HashMap` and exposed through four built-in operations.

## Quick Start

```glp
% Create a map, add entries, look up a value.

procedure demo(_).
demo(Result?) :-
    map_new(M0),
    map_put(M0?, name, alice, M1),
    map_put(M1?, age, 30, M2),
    map_get(M2?, name, Result).

% Query: demo(X).
% Output: X = alice
```

## Operations

| Operation | What it does |
|-----------|-------------|
| `map_new(M)` | Create an empty map, bind it to `M` |
| `map_put(M?, Key?, Val?, M1)` | Insert `Key → Val` into map `M`, bind result to `M1` |
| `map_get(M?, Key?, Val)` | Look up `Key` in map `M`, bind result to `Val` |
| `map_contains(M?, Key?)` | Guard: succeeds if `Key` exists in `M`, fails otherwise |

All operations are O(1).

## How Map Threading Works

Maps follow GLP's SRSW (Single Reader Single Writer) rule. Each map variable is written once and read once. When you modify a map, you pass the old variable in and get a new variable out:

```glp
map_new(M0),                %% M0 = {}
map_put(M0?, k1, v1, M1),   %% M1 = {k1: v1}       (M0 consumed)
map_put(M1?, k2, v2, M2),   %% M2 = {k1: v1, k2: v2} (M1 consumed)
map_get(M2?, k1, Result).   %% Result = v1           (M2 consumed)
```

Each variable (`M0`, `M1`, `M2`) appears with `?` (reader) exactly once after being created (writer). This threading pattern ensures each map has a single owner at any point in time.

## Looking Up Values

Use `map_get/3` to look up a key. It is a stdlib wrapper that combines a `map_contains` guard with a `_map_get` body kernel. When a key might be missing, use `otherwise` for a fallback:

```glp
procedure lookup(_?, _?, _).
lookup(M, Key, Val?) :-
    map_contains(M?, Key?) |
    _map_get(M?, Key?, Val).
lookup(_, _, not_found?) :-
    otherwise | true.
```

Or use the stdlib `map_get` wrapper with an `otherwise` clause on the calling procedure:

```glp
procedure find(_?, _?, _).
find(M, Key, Val?) :-
    map_get(M?, Key?, Val).
find(_, _, not_found?) :-
    otherwise | true.
```

## Using `map_contains` in Guards

`map_contains/2` is a guard — it can appear before the `|` in a clause. It succeeds if the key exists in the map, fails otherwise. This drives clause selection:

```glp
procedure check(_?, _?, _).
check(M, Key, yes?) :-
    map_contains(M?, Key?) | true.
check(_, _, no?) :-
    otherwise | true.
```

## Overwriting a Key

Putting a key that already exists overwrites the old value:

```glp
procedure overwrite_demo(_).
overwrite_demo(Result?) :-
    map_new(M0),
    map_put(M0?, color, red, M1),
    map_put(M1?, color, blue, M2),
    map_get(M2?, color, Result).

% Query: overwrite_demo(X).
% Output: X = blue
```

## Storing Complex Values

Map values can be any GLP term — atoms, numbers, structures, or lists:

```glp
% Storing a structure
procedure struct_demo(_).
struct_demo(Result?) :-
    map_new(M0),
    map_put(M0?, alice, person(alice, 30), M1),
    map_get(M1?, alice, Result).

% Query: struct_demo(X).
% Output: X = person(alice, 30)
```

```glp
% Storing a list
procedure list_demo(_).
list_demo(Result?) :-
    map_new(M0),
    map_put(M0?, scores, [10, 20, 30], M1),
    map_get(M1?, scores, Result).

% Query: list_demo(X).
% Output: X = [10, 20, 30]
```

Retrieved lists are fully functional GLP lists — you can pattern-match, traverse, and compute over them:

```glp
procedure get_head(_?, _).
get_head([H|_], H?).

procedure head_demo(_).
head_demo(H?) :-
    map_new(M0),
    map_put(M0?, data, [100, 200, 300], M1),
    map_get(M1?, data, List),
    get_head(List?, H).

% Query: head_demo(X).
% Output: X = 100
```

## Map Keys

Keys must be ground constants: atoms (like `alice`, `color`) or numbers (like `42`, `3.14`). Structures and lists cannot be used as keys.

```glp
% Atom keys
map_put(M0?, alice, 1, M1),

% Number keys
map_put(M1?, 42, hello, M2),

% Mixed keys in the same map
map_put(M2?, bob, 2, M3),
map_put(M3?, 99, world, M4),
```

## Building a Map from a List of Commands

A common pattern is processing a list of commands to build up a map:

```glp
CmdList ::= [] ; [_|CmdList].

procedure build_map(_?, CmdList?, _).
build_map(M, [add(Key, Val)|Cmds], Final?) :-
    map_put(M?, Key?, Val?, M1),
    build_map(M1?, Cmds?, Final).
build_map(M, [], M?).

procedure demo_build(_).
demo_build(Result?) :-
    map_new(M0),
    build_map(M0?, [add(alice, 42), add(bob, 99), add(carol, 7)], Final),
    map_get(Final?, bob, Result).

% Query: demo_build(X).
% Output: X = 99
```

## Building a Map from a Counted Loop

Use a recursive counter to insert N entries:

```glp
procedure fill_map(Number?, _?, _).
fill_map(N, M, Final?) :-
    N? > 0 |
    V := N? * 10,
    map_put(M?, N?, V?, M1),
    N1 := N? - 1,
    fill_map(N1?, M1?, Final).
fill_map(N, M, M?) :-
    N? =:= 0 | true.

procedure demo_fill(_, _).
demo_fill(A?, B?) :-
    map_new(M0),
    fill_map(5, M0?, Final),
    map_get(Final?, 1, A),
    map_get(Final?, 5, B).

% Query: demo_fill(X, Y).
% Output: X = 10, Y = 50
```

## Common Mistakes

### 1. Using `map_put` in a guard

`map_put` is a body kernel — it goes **after** the `|`, not before it.

```glp
% WRONG — map_put cannot appear in a guard
my_proc(M) :- map_put(M?, k, v, M1) | use(M1?).

% CORRECT — map_put in the body
my_proc(M) :- ground(M?) | map_put(M?, k, v, M1), use(M1?).
```

### 2. Reading a map variable twice without grounding

SRSW allows each variable to be read once. To read a map multiple times, the variable must be grounded first (e.g., via a `map_contains` guard or `ground` guard). The stdlib `map_get` handles this automatically by using `map_contains` in its guard.

```glp
% This works — map_get uses map_contains guard internally,
% which grounds M, allowing multiple reads
procedure two_lookups(_?, _, _, _, _).
two_lookups(M, K1, K2, V1?, V2?) :-
    map_get(M?, K1?, V1),
    map_get(M?, K2?, V2).
```

### 3. Using wrong `procedure` declaration types

The procedure declaration uses mode types (`_`, `_?`, `Number?`, etc.), not variable names:

```glp
% WRONG — Result is not a type
procedure my_proc(Result).

% CORRECT — _ means untyped output
procedure my_proc(_).
```
