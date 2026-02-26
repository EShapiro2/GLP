# Dart Map Support for GLP

## Motivation

GLP programs sometimes need key-value lookup with O(1) time complexity. The natural GLP approach — association lists — gives O(n) lookup. For performance-critical use cases, we bridge to Dart's `HashMap` via a small set of built-in operations.

## Operations

Four operations, divided by execution phase:

| Operation | Phase | Semantics |
|-----------|-------|-----------|
| `map_new(M)` | Body | Create empty map, bind M to it |
| `map_put(M?, Key?, Val?, M1)` | Body | Add entry, bind M1 to new map |
| `map_get(M?, Key?, Val)` | Guard + Body | Look up key, bind Val to value |
| `map_contains(M?, Key?)` | Guard | Test if key exists |

**Body goals** produce values (create/modify maps). **Guards** test values (lookup/membership) and drive clause selection via success/failure.

`map_get` is implemented as a two-step operation: a `map_contains` guard (pure test) followed by a `_map_get` body kernel (binding). This keeps guards free of side effects.

## Representation

A map is an opaque Dart object stored directly on the GLP heap as a `MapTerm` — a new `Term` subclass wrapping a Dart `Map<Object, Term>`. This follows the precedent of `MutualRefTerm` (used for mutable stream references).

- No external registry needed
- Dart's garbage collector handles cleanup automatically
- The map handle threads through variables like any other GLP term

## Functional (Immutable) Semantics

`map_put` does **not** mutate the original map. It creates a new `MapTerm` containing a copy of the old map plus the new entry. The old map variable is never used again (guaranteed by SRSW), but the copy ensures correctness if the old map is referenced from within a data structure.

## SRSW Compliance

Maps are threaded through variables, one operation at a time:

```
map_new(M0),              %% M0 = empty map
map_put(M0?, k1, v1, M1), %% M1 = {k1: v1}
map_put(M1?, k2, v2, M2), %% M2 = {k1: v1, k2: v2}
use(M2?).                  %% consume final map
```

Each variable (M0, M1, M2) is written once and read once — fully SRSW-compliant.

## GLP Usage Examples

### Building and querying a map

```prolog
example(Result) :-
    map_new(M0),
    map_put(M0?, alice, 42, M1),
    map_put(M1?, bob, 99, M2),
    lookup(M2?, alice, Result).

%% Guard-based clause selection
lookup(M, Key, Val) :-
    map_get(M?, Key?, Val) | true.

lookup(M, Key, not_found) :-
    otherwise | true.
```

### Map as agent state

```prolog
agent(Contacts, [add(Name, Addr)|Msgs]) :-
    map_put(Contacts?, Name?, Addr?, Contacts1) |
    agent(Contacts1?, Msgs?).

agent(Contacts, [lookup(Name, Reply)|Msgs]) :-
    map_get(Contacts?, Name?, Addr) |
    Reply = Addr?,
    agent(Contacts?, Msgs?).

agent(Contacts, [lookup(Name, Reply)|Msgs]) :-
    otherwise |
    Reply = not_found,
    agent(Contacts?, Msgs?).
```

## Time Complexity

| Operation | Complexity |
|-----------|------------|
| `map_new` | O(1) |
| `map_get` | O(1) amortized |
| `map_contains` | O(1) amortized |
| `map_put` | O(n) — copies the map |

`map_put` is O(n) due to the copy. This is the cost of functional/immutable semantics. In practice, SRSW guarantees the old map is dead after `map_put`, so a future optimization could mutate in place (O(1) amortized) when safe.

## Implementation Approach

The implementation uses existing GLP infrastructure:

- **`MapTerm`**: New `Term` subclass in `terms.dart`, stored in `ValueTag` heap cells (same as `MutualRefTerm`)
- **Body operations** (`map_new`, `map_put`): Implemented as body kernels (same mechanism as `_stream_append`)
- **Guard operations** (`map_contains`): Implemented as guard cases in the bytecode runner (same mechanism as `ground`, `known`)
- **`map_get`**: GLP-level wrapper combining `map_contains` guard + `_map_get` body kernel

No changes to the parser, compiler, or type system core are needed. The operations plug into existing extension points.

## Design Decisions (Resolved)

1. **`map_get` is a GLP stdlib wrapper**, not a built-in. It combines a `map_contains` guard (pure test) with a `_map_get` body kernel (binding). This keeps guards side-effect free.

2. **Untyped operations.** Keys and values are any GLP terms. No polymorphic types needed. Same approach as other body kernels (`_stream_append`, etc.).

3. **No `is_map` guard** in initial release. Maps are always threaded through fresh variables (M0, M1, M2...), so SRSW is satisfied without relaxation. Add later if a real use case needs it.

4. **Stdlib placement.** The `map_get` wrapper lives in `programs/stdlib/`. Users load it when they need maps.

### Built-ins (Dart-level)

| Operation | Kind | Description |
|-----------|------|-------------|
| `map_new(M)` | Body kernel | Create empty map, bind M |
| `map_put(M?, Key?, Val?, M1)` | Body kernel | Copy map + new entry, bind M1 |
| `_map_get(M?, Key?, Val)` | Body kernel | Look up key, bind Val |
| `map_contains(M?, Key?)` | Guard | Test if key exists (success/fail) |

### Stdlib (GLP-level)

```prolog
map_get(M, Key, Val) :-
    map_contains(M?, Key?) |
    _map_get(M?, Key?, Val).
```

## Scope

Initial release: `map_new`, `map_put`, `_map_get`, `map_contains` (built-ins) + `map_get` (stdlib wrapper).

Possible future additions: `map_remove`, `map_keys`, `map_size`, `map_merge`, `is_map`.

## Trade-offs

**Why not pure GLP?** Association lists are O(n) for lookup. Balanced trees in GLP would give O(log n) but are complex to implement correctly in GLP. Dart's HashMap gives O(1) with minimal bridging code.

**Why not a general FFI?** A general foreign function interface would be over-engineered for this need. Four focused operations are simpler, safer, and sufficient. If more Dart bridging is needed later, the same body-kernel + guard mechanism can be reused.

**Why copy on put?** Correctness. The old map might be reachable from a data structure even though SRSW says the variable won't be read again. Copying is safe. Mutation-in-place is a future optimization.
