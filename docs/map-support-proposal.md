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

---

# Full Specification

## 1. MapTerm — Dart Class

**File:** `glp_runtime/lib/runtime/terms.dart`

```dart
class MapTerm implements Term {
  final Map<Object, Term> entries;

  MapTerm(this.entries);

  @override
  String toString() {
    if (entries.isEmpty) return 'map({})';
    final pairs = entries.entries.map((e) => '${e.key}: ${e.value}').join(', ');
    return 'map({$pairs})';
  }
}
```

`MapTerm` wraps a Dart `Map<Object, Term>`. Keys are Dart-level objects (strings, ints, doubles) extracted from GLP `ConstTerm` values. Values are GLP `Term` objects stored as-is.

A `MapTerm` is stored on the heap in a `ValueTag` cell, the same way `MutualRefTerm` and other ground values are stored.

## 2. Key Extraction

Map keys must be ground constants. The key extraction function converts a GLP term to a Dart key:

```dart
Object? _extractMapKey(GlpRuntime rt, Object? arg) {
  final val = _deref(rt, arg);
  if (val is ConstTerm) return val.value;  // string, int, double
  if (val is num) return val;
  if (val is String) return val;
  return null;  // not a valid map key
}
```

Non-constant keys (structures, lists, variables) are **not supported** — the kernel aborts. This ensures O(1) hash lookup.

## 3. Body Kernel: `map_new/1`

**Signature:** `map_new(M)`
**Phase:** Body
**Args:** M — unbound writer

**Semantics:** Create a new empty `MapTerm` and bind M to it.

```dart
BodyKernelResult mapNewKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 1) {
    print('[ABORT] map_new/1: expected 1 argument, got ${args.length}');
    return BodyKernelResult.abort;
  }
  return _bindResult(rt, args[0], MapTerm({}));
}
```

**Registration:** `registry.register('map_new', 1, mapNewKernel);`

## 4. Body Kernel: `map_put/4`

**Signature:** `map_put(M?, Key?, Val?, M1)`
**Phase:** Body
**Args:** M — reader (existing map), Key — reader (ground constant), Val — reader (any term), M1 — unbound writer (new map)

**Semantics:** Dereference M to get a `MapTerm`. Extract a Dart key from Key. Dereference Val. Create a **new** `MapTerm` whose entries are a copy of M's entries plus `{key: val}`. Bind M1 to the new map.

If Key already exists, its value is **overwritten** in the new map.

```dart
BodyKernelResult mapPutKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 4) {
    print('[ABORT] map_put/4: expected 4 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }

  final mapArg = _deref(rt, args[0]);
  if (mapArg is! MapTerm) {
    print('[ABORT] map_put/4: first argument must be a MapTerm');
    return BodyKernelResult.abort;
  }

  final key = _extractMapKey(rt, args[1]);
  if (key == null) {
    print('[ABORT] map_put/4: second argument must be a ground constant');
    return BodyKernelResult.abort;
  }

  final val = _deref(rt, args[2]);
  final newEntries = Map<Object, Term>.of(mapArg.entries);
  newEntries[key] = (val is Term) ? val : ConstTerm(val);

  return _bindResult(rt, args[3], MapTerm(newEntries));
}
```

**Registration:** `registry.register('map_put', 4, mapPutKernel);`

**Complexity:** O(n) — copies the map. Future optimization: mutate in place when SRSW guarantees the old map is dead.

## 5. Body Kernel: `_map_get/3`

**Signature:** `_map_get(M?, Key?, Val)`
**Phase:** Body
**Args:** M — reader (existing map), Key — reader (ground constant), Val — unbound writer

**Semantics:** Dereference M to get a `MapTerm`. Extract key. Look up the key. Bind Val to the associated value. Abort if key not found.

**Important:** This kernel is only called after a `map_contains` guard has confirmed the key exists. Calling it with a missing key is a program error (abort).

```dart
BodyKernelResult mapGetKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    print('[ABORT] _map_get/3: expected 3 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }

  final mapArg = _deref(rt, args[0]);
  if (mapArg is! MapTerm) {
    print('[ABORT] _map_get/3: first argument must be a MapTerm');
    return BodyKernelResult.abort;
  }

  final key = _extractMapKey(rt, args[1]);
  if (key == null) {
    print('[ABORT] _map_get/3: second argument must be a ground constant');
    return BodyKernelResult.abort;
  }

  final val = mapArg.entries[key];
  if (val == null) {
    print('[ABORT] _map_get/3: key not found (guard should have checked)');
    return BodyKernelResult.abort;
  }

  return _bindResult(rt, args[2], val);
}
```

**Registration:** `registry.register('_map_get', 3, mapGetKernel);`

## 6. Guard: `map_contains/2`

**Signature:** `map_contains(M?, Key?)`
**Phase:** Guard
**Args:** M — reader (existing map), Key — reader (ground constant)

**Semantics:** Three-valued guard:
- **Success:** M is a `MapTerm` and contains the key.
- **Failure:** M is a `MapTerm` and does NOT contain the key.
- **Suspend:** M or Key is an unbound reader (standard guard suspension).

Implemented in `_evaluateGuard()` in `runner.dart`, alongside existing guards like `ground`, `known`, `is_mutual_ref`.

```dart
case 'map_contains':
  if (args.length != 2) return GuardResult.failure;
  final mapVal = getValue(args[0]);
  if (mapVal is! MapTerm) return GuardResult.failure;
  final key = _extractGuardMapKey(args[1]);
  if (key == null) return GuardResult.failure;
  return mapVal.entries.containsKey(key)
      ? GuardResult.success
      : GuardResult.failure;
```

Note: Suspension on unbound readers is handled by the generic guard dispatch logic (same as all other guards) — unbound readers are detected before `_evaluateGuard` is called.

## 7. Prelude Declarations

**File:** `glp_runtime/lib/analysis/type_checker/prelude.dart`

Add to the procedure declarations:

```glp
procedure map_new(_).
procedure map_put(_?, _?, _?, _).
procedure _map_get(_?, _?, _).
procedure map_contains(_?, _?).
```

Add `'map_contains/2'` to the built-in guards set (alongside `'is_mutual_ref/1'` etc.).

Add `'map_new/1'`, `'map_put/4'`, `'_map_get/3'` to the built-in procedures set (alongside `'_allocate_mutual_reference/2'` etc.).

## 8. Stdlib Wrapper

**File:** `programs/stdlib/map.glp`

```glp
%% map_get/3 — Look up a key in a map, bind Val to the value.
%% Fails (via otherwise in caller) if key not found.

procedure map_get(_?, _?, _).

map_get(M, Key, Val) :-
    map_contains(M?, Key?) |
    _map_get(M?, Key?, Val).
```

---

# Implementation Plan

## Step 0: Baseline

Run full test suite. Commit baseline.

## Step 1: MapTerm class

**File:** `glp_runtime/lib/runtime/terms.dart`

Add `MapTerm` class after `MutualRefTerm`. ~10 lines.

## Step 2: Body kernels

**File:** `glp_runtime/lib/runtime/body_kernels.dart`

Add:
- `_extractMapKey()` helper
- `mapNewKernel` — map_new/1
- `mapPutKernel` — map_put/4
- `mapGetKernel` — _map_get/3
- Register all three in `registerAllKernels()`

## Step 3: Guard

**File:** `glp_runtime/lib/bytecode/runner.dart`

Add `case 'map_contains':` in `_evaluateGuard()`, after existing guard cases. Need a `_extractGuardMapKey()` helper (or reuse from body_kernels).

## Step 4: Prelude declarations

**File:** `glp_runtime/lib/analysis/type_checker/prelude.dart`

Add procedure declarations for `map_new/1`, `map_put/4`, `_map_get/3`, `map_contains/2`.
Add to built-in guards set and built-in procedures set.

## Step 5: Stdlib file

**File:** `programs/stdlib/map.glp`

Create the `map_get` wrapper.

## Step 6: Test

Run full test suite. Then run the map-specific test programs.

## Step 7: Commit and push

---

# Test Plan

## Test 1: Basic map creation and put

```glp
procedure test_new(Result).
test_new(Result) :- map_new(M0), map_put(M0?, alice, 42, M1), map_contains(M1?, alice) | Result = yes.
```

**Goal:** `test_new(X).`
**Expected:** `X = yes`

## Test 2: map_get retrieval

```glp
procedure test_get(Result).
test_get(Result) :- map_new(M0), map_put(M0?, bob, 99, M1), _map_get(M1?, bob, Result).
```

**Goal:** `test_get(X).`
**Expected:** `X = 99`

## Test 3: Key not found (map_contains fails)

```glp
procedure test_missing(Result).
test_missing(Result) :- map_new(M0), map_put(M0?, alice, 42, M1), map_contains(M1?, bob) | Result = found.
test_missing(Result) :- otherwise | Result = not_found.
```

**Goal:** `test_missing(X).`
**Expected:** `X = not_found`

## Test 4: Multiple puts and gets

```glp
procedure test_multi(R1, R2).
test_multi(R1, R2) :-
    map_new(M0),
    map_put(M0?, a, 1, M1),
    map_put(M1?, b, 2, M2),
    _map_get(M2?, a, R1),
    _map_get(M2?, b, R2).
```

**Goal:** `test_multi(X, Y).`
**Expected:** `X = 1, Y = 2`

## Test 5: Overwrite existing key

```glp
procedure test_overwrite(Result).
test_overwrite(Result) :-
    map_new(M0),
    map_put(M0?, k, old, M1),
    map_put(M1?, k, new, M2),
    _map_get(M2?, k, Result).
```

**Goal:** `test_overwrite(X).`
**Expected:** `X = new`

## Test 6: Stdlib map_get wrapper

Load `programs/stdlib/map.glp`, then:

```glp
procedure test_stdlib(Result).
test_stdlib(Result) :-
    map_new(M0),
    map_put(M0?, alice, 42, M1),
    map_get(M1?, alice, Result).
```

**Goal:** `test_stdlib(X).`
**Expected:** `X = 42`

## Test 7: map_get with otherwise fallback

```glp
procedure test_fallback(Result).
test_fallback(Result) :- map_new(M0), map_get(M0?, missing, Result).
test_fallback(Result) :- otherwise | Result = not_found.
```

**Goal:** `test_fallback(X).`
**Expected:** `X = not_found`

## Test 8: Map as agent state (integration)

```glp
procedure agent(_, _, _).
agent(Contacts, [add(Name, Addr)|Msgs], Done) :-
    map_put(Contacts?, Name?, Addr?, C1) |
    agent(C1?, Msgs?, Done).
agent(Contacts, [get(Name, Reply)|Msgs], Done) :-
    map_get(Contacts?, Name?, Val) |
    Reply = Val?,
    agent(Contacts?, Msgs?, Done).
agent(Contacts, [], Done) :- Done = done.

procedure test_agent(Result).
test_agent(Result) :-
    map_new(M0),
    agent(M0?, [add(alice, 42), add(bob, 99), get(alice, R)], Done),
    Result = R?.
```

**Goal:** `test_agent(X).`
**Expected:** `X = 42`

## Regression

Run full `bash test/run_all_tests.sh` — all existing tests must still pass.
