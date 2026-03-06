# GLP Body Kernels Reference

**Last Updated**: 2026-03-05

---

## What Are Body Kernels?

Body kernels are runtime-implemented predicates that:

- Execute **inline** within the reducing goal (not spawned as separate goals)
- Have **two-valued semantics**: success or abort (no suspension)
- Are **only accessible from `-mode(system).` code**
- Expect all preconditions to be met by guards before invocation
- Are called via underscore-prefixed names (e.g., `'_add'(X?, Y?, Z)`)

Body kernels are the mechanism by which GLP system-mode code (in `programs/self.glp`) implements operations that require Dart runtime support. User-mode GLP code cannot call body kernels directly; it calls the system procedures that wrap them.

---

## Kernel Categories

### Arithmetic Operations

| Kernel | Arity | Description |
|--------|-------|-------------|
| `_add` | 3 | `_add(X?, Y?, Z)` — Z = X + Y |
| `_sub` | 3 | `_sub(X?, Y?, Z)` — Z = X - Y |
| `_mul` | 3 | `_mul(X?, Y?, Z)` — Z = X * Y |
| `_div` | 3 | `_div(X?, Y?, Z)` — Z = X / Y (real division) |
| `_idiv` | 3 | `_idiv(X?, Y?, Z)` — Z = X ~/ Y (integer division) |
| `_mod` | 3 | `_mod(X?, Y?, Z)` — Z = X mod Y |
| `_neg` | 2 | `_neg(X?, Z)` — Z = -X |

### Math Functions

| Kernel | Arity | Description |
|--------|-------|-------------|
| `_abs` | 2 | Absolute value |
| `_sqrt` | 2 | Square root |
| `_sin` | 2 | Sine |
| `_cos` | 2 | Cosine |
| `_tan` | 2 | Tangent |
| `_asin` | 2 | Arcsine |
| `_acos` | 2 | Arccosine |
| `_atan` | 2 | Arctangent |
| `_exp` | 2 | Exponential (e^x) |
| `_ln` | 2 | Natural logarithm |
| `_log10` | 2 | Base-10 logarithm |
| `_pow` | 3 | `_pow(X?, Y?, Z)` — Z = X^Y |

### Type Conversions

| Kernel | Arity | Description |
|--------|-------|-------------|
| `_integer` | 2 | Convert to integer (truncate) |
| `_real` | 2 | Convert to real (float) |
| `_round` | 2 | Round to nearest integer |
| `_floor` | 2 | Floor (round down) |
| `_ceil` | 2 | Ceiling (round up) |

### Structure Manipulation

| Kernel | Arity | Description |
|--------|-------|-------------|
| `_list_to_tuple` | 2 | Convert GLP list to tuple term |
| `_tuple_to_list` | 2 | Convert tuple term to GLP list |
| `_copy` | 2 | Deep-copy a ground term |

### Time

| Kernel | Arity | Description |
|--------|-------|-------------|
| `_now` | 1 | `_now(T)` — T = current time in milliseconds since epoch |

### Mutual Reference (O(1) Stream Append)

| Kernel | Arity | Description |
|--------|-------|-------------|
| `_allocate_mutual_reference` | 2 | Allocate a mutual-reference pair for O(1) stream append |
| `_stream_append` | 3 | Append an element to a mutual-reference stream |
| `_close_mutual_reference` | 1 | Close a mutual-reference stream |

### I/O

| Kernel | Arity | Description |
|--------|-------|-------------|
| `_output` | 1 | `_output(T)` — print ground term T to stdout |

### madGLP (Multi-Agent)

| Kernel | Arity | Description |
|--------|-------|-------------|
| `_send` | 3 | Send a message in multi-agent context |

### Module Dispatch

| Kernel | Arity | Description |
|--------|-------|-------------|
| `_activate` | 2 | `_activate(Module?, Goal)` — dispatch Goal to module's `_select/1` table |

---

## `_output/1` — Print Ground Term

**Signature**: `'_output'(T)`

**Semantics**: Prints the ground term `T` to stdout as a formatted GLP value. Lists are shown as `[a, b, c]`, atoms as-is, structs as `f(a, b)`.

**Precondition**: `T` must be ground. The calling stdlib code (e.g., `send_to_user/1`) is responsible for ensuring groundness via guards before calling `_output`.

**Runtime override**: The output destination can be overridden via `GlpRuntime.outputCallback` for testing or Flutter integration. When no callback is set, output goes to `print()` (stdout).

**Example stdlib usage** (in `-mode(system).` code):
```prolog
send_to_user(T) :- no_readers(T?) | '_output'(T?).
```

---

## `_activate/2` — Module Goal Dispatch

**Signature**: `'_activate'(Module?, Goal)`

**Semantics**: Resolves `Goal` against the target module's `_select/1` dispatch table. Used by `serve/2` to dispatch incoming RPC goals to the correct exported procedure.

**Precondition**: `Module` must be a ground `ModuleTerm` referencing a compiled module with a `_select/1` label.

---

## Design Principles

1. **Underscore prefix**: All body kernels use underscore-prefixed names to distinguish them from user-callable procedures and to signal that they are runtime primitives.

2. **System-mode only**: Body kernels are only accessible from `-mode(system).` code. The compiler/analyzer enforces this restriction.

3. **Guard-then-kernel**: The standard usage is for a stdlib procedure to use guards to verify preconditions, then call the kernel in the body. This separates concerns: guards handle the three-valued (success/suspend/fail) patient semantics, while kernels handle the two-valued (success/abort) computation.

4. **Inline execution**: Body kernels execute within the current goal's reduction cycle, not as separate spawned goals. This avoids goal-creation overhead for primitive operations.
