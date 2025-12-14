# GLP Naming Conventions

## System Primitives

All system structures and body kernel predicates use **quoted atoms starting with underscore**: `'_name'`

This convention:
- Distinguishes system primitives from user-defined predicates
- Prevents accidental name collisions
- Makes system calls visually distinct in code

## Body Kernels

Body kernels are internal runtime predicates accessible only to system predicates (stdlib). They have two-valued semantics: success or abort (no suspension).

### Arithmetic
| Kernel | Arity | Description |
|--------|-------|-------------|
| `'_add'` | 3 | Z = X + Y |
| `'_sub'` | 3 | Z = X - Y |
| `'_mul'` | 3 | Z = X * Y |
| `'_div'` | 3 | Z = X / Y (float) |
| `'_idiv'` | 3 | Z = X // Y (integer) |
| `'_mod'` | 3 | Z = X mod Y |
| `'_neg'` | 2 | Z = -X |

### Math Functions
| Kernel | Arity | Description |
|--------|-------|-------------|
| `'_abs'` | 2 | Absolute value |
| `'_sqrt'` | 2 | Square root |
| `'_sin'`, `'_cos'`, `'_tan'` | 2 | Trigonometric |
| `'_asin'`, `'_acos'`, `'_atan'` | 2 | Inverse trigonometric |
| `'_exp'` | 2 | Exponential (e^X) |
| `'_ln'` | 2 | Natural logarithm |
| `'_log10'` | 2 | Base-10 logarithm |
| `'_pow'` | 3 | X^Y |

### Type Conversion
| Kernel | Arity | Description |
|--------|-------|-------------|
| `'_integer'` | 2 | Convert to integer |
| `'_real'` | 2 | Convert to float |
| `'_round'` | 2 | Round to nearest |
| `'_floor'` | 2 | Round down |
| `'_ceil'` | 2 | Round up |

### Structure
| Kernel | Arity | Description |
|--------|-------|-------------|
| `'_list_to_tuple'` | 2 | `[foo, a, b]` → `foo(a, b)` |
| `'_tuple_to_list'` | 2 | `foo(a, b)` → `[foo, a, b]` |
| `'_copy'` | 2 | Copy term |

### Time
| Kernel | Arity | Description |
|--------|-------|-------------|
| `'_now'` | 1 | Current Unix milliseconds |

### Mutual References
| Kernel | Arity | Description |
|--------|-------|-------------|
| `'_allocate_mutual_reference'` | 2 | Create mutual reference for O(1) append |
| `'_stream_append'` | 3 | Append element via reference |
| `'_close_mutual_reference'` | 1 | Close mutual reference |

## System Structures

| Structure | Description |
|-----------|-------------|
| `'_equator'(E, C)` | Equator for many-to-one signaling (E = writer, C = constant) |

## Guards

| Guard | Description |
|-------|-------------|
| `equator(X?)` | Succeeds if X is `'_equator'(_, C)` with C constant. Relaxes SRSW like `ground`. |

## Body Kernels (for equator)

| Kernel | Description |
|--------|-------------|
| `'_equator'(X)` | If X = `'_equator'(E, C)` and E is writer, binds E = C. Else no-op. |

## Implementation

Registries use `'_name'` as keys:

```dart
bodyKernelRegistry.register("'_add'", addBodyKernel);
bodyKernelRegistry.register("'_sub'", subBodyKernel);
// etc.
```

User programs cannot call body kernels directly—they are accessed only through system predicates like `:=`.
