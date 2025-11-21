# Body Kernel Predicates Specification

**Purpose**: This document lists all body kernel predicates that must be implemented in the runtime for the `:=` system predicate to work.

**Access**: These predicates are only accessible to system predicates loaded from `stdlib/`, not to user programs.

**Safety**: All abort conditions listed below represent programming errors. The compiler should perform static type analysis to verify these conditions never occur. Runtime aborts are safety mechanisms, not expected execution paths.

---

## Binary Arithmetic Operators

### add(X?, Y?, Result)
**Operation**: Result = X + Y
**Preconditions**: X and Y must be bound to numbers
**Type Coercion**: Int + Int → Int; Int + Float → Float; Float + Any → Float
**Abort on**: Unbound X or Y, non-numeric X or Y

### sub(X?, Y?, Result)
**Operation**: Result = X - Y
**Preconditions**: X and Y must be bound to numbers
**Type Coercion**: Same as add
**Abort on**: Unbound X or Y, non-numeric X or Y

### mul(X?, Y?, Result)
**Operation**: Result = X * Y
**Preconditions**: X and Y must be bound to numbers
**Type Coercion**: Same as add
**Abort on**: Unbound X or Y, non-numeric X or Y

### div(X?, Y?, Result)
**Operation**: Result = X / Y (always returns float)
**Preconditions**: X and Y must be bound to numbers, Y ≠ 0
**Type Coercion**: Always returns Float
**Abort on**: Unbound X or Y, non-numeric X or Y, Y = 0

### idiv(X?, Y?, Result)
**Operation**: Result = X // Y (integer division)
**Preconditions**: X and Y must be bound to integers, Y ≠ 0
**Type Coercion**: Always returns Integer
**Abort on**: Unbound X or Y, non-integer X or Y, Y = 0

### mod(X?, Y?, Result)
**Operation**: Result = X mod Y
**Preconditions**: X and Y must be bound to integers, Y ≠ 0
**Type Coercion**: Always returns Integer
**Abort on**: Unbound X or Y, non-integer X or Y, Y = 0

---

## Unary Arithmetic Operators

### neg(X?, Result)
**Operation**: Result = -X
**Preconditions**: X must be bound to a number
**Type Coercion**: Preserves type (Int → Int, Float → Float)
**Abort on**: Unbound X, non-numeric X

### abs_kernel(X?, Result)
**Operation**: Result = |X| (absolute value)
**Preconditions**: X must be bound to a number
**Type Coercion**: Preserves type
**Abort on**: Unbound X, non-numeric X

---

## Mathematical Functions

### sqrt_kernel(X?, Result)
**Operation**: Result = √X
**Preconditions**: X must be bound to a number, X ≥ 0
**Type Coercion**: Always returns Float
**Abort on**: Unbound X, non-numeric X, X < 0

### sin_kernel(X?, Result)
**Operation**: Result = sin(X)
**Preconditions**: X must be bound to a number (radians)
**Type Coercion**: Always returns Float
**Abort on**: Unbound X, non-numeric X

### cos_kernel(X?, Result)
**Operation**: Result = cos(X)
**Preconditions**: X must be bound to a number (radians)
**Type Coercion**: Always returns Float
**Abort on**: Unbound X, non-numeric X

### tan_kernel(X?, Result)
**Operation**: Result = tan(X)
**Preconditions**: X must be bound to a number (radians)
**Type Coercion**: Always returns Float
**Abort on**: Unbound X, non-numeric X

### exp_kernel(X?, Result)
**Operation**: Result = e^X
**Preconditions**: X must be bound to a number
**Type Coercion**: Always returns Float
**Abort on**: Unbound X, non-numeric X

### ln_kernel(X?, Result)
**Operation**: Result = ln(X) (natural logarithm)
**Preconditions**: X must be bound to a number, X > 0
**Type Coercion**: Always returns Float
**Abort on**: Unbound X, non-numeric X, X ≤ 0

### log10_kernel(X?, Result)
**Operation**: Result = log₁₀(X)
**Preconditions**: X must be bound to a number, X > 0
**Type Coercion**: Always returns Float
**Abort on**: Unbound X, non-numeric X, X ≤ 0

### pow_kernel(X?, Y?, Result)
**Operation**: Result = X^Y
**Preconditions**: X and Y must be bound to numbers
**Type Coercion**: Always returns Float
**Abort on**: Unbound X or Y, non-numeric X or Y

### asin_kernel(X?, Result)
**Operation**: Result = arcsin(X)
**Preconditions**: X must be bound to a number, -1 ≤ X ≤ 1
**Type Coercion**: Always returns Float (radians)
**Abort on**: Unbound X, non-numeric X, X < -1 or X > 1

### acos_kernel(X?, Result)
**Operation**: Result = arccos(X)
**Preconditions**: X must be bound to a number, -1 ≤ X ≤ 1
**Type Coercion**: Always returns Float (radians)
**Abort on**: Unbound X, non-numeric X, X < -1 or X > 1

### atan_kernel(X?, Result)
**Operation**: Result = arctan(X)
**Preconditions**: X must be bound to a number
**Type Coercion**: Always returns Float (radians)
**Abort on**: Unbound X, non-numeric X

---

## Type Conversion Functions

### integer_kernel(X?, Result)
**Operation**: Result = truncate(X) to integer
**Preconditions**: X must be bound to a number
**Type Coercion**: Always returns Integer
**Abort on**: Unbound X, non-numeric X

### real_kernel(X?, Result)
**Operation**: Result = X converted to float
**Preconditions**: X must be bound to a number
**Type Coercion**: Always returns Float
**Abort on**: Unbound X, non-numeric X

### round_kernel(X?, Result)
**Operation**: Result = round(X) to nearest integer
**Preconditions**: X must be bound to a number
**Type Coercion**: Always returns Integer
**Abort on**: Unbound X, non-numeric X

### floor_kernel(X?, Result)
**Operation**: Result = floor(X) (round down)
**Preconditions**: X must be bound to a number
**Type Coercion**: Always returns Integer
**Abort on**: Unbound X, non-numeric X

### ceil_kernel(X?, Result)
**Operation**: Result = ceiling(X) (round up)
**Preconditions**: X must be bound to a number
**Type Coercion**: Always returns Integer
**Abort on**: Unbound X, non-numeric X

---

## Implementation Notes

**Error Handling**: All body kernels abort on error (they do not fail or suspend). The `:=` system predicate ensures preconditions are met before calling body kernels.

**Type Checking**: Body kernels assume callers have verified types. They still check and abort if preconditions are violated (defensive programming).

**Dart Implementation**: Map to Dart's `dart:math` library:
```dart
bodyKernelRegistry.register('add', (rt, x, y, result) {
  final xVal = rt.deref(x);
  final yVal = rt.deref(y);
  if (xVal is! num || yVal is! num) {
    rt.abort("Type error in add/3");
  }
  rt.bind(result, xVal + yVal);
});

bodyKernelRegistry.register('sqrt_kernel', (rt, x, result) {
  final xVal = rt.deref(x);
  if (xVal is! num) rt.abort("Type error in sqrt");
  if (xVal < 0) rt.abort("Domain error: sqrt of negative");
  rt.bind(result, math.sqrt(xVal));
});

// ... similar for all other kernels
```

---

## Summary

**Total Body Kernels**: 27

**Categories**:
- Binary operators: 6 (add, sub, mul, div, idiv, mod)
- Unary operators: 2 (neg, abs_kernel)
- Math functions: 14 (sqrt, trig, exp, log, pow)
- Type conversions: 5 (integer, real, round, floor, ceil)

All are called exclusively by the `:=` system predicate and are not accessible to user programs.
