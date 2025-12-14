# GLP Equators Specification

**Status:** Draft
**Date:** December 2025

## Overview

An **equator** provides coordinated many-to-one signaling in GLP. Like an emergency brake on a train, any passenger (process) can signal the driver (controller), but only the driver decides what to do next.

## Problem

GLP's SRSW requirement ensures each writer has exactly one occurrence. But for emergency signaling, multiple processes need shared access to a single trigger. The equator solves this by wrapping a writer in a structure that can be safely distributed.

## Structure

An equator is a term `'_equator'(E, C)` where:
- `E` is a writer (the trigger)
- `C` is a constant (the collapse value)

### Creation

Via unit clause:

```glp
create_equator(C, '_equator'(E, C?), E?).
```

For the call `create_equator(stop, Eq, Handle)`:
- `Eq` binds to `'_equator'(E, stop)` with fresh writer `E`
- `Handle` binds to `E?` — the sole reader, kept by the controller

Or construct explicitly:

```glp
... :- controller(E?, Commands), run(m, goal, '_equator'(E, stop), Commands?).
```

## Guard: `equator(X?)`

The `equator(X?)` guard:
- **Succeeds** when `X` has the form `'_equator'(_, C)` where `C` is a constant
- **Suspends** if `X` contains unbound readers
- **Fails** otherwise

**SRSW Relaxation:** Like `ground`, this guard permits multiple occurrences of `X?` in the clause body.

## Kernel: `'_equator'(X)`

The body kernel `'_equator'(X)` triggers collapse:
- If `X = '_equator'(E, C)` and `E` is a writer: binds `E = C`
- Otherwise: no-op

## Usage

### Controller Setup

```glp
main :-
    create_equator(stop, Eq, Handle),
    controller(Handle?, Commands),
    run(m, goal, Eq?, Commands?).
```

### Meta-interpreter Distribution

```glp
run(M, (A,B), Commands, Eq) :-
    equator(Eq?) |
    distribute(Commands?, Commands1, Commands2),
    run(M?, A?, Commands1?, Eq?),
    run(M?, B?, Commands2?, Eq?).
```

### Triggering

Any process can pull the brake:

```glp
run(M, A, [abort|_], Eq) :- '_equator'(Eq?).
```

### Detection

The controller detects via the sole reader:

```glp
controller(Handle, Commands) :-
    known(Handle?) |
    issue_stop(Commands?).
```

## Semantics

### Many-to-One Communication

The equator enables many-to-one communication:
- **Many** processes hold the equator structure `'_equator'(E, C)` containing writer `E`
- **One** controller holds reader `E?`
- Any process can bind `E = C`, signaling the controller
- Controller decides what action to take

### Safety

The equator is safe:
- `E` can only be bound to `C` (predetermined)
- Multiple readers of the structure all see the same value when triggered
- The SRSW relaxation is justified because the internal writer has exactly one occurrence

## Implementation

### Guard Implementation

```dart
GuardResult equatorGuard(Term arg) {
  final deref = dereference(arg);

  // Check for unbound reader - suspend
  if (deref is VarRef && deref.isReader && !isBound(deref)) {
    return GuardResult.suspend({deref.varId});
  }

  // Check structure
  if (deref is StructTerm &&
      deref.functor == "'_equator'" &&
      deref.arity == 2 &&
      isConstant(deref.args[1])) {
    return GuardResult.success;
  }

  return GuardResult.fail;
}
```

### Kernel Implementation

```dart
void equatorKernel(Term arg) {
  final deref = dereference(arg);

  if (deref is! StructTerm ||
      deref.functor != "'_equator'" ||
      deref.arity != 2) {
    return;  // no-op
  }

  final e = deref.args[0];
  final c = deref.args[1];

  // Check if e is unbound writer
  if (e is VarRef && !e.isReader && !isBound(e.varId)) {
    bind(e.varId, c);  // Trigger!
  }
  // else: no-op (already bound or is reader)
}
```

## References

- Book: "The Art of Grassroots Logic Programming", Chapter on Metaprogramming, Section "Equators: Emergency Brake"
