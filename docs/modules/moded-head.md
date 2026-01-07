# Module: moded-head

**Version**: 0.5  
**Date**: 2025-01-08  
**Status**: DRAFT  
**Paper References**: Definition 4.6 (lines 285-288), Example (lines 290-308)

## Purpose

Constructs a moded head H' from a clause head H and a procedure declaration. The moded head is used for well-typing checks.

## Dependencies

- `mode` — Mode enum
- `moded-term` — ModedTerm, ModedCompound, ModedConstant, ModedVariable
- `type-environment` — ProcDecl

## Definitions

### Definition 4.6: Moded Head (lines 285-288)

> Given a head H, a **moded head** H' is obtained by:
> 1. Constructing an I/O-moded term corresponding to H, then
> 2. Replacing each variable by its paired variable.

### I/O Moded Term

An **I/O moded term** is a moded term where:
- Root mode is ↓ (consume)
- Input argument positions (Type?) have mode ↓
- Output argument positions (Type) have mode ↑
- Nested structure preserves/flips mode based on type complementation

### Variable Flip (lines 279-282)

The variable flip in step 2 captures inverted roles:
- Head writer `X` becomes reader `X?` (serves as **input**—bound by the goal)
- Head reader `X?` becomes writer `X` (serves as **output**—will be bound by the body)

## Public Interface

### Functions

#### `ModedTerm modedHead(Term head, ProcDecl decl)`

Constructs a moded head H' from clause head H per Definition 4.6.

**Preconditions:**
- `head` is a valid clause head (compound term)
- `decl` provides the procedure type declaration
- `head.functor == decl.name` and `head.arity == decl.arity`

**Postconditions:** Returns a ModedTerm where:
- Root mode is ↓ (consume)
- Each argument has mode based on declared type: Type? → ↓, Type → ↑
- All variables are flipped (X ↔ X?)

**Errors:**
- Throws `ArityMismatchError` if head arity doesn't match declaration

#### `ModedTerm producedTerm(Term atom, ProcDecl decl)`

Constructs a produced moded term from a body atom.

**Preconditions:**
- `atom` is a valid body atom (compound term)  
- `decl` provides the procedure type declaration

**Postconditions:** Returns a ModedTerm where:
- Root mode is ↑ (produce)
- Each argument has mode based on declared type: Type? → ↓, Type → ↑
- Variables are NOT flipped (body atoms use original variable forms)

**Errors:**
- Throws `ArityMismatchError` if atom arity doesn't match declaration

## Algorithms

### Algorithm: Moded Head Construction

```
modedHead(head, decl):
  // Step 1: Build I/O moded term
  ioTerm = buildIOModedTerm(head, decl, Mode.consume)
  
  // Step 2: Flip all variables
  return flipAllVariables(ioTerm)

buildIOModedTerm(term, decl, parentMode):
  match term:
    Compound(functor, args):
      modedArgs = []
      for i in 1..args.length:
        argType = decl.argTypes[i-1]
        // Input (Type?) preserves consume, Output (Type) flips to produce
        argMode = argType.isInput ? Mode.consume : Mode.produce
        modedArg = buildModedSubterm(args[i-1], argMode)
        modedArgs.add(modedArg)
      return ModedCompound(parentMode, functor, args.length, modedArgs)
    
    _: throw InvalidHeadError("Head must be compound term")

buildModedSubterm(term, mode):
  match term:
    Compound(functor, args):
      modedArgs = args.map(arg => buildModedSubterm(arg, mode))
      return ModedCompound(mode, functor, args.length, modedArgs)
    
    Constant(value):
      return ModedConstant(mode, value)
    
    Variable(name, isReader):
      return ModedVariable(name, isReader)

flipAllVariables(term):
  match term:
    ModedCompound(mode, functor, arity, args):
      return ModedCompound(mode, functor, arity, args.map(flipAllVariables))
    
    ModedConstant(mode, value):
      return ModedConstant(mode, value)
    
    ModedVariable(name, isReader):
      return ModedVariable(name, !isReader)  // Flip: X ↔ X?
```

### Algorithm: Produced Term Construction (for body atoms)

```
producedTerm(atom, decl):
  return buildIOModedTerm(atom, decl, Mode.produce)
  // Note: no variable flip for body atoms
```

## Examples

### Example: merge Clause Head

**Input:**

Clause head:
```
H = merge([X|Xs], Ys, [X?|Zs?])
```

Type declaration:
```
procedure merge(Stream?, Stream?, Stream).
```

**Step 1: Build I/O moded term**

- Root mode: ↓ (consume)
- Arg 1 declared `Stream?` (input) → mode ↓
- Arg 2 declared `Stream?` (input) → mode ↓  
- Arg 3 declared `Stream` (output) → mode ↑

Result:
```
↓merge(↓[↓X|Xs], Ys, ↑[↑X?|Zs?])
```

**Step 2: Flip all variables**

- X → X?
- Xs → Xs?
- Ys → Ys?
- X? → X
- Zs? → Zs

Result:
```
H' = ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
```

### Example: merge Body Atom

**Input:**

Body atom:
```
A = merge(Ys?, Xs?, Zs)
```

Type declaration:
```
procedure merge(Stream?, Stream?, Stream).
```

**Produced moded term (no variable flip):**

- Root mode: ↑ (produce)
- Arg 1 declared `Stream?` (input) → mode ↓
- Arg 2 declared `Stream?` (input) → mode ↓
- Arg 3 declared `Stream` (output) → mode ↑

Result:
```
A' = ↑merge(Ys?, Xs?, Zs)
```

Variables stay as-is because body atoms are goals being called, not definitions being matched.

### Example: Paths of Moded Head

Moded head:
```
H' = ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
```

Paths:
```
(0,↓) → merge/3 --(1,↓)--> [|]/2 --(1,↓)--> X?
(0,↓) → merge/3 --(1,↓)--> [|]/2 --(2,↓)--> Xs?
(0,↓) → merge/3 --(2,↓)--> Ys?
(0,↓) → merge/3 --(3,↑)--> [|]/2 --(1,↑)--> X
(0,↓) → merge/3 --(3,↑)--> [|]/2 --(2,↑)--> Zs
```

## Error Conditions

| Condition | Exception |
|-----------|-----------|
| Head arity doesn't match declaration | `ArityMismatchError` |
| Head is not a compound term | `InvalidHeadError` |

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft |
| 0.5 | 2025-01-08 | Add producedTerm for body atoms; complete algorithms; more examples |
