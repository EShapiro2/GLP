# Module: moded-head

**Version**: 0.1  
**Date**: 2025-01-07  
**Status**: DRAFT  
**Paper References**: Definition 4.5 (lines 285-288), Example (lines 290-308)

## Purpose

Constructs a moded head H' from a clause head H. The moded head is used for well-typing checks.

## Dependencies

- `moded-term` — ModedTerm, Mode, I/O moded term construction

## Paper Definition

### Definition 4.5: Moded Head (lines 285-288)

> Given a head H, a **moded head** H' is obtained by:
> 1. Constructing an I/O-moded term corresponding to H, then
> 2. Replacing each variable by its paired variable.

### Context (lines 279-282)

The variable flip in step 2 captures inverted roles:
- Head writer `X` becomes reader `X?` (serves as **input**—bound by the goal)
- Head reader `X?` becomes writer `X` (serves as **output**—will be bound by the body)

### I/O Moded Term (from moded-term module)

An I/O moded term has:
- Root mode ↓ (consume)
- At most one mode inversion from ↓ to ↑ on any path
- No inversion from ↑ back to ↓

## Example (lines 290-308)

### Input

Clause head:
```
H = merge([X|Xs], Ys, [X?|Zs?])
```

Type declaration:
```
merge(Stream?, Stream?, Stream)
```

### Step 1: Construct I/O-moded term

Arguments 1, 2 are consumed (Stream?), argument 3 is produced (Stream):
```
↓merge(↓[↓X|Xs], Ys, ↑[↑X?|Zs?])
```

### Step 2: Replace each variable by its paired variable

```
H' = ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
```

### Result

The moded head H' has paths:
```
(0,↓) → merge --(1,↓)--> "."/2 --(1,↓)--> X?
(0,↓) → merge --(1,↓)--> "."/2 --(2,↓)--> Xs?
(0,↓) → merge --(2,↓)--> Ys?
(0,↓) → merge --(3,↑)--> "."/2 --(1,↑)--> X
(0,↓) → merge --(3,↑)--> "."/2 --(2,↑)--> Zs
```

## Interface

### `ModedTerm modedHead(Head h, ProcDecl decl)`

Constructs a moded head H' from head H using procedure declaration for mode information.

**Preconditions:**
- `h` is a valid clause head
- `decl` provides the procedure type (argument modes)

**Postconditions:**
- Returns an I/O moded term with all variables flipped to their pairs
- Root mode is ↓ (consume)
- Output arguments (Type, not Type?) have mode ↑
- Input arguments (Type?) have mode ↓

### Algorithm

```
modedHead(h, decl):
  // Step 1: Build I/O moded term from head
  ioTerm = buildIOModed(h, decl)
  
  // Step 2: Flip all variables
  return flipVariables(ioTerm)

buildIOModed(term, typeInfo, parentMode = consume):
  match term:
    Compound(functor, args):
      modedArgs = []
      for i, arg in enumerate(args):
        argMode = typeInfo.argMode(i)  // consume for T?, produce for T
        modedArgs.add(buildIOModed(arg, typeInfo.argType(i), argMode))
      return ModedCompound(parentMode, functor, args.length, modedArgs)
    
    Constant(value):
      return ModedConstant(parentMode, value)
    
    Variable(name, isReader):
      return ModedVariable(name, isReader)  // Keep original reader/writer

flipVariables(term):
  match term:
    ModedCompound(mode, functor, arity, args):
      return ModedCompound(mode, functor, arity, args.map(flipVariables))
    
    ModedConstant(mode, value):
      return ModedConstant(mode, value)
    
    ModedVariable(name, isReader):
      return ModedVariable(name, !isReader)  // Flip: X ↔ X?
```

## Relationship to Well-Typing

The moded head H' is used in Definition 4.6 (Well-typed Clause):
- Condition 1 requires H' to be well-typed by D
- The "accepts" predicate checks if H' has paths consistent with input paths of D

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft from paper |
