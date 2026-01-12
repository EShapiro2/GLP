# Module: moded-head

**Version**: 0.7
**Date**: 2025-01-11  
**Status**: DRAFT  
**Paper References**: Definition 4.8 (Moded Head), Example 4.9

## Purpose

Constructs a moded head H' from a clause head H and a procedure declaration. The moded head is used for well-typing checks.

## Dependencies

- `mode` — Mode enum
- `moded-term` — ModedTerm, ModedCompound, ModedConstant, ModedVariable
- `type-environment` — ProcDecl

## Definitions

### Definition 4.8: Moded Head

> Given a head H, a **moded head** H' is obtained by:
> 1. Constructing an I/O-moded term corresponding to H, then
> 2. For each variable, if its form does not match its position's structural mode, replacing it with its paired variable. Specifically, a variable at a position with structural mode ↓ should be a reader, and a variable at a position with structural mode ↑ should be a writer.

### I/O Moded Term

An **I/O moded term** is a moded term where:
- Root mode is ↓ (consume)
- Input argument positions (Type?) have mode ↓
- Output argument positions (Type) have mode ↑
- Nested structure computes combined modes via involution based on type complementation

### Step 2: Ensuring Variable-Mode Consistency

Step 2 ensures each variable's form matches its position's structural mode:
- A variable at mode ↓ should be a reader (X?)
- A variable at mode ↑ should be a writer (X)

For **simple input/output types**, this typically means replacing all variables:
- Head writer `X` at input position (mode ↓) becomes reader `X?`
- Head reader `X?` at output position (mode ↑) becomes writer `X`

For **interactive types** with internal mode complementation, some variables may already have the correct form and require no change.

## Public Interface

### Functions

#### `ModedTerm modedHead(Term head, ProcDecl decl)`

Constructs a moded head H' from clause head H per Definition 4.8.

**Preconditions:**
- `head` is a valid clause head (compound term)
- `decl` provides the procedure type declaration
- `head.functor == decl.name` and `head.arity == decl.arity`

**Postconditions:** Returns a ModedTerm where:
- `isIO(result)` is true (the result is an I/O moded term)
- Root mode is ↓ (consume)
- Each argument has mode based on declared type: Type? → ↓, Type → ↑
- Each variable's form matches its position's structural mode

**Errors:**
- Throws `ArityMismatchError` if head arity doesn't match declaration

#### `ModedTerm producedTerm(Term atom, ProcDecl decl)`

Constructs a produced moded term from a body atom.

**Preconditions:**
- `atom` is a valid body atom (compound term)  
- `decl` provides the procedure type declaration

**Postconditions:** Returns a ModedTerm where:
- `isProduced(result)` is true (the result is a produced moded term)
- Root mode is ↑ (produce)
- Each argument has mode based on declared type: Type? → ↓, Type → ↑
- Variables are NOT modified (body atoms use original variable forms)

**Errors:**
- Throws `ArityMismatchError` if atom arity doesn't match declaration

## Algorithms

### Algorithm: Moded Head Construction

```
modedHead(head, decl):
  // Step 1: Build I/O moded term
  ioTerm = buildIOModedTerm(head, decl, Mode.consume)
  
  // Step 2: Ensure each variable's form matches its position's mode
  return ensureVariablesMatchModes(ioTerm)

buildIOModedTerm(term, decl, parentMode):
  match term:
    Compound(functor, args):
      modedArgs = []
      for i in 1..args.length:
        argType = decl.argTypes[i-1]
        // Input (Type?) → consume, Output (Type) → produce
        argMode = argType.isInput ? Mode.consume : Mode.produce
        modedArg = buildModedSubterm(args[i-1], argMode, argType, typeEnv)
        modedArgs.add(modedArg)
      return ModedCompound(parentMode, functor, args.length, modedArgs)
    
    _: throw InvalidHeadError("Head must be compound term")

buildModedSubterm(term, mode, expectedType, typeEnv):
  match term:
    Compound(functor, args):
      // Look up type definition for embedded modes
      subtermModes = getSubtermModes(functor, arity, mode, expectedType, typeEnv)
      modedArgs = []
      for i in 0..<args.length:
        (subtermMode, subtermType) = subtermModes[i]
        modedArgs.add(buildModedSubterm(args[i], subtermMode, subtermType, typeEnv))
      return ModedCompound(mode, functor, args.length, modedArgs)
    
    Constant(value):
      return ModedConstant(mode, value)
    
    Variable(name, isReader):
      // Preserve original reader/writer form; structural mode stored separately
      return ModedVariable(name, isReader, structuralMode: mode)

ensureVariablesMatchModes(term):
  match term:
    ModedCompound(mode, functor, arity, args):
      return ModedCompound(mode, functor, arity, args.map(ensureVariablesMatchModes))
    
    ModedConstant(mode, value):
      return ModedConstant(mode, value)
    
    ModedVariable(name, isReader, structuralMode):
      // Check if variable form matches structural mode
      // Mode ↓ requires reader; Mode ↑ requires writer
      shouldBeReader = (structuralMode == Mode.consume)
      if isReader == shouldBeReader:
        return term  // Already correct form
      else:
        return ModedVariable(name, !isReader, structuralMode: structuralMode)  // Flip
```

### Algorithm: Produced Term Construction (for body atoms)

```
producedTerm(atom, decl):
  return buildIOModedTerm(atom, decl, Mode.produce)
  // Note: no variable modification for body atoms
```

## Examples

### Example 1: merge Clause Head (Simple Input/Output Types)

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

**Step 2: Ensure variables match modes**

| Variable | Position Mode | Current Form | Required Form | Action |
|----------|---------------|--------------|---------------|--------|
| X | ↓ | writer | reader | flip → X? |
| Xs | ↓ | writer | reader | flip → Xs? |
| Ys | ↓ | writer | reader | flip → Ys? |
| X? | ↑ | reader | writer | flip → X |
| Zs? | ↑ | reader | writer | flip → Zs |

Result:
```
H' = ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
```

### Example 2: new_channel Clause Head (Interactive Type)

**Input:**

Clause head:
```
H = new_channel(ch(Xs?, Ys), ch(Ys?, Xs))
```

Type declarations:
```
MyChan ::= ch(MyList?, MyList).
procedure new_channel(MyChan, MyChan).
```

**Step 1: Build I/O moded term**

- Root mode: ↓ (consume)
- Arg 1 declared `MyChan` (output) → mode ↑
- Arg 2 declared `MyChan` (output) → mode ↑

Inside `ch(...)` with type `MyChan = ch(MyList?, MyList)`:
- Position 1 has `MyList?` (isInput=true) → embedded mode ↓
- Position 2 has `MyList` (isInput=false) → embedded mode ↑
- Combined modes: pos1 = ↑⊕↓ = ↓, pos2 = ↑⊕↑ = ↑

Result:
```
↓new_channel(↑ch(↓Xs?, ↑Ys), ↑ch(↓Ys?, ↑Xs))
```

**Step 2: Ensure variables match modes**

| Variable | Position Mode | Current Form | Required Form | Action |
|----------|---------------|--------------|---------------|--------|
| Xs? | ↓ | reader | reader | no change |
| Ys | ↑ | writer | writer | no change |
| Ys? | ↓ | reader | reader | no change |
| Xs | ↑ | writer | writer | no change |

Result:
```
H' = ↓new_channel(↑ch(↓Xs?, ↑Ys), ↑ch(↓Ys?, ↑Xs))
```

**Key insight:** For interactive types, the programmer has already placed variables in the correct form to match the type's internal mode structure. No flipping is needed.

### Example 3: merge Body Atom

**Input:**

Body atom:
```
A = merge(Ys?, Xs?, Zs)
```

Type declaration:
```
procedure merge(Stream?, Stream?, Stream).
```

**Produced moded term (no variable modification):**

- Root mode: ↑ (produce)
- Arg 1 declared `Stream?` (input) → mode ↓
- Arg 2 declared `Stream?` (input) → mode ↓
- Arg 3 declared `Stream` (output) → mode ↑

Result:
```
A' = ↑merge(Ys?, Xs?, Zs)
```

Variables stay as-is because body atoms are goals being called, not definitions being matched.

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
| 0.6 | 2025-01-09 | Add isIO/isProduced postconditions per paper Definition 4.6 and Well-typed Clause |
| 0.7 | 2025-01-11 | Fix Definition 4.8 step 2: conditional variable replacement for interactive types |
