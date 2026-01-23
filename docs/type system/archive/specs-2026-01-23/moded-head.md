# Module: moded-head

**Version**: 0.8
**Date**: 2026-01-23  
**Status**: DRAFT  
**Paper References**: Definition 5.5 (Moded Head), Remark [Mode Correspondence]

## Purpose

Constructs a moded head H' from a clause head H and a procedure declaration. The moded head is used for well-typing checks.

## Dependencies

- `mode` — Mode enum
- `moded-term` — ModedTerm, ModedCompound, ModedConstant, ModedVariable
- `type-environment` — ProcDecl

## Definitions

### Definition 5.5: Moded Head

> Given a head H, a **moded head** H' is obtained by:
> 1. Constructing an I/O-moded term corresponding to H, then
> 2. For each variable, **if its form does not match its position's structural mode**, replacing it with its paired variable.
>
> Specifically, a variable at a position with structural mode ↓ should be a reader, and a variable at a position with structural mode ↑ should be a writer.

### I/O Moded Term

An **I/O moded term** is a moded term where:
- Root mode is ↓ (consume)
- Input argument positions (Type?) have mode ↓
- Output argument positions (Type) have mode ↑
- Nested structure inherits or flips mode based on type duality:
  - A type reference T preserves the parent mode
  - A type reference T? flips the parent mode

### Structural Mode vs. Variable Form

The **structural mode** at a position is determined by the type context—it describes the direction of data flow at that position. Variables have an inherent **form**:
- Reader X? has implicit mode ↓ (consume)
- Writer X has implicit mode ↑ (produce)

For well-typing, the variable's form must match the structural mode of its position. The conditional flip in step 2 ensures this.

### Variable Replacement Rule (Step 2)

| Structural Mode | Original Variable | Action | Result |
|-----------------|-------------------|--------|--------|
| ↓ (consume) | Reader X? | Keep | X? |
| ↓ (consume) | Writer X | Flip | X? |
| ↑ (produce) | Writer X | Keep | X |
| ↑ (produce) | Reader X? | Flip | X |

### Mode Correspondence Property (Remark)

For a moded head H' constructed from head H with procedure type D:
- The structural mode at any position p in H' equals the mode at position p in the corresponding type path in D.

This follows from the construction: argument modes come directly from the type declaration, and nested modes propagate according to type duality rules.

## Public Interface

### Functions

#### `ModedTerm modedHead(Term head, ProcDecl decl, TypeEnvironment env)`

Constructs a moded head H' from clause head H per Definition 5.5.

**Preconditions:**
- `head` is a valid clause head (compound term)
- `decl` provides the procedure type declaration
- `head.functor == decl.name` and `head.arity == decl.arity`
- `env` provides type definitions for resolving nested type modes

**Postconditions:** Returns a ModedTerm where:
- `isIO(result)` is true (the result is an I/O moded term)
- Root mode is ↓ (consume)
- Each argument has mode based on declared type: Type? → ↓, Type → ↑
- Nested modes follow type duality rules
- Variables whose form doesn't match structural mode are replaced with paired variable

**Errors:**
- Throws `ArityMismatchError` if head arity doesn't match declaration

#### `ModedTerm producedTerm(Term atom, ProcDecl decl, TypeEnvironment env)`

Constructs a produced moded term from a body atom.

**Preconditions:**
- `atom` is a valid body atom (compound term)  
- `decl` provides the procedure type declaration

**Postconditions:** Returns a ModedTerm where:
- `isProduced(result)` is true (the result is a produced moded term)
- Root mode is ↑ (produce)
- Each argument has mode based on declared type: Type? → ↓, Type → ↑
- Nested modes follow type duality rules
- Variables are NOT modified (body atoms use original variable forms)

**Errors:**
- Throws `ArityMismatchError` if atom arity doesn't match declaration

## Algorithms

### Algorithm: Moded Head Construction

```
modedHead(head, decl, env):
  // Step 1: Build I/O moded term with structural modes from type
  ioTerm = buildIOModedTerm(head, decl, env, Mode.consume)
  
  // Step 2: Conditionally replace variables whose form doesn't match structural mode
  return ensureVariablesMatchModes(ioTerm)

buildIOModedTerm(term, decl, env, parentMode):
  match term:
    Compound(functor, args):
      modedArgs = []
      for i in 1..args.length:
        argType = decl.argTypes[i-1]
        // Determine structural mode for this argument
        argMode = computeArgMode(argType, parentMode)
        modedArg = buildModedSubterm(args[i-1], argType, env, argMode)
        modedArgs.add(modedArg)
      return ModedCompound(parentMode, functor, args.length, modedArgs)
    
    _: throw InvalidHeadError("Head must be compound term")

computeArgMode(argType, parentMode):
  // Type? flips mode (duality), Type preserves mode
  if argType.isInput:
    return parentMode.flip
  return parentMode

buildModedSubterm(term, typeExpr, env, structuralMode):
  match term:
    Compound(functor, args):
      // Look up the type definition to determine nested modes
      typeDef = resolveType(typeExpr, env)
      nestedModes = getNestedModes(typeDef, functor, structuralMode, env)
      
      modedArgs = []
      for i in 1..args.length:
        argMode = nestedModes[i-1]
        argTypeExpr = getArgTypeExpr(typeDef, functor, i)
        modedArgs.add(buildModedSubterm(args[i-1], argTypeExpr, env, argMode))
      return ModedCompound(structuralMode, functor, args.length, modedArgs)
    
    Constant(value):
      return ModedConstant(structuralMode, value)
    
    Variable(name, isReader):
      // Keep original form; step 2 will fix if needed
      return ModedVariable(name, isReader)

getNestedModes(typeDef, functor, parentMode, env):
  // Find the alternative in typeDef matching functor
  alt = findAlternative(typeDef, functor)
  
  modes = []
  for argTypeExpr in alt.argTypes:
    // T? flips mode (duality), T preserves mode
    if argTypeExpr.isInput:
      modes.add(parentMode.flip)
    else:
      modes.add(parentMode)
  return modes
```

### Algorithm: Ensure Variables Match Modes (Step 2)

```
ensureVariablesMatchModes(term):
  match term:
    ModedCompound(mode, functor, arity, args):
      return ModedCompound(mode, functor, arity, 
        args.map(ensureVariablesMatchModes))
    
    ModedConstant(mode, value):
      return ModedConstant(mode, value)
    
    ModedVariable(name, isReader):
      // Get the structural mode from context (passed during recursion)
      structuralMode = getCurrentStructuralMode()  // from traversal context
      
      // Check if variable form matches structural mode
      if structuralMode == Mode.consume:
        // Position expects reader
        if isReader:
          return ModedVariable(name, isReader)  // Already correct
        else:
          return ModedVariable(name, true)  // Flip writer X to reader X?
      else:  // structuralMode == Mode.produce
        // Position expects writer
        if !isReader:
          return ModedVariable(name, isReader)  // Already correct
        else:
          return ModedVariable(name, false)  // Flip reader X? to writer X
```

**Note:** In actual implementation, structural mode is tracked during traversal.

### Algorithm: Produced Term Construction (for body atoms)

```
producedTerm(atom, decl, env):
  // Build with produce mode at root
  return buildIOModedTerm(atom, decl, env, Mode.produce)
  // Note: no variable modification for body atoms
```

## Examples

### Example 1: merge Clause Head (Simple Types)

**Input:**

Clause head:
```
H = merge([X|Xs], Ys, [X?|Zs?])
```

Type declaration:
```
procedure merge(Stream?, Stream?, Stream).
```

where `Stream ::= [] ; [_|Stream]`

**Step 1: Build I/O moded term**

- Root mode: ↓ (consume)
- Arg 1 declared `Stream?` (input): mode ↓
- Arg 2 declared `Stream?` (input): mode ↓  
- Arg 3 declared `Stream` (output): mode ↑

For arg 1 `[X|Xs]` with type `Stream?`:
- List functor `[|]` at mode ↓
- In `Stream?`, head `_?` has mode ↓ (primitive input)
- In `Stream?`, tail `Stream?` has mode ↓

For arg 3 `[X?|Zs?]` with type `Stream`:
- List functor `[|]` at mode ↑
- In `Stream`, head `_` has mode ↑ (primitive output)
- In `Stream`, tail `Stream` has mode ↑

After Step 1:
```
↓merge(↓[↓X|↓Xs], ↓Ys, ↑[↑X?|↑Zs?])
```

**Step 2: Ensure variables match modes**

| Variable | Position Mode | Original Form | Match? | Result |
|----------|---------------|---------------|--------|--------|
| X (arg 1, head) | ↓ | writer | NO | X? |
| Xs (arg 1, tail) | ↓ | writer | NO | Xs? |
| Ys (arg 2) | ↓ | writer | NO | Ys? |
| X? (arg 3, head) | ↑ | reader | NO | X |
| Zs? (arg 3, tail) | ↑ | reader | NO | Zs |

Final moded head:
```
H' = ↓merge(↓[↓X?|↓Xs?], ↓Ys?, ↑[↑X|↑Zs])
```

### Example 2: Counter Monitor (Interactive Type)

**Input:**

Clause head:
```
H = monitor(N, [read(N?)|In])
```

Type declaration:
```
procedure monitor(Integer?, Stream(CounterCall)?).
```

where:
```
CounterCall ::= add ; clear ; read(Integer?).
Stream(X) ::= [] ; [X|Stream(X)].
```

**Step 1: Build I/O moded term**

- Root mode: ↓
- Arg 1 `Integer?`: mode ↓
- Arg 2 `Stream(CounterCall)?`: mode ↓

For arg 2 `[read(N?)|In]`:
- List in `Stream(CounterCall)?` has mode ↓
- Head element type is `CounterCall?` (dual because inside Stream?)
- In `CounterCall?`, the `read(Integer?)` alternative:
  - `read` functor at mode ↓
  - `Integer?` inside `CounterCall` becomes `Integer` in `CounterCall?` → mode ↑

After Step 1:
```
↓monitor(↓N, ↓[↓read(↑N?)|↓In])
```

**Step 2: Ensure variables match modes**

| Variable | Position Mode | Original Form | Match? | Result |
|----------|---------------|---------------|--------|--------|
| N (arg 1) | ↓ | writer | NO | N? |
| N? (inside read) | ↑ | reader | NO | N |
| In (list tail) | ↓ | writer | NO | In? |

Final moded head:
```
H' = ↓monitor(↓N?, ↓[↓read(↑N)|↓In?])
```

### Example 3: Bounded Buffer Consumer (Interactive Type)

**Input:**

Clause head:
```
H = consumer([X1, X2, X3 | Xs?])
```

Type declaration:
```
procedure consumer(HollowIntegers).
```

where `HollowIntegers ::= [] ; [Integer?|HollowIntegers]`

**Step 1: Build I/O moded term**

- Root mode: ↓
- Arg 1 `HollowIntegers` (output type): mode ↑

For arg 1 `[X1, X2, X3 | Xs?]`:
- List at mode ↑
- In `HollowIntegers`, head `Integer?` flips → mode ↓
- In `HollowIntegers`, tail `HollowIntegers` preserves → mode ↑

After Step 1 (showing first element expansion):
```
↓consumer(↑[↓X1, ↓X2, ↓X3 | ↑Xs?])
```

**Step 2: Ensure variables match modes**

| Variable | Position Mode | Original Form | Match? | Result |
|----------|---------------|---------------|--------|--------|
| X1 | ↓ | writer | NO | X1? |
| X2 | ↓ | writer | NO | X2? |
| X3 | ↓ | writer | NO | X3? |
| Xs? | ↑ | reader | NO | Xs |

Final moded head:
```
H' = ↓consumer(↑[↓X1?, ↓X2?, ↓X3? | ↑Xs])
```

### Example 4: Body Atom (No Variable Modification)

**Input:**

Body atom:
```
A = merge(Ys?, Xs?, Zs)
```

Type declaration:
```
procedure merge(Stream?, Stream?, Stream).
```

**Produced moded term (no step 2):**

- Root mode: ↑ (produce)
- Arg 1 `Stream?`: mode ↓
- Arg 2 `Stream?`: mode ↓
- Arg 3 `Stream`: mode ↑

Result (variables unchanged):
```
A' = ↑merge(↓Ys?, ↓Xs?, ↑Zs)
```

## Error Conditions

| Condition | Exception |
|-----------|-----------|
| Head arity doesn't match declaration | `ArityMismatchError` |
| Head is not a compound term | `InvalidHeadError` |
| Type not found during nested mode resolution | `UndefinedTypeError` |

## Version History

| Version | Date | Changes |
|---------|------|---------|  
| 0.1 | 2025-01-07 | Initial draft |
| 0.5 | 2025-01-08 | Add producedTerm for body atoms; complete algorithms; more examples |
| 0.6 | 2025-01-09 | Add isIO/isProduced postconditions |
| 0.7 | 2025-01-12 | Conditional variable flip per paper Definition 4.8; add interactive type examples |
| 0.8 | 2026-01-23 | **Paper alignment**: Updated to Definition 5.5; "type complementation" → "type duality" throughout |
