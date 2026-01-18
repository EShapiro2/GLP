# Typed GLP

## Mission

Finish the type system paper. Get the type checker working. All GLP programs well-typed.

## Scope

### Theory & Paper
- GLP type system formal definition
- Typing rules and proofs
- Paper sections on types

### Implementation
- `glp_runtime/lib/compiler/typechecker.dart` - Type checker
- Type declaration parsing
- Procedure declaration parsing
- SRSW verification

### Programs
- Ensure all GLP programs in `programs/` are well-typed
- Add type declarations where missing
- Create positive tests (should type-check) and negative tests (should fail)

## Core Concepts

### Type Declarations
```glp
MyStream ::= [] ; [_ | MyStream].
Channel ::= ch(MyStream?, MyStream).
Status ::= found ; not_found.
```

### Procedure Declarations
```glp
procedure merge(MyStream?, MyStream?, MyStream).
procedure new_channel(Channel, Channel).
procedure social_graph(_?, MsgList?, FriendList?).
```

### SRSW Verification
- Each variable: exactly one writer, exactly one reader
- Ground guards allow multiple readers (value is safely duplicable)
- Mode annotations (`?`) indicate reader occurrences

### The `?` Involution
- `(T?)? = T`
- Essential for correctly classifying variable occurrences in recursive types

## Current State (2026-01-17)

### Working
- Type checker parses type and procedure declarations
- Basic SRSW checking
- Positive/negative test infrastructure

### In Progress
- Edge cases with recursive types
- Interactive types (channels)
- Ground guard handling

### Test Programs
- `glp_runtime/test/programs/typechecker/positive/` - Should pass
- `glp_runtime/test/programs/typechecker/negative/` - Should fail

## Key Files

- `glp_runtime/lib/compiler/typechecker.dart` - Main type checker
- `glp_runtime/lib/compiler/type_declarations.dart` - Type parsing
- `glp_runtime/bin/check_types.dart` - CLI tool
- `docs/type system/` - Handovers and documentation

## Testing Commands

**READ FIRST:** `docs/DISCIPLINE.md` Part II for complete testing protocol.

```bash
# Type check a specific program
cd glp_runtime && dart run bin/check_types.dart <program.glp>

# Run type checker tests
cd glp_runtime && dart test test/typechecker/

# Check all positive examples
for f in test/programs/typechecker/positive/*.glp; do
  dart run bin/check_types.dart "$f"
done
```

## Collaboration

### With Multiagent GLP
- Help with type declarations for multiagent programs
- `programs/multiagent/social_agent.glp` needs typing

### With Book GLP
- Ensure book examples are well-typed
- Provide type system explanations for chapters

## Terminology

- **GLP Type System** - the type system (not "PMT")
- **Typed GLP** - GLP programs with type declarations
- **SRSW** - Single Reader Single Writer constraint
