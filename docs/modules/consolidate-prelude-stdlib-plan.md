# Consolidate Prelude and Stdlib into Root

**Date:** 2026-03-05

## Problem

Declarations needed by the type checker are split between two places:

- `prelude.dart` — hardcoded Dart string with types + procedure declarations
- `programs/stdlib/*.glp` — GLP files with procedure implementations

Some declarations are in prelude but not stdlib. Some are in stdlib but not prelude. Some are in both. User programs sometimes need to add local `procedure now(Constant).` declarations because the type checker doesn't see stdlib.

## Design

### What stays in the Dart prelude

Only primitive types that cannot be expressed in GLP:

```
Number, Integer, Real, String
```

These are built-in to the runtime and have no `.glp` representation.

### What moves to a GLP root

Everything else — types, guards, system predicates, stdlib procedures — lives in `.glp` files at the root of the scope chain. This is the outermost ancestor, visible to every project and every module.

**Location:** `programs/self.glp`

**Contents:**
- Type definitions: `Stream`, `DiffList`, `Channel`, `Constant`, `Exp`, `Equator`, `MwmInput`, etc.
- Guard declarations: `integer/1`, `number/1`, `ground/1`, `=?=/2`, etc.
- System predicate declarations + clauses: `=/2`, `new_channel/2`, `send/3`, `receive/3`, `dl_append/3`, `dl_to_list/2`
- Arithmetic: `:=/2` and all its clauses (currently in `stdlib/assign.glp`)
- Time: `now/1` (currently in `stdlib/time.glp`)
- Univ: `=../2`, `..=/2` (currently in `stdlib/univ.glp`)
- MWM: `mwm/2` and helpers (currently in `stdlib/mwm.glp`)
- Equator: `equate/1` (currently in `stdlib/equator.glp`)

### Scope chain

```
Dart primitives (Number, Integer, Real, String)
    ↓
programs/self.glp (types, guards, system predicates, stdlib)
    ↓
project/self.glp (project-specific types)
    ↓
module.glp
```

### What this eliminates

- The hardcoded `typePrelude` string in `prelude.dart` (except primitives)
- The separate `programs/stdlib/` directory
- Local `procedure now(Constant).` declarations in user files
- The `_output/1` type warning in system files
- Confusion about what's declared where

## Decisions

### Unify `-mode(system).` and `-stdlib.`

Currently two separate directives:
- `-mode(system).` = allows underscore-prefixed constants (parser/compiler)
- `-stdlib.` = grants body kernel access (runtime)

These serve the same purpose: marking system code. Unify into `-mode(system).` which does both. Remove `-stdlib.` entirely.

### `programs/self.glp` is the root

The outermost `self.glp` at `programs/self.glp`. Every project directory under `programs/` sees it through ancestor scoping.

### Dart prelude shrinks to primitive types only

Only `Number`, `Integer`, `Real`, `String` — types with no GLP representation.

### Internal procedures (`_output/1`, `_send/3`, `write/1`, `writeln/1`)

Need to determine: which belong in the language (declared in root), which are internal (only visible to `-mode(system).` code), which should be removed.

## Status

**Complete.** All steps implemented and tested. 382/382 REPL, 374+5skip-14fail Dart (matching baseline).

## Implementation (completed)

1. ~~Unify `-mode(system).` and `-stdlib.`~~ — remove `-stdlib.` directive, `-mode(system).` grants both constant access and kernel access
2. Create `programs/self.glp` with all type definitions and procedure declarations from the current prelude + stdlib
3. Move stdlib clause implementations into `programs/self.glp` (or keep as separate files loaded from the same directory)
4. Shrink `prelude.dart` to just primitive types (Number, Integer, Real, String)
5. Modify `buildPreludeEnvironment()` to load `programs/self.glp` on top of primitives
6. Modify the REPL and engine to load root implementations (clauses) at startup
7. Remove `programs/stdlib/` directory
8. Test everything
