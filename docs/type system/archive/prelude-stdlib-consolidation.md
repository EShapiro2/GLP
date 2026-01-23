# Prelude/Stdlib Consolidation — COMPLETED

**From**: Overview GLP  
**Date**: 2026-01-18  
**Status**: ✓ COMPLETED by Typed GLP (2026-01-18)

---

## Summary

The current architecture has duplication and missing procedure declarations that cause type checking errors. This document provides instructions to consolidate the prelude (type checker's built-in definitions) with stdlib (runtime library), ensuring all predicates have proper type declarations.

---

## Problem Statement

Currently there are two sources of predefined definitions with overlapping content:

| Location | Content | Issue |
|----------|---------|-------|
| `glp_runtime/lib/analysis/type_checker/prelude.dart` | Type definitions, some procedure declarations, clause definitions (hardcoded Dart string) | Missing procedure declarations for `=/2`, `new_channel/2`, etc. |
| `programs/stdlib/*.glp` | Clause definitions only | No type or procedure declarations at all |

This causes two problems. First, when `=/2` appears in body position, the type checker reports "Undefined procedure: =/2" because the prelude intentionally omits the declaration (it assumed all uses would be unfolded). Second, programs importing stdlib predicates like `:=/2` or `mwm/2` get no type checking because stdlib has no declarations.

---

## Root Cause Analysis

The prelude comment states: "The following are defined guards (unit clauses) — no procedure declarations needed. They are unfolded at compile time by partial evaluation."

This is only partially correct. The partial evaluator (`transformDefinedGuards`) only unfolds unit clauses in **guard position** (before `|`). When `=/2` appears in **body position** (after `|`), it is NOT unfolded and the type checker sees it as a regular goal requiring a procedure declaration.

---

## Solution: Add Procedure Declarations

Add procedure declarations to the prelude for all predicates that can appear in body position. The partial evaluator will still unfold guard uses, but body uses will now type-check correctly.

### Step 1: Update prelude.dart

In `glp_runtime/lib/analysis/type_checker/prelude.dart`, add the following procedure declarations to the `typePrelude` string, in the `PROCEDURE DECLARATIONS` section:

```glp
% Defined predicates (have clauses, can appear in body)
procedure =(_?, _).
procedure new_channel(Channel?, Channel).
procedure send(_, Channel?, Channel).
procedure receive(_?, Channel?, Channel).
procedure dl_append(DiffList?, DiffList?, DiffList).
procedure dl_to_list(DiffList?, Stream).
```

The complete updated procedure declarations section should look like:

```glp
% =============================================================================
% PROCEDURE DECLARATIONS
% =============================================================================

% Type guards (runtime builtins)
procedure integer(Integer?).
procedure number(Number?).
procedure string(String?).
procedure atom(String?).
procedure constant(Constant?).
procedure compound(_?).
procedure is_list(Stream?).

% Groundness guards (runtime builtins)
procedure ground(_?).
procedure known(_?).
procedure unknown(_?).

% Arithmetic comparison guards (runtime builtins)
procedure <(Exp?, Exp?).
procedure >(Exp?, Exp?).
procedure =<(Exp?, Exp?).
procedure >=(Exp?, Exp?).
procedure =:=(Exp?, Exp?).
procedure =\=(Exp?, Exp?).

% Equality guard (runtime builtin)
procedure =?=(_?, _?).

% Univ operations (runtime builtins)
procedure =..(_, Stream?).
procedure ..=(Stream, _?).

% Defined predicates (have clauses below, can appear in body)
procedure =(_?, _).
procedure new_channel(Channel?, Channel).
procedure send(_, Channel?, Channel).
procedure receive(_?, Channel?, Channel).
procedure dl_append(DiffList?, DiffList?, DiffList).
procedure dl_to_list(DiffList?, Stream).
```

### Step 2: Update the Comment

Remove or update the misleading comment in prelude.dart that says "no procedure declarations needed":

**Before:**
```dart
// The following are defined guards (unit clauses) - no procedure declarations needed.
// They are unfolded at compile time by partial evaluation.
// - =/2 (unification)
// - dl_append/3, dl_to_list/2 (difference list operations)
// - new_channel/2, send/3, receive/3 (channel operations)
```

**After:**
```dart
// The following predicates have clauses below. They are unfolded when used in
// guard position (before |) but execute as normal goals in body position (after |).
// Procedure declarations are provided so body uses can be type-checked.
```

### Step 3: Add Type Declarations to stdlib Files

Each stdlib file should have type and procedure declarations. Here are the declarations to add:

**programs/stdlib/unify.glp** — No changes needed (covered by prelude)

**programs/stdlib/assign.glp** — Add at the top, after `-stdlib.`:

```glp
% Type for arithmetic expressions (matches prelude Exp)
% Exp ::= Number ; +(Exp, Exp) ; -(Exp, Exp) ; ... (already in prelude)

% Procedure declaration for arithmetic assignment
% Note: Result (left side) is always a Number, input (right side) is an Exp
procedure :=(Number, Exp?).
```

**programs/stdlib/univ.glp** — No changes needed (covered by prelude `=..` and `..=`)

**programs/stdlib/mwm.glp** — Add at the top, after `-stdlib.`:

```glp
% Types
MwmInput ::= [] ; [stream(Stream)|MwmInput] ; [merge(MwmInput)|MwmInput].
MutualRef ::= '_mutual_ref'(_, _).  % Opaque internal type
Done ::= done.  % Short-circuit termination marker

% Procedure declarations
procedure mwm(MwmInput?, Stream).
procedure mwm_main(MwmInput?, MutualRef?).
procedure mwm1(MwmInput?, MutualRef?, Done?, Done).
procedure mwm_copy(Stream?, MutualRef?, Done?, Done).
procedure close_when_done(Done?, MutualRef?).
procedure stream_append(_, MutualRef?, MutualRef).
procedure close_mutual_reference(MutualRef?).
```

**programs/stdlib/time.glp** — Add at the top, after `-stdlib.`:

```glp
procedure now(Integer).
```

**programs/stdlib/equator.glp** — Add at the top, after `-stdlib.`:

```glp
% Equator type (opaque internal structure)
Equator ::= '_equator'(_, _).

procedure equate(Equator?).
```

---

## Verification

After making these changes, verify by running:

```bash
# Type check a program that uses = in body position
cd /Users/udi/Grassroots/GLP/glp_runtime
dart run bin/check_types.dart ../programs/typed_book/streams/producers_consumers/channels.glp

# Should no longer report "Undefined procedure: =/2"
```

Run the typed REPL tests:

```bash
cd /Users/udi/Grassroots/GLP
bash test/run_typechecker_repl_tests.sh
```

Run the baseline tests to ensure no regressions:

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test && cd ..
bash test/full_run_repl_tests.sh
```

---

## Future Work: Extract Prelude to .glp File

The prelude is currently a hardcoded Dart string in `prelude.dart`. A future improvement would be to extract it to a proper `.glp` file (e.g., `programs/prelude/prelude.glp`) that can be edited without modifying Dart code. This is a separate task and not required for this fix.

---

## Design Decisions (Confirmed by Udi 2026-01-18)

1. **`=/2` signature**: `procedure =(_?, _).` — Wildcards are acceptable here since unification is polymorphic.

2. **`:=/2` signature**: `procedure :=(Number, Exp?).` — The result (left side) is always a Number. The input (right side) is an arithmetic expression.

3. **`mwm` helper types**: Use specific types. The short-circuit variables use `Done ::= done.` rather than wildcards.

---

## References

- `docs/type system/compilation-pipeline.md` — Pipeline architecture
- `glp_runtime/lib/compiler/partial_evaluator.dart` — Partial evaluation implementation
- `docs/DISCIPLINE.md` — Testing protocol

