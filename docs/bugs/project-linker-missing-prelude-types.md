# Bug: Project linker type-checking lacks prelude types

## Summary

When loading a multi-module project directory, type checking fails with
`UnknownTypeError` for any type defined in the root `programs/self.glp` prelude
(e.g., `Constant`, `Stream`, `Channel`, `DiffList`).  Only the four DFA
built-in primitives (`Integer`, `Real`, `Number`, `String`) are available.

## Minimal reproduction

```
programs/bug_repro/self.glp:
    Msg ::= text(Constant).

programs/bug_repro/main.glp:
    -module(main).
    procedure greet(Constant?, Constant).
    greet(X, X?).
```

```
GLP> programs/bug_repro
Error loading project programs/bug_repro: UnknownTypeError: Constant
```

Changing `Constant` to `String` makes it load successfully—confirming that
only the four hardcoded DFA primitives survive.

## Root cause

`_buildAncestorScope` in `project_linker.dart` (line 430) calls
`buildPreludeEnvironment()` to obtain the base type environment.

`buildPreludeEnvironment()` (in `type_environment_builder.dart`, line 74)
reads from `_preludeEnvironmentSource`, which is set by
`setPreludeEnvironmentSource()` during `GlpEngine` construction.  However,
the project linker functions (`discoverProject`, `typeCheckProject`,
`linkProject`) are library-level functions that run before or outside the
engine's prelude-source wiring.  When `_preludeEnvironmentSource` is null,
`buildPreludeEnvironment()` falls back to `typePrelude`, which is now the
empty string `''` (since all prelude content was moved to `programs/self.glp`).
The result is an empty `TypeEnvironment({}, {})`.

The ancestor scope chain only walks `self.glp` files *within* the project
directory.  It does not include `programs/self.glp` (the root prelude), so
prelude types like `Constant`, `Stream`, `Channel`, etc. are never added to
the type environment used for DFA construction.

## Impact

Every multi-module project that references any prelude-defined type (i.e.,
any non-trivial project) fails to load.  This blocks `cssg_modules_v2`,
`cssn_modules_v2`, and any future project.

## Suggested fix direction

The project linker's type-checking step needs access to the full prelude type
environment.  Options:

1. **Pass the prelude source/environment into the linker functions** — the
   engine already has it; thread it through `discoverProject` /
   `typeCheckProject`.

2. **Ensure `setPreludeEnvironmentSource()` is called before the linker
   runs** — verify the call order in the engine's project-loading path.

3. **Include `programs/self.glp` in the ancestor chain** — extend
   `discoverSelfChain` to walk above the project root up to the GLP
   programs directory.
