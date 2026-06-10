# Phase 2: Enforce Module Boundaries in combinedProgram

## Context

Phase 0 (baseline) and Phase 1 (add `exported` to all REPL-callable procedures) are complete. An infrastructure goal classification bug was also fixed: serve goals spawned by auto-activation are now tagged in `rt.infrastructureGoalIds` and excluded from the scheduler's status determination (see dynamic-module-dispatch.md §3.4–3.5).

## Spec reference

typed-glp-manual.md §19.3:
- `procedure` — visible only within this module
- `exported procedure` — callable from other modules
- Root `self.glp` procedures are visible everywhere via ancestor scoping (§19.6)

## The bug

`glp_runtime/lib/engine/glp_engine.dart`, the `combinedProgram` getter merges all loaded programs' bytecode into one flat program, making every procedure reachable from REPL goals. This violates the spec: plain `procedure` declarations should be module-local, not callable from the REPL.

## What to change

After this change, REPL goals should only be able to resolve procedures that are:
1. Declared `exported procedure` in any loaded module
2. Defined in root `self.glp` (the `__root_self__` program)
3. Defined in a project loaded via `loadProject` (static linking already flattens correctly — entry-point aliases are generated for exported procedures)

Plain `procedure` declarations in individually-loaded files must NOT be reachable from REPL goals.

## Implementation approach

Include all bytecode in the combined program (internal helpers must still be reachable when an exported procedure calls them). But filter the `labels` map so only exported procedures and root `self.glp` procedures are addressable as entry points.

The engine tracks `ModuleInfo` per loaded file, including `hasExports`. The compiler knows which procedures are exported. The task: build `combinedProgram` with a filtered labels map.

To identify which labels are exported: parse each loaded module's source to find `exported procedure` declarations and collect their `name/arity` labels. The `__root_self__` program's labels are all included. The `__project__` program's labels are all included (static linker already handles this correctly).

Read the code to understand the exact mechanism, then implement.

## Verify

```bash
cd /Users/udi/Grassroots/GLP && bash test/run_all_tests.sh
```

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test
```

Both must pass with no regressions from the baseline.

## Negative test

After implementing, verify that a plain `procedure` in a loaded file is NOT callable from the REPL. Create a test file `programs/tests/typed/test_module_boundary.glp` with:

```glp
exported procedure public_proc(Integer?, Integer).
public_proc(X, Y?) :- Y := X? + 1.

procedure private_proc(Integer?, Integer).
private_proc(X, Y?) :- Y := X? + 2.
```

Add a test to `test/run_all_tests.sh` (new Section M or append to Section L) that:
1. Loads this file
2. Runs `public_proc(5, X).` — expects `X = 6`
3. Runs `private_proc(5, X).` — expects "not found" or failure (not `X = 7`)

## Commit

```bash
git add -A && git commit -m "Enforce module boundaries: REPL goals restricted to exported procedures"
```

Push and provide merge instructions.
