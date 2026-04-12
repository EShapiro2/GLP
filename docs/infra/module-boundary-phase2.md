# Phase 2: Enforce Module Boundaries in combinedProgram

## Spec reference

typed-glp-manual.md §19.3:
- `procedure` — visible only within this module
- `exported procedure` — callable from other modules
- Root `self.glp` procedures are visible everywhere via ancestor scoping (§19.6)

## What to change

`glp_runtime/lib/engine/glp_engine.dart`, the `combinedProgram` getter.

Currently it merges all loaded programs' bytecode into one flat program, making every procedure reachable from REPL goals. This violates the spec.

After this change, REPL goals should only be able to resolve procedures that are:
1. Declared `exported procedure` in any loaded module
2. Defined in root `self.glp` (the `__root_self__` program)
3. Defined in a project loaded via `loadProject` (static linking already flattens correctly — entry-point aliases are generated for exported procedures)

Plain `procedure` declarations in individually-loaded files must NOT be reachable from REPL goals.

## Implementation guidance

The engine already tracks `ModuleInfo` per loaded file, including `hasExports`. The compiler already knows which procedures are exported (it generates entry-point aliases in the linker). The task is to filter the labels in `combinedProgram` so that only exported procedure labels (and root self.glp labels) are resolvable.

One approach: instead of merging all ops blindly, build a combined program that includes all bytecode (procedures still need their internal helpers to execute) but whose `labels` map only exposes exported procedures and root self.glp procedures. Internal procedures are present in the bytecode but not addressable as entry points.

Read the code to understand the exact mechanism, then implement.

## Verify

```bash
cd /Users/udi/Grassroots/GLP && bash test/run_all_tests.sh
```
Must be 428/428.

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test
```
Must be 0 failures.

## Negative test

After implementing, verify that a plain `procedure` in a loaded file is NOT callable from the REPL. Create a small test: load a file with both `exported procedure public(...)` and `procedure private(...)`. Verify `public(...)` succeeds and `private(...)` fails with "predicate not found". Add this test to `test/run_all_tests.sh` Section L or a new section.

## Commit

```bash
git add -A && git commit -m "Enforce module boundaries: REPL goals restricted to exported procedures"
```

Push and provide merge instructions.
