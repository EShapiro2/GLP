# Phase 1: Add `exported` to All REPL-Called Procedures

## Why

The spec (typed-glp-manual.md §19.3) says `procedure` is module-local. The REPL should only be able to call `exported procedure` declarations. We are preparing for enforcing this in the runtime by first adding `exported` to every procedure that the REPL tests call directly.

## Rule

For every `.glp` file that is loaded individually (not via project directory) in the REPL tests, and where the REPL then calls a procedure from that file as a goal: that procedure must be declared `exported procedure`.

**Do NOT change:**
- Files loaded as part of a project directory (Sections F–K) — these already have proper export declarations
- Files in Section L (dynamic dispatch) — already exported
- Files in Section I (self.glp tests) — already exported
- Section B positive type-check files — these are only loaded, no goals are called
- Section C/D negative tests — these are expected to fail
- Procedures that are only called internally (not from the REPL goal line)
- Anything in `programs/self.glp` (root prelude)

**Do change:**
- Section A files: every procedure that appears as a REPL goal needs `exported`
- Section A26 has no files (bare REPL goals like `T1 =.. [foo]`) — skip those

## How to identify what needs `exported`

Read `test/run_all_tests.sh`. For each Section A test block:
1. Find the loaded `.glp` files
2. Find the REPL goals (lines after the file loads, before `:quit`)
3. The functor of each goal is a procedure that must be `exported` in its file

For example, if the test loads `append.glp` and runs `append([a,b], [c,d], Zs).`, then `append/3` in `append.glp` must be `exported procedure`.

## How to make the change

In each `.glp` file, find lines like:
```
procedure foo(...)
```
and change to:
```
exported procedure foo(...)
```

Only for the specific procedures called as REPL goals. Leave internal helper procedures as plain `procedure`.

## Verify

```bash
cd /Users/udi/Grassroots/GLP && bash test/run_all_tests.sh
```

Must still be 428/428. Adding `exported` should not change behavior — it's a declaration change only.

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test
```

Must still be 0 failures.

## Commit

```bash
git add -A && git commit -m "Add exported to all REPL-callable procedures"
```

Then proceed to Phase 2: read `docs/infra/module-boundary-phase2.md`.
