# Step 2.5: Remove Renamed Procedure Copies

**Date**: 2026-03-11
**Context**: Step 2.4 is complete. Monomorphic types removed, parameterized proc decls in place. 390/390 REPL tests pass.

**Goal**: Remove the Section 14 workarounds — renamed procedure copies like `send_agent`, `send_user`, `merge_net_in` — that were needed before parameterized types existed. Now that `send`, `receive`, `new_channel`, and `merge` are parameterized, the type checker infers precise types at each call site, making renamed copies unnecessary.

## Background (from typed-glp-manual.md Section 14)

Before parameterized types, generic procedures like `send(_?, Channel?, Channel)` produced generic output types in body position. To get precise types, modules defined renamed copies:

```
procedure send_agent(MediatorToAgentMsg?, AgentChannel?, AgentChannel).
send_agent(X, ch(In, [X?|Out?]), ch(In?, Out)).
```

With parameterized types, the prelude's `procedure send(X?, Channel(Y, Stream(X))?, Channel(Y, Stream(X))).` infers the correct types from context — so `send_agent` is now redundant.

## Execution

### Step 1: Find all renamed copies

Search ALL .glp files under `programs/` (excluding `archive/`, `OLD`, `book 2/`) for:
- Procedure declarations named `send_agent`, `send_user`, `merge_net_in`, `merge_agent`, or any other procedure that is a renamed copy of `send`, `receive`, `new_channel`, or `merge` with precise types
- Also search for their call sites in clause bodies

### Step 2: For each renamed copy found

Replace each call to the renamed copy with a call to the original generic procedure. For example:
- `send_agent(Msg?, Ch?, Ch1)` → `send(Msg?, Ch?, Ch1)`
- `send_user(Notify?, Ch?, Ch1)` → `send(Notify?, Ch?, Ch1)`
- `merge_net_in(In?, FIn?, In1)` → `merge(In?, FIn?, In1)`

Then remove the renamed procedure declaration and its clause(s).

### Step 3: Handle defined guard copies

`send`, `receive`, and `new_channel` are **defined guards** (single-unit-clause procedures) — when called in guard position, the partial evaluator unfolds them. The renamed copies (`send_agent`, `send_user`) serve no purpose even in guard position since the PE unfolds based on clause structure, not type declarations.

However, verify that the generic `send`/`receive` are available in scope. In module files, the prelude's defined guards are inherited. If any module has local `send`/`receive` definitions (not renamed — the actual generic ones), those should stay.

### Step 4: Test after each file

Run `bash test/run_all_tests.sh` after converting each module (or batch of related files). All 390 tests must pass.

### Step 5: Update typed-glp-manual.md

Section 14 should be updated to note that renamed copies are no longer needed thanks to parameterized types (Section 17). The section can be kept for historical context but marked as obsolete.

## Key principle

The renamed copies have identical clause bodies to the originals — only the proc decl and procedure name differ. Replacing `send_agent(...)` with `send(...)` in the clause body is always correct because the clauses are structurally identical. The type checker will infer the precise types from the call context.

## After completion

Update `docs/type system/current_plan.md`: mark Step 2.5 as done.
