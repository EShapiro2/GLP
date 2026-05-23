# Bug: Local procedure not found at runtime when `-module(...)` is declared

**Status:** Open
**Discovered:** 2026-05-23
**Reporter:** Claude Code (SPM/GSG implementation pass)
**Affects:** Any module that declares `-module(...)` and contains a private (`procedure`, non-`exported`) helper called from another procedure's body.

## Symptom

At runtime, when a procedure's body calls another procedure in the same module, and the callee is declared with plain `procedure` (private), the spawn step fails with:

```
ERROR: Spawn could not find procedure label: <name>/<arity>
```

Workaround: declare the callee as `exported procedure`.

## Spec contradiction

`docs/modules/glp-module-system-spec.md` §4.1:

> By default, procedures are private — visible only within the defining module and its descendants (via ancestor scoping).

Same-module body calls to private procedures should resolve.  They don't, when `-module(...)` is explicit.

## Minimal repros

Run from `/Users/udi/Grassroots/GLP/glp_runtime`.

### Repro 1 — FAILS

```glp
%% /tmp/test_local.glp
-module(test_local).

exported procedure caller(Integer?, Integer).
caller(X, Y?) :- helper(X?, Y).

procedure helper(Integer?, Integer).
helper(X, Y?) :- Y := X? + 1.
```

```bash
echo -e 'load /tmp/test_local.glp\ncaller(5, Y).\n:quit' | dart run bin/glp_repl.dart
```

Output:
```
GLP> ✓ Loaded: /tmp/test_local.glp
GLP> ERROR: Spawn could not find procedure label: helper/2
Y = <unbound>
→ failed
```

### Repro 2 — WORKS (no `-module`)

Same as Repro 1 but with `-module(test_local).` removed.  The module name defaults to the filename.  Output: `Y = 6, → succeeds`.

### Repro 3 — WORKS (helper exported)

Same as Repro 1 but `procedure helper` → `exported procedure helper`.  Output: `Y = 6, → succeeds`.

### Repro 4 — FAILS (with exported trampoline)

```glp
-module(test_local5).

procedure caller(Integer?, Integer).
caller(X, Y?) :- helper(X?, Y).

procedure helper(Integer?, Integer).
helper(X, Y?) :- Y := X? + 1.

exported procedure top(Integer?, Integer).
top(X, Y?) :- caller(X?, Y).
```

`top(5, Y)` from REPL fails at the `caller/2` call site (not just at `helper/2`).

## Hypothesis on root cause

Informed guess, not verified by source inspection:

When `-module(M)` is declared explicitly, **exported** procedures probably get a namespaced label (e.g. `M#name/N`) AND a bare label (e.g. `name/N`).  **Private** procedures probably get only the namespaced label.  Body call sites resolve only the bare name `name/N`, so private procedures are unfindable.

Without `-module`, the module-name defaulting (from filename) probably routes through a different label-generation path that always emits the bare label.

A source-inspection task:

1. Find where procedure labels are generated during compilation (likely in `glp_runtime/lib/compiler/` or similar).
2. Compare the label set generated for `exported` vs `procedure` declarations when `-module` is present vs absent.
3. Find the body-call resolution site (probably during `Spawn` operation in the runtime executor) and confirm which label form it looks up.
4. Either fix label generation to always emit the bare form for same-module use, or fix the resolution site to fall back to module-namespaced forms.

## Regression test to add once fixed

Add to `test/run_all_tests.sh` (Section A or wherever local-procedure tests live):

```bash
# --- A?: Module-local private procedure body call ---
echo "--- A?: -module(...) + private helper called from body ---"
aN=$($DART run "$REPL" <<HEREDOC
$TYPED/module_local_private.glp
--
caller(5, Y).
:quit
--
HEREDOC
2>&1)
```

with `programs/tests/typed/module_local_private.glp`:

```glp
-module(module_local_private).

exported procedure caller(Integer?, Integer).
caller(X, Y?) :- helper(X?, Y).

procedure helper(Integer?, Integer).
helper(X, Y?) :- Y := X? + 1.
```

Expected: `Y = 6, → succeeds`.

## Impact

Any module that follows the spec's recommended discipline (`-module(...)` declared + private helpers + exported public API) will hit this on the first body call.  Workaround forces `exported` on every helper, exposing the entire implementation surface as the module's external API.

Currently affected code in this repo: `/Users/udi/Grassroots/GLP/programs/SPM/` exports several helpers (`fof_inner_lookup`, `fof_inner_set`, `route`, `close_all_routes`, `merge`, `extract`, `is_known`, `add_known`, `take_volition`, `epoch_of`, `set_epoch`, `is_even`, `is_odd`, `fof_epoch_of`, `set_fof_epoch`, `broadcast_update`, `send_snapshot`) as a workaround.  Once this bug is fixed, drop `exported` from helpers not intended as the module's public API.
