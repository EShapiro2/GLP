# SPM/GSG Implementation — Handover

**Date:** 2026-05-23
**Status:** Phase 1 working for clean GSG; secure_gsg types ready; agent clauses + plays for secure_gsg are next.

## What's done

### Architecture
- `/SPM/docs/glp-architecture.md` (v2): per-agent process shape, CVA→GLP translation, fault harness, test-play conventions, type structure sketch.  Read this first.

### CVA substrate (`cva/`) — runtime-tested ✅
- `cva/self.glp`: types (`NetMsg`, `InMsg`, `UserEvent`, `IdentityRecord`, `NetEnd`, `NetEndList`, `IdIn`, `IdInList`, `ReaderList`, `Agent`).
- `cva/network.glp`: `network/1` + `extract/3` + `merge_all/2` + `dispatch/2` + `route/4` + `merge/3` + `close_all_routes/1`.  Every procedure runtime-tested in isolation (steps 1–7 of the bottom-up smoke test).

### Clean GSG (`gsg/`) — protocol verified by trace ✅
- `gsg/self.glp`: types + helpers (`epoch_of`, `set_epoch`, `bump_epoch`, `is_known`, `add_known`, `take_volition`, `broadcast_update`, `send_snapshot`, `is_even`, `is_odd`, `fof_epoch_of`, `set_fof_epoch`, `fof_inner_lookup`, `fof_inner_set`).
- `gsg/agent.glp` (module `gsg_agent`): `agent/7` with clauses for `discover`, `offer_friendship`, `end_friendship`, `tick`, `query`, `shutdown`, NetIn handling for `friend_request` / `accept` / `unfriend` / `stream_update`, catch-alls.  `activate/13` helper for broadcast + snapshot on activation.
- `gsg/plays/play_befriend.glp` and `play_three_agents.glp`: 2-agent and 3-agent end-to-end plays.  Both verified by `:trace` mode.  Both end `→ suspended` (no clean quiescence; queries fire before message round-trip).

### Secure GSG (`secure_gsg/`) — types only
- `secure_gsg/self.glp`: types for IR-carrying cargos (`friend_request(x, IR)`, `accept(x, IR)`, `checkpoint(t, IR, L)`, `vouch`, `new_identity`, `rebind`), discriminated `SecureFMapEntry` (`full` / `skeleton` / `stub`), platform state with `LastBroadcastDate`.  Loads cleanly.
- No clauses yet.

## Open work — recommended order

1. **Reply-variable sync (architecture §3.3)** — DO THIS FIRST.  Without it, every test play has to be verified by `:trace` mode, which doesn't scale.  Design: each `will(V)` carries a `Reply` reader the agent binds when V's transaction completes.  Subsequent UserIn events use `ground(Reply?)` guards to synchronise.  Implementation: extend `GsgVolition` with reply args, track pending volitions in `Volitions` field, agent's accept/integrate clauses bind matching replies.

2. **Resolve simultaneous offer (paper §6.3)** — add `PendingOut` field to platform tracking outgoing friend_requests.  New clause that detects `friend_request(x)` from P when `(P, x) ∈ PendingOut`, compares `Self @> P` for name-order, fires resolve (= same as accept but unguarded).  Test play: two agents simultaneously offer.

3. **Secure GSG clauses** — implement in `secure_gsg/agent.glp` (module `secure_gsg_agent`) + `secure_gsg/platform.glp`:
   - Befriend extension: store IR in `full` FMap entry on accept.
   - Periodic re-broadcast: clause triggered by `tick` when `LastBroadcastDate < Date`; emits `checkpoint(Date, IR_p, L)` to all in Rec; updates `LastBroadcastDate`.
   - Integrate checkpoint (paper §8.5): three sub-clauses — (i) install skeleton if sender absent, (ii) **promote stub to full using carried IR** (spec-issue O), (iii) advance epoch on direct heal, (iv) absorb FoF pairs.
   - Restore: passively covered by Integrate checkpoint; just write a play that crashes an agent and watches the restore.
   - Replace cascade: `vouch`, `announce_new_identity`, `integrate_new_identity`, `integrate_rebind`.

4. **Fault harness** — supervisor process that allocates fresh streams on `crash` from UserIn (architecture §4.1).  Then test plays for crash → Restore and identity-loss → Replace.

## Known gotchas (documented in cheat-sheet §14, §15)

- **Each `.glp` file is its own module.**  Cross-file procedures must be visible via parent `self.glp` ancestor scoping (per spec §3.1) or via `imported procedure M#p` declaration.  Same-module multi-file does NOT work (each file is a separate compilation unit).

- **Load whole directory as a project** (`GLP>  ../programs/SPM/`), not file-by-file.  Cross-module resolution requires project loading.

- **Type aliases of `Integer`, `Constant`, etc. don't inherit SRSW constant-type relaxation** (cheat-sheet §14).  Use primitive types directly at positions where multi-reader use is needed.

- **Bug: private procedures invisible at runtime when `-module()` declared** — see `/GLP/docs/bugs/local-procedure-not-found-with-module-directive.md`.  Workaround: declare every helper `exported procedure`.  Should be fixed soon (another session was assigned).

- **Stream-extending procedures need asymmetric ↑/↓ arg modes** (chunk-writer at ↑, continuation-reader at ↓) for SRSW pairing in the empty/alias clause (cheat-sheet §3e).

- **`Spawn could not find procedure label`** at runtime is the symptom of the above module bug.  Almost always fixed by adding `exported`.

## How to run

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
echo -e '../programs/SPM/\nplay_befriend(A, B).\n:trace\nplay_befriend(A, B).\n:quit' | dart run bin/glp_repl.dart
```

`:trace` mode is the verification tool until reply-variable sync lands.

## Read before starting

In order:
1. `/SPM/docs/glp-architecture.md`
2. `/GLP/docs/typed-glp-manual.md` §§15, 15B, 3.3–3.5
3. `/GLP/docs/glp-cheat-sheet.md` §§3b–3e, 13, 14, 15
4. `/SPM/sections/cva.tex`, `/SPM/sections/gsg-cva.tex`, `/SPM/sections/secure-gsg-cva.tex`, `/SPM/sections/secure-gsg-cva-replace.tex` (paper specs)
5. `/SPM/docs/spec-issues.md` (all A–O resolutions; the implementation follows them)
6. `/GLP/programs/SPM/cva/self.glp`, `/cva/network.glp` (substrate; how the wiring works)
7. `/GLP/programs/SPM/gsg/self.glp`, `/gsg/agent.glp`, `/gsg/plays/*.glp` (clean GSG; the model for secure_gsg)
