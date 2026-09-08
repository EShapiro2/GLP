## State (SGSG Code, 2026-09-07)

The three directories are one program, `programs/spm`, entered through `spm/self.glp`, which exports every play; `cva/` still loads on its own (suite Section X10), and Section SG runs the plays.  Load `programs/spm` in glpc and post a play by plain name.

| Play | Result | What it verifies |
|---|---|---|
| `play_befriend(A, B)` | `A=1, B=1` → succeeds | Offer + Accept + Integrate accept with reply-variable sync. |
| `play_befriend_simultaneous(A, B)` | `A=1, B=1` → succeeds | Resolve simultaneous offer. |
| `play_three_agents` | → succeeds | Three-agent befriend + cross-broadcast (stream dissemination). |
| `play_secure_befriend(A, B)` | `A=1, B=1` → succeeds | Befriend extension: IR transported and stored as `full(Q, E, K, σ)`. |
| `play_secure_rebroadcast(A, B)` | `A=1, B=1` → suspended | Periodic re-broadcast; the recipient integrates the checkpoint (suspended is intentional, see the play). |
| `play_secure_unfriend(A, B, IR)` | `A=2, B=2, IR=ir([cust_a1, cust_a2], 67)` → succeeds | End friendship and Integrate unfriend on the secure FMap: only the epoch moves, K and σ preserved. |
| `play_secure_restore(A, Z, B, IR)` | `A=1, Z=0, B=1, IR=ir([cust_a1, cust_a2], 67)` → succeeds | The state-loss fault (`crash`) and passive Restore: skeleton install from the checkpoint's identity record, direct heal of the epoch. |

Code:

- `self.glp` — the program's root: the plays as entry points.
- `cva/{self.glp, network.glp}` — CVA substrate + mediator.
- `gsg/{self.glp, gsg_agent.glp}` — the social graph (SPM, GSG-CVA); `gsg/plays/`.
- `secure_gsg/{self.glp, secure_gsg_agent.glp}` — the secure social graph: Befriend extension, periodic re-broadcast, Integrate checkpoint in full (skeleton install, stub promotion, direct heal, observer heal under the epoch order), the state-loss fault and Restore, Unfriend inherited; `secure_gsg/plays/`.

| `play_secure_replace(R, A, B, C, D)` | `R=ok, A=1, B=0, C=1, D=1` → succeeds | Identity loss and the Replace cascade: bob vouches, alice2 announces on a supermajority, bob and carol rename alice to alice2 at epoch 1 and rebind, alice2's stubs become full entries. |

The Replace cascade (SPM, Replace Protocol) is implemented in full (2026-09-08): Vouch, Announce new identity (the replaced identity's custodian record supplied by the person, spec-issue F), Integrate new identity, Integrate rebind; vouches are retained in the Inbox and every stub is notified at each announcement.  Not checked: Vouch's precondition p ∈ dom(FMap_c), and w ∈ known_{p'} for the agents notified.

The fault harness below (supervisor, mediator rebind) was not built: `crash` is a clause of the agent that resets the platform state and keeps the streams, which is enough for Restore; in-flight input is not discarded.

## Next Work

Restore and Replace are done (above).  The architectural notes below were written for a fault harness with a supervisor; neither needed one (the plays script crashes and vouches directly), and the notes are kept for the message-loss scenarios still unplayed.

### Architectural questions, with proposed answers

#### Q1.  Mediator rebind semantics on crash.

When the supervisor signals "agent `p` crashed and is reborn with fresh streams", does the network mediator drop in-flight messages to/from the old endpoint, or drain them into the new endpoint?

**Proposed: drop.**  Paper §4.3 specifies that outbox and inbox are both reset on crash; the abstract semantics already considers in-flight messages from/to the crashed agent as gone.  The implementation matches by (a) discarding pending NetIn for the crashed agent, and (b) ceasing to read from the old NetOut.  Drainage would let the reborn agent see messages the paper considers lost, which would break abstract retirement.

#### Q2.  Supervisor — separate process or boot-wired?

Architecture §10 leaves this open.

**Proposed: separate process.**  The supervisor implements a stable abstraction — "a crash-restartable agent with persistent IRec".  The play instantiates `supervisor(Id, IRec, …)`, not raw `agent(Id, IRec, …)`; the supervisor instantiates the agent and intermediates UserIn / NetIn / NetOut.  Encapsulating the restart logic keeps fault plays small and keeps the agent itself fault-unaware.

#### Q3.  Crash trigger — supervisor-intercept or agent-signals-supervisor?

The `crash` UserIn event is in the type but no agent clause handles it.

**Proposed: supervisor intercepts.**  The supervisor sits between actor and agent on the UserIn stream as a filter.  On `crash` it tears down the agent and re-spawns; non-crash events pass through to the agent unchanged.  The agent stays focused on protocol semantics and never sees `crash`.

#### Q4.  Scope of the next session.

**Proposed: split.**  Harness + PASS B-full (the stub install / promote / direct heal branches of Integrate checkpoint, exercised by a crash-and-recover play) is one session.  PASS C (Replace cascade — vouch, new_identity, rebind, supermajority counting, stub installation from rebinds) is its own session: three new cargo types, two new volitions, a fix-point cascade, and supermajority arithmetic.  Doing both in one session is too large to test incrementally.

## Deferrals (paper-spec-code gap; must close before declaring done)

- **Replace preconditions**: Vouch does not check p ∈ dom(FMap_c); Announce does not check w ∈ known_{p'}.
- **Integrate accept precondition `epoch_p(q) < x`** (`gsg/gsg_agent.glp`): not enforced; stale/duplicate accepts re-trigger broadcast/snapshot.
- **`stream_update` precondition `q ∈ dom(FMap_r)`**: receiver doesn't check membership before FoFMap update.
- **Application-data field**: FMap entry data slot is implicit `⊥`; Get/Set data operations not exposed.
- **`checkpoint/3` carries the date** (`checkpoint(Date, IR, L)`); the paper's cargo is `checkpoint(R, L)`, the date having been dropped as redundant under monotone absorption.

## How to Run

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
printf '/Users/udi/Grassroots/GLP/programs/spm\n:limit 1000000\nplay_secure_restore(A, Z, B, IR).\n:quit\n' | bin/glpc
```

All plays: suite Section SG (`bash test/run_all_tests.sh` from the GLP root).

## Read Before Starting

1. `/Users/udi/Grassroots/CLAUDE.md` — top-level project rules; in particular **"Paper – Spec – Code Harmonisation"** and **"Standing Principle: Do the Right Thing"**.
2. `/Users/udi/Grassroots/GLP/CLAUDE.md` — GLP project rules + the mandatory reading list it points to (DISCIPLINE, typed-glp-manual, glp-cheat-sheet).
3. `/Users/udi/Grassroots/GLP/docs/typed-glp-manual.md` §8 — guard rule (no recursion in guards; only compile-time-unfoldable calls).
4. `/Users/udi/Grassroots/GLP/programs/SPM/docs/glp-architecture.md` — implementation architecture (focus §3.3 reply-variable sync; §4 fault harness for the next session).
5. `/Users/udi/Grassroots/SPM/docs/spec-issues.md` — A–O resolutions; the implementation follows them.
6. `/Users/udi/Grassroots/SPM/sections/{cva,gsg-cva,secure-gsg-cva,secure-gsg-cva-replace,secure-gsg-cva-restore}.tex`.
7. Existing code under `/Users/udi/Grassroots/GLP/programs/SPM/{cva,gsg,secure_gsg}/`.

## Idioms in the Existing Code

- **Reply-variable sync** (architecture §3.3): each volition carries a `Constant` Reply the agent binds to `ok` on protocol completion; play uses `await(R?)` to synchronise.  See `gsg/self.glp` `bind_ok/1`, `take_pending_offer/4` (Integrate accept); `gsg/agent.glp` will/await clauses.
- **Body-dispatch on combined check+take.**  List-search preconditions can't be guards (typed-glp-manual §8.1).  Use a `try_take_*` helper returning `(NewList, Matched, ...extracted)` in one recursion, then a 2-clause `*_step` dispatcher branching on `Matched` with head-literal `true`/`false` + trivial `ground(X?)` guard.  See `gsg/self.glp` `try_take_accept_offer_vol/4`, `try_take_friend_request_from/5`; `gsg/agent.glp` `friend_request_step`, `accept_offer_step`.
- **Three-way classify dispatch** for Resolve simultaneous offer: `classify_pending/5` returns `pm_resolve` / `pm_ignore` / `pm_none` in a single Pending read, doing name-order comparison with `P @< Self` (GLP defines `@<` but not `@>`).  See `gsg/self.glp` `classify_pending`; `gsg/agent.glp` `friend_request_dispatch`.
- **`bind_ok` helper** (modelled on `misc/rv_agent.glp` `complete_rendezvous`): unit clause with head literal binds a writer via head construction.  This is the only reliable way to bind a play-provided `Constant` Reply from the agent's body — head literals against unbound readers in the agent's own head suspend (three-valued unification ASK-only); body `=` between Constant writers fails type-check on `_` ≠ `Constant`.
- **`SecureFMap` entry kinds** (paper §8.5 spec-issue E): `full(Q, E, K, σ)`, `skeleton(Q, K, σ)` (epoch implicit 0), `stub(Q, E)` (K, σ both ⊥).  `set_full_entry/6` replaces any kind with a new full entry; helpers `secure_epoch_of`, `secure_broadcast_update`, `secure_send_snapshot` handle all three kinds.
- **Stream-extending procedures** use the chunk-writer / continuation-reader asymmetric mode convention (cheat-sheet §3e): `broadcast_*` and `send_snapshot` take `Stream(...)` at ↑ for the chunk and `Stream(...)?` at ↓ for the continuation; empty/alias clause aliases the two.
