## State

Committed at GLP `1ca04b2c`.  Five plays load, type-check, and run:

| Play | Result | What it verifies |
|---|---|---|
| `play_befriend(A, B)` | `A=1, B=1` → succeeds | Paper §6.3 Offer + Accept + Integrate accept with reply-variable sync. |
| `play_befriend_simultaneous(A, B)` | `A=1, B=1` → succeeds | Paper §6.3 Resolve simultaneous offer. |
| `play_three_agents` | → succeeds | Three-agent befriend + cross-broadcast (paper §6.5 stream dissemination). |
| `play_secure_befriend(A, B)` | `A=1, B=1` → succeeds | Paper §8.2 Befriend extension — IR transported on friend_request/accept, stored as `full(Q, E, K, σ)` in FMap. |
| `play_secure_rebroadcast(A, B)` | `A=1, B=1` → suspended | Paper §8.5 Re-broadcast plumbing — tick fires checkpoint, recipient runs Integrate checkpoint's FoF-absorption branch.  Suspended is intentional (bob omits shutdown so he is alive to integrate; doc'd in the play). |

Code:

- `cva/{self.glp, network.glp}` — CVA substrate + mediator.
- `gsg/{self.glp, agent.glp}` — clean GSG (paper §6.1–§6.5).
- `gsg/plays/{play_befriend, play_befriend_simultaneous, play_three_agents}.glp`.
- `secure_gsg/{self.glp, agent.glp}` — secure GSG (paper §8.2 in full; §8.5 partial — see Deferrals).
- `secure_gsg/plays/{play_secure_befriend, play_secure_rebroadcast}.glp`.

All helpers are declared `exported procedure` as a workaround for `/GLP/docs/bugs/local-procedure-not-found-with-module-directive.md` (still open as far as this session knows).

## Next Work — Fault Harness + PASS B-full

Required to exercise the parts of the paper that only fire under faults: state-loss Restore (paper §8.5 stub install / direct heal in Integrate checkpoint) and identity-loss Replace (paper §8.7).

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

Tracked per the "Paper – Spec – Code Harmonisation" rule in `/Users/udi/Grassroots/CLAUDE.md`.

- **Integrate accept precondition `epoch_p(q) < x`** (`gsg/agent.glp`): not enforced; stale/duplicate accepts re-trigger broadcast/snapshot.  Fix: precondition check via body dispatch on the `epoch_of(P, FMap)` result, analogous to PASS 3 dispatch.
- **Integrate unfriend precondition `epoch_q(p) < x`** (`gsg/agent.glp`): same shape; not enforced.
- **Integrate checkpoint branches** (`secure_gsg/agent.glp`): only the FoF-absorption case is implemented.  Skeleton install (when P ∉ dom(FMap_r)), stub promotion (spec-issue O), and direct heal ((r, e) ∈ L update of FMap_r[p].epoch) are the three branches the harness session will add.
- **`stream_update` precondition `q ∈ dom(FMap_r)`** (paper §6.5): receiver doesn't check membership before FoFMap update; spurious stream updates from non-friends are absorbed.
- **Application-data field** (paper §6.7): FMap entry data slot is implicit `⊥`; Get/Set data operations not exposed.

## How to Run

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
echo -e '../programs/SPM/\nplay_secure_befriend(A, B).\n:quit' | dart run bin/glp_repl.dart
```

All five plays in one shot:

```bash
echo -e '../programs/SPM/\nplay_befriend(A, B).\nplay_befriend_simultaneous(A, B).\nplay_three_agents.\nplay_secure_befriend(A, B).\nplay_secure_rebroadcast(A, B).\n:quit' | dart run bin/glp_repl.dart
```

`:trace` before the goal enables full trace; redirect to `/private/tmp/spm.txt` and `Read` it.

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
