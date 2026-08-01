# GLP Multiagent Flutter Apps

**Updated: 2026-08-01**

Flutter desktop apps for simulating GLP multiagent systems.  Each app names its
own GLP program and uses its own execution backend and agent topology.

## Apps (entry points)

| Entry point | App | Agents | Backend | Status |
|---|---|---|---|---|
| `lib/main.dart` | Interactive SG | Alice, Bob, Charlie | Multi-window (`desktop_multi_window`) | Working |
| `lib/main_cssg.dart` | CSSN group plays (REPL) | Alice, Bob, Carol, Dave | REPL subprocess | Working |

`main_sg_mad.dart`, `main_cssg_mad.dart` and `main_cssg_mad_modules.dart` were
retired on 2026-08-01: each named a program directory that no longer exists
(`programs/typed_book/cssg`, `programs/social/child_safe`), and the CSSG
programs they ran are removed with `programs/book/cssg` (Udi, 2026-08-01).  The
table above lists only the entry points this file documents; `lib/` holds
others.

### Interactive SG (`main.dart`)

Multi-window app.  Coordinator spawns one OS window per agent.  User types
commands (`connect`, `decision`, `send`, `introduce`, `accept_intro`) in each
agent's text field.  Uses `desktop_multi_window` plugin and `MadRouter` for
cross-window message routing.  Full 10-step introduction protocol verified
working.

### CSSN group plays — REPL (`main_cssg.dart`)

Single-window app with four read-only panels (Alice, Bob, Carol, Dave).
Runs plays 8–10 by spawning a `glp_repl` subprocess.  Tagged output is parsed
and routed to the correct panel.  Uses `ReplPlayRunner` from `glp_runtime`,
naming its own file list — the runner has no default one.

## Shared infrastructure

| File | Purpose |
|---|---|
| `lib/isolate_protocol.dart` | Message types between main isolate and agent isolates, agent isolate entry point, lifecycle documentation |
| `lib/mad_router.dart` | `IsolateRouter` — routes MAD messages between agent isolates via `SendPort`, with message buffering for unregistered agents |

## Two-phase deferred-start protocol

Used by `main_grassapp_duo.dart`.  Documented in `isolate_protocol.dart`.

1. **Phase 1 — Spawn**: Main spawns all agent isolates with `deferStart: true`.
   Each isolate creates its `AgentRuntime`, sends `AgentReady` (with its
   `SendPort`), then enters the command loop **without** running GLP.
2. **Phase 2 — Wait**: Main collects all `AgentReady` messages and registers
   every agent's port in `IsolateRouter`.
3. **Phase 3 — Start**: Main sends `StartAgent` to each isolate.  The isolate
   receives it, runs `agent.initialize()` (GLP initialization), then continues
   the command loop for `DeliverMad` / `UserInput` / `DisposeAgent`.

This eliminates race conditions: all ports are registered before any GLP code
sends network messages, so no message can be dropped due to a missing target.

## GLP source files

Each app names its own.  `main_cssg.dart` runs `programs/book/cssn`;
`main.dart` and the grassapp apps take theirs from `lib/glp_sources.dart`.  The
table that stood here listed `programs/typed_book/cssg`, a directory that has
not existed for some time.

## Known Issues

### madGLP plays stall after introduction step (OPEN)

Observed in `main_sg_mad.dart`, retired 2026-08-01; recorded here because the
mechanism it names is live and the cross-isolate path is `main_grassapp_duo`'s
as much as it was that app's.

SG Play 1 (multi-isolate) stops after the introduction step.  Cold calls and
friend messages work — Alice sends `connect(bob)`, Bob accepts, they exchange
messages, Bob connects to Charlie, Bob introduces Alice to Charlie, both
accept the introduction.  But `connected(charlie)` / `connected(alice)` never
appears — the intro channel ack/nack handshake does not propagate across
isolates.

**What works**: cold call (`connect`), friend acceptance (`decision`),
messaging (`send`/`received`), introduction offer (`befriend_intro`),
introduction acceptance (`accept_intro`).

**What stalls**: the `connected(...)` notification after both sides accept
an introduction, and everything that follows (cross-intro messaging).

**Hypothesis**: The intro channel ack/nack involves cross-heap variable
propagation via madGLP's `globalize`/`localize` mechanism.  Something in how
the multi-isolate Flutter app drives this propagation differs from the headless
multi-isolate tests that work.  The next debugging step is to compare the
Flutter app's isolate protocol with the existing working headless multi-isolate
tests in `glp_runtime/test/multiagent/`.

**Key test files for comparison** (these test the same scenarios without Flutter):
- `glp_runtime/test/multiagent/mad_scenarios_test.dart`
- `glp_runtime/test/multiagent/mad_cold_call_isolate_test.dart`
- `glp_runtime/test/multiagent/isolate_manager_test.dart`
- `glp_runtime/test/multiagent/multiagent_glp_test.dart`
- `glp_runtime/lib/multiagent/archive-irma-2026-01-30/tests/isolate_friend_introduction_test.dart` (archived)
- `glp_runtime/lib/multiagent/archive-irma-2026-01-30/tests/isolate_play_alice_bob_charlie_test.dart` (archived)
