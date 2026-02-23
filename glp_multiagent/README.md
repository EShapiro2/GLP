# GLP Multiagent Flutter Apps

**Updated: 2026-02-21**

Flutter desktop apps for simulating GLP multiagent systems.  Each app runs
agents with the same GLP code (`typed_social_agent.glp`, `typed_ui_mediator.glp`,
`typed_ui_actors.glp`) but uses a different execution backend and agent topology.

## Apps (entry points)

| Entry point | App | Agents | Backend | Status |
|---|---|---|---|---|
| `lib/main.dart` | Interactive SG | Alice, Bob, Charlie | Multi-window (`desktop_multi_window`) | Working |
| `lib/main_cssg.dart` | CSSG Plays (REPL) | Alice, Bob, Carol, Dave | REPL subprocess | Working |
| `lib/main_sg_mad.dart` | SG Plays (madGLP) | Alice, Bob, Charlie | Multi-isolate (`AgentRuntime` + `IsolateRouter`) | **Partial** — see Known Issues |
| `lib/main_cssg_mad.dart` | CSSG Plays (madGLP) | Alice, Bob, Carol, Dave | Multi-isolate (`AgentRuntime` + `IsolateRouter`) | **Partial** — see Known Issues |

### Interactive SG (`main.dart`)

Multi-window app.  Coordinator spawns one OS window per agent.  User types
commands (`connect`, `decision`, `send`, `introduce`, `accept_intro`) in each
agent's text field.  Uses `desktop_multi_window` plugin and `MadRouter` for
cross-window message routing.  Full 10-step introduction protocol verified
working.

### CSSG Plays — REPL (`main_cssg.dart`)

Single-window app with four read-only panels (Alice, Bob, Carol, Dave).
Runs plays 4–7 by spawning a `glp_repl` subprocess.  Tagged output is parsed
and routed to the correct panel.  Uses `ReplPlayRunner` from `glp_runtime`.

### SG Plays — madGLP (`main_sg_mad.dart`)

Single-window app with three read-only panels (Alice, Bob, Charlie).
Runs plays 1–3.  Each agent runs in its own Dart isolate via `AgentRuntime`.
Uses `IsolateRouter` for cross-isolate MAD message routing and the two-phase
deferred-start protocol (see below).

### CSSG Plays — madGLP (`main_cssg_mad.dart`)

Single-window app with four read-only panels (Alice, Carol, Bob, Dave).
Runs plays 4–7.  Same multi-isolate architecture as SG madGLP.  Parent–child
channels are bootstrapped via `parent_connect` cold call over madGLP.

## Shared infrastructure

| File | Purpose |
|---|---|
| `lib/isolate_protocol.dart` | Message types between main isolate and agent isolates, agent isolate entry point, lifecycle documentation |
| `lib/mad_router.dart` | `IsolateRouter` — routes MAD messages between agent isolates via `SendPort`, with message buffering for unregistered agents |

## Two-phase deferred-start protocol

Used by both madGLP apps (`main_sg_mad.dart`, `main_cssg_mad.dart`).
Documented in `isolate_protocol.dart`.

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

All GLP source files are in `/Users/udi/Grassroots/GLP/programs/typed_book/cssg/`.

| File | Purpose |
|---|---|
| `typed_social_agent.glp` | `agent/4`, channel ops, merge, response handling |
| `typed_ui_mediator.glp` | Ground-term mediator (`agent/4` ↔ Dart UI) |
| `typed_ui_actors.glp` | Scripted UI actors — talk to `ui_mediator` (ground terms) |
| `play_ui_boot.glp` | Interactive Flutter UI boot: `agent_init/3` |
| `play_ui_madglp_boot.glp` | madGLP boot with mediator + actors (multi-isolate Flutter UI) |
| `play_ui_dglp_boot.glp` | dGLP boot with mediator (single-isolate REPL) |
| `play_dglp_boot.glp` | dGLP boot without mediator (single-isolate REPL) |
| `play_madglp_boot.glp` | madGLP boot without mediator (headless multi-isolate) |

## Known Issues

### madGLP plays stall after introduction step (OPEN)

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
