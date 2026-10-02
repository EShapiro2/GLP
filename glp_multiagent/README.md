# GLP Multiagent Flutter Apps

**Updated: 2026-08-01**

Flutter desktop apps for simulating GLP multiagent systems.  Each app names its
own GLP program and uses its own execution backend and agent topology.

## Apps (entry points)

| Entry point | App | Agents | Backend | Status |
|---|---|---|---|---|
| `lib/main.dart` | Interactive SG | Alice, Bob, Charlie | Multi-window (`desktop_multi_window`) | Working |

`main_sg_mad.dart`, `main_cssg_mad.dart` and `main_cssg_mad_modules.dart` were
retired on 2026-08-01: each named a program directory that no longer exists
(`programs/typed_book/cssg`, `programs/social/child_safe`), and the CSSG
programs they ran are removed with `programs/book/cssg` (Udi, 2026-08-01).  The
table above lists only the entry points this file documents; `lib/` holds
others.

### Interactive SG (`main.dart`)

The person acts through the inbox cards and compose forms of the agent's
screen, and each act (`connect`, `decision`, `send`, `introduce`,
`accept_intro`, ...) reaches the agent as a ground term
(`lib/isolate_protocol.dart`, `UserInput`), never as typed text: the agent
runtime parses nothing the person sends (2026-10-02).

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

Each app names its own.  `main.dart` and the grassapp apps take theirs from
`lib/glp_sources.dart`.  The apps that ran plays by spawning a `glp_repl`
subprocess --- `main_cssg.dart`, retired on 2026-08-02, and
`main_cssn_village.dart`, deleted on 2026-10-02 with the `ReplPlayRunner` it
ran on --- are in git.

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
