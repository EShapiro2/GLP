# CSSG Code Status Report

**Date: 2026-02-20**

## Current state

The ack/nack friend introduction protocol is implemented and working.
All files typecheck and load successfully in the REPL.
All plays run correctly: silent (play1–3 with sink) and Flutter (fplay1–3 with tagged output).
Dart/Flutter integration (steps 6–7 in DESIGN.md) is not yet implemented.

## Files

| File | Status | Description |
|------|--------|-------------|
| `typed_social_agent.glp` | Typechecks ✅, runs ✅ | Agent with ack/nack intro via channel passing |
| `typed_ui_mediator.glp` | Typechecks ✅, runs ✅ | Mediator storing channel(Ch) in pending for intro |
| `typed_ui_actors.glp` | Typechecks ✅, runs ✅ | Three test scripts: play1/play2/play3 |
| `play_ui_dglp_boot.glp` | Typechecks ✅, runs ✅ | Wiring: network3 + agents + mediators + actors |
| `play_ui_sim_boot.glp` | Typechecks ✅, runs ✅ | Simulated UI boot: tee/sink + tee/tagged plays |
| `play_dglp_boot.glp` | Unchanged | Original dGLP entry point (no mediator, no intro) |

## Architecture

```
actor ↔ ui_mediator ↔ agent ↔ network3
         (ground)      (non-ground)
```

- **actor**: ground-term test scripts (alice1, bob1, charlie1, etc.)
- **ui_mediator**: translates between ground UI terms and non-ground agent terms; stores channels and response writers in pending list keyed by request IDs
- **agent**: full SG agent with cold-call befriend, friend messaging, friend-mediated introduction with ack/nack
- **network3**: 3-way message router (cold-call 2-arg and friend-to-friend 3-arg)

## Test plays

| Play | Scenario | Result |
|------|----------|--------|
| play1 | Both accept intro | Alice and Charlie become friends, exchange messages |
| play2 | Alice accepts, Charlie rejects | Alice gets rejected(charlie); Charlie done immediately |
| play3 | Both reject intro | Both done immediately after sending nack |

## Next task: Flutter integration via REPL subprocess

**Goal:** Show fplay1–3 in the Flutter UI with per-agent read-only transcript windows.

**Approach:** The existing `glp_multiagent` coordinator manages per-agent panels and routes `AgentOutput` messages. For simulated plays, we reuse the same coordinator — the only difference is that instead of spawning one isolate per agent (madGLP), we spawn the REPL as a single subprocess.

**Steps:**
1. Add play buttons ("Play 1", "Play 2", "Play 3") to the coordinator control bar.
2. On click: create read-only `AgentState` entries for alice/bob/charlie.
3. Spawn REPL as subprocess: `dart run bin/glp_repl.dart` (working dir: `glp_runtime/`).
4. Pipe stdin: load 4 cssg files, then `fplayN.`, then `:quit`.
5. Capture stdout: parse `tagged(ID, cmd/notify(...))` lines, send as `AgentOutput(id, content)` to coordinator.
6. Coordinator routes to agent panels (existing code, no changes).

**Display:** `cmd(...)` → shown as user input. `notify(...)` → shown as agent output.

**Key rule:** GLP runs through the REPL subprocess. No GlpEngine/AgentRuntime API calls for simulated plays.

**No GLP changes needed.** `fplay1`–`fplay3` in `play_ui_sim_boot.glp` already produce the correct tagged output.

## Future

1. CSSG extension: four-party consent for parent-mediated child befriending
