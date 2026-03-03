# CSSG Code Status Report

**Date: 2026-02-21**

## Current state

All three precisely-typed files (agent, mediator, actors) typecheck cleanly.
All seven plays (fplay1–fplay7) run to completion in both the REPL and Flutter UI.

Mode fixes required changes to both type definitions and clause annotations:

- `PendingValue ::= response(Response?) ; channel(IntroChannel) ; error.`
- `AgentContent ::= befriend(Constant, Response?)` in mediator (was `Response`)
- `NetColdCall ::= intro(Constant, Response?)` in agent (was `Response`)
- Agent cold-call clause: `Resp?` (reader) in receive head, `Resp` (writer) in
  `befriend(From?, Resp)` sent to mediator via `lookup_send`
- Mediator befriend clause: `Resp?` (reader) in receive, `Resp` (writer) in
  `response(Resp)` stored in pending list

The `Response?` mode annotation in the type definitions makes the Response field
an output (produce) position, allowing a writer to be placed there. Without `?`,
the field is input (consume) position, which conflicts with SRSW requirements.

## Files

| File | Status | Description |
|------|--------|-------------|
| `typed_social_agent.glp` | Typechecks ✅ | Agent with SG intro + CSSG four-party consent |
| `typed_ui_mediator.glp` | Typechecks ✅ | Mediator with SG + CSSG clauses (mode errors fixed) |
| `typed_ui_actors.glp` | Typechecks ✅ | Seven test scripts: play1–7 |
| `play_ui_sim_boot.glp` | Typechecks ✅ | Boot goals: network, tee/sink, tee/tagged |

## Architecture

```
actor ↔ ui_mediator ↔ agent ↔ network
         (ground)      (non-ground)
```

- **actor**: ground-term test scripts (alice1–7, bob1–7, charlie1–3, carol4–7, dave4–7)
- **ui_mediator**: translates between ground UI terms and non-ground agent terms; stores channels and response writers in pending list keyed by request IDs
- **agent**: full SG + CSSG agent with cold-call befriend, friend messaging, friend-mediated introduction with ack/nack, parent-initiated child introduction with four-party consent
- **network3**: 3-way message router for SG plays (alice, bob, charlie)
- **network2**: 2-way message router for CSSG plays (alice, bob — only parents on network)

## Test plays

### SG plays (3 agents: Alice, Bob, Charlie)

| Play | Scenario | Result |
|------|----------|--------|
| play1 | Both accept intro | Alice and Charlie become friends, exchange messages |
| play2 | Alice accepts, Charlie rejects | Alice gets rejected(charlie); Charlie done immediately |
| play3 | Both reject intro | Both done immediately after sending nack |

### CSSG plays (4 agents: Alice, Bob, Carol, Dave)

| Play | Scenario | Result |
|------|----------|--------|
| play4 | All four consent | Carol and Dave become friends, exchange messages |
| play5 | Bob (p2) rejects | Carol gets intro_rejected(dave); Dave never learns of proposal |
| play6 | Carol (c1) rejects | Dave gets intro_rejected(carol); Carol done immediately |
| play7 | Dave (c2) rejects | Carol gets intro_rejected(dave); Dave done immediately |

## Flutter integration

### Two separate apps

- **Social Graph** (`lib/main.dart`): Green Play 1/2/3 buttons. Three agent panels (Alice, Bob, Charlie).
- **Child-Safe Social Graph** (`lib/main_cssg.dart`): Blue Play 4/5/6/7 buttons. Four agent panels with parent-child grouping and color families:
  - Indigo family: Alice (parent, dark), Carol (child, light)
  - Teal family: Bob (parent, dark), Dave (child, light)

### REPL subprocess approach

GLP runs through the REPL subprocess. `ReplPlayRunner` spawns the REPL, pipes load commands + `fplayN.` + `:quit` to stdin, parses `tagged(id, cmd/notify(...))` lines from stdout, and delivers them via callbacks. No GlpEngine/AgentRuntime API calls for simulated plays.

All seven plays verified in both REPL and Flutter UI (2026-02-21).

### How to build and run

Both Flutter targets produce the same output path, so build one, copy aside, then build the other:

```bash
cd /Users/udi/Grassroots/GLP/glp_multiagent

# Build SG, copy aside, build CSSG, copy aside, launch both
flutter build macos
cp -R build/macos/Build/Products/Release/glp_multiagent.app \
      build/macos/Build/Products/Release/glp_sg.app
flutter build macos --target lib/main_cssg.dart
cp -R build/macos/Build/Products/Release/glp_multiagent.app \
      build/macos/Build/Products/Release/glp_cssg.app
open build/macos/Build/Products/Release/glp_sg.app
open build/macos/Build/Products/Release/glp_cssg.app
```

### How to test in the REPL (no Flutter)

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart run bin/glp_repl.dart
```

At the `GLP>` prompt, load the four files then run any play:

```
../programs/typed_book/cssg/typed_social_agent.glp
../programs/typed_book/cssg/typed_ui_mediator.glp
../programs/typed_book/cssg/typed_ui_actors.glp
../programs/typed_book/cssg/play_ui_sim_boot.glp
play1.
play4.
fplay1.
fplay4.
:quit
```
