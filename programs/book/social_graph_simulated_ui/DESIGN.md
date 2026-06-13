# Social Graph — Simulated UI Design Notes (Frozen Snapshot)

**This is a frozen snapshot.** Active development continues in `../cssg/`.

## Ack/Nack Friend Introduction Protocol

### Design (implemented, working)

Follows the original SG design: the intro channel is passed to the user via the mediator and returned with accept/reject. On accept, the agent sends ack on the channel and awaits the peer's ack/nack. On reject, the agent sends nack and is done.

### Protocol flow

1. **Introducer r** creates channel pair `(PQCh, QPCh)`, sends `intro(Q, QPCh)` to P and `intro(P, PQCh)` to Q via friend channels. Fire and forget.

2. **Receiving agent P** gets `intro(Other, Ch)` on NetIn:
   - Sends `befriend_intro(From, Other, Ch)` to user via mediator
   - Agent continues immediately (no waiting, no concurrent process yet)

3. **Mediator** receives `befriend_intro(From, Other, Ch)`:
   - Stores `channel(Ch)` in pending list keyed by `req(N)`
   - Sends ground `befriend_intro(From, Other, req(N))` to user

4. **User accepts** — sends `accept_intro(Other, req(N))`:
   - Mediator retrieves `channel(Ch)` from pending, forwards to agent as `accept_intro(Other, channel(Ch))`
   - Agent sends `ack(Id)` on channel (via `send` defined guard in clause guard)
   - Agent spawns `intro_await_peer(Other, Ch1, Result)` concurrently
   - Agent spawns `inject_intro_result(Result, UserIn, UserIn1)` to inject result when ready
   - Agent continues with `UserIn1`

5. **User rejects** — sends `reject_intro(Other, req(N))`:
   - Mediator retrieves `channel(Ch)` from pending, forwards to agent as `reject_intro(Other, channel(Ch))`
   - Agent binds channel output to `[nack]` in the clause head (closes channel)
   - Agent continues. No waiting for peer. No notification to user (user already knows).

6. **intro_await_peer** reads peer's first message on the channel:
   - If `ack(PeerId)`: writes `intro_result(Other, ch(ChIn, ChOut))` — live channel for friend communication
   - If `nack`: closes our output (`[]`), writes `intro_rejected(Other)`

7. **inject_intro_result** waits for the result, then prepends it to UserIn (same design as `inject_msg`).

8. **Agent** processes injected messages on UserIn:
   - `intro_result(Other, Ch)`: adds friend output, merges channel input into NetIn, notifies user `connected(Other)`
   - `intro_rejected(Other)`: notifies user `rejected(Other)`

### Key design choices

- Each side responds independently. No deadlock: accepting does not wait for the peer's decision before continuing.
- Only if both ack does the friend get added. If either nacks, the accepting side learns via `intro_rejected`.
- The rejecting side is done immediately — no need to read the peer's response.

### Files

- `typed_social_agent.glp`: agent with ack/nack intro, intro_await_peer, inject_intro_result
- `typed_ui_mediator.glp`: mediator storing channel(Ch) in pending for intro
- `typed_ui_actors.glp`: three test scripts (play1/play2/play3)
- `play_ui_dglp_boot.glp`: wiring goals for play1/play2/play3

### Types

- `PendingValue ::= response(Response?) ; channel(Channel?) ; error.`
- `UserContent` includes `accept_intro(Constant, PendingValue)` and `reject_intro(Constant, PendingValue)`
- `AgentContent` includes `befriend_intro(Constant, Constant, Channel)`
- `IntroResult ::= intro_result(Constant, Channel) ; intro_rejected(Constant).`

### Test plays

| Play | Scenario | Result |
|------|----------|--------|
| play1 | Both accept | Alice and Charlie become friends, exchange messages |
| play2 | Alice accepts, Charlie rejects | Alice gets `rejected(charlie)` via ack/nack; Charlie done immediately |
| play3 | Both reject | Both done immediately after sending nack |

All three plays typecheck and run correctly (suspend with idle infrastructure, not deadlock).

---

## Simulated UI: Single-Isolate Actor-Driven Plays with Per-Agent Windows

### Goal

Run actor-driven plays in a single isolate (dGLP mode) with each agent getting its own read-only transcript window in the Flutter UI. The window shows what the simulated user "types" (actor commands) and what the agent responds (mediator notifications). No interactive user input — actors drive everything. Interactive input is only available in multi-isolate (madGLP) mode.

### Existing architecture (Flutter multiagent app)

The existing `glp_multiagent` Flutter app runs each agent in a separate Dart isolate:

```
Flutter main isolate (UI)
  ├─ Agent isolate (alice)
  │   └─ GlpEngine runs: agent_init(alice, UserIn, NetIn)
  │       ├─ agent/4
  │       ├─ ui_mediator/5
  │       └─ send_to_user/1 (kernel predicate → Dart callback)
  ├─ Agent isolate (bob)
  └─ Agent isolate (charlie)
```

Each agent isolate has:
- `InputInjector` for user input: Dart parses user text, injects ground terms into GLP heap as a stream
- `InputInjector` for network input: MAD messages injected similarly
- `send_to_user/1` kernel predicate: GLP writes ground terms → Dart callback → Flutter UI

### Design

All agents run in a single GLP engine (dGLP mode), sharing one heap and scheduler. The network uses `network3` which connects streams directly (no serialization). Actors drive the protocol — no keyboard input. Each agent gets a read-only transcript window.

The actor's output is tee'd to both the mediator and a display stream. The mediator's output is tee'd to both the actor and a second display stream. Both display streams go through `send_to_user_tagged(Id, Stream)` which tags each term with the agent ID before calling `_output/1`:

```prolog
%% Per-agent wiring (inside boot goal):
actor(ch(ActorIn?, ActorOut)),
tee(ActorOut?, MediatorIn, DisplayCmd),
agent(Id?, AgentIn?, NetIn?,
      [output('_user', AgentToUser),
       output('_net', NetOut)]),
ui_mediator(Id?,
    ch(AgentToUser?, AgentIn),
    ch(MediatorIn?, MediatorOut),
    [], 1),
tee(MediatorOut?, ActorIn, DisplayNotify),
send_to_user_tagged(Id?, DisplayCmd?, DisplayNotify?)
```

The `tee` process copies each element from an input stream to two output streams:
```prolog
procedure tee(Stream?, Stream, Stream).
tee([X|Xs], [X?|Ys?], [X?|Zs?]) :- ground(X?) | tee(Xs?, Ys, Zs).
tee([], [], []).
```

The `send_to_user_tagged` process merges both display streams and tags output with the agent ID:
```prolog
procedure send_to_user_tagged(Constant?, Stream?, Stream?).
send_to_user_tagged(Id, [T|Cmds], Notifies) :-
    ground(T?) |
    '_output'(tagged(Id?, cmd(T?))),
    send_to_user_tagged(Id?, Cmds?, Notifies?).
send_to_user_tagged(Id, Cmds, [T|Notifies]) :-
    ground(T?) |
    '_output'(tagged(Id?, notify(T?))),
    send_to_user_tagged(Id?, Cmds?, Notifies?).
send_to_user_tagged(_, [], []).
send_to_user_tagged(_, Cmds, []) :- sink(Cmds?).
```

For pure GLP testing (no Flutter), replace `send_to_user_tagged` with `sink`:
```prolog
procedure sink(Stream?).
sink([_|Xs]) :- sink(Xs?).
sink([]).
```

The Dart `outputCallback` receives strings like `tagged(alice, cmd(connect(bob)))` or `tagged(bob, notify(connected(alice)))` and routes to the appropriate agent panel, distinguishing commands (shown as user input) from notifications (shown as agent output).

### Implementation

GLP-side:
1. `tee/3` — stream splitter
2. `sink/1` — dummy consumer for pure GLP testing
3. `send_to_user_tagged/3` — merges display streams, tags with agent ID
4. Simulated boot file (`play_ui_sim_boot.glp`) — wires actor + tee + mediator + sink
5. Pure GLP test — run plays with sink, verify everything still works

Dart/Flutter-side:
6. Parse `tagged(id, cmd(...))` / `tagged(id, notify(...))` in `outputCallback` — route to per-agent panel
7. Per-agent read-only panels — show commands and notifications as transcript

### Execution rule

All GLP code runs through the REPL. The REPL is the only supported way to compile, typecheck, and execute GLP programs (see `/Grassroots/GLP/CLAUDE.md`). The Dart/Flutter side must use the REPL infrastructure — it must not call `GlpEngine.runGoal` directly to execute play goals. The existing multi-isolate architecture already follows this rule: each agent isolate runs a REPL-based engine. The simulated UI must do the same.

### Current state (complete, 2026-02-20)

All seven steps implemented and working.

GLP-side (steps 1–5): `play_ui_sim_boot.glp` contains `tee/3`, `sink/1`, `send_to_user_tagged/3`, silent plays (`play1`–`play3` with sink), and Flutter plays (`fplay1`–`fplay3` with tagged output). The `tee/3` splitter requires a `ground(X?)` guard because X is read into two output streams (SRSW constraint). `send_to_user_tagged/3` is untyped because `_output/1` is a kernel predicate with no procedure declaration. All plays verified in the REPL.

Dart/Flutter-side (steps 6–7): `ReplPlayRunner` (`glp_runtime/lib/multiagent/repl_play_runner.dart`) encapsulates REPL subprocess management. `main.dart` wires play buttons to `ReplPlayRunner` and routes parsed output to read-only agent panels. All three plays verified in the Flutter UI.

### Dart/Flutter integration (implemented)

**Approach:** The REPL runs as a subprocess. `ReplPlayRunner` spawns it, pipes load commands + `fplayN.` + `:quit` to stdin, parses `tagged(id, cmd/notify(...))` lines from stdout, and delivers them via callbacks. `main.dart` creates read-only panels and routes output — no GlpEngine or AgentRuntime calls for simulated plays.

**Key rule:** GLP runs through the REPL subprocess. The Flutter app does not call GlpEngine, AgentRuntime, or any GLP API directly for simulated plays.

