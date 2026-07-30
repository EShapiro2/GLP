# CSSG Code Design Notes

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

- `typed_social_agent.glp`: agent with ack/nack intro + CSSG, intro_await_peer, inject_intro_result
- `typed_ui_mediator.glp`: mediator storing channel(Ch) in pending for intro + CSSG
- `typed_ui_actors.glp`: seven test scripts (play1–7)
- `play_ui_sim_boot.glp`: boot goals with tee/sink (silent) and tee/tagged (Flutter) for all plays

### Types

- `PendingValue ::= response(Response?) ; channel(IntroChannel) ; error.`
- `AgentContent ::= befriend(Constant, Response?) ; ...` (Response? makes the field output/produce)
- `NetColdCall ::= intro(Constant, Response?).` (same — Response? for output position)
- `UserContent` includes `accept_intro(Constant, PendingValue)` and `reject_intro(Constant, PendingValue)`
- `AgentContent` includes `befriend_intro(Constant, Constant, IntroChannel)`
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
      [output(person, AgentToUser),
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

GLP-side (steps 1–5): `play_ui_sim_boot.glp` contains `tee/3`, `sink/1`, `send_to_user_tagged/3`, silent plays (`play1`–`play7` with sink), and Flutter plays (`fplay1`–`fplay7` with tagged output). The `tee/3` splitter requires a `ground(X?)` guard because X is read into two output streams (SRSW constraint). `send_to_user_tagged/3` is untyped because `_output/1` is a kernel predicate with no procedure declaration. All plays verified in the REPL.

Dart/Flutter-side (steps 6–7): `ReplPlayRunner` (`glp_runtime/lib/multiagent/repl_play_runner.dart`) encapsulates REPL subprocess management. Two separate Flutter apps (`main.dart` for SG, `main_cssg.dart` for CSSG) wire play buttons to `ReplPlayRunner` and route parsed output to read-only agent panels. All seven plays verified in the Flutter UI.

### Dart/Flutter integration (implemented)

**Approach:** The REPL runs as a subprocess. `ReplPlayRunner` spawns it, pipes load commands + `fplayN.` + `:quit` to stdin, parses `tagged(id, cmd/notify(...))` lines from stdout, and delivers them via callbacks. `main.dart` creates read-only panels and routes output — no GlpEngine or AgentRuntime calls for simulated plays.

**Key rule:** GLP runs through the REPL subprocess. The Flutter app does not call GlpEngine, AgentRuntime, or any GLP API directly for simulated plays.

---

## CSSG Extension: Child-Safe Social Graph

### Overview

Child befriend extends the ack/nack friend-mediated introduction protocol with four-party consent. Two parents (p1, p2) and two children (c1, c2) must all consent for a c1-c2 friendship channel to become live. The protocol reuses the existing ack/nack handshake (intro_await_peer, inject_intro_result) for the final c1-c2 channel establishment.

Unfriending is deferred (as in the existing SG implementation).

### Agents and names

Agent names are simple atoms: parents are `alice`, `bob`; children are `carol`, `dave`. There are no derived/structured names.

### Parent-child relationship

The parent-child relationship is established at boot time, not through the social protocol. Each parent-child pair gets a bidirectional channel:

- Parent has `output(child(carol), ChildOut)` in its output list (sends to child via `lookup_send(child(carol), ...)`)
- Child has `output(child(alice), ParentOut)` in its output list (sends to parent via `lookup_send(child(alice), ...)`)
- Parent's input from child is merged into parent's NetIn
- Child's input from parent is merged into child's NetIn

Output list keys are structured: `person`, `'_net'`, `friend(Name)`, `child(Name)`. This distinguishes friends from children, allowing the agent to find its child output without knowing the child's name in advance. The `lookup_send` key type is `OutputKey` (see type definitions).

Children have the same agent/4 structure as adults. The only difference is boot wiring: children have no network connection. Their only external communication is via the parent-child channel and (eventually) friendship channels established through this protocol.

### Network

Only parents (alice, bob) are on the network. A 2-way network suffices. Children (carol, dave) are not routable through the network — they communicate only via parent-child channels and direct friendship channels.

### User command

`child_introduce(carol, bob, dave)` — alice tells her agent: "introduce my child carol to dave (bob's child)". The knowledge of who is whose child resides in the user/actor, not in the agent. The agent needs the child's name to know which output to send on.

Alice's consent is implicit in initiating the command.

### Protocol flow

Participants: alice (p1), bob (p2), carol (c1, alice's child), dave (c2, bob's child).

Preconditions: alice and bob are friends. alice has a parent-child channel with carol. bob has a parent-child channel with dave.

**Step 1 — alice initiates.** Alice's user sends `child_introduce(carol, bob, dave)`. Alice's agent:
- Creates channel pair `(CarolCh, DaveCh)` using `new_channel`
- Sends `child_intro(dave, CarolCh)` to carol via parent-child channel (`lookup_send(carol, ...)`)
- Sends `child_intro(carol, DaveCh)` to bob via friend channel (`lookup_send(bob, ...)`)
- Alice is done (fire and forget). Alice's consent is implicit in initiating.

**Step 2 — carol receives proposal.** Carol's agent receives `child_intro(dave, Ch)` on NetIn (from parent). Carol's agent notifies carol's user via mediator: `child_befriend(alice, dave, Ch)`. Carol's user responds:
- Accept: carol sends `ack(carol)` on the channel, spawns `intro_await_peer(dave, Ch1, Result)`, injects result into UserIn. (Same mechanism as existing SG intro accept.)
- Reject: carol sends `nack` on the channel. Done.

**Step 3 — bob receives proposal.** Bob's agent receives `child_intro(carol, Ch)` on the friend channel from alice. This is a consent gate for bob. Bob's agent notifies bob's user via mediator: `child_befriend(alice, carol, Ch)` (same notification type as children receive). Bob's user responds:
- Approve: bob's agent forwards `child_intro(carol, Ch)` to dave via parent-child channel (`lookup_send(child(dave), ...)`). Bob is done (fire and forget after forwarding).
- Reject: bob sends `nack` on Ch. Bob does NOT forward to dave. Done.

**Step 4 — dave receives proposal.** Dave's agent receives `child_intro(carol, Ch)` on NetIn (from parent, forwarded by bob). Same as carol in Step 2: dave's user decides, ack or nack on the channel.

**Step 5 — channel resolution.** The ack/nack handshake between carol and dave proceeds exactly as in SG friend introduction:
- Both ack → `intro_await_peer` on each side reads the peer's ack, writes `intro_result(Other, Ch)`. The friendship channel becomes live. Each child adds the friend output, merges friend input into NetIn, notifies user `connected(Other)`.
- One acks, other nacks (or bob nacked) → the acking side gets `intro_rejected(Other)` via `intro_await_peer`.
- Both nack → both sides are done immediately.

### What is reused from SG

- `intro_await_peer/3` — reused exactly as-is for the carol-dave handshake
- `inject_intro_result/3` — reused exactly as-is
- agent/4 clauses for `intro_result` and `intro_rejected` — reused by carol and dave
- The ack/nack mechanism on the channel (first message is `ack(Id)` or `nack`)
- Mediator channel escrow (storing channel in pending, retrieving on accept/reject)

### New agent/4 clauses (implemented)

1. **Parent initiates child introduction.** Handles `child_introduce(MyChild, Friend, FriendChild)` from UserIn. Creates channel pair, sends `child_intro(FriendChild, C1Ch)` to own child and `child_intro(MyChild, C2Ch)` to friend. Fire and forget.

2. **Child receives child_intro from parent.** Handles `child_intro(Other, Ch)` on NetIn. Notifies user via mediator: `child_befriend(From, Other, Ch)`. On accept: ack + intro_await_peer + inject_intro_result (same as SG intro accept). On reject: nack on channel.

3. **Parent receives child_intro on friend channel (consent gate).** Handles `child_intro(OtherChild, Ch)` on NetIn from friend. Notifies user via mediator: `child_befriend(From, OtherChild, Ch)`. User may accept, reject, or approve:
   - **Reject:** agent sends nack on Ch. Does NOT forward to own child. Done.
   - **Approve:** agent forwards `child_intro(OtherChild, Ch)` to own child via `lookup_send(child(MyChild), ...)`.

4. **Parent approves child_intro.** Handles `approve_child_intro(OtherChild, MyChild, channel(Ch))` from UserIn. Forwards `child_intro(OtherChild, Ch)` to own child.

### New mediator clauses (implemented)

1. **child_befriend(From, Other, Ch)** — single agent → user notification used for both children and parents receiving `child_intro`. The channel is stored in pending. User sees ground `child_befriend(From, Other, ReqId)`. User responds with one of:
   - `accept_child_intro(Other, ReqId)` — child accepts the proposal
   - `reject_child_intro(Other, ReqId)` — child or parent rejects
   - `approve_child_intro(OtherChild, MyChild, ReqId)` — parent approves and forwards to own child

2. **accept_child_intro** — mediator retrieves `channel(Ch)` from pending, forwards to agent as `accept_child_intro(Other, channel(Ch))`.

3. **reject_child_intro** — mediator retrieves `channel(Ch)` from pending, forwards to agent as `reject_child_intro(Other, channel(Ch))`.

4. **approve_child_intro** — mediator retrieves `channel(Ch)` from pending, forwards to agent as `approve_child_intro(OtherChild, MyChild, channel(Ch))`.

### New type definitions (implemented)

```
%% User commands
UserContent += child_introduce(Constant, Constant, Constant).

UserContent += accept_child_intro(Constant, PendingValue)
             ; reject_child_intro(Constant, PendingValue)
             ; approve_child_intro(Constant, Constant, PendingValue).

%% Agent → user notifications
AgentContent += child_befriend(Constant, Constant, Channel).

%% Friend-to-friend and parent-child channel content
FriendContent += child_intro(Constant, Channel).

%% Output keys (structured)
OutputKey ::= person ; '_net' ; friend(Constant) ; child(Constant).
```

### Boot configuration (single isolate, 4 agents)

```
%% Schematic — not actual GLP syntax

%% Network: only parents
network2(ch(AliceNetOut?, AliceNetIn),
         ch(BobNetOut?, BobNetIn)),

%% Parent-child channels
%% alice ↔ carol
%% bob ↔ dave

%% Alice: network + child channel
agent(alice, AliceAgentIn?, AliceNetAndChildIn?,
      [output(person, AliceToUser),
       output('_net', AliceNetOut),
       output(child(carol), AliceToCarol)]),
merge(AliceNetIn?, AliceFromCarol?, AliceNetAndChildIn),

%% Carol: parent channel only, no network
agent(carol, CarolAgentIn?, CarolFromAlice?,
      [output(person, CarolToUser),
       output(child(alice), CarolToAlice)]),

%% Bob: network + child channel
agent(bob, BobAgentIn?, BobNetAndChildIn?,
      [output(person, BobToUser),
       output('_net', BobNetOut),
       output(child(dave), BobToDave)]),
merge(BobNetIn?, BobFromDave?, BobNetAndChildIn),

%% Dave: parent channel only, no network
agent(dave, DaveAgentIn?, DaveFromBob?,
      [output(person, DaveToUser),
       output(child(bob), DaveToBob)]),

%% Wire parent-child channels
%% alice → carol: AliceToCarol? feeds CarolFromAlice
%% carol → alice: CarolToAlice? feeds AliceFromCarol
%% bob → dave: BobToDave? feeds DaveFromBob
%% dave → bob: DaveToBob? feeds BobFromDave
```

### Test plays

| Play | Scenario | Expected result |
|------|----------|-----------------|
| play4 | All four accept | carol and dave become friends, exchange messages |
| play5 | bob (p2) rejects | carol gets intro_rejected(dave); dave never learns of proposal |
| play6 | carol (c1) rejects | dave gets intro_rejected(carol) (if dave already acked); carol done immediately |
| play7 | dave (c2) rejects | carol gets intro_rejected(dave); dave done immediately |

Common setup for all plays: alice and bob become friends first (cold call, same as existing plays). Then alice initiates `child_introduce(carol, bob, dave)`.

### Implementation (complete, 2026-02-20)

All CSSG code implemented, typechecked, and tested. Both silent plays (play4–7 with sink) and Flutter plays (fplay4–7 with tagged output) run correctly in the REPL and Flutter UI.

**Step 3 simplification:** During implementation, the separate `child_befriend_request` notification for parents was eliminated. Both children and parents receive the same `child_befriend(From, Other, Ch)` notification. The parent's user distinguishes consent approval (forward to child) from direct acceptance by choosing `approve_child_intro` rather than `accept_child_intro`. This keeps the protocol simpler and the agent/mediator code uniform.

### Files

| File | Description |
|------|-------------|
| `typed_social_agent.glp` | Agent with SG + CSSG clauses, all types |
| `typed_ui_mediator.glp` | Mediator with SG + CSSG clauses |
| `typed_ui_actors.glp` | Actor scripts for play1–7 |
| `play_ui_sim_boot.glp` | Boot goals: network, agents, mediators, actors, tee, sink/tagged |

---

## Separate Flutter Apps for SG and CSSG

The Flutter multiagent app (`glp_multiagent`) has two separate entry points:

- **`lib/main.dart`** — Social Graph app. Green Play 1/2/3 buttons. Three agent panels (Alice, Bob, Charlie).
- **`lib/main_cssg.dart`** — Child-Safe Social Graph app. Blue Play 4/5/6/7 buttons. Four agent panels (Alice, Carol, Bob, Dave) with parent-child grouping and color families:
  - Indigo family: Alice (parent, dark), Carol (child, light)
  - Teal family: Bob (parent, dark), Dave (child, light)

### How to build and run both apps

Both targets produce the same output path (`glp_multiagent.app`), so build one, copy it aside, then build the other:

```bash
cd /Users/udi/Grassroots/GLP/glp_multiagent

# 1. Build the SG app (default entry point)
flutter build macos

# 2. Copy it aside
cp -R build/macos/Build/Products/Release/glp_multiagent.app \
      build/macos/Build/Products/Release/glp_sg.app

# 3. Build the CSSG app (alternate entry point)
flutter build macos --target lib/main_cssg.dart

# 4. Copy it aside
cp -R build/macos/Build/Products/Release/glp_multiagent.app \
      build/macos/Build/Products/Release/glp_cssg.app

# 5. Launch both
open build/macos/Build/Products/Release/glp_sg.app
open build/macos/Build/Products/Release/glp_cssg.app
```

Both apps use the same `ReplPlayRunner` infrastructure from `glp_runtime`.

### How to test in the REPL (no Flutter)

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart run bin/glp_repl.dart
```

Then at the `GLP>` prompt:

```
../programs/book/cssg/typed_social_agent.glp
../programs/book/cssg/typed_ui_mediator.glp
../programs/book/cssg/typed_ui_actors.glp
../programs/book/cssg/play_ui_sim_boot.glp
play1.
play4.
fplay1.
fplay4.
:quit
```

Silent plays (`play1`–`play7`) run and terminate silently (output consumed by `sink`).
Flutter plays (`fplay1`–`fplay7`) emit `tagged(id, cmd/notify(...))` lines to stdout.
