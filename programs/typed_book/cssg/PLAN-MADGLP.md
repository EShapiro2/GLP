# Plan: Multi-Isolate (madGLP) Scripted Actor Plays

**Date: 2026-02-21 (revised v2)**

## Goal

Run the same scripted actor plays (plays 1-7) using true multi-isolate execution (madGLP) instead of the current single-isolate simulation (dGLP). Each play uses the same read-only Flutter UI panels, the same agent code, and the same actors. The only difference is how agents are wired at boot:

- **Single-isolate (dGLP):** All agents share one GLP engine / heap. The network (`network2`/`network3`) is stream wiring within that heap. Run via REPL subprocess.
- **Multi-isolate (madGLP):** Each isolate has its own GLP engine / heap. Network messages are serialized and routed between Dart isolates via `IsolateRouter` + `SendPort`. Run via `AgentRuntime` + `Isolate.spawn`.

The UI should clearly indicate which mode is running (read-only label, not a toggle).

## Architecture: every agent in its own isolate

Every agent gets its own isolate — both in SG and CSSG:

- **SG plays (1-3):** 3 isolates (alice, bob, charlie). All communicate via madGLP network.
- **CSSG plays (4-7):** 4 isolates (alice, bob, carol, dave). All four are on the madGLP network. Parent-child channels are established at boot time via a special cold call (see below).

### Parent-child channel bootstrap via cold call

In single-isolate mode, parent-child channels are shared-heap streams wired by the boot clause:

```
merge(AliceToCarol?, [], CarolFromAlice)
merge(CarolToAlice?, [], AliceFromCarol)
```

In multi-isolate mode, there is no shared heap. Instead, the parent-child channel is established at boot time using a cold call over the madGLP network, the same mechanism that regular `befriend` uses to establish cross-isolate channels.

**Protocol:**

1. The parent (alice) boots and sends a cold call to the child:
   `msg(carol, parent_connect(alice, ParentToChild, Response))` via `send_to_net`.
   `ParentToChild` is a fresh stream variable — Alice will write to it, Carol will read from it (via madGLP).

2. The parent immediately wires everything — `output(child(carol), ParentToChild)` in the output list, and merges `Response?`-derived child stream into agent input. The agent suspends normally on unbound readers until the child responds.

3. The child (carol) boots and intercepts the first message on `NetIn`:
   `[parent_connect(Parent, ParentToChild, Response) | NetInRest]`
   Carol binds `Response = accept(ChildToParent)` where `ChildToParent` is a fresh stream variable. Carol directly wires `ParentToChild?` into agent input merge and `output(child(alice), ChildToParent)` into output list.

4. madGLP propagates the `Response` binding back to Alice's isolate. Alice's suspended goals resume.

**Why this works:** Both parent and child know at boot time that they will be connected — the child always accepts. So both sides wire their streams immediately. The agent code uses `lookup_send(child(carol), ...)` on its output list and doesn't know or care whether the stream is local (shared heap) or cross-isolate (madGLP). Normal GLP suspension handles the timing — goals that read unbound cross-isolate variables simply suspend until madGLP delivers the bindings.

## Phased approach: SG first, then CSSG

### Phase 1: SG multi-isolate scripted plays (plays 1-3)

The SG madGLP boot already exists (`social_graph/play_ui_madglp_boot.glp`) and works for interactive use. Create a new scripted version that adds actors, tee, and `send_to_user_tagged`.

### Phase 2: CSSG multi-isolate scripted plays (plays 4-7)

Create new boot code with `parent_init` and `child_init` that establish parent-child channels via the cold call protocol above, then start agents with actors, tee, and `send_to_user_tagged`.

## Files

### Existing files: used as-is (no modifications)

| File | Role |
|------|------|
| `cssg/typed_social_agent.glp` | Agent code (SG + CSSG). Same in both modes. |
| `cssg/typed_ui_mediator.glp` | Mediator code. Same in both modes. |
| `cssg/typed_ui_actors.glp` | Actor scripts (play1-7). Same in both modes. |
| `cssg/play_ui_sim_boot.glp` | Single-isolate boot (stays for dGLP mode). |
| `social_graph/play_ui_madglp_boot.glp` | Existing SG madGLP boot (interactive, reference). |
| `glp_multiagent/lib/mad_router.dart` | `IsolateRouter` (unchanged). |
| `glp_multiagent/lib/isolate_protocol.dart` | Isolate message protocol (unchanged). |
| `glp_runtime/lib/multiagent/repl_play_runner.dart` | REPL subprocess runner (stays for dGLP mode). |

### Existing files: modified

| File | What changes |
|------|-------------|
| `glp_runtime/lib/multiagent/agent_runtime.dart` | Make the entry-point goal configurable. Currently hardcodes `agent_init/3`. Needs to support configurable goal labels and argument lists. The change is small: add `goalLabel` and `extraArgs` parameters, replacing the hardcoded goal setup. `Id` is always arg 0, extra args follow, `NetIn` is always the last arg. |

### New files: created

| File | Description |
|------|-------------|
| **GLP** | |
| `cssg/play_ui_madglp_boot.glp` | madGLP boot for scripted plays. SG boots (mboot1-3) spawn 3 isolates with `agent_init_play`. CSSG boots (mboot4-7) spawn 4 isolates — parents with `parent_init`, children with `child_init`. Includes tee, send_to_user_tagged, ui_actor dispatch, and parent-child cold call protocol. |
| **Flutter** | |
| `glp_multiagent/lib/main_sg_mad.dart` | Multi-isolate SG Flutter app. Same read-only 3-panel UI as `main.dart` (Alice, Bob, Charlie) with Play 1/2/3 buttons. Uses `AgentRuntime` + `IsolateRouter`. App bar says "Social Graph (Multi-Isolate)". |
| `glp_multiagent/lib/main_cssg_mad.dart` | Multi-isolate CSSG Flutter app. Same read-only 4-panel UI as `main_cssg.dart` (Alice, Carol, Bob, Dave) with Play 4/5/6/7 buttons. Uses `AgentRuntime` + `IsolateRouter`. Each play spawns 4 isolates. App bar says "Child-Safe Social Graph (Multi-Isolate)". |

## GLP boot design detail

### SG madGLP scripted boot (mboot1)

Same structure as the existing interactive `play_ui_madglp_boot.glp`, but with actors, tee, and `send_to_user_tagged` added:

```glp
mboot1 :-
    agent_init_play(alice, 1, _)@alice,
    agent_init_play(bob, 1, _)@bob,
    agent_init_play(charlie, 1, _)@charlie.

agent_init_play(Id, PlayNum, NetIn) :-
    ground(Id?), ground(PlayNum?) |
    send_to_net(NetOut?),
    agent(Id?, AgentIn?, NetIn?,
          [output('_user', AgentToUser), output('_net', NetOut)]),
    ui_mediator(Id?, ch(AgentToUser?, AgentIn),
                ch(MedIn?, MedOut), [], 1),
    ui_actor(Id?, PlayNum?, ch(ActorIn?, ActorOut)),
    tee(ActorOut?, MedIn, DispCmd),
    tee(MedOut?, ActorIn, DispNotify),
    send_to_user_tagged(Id?, DispCmd?, DispNotify?).
```

The `ui_actor` dispatch selects the correct actor for each (Id, PlayNum) pair:

```glp
ui_actor(alice, 1, Ch) :- alice1(Ch?).
ui_actor(bob, 1, Ch) :- bob1(Ch?).
ui_actor(charlie, 1, Ch) :- charlie1(Ch?).
ui_actor(alice, 2, Ch) :- alice2(Ch?).
%% ... etc for all (agent, play) pairs
```

### CSSG madGLP scripted boot (mboot4)

Four isolates. Parents send a `parent_connect` cold call to their child at boot time and wire everything immediately. Children intercept the cold call as the first message on `NetIn` and wire everything immediately.

```glp
mboot4 :-
    parent_init(alice, carol, 4, _)@alice,
    child_init(carol, 4, _)@carol,
    parent_init(bob, dave, 4, _)@bob,
    child_init(dave, 4, _)@dave.
```

**Parent boot:**

```glp
parent_init(Id, Child, PlayNum, NetIn) :-
    ground(Id?), ground(Child?), ground(PlayNum?) |
    send_to_net(NetOut?),

    %% Send parent_connect cold call to child
    %% ParentToChild: parent writes, child reads (via madGLP)
    %% Response: child will bind to accept(ChildToParent)
    lookup_send('_net', msg(Child?, parent_connect(Id?, ParentToChild, Response)),
                [output('_net', NetOut)], _),

    %% Wire immediately — agent suspends on unbound readers until child responds
    merge(ChildToParent?, NetIn?, AgentNetIn),

    agent(Id?, AgentIn?, AgentNetIn?,
          [output('_user', AgentToUser),
           output('_net', NetOut),
           output(child(Child?), ParentToChild)]),
    ui_mediator(Id?, ch(AgentToUser?, AgentIn),
                ch(MedIn?, MedOut), [], 1),
    ui_actor(Id?, PlayNum?, ch(ActorIn?, ActorOut)),
    tee(ActorOut?, MedIn, DispCmd),
    tee(MedOut?, ActorIn, DispNotify),
    send_to_user_tagged(Id?, DispCmd?, DispNotify?).

    %% Note: ChildToParent comes from Response? = accept(ChildToParent?)
    %% when child binds it. Until then, merge suspends on ChildToParent?.
```

**Child boot:**

```glp
child_init(Id, PlayNum, [parent_connect(Parent, ParentToChild, Response) | NetInRest]) :-
    ground(Id?), ground(PlayNum?), ground(Parent?) |

    %% Accept: bind Response with return channel
    Response = accept(ChildToParent),

    %% Merge parent's stream with remaining network input
    merge(ParentToChild?, NetInRest?, AgentNetIn),

    agent(Id?, AgentIn?, AgentNetIn?,
          [output('_user', AgentToUser),
           output(child(Parent?), ChildToParent)]),
    ui_mediator(Id?, ch(AgentToUser?, AgentIn),
                ch(MedIn?, MedOut), [], 1),
    ui_actor(Id?, PlayNum?, ch(ActorIn?, ActorOut)),
    tee(ActorOut?, MedIn, DispCmd),
    tee(MedOut?, ActorIn, DispNotify),
    send_to_user_tagged(Id?, DispCmd?, DispNotify?).
```

**How it works:**

1. `mboot4` spawns 4 isolates concurrently.
2. Alice runs `parent_init(alice, carol, 4, NetIn)`:
   - Sends `msg(carol, parent_connect(alice, ParentToChild, Response))` on the network.
   - Wires everything immediately. Agent and merge suspend on unbound `ChildToParent?`.
3. Carol runs `child_init(carol, 4, NetIn)`:
   - Suspends on `NetIn?` until the first message arrives.
   - Receives `parent_connect(alice, ParentToChild, Response)`.
   - Binds `Response = accept(ChildToParent)`.
   - madGLP propagates the binding back to Alice's isolate.
   - Wires agent with `ParentToChild?` merged into input, `ChildToParent` as output.
4. Alice's suspended goals resume — `ChildToParent?` is now bound, merge starts flowing.
5. Both agents have cross-isolate parent-child streams in their output lists. `lookup_send(child(...), ...)` works transparently.

## Dart/Flutter design detail

### AgentRuntime modification

Currently `agent_runtime.dart` hardcodes `agent_init/3` as the entry point:

```dart
final entryPC = program.labels['agent_init/3'];
// Creates 3 args: Id, UserIn, NetIn
```

Modify to accept a configurable goal. Add fields:

```dart
final String goalLabel;       // e.g. 'agent_init_play/3', 'parent_init/4', 'child_init/3'
final List<String> extraArgs; // e.g. ['carol'] for parent_init, ['4'] for PlayNum
```

The `initialize()` method uses `goalLabel` to look up the entry PC and constructs arguments accordingly. `Id` is always arg 0 (from `agentId`). Extra args follow. `NetIn` is always the last argument (injected by AgentRuntime as before).

For SG scripted: `goalLabel='agent_init_play/3'`, `extraArgs=['1']` → goal is `agent_init_play(alice, 1, NetIn)`.
For CSSG parent: `goalLabel='parent_init/4'`, `extraArgs=['carol', '4']` → goal is `parent_init(alice, carol, 4, NetIn)`.
For CSSG child: `goalLabel='child_init/3'`, `extraArgs=['4']` → goal is `child_init(carol, 4, NetIn)`.

The `_userInput` InputInjector is still created but never injected into — actors drive input within GLP. This is the simplest approach (no additional changes needed).

### Flutter apps

`main_sg_mad.dart` and `main_cssg_mad.dart` are structurally similar to the existing `main.dart` and `main_cssg.dart`. The differences:

1. **Runner:** Uses `AgentRuntime` + `IsolateRouter` instead of `ReplPlayRunner`.
2. **GLP source loading:** Loads `typed_social_agent.glp`, `typed_ui_mediator.glp`, `typed_ui_actors.glp` (from cssg/), and `play_ui_madglp_boot.glp` (from cssg/).
3. **Play execution:** When Play N is clicked:
   - Tears down any running isolates.
   - Creates read-only agent panels.
   - Spawns agent isolates with the correct GLP sources, goal label, and extra args.
   - Output from `_output/1` arrives via `AgentOutput` messages, parsed as `tagged(id, cmd/notify(...))` and routed to per-agent panels.
4. **CSSG spawns 4 isolates:** alice (parent_init), carol (child_init), bob (parent_init), dave (child_init).
5. **Mode label:** App bar title includes "(Multi-Isolate)".

### Build workflow (4 apps)

```bash
cd /Users/udi/Grassroots/GLP/glp_multiagent

# 1. SG single-isolate
flutter build macos --target lib/main.dart

# 2. CSSG single-isolate
flutter build macos --target lib/main_cssg.dart

# 3. SG multi-isolate
flutter build macos --target lib/main_sg_mad.dart

# 4. CSSG multi-isolate
flutter build macos --target lib/main_cssg_mad.dart
```
