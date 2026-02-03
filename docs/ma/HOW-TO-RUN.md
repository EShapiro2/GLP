# How to Run GLP Social Agent Programs

**Updated: 2026-02-03**

## Current Status

| Mode | Status | Result |
|------|--------|--------|
| dGLP | ✅ WORKING | `→ succeeds` |
| madGLP | 🔧 MESSAGES WORK, COMPLETION BLOCKED | Agents suspended forever |

---

## Quick Reference

| Mode | Files | How to Run |
|------|-------|------------|
| dGLP | `social_agent.glp` | REPL: `play.` |
| madGLP | `social_agent.glp` + `play_madglp_boot.glp` | Dart test |

## File Structure

```
programs/typed_book/social_graph/
├── social_agent.glp        # SHARED: agent/4, actors, network3, close_outputs, helpers
├── play_dglp_boot.glp      # dGLP boot (thin wrapper, not needed - play/0 is in social_agent.glp)
├── play_madglp_boot.glp    # madGLP boot: boot/0 with @agent syntax
└── play_dglp.glp           # STANDALONE dGLP (duplicates social_agent.glp) - DEPRECATED
```

---

## dGLP (Deterministic GLP)

Single-process execution using REPL. All agents run in the same process, with `network3` switch routing messages.

### Run Command

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
echo -e 'load ../programs/typed_book/social_graph/social_agent.glp\nplay.' | dart run bin/glp_repl.dart
```

### Expected Output

```
✓ Loaded: ../programs/typed_book/social_graph/social_agent.glp
→ succeeds
```

### How It Works

`social_agent.glp` contains everything:
- `play/0` - entry point that sets up network3 and spawns agents
- `agent/4` - main agent loop
- `network3/3` - message routing switch
- `*_actor` - scripted actors (alice, bob, charlie)
- `close_outputs/1` - closes all output streams on agent termination

---

## madGLP (Multi-Agent Deterministic GLP)

Multi-isolate execution using `IsolateManager`. Each agent runs in its own Dart isolate.

### Run Command

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart test test/multiagent/isolate_manager_test.dart -n "runs full play"
```

Or run all multiagent tests:
```bash
dart test test/multiagent/isolate_manager_test.dart
```

### Boot File (`play_madglp_boot.glp`)

```glp
-mode(system).

procedure boot.
boot :-
    agent_init(alice, _)@alice,
    agent_init(bob, _)@bob,
    agent_init(charlie, _)@charlie.

procedure agent_init(Constant?, Stream?).
agent_init(Id, NetIn) :-
    ground(Id?) |
    send_to_net(NetOut?),
    agent(Id?, UserOut?, NetIn?, [output('_user', UserIn), output('_net', NetOut)]),
    actor(Id?, ch(UserIn?, UserOut)).

procedure actor(_?, Channel?).
actor(alice, Ch) :- alice_actor(Ch?).
actor(bob, Ch) :- bob_actor(Ch?).
actor(charlie, Ch) :- charlie_actor(Ch?).
```

### Key Differences from dGLP

| Aspect | dGLP | madGLP |
|--------|------|--------|
| Process model | Single process | Separate isolates |
| Network routing | `network3` switch in GLP | `IsolateManager` in Dart |
| NetIn stream | Created by `network3` | Provided by `send_to_net` kernel |
| Entry point | `play.` | `boot :- ...@agent` |
| Stream closure | Propagates via heap | **NO PROPAGATION** (blocked) |

### Key Components

| Component | Location | Purpose |
|-----------|----------|---------|
| `IsolateManager` | `lib/multiagent/isolate_manager.dart` | Boots and manages agent isolates |
| `BootLoader` | `lib/multiagent/boot_loader.dart` | Parses boot files with `@agent` syntax |
| `MadContext` | `lib/multiagent/mad_context.dart` | Manages global writers table, message routing |
| `send_to_net` | `lib/runtime/body_kernels.dart` | Kernel that returns network output stream |
| `_deepDeref` | `lib/runtime/body_kernels.dart` | Recursively dereferences nested structures |

---

## Test Scenario (7 steps)

1. Alice cold-calls Bob (Bob accepts)
2. Alice sends "Hi Bob, this is Alice"
3. Bob cold-calls Charlie (Charlie accepts)
4. Charlie sends "Hi Bob, this is Charlie"
5. Bob introduces Alice to Charlie (both accept)
6. Alice sends "Hi Charlie, this is Alice"
7. Charlie responds "Hi Alice, this is Charlie"

---

## Current Issue: madGLP Completion

### Problem
Messages flow correctly between agents, but agents remain `suspended` forever because stream closure doesn't propagate across isolate boundaries.

### What's Working
- `_deepDeref` properly serializes nested structures
- Messages like `msg(bob, intro(alice, X))` are sent correctly
- All 7 protocol steps execute

### What's Blocked
- No mechanism to propagate `[]` (stream closure) across isolates
- Agents waiting on `NetIn?` stay suspended forever
- Test times out with all agents showing `suspended, goals=0`

### Next Steps (Options)
1. **End-of-Stream Message**: Add `MessageType.eos` to signal stream termination
2. **Global Completion Detection**: `IsolateManager` detects idle state
3. **Actor-Driven Termination**: Actors send explicit `done` signal

See `/Users/udi/.claude/plans/harmonic-strolling-graham.md` for detailed analysis.

---

## Recent Changes (2026-02-03)

1. **Fixed type error**: `add_output` procedure now uses `String?` instead of `_?`
2. **Added `close_outputs/1`**: Agents now close all output streams on termination
3. **Actors close output**: Each actor closes its output stream when script ends
4. **dGLP succeeds**: Play terminates cleanly with `→ succeeds`
5. **Fixed VarRef bug**: Added `_deepDeref` to properly serialize nested structures for madGLP
6. **Fixed message format**: `send_to_net` now sends `msg(Q, T)` instead of just `T` (matches dGLP)
7. **Strengthened types**: Added `NetMsg`, `NetStream`, `GlobalName`, `AgentId`, `Decision` types
8. **madGLP messages work**: Protocol executes correctly, but completion blocked
