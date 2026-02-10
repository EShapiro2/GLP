# How to Run GLP Social Agent Programs

**Updated: 2026-02-11**

## Current Status

| Mode | Status | Result |
|------|--------|--------|
| dGLP (single-isolate) | ✅ WORKING | `→ succeeds` |
| madGLP (multi-isolate, headless) | 🔧 IN PROGRESS | Not yet working |
| madGLP (visual UI) | 🔧 READY FOR TESTING | Flutter app + play_ui_boot.glp |

---

## Quick Reference

| Mode | Files | How to Run |
|------|-------|------------|
| dGLP | `social_agent.glp` | REPL: `play.` |
| madGLP (headless, standalone) | `play_alice_bob_charlie_actor_boot.glp` | `dart test test/multiagent/multiagent_glp_test.dart -n "alice-bob-charlie"` |
| madGLP (headless, shared) | `social_agent.glp` + `play_madglp_boot.glp` | `dart test test/multiagent/isolate_manager_test.dart -n "runs full play"` |
| madGLP (visual UI) | `social_agent.glp` + `play_ui_boot.glp` | `flutter run -d macos` |

---

## Test Commands

### All multiagent tests (12 passing + 1 skipped)

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart test test/multiagent/multiagent_glp_test.dart
```

### Full REPL test suite (317 tests)

```bash
bash /Users/udi/Grassroots/GLP/test/run_all_tests.sh
```

### All Dart unit tests

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart test
```

---

## File Structure

```
programs/typed_book/social_graph/
├── social_agent.glp                        # SHARED: agent/4, actors, network3, close_outputs, helpers
├── play_dglp.glp                           # STANDALONE dGLP: all code + play/0 entry point
├── play_alice_bob_charlie_actor_boot.glp   # STANDALONE madGLP: all code + boot/0 (no shared source needed)
├── play_madglp_boot.glp                    # THIN madGLP boot (requires social_agent.glp as shared source)
├── play_dglp_boot.glp                      # dGLP boot (thin wrapper, not needed - play/0 is in social_agent.glp)
└── play_ui_boot.glp                        # madGLP boot: agent_init/3 for visual UI (Flutter app)
```

### Key distinction: two madGLP boot approaches

| Approach | Boot file | Shared source | Test file |
|----------|-----------|---------------|-----------|
| Standalone | `play_alice_bob_charlie_actor_boot.glp` | None (self-contained) | `multiagent_glp_test.dart` |
| Shared | `play_madglp_boot.glp` | `social_agent.glp` | `isolate_manager_test.dart` |

Both use `agent_init(alice, _)@alice` boot syntax (arity 2). The standalone file contains all agent/4 code, actors, and helpers inline. The shared approach loads `social_agent.glp` as `sharedSource` in the IsolateManager.

### Dart test files

```
glp_runtime/test/multiagent/
├── multiagent_glp_test.dart          # 12 passing madGLP tests + alice-bob-charlie (skipped)
├── isolate_manager_test.dart         # IsolateManager boot test + full play (shared source model)
├── boot_loader_test.dart             # BootLoader parsing tests
├── global_send_test.dart             # global_send goal mechanism
├── global_writers_table_test.dart    # GlobalWritersTable tests
├── globalize_test.dart               # Globalize operation tests
├── localize_test.dart                # Localize operation tests
├── mad_cold_call_isolate_test.dart   # Cold-call protocol in isolates
├── mad_error_handling_test.dart      # Error handling tests
├── mad_scenarios_test.dart           # End-to-end madGLP scenarios
└── mad_transactions_test.dart        # Transaction handling tests
```

Debug artifacts are in `glp_runtime/test/archive/` (not run by test suites).

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

### Run Command (standalone boot)

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart test test/multiagent/multiagent_glp_test.dart -n "alice-bob-charlie"
```

### Run Command (shared source boot)

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart test test/multiagent/isolate_manager_test.dart -n "runs full play"
```

### Run all multiagent tests

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart test test/multiagent/multiagent_glp_test.dart
```

### Boot Format

Both boot files use the same arity-2 format:

```glp
procedure boot.
boot :-
    agent_init(alice, _)@alice,
    agent_init(bob, _)@bob,
    agent_init(charlie, _)@charlie.
```

The `agent_init/2` procedure receives the agent ID and network input stream, then internally creates the UI channel, spawns `send_to_net/1`, and calls `agent/4`.

### Key Differences from dGLP

| Aspect | dGLP | madGLP |
|--------|------|--------|
| Process model | Single process | Separate isolates |
| Network routing | `network3` switch in GLP | `IsolateManager` in Dart |
| NetIn stream | Created by `network3` | Provided by madGLP serializer |
| Entry point | `play.` | `boot :- ...@agent` |

### Key Components

| Component | Location | Purpose |
|-----------|----------|---------|
| `IsolateManager` | `lib/multiagent/isolate_manager.dart` | Boots and manages agent isolates |
| `BootLoader` | `lib/multiagent/boot_loader.dart` | Parses boot files with `@agent` syntax |
| `MadContext` | `lib/multiagent/mad_context.dart` | Manages global writers table, message routing |
| `send_to_net/1` | Embedded in `isolate_manager.dart` | GLP predicate processing network output stream |

---

## madGLP Visual UI (Flutter App)

Interactive multi-window execution using `glp_multiagent` Flutter app. Each agent runs in its own window with REPL-style input.

### Run Command

```bash
cd /Users/udi/Grassroots/GLP/glp_multiagent
flutter run -d macos
```

### Setup

1. Click "Alice-Bob-Charlie" to spawn three agent windows
2. Each window shows an agent with input field for GLP terms

### Boot File (`play_ui_boot.glp`)

```glp
-mode(system).

procedure agent_init(Constant?, Channel?, Channel?).
agent_init(Id, ch(UserIn, UserOut?), ch(NetIn, NetOut?)) :-
    ground(Id?) |
    agent(Id?, UserOut?, NetIn?, [output('_user', UserIn), output('_net', NetOut)]).
```

This adapter provides `agent_init/3` which the Flutter app calls, bridging to the existing `agent/4`.

### User Commands

In any agent window, type GLP terms:
- `connect(bob)` - cold-call Bob
- `send(bob, hello)` - send text message to friend
- `introduce(alice, charlie)` - introduce two friends

### Flutter App Architecture

| Component | Location | Purpose |
|-----------|----------|---------|
| Coordinator | `glp_multiagent/lib/main.dart` | Spawns windows, routes messages |
| Agent Window | `glp_multiagent/lib/main.dart` | Per-agent GLP runtime + UI |
| MadRouter | `glp_multiagent/lib/mad_router.dart` | Routes messages between windows |

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

## Current Issue: madGLP Blocked at Step 1

**Date: 2026-02-11**

### What works

- madGLP infrastructure routes messages correctly between isolates
- 12 simpler multi-agent tests pass (shared variables, streams, bidirectional, three-agent pipeline, etc.)
- Alice's cold-call `msg(bob, intro(alice, Resp))` reaches Bob's agent via madGLP
- Bob's agent processes it and writes `msg(agent, _user, befriend(alice, Resp?))` to the user output stream

### Where it stalls

Bob's `ui_relay` has a `no_readers(Msg?)` guard (line 89 of `play_alice_bob_charlie_actor_boot.glp`). The `befriend(alice, Resp?)` message contains an unbound reader (`Resp?` — the response variable). The `no_readers` guard fails because there IS a reader inside the message, so the relay suspends instead of forwarding to the actor.

Bob's actor never sees the `befriend` request, never responds, and the protocol stalls.

### Root cause

The `ui_relay` `no_readers` guard is incompatible with the cold-call protocol, which inherently passes unbound response variables through messages. This is a GLP program design issue in `play_alice_bob_charlie_actor_boot.glp`, not an infrastructure bug.
