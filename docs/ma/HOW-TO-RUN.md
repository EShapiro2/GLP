# How to Run GLP Social Agent Programs

**Updated: 2026-02-12**

## Current Status

| Mode | Status | Result |
|------|--------|--------|
| dGLP (single-isolate) | ✅ WORKING | `→ succeeds` |
| madGLP (multi-isolate, headless) | ✅ Protocol completes | All 7 steps run, agents terminate |
| madGLP (visual UI) | ⚠️ INTEGRATED, UNTESTED | Boot updated, needs manual testing |

---

## Quick Reference

| Mode | Files | How to Run |
|------|-------|------------|
| dGLP | `social_agent.glp` | REPL: `play.` |
| madGLP (headless) | `social_agent.glp` + `play_madglp_boot.glp` | `dart test test/multiagent/isolate_manager_test.dart -n "runs full play"` |
| madGLP (visual UI) | `social_agent.glp` + `ui_mediator.glp` + `play_ui_boot.glp` | `cd glp_multiagent && flutter run -d macos` |

---

## Test Commands

### UI I/O tests (8 passing)

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart test test/multiagent/output_kernel_test.dart    # 5 tests: _output/1 kernel + send_to_user/1
dart test test/multiagent/ui_mediator_test.dart       # 3 tests: ui_mediator grounding + passthrough
```

### All multiagent tests (69 passing + 5 skipped + 2 pre-existing failures)

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart test test/multiagent/
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
├── social_agent.glp        # SHARED: agent/4, actors, network3, close_outputs, helpers
├── ui_mediator.glp         # Ground-term mediator between agent/4 and Dart UI
├── play_dglp_boot.glp      # dGLP boot (thin wrapper - play/0 is in social_agent.glp)
├── play_madglp_boot.glp    # madGLP boot: boot/0 with @agent syntax (headless with actors)
├── play_ui_boot.glp        # madGLP boot: agent_init/3 for visual UI
└── ui_agent.glp            # OBSOLETE: uses old agent/3 with FriendsList
```

One shared program (`social_agent.glp`) with three boot files. The `ui_mediator.glp` provides the ground-term boundary between agent/4 and Dart.

### Dart runtime files

```
glp_runtime/lib/multiagent/
├── agent_runtime.dart      # AgentRuntime class (extracted from Flutter main.dart)
├── isolate_manager.dart    # IsolateManager + madPredicatesSource (send_to_net, send_to_user)
├── mad_context.dart        # MadContext: W_p, M_p, message routing
├── boot_loader.dart        # BootLoader: parses @agent syntax
├── message_queue.dart      # Message types and serialization
├── payload_serializer.dart # Binary payload serialization
└── global_writers_table.dart # GlobalWritersTable (W_p)
```

### Dart test files

```
glp_runtime/test/multiagent/
├── output_kernel_test.dart          # _output/1 kernel + send_to_user/1 (5 tests)
├── ui_mediator_test.dart            # ui_mediator grounding + passthrough (3 tests)
├── multiagent_glp_test.dart         # 12 passing madGLP tests + alice-bob-charlie (skipped)
├── isolate_manager_test.dart        # IsolateManager boot test + full play (shared source model)
├── boot_loader_test.dart            # BootLoader parsing tests
├── global_send_test.dart            # global_send goal mechanism
├── global_writers_table_test.dart   # GlobalWritersTable tests
├── globalize_test.dart              # Globalize operation tests
├── localize_test.dart               # Localize operation tests
├── mad_cold_call_isolate_test.dart  # Cold-call protocol in isolates
├── mad_error_handling_test.dart     # Error handling tests
├── mad_scenarios_test.dart          # End-to-end madGLP scenarios
└── mad_transactions_test.dart       # Transaction handling tests
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

### Current Status: INTEGRATED, NEEDS MANUAL TESTING

The Flutter app uses `AgentRuntime` (extracted from main.dart into `glp_runtime`) which boots `agent_init/3` from `play_ui_boot.glp`. All I/O goes through GLP:

- **User output**: `send_to_user` → `_output/1` kernel → `outputCallback` → Flutter UI
- **Network output**: `send_to_net` → `global_send` → `MadContext` → `onMessageReady` → coordinator → `MadRouter`
- **User input**: Flutter text field → `InputInjector` → GLP `UserIn` stream → `ui_mediator` → `agent/4`
- **Network input**: coordinator → `MadContext` → `InputInjector` → GLP `NetIn` stream → `agent/4`

### Run Command

```bash
cd /Users/udi/Grassroots/GLP/glp_multiagent
flutter run -d macos
```

### User Commands

In any agent window, type GLP terms:
- `connect(bob)` — cold-call Bob
- `send(bob, hello)` — send text message to friend
- `introduce(alice, charlie)` — introduce two friends
- `decision(yes, bob, 1)` — accept befriend request (req ID from mediator output)

### Architecture

```
Dart (Flutter)                           GLP
                                         ┌─────────────────────────┐
UserInput ──InputInjector──► UserIn ────►│ ui_mediator ──► agent/4 │
                                         │      │                  │
           ◄── outputCallback ◄── _output/1 ◄── send_to_user ◄──┘│
                                         │                         │
                                         │ agent/4 ──► send_to_net │
                                         │              │          │
           ◄── onMessageReady ◄── global_send ◄────────┘          │
             (MadContext)                │                         │
                                         │                         │
NetIn ──────InputInjector──────────────►│ ──────────► agent/4     │
  (from MadContext)                      └─────────────────────────┘
```

### Flutter App Components

| Component | Location | Purpose |
|-----------|----------|---------|
| Coordinator | `glp_multiagent/lib/main.dart` | Spawns windows, routes messages via MadRouter |
| Agent Window | `glp_multiagent/lib/main.dart` | Flutter UI + AgentRuntime wiring |
| AgentRuntime | `glp_runtime/lib/multiagent/agent_runtime.dart` | GLP runtime, MadContext, I/O, execution |
| MadRouter | `glp_multiagent/lib/mad_router.dart` | Routes messages between windows |
| InputInjector | `glp_runtime/lib/runtime/external_io.dart` | Dart injects terms into GLP stream |

### GLP Files for Visual UI

| File | Purpose | Status |
|------|---------|--------|
| `social_agent.glp` | agent/4, protocol, helpers | ✅ Working |
| `ui_mediator.glp` | Ground-term mediator (agent/4 ↔ Dart) | ✅ Tested |
| `play_ui_boot.glp` | Boot: agent_init/3 with mediator | ✅ Integrated |
| `ui_agent.glp` | Old mediator (agent/3) | ❌ Obsolete |

### Dart Implementation Files for UI I/O

| File | What | Status |
|------|------|--------|
| `glp_runtime/lib/multiagent/agent_runtime.dart` | AgentRuntime class | ✅ |
| `glp_runtime/lib/runtime/body_kernels.dart` | `_output/1` kernel + `formatGroundTerm()` | ✅ |
| `glp_runtime/lib/runtime/runtime.dart` | `outputCallback` field | ✅ |
| `glp_runtime/lib/multiagent/isolate_manager.dart` | `madPredicatesSource` (send_to_net, send_to_user) | ✅ |

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

## Current Status Summary (2026-02-12)

- dGLP: ✅ Working — `play.` succeeds
- madGLP headless: ✅ Protocol completes — all 7 steps run in multi-isolate test
- madGLP visual UI: ⚠️ Integrated — boot updated, needs manual testing with `flutter run`
- UI I/O components: ✅ All tested in isolation (8 tests pass)
- 69 multiagent tests pass, 5 skipped, 2 pre-existing isolate timeout failures
