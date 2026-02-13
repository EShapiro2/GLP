# How to Run GLP Social Agent Programs

**Updated: 2026-02-13**

## Current Status

| Mode | Status | Result |
|------|--------|--------|
| dGLP (single-isolate, no UI) | ✅ WORKING | `→ succeeds` |
| dGLP (single-isolate, with mediator + UI actors) | ✅ WORKING | `→ suspended` (all processes active) |
| madGLP (multi-isolate, headless, no UI) | ✅ WORKING | All 3 agents complete |
| madGLP (multi-isolate, headless, with mediator + UI actors) | ✅ WORKING | All 3 agents complete |
| madGLP (visual Flutter UI) | ⚠️ INTEGRATED, UNTESTED | Boot updated, needs manual testing |

---

## Quick Reference

| Mode | GLP Files | How to Run |
|------|-----------|------------|
| dGLP (no UI) | `typed_social_agent.glp` + `typed_actors.glp` + `play_dglp_boot.glp` | REPL: `play.` |
| dGLP (with mediator) | `typed_social_agent.glp` + `typed_ui_mediator.glp` + `typed_ui_actors.glp` + `play_ui_dglp_boot.glp` | REPL: `play.` |
| madGLP (headless, no UI) | `typed_social_agent.glp` + `typed_actors.glp` + `play_madglp_boot.glp` | `dart test test/multiagent/isolate_manager_test.dart -n "no UI"` |
| madGLP (headless, with mediator) | `typed_social_agent.glp` + `typed_ui_mediator.glp` + `typed_ui_actors.glp` + `play_ui_madglp_boot.glp` | `dart test test/multiagent/isolate_manager_test.dart -n "UI mediator"` |
| madGLP (visual UI) | `typed_social_agent.glp` + `typed_ui_mediator.glp` + `play_ui_boot.glp` | `cd glp_multiagent && flutter build macos --release` |

---

## Test Commands

### Isolate manager tests (3 passing)

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart test test/multiagent/isolate_manager_test.dart
```

Tests:
- `boots three agents from boot config` — trivial boot, agents complete immediately
- `runs full play with actor scripts (no UI)` — full protocol via `play_madglp_boot.glp` + `typed_actors.glp`
- `runs full play with UI mediator and UI actors` — full protocol via `play_ui_madglp_boot.glp` + `typed_ui_mediator.glp` + `typed_ui_actors.glp`

### UI mediator tests (3 passing)

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart test test/multiagent/ui_mediator_test.dart
```

Tests:
- `grounds befriend output with request ID` — verifies `befriend(bob, req(1))` output
- `passes ground connected message through` — verifies `connected(bob)` passthrough
- `passes ground received message through` — verifies `received(bob, hello)` passthrough

### UI I/O tests (5 passing)

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart test test/multiagent/output_kernel_test.dart
```

### All multiagent tests (71 passing + 5 skipped + 1 pre-existing failure)

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart test test/multiagent/
```

The 1 failure is `mad_cold_call_isolate_test.dart: Alice sends Resp? to Bob, Bob binds to pong, Alice receives pong` — a pre-existing isolate timeout.

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

### Active typed files

```
programs/typed_book/social_graph/
├── typed_social_agent.glp    # agent/4, channel ops, merge, helpers (typed, -mode(system))
├── typed_ui_mediator.glp     # Ground-term mediator: agent/4 ↔ Dart UI (typed)
├── typed_actors.glp          # Scripted actors that talk directly to agent/4 (typed)
├── typed_ui_actors.glp       # Scripted actors that talk to ui_mediator (typed, ground terms)
├── play_dglp_boot.glp        # dGLP boot: network3 + play (untyped, cross-file)
├── play_ui_dglp_boot.glp     # dGLP boot with mediator: network3 + play (untyped, cross-file)
├── play_madglp_boot.glp      # madGLP boot: boot + agent_init/2 + actor dispatch (typed)
├── play_ui_madglp_boot.glp   # madGLP boot with mediator: boot + agent_init/2 (typed)
└── play_ui_boot.glp          # Flutter UI boot: agent_init/3 with send_to_user (for visual UI)
```

### Boot file variants

| Boot file | Stack | Use case |
|-----------|-------|----------|
| `play_dglp_boot.glp` | network3 → agent/4 → actor | Single-isolate REPL, no mediator |
| `play_ui_dglp_boot.glp` | network3 → agent/4 → ui_mediator → ui_actor | Single-isolate REPL, with mediator |
| `play_madglp_boot.glp` | agent/4 → actor + send_to_net | Multi-isolate headless, no mediator |
| `play_ui_madglp_boot.glp` | agent/4 → ui_mediator → ui_actor + send_to_net | Multi-isolate headless, with mediator |
| `play_ui_boot.glp` | agent/4 → ui_mediator → send_to_user + send_to_net | Multi-isolate Flutter UI (human input) |

### Archived untyped originals

```
programs/typed_book/social_graph/archive/
├── social_agent.glp    # Original monolithic file (all code in one file)
└── ui_mediator.glp     # Original untyped mediator
```

### Dart runtime files

```
glp_runtime/lib/multiagent/
├── agent_runtime.dart      # AgentRuntime class (for Flutter UI)
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
├── isolate_manager_test.dart        # 3 tests: trivial boot, full play (no UI), full play (with UI)
├── ui_mediator_test.dart            # 3 tests: mediator grounding + passthrough
├── output_kernel_test.dart          # 5 tests: _output/1 kernel + send_to_user/1
├── multiagent_glp_test.dart         # 12 tests: madGLP shared-variable and stream tests
├── boot_loader_test.dart            # BootLoader parsing tests
├── global_send_test.dart            # global_send goal mechanism
├── global_writers_table_test.dart   # GlobalWritersTable tests
├── globalize_test.dart              # Globalize operation tests
├── localize_test.dart               # Localize operation tests
├── mad_cold_call_isolate_test.dart  # Cold-call protocol in isolates (1 pre-existing failure)
├── mad_error_handling_test.dart     # Error handling tests (5 skipped)
├── mad_scenarios_test.dart          # End-to-end madGLP scenarios
└── mad_transactions_test.dart       # Transaction handling tests
```

---

## dGLP (Deterministic GLP)

Single-process execution using REPL. All agents run in the same process.

### Without mediator (actors talk directly to agent/4)

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
echo -e 'load ../programs/typed_book/social_graph/typed_social_agent.glp\nload ../programs/typed_book/social_graph/typed_actors.glp\nload ../programs/typed_book/social_graph/play_dglp_boot.glp\nplay.\n:quit' | dart run bin/glp_repl.dart
```

Expected output: `→ succeeds`

### With mediator (UI actors talk to mediator using ground terms)

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
echo -e 'load ../programs/typed_book/social_graph/typed_social_agent.glp\nload ../programs/typed_book/social_graph/typed_ui_mediator.glp\nload ../programs/typed_book/social_graph/typed_ui_actors.glp\nload ../programs/typed_book/social_graph/play_ui_dglp_boot.glp\nplay.\n:quit' | dart run bin/glp_repl.dart
```

Expected output: `→ suspended` (all concurrent processes are active and communicating)

### How dGLP works

`network3` routes messages between agents in GLP. Each agent runs `agent/4` with output lists. Actors (or UI actors via mediator) drive the protocol.

---

## madGLP (Multi-Agent Deterministic GLP)

Multi-isolate execution using `IsolateManager`. Each agent runs in its own Dart isolate.

### Headless without mediator

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart test test/multiagent/isolate_manager_test.dart -n "no UI"
```

### Headless with mediator + UI actors

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart test test/multiagent/isolate_manager_test.dart -n "UI mediator"
```

### All multiagent tests

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart test test/multiagent/multiagent_glp_test.dart
```

### Boot format

Both madGLP boot files use arity-2 `agent_init`:

```prolog
procedure boot.
boot :-
    agent_init(alice, _)@alice,
    agent_init(bob, _)@bob,
    agent_init(charlie, _)@charlie.

procedure agent_init(Constant?, Stream?).
agent_init(Id, NetIn) :-
    ground(Id?) |
    send_to_net(NetOut?),
    agent(Id?, ..., NetIn?, ...),
    ...
```

### Key differences from dGLP

| Aspect | dGLP | madGLP |
|--------|------|--------|
| Process model | Single process | Separate isolates |
| Network routing | `network3` switch in GLP | `IsolateManager` in Dart |
| NetIn stream | Created by `network3` | Provided by madGLP serializer |
| Entry point | `play.` | `boot :- ...@agent` |
| Completion detection | Process terminates | Idle-tick detection (2 consecutive empty ticks) |

---

## madGLP Visual UI (Flutter App)

Interactive multi-window execution using `glp_multiagent` Flutter app. Each agent runs in its own window with REPL-style input.

### Current Status: INTEGRATED, NEEDS MANUAL TESTING

The mediator pending-key bug has been fixed (was storing bare `N` but sending `req(N)` to user). This should resolve the "decision got 0 activations" error seen previously.

### Build and Launch

**IMPORTANT: Use release build, not `flutter run`.**

```bash
cd /Users/udi/Grassroots/GLP/glp_multiagent && flutter build macos --release 2>&1
```

Then launch:

```bash
open /Users/udi/Grassroots/GLP/glp_multiagent/build/macos/Build/Products/Release/glp_multiagent.app
```

To kill a running instance:

```bash
pkill -f glp_multiagent
```

### GLP files loaded by Flutter app

The Flutter app (`main.dart`) loads these three files in order:
1. `typed_social_agent.glp` — agent/4, channel ops, helpers
2. `typed_ui_mediator.glp` — ground-term mediator
3. `play_ui_boot.glp` — interactive boot: `agent_init(Id, UserIn, NetIn)`

### Spawning Agents

1. The **coordinator window** opens first.
2. Click **"Alice↔Bob↔Charlie"** to spawn three agent windows.
3. Each agent window has a text input field at the bottom for commands.

### Interactive Protocol — The Full Introduction Script

The protocol below matches the automated actor scripts in `typed_ui_actors.glp`. The ui_mediator replaces non-ground variables with `req(N)` identifiers, so the user types ground terms only.

#### Step 1 — Alice: cold-call Bob

Type in **Alice's** window:
```
connect(bob)
```
**Wait for:** Bob's window shows `befriend(alice, req(1))`.

#### Step 2 — Bob: accept Alice's friend request

Type in **Bob's** window (use the req number from Bob's output):
```
decision(yes, alice, req(1))
```
**Wait for:** Both Alice and Bob show `connected(...)`.

#### Step 3 — Alice: send message to Bob

Type in **Alice's** window:
```
send(bob, 'Hi Bob, this is Alice')
```
**Wait for:** Bob shows `received(alice, Hi Bob, this is Alice)`.

#### Step 4 — Bob: cold-call Charlie

Type in **Bob's** window:
```
connect(charlie)
```
**Wait for:** Charlie shows `befriend(bob, req(1))`.

#### Step 5 — Charlie: accept Bob's friend request and greet

Type in **Charlie's** window (use the req number from Charlie's output):
```
decision(yes, bob, req(1))
```
Then:
```
send(bob, 'Hi Bob, this is Charlie')
```
**Wait for:** Both Bob and Charlie show `connected(...)`. Bob shows `received(charlie, ...)`.

#### Step 6 — Bob: introduce Alice to Charlie

Type in **Bob's** window:
```
introduce(alice, charlie)
```
**Wait for:**
- Alice shows `befriend_intro(bob, charlie, req(N))`
- Charlie shows `befriend_intro(bob, alice, req(N))`

Note the req numbers — they may differ between Alice and Charlie.

#### Step 7 — Alice: accept introduction to Charlie

Type in **Alice's** window (use Alice's req number from step 6):
```
accept_intro(charlie, req(N))
```

#### Step 8 — Charlie: accept introduction to Alice

Type in **Charlie's** window (use Charlie's req number from step 6):
```
accept_intro(alice, req(N))
```
**Wait for:** Both Alice and Charlie show `connected(...)`.

#### Step 9 — Alice sends to Charlie

Type in **Alice's** window:
```
send(charlie, 'Hi Charlie, this is Alice')
```
**Wait for:** Charlie shows `received(alice, ...)`.

#### Step 10 — Charlie sends to Alice

Type in **Charlie's** window:
```
send(alice, 'Hi Alice, this is Charlie')
```
**Wait for:** Alice shows `received(charlie, ...)`.

### Architecture

```
Dart (Flutter)                           GLP
                                         +---------------------------+
UserInput --InputInjector--> UserIn ---->| ui_mediator --> agent/4   |
                                         |      |                    |
           <-- outputCallback <-- _output/1 <-- send_to_user <-----+|
                                         |                           |
                                         | agent/4 --> send_to_net   |
                                         |              |            |
           <-- onMessageReady <-- global_send <---------+            |
             (MadContext)                |                           |
                                         |                           |
NetIn ------InputInjector--------------->| ----------> agent/4       |
  (from MadContext)                      +---------------------------+
```

### Flutter App Components

| Component | Location | Purpose |
|-----------|----------|---------|
| Coordinator | `glp_multiagent/lib/main.dart` | Spawns windows, routes messages via MadRouter |
| Agent Window | `glp_multiagent/lib/main.dart` | Flutter UI + AgentRuntime wiring |
| AgentRuntime | `glp_runtime/lib/multiagent/agent_runtime.dart` | GLP runtime, MadContext, I/O, execution |
| MadRouter | `glp_multiagent/lib/mad_router.dart` | Routes messages between windows |
| InputInjector | `glp_runtime/lib/runtime/external_io.dart` | Dart injects terms into GLP stream |

---

## Test Scenario (7 steps)

All four headless modes (dGLP ± mediator, madGLP ± mediator) run the same scenario:

1. Alice cold-calls Bob (Bob accepts)
2. Alice sends "Hi Bob, this is Alice"
3. Bob cold-calls Charlie (Charlie accepts, sends "Hi Bob, this is Charlie")
4. Bob introduces Alice to Charlie (both accept)
5. Alice sends "Hi Charlie, this is Alice"
6. Charlie responds "Hi Alice, this is Charlie"

---

## Known Issues

- **1 pre-existing test failure**: `mad_cold_call_isolate_test.dart: Alice sends Resp?` times out due to `useReader[0]` direction mismatch in globalize/localize.
- **Duplicate messages in Flutter UI**: The `_reactivateSuspendedGoals()` method in `agent_runtime.dart` likely causes duplicate processing. See `/Users/udi/Grassroots/GLP/docs/ma/agent-runtime-spec.md`.
- **Focus fix requires release build**: The `DispatchQueue.main.async` focus fix in `AppDelegate.swift` works in release mode but not reliably in debug mode.

---

## Current Status Summary (2026-02-13)

- dGLP (no mediator): ✅ Working — `play.` succeeds
- dGLP (with mediator + UI actors): ✅ Working — `play.` runs, processes active
- madGLP headless (no mediator): ✅ Working — all 3 agents complete
- madGLP headless (with mediator + UI actors): ✅ Working — all 3 agents complete
- madGLP visual Flutter UI: ⚠️ Integrated, pending-key bug fixed, needs manual testing
- 71 multiagent tests pass, 5 skipped, 1 pre-existing failure
