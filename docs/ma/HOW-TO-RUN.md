# How to Run GLP Social Agent Programs

**Updated: 2026-02-20**

**Prerequisite:** Read `/Users/udi/Grassroots/GLP/CLAUDE.md` before working with GLP code. The REPL is the only way to compile, typecheck, and run GLP code.

## Current Status

| Mode | Status | Result |
|------|--------|--------|
| dGLP (single-isolate, no UI) | ✅ WORKING | `→ succeeds` |
| dGLP (single-isolate, with mediator + UI actors) | ✅ WORKING | `→ suspended` (all concurrent processes active) |
| madGLP (multi-isolate, headless, no UI) | ✅ WORKING | Full protocol completes |
| madGLP (multi-isolate, headless, with mediator + UI actors) | ✅ WORKING | Full protocol completes |
| madGLP (visual Flutter UI) | ✅ WORKING | Full 10-step interactive script verified (2026-02-16) |
| Simulated plays (Flutter UI, REPL subprocess) | ✅ WORKING | fplay1–3 verified (2026-02-20) |

---

## Quick Reference

| Mode | GLP Files | How to Run |
|------|-----------|------------|
| dGLP (no UI) | `typed_social_agent.glp` + `typed_actors.glp` + `play_dglp_boot.glp` | REPL: `play.` |
| dGLP (with mediator) | `typed_social_agent.glp` + `typed_ui_mediator.glp` + `typed_ui_actors.glp` + `play_ui_dglp_boot.glp` | REPL: `play.` |
| madGLP (headless, no UI) | `typed_social_agent.glp` + `typed_actors.glp` + `play_madglp_boot.glp` | `dart test test/multiagent/isolate_manager_test.dart -n "no UI"` |
| madGLP (headless, with mediator) | `typed_social_agent.glp` + `typed_ui_mediator.glp` + `typed_ui_actors.glp` + `play_ui_madglp_boot.glp` | `dart test test/multiagent/isolate_manager_test.dart -n "UI mediator"` |
| madGLP (visual UI) | `typed_social_agent.glp` + `typed_ui_mediator.glp` + `play_ui_boot.glp` | `cd glp_multiagent && flutter build macos --release` |
| Simulated plays (Flutter) | `cssg/` files via `ReplPlayRunner` | Build Flutter app, click Play 1/2/3 |
| Simulated plays (REPL) | `cssg/` 4 files + `fplayN.` | See below |

### Simulated plays via REPL

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
echo -e '../programs/typed_book/cssg/typed_social_agent.glp\n../programs/typed_book/cssg/typed_ui_mediator.glp\n../programs/typed_book/cssg/typed_ui_actors.glp\n../programs/typed_book/cssg/play_ui_sim_boot.glp\nfplay1.\n:quit' | dart run bin/glp_repl.dart
```

Replace `fplay1.` with `fplay2.` or `fplay3.` for other scenarios. Use `play1.` (without `f`) for silent plays (no tagged output).

A frozen snapshot of these files (before CSSG extension) is in `programs/typed_book/social_graph_simulated_ui/`.

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
├── isolate_manager.dart    # IsolateManager (headless multi-isolate execution)
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

### Current Status: WORKING

Full 10-step interactive protocol verified (2026-02-16). All steps complete: cold-call, messaging, introduction, and cross-introduction messaging.

### Build and Launch

```bash
cd /Users/udi/Grassroots/GLP/glp_multiagent && flutter run -d macos
```

Or build release and launch:

```bash
cd /Users/udi/Grassroots/GLP/glp_multiagent && flutter build macos --release
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
- **Type checker warnings for cross-file references**: When files are loaded separately, the type checker warns about "Undefined procedure" for procedures defined in other files. These are compile-time warnings only and do not affect runtime.

### Mediator pending list bug — FIXED (2026-02-16)

**Root cause:** The mediator stored opaque variables in the pending list. When `lookup_pending` retrieved a variable and the agent tried to bind it, the binding did not propagate back through the reader-of-reader chain created by the extra pending-list indirection.

**Fix:** Introduced a precise `PendingValue` type that wraps stored values:
```prolog
PendingValue ::= response(Response?) ; channel(Channel?) ; error.
PendingEntry ::= pending(ReqId, PendingValue).
```
The mediator stores `response(Resp?)` or `channel(Ch?)` in pending and passes the whole `PendingValue` to the agent without destructuring. The agent unwraps the `PendingValue` in its clause heads: `decision(Dec, From, response(Resp?))` and `accept_intro(Other, channel(ch(FIn, FOut?)))`. This eliminates the double-reader problem because the agent destructures the wrapper to access the original reader directly.

Both `typed_social_agent.glp` and `typed_ui_mediator.glp` typecheck. All five execution modes pass.

---

## Current Status Summary (2026-02-16)

- dGLP (no mediator): ✅ Working — `play.` succeeds
- dGLP (with mediator + UI actors): ✅ Working — `play.` suspends (all processes active)
- madGLP headless (no mediator): ✅ Working — full protocol completes
- madGLP headless (with mediator + UI actors): ✅ Working — full protocol completes
- madGLP visual Flutter UI: ✅ Working — full 10-step interactive script verified

---

## Bugs Fixed in This Session (2026-02-13)

### 1. ONE way to run GLP — GlpEngine constructor loads stdlib

Previously three code paths (REPL, IsolateManager, AgentRuntime) each set up GlpEngine differently. Paths 2 and 3 skipped stdlib, causing `:=/2` to be missing. Fixed by making `GlpEngine({required String stdlibDir})` load stdlib in the constructor. `enableMadGLP()` loads madPredicates internally. `loadStdlib` is now private.

### 2. Source file concatenation bug — separate loadSource per file

Multiple GLP files were concatenated with `sources.join('\n')` and loaded as one `loadSource()` call. The parser failed on the second file's `-mode(system)` directive. Fixed by storing files as `List<String>` and loading each separately.

### 3. loadSource filename collision — files overwriting each other

`loadSource()` without a `filename:` parameter defaults to key `'_source_'`. When loading 3+ files, each overwrites the previous in `_loadedPrograms`. Only the last file survives. Fixed by passing unique filenames: `filename: 'shared_$i'` and `filename: 'program'`.

### 4. All paths use repo-relative paths

Replaced all absolute `/Users/udi/Grassroots/GLP/...` paths with repo-relative paths (`../programs/stdlib`, `../programs/typed_book/social_graph/...`).
