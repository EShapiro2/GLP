# GLP Multiagent UI — HOW TO RUN

**Updated: 2026-02-16**

## Build and Launch

**IMPORTANT: Use release build, not `flutter run`.**
Debug mode (`flutter run -d macos`) has a different timing for the Flutter engine
and the focus fix does not work reliably. Always use release build.

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

## Spawning Agents

1. The **coordinator window** opens first.
2. Click **"Alice↔Bob↔Charlie"** to spawn three agent windows.
3. Each agent window has a text input field at the bottom for commands.

## Focus Fix

The child window keyboard focus fix is in
`macos/Runner/AppDelegate.swift` — it uses `DispatchQueue.main.async`
to defer `makeFirstResponder` so AppKit finishes its responder-chain
updates before we override first responder. Without this, child windows
(spawned by `desktop_multi_window`) silently lose keyboard input.

## Interactive Protocol — The Full Introduction Script

The protocol below matches the automated actor scripts in
`typed_ui_actors.glp` (`alice_ui_actor`, `bob_ui_actor`, `charlie_ui_actor`).
The ui_mediator replaces non-ground variables with `req(N)` identifiers,
so the user types ground terms only.

### Step 1 — Alice: cold-call Bob

Type in **Alice's** window:
```
connect(bob)
```
**Wait for:** Bob's window shows `befriend(alice, req(1))`.

### Step 2 — Bob: accept Alice's friend request

Type in **Bob's** window (use the req number from Bob's output):
```
decision(yes, alice, req(1))
```
**Wait for:** Both Alice and Bob show `connected(...)`.

### Step 3 — Alice: send message to Bob

Type in **Alice's** window:
```
send(bob, 'Hi Bob, this is Alice')
```
**Wait for:** Bob shows `received(alice, Hi Bob, this is Alice)`.

### Step 4 — Bob: cold-call Charlie

Type in **Bob's** window:
```
connect(charlie)
```
**Wait for:** Charlie shows `befriend(bob, req(1))`.

### Step 5 — Charlie: accept Bob's friend request and greet

Type in **Charlie's** window (use the req number from Charlie's output):
```
decision(yes, bob, req(1))
```
Then:
```
send(bob, 'Hi Bob, this is Charlie')
```
**Wait for:** Both Bob and Charlie show `connected(...)`. Bob shows `received(charlie, ...)`.

### Step 6 — Bob: introduce Alice to Charlie

Type in **Bob's** window:
```
introduce(alice, charlie)
```
**Wait for:**
- Alice shows `befriend_intro(bob, charlie, req(N))`
- Charlie shows `befriend_intro(bob, alice, req(N))`

Note the req numbers — they may differ between Alice and Charlie.

### Step 7 — Alice: accept introduction to Charlie

Type in **Alice's** window (use Alice's req number from step 6):
```
accept_intro(charlie, req(N))
```

### Step 8 — Charlie: accept introduction to Alice

Type in **Charlie's** window (use Charlie's req number from step 6):
```
accept_intro(alice, req(N))
```
**Wait for:** Both Alice and Charlie show `connected(...)`.
Alice and Charlie are now direct friends via Bob's introduction.

### Step 9 — Verify: Alice sends to Charlie

Type in **Alice's** window:
```
send(charlie, 'Hi Charlie, this is Alice')
```
**Wait for:** Charlie shows `received(alice, ...)`.

### Step 10 — Verify: Charlie sends to Alice

Type in **Charlie's** window:
```
send(alice, 'Hi Alice, this is Charlie')
```
**Wait for:** Alice shows `received(charlie, ...)`.

## Architecture

- **Coordinator** (`CoordinatorApp`): spawns agent windows, routes
  madGLP binary messages between them via `MadRouter`.
- **Agent windows** (`AgentApp`): each runs `AgentRuntime` with its own
  GLP engine and `MadContext`. User input is parsed and injected via
  `InputInjector`.
- **GLP files loaded** (in order):
  1. `typed_social_agent.glp` — agent/4, channel ops, merge, helpers (typed)
  2. `typed_ui_mediator.glp` — ground-term mediator between agent/4 and Dart (typed)
  3. `play_ui_boot.glp` — interactive boot: `agent_init(Id, UserIn, NetIn)`
- **GLP directory**: `/Users/udi/Grassroots/GLP/programs/typed_book/social_graph`
- **Friends list in coordinator** is display-only — it does NOT set up
  GLP-level friendships. Friendships form only through `connect` commands.

## Headless Testing (no Flutter)

The same protocol runs headlessly via automated UI actors:

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime

# madGLP with mediator + UI actors (all 3 agents complete)
dart test test/multiagent/isolate_manager_test.dart -n "UI mediator"

# madGLP without mediator (actors talk directly to agent/4)
dart test test/multiagent/isolate_manager_test.dart -n "no UI"

# All multiagent tests (71 pass, 5 skip, 1 pre-existing fail)
dart test test/multiagent/
```

## Known Issues

- **Mediator pending-key bug (FIXED 2026-02-13)**: The mediator previously stored bare `N`
  as the pending key but sent `req(N)` to the user. Fixed: mediator now stores `req(N)` as key.
- **Mediator double-reader bug (FIXED 2026-02-16)**: The mediator's pending list created
  reader-of-reader indirection when passing opaque variables back to the agent. Fixed by
  wrapping pending values in `PendingValue ::= response(Response?) ; channel(Channel?) ; error`
  and having the agent unwrap them. Full 10-step interactive script now verified working.
- **Focus fix requires release build**: The `DispatchQueue.main.async`
  focus fix in `AppDelegate.swift` works in release mode but not reliably
  in debug mode (`flutter run -d macos`).

## File Locations

| File | Purpose |
|------|---------|
| `lib/main.dart` | Coordinator + Agent window code |
| `lib/mad_router.dart` | Message routing between windows |
| `macos/Runner/AppDelegate.swift` | Focus fix (makeFirstResponder) |
| `macos/Runner/MainFlutterWindow.swift` | Main window setup |
| `pubspec.yaml` | Dependencies (glp_runtime, desktop_multi_window) |

## Related GLP Files

| File | Purpose |
|------|---------|
| `typed_social_agent.glp` | agent/4, channel ops, merge, response handling |
| `typed_ui_mediator.glp` | Ground-term mediator (agent/4 ↔ Dart UI) |
| `typed_actors.glp` | Scripted actors — talk directly to agent/4 |
| `typed_ui_actors.glp` | Scripted UI actors — talk to ui_mediator (ground terms) |
| `play_ui_boot.glp` | Flutter UI boot: agent_init/3 |
| `play_ui_dglp_boot.glp` | dGLP boot with mediator (single-isolate REPL) |
| `play_ui_madglp_boot.glp` | madGLP boot with mediator (headless multi-isolate) |
| `play_dglp_boot.glp` | dGLP boot without mediator (single-isolate REPL) |
| `play_madglp_boot.glp` | madGLP boot without mediator (headless multi-isolate) |
