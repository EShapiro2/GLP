# GLP Multiagent UI — HOW TO RUN

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
`social_agent.glp` (`alice_actor`, `bob_actor`, `charlie_actor`).
The ui_mediator replaces non-ground writers with `req(N)` identifiers,
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
send(bob, hi)
```
**Wait for:** Bob shows `received(alice, hi)`.

### Step 4 — Bob: cold-call Charlie

Type in **Bob's** window:
```
connect(charlie)
```
**Wait for:** Charlie shows `befriend(bob, req(1))`.

### Step 5 — Charlie: accept Bob's friend request

Type in **Charlie's** window (use the req number from Charlie's output):
```
decision(yes, bob, req(1))
```
**Wait for:** Both Bob and Charlie show `connected(...)`.

### Step 6 — Charlie: send message to Bob

Type in **Charlie's** window:
```
send(bob, hi)
```
**Wait for:** Bob shows `received(charlie, hi)`.

### Step 7 — Bob: introduce Alice to Charlie

Type in **Bob's** window:
```
introduce(alice, charlie)
```
**Wait for:**
- Alice shows `befriend_intro(bob, charlie, req(N))`
- Charlie shows `befriend_intro(bob, alice, req(N))`

Note the req numbers — they may differ between Alice and Charlie.

### Step 8 — Alice: accept introduction to Charlie

Type in **Alice's** window (use Alice's req number from step 7):
```
accept_intro(charlie, req(N))
```

### Step 9 — Charlie: accept introduction to Alice

Type in **Charlie's** window (use Charlie's req number from step 7):
```
accept_intro(alice, req(N))
```
**Wait for:** Both Alice and Charlie show `connected(...)`.
Alice and Charlie are now direct friends via Bob's introduction.

### Step 10 — Verify: Alice sends to Charlie

Type in **Alice's** window:
```
send(charlie, hi)
```
**Wait for:** Charlie shows `received(alice, hi)`.

### Step 11 — Verify: Charlie sends to Alice

Type in **Charlie's** window:
```
send(alice, hi)
```
**Wait for:** Alice shows `received(charlie, hi)`.

## Architecture

- **Coordinator** (`CoordinatorApp`): spawns agent windows, routes
  madGLP binary messages between them via `MadRouter`.
- **Agent windows** (`AgentApp`): each runs `AgentRuntime` with its own
  GLP engine and `MadContext`. User input is parsed and injected via
  `InputInjector`.
- **GLP files loaded** (in order):
  1. `social_agent.glp` — agent/4 main loop, channel ops, actors
  2. `ui_mediator.glp` — ground-term translation between agent and Dart
  3. `play_ui_boot.glp` — interactive boot: `agent_init(Id, UserIn, NetIn)`
- **GLP directory**: `/Users/udi/Grassroots/GLP/programs/typed_book/social_graph`
- **Friends list in coordinator** is display-only — it does NOT set up
  GLP-level friendships. Friendships form only through `connect` commands.

## Known Issues

- **Duplicate messages**: The `_reactivateSuspendedGoals()` method in
  `agent_runtime.dart` likely causes duplicate processing of incoming
  messages. See `/Users/udi/Grassroots/GLP/docs/ma/agent-runtime-spec.md`
  for analysis.
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
