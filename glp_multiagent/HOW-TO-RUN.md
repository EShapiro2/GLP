# GLP Multiagent UI — HOW TO RUN

**Updated: 2026-08-01**

## The apps this file covers

| App | Entry point | Build target |
|---|---|---|
| Interactive SG (multi-window) | `lib/main.dart` | default |
| CSSN group plays (REPL subprocess) | `lib/main_cssg.dart` | `--target=lib/main_cssg.dart` |

The two madGLP multi-isolate apps, `lib/main_sg_mad.dart` and
`lib/main_cssg_mad.dart`, and `lib/main_cssg_mad_modules.dart` beside them, were
retired on 2026-08-01 with the program directories they named.  `lib/` holds
further entry points this file does not cover.

## Build and launch

**IMPORTANT: Use release build, not `flutter run`.**
Debug mode has different timing for the Flutter engine and the focus fix
does not work reliably in the multi-window app.  Always use release build.

### Interactive SG (multi-window) — the default app

```bash
cd /Users/udi/Grassroots/GLP/glp_multiagent && flutter build macos --release 2>&1
```

Launch:

```bash
open /Users/udi/Grassroots/GLP/glp_multiagent/build/macos/Build/Products/Release/glp_multiagent.app
```

### CSSN group plays — REPL

```bash
cd /Users/udi/Grassroots/GLP/glp_multiagent && flutter build macos --release --target=lib/main_cssg.dart 2>&1
```

Launch same path as above — the build replaces the binary.

### Kill a running instance

```bash
pkill -f glp_multiagent
```

## Interactive SG — using the multi-window app

1. The **coordinator window** opens first.
2. Click **"Alice↔Bob↔Charlie"** to spawn three agent windows.
3. Each agent window has a text input field at the bottom for commands.

### Full introduction script

The protocol below matches the automated actor scripts in
`typed_ui_actors.glp` (`alice_ui_actor`, `bob_ui_actor`, `charlie_ui_actor`).
The ui_mediator replaces non-ground variables with `req(N)` identifiers,
so the user types ground terms only.

**Step 1** — Alice: `connect(bob)` → wait for Bob to show `befriend(alice, req(1))`

**Step 2** — Bob: `decision(yes, alice, req(1))` → wait for both to show `connected(...)`

**Step 3** — Alice: `send(bob, 'Hi Bob, this is Alice')` → Bob shows `received(alice, ...)`

**Step 4** — Bob: `connect(charlie)` → Charlie shows `befriend(bob, req(1))`

**Step 5** — Charlie: `decision(yes, bob, req(1))` then `send(bob, 'Hi Bob, this is Charlie')`

**Step 6** — Bob: `introduce(alice, charlie)` → Alice shows `befriend_intro(bob, charlie, req(N))`, Charlie shows `befriend_intro(bob, alice, req(N))`

**Step 7** — Alice: `accept_intro(charlie, req(N))`

**Step 8** — Charlie: `accept_intro(alice, req(N))` → both show `connected(...)`

**Step 9** — Alice: `send(charlie, 'Hi Charlie, this is Alice')` → Charlie shows `received(alice, ...)`

**Step 10** — Charlie: `send(alice, 'Hi Alice, this is Charlie')` → Alice shows `received(charlie, ...)`

## SG / CSSG Plays — using the madGLP apps

These are non-interactive.  Click a Play button (Play 1/2/3 for SG, Play 4/5/6/7 for CSSG) and watch the agents' panels fill with tagged output.

## Focus fix (multi-window app only)

The child window keyboard focus fix is in
`macos/Runner/AppDelegate.swift` — it uses `DispatchQueue.main.async`
to defer `makeFirstResponder` so AppKit finishes its responder-chain
updates before we override first responder. Without this, child windows
(spawned by `desktop_multi_window`) silently lose keyboard input.
Requires release build.

## Headless testing (no Flutter)

The same protocol runs headlessly via automated UI actors:

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime

# madGLP with mediator + UI actors (all 3 agents complete)
dart test test/multiagent/isolate_manager_test.dart -n "UI mediator"

# madGLP without mediator (actors talk directly to agent/4)
dart test test/multiagent/isolate_manager_test.dart -n "no UI"

# All multiagent tests
dart test test/multiagent/
```

## Architecture

- **Interactive SG** (`main.dart`): Coordinator spawns agent windows via
  `desktop_multi_window`, routes madGLP binary messages between them via
  `MadRouter`. Each window runs `AgentRuntime` + `MadContext`.
- **CSSN group plays** (`main_cssg.dart`): Spawns `glp_repl` subprocess, parses
  tagged output, routes to per-agent panels.

## File locations

| File | Purpose |
|---|---|
| `lib/main.dart` | Interactive SG — coordinator + agent windows |
| `lib/main_cssg.dart` | CSSN group plays via REPL subprocess |
| `lib/isolate_protocol.dart` | Isolate protocol: message types, lifecycle, entry point |
| `lib/mad_router.dart` | `IsolateRouter` — cross-isolate MAD message routing |
| `macos/Runner/AppDelegate.swift` | Focus fix (makeFirstResponder) |
| `pubspec.yaml` | Dependencies (glp_runtime, desktop_multi_window) |

## Related GLP files

Each app names its own.  `main_cssg.dart` runs `programs/book/cssn`; `main.dart`
takes its sources from `lib/glp_sources.dart`.  The table that stood here listed
`programs/typed_book/cssg`, a directory that has not existed for some time.

## Known issues

- **madGLP plays stall after introduction step (OPEN)**: Observed in the retired
  `main_sg_mad.dart`; see README.md for details.
- **Focus fix requires release build**: The `DispatchQueue.main.async` focus
  fix in `AppDelegate.swift` works in release mode but not reliably in debug
  mode (`flutter run -d macos`).
- **Mediator pending-key bug (FIXED 2026-02-13)**: Mediator stored bare `N`
  as pending key but sent `req(N)` to user. Fixed.
- **Mediator double-reader bug (FIXED 2026-02-16)**: Fixed by wrapping pending
  values in `PendingValue` and having the agent unwrap them.
