# GLP Multiagent UI — HOW TO RUN

**Updated: 2026-02-21**

## Four apps, four entry points

| App | Entry point | Build target |
|---|---|---|
| Interactive SG (multi-window) | `lib/main.dart` | default |
| CSSG Plays (REPL subprocess) | `lib/main_cssg.dart` | `--dart-define=ENTRY=cssg` or `--target=lib/main_cssg.dart` |
| SG Plays (madGLP multi-isolate) | `lib/main_sg_mad.dart` | `--target=lib/main_sg_mad.dart` |
| CSSG Plays (madGLP multi-isolate) | `lib/main_cssg_mad.dart` | `--target=lib/main_cssg_mad.dart` |

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

### SG Plays — madGLP (multi-isolate)

```bash
cd /Users/udi/Grassroots/GLP/glp_multiagent && flutter build macos --release --target=lib/main_sg_mad.dart 2>&1
```

Launch same path as above — the build replaces the binary.

### CSSG Plays — REPL

```bash
cd /Users/udi/Grassroots/GLP/glp_multiagent && flutter build macos --release --target=lib/main_cssg.dart 2>&1
```

### CSSG Plays — madGLP (multi-isolate)

```bash
cd /Users/udi/Grassroots/GLP/glp_multiagent && flutter build macos --release --target=lib/main_cssg_mad.dart 2>&1
```

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
- **CSSG REPL** (`main_cssg.dart`): Spawns `glp_repl` subprocess, parses
  tagged output, routes to per-agent panels.
- **SG/CSSG madGLP** (`main_sg_mad.dart`, `main_cssg_mad.dart`): Single
  window, each agent in its own Dart isolate via `AgentRuntime` +
  `IsolateRouter`. Uses two-phase deferred-start protocol documented in
  `isolate_protocol.dart`.

## File locations

| File | Purpose |
|---|---|
| `lib/main.dart` | Interactive SG — coordinator + agent windows |
| `lib/main_cssg.dart` | CSSG plays via REPL subprocess |
| `lib/main_sg_mad.dart` | SG plays via multi-isolate madGLP |
| `lib/main_cssg_mad.dart` | CSSG plays via multi-isolate madGLP |
| `lib/isolate_protocol.dart` | Isolate protocol: message types, lifecycle, entry point |
| `lib/mad_router.dart` | `IsolateRouter` — cross-isolate MAD message routing |
| `macos/Runner/AppDelegate.swift` | Focus fix (makeFirstResponder) |
| `pubspec.yaml` | Dependencies (glp_runtime, desktop_multi_window) |

## Related GLP files

All in `/Users/udi/Grassroots/GLP/programs/typed_book/cssg/`.

| File | Purpose |
|---|---|
| `typed_social_agent.glp` | `agent/4`, channel ops, merge, response handling |
| `typed_ui_mediator.glp` | Ground-term mediator (`agent/4` ↔ Dart UI) |
| `typed_ui_actors.glp` | Scripted UI actors — talk to `ui_mediator` (ground terms) |
| `play_ui_boot.glp` | Interactive Flutter UI boot: `agent_init/3` |
| `play_ui_madglp_boot.glp` | madGLP boot with mediator + actors (multi-isolate Flutter UI) |
| `play_ui_dglp_boot.glp` | dGLP boot with mediator (single-isolate REPL) |
| `play_dglp_boot.glp` | dGLP boot without mediator (single-isolate REPL) |
| `play_madglp_boot.glp` | madGLP boot without mediator (headless multi-isolate) |

## Known issues

- **madGLP plays stall after introduction step (OPEN)**: See README.md for details.
- **Focus fix requires release build**: The `DispatchQueue.main.async` focus
  fix in `AppDelegate.swift` works in release mode but not reliably in debug
  mode (`flutter run -d macos`).
- **Mediator pending-key bug (FIXED 2026-02-13)**: Mediator stored bare `N`
  as pending key but sent `req(N)` to user. Fixed.
- **Mediator double-reader bug (FIXED 2026-02-16)**: Fixed by wrapping pending
  values in `PendingValue` and having the agent unwrap them.
