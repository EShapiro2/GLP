# Single-Window Isolate Migration Plan

## Goal

Migrate `glp_multiagent` from multi-window (separate FlutterEngine per agent) to single-window with Dart isolates (one per agent). This eliminates the unfixable macOS FlutterEngine focus bug where secondary engines freeze all input after Cmd-Tab.

## Architecture

**Current:** Coordinator window → spawns agent windows via `DesktopMultiWindow.createWindow()` → routes messages via method channels between windows.

**New:** Single window with agent panels side-by-side → spawns agent isolates via `Isolate.spawn()` → routes messages via `SendPort`/`ReceivePort`.

Hub-and-spoke topology is preserved: all inter-agent messages go through the main isolate (same as current coordinator routing).

## Message Protocol

New file `isolate_protocol.dart`:

**Main → Agent isolate:**
- `InitAgent` — carries agentId, glpSource, friends, replyPort
- `UserInput` — user typed text
- `DeliverMad` — incoming MAD message (from, payload)
- `DisposeAgent` — graceful shutdown

**Agent isolate → Main:**
- `AgentReady` — agent initialized, carries commandPort
- `AgentOutput` — output line for display
- `AgentLog` — trace log entry
- `AgentSendMad` — outbound MAD message (to, payload)
- `AgentStats` — goals/heap/wp/mp counters
- `AgentError` — initialization or runtime error

## Agent Isolate Entry Point

Top-level function `agentIsolateEntry(InitAgent init)`:
1. Creates `ReceivePort` for commands
2. Creates `AgentRuntime` (pure Dart, no Flutter dependency)
3. Wires `onOutput`, `onLog`, `onSendMadMessage` callbacks to send `FromAgentMsg` via `init.replyPort`
4. Sends `AgentReady` with commandPort
5. Calls `agent.initialize()`
6. Listens for `UserInput`, `DeliverMad`, `DisposeAgent` on commandPort

AgentRuntime is used as-is — no modifications needed. It already has the right callback interface.

## IsolateRouter (replaces MadRouter)

Rewrite `mad_router.dart`:
- Maps `agentId → SendPort` instead of `agentId → windowId`
- `route()` calls `targetPort.send(DeliverMad(...))` instead of `DesktopMultiWindow.invokeMethod`
- No base64 encoding needed — `Uint8List` transfers directly through SendPorts

## UI Layout

Single window with:
```
+---------------------------------------------------------------+
| GLP Coordinator (Isolate Mode)                                 |
+---------------------------------------------------------------+
| GLP dir: [____________________________] [Set]                  |
| [Alice↔Bob↔Charlie]  [Close All]                              |
+---------------------------------------------------------------+
|  Alice (Bob)       |  Bob (Alice,Charlie) |  Charlie (Bob)    |
| +-output log-----+ | +-output log------+ | +-output log----+ |
| | < hello world  | | | < hello world   | | | < hello world | |
| | > connect(bob) | | | > connect(alice)| | | > connect(bob)| |
| +----------------+ | +-----------------+ | +---------------+ |
| [input___] [Send]  | [input___] [Send]   | [input___] [Send] |
| G:0 H:0 W:0 M:0   | G:0 H:0 W:0 M:0    | G:0 H:0 W:0 M:0  |
+--------------------+---------------------+--------------------+
| Routing log                                                    |
+---------------------------------------------------------------+
```

Each agent panel reuses the exact same output log + input + status bar UI from the current AgentScreen.

`AgentState` class holds per-agent UI state (outputLog, controllers, focus node, stats).

## Files Changed

| File | Action |
|------|--------|
| `lib/isolate_protocol.dart` | NEW — message classes + isolate entry |
| `lib/main.dart` | REWRITE — single window, isolate spawning, panel UI |
| `lib/mad_router.dart` | REWRITE → IsolateRouter with SendPort |
| `pubspec.yaml` | EDIT — remove desktop_multi_window |
| `macos/Runner/AppDelegate.swift` | SIMPLIFY — remove swizzle + FocusRestorer |
| `macos/Runner/FocusRestorer.swift` | DELETE |
| `macos/Runner/Info.plist` | EDIT — remove FLTEnableMergedPlatformUIThread |

## Files NOT Changed

- `glp_runtime/lib/multiagent/agent_runtime.dart` — used as-is inside isolates
- `glp_runtime/lib/multiagent/isolate_manager.dart` — untouched, used by headless tests
- Any GLP programs

## Implementation Order

1. Create `isolate_protocol.dart`
2. Rewrite `mad_router.dart` → IsolateRouter
3. Rewrite `main.dart` (remove multi-window, add isolates + panel UI)
4. Edit `pubspec.yaml`
5. Simplify macOS native files
6. `flutter clean && flutter pub get && flutter build macos`
7. Manual test: spawn agents, type commands, Cmd-Tab, verify input works
