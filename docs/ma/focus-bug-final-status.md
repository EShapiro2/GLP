# Focus Bug — Final Status Report

Date: 2026-02-12

## Summary

After extensive investigation (13+ approaches across two Flutter versions), the macOS
child window focus bug in `desktop_multi_window` is confirmed to be an unfixable
Flutter engine-level issue. The decision is to **bypass the bug entirely** by
migrating from multi-window (separate engines) to a single-window architecture
with Dart isolates.

## The Bug

Child windows created by `desktop_multi_window` v0.2.1 lose ALL input (keyboard
AND mouse/tap) after Cmd-Tab to another app and back. The Flutter engine in the
child window stops processing events entirely. The main window is never affected.

This is NOT just a keyboard focus issue — after Cmd-Tab back, clicking the text
field does not activate the cursor. The engine is frozen for input.

## Root Cause Analysis

The bug is in Flutter's engine lifecycle management for secondary FlutterEngine
instances. When the macOS app is deactivated (Cmd-Tab away) and reactivated
(Cmd-Tab back):

1. The main window's engine resumes event processing normally
2. Child window engines (created by the plugin with separate FlutterEngine instances)
   do NOT resume — they stop processing all input events
3. macOS-level events DO reach the child window (NSWindow becomes key, NSWindowDelegate
   fires) but the Flutter engine inside ignores them
4. All engine state checks report correct values (responder chain, TextInputPlugin,
   KeyboardManager) — the engine just doesn't process events

This bug exists on both Flutter 3.41.0 (latest) and Flutter 3.32.8 (pre-merged-thread),
proving it is NOT related to the merged UI/platform thread mode introduced in 3.35.
It is a long-standing architectural limitation of running multiple FlutterEngine
instances in a single macOS application.

## What Was Tried (All Failed)

| # | Approach | Result |
|---|----------|--------|
| 1 | FlutterView.acceptsFirstResponder swizzle | makeFirstResponder succeeds, engine ignores events |
| 2 | FlutterKeyboardManager.reset() | Called successfully, no effect |
| 3 | engine.windowDidBecomeKey:/windowDidResignKey: | Private API called, no effect |
| 4 | Synthetic mouse events (window.sendEvent) | Events sent, engine ignores them |
| 5 | NSEvent local monitor + manual forwarding | Events intercepted and re-sent, engine ignores |
| 6 | TextInput.hide + unfocus + refocus (Dart side) | No effect |
| 7 | FLTEnableMergedPlatformUIThread = false | Disabled merged threads, bug persists |
| 8 | Force cycling first responder | Resign + re-make, no effect |
| 9 | NSTextInputContext.activate() | Crashes |
| 10 | makeFirstResponder(TextInputPlugin) | Crashes on Cmd-Tab back |
| 11 | Window orderOut + makeKeyAndOrderFront | Visible flash, bug persists |
| 12 | Flutter downgrade to 3.32.8 | Same bug |
| 13 | NSEvent local key monitor + window.sendEvent | Events delivered, engine ignores |

## Decision: Migrate to Single-Window Architecture

### Current Architecture (broken)
- Coordinator = main window (main engine)
- Each agent = separate child window (separate FlutterEngine)
- Communication via DesktopMultiWindow.invokeMethod (platform channels)
- Bug: child engines freeze after Cmd-Tab

### New Architecture (planned)
- Single Flutter window with split-pane layout
- Main isolate = UI + message router (coordinator)
- One Dart isolate per agent running AgentRuntime
- Communication via SendPort/ReceivePort
- No second FlutterEngine = no bug

### Why This Works
- AgentRuntime already does NOT use isolates internally
- AgentRuntime does synchronous GLP bytecode execution (can block) — needs its own isolate
- UI per agent is simple (output log + text input + status bar)
- Message routing is already centralized through the coordinator
- The main isolate can render all agent panels and relay messages

## Current State of Files

### Flutter
- Upgraded back to **Flutter 3.41.0** (Dart 3.11.0)

### glp_multiagent
- `pubspec.yaml`: points to `../desktop_multi_window_local` (will change when migrating)
- `macos/Runner/AppDelegate.swift`: has FlutterView.acceptsFirstResponder swizzle
- `macos/Runner/FocusRestorer.swift`: simplified to no-op
- `macos/Runner/Info.plist`: has FLTEnableMergedPlatformUIThread = false
- `lib/main.dart`: current multi-window architecture with coordinator + agent windows

### desktop_multi_window_local (local fork)
- `pubspec.yaml`: SDK constraint widened to `<4.0.0`
- `macos/Classes/FlutterWindow.swift`: cleaned to no-op windowDidBecomeKey

### focus_test
- `pubspec.yaml`: SDK constraint `^3.8.0`, points to local fork
- Used for testing, can be archived

## Next Steps

1. Plan and implement single-window isolate architecture for glp_multiagent
2. Create IsolateAgent wrapper (spawns isolate, manages SendPort/ReceivePort)
3. Replace CoordinatorScreen with unified layout (side-by-side agent panels)
4. Port MadRouter from window-IDs to isolate-ports
5. Remove desktop_multi_window dependency
6. Clean up native macOS code (remove swizzle, FocusRestorer, Info.plist key)

## Documentation

- `/GLP/docs/ma/focus-bug-diagnosis-v2.md` — technical diagnosis with all findings
- `/GLP/docs/ma/focus-bug-report-for-llms.md` — detailed report (v2) for AI assistants
- `/GLP/docs/ma/focus-bug-final-status.md` — this document
