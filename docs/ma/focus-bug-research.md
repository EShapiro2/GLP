# Research Task: Flutter desktop_multi_window Focus Bug

## Problem

We have a Flutter macOS desktop app using the `desktop_multi_window` package (v0.2.1) to spawn multiple child windows. Each child window has a `TextField` for user input.

**Bug:** After the user switches away from a child window (e.g., to a terminal or another app) and switches back, the `TextField` in the child window no longer accepts keyboard input. The user cannot type or paste.

## What We've Tried (None Worked)

1. `FocusNode.requestFocus()` after `_sendInput()` — doesn't help when returning from another app
2. `GestureDetector.onTap` → `requestFocus()` — click in output area doesn't restore typing
3. `WidgetsBindingObserver.didChangeAppLifecycleState` → `requestFocus()` on `resumed` — may not fire for child windows in multi-window apps
4. `MouseRegion.onEnter` → `requestFocus()` — fires but doesn't restore keyboard input
5. `Listener.onPointerDown` → `requestFocus()` — fires but doesn't restore keyboard input
6. `WindowController.fromWindowId(id).show()` (which calls native `makeKeyAndOrderFront` + `NSApp.activate`) followed by `requestFocus()` — doesn't help
7. Replacing `SelectableText` with `Text` in the output area to prevent focus stealing — doesn't help

## Setup

- Flutter macOS desktop app
- `desktop_multi_window` package v0.2.1 (https://pub.dev/packages/desktop_multi_window)
- Child windows spawned via `DesktopMultiWindow.createWindow()`
- Each child window is a separate Flutter `MaterialApp` with its own widget tree
- The child window contains: AppBar, output ListView (Text widgets), TextField with FocusNode, status bar
- `autofocus: true` is set on the TextField

## What We Need to Know

1. What is the correct way to ensure a `TextField` in a `desktop_multi_window` child window regains keyboard focus after the user switches to another app and back?
2. Is this a known issue with `desktop_multi_window`? Are there workarounds?
3. Is there a native macOS approach (e.g., NSWindow delegate methods, `becomeFirstResponder`) that needs to be called from the Flutter side?
4. Would upgrading to `desktop_multi_window` v0.3.0 help?
5. Alternative packages or approaches for multi-window Flutter desktop apps that handle focus correctly?

## Key Constraint

The child windows are spawned as separate processes — they share no Flutter state with the coordinator window. Each child window has its own `runApp()` call.
