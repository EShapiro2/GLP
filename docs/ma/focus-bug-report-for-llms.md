# Flutter macOS Multi-Window Focus Bug — Detailed Report (v2)

## Request

We have been unable to fix this bug after extensive investigation (12+ approaches tried). Please suggest fundamentally new approaches. We need concrete, actionable suggestions — not things we've already done.

## The Bug

In a Flutter macOS app using the `desktop_multi_window` plugin (v0.2.1), child windows lose ALL input (keyboard AND mouse/tap) after Cmd-Tab to another app and back. The main window is NOT affected — only child windows.

**CRITICAL NEW FINDING**: This is NOT just a keyboard focus issue. After Cmd-Tab back:
- Keyboard is dead (no characters appear)
- Clicking the text field does NOT activate it — the text cursor does NOT appear
- The Flutter engine in the child window stops processing ALL input events
- macOS-level events DO reach the window (it comes forward, becomes key) but Flutter ignores them

## Architecture

The `desktop_multi_window` plugin creates child windows like this:
- Each child window gets its own `NSWindow`
- Each child window gets its own `FlutterDartProject` + `FlutterViewController` + `FlutterEngine`
- The child window's `FlutterViewController` is set as `window.contentViewController`
- The child window has an `NSWindowDelegate` (on the FlutterWindow class)

```swift
init(id: Int64, arguments: String) {
    windowId = id
    window = NSWindow(
      contentRect: NSRect(x: 0, y: 0, width: 480, height: 270),
      styleMask: [.miniaturizable, .closable, .resizable, .titled, .fullSizeContentView],
      backing: .buffered, defer: false)
    let project = FlutterDartProject()
    project.dartEntrypointArguments = ["multi_window", "\(windowId)", arguments]
    let flutterViewController = FlutterViewController(project: project)
    window.contentViewController = flutterViewController
    // ... plugin registration ...
    window.delegate = self
    window.isReleasedWhenClosed = false
}
```

## Reproduction Steps

1. Launch the app. Main window appears with a text field.
2. Click a button to spawn a child window (separate engine). Child window has a text field.
3. Click into the child window's text field. Type — keyboard works.
4. Cmd-Tab to another app (e.g., Terminal).
5. Cmd-Tab back to the child window.
6. Try to type — **keyboard is dead**. No characters appear.
7. Click on the text field — **cursor does NOT appear**. The text field does not visually activate. Flutter's event processing is completely frozen for this window.

## Flutter Versions Tested

- **Flutter 3.41.0** (latest, Dart 3.11.0) — bug present
- **Flutter 3.32.8** (pre-merged-thread, Dart 3.8.1, released 2025-07-25) — **same bug**

This proves the bug is NOT caused by Flutter's merged UI/platform thread mode (introduced in 3.35).

## What We've Verified After Cmd-Tab Back

| Check | Result |
|-------|--------|
| `windowDidBecomeKey` fires (NSWindowDelegate) | YES |
| `engine.windowDidBecomeKey:` (private API) called | YES |
| `window.makeFirstResponder(flutterView)` returns | SUCCESS |
| `FlutterView.acceptsFirstResponder` | true (swizzled) |
| `TextInputPlugin.isFirstResponder` | true |
| `TextInputPlugin.currentViewController` | matches expected VC |
| `TextInputPlugin.textInputContext` | not nil |
| `FlutterKeyboardManager.reset()` called | YES, succeeds |
| NSEvent local monitor intercepts keyDown events | YES — events reach the window |
| `window.sendEvent(keyEvent)` called manually | YES — no effect |

**Events ARE being delivered to the NSWindow. The Flutter engine inside is not processing them.**

## What We've Tried (All Failed)

1. **FlutterView.acceptsFirstResponder swizzle** → swizzled to return true. No effect on the bug.
2. **FlutterKeyboardManager.reset()** → called on windowDidBecomeKey. No effect.
3. **engine.windowDidBecomeKey:/windowDidResignKey:** → private selectors called. No effect.
4. **Synthetic mouse events via window.sendEvent()** → NSEvent mouseDown/mouseUp posted. No effect.
5. **Synthetic mouse events via NSEvent.addLocalMonitorForEvents** → intercepted and re-sent. No effect.
6. **TextInput.hide + unfocus + refocus (Dart side)** → via WidgetsBindingObserver. No effect.
7. **FLTEnableMergedPlatformUIThread = false** → disabled merged threads. Bug persists.
8. **Force cycling first responder** → resign + re-make. No effect.
9. **NSTextInputContext.activate()** → crashes on Cmd-Tab back.
10. **makeFirstResponder(TextInputPlugin)** → TIP is NSView, makeFirstResponder succeeds. Crashes on return.
11. **Window orderOut + makeKeyAndOrderFront** → hide and re-show window. Causes flash, bug persists.
12. **Flutter version downgrade to 3.32.8** → same bug.
13. **NSEvent local key monitor + manual window.sendEvent()** → events intercepted and forwarded. Engine ignores them.

## Key Insight

The problem is NOT in event routing or the responder chain. Events DO reach the window. The problem is that the **FlutterEngine/FlutterViewController stops processing events** after the app is deactivated and reactivated. This is an engine-level freeze, not a focus issue.

This likely means:
- The FlutterEngine's run loop or event processing is paused/suspended when the app goes to background
- For the MAIN window's engine, Flutter properly resumes processing on app activation
- For CHILD window engines (created by the plugin), this resume mechanism doesn't work
- The engine may rely on NSApplication lifecycle notifications that only reach the main FlutterViewController

## Available Engine Methods (via Runtime Introspection)

**FlutterEngine:** `windowDidBecomeKey:`, `windowDidResignKey:`, `onFocusChangeRequest:`, `textInputPlugin`, `keyboardManager`, `sendKeyEvent:callback:userData:`

**FlutterTextInputPlugin:** `isFirstResponder`, `currentViewController`, `textInputContext`, `handleMethodCall:result:`, `setEditingState:`, `keyDown:`, `handleKeyEvent:`, `insertText:replacementRange:`

**FlutterKeyboardManager:** `reset`, `handleEvent:withContext:`, `processNextEvent`, `pendingEvents`, `processingEvent`, `getPressedState`

## Questions

1. What mechanism does Flutter use to pause/resume engine event processing on app deactivation/reactivation on macOS?
2. How does the main window's engine get resumed but child engines don't?
3. Is there a method on FlutterEngine to explicitly resume event processing (similar to `flutterEngine.lifecycleChannel` on iOS)?
4. Would using a SHARED FlutterEngine across all windows (instead of one engine per window) avoid this bug?
5. Is there a way to notify the child engine that the app has been reactivated, triggering the same resume path the main engine uses?

## Environment

- macOS (Apple Silicon), Flutter 3.32.8 / 3.41.0
- Plugin: `desktop_multi_window` v0.2.1
- Test app: simple app with button to spawn child window, each window has a TextField
