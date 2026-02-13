# Focus Bug Deep Diagnosis — v2

Date: 2026-02-12

## Setup
- Flutter 3.41.0, Dart 3.11.0, macOS (Apple Silicon)
- `desktop_multi_window` v0.2.1 — child windows with separate FlutterEngine + FlutterViewController
- Merged thread mode disabled via `FLTEnableMergedPlatformUIThread = false` in Info.plist
- `FlutterView.acceptsFirstResponder` swizzled to always return `true`

## The Bug
Child windows lose keyboard input after Cmd-Tab away and back. Main window unaffected.

## What We've Confirmed (All Correct)

After Cmd-Tab back to child window:

| Check | Result |
|-------|--------|
| `windowDidBecomeKey` fires (NSWindowDelegate) | ✅ YES |
| `engine.windowDidBecomeKey:` called | ✅ YES |
| `engine.windowDidResignKey:` called on leave | ✅ YES |
| `makeFirstResponder(flutterView)` returns | ✅ SUCCESS |
| `FlutterView.acceptsFirstResponder` | ✅ true (swizzled) |
| `TIP.isFirstResponder` | ✅ true |
| `TIP.currentViewController` matches expected VC | ✅ same object |
| `TIP.textInputContext` | ✅ not nil |

## Conclusion

Everything at both the AppKit level and the Flutter engine internal level appears correct:
- The responder chain is correct
- The engine knows the window is key
- The TextInputPlugin thinks it is first responder
- The TextInputPlugin is attached to the correct ViewController
- The NSTextInputContext exists

Yet keyboard input does not reach the Dart TextField.

## Engine Methods Available

On `FlutterEngine`:
- `windowDidBecomeKey:` / `windowDidResignKey:`
- `onFocusChangeRequest:`
- `textInputPlugin` (property)
- `keyboardManager` (property)
- `sendKeyEvent:callback:userData:`

On `FlutterTextInputPlugin`:
- `isFirstResponder`
- `currentViewController` / `textInputContext` / `setTextInputContext:`
- `client` / `setClient:`
- `handleMethodCall:result:`
- `setEditingState:` / `editingState` / `updateEditState`
- `inputContext`
- `keyDown:` / `keyUp:` / `handleKeyEvent:`
- `insertText:replacementRange:`
- `resignAndRemoveFromSuperview`

## Cross-Version Testing (2026-02-12)

Downgraded Flutter from 3.41.0 to 3.32.8 (pre-merged-thread, released 2025-07-25).
**Same bug**. This proves:
- The bug is NOT caused by Flutter's merged UI/platform thread mode (introduced in 3.35)
- The bug is NOT a regression from any recent Flutter version
- The bug is inherent to how `desktop_multi_window` creates child windows with separate engines

## What Has Been Tried (All Failed)

1. **FlutterView.acceptsFirstResponder swizzle** → returns true, makeFirstResponder succeeds, but keyboard still dead
2. **FlutterKeyboardManager.reset()** → called successfully on windowDidBecomeKey, no effect
3. **engine.windowDidBecomeKey:/windowDidResignKey:** → called successfully, no effect
4. **Synthetic mouse events** → NSEvent mouseDown/mouseUp posted to child window, no effect
5. **TextInput.hide + unfocus + refocus** (Dart side, via WidgetsBindingObserver) → no effect
6. **FLTEnableMergedPlatformUIThread = false** (Info.plist) → disables merged threads, but bug persists
7. **Force cycling first responder** (resign + re-make first responder) → no effect
8. **NSTextInputContext.activate()** → crashed when combined with other fixes
9. **Flutter version downgrade to 3.32.8** → same bug

## Additional Findings (late 2026-02-12)

10. **Synthetic mouse events via window.sendEvent()** → no effect
11. **NSEvent local key monitor** → intercepted keyDown events, forwarded via window.sendEvent() → engine ignores
12. **NSTextInputContext.activate() + makeFirstResponder(TIP)** → crashes on Cmd-Tab back
13. **Window orderOut + makeKeyAndOrderFront** → visible flash, bug persists

**CRITICAL**: This is NOT just a keyboard focus issue. After Cmd-Tab, clicking the text
field does NOT activate the cursor. The engine stops processing ALL input (keyboard AND mouse/tap).

Events ARE being delivered to the NSWindow (confirmed via NSEvent local monitor).
The Flutter engine inside is simply not processing them.

## Conclusion

The bug is an unfixable Flutter engine-level issue: secondary FlutterEngine instances
do not properly resume event processing after macOS app deactivation/reactivation.
The main engine resumes correctly; child engines do not.

**Decision**: Bypass by migrating to single-window architecture with Dart isolates.
See `/GLP/docs/ma/focus-bug-final-status.md` for full status and migration plan.
