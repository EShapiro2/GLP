# Claude Code Task: Fix macOS Child Window Keyboard Focus After Cmd-Tab

## Problem
Child windows in `desktop_multi_window` lose keyboard input after Cmd-Tab. Two co-occurring root causes:
1. Flutter's merged-thread mode starves NotificationCenter, so the engine never learns child windows regained key status.
2. FlutterKeyboardManager's async event queue gets stuck — a responder callback never returns after Cmd-Tab, blocking all subsequent key events.

The fix addresses both: reset the stuck queue, then restore engine focus state.

## What to do

### 1. Create local fork of desktop_multi_window plugin

```bash
cp -r ~/.pub-cache/hosted/pub.dev/desktop_multi_window-0.2.1 /Users/udi/Grassroots/GLP/desktop_multi_window_local
```

In `/Users/udi/Grassroots/GLP/glp_multiagent/pubspec.yaml`, change the `desktop_multi_window` dependency from `^0.2.0` to:

```yaml
  desktop_multi_window:
    path: ../desktop_multi_window_local
```

### 2. Find and modify the plugin's NSWindowDelegate

Look in `/Users/udi/Grassroots/GLP/desktop_multi_window_local/macos/Classes/` for the Swift file containing `extension FlutterWindow: NSWindowDelegate` with the `windowDidBecomeKey` method. (Likely `FlutterWindow.swift` or similar.)

**Replace** the `windowDidBecomeKey` implementation with:

```swift
func windowDidBecomeKey(_ notification: Notification) {
    guard let flutterVC = window.contentViewController as? FlutterViewController else { return }

    // 0. Reset FlutterKeyboardManager's event queue.
    //    After Cmd-Tab, async responder callbacks may never return,
    //    leaving the queue permanently stuck. This clears it.
    if let km = flutterVC.value(forKey: "keyboardManager") as? NSObject {
        let resetSel = NSSelectorFromString("reset")
        if km.responds(to: resetSel) {
            km.perform(resetSel)
        }
        // Resync modifier key state (Cmd/Alt/Ctrl often desync on Cmd-Tab)
        let syncSel = NSSelectorFromString("syncModifiersIfNeeded:timestamp:")
        if km.responds(to: syncSel) {
            typealias SyncFn = @convention(c) (AnyObject, Selector, NSEvent.ModifierFlags, TimeInterval) -> Void
            let imp = km.method(for: syncSel)
            let fn = unsafeBitCast(imp, to: SyncFn.self)
            fn(km, syncSel, NSEvent.modifierFlags, ProcessInfo.processInfo.systemUptime)
        }
    }

    // 1. Inform engine this window is key (private API).
    //    Under merged-thread mode, the VC's NotificationCenter observer never fires,
    //    so engine doesn't know the window regained focus.
    let becomeSel = NSSelectorFromString("windowDidBecomeKey:")
    if flutterVC.engine.responds(to: becomeSel) {
        flutterVC.engine.perform(becomeSel, with: NSNumber(value: flutterVC.viewIdentifier))
    }

    // 2. Reactivate NSTextInputContext (deactivated on Cmd-Tab)
    if let context = flutterVC.view.inputContext {
        context.activate()
    }

    // 3. Restore FlutterView as first responder
    window.makeFirstResponder(flutterVC.view)

    // 4. Ask Dart framework to re-establish text input connection
    let codec = FlutterJSONMethodCodec.sharedInstance()
    if let message = codec.encode(FlutterMethodCall(
        methodName: "TextInputClient.requestExistingInputState",
        arguments: nil
    )) {
        flutterVC.engine.binaryMessenger.send(onChannel: "flutter/textinput", message: message)
    }
}
```

**Add** `windowDidResignKey` if it doesn't already exist in that extension:

```swift
func windowDidResignKey(_ notification: Notification) {
    guard let flutterVC = window.contentViewController as? FlutterViewController else { return }

    let resignSel = NSSelectorFromString("windowDidResignKey:")
    if flutterVC.engine.responds(to: resignSel) {
        flutterVC.engine.perform(resignSel, with: NSNumber(value: flutterVC.viewIdentifier))
    }
}
```

**If `flutterVC.viewIdentifier` doesn't compile**, replace it with `0` (single-view engines use view ID 0).

### 3. Add Info.plist key for diagnostic testing

In `/Users/udi/Grassroots/GLP/glp_multiagent/macos/Runner/Info.plist`, add inside the top-level `<dict>`:

```xml
<key>FLTEnableMergedPlatformUIThread</key>
<false/>
```

This disables merged-thread mode to isolate the bug. Can be removed later once the fix is confirmed.

### 4. Simplify FocusRestorer.swift (it's dead code under merged threads)

Replace `/Users/udi/Grassroots/GLP/glp_multiagent/macos/Runner/FocusRestorer.swift` with:

```swift
import Cocoa
import FlutterMacOS

/// Focus restoration is now handled by the plugin's NSWindowDelegate.
/// NotificationCenter observers don't fire under merged-thread mode.
class FocusRestorer {
    static let shared = FocusRestorer()
    func startMonitoring() { /* no-op */ }
}
```

### 5. Clean build and test

```bash
cd /Users/udi/Grassroots/GLP/glp_multiagent
flutter clean
flutter pub get
flutter run -d macos
```

### Critical ordering in windowDidBecomeKey

The 5 operations MUST be in this order:
0. Keyboard manager reset + modifier resync (unstick the event queue first)
1. Engine focus notification (engine needs to know window is key)
2. inputContext.activate() (context must be active for text input)
3. makeFirstResponder (responder chain)
4. requestExistingInputState (Dart re-establishes text input)

Step 0 must come first because the engine focus notification may trigger internal key-event processing that would immediately re-stall if the queue is still stuck. Steps 1-4 must stay in their order because each depends on the prior state change.

### Compilation notes

- `flutterVC.value(forKey: "keyboardManager")` uses KVC to access private FlutterKeyboardManager
- `NSEvent.modifierFlags` is a static AppKit property returning current global modifier state
- If `flutterVC.viewIdentifier` doesn't compile, use `0` instead
- All selector-based calls are guarded by `responds(to:)` so they silently no-op if the API doesn't exist

### If only step 0 (reset) fixes it

Then the root cause is purely the stalled keyboard manager queue, not the engine focus state. In that case steps 1-4 are belt-and-suspenders but harmless. Keep them for robustness.

### Fallback if nothing works

Downgrade to Flutter 3.34.x where merged threads default to off. But try all 5 steps first.
