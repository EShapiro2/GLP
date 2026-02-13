# Focus Bug Diagnosis — desktop_multi_window Child Windows

**Date:** 2026-02-12

## Root Cause Found

`FlutterView.acceptsFirstResponder` returns `false` in child windows created
by `desktop_multi_window` v0.2.1. This means **no native `makeFirstResponder`
fix can work** — the view itself rejects first responder status.

Diagnostic output from `windowDidBecomeKey`:
```
🔍 FOCUS[1]: acceptsFirstResponder=false
🔍 FOCUS[1]: currentFirstResponder=Optional(<NSWindow: 0x...>)
```

The window's first responder remains the `NSWindow` itself (the default
fallback when no view accepts first responder).

## What Was Tried

1. `DispatchQueue.main.async { makeFirstResponder(view) }` — no effect
2. `DispatchQueue.main.asyncAfter(deadline: .now() + 0.05)` — no effect
3. `makeFirstResponder(nil)` then `makeFirstResponder(view)` after 100ms — no effect
4. Same with 150ms delay — no effect
5. Polling every 100ms for up to 2 seconds waiting for `acceptsFirstResponder`
   to become `true` — it never does
6. All approaches from AppDelegate global observer — no effect
7. All approaches from plugin's own `NSWindowDelegate` — no effect

## Why It Once Worked (Hypothesis)

In one earlier session, keyboard input worked in child windows. This may have
been a transient macOS state or a different build configuration. It was never
reproducible.

## Architecture

- `desktop_multi_window` v0.2.1 creates child windows via `FlutterWindow.init()`
- Each child gets a new `FlutterDartProject` and `FlutterViewController`
- `window.contentViewController = flutterViewController`
- The plugin sets `window.delegate = self` (FlutterWindow)
- The delegate only implements `windowWillClose` and `windowShouldClose`
- No focus management whatsoever

## The Real Problem

The `FlutterView` (subclass of `NSView` inside `FlutterViewController`) has
`acceptsFirstResponder` returning `false`. This is likely controlled by the
Flutter engine itself. In the main window (created via NIB/XIB), the engine
initialization includes steps that eventually set this to `true`. In child
windows created programmatically, this step may be missing or fails silently.

## Possible Next Steps

1. **Investigate Flutter engine source** — find what controls
   `acceptsFirstResponder` on `FlutterView` and why it stays `false`
   in child windows
2. **Fork desktop_multi_window** — modify window creation to ensure the
   engine fully initializes before showing the window
3. **Use a different multi-window approach** — e.g., separate processes
   instead of in-process child windows, or `window_manager_plus`
4. **Use the automated (non-interactive) test** — bypass the UI focus
   issue entirely by using `play_madglp_boot.glp` with scripted actors

## Environment

- Flutter 3.38.5 (stable)
- macOS (Apple Silicon)
- `desktop_multi_window` 0.2.1
- Xcode with command-line tools
