# GLP Multi-Window Multiagent Simulation

Flutter desktop application for simulating GLP multiagent systems with separate windows per agent.

## Phase 1 Setup (macOS)

This project requires Flutter and must be built on macOS.

### Prerequisites

1. Flutter SDK installed (https://flutter.dev/docs/get-started/install/macos)
2. Xcode with command-line tools
3. macOS desktop support enabled:
   ```bash
   flutter config --enable-macos-desktop
   ```

### Setup Steps

Run on your Mac:

```bash
cd /Users/udi/GLP/glp_multiagent

# Initialize Flutter project files (creates macos/ directory, etc.)
flutter create --platforms=macos .

# Get dependencies
flutter pub get

# Run the app
flutter run -d macos
```

### Configure macOS for Multi-Window

After `flutter create`, update `macos/Runner/AppDelegate.swift`:

```swift
import Cocoa
import FlutterMacOS
import desktop_multi_window

@main
class AppDelegate: FlutterAppDelegate {
  override func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool {
    return false  // Keep app running when main window closes
  }

  override func applicationSupportsSecureRestorableState(_ app: NSApplication) -> Bool {
    return true
  }
}
```

Also update `macos/Runner/MainFlutterWindow.swift` to register the multi-window plugin:

```swift
import Cocoa
import FlutterMacOS
import desktop_multi_window

class MainFlutterWindow: NSWindow {
  override func awakeFromNib() {
    let flutterViewController = FlutterViewController()
    let windowFrame = self.frame
    self.contentViewController = flutterViewController
    self.setFrame(windowFrame, display: true)

    // Register multi-window plugin
    FlutterMultiWindowPlugin.registerGeneratedPlugins = RegisterGeneratedPlugins

    RegisterGeneratedPlugins(registry: flutterViewController)

    super.awakeFromNib()
  }
}
```

### Verify Build

```bash
flutter run -d macos
```

Expected: A coordinator window opens with a "Spawn Agent Window" button. Clicking it should open a new agent window.

## Project Structure

```
glp_multiagent/
├── lib/
│   └── main.dart          # Main entry point with Coordinator and Agent windows
├── macos/                 # macOS platform files (created by flutter create)
├── pubspec.yaml           # Dependencies including glp_runtime
└── README.md              # This file
```

## Phases

- **Phase 0** (Complete): GLP I/O spec and Dart-side implementation in glp_runtime
- **Phase 1** (Current): Flutter project setup with multi-window support
- **Phase 2**: Single agent window with GLP runtime integration
- **Phase 3+**: Network coordination, message routing, full multiagent simulation
