import Cocoa
import FlutterMacOS

class MainFlutterWindow: NSWindow {
  override func awakeFromNib() {
    let flutterViewController = FlutterViewController()
    self.contentViewController = flutterViewController
    // Give the UI room: the agent panel (inbox cards + Accept/Decline + outbox)
    // clips at a short default size, so open larger and centered.
    // Disable saved-frame restoration so the size below always applies.
    self.setFrameAutosaveName("")
    let targetSize = NSSize(width: 460, height: 950)
    if let screen = self.screen ?? NSScreen.main {
      let v = screen.visibleFrame
      let origin = NSPoint(x: v.midX - targetSize.width / 2,
                           y: v.midY - targetSize.height / 2)
      self.setFrame(NSRect(origin: origin, size: targetSize), display: true)
    } else {
      self.setContentSize(targetSize)
    }

    RegisterGeneratedPlugins(registry: flutterViewController)

    super.awakeFromNib()
  }
}
