import Cocoa
import FlutterMacOS

class MainFlutterWindow: NSWindow {
  override func awakeFromNib() {
    let flutterViewController = FlutterViewController()
    self.contentViewController = flutterViewController
    // A phone-shaped window, the size of the app's one screen.
    self.setContentSize(NSSize(width: 430, height: 860))
    self.contentMinSize = NSSize(width: 360, height: 600)
    self.title = "GAS"
    self.center()

    RegisterGeneratedPlugins(registry: flutterViewController)

    super.awakeFromNib()
  }
}
