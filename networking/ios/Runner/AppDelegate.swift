import Flutter
import UIKit

@main
@objc class AppDelegate: FlutterAppDelegate, FlutterImplicitEngineDelegate {
  /// The place predicates' platform half (spec §System Predicates). Held here
  /// because it owns a CLLocationManager and the registrations standing with
  /// it; releasing it releases them.
  private var placeGeofence: PlaceGeofencePlugin?

  override func application(
    _ application: UIApplication,
    didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]?
  ) -> Bool {
    return super.application(application, didFinishLaunchingWithOptions: launchOptions)
  }

  func didInitializeImplicitFlutterEngine(_ engineBridge: FlutterImplicitEngineBridge) {
    GeneratedPluginRegistrant.register(with: engineBridge.pluginRegistry)

    if let registrar = engineBridge.pluginRegistry.registrar(
      forPlugin: "PlaceGeofencePlugin")
    {
      placeGeofence = PlaceGeofencePlugin(messenger: registrar.messenger())
    }
  }

  override func applicationWillTerminate(_ application: UIApplication) {
    // Nothing is left registered with the platform once the layer is done
    // with it, and process death is one way of being done.
    placeGeofence?.releaseEverything()
    super.applicationWillTerminate(application)
  }
}
