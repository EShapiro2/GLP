import Flutter
import UIKit

@main
@objc class AppDelegate: FlutterAppDelegate, FlutterImplicitEngineDelegate {
  /// The attestation exchange's producer (GNA, api.tex, "Attestation"): App
  /// Attest key generation, attestation over the identity key, and the
  /// per-session assertion.  The networking layer's own
  /// (networking/ios/Runner/PlatformAttestationPlugin.swift, which this
  /// project compiles from there), registered as the layer's demo app
  /// registers it.  Dart binds to it through the layer's
  /// NativePlatformAttestation; no entry point here does since the GrassApp
  /// duo went with programs/grassapp (2026-10-13).
  private var platformAttestation: PlatformAttestationPlugin?

  override func application(
    _ application: UIApplication,
    didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]?
  ) -> Bool {
    return super.application(application, didFinishLaunchingWithOptions: launchOptions)
  }

  func didInitializeImplicitFlutterEngine(_ engineBridge: FlutterImplicitEngineBridge) {
    GeneratedPluginRegistrant.register(with: engineBridge.pluginRegistry)

    if let registrar = engineBridge.pluginRegistry.registrar(
      forPlugin: "PlatformAttestationPlugin")
    {
      platformAttestation = PlatformAttestationPlugin(messenger: registrar.messenger())
    }
  }
}
