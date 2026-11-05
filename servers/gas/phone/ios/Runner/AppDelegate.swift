import Flutter
import UIKit

@main
@objc class AppDelegate: FlutterAppDelegate, FlutterImplicitEngineDelegate {
  /// The attestation exchange's producer (GNA §2.6), as GNA's own app holds
  /// it (PlatformAttestationPlugin.swift, copied from networking/ios): App
  /// Attest key generation, attestation over the identity key, and the
  /// per-session assertion, on the channel grassroots/attestation.
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
