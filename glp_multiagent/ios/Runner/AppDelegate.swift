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

  /// The channel naming the app installed, its developer, its name and its
  /// version number, which the host gives the runtime for self_app/2
  /// (lib/app_identity.dart; GLP paper cce13ce, "Networking seam").
  private var appIdentity: FlutterMethodChannel?

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
    if let registrar = engineBridge.pluginRegistry.registrar(forPlugin: "AppIdentity") {
      let channel = FlutterMethodChannel(
        name: "grassroots/app_identity", binaryMessenger: registrar.messenger())
      channel.setMethodCallHandler { call, result in
        guard call.method == "identity" else {
          result(FlutterMethodNotImplemented)
          return
        }
        result(AppDelegate.identity)
      }
      appIdentity = channel
    }
  }

  /// The app installed, as the attestation names it to a peer (GNA, Section
  /// 2.6): the developer its team identifier, the prefix of its App ID that
  /// Info.plist carries as `GrassrootsAppIdentifierPrefix` without its dot;
  /// its name the bundle identifier; and its version the bundle version,
  /// `CFBundleVersion`, a whole number.  Nil where any is missing: a build
  /// signed with no team names none.
  private static var identity: [String: Any]? {
    guard
      let prefix = Bundle.main.object(
        forInfoDictionaryKey: "GrassrootsAppIdentifierPrefix") as? String,
      prefix.hasSuffix("."), prefix.count > 1, !prefix.contains("$"),
      let bundleId = Bundle.main.bundleIdentifier,
      let text = Bundle.main.object(forInfoDictionaryKey: "CFBundleVersion") as? String,
      let version = Int(text), version >= 0
    else { return nil }
    return ["developer": String(prefix.dropLast()), "name": bundleId, "version": version]
  }
}
