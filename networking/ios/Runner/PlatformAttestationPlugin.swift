import CryptoKit
import DeviceCheck
import Flutter
import Foundation

/// The iOS producer of the attestation exchange (spec §Session Establishment).
///
/// "Each agent holds an attestation key generated in its platform's secure
/// element and attested there once---App Attest on iOS ...---over a challenge
/// naming the agent's identity public key pk.  Per session each side sends
/// that attestation together with a signature by the attestation key over the
/// digest H("glp attest" | pk | h)".
///
/// On iOS the attestation key is an App Attest key: generated in the Secure
/// Enclave, attested once by Apple over a clientDataHash of SHA-256(pk), and
/// never leaving the enclave. It signs nothing but assertions, so the
/// per-session signature is an assertion with the session's digest as its
/// clientDataHash. Verification is the core's (`app_attest.dart`); this file
/// produces and nothing else.
///
/// The channel is `grassroots/attestation`; its two methods are documented on
/// the Dart side, `native_platform_attestation.dart`.
///
/// What is kept, per identity key: the key identifier and the attestation
/// object. Neither is secret — the identifier is a hash of the public key and
/// the attestation is sent to every peer — and both live in the application's
/// defaults, which go with the application when it is deleted, as App Attest
/// keys do.
///
/// Where this device provides no attestation — the simulator, a device
/// without App Attest, a build whose App ID cannot be named — `attest`
/// answers null, which the peer reports as unattested.
class PlatformAttestationPlugin: NSObject {
  private let channel: FlutterMethodChannel

  /// Where the key identifiers and attestations are kept.
  private static let storeKey = "grassroots.appAttest"

  /// Attestation requests waiting on one already in flight, by identity key:
  /// concurrent sessions at startup share one key generation.
  private var waiting: [String: [FlutterResult]] = [:]

  init(messenger: FlutterBinaryMessenger) {
    channel = FlutterMethodChannel(
      name: "grassroots/attestation", binaryMessenger: messenger)
    super.init()
    channel.setMethodCallHandler { [weak self] call, result in
      self?.handle(call, result: result)
    }
  }

  // MARK: - Method channel

  private func handle(_ call: FlutterMethodCall, result: @escaping FlutterResult) {
    guard let args = call.arguments as? [String: Any],
      let pk = (args["identityPublicKey"] as? FlutterStandardTypedData)?.data
    else {
      result(
        FlutterError(
          code: "badArguments", message: "identityPublicKey is required",
          details: nil))
      return
    }
    switch call.method {
    case "attest":
      attest(identityKey: pk, result: result)
    case "sign":
      guard let digest = (args["digest"] as? FlutterStandardTypedData)?.data else {
        result(
          FlutterError(
            code: "badArguments", message: "digest is required", details: nil))
        return
      }
      sign(identityKey: pk, digest: digest, result: result)
    default:
      result(FlutterMethodNotImplemented)
    }
  }

  // MARK: - The application identifier

  /// `<teamID>.<bundleID>`: the App ID the key is attested under, whose
  /// SHA-256 App Attest carries as the rpIdHash. The prefix is the build's
  /// `AppIdentifierPrefix`, which Info.plist carries as
  /// `GrassrootsAppIdentifierPrefix`; a build signed with no team has none,
  /// and cannot name its App ID.
  private var appId: String? {
    guard
      let prefix = Bundle.main.object(
        forInfoDictionaryKey: "GrassrootsAppIdentifierPrefix") as? String,
      prefix.hasSuffix("."), prefix.count > 1, !prefix.contains("$"),
      let bundleId = Bundle.main.bundleIdentifier
    else { return nil }
    return prefix + bundleId
  }

  // MARK: - Attest

  private func attest(identityKey pk: Data, result: @escaping FlutterResult) {
    guard #available(iOS 14.0, *) else {
      result(nil)
      return
    }
    let service = DCAppAttestService.shared
    guard service.isSupported else {
      // The simulator, or a device without App Attest.
      result(nil)
      return
    }
    guard let appId = appId else {
      NSLog("[attest] This build cannot name its App ID; it offers no attestation")
      result(nil)
      return
    }

    let id = Self.hex(pk)
    if let attestation = entry(id)?.attestation {
      result(Self.reply(appId: appId, attestation: attestation))
      return
    }
    if waiting[id] != nil {
      waiting[id]!.append(result)
      return
    }
    waiting[id] = [result]

    let finish: (Any?) -> Void = { [weak self] value in
      DispatchQueue.main.async {
        let results = self?.waiting.removeValue(forKey: id) ?? []
        for r in results { r(value) }
      }
    }

    // Apple attests over a hash of the client data; the client data is the
    // identity key, so the attestation names it.
    let clientDataHash = Data(SHA256.hash(data: pk))
    let attestKey: (String) -> Void = { [weak self] keyId in
      service.attestKey(keyId, clientDataHash: clientDataHash) { attestation, error in
        DispatchQueue.main.async {
          guard let self = self else { return }
          if let attestation = attestation {
            self.store(id, keyId: keyId, attestation: attestation)
            finish(Self.reply(appId: appId, attestation: attestation))
          } else {
            if Self.isInvalidKey(error) { self.forget(id) }
            finish(Self.flutterError("attestKey", error))
          }
        }
      }
    }

    if let keyId = entry(id)?.keyId {
      // Generated before and not yet attested — Apple's server was not
      // reachable then. Apple's guidance is to retry with the same key.
      attestKey(keyId)
      return
    }
    service.generateKey { [weak self] keyId, error in
      guard let keyId = keyId else {
        finish(Self.flutterError("generateKey", error))
        return
      }
      DispatchQueue.main.async {
        self?.store(id, keyId: keyId, attestation: nil)
        attestKey(keyId)
      }
    }
  }

  // MARK: - Sign

  private func sign(identityKey pk: Data, digest: Data, result: @escaping FlutterResult) {
    guard #available(iOS 14.0, *) else {
      result(nil)
      return
    }
    let id = Self.hex(pk)
    guard let entry = entry(id), entry.attestation != nil else {
      // No attested key for this identity: the same condition as `attest`
      // answering null.
      result(nil)
      return
    }
    // The digest is already SHA-256, and stands as the clientDataHash: the
    // Secure Enclave signs SHA-256(authenticatorData | digest).
    DCAppAttestService.shared.generateAssertion(entry.keyId, clientDataHash: digest) {
      [weak self] assertion, error in
      DispatchQueue.main.async {
        if let assertion = assertion {
          result(FlutterStandardTypedData(bytes: assertion))
        } else {
          if Self.isInvalidKey(error) { self?.forget(id) }
          result(Self.flutterError("generateAssertion", error))
        }
      }
    }
  }

  // MARK: - What is kept

  private struct Entry {
    let keyId: String
    let attestation: Data?
  }

  private func entry(_ id: String) -> Entry? {
    guard
      let all = UserDefaults.standard.dictionary(forKey: Self.storeKey),
      let record = all[id] as? [String: Any],
      let keyId = record["keyId"] as? String
    else { return nil }
    return Entry(keyId: keyId, attestation: record["attestation"] as? Data)
  }

  private func store(_ id: String, keyId: String, attestation: Data?) {
    var all = UserDefaults.standard.dictionary(forKey: Self.storeKey) ?? [:]
    var record: [String: Any] = ["keyId": keyId]
    if let attestation = attestation { record["attestation"] = attestation }
    all[id] = record
    UserDefaults.standard.set(all, forKey: Self.storeKey)
  }

  /// A key the system no longer honours — after a restore, or a reinstall
  /// the defaults survived — is forgotten, so the next request generates a
  /// fresh one.
  private func forget(_ id: String) {
    var all = UserDefaults.standard.dictionary(forKey: Self.storeKey) ?? [:]
    all.removeValue(forKey: id)
    UserDefaults.standard.set(all, forKey: Self.storeKey)
  }

  // MARK: - Helpers

  private static func reply(appId: String, attestation: Data) -> [String: Any] {
    [
      "platform": "ios",
      "appId": appId,
      "attestationObject": FlutterStandardTypedData(bytes: attestation),
    ]
  }

  private static func isInvalidKey(_ error: Error?) -> Bool {
    guard #available(iOS 14.0, *), let error = error as? DCError else { return false }
    return error.code == .invalidKey
  }

  private static func flutterError(_ step: String, _ error: Error?) -> FlutterError {
    FlutterError(
      code: "attestationFailed",
      message: "\(step) failed: \(error?.localizedDescription ?? "no result")",
      details: nil)
  }

  private static func hex(_ data: Data) -> String {
    data.map { String(format: "%02x", $0) }.joined()
  }
}
