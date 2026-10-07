/// The attested application identity: what `onPeerConnected` carries for a
/// peer whose attestation verified.
///
/// Spec §Session Establishment: "What the attestation names is the application
/// published under its developer's key, not the code.  Android's certificate
/// names the package, its version and the digest of its signing certificate;
/// App Attest names the application identifier the key was attested under, so
/// a version is attested on iOS only where the identifier carries it.  Neither
/// platform measures the running code, so the identity onPeerConnected carries
/// is the platform's application identity and is called that."
///
/// So there is one type per platform, each holding exactly what that platform
/// names, and neither pretends to be a hash of the running binary.
library;

import 'dart:typed_data';

/// The application identity a platform attested.
sealed class ApplicationIdentity {
  const ApplicationIdentity();
}

/// iOS: App Attest's application identifier, `<teamID>.<bundleID>`.
///
/// App Attest's `authData` carries the SHA-256 of this identifier as its
/// rpIdHash, under Apple's signature; the identifier itself travels beside the
/// attestation object and is accepted only where its SHA-256 is that hash.
class IosApplicationIdentity extends ApplicationIdentity {
  /// The App ID, `<teamID>.<bundleID>`.
  final String appId;

  const IosApplicationIdentity(this.appId);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is IosApplicationIdentity && other.appId == appId;

  @override
  int get hashCode => Object.hash(IosApplicationIdentity, appId);

  @override
  String toString() => 'IosApplicationIdentity($appId)';
}

/// One package an Android attestation names, with its version.
class AndroidPackage {
  final String name;
  final int version;

  const AndroidPackage({required this.name, required this.version});

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is AndroidPackage && other.name == name && other.version == version;

  @override
  int get hashCode => Object.hash(name, version);

  @override
  String toString() => '$name@$version';
}

/// Android: the packages the key attestation names, each with its version, and
/// the SHA-256 digests of the certificates the application is signed with.
///
/// Google's `AttestationApplicationId` is a set of packages because packages
/// sharing a user ID share a keystore; for an application of its own it holds
/// one.
class AndroidApplicationIdentity extends ApplicationIdentity {
  final List<AndroidPackage> packages;
  final List<Uint8List> signatureDigests;

  const AndroidApplicationIdentity({
    required this.packages,
    required this.signatureDigests,
  });

  /// The package names alone.
  List<String> get packageNames =>
      [for (final p in packages) p.name];

  @override
  bool operator ==(Object other) {
    if (identical(this, other)) return true;
    if (other is! AndroidApplicationIdentity) return false;
    if (other.packages.length != packages.length) return false;
    for (var i = 0; i < packages.length; i++) {
      if (other.packages[i] != packages[i]) return false;
    }
    if (other.signatureDigests.length != signatureDigests.length) return false;
    for (var i = 0; i < signatureDigests.length; i++) {
      if (!_sameBytes(other.signatureDigests[i], signatureDigests[i])) {
        return false;
      }
    }
    return true;
  }

  @override
  int get hashCode => Object.hash(
        Object.hashAll(packages),
        Object.hashAll([for (final d in signatureDigests) ...d]),
      );

  @override
  String toString() => 'AndroidApplicationIdentity(${packages.join(", ")}; '
      '${signatureDigests.length} signing digest(s))';
}

bool _sameBytes(Uint8List a, Uint8List b) {
  if (a.length != b.length) return false;
  for (var i = 0; i < a.length; i++) {
    if (a[i] != b[i]) return false;
  }
  return true;
}
