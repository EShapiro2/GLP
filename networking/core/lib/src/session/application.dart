/// The attested application: what `onPeerConnected` carries for a peer whose
/// identity is attested (GNA §2.3, `onPeerConnected(pk, underlay,
/// application?)`: "for an attested identity, the attested application: its
/// developer, its name and its version number; null for an unattested
/// identity").
///
/// GNA §2.6: "What the attestation names is the application published under
/// its developer's key, not the code.  To report the same on both platforms,
/// we take the attested application to be its developer, its name and its
/// version number.  Android's certificate names the digest of the developer's
/// signing certificate, the package and its version; App Attest names the
/// application identifier the key was attested under, the developer's team
/// identifier and the bundle identifier, and no version.  On both platforms
/// the version number is the one the application signs in the per-session
/// digest, which only the genuine application can sign."
///
/// The identity is the agent's public key (GNA §2.1), and nothing here.
library;

import '../platform/compat.dart';

/// An application published under its developer's key: its developer, its
/// name and its version number.
@immutable
class Application {
  const Application({
    required this.developer,
    required this.name,
    required this.version,
  });

  /// On Android the SHA-256 digest of the developer's signing certificate,
  /// in lowercase hex; on iOS the developer's team identifier.
  final String developer;

  /// On Android the package; on iOS the bundle identifier.
  final String name;

  /// The version number the application signed in the session's digest.
  final int version;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Application &&
          other.developer == developer &&
          other.name == name &&
          other.version == version;

  @override
  int get hashCode => Object.hash(developer, name, version);

  @override
  String toString() => 'Application($developer, $name, $version)';
}
