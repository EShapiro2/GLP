// Attestation on Android and iPhone (GNA §2.6: Android's hardware key
// attestation, iOS's App Attest): each connection opens with the platform's
// signed word of which application, at which version, the other person runs.
// GAS counts a connection as GAS's when its application is this app's own,
// as the platform attests this app, or one of GAS's builds for the other
// platform, [otherGasBuilds].  Where the platform attests nothing (a
// desktop, an emulator), nothing is required.
import 'dart:io';
import 'dart:typed_data';

import 'package:grassroots_networking/grassroots_networking.dart';

/// GAS's builds for the other platform, which count as GAS beside this app's
/// own attested application: (developer, name), the developer the SHA-256 of
/// the signing certificate on Android and the team identifier on iOS, the
/// name the package or the bundle identifier.
const otherGasBuilds = <(String, String)>[];

/// The platform's attestation, where the phone has one.
final PlatformAttestation? phoneAttestation =
    (Platform.isAndroid || Platform.isIOS) ? NativePlatformAttestation() : null;

/// This app's own attested application: its attestation, over a session of
/// its own, verified as a peer's is; null where the platform attests nothing.
Future<Application?> ownApplication(PlatformAttestation pa, Uint8List me) async {
  try {
    final att = await pa.attestationFor(me);
    final v = await pa.applicationVersion();
    if (att == null || v == null) return null;
    final h = Uint8List(32);
    final sig = await pa.signSessionDigest(
        identityPublicKey: me, digest: attestationDigest(identityPublicKey: me, version: v, handshakeHash: h));
    if (sig == null) return null;
    final verdict = await pa.verify(
        evidence: AttestationEvidence(attestation: att, version: v, signature: sig),
        handshakeHash: h,
        peerIdentityKey: me);
    return verdict is AttestedApplication ? verdict.application : null;
  } on Object {
    return null;
  }
}

/// Whether the application a connection reports counts as GAS.
bool Function(Object?) admitsLike(Application own) => (a) =>
    a is Application &&
    ((a.developer == own.developer && a.name == own.name) || otherGasBuilds.contains((a.developer, a.name)));
