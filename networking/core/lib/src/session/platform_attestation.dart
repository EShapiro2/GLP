/// Platform attestation: the remote attestation of the application a peer
/// runs, exchanged after the Noise handshake.
///
/// Spec `docs/GLP_Networking_API/sections/api.tex` §Session Establishment.
/// The exchange has two parts, one long-lived and one per session. Each agent
/// holds an *attestation key* generated in its platform's secure element and
/// attested there once — App Attest on iOS, hardware key attestation on
/// Android — over a challenge naming the agent's identity public key `pk`.
/// Per session each side sends that attestation together with the
/// application's version number `v` and a signature by the attestation key
/// over the digest `H("glp attest" | pk | v | h)`, `h` being the final Noise
/// handshake hash (GNA §2.6), and each side verifies both: the attestation
/// against the platform's root, and the signature against the key the
/// attestation carries. Either failing tears the session down and
/// `onPeerConnected` does not fire. The version number reported for the
/// peer's application is the one it signed, on both platforms.
///
/// The attestation is long-lived because a platform fixes the challenge when
/// the key is generated, so a per-session challenge would demand a
/// secure-element key generation per session. The attestation binds the key to
/// the application and to the identity key; the signature binds it to this
/// session. Nothing of the session is cached: the signature is computed afresh
/// over every handshake hash, so a peer that resumes after a restart proves
/// itself again before it is reachable.
///
/// The handshake hash is what binds the exchange to this session. An identity
/// key is long-lived, so an attestation over the key alone would be replayable
/// onto every channel that agent opens; `h` is unique per session, identical
/// at both ends, and chosen by neither side alone.
library;

import 'dart:convert';
import 'dart:typed_data';

import 'package:cryptography/dart.dart' show DartSha256;

import 'application.dart';
import 'attestation_verifier.dart';

/// Domain separator for the attestation digest.
const String kAttestationLabel = 'glp attest';

/// The digest a platform signs per session: `H("glp attest" | pk | v | h)`
/// (GNA §2.6).
///
/// [identityPublicKey] is the attesting agent's OWN identity key, [version]
/// the version number of the application it runs, and [handshakeHash] the
/// final Noise handshake hash of the session being attested. The version
/// number is written as eight bytes, big-endian; [version] is a whole number
/// below 2^63.  The receiver computes the peer's digest from the peer's key,
/// the version number its attestation message carries, and the shared hash.
Uint8List attestationDigest({
  required Uint8List identityPublicKey,
  required int version,
  required Uint8List handshakeHash,
}) {
  if (version < 0) {
    throw ArgumentError.value(version, 'version', 'not a version number');
  }
  final message = BytesBuilder()
    ..add(utf8.encode(kAttestationLabel))
    ..add(identityPublicKey)
    ..add((ByteData(8)..setUint64(0, version, Endian.big))
        .buffer
        .asUint8List())
    ..add(handshakeHash);
  return Uint8List.fromList(
    const DartSha256().hashSync(message.toBytes()).bytes,
  );
}

/// What verifying a peer's attestation yielded.
///
/// Three outcomes, and the specification turns on all three being distinct:
/// an attestation that verifies, a platform that offers none, and one offered
/// and found invalid. "Absence and failure are distinct: an attestation
/// offered and found invalid always tears the session down", while a platform
/// with none is reported unattested rather than refused.
sealed class AttestationVerdict {
  const AttestationVerdict();
}

/// The peer's identity is attested, and this is the attested application:
/// its developer, its name and the version number it signed (GNA §2.6).
class AttestedApplication extends AttestationVerdict {
  /// The attested application, carried to GLP on `onPeerConnected`.
  final Application application;

  const AttestedApplication(this.application);
}

/// The peer's platform provides no attestation — a headless server profile
/// has none. The peer's identity is reported unattested, and
/// `onPeerConnected` carries null for the application. Whether to transact
/// with an unattested peer is a GLP-level decision.
class UnattestedPlatform extends AttestationVerdict {
  /// Why no attestation was available, for the log. Not carried to GLP:
  /// unattested is unattested.
  final String reason;

  const UnattestedPlatform(this.reason);
}

/// The peer offered an attestation and it did not verify. The session is torn
/// down and `onPeerConnected` does not fire.
class InvalidAttestation extends AttestationVerdict {
  final String reason;

  const InvalidAttestation(this.reason);
}

/// What one side sends: the long-lived attestation, the application's
/// version number, and the per-session signature over the digest that names
/// it.
///
/// The three travel together and are verified together. None alone is
/// meaningful — the attestation without the signature does not name this
/// session, the signature without the attestation names no hardware-held
/// key, and the version number is the application's only under the signature.
class AttestationEvidence {
  /// The platform's attestation of this agent's attestation key, over a
  /// challenge naming the agent's identity public key. Long-lived: obtained
  /// once per install, not once per session.
  final Uint8List attestation;

  /// The version number of the application, which the signature names (GNA
  /// §2.6: "the attestation message carries v").
  final int version;

  /// A signature by the attested key over `H("glp attest" | pk | v | h)`.
  final Uint8List signature;

  const AttestationEvidence({
    required this.attestation,
    required this.version,
    required this.signature,
  });
}

/// The platform's attestation service, as the layer needs it.
///
/// Kept as an interface because the binding is a platform dependency — App
/// Attest on iOS, hardware key attestation on Android — while the exchange
/// itself, its binding to the handshake hash, and what a verdict does to the
/// session are decided above it, in the session layer, and are the same on
/// every platform.
abstract class PlatformAttestation {
  /// This agent's long-lived attestation: its attestation key as attested by
  /// the platform over a challenge naming [identityPublicKey].
  ///
  /// Obtained once per install and cached by the binding — the platform fixes
  /// the challenge at key generation, so re-attesting per session would mean a
  /// secure-element key generation per session.
  ///
  /// Returns null where this platform provides no attestation at all — the
  /// headless server profile has none — which the peer reports as unattested
  /// rather than refusing.
  Future<Uint8List?> attestationFor(Uint8List identityPublicKey);

  /// The version number of the application this agent runs, which it signs
  /// in each session's digest (GNA §2.6: "On iOS the attestation key remains
  /// valid through updates of the application, so an updated application
  /// needs no new attestation and signs its new version number in each new
  /// session").
  ///
  /// Returns null where this platform provides no attestation, the same
  /// condition as [attestationFor] returning null.
  Future<int?> applicationVersion();

  /// Sign the per-session [digest] with the key attested for
  /// [identityPublicKey].
  ///
  /// Returns null where this platform holds no attestation key, which is the
  /// same condition as [attestationFor] returning null.
  Future<Uint8List?> signSessionDigest({
    required Uint8List identityPublicKey,
    required Uint8List digest,
  });

  /// Verify a peer's [evidence] for the session whose handshake hash is
  /// [handshakeHash] — the attestation against this platform's pinned root,
  /// its challenge naming [peerIdentityKey]; and the signature against the
  /// key the attestation carries, over the digest of [peerIdentityKey], the
  /// version number the evidence carries, and [handshakeHash].
  ///
  /// [evidence] is null when the peer sent none, which is
  /// [UnattestedPlatform] and not a failure.
  Future<AttestationVerdict> verify({
    required AttestationEvidence? evidence,
    required Uint8List handshakeHash,
    required Uint8List peerIdentityKey,
  });
}

/// The attestation of a platform that has none to offer.
///
/// This is the headless server profile's (spec §Rendezvous Server:
/// "Having no platform attestation to offer, a rendezvous server is reported
/// unattested, and this costs nothing"), and it is also what a unit test and
/// any embedding without a native producer get.
///
/// It offers nothing, and it verifies what a peer offers: verification is
/// arithmetic over bytes against the pinned roots and lives in the core, so a
/// profile with no attestation of its own still tells a valid attestation from
/// an invalid one (spec §Session Establishment: "the layer verifies
/// attestations"). A peer that offers nothing is unattested; a peer whose
/// offer does not verify is invalid, and its session is torn down.
class NoPlatformAttestation implements PlatformAttestation {
  const NoPlatformAttestation({this.verifier = const AttestationVerifier()});

  /// The verifier of what peers offer.
  final AttestationVerifier verifier;

  @override
  Future<Uint8List?> attestationFor(Uint8List identityPublicKey) async => null;

  @override
  Future<int?> applicationVersion() async => null;

  @override
  Future<Uint8List?> signSessionDigest({
    required Uint8List identityPublicKey,
    required Uint8List digest,
  }) async =>
      null;

  @override
  Future<AttestationVerdict> verify({
    required AttestationEvidence? evidence,
    required Uint8List handshakeHash,
    required Uint8List peerIdentityKey,
  }) async =>
      verifier.verify(
        evidence: evidence,
        handshakeHash: handshakeHash,
        peerIdentityKey: peerIdentityKey,
      );
}

/// Tag byte for "my platform provides no attestation".
const int _attestationAbsent = 0x00;

/// Tag byte for "an attestation follows".
const int _attestationPresent = 0x01;

/// Width of the version-number field, a whole number big-endian.
const int _versionBytes = 8;

/// Width of the attestation-length field: the attestation is variable-length
/// and the signature takes the remainder, so exactly one length is carried.
const int _attestationLengthBytes = 4;

/// Frame the evidence for the wire: one tag byte, the version number, the
/// attestation length, the attestation, then the signature as the remainder.
/// The statement of no attestation is the tag alone, and carries no
/// application (GNA §2.6).
///
/// The tag is what keeps absence and failure distinct on the wire: a peer
/// with no platform attestation says so explicitly rather than sending
/// nothing, which would be indistinguishable from a lost packet.
///
/// The attestation carries its length because both fields are variable — an
/// Android chain and an App Attest object are both several kilobytes, and a
/// signature is tens of bytes — and the signature needs none, being whatever
/// follows.
Uint8List encodeAttestationPayload(AttestationEvidence? evidence) {
  if (evidence == null) return Uint8List.fromList([_attestationAbsent]);
  if (evidence.attestation.isEmpty) {
    throw ArgumentError('Cannot frame evidence with an empty attestation');
  }
  if (evidence.signature.isEmpty) {
    throw ArgumentError('Cannot frame evidence with an empty signature');
  }
  if (evidence.version < 0) {
    throw ArgumentError('Cannot frame the version number ${evidence.version}');
  }
  final header = ByteData(_versionBytes + _attestationLengthBytes)
    ..setUint64(0, evidence.version, Endian.big)
    ..setUint32(_versionBytes, evidence.attestation.length, Endian.big);
  return Uint8List.fromList([
    _attestationPresent,
    ...header.buffer.asUint8List(),
    ...evidence.attestation,
    ...evidence.signature,
  ]);
}

/// Read framed evidence. Null means the peer's platform provides none.
///
/// Throws [FormatException] on anything else. There is no old version in the
/// wild and a malformed payload is malformed, not an older shape to be read
/// tolerantly.
AttestationEvidence? decodeAttestationPayload(Uint8List payload) {
  if (payload.isEmpty) {
    throw const FormatException('Attestation payload is empty');
  }
  switch (payload[0]) {
    case _attestationAbsent:
      if (payload.length != 1) {
        throw const FormatException(
          'Attestation payload claims absence and carries bytes',
        );
      }
      return null;
    case _attestationPresent:
      const headerEnd = 1 + _versionBytes + _attestationLengthBytes;
      if (payload.length < headerEnd) {
        throw const FormatException(
          'Attestation payload is too short for its version number and its '
          'length field',
        );
      }
      final header = ByteData.sublistView(payload, 1, headerEnd);
      final version = header.getUint64(0, Endian.big);
      if (version < 0) {
        throw const FormatException(
          'Attestation payload carries a version number of 2^63 or more',
        );
      }
      final attestationLength = header.getUint32(_versionBytes, Endian.big);
      if (attestationLength == 0) {
        throw const FormatException(
          'Attestation payload claims presence and carries no attestation',
        );
      }
      final signatureStart = headerEnd + attestationLength;
      if (payload.length < signatureStart) {
        throw FormatException(
          'Attestation payload claims $attestationLength attestation bytes '
          'and carries ${payload.length - headerEnd}',
        );
      }
      if (payload.length == signatureStart) {
        throw const FormatException(
          'Attestation payload carries no signature',
        );
      }
      return AttestationEvidence(
        attestation:
            Uint8List.sublistView(payload, headerEnd, signatureStart),
        version: version,
        signature: Uint8List.sublistView(payload, signatureStart),
      );
    default:
      throw FormatException('Unknown attestation tag ${payload[0]}');
  }
}
