/// Platform attestation: the remote attestation of the application a peer
/// runs, exchanged after the Noise handshake.
///
/// Spec `docs/GLP_Networking_API/sections/api.tex` §Session Establishment.
/// The exchange has two parts, one long-lived and one per session. Each agent
/// holds an *attestation key* generated in its platform's secure element and
/// attested there once — App Attest on iOS, hardware key attestation on
/// Android — over a challenge naming the agent's identity public key `pk`.
/// Per session each side sends that attestation together with a signature by
/// the attestation key over the digest `H("glp attest" | pk | h)`, `h` being
/// the final Noise handshake hash, and each side verifies both: the
/// attestation against the platform's root, and the signature against the key
/// the attestation carries. Either failing tears the session down and
/// `onPeerConnected` does not fire.
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

import '../platform/compat.dart';

/// Domain separator for the attestation digest.
const String kAttestationLabel = 'glp attest';

/// The digest a platform attests over: `H("glp attest" | pk | h)`.
///
/// [identityPublicKey] is the attesting agent's OWN identity key, and
/// [handshakeHash] the final Noise handshake hash of the session being
/// attested. Both ends compute the peer's expected digest from the peer's key
/// and the shared hash, so neither has to be told what was attested.
Uint8List attestationDigest({
  required Uint8List identityPublicKey,
  required Uint8List handshakeHash,
}) {
  final message = BytesBuilder()
    ..add(utf8.encode(kAttestationLabel))
    ..add(identityPublicKey)
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

/// The peer attested, and this is the binary hash it attested to.
class AttestedBinary extends AttestationVerdict {
  /// The attested binary hash, carried to GLP on `onPeerConnected`.
  final Uint8List binaryHash;

  const AttestedBinary(this.binaryHash);
}

/// The peer's platform provides no attestation — a headless server profile
/// has none. The peer is reported unattested, and `onPeerConnected` carries a
/// null binary hash. Whether to transact with an unattested peer is a
/// GLP-level decision.
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

/// What one side sends: the long-lived attestation and the per-session
/// signature over the digest.
///
/// The two travel together and are verified together. Neither alone is
/// meaningful — the attestation without the signature does not name this
/// session, and the signature without the attestation names no hardware-held
/// key.
class AttestationEvidence {
  /// The platform's attestation of this agent's attestation key, over a
  /// challenge naming the agent's identity public key. Long-lived: obtained
  /// once per install, not once per session.
  final Uint8List attestation;

  /// A signature by the attested key over `H("glp attest" | pk | h)`.
  final Uint8List signature;

  const AttestationEvidence({
    required this.attestation,
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

  /// Sign the per-session [digest] with the attested key.
  ///
  /// Returns null where this platform holds no attestation key, which is the
  /// same condition as [attestationFor] returning null.
  Future<Uint8List?> signSessionDigest(Uint8List digest);

  /// Verify a peer's [evidence] — the attestation against this platform's
  /// pinned root, its challenge naming [peerIdentityKey]; and the signature
  /// against the key the attestation carries, over [digest].
  ///
  /// [evidence] is null when the peer sent none, which is
  /// [UnattestedPlatform] and not a failure.
  Future<AttestationVerdict> verify({
    required AttestationEvidence? evidence,
    required Uint8List digest,
    required Uint8List peerIdentityKey,
  });
}

/// The attestation of a platform that has none.
///
/// This is the headless server profile's (spec §Rendezvous Server:
/// "Having no platform attestation to offer, a rendezvous server is reported
/// unattested, and this costs nothing"), and it is also what a unit test and
/// any embedding without a native binding get.
///
/// It offers nothing, and it reports every peer unattested.
///
/// A peer that DOES offer an attestation is reported unattested too, and not
/// invalid: this build cannot verify it, and "cannot determine" is not "found
/// invalid". Reporting unattested claims nothing about the peer, which is the
/// truth; reporting invalid would claim a verification that never ran, and
/// would tear down every session with an attesting peer.
class NoPlatformAttestation implements PlatformAttestation {
  const NoPlatformAttestation();

  @override
  Future<Uint8List?> attestationFor(Uint8List identityPublicKey) async => null;

  @override
  Future<Uint8List?> signSessionDigest(Uint8List digest) async => null;

  @override
  Future<AttestationVerdict> verify({
    required AttestationEvidence? evidence,
    required Uint8List digest,
    required Uint8List peerIdentityKey,
  }) async {
    if (evidence == null) {
      return const UnattestedPlatform("the peer's platform offers none");
    }
    debugPrint('[attest] An attestation was offered; this build verifies none');
    return const UnattestedPlatform('this build has no verifier');
  }
}

/// Tag byte for "my platform provides no attestation".
const int _attestationAbsent = 0x00;

/// Tag byte for "an attestation follows".
const int _attestationPresent = 0x01;

/// Width of the attestation-length field: the attestation is variable-length
/// and the signature takes the remainder, so exactly one length is carried.
const int _attestationLengthBytes = 4;

/// Frame the evidence for the wire: one tag byte, the attestation length, the
/// attestation, then the signature as the remainder.
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
  final header = ByteData(_attestationLengthBytes)
    ..setUint32(0, evidence.attestation.length, Endian.big);
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
      const headerEnd = 1 + _attestationLengthBytes;
      if (payload.length < headerEnd) {
        throw const FormatException(
          'Attestation payload is too short for its length field',
        );
      }
      final attestationLength = ByteData.sublistView(payload, 1, headerEnd)
          .getUint32(0, Endian.big);
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
        signature: Uint8List.sublistView(payload, signatureStart),
      );
    default:
      throw FormatException('Unknown attestation tag ${payload[0]}');
  }
}
