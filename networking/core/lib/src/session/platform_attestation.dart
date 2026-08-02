/// Platform attestation: the remote attestation of the application a peer
/// runs, exchanged after the Noise handshake.
///
/// Spec `docs/GLP_Networking_API/sections/api.tex` §Session Establishment.
/// Each side asks its platform to attest over the digest
/// `H("glp attest" | pk | h)` — `pk` its own identity public key, `h` the
/// final Noise handshake hash — sends it, and verifies the other's. An
/// attestation that fails verification tears the session down and
/// `onPeerConnected` does not fire. Nothing is cached across sessions: a
/// layer restart yields a fresh handshake and hence a fresh attestation.
///
/// The handshake hash is what binds the attestation to this session. An
/// identity key is long-lived, so an attestation over the key alone would be
/// replayable onto every channel that agent opens; `h` is unique per session,
/// identical at both ends, and chosen by neither side alone.
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

/// The platform's attestation service, as the layer needs it.
///
/// Kept as an interface because the binding is a platform dependency — App
/// Attest on iOS, Play Integrity on Android — while the exchange itself, its
/// binding to the handshake hash, and what a verdict does to the session are
/// decided above it, in the session layer, and are the same on every platform.
abstract class PlatformAttestation {
  /// Attest this agent's binary over [digest].
  ///
  /// Returns null where this platform provides no attestation at all — the
  /// headless server profile has none — which the peer reports as unattested
  /// rather than refusing.
  Future<Uint8List?> attest(Uint8List digest);

  /// Verify a peer's [attestation] against the [digest] it should have
  /// attested over.
  ///
  /// [attestation] is null when the peer sent none, which is
  /// [UnattestedPlatform] and not a failure.
  Future<AttestationVerdict> verify(Uint8List? attestation, Uint8List digest);
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
  Future<Uint8List?> attest(Uint8List digest) async => null;

  @override
  Future<AttestationVerdict> verify(
    Uint8List? attestation,
    Uint8List digest,
  ) async {
    if (attestation == null) {
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

/// Frame an attestation for the wire: one tag byte, then the attestation.
///
/// The tag is what keeps absence and failure distinct on the wire: a peer
/// with no platform attestation says so explicitly rather than sending
/// nothing, which would be indistinguishable from a lost packet.
Uint8List encodeAttestationPayload(Uint8List? attestation) {
  if (attestation == null) return Uint8List.fromList([_attestationAbsent]);
  return Uint8List.fromList([_attestationPresent, ...attestation]);
}

/// Read a framed attestation. Null means the peer's platform provides none.
///
/// Throws [FormatException] on anything else. There is no old version in the
/// wild and a malformed payload is malformed, not an older shape to be read
/// tolerantly.
Uint8List? decodeAttestationPayload(Uint8List payload) {
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
      if (payload.length == 1) {
        throw const FormatException(
          'Attestation payload claims presence and carries none',
        );
      }
      return Uint8List.sublistView(payload, 1);
    default:
      throw FormatException('Unknown attestation tag ${payload[0]}');
  }
}
