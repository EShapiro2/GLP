import 'dart:typed_data';

import 'package:test/test.dart';

import 'package:grassroots_networking_core/src/session/platform_attestation.dart';

/// The attestation digest and its wire framing (spec §Session Establishment).
///
/// The digest is what binds an attestation to one session: an identity key is
/// long-lived, so an attestation over the key alone would be replayable onto
/// every channel that agent opens, while the handshake hash is unique per
/// session, identical at both ends, and chosen by neither side alone.
void main() {
  Uint8List bytes(int fill, [int length = 32]) =>
      Uint8List.fromList(List.filled(length, fill));

  group('attestationDigest', () {
    test('is a 32-byte digest and is deterministic', () {
      final a = attestationDigest(
        identityPublicKey: bytes(0x11),
        handshakeHash: bytes(0x22),
      );
      final b = attestationDigest(
        identityPublicKey: bytes(0x11),
        handshakeHash: bytes(0x22),
      );
      expect(a.length, 32);
      expect(a, b);
    });

    test('a different handshake hash gives a different digest', () {
      final session1 = attestationDigest(
        identityPublicKey: bytes(0x11),
        handshakeHash: bytes(0x22),
      );
      final session2 = attestationDigest(
        identityPublicKey: bytes(0x11),
        handshakeHash: bytes(0x23),
      );
      expect(session1, isNot(session2),
          reason: 'this is what stops an attestation being replayed onto '
              'another session of the same agent');
    });

    test('a different identity key gives a different digest', () {
      expect(
        attestationDigest(
          identityPublicKey: bytes(0x11),
          handshakeHash: bytes(0x22),
        ),
        isNot(attestationDigest(
          identityPublicKey: bytes(0x12),
          handshakeHash: bytes(0x22),
        )),
      );
    });

    test('what separates pk from h is their fixed length, not the label', () {
      // The digest is a plain concatenation, as the specification writes it,
      // so the boundary between pk and h is not marked. It does not have to
      // be: pk is a 32-byte Ed25519 key and h a 32-byte SHA-256 hash, both
      // fixed, so no other pair produces the same bytes. Recorded here
      // because the invariant is a length invariant and would be silently
      // lost if either field ever became variable-length.
      final split4And4 = attestationDigest(
        identityPublicKey: bytes(0x11, 4),
        handshakeHash: bytes(0x11, 4),
      );
      final split3And5 = attestationDigest(
        identityPublicKey: bytes(0x11, 3),
        handshakeHash: bytes(0x11, 5),
      );
      expect(split4And4, split3And5,
          reason: 'variable-length fields WOULD collide — the label is a '
              'domain separator against other uses of the hash, not a '
              'field separator');
    });
  });

  group('wire framing', () {
    test('absence round-trips as absence, not as an empty attestation', () {
      final encoded = encodeAttestationPayload(null);
      expect(encoded.length, 1);
      expect(decodeAttestationPayload(encoded), isNull);
    });

    test('an attestation round-trips', () {
      final attestation = bytes(0x7f, 100);
      final decoded = decodeAttestationPayload(
        encodeAttestationPayload(attestation),
      );
      expect(decoded, attestation);
    });

    test('a malformed payload throws rather than being read tolerantly', () {
      // No old version is in the wild, so tolerance for a previous shape
      // would be a compatibility shim by another name.
      expect(() => decodeAttestationPayload(Uint8List(0)), throwsFormatException);
      expect(
        () => decodeAttestationPayload(Uint8List.fromList([0x00, 0x01])),
        throwsFormatException,
        reason: 'claims absence and carries bytes',
      );
      expect(
        () => decodeAttestationPayload(Uint8List.fromList([0x01])),
        throwsFormatException,
        reason: 'claims presence and carries none',
      );
      expect(
        () => decodeAttestationPayload(Uint8List.fromList([0x02, 0x03])),
        throwsFormatException,
        reason: 'unknown tag',
      );
    });
  });

  group('NoPlatformAttestation', () {
    const attestation = NoPlatformAttestation();

    test('offers nothing', () async {
      expect(await attestation.attest(bytes(0x01)), isNull);
    });

    test('a peer that offers nothing is unattested, which is not a failure',
        () async {
      final verdict = await attestation.verify(null, bytes(0x01));
      expect(verdict, isA<UnattestedPlatform>());
    });

    test('a peer that offers one is unattested, not invalid', () async {
      // "Cannot determine" is not "found invalid". Reporting invalid would
      // claim a verification that never ran and would tear down every session
      // with an attesting peer.
      final verdict = await attestation.verify(bytes(0x55, 64), bytes(0x01));
      expect(verdict, isA<UnattestedPlatform>());
      expect(verdict, isNot(isA<InvalidAttestation>()));
    });
  });
}
