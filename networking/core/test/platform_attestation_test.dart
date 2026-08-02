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
    test('absence round-trips as absence, not as empty evidence', () {
      final encoded = encodeAttestationPayload(null);
      expect(encoded.length, 1);
      expect(decodeAttestationPayload(encoded), isNull);
    });

    test('evidence round-trips with both halves intact', () {
      final evidence = AttestationEvidence(
        attestation: bytes(0x7f, 4096),
        signature: bytes(0x5a, 71),
      );
      final decoded =
          decodeAttestationPayload(encodeAttestationPayload(evidence))!;
      expect(decoded.attestation, evidence.attestation);
      expect(decoded.signature, evidence.signature);
    });

    test('the split is by the carried length, not by a guess', () {
      // The signature takes the remainder, so the attestation's length field
      // is the only thing deciding the boundary. A one-byte error in it would
      // silently move bytes from one field to the other, which is why the
      // round-trip is asserted on both halves separately and on a signature
      // whose length is not a round number.
      final evidence = AttestationEvidence(
        attestation: bytes(0xAB, 1),
        signature: bytes(0xCD, 1),
      );
      final encoded = encodeAttestationPayload(evidence);
      expect(encoded.length, 1 + 4 + 1 + 1);
      final decoded = decodeAttestationPayload(encoded)!;
      expect(decoded.attestation, hasLength(1));
      expect(decoded.signature, hasLength(1));
      expect(decoded.attestation.single, 0xAB);
      expect(decoded.signature.single, 0xCD);
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
        reason: 'claims presence and carries no length field',
      );
      expect(
        () => decodeAttestationPayload(
            Uint8List.fromList([0x01, 0x00, 0x00, 0x00, 0x00])),
        throwsFormatException,
        reason: 'claims presence and a zero-length attestation',
      );
      expect(
        () => decodeAttestationPayload(
            Uint8List.fromList([0x01, 0x00, 0x00, 0x00, 0x08, 0x01, 0x02])),
        throwsFormatException,
        reason: 'claims more attestation bytes than it carries',
      );
      expect(
        () => decodeAttestationPayload(
            Uint8List.fromList([0x01, 0x00, 0x00, 0x00, 0x02, 0x01, 0x02])),
        throwsFormatException,
        reason: 'carries the attestation and no signature',
      );
      expect(
        () => decodeAttestationPayload(Uint8List.fromList([0x02, 0x03])),
        throwsFormatException,
        reason: 'unknown tag',
      );
    });

    test('a version-4 payload does not decode as version 5', () {
      // Version 4 framed the attestation alone: [0x01][attestation...]. Read
      // as version 5 its first four attestation bytes are a length, so it is
      // either short or signature-less — it must never be mistaken for valid
      // evidence. This is the wire half of why the version bumped; ANNOUNCE
      // refuses the mismatch before a session gets here.
      final version4 =
          Uint8List.fromList([0x01, ...List.filled(64, 0x5a)]);
      expect(() => decodeAttestationPayload(version4), throwsFormatException);
    });

    test('framing rejects half-evidence at the encoder', () {
      expect(
        () => encodeAttestationPayload(AttestationEvidence(
          attestation: Uint8List(0),
          signature: bytes(0x01, 64),
        )),
        throwsArgumentError,
      );
      expect(
        () => encodeAttestationPayload(AttestationEvidence(
          attestation: bytes(0x01, 64),
          signature: Uint8List(0),
        )),
        throwsArgumentError,
      );
    });
  });

  group('NoPlatformAttestation', () {
    const attestation = NoPlatformAttestation();

    test('offers neither an attestation nor a signature', () async {
      expect(await attestation.attestationFor(bytes(0x11)), isNull);
      expect(await attestation.signSessionDigest(bytes(0x01)), isNull);
    });

    test('a peer that offers nothing is unattested, which is not a failure',
        () async {
      final verdict = await attestation.verify(
        evidence: null,
        digest: bytes(0x01),
        peerIdentityKey: bytes(0x11),
      );
      expect(verdict, isA<UnattestedPlatform>());
    });

    test('a peer that offers evidence is unattested, not invalid', () async {
      // "Cannot determine" is not "found invalid". Reporting invalid would
      // claim a verification that never ran and would tear down every session
      // with an attesting peer.
      final verdict = await attestation.verify(
        evidence: AttestationEvidence(
          attestation: bytes(0x55, 512),
          signature: bytes(0x66, 64),
        ),
        digest: bytes(0x01),
        peerIdentityKey: bytes(0x11),
      );
      expect(verdict, isA<UnattestedPlatform>());
      expect(verdict, isNot(isA<InvalidAttestation>()));
    });
  });
}
