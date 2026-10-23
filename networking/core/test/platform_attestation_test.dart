import 'dart:typed_data';

import 'package:test/test.dart';

import 'package:grassroots_networking_core/src/session/application.dart';
import 'package:grassroots_networking_core/src/session/attestation_verifier.dart';
import 'package:grassroots_networking_core/src/session/platform_attestation.dart';

import 'helpers/attestation_fixtures.dart';

/// The attestation digest and its wire framing (GNA §2.6).
///
/// The digest, `H("glp attest" | pk | v | h)`, is what binds an attestation to
/// one session and to the application's version number: an identity key is
/// long-lived, so an attestation over the key alone would be replayable onto
/// every channel that agent opens, while the handshake hash is unique per
/// session, identical at both ends, and chosen by neither side alone; and the
/// version number is the application's only under the signature.
void main() {
  Uint8List bytes(int fill, [int length = 32]) =>
      Uint8List.fromList(List.filled(length, fill));

  group('attestationDigest', () {
    test('is a 32-byte digest and is deterministic', () {
      final a = attestationDigest(
        identityPublicKey: bytes(0x11),
        version: 7,
        handshakeHash: bytes(0x22),
      );
      final b = attestationDigest(
        identityPublicKey: bytes(0x11),
        version: 7,
        handshakeHash: bytes(0x22),
      );
      expect(a.length, 32);
      expect(a, b);
    });

    test('a different handshake hash gives a different digest', () {
      final session1 = attestationDigest(
        identityPublicKey: bytes(0x11),
        version: 7,
        handshakeHash: bytes(0x22),
      );
      final session2 = attestationDigest(
        identityPublicKey: bytes(0x11),
        version: 7,
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
          version: 7,
          handshakeHash: bytes(0x22),
        ),
        isNot(attestationDigest(
          identityPublicKey: bytes(0x12),
          version: 7,
          handshakeHash: bytes(0x22),
        )),
      );
    });

    test('a different version number gives a different digest', () {
      expect(
        attestationDigest(
          identityPublicKey: bytes(0x11),
          version: 7,
          handshakeHash: bytes(0x22),
        ),
        isNot(attestationDigest(
          identityPublicKey: bytes(0x11),
          version: 8,
          handshakeHash: bytes(0x22),
        )),
        reason: 'this is what makes the version number the one the '
            'application signed',
      );
    });

    test('the version number is eight bytes, big-endian, between pk and h',
        () {
      expect(
        attestationDigest(
          identityPublicKey: bytes(0x11),
          version: 0x0102030405060708,
          handshakeHash: bytes(0x22),
        ),
        sha256([
          ...'glp attest'.codeUnits,
          ...bytes(0x11),
          1, 2, 3, 4, 5, 6, 7, 8,
          ...bytes(0x22),
        ]),
      );
      expect(
          () => attestationDigest(
                identityPublicKey: bytes(0x11),
                version: -1,
                handshakeHash: bytes(0x22),
              ),
          throwsArgumentError);
    });

    test('what separates pk from h is their fixed length, not the label', () {
      // The digest is a plain concatenation, as the specification writes it,
      // so the boundary between pk and h is not marked. It does not have to
      // be: pk is a 32-byte Ed25519 key, v eight bytes and h a 32-byte
      // SHA-256 hash, all fixed, so no other triple produces the same bytes.
      // Recorded here because the invariant is a length invariant and would
      // be silently lost if a field ever became variable-length: pk of four
      // bytes, v = 7 and h of four bytes, and pk of five bytes, the fifth
      // 0x00, v = 0x0711 and h of three bytes, hash the same bytes.
      final split4And4 = attestationDigest(
        identityPublicKey: bytes(0x11, 4),
        version: 7,
        handshakeHash: bytes(0x11, 4),
      );
      final split5And3 = attestationDigest(
        identityPublicKey: Uint8List.fromList([...bytes(0x11, 4), 0x00]),
        version: 0x0711,
        handshakeHash: bytes(0x11, 3),
      );
      expect(split4And4, split5And3,
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

    test('evidence round-trips with its three parts intact', () {
      final evidence = AttestationEvidence(
        attestation: bytes(0x7f, 4096),
        version: 0x0102030405060708,
        signature: bytes(0x5a, 71),
      );
      final decoded =
          decodeAttestationPayload(encodeAttestationPayload(evidence))!;
      expect(decoded.attestation, evidence.attestation);
      expect(decoded.version, evidence.version);
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
        version: 42,
        signature: bytes(0xCD, 1),
      );
      final encoded = encodeAttestationPayload(evidence);
      expect(encoded.length, 1 + 8 + 4 + 1 + 1,
          reason: 'the tag, the version number, the length, the attestation '
              'and the signature');
      expect(encoded.sublist(1, 9), [0, 0, 0, 0, 0, 0, 0, 42],
          reason: 'the version number, eight bytes, big-endian');
      final decoded = decodeAttestationPayload(encoded)!;
      expect(decoded.version, 42);
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
      const v = [0, 0, 0, 0, 0, 0, 0, 42];
      expect(
        () => decodeAttestationPayload(Uint8List.fromList([0x01])),
        throwsFormatException,
        reason: 'claims presence and carries no version number',
      );
      expect(
        () => decodeAttestationPayload(Uint8List.fromList([0x01, ...v])),
        throwsFormatException,
        reason: 'claims presence and carries no length field',
      );
      expect(
        () => decodeAttestationPayload(
            Uint8List.fromList([0x01, ...v, 0x00, 0x00, 0x00, 0x00])),
        throwsFormatException,
        reason: 'claims presence and a zero-length attestation',
      );
      expect(
        () => decodeAttestationPayload(Uint8List.fromList(
            [0x01, ...v, 0x00, 0x00, 0x00, 0x08, 0x01, 0x02])),
        throwsFormatException,
        reason: 'claims more attestation bytes than it carries',
      );
      expect(
        () => decodeAttestationPayload(Uint8List.fromList(
            [0x01, ...v, 0x00, 0x00, 0x00, 0x02, 0x01, 0x02])),
        throwsFormatException,
        reason: 'carries the attestation and no signature',
      );
      expect(
        () => decodeAttestationPayload(Uint8List.fromList(
            [0x01, 0x80, 0, 0, 0, 0, 0, 0, 0, 0x00, 0x00, 0x00, 0x01, 1, 2])),
        throwsFormatException,
        reason: 'a version number of 2^63 or more',
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
          version: 42,
          signature: bytes(0x01, 64),
        )),
        throwsArgumentError,
      );
      expect(
        () => encodeAttestationPayload(AttestationEvidence(
          attestation: bytes(0x01, 64),
          version: 42,
          signature: Uint8List(0),
        )),
        throwsArgumentError,
      );
      expect(
        () => encodeAttestationPayload(AttestationEvidence(
          attestation: bytes(0x01, 64),
          version: -1,
          signature: bytes(0x01, 64),
        )),
        throwsArgumentError,
      );
    });
  });

  group('NoPlatformAttestation', () {
    const attestation = NoPlatformAttestation();

    test('offers no attestation, no version number and no signature',
        () async {
      expect(await attestation.attestationFor(bytes(0x11)), isNull);
      expect(await attestation.applicationVersion(), isNull);
      expect(
        await attestation.signSessionDigest(
          identityPublicKey: bytes(0x11),
          digest: bytes(0x01),
        ),
        isNull,
      );
    });

    test('a peer that offers nothing is unattested, which is not a failure',
        () async {
      final verdict = await attestation.verify(
        evidence: null,
        handshakeHash: bytes(0x01),
        peerIdentityKey: bytes(0x11),
      );
      expect(verdict, isA<UnattestedPlatform>());
    });

    test('a peer whose evidence does not verify is invalid, not unattested',
        () async {
      // It offers nothing of its own and still verifies what a peer offers:
      // the verifiers are in the core, so "cannot determine" no longer
      // arises, and an attestation "offered and found invalid" tears the
      // session down (spec §Session Establishment). Reporting it unattested
      // would let any bytes pass as an unattested peer's.
      final verdict = await attestation.verify(
        evidence: AttestationEvidence(
          attestation: bytes(0x55, 512),
          version: 42,
          signature: bytes(0x66, 64),
        ),
        handshakeHash: bytes(0x01),
        peerIdentityKey: bytes(0x11),
      );
      expect(verdict, isA<InvalidAttestation>());
    });

    test(
        'a peer whose evidence verifies is attested, with the application '
        'and the version number it signed', () async {
      final ios = AppAttestFixture();
      final pk = bytes(0x11);
      final digest = attestationDigest(
        identityPublicKey: pk,
        version: 42,
        handshakeHash: bytes(0x22),
      );
      final verdict = await NoPlatformAttestation(
        verifier: AttestationVerifier(
          clock: () => fixtureTime,
          appAttestRoots: [ios.root],
        ),
      ).verify(
        evidence: AttestationEvidence(
          attestation: encodePlatformAttestation(AppAttestContent(
            appId: ios.appId,
            attestationObject: ios.attestationObject(pk),
          )),
          version: 42,
          signature: ios.assertion(digest),
        ),
        handshakeHash: bytes(0x22),
        peerIdentityKey: pk,
      );
      expect(verdict, isA<AttestedApplication>());
      expect((verdict as AttestedApplication).application,
          const Application(
            developer: 'ABCDE12345',
            name: 'com.eshapiro.grassapp',
            version: 42,
          ));
    });
  });
}
