import 'dart:typed_data';

import 'package:test/test.dart';

import 'package:grassroots_networking_core/src/session/android_key_attestation.dart';
import 'package:grassroots_networking_core/src/session/app_attest.dart';
import 'package:grassroots_networking_core/src/session/application_identity.dart';
import 'package:grassroots_networking_core/src/session/attestation_verifier.dart';
import 'package:grassroots_networking_core/src/session/platform_attestation.dart';
import 'package:grassroots_networking_core/src/session/x509.dart';

import 'helpers/attestation_fixtures.dart';

/// Both halves of the exchange of spec §Session Establishment, for both
/// platforms: "Per session each side sends that attestation together with a
/// signature by the attestation key over the digest H("glp attest" | pk | h)
/// ... and each side verifies both: the attestation against the platform's
/// root, and the signature against the key the attestation carries.  Either
/// failing tears the session down."
///
/// The fixtures are built in the test (helpers/attestation_fixtures.dart),
/// with their private keys, so the per-session signature is real ECDSA by the
/// attested key.
void main() {
  /// The peer's identity key, which the attestation's challenge names.
  final pk = Uint8List.fromList(List.generate(32, (i) => i + 1));

  /// This session's digest, and another session's.
  final digest = attestationDigest(
    identityPublicKey: pk,
    handshakeHash: Uint8List.fromList(List.filled(32, 0x22)),
  );
  final otherSessionDigest = attestationDigest(
    identityPublicKey: pk,
    handshakeHash: Uint8List.fromList(List.filled(32, 0x23)),
  );

  group('App Attest, both halves', () {
    final ios = AppAttestFixture();

    AppAttestResult verify({
      Uint8List? attestationObject,
      String? appId,
      Uint8List? assertion,
      Uint8List? sessionDigest,
      Uint8List? challenge,
      bool allowDevelopment = false,
    }) =>
        verifyAppAttestEvidence(
          attestationObject: attestationObject ?? ios.attestationObject(pk),
          appId: appId ?? ios.appId,
          assertion: assertion ?? ios.assertion(digest),
          digest: sessionDigest ?? digest,
          expectedChallenge: challenge ?? pk,
          at: fixtureTime,
          pinnedRoots: [ios.root],
          allowDevelopmentEnvironment: allowDevelopment,
        );

    test('an attestation and an assertion over this session verify', () {
      final result = verify();
      expect(result.appId, ios.appId);
      expect(result.keyIdentifier, ios.keyIdentifier);
    });

    test('an assertion over another session\'s digest is refused', () {
      // This is the binding to the session: the handshake hash is unique per
      // session, so an assertion lifted from one session does not verify on
      // another.
      expect(
        () => verify(assertion: ios.assertion(otherSessionDigest)),
        throwsA(isA<X509Exception>().having(
            (e) => e.message, 'message', contains('this session'))),
      );
    });

    test('an assertion by a key other than the attested one is refused', () {
      expect(
        () => verify(
          assertion: ios.assertion(digest, signer: TestEcKey('someone else')),
        ),
        throwsA(isA<X509Exception>()),
      );
    });

    test('an assertion naming another application is refused', () {
      expect(
        () => verify(
          assertion:
              ios.assertion(digest, forAppId: 'ABCDE12345.com.example.other'),
        ),
        throwsA(isA<X509Exception>().having(
            (e) => e.message, 'message', contains('different application'))),
      );
    });

    test('an assertion with counter 0 is refused', () {
      expect(
        () => verify(assertion: ios.assertion(digest, counter: 0)),
        throwsA(isA<X509Exception>().having(
            (e) => e.message, 'message', contains('counter'))),
      );
    });

    test('a malformed assertion is refused', () {
      expect(
        () => verify(assertion: Uint8List.fromList([1, 2, 3])),
        throwsA(isA<X509Exception>()),
      );
    });

    test('an App ID whose hash is not the attested one is refused', () {
      // The App ID travels beside the attestation object; Apple attests only
      // its SHA-256. A claimed identifier is accepted only where it hashes to
      // that.
      expect(
        () => verify(appId: 'ABCDE12345.com.example.other'),
        throwsA(isA<X509Exception>().having(
            (e) => e.message, 'message', contains('different application'))),
      );
    });

    test('an attestation naming another identity key is refused', () {
      expect(
        () => verify(challenge: Uint8List.fromList(List.filled(32, 0xAA))),
        throwsA(isA<X509Exception>().having(
            (e) => e.message, 'message', contains('nonce'))),
      );
    });

    test('a development-sandbox attestation is refused unless allowed', () {
      final sandbox = AppAttestFixture(aaguid: 'appattestdevelop');
      expect(
        () => verifyAppAttestEvidence(
          attestationObject: sandbox.attestationObject(pk),
          appId: sandbox.appId,
          assertion: sandbox.assertion(digest),
          digest: digest,
          expectedChallenge: pk,
          at: fixtureTime,
          pinnedRoots: [sandbox.root],
        ),
        throwsA(isA<X509Exception>().having(
            (e) => e.message, 'message', contains('sandbox'))),
      );
      expect(
        verifyAppAttestEvidence(
          attestationObject: sandbox.attestationObject(pk),
          appId: sandbox.appId,
          assertion: sandbox.assertion(digest),
          digest: digest,
          expectedChallenge: pk,
          at: fixtureTime,
          pinnedRoots: [sandbox.root],
          allowDevelopmentEnvironment: true,
        ).appId,
        sandbox.appId,
      );
    });

    test('Apple\'s root does not anchor a test chain', () {
      expect(
        () => verifyAppAttestEvidence(
          attestationObject: ios.attestationObject(pk),
          appId: ios.appId,
          assertion: ios.assertion(digest),
          digest: digest,
          expectedChallenge: pk,
          at: fixtureTime,
        ),
        throwsA(isA<X509Exception>()),
      );
    });
  });

  group('Android key attestation, both halves', () {
    final android = AndroidFixture();

    AndroidAttestationResult verify({
      List<Uint8List>? chain,
      Uint8List? signature,
      Uint8List? sessionDigest,
      Uint8List? challenge,
    }) =>
        verifyAndroidEvidence(
          chain: chain ?? android.chain(pk),
          signature: signature ?? android.sign(digest),
          digest: sessionDigest ?? digest,
          expectedChallenge: challenge ?? pk,
          at: fixtureTime,
          pinnedRoots: [android.root],
        );

    test('a chain and a signature over this session verify, naming the '
        'package, its version and the signing digest', () {
      final identity = verify().keyDescription.applicationIdentity!;
      expect(identity.packages,
          [const AndroidPackage(name: 'com.eshapiro.grassapp', version: 7)]);
      expect(identity.signatureDigests.single, android.signingDigest);
    });

    test('a signature over another session\'s digest is refused', () {
      expect(
        () => verify(signature: android.sign(otherSessionDigest)),
        throwsA(isA<X509Exception>().having(
            (e) => e.message, 'message', contains('this session'))),
      );
    });

    test('a signature by a key other than the attested one is refused', () {
      expect(
        () => verify(signature: TestEcKey('someone else').sign(digest)),
        throwsA(isA<X509Exception>()),
      );
    });

    test('a signature that is not DER is refused', () {
      expect(
        () => verify(signature: Uint8List.fromList(List.filled(64, 0x01))),
        throwsA(isA<X509Exception>()),
      );
    });

    test('a chain naming another identity key is refused', () {
      expect(
        () => verify(challenge: Uint8List.fromList(List.filled(32, 0xAA))),
        throwsA(isA<X509Exception>().having(
            (e) => e.message, 'message', contains('challenge'))),
      );
    });

    test('a software-level key is refused', () {
      final software = AndroidFixture(securityLevel: 0);
      expect(
        () => verifyAndroidEvidence(
          chain: software.chain(pk),
          signature: software.sign(digest),
          digest: digest,
          expectedChallenge: pk,
          at: fixtureTime,
          pinnedRoots: [software.root],
        ),
        throwsA(isA<X509Exception>().having(
            (e) => e.message, 'message', contains('not hardware-backed'))),
      );
    });

    test('a certificate forged with a genuinely attested key is refused', () {
      // The attack the CA check stops. An Android attestation key is a
      // signing key the application holds, so an application can sign a
      // certificate of its own making — here naming another identity key and
      // another package — and chain it through its genuine attestation
      // certificate to the root. The genuine certificate is not a CA.
      final otherKey = Uint8List.fromList(List.filled(32, 0xEE));
      final forged = certificate(
        subject: 'Forged Key',
        subjectKey: TestEcKey('forged'),
        issuer: 'Android Keystore Key',
        issuerKey: android.attestationKey,
        serial: 4,
        extensions: [
          extension(
            '1.3.6.1.4.1.11129.2.1.17',
            AndroidFixture.keyDescription(
              challenge: otherKey,
              packageName: 'com.example.repackaged',
              version: 1,
              signingDigest: Uint8List(32),
            ),
          ),
        ],
      );
      expect(
        () => verifyAndroidEvidence(
          chain: [forged, ...android.chain(pk)],
          signature: TestEcKey('forged').sign(digest),
          digest: digest,
          expectedChallenge: otherKey,
          at: fixtureTime,
          pinnedRoots: [android.root],
        ),
        throwsA(isA<X509Exception>().having(
            (e) => e.message, 'message', contains('not a CA'))),
      );
    });
  });

  group('the attestation field', () {
    test('App Attest round-trips', () {
      final content = decodePlatformAttestation(encodePlatformAttestation(
        AppAttestContent(
          appId: 'ABCDE12345.com.eshapiro.grassapp',
          attestationObject: Uint8List.fromList([9, 8, 7]),
        ),
      ));
      expect(content, isA<AppAttestContent>());
      content as AppAttestContent;
      expect(content.appId, 'ABCDE12345.com.eshapiro.grassapp');
      expect(content.attestationObject, [9, 8, 7]);
    });

    test('an Android chain round-trips, leaf first', () {
      final content = decodePlatformAttestation(encodePlatformAttestation(
        AndroidKeyAttestationContent([
          Uint8List.fromList([1]),
          Uint8List.fromList([2, 2]),
          Uint8List.fromList([3, 3, 3]),
        ]),
      ));
      expect(content, isA<AndroidKeyAttestationContent>());
      expect((content as AndroidKeyAttestationContent).chain,
          [[1], [2, 2], [3, 3, 3]]);
    });

    test('anything else throws rather than being read tolerantly', () {
      for (final (bytes, reason) in [
        (<int>[], 'empty'),
        ([0x03, 0x00], 'unknown platform'),
        ([0x01, 0x00], 'App Attest without its App ID length'),
        ([0x01, 0x00, 0x00, 0x01], 'App Attest with an empty App ID'),
        ([0x01, 0x00, 0x02, 0x41, 0x42], 'App Attest with no object'),
        ([0x02], 'Android without its count'),
        ([0x02, 0x00], 'Android with no certificate'),
        ([0x02, 0x01, 0x00, 0x00, 0x00, 0x05, 0x01], 'truncated certificate'),
        ([0x02, 0x01, 0x00, 0x00, 0x00, 0x01, 0x01, 0xFF], 'trailing bytes'),
      ]) {
        expect(
          () => decodePlatformAttestation(Uint8List.fromList(bytes)),
          throwsFormatException,
          reason: reason,
        );
      }
    });
  });

  group('AttestationVerifier', () {
    final ios = AppAttestFixture();
    final android = AndroidFixture();
    final verifier = AttestationVerifier(
      clock: () => fixtureTime,
      appAttestRoots: [ios.root],
      androidRoots: [android.root],
    );

    AttestationEvidence iosEvidence({Uint8List? assertion}) =>
        AttestationEvidence(
          attestation: encodePlatformAttestation(AppAttestContent(
            appId: ios.appId,
            attestationObject: ios.attestationObject(pk),
          )),
          signature: assertion ?? ios.assertion(digest),
        );

    AttestationEvidence androidEvidence({Uint8List? signature}) =>
        AttestationEvidence(
          attestation: encodePlatformAttestation(
            AndroidKeyAttestationContent(android.chain(pk)),
          ),
          signature: signature ?? android.sign(digest),
        );

    AttestationVerdict run(AttestationEvidence? evidence) => verifier.verify(
          evidence: evidence,
          digest: digest,
          peerIdentityKey: pk,
        );

    test('no evidence is unattested, not a failure', () {
      expect(run(null), isA<UnattestedPlatform>());
    });

    test('an iOS peer is attested as its App ID', () {
      final verdict = run(iosEvidence());
      expect(verdict, isA<AttestedApplication>());
      expect((verdict as AttestedApplication).identity,
          IosApplicationIdentity(ios.appId));
    });

    test('an Android peer is attested as its package, version and digest', () {
      final verdict = run(androidEvidence());
      expect(verdict, isA<AttestedApplication>());
      expect(
        (verdict as AttestedApplication).identity,
        AndroidApplicationIdentity(
          packages: const [
            AndroidPackage(name: 'com.eshapiro.grassapp', version: 7),
          ],
          signatureDigests: [android.signingDigest],
        ),
      );
    });

    test('a signature over another session is invalid, on either platform',
        () {
      expect(run(iosEvidence(assertion: ios.assertion(otherSessionDigest))),
          isA<InvalidAttestation>());
      expect(
          run(androidEvidence(signature: android.sign(otherSessionDigest))),
          isA<InvalidAttestation>());
    });

    test('evidence verified against another peer\'s key is invalid', () {
      // The digest and the challenge both name the peer's key: the same
      // evidence presented as another agent's does not verify.
      final otherPk = Uint8List.fromList(List.filled(32, 0x77));
      expect(
        verifier.verify(
          evidence: iosEvidence(),
          digest: attestationDigest(
            identityPublicKey: otherPk,
            handshakeHash: Uint8List.fromList(List.filled(32, 0x22)),
          ),
          peerIdentityKey: otherPk,
        ),
        isA<InvalidAttestation>(),
      );
    });

    test('a field that does not decode is invalid, not unattested', () {
      expect(
        run(AttestationEvidence(
          attestation: Uint8List.fromList([0x55, 1, 2, 3]),
          signature: Uint8List.fromList([0x66]),
        )),
        isA<InvalidAttestation>(),
      );
    });

    test('by default the real roots are pinned, and a test chain is invalid',
        () {
      expect(
        const AttestationVerifier().verify(
          evidence: iosEvidence(),
          digest: digest,
          peerIdentityKey: pk,
        ),
        isA<InvalidAttestation>(),
      );
    });
  });
}
