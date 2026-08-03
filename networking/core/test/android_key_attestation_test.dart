import 'dart:convert';
import 'dart:typed_data';

import 'package:test/test.dart';

import 'package:grassroots_networking_core/src/session/android_key_attestation.dart';
import 'package:grassroots_networking_core/src/session/attestation_roots.dart';
import 'package:grassroots_networking_core/src/session/x509.dart';

/// The Android key-attestation verifier (spec §Session Establishment).
///
/// WHAT THESE FIXTURES ARE, AND WHAT THEY ARE NOT. The chain below was built
/// here with openssl, and its KeyDescription extension was encoded to Google's
/// published schema
/// (source.android.com/docs/security/features/keystore/attestation). It
/// exercises every branch of the verifier. It does NOT prove that a real
/// device emits what this parser expects: no Android device, SDK or emulator
/// exists on the machine this was written on, so a shared misreading of the
/// schema between the fixture and the parser would pass both. That gap closes
/// only against hardware.
///
/// What the fixtures DO establish honestly is the chain arithmetic, the
/// challenge binding, the security-level refusal and the application-identity
/// matching — all of which are this layer's logic rather than Android's format.
void main() {
  Uint8List der(String b64) => Uint8List.fromList(base64.decode(b64));

  // A test root and a leaf it signed, the leaf carrying a KeyDescription whose
  // challenge is the 32 bytes 0x00..0x1f and whose application identity is
  // com.eshapiro.grassapp.
  final testRoot = der(
    'MIIByTCCAW+gAwIBAgIUdKZRacY+qIIuDvd+j8sCoqvSEz8wCgYIKoZIzj0EAwIwOjEeMBwGA1'
    'UEAwwVVGVzdCBBdHRlc3RhdGlvbiBSb290MRgwFgYDVQQKDA9HcmFzc3Jvb3RzIFRlc3QwHhcN'
    'MjYwODAyMjE0NzMxWhcNMzYwNzMwMjE0NzMxWjA6MR4wHAYDVQQDDBVUZXN0IEF0dGVzdGF0aW'
    '9uIFJvb3QxGDAWBgNVBAoMD0dyYXNzcm9vdHMgVGVzdDBZMBMGByqGSM49AgEGCCqGSM49AwEH'
    'A0IABBQksUlfODZQZ9Wf6t3couJW+k8MzWtMJrdbHC6P60xoDLo+D2to1G65foAE/U2hdCjRrK'
    'h4iFYyubT1GwmiMdyjUzBRMB0GA1UdDgQWBBRkkWXrnDL4zDMLNxAnLcis9EX4ljAfBgNVHSME'
    'GDAWgBRkkWXrnDL4zDMLNxAnLcis9EX4ljAPBgNVHRMBAf8EBTADAQH/MAoGCCqGSM49BAMCA0'
    'gAMEUCICOTIKBUTE3Jtv48uqhp0deejd5PRxIx2aVug3vnf/nxAiEAzwzjUnYln8NJC16raQDD'
    'VXrrXqCix+8J5XLX5wzJ9ms=',
  );
  final testLeaf = der(
    'MIICPDCCAeKgAwIBAgIUE52yQl9vodcdQ15thHM1rtrlzCIwCgYIKoZIzj0EAwIwOjEeMBwGA1'
    'UEAwwVVGVzdCBBdHRlc3RhdGlvbiBSb290MRgwFgYDVQQKDA9HcmFzc3Jvb3RzIFRlc3QwHhcN'
    'MjYwODAzMDYwODExWhcNMzEwODAyMDYwODExWjAcMRowGAYDVQQDDBFUZXN0IEF0dGVzdGVkIE'
    'tleTBZMBMGByqGSM49AgEGCCqGSM49AwEHA0IABC+Rqt8WzfesCuFnQLx8+lw+Y9Ce6TZbFwpr'
    'o6hZFvZdO9PD+5tFzVf9rsljUPNWLr47pJQVo683nKYY5k67ahqjgeMwgeAwCQYDVR0TBAIwAD'
    'CBkgYKKwYBBAHWeQIBEQSBgzCBgAICAMgKAQECAgDICgEBBCAAAQIDBAUGBwgJCgsMDQ4PEBES'
    'ExQVFhcYGRobHB0eHwQAMEq/hUVGBEQwQjEcMBoEFWNvbS5lc2hhcGlyby5ncmFzc2FwcAIBBz'
    'EiBCCNGUywkto39S+Ct1HmtxW3sQnje1ys3uNONhIx1u/K0TAAMB0GA1UdDgQWBBTB06+HBD1n'
    'QfAnHNo3DXtlkKqECzAfBgNVHSMEGDAWgBRkkWXrnDL4zDMLNxAnLcis9EX4ljAKBggqhkjOPQ'
    'QDAgNIADBFAiAe48BLd/UF3+f2LA7WoTNFGoAk6/WNLvQf1bAoggucHQIhAPesl99nZuGe8Afy'
    '73V95YCgFLPy2nSPf8WIzFtLy7jZ',
  );
  // Identical but for its SecurityLevel, which is Software(0) in both places.
  final softwareLeaf = der(
    'MIICOzCCAeKgAwIBAgIUE52yQl9vodcdQ15thHM1rtrlzCMwCgYIKoZIzj0EAwIwOjEeMBwGA1'
    'UEAwwVVGVzdCBBdHRlc3RhdGlvbiBSb290MRgwFgYDVQQKDA9HcmFzc3Jvb3RzIFRlc3QwHhcN'
    'MjYwODAzMDYwODExWhcNMzEwODAyMDYwODExWjAcMRowGAYDVQQDDBFUZXN0IEF0dGVzdGVkIE'
    'tleTBZMBMGByqGSM49AgEGCCqGSM49AwEHA0IABC+Rqt8WzfesCuFnQLx8+lw+Y9Ce6TZbFwpr'
    'o6hZFvZdO9PD+5tFzVf9rsljUPNWLr47pJQVo683nKYY5k67ahqjgeMwgeAwCQYDVR0TBAIwAD'
    'CBkgYKKwYBBAHWeQIBEQSBgzCBgAICAMgKAQACAgDICgEABCAAAQIDBAUGBwgJCgsMDQ4PEBES'
    'ExQVFhcYGRobHB0eHwQAMEq/hUVGBEQwQjEcMBoEFWNvbS5lc2hhcGlyby5ncmFzc2FwcAIBBz'
    'EiBCCNGUywkto39S+Ct1HmtxW3sQnje1ys3uNONhIx1u/K0TAAMB0GA1UdDgQWBBTB06+HBD1n'
    'QfAnHNo3DXtlkKqECzAfBgNVHSMEGDAWgBRkkWXrnDL4zDMLNxAnLcis9EX4ljAKBggqhkjOPQ'
    'QDAgNHADBEAiBty6xGJltvEk2Nz0Eg7pGoBxpqIl6UicGAmhRI74o3RQIgVQ0pzL0wMvDQeZJm'
    'O/MkvxN0krg4cByFtzFQ55AHlxw=',
  );

  /// The challenge the fixture attests over: the agent's identity key.
  final challenge = Uint8List.fromList(List.generate(32, (i) => i));
  final signingDigest = Uint8List.fromList([
    0x8d, 0x19, 0x4c, 0xb0, 0x92, 0xda, 0x37, 0xf5, //
    0x2f, 0x82, 0xb7, 0x51, 0xe6, 0xb7, 0x15, 0xb7,
    0xb1, 0x09, 0xe3, 0x7b, 0x5c, 0xac, 0xde, 0xe3,
    0x4e, 0x36, 0x12, 0x31, 0xd6, 0xef, 0xca, 0xd1,
  ]);
  final now = DateTime.utc(2027, 1, 1); // inside the fixture's window

  group('KeyDescription parsing', () {
    test('reads the challenge, the levels and the application identity', () {
      final leaf = X509Certificate.fromDer(testLeaf);
      final kd = AndroidKeyDescription.fromCertificate(leaf);

      expect(kd.attestationVersion, 200);
      expect(kd.attestationSecurityLevel,
          AndroidSecurityLevel.trustedEnvironment);
      expect(kd.keymasterSecurityLevel,
          AndroidSecurityLevel.trustedEnvironment);
      expect(kd.attestationChallenge, challenge);

      final identity = kd.applicationIdentity;
      expect(identity, isNotNull);
      expect(identity!.packageNames, ['com.eshapiro.grassapp']);
      expect(identity.signatureDigests, hasLength(1));
      expect(identity.signatureDigests.single, signingDigest);
    });

    test('the tag-709 walk finds a high-tag-number field', () {
      // attestationApplicationId is tag 709, which needs DER's
      // high-tag-number form (0xBF 0x85 0x45). A parser that only handled the
      // low form would silently find no application identity and this layer
      // would then accept any package, so the identity being read at all is
      // the assertion.
      final kd = AndroidKeyDescription.fromCertificate(
        X509Certificate.fromDer(testLeaf),
      );
      expect(kd.applicationIdentity?.packageNames, isNotEmpty);
    });

    test('a certificate with no KeyDescription is refused', () {
      // The pinned Google root is a real certificate that carries no
      // KeyDescription — it is a CA, not an attested key.
      final root = X509Certificate.fromDer(androidAttestationRootsDer().first);
      expect(
        () => AndroidKeyDescription.fromCertificate(root),
        throwsA(isA<X509Exception>().having(
          (e) => e.message,
          'message',
          contains('no KeyDescription'),
        )),
      );
    });
  });

  group('verifyAndroidAttestation', () {
    test('a chain to the pinned root with the right challenge verifies', () {
      final result = verifyAndroidAttestation(
        chain: [testLeaf, testRoot],
        expectedChallenge: challenge,
        at: now,
        pinnedRoots: [testRoot],
        expectedPackageNames: {'com.eshapiro.grassapp'},
        expectedSignatureDigests: [signingDigest],
      );
      expect(result.keyDescription.attestationChallenge, challenge);
      expect(result.leaf.der, testLeaf);
    });

    test('a challenge that is not this agent\'s identity key is refused', () {
      // This is the binding the whole two-step rests on: the attestation names
      // the identity key, and an attestation naming another key is an
      // attestation of some other agent.
      final wrong = Uint8List.fromList(List.filled(32, 0xAA));
      expect(
        () => verifyAndroidAttestation(
          chain: [testLeaf, testRoot],
          expectedChallenge: wrong,
          at: now,
          pinnedRoots: [testRoot],
        ),
        throwsA(isA<X509Exception>().having(
          (e) => e.message,
          'message',
          contains('challenge'),
        )),
      );
    });

    test('a software-level attestation is refused', () {
      // A software-level key is in the same place an attacker with the device
      // already is, so it attests nothing this protocol wants.
      expect(
        () => verifyAndroidAttestation(
          chain: [softwareLeaf, testRoot],
          expectedChallenge: challenge,
          at: now,
          pinnedRoots: [testRoot],
        ),
        throwsA(isA<X509Exception>().having(
          (e) => e.message,
          'message',
          contains('not hardware-backed'),
        )),
      );
    });

    test('a chain that does not reach a pinned root is refused', () {
      // The real Google roots as the pin set: the fixture chains to neither.
      expect(
        () => verifyAndroidAttestation(
          chain: [testLeaf, testRoot],
          expectedChallenge: challenge,
          at: now,
        ),
        throwsA(isA<X509Exception>()),
      );
    });

    test('a package or signing digest this build does not accept is refused',
        () {
      expect(
        () => verifyAndroidAttestation(
          chain: [testLeaf, testRoot],
          expectedChallenge: challenge,
          at: now,
          pinnedRoots: [testRoot],
          expectedPackageNames: {'com.example.other'},
        ),
        throwsA(isA<X509Exception>()),
      );
      expect(
        () => verifyAndroidAttestation(
          chain: [testLeaf, testRoot],
          expectedChallenge: challenge,
          at: now,
          pinnedRoots: [testRoot],
          expectedSignatureDigests: [Uint8List(32)],
        ),
        throwsA(isA<X509Exception>()),
      );
    });

    test('an expired chain is refused', () {
      expect(
        () => verifyAndroidAttestation(
          chain: [testLeaf, testRoot],
          expectedChallenge: challenge,
          at: DateTime.utc(2040, 1, 1),
          pinnedRoots: [testRoot],
        ),
        throwsA(isA<X509Exception>()),
      );
    });
  });
}
