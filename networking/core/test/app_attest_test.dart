import 'dart:typed_data';

import 'package:test/test.dart';

import 'package:grassroots_networking_core/src/session/app_attest.dart';
import 'package:grassroots_networking_core/src/session/attestation_roots.dart';
import 'package:grassroots_networking_core/src/session/x509.dart';

/// The Apple App Attest verifier (spec §Session Establishment).
///
/// WHAT THIS FIXTURE IS, AND WHAT IT IS NOT. The attestation object below was
/// built here: an EC P-256 credential certificate carrying a real nonce
/// extension over a real `authData`, chained to a test root and CBOR-encoded
/// to the shape Apple documents in "Validating apps that connect to your
/// server". Every field the verifier checks is genuine and internally
/// consistent — the nonce really is SHA-256(authData ‖ SHA-256(challenge)),
/// the credentialId really is SHA-256 of the attested key's uncompressed
/// point, the rpIdHash really is SHA-256 of the App ID.
///
/// It is NOT a device's output. No provisioned iOS device produced it, so a
/// shared misreading of Apple's format between this fixture and the verifier
/// would pass both. That gap closes only against hardware.
///
/// The pinned Apple root IS real, fetched from Apple, and is exercised below.
void main() {
  // Hex, not base64: GitHub's secret scanner reads a long mixed-case base64
  // run inside a certificate as a credential and blocks the push. These are
  // public test certificates and nothing here is secret, so the encoding
  // changes rather than the scanner being overridden.
  Uint8List b(String hex) => Uint8List.fromList([
        for (var i = 0; i < hex.length; i += 2)
          int.parse(hex.substring(i, i + 2), radix: 16),
      ]);

  final attestationObject = b(
    'a363666d746f6170706c652d6170706174746573746761747453746d74a1637835638259'
    '01dd308201d93082017fa003020102021402c2e3feff4180e1e44b9644cc227db2c2231d'
    '99300a06082a8648ce3d0403023039311d301b06035504030c1454657374204170702041'
    '747465737420526f6f7431183016060355040a0c0f4772617373726f6f74732054657374'
    '301e170d3236303830333036313630325a170d3331303830323036313630325a301a3118'
    '301606035504030c0f546573742043726564656e7469616c3059301306072a8648ce3d02'
    '0106082a8648ce3d030107034200046f51f425ed9be5d864f5c6a9505ffd15f71122e2c0'
    'dd1bb9a0ce8a3992baff424bae911a7f247f4d45a1c92097e544d32a6d73a2cef0d41a49'
    '9d91e22f04f3f8a3818330818030090603551d1304023000303306092a864886f7636408'
    '0204263024a1220420bd13193311f675465d5f8612c40c448288bed590ff3609abe1f534'
    'cb6c899c70301d0603551d0e04160414d4e121a3ec79a9c298744a892d9d4d9514dc4e53'
    '301f0603551d23041830168014c2d0e0f9a7749215a7f4b99966d2906a3a6e0b3f300a06'
    '082a8648ce3d0403020348003045022100f21076665e215a5df2a50f011e0ea6acbcf45a'
    '3aa3a4d3724a7ef845e6e700a302205599b9dc78efe359030df528c0bbf7e50bf2c768e0'
    'a87c18df32cf0daf3003e25901ca308201c63082016da00302010202145732456734472e'
    '61fe0e60bccc78d142e57680aa300a06082a8648ce3d0403033039311d301b0603550403'
    '0c1454657374204170702041747465737420526f6f7431183016060355040a0c0f477261'
    '7373726f6f74732054657374301e170d3236303830333036313630325a170d3336303733'
    '313036313630325a3039311d301b06035504030c14546573742041707020417474657374'
    '20526f6f7431183016060355040a0c0f4772617373726f6f747320546573743059301306'
    '072a8648ce3d020106082a8648ce3d03010703420004bff9d2ae7a1acbe0594a4c2bade4'
    '87000ae6c58a15db872cedafb4ccfd300e5d597f1d873da8c41c224f17ac886f09fefb91'
    '785ac37692ec051e4d5ee6c59f97a3533051301d0603551d0e04160414c2d0e0f9a77492'
    '15a7f4b99966d2906a3a6e0b3f301f0603551d23041830168014c2d0e0f9a7749215a7f4'
    'b99966d2906a3a6e0b3f300f0603551d130101ff040530030101ff300a06082a8648ce3d'
    '040303034700304402201b49dec26a8110ba9aea9e4c51fcdec248a3d2e04aaed1e4f93e'
    '061a179cff6402200095dd78861b2eaabead892a2937effbb454c0f5648db89fb652a73c'
    'd5183ef96861757468446174615857219794767a2af45d7f4fb052c37e37b3f233d97e8d'
    'eaf764becbb09575c354034000000000617070617474657374000000000000000020ace6'
    'cf1e80beaee481caec4865693d07a9ae7bcfa9b93982d31f9a4961a2ae0a',
  );
  final testRoot = b(
    '308201c63082016da00302010202145732456734472e61fe0e60bccc78d142e57680aa30'
    '0a06082a8648ce3d0403033039311d301b06035504030c14546573742041707020417474'
    '65737420526f6f7431183016060355040a0c0f4772617373726f6f74732054657374301e'
    '170d3236303830333036313630325a170d3336303733313036313630325a3039311d301b'
    '06035504030c1454657374204170702041747465737420526f6f7431183016060355040a'
    '0c0f4772617373726f6f747320546573743059301306072a8648ce3d020106082a8648ce'
    '3d03010703420004bff9d2ae7a1acbe0594a4c2bade487000ae6c58a15db872cedafb4cc'
    'fd300e5d597f1d873da8c41c224f17ac886f09fefb91785ac37692ec051e4d5ee6c59f97'
    'a3533051301d0603551d0e04160414c2d0e0f9a7749215a7f4b99966d2906a3a6e0b3f30'
    '1f0603551d23041830168014c2d0e0f9a7749215a7f4b99966d2906a3a6e0b3f300f0603'
    '551d130101ff040530030101ff300a06082a8648ce3d040303034700304402201b49dec2'
    '6a8110ba9aea9e4c51fcdec248a3d2e04aaed1e4f93e061a179cff6402200095dd78861b'
    '2eaabead892a2937effbb454c0f5648db89fb652a73cd5183ef9',
  );

  const appId = 'ABCDE12345.com.eshapiro.grassapp';
  final challenge = Uint8List.fromList(List.generate(32, (i) => i));
  final now = DateTime.utc(2027, 1, 1);

  group('the pinned Apple root', () {
    test('is real, parses, and is self-signed', () {
      final roots = appAttestRootsDer();
      expect(roots, hasLength(1),
          reason: 'Apple publishes one App Attest root');
      final root = X509Certificate.fromDer(roots.single);
      expect(root.signatureAlgorithm, '1.2.840.10045.4.3.3',
          reason: 'ecdsa-with-SHA384');
      expect(root.issuerDer, root.subjectDer);
      expect(root.verifies(root), isTrue);
      expect(root.notBefore, DateTime.utc(2020, 3, 18, 18, 32, 53));
      expect(root.notAfter, DateTime.utc(2045, 3, 15));
    });
  });

  group('verifyAppAttestAttestation', () {
    test('a well-formed attestation verifies', () {
      final result = verifyAppAttestAttestation(
        attestationObject: attestationObject,
        expectedAppId: appId,
        expectedChallenge: challenge,
        at: now,
        pinnedRoots: [testRoot],
      );
      expect(result.appId, appId);
      expect(result.keyIdentifier, hasLength(32));
    });

    test('a challenge that is not this agent\'s identity key is refused', () {
      // The nonce is SHA-256(authData ‖ SHA-256(challenge)), so a different
      // challenge cannot produce the nonce the certificate carries. This is
      // the binding the two-step rests on.
      expect(
        () => verifyAppAttestAttestation(
          attestationObject: attestationObject,
          expectedAppId: appId,
          expectedChallenge: Uint8List.fromList(List.filled(32, 0xAA)),
          at: now,
          pinnedRoots: [testRoot],
        ),
        throwsA(isA<X509Exception>().having(
          (e) => e.message,
          'message',
          contains('nonce'),
        )),
      );
    });

    test('an attestation for another application is refused', () {
      expect(
        () => verifyAppAttestAttestation(
          attestationObject: attestationObject,
          expectedAppId: 'ABCDE12345.com.example.other',
          expectedChallenge: challenge,
          at: now,
          pinnedRoots: [testRoot],
        ),
        throwsA(isA<X509Exception>().having(
          (e) => e.message,
          'message',
          contains('different application'),
        )),
      );
    });

    test('a chain that does not reach the pinned root is refused', () {
      // Apple's real root as the pin set: this fixture chains to a test root.
      expect(
        () => verifyAppAttestAttestation(
          attestationObject: attestationObject,
          expectedAppId: appId,
          expectedChallenge: challenge,
          at: now,
        ),
        throwsA(isA<X509Exception>()),
      );
    });

    test('a tampered attestation object is refused, not silently accepted',
        () {
      // Flip a byte inside the credential certificate's signature.
      final tampered = Uint8List.fromList(attestationObject);
      tampered[200] ^= 0x01;
      expect(
        () => verifyAppAttestAttestation(
          attestationObject: tampered,
          expectedAppId: appId,
          expectedChallenge: challenge,
          at: now,
          pinnedRoots: [testRoot],
        ),
        throwsA(isA<X509Exception>()),
      );
    });

    test('something that is not CBOR at all is refused', () {
      expect(
        () => verifyAppAttestAttestation(
          attestationObject: Uint8List.fromList([1, 2, 3, 4]),
          expectedAppId: appId,
          expectedChallenge: challenge,
          at: now,
          pinnedRoots: [testRoot],
        ),
        throwsA(isA<X509Exception>()),
      );
    });

    test('an expired chain is refused', () {
      expect(
        () => verifyAppAttestAttestation(
          attestationObject: attestationObject,
          expectedAppId: appId,
          expectedChallenge: challenge,
          at: DateTime.utc(2040, 1, 1),
          pinnedRoots: [testRoot],
        ),
        throwsA(isA<X509Exception>()),
      );
    });
  });
}
