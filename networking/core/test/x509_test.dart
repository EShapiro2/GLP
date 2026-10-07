import 'dart:typed_data';

import 'package:test/test.dart';

import 'package:grassroots_networking_core/src/session/attestation_roots.dart';
import 'package:grassroots_networking_core/src/session/x509.dart';

import 'helpers/attestation_fixtures.dart';

/// The X.509 slice attestation verification rests on (spec §Session
/// Establishment).
///
/// The pinned Google roots are the fixtures wherever they can be: they are
/// real certificates, one RSA-4096 and one EC P-384, both self-signed, so
/// verifying each against itself exercises both signature paths against bytes
/// Google published rather than against something this test made up.
void main() {
  final roots = androidAttestationRootsDer();
  // Inside both roots' windows: the RSA one runs 2022→2042, the EC one
  // 2025→2035.
  final now = DateTime.utc(2026, 8, 3);

  group('pinned Android roots', () {
    test('there are two, and both parse', () {
      expect(roots, hasLength(2),
          reason: 'pinning one rejects roughly half the fleet — the RSA root '
              'and the EC one effective 2026-02-01 are both live');
      for (final der in roots) {
        expect(() => X509Certificate.fromDer(der), returnsNormally);
      }
    });

    test('the RSA root is self-signed and verifies under its own key', () {
      final rsa = X509Certificate.fromDer(roots[0]);
      expect(rsa.signatureAlgorithm, '1.2.840.113549.1.1.11',
          reason: 'sha256WithRSAEncryption');
      expect(rsa.issuerDer, rsa.subjectDer, reason: 'self-signed');
      expect(rsa.verifies(rsa), isTrue);
    });

    test('the EC root is self-signed and verifies under its own key', () {
      final ec = X509Certificate.fromDer(roots[1]);
      expect(ec.signatureAlgorithm, '1.2.840.10045.4.3.3',
          reason: 'ecdsa-with-SHA384');
      expect(ec.issuerDer, ec.subjectDer, reason: 'self-signed');
      expect(ec.verifies(ec), isTrue);
    });

    test('their validity windows are the published ones', () {
      final rsa = X509Certificate.fromDer(roots[0]);
      final ec = X509Certificate.fromDer(roots[1]);
      expect(rsa.notBefore, DateTime.utc(2022, 3, 20, 18, 7, 48));
      expect(rsa.notAfter, DateTime.utc(2042, 3, 15, 18, 7, 48));
      expect(ec.notBefore, DateTime.utc(2025, 7, 17, 22, 32, 18));
      expect(ec.notAfter, DateTime.utc(2035, 7, 15, 22, 32, 18));
      expect(rsa.isValidAt(now), isTrue);
      expect(ec.isValidAt(now), isTrue);
    });

    test('a single-byte change anywhere stops the signature verifying', () {
      // The whole pin rests on this: a chain is an adversary's input, and a
      // verifier that accepts a mutated certificate accepts a forged one.
      for (final index in [0, 1]) {
        final cert = X509Certificate.fromDer(roots[index]);
        final tampered = Uint8List.fromList(roots[index]);
        // Flip a bit inside the signature itself.
        tampered[tampered.length - 2] ^= 0x01;
        final forged = X509Certificate.fromDer(tampered);
        expect(cert.verifies(forged), isFalse,
            reason: 'root $index accepted a tampered signature');
      }
    });
  });

  group('validateChain', () {
    test('a pinned root anchors by byte identity', () {
      expect(
        () => validateChain(chain: [roots[1]], pinnedRoots: roots, at: now),
        returnsNormally,
      );
    });

    test('an unpinned self-signed certificate does not anchor', () {
      // The EC root alone as the pin set, presented the RSA root: it neither
      // matches by bytes nor chains to it.
      expect(
        () => validateChain(
          chain: [roots[0]],
          pinnedRoots: [roots[1]],
          at: now,
        ),
        throwsA(isA<X509Exception>()),
      );
    });

    test('an empty chain and an empty pin set are both refused', () {
      expect(
        () => validateChain(chain: [], pinnedRoots: roots, at: now),
        throwsA(isA<X509Exception>()),
      );
      expect(
        () => validateChain(chain: [roots[0]], pinnedRoots: [], at: now),
        throwsA(isA<X509Exception>()),
        reason: 'a build with no pins verifies nothing; it must not pass '
            'everything',
      );
    });

    test('a certificate outside its validity window is refused', () {
      // Before the EC root existed.
      expect(
        () => validateChain(
          chain: [roots[1]],
          pinnedRoots: roots,
          at: DateTime.utc(2020, 1, 1),
        ),
        throwsA(isA<X509Exception>()),
      );
      // After the RSA root expires.
      expect(
        () => validateChain(
          chain: [roots[0]],
          pinnedRoots: roots,
          at: DateTime.utc(2050, 1, 1),
        ),
        throwsA(isA<X509Exception>()),
      );
    });

    test('a chain whose issuer names do not join is refused', () {
      // Two unrelated self-signed certificates stacked: link 0's issuer is not
      // link 1's subject, and the mismatch is caught before any signature
      // arithmetic.
      expect(
        () => validateChain(
          chain: [roots[0], roots[1]],
          pinnedRoots: roots,
          at: now,
        ),
        throwsA(isA<X509Exception>().having(
          (e) => e.message,
          'message',
          contains('issuer'),
        )),
      );
    });

    test('a certificate issued by one that is not a CA is refused', () {
      // RFC 5280 §6.1.4(k). A leaf's key may sign, but not issue: without
      // this an application's attestation key could issue a certificate of
      // its own making that chains to the root through its genuine one.
      final rootKey = TestEcKey('x509 root');
      final leafKey = TestEcKey('x509 leaf');
      final root = rootCertificate('X509 Test Root', rootKey);
      final leaf = certificate(
        subject: 'X509 Test Leaf',
        subjectKey: leafKey,
        issuer: 'X509 Test Root',
        issuerKey: rootKey,
        serial: 2,
      );
      final issuedByLeaf = certificate(
        subject: 'X509 Test Forgery',
        subjectKey: TestEcKey('x509 forgery'),
        issuer: 'X509 Test Leaf',
        issuerKey: leafKey,
        serial: 3,
      );
      expect(
        validateChain(chain: [leaf, root], pinnedRoots: [root], at: fixtureTime)
            .subjectDer,
        X509Certificate.fromDer(leaf).subjectDer,
        reason: 'the leaf itself chains: its issuer is a CA',
      );
      expect(
        () => validateChain(
          chain: [issuedByLeaf, leaf, root],
          pinnedRoots: [root],
          at: fixtureTime,
        ),
        throwsA(isA<X509Exception>().having(
          (e) => e.message,
          'message',
          contains('not a CA'),
        )),
      );
      // Anchored by signature rather than by bytes, the same: a pinned
      // certificate that is not a CA anchors nothing.
      expect(
        () => validateChain(
          chain: [issuedByLeaf],
          pinnedRoots: [leaf],
          at: fixtureTime,
        ),
        throwsA(isA<X509Exception>()),
      );
    });
  });
}
