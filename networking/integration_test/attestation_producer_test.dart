// The platform's attestation producer, run on the platform through its
// channel (spec §Session Establishment).
//
// Run on a simulator or a device:
//
//   flutter test integration_test/attestation_producer_test.dart -d <device>
//
// What it establishes depends on where it runs, and it says which. A device
// that provides no attestation — an iOS simulator, a device without App
// Attest — answers null to both calls, which the peer reports as unattested.
// A device that provides one answers with its platform's attestation over the
// identity key and signs a digest with the key it attested, and both are
// checked here as far as a device can check its own: the reply's shape, the
// App ID or chain it names, and a fresh signature per digest. Verification
// against the platform's root is the core's, and is exercised by the core's
// tests.

import 'package:flutter/foundation.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:integration_test/integration_test.dart';

import 'package:grassroots_networking/grassroots_networking.dart';

void main() {
  IntegrationTestWidgetsFlutterBinding.ensureInitialized();

  final pk = Uint8List.fromList(List.generate(32, (i) => 0x40 + i));
  final digest1 = Uint8List.fromList(List.filled(32, 0x11));
  final digest2 = Uint8List.fromList(List.filled(32, 0x22));

  testWidgets('the producer is registered on this platform', (tester) async {
    // A call without its argument is answered by the producer with an error,
    // where an unregistered channel would throw MissingPluginException; a
    // null from `attest` below is then the producer's answer, not the
    // channel's silence.
    await expectLater(
      platformAttestationChannel.invokeMethod<Object?>('attest', {}),
      throwsA(isA<PlatformException>()
          .having((e) => e.code, 'code', 'badArguments')),
    );
  });

  testWidgets('the producer answers through its channel', (tester) async {
    final producer = NativePlatformAttestation();
    final attestation = await producer.attestationFor(pk);

    if (attestation == null) {
      debugPrint('[attestation_producer_test] this device provides no '
          'attestation: attest answered null');
      expect(
        await producer.signSessionDigest(identityPublicKey: pk, digest: digest1),
        isNull,
        reason: 'no attested key, so no signature: the same condition',
      );
      return;
    }

    final content = decodePlatformAttestation(attestation);
    switch (content) {
      case AppAttestContent(:final appId, :final attestationObject):
        debugPrint('[attestation_producer_test] App Attest under $appId, '
            '${attestationObject.length} bytes');
        expect(appId, endsWith('.com.example.bitchatTransport'));
        expect(attestationObject, isNotEmpty);
      case AndroidKeyAttestationContent(:final chain):
        debugPrint('[attestation_producer_test] key attestation, '
            '${chain.length} certificates');
        expect(chain.length, greaterThanOrEqualTo(2));
    }

    // The long-lived half is obtained once: asking again is the same bytes.
    expect(await producer.attestationFor(pk), attestation);

    final s1 =
        await producer.signSessionDigest(identityPublicKey: pk, digest: digest1);
    final s2 =
        await producer.signSessionDigest(identityPublicKey: pk, digest: digest2);
    expect(s1, isNotNull);
    expect(s2, isNotNull);
    expect(s1, isNot(s2), reason: 'a signature per session digest');
  });
}
