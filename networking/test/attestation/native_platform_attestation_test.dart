import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';

import 'package:grassroots_networking/src/attestation/native_platform_attestation.dart';
import 'package:grassroots_networking_core/src/session/attestation_verifier.dart';
import 'package:grassroots_networking_core/src/session/platform_attestation.dart';

/// The binding to the native producers (spec §Session Establishment), over a
/// mocked `grassroots/attestation` channel: what the producer replies is
/// framed into the attestation field the core's verifier reads, the
/// long-lived attestation is requested once, and a platform with no producer
/// is a platform with no attestation.
void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  final messenger =
      TestDefaultBinaryMessengerBinding.instance.defaultBinaryMessenger;
  final pk = Uint8List.fromList(List.generate(32, (i) => i));
  final digest = Uint8List.fromList(List.filled(32, 0x5d));

  late List<MethodCall> calls;

  void answer(Future<Object?>? Function(MethodCall call) handler) {
    messenger.setMockMethodCallHandler(platformAttestationChannel, (call) {
      calls.add(call);
      return handler(call);
    });
  }

  setUp(() => calls = []);
  tearDown(
      () => messenger.setMockMethodCallHandler(platformAttestationChannel, null));

  test('an iOS reply is framed as App Attest, its App ID beside the object',
      () async {
    answer((call) async => {
          'platform': 'ios',
          'appId': 'ABCDE12345.com.example.bitchatTransport',
          'attestationObject': Uint8List.fromList([0xa3, 1, 2]),
        });
    final bytes = await NativePlatformAttestation().attestationFor(pk);
    final content = decodePlatformAttestation(bytes!);
    expect(content, isA<AppAttestContent>());
    content as AppAttestContent;
    expect(content.appId, 'ABCDE12345.com.example.bitchatTransport');
    expect(content.attestationObject, [0xa3, 1, 2]);
    expect(calls.single.method, 'attest');
    expect(calls.single.arguments['identityPublicKey'], pk,
        reason: 'the challenge names the agent\'s identity key');
  });

  test('an Android reply is framed as the chain, leaf first', () async {
    answer((call) async => {
          'platform': 'android',
          'chain': [
            Uint8List.fromList([1]),
            Uint8List.fromList([2, 2]),
          ],
        });
    final bytes = await NativePlatformAttestation().attestationFor(pk);
    final content = decodePlatformAttestation(bytes!);
    expect((content as AndroidKeyAttestationContent).chain, [
      [1],
      [2, 2],
    ]);
  });

  test('a device that provides none replies null, and offers none', () async {
    answer((call) async => null);
    final binding = NativePlatformAttestation();
    expect(await binding.attestationFor(pk), isNull);
  });

  test('an embedding with no producer registered offers none', () async {
    // No handler: the channel throws MissingPluginException, which is a
    // platform with no attestation, not a failure.
    final binding = NativePlatformAttestation();
    expect(await binding.attestationFor(pk), isNull);
    expect(
      await binding.signSessionDigest(identityPublicKey: pk, digest: digest),
      isNull,
    );
  });

  test('the long-lived attestation is requested once, however many sessions '
      'ask', () async {
    answer((call) async => {
          'platform': 'android',
          'chain': [Uint8List.fromList([9])],
        });
    final binding = NativePlatformAttestation();
    final results = await Future.wait([
      binding.attestationFor(pk),
      binding.attestationFor(pk),
      binding.attestationFor(pk),
    ]);
    expect(results.toSet().length, 1);
    expect(calls, hasLength(1));
    await binding.attestationFor(pk);
    expect(calls, hasLength(1));
  });

  test('a request that fails is not kept, so a later session asks again',
      () async {
    var attempt = 0;
    answer((call) async {
      attempt++;
      if (attempt == 1) {
        throw PlatformException(code: 'serverUnavailable');
      }
      return {
        'platform': 'android',
        'chain': [Uint8List.fromList([7])],
      };
    });
    final binding = NativePlatformAttestation();
    await expectLater(
        binding.attestationFor(pk), throwsA(isA<PlatformException>()));
    expect(await binding.attestationFor(pk), isNotNull);
    expect(calls, hasLength(2));
  });

  test('the per-session signature is asked of the key attested for the '
      'identity key, over the digest', () async {
    answer((call) async => Uint8List.fromList([0x30, 0x01]));
    final signature = await NativePlatformAttestation()
        .signSessionDigest(identityPublicKey: pk, digest: digest);
    expect(signature, [0x30, 0x01]);
    expect(calls.single.method, 'sign');
    expect(calls.single.arguments['identityPublicKey'], pk);
    expect(calls.single.arguments['digest'], digest);
  });

  test('a reply of any other shape is refused', () async {
    for (final reply in <Map<Object?, Object?>>[
      {'platform': 'windows'},
      {'platform': 'ios', 'appId': '', 'attestationObject': Uint8List(1)},
      {'platform': 'ios', 'appId': 'A.b'},
      {'platform': 'android', 'chain': <Object?>[]},
      {
        'platform': 'android',
        'chain': ['not bytes'],
      },
    ]) {
      expect(() => platformAttestationFromChannel(reply), throwsFormatException,
          reason: '$reply');
    }
  });

  test('verification is the core verifier\'s', () async {
    final binding = NativePlatformAttestation();
    expect(
      await binding.verify(evidence: null, digest: digest, peerIdentityKey: pk),
      isA<UnattestedPlatform>(),
    );
    expect(
      await binding.verify(
        evidence: AttestationEvidence(
          attestation: Uint8List.fromList([0x01, 0x00, 0x01, 0x41, 0x00]),
          signature: Uint8List.fromList([0x00]),
        ),
        digest: digest,
        peerIdentityKey: pk,
      ),
      isA<InvalidAttestation>(),
    );
  });
}
