import 'dart:async';
import 'dart:convert';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:sodium/sodium_sumo.dart';
import 'package:test/test.dart';

import 'package:grassroots_networking_core/grassroots_networking_core.dart';

import 'helpers/attestation_fixtures.dart';

/// A producer built on the test fixtures: what a phone's native producer
/// supplies, with keys the test holds.
class _FixtureProducer implements PlatformAttestation {
  _FixtureProducer(this.ios);

  final AppAttestFixture ios;

  /// Where set, every session is signed over this digest instead of its own:
  /// a signature lifted from another session.
  Uint8List? replayedDigest;

  /// The verdicts this side reached on what its peers offered.
  final List<AttestationVerdict> verdicts = [];

  final verifier = AttestationVerifier(
    clock: () => fixtureTime,
    appAttestRoots: [AppAttestFixture().root],
  );

  @override
  Future<Uint8List?> attestationFor(Uint8List identityPublicKey) async =>
      encodePlatformAttestation(AppAttestContent(
        appId: ios.appId,
        attestationObject: ios.attestationObject(identityPublicKey),
      ));

  @override
  Future<Uint8List?> signSessionDigest({
    required Uint8List identityPublicKey,
    required Uint8List digest,
  }) async {
    return ios.assertion(replayedDigest ?? digest);
  }

  @override
  Future<AttestationVerdict> verify({
    required AttestationEvidence? evidence,
    required Uint8List digest,
    required Uint8List peerIdentityKey,
  }) async {
    final verdict = verifier.verify(
      evidence: evidence,
      digest: digest,
      peerIdentityKey: peerIdentityKey,
    );
    verdicts.add(verdict);
    return verdict;
  }
}

/// The exchange of spec §Session Establishment end to end, over the headless
/// profile's real sessions: each side attests, signs this session's digest
/// with the attested key, and verifies the other's; onPeerConnected carries
/// the attested application identity, and a signature lifted from another
/// session tears the session down.
///
/// Needs a native libsodium, as the headless e2e test does; self-skips when
/// absent.
void main() {
  SodiumSumo? sodium;
  String? sodiumUnavailable;

  setUpAll(() async {
    try {
      sodium = await initHeadlessSodium();
    } on Object catch (e) {
      sodiumUnavailable = 'libsodium unavailable, skipping: $e';
    }
  });

  Future<GrassrootsIdentity> identityFromSeed(int fill, String name) async {
    final seed = Uint8List.fromList(List.filled(32, fill));
    return GrassrootsIdentity.create(
      keyPair: await Ed25519().newKeyPairFromSeed(seed),
      nickname: name,
    );
  }

  Future<(HeadlessGrassrootsNetwork, HeadlessGrassrootsNetwork)> pair(
    PlatformAttestation serviceAttestation,
    PlatformAttestation callerAttestation,
    int seed,
  ) async {
    final service = HeadlessGrassrootsNetwork(
      identity: await identityFromSeed(seed, 'service'),
      sodium: sodium!,
      attestation: serviceAttestation,
    );
    final caller = HeadlessGrassrootsNetwork(
      identity: await identityFromSeed(seed + 1, 'caller'),
      sodium: sodium!,
      attestation: callerAttestation,
    );
    addTearDown(() async {
      await caller.dispose();
      await service.dispose();
    });
    service.putKnownPeer(caller.identity.publicKey);
    expect(await service.start(), isTrue);
    expect(await caller.start(), isTrue);
    return (service, caller);
  }

  Future<void> dial(
    HeadlessGrassrootsNetwork caller,
    HeadlessGrassrootsNetwork service,
  ) async {
    caller.putPeerAddress(
      service.identity.publicKey,
      '127.0.0.1:${service.boundPort}',
    );
    await caller.send(
      service.identity.publicKey,
      Uint8List.fromList(utf8.encode('hello')),
    );
  }

  test('each side is reported with the identity its platform attested',
      () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    final ios = AppAttestFixture();
    final otherApp = AppAttestFixture(appId: 'ABCDE12345.com.eshapiro.other');
    final (service, caller) = await pair(
      _FixtureProducer(ios),
      _FixtureProducer(otherApp),
      0x41,
    );

    final serviceSaw = Completer<ApplicationIdentity?>();
    final callerSaw = Completer<ApplicationIdentity?>();
    service.onPeerConnected = (pk, transport, identity) {
      if (!serviceSaw.isCompleted) serviceSaw.complete(identity);
    };
    caller.onPeerConnected = (pk, transport, identity) {
      if (!callerSaw.isCompleted) callerSaw.complete(identity);
    };

    await dial(caller, service);

    expect(
      await callerSaw.future.timeout(const Duration(seconds: 15)),
      IosApplicationIdentity(ios.appId),
      reason: 'the caller verified the service\'s App Attest offer',
    );
    expect(
      await serviceSaw.future.timeout(const Duration(seconds: 15)),
      IosApplicationIdentity(otherApp.appId),
      reason: 'the service verified the caller\'s App Attest offer',
    );
  });

  test('a signature lifted from another session tears the session down',
      () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    final verifyingService = _FixtureProducer(AppAttestFixture());
    final replaying = _FixtureProducer(AppAttestFixture())
      ..replayedDigest = attestationDigest(
        identityPublicKey: Uint8List(32),
        handshakeHash: Uint8List.fromList(List.filled(32, 0x99)),
      );
    final (service, caller) = await pair(verifyingService, replaying, 0x51);

    var serviceConnected = false;
    final callerSaw = Completer<void>();
    service.onPeerConnected = (_, __, ___) => serviceConnected = true;
    caller.onPeerConnected = (_, __, ___) {
      if (!callerSaw.isCompleted) callerSaw.complete();
    };

    await dial(caller, service);

    // The caller verifies the honest service and finds it reachable; the
    // service verifies the replayed signature and tears the session down.
    await callerSaw.future.timeout(const Duration(seconds: 15));
    await Future<void>.delayed(const Duration(seconds: 2));
    expect(verifyingService.verdicts, isNotEmpty,
        reason: 'the service must have been given the offer to verify');
    expect(verifyingService.verdicts.first, isA<InvalidAttestation>());
    expect(
      (verifyingService.verdicts.first as InvalidAttestation).reason,
      contains('this session'),
      reason: 'refused for the signature, not for anything else',
    );
    expect(serviceConnected, isFalse,
        reason: 'onPeerConnected does not fire on a failed attestation');
    expect(service.isPeerReachable(caller.identity.publicKey), isFalse);
  });

  test('a profile with nothing to offer is unattested at its peer, and still '
      'verifies what it is offered', () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    final ios = AppAttestFixture();
    final (service, caller) = await pair(
      NoPlatformAttestation(
        verifier: AttestationVerifier(
          clock: () => fixtureTime,
          appAttestRoots: [ios.root],
        ),
      ),
      _FixtureProducer(ios),
      0x61,
    );

    final serviceSaw = Completer<ApplicationIdentity?>();
    final callerSaw = Completer<ApplicationIdentity?>();
    service.onPeerConnected = (pk, transport, identity) {
      if (!serviceSaw.isCompleted) serviceSaw.complete(identity);
    };
    caller.onPeerConnected = (pk, transport, identity) {
      if (!callerSaw.isCompleted) callerSaw.complete(identity);
    };

    await dial(caller, service);

    expect(await callerSaw.future.timeout(const Duration(seconds: 15)), isNull,
        reason: 'a rendezvous-like server is unattested by design');
    expect(await serviceSaw.future.timeout(const Duration(seconds: 15)),
        IosApplicationIdentity(ios.appId));
  });
}
