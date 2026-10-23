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
  _FixtureProducer(this.ios, {this.version = 42});

  final AppAttestFixture ios;

  /// The version number of the application, which it signs (GNA §2.6).
  final int version;

  /// Where set, every session is signed over this digest instead of its own:
  /// a signature lifted from another session.
  Uint8List? replayedDigest;

  /// Where set, the signature is over the session's digest naming this
  /// version number, while the attestation message carries [version]: what
  /// a peer claiming a version number it did not sign sends.  The producer
  /// learns the session's handshake hash from the attestation of the other
  /// side, which it verifies, since the two sides share it; its own
  /// signature waits for that.
  int? signedVersion;

  final Completer<Uint8List> _sessionHash = Completer<Uint8List>();

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
  Future<int?> applicationVersion() async => version;

  @override
  Future<Uint8List?> signSessionDigest({
    required Uint8List identityPublicKey,
    required Uint8List digest,
  }) async {
    final lying = signedVersion;
    if (lying != null) {
      return ios.assertion(attestationDigest(
        identityPublicKey: identityPublicKey,
        version: lying,
        handshakeHash: await _sessionHash.future,
      ));
    }
    return ios.assertion(replayedDigest ?? digest);
  }

  @override
  Future<AttestationVerdict> verify({
    required AttestationEvidence? evidence,
    required Uint8List handshakeHash,
    required Uint8List peerIdentityKey,
  }) async {
    if (!_sessionHash.isCompleted) _sessionHash.complete(handshakeHash);
    final verdict = verifier.verify(
      evidence: evidence,
      handshakeHash: handshakeHash,
      peerIdentityKey: peerIdentityKey,
    );
    verdicts.add(verdict);
    return verdict;
  }
}

/// The exchange of GNA §2.6 end to end, over the headless profile's real
/// sessions: each side attests, signs this session's digest, which names its
/// application's version number, with the attested key, and verifies the
/// other's; onPeerConnected carries the attested application, its developer,
/// its name and the version number it signed, or null for an unattested
/// identity; and a signature lifted from another session, or over another
/// version number than the one carried, tears the session down.
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

  Future<GrassrootsIdentity> identityFromSeed(int fill) async {
    final seed = Uint8List.fromList(List.filled(32, fill));
    return GrassrootsIdentity.create(
      keyPair: await Ed25519().newKeyPairFromSeed(seed),
    );
  }

  Future<(HeadlessGrassrootsNetwork, HeadlessGrassrootsNetwork)> pair(
    PlatformAttestation serviceAttestation,
    PlatformAttestation callerAttestation,
    int seed,
  ) async {
    final service = HeadlessGrassrootsNetwork(
      outboundQueueStore: MemoryOutboundQueueStore(),
      identity: await identityFromSeed(seed),
      sodium: sodium!,
      attestation: serviceAttestation,
    );
    final caller = HeadlessGrassrootsNetwork(
      outboundQueueStore: MemoryOutboundQueueStore(),
      identity: await identityFromSeed(seed + 1),
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
    caller.wan.putPeerAddress(
      service.identity.publicKey,
      '127.0.0.1:${service.boundPort}',
    );
    await caller.send(
      service.identity.publicKey,
      Uint8List.fromList(utf8.encode('hello')),
    );
  }

  test(
      'each side is reported with the application its platform attested and '
      'the version number it signed', () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    final ios = AppAttestFixture();
    final otherApp = AppAttestFixture(appId: 'ABCDE12345.com.eshapiro.other');
    final (service, caller) = await pair(
      _FixtureProducer(ios, version: 42),
      _FixtureProducer(otherApp, version: 7),
      0x41,
    );

    final serviceSaw = Completer<Application?>();
    final callerSaw = Completer<Application?>();
    service.onPeerConnected = (pk, underlay, application) {
      if (!serviceSaw.isCompleted) serviceSaw.complete(application);
    };
    caller.onPeerConnected = (pk, underlay, application) {
      if (!callerSaw.isCompleted) callerSaw.complete(application);
    };

    await dial(caller, service);

    expect(
      await callerSaw.future.timeout(const Duration(seconds: 15)),
      const Application(
        developer: 'ABCDE12345',
        name: 'com.eshapiro.grassapp',
        version: 42,
      ),
      reason: 'the caller verified the service\'s App Attest offer',
    );
    expect(
      await serviceSaw.future.timeout(const Duration(seconds: 15)),
      const Application(
        developer: 'ABCDE12345',
        name: 'com.eshapiro.other',
        version: 7,
      ),
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
        version: 42,
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

    final serviceSaw = Completer<Application?>();
    final callerSaw = Completer<Application?>();
    service.onPeerConnected = (pk, underlay, application) {
      if (!serviceSaw.isCompleted) serviceSaw.complete(application);
    };
    caller.onPeerConnected = (pk, underlay, application) {
      if (!callerSaw.isCompleted) callerSaw.complete(application);
    };

    await dial(caller, service);

    expect(await callerSaw.future.timeout(const Duration(seconds: 15)), isNull,
        reason: 'a rendezvous-like server is unattested by design');
    expect(await serviceSaw.future.timeout(const Duration(seconds: 15)),
        const Application(
          developer: 'ABCDE12345',
          name: 'com.eshapiro.grassapp',
          version: 42,
        ));
  });

  test(
      'a signature over another version number than the one carried fails, '
      'and tears the session down', () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    final verifyingService = _FixtureProducer(AppAttestFixture());
    // The caller's message carries 42, and its signature names 41.
    final lying = _FixtureProducer(AppAttestFixture(), version: 42)
      ..signedVersion = 41;
    final (service, caller) = await pair(verifyingService, lying, 0x55);

    var serviceConnected = false;
    final callerSaw = Completer<void>();
    service.onPeerConnected = (_, __, ___) => serviceConnected = true;
    caller.onPeerConnected = (_, __, ___) {
      if (!callerSaw.isCompleted) callerSaw.complete();
    };

    await dial(caller, service);

    await callerSaw.future.timeout(const Duration(seconds: 15));
    await Future<void>.delayed(const Duration(seconds: 2));
    expect(verifyingService.verdicts, isNotEmpty,
        reason: 'the service must have been given the offer to verify');
    expect(verifyingService.verdicts.first, isA<InvalidAttestation>());
    expect(
      (verifyingService.verdicts.first as InvalidAttestation).reason,
      contains('this session'),
      reason: 'refused for the signature, whose digest names 41 where the '
          'message carries 42',
    );
    expect(serviceConnected, isFalse,
        reason: 'onPeerConnected does not fire on a failed attestation');
    expect(service.isPeerReachable(caller.identity.publicKey), isFalse,
        reason: 'the session is torn down');
  });
}
