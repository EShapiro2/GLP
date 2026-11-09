import 'dart:async';
import 'dart:convert';

import 'package:cryptography/cryptography.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';

import 'package:grassroots_networking/src/grassroots_network.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/models/underlay.dart';
import 'package:grassroots_networking_core/src/session/application.dart';
import 'package:grassroots_networking_core/src/session/platform_attestation.dart';
import 'package:grassroots_networking_core/src/store/store.dart';

import 'helpers/event_waits.dart';
import 'helpers/sodium_test_bootstrap.dart';

/// A platform that attests, with a verdict the test chooses.
///
/// The two-step of GNA §2.6: a long-lived attestation over a challenge naming
/// the identity key, and a per-session signature over the digest, which
/// names the application's version number. [offers] null is a platform with
/// neither.
class _StubAttestation implements PlatformAttestation {
  _StubAttestation({this.offers, required this.verdictFor, this.version = 5});

  /// What this platform offers as its long-lived attestation. Null is a
  /// platform with none.
  final Uint8List? offers;

  /// The version number of its application.
  final int version;

  /// The verdict this platform returns for a peer's evidence.
  final AttestationVerdict Function(AttestationEvidence? offered) verdictFor;

  /// The identity keys this platform was asked to attest.
  final List<Uint8List> attestedKeys = [];

  /// The digests this platform was asked to sign — one per session.
  final List<Uint8List> signed = [];

  /// The digests this platform verified the peer's signature against: the
  /// peer's key, the version number its message carried, and the session's
  /// handshake hash.
  final List<Uint8List> verified = [];

  /// Completes when this platform first verifies a peer's evidence.
  final Completer<void> firstVerified = Completer<void>();

  @override
  Future<Uint8List?> attestationFor(Uint8List identityPublicKey) async {
    attestedKeys.add(identityPublicKey);
    return offers;
  }

  @override
  Future<int?> applicationVersion() async => offers == null ? null : version;

  @override
  Future<Uint8List?> signSessionDigest({
    required Uint8List identityPublicKey,
    required Uint8List digest,
  }) async {
    if (offers == null) return null;
    signed.add(digest);
    // A stub signature that is a function of the digest, so a test can tell
    // one session's signature from another's.
    return Uint8List.fromList([0x51, ...digest]);
  }

  @override
  Future<AttestationVerdict> verify({
    required AttestationEvidence? evidence,
    required Uint8List handshakeHash,
    required Uint8List peerIdentityKey,
  }) async {
    if (evidence != null) {
      verified.add(attestationDigest(
        identityPublicKey: peerIdentityKey,
        version: evidence.version,
        handshakeHash: handshakeHash,
      ));
      if (!firstVerified.isCompleted) firstVerified.complete();
    }
    return verdictFor(evidence);
  }
}

/// The attestation exchange of §Session Establishment.
///
/// "The handshake is followed by a mutual attestation exchange, and neither
/// peer is reported reachable until it succeeds. ... An attestation that fails
/// verification tears the session down, and onPeerConnected does not fire."
/// Two full [GrassrootsNetwork] instances talk over loopback UDP.
void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  late SodiumSumo sodium;
  setUpAll(() async {
    sodium = await initTestSodium();
  });

  setUp(() {
    // The layer keeps its fair-delivery queue in the application's
    // SharedPreferences (GNA §2.10); a fresh store for every test.
    SharedPreferences.setMockInitialValues({});
    final messenger =
        TestDefaultBinaryMessengerBinding.instance.defaultBinaryMessenger;
    messenger.setMockMethodCallHandler(
      const MethodChannel('dev.fluttercommunity.plus/connectivity'),
      (call) async => ['wifi'],
    );
    messenger.setMockStreamHandler(
      const EventChannel('dev.fluttercommunity.plus/connectivity_status'),
      MockStreamHandler.inline(onListen: (arguments, events) {}),
    );
  });

  Future<GrassrootsIdentity> identityFromSeed(int fill) async {
    final seed = Uint8List.fromList(List.filled(32, fill));
    return GrassrootsIdentity.create(
      keyPair: await Ed25519().newKeyPairFromSeed(seed),
    );
  }

  /// Each node's host-address reader (hermetic: see the glare test), which
  /// tells when its candidates are read.
  final hostReaders = Expando<LoopbackHostReader>();

  Future<GrassrootsNetwork> startNode(
    GrassrootsIdentity id, {
    PlatformAttestation? attestation,
  }) async {
    final store = Store<AppState>(
      appReducer,
      initialState: const AppState(),
    );
    final hosts = LoopbackHostReader();
    final network = GrassrootsNetwork(
      identity: id,
      underlays: const {Underlay.lan, Underlay.wan},
      store: store,
      sodium: sodium,
      localHostAddressReader: hosts.call,
      platformAttestation: attestation,
      config: const GrassrootsNetworkConfig(
        announceInterval: Duration(seconds: 2),
      ),
    );
    hostReaders[network] = hosts;
    expect(await network.initialize(), isTrue);
    return network;
  }

  Future<String> listeningAddress(GrassrootsNetwork network) async {
    await hostReaders[network]!.read;
    return network.debugLocalCandidates().first;
  }

  test('a peer whose platform provides none is reachable and unattested',
      () async {
    final callerId = await identityFromSeed(0x81);
    final calleeId = await identityFromSeed(0x82);
    final caller = await startNode(callerId);
    final callee = await startNode(calleeId);
    addTearDown(() async {
      await caller.dispose();
      await callee.dispose();
    });

    final connects = <(String, Underlay, Application?)>[];
    final firstConnect = Completer<void>();
    caller.onPeerConnected = (pk, underlay, application) {
      connects.add((
        pk.map((b) => b.toRadixString(16).padLeft(2, '0')).join(),
        underlay,
        application,
      ));
      if (!firstConnect.isCompleted) firstConnect.complete();
    };

    final calleeAddress = await listeningAddress(callee);
    caller.putKnownPeer(calleeId.publicKey);
    callee.putKnownPeer(callerId.publicKey);
    caller.wan!.putPeerAddress(calleeId.publicKey, calleeAddress);

    await caller.send(
      calleeId.publicKey,
      Uint8List.fromList(utf8.encode('hello')),
    );

    await firstConnect.future;
    expect(connects, isNotEmpty,
        reason: 'the exchange must complete and the peer become reachable');
    // Loopback is among the agent's own local prefixes, so the UDP session
    // between two agents on one machine is LAN (GNA 582ef60: "two agents on
    // one machine are LAN contact to each other"; WAN until then).
    expect(connects.single.$2, Underlay.lan);
    expect(connects.single.$3, isNull,
        reason: 'a platform with no attestation yields null for the '
            'application: the identity is unattested, not refused');
    expect(caller.isPeerReachable(calleeId.publicKey), isTrue);
  }, timeout: Timeout.none);

  test('the attestation names the identity key and the signature the session',
      () async {
    final callerAttestation = _StubAttestation(
      offers: Uint8List.fromList(List.filled(48, 0xa1)),
      verdictFor: (offered) => const AttestedApplication(Application(
        developer: 'ABCDE12345',
        name: 'com.eshapiro.callee',
        version: 5,
      )),
    );
    final calleeAttestation = _StubAttestation(
      offers: Uint8List.fromList(List.filled(48, 0xc3)),
      version: 3,
      verdictFor: (offered) => AttestedApplication(Application(
        developer: 'd4' * 32,
        name: 'com.eshapiro.caller',
        version: offered!.version,
      )),
    );

    final callerId = await identityFromSeed(0x83);
    final calleeId = await identityFromSeed(0x84);
    final caller = await startNode(callerId, attestation: callerAttestation);
    final callee = await startNode(calleeId, attestation: calleeAttestation);
    addTearDown(() async {
      await caller.dispose();
      await callee.dispose();
    });

    Application? attested;
    var connected = false;
    final callerConnects = Completer<void>();
    caller.onPeerConnected = (pk, transport, application) {
      connected = true;
      attested = application;
      if (!callerConnects.isCompleted) callerConnects.complete();
    };
    // Each side judges the other's half on its own schedule: what the callee
    // verified is asserted below, so wait for the callee as well.
    var calleeConnected = false;
    final calleeConnects = Completer<void>();
    callee.onPeerConnected = (_, __, ___) {
      calleeConnected = true;
      if (!calleeConnects.isCompleted) calleeConnects.complete();
    };

    final calleeAddress = await listeningAddress(callee);
    caller.putKnownPeer(calleeId.publicKey);
    callee.putKnownPeer(callerId.publicKey);
    caller.wan!.putPeerAddress(calleeId.publicKey, calleeAddress);
    await caller.send(
      calleeId.publicKey,
      Uint8List.fromList(utf8.encode('hello')),
    );

    await callerConnects.future;
    await calleeConnects.future;
    expect(connected && calleeConnected, isTrue);
    expect(
        attested,
        const Application(
          developer: 'ABCDE12345',
          name: 'com.eshapiro.callee',
          version: 5,
        ),
        reason: 'onPeerConnected carries the attested application');

    // The long-lived half names the agent's OWN identity key — not the peer's
    // and not the session. This is what makes it long-lived: it is the same
    // attestation on every session the agent opens.
    expect(callerAttestation.attestedKeys, isNotEmpty);
    expect(callerAttestation.attestedKeys.first, callerId.publicKey);
    expect(calleeAttestation.attestedKeys.first, calleeId.publicKey);

    // The per-session half is the signature. Each side signed over its OWN
    // key's digest, naming its OWN version number, and verified over the
    // PEER's, both against the same handshake hash — so the two digests
    // differ, and the digest one side signed is the digest the other
    // verified, the version number the message carried included.
    expect(callerAttestation.signed, isNotEmpty);
    expect(callerAttestation.verified, isNotEmpty);
    expect(callerAttestation.signed.first,
        isNot(callerAttestation.verified.first));
    expect(callerAttestation.signed.first, calleeAttestation.verified.first,
        reason: 'what the caller signed is what the callee verified');
    expect(calleeAttestation.signed.first, callerAttestation.verified.first);
  }, timeout: Timeout.none);

  test('a failed attestation tears the session down and onPeerConnected does '
      'not fire', () async {
    // The callee offers an attestation the caller rejects.
    final callerAttestation = _StubAttestation(
      offers: null,
      verdictFor: (offered) => const InvalidAttestation('forged'),
    );
    final calleeAttestation = _StubAttestation(
      offers: Uint8List.fromList(List.filled(48, 0xee)),
      verdictFor: (offered) => const UnattestedPlatform('none offered'),
    );

    final callerId = await identityFromSeed(0x85);
    final calleeId = await identityFromSeed(0x86);
    final caller = await startNode(callerId, attestation: callerAttestation);
    final callee = await startNode(calleeId, attestation: calleeAttestation);
    addTearDown(() async {
      await caller.dispose();
      await callee.dispose();
    });

    var connected = false;
    caller.onPeerConnected = (pk, transport, application) => connected = true;

    final calleeAddress = await listeningAddress(callee);
    caller.putKnownPeer(calleeId.publicKey);
    callee.putKnownPeer(callerId.publicKey);
    caller.wan!.putPeerAddress(calleeId.publicKey, calleeAddress);
    await caller.send(
      calleeId.publicKey,
      Uint8List.fromList(utf8.encode('hello')),
    );

    // Wait long enough that a session that was going to come up would have.
    await callerAttestation.firstVerified.future;
    expect(callerAttestation.verified, isNotEmpty,
        reason: 'the caller must have been given something to verify');
    await Future<void>.delayed(const Duration(seconds: 3));

    expect(connected, isFalse,
        reason: 'onPeerConnected does not fire on a failed attestation');
    expect(caller.isPeerReachable(calleeId.publicKey), isFalse,
        reason: 'the session was torn down');
  });
}
