import 'dart:convert';
import 'dart:io' show InternetAddress;

import 'package:cryptography/cryptography.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';

import 'package:grassroots_networking/src/grassroots_network.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/session/platform_attestation.dart';
import 'package:grassroots_networking_core/src/store/store.dart';

import 'helpers/sodium_test_bootstrap.dart';

/// Hermetic host candidates (see the glare test).
Future<List<InternetAddress>> _loopbackHostAddresses() async =>
    [InternetAddress.loopbackIPv4];

/// A platform that attests, with a verdict the test chooses.
class _StubAttestation implements PlatformAttestation {
  _StubAttestation({this.offers, required this.verdictFor});

  /// What this platform offers when asked to attest. Null is a platform with
  /// none.
  final Uint8List? offers;

  /// The verdict this platform returns for a peer's attestation.
  final AttestationVerdict Function(Uint8List? offered) verdictFor;

  /// The digests this platform was asked to attest over.
  final List<Uint8List> attested = [];

  /// The digests this platform was asked to verify against.
  final List<Uint8List> verified = [];

  @override
  Future<Uint8List?> attest(Uint8List digest) async {
    attested.add(digest);
    return offers;
  }

  @override
  Future<AttestationVerdict> verify(
    Uint8List? attestation,
    Uint8List digest,
  ) async {
    verified.add(digest);
    return verdictFor(attestation);
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

  Future<GrassrootsIdentity> identityFromSeed(int fill, String name) async {
    final seed = Uint8List.fromList(List.filled(32, fill));
    return GrassrootsIdentity.create(
      keyPair: await Ed25519().newKeyPairFromSeed(seed),
      nickname: name,
    );
  }

  Future<GrassrootsNetwork> startNode(
    GrassrootsIdentity id, {
    PlatformAttestation? attestation,
  }) async {
    final store = Store<AppState>(
      appReducer,
      initialState: const AppState(
        settings: SettingsState(bluetoothEnabled: false),
      ),
    );
    final network = GrassrootsNetwork(
      identity: id,
      store: store,
      sodium: sodium,
      localHostAddressReader: _loopbackHostAddresses,
      platformAttestation: attestation,
      config: const GrassrootsNetworkConfig(
        announceInterval: Duration(seconds: 2),
      ),
    );
    expect(await network.initialize(), isTrue);
    return network;
  }

  Future<String> listeningAddress(GrassrootsNetwork network) async {
    final deadline = DateTime.now().add(const Duration(seconds: 12));
    while (network.debugLocalCandidates().isEmpty &&
        DateTime.now().isBefore(deadline)) {
      await Future<void>.delayed(const Duration(milliseconds: 200));
    }
    return network.debugLocalCandidates().first;
  }

  Future<bool> waitFor(bool Function() predicate, {int seconds = 20}) async {
    final deadline = DateTime.now().add(Duration(seconds: seconds));
    while (!predicate() && DateTime.now().isBefore(deadline)) {
      await Future<void>.delayed(const Duration(milliseconds: 100));
    }
    return predicate();
  }

  test('a peer whose platform provides none is reachable and unattested',
      () async {
    final callerId = await identityFromSeed(0x81, 'caller');
    final calleeId = await identityFromSeed(0x82, 'callee');
    final caller = await startNode(callerId);
    final callee = await startNode(calleeId);
    addTearDown(() async {
      await caller.dispose();
      await callee.dispose();
    });

    final connects = <(String, MessageTransport, Uint8List?)>[];
    caller.onPeerConnected = (pk, transport, binaryHash) => connects.add((
          pk.map((b) => b.toRadixString(16).padLeft(2, '0')).join(),
          transport,
          binaryHash,
        ));

    final calleeAddress = await listeningAddress(callee);
    caller.putKnownPeer(calleeId.publicKey);
    callee.putKnownPeer(callerId.publicKey);
    caller.putPeerAddress(calleeId.publicKey, calleeAddress);

    await caller.send(
      calleeId.publicKey,
      Uint8List.fromList(utf8.encode('hello')),
    );

    expect(await waitFor(() => connects.isNotEmpty), isTrue,
        reason: 'the exchange must complete and the peer become reachable');
    expect(connects.single.$2, MessageTransport.udp);
    expect(connects.single.$3, isNull,
        reason: 'a platform with no attestation yields a null binary hash — '
            'unattested, not refused');
    expect(caller.isPeerReachable(calleeId.publicKey), isTrue);
  });

  test('both sides attest over the digest bound to this session', () async {
    final callerAttestation = _StubAttestation(
      offers: Uint8List.fromList(List.filled(48, 0xa1)),
      verdictFor: (offered) =>
          AttestedBinary(Uint8List.fromList(List.filled(32, 0xb2))),
    );
    final calleeAttestation = _StubAttestation(
      offers: Uint8List.fromList(List.filled(48, 0xc3)),
      verdictFor: (offered) =>
          AttestedBinary(Uint8List.fromList(List.filled(32, 0xd4))),
    );

    final callerId = await identityFromSeed(0x83, 'caller');
    final calleeId = await identityFromSeed(0x84, 'callee');
    final caller = await startNode(callerId, attestation: callerAttestation);
    final callee = await startNode(calleeId, attestation: calleeAttestation);
    addTearDown(() async {
      await caller.dispose();
      await callee.dispose();
    });

    Uint8List? attestedHash;
    var connected = false;
    caller.onPeerConnected = (pk, transport, binaryHash) {
      connected = true;
      attestedHash = binaryHash;
    };

    final calleeAddress = await listeningAddress(callee);
    caller.putKnownPeer(calleeId.publicKey);
    callee.putKnownPeer(callerId.publicKey);
    caller.putPeerAddress(calleeId.publicKey, calleeAddress);
    await caller.send(
      calleeId.publicKey,
      Uint8List.fromList(utf8.encode('hello')),
    );

    expect(await waitFor(() => connected), isTrue);
    expect(attestedHash, Uint8List.fromList(List.filled(32, 0xb2)),
        reason: 'onPeerConnected carries the attested binary hash');

    // Each side attested over its OWN key and verified over the PEER's, both
    // against the same handshake hash — so the two digests differ, and the
    // digest one side attested is the digest the other verified.
    expect(callerAttestation.attested, isNotEmpty);
    expect(callerAttestation.verified, isNotEmpty);
    expect(callerAttestation.attested.first,
        isNot(callerAttestation.verified.first));
    expect(callerAttestation.attested.first, calleeAttestation.verified.first,
        reason: 'what the caller attested is what the callee verified');
    expect(calleeAttestation.attested.first, callerAttestation.verified.first);
  });

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

    final callerId = await identityFromSeed(0x85, 'caller');
    final calleeId = await identityFromSeed(0x86, 'callee');
    final caller = await startNode(callerId, attestation: callerAttestation);
    final callee = await startNode(calleeId, attestation: calleeAttestation);
    addTearDown(() async {
      await caller.dispose();
      await callee.dispose();
    });

    var connected = false;
    caller.onPeerConnected = (pk, transport, binaryHash) => connected = true;

    final calleeAddress = await listeningAddress(callee);
    caller.putKnownPeer(calleeId.publicKey);
    callee.putKnownPeer(callerId.publicKey);
    caller.putPeerAddress(calleeId.publicKey, calleeAddress);
    await caller.send(
      calleeId.publicKey,
      Uint8List.fromList(utf8.encode('hello')),
    );

    // Wait long enough that a session that was going to come up would have.
    expect(await waitFor(() => callerAttestation.verified.isNotEmpty), isTrue,
        reason: 'the caller must have been given something to verify');
    await Future<void>.delayed(const Duration(seconds: 3));

    expect(connected, isFalse,
        reason: 'onPeerConnected does not fire on a failed attestation');
    expect(caller.isPeerReachable(calleeId.publicKey), isFalse,
        reason: 'the session was torn down');
  });
}
