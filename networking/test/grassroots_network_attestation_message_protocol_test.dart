import 'dart:convert';
import 'dart:io';

import 'package:cryptography/cryptography.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';

import 'package:grassroots_networking/src/grassroots_network.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/models/packet.dart';
import 'package:grassroots_networking_core/src/protocol/fragment_handler.dart';
import 'package:grassroots_networking_core/src/session/application_identity.dart';
import 'package:grassroots_networking_core/src/session/platform_attestation.dart';
import 'package:grassroots_networking_core/src/store/store.dart';

import 'helpers/sodium_test_bootstrap.dart';
import 'package:grassroots_networking_core/src/models/underlay.dart';

/// Hermetic host candidates (see the glare test).
Future<List<InternetAddress>> _loopbackHostAddresses() async =>
    [InternetAddress.loopbackIPv4];

/// A platform that offers [offers] as its long-lived attestation and accepts
/// what it is offered as [identity], recording each offer it was given.
class _LargeAttestation implements PlatformAttestation {
  _LargeAttestation({required this.offers, required this.identity});

  final Uint8List offers;
  final ApplicationIdentity identity;
  final List<AttestationEvidence?> offered = [];

  @override
  Future<Uint8List?> attestationFor(Uint8List identityPublicKey) async =>
      offers;

  @override
  Future<Uint8List?> signSessionDigest({
    required Uint8List identityPublicKey,
    required Uint8List digest,
  }) async =>
      Uint8List.fromList([0x51, ...digest]);

  @override
  Future<AttestationVerdict> verify({
    required AttestationEvidence? evidence,
    required Uint8List digest,
    required Uint8List peerIdentityKey,
  }) async {
    offered.add(evidence);
    return AttestedApplication(identity);
  }
}

/// A loopback UDP relay in front of the callee that loses the caller's
/// datagrams [dropTowardCallee] picks. The frame's type byte is in the clear,
/// so a session-encrypted attestation fragment is told by its first byte.
class _LossyRelay {
  _LossyRelay._(this._socket, this._calleePort) {
    _socket.listen((event) {
      if (event != RawSocketEvent.read) return;
      for (var d = _socket.receive(); d != null; d = _socket.receive()) {
        _relay(d);
      }
    });
  }

  static Future<_LossyRelay> start(int calleePort) async => _LossyRelay._(
        await RawDatagramSocket.bind(InternetAddress.loopbackIPv4, 0),
        calleePort,
      );

  final RawDatagramSocket _socket;
  final int _calleePort;
  InternetAddress? _callerAddress;
  int? _callerPort;
  bool Function(Uint8List datagram) dropTowardCallee = (_) => false;
  int attestationFragmentsSeen = 0;
  int dropped = 0;

  int get port => _socket.port;

  void _relay(Datagram d) {
    if (d.port == _calleePort &&
        d.address.address == InternetAddress.loopbackIPv4.address) {
      final address = _callerAddress;
      final port = _callerPort;
      if (address != null && port != null) _socket.send(d.data, address, port);
      return;
    }
    _callerAddress = d.address;
    _callerPort = d.port;
    if (d.data.isNotEmpty &&
        d.data.first == PacketType.secureAttestationFragment.value) {
      attestationFragmentsSeen++;
    }
    if (dropTowardCallee(d.data)) {
      dropped++;
      return;
    }
    _socket.send(d.data, InternetAddress.loopbackIPv4, _calleePort);
  }

  void close() => _socket.close();
}

/// The attestation exchange over the Flutter coordinator's UDP carrier rides
/// the Message Protocol (spec §Session Establishment, §Message Protocol): an
/// attestation larger than a datagram is fragmented and reassembled, and a
/// lost fragment is retransmitted. Two full [GrassrootsNetwork] instances
/// over loopback UDP.
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

  Future<GrassrootsIdentity> identityFromSeed(int fill) async =>
      GrassrootsIdentity.create(
        keyPair: await Ed25519()
            .newKeyPairFromSeed(Uint8List.fromList(List.filled(32, fill))),
      );

  Future<GrassrootsNetwork> startNode(
    GrassrootsIdentity id,
    PlatformAttestation attestation,
  ) async {
    final network = GrassrootsNetwork(
      identity: id,
      underlays: const {Underlay.lan, Underlay.wan},
      rendezvousServers: const [],
      store: Store<AppState>(
        appReducer,
        initialState: const AppState(),
      ),
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

  const callerIdentity =
      IosApplicationIdentity('ABCDE12345.com.eshapiro.caller');
  const calleeIdentity =
      IosApplicationIdentity('ABCDE12345.com.eshapiro.callee');

  test('a multi-kilobyte attestation over UDP reaches the peer whole',
      () async {
    final calleeOffers =
        Uint8List.fromList(List.generate(6000, (i) => (i * 7) % 256));
    final callerAttestation = _LargeAttestation(
      offers: Uint8List.fromList(List.generate(5000, (i) => (i * 3) % 256)),
      identity: calleeIdentity,
    );
    final calleeAttestation =
        _LargeAttestation(offers: calleeOffers, identity: callerIdentity);

    final callerId = await identityFromSeed(0x91);
    final calleeId = await identityFromSeed(0x92);
    final caller = await startNode(callerId, callerAttestation);
    final callee = await startNode(calleeId, calleeAttestation);
    addTearDown(() async {
      await caller.dispose();
      await callee.dispose();
    });

    ApplicationIdentity? callerSaw;
    ApplicationIdentity? calleeSaw;
    caller.onPeerConnected = (_, __, identity) => callerSaw = identity;
    callee.onPeerConnected = (_, __, identity) => calleeSaw = identity;

    final calleeAddress = await listeningAddress(callee);
    caller.putKnownPeer(calleeId.publicKey);
    callee.putKnownPeer(callerId.publicKey);
    caller.wan!.putPeerAddress(calleeId.publicKey, calleeAddress);
    await caller.send(
      calleeId.publicKey,
      Uint8List.fromList(utf8.encode('hello')),
    );

    expect(await waitFor(() => callerSaw != null && calleeSaw != null), isTrue,
        reason: 'both sides verified the other\'s multi-kilobyte offer');
    expect(callerSaw, calleeIdentity);
    expect(calleeSaw, callerIdentity);
    // What the callee offered is what the caller was given, byte for byte:
    // fragmented by the carrier and reassembled whole.
    expect(calleeOffers.length, greaterThan(5 * FragmentHandler.udpMaxChunk));
    expect(callerAttestation.offered.first!.attestation, calleeOffers);
  });

  test('a lost attestation fragment over UDP is retransmitted', () async {
    final callerAttestation = _LargeAttestation(
      offers: Uint8List.fromList(List.generate(3000, (i) => (i * 5) % 256)),
      identity: calleeIdentity,
    );
    final calleeAttestation = _LargeAttestation(
      offers: Uint8List.fromList(List.generate(3000, (i) => (i * 11) % 256)),
      identity: callerIdentity,
    );

    final callerId = await identityFromSeed(0x93);
    final calleeId = await identityFromSeed(0x94);
    final caller = await startNode(callerId, callerAttestation);
    final callee = await startNode(calleeId, calleeAttestation);
    final calleeAddress = await listeningAddress(callee);
    final relay =
        await _LossyRelay.start(int.parse(calleeAddress.split(':').last));
    addTearDown(() async {
      relay.close();
      await caller.dispose();
      await callee.dispose();
    });

    // Lose the first attestation fragment the caller sends, once.
    relay.dropTowardCallee = (datagram) =>
        datagram.isNotEmpty &&
        datagram.first == PacketType.secureAttestationFragment.value &&
        relay.dropped == 0;

    ApplicationIdentity? calleeSaw;
    callee.onPeerConnected = (_, __, identity) => calleeSaw = identity;

    caller.putKnownPeer(calleeId.publicKey);
    callee.putKnownPeer(callerId.publicKey);
    caller.wan!.putPeerAddress(calleeId.publicKey, '127.0.0.1:${relay.port}');
    await caller.send(
      calleeId.publicKey,
      Uint8List.fromList(utf8.encode('hello')),
    );

    expect(await waitFor(() => calleeSaw != null), isTrue,
        reason: 'the exchange completed although a fragment was lost');
    expect(calleeSaw, callerIdentity);
    expect(relay.dropped, 1);
    // Three fragments at the IP carrier's chunk; the callee had them all, so
    // the lost one came again.
    expect(relay.attestationFragmentsSeen, greaterThanOrEqualTo(4));
    expect(calleeAttestation.offered.first!.attestation,
        callerAttestation.offers);
  });

  test(
      'send is held until the peer is reachable, and the receiver delivers a '
      'payload only once it has the sender\'s attestation (GNA, 2026-10-11, '
      'Task A)', () async {
    final callerAttestation = _LargeAttestation(
      offers: Uint8List.fromList(List.generate(3000, (i) => (i * 13) % 256)),
      identity: calleeIdentity,
    );
    final calleeAttestation = _LargeAttestation(
      offers: Uint8List.fromList(List.generate(3000, (i) => (i * 17) % 256)),
      identity: callerIdentity,
    );

    final callerId = await identityFromSeed(0x95);
    final calleeId = await identityFromSeed(0x96);
    final caller = await startNode(callerId, callerAttestation);
    final callee = await startNode(calleeId, calleeAttestation);
    final calleeAddress = await listeningAddress(callee);
    final relay =
        await _LossyRelay.start(int.parse(calleeAddress.split(':').last));
    addTearDown(() async {
      relay.close();
      await caller.dispose();
      await callee.dispose();
    });

    // The caller's attestation is lost once on its way, so the caller can be
    // ready to send while the callee still waits for the caller's half.
    relay.dropTowardCallee = (datagram) =>
        datagram.isNotEmpty &&
        datagram.first == PacketType.secureAttestationFragment.value &&
        relay.dropped == 0;

    final calleeEvents = <String>[];
    callee.onPeerConnected = (_, __, ___) => calleeEvents.add('connected');
    callee.onMessageReceived = (_, payload, __, ___) =>
        calleeEvents.add('received ${utf8.decode(payload)}');

    caller.putKnownPeer(calleeId.publicKey);
    callee.putKnownPeer(callerId.publicKey);
    caller.wan!.putPeerAddress(calleeId.publicKey, '127.0.0.1:${relay.port}');
    await caller.send(
      calleeId.publicKey,
      Uint8List.fromList(utf8.encode('hello')),
    );
    expect(caller.queuedMessageCountForPeer(calleeId.publicKey), 1,
        reason: 'held: the callee is not reachable yet');

    expect(
        await waitFor(() => calleeEvents.contains('received hello')), isTrue,
        reason: 'delivered once the exchange completed');
    expect(calleeEvents.first, 'connected',
        reason: 'nothing is delivered before the callee has the caller\'s '
            'attestation');
    // The callee delivers the message once it has all its fragments, before
    // their acknowledgments reach the caller, whose queue gives it up only
    // then (spec §Message Protocol).
    expect(
        await waitFor(
            () => caller.queuedMessageCountForPeer(calleeId.publicKey) == 0),
        isTrue,
        reason: 'the queue gives the message up once it is acknowledged');
  });
}
