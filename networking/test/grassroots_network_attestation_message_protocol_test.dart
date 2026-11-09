import 'dart:async';
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
import 'package:grassroots_networking_core/src/session/application.dart';
import 'package:grassroots_networking_core/src/session/platform_attestation.dart';
import 'package:grassroots_networking_core/src/store/store.dart';

import 'helpers/event_waits.dart';
import 'helpers/sodium_test_bootstrap.dart';
import 'package:grassroots_networking_core/src/models/underlay.dart';

/// A platform that offers [offers] as its long-lived attestation and accepts
/// what it is offered as [application], recording each offer it was given.
class _LargeAttestation implements PlatformAttestation {
  _LargeAttestation({required this.offers, required this.application});

  final Uint8List offers;
  final Application application;
  final List<AttestationEvidence?> offered = [];

  @override
  Future<Uint8List?> attestationFor(Uint8List identityPublicKey) async =>
      offers;

  @override
  Future<int?> applicationVersion() async => 5;

  @override
  Future<Uint8List?> signSessionDigest({
    required Uint8List identityPublicKey,
    required Uint8List digest,
  }) async =>
      Uint8List.fromList([0x51, ...digest]);

  @override
  Future<AttestationVerdict> verify({
    required AttestationEvidence? evidence,
    required Uint8List handshakeHash,
    required Uint8List peerIdentityKey,
  }) async {
    offered.add(evidence);
    return AttestedApplication(application);
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

  /// Each node's host-address reader (hermetic: see the glare test), which
  /// tells when its candidates are read.
  final hostReaders = Expando<LoopbackHostReader>();

  Future<GrassrootsNetwork> startNode(
    GrassrootsIdentity id,
    PlatformAttestation attestation, {
    void Function(dynamic action)? onAction,
  }) async {
    final hosts = LoopbackHostReader();
    final network = GrassrootsNetwork(
      identity: id,
      underlays: const {Underlay.lan, Underlay.wan},
      store: Store<AppState>(
        appReducer,
        initialState: const AppState(),
        middleware: [
          (store, action, next) {
            onAction?.call(action);
            next(action);
          },
        ],
      ),
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

  const callerApplication =
      Application(
          developer: 'ABCDE12345', name: 'com.eshapiro.caller', version: 5);
  const calleeApplication =
      Application(
          developer: 'ABCDE12345', name: 'com.eshapiro.callee', version: 5);

  test('a multi-kilobyte attestation over UDP reaches the peer whole',
      () async {
    final calleeOffers =
        Uint8List.fromList(List.generate(6000, (i) => (i * 7) % 256));
    final callerAttestation = _LargeAttestation(
      offers: Uint8List.fromList(List.generate(5000, (i) => (i * 3) % 256)),
      application: calleeApplication,
    );
    final calleeAttestation =
        _LargeAttestation(offers: calleeOffers, application: callerApplication);

    final callerId = await identityFromSeed(0x91);
    final calleeId = await identityFromSeed(0x92);
    final caller = await startNode(callerId, callerAttestation);
    final callee = await startNode(calleeId, calleeAttestation);
    addTearDown(() async {
      await caller.dispose();
      await callee.dispose();
    });

    Application? callerSaw;
    Application? calleeSaw;
    final callerConnects = Completer<void>();
    final calleeConnects = Completer<void>();
    caller.onPeerConnected = (_, __, application) {
      callerSaw = application;
      if (!callerConnects.isCompleted) callerConnects.complete();
    };
    callee.onPeerConnected = (_, __, application) {
      calleeSaw = application;
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
    expect(callerSaw != null && calleeSaw != null, isTrue,
        reason: 'both sides verified the other\'s multi-kilobyte offer');
    expect(callerSaw, calleeApplication);
    expect(calleeSaw, callerApplication);
    // What the callee offered is what the caller was given, byte for byte:
    // fragmented by the carrier and reassembled whole.
    expect(calleeOffers.length, greaterThan(5 * FragmentHandler.udpMaxChunk));
    expect(callerAttestation.offered.first!.attestation, calleeOffers);
  }, timeout: Timeout.none);

  test('a lost attestation fragment over UDP is retransmitted', () async {
    final callerAttestation = _LargeAttestation(
      offers: Uint8List.fromList(List.generate(3000, (i) => (i * 5) % 256)),
      application: calleeApplication,
    );
    final calleeAttestation = _LargeAttestation(
      offers: Uint8List.fromList(List.generate(3000, (i) => (i * 11) % 256)),
      application: callerApplication,
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

    Application? calleeSaw;
    final calleeConnects = Completer<void>();
    callee.onPeerConnected = (_, __, application) {
      calleeSaw = application;
      if (!calleeConnects.isCompleted) calleeConnects.complete();
    };

    caller.putKnownPeer(calleeId.publicKey);
    callee.putKnownPeer(callerId.publicKey);
    caller.wan!.putPeerAddress(calleeId.publicKey, '127.0.0.1:${relay.port}');
    await caller.send(
      calleeId.publicKey,
      Uint8List.fromList(utf8.encode('hello')),
    );

    await calleeConnects.future;
    expect(calleeSaw, isNotNull,
        reason: 'the exchange completed although a fragment was lost');
    expect(calleeSaw, callerApplication);
    expect(relay.dropped, 1);
    // Three fragments at the IP carrier's chunk; the callee had them all, so
    // the lost one came again.
    expect(relay.attestationFragmentsSeen, greaterThanOrEqualTo(4));
    expect(calleeAttestation.offered.first!.attestation,
        callerAttestation.offers);
  }, timeout: Timeout.none);

  test(
      'send is held until the peer is reachable, and the receiver delivers a '
      'payload only once it has the sender\'s attestation (GNA, 2026-10-11, '
      'Task A)', () async {
    final callerAttestation = _LargeAttestation(
      offers: Uint8List.fromList(List.generate(3000, (i) => (i * 13) % 256)),
      application: calleeApplication,
    );
    final calleeAttestation = _LargeAttestation(
      offers: Uint8List.fromList(List.generate(3000, (i) => (i * 17) % 256)),
      application: callerApplication,
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
    final helloReceived = Completer<void>();
    callee.onPeerConnected = (_, __, ___) => calleeEvents.add('connected');
    callee.onMessageReceived = (_, payload, __, ___) {
      calleeEvents.add('received ${utf8.decode(payload)}');
      if (utf8.decode(payload) == 'hello' && !helloReceived.isCompleted) {
        helloReceived.complete();
      }
    };

    caller.putKnownPeer(calleeId.publicKey);
    callee.putKnownPeer(callerId.publicKey);
    caller.wan!.putPeerAddress(calleeId.publicKey, '127.0.0.1:${relay.port}');
    final messageId = await caller.send(
      calleeId.publicKey,
      Uint8List.fromList(utf8.encode('hello')),
    );
    // The delivery attempt runs after send has returned, so the hold is
    // awaited on the store's queued status rather than read at once.
    await stateWhere(caller.store,
        (s) => s.messages.outgoingMessages[messageId]?.status == MessageStatus.queued);
    expect(caller.queuedMessageCountForPeer(calleeId.publicKey), 1,
        reason: 'held: the callee is not reachable yet');

    await helloReceived.future;
    expect(calleeEvents.contains('received hello'), isTrue,
        reason: 'delivered once the exchange completed');
    expect(calleeEvents.first, 'connected',
        reason: 'nothing is delivered before the callee has the caller\'s '
            'attestation');
    // The callee delivers the message once it has all its fragments, before
    // their acknowledgments reach the caller, whose queue gives it up only
    // then (spec §Message Protocol).
    // The caller's carrier reports the message sent once its fragments are
    // acknowledged, and the queue gives it up on the turn that follows.
    await stateWhere(caller.store, (s) {
      final status = s.messages.outgoingMessages[messageId]?.status;
      return status == MessageStatus.sent ||
          status == MessageStatus.delivered ||
          status == MessageStatus.read;
    });
    await Future<void>.delayed(Duration.zero);
    expect(caller.queuedMessageCountForPeer(calleeId.publicKey), 0,
        reason: 'the queue gives the message up once it is acknowledged');
  }, timeout: Timeout.none);

  test(
      '[F05] send completes once its message is saved, and its delivery '
      'attempt runs after it: while every fragment is lost, send has returned '
      'and no delivery outcome is recorded yet (api.tex, Send payload)',
      () async {
    final callerAttestation = _LargeAttestation(
      offers: Uint8List.fromList(List.generate(3000, (i) => (i * 19) % 256)),
      application: calleeApplication,
    );
    final calleeAttestation = _LargeAttestation(
      offers: Uint8List.fromList(List.generate(3000, (i) => (i * 23) % 256)),
      application: callerApplication,
    );

    final callerId = await identityFromSeed(0x97);
    final calleeId = await identityFromSeed(0x98);

    // The message ids for which the caller's store records a delivery
    // outcome: sent, or queued again after the attempt failed.
    final outcomes = <String>[];
    final caller = await startNode(callerId, callerAttestation,
        onAction: (action) {
      if (action is MessageSentAction) outcomes.add(action.messageId);
      if (action is MessageQueuedAction) outcomes.add(action.messageId);
    });
    final callee = await startNode(calleeId, calleeAttestation);
    final calleeAddress = await listeningAddress(callee);
    final relay =
        await _LossyRelay.start(int.parse(calleeAddress.split(':').last));
    addTearDown(() async {
      relay.close();
      await caller.dispose();
      await callee.dispose();
    });

    final helloReceived = Completer<void>();
    final secondReceived = Completer<void>();
    final callerConnects = Completer<void>();
    callee.onMessageReceived = (_, payload, __, ___) {
      final text = utf8.decode(payload);
      if (text == 'hello' && !helloReceived.isCompleted) {
        helloReceived.complete();
      }
      if (text == 'second' && !secondReceived.isCompleted) {
        secondReceived.complete();
      }
    };
    caller.onPeerConnected = (_, __, ___) {
      if (!callerConnects.isCompleted) callerConnects.complete();
    };

    caller.putKnownPeer(calleeId.publicKey);
    callee.putKnownPeer(callerId.publicKey);
    caller.wan!.putPeerAddress(calleeId.publicKey, '127.0.0.1:${relay.port}');
    await caller.send(
      calleeId.publicKey,
      Uint8List.fromList(utf8.encode('hello')),
    );
    await helloReceived.future;
    await callerConnects.future;

    // Every message fragment sent toward the callee from here on is lost, so
    // the second message's delivery attempt cannot finish.
    relay.dropTowardCallee = (datagram) =>
        datagram.isNotEmpty &&
        datagram.first == PacketType.secureFragment.value;
    final messageId = await caller.send(
      calleeId.publicKey,
      Uint8List.fromList(utf8.encode('second')),
    );
    expect(outcomes.where((id) => id == messageId), isEmpty,
        reason: 'send returned with the message saved, while its delivery '
            'attempt is still in flight');

    // The fragments get through, and the message is delivered.
    relay.dropTowardCallee = (_) => false;
    await secondReceived.future;
  }, timeout: Timeout.none);
}
