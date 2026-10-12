import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:redux/redux.dart';
import 'package:sodium/sodium_sumo.dart';
import 'package:test/test.dart';

import 'package:grassroots_networking_core/grassroots_networking_core.dart';

import 'helpers/attestation_fixtures.dart';

/// An Android producer on the test fixtures: a chain of three P-256
/// certificates over the identity key, and the per-session signature by the
/// attestation key it carries.
class _AndroidProducer implements PlatformAttestation {
  final fixture = AndroidFixture();
  final verifier = AttestationVerifier(
    clock: () => fixtureTime,
    androidRoots: [AndroidFixture().root],
  );

  @override
  Future<Uint8List?> attestationFor(Uint8List identityPublicKey) async =>
      encodePlatformAttestation(
          AndroidKeyAttestationContent(fixture.chain(identityPublicKey)));

  @override
  Future<Uint8List?> signSessionDigest({
    required Uint8List identityPublicKey,
    required Uint8List digest,
  }) async =>
      fixture.sign(digest);

  @override
  Future<AttestationVerdict> verify({
    required AttestationEvidence? evidence,
    required Uint8List digest,
    required Uint8List peerIdentityKey,
  }) async =>
      verifier.verify(
        evidence: evidence,
        digest: digest,
        peerIdentityKey: peerIdentityKey,
      );
}

/// An App Attest producer whose attestation object carries a receipt of
/// [receiptBytes], as Apple's does: what makes a real object several
/// kilobytes.
class _AppAttestProducer implements PlatformAttestation {
  static const int receiptBytes = 4096;

  final fixture = AppAttestFixture();
  final verifier = AttestationVerifier(
    clock: () => fixtureTime,
    appAttestRoots: [AppAttestFixture().root],
  );

  @override
  Future<Uint8List?> attestationFor(Uint8List identityPublicKey) async =>
      encodePlatformAttestation(AppAttestContent(
        appId: fixture.appId,
        attestationObject: fixture.attestationObject(
          identityPublicKey,
          receipt: List.generate(receiptBytes, (i) => i % 251),
        ),
      ));

  @override
  Future<Uint8List?> signSessionDigest({
    required Uint8List identityPublicKey,
    required Uint8List digest,
  }) async =>
      fixture.assertion(digest);

  @override
  Future<AttestationVerdict> verify({
    required AttestationEvidence? evidence,
    required Uint8List digest,
    required Uint8List peerIdentityKey,
  }) async =>
      verifier.verify(
        evidence: evidence,
        digest: digest,
        peerIdentityKey: peerIdentityKey,
      );
}

/// What [producer] puts on the wire as its half of the exchange, signed over
/// a stand-in digest: the size is the point, and a signature's size does not
/// depend on what it signs.
Future<Uint8List> _exchangePayload(
  PlatformAttestation producer,
  Uint8List identityPublicKey,
) async =>
    encodeAttestationPayload(AttestationEvidence(
      attestation: (await producer.attestationFor(identityPublicKey))!,
      signature: (await producer.signSessionDigest(
        identityPublicKey: identityPublicKey,
        digest: Uint8List(32),
      ))!,
    ));

/// A loopback UDP relay between a caller and a service that loses the
/// datagrams [dropTowardService] picks on their way to the service. The
/// frame's type byte is in the clear, so a session-encrypted attestation
/// fragment is told by its first byte alone.
class _LossyRelay {
  _LossyRelay._(this._socket, this._servicePort) {
    _socket.listen((event) {
      if (event != RawSocketEvent.read) return;
      for (var d = _socket.receive(); d != null; d = _socket.receive()) {
        _relay(d);
      }
    });
  }

  static Future<_LossyRelay> start(int servicePort) async => _LossyRelay._(
        await RawDatagramSocket.bind(InternetAddress.loopbackIPv4, 0),
        servicePort,
      );

  final RawDatagramSocket _socket;
  final int _servicePort;
  InternetAddress? _callerAddress;
  int? _callerPort;

  /// Picks the caller's datagrams that are lost on the way to the service.
  bool Function(Uint8List datagram) dropTowardService = (_) => false;

  /// Picks the service's datagrams that are lost on the way to the caller.
  bool Function(Uint8List datagram) dropTowardCaller = (_) => false;

  /// Datagrams lost on the way to the caller.
  int droppedTowardCaller = 0;

  /// Attestation fragments the caller sent toward the service, lost or not.
  int attestationFragmentsSeen = 0;

  /// Datagrams lost on the way to the service.
  int dropped = 0;

  /// Every datagram the caller sent toward the service, lost or not: when,
  /// on [clock], and its frame's type byte.
  final List<({Duration at, int type})> towardService = [];

  /// The clock [towardService] is read on.
  final Stopwatch clock = Stopwatch()..start();

  int get port => _socket.port;

  void _relay(Datagram d) {
    final fromService = d.port == _servicePort &&
        d.address.address == InternetAddress.loopbackIPv4.address;
    if (fromService) {
      if (dropTowardCaller(d.data)) {
        droppedTowardCaller++;
        return;
      }
      final address = _callerAddress;
      final port = _callerPort;
      if (address != null && port != null) _socket.send(d.data, address, port);
      return;
    }
    _callerAddress = d.address;
    _callerPort = d.port;
    if (d.data.isNotEmpty) {
      towardService.add((at: clock.elapsed, type: d.data.first));
    }
    if (d.data.isNotEmpty &&
        d.data.first == PacketType.secureAttestationFragment.value) {
      attestationFragmentsSeen++;
    }
    if (dropTowardService(d.data)) {
      dropped++;
      return;
    }
    _socket.send(d.data, InternetAddress.loopbackIPv4, _servicePort);
  }

  void close() => _socket.close();
}

/// A platform whose producer never answers: its holder never sends its half
/// of the exchange. It verifies what it is offered as the default does.
class _SilentProducer implements PlatformAttestation {
  @override
  Future<Uint8List?> attestationFor(Uint8List identityPublicKey) =>
      Completer<Uint8List?>().future;

  @override
  Future<Uint8List?> signSessionDigest({
    required Uint8List identityPublicKey,
    required Uint8List digest,
  }) =>
      Completer<Uint8List?>().future;

  @override
  Future<AttestationVerdict> verify({
    required AttestationEvidence? evidence,
    required Uint8List digest,
    required Uint8List peerIdentityKey,
  }) =>
      const NoPlatformAttestation().verify(
        evidence: evidence,
        digest: digest,
        peerIdentityKey: peerIdentityKey,
      );
}

/// A platform with nothing to offer, as the default, that counts the
/// attestations it is asked to judge.
class _CountingVerifier implements PlatformAttestation {
  int verifications = 0;

  @override
  Future<Uint8List?> attestationFor(Uint8List identityPublicKey) async => null;

  @override
  Future<Uint8List?> signSessionDigest({
    required Uint8List identityPublicKey,
    required Uint8List digest,
  }) async =>
      null;

  @override
  Future<AttestationVerdict> verify({
    required AttestationEvidence? evidence,
    required Uint8List digest,
    required Uint8List peerIdentityKey,
  }) {
    verifications++;
    return const NoPlatformAttestation().verify(
      evidence: evidence,
      digest: digest,
      peerIdentityKey: peerIdentityKey,
    );
  }
}

bool _isAttestationFragment(Uint8List datagram) =>
    datagram.isNotEmpty &&
    datagram.first == PacketType.secureAttestationFragment.value;

/// The attestation exchange rides the Message Protocol (spec §Session
/// Establishment: each side sends its attestation and signature "as a message
/// of the Message Protocol (Section 2.9) after handshake message 3 and before
/// the peer is reported reachable"; §Message Protocol: "One message protocol
/// carries send payloads, and the attestation exchange"). A real attestation
/// is larger than one datagram, so it must be fragmented; a fragment can be
/// lost, so it must be acknowledged and retransmitted.
///
/// The headless cases need a native libsodium and self-skip without one, as
/// the other headless tests do.
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

  Future<GrassrootsIdentity> identityFromSeed(int fill, String name) async =>
      GrassrootsIdentity.create(
        keyPair: await Ed25519()
            .newKeyPairFromSeed(Uint8List.fromList(List.filled(32, fill))),
        nickname: name,
      );

  Future<(HeadlessGrassrootsNetwork, HeadlessGrassrootsNetwork)> pair(
    PlatformAttestation serviceAttestation,
    PlatformAttestation callerAttestation,
    int seed,
  ) async {
    final service = HeadlessGrassrootsNetwork(
      identity: await identityFromSeed(seed, 'service'),
      rendezvousServers: const [],
      sodium: sodium!,
      attestation: serviceAttestation,
    );
    final caller = HeadlessGrassrootsNetwork(
      identity: await identityFromSeed(seed + 1, 'caller'),
      rendezvousServers: const [],
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

  /// The caller dials [port] — the service's own, or a relay's in front of
  /// it — by sending it a payload.
  Future<void> dial(
    HeadlessGrassrootsNetwork caller,
    HeadlessGrassrootsNetwork service,
    int port,
  ) async {
    caller.putPeerAddress(service.identity.publicKey, '127.0.0.1:$port');
    await caller.send(
      service.identity.publicKey,
      Uint8List.fromList(utf8.encode('hello')),
    );
  }

  final androidIdentity = AndroidApplicationIdentity(
    packages: const [AndroidPackage(name: 'com.eshapiro.grassapp', version: 7)],
    signatureDigests: [AndroidFixture().signingDigest],
  );

  group('headless profile, over UDP', () {
    test(
        'an Android chain of three certificates, above one datagram, reaches '
        'the peer and is verified', () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final (service, caller) =
          await pair(_AndroidProducer(), _AndroidProducer(), 0x71);

      // The report's smallest case (2026-10-07): as one packet the exchange
      // exceeds the carrier's datagram limit and was refused; as a message it
      // needs more than one fragment.
      final payload =
          await _exchangePayload(_AndroidProducer(), caller.identity.publicKey);
      expect(
        GrassrootsPacket(type: PacketType.attestationFragment, payload: payload)
            .serialize()
            .length,
        greaterThan(UdpTransportService.maxDatagramBytes),
      );
      expect(payload.length, greaterThan(FragmentHandler.udpMaxChunk));

      final serviceSaw = Completer<ApplicationIdentity?>();
      final callerSaw = Completer<ApplicationIdentity?>();
      service.onPeerConnected = (_, __, identity) {
        if (!serviceSaw.isCompleted) serviceSaw.complete(identity);
      };
      caller.onPeerConnected = (_, __, identity) {
        if (!callerSaw.isCompleted) callerSaw.complete(identity);
      };

      await dial(caller, service, service.boundPort!);

      expect(await serviceSaw.future.timeout(const Duration(seconds: 15)),
          androidIdentity,
          reason: 'the service verified the caller\'s Android chain');
      expect(await callerSaw.future.timeout(const Duration(seconds: 15)),
          androidIdentity,
          reason: 'the caller verified the service\'s Android chain');
    });

    test('a multi-kilobyte App Attest object reaches the peer and is verified',
        () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final (service, caller) =
          await pair(_AppAttestProducer(), _AppAttestProducer(), 0x73);

      final payload = await _exchangePayload(
          _AppAttestProducer(), caller.identity.publicKey);
      expect(payload.length, greaterThan(4096));
      expect(payload.length, greaterThan(3 * FragmentHandler.udpMaxChunk),
          reason: 'four fragments or more');

      final serviceSaw = Completer<ApplicationIdentity?>();
      final callerSaw = Completer<ApplicationIdentity?>();
      service.onPeerConnected = (_, __, identity) {
        if (!serviceSaw.isCompleted) serviceSaw.complete(identity);
      };
      caller.onPeerConnected = (_, __, identity) {
        if (!callerSaw.isCompleted) callerSaw.complete(identity);
      };

      await dial(caller, service, service.boundPort!);

      final appId = AppAttestFixture().appId;
      expect(await serviceSaw.future.timeout(const Duration(seconds: 15)),
          IosApplicationIdentity(appId));
      expect(await callerSaw.future.timeout(const Duration(seconds: 15)),
          IosApplicationIdentity(appId));
    });

    test(
        'a lost attestation fragment is retransmitted and the exchange '
        'completes', () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final (service, caller) =
          await pair(_AndroidProducer(), _AndroidProducer(), 0x75);
      final relay = await _LossyRelay.start(service.boundPort!);
      addTearDown(relay.close);

      // Lose the first attestation fragment the caller sends, once.
      relay.dropTowardService =
          (datagram) => _isAttestationFragment(datagram) && relay.dropped == 0;

      final serviceSaw = Completer<ApplicationIdentity?>();
      service.onPeerConnected = (_, __, identity) {
        if (!serviceSaw.isCompleted) serviceSaw.complete(identity);
      };

      await dial(caller, service, relay.port);

      expect(await serviceSaw.future.timeout(const Duration(seconds: 15)),
          androidIdentity,
          reason: 'the exchange completed although a fragment was lost');
      expect(relay.dropped, 1);
      // The chain needs two fragments; the service had both, so the lost one
      // came again.
      final fragments = ((await _exchangePayload(
                      _AndroidProducer(), caller.identity.publicKey))
                  .length /
              FragmentHandler.udpMaxChunk)
          .ceil();
      expect(fragments, 2);
      expect(relay.attestationFragmentsSeen, greaterThanOrEqualTo(fragments + 1),
          reason: 'the lost fragment was retransmitted');
    });

    test(
        'an attestation never acknowledged tears its session down when its '
        'attempts are exhausted', () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final (service, caller) =
          await pair(_AndroidProducer(), _AndroidProducer(), 0x77);
      final relay = await _LossyRelay.start(service.boundPort!);
      addTearDown(relay.close);

      // Every attestation fragment the caller sends is lost; the service's
      // reaches the caller.
      relay.dropTowardService = _isAttestationFragment;

      var serviceConnected = false;
      service.onPeerConnected = (_, __, ___) => serviceConnected = true;
      final callerConnected = Completer<void>();
      final callerDisconnected = Completer<Underlay>();
      caller.onPeerConnected = (_, __, ___) {
        if (!callerConnected.isCompleted) callerConnected.complete();
      };
      caller.onPeerDisconnected = (_, underlay) {
        if (!callerDisconnected.isCompleted) {
          callerDisconnected.complete(underlay);
        }
      };

      await dial(caller, service, relay.port);

      // The caller verified the service and reports it reachable...
      await callerConnected.future.timeout(const Duration(seconds: 15));
      // ...until its own attestation exhausts its attempts (spec §Message
      // Protocol: "When a message's attempts are exhausted the session is
      // torn down").
      expect(
        await callerDisconnected.future.timeout(const Duration(seconds: 60)),
        Underlay.wan,
      );
      expect(caller.isPeerReachable(service.identity.publicKey), isFalse);
      expect(relay.attestationFragmentsSeen, greaterThan(2),
          reason: 'the fragments were retransmitted before giving up');
      expect(serviceConnected, isFalse,
          reason: 'the service never received the caller\'s attestation');
    }, timeout: const Timeout(Duration(minutes: 2)));
  });

  // GNA, 2026-10-11, Task A: `send` held until the peer is reachable; a
  // payload arriving on a session whose exchange has not completed dropped
  // unacknowledged, so the sender retransmits; and Attestation Q1: the
  // receiver waits for the peer's attestation as long as the sender's
  // attempts last, then tears the session down.
  group('the exchange gates the payloads, headless over UDP', () {
    test(
        'send is held until the peer is reachable: no payload leaves before '
        'the caller reports the service connected', () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final (service, caller) = await pair(
          const NoPlatformAttestation(), const NoPlatformAttestation(), 0x81);
      final relay = await _LossyRelay.start(service.boundPort!);
      addTearDown(relay.close);

      Duration? callerConnectedAt;
      caller.onPeerConnected = (_, __, ___) =>
          callerConnectedAt ??= relay.clock.elapsed;
      final delivered = Completer<void>();
      service.onMessageReceived = (_, payload, __, ___) {
        if (utf8.decode(payload) == 'hello' && !delivered.isCompleted) {
          delivered.complete();
        }
      };

      await dial(caller, service, relay.port);
      await delivered.future.timeout(const Duration(seconds: 15));

      final payloadsAt = [
        for (final d in relay.towardService)
          if (d.type == PacketType.secureFragment.value) d.at,
      ];
      expect(payloadsAt, isNotEmpty);
      expect(callerConnectedAt, isNotNull);
      expect(payloadsAt, everyElement(greaterThanOrEqualTo(callerConnectedAt!)),
          reason: 'the payload waited for the service to be reachable');
    });

    test(
        'a payload arriving before the receiver has the sender\'s attestation '
        'is dropped unacknowledged, and its retransmission is delivered after '
        'the exchange completes', () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final (service, caller) = await pair(
          const NoPlatformAttestation(), const NoPlatformAttestation(), 0x83);
      final relay = await _LossyRelay.start(service.boundPort!);
      addTearDown(relay.close);

      // The caller's attestation is lost once on its way: the caller, which
      // has the service's, sends its payload while the service still waits
      // for the caller's attestation's retransmission.
      relay.dropTowardService =
          (datagram) => _isAttestationFragment(datagram) && relay.dropped == 0;

      final events = <String>[];
      Duration? serviceConnectedAt;
      service.onPeerConnected = (_, __, ___) {
        serviceConnectedAt ??= relay.clock.elapsed;
        events.add('connected');
      };
      final delivered = Completer<void>();
      service.onMessageReceived = (_, payload, __, ___) {
        events.add('received ${utf8.decode(payload)}');
        if (!delivered.isCompleted) delivered.complete();
      };

      await dial(caller, service, relay.port);
      await delivered.future.timeout(const Duration(seconds: 15));

      expect(events.first, 'connected',
          reason: 'nothing is delivered before the exchange completes');
      expect(events, contains('received hello'));
      final payloadsAt = [
        for (final d in relay.towardService)
          if (d.type == PacketType.secureFragment.value) d.at,
      ];
      expect(payloadsAt.where((at) => at < serviceConnectedAt!), isNotEmpty,
          reason: 'the payload reached the service before its exchange '
              'completed');
      expect(payloadsAt.length, greaterThanOrEqualTo(2),
          reason: 'unacknowledged, it was sent again');
    });

    test(
        'a receiver whose peer never attests tears the session down when the '
        'sender\'s attempts would have run out', () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      // The caller's producer never answers, so the caller never sends its
      // half of the exchange; nothing on the wire is lost.
      final (service, caller) =
          await pair(const NoPlatformAttestation(), _SilentProducer(), 0x85);
      final callerHex = caller.identity.publicKey
          .map((b) => b.toRadixString(16).padLeft(2, '0'))
          .join();
      var serviceConnected = false;
      service.onPeerConnected = (_, __, ___) => serviceConnected = true;
      var delivered = false;
      service.onMessageReceived = (_, __, ___, ____) => delivered = true;

      bool sessionStands() =>
          service.store.state.peers
              .getPeerByPubkeyHex(callerHex)
              ?.hasLiveUdpConnection ??
          false;

      await dial(caller, service, service.boundPort!);
      final clock = Stopwatch()..start();
      while (!sessionStands() && clock.elapsed < const Duration(seconds: 15)) {
        await Future<void>.delayed(const Duration(milliseconds: 50));
      }
      expect(sessionStands(), isTrue, reason: 'the handshake completed');
      final establishedAt = clock.elapsed;

      while (sessionStands() && clock.elapsed < const Duration(seconds: 60)) {
        await Future<void>.delayed(const Duration(milliseconds: 100));
      }
      expect(sessionStands(), isFalse,
          reason: 'the session was torn down for want of an attestation');
      final window = MessageTransportSender(
        maxChunk: FragmentHandler.udpMaxChunk,
        sendPacket: (_, __) async => false,
      ).attemptWindow;
      expect(clock.elapsed - establishedAt,
          greaterThanOrEqualTo(window - const Duration(seconds: 2)),
          reason: 'it waited as long as the sender\'s attempts last');
      expect(serviceConnected, isFalse);
      expect(delivered, isFalse,
          reason: 'no payload is delivered on a session never attested');
    }, timeout: const Timeout(Duration(minutes: 2)));
  });

  // GNA, 2026-10-11, Task B: no duplicate filter; a duplicate payload is
  // delivered, and a second attestation on a session already attested is
  // ignored.
  group('delivery at least once, headless over UDP', () {
    test(
        'an attestation re-sent after its acknowledgment was lost is ignored '
        'on the session it already attested', () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final serviceAttestation = _CountingVerifier();
      final (service, caller) = await pair(
          serviceAttestation, const NoPlatformAttestation(), 0x87);
      final relay = await _LossyRelay.start(service.boundPort!);
      addTearDown(relay.close);

      // The service's acknowledgments of the caller's attestation — its
      // fragment's and the whole message's, the first of each the service
      // sends — are lost, so the caller sends it again and the service,
      // which has it, receives it twice.
      var fragmentAckLost = false;
      var messageAckLost = false;
      relay.dropTowardCaller = (datagram) {
        if (datagram.isEmpty) return false;
        if (!fragmentAckLost &&
            datagram.first == PacketType.secureFragmentAck.value) {
          return fragmentAckLost = true;
        }
        if (!messageAckLost && datagram.first == PacketType.secureAck.value) {
          return messageAckLost = true;
        }
        return false;
      };

      var serviceConnects = 0;
      service.onPeerConnected = (_, __, ___) => serviceConnects++;
      final delivered = Completer<void>();
      service.onMessageReceived = (_, payload, __, ___) {
        if (!delivered.isCompleted) delivered.complete();
      };

      await dial(caller, service, relay.port);
      await delivered.future.timeout(const Duration(seconds: 15));
      // Past the caller's retransmission of its attestation.
      final settle = Stopwatch()..start();
      while (relay.attestationFragmentsSeen < 2 &&
          settle.elapsed < const Duration(seconds: 10)) {
        await Future<void>.delayed(const Duration(milliseconds: 100));
      }
      await Future<void>.delayed(const Duration(seconds: 1));

      expect(relay.droppedTowardCaller, 2);
      expect(relay.attestationFragmentsSeen, greaterThanOrEqualTo(2),
          reason: 'the attestation reached the service twice');
      expect(serviceAttestation.verifications, 1,
          reason: 'the second attestation was ignored, not judged again');
      expect(serviceConnects, 1);
      expect(service.isPeerReachable(caller.identity.publicKey), isTrue,
          reason: 'and the session it already attested stands');
    });
  });

  group('the GATT carrier', () {
    test(
        'fragments the attestation, retransmits a lost fragment, and delivers '
        'the message to the coordinator, not the application', () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final senderId = await identityFromSeed(0x79, 'sender');
      final receiverId = await identityFromSeed(0x7a, 'receiver');
      final router = MessageRouter(
        store: Store<AppState>(
          appReducer,
          initialState: const AppState(),
        ),
        protocolHandler: ProtocolHandler(identity: receiverId, sodium: sodium!),
        fragmentHandler: FragmentHandler(),
      );

      // The session layer stands in as the identity transform: the clear
      // fragment travels as its session-encrypted type, and decrypting is
      // naming its sender and its session. What is under test is the protocol
      // above it.
      final sessionHash = Uint8List.fromList(List.filled(32, 0x3c));
      router.decryptSessionPacket = (packet, transport, {String? peerId}) async =>
          (
            packet.copyWith(type: packet.type.clearVariant),
            senderId.publicKey,
            sessionHash,
          );

      final payload =
          await _exchangePayload(_AndroidProducer(), senderId.publicKey);
      final fragments = (payload.length / FragmentHandler.bleMaxChunk).ceil();
      expect(fragments, greaterThan(4),
          reason: 'more fragments than the GATT window holds');

      late final MessageTransportSender sender;
      final peerHex = senderId.publicKey
          .map((b) => b.toRadixString(16).padLeft(2, '0'))
          .join();
      final attestations = <(Uint8List, PeerTransport)>[];
      final sessions = <Uint8List>[];
      final payloads = <Uint8List>[];
      router.onAttestationReceived = (from, body, transport, handshakeHash) {
        attestations.add((body, transport));
        sessions.add(handshakeHash);
      };
      router.onMessageReceived = (_, __, body, ___) => payloads.add(body);
      // The receiver's acknowledgments return over the link.
      router.onFragmentAckRequested = (transport, peerId, messageId, index) =>
          Timer.run(() => sender.handleFragmentAck(peerHex, messageId, index));
      router.onAckRequested = (transport, peerId, messageId) =>
          Timer.run(() => sender.handleMessageAck(messageId));

      // GATT writes toward the receiver; fragment 2's first write is lost.
      final writes = <int, int>{};
      sender = MessageTransportSender.gatt(sendPacket: (_, packet) async {
        final index = FragmentHandler.decodeFragment(packet.payload,
                type: packet.type)
            .index;
        writes.update(index, (n) => n + 1, ifAbsent: () => 1);
        if (index == 2 && writes[index] == 1) return true; // written, lost
        final onTheWire = packet.copyWith(type: packet.type.secureVariant);
        Timer.run(() => router.processPacket(
              GrassrootsPacket.deserialize(onTheWire.serialize()),
              transport: PeerTransport.bleDirect,
              bleDeviceId: 'gatt-path',
            ));
        return true;
      });
      addTearDown(sender.dispose);

      final delivered = await sender
          .sendMessage(
            peerHex,
            '00000000-0000-4000-8000-0000000000ba',
            payload,
            fragmentType: PacketType.attestationFragment,
          )
          .timeout(const Duration(seconds: 20));

      expect(delivered, isTrue);
      expect(writes.length, fragments);
      expect(writes[2], 2, reason: 'the lost fragment was written again');
      expect(attestations, hasLength(1));
      expect(attestations.single.$1, payload,
          reason: 'reassembled whole, byte for byte');
      expect(attestations.single.$2, PeerTransport.bleDirect);
      expect(sessions.single, sessionHash,
          reason: 'judged against the session that decrypted it');
      expect(payloads, isEmpty,
          reason: 'the exchange never reaches the application');
      expect(
        decodeAttestationPayload(attestations.single.$1)!.attestation,
        (await _AndroidProducer().attestationFor(senderId.publicKey))!,
      );
    }, timeout: const Timeout(Duration(minutes: 1)));
  });

  group('the protocol', () {
    test('a bound message fails untransmitted once what it is bound to ends',
        () async {
      final peer = 'ab' * 32;
      var transmissions = 0;
      var current = true;
      final sender = MessageTransportSender(
        maxChunk: 100,
        initialBackoff: const Duration(milliseconds: 20),
        maxBackoff: const Duration(milliseconds: 40),
        // Nothing acknowledges: only the binding can end the message early.
        sendPacket: (_, __) async {
          transmissions++;
          return true;
        },
      );
      addTearDown(sender.dispose);

      final done = sender.sendMessage(
        peer,
        '00000000-0000-4000-8000-0000000000bb',
        Uint8List(50),
        fragmentType: PacketType.attestationFragment,
        isCurrent: () => current,
      );
      await Future<void>.delayed(const Duration(milliseconds: 5));
      expect(transmissions, 1);
      current = false; // the session it was bound to is gone

      expect(await done.timeout(const Duration(seconds: 5)), isFalse);
      expect(transmissions, 1,
          reason: 'nothing of it went out after its session ended');
    });

    test('a payload fragment cannot complete an attestation, nor the reverse',
        () {
      final handler = FragmentHandler();
      addTearDown(handler.dispose);
      const id = '00000000-0000-4000-8000-0000000000bc';
      final attestation = handler.fragment(
        payload: Uint8List.fromList(List.generate(300, (i) => i % 256)),
        messageId: id,
        maxChunk: 200,
        type: PacketType.attestationFragment,
      );
      expect(attestation.fragments.map((f) => f.type).toSet(),
          {PacketType.attestationFragment});
      final payload = handler.fragment(
        payload: Uint8List(300),
        messageId: id,
        maxChunk: 200,
      );

      Fragment decode(GrassrootsPacket p) =>
          FragmentHandler.decodeFragment(p.payload, type: p.type);

      expect(handler.addFragment(decode(attestation.fragments[0])), isNull);
      // The payload's second fragment shares the id and the count: dropped.
      expect(handler.addFragment(decode(payload.fragments[1])), isNull);
      final done = handler.addFragment(decode(attestation.fragments[1]));
      expect(done, isNotNull);
      expect(done!.type, PacketType.attestationFragment);
      expect(done.payload, List.generate(300, (i) => i % 256));
      expect(
        () => FragmentHandler.decodeFragment(attestation.fragments[0].payload,
            type: PacketType.ack),
        throwsArgumentError,
      );
    });
  });
}
