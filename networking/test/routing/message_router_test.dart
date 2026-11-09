import 'dart:async';
import 'dart:typed_data';
import 'package:flutter_test/flutter_test.dart';
import 'package:redux/redux.dart';
import 'package:cryptography/cryptography.dart';
import 'package:sodium_libs/sodium_libs.dart';
import 'package:grassroots_networking_core/src/routing/message_router.dart';
import 'package:grassroots_networking_core/src/protocol/protocol_handler.dart';
import 'package:grassroots_networking_core/src/protocol/fragment_handler.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/models/packet.dart';
import 'package:grassroots_networking_core/src/models/peer.dart';
import 'package:grassroots_networking_core/src/store/store.dart';

import '../helpers/sodium_test_bootstrap.dart';

/// Helper to create a self-signed ANNOUNCE payload:
/// [pubkey(32) + version(2) + signature(64)]
///
/// The trailing Ed25519 signature covers every preceding byte and must be
/// produced by [signer], whose identity key matches [pubkey] — this is what
/// `ProtocolHandler.decodeAnnounce` verifies.
Uint8List buildAnnouncePayload({
  required Uint8List pubkey,
  required ProtocolHandler signer,
}) {
  final buffer = BytesBuilder();

  buffer.add(pubkey);

  final versionBytes = ByteData(2);
  versionBytes.setUint16(0, ProtocolHandler.protocolVersion, Endian.big);
  buffer.add(versionBytes.buffer.asUint8List());

  final record = buffer.toBytes();
  buffer.add(signer.signBytes(record));
  return buffer.toBytes();
}

void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  late Sodium sodium;
  setUpAll(() async {
    sodium = await initTestSodium();
  });

  group('MessageRouter', () {
    late MessageRouter router;
    late Store<AppState> store;
    late GrassrootsIdentity identity;
    late GrassrootsIdentity otherIdentity;
    late ProtocolHandler protocolHandler;
    late ProtocolHandler otherProtocolHandler;
    late FragmentHandler fragmentHandler;

    /// The clock [fragmentHandler] measures its buffers' silence and its
    /// records of released messages by; a test moves it and runs the sweep
    /// (`debugDropSilentBuffers`) itself.
    late DateTime clock;
    late Uint8List otherPubkey;

    setUp(() async {
      final algorithm = Ed25519();
      final keyPair = await algorithm.newKeyPair();
      identity = await GrassrootsIdentity.create(
        keyPair: keyPair,
      );

      final otherKeyPair = await algorithm.newKeyPair();
      otherIdentity = await GrassrootsIdentity.create(
        keyPair: otherKeyPair,
      );

      store = Store<AppState>(
        appReducer,
        initialState: const AppState(),
      );

      protocolHandler = ProtocolHandler(identity: identity, sodium: sodium);
      otherProtocolHandler =
          ProtocolHandler(identity: otherIdentity, sodium: sodium);
      clock = DateTime.utc(2026, 11, 9, 12);
      fragmentHandler = FragmentHandler(now: () => clock);

      router = MessageRouter(
        store: store,
        protocolHandler: protocolHandler,
        fragmentHandler: fragmentHandler,
      );

      otherPubkey = otherIdentity.publicKey;
    });

    /// Self-signed ANNOUNCE payload from the other peer's perspective.
    Uint8List otherAnnouncePayload() {
      return buildAnnouncePayload(
        pubkey: otherPubkey,
        signer: otherProtocolHandler,
      );
    }

    /// The ANNOUNCE wire frame is just the payload in a packet — the frame
    /// itself carries no identity or signature.
    GrassrootsPacket announcePacket(Uint8List payload) {
      return GrassrootsPacket(
        type: PacketType.announce,
        payload: payload,
      );
    }

    /// The handshake hash the stubbed session reports.
    final stubSessionHash = Uint8List.fromList(List.filled(32, 0x5e));

    /// Stub the Noise session layer: every session-encrypted packet
    /// "decrypts" back to its clear variant with [sender] (default: the
    /// other peer) as the session-authenticated sender identity, under the
    /// session whose handshake hash is [stubSessionHash].
    void stubSession({Uint8List? sender}) {
      final senderPubkey = sender ?? otherPubkey;
      router.decryptSessionPacket =
          (packet, transport, {String? peerId}) async {
        return (
          packet.copyWith(type: packet.type.clearVariant),
          senderPubkey,
          stubSessionHash,
        );
      };
    }

    /// Build a session-encrypted packet as it would arrive off the wire.
    /// [type] is the CLEAR type; the packet carries its secure variant.
    GrassrootsPacket securePacket({
      required PacketType type,
      Uint8List? payload,
    }) {
      return GrassrootsPacket(
        type: type.secureVariant,
        payload: payload ?? Uint8List(0),
      );
    }

    /// Build a session-encrypted one-fragment message (spec §Message
    /// Transport: every message travels as self-contained fragments; a
    /// small message is a single fragment carrying the 36-char [messageId],
    /// index 0, count 1, and [body]).
    GrassrootsPacket secureMessagePacket({
      required String messageId,
      Uint8List? body,
    }) {
      return GrassrootsPacket(
        type: PacketType.secureFragment,
        payload: FragmentHandler.encodeFragment(
          messageId: messageId,
          index: 0,
          count: 1,
          chunk: body ?? Uint8List(0),
        ),
      );
    }

    // =========================================================================
    // Receive-Path Hardening
    // =========================================================================

    group('receive-path hardening', () {
      test('drops clear fragment packets unconditionally', () async {
        bool anyCalled = false;
        bool decryptCalled = false;
        router.onMessageReceived =
            (_, _____, __, ___, ____) => anyCalled = true;
        router.onFragmentAckRequested =
            (_, __, ___, ____) => anyCalled = true;
        router.decryptSessionPacket =
            (packet, transport, {String? peerId}) async {
          decryptCalled = true;
          return (packet, otherPubkey, Uint8List(32));
        };

        final p = GrassrootsPacket(
          type: PacketType.fragment,
          payload: Uint8List.fromList([1, 2, 3]),
        );

        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          rssi: -60,
        );

        expect(anyCalled, isFalse);
        expect(decryptCalled, isFalse,
            reason: 'Clear application-data packets must never reach the '
                'session layer — they are dropped outright.');
      });

      test('drops ANNOUNCE with a tampered payload byte', () async {
        bool announced = false;
        router.onPeerAnnounced =
            (_, __, {String? udpPeerId, bool inSession = false}) =>
                announced = true;

        final payload = otherAnnouncePayload();
        // Flip a version byte (offset 33 = inside pubkey 32 + version 2).
        // The record stays well-formed but its self-signature no longer
        // verifies.
        payload[33] ^= 0xFF;

        await router.processPacket(
          announcePacket(payload),
          transport: PeerTransport.bleDirect,
          rssi: -60,
        );

        expect(announced, isFalse);
        expect(store.state.peers.getPeerByPubkey(otherPubkey), isNull);
      });

      test('drops every clear application-data packet type', () async {
        // Spec: only three packet classes are accepted off the wire —
        // self-signed ANNOUNCE, Noise handshake, and session-encrypted
        // packets. Every clear application-data type is dropped without
        // reaching the session layer or any handler.
        const clearAppTypes = [
          PacketType.fragment,
          PacketType.fragmentAck,
          PacketType.signaling,
        ];

        for (final type in clearAppTypes) {
          bool anyCalled = false;
          bool decryptCalled = false;
          router.onMessageReceived =
              (_, _____, __, ___, ____) => anyCalled = true;
          router.onFragmentAckReceived =
              (_, __, ___, ____) => anyCalled = true;
          router.onFragmentAckRequested =
              (_, __, ___, ____) => anyCalled = true;
          router.onSignalingReceived =
              (_, __, {String? observedIp, int? observedPort}) =>
                  anyCalled = true;
          router.decryptSessionPacket =
              (packet, transport, {String? peerId}) async {
            decryptCalled = true;
            return (packet, otherPubkey, Uint8List(32));
          };

          await router.processPacket(
            GrassrootsPacket(
              type: type,
              payload: Uint8List.fromList([1, 2, 3]),
            ),
            transport: PeerTransport.bleDirect,
            rssi: -60,
          );

          expect(anyCalled, isFalse,
              reason: 'clear ${type.name} must not reach any handler');
          expect(decryptCalled, isFalse,
              reason: 'clear ${type.name} must not reach the session layer');
        }
      });

      test('drops session-encrypted packets when no session covers them',
          () async {
        bool messageReceived = false;
        router.onMessageReceived =
            (_, _____, __, ___, ____) => messageReceived = true;
        router.decryptSessionPacket =
            (packet, transport, {String? peerId}) async => null;

        await router.processPacket(
          securePacket(
            type: PacketType.fragment,
            payload: Uint8List.fromList([1]),
          ),
          transport: PeerTransport.bleDirect,
          rssi: -60,
        );

        expect(messageReceived, isFalse);
      });
    });

    // =========================================================================
    // BLE Packet Processing - ANNOUNCE
    // =========================================================================

    group('processPacket (BLE) - ANNOUNCE', () {
      test('can reject verified BLE ANNOUNCE before peer state is created',
          () async {
        String? rejectedDeviceId;
        router.shouldAcceptBleAnnounce =
            (_, {String? bleDeviceId, BleRole? bleRole}) => false;
        router.onBleAnnounceRejected = (_, bleDeviceId) {
          rejectedDeviceId = bleDeviceId;
        };

        final p = announcePacket(otherAnnouncePayload());

        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          bleDeviceId: 'central:test',
          rssi: -55,
        );

        expect(store.state.peers.getPeerByPubkey(otherPubkey), isNull);
        expect(rejectedDeviceId, equals('central:test'));
      });

      test('decodes ANNOUNCE and dispatches PeerAnnounceReceivedAction',
          () async {
        final p = announcePacket(otherAnnouncePayload());

        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          rssi: -55,
        );

        final peer = store.state.peers.getPeerByPubkey(otherPubkey);
        expect(peer, isNotNull);
        expect(peer!.rssi, equals(-55));
        expect(peer.transport, equals(PeerTransport.bleDirect));
      });

      test('includes bleDeviceId in dispatch', () async {
        final p = announcePacket(otherAnnouncePayload());

        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          bleDeviceId: 'ble-device-1',
          bleRole: BleRole.central,
          rssi: -60,
        );

        final peer = store.state.peers.getPeerByPubkey(otherPubkey);
        expect(peer, isNotNull);
        expect(peer!.bleDeviceId, equals('ble-device-1'));
      });

      test('uses scan RSSI when BLE announce arrives without payload RSSI',
          () async {
        store.dispatch(BleDeviceDiscoveredAction(
          deviceId: 'scan-device-1',
          rssi: -42,
        ));

        final p = announcePacket(otherAnnouncePayload());

        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          bleDeviceId: 'scan-device-1',
          bleRole: BleRole.central,
          rssi: null,
        );

        final peer = store.state.peers.getPeerByPubkey(otherPubkey);
        expect(peer, isNotNull);
        expect(peer!.bleDeviceId, equals('scan-device-1'));
        expect(peer.rssi, equals(-42));
      });

      test('keeps peripheral-only RSSI as null', () async {
        final p = announcePacket(otherAnnouncePayload());

        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          bleDeviceId: 'peripheral:central-1',
          bleRole: BleRole.peripheral,
          rssi: null,
        );

        final peer = store.state.peers.getPeerByPubkey(otherPubkey);
        expect(peer, isNotNull);
        expect(peer!.hasBleConnection, isTrue);
        expect(peer.blePeripheralDeviceId, equals('peripheral:central-1'));
        expect(peer.rssi, isNull);
      });

      test('never stores an address from ANNOUNCE (identity beacon only)',
          () async {
        final p = announcePacket(otherAnnouncePayload());

        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          rssi: -50,
        );

        // Spec §ANNOUNCE and Liveness: address distribution is not
        // ANNOUNCE's role — addresses reach the layer via putPeerAddress.
        final peer = store.state.peers.getPeerByPubkey(otherPubkey);
        expect(peer, isNotNull);
        expect(peer!.udpAddress, isNull);
      });

      test('fires onPeerAnnounced callback', () async {
        AnnounceData? receivedData;
        PeerTransport? receivedTransport;
        router.onPeerAnnounced =
            (data, transport, {String? udpPeerId, bool inSession = false}) {
          receivedData = data;
          receivedTransport = transport;
        };

        final p = announcePacket(otherAnnouncePayload());

        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          rssi: -45,
        );

        expect(receivedData, isNotNull);
        expect(receivedData!.publicKey, equals(otherPubkey));
        expect(receivedTransport, equals(PeerTransport.bleDirect));
      });

      test('always processes ANNOUNCE even if seen before (no dedup)',
          () async {
        final p = announcePacket(otherAnnouncePayload());

        int announceCount = 0;
        router.onPeerAnnounced =
            (_, __, {String? udpPeerId, bool inSession = false}) => announceCount++;

        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          rssi: -55,
        );
        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          rssi: -50,
        );

        expect(announceCount, equals(2));
      });
    });

    // =========================================================================
    // BLE Packet Processing - MESSAGE
    // =========================================================================

    group('processPacket (BLE) - MESSAGE', () {
      test('delivers session-decrypted message with the session sender',
          () async {
        stubSession();

        String? receivedId;
        String? receivedSenderMessageId;
        Uint8List? receivedPubkey;
        Uint8List? receivedPayload;
        PeerTransport? receivedTransport;
        router.onMessageReceived =
            (id, senderMessageId, pubkey, payload, transport) {
          receivedId = id;
          receivedSenderMessageId = senderMessageId;
          receivedPubkey = pubkey;
          receivedPayload = payload;
          receivedTransport = transport;
        };

        const messageId = 'aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa';
        final body = Uint8List.fromList([1, 2, 3, 4, 5]);
        final p = secureMessagePacket(messageId: messageId, body: body);

        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          rssi: -60,
        );

        expect(receivedId,
            equals(MessageRouter.deliveryIdOf(otherPubkey, messageId)));
        // The sender's own id, the wire id its fragments carry.
        expect(receivedSenderMessageId, equals(messageId));
        expect(receivedPubkey, equals(otherPubkey));
        expect(receivedPayload, equals(body));
        expect(receivedTransport, equals(PeerTransport.bleDirect));
      });

      test('reports the authoritative arrival transport (UDP)', () async {
        stubSession();

        PeerTransport? receivedTransport;
        router.onMessageReceived = (_, ____, __, ___, transport) {
          receivedTransport = transport;
        };

        final p = secureMessagePacket(
          messageId: 'bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb',
          body: Uint8List.fromList([9, 9, 9]),
        );

        await router.processPacket(
          p,
          transport: PeerTransport.wan,
        );

        expect(receivedTransport, equals(PeerTransport.wan));
      });

      test('drops a fragment whose payload lacks the fragment header',
          () async {
        stubSession();

        bool anyCalled = false;
        router.onMessageReceived =
            (_, _____, __, ___, ____) => anyCalled = true;
        router.onFragmentAckRequested =
            (_, __, ___, ____) => anyCalled = true;

        // Shorter than the fragment header (messageId + index + count).
        final p = securePacket(
          type: PacketType.fragment,
          payload: Uint8List.fromList([1, 2, 3]),
        );

        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          rssi: -60,
        );

        expect(anyCalled, isFalse);
      });

      test('does not overwrite known RSSI when payload RSSI is null', () async {
        stubSession();

        store.dispatch(PeerAnnounceReceivedAction(
          underSession: true,
          publicKey: otherPubkey,
          protocolVersion: ProtocolHandler.protocolVersion,
          rssi: -44,
          bleCentralDeviceId: 'central:peer-1',
        ));

        final p = secureMessagePacket(
          messageId: 'cccccccc-cccc-cccc-cccc-cccccccccccc',
          body: Uint8List.fromList([42]),
        );

        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          rssi: null,
        );

        final peer = store.state.peers.getPeerByPubkey(otherPubkey);
        expect(peer, isNotNull);
        expect(peer!.rssi, equals(-44));
      });
    });

    // =========================================================================
    // BLE Packet Processing - Deduplication
    // =========================================================================

    group('processPacket - delivery at least once', () {
      // GNA, 2026-10-11, Task B: the duplicate filter is gone. "A re-send
      // after a lost acknowledgment, possibly over another underlay, may
      // deliver a message multiple times, and madGLP dedupes repeated
      // deliveries" (spec §Message Protocol). A re-send within
      // reassemblyTimeout of the message's release is acknowledged and not
      // delivered again; one after it is delivered again.
      test(
          'a re-sent MESSAGE is acknowledged at once, and not delivered '
          'again, while its release is recorded; delivered again after',
          () async {
        stubSession();

        int messageCount = 0;
        router.onMessageReceived = (_, _____, __, ___, ____) => messageCount++;
        int ackCount = 0;
        router.onFragmentAckRequested = (_, __, ___, ____) => ackCount++;

        final p = secureMessagePacket(
          messageId: '22222222-2222-2222-2222-222222222222',
          body: Uint8List.fromList([1]),
        );

        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          rssi: -60,
        );
        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          rssi: -60,
        );
        expect((messageCount, ackCount), (1, 2),
            reason: 'released lately: acknowledged, not delivered again');

        clock = clock.add(
            FragmentHandler.reassemblyTimeout + const Duration(seconds: 1));
        fragmentHandler.debugDropSilentBuffers();
        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          rssi: -60,
        );

        expect((messageCount, ackCount), (2, 3),
            reason: 'its record gone, the re-send is delivered again');
      });

      test(
          'a payload re-sent over another underlay after its release is '
          'acknowledged on that underlay, so its sender stops, and not '
          'delivered again', () async {
        stubSession();

        final deliveries = <String>[];
        final ackRequests = <String>[];
        router.onMessageReceived =
            (messageId, _____, __, ___, ____) => deliveries.add(messageId);
        router.onFragmentAckRequested =
            (_, __, messageId, ___) => ackRequests.add(messageId);

        const messageId = '44444444-4444-4444-4444-444444444444';
        final p = secureMessagePacket(
          messageId: messageId,
          body: Uint8List.fromList([1]),
        );

        await router.processPacket(p,
            transport: PeerTransport.bleDirect, rssi: -60);
        await router.processPacket(p,
            transport: PeerTransport.wan, rssi: -60);

        final deliveryId = MessageRouter.deliveryIdOf(otherPubkey, messageId);
        expect(deliveries, [deliveryId]);
        expect(ackRequests, [messageId, messageId]);
      });

      test('a fresh message is never dropped', () async {
        // Ten thousand distinct messages: the capacity at which the Bloom
        // filter that was the duplicate filter took about one fresh message
        // in a hundred for a duplicate, and dropped it.
        stubSession();

        final delivered = <String>{};
        router.onMessageReceived =
            (messageId, _____, __, ___, ____) => delivered.add(messageId);

        const count = 10000;
        for (var i = 0; i < count; i++) {
          final messageId =
              '00000000-0000-4000-8000-${i.toRadixString(16).padLeft(12, '0')}';
          await router.processPacket(
            secureMessagePacket(
              messageId: messageId,
              body: Uint8List.fromList([i & 0xff]),
            ),
            transport: PeerTransport.wan,
          );
        }

        expect(delivered, hasLength(count));
      });
    });

    // =========================================================================
    // BLE Packet Processing - Fragments
    // =========================================================================

    group('processPacket - fragments', () {
      test('reassembles fragmented message and delivers', () async {
        stubSession();

        Uint8List? reassembledPayload;
        Uint8List? reassembledSender;
        router.onMessageReceived = (_, ____, sender, payload, ___) {
          reassembledSender = sender;
          reassembledPayload = payload;
        };

        final payload = Uint8List(1000);
        for (var i = 0; i < payload.length; i++) {
          payload[i] = i % 256;
        }

        const messageId = '00000000-0000-4000-8000-000000000009';
        final fragmented = fragmentHandler.fragment(
          payload: payload,
          messageId: messageId,
          maxChunk: 270,
        );

        final ackedIndices = <int>[];
        router.onFragmentAckRequested = (_, __, id, index) {
          expect(id, messageId);
          ackedIndices.add(index);
        };

        for (final fragment in fragmented.fragments) {
          // On the wire each fragment travels session-encrypted.
          await router.processPacket(
            fragment.copyWith(type: fragment.type.secureVariant),
            transport: PeerTransport.bleDirect,
            rssi: -60,
          );
        }

        expect(reassembledPayload, isNotNull);
        expect(reassembledPayload, equals(payload));
        expect(reassembledSender, equals(otherPubkey));
        // Every fragment was acknowledged (spec §Message Transport).
        expect(ackedIndices, hasLength(fragmented.fragments.length));
      });
    });

    // =========================================================================
    // The last fragment is acknowledged after the save (GNA, 2026-11-08)
    // =========================================================================

    group('the last fragment is acknowledged after the save', () {
      // A message whose fragments are all acknowledged is settled at its
      // sender and never sent again, so a fixed fragment, its last by index,
      // is acknowledged only once the whole message is reassembled, kept
      // (keepDelivered) and delivered, whatever order the fragments arrive
      // in; every other fragment is acknowledged on arrival, and no fragment
      // is acknowledged that was not stored.
      const messageId = '00000000-0000-4000-8000-0000000000c1';
      final body = Uint8List.fromList(List.generate(700, (i) => i % 256));
      late List<GrassrootsPacket> fragments;

      setUp(() {
        fragments = fragmentHandler
            .fragment(payload: body, messageId: messageId, maxChunk: 270)
            .fragments;
      });

      Future<void> arrive(int index) => router.processPacket(
            fragments[index].copyWith(
                type: fragments[index].type.secureVariant),
            transport: PeerTransport.wan,
            udpPeerId: 'peer-c1',
          );

      String deliveryId() => MessageRouter.deliveryIdOf(otherPubkey, messageId);

      test(
          'the last fragment is acknowledged only once its save has '
          'completed, and after the message is delivered', () async {
        stubSession();
        expect(fragments, hasLength(3));
        final events = <String>[];
        final saveGate = Completer<void>();
        final saveBegun = Completer<String>();
        var saves = 0;
        router.keepDelivered =
            (id, senderMessageId, sender, payload, transport) {
          saves++;
          events.add('save $id');
          saveBegun.complete(id);
          return saveGate.future;
        };
        router.onMessageReceived =
            (id, ___, _, payload, __) => events.add('delivered $id');
        final lastAck = Completer<void>();
        router.onFragmentAckRequested = (_, __, id, index) {
          expect(id, messageId);
          events.add('ack $index');
          if (index == 2 && !lastAck.isCompleted) {
            lastAck.complete();
          }
        };

        await arrive(0);
        await arrive(1);
        expect(events, ['ack 0', 'ack 1'],
            reason: 'a fragment that does not complete the message is '
                'acknowledged on arrival');

        await arrive(2);
        expect(await saveBegun.future, deliveryId());
        expect(events, ['ack 0', 'ack 1', 'save ${deliveryId()}'],
            reason: 'no acknowledgment of the last fragment, and no '
                'delivery, while its save is under way');

        // Retransmissions while the save is under way: the last fragment
        // waits for that save; another is acknowledged at once.
        await arrive(2);
        await arrive(0);
        expect(saves, 1, reason: 'the save under way is not begun again');
        expect(events, ['ack 0', 'ack 1', 'save ${deliveryId()}', 'ack 0']);

        saveGate.complete();
        await lastAck.future;
        expect(events, [
          'ack 0',
          'ack 1',
          'save ${deliveryId()}',
          'ack 0',
          'delivered ${deliveryId()}',
          'ack 2',
        ]);
      }, timeout: Timeout.none);

      test(
          'a single-fragment message: its one packet is its last fragment, '
          'acknowledged once the save has completed', () async {
        stubSession();
        final saveGate = Completer<void>();
        router.keepDelivered = (_, _____, __, ___, ____) => saveGate.future;
        final acked = Completer<int>();
        router.onFragmentAckRequested = (_, __, ___, index) =>
            acked.complete(index);
        final delivered = <String>[];
        router.onMessageReceived = (id, ____, _, __, ___) => delivered.add(id);

        await router.processPacket(
          secureMessagePacket(
              messageId: messageId, body: Uint8List.fromList([5])),
          transport: PeerTransport.bleDirect,
        );
        expect(acked.isCompleted, isFalse);
        expect(delivered, isEmpty);

        saveGate.complete();
        expect(await acked.future, 0);
        expect(delivered, [deliveryId()]);
      }, timeout: Timeout.none);

      test(
          'a failed save leaves the last fragment unacknowledged and '
          'the buffer kept; its retransmission completes the message again, '
          'and the save is retried', () async {
        stubSession();
        final firstSave = Completer<void>();
        final secondSaveBegun = Completer<void>();
        var saves = 0;
        router.keepDelivered = (_, ____, __, payload, ___) {
          saves++;
          expect(payload, body, reason: 'reassembled whole each time');
          if (saves == 1) return firstSave.future;
          secondSaveBegun.complete();
          return Future.value();
        };
        final delivered = <String>[];
        router.onMessageReceived = (id, ____, _, __, ___) => delivered.add(id);
        final acks = <int>[];
        final lastAck = Completer<void>();
        router.onFragmentAckRequested = (_, __, ___, index) {
          acks.add(index);
          if (index == 2) lastAck.complete();
        };

        await arrive(0);
        await arrive(1);
        await arrive(2);
        expect(saves, 1);

        // The save fails. The router's handler of the failure was registered
        // on the save's future first, so it has run when this wait resumes.
        firstSave.completeError(StateError('the store is full'));
        await firstSave.future.then((_) {}, onError: (Object _) {});
        expect(acks, [0, 1],
            reason: 'the last fragment is not acknowledged');
        expect(delivered, isEmpty, reason: 'nor is the message delivered');

        // The sender retransmits the one fragment it has no acknowledgment
        // of; the buffer kept the others.
        await arrive(2);
        await secondSaveBegun.future;
        await lastAck.future;
        expect(saves, 2);
        expect(acks, [0, 1, 2]);
        expect(delivered, [deliveryId()]);
      }, timeout: Timeout.none);

      test(
          'the reassembly buffer outlives the timeout while fragments keep '
          'arriving, and the message completes and is saved; a silence longer '
          'than the timeout drops it', () async {
        // The clock the buffers' silence is measured by is the test's, and
        // the test runs the sweep the periodic timer runs.
        var clock = DateTime.utc(2026, 11, 8, 12);
        final handler = FragmentHandler(now: () => clock);
        addTearDown(handler.dispose);
        final spacedRouter = MessageRouter(
          store: store,
          protocolHandler: protocolHandler,
          fragmentHandler: handler,
        );
        spacedRouter.decryptSessionPacket =
            (packet, transport, {String? peerId}) async => (
                  packet.copyWith(type: packet.type.clearVariant),
                  otherPubkey,
                  stubSessionHash,
                );
        final saved = <String>[];
        spacedRouter.keepDelivered = (id, ___, _, payload, __) async {
          expect(payload, body);
          saved.add(id);
        };
        final delivered = <String>[];
        spacedRouter.onMessageReceived =
            (id, ____, _, __, ___) => delivered.add(id);
        final acks = <(String, int)>[];
        final lastAck = Completer<void>();
        spacedRouter.onFragmentAckRequested = (_, __, id, index) {
          acks.add((id, index));
          if (id == messageId && index == 2) lastAck.complete();
        };

        Future<void> arriveOf(List<GrassrootsPacket> of, int index) =>
            spacedRouter.processPacket(
              of[index].copyWith(type: of[index].type.secureVariant),
              transport: PeerTransport.wan,
              udpPeerId: 'peer-c1',
            );
        // Less than the timeout between fragments, more than it in all.
        final gap = FragmentHandler.reassemblyTimeout * 0.75;

        await arriveOf(fragments, 0);
        clock = clock.add(gap);
        handler.debugDropSilentBuffers();
        await arriveOf(fragments, 1);
        clock = clock.add(gap);
        handler.debugDropSilentBuffers();
        expect(gap * 2, greaterThan(FragmentHandler.reassemblyTimeout));
        await arriveOf(fragments, 2);
        await lastAck.future;
        expect(saved, [deliveryId()],
            reason: 'the buffer was kept while fragments arrived, so the '
                'last one completed the message, and it was saved');
        expect(delivered, [deliveryId()]);

        // A second message, then a silence longer than the timeout: its
        // buffer is dropped, and its other fragments complete nothing.
        const silentId = '00000000-0000-4000-8000-0000000000c2';
        final silent = fragmentHandler
            .fragment(payload: body, messageId: silentId, maxChunk: 270)
            .fragments;
        await arriveOf(silent, 0);
        clock = clock.add(
            FragmentHandler.reassemblyTimeout + const Duration(seconds: 1));
        handler.debugDropSilentBuffers();
        await arriveOf(silent, 1);
        await arriveOf(silent, 2);
        expect(saved, [deliveryId()],
            reason: 'the dropped buffer lost fragment 0: nothing completes');
        expect(delivered, [deliveryId()]);
        expect(acks.where((a) => a.$1 == silentId).map((a) => a.$2), [0, 1],
            reason: 'each lands in a buffer it does not complete, and the '
                'last is held back: the sender sends it again until it gives '
                'up and sends the whole message again');
      }, timeout: Timeout.none);

      /// A payload router over the setUp's, its saves recorded in [kept]
      /// until the test removes them, as the runtime's acknowledgment does.
      ({List<int> acks, List<String> delivered, Set<String> kept, List<int> saves})
          keptRouter() {
        final kept = <String>{};
        final saves = <int>[];
        router.keepDelivered = (id, ____, _, __, ___) async {
          saves.add(saves.length + 1);
          kept.add(id);
        };
        router.isKeptDelivered = kept.contains;
        final delivered = <String>[];
        router.onMessageReceived = (id, ____, _, __, ___) => delivered.add(id);
        final acks = <int>[];
        return (acks: acks, delivered: delivered, kept: kept, saves: saves);
      }

      Future<void> settleWhole(List<int> acks) async {
        final lastAck = Completer<void>();
        router.onFragmentAckRequested = (_, __, ___, index) {
          acks.add(index);
          if (index == 2 && !lastAck.isCompleted) lastAck.complete();
        };
        for (var i = 0; i < fragments.length; i++) {
          await arrive(i);
        }
        await lastAck.future;
      }

      test(
          'after the save, and after the runtime acknowledged the message, a '
          'retransmitted last fragment is acknowledged at once and the '
          'message not delivered again: its release is recorded', () async {
        stubSession();
        final r = keptRouter();
        await settleWhole(r.acks);
        expect(r.acks, [0, 1, 2]);
        expect(r.delivered, [deliveryId()]);

        // The runtime acknowledges the message at once (the glue does, as
        // soon as the cycle is saved); then the sender, whose
        // acknowledgment of the last fragment was lost, retransmits it.
        r.kept.remove(deliveryId());
        await arrive(2);
        expect(r.acks, [0, 1, 2, 2],
            reason: 'acknowledged at once, so the sender settles');
        expect(r.delivered, [deliveryId()], reason: 'not delivered again');
        expect(r.saves, [1], reason: 'nor saved again');
      }, timeout: Timeout.none);

      test(
          'after reassemblyTimeout the record of the release is gone: a kept '
          'message is still acknowledged at once; once the runtime '
          'acknowledged it, the last fragment alone is held back in a fresh '
          'buffer', () async {
        stubSession();
        final r = keptRouter();
        await settleWhole(r.acks);

        clock = clock.add(
            FragmentHandler.reassemblyTimeout + const Duration(seconds: 1));
        fragmentHandler.debugDropSilentBuffers();

        // Still kept: acknowledged at once, not delivered again.
        await arrive(2);
        expect(r.acks, [0, 1, 2, 2]);
        expect(r.delivered, [deliveryId()]);

        // Acknowledged by the runtime, and its release no longer recorded:
        // the last fragment lands in a fresh buffer and is held back; the
        // sender retransmits it until it gives up and sends the whole
        // message again (exhaustion, teardown, requeue), which is saved and
        // delivered again (at least once).
        r.kept.remove(deliveryId());
        await arrive(2);
        expect(r.acks, [0, 1, 2, 2]);
        expect(r.delivered, [deliveryId()]);
        expect(r.saves, [1]);
      }, timeout: Timeout.none);

      test(
          'an attestation: after it reached the coordinator, a retransmitted '
          'last fragment is acknowledged at once and the coordinator not '
          'called again; after reassemblyTimeout the record is gone',
          () async {
        stubSession();
        final attestation = Uint8List.fromList(List.generate(700, (i) => i));
        final attestationFragments = fragmentHandler
            .fragment(
              payload: attestation,
              messageId: messageId,
              maxChunk: 270,
              type: PacketType.attestationFragment,
            )
            .fragments;
        expect(attestationFragments, hasLength(3));
        final judged = <Uint8List>[];
        router.onAttestationReceived = (_, body, __, ___) => judged.add(body);
        router.onMessageReceived = (_, _____, __, ___, ____) =>
            fail('the exchange never reaches the application');
        final acks = <int>[];
        router.onFragmentAckRequested = (_, __, ___, index) => acks.add(index);
        Future<void> arriveAttestation(int index) => router.processPacket(
              attestationFragments[index].copyWith(
                  type: attestationFragments[index].type.secureVariant),
              transport: PeerTransport.wan,
              udpPeerId: 'peer-c1',
            );

        for (var i = 0; i < attestationFragments.length; i++) {
          await arriveAttestation(i);
        }
        expect(judged, [attestation]);
        expect(acks, [0, 1, 2]);

        await arriveAttestation(2);
        expect(acks, [0, 1, 2, 2], reason: 'acknowledged at once');
        expect(judged, hasLength(1), reason: 'not judged again');

        clock = clock.add(
            FragmentHandler.reassemblyTimeout + const Duration(seconds: 1));
        fragmentHandler.debugDropSilentBuffers();
        await arriveAttestation(2);
        expect(acks, [0, 1, 2, 2],
            reason: 'its record gone, the last fragment alone is held back');
        expect(judged, hasLength(1));
      }, timeout: Timeout.none);

      /// A router as the setUp's, over [handler]: the session stub, a save
      /// that records the saved payloads in [saved], and every
      /// acknowledgment, with the session it goes out on, in [acks].
      MessageRouter routerOver(
        FragmentHandler handler, {
        required List<Uint8List> saved,
        required _AckLog acks,
        Future<void> Function()? saveGate,
        List<String>? events,
      }) {
        final r = MessageRouter(
          store: store,
          protocolHandler: protocolHandler,
          fragmentHandler: handler,
        );
        r.decryptSessionPacket = (packet, transport, {String? peerId}) async =>
            (
              packet.copyWith(type: packet.type.clearVariant),
              otherPubkey,
              stubSessionHash,
            );
        r.keepDelivered = (_, ____, __, payload, ___) async {
          events?.add('save');
          await saveGate?.call();
          saved.add(payload);
        };
        r.onMessageReceived =
            (_, _____, __, ___, ____) => events?.add('delivered');
        r.onFragmentAckRequested = (transport, _, id, index) {
          events?.add('ack $index');
          acks.add((transport, index));
        };
        return r;
      }

      Future<void> deliver(MessageRouter r, GrassrootsPacket fragment,
              PeerTransport transport) =>
          r.processPacket(
            fragment.copyWith(type: fragment.type.secureVariant),
            transport: transport,
            udpPeerId: transport.isIp ? 'peer-c1' : null,
            bleDeviceId: transport.isIp ? null : 'central:c1',
          );

      test(
          'a fragment of another attempt at the message, at another chunk '
          'size, replaces the stale buffer: nothing is acknowledged that was '
          'dropped, and the message completes and is saved from the new '
          'attempt alone', () async {
        // The message goes over BLE in three fragments; the link drops after
        // two; the sender sends it again over UDP at once, in two.
        final handler = FragmentHandler();
        addTearDown(handler.dispose);
        final saved = <Uint8List>[];
        final acks = _AckLog();
        final r = routerOver(handler, saved: saved, acks: acks);
        final overBle = fragmentHandler
            .fragment(payload: body, messageId: messageId, maxChunk: 270)
            .fragments;
        final overUdp = fragmentHandler
            .fragment(payload: body, messageId: messageId, maxChunk: 400)
            .fragments;
        expect((overBle.length, overUdp.length), (3, 2));

        await deliver(r, overBle[0], PeerTransport.bleDirect);
        await deliver(r, overBle[1], PeerTransport.bleDirect);
        await deliver(r, overUdp[0], PeerTransport.wan);
        expect(acks.log, [
          (PeerTransport.bleDirect, 0),
          (PeerTransport.bleDirect, 1),
          (PeerTransport.wan, 0),
        ], reason: 'the UDP fragment is stored, in a buffer it starts, and '
            'acknowledged as stored');
        expect(saved, isEmpty);

        await deliver(r, overUdp[1], PeerTransport.wan);
        await acks.until((PeerTransport.wan, 1));
        expect(saved, hasLength(1));
        expect(saved.single, body,
            reason: 'reassembled from the UDP attempt alone, byte for byte');
        expect(acks.log, [
          (PeerTransport.bleDirect, 0),
          (PeerTransport.bleDirect, 1),
          (PeerTransport.wan, 0),
          (PeerTransport.wan, 1),
        ], reason: 'the last UDP fragment once saved; the BLE attempt\'s '
            'last fragment never');

        // The single-fragment form: a one-fragment attempt replaces a stale
        // three-fragment buffer and completes at once.
        final oneHandler = FragmentHandler();
        addTearDown(oneHandler.dispose);
        final oneSaved = <Uint8List>[];
        final oneAcks = _AckLog();
        final one = routerOver(oneHandler, saved: oneSaved, acks: oneAcks);
        final overUdpWhole = fragmentHandler
            .fragment(payload: body, messageId: messageId, maxChunk: 1100)
            .fragments;
        expect(overUdpWhole, hasLength(1));
        await deliver(one, overBle[0], PeerTransport.bleDirect);
        await deliver(one, overUdpWhole[0], PeerTransport.wan);
        await oneAcks.until((PeerTransport.wan, 0));
        expect(oneSaved.single, body);
        expect(oneAcks.log,
            [(PeerTransport.bleDirect, 0), (PeerTransport.wan, 0)]);
      }, timeout: Timeout.none);

      test(
          'a receiver that lost its buffer never acknowledges the last '
          'fragment alone: it is retransmitted into a fresh buffer and held '
          'back, nothing is saved, and the whole message sent again completes',
          () async {
        final saved = <Uint8List>[];
        final acks = _AckLog();
        final before = FragmentHandler();
        addTearDown(before.dispose);
        final r1 = routerOver(before, saved: saved, acks: acks);
        await deliver(r1, fragments[0], PeerTransport.wan);
        await deliver(r1, fragments[1], PeerTransport.wan);
        expect(acks.log, [(PeerTransport.wan, 0), (PeerTransport.wan, 1)]);

        // The receiver restarts: a new router over a new buffer. The
        // sender's lane retransmits only the fragment it has no
        // acknowledgment of.
        final after = FragmentHandler();
        addTearDown(after.dispose);
        final r2 = routerOver(after, saved: saved, acks: acks);
        await deliver(r2, fragments[2], PeerTransport.wan);
        expect(acks.log, [(PeerTransport.wan, 0), (PeerTransport.wan, 1)],
            reason: 'the last fragment, alone in a fresh buffer, is held back');
        expect(saved, isEmpty);

        // The sender gives up, tears the session down and sends the whole
        // message again (F06): it completes, is saved, and is settled.
        for (final fragment in fragments) {
          await deliver(r2, fragment, PeerTransport.wan);
        }
        await acks.until((PeerTransport.wan, 2));
        expect(saved.single, body);
        expect(acks.log.last, (PeerTransport.wan, 2));
      }, timeout: Timeout.none);

      test(
          'the last fragment arriving first is held back until the message '
          'is reassembled, saved and delivered; the others are acknowledged '
          'on arrival', () async {
        final handler = FragmentHandler();
        addTearDown(handler.dispose);
        final saved = <Uint8List>[];
        final acks = _AckLog();
        final events = <String>[];
        final saveGate = Completer<void>();
        final r = routerOver(handler,
            saved: saved,
            acks: acks,
            events: events,
            saveGate: () => saveGate.future);

        await deliver(r, fragments[2], PeerTransport.wan);
        expect(events, isEmpty, reason: 'held back, and nothing to save');
        await deliver(r, fragments[0], PeerTransport.wan);
        await deliver(r, fragments[1], PeerTransport.wan);
        expect(events, ['ack 0', 'ack 1', 'save'],
            reason: 'the fragment that completes the message is not the held '
                'one: acknowledged on arrival; the last waits for the save');

        saveGate.complete();
        await acks.until((PeerTransport.wan, 2));
        expect(events, ['ack 0', 'ack 1', 'save', 'delivered', 'ack 2']);
        expect(saved.single, body);
      }, timeout: Timeout.none);
    });

    // =========================================================================
    // The attestation exchange gates the payloads (GNA, 2026-10-11, Task A)
    // =========================================================================

    group('the attestation exchange gates the payloads', () {
      test(
          'a payload on a session whose exchange has not completed is dropped '
          'unacknowledged; on a completed one it is delivered and acknowledged',
          () async {
        stubSession();
        var attested = false;
        final asked = <Uint8List>[];
        router.isSessionAttested = (_, __, handshakeHash) {
          asked.add(handshakeHash);
          return attested;
        };
        final delivered = <String>[];
        final fragmentAcks = <String>[];
        router.onMessageReceived = (id, ____, _, __, ___) => delivered.add(id);
        router.onFragmentAckRequested =
            (_, __, id, ___) => fragmentAcks.add(id);

        const messageId = '55555555-5555-4555-8555-555555555555';
        final p = secureMessagePacket(
          messageId: messageId,
          body: Uint8List.fromList([7]),
        );

        await router.processPacket(p, transport: PeerTransport.wan);
        expect(delivered, isEmpty);
        expect(fragmentAcks, isEmpty,
            reason: 'unacknowledged, so the sender retransmits');
        expect(asked.single, stubSessionHash,
            reason: 'asked of the session that decrypted it');

        attested = true;
        await router.processPacket(p, transport: PeerTransport.wan);
        expect(delivered, [MessageRouter.deliveryIdOf(otherPubkey, messageId)]);
        expect(fragmentAcks, [messageId],
            reason: 'its one fragment, the last one, acknowledged '
                'once it is delivered');
      });

      test(
          'an attestation reaches the coordinator, with the handshake hash of '
          'the session that decrypted it, before its session is attested',
          () async {
        stubSession();
        router.isSessionAttested = (_, __, ___) => false;
        final judged = <(Uint8List, Uint8List)>[];
        router.onAttestationReceived = (_, body, __, handshakeHash) =>
            judged.add((body, handshakeHash));
        router.onMessageReceived = (_, _____, __, ___, ____) =>
            fail('the exchange never reaches the application');

        await router.processPacket(
          GrassrootsPacket(
            type: PacketType.secureAttestationFragment,
            payload: FragmentHandler.encodeFragment(
              messageId: '66666666-6666-4666-8666-666666666666',
              index: 0,
              count: 1,
              chunk: Uint8List.fromList([9, 9]),
            ),
          ),
          transport: PeerTransport.wan,
        );

        expect(judged, hasLength(1));
        expect(judged.single.$1, [9, 9]);
        expect(judged.single.$2, stubSessionHash);
      });
    });

    // =========================================================================
    // Packet Processing - ACK/NACK
    // =========================================================================

    group('processPacket - ACK/NACK', () {
      test(
          'a frame of the retired whole-message ACK type does not decode, so '
          'it never reaches the router (GNA, 2026-11-08)', () {
        // 0x0F was the session-encrypted whole-message ACK; a message whose
        // fragments are all acknowledged is settled, and nothing else is.
        expect(
          () => GrassrootsPacket.deserialize(Uint8List.fromList(
              [0x0F, 0, 0, 0, 8, ...'acked-id'.codeUnits])),
          throwsArgumentError,
        );
      });

      test('routes a fragment ACK to onFragmentAckReceived', () async {
        stubSession();

        String? ackedId;
        int? ackedIndex;
        router.onFragmentAckReceived = (_, id, index, __) {
          ackedId = id;
          ackedIndex = index;
        };

        const messageId = '00000000-0000-4000-8000-00000000000a';
        final p = securePacket(
          type: PacketType.fragmentAck,
          payload: FragmentHandler.encodeFragmentAck(
              messageId: messageId, index: 3),
        );

        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          rssi: -60,
        );

        expect(ackedId, equals(messageId));
        expect(ackedIndex, equals(3));
      });
    });

    // =========================================================================
    // UDP Packet Processing - ANNOUNCE
    // =========================================================================

    group('processPacket (UDP) - ANNOUNCE', () {
      test('decodes ANNOUNCE and dispatches to Redux', () async {
        final p = announcePacket(otherAnnouncePayload());

        await router.processPacket(
          p,
          transport: PeerTransport.wan,
          udpPeerId: 'peer-123',
        );

        final peer = store.state.peers.getPeerByPubkey(otherPubkey);
        expect(peer, isNotNull);
        expect(peer!.transport, equals(PeerTransport.wan));
      });

      test('identifies UDP connection with the verified announce pubkey',
          () async {
        Uint8List? identifiedPubkey;
        String? identifiedPeerId;
        router.onUdpPeerIdentified = (pubkey, udpPeerId) {
          identifiedPubkey = pubkey;
          identifiedPeerId = udpPeerId;
        };

        final p = announcePacket(otherAnnouncePayload());

        await router.processPacket(
          p,
          transport: PeerTransport.wan,
          udpPeerId: 'temp-key-1',
        );

        expect(identifiedPubkey, equals(otherPubkey));
        expect(identifiedPeerId, equals('temp-key-1'));
      });

      test('does not attach scan-discovered BLE ID to UDP ANNOUNCE', () async {
        store.dispatch(BleDeviceDiscoveredAction(
          deviceId: 'scan-device-1',
          rssi: -42,
        ));

        final p = announcePacket(otherAnnouncePayload());

        await router.processPacket(
          p,
          transport: PeerTransport.wan,
          udpPeerId: 'peer-123',
        );

        final peer = store.state.peers.getPeerByPubkey(otherPubkey);
        expect(peer, isNotNull);
        expect(peer!.hasBleConnection, isFalse);
        expect(peer.bleDeviceId, isNull);
        // UDP-only peer has no BLE link, so rssi is null.
        expect(peer.rssi, isNull);
        expect(store.state.peers.nearbyBlePeers, isEmpty);
      });

      test('does not use peerId as fallback address when not in payload',
          () async {
        // udpPeerId is a hex pubkey, not an ip:port address — it must not
        // be stored as udpAddress.
        final p = announcePacket(otherAnnouncePayload());

        await router.processPacket(
          p,
          transport: PeerTransport.wan,
          udpPeerId: 'fallback-peer-id',
        );

        final peer = store.state.peers.getPeerByPubkey(otherPubkey);
        expect(peer, isNotNull);
        expect(peer!.udpAddress, isNull);
      });

      test('fires onPeerAnnounced callback with UDP transport', () async {
        PeerTransport? receivedTransport;
        router.onPeerAnnounced =
            (_, transport, {String? udpPeerId, bool inSession = false}) {
          receivedTransport = transport;
        };

        final p = announcePacket(otherAnnouncePayload());

        await router.processPacket(
          p,
          transport: PeerTransport.wan,
          udpPeerId: 'peer-456',
        );

        expect(receivedTransport, equals(PeerTransport.wan));
      });
    });

    // =========================================================================
    // UDP Packet Processing - MESSAGE
    // =========================================================================

    group('processPacket (UDP) - MESSAGE', () {
      test('delivers message via onMessageReceived', () async {
        stubSession();

        String? receivedId;
        Uint8List? receivedPubkey;
        Uint8List? receivedPayload;
        router.onMessageReceived = (id, __, pubkey, payload, _) {
          receivedId = id;
          receivedPubkey = pubkey;
          receivedPayload = payload;
        };

        const messageId = 'dddddddd-dddd-dddd-dddd-dddddddddddd';
        final body = Uint8List.fromList([10, 20, 30]);
        final p = secureMessagePacket(messageId: messageId, body: body);

        await router.processPacket(
          p,
          transport: PeerTransport.wan,
          udpPeerId: 'peer-789',
        );

        expect(receivedId,
            equals(MessageRouter.deliveryIdOf(otherPubkey, messageId)));
        expect(receivedPubkey, equals(otherPubkey));
        expect(receivedPayload, equals(body));
      });

      test('marks existing peer as seen over UDP', () async {
        stubSession();

        store.dispatch(PeerIdentityRegisteredAction(publicKey: otherPubkey));

        final p = secureMessagePacket(
          messageId: 'eeeeeeee-eeee-eeee-eeee-eeeeeeeeeeee',
          body: Uint8List.fromList([9, 8, 7]),
        );

        await router.processPacket(
          p,
          transport: PeerTransport.wan,
          udpPeerId: 'peer-seen-test',
        );

        final peer = store.state.peers.getPeerByPubkey(otherPubkey);
        expect(peer, isNotNull);
        expect(peer!.lastWanSeen, isNotNull);
        expect(peer.lastLanSeen, isNull,
            reason: 'a packet on the WAN session refreshes the WAN clock alone');
        expect(peer.lastSeen, isNotNull);
      });

      test(
          'acknowledges the last fragment with canonical UDP peer id',
          () async {
        stubSession();

        PeerTransport? ackTransport;
        String? ackPeerId;
        String? ackMessageId;
        int? ackIndex;
        router.onMessageReceived = (_, _____, __, ___, ____) {};
        router.onFragmentAckRequested = (transport, peerId, messageId, index) {
          ackTransport = transport;
          ackPeerId = peerId;
          ackMessageId = messageId;
          ackIndex = index;
        };

        const messageId = 'ffffffff-ffff-ffff-ffff-ffffffffffff';
        final p = secureMessagePacket(
          messageId: messageId,
          body: Uint8List.fromList([1, 2, 3]),
        );

        await router.processPacket(
          p,
          transport: PeerTransport.wan,
          udpPeerId: 'peer-ack-test',
        );

        expect(ackTransport, equals(PeerTransport.wan));
        expect(
          ackPeerId,
          equals(
            'wan/'
            '${otherPubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join()}',
          ),
        );
        expect(ackMessageId, equals(messageId));
        expect(ackIndex, 0);
      });

      test('acknowledges the last fragment of BLE messages too',
          () async {
        stubSession();

        PeerTransport? ackTransport;
        String? ackPeerId;
        String? ackMessageId;
        int? ackIndex;
        router.onMessageReceived = (_, _____, __, ___, ____) {};
        router.onFragmentAckRequested = (transport, peerId, messageId, index) {
          ackTransport = transport;
          ackPeerId = peerId;
          ackMessageId = messageId;
          ackIndex = index;
        };

        const messageId = '99999999-9999-9999-9999-999999999999';
        final p = secureMessagePacket(
          messageId: messageId,
          body: Uint8List.fromList([1, 2, 3]),
        );

        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          rssi: -60,
        );

        expect(ackTransport, equals(PeerTransport.bleDirect));
        expect(ackPeerId, isNull);
        expect(ackMessageId, equals(messageId));
        expect(ackIndex, 0);
      });
    });

    // =========================================================================
    // BLE Liveness - the stale sweep's clock
    // =========================================================================

    group('processPacket (BLE) - liveness', () {
      test(
          'an authenticated fragment ACK over BLE refreshes lastBleSeen and '
          'lastSeen', () async {
        stubSession();
        store.dispatch(PeerIdentityRegisteredAction(publicKey: otherPubkey));
        store.dispatch(AssociateBleDeviceAction(
          publicKey: otherPubkey,
          deviceId: 'central:liveness-dev',
          role: BleRole.central,
        ));
        expect(store.state.peers.getPeerByPubkey(otherPubkey)!.lastBleSeen,
            isNull);

        const livenessId = '00000000-0000-4000-8000-0000000000b1';
        final ackReceived = Completer<String>();
        router.onFragmentAckReceived =
            (_, id, __, ___) => ackReceived.complete(id);

        await router.processPacket(
          securePacket(
            type: PacketType.fragmentAck,
            payload: FragmentHandler.encodeFragmentAck(
                messageId: livenessId, index: 0),
          ),
          transport: PeerTransport.bleDirect,
          bleDeviceId: 'central:liveness-dev',
          bleRole: BleRole.central,
        );

        expect(await ackReceived.future, equals(livenessId));
        final peer = store.state.peers.getPeerByPubkey(otherPubkey)!;
        expect(peer.lastBleSeen, isNotNull);
        expect(peer.lastSeen, isNotNull);
      }, timeout: Timeout.none);

      test('an authenticated packet over BLE does not refresh a peer with no '
          'BLE link', () async {
        stubSession();
        store.dispatch(PeerIdentityRegisteredAction(publicKey: otherPubkey));

        const lateId = '00000000-0000-4000-8000-0000000000b2';
        final ackReceived = Completer<String>();
        router.onFragmentAckReceived =
            (_, id, __, ___) => ackReceived.complete(id);

        await router.processPacket(
          securePacket(
            type: PacketType.fragmentAck,
            payload:
                FragmentHandler.encodeFragmentAck(messageId: lateId, index: 0),
          ),
          transport: PeerTransport.bleDirect,
          bleDeviceId: 'central:liveness-dev',
          bleRole: BleRole.central,
        );

        expect(await ackReceived.future, equals(lateId));
        final peer = store.state.peers.getPeerByPubkey(otherPubkey)!;
        expect(peer.lastBleSeen, isNull);
        expect(peer.lastSeen, isNull);
      }, timeout: Timeout.none);

      test('a clear fragment ACK over BLE does not refresh lastBleSeen',
          () async {
        store.dispatch(PeerIdentityRegisteredAction(publicKey: otherPubkey));

        await router.processPacket(
          GrassrootsPacket(
            type: PacketType.fragmentAck,
            payload: FragmentHandler.encodeFragmentAck(
                messageId: '00000000-0000-4000-8000-0000000000b3', index: 0),
          ),
          transport: PeerTransport.bleDirect,
          bleDeviceId: 'central:liveness-dev',
          bleRole: BleRole.central,
        );

        expect(store.state.peers.getPeerByPubkey(otherPubkey)!.lastBleSeen,
            isNull);
      });
    });

    // =========================================================================
    // UDP Packet Processing - ACK
    // =========================================================================

    group('processPacket (UDP) - ACK', () {
      test(
          'delivers a fragment ACK via onFragmentAckReceived, naming its '
          'sender and the session it arrived on', () async {
        stubSession();

        (Uint8List, String, int, PeerTransport)? received;
        router.onFragmentAckReceived =
            (sender, id, index, transport) =>
                received = (sender, id, index, transport);

        const messageId = '00000000-0000-4000-8000-0000000000b4';
        final p = securePacket(
          type: PacketType.fragmentAck,
          payload:
              FragmentHandler.encodeFragmentAck(messageId: messageId, index: 1),
        );

        await router.processPacket(
          p,
          transport: PeerTransport.lan,
          udpPeerId: 'peer-abc',
        );

        expect(received, isNotNull);
        expect(received!.$1, equals(otherPubkey));
        expect(received!.$2, equals(messageId));
        expect(received!.$3, equals(1));
        expect(received!.$4, equals(PeerTransport.lan),
            reason: 'it drives the lane of the session it arrived on');
      });
    });

    // =========================================================================
    // Invalid / Malformed Packets
    // =========================================================================

    group('processPacket - invalid/malformed packets', () {
      test('deserialize rejects data shorter than header size', () {
        expect(
          () => GrassrootsPacket.deserialize(
            Uint8List(GrassrootsPacket.headerSize - 1),
          ),
          throwsA(isA<FormatException>()),
        );
      });

      test('deserialize rejects data with unknown packet type', () {
        // Build a buffer with headerSize bytes, but with type=0xFF
        final data = Uint8List(GrassrootsPacket.headerSize);
        data[0] = 0xFF; // unknown type
        expect(
          () => GrassrootsPacket.deserialize(data),
          throwsA(isA<ArgumentError>()),
        );
      });
    });
  });
}

/// The fragment acknowledgments a router sends, each with the session it goes
/// out on, and a wait on one of them.
class _AckLog {
  final List<(PeerTransport, int)> log = [];
  final List<((PeerTransport, int), Completer<void>)> _waits = [];

  void add((PeerTransport, int) ack) {
    log.add(ack);
    for (final wait in _waits.toList()) {
      if (wait.$1 == ack) {
        _waits.remove(wait);
        wait.$2.complete();
      }
    }
  }

  /// Completes once [ack] has been sent: at once if it has.
  Future<void> until((PeerTransport, int) ack) {
    if (log.contains(ack)) return Future.value();
    final done = Completer<void>();
    _waits.add((ack, done));
    return done.future;
  }
}
