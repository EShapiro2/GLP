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
      fragmentHandler = FragmentHandler();

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
        router.onMessageReceived = (_, __, ___, ____) => anyCalled = true;
        router.onAckRequested = (_, __, ___) => anyCalled = true;
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
            (_, __, {bool isNew = false, String? udpPeerId}) =>
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
          PacketType.ack,
          PacketType.readReceipt,
          PacketType.signaling,
        ];

        for (final type in clearAppTypes) {
          bool anyCalled = false;
          bool decryptCalled = false;
          router.onMessageReceived = (_, __, ___, ____) => anyCalled = true;
          router.onAckReceived = (_) => anyCalled = true;
          router.onReadReceiptReceived = (_) => anyCalled = true;
          router.onAckRequested = (_, __, ___) => anyCalled = true;
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
            (_, __, ___, ____) => messageReceived = true;
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
            (data, transport, {bool isNew = false, String? udpPeerId}) {
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
            (_, __, {bool isNew = false, String? udpPeerId}) => announceCount++;

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
        Uint8List? receivedPubkey;
        Uint8List? receivedPayload;
        PeerTransport? receivedTransport;
        router.onMessageReceived = (id, pubkey, payload, transport) {
          receivedId = id;
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

        expect(receivedId, equals(messageId));
        expect(receivedPubkey, equals(otherPubkey));
        expect(receivedPayload, equals(body));
        expect(receivedTransport, equals(PeerTransport.bleDirect));
      });

      test('reports the authoritative arrival transport (UDP)', () async {
        stubSession();

        PeerTransport? receivedTransport;
        router.onMessageReceived = (_, __, ___, transport) {
          receivedTransport = transport;
        };

        final p = secureMessagePacket(
          messageId: 'bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb',
          body: Uint8List.fromList([9, 9, 9]),
        );

        await router.processPacket(
          p,
          transport: PeerTransport.udp,
        );

        expect(receivedTransport, equals(PeerTransport.udp));
      });

      test('drops a fragment whose payload lacks the fragment header',
          () async {
        stubSession();

        bool anyCalled = false;
        router.onMessageReceived = (_, __, ___, ____) => anyCalled = true;
        router.onAckRequested = (_, __, ___) => anyCalled = true;

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
      // after a lost acknowledgment ... may deliver a message twice, which
      // madGLP tolerates" (spec §Message Protocol).
      test('a re-sent MESSAGE is delivered every time it arrives', () async {
        stubSession();

        int messageCount = 0;
        router.onMessageReceived = (_, __, ___, ____) => messageCount++;

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
        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          rssi: -60,
        );

        expect(messageCount, equals(3));
      });

      test(
          'a payload delivered twice reaches onMessageReceived twice, and is '
          'acknowledged each time so the sender stops', () async {
        stubSession();

        final deliveries = <String>[];
        final ackRequests = <String>[];
        router.onMessageReceived =
            (messageId, __, ___, ____) => deliveries.add(messageId);
        router.onAckRequested =
            (_, __, messageId) => ackRequests.add(messageId);

        const messageId = '44444444-4444-4444-4444-444444444444';
        final p = secureMessagePacket(
          messageId: messageId,
          body: Uint8List.fromList([1]),
        );

        await router.processPacket(p,
            transport: PeerTransport.bleDirect, rssi: -60);
        await router.processPacket(p,
            transport: PeerTransport.udp, rssi: -60);

        expect(deliveries, [messageId, messageId]);
        expect(ackRequests, [messageId, messageId]);
      });

      test('a fresh message is never dropped', () async {
        // Ten thousand distinct messages: the capacity at which the Bloom
        // filter that was the duplicate filter took about one fresh message
        // in a hundred for a duplicate, and dropped it.
        stubSession();

        final delivered = <String>{};
        router.onMessageReceived =
            (messageId, __, ___, ____) => delivered.add(messageId);

        const count = 10000;
        for (var i = 0; i < count; i++) {
          final messageId =
              '00000000-0000-4000-8000-${i.toRadixString(16).padLeft(12, '0')}';
          await router.processPacket(
            secureMessagePacket(
              messageId: messageId,
              body: Uint8List.fromList([i & 0xff]),
            ),
            transport: PeerTransport.udp,
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
        router.onMessageReceived = (_, sender, payload, ___) {
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
        final acks = <String>[];
        router.onMessageReceived = (id, _, __, ___) => delivered.add(id);
        router.onFragmentAckRequested =
            (_, __, id, ___) => fragmentAcks.add(id);
        router.onAckRequested = (_, __, id) => acks.add(id);

        const messageId = '55555555-5555-4555-8555-555555555555';
        final p = secureMessagePacket(
          messageId: messageId,
          body: Uint8List.fromList([7]),
        );

        await router.processPacket(p, transport: PeerTransport.udp);
        expect(delivered, isEmpty);
        expect(fragmentAcks, isEmpty,
            reason: 'unacknowledged, so the sender retransmits');
        expect(acks, isEmpty);
        expect(asked.single, stubSessionHash,
            reason: 'asked of the session that decrypted it');

        attested = true;
        await router.processPacket(p, transport: PeerTransport.udp);
        expect(delivered, [messageId]);
        expect(fragmentAcks, [messageId]);
        expect(acks, [messageId]);
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
        router.onMessageReceived = (_, __, ___, ____) =>
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
          transport: PeerTransport.udp,
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
      test('routes ACK to onAckReceived callback', () async {
        stubSession();

        String? receivedMessageId;
        router.onAckReceived = (messageId) => receivedMessageId = messageId;

        const messageId = 'acked-message-id';
        final p = securePacket(
          type: PacketType.ack,
          payload: Uint8List.fromList(messageId.codeUnits),
        );

        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          rssi: -60,
        );

        expect(receivedMessageId, equals(messageId));
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
    // Packet Processing - ReadReceipt
    // =========================================================================

    group('processPacket - readReceipt', () {
      test('routes read receipt to onReadReceiptReceived callback', () async {
        stubSession();

        String? receivedMessageId;
        router.onReadReceiptReceived = (id) => receivedMessageId = id;

        const messageId = 'msg-to-read';
        final p = securePacket(
          type: PacketType.readReceipt,
          payload: Uint8List.fromList(messageId.codeUnits),
        );

        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          rssi: -60,
        );

        expect(receivedMessageId, equals(messageId));
      });

      test('ignores read receipt with empty payload', () async {
        stubSession();

        String? receivedMessageId;
        router.onReadReceiptReceived = (id) => receivedMessageId = id;

        final p = securePacket(
          type: PacketType.readReceipt,
          payload: Uint8List(0),
        );

        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          rssi: -60,
        );

        expect(receivedMessageId, isNull);
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
          transport: PeerTransport.udp,
          udpPeerId: 'peer-123',
        );

        final peer = store.state.peers.getPeerByPubkey(otherPubkey);
        expect(peer, isNotNull);
        expect(peer!.transport, equals(PeerTransport.udp));
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
          transport: PeerTransport.udp,
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
          transport: PeerTransport.udp,
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
          transport: PeerTransport.udp,
          udpPeerId: 'fallback-peer-id',
        );

        final peer = store.state.peers.getPeerByPubkey(otherPubkey);
        expect(peer, isNotNull);
        expect(peer!.udpAddress, isNull);
      });

      test('fires onPeerAnnounced callback with UDP transport', () async {
        PeerTransport? receivedTransport;
        router.onPeerAnnounced =
            (_, transport, {bool isNew = false, String? udpPeerId}) {
          receivedTransport = transport;
        };

        final p = announcePacket(otherAnnouncePayload());

        await router.processPacket(
          p,
          transport: PeerTransport.udp,
          udpPeerId: 'peer-456',
        );

        expect(receivedTransport, equals(PeerTransport.udp));
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
        router.onMessageReceived = (id, pubkey, payload, _) {
          receivedId = id;
          receivedPubkey = pubkey;
          receivedPayload = payload;
        };

        const messageId = 'dddddddd-dddd-dddd-dddd-dddddddddddd';
        final body = Uint8List.fromList([10, 20, 30]);
        final p = secureMessagePacket(messageId: messageId, body: body);

        await router.processPacket(
          p,
          transport: PeerTransport.udp,
          udpPeerId: 'peer-789',
        );

        expect(receivedId, equals(messageId));
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
          transport: PeerTransport.udp,
          udpPeerId: 'peer-seen-test',
        );

        final peer = store.state.peers.getPeerByPubkey(otherPubkey);
        expect(peer, isNotNull);
        expect(peer!.lastUdpSeen, isNotNull);
        expect(peer.lastSeen, isNotNull);
      });

      test('triggers onAckRequested with canonical UDP peer id', () async {
        stubSession();

        PeerTransport? ackTransport;
        String? ackPeerId;
        String? ackMessageId;
        router.onMessageReceived = (_, __, ___, ____) {};
        router.onAckRequested = (transport, peerId, messageId) {
          ackTransport = transport;
          ackPeerId = peerId;
          ackMessageId = messageId;
        };

        const messageId = 'ffffffff-ffff-ffff-ffff-ffffffffffff';
        final p = secureMessagePacket(
          messageId: messageId,
          body: Uint8List.fromList([1, 2, 3]),
        );

        await router.processPacket(
          p,
          transport: PeerTransport.udp,
          udpPeerId: 'peer-ack-test',
        );

        expect(ackTransport, equals(PeerTransport.udp));
        expect(
          ackPeerId,
          equals(
            otherPubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join(),
          ),
        );
        expect(ackMessageId, equals(messageId));
      });

      test('triggers onAckRequested for BLE messages too', () async {
        stubSession();

        PeerTransport? ackTransport;
        String? ackPeerId;
        String? ackMessageId;
        router.onMessageReceived = (_, __, ___, ____) {};
        router.onAckRequested = (transport, peerId, messageId) {
          ackTransport = transport;
          ackPeerId = peerId;
          ackMessageId = messageId;
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
      });
    });

    // =========================================================================
    // UDP Packet Processing - ACK
    // =========================================================================

    group('processPacket (UDP) - ACK', () {
      test('delivers ACK via onAckReceived', () async {
        stubSession();

        String? receivedId;
        router.onAckReceived = (id) => receivedId = id;

        const messageId = 'ack-msg1';
        final p = securePacket(
          type: PacketType.ack,
          payload: Uint8List.fromList(messageId.codeUnits),
        );

        await router.processPacket(
          p,
          transport: PeerTransport.udp,
          udpPeerId: 'peer-abc',
        );

        expect(receivedId, equals(messageId));
      });
    });

    // =========================================================================
    // UDP Packet Processing - ReadReceipt
    // =========================================================================

    group('processPacket (UDP) - ReadReceipt', () {
      test('delivers read receipt via onReadReceiptReceived', () async {
        stubSession();

        String? receivedId;
        router.onReadReceiptReceived = (id) => receivedId = id;

        const messageId = 'rr-msg-1';
        final p = securePacket(
          type: PacketType.readReceipt,
          payload: Uint8List.fromList(messageId.codeUnits),
        );

        await router.processPacket(
          p,
          transport: PeerTransport.udp,
          udpPeerId: 'peer-def',
        );

        expect(receivedId, equals(messageId));
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
