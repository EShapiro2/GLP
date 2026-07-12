import 'dart:typed_data';
import 'package:flutter_test/flutter_test.dart';
import 'package:redux/redux.dart';
import 'package:cryptography/cryptography.dart';
import 'package:sodium_libs/sodium_libs.dart';
import 'package:grassroots_networking/src/routing/message_router.dart';
import 'package:grassroots_networking/src/protocol/protocol_handler.dart';
import 'package:grassroots_networking/src/protocol/fragment_handler.dart';
import 'package:grassroots_networking/src/models/identity.dart';
import 'package:grassroots_networking/src/models/packet.dart';
import 'package:grassroots_networking/src/models/peer.dart';
import 'package:grassroots_networking/src/store/store.dart';

import '../helpers/sodium_test_bootstrap.dart';

/// Helper to create a self-signed ANNOUNCE payload:
/// [pubkey(32) + version(2) + nickLen(1) + nick + candidateCount(2)
///  + candidates + signature(64)]
///
/// The trailing Ed25519 signature covers every preceding byte and must be
/// produced by [signer], whose identity key matches [pubkey] — this is what
/// `ProtocolHandler.decodeAnnounce` verifies.
Uint8List buildAnnouncePayload({
  required Uint8List pubkey,
  required ProtocolHandler signer,
  String nickname = 'OtherPeer',
  String? address,
  Set<String> addressCandidates = const {},
}) {
  final nicknameBytes = Uint8List.fromList(nickname.codeUnits);
  final candidates = <String>{
    if (address != null && address.isNotEmpty) address,
    ...addressCandidates,
  };
  final buffer = BytesBuilder();

  buffer.add(pubkey);

  final versionBytes = ByteData(2);
  versionBytes.setUint16(0, 1, Endian.big);
  buffer.add(versionBytes.buffer.asUint8List());

  buffer.addByte(nicknameBytes.length);
  buffer.add(nicknameBytes);

  final candidateCountBytes = ByteData(2);
  candidateCountBytes.setUint16(0, candidates.length, Endian.big);
  buffer.add(candidateCountBytes.buffer.asUint8List());
  for (final candidate in candidates) {
    final candidateBytes = Uint8List.fromList(candidate.codeUnits);
    final lenBytes = ByteData(2);
    lenBytes.setUint16(0, candidateBytes.length, Endian.big);
    buffer.add(lenBytes.buffer.asUint8List());
    buffer.add(candidateBytes);
  }

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
        nickname: 'TestUser',
      );

      final otherKeyPair = await algorithm.newKeyPair();
      otherIdentity = await GrassrootsIdentity.create(
        keyPair: otherKeyPair,
        nickname: 'OtherPeer',
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

    tearDown(() {
      router.dispose();
    });

    /// Self-signed ANNOUNCE payload from the other peer's perspective.
    Uint8List otherAnnouncePayload({
      String nickname = 'OtherPeer',
      String? address,
      Set<String> addressCandidates = const {},
    }) {
      return buildAnnouncePayload(
        pubkey: otherPubkey,
        signer: otherProtocolHandler,
        nickname: nickname,
        address: address,
        addressCandidates: addressCandidates,
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

    /// Stub the Noise session layer: every session-encrypted packet
    /// "decrypts" back to its clear variant with [sender] (default: the
    /// other peer) as the session-authenticated sender identity.
    void stubSession({Uint8List? sender}) {
      final senderPubkey = sender ?? otherPubkey;
      router.decryptSessionPacket =
          (packet, transport, {String? peerId}) async {
        return (packet.copyWith(type: packet.type.clearVariant), senderPubkey);
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

    /// Build a session-encrypted MESSAGE whose payload opens with the
    /// 36-char [messageId] (the delivery identity, stable across retries)
    /// followed by [body] — the format `createMessagePacket` produces.
    GrassrootsPacket secureMessagePacket({
      required String messageId,
      Uint8List? body,
    }) {
      final clear = otherProtocolHandler.createMessagePacket(
        payload: body ?? Uint8List(0),
        messageId: messageId,
      );
      return clear.copyWith(type: PacketType.secureMessage);
    }

    // =========================================================================
    // Receive-Path Hardening
    // =========================================================================

    group('receive-path hardening', () {
      test('drops clear MESSAGE packets unconditionally', () async {
        bool anyCalled = false;
        bool decryptCalled = false;
        router.onMessageReceived = (_, __, ___, ____) => anyCalled = true;
        router.onAckRequested = (_, __, ___) => anyCalled = true;
        router.decryptSessionPacket =
            (packet, transport, {String? peerId}) async {
          decryptCalled = true;
          return (packet, otherPubkey);
        };

        final p = GrassrootsPacket(
          type: PacketType.message,
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

        final payload = otherAnnouncePayload(nickname: 'Alice');
        // Flip a nickname byte (offset 35 = pubkey 32 + version 2 + nickLen
        // 1). The record stays well-formed but its self-signature no longer
        // verifies.
        payload[35] ^= 0xFF;

        await router.processPacket(
          announcePacket(payload),
          transport: PeerTransport.bleDirect,
          rssi: -60,
        );

        expect(announced, isFalse);
        expect(store.state.peers.getPeerByPubkey(otherPubkey), isNull);
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
            type: PacketType.message,
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

        final p = announcePacket(otherAnnouncePayload(nickname: 'Alice'));

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
        final p = announcePacket(otherAnnouncePayload(nickname: 'Alice'));

        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          rssi: -55,
        );

        final peer = store.state.peers.getPeerByPubkey(otherPubkey);
        expect(peer, isNotNull);
        expect(peer!.nickname, equals('Alice'));
        expect(peer.rssi, equals(-55));
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

      test('includes udpAddress from ANNOUNCE payload', () async {
        final p = announcePacket(otherAnnouncePayload(
          address: '[2001:db8::a]:4001',
        ));

        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          rssi: -50,
        );

        final peer = store.state.peers.getPeerByPubkey(otherPubkey);
        expect(peer, isNotNull);
        expect(
          peer!.udpAddress,
          equals('[2001:db8::a]:4001'),
        );
      });

      test('preserves IPv4 udpAddress from ANNOUNCE payload', () async {
        final p = announcePacket(otherAnnouncePayload(
          address: '203.0.113.5:4001',
        ));

        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          rssi: -50,
        );

        final peer = store.state.peers.getPeerByPubkey(otherPubkey);
        expect(peer, isNotNull);
        expect(peer!.udpAddress, equals('203.0.113.5:4001'));
      });

      test('preserves UDP address candidates from ANNOUNCE payload', () async {
        final p = announcePacket(otherAnnouncePayload(
          address: '[2606:4700::1]:4001',
          addressCandidates: const {
            '[2606:4700::1]:4001',
            '198.51.100.5:4002',
          },
        ));

        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          rssi: -50,
        );

        final peer = store.state.peers.getPeerByPubkey(otherPubkey);
        expect(peer, isNotNull);
        expect(
          peer!.udpAddressCandidates,
          containsAll(const {
            '[2606:4700::1]:4001',
            '198.51.100.5:4002',
          }),
        );
      });

      test('fires onPeerAnnounced callback', () async {
        AnnounceData? receivedData;
        PeerTransport? receivedTransport;
        router.onPeerAnnounced =
            (data, transport, {bool isNew = false, String? udpPeerId}) {
          receivedData = data;
          receivedTransport = transport;
        };

        final p = announcePacket(otherAnnouncePayload(nickname: 'Bob'));

        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          rssi: -45,
        );

        expect(receivedData, isNotNull);
        expect(receivedData!.nickname, equals('Bob'));
        expect(receivedTransport, equals(PeerTransport.bleDirect));
      });

      test('always processes ANNOUNCE even if seen before (no dedup)',
          () async {
        final p = announcePacket(otherAnnouncePayload(nickname: 'Charlie'));

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

      test('drops MESSAGE whose payload lacks a messageId prefix', () async {
        stubSession();

        bool anyCalled = false;
        router.onMessageReceived = (_, __, ___, ____) => anyCalled = true;
        router.onAckRequested = (_, __, ___) => anyCalled = true;

        // Shorter than ProtocolHandler.messageIdLength (36 bytes).
        final p = securePacket(
          type: PacketType.message,
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
          nickname: 'Alice',
          protocolVersion: 1,
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

    group('processPacket - deduplication', () {
      test('delivers a re-sent MESSAGE only once (dedup by messageId)',
          () async {
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

        expect(messageCount, equals(1));
      });

      test('markSeen prevents delivery of a pre-marked messageId', () async {
        stubSession();

        int messageCount = 0;
        router.onMessageReceived = (_, __, ___, ____) => messageCount++;

        const messageId = '33333333-3333-3333-3333-333333333333';
        router.markSeen(messageId);

        final p = secureMessagePacket(
          messageId: messageId,
          body: Uint8List.fromList([1]),
        );

        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          rssi: -60,
        );

        expect(messageCount, equals(0));
      });

      test('isDuplicate returns correct results', () {
        expect(router.isDuplicate('never-seen'), isFalse);

        router.markSeen('seen-id');
        expect(router.isDuplicate('seen-id'), isTrue);
      });

      test(
          'duplicate MESSAGE re-ACKs without re-firing onMessageReceived. '
          'This is what stops the sender\'s watchdog from looping forever '
          'when its original ACK was lost.', () async {
        stubSession();

        int deliveries = 0;
        final ackRequests = <String>[];
        router.onMessageReceived = (_, __, ___, ____) => deliveries++;
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
            transport: PeerTransport.bleDirect, rssi: -60);
        await router.processPacket(p,
            transport: PeerTransport.bleDirect, rssi: -60);

        expect(deliveries, equals(1),
            reason: 'Recipient must not double-deliver to the app.');
        expect(ackRequests, hasLength(3),
            reason:
                'Recipient must re-ACK every duplicate so the sender can stop '
                'retrying.');
        expect(ackRequests, everyElement(messageId));
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

        final fragmented = fragmentHandler.fragment(payload: payload);

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

      test('NACK is silently ignored', () async {
        stubSession();

        bool anyCalled = false;
        router.onMessageReceived = (_, __, ___, ____) => anyCalled = true;
        router.onAckReceived = (_) => anyCalled = true;
        router.onReadReceiptReceived = (_) => anyCalled = true;

        final p = securePacket(
          type: PacketType.nack,
          payload: Uint8List(0),
        );

        await router.processPacket(
          p,
          transport: PeerTransport.bleDirect,
          rssi: -60,
        );

        expect(anyCalled, isFalse);
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
        final p = announcePacket(otherAnnouncePayload(
          nickname: 'UdpPeer',
          address: '[2001:db8::1]:4001',
        ));

        await router.processPacket(
          p,
          transport: PeerTransport.udp,
          udpPeerId: 'peer-123',
        );

        final peer = store.state.peers.getPeerByPubkey(otherPubkey);
        expect(peer, isNotNull);
        expect(peer!.nickname, equals('UdpPeer'));
        expect(peer.transport, equals(PeerTransport.udp));
        expect(
          peer.udpAddress,
          equals('[2001:db8::1]:4001'),
        );
      });

      test('identifies UDP connection with the verified announce pubkey',
          () async {
        Uint8List? identifiedPubkey;
        String? identifiedPeerId;
        router.onUdpPeerIdentified = (pubkey, udpPeerId) {
          identifiedPubkey = pubkey;
          identifiedPeerId = udpPeerId;
        };

        final p = announcePacket(otherAnnouncePayload(nickname: 'UdpPeer'));

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

        final p = announcePacket(otherAnnouncePayload(
          nickname: 'UdpPeer',
          address: '[2001:db8::1]:4001',
        ));

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
        final p = announcePacket(otherAnnouncePayload(nickname: 'NoPeer'));

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

        store.dispatch(FriendEstablishedAction(publicKey: otherPubkey));

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

    // =========================================================================
    // Dispose
    // =========================================================================

    group('dispose', () {
      test('cleans up without errors', () {
        expect(() => router.dispose(), returnsNormally);
      });

      test('double dispose is safe', () {
        router.dispose();
        expect(() => router.dispose(), returnsNormally);
      });
    });
  });
}
