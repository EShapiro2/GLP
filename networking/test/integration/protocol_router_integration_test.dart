import 'dart:typed_data';
import 'package:cryptography/cryptography.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs.dart';
import 'package:grassroots_networking/src/models/identity.dart';
import 'package:grassroots_networking/src/models/packet.dart';
import 'package:grassroots_networking/src/models/peer.dart';
import 'package:grassroots_networking/src/protocol/protocol_handler.dart';
import 'package:grassroots_networking/src/protocol/fragment_handler.dart';
import 'package:grassroots_networking/src/routing/message_router.dart';
import 'package:grassroots_networking/src/store/store.dart';

import '../helpers/sodium_test_bootstrap.dart';

/// Simulate the sender's session layer: a clear application packet travels
/// the wire as its session-encrypted variant.
GrassrootsPacket seal(GrassrootsPacket clear) =>
    clear.copyWith(type: clear.type.secureVariant);

void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  late Sodium sodium;
  setUpAll(() async {
    sodium = await initTestSodium();
  });

  late GrassrootsIdentity aliceIdentity;
  late GrassrootsIdentity bobIdentity;
  late ProtocolHandler aliceProtocol;
  late ProtocolHandler bobProtocol;
  late MessageRouter aliceRouter;
  late MessageRouter bobRouter;
  late Store<AppState> aliceStore;
  late Store<AppState> bobStore;

  setUp(() async {
    final algorithm = Ed25519();

    final aliceKeyPair = await algorithm.newKeyPair();
    aliceIdentity = await GrassrootsIdentity.create(
      keyPair: aliceKeyPair,
      nickname: 'Alice',
    );

    final bobKeyPair = await algorithm.newKeyPair();
    bobIdentity = await GrassrootsIdentity.create(
      keyPair: bobKeyPair,
      nickname: 'Bob',
    );

    aliceStore = Store<AppState>(appReducer, initialState: const AppState());
    bobStore = Store<AppState>(appReducer, initialState: const AppState());

    aliceProtocol = ProtocolHandler(identity: aliceIdentity, sodium: sodium);
    bobProtocol = ProtocolHandler(identity: bobIdentity, sodium: sodium);

    aliceRouter = MessageRouter(
      store: aliceStore,
      protocolHandler: aliceProtocol,
      fragmentHandler: FragmentHandler(),
    );

    bobRouter = MessageRouter(
      store: bobStore,
      protocolHandler: bobProtocol,
      fragmentHandler: FragmentHandler(),
    );

    // Simulated Noise sessions: each router "decrypts" a secure packet back
    // to its clear variant and reports the session-authenticated sender.
    bobRouter.decryptSessionPacket =
        (packet, transport, {String? peerId}) async => (
              packet.copyWith(type: packet.type.clearVariant),
              aliceIdentity.publicKey,
            );
    aliceRouter.decryptSessionPacket =
        (packet, transport, {String? peerId}) async => (
              packet.copyWith(type: packet.type.clearVariant),
              bobIdentity.publicKey,
            );
  });

  tearDown(() {
    aliceRouter.dispose();
    bobRouter.dispose();
  });

  group('BLE ANNOUNCE roundtrip', () {
    test('Alice creates ANNOUNCE, Bob receives and decodes it', () async {
      // Alice creates a self-signed ANNOUNCE payload
      final announcePayload = aliceProtocol.createAnnouncePayload();

      // Alice wraps it in a GrassrootsPacket (as BLE transport does).
      // The wire frame carries no identity — the payload is self-signed.
      final packet = GrassrootsPacket(
        type: PacketType.announce,
        payload: announcePayload,
      );

      // Bob's router processes the BLE packet
      AnnounceData? receivedAnnounce;
      PeerTransport? receivedTransport;
      bobRouter.onPeerAnnounced =
          (data, transport, {bool isNew = false, String? udpPeerId}) {
        receivedAnnounce = data;
        receivedTransport = transport;
      };

      await bobRouter.processPacket(
        packet,
        transport: PeerTransport.bleDirect,
        bleDeviceId: 'device-alice',
        rssi: -50,
      );

      // Verify Bob decoded (and signature-verified) Alice's announce
      expect(receivedAnnounce, isNotNull);
      expect(receivedAnnounce!.publicKey, equals(aliceIdentity.publicKey));
      expect(receivedAnnounce!.nickname, equals('Alice'));
      expect(
        receivedAnnounce!.protocolVersion,
        equals(ProtocolHandler.protocolVersion),
      );
      expect(receivedTransport, equals(PeerTransport.bleDirect));

      // Verify Bob's Redux store was updated
      final peerState =
          bobStore.state.peers.getPeerByPubkey(aliceIdentity.publicKey);
      expect(peerState, isNotNull);
      expect(peerState!.nickname, equals('Alice'));
      expect(peerState.transport, equals(PeerTransport.bleDirect));
      expect(peerState.rssi, equals(-50));
    });

    test('ANNOUNCE with UDP address roundtrips correctly', () async {
      const address = '[2001:db8::1]:4001';
      final announcePayload =
          aliceProtocol.createAnnouncePayload(address: address);

      final packet = GrassrootsPacket(
        type: PacketType.announce,
        payload: announcePayload,
      );

      AnnounceData? receivedAnnounce;
      bobRouter.onPeerAnnounced =
          (data, transport, {bool isNew = false, String? udpPeerId}) {
        receivedAnnounce = data;
      };

      await bobRouter.processPacket(
        packet,
        transport: PeerTransport.bleDirect,
        bleDeviceId: 'device-alice',
        rssi: -60,
      );

      expect(receivedAnnounce, isNotNull);
      expect(receivedAnnounce!.udpAddress, equals(address));

      // Verify UDP address stored in Redux
      final peerState =
          bobStore.state.peers.getPeerByPubkey(aliceIdentity.publicKey);
      expect(peerState!.udpAddress, equals(address));
    });

    test('ANNOUNCE with a tampered payload byte is dropped', () async {
      final announcePayload = aliceProtocol.createAnnouncePayload();
      // Flip a byte inside the signed region — the self-signature no longer
      // verifies, so Bob must not identify anyone from this record.
      announcePayload[35] ^= 0xFF;

      final packet = GrassrootsPacket(
        type: PacketType.announce,
        payload: announcePayload,
      );

      bool announced = false;
      bobRouter.onPeerAnnounced =
          (_, __, {bool isNew = false, String? udpPeerId}) {
        announced = true;
      };

      await bobRouter.processPacket(
        packet,
        transport: PeerTransport.bleDirect,
        bleDeviceId: 'device-alice',
        rssi: -50,
      );

      expect(announced, isFalse);
      expect(
        bobStore.state.peers.getPeerByPubkey(aliceIdentity.publicKey),
        isNull,
      );
    });
  });

  group('BLE MESSAGE roundtrip', () {
    test('Alice sends MESSAGE to Bob over a session, Bob receives it',
        () async {
      const messageId = '11111111-1111-1111-1111-111111111111';
      final messagePayload = Uint8List.fromList([10, 20, 30, 40, 50]);

      // Alice creates a clear message packet (payload = messageId + body);
      // her session layer seals it.
      final packet = aliceProtocol.createMessagePacket(
        payload: messagePayload,
        messageId: messageId,
      );

      // Bob's router processes it
      String? receivedId;
      Uint8List? receivedPayload;
      Uint8List? receivedSender;
      bobRouter.onMessageReceived = (id, sender, payload, _) {
        receivedId = id;
        receivedSender = sender;
        receivedPayload = payload;
      };

      await bobRouter.processPacket(
        seal(packet),
        transport: PeerTransport.bleDirect,
        rssi: -70,
      );

      expect(receivedId, equals(messageId));
      expect(receivedPayload, equals(messagePayload));
      expect(receivedSender, equals(aliceIdentity.publicKey));
    });

    test('clear MESSAGE packet (no session) is dropped', () async {
      final packet = aliceProtocol.createMessagePacket(
        payload: Uint8List.fromList([1, 2, 3]),
        messageId: '22222222-2222-2222-2222-222222222222',
      );

      bool messageReceived = false;
      bobRouter.onMessageReceived = (_, __, ___, ____) {
        messageReceived = true;
      };

      // Fed as-is — a clear application-data packet carries no
      // authentication and must be dropped.
      await bobRouter.processPacket(
        packet,
        transport: PeerTransport.bleDirect,
        rssi: -70,
      );

      expect(messageReceived, isFalse);
    });
  });

  group('BLE READ_RECEIPT roundtrip', () {
    test('Alice sends read receipt, Bob receives message ID', () async {
      const messageId = 'msg-12345678';

      final packet = aliceProtocol.createReadReceiptPacket(
        messageId: messageId,
      );

      String? receivedMessageId;
      bobRouter.onReadReceiptReceived = (id) {
        receivedMessageId = id;
      };

      await bobRouter.processPacket(
        seal(packet),
        transport: PeerTransport.bleDirect,
        rssi: -70,
      );

      expect(receivedMessageId, equals(messageId));
    });
  });

  group('UDP ANNOUNCE roundtrip', () {
    test('Alice creates UDP ANNOUNCE, Bob receives and decodes it', () async {
      final announcePayload = aliceProtocol.createAnnouncePayload();

      final packet = GrassrootsPacket(
        type: PacketType.announce,
        payload: announcePayload,
      );

      // Bob's router processes it
      AnnounceData? receivedAnnounce;
      PeerTransport? receivedTransport;
      bobRouter.onPeerAnnounced =
          (data, transport, {bool isNew = false, String? udpPeerId}) {
        receivedAnnounce = data;
        receivedTransport = transport;
      };

      await bobRouter.processPacket(
        packet,
        transport: PeerTransport.udp,
        udpPeerId: 'peer-alice-id',
      );

      expect(receivedAnnounce, isNotNull);
      expect(receivedAnnounce!.publicKey, equals(aliceIdentity.publicKey));
      expect(receivedAnnounce!.nickname, equals('Alice'));
      expect(
        receivedAnnounce!.protocolVersion,
        equals(ProtocolHandler.protocolVersion),
      );
      expect(receivedTransport, equals(PeerTransport.udp));

      // Verify Bob's Redux store was updated
      final peerState =
          bobStore.state.peers.getPeerByPubkey(aliceIdentity.publicKey);
      expect(peerState, isNotNull);
      expect(peerState!.nickname, equals('Alice'));
      expect(peerState.transport, equals(PeerTransport.udp));
    });

    test('UDP ANNOUNCE with address roundtrips correctly', () async {
      const address = '203.0.113.5:4001';
      final announcePayload =
          aliceProtocol.createAnnouncePayload(address: address);

      final packet = GrassrootsPacket(
        type: PacketType.announce,
        payload: announcePayload,
      );

      AnnounceData? receivedAnnounce;
      bobRouter.onPeerAnnounced =
          (data, transport, {bool isNew = false, String? udpPeerId}) {
        receivedAnnounce = data;
      };

      await bobRouter.processPacket(
        packet,
        transport: PeerTransport.udp,
        udpPeerId: 'peer-alice-id',
      );

      expect(receivedAnnounce!.udpAddress, equals(address));

      final peerState =
          bobStore.state.peers.getPeerByPubkey(aliceIdentity.publicKey);
      expect(peerState!.udpAddress, equals(address));
    });
  });

  group('UDP MESSAGE roundtrip', () {
    test('Alice sends UDP message, Bob receives it', () async {
      const messageId = '33333333-3333-3333-3333-333333333333';
      final messagePayload = Uint8List.fromList([99, 88, 77]);

      final packet = aliceProtocol.createMessagePacket(
        payload: messagePayload,
        messageId: messageId,
      );

      String? receivedId;
      Uint8List? receivedPayload;
      Uint8List? receivedSender;
      bobRouter.onMessageReceived = (id, sender, payload, _) {
        receivedId = id;
        receivedSender = sender;
        receivedPayload = payload;
      };

      await bobRouter.processPacket(
        seal(packet),
        transport: PeerTransport.udp,
        udpPeerId: 'peer-alice-id',
      );

      expect(receivedId, equals(messageId));
      expect(receivedPayload, equals(messagePayload));
      expect(receivedSender, equals(aliceIdentity.publicKey));
    });
  });

  group('UDP ACK roundtrip', () {
    test('Alice sends ACK, Bob receives message ID', () async {
      const messageId = 'ack12345';

      final packet = aliceProtocol.createAckPacket(messageId: messageId);

      String? receivedMessageId;
      bobRouter.onAckReceived = (id) {
        receivedMessageId = id;
      };

      await bobRouter.processPacket(
        seal(packet),
        transport: PeerTransport.udp,
        udpPeerId: 'peer-alice-id',
      );

      expect(receivedMessageId, equals(messageId));
    });
  });

  group('UDP READ_RECEIPT roundtrip', () {
    test('Alice sends read receipt via UDP, Bob receives it', () async {
      const messageId = 'rcpt1234';

      final packet = aliceProtocol.createReadReceiptPacket(
        messageId: messageId,
      );

      String? receivedMessageId;
      bobRouter.onReadReceiptReceived = (id) {
        receivedMessageId = id;
      };

      await bobRouter.processPacket(
        seal(packet),
        transport: PeerTransport.udp,
        udpPeerId: 'peer-alice-id',
      );

      expect(receivedMessageId, equals(messageId));
    });
  });

  group('BLE dedup across multiple packets', () {
    test('same message sent twice is only delivered once', () async {
      final messagePayload = Uint8List.fromList([1, 2, 3]);
      final packet = aliceProtocol.createMessagePacket(
        payload: messagePayload,
        messageId: '44444444-4444-4444-4444-444444444444',
      );

      int receiveCount = 0;
      bobRouter.onMessageReceived = (_, __, ___, ____) {
        receiveCount++;
      };

      await bobRouter.processPacket(
        seal(packet),
        transport: PeerTransport.bleDirect,
        rssi: -50,
      );
      await bobRouter.processPacket(
        seal(packet),
        transport: PeerTransport.bleDirect,
        rssi: -50,
      );

      expect(receiveCount, equals(1));
    });

    test('ANNOUNCE is always processed even if seen before', () async {
      final announcePayload = aliceProtocol.createAnnouncePayload();
      final packet = GrassrootsPacket(
        type: PacketType.announce,
        payload: announcePayload,
      );

      int announceCount = 0;
      bobRouter.onPeerAnnounced =
          (_, __, {bool isNew = false, String? udpPeerId}) {
        announceCount++;
      };

      await bobRouter.processPacket(
        packet,
        transport: PeerTransport.bleDirect,
        rssi: -50,
      );
      await bobRouter.processPacket(
        packet,
        transport: PeerTransport.bleDirect,
        rssi: -50,
      );

      expect(announceCount, equals(2));
    });
  });

  group('cross-transport peer discovery', () {
    test('peer announced via BLE then UDP updates transport info', () async {
      // First: Alice announces via BLE
      final bleAnnouncePayload = aliceProtocol.createAnnouncePayload();
      final blePacket = GrassrootsPacket(
        type: PacketType.announce,
        payload: bleAnnouncePayload,
      );

      await bobRouter.processPacket(
        blePacket,
        transport: PeerTransport.bleDirect,
        bleDeviceId: 'device-alice',
        bleRole: BleRole.central,
        rssi: -40,
      );

      var peer = bobStore.state.peers.getPeerByPubkey(aliceIdentity.publicKey);
      expect(peer, isNotNull);
      expect(peer!.transport, equals(PeerTransport.bleDirect));
      expect(peer.bleDeviceId, equals('device-alice'));

      // Then: Alice announces via UDP with address
      const udpAddr = '[2001:db8::a]:4001';
      final udpAnnouncePayload =
          aliceProtocol.createAnnouncePayload(address: udpAddr);
      final udpPacket = GrassrootsPacket(
        type: PacketType.announce,
        payload: udpAnnouncePayload,
      );

      await bobRouter.processPacket(
        udpPacket,
        transport: PeerTransport.udp,
        udpPeerId: 'peer-alice-udp',
      );

      // Peer should now have UDP address too
      peer = bobStore.state.peers.getPeerByPubkey(aliceIdentity.publicKey);
      expect(peer, isNotNull);
      expect(peer!.udpAddress, equals(udpAddr));
    });
  });

  group('bidirectional communication', () {
    test('Alice and Bob exchange announces and messages', () async {
      // Alice announces to Bob
      final aliceAnnouncePayload = aliceProtocol.createAnnouncePayload();
      final aliceAnnouncePacket = GrassrootsPacket(
        type: PacketType.announce,
        payload: aliceAnnouncePayload,
      );

      await bobRouter.processPacket(
        aliceAnnouncePacket,
        transport: PeerTransport.bleDirect,
        bleDeviceId: 'device-alice',
        rssi: -45,
      );

      // Bob announces to Alice
      final bobAnnouncePayload = bobProtocol.createAnnouncePayload();
      final bobAnnouncePacket = GrassrootsPacket(
        type: PacketType.announce,
        payload: bobAnnouncePayload,
      );

      await aliceRouter.processPacket(
        bobAnnouncePacket,
        transport: PeerTransport.bleDirect,
        bleDeviceId: 'device-bob',
        rssi: -55,
      );

      // Both stores should know about each other
      final bobInAliceStore =
          aliceStore.state.peers.getPeerByPubkey(bobIdentity.publicKey);
      final aliceInBobStore =
          bobStore.state.peers.getPeerByPubkey(aliceIdentity.publicKey);
      expect(bobInAliceStore, isNotNull);
      expect(aliceInBobStore, isNotNull);
      expect(bobInAliceStore!.nickname, equals('Bob'));
      expect(aliceInBobStore!.nickname, equals('Alice'));

      // Alice sends message to Bob
      final helloPayload = Uint8List.fromList('hello bob'.codeUnits);
      final aliceMsg = aliceProtocol.createMessagePacket(
        payload: helloPayload,
        messageId: '55555555-5555-5555-5555-555555555555',
      );

      Uint8List? bobReceived;
      bobRouter.onMessageReceived = (_, __, payload, ___) {
        bobReceived = payload;
      };
      await bobRouter.processPacket(
        seal(aliceMsg),
        transport: PeerTransport.bleDirect,
        rssi: -50,
      );
      expect(bobReceived, equals(helloPayload));

      // Bob sends message to Alice
      final replyPayload = Uint8List.fromList('hi alice'.codeUnits);
      final bobMsg = bobProtocol.createMessagePacket(
        payload: replyPayload,
        messageId: '66666666-6666-6666-6666-666666666666',
      );

      Uint8List? aliceReceived;
      aliceRouter.onMessageReceived = (_, __, payload, ___) {
        aliceReceived = payload;
      };
      await aliceRouter.processPacket(
        seal(bobMsg),
        transport: PeerTransport.bleDirect,
        rssi: -50,
      );
      expect(aliceReceived, equals(replyPayload));
    });
  });
}
