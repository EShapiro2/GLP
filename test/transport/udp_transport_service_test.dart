import 'dart:async';
import 'dart:io';
import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:cryptography/cryptography.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs.dart';
import 'package:grassroots_networking/src/transport/udp_transport_service.dart';
import 'package:grassroots_networking/src/transport/transport_service.dart';
import 'package:grassroots_networking/src/models/identity.dart';
import 'package:grassroots_networking/src/protocol/protocol_handler.dart';
import 'package:grassroots_networking/src/store/store.dart';

import '../helpers/sodium_test_bootstrap.dart';

/// Create a test identity
Future<GrassrootsIdentity> _createTestIdentity(String nickname) async {
  final algorithm = Ed25519();
  final keyPair = await algorithm.newKeyPair();
  return GrassrootsIdentity.create(keyPair: keyPair, nickname: nickname);
}

/// Create a minimal Redux store for testing
Store<AppState> _createTestStore() {
  return Store<AppState>(
    appReducer,
    initialState: AppState.initial,
  );
}

InternetAddress _loopbackForFamily(InternetAddressType family) =>
    family == InternetAddressType.IPv6
        ? InternetAddress.loopbackIPv6
        : InternetAddress.loopbackIPv4;

InternetAddress _loopbackFor(UdpTransportService service) =>
    _loopbackForFamily(service.activeAddressType ?? InternetAddressType.IPv6);

void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  late Sodium sodium;
  setUpAll(() async {
    sodium = await initTestSodium();
  });

  group('UdpTransportService', () {
    late GrassrootsIdentity identity;
    late Store<AppState> store;
    late ProtocolHandler protocolHandler;

    setUp(() async {
      identity = await _createTestIdentity('TestPeer');
      store = _createTestStore();
      protocolHandler = ProtocolHandler(identity: identity, sodium: sodium);
    });

    // =========================================================================
    // Lifecycle tests
    // =========================================================================
    group('lifecycle', () {
      test('starts in uninitialized state', () {
        final service = UdpTransportService(
          identity: identity,
          store: store,
          protocolHandler: protocolHandler,
          connectHandshakeTimeout: const Duration(milliseconds: 100),
        );
        expect(service.state, equals(TransportState.uninitialized));
        expect(service.rawSocket, isNull);
        expect(service.localPort, isNull);
        expect(service.isMultiplexerActive, isFalse);
      });

      test('initialize binds socket and transitions to ready', () async {
        final service = UdpTransportService(
          identity: identity,
          store: store,
          protocolHandler: protocolHandler,
          connectHandshakeTimeout: const Duration(milliseconds: 100),
        );

        final result = await service.initialize();

        expect(result, isTrue);
        expect(service.state, equals(TransportState.ready));
        expect(service.rawSocket, isNotNull);
        expect(service.localPort, isNotNull);
        expect(service.localPort, greaterThan(0));
        expect(service.activeAddressType, isNotNull);
        expect(service.isMultiplexerActive, isFalse);

        await service.dispose();
      });

      test('initialize supports IPv4 and IPv6 when sockets bind', () async {
        final service = UdpTransportService(
          identity: identity,
          store: store,
          protocolHandler: protocolHandler,
        );
        await service.initialize();

        expect(service.activeAddressTypes, contains(InternetAddressType.IPv6));
        expect(service.activeAddressTypes, contains(InternetAddressType.IPv4));
        expect(service.canDialAddress(InternetAddress.loopbackIPv4), isTrue);
        expect(service.canDialAddress(InternetAddress.loopbackIPv6), isTrue);

        await service.dispose();
      });

      test('initialize only runs once', () async {
        final service = UdpTransportService(
          identity: identity,
          store: store,
          protocolHandler: protocolHandler,
        );

        await service.initialize();
        final port1 = service.localPort;

        // Second initialize should be a no-op, return true (already usable)
        final result = await service.initialize();
        expect(result, isTrue);
        expect(service.localPort, equals(port1));

        await service.dispose();
      });

      test('startMultiplexer transitions to active', () async {
        final service = UdpTransportService(
          identity: identity,
          store: store,
          protocolHandler: protocolHandler,
        );

        await service.initialize();
        service.startMultiplexer();

        expect(service.state, equals(TransportState.active));
        expect(service.isMultiplexerActive, isTrue);

        await service.dispose();
      });

      test('startMultiplexer fails without initialize', () async {
        final service = UdpTransportService(
          identity: identity,
          store: store,
          protocolHandler: protocolHandler,
        );

        // Should log error, not crash
        service.startMultiplexer();
        expect(service.isMultiplexerActive, isFalse);
      });

      test('startMultiplexer only runs once', () async {
        final service = UdpTransportService(
          identity: identity,
          store: store,
          protocolHandler: protocolHandler,
        );

        await service.initialize();
        service.startMultiplexer();
        service.startMultiplexer(); // Should be no-op

        expect(service.isMultiplexerActive, isTrue);

        await service.dispose();
      });

      test('stop transitions from active to ready', () async {
        final service = UdpTransportService(
          identity: identity,
          store: store,
          protocolHandler: protocolHandler,
        );

        await service.initialize();
        service.startMultiplexer();
        expect(service.state, equals(TransportState.active));

        await service.stop();
        expect(service.state, equals(TransportState.ready));
        expect(service.isMultiplexerActive, isFalse);

        // Raw socket should still be available (for re-use)
        expect(service.rawSocket, isNotNull);

        await service.dispose();
      });

      test('dispose cleans up everything', () async {
        final service = UdpTransportService(
          identity: identity,
          store: store,
          protocolHandler: protocolHandler,
        );

        await service.initialize();
        service.startMultiplexer();
        await service.dispose();

        expect(service.state, equals(TransportState.disposed));
        expect(service.rawSocket, isNull);
        expect(service.localPort, isNull);
        expect(service.isMultiplexerActive, isFalse);
      });

      test('start() calls startMultiplexer if not already started', () async {
        final service = UdpTransportService(
          identity: identity,
          store: store,
          protocolHandler: protocolHandler,
        );

        await service.initialize();
        expect(service.isMultiplexerActive, isFalse);

        await service.start();
        expect(service.isMultiplexerActive, isTrue);
        expect(service.state, equals(TransportState.active));

        await service.dispose();
      });
    });

    // =========================================================================
    // Connection state tests
    // =========================================================================
    group('connection state', () {
      test('initially has zero connected peers', () {
        final service = UdpTransportService(
          identity: identity,
          store: store,
          protocolHandler: protocolHandler,
        );
        expect(service.connectedCount, equals(0));
      });

      test('sendToPeer fails when not connected', () async {
        final service = UdpTransportService(
          identity: identity,
          store: store,
          protocolHandler: protocolHandler,
        );
        await service.initialize();

        final result = await service.sendToPeer(
          'deadbeef' * 8, // 64-char hex pubkey
          Uint8List.fromList([1, 2, 3]),
        );
        expect(result, isFalse);

        await service.dispose();
      });

      test('connectToPeer fails without multiplexer', () async {
        final service = UdpTransportService(
          identity: identity,
          store: store,
          protocolHandler: protocolHandler,
        );
        await service.initialize();
        // Don't call startMultiplexer

        final result = await service.connectToPeer(
          'deadbeef' * 8,
          _loopbackFor(service),
          12345,
        );
        expect(result, isFalse);

        await service.dispose();
      });

      test('connectToPeer returns false for unreachable IPv4 endpoint',
          () async {
        final service = UdpTransportService(
          identity: identity,
          store: store,
          protocolHandler: protocolHandler,
          connectHandshakeTimeout: const Duration(milliseconds: 100),
        );
        await service.initialize();
        service.startMultiplexer();

        final result = await service.connectToPeer(
          'deadbeef' * 8,
          InternetAddress('203.0.113.1'),
          65535,
        );

        expect(result, isFalse);
        expect(service.connectedCount, equals(0));
        expect(
          service.lastConnectFailureKind,
          isNotNull,
        );

        await service.dispose();
      });
    });

    // =========================================================================
    // Raw socket access for hole-punching
    // =========================================================================
    group('raw socket access', () {
      test('raw socket is available after initialize for punch packets',
          () async {
        final service = UdpTransportService(
          identity: identity,
          store: store,
          protocolHandler: protocolHandler,
        );

        await service.initialize();

        // Raw socket should be usable for sending punch packets
        expect(service.rawSocket, isNotNull);
        expect(service.localPort, isNotNull);

        // We should be able to send raw bytes on this socket
        // (This is how hole-punch service will use it)
        final sent = service.rawSocket!.send(
          Uint8List.fromList([0x47, 0x52, 0x53, 0x50]), // "GRSP" magic
          _loopbackFor(service),
          service.localPort!, // send to self just to verify send works
        );
        expect(sent, greaterThan(0));

        await service.dispose();
      });

      test('raw socket is available even after multiplexer starts', () async {
        final service = UdpTransportService(
          identity: identity,
          store: store,
          protocolHandler: protocolHandler,
        );

        await service.initialize();
        service.startMultiplexer();

        // Raw socket still available for send (multiplexer only owns reads)
        expect(service.rawSocket, isNotNull);
        final sent = service.rawSocket!.send(
          Uint8List.fromList([0x47, 0x52, 0x53, 0x50]),
          _loopbackFor(service),
          service.localPort!,
        );
        expect(sent, greaterThan(0));

        await service.dispose();
      });
    });

    // =========================================================================
    // Two-peer integration test (loopback)
    // =========================================================================
    group('peer-to-peer integration', () {
      test('two services can connect and exchange data on loopback', () async {
        // Create two services (Alice and Bob) on localhost
        final aliceIdentity = await _createTestIdentity('Alice');
        final bobIdentity = await _createTestIdentity('Bob');
        final aliceStore = _createTestStore();
        final bobStore = _createTestStore();
        final aliceProto =
            ProtocolHandler(identity: aliceIdentity, sodium: sodium);
        final bobProto = ProtocolHandler(identity: bobIdentity, sodium: sodium);

        final alice = UdpTransportService(
          identity: aliceIdentity,
          store: aliceStore,
          protocolHandler: aliceProto,
        );
        final bob = UdpTransportService(
          identity: bobIdentity,
          store: bobStore,
          protocolHandler: bobProto,
        );

        // Initialize both
        await alice.initialize();
        await bob.initialize();
        expect(alice.activeAddressType, equals(bob.activeAddressType));
        final loopback = _loopbackForFamily(bob.activeAddressType!);

        // Start multiplexers
        alice.startMultiplexer();
        bob.startMultiplexer();

        // Track received data on Bob's side
        final bobReceived = Completer<Uint8List>();
        bob.onUdpDataReceived = (peerId, data) {
          if (!bobReceived.isCompleted) {
            bobReceived.complete(data);
          }
        };

        // Alice connects to Bob
        final bobPubkeyHex = bobIdentity.publicKey
            .map((b) => b.toRadixString(16).padLeft(2, '0'))
            .join();

        final connected = await alice.connectToPeer(
          bobPubkeyHex,
          loopback,
          bob.localPort!,
        );

        expect(connected, isTrue);
        expect(alice.connectedCount, equals(1));

        // Alice sends data to Bob. The UDP transport's receive path now
        // frames by GrassrootsPacket boundary (using the header's
        // payload-length field), so we send a real serialized packet
        // instead of an arbitrary byte buffer.
        final testPayload = Uint8List.fromList(List.generate(100, (i) => i));
        final packet = aliceProto.createMessagePacket(
          payload: testPayload,
          messageId: '550e8400-e29b-41d4-a716-446655440000',
        );
        final testData = packet.serialize();
        final sent = await alice.sendToPeer(bobPubkeyHex, testData);
        expect(sent, isTrue);

        // Bob should receive the framed packet.
        final received = await bobReceived.future.timeout(
          const Duration(seconds: 5),
          onTimeout: () => throw TimeoutException('Bob did not receive data'),
        );
        expect(received, equals(testData));

        // Cleanup
        await alice.dispose();
        await bob.dispose();
      }, timeout: const Timeout(Duration(seconds: 10)));

      test('two services can connect and exchange data over IPv4 loopback',
          () async {
        final aliceIdentity = await _createTestIdentity('Alice');
        final bobIdentity = await _createTestIdentity('Bob');
        final aliceStore = _createTestStore();
        final bobStore = _createTestStore();
        final aliceProto =
            ProtocolHandler(identity: aliceIdentity, sodium: sodium);
        final bobProto = ProtocolHandler(identity: bobIdentity, sodium: sodium);

        final alice = UdpTransportService(
          identity: aliceIdentity,
          store: aliceStore,
          protocolHandler: aliceProto,
        );
        final bob = UdpTransportService(
          identity: bobIdentity,
          store: bobStore,
          protocolHandler: bobProto,
        );

        await alice.initialize();
        await bob.initialize();
        alice.startMultiplexer();
        bob.startMultiplexer();

        final bobReceived = Completer<Uint8List>();
        bob.onUdpDataReceived = (peerId, data) {
          if (!bobReceived.isCompleted) {
            bobReceived.complete(data);
          }
        };

        final bobPubkeyHex = bobIdentity.publicKey
            .map((b) => b.toRadixString(16).padLeft(2, '0'))
            .join();

        final connected = await alice.connectToPeer(
          bobPubkeyHex,
          InternetAddress.loopbackIPv4,
          bob.localPortForAddressType(InternetAddressType.IPv4)!,
        );

        expect(connected, isTrue);
        // Send a real serialized packet so the receive-side framer can
        // slice it cleanly (the transport now frames the UDX byte
        // stream into GrassrootsPacket boundaries via the header's
        // payload-length field).
        final packet = aliceProto.createMessagePacket(
          payload: Uint8List.fromList([4, 3, 2, 1]),
          messageId: '6f9619ff-8b86-4d01-b42d-00cf4fc964ff',
        );
        final testData = packet.serialize();
        expect(await alice.sendToPeer(bobPubkeyHex, testData), isTrue);
        expect(
          await bobReceived.future.timeout(const Duration(seconds: 5)),
          equals(testData),
        );

        await alice.dispose();
        await bob.dispose();
      }, timeout: const Timeout(Duration(seconds: 10)));

      test('disconnectFromPeer removes the connection', () async {
        final aliceIdentity = await _createTestIdentity('Alice');
        final bobIdentity = await _createTestIdentity('Bob');
        final aliceStore = _createTestStore();
        final bobStore = _createTestStore();
        final aliceProto =
            ProtocolHandler(identity: aliceIdentity, sodium: sodium);
        final bobProto = ProtocolHandler(identity: bobIdentity, sodium: sodium);

        final alice = UdpTransportService(
          identity: aliceIdentity,
          store: aliceStore,
          protocolHandler: aliceProto,
        );
        final bob = UdpTransportService(
          identity: bobIdentity,
          store: bobStore,
          protocolHandler: bobProto,
        );

        await alice.initialize();
        await bob.initialize();
        expect(alice.activeAddressType, equals(bob.activeAddressType));
        final loopback = _loopbackForFamily(bob.activeAddressType!);
        alice.startMultiplexer();
        bob.startMultiplexer();

        final bobPubkeyHex = bobIdentity.publicKey
            .map((b) => b.toRadixString(16).padLeft(2, '0'))
            .join();

        await alice.connectToPeer(
          bobPubkeyHex,
          loopback,
          bob.localPort!,
        );
        expect(alice.connectedCount, equals(1));

        await alice.disconnectFromPeer(bobPubkeyHex);
        expect(alice.connectedCount, equals(0));

        // Sending should now fail
        final sent =
            await alice.sendToPeer(bobPubkeyHex, Uint8List.fromList([1, 2, 3]));
        expect(sent, isFalse);

        await alice.dispose();
        await bob.dispose();
      }, timeout: const Timeout(Duration(seconds: 10)));
    });

    // =========================================================================
    // Hole-punch flow simulation
    // =========================================================================
    group('hole-punch flow', () {
      test('raw socket punch then multiplexer start works', () async {
        // Simulates the hole-punch flow:
        // 1. Bind raw socket
        // 2. Send punch packets (raw UDP)
        // 3. Receive punch response (raw UDP)
        // 4. Start multiplexer on same socket
        // 5. Create UDX stream

        final aliceIdentity = await _createTestIdentity('Alice');
        final bobIdentity = await _createTestIdentity('Bob');
        final aliceStore = _createTestStore();
        final bobStore = _createTestStore();
        final aliceProto =
            ProtocolHandler(identity: aliceIdentity, sodium: sodium);
        final bobProto = ProtocolHandler(identity: bobIdentity, sodium: sodium);

        final alice = UdpTransportService(
          identity: aliceIdentity,
          store: aliceStore,
          protocolHandler: aliceProto,
        );
        final bob = UdpTransportService(
          identity: bobIdentity,
          store: bobStore,
          protocolHandler: bobProto,
        );

        // Phase 1: Both bind sockets (no multiplexer yet)
        await alice.initialize();
        await bob.initialize();
        expect(alice.activeAddressType, equals(bob.activeAddressType));
        final loopback = _loopbackForFamily(bob.activeAddressType!);

        // Phase 2: Exchange punch packets on raw sockets
        final punchData =
            Uint8List.fromList([0x47, 0x52, 0x53, 0x50]); // "GRSP"

        // Alice sends punch to Bob's port
        alice.rawSocket!.send(punchData, loopback, bob.localPort!);
        // Bob sends punch to Alice's port
        bob.rawSocket!.send(punchData, loopback, alice.localPort!);

        // Small delay for packets to arrive
        await Future.delayed(const Duration(milliseconds: 100));

        // Phase 3: Both start multiplexers (takes over reads)
        alice.startMultiplexer();
        bob.startMultiplexer();

        // Phase 4: Alice connects to Bob via UDX
        final bobPubkeyHex = bobIdentity.publicKey
            .map((b) => b.toRadixString(16).padLeft(2, '0'))
            .join();

        final bobReceived = Completer<Uint8List>();
        bob.onUdpDataReceived = (peerId, data) {
          if (!bobReceived.isCompleted) {
            bobReceived.complete(data);
          }
        };

        final connected = await alice.connectToPeer(
          bobPubkeyHex,
          loopback,
          bob.localPort!,
        );
        expect(connected, isTrue);

        // Phase 5: Send a real serialized packet so the receive-side
        // framer (which slices by GrassrootsPacket header length) can
        // emit it cleanly.
        final packet = aliceProto.createMessagePacket(
          payload: Uint8List.fromList([1, 2, 3, 4, 5]),
          messageId: '7c9e6679-7425-40de-944b-e07fc1f90ae7',
        );
        final testData = packet.serialize();
        await alice.sendToPeer(bobPubkeyHex, testData);

        final received = await bobReceived.future.timeout(
          const Duration(seconds: 5),
          onTimeout: () => throw TimeoutException('Bob did not receive data'),
        );
        expect(received, equals(testData));

        await alice.dispose();
        await bob.dispose();
      }, timeout: const Timeout(Duration(seconds: 10)));
    });

    // =========================================================================
    // State stream tests
    // =========================================================================
    group('state transitions', () {
      test('emits state changes on stream', () async {
        final service = UdpTransportService(
          identity: identity,
          store: store,
          protocolHandler: protocolHandler,
        );

        // Note: stateStream is not on the abstract interface, so we test via
        // the public state getter at key points.

        expect(service.state, equals(TransportState.uninitialized));

        await service.initialize();
        expect(service.state, equals(TransportState.ready));

        service.startMultiplexer();
        expect(service.state, equals(TransportState.active));

        await service.stop();
        expect(service.state, equals(TransportState.ready));

        await service.dispose();
        expect(service.state, equals(TransportState.disposed));
      });
    });
  });
}
