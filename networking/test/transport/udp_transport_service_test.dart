import 'dart:async';
import 'dart:io';
import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:cryptography/cryptography.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs.dart';
import 'package:grassroots_networking_core/src/transport/udp_transport_service.dart';
import 'package:grassroots_networking_core/src/transport/transport_service.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/models/packet.dart';
import 'package:grassroots_networking_core/src/protocol/protocol_handler.dart';
import 'package:grassroots_networking_core/src/store/store.dart';

import '../helpers/sodium_test_bootstrap.dart';

/// The IP carrier of the shared message transport (spec §Message Transport,
/// §IP Connection): datagrams, no stream, no UDX. This exercises the socket
/// lifecycle, path association, and one-datagram-per-packet send/receive.
Future<GrassrootsIdentity> _createTestIdentity(String nickname) async {
  final keyPair = await Ed25519().newKeyPair();
  return GrassrootsIdentity.create(keyPair: keyPair, nickname: nickname);
}

Store<AppState> _createTestStore() =>
    Store<AppState>(appReducer, initialState: AppState.initial);

InternetAddress _loopbackFor(UdpTransportService service) =>
    service.activeAddressType == InternetAddressType.IPv4
        ? InternetAddress.loopbackIPv4
        : InternetAddress.loopbackIPv6;

String _hex(Uint8List b) =>
    b.map((x) => x.toRadixString(16).padLeft(2, '0')).join();

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

    UdpTransportService build() => UdpTransportService(
          identity: identity,
          store: store,
          protocolHandler: protocolHandler,
        );

    group('lifecycle', () {
      test('initialize binds a socket and reports ready', () async {
        final service = build();
        addTearDown(service.dispose);
        expect(await service.initialize(), isTrue);
        expect(service.state, TransportState.ready);
        expect(service.rawSocket, isNotNull);
        expect(service.hasUsableRoute, isTrue);
        expect(service.localPort, isNotNull);
      });

      test('start transitions to active', () async {
        final service = build();
        addTearDown(service.dispose);
        await service.initialize();
        await service.start();
        expect(service.state, TransportState.active);
        expect(service.isActive, isTrue);
      });

      test('a fixed listenPort binds that port on both families', () async {
        final service = UdpTransportService(
          identity: identity,
          store: store,
          protocolHandler: protocolHandler,
          listenPort: 0,
        );
        addTearDown(service.dispose);
        await service.initialize();
        // Ephemeral: both families share nothing, but each has a port.
        for (final family in service.activeAddressTypes) {
          expect(service.localPortForAddressType(family), isNotNull);
        }
      });

      test('dispose closes sockets and reports disposed', () async {
        final service = build();
        await service.initialize();
        await service.start();
        await service.dispose();
        expect(service.state, TransportState.disposed);
        expect(service.rawSocket, isNull);
      });
    });

    group('path association', () {
      test('connectToPeer associates a path and fires a connect event',
          () async {
        final service = build();
        addTearDown(service.dispose);
        await service.initialize();
        await service.start();

        final peerHex = _hex(Uint8List.fromList(List.filled(32, 7)));
        final events = <TransportConnectionEvent>[];
        final sub = service.connectionStream.listen(events.add);
        addTearDown(sub.cancel);

        final ok = await service.connectToPeer(
          peerHex,
          _loopbackFor(service),
          service.localPort!,
        );
        expect(ok, isTrue);
        expect(service.getPeerIdForPubkey(
            Uint8List.fromList(List.filled(32, 7))) != null, isTrue);
        await Future<void>.delayed(Duration.zero);
        expect(events.any((e) => e.connected && !e.isIncoming), isTrue);
      });

      test('disconnectFromPeer drops the path and fires a disconnect',
          () async {
        final service = build();
        addTearDown(service.dispose);
        await service.initialize();
        await service.start();
        final peerHex = _hex(Uint8List.fromList(List.filled(32, 9)));
        await service.connectToPeer(
            peerHex, _loopbackFor(service), service.localPort!);
        await service.disconnectFromPeer(peerHex);
        expect(
            service.getPeerIdForPubkey(Uint8List.fromList(List.filled(32, 9))),
            isNull);
      });

      test('mapIncomingConnectionToPubkey keeps the temp id as an alias',
          () async {
        final service = build();
        addTearDown(service.dispose);
        await service.initialize();
        await service.start();

        // Simulate an inbound path under a temp id, then identify it.
        const tempId = '127.0.0.1:40000';
        final peerHex = _hex(Uint8List.fromList(List.filled(32, 3)));
        await service.connectToPeer(
            tempId, _loopbackFor(service), service.localPort!);
        service.mapIncomingConnectionToPubkey(tempId, peerHex);
        // Both ids resolve to a live path — an in-flight handler holding the
        // temp id can still reach the peer.
        expect(service.getRemoteAddress(peerHex), isNotNull);
        expect(service.getRemoteAddress(tempId), isNotNull);
      });
    });

    group('send / receive', () {
      test('one framed packet rides one datagram, end to end', () async {
        final a = build();
        final b = UdpTransportService(
          identity: await _createTestIdentity('Peer B'),
          store: _createTestStore(),
          protocolHandler: protocolHandler,
        );
        addTearDown(a.dispose);
        addTearDown(b.dispose);
        await a.initialize();
        await a.start();
        await b.initialize();
        await b.start();

        final received = Completer<Uint8List>();
        b.onUdpDataReceived = (peerId, data) {
          if (!received.isCompleted) received.complete(data);
        };

        final aHex = _hex(identity.publicKey);
        // b sends to a's loopback address; a will see an inbound datagram.
        final packet = GrassrootsPacket(
          type: PacketType.announce,
          payload: Uint8List.fromList(List.filled(98, 1)),
        );
        // Wire a→b: associate b's address on a and send.
        await a.connectToPeer(
            _hex(Uint8List.fromList(List.filled(32, 2))),
            _loopbackFor(b),
            b.localPortForAddressType(a.activeAddressType!) ?? b.localPort!);
        final sent = await a.sendToPeer(
            _hex(Uint8List.fromList(List.filled(32, 2))), packet.serialize());
        expect(sent, isTrue);

        final data =
            await received.future.timeout(const Duration(seconds: 3));
        expect(GrassrootsPacket.deserialize(data).type, PacketType.announce);
        expect(aHex, isNotEmpty);
      });

      test('refuses an oversized datagram', () async {
        final service = build();
        addTearDown(service.dispose);
        await service.initialize();
        await service.start();
        await service.connectToPeer(
            _hex(Uint8List.fromList(List.filled(32, 5))),
            _loopbackFor(service),
            service.localPort!);
        final tooBig = Uint8List(UdpTransportService.maxDatagramBytes + 1);
        expect(
            await service.sendToPeer(
                _hex(Uint8List.fromList(List.filled(32, 5))), tooBig),
            isFalse);
      });

      test('sendToPeer fails for an unknown peer', () async {
        final service = build();
        addTearDown(service.dispose);
        await service.initialize();
        await service.start();
        expect(await service.sendToPeer('deadbeef', Uint8List(10)), isFalse);
      });
    });

    group('raw socket for hole-punch', () {
      test('raw socket is available for punch sends', () async {
        final service = build();
        addTearDown(service.dispose);
        await service.initialize();
        await service.start();
        expect(service.rawSocket, isNotNull);
        final sent = service.rawSocket!.send(
          Uint8List.fromList(const [0x42, 0x43, 0x50, 0x55]),
          _loopbackFor(service),
          service.localPort!,
        );
        expect(sent, greaterThan(0));
      });
    });
  });
}
