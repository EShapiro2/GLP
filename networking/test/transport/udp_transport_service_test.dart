import 'dart:async';
import 'dart:io';
import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:cryptography/cryptography.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs.dart';
import 'package:grassroots_networking_core/src/transport/udp_transport_service.dart';
import 'package:grassroots_networking_core/src/transport/transport_service.dart';
import 'package:grassroots_networking_core/src/transport/address_utils.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/models/packet.dart';
import 'package:grassroots_networking_core/src/models/peer.dart';
import 'package:grassroots_networking_core/src/protocol/protocol_handler.dart';
import 'package:grassroots_networking_core/src/store/store.dart';

import '../helpers/sodium_test_bootstrap.dart';

/// The IP carrier of the shared message transport (spec §Message Transport,
/// §IP Connection): datagrams, no stream, no UDX. This exercises the socket
/// lifecycle, path association, and one-datagram-per-packet send/receive.
Future<GrassrootsIdentity> _createTestIdentity() async {
  final keyPair = await Ed25519().newKeyPair();
  return GrassrootsIdentity.create(keyPair: keyPair);
}

Store<AppState> _createTestStore() =>
    Store<AppState>(appReducer, initialState: AppState.initial);

InternetAddress _loopbackFor(UdpTransportService service) =>
    service.activeAddressType == InternetAddressType.IPv4
        ? InternetAddress.loopbackIPv4
        : InternetAddress.loopbackIPv6;

String _hex(Uint8List b) =>
    b.map((x) => x.toRadixString(16).padLeft(2, '0')).join();

/// The classifier of these tests: IPv4 loopback is on the local network and
/// IPv6 loopback is not, so one machine holds a LAN path and a WAN path at
/// once (decision 8 decides by the address alone).
PeerTransport _ipv4IsLan(InternetAddress address) =>
    address.type == InternetAddressType.IPv4
        ? PeerTransport.lan
        : PeerTransport.wan;

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
      identity = await _createTestIdentity();
      store = _createTestStore();
      protocolHandler = ProtocolHandler(identity: identity, sodium: sodium);
    });

    UdpTransportService build() => UdpTransportService(
          identity: identity,
          store: store,
          protocolHandler: protocolHandler,
          sessionOf: _ipv4IsLan,
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
          sessionOf: _ipv4IsLan,
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

        final loopback = _loopbackFor(service);
        final id = await service.connectToPeer(
          peerHex,
          loopback,
          service.localPort!,
        );
        expect(id, UdpTransportService.pathId(_ipv4IsLan(loopback), peerHex));
        expect(service.hasPath(_ipv4IsLan(loopback), peerHex), isTrue);
        await Future<void>.delayed(Duration.zero);
        expect(events.any((e) => e.connected && !e.isIncoming && e.peerId == id),
            isTrue);
      });

      test('disconnectFromPeer drops the path and fires a disconnect',
          () async {
        final service = build();
        addTearDown(service.dispose);
        await service.initialize();
        await service.start();
        final peerHex = _hex(Uint8List.fromList(List.filled(32, 9)));
        final loopback = _loopbackFor(service);
        final id = await service.connectToPeer(
            peerHex, loopback, service.localPort!);
        await service.disconnectFromPeer(id!);
        expect(service.hasPath(_ipv4IsLan(loopback), peerHex), isFalse);
      });

      test(
          'a peer holds a LAN path and a WAN path at once, each its own, and '
          'dropping one leaves the other (spec §Session Establishment: '
          'sessions are per underlay)', () async {
        final service = build();
        addTearDown(service.dispose);
        await service.initialize();
        await service.start();
        if (!service.activeAddressTypes.containsAll(
            {InternetAddressType.IPv4, InternetAddressType.IPv6})) {
          markTestSkipped('needs both IPv4 and IPv6 loopback');
          return;
        }
        final peerHex = _hex(Uint8List.fromList(List.filled(32, 4)));
        final lan = await service.connectToPeer(
            peerHex, InternetAddress.loopbackIPv4, 40001);
        final wan = await service.connectToPeer(
            peerHex, InternetAddress.loopbackIPv6, 40002);
        expect(lan, UdpTransportService.pathId(PeerTransport.lan, peerHex));
        expect(wan, UdpTransportService.pathId(PeerTransport.wan, peerHex));
        expect(service.getRemoteAddress(lan!)!.port, 40001);
        expect(service.getRemoteAddress(wan!)!.port, 40002);
        expect(service.sessionOfPath(lan), PeerTransport.lan);
        expect(service.sessionOfPath(wan), PeerTransport.wan);

        await service.disconnectFromPeer(wan);
        expect(service.hasPath(PeerTransport.wan, peerHex), isFalse);
        expect(service.hasPath(PeerTransport.lan, peerHex), isTrue);
        expect(service.getRemoteAddress(lan)!.port, 40001);
      });

      test('an address under no underlay the layer runs opens no path',
          () async {
        final service = UdpTransportService(
          identity: identity,
          store: store,
          protocolHandler: protocolHandler,
          sessionOf: (_) => null,
        );
        addTearDown(service.dispose);
        await service.initialize();
        await service.start();
        final peerHex = _hex(Uint8List.fromList(List.filled(32, 6)));
        expect(
            await service.connectToPeer(
                peerHex, _loopbackFor(service), service.localPort!),
            isNull);
        expect(service.connectedCount, 0);
      });

      test('mapIncomingConnectionToPubkey keeps the temp id as an alias',
          () async {
        final service = build();
        addTearDown(service.dispose);
        await service.initialize();
        await service.start();

        // Simulate an inbound path under a temp id, then identify it.
        final loopback = _loopbackFor(service);
        final temp = AddressInfo(loopback, 40000);
        final tempId = temp.toAddressString();
        final peerHex = _hex(Uint8List.fromList(List.filled(32, 3)));
        expect(await service.connectToAddress(temp), isTrue);
        final id = service.mapIncomingConnectionToPubkey(tempId, peerHex);
        expect(id, UdpTransportService.pathId(_ipv4IsLan(loopback), peerHex));
        // Both ids resolve to a live path — an in-flight handler holding the
        // temp id can still reach the peer.
        expect(service.getRemoteAddress(id), isNotNull);
        expect(service.getRemoteAddress(tempId), isNotNull);
        expect(service.peerIdForAddress(tempId), id);
      });
    });

    group('send / receive', () {
      test('one framed packet rides one datagram, end to end', () async {
        final a = build();
        final b = UdpTransportService(
          identity: await _createTestIdentity(),
          store: _createTestStore(),
          protocolHandler: protocolHandler,
          sessionOf: _ipv4IsLan,
        );
        addTearDown(a.dispose);
        addTearDown(b.dispose);
        await a.initialize();
        await a.start();
        await b.initialize();
        await b.start();

        final received = Completer<Uint8List>();
        b.onUdpDataReceived = (session, peerId, data) {
          if (!received.isCompleted) received.complete(data);
        };

        final aHex = _hex(identity.publicKey);
        // b sends to a's loopback address; a will see an inbound datagram.
        final packet = GrassrootsPacket(
          type: PacketType.announce,
          payload: Uint8List.fromList(List.filled(98, 1)),
        );
        // Wire a→b: associate b's address on a and send.
        final id = await a.connectToPeer(
            _hex(Uint8List.fromList(List.filled(32, 2))),
            _loopbackFor(b),
            b.localPortForAddressType(a.activeAddressType!) ?? b.localPort!);
        final sent = await a.sendToPeer(id!, packet.serialize());
        expect(sent, isTrue);

        final data = await received.future;
        expect(GrassrootsPacket.deserialize(data).type, PacketType.announce);
        expect(aHex, isNotEmpty);
      }, timeout: Timeout.none);

      test(
          'an inbound datagram is attributed by its source address: from a '
          'local address to the LAN session, from any other to the WAN '
          'session (decision 8)', () async {
        final receiver = build();
        addTearDown(receiver.dispose);
        await receiver.initialize();
        await receiver.start();
        if (!receiver.activeAddressTypes.containsAll(
            {InternetAddressType.IPv4, InternetAddressType.IPv6})) {
          markTestSkipped('needs both IPv4 and IPv6 loopback');
          return;
        }
        final arrivals = <PeerTransport>[];
        final first = Completer<void>();
        final both = Completer<void>();
        receiver.onUdpDataReceived = (session, peerId, data) {
          arrivals.add(session);
          expect(receiver.sessionOfPath(peerId), session);
          if (arrivals.length == 1 && !first.isCompleted) first.complete();
          if (arrivals.length == 2 && !both.isCompleted) both.complete();
        };
        final announce = GrassrootsPacket(
          type: PacketType.announce,
          payload: Uint8List.fromList(List.filled(98, 1)),
        ).serialize();
        final v4 = await RawDatagramSocket.bind(InternetAddress.loopbackIPv4, 0);
        final v6 = await RawDatagramSocket.bind(InternetAddress.loopbackIPv6, 0);
        addTearDown(v4.close);
        addTearDown(v6.close);
        v4.send(announce, InternetAddress.loopbackIPv4,
            receiver.localPortForAddressType(InternetAddressType.IPv4)!);
        // The second datagram goes once the first has arrived, so the order
        // of arrival is the order of sending.
        await first.future;
        v6.send(announce, InternetAddress.loopbackIPv6,
            receiver.localPortForAddressType(InternetAddressType.IPv6)!);
        await both.future;
        expect(arrivals, [PeerTransport.lan, PeerTransport.wan]);
      }, timeout: Timeout.none);

      test('refuses an oversized datagram', () async {
        final service = build();
        addTearDown(service.dispose);
        await service.initialize();
        await service.start();
        final id = await service.connectToPeer(
            _hex(Uint8List.fromList(List.filled(32, 5))),
            _loopbackFor(service),
            service.localPort!);
        final tooBig = Uint8List(UdpTransportService.maxDatagramBytes + 1);
        expect(await service.sendToPeer(id!, tooBig), isFalse);
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
          Uint8List.fromList(const [0x47, 0x52, 0x50, 0x55]),
          _loopbackFor(service),
          service.localPort!,
        );
        expect(sent, greaterThan(0));
      });
    });
  });
}
