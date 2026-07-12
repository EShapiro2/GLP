import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:redux/redux.dart';
import 'package:grassroots_networking/src/signaling/signaling_service.dart';
import 'package:grassroots_networking/src/signaling/signaling_codec.dart';
import 'package:grassroots_networking/src/models/peer.dart';
import 'package:grassroots_networking/src/store/store.dart';

// ===== Helpers =====

Uint8List _testPubkey(int seed) {
  final key = Uint8List(32);
  for (int i = 0; i < 32; i++) {
    key[i] = (seed + i) % 256;
  }
  return key;
}

String _pubkeyHex(Uint8List key) =>
    key.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

Store<AppState> _storeWithPeers(
  Map<String, PeerState> peers, {
  SettingsState settings = const SettingsState(),
}) {
  return Store<AppState>(
    appReducer,
    initialState: AppState(
      peers: PeersState(peers: peers),
      settings: settings,
    ),
  );
}

PeerState _knownPeer(Uint8List pubkey, {String? udpAddress}) {
  return PeerState(
    publicKey: pubkey,
    connectionState: PeerConnectionState.connected,
    udpAddress: udpAddress,
  );
}

void main() {
  final aliceKey = _testPubkey(1);
  final bobKey = _testPubkey(2);
  final anchorKey = _testPubkey(5);
  final anchor2Key = _testPubkey(6);
  final bobHex = _pubkeyHex(bobKey);
  final anchorHex = _pubkeyHex(anchorKey);
  final anchor2Hex = _pubkeyHex(anchor2Key);
  const anchorAddress = '[2001:db8:ffff::1]:9514';
  const anchor2Address = '198.51.100.44:9514';
  const reflectedIp = '2400::12';
  const directPunchIp = '2400::13';
  const codec = SignalingCodec();

  SettingsState settingsWithAnchors(List<(String, String)> anchors) {
    return SettingsState(
      rendezvousServers: [
        for (final (address, hex) in anchors)
          RendezvousServerSettings(address: address, pubkeyHex: hex),
      ],
    );
  }

  // ==========================================================================
  // Outgoing: fanOutReconnect / fanOutAvailable
  // ==========================================================================

  group('fanOutReconnect', () {
    test('sends RECONNECT to every configured rendezvous server', () async {
      final store = _storeWithPeers(
        {bobHex: _knownPeer(bobKey)},
        settings: settingsWithAnchors([
          (anchorAddress, anchorHex),
          (anchor2Address, anchor2Hex),
        ]),
      );
      final service = SignalingService(store: store);

      final sentTo = <String>[];
      service.sendSignaling = (recipient, payload) async {
        sentTo.add(_pubkeyHex(recipient));
        final msg = codec.decode(payload) as ReconnectMessage;
        expect(msg.initiatorPubkey, equals(aliceKey));
        expect(msg.peerPubkey, equals(bobKey));
        return true;
      };

      final sent = await service.fanOutReconnect(
        bobKey,
        initiatorPubkey: aliceKey,
      );

      expect(sent, equals(2));
      expect(sentTo.toSet(), equals({anchorHex, anchor2Hex}));
    });

    test('returns 0 when no rendezvous servers are configured', () async {
      final store = _storeWithPeers({bobHex: _knownPeer(bobKey)});
      final service = SignalingService(store: store);

      var sends = 0;
      service.sendSignaling = (recipient, payload) async {
        sends++;
        return true;
      };

      final sent = await service.fanOutReconnect(
        bobKey,
        initiatorPubkey: aliceKey,
      );

      expect(sent, equals(0));
      expect(sends, equals(0));
    });

    test('excludes the target itself from the server set', () async {
      // A rendezvous server that IS the target must not receive a RECONNECT
      // asking to be reconnected to itself.
      final store = _storeWithPeers(
        const {},
        settings: settingsWithAnchors([
          (anchorAddress, anchorHex),
        ]),
      );
      final service = SignalingService(store: store);

      var sends = 0;
      service.sendSignaling = (recipient, payload) async {
        sends++;
        return true;
      };

      final sent = await service.fanOutReconnect(
        anchorKey,
        initiatorPubkey: aliceKey,
      );

      expect(sent, equals(0));
      expect(sends, equals(0));
    });

    test('orders servers lexicographically by pubkey hex', () async {
      final store = _storeWithPeers(
        {bobHex: _knownPeer(bobKey)},
        settings: settingsWithAnchors([
          // Configured in reverse lexicographic order.
          (anchor2Address, anchor2Hex),
          (anchorAddress, anchorHex),
        ]),
      );
      final service = SignalingService(store: store);

      final sentTo = <String>[];
      service.sendSignaling = (recipient, payload) async {
        sentTo.add(_pubkeyHex(recipient));
        return true;
      };

      await service.fanOutReconnect(bobKey, initiatorPubkey: aliceKey);

      final expected = [anchorHex, anchor2Hex]..sort();
      expect(sentTo, equals(expected));
    });

    test('counts only successful sends', () async {
      final store = _storeWithPeers(
        {bobHex: _knownPeer(bobKey)},
        settings: settingsWithAnchors([
          (anchorAddress, anchorHex),
          (anchor2Address, anchor2Hex),
        ]),
      );
      final service = SignalingService(store: store);

      service.sendSignaling = (recipient, payload) async {
        return _pubkeyHex(recipient) == anchorHex;
      };

      final sent = await service.fanOutReconnect(
        bobKey,
        initiatorPubkey: aliceKey,
      );

      expect(sent, equals(1));
    });
  });

  group('fanOutAvailable', () {
    test('sends AVAILABLE to every configured rendezvous server', () async {
      final store = _storeWithPeers(
        {bobHex: _knownPeer(bobKey)},
        settings: settingsWithAnchors([
          (anchorAddress, anchorHex),
          (anchor2Address, anchor2Hex),
        ]),
      );
      final service = SignalingService(store: store);

      final sentTo = <String>[];
      service.sendSignaling = (recipient, payload) async {
        sentTo.add(_pubkeyHex(recipient));
        final msg = codec.decode(payload) as AvailableMessage;
        expect(msg.peerPubkey, equals(bobKey));
        return true;
      };

      final sent = await service.fanOutAvailable(bobKey);

      expect(sent, equals(2));
      expect(sentTo.toSet(), equals({anchorHex, anchor2Hex}));
    });

    test('returns 0 when no rendezvous servers are configured', () async {
      final store = _storeWithPeers({bobHex: _knownPeer(bobKey)});
      final service = SignalingService(store: store);

      var sends = 0;
      service.sendSignaling = (recipient, payload) async {
        sends++;
        return true;
      };

      final sent = await service.fanOutAvailable(bobKey);

      expect(sent, equals(0));
      expect(sends, equals(0));
    });
  });

  // ==========================================================================
  // Outgoing: requestDirectPunch
  // ==========================================================================

  group('requestDirectPunch', () {
    test('sends PUNCH_INITIATE directly to a known peer record', () async {
      final store = _storeWithPeers({bobHex: _knownPeer(bobKey)});
      final service = SignalingService(store: store);

      Uint8List? sentTo;
      PunchInitiateMessage? sentMsg;
      service.sendSignaling = (recipient, payload) async {
        sentTo = recipient;
        sentMsg = codec.decode(payload) as PunchInitiateMessage;
        return true;
      };

      final ok = await service.requestDirectPunch(
        bobKey,
        requesterPubkey: aliceKey,
        requesterIp: directPunchIp,
        requesterPort: 4242,
      );

      expect(ok, isTrue);
      expect(sentTo, equals(bobKey));
      expect(sentMsg!.peerPubkey, equals(aliceKey));
      expect(sentMsg!.ip, equals(directPunchIp));
      expect(sentMsg!.port, equals(4242));
    });

    test('returns false when the target has no peer record', () async {
      final store = _storeWithPeers(const {});
      final service = SignalingService(store: store);

      var sends = 0;
      service.sendSignaling = (recipient, payload) async {
        sends++;
        return true;
      };

      final ok = await service.requestDirectPunch(
        bobKey,
        requesterPubkey: aliceKey,
        requesterIp: directPunchIp,
        requesterPort: 4242,
      );

      expect(ok, isFalse);
      expect(sends, equals(0));
    });

    test('uses the direct-only send path when required', () async {
      final store = _storeWithPeers({bobHex: _knownPeer(bobKey)});
      final service = SignalingService(store: store);

      var directSends = 0;
      var normalSends = 0;
      service.sendDirectSignaling = (recipient, payload) async {
        directSends++;
        return true;
      };
      service.sendSignaling = (recipient, payload) async {
        normalSends++;
        return true;
      };

      final ok = await service.requestDirectPunch(
        bobKey,
        requesterPubkey: aliceKey,
        requesterIp: directPunchIp,
        requesterPort: 4242,
        requireDirectTransport: true,
      );

      expect(ok, isTrue);
      expect(directSends, equals(1));
      expect(normalSends, equals(0));
    });
  });

  // ==========================================================================
  // Incoming: trust filter
  // ==========================================================================

  group('processSignaling trust filter', () {
    test('drops signaling from an unknown, non-rendezvous sender', () {
      final store = _storeWithPeers(const {});
      final service = SignalingService(store: store);

      var fired = false;
      service.onPunchInitiate = (peer, ip, port, readyRecipient) {
        fired = true;
      };

      service.processSignaling(
        bobKey,
        codec.encode(PunchInitiateMessage(
          peerPubkey: aliceKey,
          ip: directPunchIp,
          port: 4242,
        )),
      );

      expect(fired, isFalse);
    });

    test('accepts signaling from an identified peer', () {
      final store = _storeWithPeers({bobHex: _knownPeer(bobKey)});
      final service = SignalingService(store: store);

      var fired = false;
      service.onPunchInitiate = (peer, ip, port, readyRecipient) {
        fired = true;
      };

      service.processSignaling(
        bobKey,
        codec.encode(PunchInitiateMessage(
          peerPubkey: aliceKey,
          ip: directPunchIp,
          port: 4242,
        )),
      );

      expect(fired, isTrue);
    });

    test('accepts signaling from a configured rendezvous server', () {
      final store = _storeWithPeers(
        const {},
        settings: settingsWithAnchors([(anchorAddress, anchorHex)]),
      );
      final service = SignalingService(store: store);

      var fired = false;
      service.onPunchInitiate = (peer, ip, port, readyRecipient) {
        fired = true;
      };

      service.processSignaling(
        anchorKey,
        codec.encode(PunchInitiateMessage(
          peerPubkey: bobKey,
          ip: directPunchIp,
          port: 4242,
        )),
      );

      expect(fired, isTrue);
    });
  });

  // ==========================================================================
  // Incoming: callbacks
  // ==========================================================================

  group('PunchInitiate callback', () {
    test('fires onPunchInitiate with correct params', () {
      final store = _storeWithPeers(
        const {},
        settings: settingsWithAnchors([(anchorAddress, anchorHex)]),
      );
      final service = SignalingService(store: store);

      Uint8List? punchPeer;
      String? punchIp;
      int? punchPort;
      Uint8List? readyRecipient;
      service.onPunchInitiate = (peer, ip, port, recipient) {
        punchPeer = peer;
        punchIp = ip;
        punchPort = port;
        readyRecipient = recipient;
      };

      service.processSignaling(
        anchorKey,
        codec.encode(PunchInitiateMessage(
          peerPubkey: bobKey,
          ip: directPunchIp,
          port: 4747,
        )),
      );

      expect(punchPeer, equals(bobKey));
      expect(punchIp, equals(directPunchIp));
      expect(punchPort, equals(4747));
      // PUNCH_READY goes back to whoever coordinated the punch.
      expect(readyRecipient, equals(anchorKey));
    });
  });

  group('PunchReady callback', () {
    test('fires onPunchReady with the ready peer', () {
      final store = _storeWithPeers(
        const {},
        settings: settingsWithAnchors([(anchorAddress, anchorHex)]),
      );
      final service = SignalingService(store: store);

      Uint8List? readyPeer;
      service.onPunchReady = (peer) => readyPeer = peer;

      service.processSignaling(
        anchorKey,
        codec.encode(PunchReadyMessage(peerPubkey: bobKey)),
      );

      expect(readyPeer, equals(bobKey));
    });
  });

  group('AddrReflect callback', () {
    test('fires onAddrReflected with reflected address', () {
      final store = _storeWithPeers(
        const {},
        settings: settingsWithAnchors([(anchorAddress, anchorHex)]),
      );
      final service = SignalingService(store: store);

      Uint8List? reflector;
      String? ip;
      int? port;
      service.onAddrReflected = (sender, reflIp, reflPort) {
        reflector = sender;
        ip = reflIp;
        port = reflPort;
      };

      service.processSignaling(
        anchorKey,
        codec.encode(AddrReflectMessage(ip: reflectedIp, port: 60123)),
      );

      expect(reflector, equals(anchorKey));
      expect(ip, equals(reflectedIp));
      expect(port, equals(60123));
    });
  });

  // ==========================================================================
  // Incoming: server-bound messages are ignored (agents do not mediate)
  // ==========================================================================

  group('server-bound messages', () {
    test('ignores RECONNECT — agents do not run the matcher', () {
      final store = _storeWithPeers({bobHex: _knownPeer(bobKey)});
      final service = SignalingService(store: store);

      var sends = 0;
      service.sendSignaling = (recipient, payload) async {
        sends++;
        return true;
      };

      service.processSignaling(
        bobKey,
        codec.encode(ReconnectMessage(
          initiatorPubkey: bobKey,
          peerPubkey: aliceKey,
        )),
      );

      // No PUNCH_INITIATE (or anything else) goes out: the layer holds no
      // social graph and never mediates between peers.
      expect(sends, equals(0));
    });

    test('ignores AVAILABLE — agents do not run the matcher', () {
      final store = _storeWithPeers({bobHex: _knownPeer(bobKey)});
      final service = SignalingService(store: store);

      var sends = 0;
      service.sendSignaling = (recipient, payload) async {
        sends++;
        return true;
      };

      service.processSignaling(
        bobKey,
        codec.encode(AvailableMessage(peerPubkey: aliceKey)),
      );

      expect(sends, equals(0));
    });
  });

  // ==========================================================================
  // Codec: removed message types stay removed on the wire
  // ==========================================================================

  group('codec', () {
    test('rejects the retired RV_LIST wire byte (0x0a)', () {
      expect(
        () => codec.decode(Uint8List.fromList([0x0a, 0x00, 0x00])),
        throwsArgumentError,
      );
    });

    test('rejects the retired FRIEND_LIST wire byte (0x0b)', () {
      expect(
        () => codec.decode(Uint8List.fromList([0x0b, 0x00, 0x00])),
        throwsArgumentError,
      );
    });
  });
}
