import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:redux/redux.dart';
import 'package:grassroots_networking/src/signaling/signaling_service.dart';
import 'package:grassroots_networking/src/signaling/signaling_codec.dart';
import 'package:grassroots_networking/src/signaling/address_table.dart';
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
  Map<String, Set<String>> friendsOfFriends = const {},
  SettingsState settings = const SettingsState(),
  Map<String, Map<String, String>> friendRvServers = const {},
}) {
  // Synthesize accepted FriendshipState entries for any PeerState marked
  // isFriend, attaching RV-list data from [friendRvServers] when provided.
  // RV-list data lives on the friendship record (friendship-scoped + persisted).
  final now = DateTime.now();
  final friendships = <String, FriendshipState>{};
  for (final entry in peers.entries) {
    final peer = entry.value;
    if (!peer.isFriend) continue;
    friendships[entry.key] = FriendshipState(
      peerPubkeyHex: entry.key,
      nickname: peer.nickname,
      status: FriendshipStatus.accepted,
      createdAt: now,
      updatedAt: now,
      knownRvServers: friendRvServers[entry.key] ?? const {},
    );
  }
  return Store<AppState>(
    appReducer,
    initialState: AppState(
      peers: PeersState(peers: peers, friendsOfFriends: friendsOfFriends),
      settings: settings,
      friendships: FriendshipsState(friendships: friendships),
    ),
  );
}

PeerState _wellConnectedFriend(Uint8List pubkey, {String? udpAddress}) {
  return PeerState(
    publicKey: pubkey,
    nickname: 'Friend-${_pubkeyHex(pubkey).substring(0, 4)}',
    connectionState: PeerConnectionState.connected,
    isFriend: true,
    udpAddress: udpAddress ?? '[2606:4700::1]:4001',
  );
}

PeerState _regularFriend(Uint8List pubkey, {String? udpAddress}) {
  return PeerState(
    publicKey: pubkey,
    nickname: 'Peer-${_pubkeyHex(pubkey).substring(0, 4)}',
    connectionState: PeerConnectionState.connected,
    isFriend: true,
    udpAddress: udpAddress,
  );
}

PeerState _stranger(Uint8List pubkey) {
  return PeerState(
    publicKey: pubkey,
    nickname: 'Stranger',
    connectionState: PeerConnectionState.connected,
    isFriend: false,
  );
}

void main() {
  final aliceKey = _testPubkey(1);
  final bobKey = _testPubkey(2);
  final friendKey = _testPubkey(3);
  final friend2Key = _testPubkey(4);
  final anchorKey = _testPubkey(5);
  final anchor2Key = _testPubkey(6);
  final aliceHex = _pubkeyHex(aliceKey);
  final bobHex = _pubkeyHex(bobKey);
  final friendHex = _pubkeyHex(friendKey);
  final friend2Hex = _pubkeyHex(friend2Key);
  final anchorHex = _pubkeyHex(anchorKey);
  final anchor2Hex = _pubkeyHex(anchor2Key);
  const anchorAddress = '[2001:db8:ffff::1]:9514';
  const anchor2Address = '198.51.100.44:9514';
  const reflectedIp = '2400::12';
  const directPunchIp = '2400::13';
  const codec = SignalingCodec();

  // ==========================================================================
  // Outgoing: fanOutReconnect / fanOutAvailable
  // ==========================================================================

  group('fanOutReconnect', () {
    late SignalingService service;
    late List<(Uint8List, Uint8List)> sentMessages;

    setUp(() {
      sentMessages = [];
    });

    tearDown(() => service.dispose());

    test('sends RECONNECT to every eligible well-connected friend', () async {
      service = SignalingService(
        store: _storeWithPeers(
          {
            friendHex: _wellConnectedFriend(friendKey),
            friend2Hex: _wellConnectedFriend(friend2Key),
          },
          friendsOfFriends: {
            friendHex: {bobHex},
            friend2Hex: {bobHex},
          },
        ),
      );
      service.sendSignaling = (recipient, payload) async {
        sentMessages.add((recipient, payload));
        return true;
      };

      final sent = await service.fanOutReconnect(
        bobKey,
        initiatorPubkey: aliceKey,
      );

      expect(sent, equals(2));
      expect(sentMessages, hasLength(2));
      for (final (recipient, payload) in sentMessages) {
        final decoded = codec.decode(payload) as ReconnectMessage;
        expect(decoded.initiatorPubkey, equals(aliceKey));
        expect(decoded.peerPubkey, equals(bobKey));
        expect(
          [
            friendKey,
            friend2Key,
          ].any((k) => _pubkeyHex(k) == _pubkeyHex(recipient)),
          isTrue,
        );
      }
    });

    test(
      'sends RECONNECT to configured rendezvous servers when no friends exist',
      () async {
        service = SignalingService(
          store: _storeWithPeers(
            {},
            settings: SettingsState(
              rendezvousServers: [
                RendezvousServerSettings(
                  pubkeyHex: anchorHex,
                  address: anchorAddress,
                ),
              ],
            ),
          ),
        );
        service.sendSignaling = (recipient, payload) async {
          sentMessages.add((recipient, payload));
          return true;
        };

        final sent = await service.fanOutReconnect(
          bobKey,
          initiatorPubkey: aliceKey,
        );

        expect(sent, equals(1));
        expect(sentMessages, hasLength(1));
        expect(_pubkeyHex(sentMessages.single.$1), equals(anchorHex));
        final decoded =
            codec.decode(sentMessages.single.$2) as ReconnectMessage;
        expect(decoded.initiatorPubkey, equals(aliceKey));
        expect(decoded.peerPubkey, equals(bobKey));
      },
    );

    test('returns 0 when there are no facilitators', () async {
      service = SignalingService(store: _storeWithPeers({}));
      service.sendSignaling = (recipient, payload) async {
        sentMessages.add((recipient, payload));
        return true;
      };

      final sent = await service.fanOutReconnect(
        bobKey,
        initiatorPubkey: aliceKey,
      );

      expect(sent, equals(0));
      expect(sentMessages, isEmpty);
    });

    test(
      'does not send RECONNECT to friends that did not advertise the target',
      () async {
        service = SignalingService(
          store: _storeWithPeers(
            {friendHex: _wellConnectedFriend(friendKey)},
            friendsOfFriends: {
              friendHex: {aliceHex},
            },
          ),
        );
        service.sendSignaling = (recipient, payload) async {
          sentMessages.add((recipient, payload));
          return true;
        };

        final sent = await service.fanOutReconnect(
          bobKey,
          initiatorPubkey: aliceKey,
        );

        expect(sent, equals(0));
        expect(sentMessages, isEmpty);
      },
    );

    test('excludes the target itself from the facilitator set', () async {
      // Bob is one of the well-connected friends — he can't be a facilitator
      // for his own reconnection.
      service = SignalingService(
        store: _storeWithPeers(
          {
            bobHex: _wellConnectedFriend(bobKey),
            friendHex: _wellConnectedFriend(friendKey),
          },
          friendsOfFriends: {
            bobHex: {bobHex},
            friendHex: {bobHex},
          },
        ),
      );
      service.sendSignaling = (recipient, payload) async {
        sentMessages.add((recipient, payload));
        return true;
      };

      await service.fanOutReconnect(bobKey, initiatorPubkey: aliceKey);

      expect(sentMessages, hasLength(1));
      expect(_pubkeyHex(sentMessages.single.$1), equals(friendHex));
    });

    test('orders facilitators lexicographically by pubkey hex', () async {
      // Configure two rendezvous servers — pubkeys are seed=5 and seed=6, so
      // anchorHex < anchor2Hex. The fan-out must hit them in that order.
      service = SignalingService(
        store: _storeWithPeers(
          {},
          settings: SettingsState(
            rendezvousServers: [
              // intentionally listed in reverse lexicographic order
              RendezvousServerSettings(
                pubkeyHex: anchor2Hex,
                address: anchor2Address,
              ),
              RendezvousServerSettings(
                pubkeyHex: anchorHex,
                address: anchorAddress,
              ),
            ],
          ),
        ),
      );
      service.sendSignaling = (recipient, payload) async {
        sentMessages.add((recipient, payload));
        return true;
      };

      await service.fanOutReconnect(bobKey, initiatorPubkey: aliceKey);

      expect(sentMessages, hasLength(2));
      expect(
        sentMessages.map((m) => _pubkeyHex(m.$1)).toList(),
        equals([anchorHex, anchor2Hex]),
        reason: 'facilitators must be visited in lexicographic order',
      );
    });
  });

  group('fanOutAvailable', () {
    late SignalingService service;
    late List<(Uint8List, Uint8List)> sentMessages;

    setUp(() {
      sentMessages = [];
    });

    tearDown(() => service.dispose());

    test(
      "sends AVAILABLE to target's known RVs at the advertised address",
      () async {
        // Bob has previously told us he uses anchor as his RV at anchorAddress.
        // AVAILABLE should target Bob's RV at exactly that address.
        service = SignalingService(
          store: _storeWithPeers(
            {
              bobHex: PeerState(
                publicKey: bobKey,
                nickname: 'Bob',
                connectionState: PeerConnectionState.connected,
                isFriend: true,
              ),
            },
            friendRvServers: {
              bobHex: {anchorHex: anchorAddress},
            },
          ),
        );
        final addressSends = <(Uint8List, String, Uint8List)>[];
        service.sendSignaling = (recipient, payload) async {
          sentMessages.add((recipient, payload));
          return true;
        };
        service.sendSignalingToAddress = (recipient, address, payload) async {
          addressSends.add((recipient, address, payload));
          return true;
        };

        final sent = await service.fanOutAvailable(bobKey);

        expect(sent, equals(1));
        expect(addressSends, hasLength(1));
        expect(_pubkeyHex(addressSends.single.$1), equals(anchorHex));
        expect(addressSends.single.$2, equals(anchorAddress));
        final decoded =
            codec.decode(addressSends.single.$3) as AvailableMessage;
        expect(decoded.peerPubkey, equals(bobKey));
      },
    );

    test('does not send AVAILABLE to well-connected friends', () async {
      service = SignalingService(
        store: _storeWithPeers({
          friendHex: _wellConnectedFriend(friendKey),
          bobHex: _regularFriend(bobKey),
        }),
      );
      service.sendSignaling = (recipient, payload) async {
        sentMessages.add((recipient, payload));
        return true;
      };

      final sent = await service.fanOutAvailable(bobKey);

      expect(sent, equals(0));
      expect(sentMessages, isEmpty);
    });

    test(
      'targets only the target RVs, even with WC friends available',
      () async {
        service = SignalingService(
          store: _storeWithPeers(
            {
              friendHex: _wellConnectedFriend(friendKey),
              bobHex: PeerState(
                publicKey: bobKey,
                nickname: 'Bob',
                connectionState: PeerConnectionState.connected,
                isFriend: true,
              ),
            },
            friendRvServers: {
              bobHex: {
                anchorHex: anchorAddress,
                anchor2Hex: anchor2Address,
              },
            },
          ),
        );
        final addressSends = <(Uint8List, String, Uint8List)>[];
        service.sendSignaling = (recipient, payload) async {
          sentMessages.add((recipient, payload));
          return true;
        };
        service.sendSignalingToAddress = (recipient, address, payload) async {
          addressSends.add((recipient, address, payload));
          return true;
        };

        final sent = await service.fanOutAvailable(bobKey);

        expect(sent, equals(2));
        expect(addressSends, hasLength(2));
        expect(sentMessages, isEmpty);
        expect(
          addressSends.map((s) => _pubkeyHex(s.$1)).toSet(),
          equals({anchorHex, anchor2Hex}),
        );
      },
    );

    test('returns 0 when the target has no known RVs', () async {
      service = SignalingService(
        store: _storeWithPeers({bobHex: _regularFriend(bobKey)}),
      );
      service.sendSignaling = (recipient, payload) async {
        sentMessages.add((recipient, payload));
        return true;
      };

      final sent = await service.fanOutAvailable(bobKey);

      expect(sent, equals(0));
      expect(sentMessages, isEmpty);
    });
  });

  // ==========================================================================
  // Outgoing: requestDirectPunch (BLE-mediated direct PUNCH_INITIATE)
  // ==========================================================================

  group('requestDirectPunch', () {
    late SignalingService service;
    late List<(Uint8List, Uint8List)> sentMessages;

    setUp(() {
      sentMessages = [];
      service = SignalingService(
        store: _storeWithPeers({bobHex: _regularFriend(bobKey)}),
      );
      service.sendSignaling = (recipient, payload) async {
        sentMessages.add((recipient, payload));
        return true;
      };
    });

    tearDown(() => service.dispose());

    test('sends PUNCH_INITIATE directly to the target friend', () async {
      final ok = await service.requestDirectPunch(
        bobKey,
        requesterPubkey: aliceKey,
        requesterIp: directPunchIp,
        requesterPort: 7000,
      );

      expect(ok, isTrue);
      expect(sentMessages, hasLength(1));
      expect(_pubkeyHex(sentMessages.single.$1), equals(bobHex));
      final decoded =
          codec.decode(sentMessages.single.$2) as PunchInitiateMessage;
      expect(decoded.peerPubkey, equals(aliceKey));
      expect(decoded.ip, equals(directPunchIp));
      expect(decoded.port, equals(7000));
    });

    test('returns false when target is not a friend', () async {
      service.dispose();
      service = SignalingService(
        store: _storeWithPeers({bobHex: _stranger(bobKey)}),
      );
      service.sendSignaling = (recipient, payload) async {
        sentMessages.add((recipient, payload));
        return true;
      };

      final ok = await service.requestDirectPunch(
        bobKey,
        requesterPubkey: aliceKey,
        requesterIp: directPunchIp,
        requesterPort: 7000,
      );

      expect(ok, isFalse);
      expect(sentMessages, isEmpty);
    });
  });

  // ==========================================================================
  // Incoming: processAnnounceFromFriend
  // ==========================================================================

  group('processAnnounceFromFriend', () {
    late SignalingService service;
    late List<(Uint8List, Uint8List)> sentMessages;

    setUp(() {
      sentMessages = [];
      service = SignalingService(
        store: _storeWithPeers({bobHex: _regularFriend(bobKey)}),
      );
      service.sendSignaling = (recipient, payload) async {
        sentMessages.add((recipient, payload));
        return true;
      };
    });

    tearDown(() => service.dispose());

    test('registers observed address in the local address table', () {
      service.processAnnounceFromFriend(
        bobKey,
        observedIp: '203.0.113.10',
        observedPort: 7000,
      );

      final entry = service.addressTable.lookup(bobHex);
      expect(entry, isNotNull);
      expect(entry!.ip, equals('203.0.113.10'));
      expect(entry.port, equals(7000));
    });

    test('reflects observed address back to sender via ADDR_REFLECT', () {
      service.processAnnounceFromFriend(
        bobKey,
        observedIp: reflectedIp,
        observedPort: 7000,
      );

      expect(sentMessages, hasLength(1));
      expect(_pubkeyHex(sentMessages.single.$1), equals(bobHex));
      final decoded =
          codec.decode(sentMessages.single.$2) as AddrReflectMessage;
      expect(decoded.ip, equals(reflectedIp));
      expect(decoded.port, equals(7000));
    });

    test('does not reflect when there is no observed address', () {
      service.processAnnounceFromFriend(
        bobKey,
        claimedAddress: '[2001:db8::1]:5000',
      );

      expect(sentMessages, isEmpty);
    });
  });

  // ==========================================================================
  // Incoming: trust filter and unsupported messages
  // ==========================================================================

  group('processSignaling trust filter', () {
    late SignalingService service;
    late List<(Uint8List, Uint8List)> sentMessages;
    Uint8List? lastReflectIp;

    setUp(() {
      sentMessages = [];
      lastReflectIp = null;
      service = SignalingService(
        store: _storeWithPeers({
          friendHex: _wellConnectedFriend(friendKey),
          bobHex: _stranger(bobKey),
        }),
      );
      service.sendSignaling = (recipient, payload) async {
        sentMessages.add((recipient, payload));
        return true;
      };
      service.onAddrReflected = (senderPubkey, ip, port) {
        lastReflectIp = Uint8List.fromList(ip.codeUnits);
      };
    });

    tearDown(() => service.dispose());

    test('drops signaling from a non-friend, non-rendezvous sender', () {
      service.processSignaling(
        bobKey,
        codec.encode(AddrReflectMessage(ip: reflectedIp, port: 7000)),
      );
      expect(lastReflectIp, isNull);
    });

    test('accepts signaling from a friend', () {
      service.processSignaling(
        friendKey,
        codec.encode(AddrReflectMessage(ip: reflectedIp, port: 7000)),
      );
      expect(lastReflectIp, isNotNull);
    });

    test("accepts signaling from a friend's advertised rendezvous server", () {
      final service2 = SignalingService(
        store: _storeWithPeers(
          {
            bobHex: PeerState(
              publicKey: bobKey,
              nickname: 'Bob',
              connectionState: PeerConnectionState.connected,
              isFriend: true,
            ),
          },
          friendRvServers: {
            bobHex: {anchorHex: anchorAddress},
          },
        ),
      );
      Uint8List? gotPeer;
      String? gotIp;
      int? gotPort;
      Uint8List? gotReadyRecipient;
      service2.onPunchInitiate = (peer, ip, port, readyRecipient) {
        gotPeer = peer;
        gotIp = ip;
        gotPort = port;
        gotReadyRecipient = readyRecipient;
      };

      service2.processSignaling(
        anchorKey,
        codec.encode(
          PunchInitiateMessage(
            peerPubkey: bobKey,
            ip: directPunchIp,
            port: 7000,
          ),
        ),
      );

      expect(_pubkeyHex(gotPeer!), equals(bobHex));
      expect(gotIp, equals(directPunchIp));
      expect(gotPort, equals(7000));
      expect(_pubkeyHex(gotReadyRecipient!), equals(anchorHex));

      service2.dispose();
    });

    test("does not trust rendezvous servers advertised by non-friends", () {
      // Non-friends can't even have a known-RV record under the
      // friendship-scoped model (RV_LIST is only ever processed for
      // accepted friends in the reducer). The setup below intentionally
      // omits friendRvServers so the trust gate has no friend-vouched RV
      // for anchorKey — anchorKey should still be rejected.
      final service2 = SignalingService(
        store: _storeWithPeers({
          bobHex: PeerState(
            publicKey: bobKey,
            nickname: 'Bob',
            connectionState: PeerConnectionState.connected,
            isFriend: false,
          ),
        }),
      );
      Uint8List? gotPeer;
      service2.onPunchInitiate = (peer, ip, port, readyRecipient) {
        gotPeer = peer;
      };

      service2.processSignaling(
        anchorKey,
        codec.encode(
          PunchInitiateMessage(
            peerPubkey: bobKey,
            ip: directPunchIp,
            port: 7000,
          ),
        ),
      );

      expect(gotPeer, isNull);

      service2.dispose();
    });
  });

  // ==========================================================================
  // Incoming callbacks: PunchInitiate, PunchReady, AddrReflect
  // ==========================================================================

  group('PunchInitiate callback', () {
    late SignalingService service;

    setUp(() {
      service = SignalingService(
        store: _storeWithPeers({friendHex: _wellConnectedFriend(friendKey)}),
      );
      service.sendSignaling = (recipient, payload) async => true;
    });

    tearDown(() => service.dispose());

    test('fires onPunchInitiate with correct params', () {
      Uint8List? gotPeer;
      String? gotIp;
      int? gotPort;
      Uint8List? gotReadyRecipient;

      service.onPunchInitiate = (peer, ip, port, readyRecipient) {
        gotPeer = peer;
        gotIp = ip;
        gotPort = port;
        gotReadyRecipient = readyRecipient;
      };

      service.processSignaling(
        friendKey,
        codec.encode(
          PunchInitiateMessage(
            peerPubkey: bobKey,
            ip: directPunchIp,
            port: 7000,
          ),
        ),
      );

      expect(_pubkeyHex(gotPeer!), equals(bobHex));
      expect(gotIp, equals(directPunchIp));
      expect(gotPort, equals(7000));
      expect(_pubkeyHex(gotReadyRecipient!), equals(friendHex));
    });
  });

  group('PunchReady callback', () {
    late SignalingService service;

    setUp(() {
      service = SignalingService(
        store: _storeWithPeers({friendHex: _wellConnectedFriend(friendKey)}),
      );
      service.sendSignaling = (recipient, payload) async => true;
    });

    tearDown(() => service.dispose());

    test('fires onPunchReady with the ready peer', () {
      Uint8List? gotPeer;
      service.onPunchReady = (peer) => gotPeer = peer;

      service.processSignaling(
        friendKey,
        codec.encode(PunchReadyMessage(peerPubkey: bobKey)),
      );

      expect(_pubkeyHex(gotPeer!), equals(bobHex));
    });
  });

  group('AddrReflect callback', () {
    late SignalingService service;

    setUp(() {
      service = SignalingService(
        store: _storeWithPeers({friendHex: _wellConnectedFriend(friendKey)}),
      );
      service.sendSignaling = (recipient, payload) async => true;
    });

    tearDown(() => service.dispose());

    test('fires onAddrReflected with reflected address', () {
      String? gotIp;
      int? gotPort;
      service.onAddrReflected = (senderPubkey, ip, port) {
        gotIp = ip;
        gotPort = port;
      };

      service.processSignaling(
        friendKey,
        codec.encode(AddrReflectMessage(ip: reflectedIp, port: 7000)),
      );

      expect(gotIp, equals(reflectedIp));
      expect(gotPort, equals(7000));
    });

    test('accepts reflection from the configured rendezvous server', () {
      final service2 = SignalingService(
        store: _storeWithPeers(
          {},
          settings: SettingsState(
            rendezvousServers: [
              RendezvousServerSettings(
                pubkeyHex: anchorHex,
                address: anchorAddress,
              ),
            ],
          ),
        ),
      );
      service2.sendSignaling = (recipient, payload) async => true;

      String? gotIp;
      service2.onAddrReflected = (senderPubkey, ip, port) => gotIp = ip;

      service2.processSignaling(
        anchorKey,
        codec.encode(AddrReflectMessage(ip: reflectedIp, port: 7000)),
      );

      expect(gotIp, equals(reflectedIp));
      service2.dispose();
    });
  });

  // ==========================================================================
  // Incoming: client-side friends-based mediator
  // ==========================================================================

  group('client-as-friend mediator', () {
    late SignalingService service;
    late List<(Uint8List, Uint8List)> sentMessages;

    setUp(() {
      sentMessages = [];
      service = SignalingService(
        store: _storeWithPeers({
          friendHex: _wellConnectedFriend(friendKey),
          // Two friends — Alice and Bob — for whom this client is a mutual
          // mediator.
          aliceHex: _regularFriend(aliceKey),
          bobHex: _regularFriend(bobKey),
        }),
      );
      service.sendSignaling = (recipient, payload) async {
        sentMessages.add((recipient, payload));
        return true;
      };
    });

    tearDown(() => service.dispose());

    test('ignores AVAILABLE instead of running a two-sided matcher', () {
      service.processSignaling(
        bobKey,
        codec.encode(AvailableMessage(peerPubkey: aliceKey)),
        observedIp: '203.0.113.20',
        observedPort: 9001,
      );

      expect(sentMessages, isEmpty);
    });

    test(
      'uses stored friend addresses when signaling has no observed source',
      () {
        service.dispose();
        sentMessages = [];
        service = SignalingService(
          store: _storeWithPeers({
            aliceHex: _regularFriend(aliceKey),
            bobHex: PeerState(
              publicKey: bobKey,
              nickname: 'Bob',
              connectionState: PeerConnectionState.connected,
              isFriend: true,
              hasLiveUdpConnection: true,
            ),
          }),
        );
        service.sendSignaling = (recipient, payload) async {
          sentMessages.add((recipient, payload));
          return true;
        };

        service.processAnnounceFromFriend(
          aliceKey,
          claimedAddress: '[2606:4700::10]:7000',
        );
        service.processAnnounceFromFriend(
          bobKey,
          claimedAddress: '[2606:4700::20]:9001',
        );

        service.processSignaling(
          aliceKey,
          codec.encode(
            ReconnectMessage(initiatorPubkey: aliceKey, peerPubkey: bobKey),
          ),
        );

        expect(sentMessages, hasLength(2));
        final toAlice = sentMessages
            .firstWhere((m) => _pubkeyHex(m.$1) == aliceHex);
        final initiateToAlice =
            codec.decode(toAlice.$2) as PunchInitiateMessage;
        expect(_pubkeyHex(initiateToAlice.peerPubkey), equals(bobHex));
        expect(initiateToAlice.ip, equals('2606:4700::20'));
        expect(initiateToAlice.port, equals(9001));

        final toBob = sentMessages
            .firstWhere((m) => _pubkeyHex(m.$1) == bobHex);
        final initiateToBob = codec.decode(toBob.$2) as PunchInitiateMessage;
        expect(_pubkeyHex(initiateToBob.peerPubkey), equals(aliceHex));
        expect(initiateToBob.ip, equals('2606:4700::10'));
        expect(initiateToBob.port, equals(7000));
      },
    );

    test('coordinates a single-step mediation when target is live', () {
      service.dispose();
      sentMessages = [];
      service = SignalingService(
        store: _storeWithPeers({
          aliceHex: _regularFriend(
            aliceKey,
            udpAddress: '[2606:4700::10]:7000',
          ),
          bobHex: PeerState(
            publicKey: bobKey,
            nickname: 'Bob',
            connectionState: PeerConnectionState.connected,
            isFriend: true,
            udpAddress: '[2606:4700::20]:9001',
            hasLiveUdpConnection: true,
          ),
        }),
      );
      service.sendSignaling = (recipient, payload) async {
        sentMessages.add((recipient, payload));
        return true;
      };

      service.processSignaling(
        aliceKey,
        codec.encode(
          ReconnectMessage(initiatorPubkey: aliceKey, peerPubkey: bobKey),
        ),
      );

      expect(sentMessages, hasLength(2));
      final toAlice = sentMessages
          .firstWhere((m) => _pubkeyHex(m.$1) == aliceHex);
      final initiateToAlice = codec.decode(toAlice.$2) as PunchInitiateMessage;
      expect(_pubkeyHex(initiateToAlice.peerPubkey), equals(bobHex));
      expect(initiateToAlice.ip, equals('2606:4700::20'));
      expect(initiateToAlice.port, equals(9001));

      final toBob = sentMessages
          .firstWhere((m) => _pubkeyHex(m.$1) == bobHex);
      final initiateToBob = codec.decode(toBob.$2) as PunchInitiateMessage;
      expect(_pubkeyHex(initiateToBob.peerPubkey), equals(aliceHex));
      expect(initiateToBob.ip, equals('2606:4700::10'));
      expect(initiateToBob.port, equals(7000));
    });

    test('drops RECONNECT when no UDP address is known', () {
      service.processSignaling(
        aliceKey,
        codec.encode(
          ReconnectMessage(initiatorPubkey: aliceKey, peerPubkey: bobKey),
        ),
      );

      expect(sentMessages, isEmpty);
    });

    test('drops RECONNECT when inner initiator differs from signed sender', () {
      service.processSignaling(
        aliceKey,
        codec.encode(
          ReconnectMessage(initiatorPubkey: bobKey, peerPubkey: bobKey),
        ),
        observedIp: '198.51.100.10',
        observedPort: 7000,
      );

      service.processSignaling(
        bobKey,
        codec.encode(AvailableMessage(peerPubkey: aliceKey)),
        observedIp: '203.0.113.20',
        observedPort: 9001,
      );

      expect(sentMessages, isEmpty);
    });
  });

  // ==========================================================================
  // Incoming: RV_LIST stores per-peer rendezvous server pubkeys
  // ==========================================================================

  group('RV_LIST handling', () {
    test("updates the friend's knownRvServers on receive", () {
      final store = _storeWithPeers({bobHex: _regularFriend(bobKey)});
      final service = SignalingService(store: store);
      service.sendSignaling = (recipient, payload) async => true;

      service.processSignaling(
        bobKey,
        codec.encode(
          RvListMessage(
            entries: [
              RvServerEntry(pubkey: anchorKey, address: anchorAddress),
              RvServerEntry(pubkey: anchor2Key, address: anchor2Address),
            ],
          ),
        ),
      );

      // knownRvServers moved to the friendship record (friendship-scoped,
      // persisted). PeerState no longer owns it.
      final updated = store.state.friendships.getFriendship(bobHex);
      expect(updated, isNotNull);
      expect(
        updated!.knownRvServers,
        equals({anchorHex: anchorAddress, anchor2Hex: anchor2Address}),
      );

      service.dispose();
    });

    test("subsequent fanOutAvailable targets the friend's advertised RVs at "
        'their advertised address', () async {
      final store = _storeWithPeers({bobHex: _regularFriend(bobKey)});
      final service = SignalingService(store: store);
      final addressSends = <(Uint8List, String, Uint8List)>[];
      service.sendSignaling = (recipient, payload) async => true;
      service.sendSignalingToAddress = (recipient, address, payload) async {
        addressSends.add((recipient, address, payload));
        return true;
      };

      // Bob tells us about his RV server.
      service.processSignaling(
        bobKey,
        codec.encode(
          RvListMessage(
            entries: [RvServerEntry(pubkey: anchorKey, address: anchorAddress)],
          ),
        ),
      );

      // Now we detect Bob went silent; AVAILABLE should target anchorKey
      // at anchorAddress.
      final sent = await service.fanOutAvailable(bobKey);

      expect(sent, equals(1));
      expect(addressSends, hasLength(1));
      expect(_pubkeyHex(addressSends.single.$1), equals(anchorHex));
      expect(addressSends.single.$2, equals(anchorAddress));

      service.dispose();
    });
  });

  // ==========================================================================
  // Incoming: FRIEND_LIST stores the friends-of-friends map
  // ==========================================================================

  group('FRIEND_LIST handling', () {
    test('codec round-trips advertised friend pubkeys', () {
      final encoded = codec.encode(
        FriendListMessage(friendPubkeys: [aliceKey, friendKey]),
      );

      final decoded = codec.decode(encoded) as FriendListMessage;

      expect(decoded.friendPubkeys, hasLength(2));
      expect(decoded.friendPubkeys[0], equals(aliceKey));
      expect(decoded.friendPubkeys[1], equals(friendKey));
    });

    test("updates the sender's friends-of-friends set on receive", () {
      final store = _storeWithPeers({bobHex: _regularFriend(bobKey)});
      final service = SignalingService(store: store);
      service.sendSignaling = (recipient, payload) async => true;

      service.processSignaling(
        bobKey,
        codec.encode(FriendListMessage(friendPubkeys: [aliceKey, friendKey])),
      );

      expect(
        store.state.peers.friendsOfFriends[bobHex],
        equals({aliceHex, friendHex}),
      );

      service.dispose();
    });

    test('sendFriendList emits a FRIEND_LIST message', () async {
      final store = _storeWithPeers({bobHex: _regularFriend(bobKey)});
      final service = SignalingService(store: store);
      final sentMessages = <(Uint8List, Uint8List)>[];
      service.sendSignaling = (recipient, payload) async {
        sentMessages.add((recipient, payload));
        return true;
      };

      final sent = await service.sendFriendList(bobKey, [aliceKey]);

      expect(sent, isTrue);
      expect(sentMessages, hasLength(1));
      expect(_pubkeyHex(sentMessages.single.$1), equals(bobHex));
      final decoded = codec.decode(sentMessages.single.$2) as FriendListMessage;
      expect(decoded.friendPubkeys.single, equals(aliceKey));

      service.dispose();
    });
  });

  // ==========================================================================
  // Address table TTL cleanup
  // ==========================================================================

  group('address table stale cleanup', () {
    test('removes entries older than TTL', () {
      final table = AddressTable();
      table.register(friendHex, '203.0.113.10', 7000);
      table.removeStale(Duration.zero);
      expect(table.lookup(friendHex), isNull);
    });

    test('keeps fresh entries', () {
      final table = AddressTable();
      table.register(friendHex, '203.0.113.10', 7000);
      table.removeStale(const Duration(seconds: 60));
      expect(table.lookup(friendHex), isNotNull);
    });
  });
}
