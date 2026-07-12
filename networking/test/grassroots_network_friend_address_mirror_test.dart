import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking/src/grassroots_network.dart'
    show computeFriendUdpAddressMirrorActions;
import 'package:grassroots_networking/src/store/friendships_state.dart';
import 'package:grassroots_networking/src/store/peers_state.dart';

/// Regression tests for friend UDP-address persistence.
///
/// The peers slice (live, not persisted) is the canonical projection of a
/// peer's current address; the friendships slice is the persisted record.
/// `computeFriendUdpAddressMirrorActions` produces the dispatches that copy a
/// friend's live address into the friendship record — previously
/// `UpdateFriendshipUdpAddressAction` had no production dispatch site at all,
/// so `friendship.udpAddress` was always null and the restart hydration path
/// in main.dart was dead code.
void main() {
  Uint8List pubkey(int seed) =>
      Uint8List.fromList(List.generate(32, (i) => (seed + i) % 256));

  String pubkeyHex(Uint8List p) =>
      p.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  final now = DateTime(2026, 6, 10);

  FriendshipState friendship(
    int seed, {
    String? udpAddress,
    FriendshipStatus status = FriendshipStatus.accepted,
  }) =>
      FriendshipState(
        peerPubkeyHex: pubkeyHex(pubkey(seed)),
        nickname: 'F$seed',
        status: status,
        udpAddress: udpAddress,
        createdAt: now,
        updatedAt: now,
      );

  PeerState peer(int seed, {String? udpAddress}) => PeerState(
        publicKey: pubkey(seed),
        nickname: 'F$seed',
        udpAddress: udpAddress,
      );

  FriendshipsState friendshipsWith(List<FriendshipState> entries) =>
      FriendshipsState(
        friendships: {for (final f in entries) f.peerPubkeyHex: f},
      );

  PeersState peersWith(List<PeerState> entries) => PeersState.initial.copyWith(
        peers: {for (final p in entries) p.pubkeyHex: p},
      );

  group('computeFriendUdpAddressMirrorActions', () {
    test('mirrors a newly learned address (stored was null)', () {
      final actions = computeFriendUdpAddressMirrorActions(
        friendships: friendshipsWith([friendship(1)]),
        peers: peersWith([peer(1, udpAddress: '203.0.113.5:9514')]),
      );

      expect(actions, hasLength(1));
      expect(actions.single.peerPubkeyHex, pubkeyHex(pubkey(1)));
      expect(actions.single.udpAddress, '203.0.113.5:9514');
    });

    test('mirrors a changed address', () {
      final actions = computeFriendUdpAddressMirrorActions(
        friendships:
            friendshipsWith([friendship(1, udpAddress: '198.51.100.7:9514')]),
        peers: peersWith([peer(1, udpAddress: '203.0.113.5:9514')]),
      );

      expect(actions, hasLength(1));
      expect(actions.single.udpAddress, '203.0.113.5:9514');
    });

    test('no action when stored and live addresses match (convergence)', () {
      final actions = computeFriendUdpAddressMirrorActions(
        friendships:
            friendshipsWith([friendship(1, udpAddress: '203.0.113.5:9514')]),
        peers: peersWith([peer(1, udpAddress: '203.0.113.5:9514')]),
      );

      expect(actions, isEmpty);
    });

    test('never clears: peer with no live address produces no action', () {
      final actions = computeFriendUdpAddressMirrorActions(
        friendships:
            friendshipsWith([friendship(1, udpAddress: '203.0.113.5:9514')]),
        peers: peersWith([peer(1, udpAddress: null)]),
      );

      expect(actions, isEmpty);
    });

    test('ignores non-accepted friendships', () {
      final actions = computeFriendUdpAddressMirrorActions(
        friendships: friendshipsWith([
          friendship(1, status: FriendshipStatus.pending),
          friendship(2, status: FriendshipStatus.received),
          friendship(3, status: FriendshipStatus.declined),
        ]),
        peers: peersWith([
          peer(1, udpAddress: '203.0.113.1:9514'),
          peer(2, udpAddress: '203.0.113.2:9514'),
          peer(3, udpAddress: '203.0.113.3:9514'),
        ]),
      );

      expect(actions, isEmpty);
    });

    test('ignores friends with no peer record', () {
      final actions = computeFriendUdpAddressMirrorActions(
        friendships: friendshipsWith([friendship(1)]),
        peers: peersWith([]),
      );

      expect(actions, isEmpty);
    });

    test('handles multiple friends independently', () {
      final actions = computeFriendUdpAddressMirrorActions(
        friendships: friendshipsWith([
          friendship(1), // new address → mirror
          friendship(2, udpAddress: '203.0.113.2:9514'), // unchanged → skip
          friendship(3, udpAddress: '198.51.100.3:9514'), // gone live → skip
        ]),
        peers: peersWith([
          peer(1, udpAddress: '203.0.113.1:9514'),
          peer(2, udpAddress: '203.0.113.2:9514'),
          peer(3, udpAddress: null),
        ]),
      );

      expect(actions, hasLength(1));
      expect(actions.single.peerPubkeyHex, pubkeyHex(pubkey(1)));
      expect(actions.single.udpAddress, '203.0.113.1:9514');
    });
  });
}
