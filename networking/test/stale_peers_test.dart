import 'dart:typed_data';
import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking/grassroots_networking.dart';

void main() {
  group('computeStaleIpPeerPubkeys', () {
    String toHex(Uint8List pubkey) =>
        pubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

    test('returns only connected UDP peers that missed the stale threshold',
        () {
      final now = DateTime.now();
      final stalePubkey =
          Uint8List.fromList(List.generate(32, (i) => (i + 1) % 256));
      final freshPubkey =
          Uint8List.fromList(List.generate(32, (i) => (i + 33) % 256));
      final bleOnlyPubkey =
          Uint8List.fromList(List.generate(32, (i) => (i + 65) % 256));

      final stale = computeStaleIpPeerPubkeys(
        peers: [
          PeerState(
            publicKey: stalePubkey,
            lastSeen: now,
            lastWanSeen: now.subtract(const Duration(seconds: 31)),
          ),
          PeerState(
            publicKey: freshPubkey,
            lastSeen: now,
            lastWanSeen: now.subtract(const Duration(seconds: 5)),
          ),
          PeerState(
            publicKey: bleOnlyPubkey,
            lastSeen: now,
          ),
        ],
        underlay: Underlay.wan,
        connectedPubkeys: {
          toHex(stalePubkey),
          toHex(freshPubkey),
        },
        staleThreshold: const Duration(seconds: 20),
        now: now,
      );

      expect(stale, equals({toHex(stalePubkey)}));
    });

    test(
        'each IP session is stale by its own clock: a silent WAN session is '
        'stale while the same peer\'s live LAN session is not (spec '
        '§ANNOUNCE and Liveness: "independently per underlay")', () {
      final now = DateTime.now();
      final pubkey = Uint8List.fromList(List.generate(32, (i) => (i + 7) % 256));
      final peer = PeerState(
        publicKey: pubkey,
        lastSeen: now,
        lastLanSeen: now.subtract(const Duration(seconds: 2)),
        lastWanSeen: now.subtract(const Duration(seconds: 31)),
      );

      Set<String> staleOn(Underlay underlay) => computeStaleIpPeerPubkeys(
            peers: [peer],
            underlay: underlay,
            connectedPubkeys: {toHex(pubkey)},
            staleThreshold: const Duration(seconds: 20),
            now: now,
          );

      expect(staleOn(Underlay.wan), {toHex(pubkey)});
      expect(staleOn(Underlay.lan), isEmpty);
    });
  });

  group('computeStaleBlePeerPubkeys', () {
    String toHex(Uint8List pubkey) =>
        pubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

    test(
        'flags BLE-attached peers whose lastBleSeen is older than the '
        'staleness window, ignoring isFriend (friends and strangers alike)',
        () {
      final now = DateTime.now();
      final staleFriend =
          Uint8List.fromList(List.generate(32, (i) => (i + 1) % 256));
      final staleStranger =
          Uint8List.fromList(List.generate(32, (i) => (i + 11) % 256));
      final freshPeer =
          Uint8List.fromList(List.generate(32, (i) => (i + 21) % 256));
      final udpOnly =
          Uint8List.fromList(List.generate(32, (i) => (i + 31) % 256));

      final stale = computeStaleBlePeerPubkeys(
        peers: [
          PeerState(
            publicKey: staleFriend,
            bleCentralDeviceId: 'central:friend',
            lastBleSeen: now.subtract(const Duration(seconds: 31)),
          ),
          PeerState(
            publicKey: staleStranger,
            blePeripheralDeviceId: 'peripheral:stranger',
            lastBleSeen: now.subtract(const Duration(seconds: 60)),
          ),
          PeerState(
            publicKey: freshPeer,
            bleCentralDeviceId: 'central:fresh',
            lastBleSeen: now.subtract(const Duration(seconds: 5)),
          ),
          PeerState(
            publicKey: udpOnly,
            udpAddress: '[2001:db8::1]:4001',
            // No BLE attachment, no lastBleSeen — must not be flagged.
          ),
        ],
        staleThreshold: const Duration(seconds: 20),
        now: now,
      );

      expect(stale, equals({toHex(staleFriend), toHex(staleStranger)}));
    });

    test('does NOT flag peers with no lastBleSeen — they\'re treated as fresh',
        () {
      final now = DateTime.now();
      final neverSeenButAttached =
          Uint8List.fromList(List.generate(32, (i) => (i + 1) % 256));

      final stale = computeStaleBlePeerPubkeys(
        peers: [
          PeerState(
            publicKey: neverSeenButAttached,
            bleCentralDeviceId: 'central:new',
            // lastBleSeen left null — the very next ANNOUNCE/RSSI tick
            // populates it. Sweeping this would prematurely clear a path
            // that's mid-handshake.
          ),
        ],
        staleThreshold: const Duration(seconds: 20),
        now: now,
      );

      expect(stale, isEmpty);
    });
  });
}
