import 'dart:typed_data';
import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking/grassroots_networking.dart';
import 'package:grassroots_networking_core/src/mesh/bloom_filter.dart';
import 'package:grassroots_networking_core/src/protocol/fragment_handler.dart';

void main() {
  group('GrassrootsPacket', () {
    late Uint8List testPayload;

    setUp(() {
      testPayload = Uint8List.fromList([1, 2, 3, 4, 5]);
    });

    test('serializes and deserializes correctly', () {
      final packet = GrassrootsPacket(
        type: PacketType.message,
        payload: testPayload,
      );

      final serialized = packet.serialize();
      final deserialized = GrassrootsPacket.deserialize(serialized);

      expect(deserialized.type, equals(packet.type));
      expect(deserialized.payload, equals(packet.payload));
    });
  });

  group('BloomFilter', () {
    test('returns false for items not added', () {
      final filter = BloomFilter();
      expect(filter.mightContain('test-item'), isFalse);
    });

    test('returns true for added items', () {
      final filter = BloomFilter();
      filter.add('test-item');
      expect(filter.mightContain('test-item'), isTrue);
    });

    test('checkAndAdd returns correct values', () {
      final filter = BloomFilter();

      // First time - not present
      expect(filter.checkAndAdd('item1'), isFalse);

      // Second time - already present
      expect(filter.checkAndAdd('item1'), isTrue);

      // Different item - not present
      expect(filter.checkAndAdd('item2'), isFalse);
    });

    test('clears correctly', () {
      final filter = BloomFilter();
      filter.add('test-item');
      expect(filter.mightContain('test-item'), isTrue);

      filter.clear();
      expect(filter.mightContain('test-item'), isFalse);
    });

    test('handles many items without excessive false positives', () {
      final filter = BloomFilter();

      // Add 1000 items
      for (var i = 0; i < 1000; i++) {
        filter.add('item-$i');
      }

      // All added items should be found
      for (var i = 0; i < 1000; i++) {
        expect(filter.mightContain('item-$i'), isTrue);
      }

      // Check false positive rate on items NOT added
      var falsePositives = 0;
      for (var i = 1000; i < 2000; i++) {
        if (filter.mightContain('item-$i')) {
          falsePositives++;
        }
      }

      // False positive rate should be low (< 5%)
      expect(falsePositives, lessThan(50));
    });
  });

  group('FragmentHandler', () {
    late FragmentHandler handler;

    setUp(() {
      handler = FragmentHandler();
    });

    tearDown(() {
      handler.dispose();
    });

    const maxChunk = 270;
    const messageId = '00000000-0000-4000-8000-000000000abc';

    test('does not fragment small payloads', () {
      expect(handler.needsFragmentation(Uint8List(100), maxChunk: maxChunk),
          isFalse);
    });

    test('fragments large payloads', () {
      expect(handler.needsFragmentation(Uint8List(1000), maxChunk: maxChunk),
          isTrue);
    });

    test('fragments and reassembles correctly', () {
      final payload = Uint8List.fromList(List.generate(1500, (i) => i % 256));

      final fragmented = handler.fragment(
          payload: payload, messageId: messageId, maxChunk: maxChunk);

      expect(fragmented.fragments.length, greaterThan(1));
      for (final f in fragmented.fragments) {
        expect(f.type, equals(PacketType.fragment));
      }

      ReassembledMessage? result;
      for (final fragment in fragmented.fragments) {
        result =
            handler.addFragment(FragmentHandler.decodeFragment(fragment.payload));
      }

      expect(result, isNotNull);
      expect(result!.payload, equals(payload));
    });

    test('returns null for incomplete fragments', () {
      final payload = Uint8List.fromList(List.generate(1500, (i) => i % 256));

      final fragmented = handler.fragment(
          payload: payload, messageId: messageId, maxChunk: maxChunk);

      final result = handler.addFragment(
          FragmentHandler.decodeFragment(fragmented.fragments.first.payload));
      expect(result, isNull);
    });
  });

  group('computeStaleUdpPeerPubkeys', () {
    test('returns only connected UDP peers that missed the stale threshold',
        () {
      final now = DateTime.now();
      final stalePubkey =
          Uint8List.fromList(List.generate(32, (i) => (i + 1) % 256));
      final freshPubkey =
          Uint8List.fromList(List.generate(32, (i) => (i + 33) % 256));
      final bleOnlyPubkey =
          Uint8List.fromList(List.generate(32, (i) => (i + 65) % 256));

      String toHex(Uint8List pubkey) =>
          pubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

      final stale = computeStaleUdpPeerPubkeys(
        peers: [
          PeerState(
            publicKey: stalePubkey,
            lastSeen: now,
            lastUdpSeen: now.subtract(const Duration(seconds: 31)),
          ),
          PeerState(
            publicKey: freshPubkey,
            lastSeen: now,
            lastUdpSeen: now.subtract(const Duration(seconds: 5)),
          ),
          PeerState(
            publicKey: bleOnlyPubkey,
            lastSeen: now,
          ),
        ],
        connectedUdpPubkeys: {
          toHex(stalePubkey),
          toHex(freshPubkey),
        },
        staleThreshold: const Duration(seconds: 20),
        now: now,
      );

      expect(stale, equals({toHex(stalePubkey)}));
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
