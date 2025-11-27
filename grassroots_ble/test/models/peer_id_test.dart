import 'dart:typed_data';

import 'package:test/test.dart';
import 'package:grassroots_ble/src/models/peer_id.dart';
import 'package:grassroots_ble/src/constants.dart';

void main() {
  group('PeerId', () {
    test('creates from bytes', () {
      final bytes = Uint8List.fromList([1, 2, 3, 4, 5, 6, 7, 8]);
      final peerId = PeerId(bytes);
      expect(peerId.bytes, equals(bytes));
    });

    test('throws on wrong size bytes', () {
      expect(
        () => PeerId(Uint8List.fromList([1, 2, 3])),
        throwsArgumentError,
      );
      expect(
        () => PeerId(Uint8List.fromList([1, 2, 3, 4, 5, 6, 7, 8, 9])),
        throwsArgumentError,
      );
    });

    test('creates from hex string', () {
      final peerId = PeerId.fromHex('0102030405060708');
      expect(peerId.bytes, equals([1, 2, 3, 4, 5, 6, 7, 8]));
    });

    test('creates from hex string with separators', () {
      final peerId = PeerId.fromHex('01-02-03-04-05-06-07-08');
      expect(peerId.bytes, equals([1, 2, 3, 4, 5, 6, 7, 8]));
    });

    test('throws on invalid hex length', () {
      expect(
        () => PeerId.fromHex('0102'),
        throwsArgumentError,
      );
    });

    test('creates from fingerprint', () {
      final fingerprint = Uint8List.fromList(
        List.generate(32, (i) => i),
      );
      final peerId = PeerId.fromFingerprint(fingerprint);
      expect(peerId.bytes, equals([0, 1, 2, 3, 4, 5, 6, 7]));
    });

    test('toHex returns lowercase hex', () {
      final peerId = PeerId.fromHex('AABBCCDD11223344');
      expect(peerId.toHex(), equals('aabbccdd11223344'));
    });

    test('toShortString returns first 4 bytes', () {
      final peerId = PeerId.fromHex('aabbccdd11223344');
      expect(peerId.toShortString(), equals('aabbccdd'));
    });

    test('broadcast PeerId is all 0xFF', () {
      final broadcast = PeerId.broadcast;
      expect(broadcast.bytes, equals([0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]));
      expect(broadcast.isBroadcast, isTrue);
    });

    test('regular PeerId is not broadcast', () {
      final peerId = PeerId.fromHex('0102030405060708');
      expect(peerId.isBroadcast, isFalse);
    });

    test('equality works', () {
      final a = PeerId.fromHex('0102030405060708');
      final b = PeerId.fromHex('0102030405060708');
      final c = PeerId.fromHex('0102030405060709');

      expect(a == b, isTrue);
      expect(a == c, isFalse);
      expect(a.hashCode == b.hashCode, isTrue);
    });

    test('copies bytes on creation', () {
      final bytes = Uint8List.fromList([1, 2, 3, 4, 5, 6, 7, 8]);
      final peerId = PeerId(bytes);
      bytes[0] = 99;
      expect(peerId.bytes[0], equals(1)); // Should not be affected
    });
  });
}
