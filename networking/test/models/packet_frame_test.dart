import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking/src/models/packet.dart';

/// Spec conformance: the wire frame (GLP Networking API §IP Communication /
/// §BLE Communication). Each message is framed by a packet-type byte followed
/// by a 4-byte big-endian payload length — five bytes, shared by both media,
/// carrying no identity, no signature, and nothing else.
void main() {
  group('five-byte frame', () {
    test('header is exactly type(1) + length(4, big-endian)', () {
      final payload = Uint8List.fromList([0xDE, 0xAD, 0xBE, 0xEF, 0x42]);
      final packet = GrassrootsPacket(
        type: PacketType.message,
        payload: payload,
      );

      final bytes = packet.serialize();

      expect(GrassrootsPacket.headerSize, equals(5));
      expect(bytes.length, equals(5 + payload.length));
      // Byte 0: the type byte.
      expect(bytes[0], equals(PacketType.message.value));
      // Bytes 1..4: payload length, big-endian.
      expect(bytes[1], equals(0));
      expect(bytes[2], equals(0));
      expect(bytes[3], equals(0));
      expect(bytes[4], equals(payload.length));
      // The payload follows immediately — nothing else is in the frame.
      expect(bytes.sublist(5), equals(payload));
    });

    test('length field is genuinely big-endian for multi-byte lengths', () {
      final payload = Uint8List(0x0102); // 258 bytes
      final bytes = GrassrootsPacket(
        type: PacketType.signaling,
        payload: payload,
      ).serialize();

      expect(bytes[1], equals(0x00));
      expect(bytes[2], equals(0x00));
      expect(bytes[3], equals(0x01));
      expect(bytes[4], equals(0x02));
    });

    test('round-trips every packet type', () {
      final payload = Uint8List.fromList(List.generate(17, (i) => i));
      for (final type in PacketType.values) {
        final decoded = GrassrootsPacket.deserialize(
          GrassrootsPacket(type: type, payload: payload).serialize(),
        );
        expect(decoded.type, equals(type));
        expect(decoded.payload, equals(payload));
      }
    });

    test('round-trips an empty payload', () {
      final bytes = GrassrootsPacket(
        type: PacketType.ack,
        payload: Uint8List(0),
      ).serialize();

      expect(bytes.length, equals(5));
      final decoded = GrassrootsPacket.deserialize(bytes);
      expect(decoded.type, equals(PacketType.ack));
      expect(decoded.payload, isEmpty);
    });

    test('rejects a buffer shorter than the header', () {
      for (var len = 0; len < 5; len++) {
        expect(
          () => GrassrootsPacket.deserialize(Uint8List(len)),
          throwsA(isA<FormatException>()),
          reason: '$len bytes is shorter than the 5-byte header',
        );
      }
    });

    test('rejects a buffer shorter than header + declared length', () {
      final bytes = GrassrootsPacket(
        type: PacketType.message,
        payload: Uint8List.fromList([1, 2, 3, 4]),
      ).serialize();
      final truncated = Uint8List.sublistView(bytes, 0, bytes.length - 1);

      expect(
        () => GrassrootsPacket.deserialize(truncated),
        throwsA(isA<FormatException>()),
      );
    });

    test('reads exactly the declared length, ignoring trailing bytes', () {
      // Stream transports slice packets out of an accumulation buffer, so a
      // packet followed by the next packet's bytes must decode to exactly its
      // declared payload.
      final first = GrassrootsPacket(
        type: PacketType.message,
        payload: Uint8List.fromList([9, 9, 9]),
      ).serialize();
      final buffered = Uint8List.fromList([...first, 0xAA, 0xBB]);

      final decoded = GrassrootsPacket.deserialize(buffered);

      expect(decoded.payload, equals([9, 9, 9]));
    });

    test('rejects an unknown type byte', () {
      final bytes = Uint8List.fromList([0x7F, 0, 0, 0, 0]);

      expect(
        () => GrassrootsPacket.deserialize(bytes),
        throwsArgumentError,
      );
    });

    test('peekPayloadLength reads the declared length without parsing', () {
      final bytes = GrassrootsPacket(
        type: PacketType.message,
        payload: Uint8List(300),
      ).serialize();

      expect(GrassrootsPacket.peekPayloadLength(bytes), equals(300));
      // Shorter than the header: nothing to peek yet.
      expect(GrassrootsPacket.peekPayloadLength(Uint8List(4)), isNull);
    });
  });
}
