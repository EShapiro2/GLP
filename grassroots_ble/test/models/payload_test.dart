import 'dart:typed_data';

import 'package:test/test.dart';
import 'package:grassroots_ble/src/models/payloads/chat_payload.dart';
import 'package:grassroots_ble/src/models/payloads/delivery_ack_payload.dart';
import 'package:grassroots_ble/src/models/payloads/fragment_header.dart';

void main() {
  group('ChatPayload', () {
    test('encodes and decodes correctly', () {
      final messageId = Uint8List.fromList(List.generate(16, (i) => i));
      final content = 'Hello, world!';

      final payload = ChatPayload(messageId: messageId, content: content);
      final encoded = payload.encode();
      final decoded = ChatPayload.decode(encoded);

      expect(decoded.messageId, equals(messageId));
      expect(decoded.content, equals(content));
    });

    test('handles empty content', () {
      final messageId = Uint8List.fromList(List.generate(16, (i) => i));
      final payload = ChatPayload(messageId: messageId, content: '');
      final encoded = payload.encode();
      final decoded = ChatPayload.decode(encoded);

      expect(decoded.content, equals(''));
    });

    test('handles unicode content', () {
      final messageId = Uint8List.fromList(List.generate(16, (i) => i));
      final content = 'Hello, 世界! 🌍';

      final payload = ChatPayload(messageId: messageId, content: content);
      final encoded = payload.encode();
      final decoded = ChatPayload.decode(encoded);

      expect(decoded.content, equals(content));
    });

    test('throws on truncated data', () {
      final data = Uint8List.fromList(List.generate(10, (i) => i));
      expect(() => ChatPayload.decode(data), throwsFormatException);
    });
  });

  group('DeliveryAckPayload', () {
    test('encodes and decodes correctly', () {
      final messageId = Uint8List.fromList(List.generate(16, (i) => i * 2));

      final payload = DeliveryAckPayload(messageId: messageId);
      final encoded = payload.encode();
      final decoded = DeliveryAckPayload.decode(encoded);

      expect(decoded.messageId, equals(messageId));
    });

    test('messageIdHex returns correct hex', () {
      final messageId = Uint8List.fromList([
        0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77,
        0x88, 0x99, 0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF,
      ]);
      final payload = DeliveryAckPayload(messageId: messageId);
      expect(payload.messageIdHex, equals('00112233445566778899aabbccddeeff'));
    });
  });

  group('FragmentHeader', () {
    test('encodes and decodes correctly', () {
      final header = FragmentHeader(
        messageId: 1234,
        fragmentIndex: 2,
        totalFragments: 5,
      );

      final encoded = header.encode();
      expect(encoded.length, equals(FragmentHeader.size));

      final decoded = FragmentHeader.decode(encoded);
      expect(decoded.messageId, equals(1234));
      expect(decoded.fragmentIndex, equals(2));
      expect(decoded.totalFragments, equals(5));
    });

    test('isFirst returns true for index 0', () {
      final header = FragmentHeader(
        messageId: 1,
        fragmentIndex: 0,
        totalFragments: 3,
      );
      expect(header.isFirst, isTrue);
      expect(header.isLast, isFalse);
      expect(header.isMiddle, isFalse);
    });

    test('isLast returns true for last index', () {
      final header = FragmentHeader(
        messageId: 1,
        fragmentIndex: 2,
        totalFragments: 3,
      );
      expect(header.isFirst, isFalse);
      expect(header.isLast, isTrue);
      expect(header.isMiddle, isFalse);
    });

    test('isMiddle returns true for middle index', () {
      final header = FragmentHeader(
        messageId: 1,
        fragmentIndex: 1,
        totalFragments: 3,
      );
      expect(header.isFirst, isFalse);
      expect(header.isLast, isFalse);
      expect(header.isMiddle, isTrue);
    });

    test('throws on invalid fragmentIndex', () {
      expect(
        () => FragmentHeader(
          messageId: 1,
          fragmentIndex: 5,
          totalFragments: 3,
        ),
        throwsArgumentError,
      );
    });

    test('throws on zero totalFragments', () {
      expect(
        () => FragmentHeader(
          messageId: 1,
          fragmentIndex: 0,
          totalFragments: 0,
        ),
        throwsArgumentError,
      );
    });

    test('equality works', () {
      final a = FragmentHeader(messageId: 1, fragmentIndex: 0, totalFragments: 3);
      final b = FragmentHeader(messageId: 1, fragmentIndex: 0, totalFragments: 3);
      final c = FragmentHeader(messageId: 2, fragmentIndex: 0, totalFragments: 3);

      expect(a == b, isTrue);
      expect(a == c, isFalse);
    });
  });
}
