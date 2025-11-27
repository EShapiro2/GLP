import 'package:test/test.dart';
import 'package:grassroots_ble/src/models/message_type.dart';

void main() {
  group('MessageType', () {
    test('has correct values', () {
      expect(MessageType.message.value, equals(0x01));
      expect(MessageType.deliveryAck.value, equals(0x02));
      expect(MessageType.readReceipt.value, equals(0x03));
      expect(MessageType.noiseHandshakeInit.value, equals(0x10));
      expect(MessageType.noiseHandshakeResp.value, equals(0x11));
      expect(MessageType.noiseHandshakeFinal.value, equals(0x12));
      expect(MessageType.friendRequest.value, equals(0x20));
      expect(MessageType.friendAccept.value, equals(0x21));
      expect(MessageType.friendReject.value, equals(0x22));
      expect(MessageType.introduction.value, equals(0x30));
      expect(MessageType.friendListShare.value, equals(0x31));
      expect(MessageType.fragmentStart.value, equals(0xF0));
      expect(MessageType.fragmentContinue.value, equals(0xF1));
      expect(MessageType.fragmentEnd.value, equals(0xF2));
    });

    test('fromValue returns correct type', () {
      expect(MessageType.fromValue(0x01), equals(MessageType.message));
      expect(MessageType.fromValue(0x10), equals(MessageType.noiseHandshakeInit));
      expect(MessageType.fromValue(0xF2), equals(MessageType.fragmentEnd));
    });

    test('fromValue throws on unknown value', () {
      expect(() => MessageType.fromValue(0x99), throwsArgumentError);
    });

    test('tryFromValue returns null on unknown value', () {
      expect(MessageType.tryFromValue(0x99), isNull);
      expect(MessageType.tryFromValue(0x01), equals(MessageType.message));
    });

    test('isHandshake returns true for handshake types', () {
      expect(MessageType.noiseHandshakeInit.isHandshake, isTrue);
      expect(MessageType.noiseHandshakeResp.isHandshake, isTrue);
      expect(MessageType.noiseHandshakeFinal.isHandshake, isTrue);
      expect(MessageType.message.isHandshake, isFalse);
    });

    test('isFragment returns true for fragment types', () {
      expect(MessageType.fragmentStart.isFragment, isTrue);
      expect(MessageType.fragmentContinue.isFragment, isTrue);
      expect(MessageType.fragmentEnd.isFragment, isTrue);
      expect(MessageType.message.isFragment, isFalse);
    });

    test('isFriendship returns true for friendship types', () {
      expect(MessageType.friendRequest.isFriendship, isTrue);
      expect(MessageType.friendAccept.isFriendship, isTrue);
      expect(MessageType.friendReject.isFriendship, isTrue);
      expect(MessageType.introduction.isFriendship, isTrue);
      expect(MessageType.friendListShare.isFriendship, isTrue);
      expect(MessageType.message.isFriendship, isFalse);
    });
  });
}
