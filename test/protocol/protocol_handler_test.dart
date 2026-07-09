import 'dart:convert';
import 'dart:typed_data';
import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking/src/protocol/protocol_handler.dart';
import 'package:grassroots_networking/src/models/identity.dart';
import 'package:grassroots_networking/src/models/packet.dart';
import 'package:cryptography/cryptography.dart';
import 'package:sodium_libs/sodium_libs.dart';

import '../helpers/sodium_test_bootstrap.dart';

void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  late Sodium sodium;
  setUpAll(() async {
    sodium = await initTestSodium();
  });

  group('ProtocolHandler', () {
    late ProtocolHandler handler;
    late GrassrootsIdentity testIdentity;

    setUp(() async {
      // Create a test identity for testing
      final algorithm = Ed25519();
      final keyPair = await algorithm.newKeyPair();
      testIdentity = await GrassrootsIdentity.create(
        keyPair: keyPair,
        nickname: 'TestUser',
      );
      handler = ProtocolHandler(identity: testIdentity, sodium: sodium);
    });

    group('createAnnouncePayload', () {
      test('encodes public key, version, and nickname correctly', () {
        final payload = handler.createAnnouncePayload();

        // Verify payload structure (record + trailing signature)
        expect(
            payload.length,
            greaterThanOrEqualTo(32 +
                2 +
                1 +
                'TestUser'.length +
                2 +
                ProtocolHandler.announceSignatureLength));

        // Public key (first 32 bytes)
        final pubkeyFromPayload = payload.sublist(0, 32);
        expect(pubkeyFromPayload, equals(testIdentity.publicKey));

        // Protocol version (next 2 bytes)
        final versionData =
            ByteData.view(payload.buffer, payload.offsetInBytes + 32, 2);
        final version = versionData.getUint16(0, Endian.big);
        expect(version, equals(ProtocolHandler.protocolVersion));

        // Nickname length and nickname
        final nickLen = payload[34];
        expect(nickLen, equals('TestUser'.length));
        final nickname =
            String.fromCharCodes(payload.sublist(35, 35 + nickLen));
        expect(nickname, equals('TestUser'));
      });

      test('creates payload without candidates when not provided', () {
        final payload = handler.createAnnouncePayload();

        // Candidate count should be 0 after the nickname.
        const offset = 32 + 2 + 1 + 'TestUser'.length;
        final candidateCountData =
            ByteData.view(payload.buffer, payload.offsetInBytes + offset, 2);
        final candidateCount = candidateCountData.getUint16(0, Endian.big);
        expect(candidateCount, equals(0));
      });

      test('includes UDP address as a candidate when provided', () {
        const testAddress = '[::1]:4001';
        final payload = handler.createAnnouncePayload(address: testAddress);
        final decoded = handler.decodeAnnounce(payload);

        expect(decoded.udpAddress, equals(testAddress));
        expect(decoded.addressCandidates, equals({testAddress}));
      });

      test('handles empty nickname', () async {
        final algorithm = Ed25519();
        final keyPair = await algorithm.newKeyPair();
        final emptyNickIdentity = await GrassrootsIdentity.create(
          keyPair: keyPair,
          nickname: '',
        );
        final emptyHandler =
            ProtocolHandler(identity: emptyNickIdentity, sodium: sodium);

        final payload = emptyHandler.createAnnouncePayload();

        // Should have valid structure with 0-length nickname.
        // pubkey + version + nickLen(0) + candidateCount(0) + signature
        expect(
            payload.length,
            equals(
                32 + 2 + 1 + 2 + ProtocolHandler.announceSignatureLength));
        expect(payload[34], equals(0)); // nickname length = 0
      });

      test('includes UDP address candidates when provided', () {
        final payload = handler.createAnnouncePayload(
          address: '[2606:4700::1]:5000',
          addressCandidates: const {
            '[2606:4700::1]:5000',
            '198.51.100.7:5001',
          },
        );
        final decoded = handler.decodeAnnounce(payload);

        expect(
          decoded.addressCandidates,
          containsAll(const {
            '[2606:4700::1]:5000',
            '198.51.100.7:5001',
          }),
        );
      });

      test('round-trips a non-ASCII / emoji nickname as UTF-8', () async {
        final keyPair = await Ed25519().newKeyPair();
        const fancyNick = 'Zoë 🌱🚀 名字';
        final fancyIdentity = await GrassrootsIdentity.create(
          keyPair: keyPair,
          nickname: fancyNick,
        );
        final fancyHandler =
            ProtocolHandler(identity: fancyIdentity, sodium: sodium);

        final payload = fancyHandler.createAnnouncePayload();
        final decoded = fancyHandler.decodeAnnounce(payload);

        expect(decoded.nickname, equals(fancyNick));
        // The 1-byte length prefix counts UTF-8 bytes, not characters.
        expect(payload[34], equals(utf8.encode(fancyNick).length));
      });
    });

    group('decodeAnnounce', () {
      test('decodes announce payload created by createAnnouncePayload', () {
        final payload = handler.createAnnouncePayload();
        final decoded = handler.decodeAnnounce(payload);

        expect(decoded.publicKey, equals(testIdentity.publicKey));
        expect(decoded.nickname, equals('TestUser'));
        expect(decoded.protocolVersion,
            equals(ProtocolHandler.protocolVersion));
        expect(decoded.udpAddress, isNull);
        expect(decoded.addressCandidates, isEmpty);
      });

      test('decodes announce with UDP address', () {
        const testAddress = '[2001:db8::64]:5000';
        final payload = handler.createAnnouncePayload(address: testAddress);
        final decoded = handler.decodeAnnounce(payload);

        expect(decoded.publicKey, equals(testIdentity.publicKey));
        expect(decoded.nickname, equals('TestUser'));
        expect(decoded.protocolVersion,
            equals(ProtocolHandler.protocolVersion));
        expect(decoded.udpAddress, equals(testAddress));
        expect(decoded.addressCandidates, contains(testAddress));
      });

      test('throws when candidate set is missing', () {
        final nicknameBytes = utf8.encode('MalformedPeer');
        final buffer = ByteData(32 + 2 + 1 + nicknameBytes.length);
        var offset = 0;

        buffer.buffer
            .asUint8List()
            .setRange(offset, offset + 32, testIdentity.publicKey);
        offset += 32;

        buffer.setUint16(offset, ProtocolHandler.protocolVersion, Endian.big);
        offset += 2;

        buffer.setUint8(offset++, nicknameBytes.length);
        buffer.buffer
            .asUint8List()
            .setRange(offset, offset + nicknameBytes.length, nicknameBytes);

        final payload = buffer.buffer.asUint8List();
        expect(
          () => handler.decodeAnnounce(payload),
          throwsA(isA<FormatException>()),
        );
      });

      test('handles empty nickname in payload', () {
        // pubkey(32) + version(2) + nickLen(1) + candidateCount(2)
        final buffer = ByteData(32 + 2 + 1 + 2);
        var offset = 0;

        // Public key
        buffer.buffer
            .asUint8List()
            .setRange(offset, offset + 32, testIdentity.publicKey);
        offset += 32;

        // Version
        buffer.setUint16(offset, ProtocolHandler.protocolVersion, Endian.big);
        offset += 2;

        // Nickname length = 0
        buffer.setUint8(offset++, 0);

        // Candidate count = 0
        buffer.setUint16(offset, 0, Endian.big);

        // Self-sign the record so it verifies against the carried pubkey.
        final record = buffer.buffer.asUint8List();
        final payload =
            Uint8List.fromList([...record, ...handler.signBytes(record)]);
        final decoded = handler.decodeAnnounce(payload);

        expect(decoded.nickname, equals(''));
        expect(decoded.udpAddress, isNull);
        expect(decoded.addressCandidates, isEmpty);
      });

      test('throws when a signed payload byte is tampered', () {
        final payload = handler.createAnnouncePayload(
          address: '[2001:db8::7]:4001',
        );

        // Flip a bit inside the nickname — structure still parses, but the
        // trailing signature no longer covers the bytes.
        payload[36] ^= 0xFF;

        expect(
          () => handler.decodeAnnounce(payload),
          throwsA(isA<FormatException>()),
        );
      });

      test('throws when the signature is truncated', () {
        final payload = handler.createAnnouncePayload();
        final truncated = payload.sublist(0, payload.length - 1);

        expect(
          () => handler.decodeAnnounce(truncated),
          throwsA(isA<FormatException>()),
        );
      });

      test('throws when the signature is tampered', () {
        final payload = handler.createAnnouncePayload();
        payload[payload.length - 1] ^= 0xFF;

        expect(
          () => handler.decodeAnnounce(payload),
          throwsA(isA<FormatException>()),
        );
      });
    });

    group('createMessagePacket', () {
      const messageId = '550e8400-e29b-41d4-a716-446655440000';

      test('prefixes the body with the 36-char messageId', () {
        final body = utf8.encode('Hello, World!');
        final packet = handler.createMessagePacket(
          payload: body,
          messageId: messageId,
        );

        expect(packet.type, equals(PacketType.message));
        expect(packet.payload.length,
            equals(ProtocolHandler.messageIdLength + body.length));
        expect(
          utf8.decode(
              packet.payload.sublist(0, ProtocolHandler.messageIdLength)),
          equals(messageId),
        );
        expect(
          packet.payload.sublist(ProtocolHandler.messageIdLength),
          equals(body),
        );
      });

      test('throws for a messageId that is not 36 characters', () {
        expect(
          () => handler.createMessagePacket(
            payload: utf8.encode('Hello'),
            messageId: 'short-id',
          ),
          throwsArgumentError,
        );
      });

      test('creates packet with empty body', () {
        final packet = handler.createMessagePacket(
          payload: Uint8List(0),
          messageId: messageId,
        );

        expect(packet.type, equals(PacketType.message));
        expect(
            packet.payload.length, equals(ProtocolHandler.messageIdLength));
        expect(utf8.decode(packet.payload), equals(messageId));
      });

      test('creates packet with large body', () {
        final largeBody = Uint8List(1000);
        for (var i = 0; i < 1000; i++) {
          largeBody[i] = i % 256;
        }

        final packet = handler.createMessagePacket(
          payload: largeBody,
          messageId: messageId,
        );

        expect(packet.payload.length,
            equals(ProtocolHandler.messageIdLength + 1000));
        expect(
          packet.payload.sublist(ProtocolHandler.messageIdLength),
          equals(largeBody),
        );
      });
    });

    group('createReadReceiptPacket', () {
      test('creates read receipt with message ID', () {
        const messageId = 'test-message-id-12345';
        final packet = handler.createReadReceiptPacket(messageId: messageId);

        expect(packet.type, equals(PacketType.readReceipt));
        expect(utf8.decode(packet.payload), equals(messageId));
      });

      test('handles UUID message IDs', () {
        const messageId = '550e8400-e29b-41d4-a716-446655440000';
        final packet = handler.createReadReceiptPacket(messageId: messageId);

        final decodedId = utf8.decode(packet.payload);
        expect(decodedId, equals(messageId));
      });
    });

    group('decodeReadReceipt', () {
      test('decodes read receipt payload', () {
        const messageId = 'msg-abc-123';
        final payload = utf8.encode(messageId);
        final decoded = handler.decodeReadReceipt(payload);

        expect(decoded, equals(messageId));
      });

      test('handles empty message ID', () {
        final payload = utf8.encode('');
        final decoded = handler.decodeReadReceipt(payload);

        expect(decoded, equals(''));
      });
    });

    group('createAckPacket', () {
      test('creates ACK with message ID', () {
        const messageId = 'ack-msg-1';
        final packet = handler.createAckPacket(messageId: messageId);

        expect(packet.type, equals(PacketType.ack));
        expect(utf8.decode(packet.payload), equals(messageId));
      });
    });

    group('signBytes and verifyBytes', () {
      test('signed message verifies successfully', () {
        final message = utf8.encode('Hello');
        final signature = handler.signBytes(message);

        expect(signature.length, equals(64));
        expect(
          handler.verifyBytes(
            signature: signature,
            message: message,
            publicKey: testIdentity.publicKey,
          ),
          isTrue,
        );
      });

      test('tampered message fails verification', () {
        final message = utf8.encode('Original');
        final signature = handler.signBytes(message);

        message[0] = message[0] ^ 0xFF;

        expect(
          handler.verifyBytes(
            signature: signature,
            message: message,
            publicKey: testIdentity.publicKey,
          ),
          isFalse,
        );
      });

      test('tampered signature fails verification', () {
        final message = utf8.encode('Data');
        final signature = handler.signBytes(message);

        signature[0] = signature[0] ^ 0xFF;

        expect(
          handler.verifyBytes(
            signature: signature,
            message: message,
            publicKey: testIdentity.publicKey,
          ),
          isFalse,
        );
      });

      test('signature from a different identity fails verification', () async {
        final otherKeyPair = await Ed25519().newKeyPair();
        final otherIdentity = await GrassrootsIdentity.create(
          keyPair: otherKeyPair,
          nickname: 'Other',
        );
        final otherHandler =
            ProtocolHandler(identity: otherIdentity, sodium: sodium);

        final message = utf8.encode('Forged');
        final signature = otherHandler.signBytes(message);

        // Verification against testIdentity's key must fail: the signature
        // was produced by otherIdentity.
        expect(
          handler.verifyBytes(
            signature: signature,
            message: message,
            publicKey: testIdentity.publicKey,
          ),
          isFalse,
        );
      });
    });

    group('round-trip encoding/decoding', () {
      test('announce payload round-trip', () {
        final originalPayload = handler.createAnnouncePayload(
          address: '[2001:db8::a]:8000',
        );
        final decoded = handler.decodeAnnounce(originalPayload);

        // Re-encode with decoded data
        final reEncodedIdentity = GrassrootsIdentity.fromMap({
          'publicKey': decoded.publicKey,
          'privateKey': testIdentity.privateKey,
          'nickname': decoded.nickname,
        });
        final reEncodedHandler =
            ProtocolHandler(identity: reEncodedIdentity, sodium: sodium);
        final reEncodedPayload = reEncodedHandler.createAnnouncePayload(
          address: decoded.udpAddress,
        );

        expect(reEncodedPayload, equals(originalPayload));
      });
    });
  });
}
