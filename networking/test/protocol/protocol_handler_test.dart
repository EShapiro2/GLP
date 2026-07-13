import 'dart:convert';
import 'dart:typed_data';
import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking_core/src/protocol/protocol_handler.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/models/packet.dart';
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
      test('encodes exactly pubkey + version + signature', () {
        final payload = handler.createAnnouncePayload();

        // The irreducible record (spec §ANNOUNCE and Liveness):
        // pubkey(32) + version(2) + signature(64).
        expect(payload.length, equals(announcePayloadLength));

        // Public key (first 32 bytes)
        final pubkeyFromPayload = payload.sublist(0, 32);
        expect(pubkeyFromPayload, equals(testIdentity.publicKey));

        // Protocol version (next 2 bytes)
        final versionData =
            ByteData.view(payload.buffer, payload.offsetInBytes + 32, 2);
        final version = versionData.getUint16(0, Endian.big);
        expect(version, equals(ProtocolHandler.protocolVersion));
      });

      test('carries no nickname regardless of the identity nickname', () async {
        final algorithm = Ed25519();
        final keyPair = await algorithm.newKeyPair();
        final namedIdentity = await GrassrootsIdentity.create(
          keyPair: keyPair,
          nickname: 'Zoë 🌱🚀 名字',
        );
        final namedHandler =
            ProtocolHandler(identity: namedIdentity, sodium: sodium);

        final payload = namedHandler.createAnnouncePayload();
        expect(payload.length, equals(announcePayloadLength));
      });
    });

    group('decodeAnnounce', () {
      test('decodes announce payload created by createAnnouncePayload', () {
        final payload = handler.createAnnouncePayload();
        final decoded = handler.decodeAnnounce(payload);

        expect(decoded.publicKey, equals(testIdentity.publicKey));
        expect(decoded.protocolVersion,
            equals(ProtocolHandler.protocolVersion));
      });

      test('throws on a payload longer than the fixed record', () {
        final payload = handler.createAnnouncePayload();
        final extended = Uint8List.fromList([...payload, 0x00]);

        expect(
          () => handler.decodeAnnounce(extended),
          throwsA(isA<FormatException>()),
        );
      });

      test('throws when a signed payload byte is tampered', () {
        final payload = handler.createAnnouncePayload();

        // Flip a bit inside the version field — structure still parses, but
        // the trailing signature no longer covers the bytes.
        payload[33] ^= 0xFF;

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

      test('verifies the signature against the CARRIED key: a record '
          'claiming one identity but signed by another is rejected', () async {
        final otherKeyPair = await Ed25519().newKeyPair();
        final otherIdentity = await GrassrootsIdentity.create(
          keyPair: otherKeyPair,
          nickname: 'Other',
        );
        final otherHandler =
            ProtocolHandler(identity: otherIdentity, sodium: sodium);

        // Well-formed record carrying testIdentity's key + version...
        final record = BytesBuilder()..add(testIdentity.publicKey);
        final versionBytes = ByteData(2)
          ..setUint16(0, ProtocolHandler.protocolVersion, Endian.big);
        record.add(versionBytes.buffer.asUint8List());
        final recordBytes = record.toBytes();

        // ...but signed under a different identity. Forged: an ANNOUNCE whose
        // trailing signature does not verify against the pubkey it carries
        // must not identify anyone.
        final forged = Uint8List.fromList(
            [...recordBytes, ...otherHandler.signBytes(recordBytes)]);
        expect(forged.length, equals(announcePayloadLength));

        expect(
          () => handler.decodeAnnounce(forged),
          throwsA(isA<FormatException>()),
        );
      });
    });

    group('protocol version refusal', () {
      test('an ANNOUNCE carrying a different protocol version is refused',
          () async {
        // Build a payload identical to ours but with version+1, re-signed —
        // structurally valid, wrong protocol (spec: the wire changed
        // incompatibly; mixed versions are refused at ANNOUNCE).
        final own = handler.createAnnouncePayload();
        final tampered = Uint8List.fromList(own.sublist(0, 34));
        final view = ByteData.view(tampered.buffer);
        view.setUint16(32, ProtocolHandler.protocolVersion + 1, Endian.big);
        final resigned = Uint8List.fromList(
            [...tampered, ...handler.signBytes(tampered)]);

        expect(
          () => handler.decodeAnnounce(resigned),
          throwsA(isA<FormatException>().having(
              (e) => e.message, 'message', contains('refused'))),
        );
      });

      test('an ANNOUNCE carrying our protocol version is accepted', () {
        final decoded = handler.decodeAnnounce(handler.createAnnouncePayload());
        expect(decoded.protocolVersion, ProtocolHandler.protocolVersion);
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
        final originalPayload = handler.createAnnouncePayload();
        final decoded = handler.decodeAnnounce(originalPayload);

        // Re-encode with decoded data
        final reEncodedIdentity = GrassrootsIdentity.fromMap({
          'publicKey': decoded.publicKey,
          'privateKey': testIdentity.privateKey,
          'nickname': 'TestUser',
        });
        final reEncodedHandler =
            ProtocolHandler(identity: reEncodedIdentity, sodium: sodium);
        final reEncodedPayload = reEncodedHandler.createAnnouncePayload();

        expect(reEncodedPayload, equals(originalPayload));
      });
    });
  });
}
