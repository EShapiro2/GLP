import 'dart:convert';
import 'dart:typed_data';
import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking_core/src/protocol/protocol_handler.dart';
import 'package:grassroots_networking_core/src/protocol/layer_version.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
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

        // The version declaration (next 9 bytes; GNA api.tex, "Versions")
        expect(VersionDeclaration.decode(payload, 32),
            VersionDeclaration.smartphone);
      });

      test(
          'is the same fixed-length record for any identity: the identity is '
          'the key pair and carries nothing else to include', () async {
        final algorithm = Ed25519();
        final keyPair = await algorithm.newKeyPair();
        final anyIdentity = await GrassrootsIdentity.create(
          keyPair: keyPair,
        );
        final anyHandler =
            ProtocolHandler(identity: anyIdentity, sodium: sodium);

        final payload = anyHandler.createAnnouncePayload();
        expect(payload.length, equals(announcePayloadLength));
      });
    });

    group('decodeAnnounce', () {
      test('decodes announce payload created by createAnnouncePayload', () {
        final payload = handler.createAnnouncePayload();
        final decoded = handler.decodeAnnounce(payload);

        expect(decoded.publicKey, equals(testIdentity.publicKey));
        expect(decoded.declaration, VersionDeclaration.smartphone);
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
        );
        final otherHandler =
            ProtocolHandler(identity: otherIdentity, sodium: sodium);

        // Well-formed record carrying testIdentity's key + version...
        final record = BytesBuilder()..add(testIdentity.publicKey);
        record.add(VersionDeclaration.smartphone.encode());
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

    // GNA api.tex, "Versions": "Each peer checks that the version the other
    // works with lies between its own earliest compatible version and its own
    // version ... otherwise the connection is refused."
    group('versions (GNA, "Versions")', () {
      /// An ANNOUNCE of ours carrying [declared] in place of our declaration,
      /// re-signed: structurally valid.
      Uint8List announcing(VersionDeclaration declared) {
        final record = Uint8List.fromList(
            [...testIdentity.publicKey, ...declared.encode()]);
        return Uint8List.fromList([...record, ...handler.signBytes(record)]);
      }

      test('a server one version ahead that still works with our version is '
          'accepted, and we talk to it as the version it works with', () {
        final decoded = handler.decodeAnnounce(announcing(
            const VersionDeclaration(
                kind: LayerKind.server,
                version: 2,
                earliest: 1,
                worksWithSmartphone: 1,
                worksWithServer: 2)));
        expect(decoded.declaration.version, 2);
        expect(decoded.talksAs, 1);
      });

      test('a peer that works with a smartphone version we are not between '
          'our earliest and our version is refused', () {
        for (final w in [0, 2]) {
          expect(
            () => handler.decodeAnnounce(announcing(VersionDeclaration(
                kind: LayerKind.server,
                version: 2,
                earliest: 2,
                worksWithSmartphone: w,
                worksWithServer: 2))),
            throwsA(isA<FormatException>().having(
                (e) => e.message, 'message', contains('refused'))),
            reason: 'works with smartphone $w, we are 1 back to 1',
          );
        }
      });

      test('an ANNOUNCE carrying our own declaration is accepted', () {
        final decoded = handler.decodeAnnounce(handler.createAnnouncePayload());
        expect(decoded.declaration, VersionDeclaration.smartphone);
        expect(decoded.talksAs, VersionDeclaration.smartphoneVersion);
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
        });
        final reEncodedHandler =
            ProtocolHandler(identity: reEncodedIdentity, sodium: sodium);
        final reEncodedPayload = reEncodedHandler.createAnnouncePayload();

        expect(reEncodedPayload, equals(originalPayload));
      });
    });
  });
}
