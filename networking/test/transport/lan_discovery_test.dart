import 'dart:convert' show utf8;
import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:cryptography/cryptography.dart';
import 'package:cryptography/dart.dart' show DartSha256;
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking/src/transport/lan_discovery_service.dart';

/// Tests for the LAN recognition token (spec `GLP_Networking_API`
/// §Proximity Concepts and Principles, §LAN): the advertised DNS-SD
/// instance name is the first 8 bytes, hex-encoded, of
/// SHA-256("glp lan suffix" | pk | T), with the same 15-minute slot T
/// as the BLE suffix; a peer who knows pk matches the current and
/// adjacent slots.
void main() {
  late GrassrootsIdentity identity;

  setUp(() async {
    final keyPair = await Ed25519().newKeyPair();
    identity = await GrassrootsIdentity.create(
      keyPair: keyPair,
      nickname: 'Alice',
    );
  });

  group('LAN token derivation', () {
    test('is the first 8 bytes of SHA-256("glp lan suffix" | pk | T) in hex',
        () {
      const slot = 12345;
      final input = <int>[
        ...utf8.encode('glp lan suffix'),
        ...identity.publicKey,
        for (var i = 7; i >= 0; i--) (slot >> (8 * i)) & 0xff,
      ];
      final expected = const DartSha256()
          .hashSync(input)
          .bytes
          .sublist(0, 8)
          .map((b) => b.toRadixString(16).padLeft(2, '0'))
          .join();
      expect(
        GrassrootsIdentity.deriveLanTokenForSlot(identity.publicKey, slot),
        equals(expected),
      );
    });

    test('is 8 bytes hex-encoded (16 hex chars)', () {
      final token =
          GrassrootsIdentity.deriveLanTokenForSlot(identity.publicKey, 7);
      expect(token, hasLength(16));
      expect(token, matches(RegExp(r'^[0-9a-f]{16}$')));
    });

    test('is deterministic per (pubkey, slot)', () {
      expect(
        GrassrootsIdentity.deriveLanTokenForSlot(identity.publicKey, 42),
        equals(
            GrassrootsIdentity.deriveLanTokenForSlot(identity.publicKey, 42)),
      );
    });

    test('rotates: different slots produce different tokens', () {
      expect(
        GrassrootsIdentity.deriveLanTokenForSlot(identity.publicKey, 100),
        isNot(equals(
            GrassrootsIdentity.deriveLanTokenForSlot(identity.publicKey, 101))),
      );
    });

    test('differs from the BLE suffix for the same (pubkey, slot) — '
        'per-medium label', () {
      const slot = 4242;
      final lan =
          GrassrootsIdentity.deriveLanTokenForSlot(identity.publicKey, slot);
      final bleUuid = GrassrootsIdentity.deriveServiceUuidForSlot(
          identity.publicKey, slot);
      final bleSuffix = bleUuid.replaceAll('-', '').substring(16);
      expect(lan, isNot(equals(bleSuffix)));
    });

    test('different identities produce different tokens', () async {
      final other = await GrassrootsIdentity.create(
        keyPair: await Ed25519().newKeyPair(),
        nickname: 'Bob',
      );
      expect(
        GrassrootsIdentity.deriveLanTokenForSlot(identity.publicKey, 9),
        isNot(equals(
            GrassrootsIdentity.deriveLanTokenForSlot(other.publicKey, 9))),
      );
    });

    test('lanToken getter is the current-slot derivation', () {
      final before = GrassrootsIdentity.currentBleSlot();
      final token = identity.lanToken;
      final after = GrassrootsIdentity.currentBleSlot();
      expect(
        {
          for (var s = before; s <= after; s++)
            GrassrootsIdentity.deriveLanTokenForSlot(identity.publicKey, s),
        },
        contains(token),
      );
    });

    test('candidateLanTokens covers previous, current, and next slot', () {
      final now = DateTime.fromMillisecondsSinceEpoch(
          1000 * GrassrootsIdentity.bleSlotDuration.inSeconds * 7000);
      final slot = GrassrootsIdentity.currentBleSlot(now: now);
      final candidates =
          GrassrootsIdentity.candidateLanTokens(identity.publicKey, now: now);
      expect(candidates, hasLength(3));
      for (var d = -1; d <= 1; d++) {
        expect(
          candidates,
          contains(GrassrootsIdentity.deriveLanTokenForSlot(
              identity.publicKey, slot + d)),
        );
      }
    });

    test('rejects a short public key', () {
      expect(
        () => GrassrootsIdentity.deriveLanTokenForSlot(
            Uint8List.fromList(List.filled(16, 1)), 0),
        throwsArgumentError,
      );
    });
  });

  group('matchLanToken', () {
    late GrassrootsIdentity other;

    setUp(() async {
      other = await GrassrootsIdentity.create(
        keyPair: await Ed25519().newKeyPair(),
        nickname: 'Bob',
      );
    });

    test('recognizes a known peer by its current-slot token', () {
      final now = DateTime.fromMillisecondsSinceEpoch(
          1000 * GrassrootsIdentity.bleSlotDuration.inSeconds * 5000);
      final slot = GrassrootsIdentity.currentBleSlot(now: now);
      final token =
          GrassrootsIdentity.deriveLanTokenForSlot(other.publicKey, slot);
      expect(
        matchLanToken(token, [identity.publicKey, other.publicKey], now: now),
        equals(other.publicKey),
      );
    });

    test('recognizes adjacent-slot tokens (clock skew, late rotation)', () {
      final now = DateTime.fromMillisecondsSinceEpoch(
          1000 * GrassrootsIdentity.bleSlotDuration.inSeconds * 5000);
      final slot = GrassrootsIdentity.currentBleSlot(now: now);
      for (var d = -1; d <= 1; d++) {
        final token = GrassrootsIdentity.deriveLanTokenForSlot(
            other.publicKey, slot + d);
        expect(
          matchLanToken(token, [other.publicKey], now: now),
          equals(other.publicKey),
          reason: 'slot offset $d must match',
        );
      }
    });

    test('does not match a token two slots away', () {
      final now = DateTime.fromMillisecondsSinceEpoch(
          1000 * GrassrootsIdentity.bleSlotDuration.inSeconds * 5000);
      final slot = GrassrootsIdentity.currentBleSlot(now: now);
      final token =
          GrassrootsIdentity.deriveLanTokenForSlot(other.publicKey, slot + 2);
      expect(matchLanToken(token, [other.publicKey], now: now), isNull);
    });

    test('returns null for an unknown token', () {
      expect(
        matchLanToken('deadbeefdeadbeef', [identity.publicKey]),
        isNull,
      );
    });

    test('matches case-insensitively (instance names may be upcased)', () {
      final now = DateTime.fromMillisecondsSinceEpoch(
          1000 * GrassrootsIdentity.bleSlotDuration.inSeconds * 5000);
      final slot = GrassrootsIdentity.currentBleSlot(now: now);
      final token = GrassrootsIdentity.deriveLanTokenForSlot(
          other.publicKey, slot);
      expect(
        matchLanToken(token.toUpperCase(), [other.publicKey], now: now),
        equals(other.publicKey),
      );
    });
  });
}
