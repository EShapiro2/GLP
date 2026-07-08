import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking/src/signaling/peer_link.dart';
import 'package:grassroots_networking/src/signaling/signaling_codec.dart';

Uint8List _bytes(int seed, [int length = 32]) =>
    Uint8List.fromList(List<int>.generate(length, (i) => (seed + i) & 0xff));

PeerLinkInvite _invite({
  Uint8List? inviter,
  Uint8List? rvPubkey,
  String rvAddress = '[2001:db8::1]:9516',
  Uint8List? nonce,
  int? expiresAtMs,
  Uint8List? signature,
}) =>
    PeerLinkInvite(
      inviterPubkey: inviter ?? _bytes(1),
      rvPubkey: rvPubkey ?? _bytes(2),
      rvAddress: rvAddress,
      nonce: nonce ?? _bytes(3),
      expiresAtMs: expiresAtMs ??
          DateTime.now().add(const Duration(hours: 1)).millisecondsSinceEpoch,
      signature: signature ?? _bytes(4, 64),
    );

void main() {
  group('PeerLinkInvite URI codec', () {
    test('round-trips through the grassroots://invite URI', () {
      final invite = _invite();
      final parsed = PeerLinkInvite.fromUri(invite.toUri());

      expect(parsed.inviterPubkey, equals(invite.inviterPubkey));
      expect(parsed.rvPubkey, equals(invite.rvPubkey));
      expect(parsed.rvAddress, equals(invite.rvAddress));
      expect(parsed.nonce, equals(invite.nonce));
      expect(parsed.expiresAtMs, equals(invite.expiresAtMs));
      expect(parsed.signature, equals(invite.signature));
    });

    test('URI has the expected scheme/host and stripped padding', () {
      final uri = _invite().toUri();
      expect(uri, startsWith('grassroots://invite/'));
      expect(uri.contains('='), isFalse);
    });

    test('rejects non-invite URIs', () {
      expect(() => PeerLinkInvite.fromUri('https://example.com/x'),
          throwsFormatException);
      expect(() => PeerLinkInvite.fromUri('grassroots://other/abc'),
          throwsFormatException);
      expect(() => PeerLinkInvite.fromUri('grassroots://invite/!!!'),
          throwsFormatException);
    });

    test('rejects truncated, trailing, and unknown-version records', () {
      final bytes = _invite().encode();
      expect(() => PeerLinkInvite.decode(bytes.sublist(0, bytes.length - 1)),
          throwsFormatException);
      expect(
          () => PeerLinkInvite.decode(
              Uint8List.fromList(<int>[...bytes, 0x00])),
          throwsFormatException);
      final wrongVersion = Uint8List.fromList(bytes);
      wrongVersion[0] = 0x7f;
      expect(
          () => PeerLinkInvite.decode(wrongVersion), throwsFormatException);
    });

    test('isExpired follows expiresAtMs', () {
      expect(
          _invite(
                  expiresAtMs: DateTime.now()
                      .subtract(const Duration(seconds: 1))
                      .millisecondsSinceEpoch)
              .isExpired,
          isTrue);
      expect(_invite().isExpired, isFalse);
    });
  });

  group('Invite derivations', () {
    test('deriveInviteId is deterministic and nonce-sensitive', () {
      expect(deriveInviteId(_bytes(3)), equals(deriveInviteId(_bytes(3))));
      expect(deriveInviteId(_bytes(3)),
          isNot(equals(deriveInviteId(_bytes(4)))));
      expect(deriveInviteId(_bytes(3)), hasLength(32));
    });

    test('deriveInviteKey binds every RvServer/expiry input', () async {
      Future<Uint8List> key({
        Uint8List? nonce,
        Uint8List? inviter,
        Uint8List? rvKey,
        String rvAddress = 'a:1',
        int expiresAtMs = 1000,
      }) =>
          deriveInviteKey(
            nonce: nonce ?? _bytes(3),
            inviterPubkey: inviter ?? _bytes(1),
            rvPubkey: rvKey ?? _bytes(2),
            rvAddress: rvAddress,
            expiresAtMs: expiresAtMs,
          );

      final base = await key();
      expect(base, equals(await key())); // deterministic
      expect(base, isNot(equals(await key(nonce: _bytes(9)))));
      expect(base, isNot(equals(await key(inviter: _bytes(9)))));
      expect(base, isNot(equals(await key(rvKey: _bytes(9)))));
      expect(base, isNot(equals(await key(rvAddress: 'b:2'))));
      expect(base, isNot(equals(await key(expiresAtMs: 2000))));
    });

    test('computeRedemptionMac matches a manual HMAC-SHA256', () async {
      final inviteKey = _bytes(5);
      final mac = await computeRedemptionMac(
        inviteKey: inviteKey,
        inviterPubkey: _bytes(1),
        redeemerPubkey: _bytes(2),
        inviteId: _bytes(6),
        redeemerNonce: _bytes(7),
      );
      final manual = await Hmac.sha256().calculateMac(
        <int>[..._bytes(1), ..._bytes(2), ..._bytes(6), ..._bytes(7)],
        secretKey: SecretKey(inviteKey),
      );
      expect(mac, equals(Uint8List.fromList(manual.bytes)));
    });
  });

  group('Invite signing', () {
    test('canonical bytes round-trip a real Ed25519 sign/verify', () async {
      // Pure-Dart Ed25519 (same primitive libsodium implements): proves the
      // signable-byte encoding is stable through the URI round-trip.
      final ed = Ed25519();
      final keyPair = await ed.newKeyPair();
      final publicKey = await keyPair.extractPublicKey();

      final unsigned = _invite(
        inviter: Uint8List.fromList(publicKey.bytes),
        signature: Uint8List(64),
      );
      final sig = await ed.sign(unsigned.signableBytes(), keyPair: keyPair);
      final invite = PeerLinkInvite(
        inviterPubkey: Uint8List.fromList(publicKey.bytes),
        rvPubkey: unsigned.rvPubkey,
        rvAddress: unsigned.rvAddress,
        nonce: unsigned.nonce,
        expiresAtMs: unsigned.expiresAtMs,
        signature: Uint8List.fromList(sig.bytes),
      );

      final parsed = PeerLinkInvite.fromUri(invite.toUri());
      final valid = await ed.verify(
        parsed.signableBytes(),
        signature: Signature(parsed.signature,
            publicKey: SimplePublicKey(parsed.inviterPubkey,
                type: KeyPairType.ed25519)),
      );
      expect(valid, isTrue);

      // Any field tamper breaks the signature.
      final tampered = PeerLinkInvite(
        inviterPubkey: parsed.inviterPubkey,
        rvPubkey: parsed.rvPubkey,
        rvAddress: '[2001:db8::2]:9516',
        nonce: parsed.nonce,
        expiresAtMs: parsed.expiresAtMs,
        signature: parsed.signature,
      );
      final tamperedValid = await ed.verify(
        tampered.signableBytes(),
        signature: Signature(tampered.signature,
            publicKey: SimplePublicKey(tampered.inviterPubkey,
                type: KeyPairType.ed25519)),
      );
      expect(tamperedValid, isFalse);
    });
  });

  group('Invite signaling codec', () {
    const codec = SignalingCodec();

    test('RegisterInvite round-trips', () {
      final msg = RegisterInviteMessage(
        inviteId: _bytes(1),
        inviteKey: _bytes(2),
        expiresAtMs: 0x0102030405060708,
      );
      final decoded = codec.decode(codec.encode(msg)) as RegisterInviteMessage;
      expect(decoded.inviteId, equals(msg.inviteId));
      expect(decoded.inviteKey, equals(msg.inviteKey));
      expect(decoded.expiresAtMs, equals(msg.expiresAtMs));
    });

    test('RedeemInvite round-trips', () {
      final msg = RedeemInviteMessage(
        inviteId: _bytes(1),
        redeemerNonce: _bytes(2),
        mac: _bytes(3),
      );
      final decoded = codec.decode(codec.encode(msg)) as RedeemInviteMessage;
      expect(decoded.inviteId, equals(msg.inviteId));
      expect(decoded.redeemerNonce, equals(msg.redeemerNonce));
      expect(decoded.mac, equals(msg.mac));
    });

    test('InviteRedeemed round-trips', () {
      final msg = InviteRedeemedMessage(
        inviteId: _bytes(1),
        redeemerPubkey: _bytes(2),
      );
      final decoded =
          codec.decode(codec.encode(msg)) as InviteRedeemedMessage;
      expect(decoded.inviteId, equals(msg.inviteId));
      expect(decoded.redeemerPubkey, equals(msg.redeemerPubkey));
    });

    test('InviteRedeemedAck round-trips', () {
      final msg = InviteRedeemedAckMessage(inviteId: _bytes(1));
      final decoded =
          codec.decode(codec.encode(msg)) as InviteRedeemedAckMessage;
      expect(decoded.inviteId, equals(msg.inviteId));
    });

    test('wrong-length payloads are malformed', () {
      final good = codec.encode(RegisterInviteMessage(
        inviteId: _bytes(1),
        inviteKey: _bytes(2),
        expiresAtMs: 1,
      ));
      expect(() => codec.decode(good.sublist(0, good.length - 1)),
          throwsFormatException);
    });
  });
}
