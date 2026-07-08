import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart' show Hmac, SecretKey;

import 'package:bootstrap_anchor/src/address_table.dart';
import 'package:bootstrap_anchor/src/identity.dart';
import 'package:bootstrap_anchor/src/invite_table.dart';
import 'package:bootstrap_anchor/src/peer_table.dart';
import 'package:bootstrap_anchor/src/protocol.dart';
import 'package:bootstrap_anchor/src/signaling_codec.dart';
import 'package:bootstrap_anchor/src/signaling_handler.dart';
import 'package:test/test.dart';

Future<Protocol> _createProtocol() async {
  final identity = await AnchorIdentity.generate(nickname: 'anchor');
  return Protocol(identity: identity);
}

Uint8List _pubkey(int seed) =>
    Uint8List.fromList(List<int>.generate(32, (i) => (seed + i) & 0xff));

String _hex(Uint8List bytes) =>
    bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

class _SentSignal {
  final Uint8List recipient;
  final SignalingMessage message;

  const _SentSignal(this.recipient, this.message);
}

void main() {
  group('AddressTable', () {
    test('stores separate IPv4 and IPv6 entries per pubkey', () {
      final table = AddressTable();
      final pubkeyHex = _hex(_pubkey(1));

      table.register(pubkeyHex, '2001:db8::20', 9000);
      table.register(pubkeyHex, '203.0.113.20', 9001);

      final ipv6Entry = table.lookup(
        pubkeyHex,
        family: InternetAddressType.IPv6,
      );
      final ipv4Entry = table.lookup(
        pubkeyHex,
        family: InternetAddressType.IPv4,
      );

      expect(ipv6Entry, isNotNull);
      expect(ipv6Entry!.ip, equals('2001:db8::20'));
      expect(ipv6Entry.port, equals(9000));
      expect(ipv4Entry, isNotNull);
      expect(ipv4Entry!.ip, equals('203.0.113.20'));
      expect(ipv4Entry.port, equals(9001));
      expect(table.length, equals(2));
    });

    test('removeStale preserves protected pubkeys', () {
      final table = AddressTable();
      final livePubkeyHex = _hex(_pubkey(1));
      final stalePubkeyHex = _hex(_pubkey(2));

      table.register(livePubkeyHex, '2001:db8::20', 9000);
      table.register(stalePubkeyHex, '2001:db8::21', 9001);

      table.removeStale(
        Duration.zero,
        protectedPubkeys: {livePubkeyHex},
      );

      expect(table.lookup(livePubkeyHex), isNotNull);
      expect(table.lookup(stalePubkeyHex), isNull);
      expect(table.length, equals(1));
    });
  });

  group('SignalingHandler', () {
    late AddressTable addressTable;
    late PeerTable peerTable;
    late InviteTable inviteTable;
    late SignalingCodec codec;
    late SignalingHandler handler;
    late List<_SentSignal> sentSignals;
    late Uint8List aPubkey;
    late Uint8List bPubkey;

    setUp(() async {
      addressTable = AddressTable();
      peerTable = PeerTable();
      inviteTable = InviteTable();
      codec = const SignalingCodec();
      aPubkey = _pubkey(1);
      bPubkey = _pubkey(2);
      handler = SignalingHandler(
        protocol: await _createProtocol(),
        peerTable: peerTable,
        addressTable: addressTable,
        inviteTable: inviteTable,
        codec: codec,
      );
      sentSignals = <_SentSignal>[];
      handler.sendSignaling = (recipientPubkey, payload) async {
        sentSignals.add(_SentSignal(recipientPubkey, codec.decode(payload)));
        return true;
      };

      peerTable.addVerified(_hex(aPubkey), nickname: 'A');
      peerTable.addVerified(_hex(bPubkey), nickname: 'B');
    });

    test('processAnnounce reflects the observed address back', () {
      final announce = AnnounceData(
        publicKey: aPubkey,
        nickname: 'A',
        protocolVersion: 1,
        udpAddress: '[2001:db8::99]:7000',
      );

      handler.processAnnounce(
        announce,
        observedIp: '2001:db8::10',
        observedPort: 7001,
      );

      expect(
        addressTable.lookup(_hex(aPubkey)),
        isNull,
        reason: 'processAnnounce must not touch the address table',
      );

      expect(sentSignals, hasLength(1));
      expect(sentSignals.single.recipient, equals(aPubkey));
      final reflect = sentSignals.single.message as AddrReflectMessage;
      expect(reflect.ip, equals('2001:db8::10'));
      expect(reflect.port, equals(7001));
    });

    test(
      'first request is parked until counterpart arrives, then both '
      'sides receive a PunchInitiate',
      () {
        // A (whose IP changed) sends RECONNECT first.
        handler.processSignaling(
          aPubkey,
          codec.encode(ReconnectMessage(
            initiatorPubkey: aPubkey,
            peerPubkey: bPubkey,
          )),
          observedIp: '198.51.100.10',
          observedPort: 7000,
        );
        expect(sentSignals, isEmpty,
            reason: 'no counterpart yet — server must park the request');
        expect(
          addressTable.lookup(_hex(aPubkey)),
          isNotNull,
          reason: 'sender address recorded from observed source',
        );

        // B (who detected A went silent) sends AVAILABLE — match!
        handler.processSignaling(
          bPubkey,
          codec.encode(AvailableMessage(peerPubkey: aPubkey)),
          observedIp: '203.0.113.20',
          observedPort: 9001,
        );

        expect(sentSignals, hasLength(2),
            reason: 'both sides must receive a PunchInitiate');

        final toA = sentSignals.firstWhere((s) => s.recipient == aPubkey);
        final initiateToA = toA.message as PunchInitiateMessage;
        expect(initiateToA.peerPubkey, equals(bPubkey));
        expect(initiateToA.ip, equals('203.0.113.20'));
        expect(initiateToA.port, equals(9001));

        final toB = sentSignals.firstWhere((s) => s.recipient == bPubkey);
        final initiateToB = toB.message as PunchInitiateMessage;
        expect(initiateToB.peerPubkey, equals(aPubkey));
        expect(initiateToB.ip, equals('198.51.100.10'));
        expect(initiateToB.port, equals(7000));
      },
    );

    test('matching is symmetric — AVAILABLE arriving first also works', () {
      handler.processSignaling(
        bPubkey,
        codec.encode(AvailableMessage(peerPubkey: aPubkey)),
        observedIp: '203.0.113.20',
        observedPort: 9001,
      );
      expect(sentSignals, isEmpty);

      handler.processSignaling(
        aPubkey,
        codec.encode(ReconnectMessage(
          initiatorPubkey: aPubkey,
          peerPubkey: bPubkey,
        )),
        observedIp: '198.51.100.10',
        observedPort: 7000,
      );

      expect(sentSignals, hasLength(2));
    });

    test('drops RECONNECT when inner initiator differs from signed sender', () {
      handler.processSignaling(
        aPubkey,
        codec.encode(ReconnectMessage(
          initiatorPubkey: bPubkey,
          peerPubkey: bPubkey,
        )),
        observedIp: '198.51.100.10',
        observedPort: 7000,
      );

      handler.processSignaling(
        bPubkey,
        codec.encode(AvailableMessage(peerPubkey: aPubkey)),
        observedIp: '203.0.113.20',
        observedPort: 9001,
      );

      expect(sentSignals, isEmpty);
    });

    test('forwards PUNCH_READY to the counterpart after coordination', () {
      handler.processSignaling(
        aPubkey,
        codec.encode(ReconnectMessage(
          initiatorPubkey: aPubkey,
          peerPubkey: bPubkey,
        )),
        observedIp: '198.51.100.10',
        observedPort: 7000,
      );
      handler.processSignaling(
        bPubkey,
        codec.encode(AvailableMessage(peerPubkey: aPubkey)),
        observedIp: '203.0.113.20',
        observedPort: 9001,
      );
      sentSignals.clear();

      handler.processSignaling(
        aPubkey,
        codec.encode(PunchReadyMessage(peerPubkey: bPubkey)),
        observedIp: '198.51.100.10',
        observedPort: 7000,
      );

      expect(sentSignals, hasLength(1));
      expect(sentSignals.single.recipient, equals(bPubkey));
      expect(sentSignals.single.message, isA<PunchReadyMessage>());
    });

    test('drops requests with sender targeting itself', () {
      handler.processSignaling(
        aPubkey,
        codec.encode(ReconnectMessage(
          initiatorPubkey: aPubkey,
          peerPubkey: aPubkey,
        )),
        observedIp: '198.51.100.10',
        observedPort: 7000,
      );
      expect(sentSignals, isEmpty);
    });

    test('drops duplicate coordination attempts inside the cooldown window',
        () {
      handler.processSignaling(
        aPubkey,
        codec.encode(ReconnectMessage(
          initiatorPubkey: aPubkey,
          peerPubkey: bPubkey,
        )),
        observedIp: '198.51.100.10',
        observedPort: 7000,
      );
      handler.processSignaling(
        bPubkey,
        codec.encode(AvailableMessage(peerPubkey: aPubkey)),
        observedIp: '203.0.113.20',
        observedPort: 9001,
      );
      expect(sentSignals, hasLength(2));
      sentSignals.clear();

      // Retry immediately — still inside cooldown. Server should drop it.
      handler.processSignaling(
        aPubkey,
        codec.encode(ReconnectMessage(
          initiatorPubkey: aPubkey,
          peerPubkey: bPubkey,
        )),
        observedIp: '198.51.100.11',
        observedPort: 7001,
      );
      expect(sentSignals, isEmpty);
    });
  });

  group('Invite registration and redemption', () {
    late InviteTable inviteTable;
    late SignalingCodec codec;
    late SignalingHandler handler;
    late List<_SentSignal> sentSignals;
    late Uint8List aPubkey; // inviter
    late Uint8List bPubkey; // redeemer
    late Uint8List inviteId;
    late Uint8List inviteKey;
    late Uint8List redeemerNonce;

    /// Pump the event loop so the handler's unawaited async redemption
    /// (HMAC verification) completes.
    Future<void> settle() =>
        Future<void>.delayed(const Duration(milliseconds: 20));

    Future<Uint8List> claimMac({
      Uint8List? inviter,
      Uint8List? redeemer,
      Uint8List? key,
    }) async {
      final mac = await Hmac.sha256().calculateMac(
        <int>[
          ...(inviter ?? aPubkey),
          ...(redeemer ?? bPubkey),
          ...inviteId,
          ...redeemerNonce,
        ],
        secretKey: SecretKey(key ?? inviteKey),
      );
      return Uint8List.fromList(mac.bytes);
    }

    int futureExpiry() =>
        DateTime.now().add(const Duration(hours: 1)).millisecondsSinceEpoch;

    void register({int? expiresAtMs}) {
      handler.processSignaling(
        aPubkey,
        codec.encode(RegisterInviteMessage(
          inviteId: inviteId,
          inviteKey: inviteKey,
          expiresAtMs: expiresAtMs ?? futureExpiry(),
        )),
      );
    }

    Future<void> redeem({Uint8List? mac, Uint8List? sender}) async {
      handler.processSignaling(
        sender ?? bPubkey,
        codec.encode(RedeemInviteMessage(
          inviteId: inviteId,
          redeemerNonce: redeemerNonce,
          mac: mac ?? await claimMac(),
        )),
      );
      await settle();
    }

    setUp(() async {
      inviteTable = InviteTable();
      codec = const SignalingCodec();
      aPubkey = _pubkey(1);
      bPubkey = _pubkey(2);
      inviteId = _pubkey(0xA0);
      inviteKey = _pubkey(0xB0);
      redeemerNonce = _pubkey(0xC0);
      handler = SignalingHandler(
        protocol: await _createProtocol(),
        peerTable: PeerTable(),
        addressTable: AddressTable(),
        inviteTable: inviteTable,
        codec: codec,
      );
      sentSignals = <_SentSignal>[];
      handler.sendSignaling = (recipientPubkey, payload) async {
        sentSignals.add(_SentSignal(recipientPubkey, codec.decode(payload)));
        return true;
      };
    });

    test('valid redemption flips the invite and notifies the inviter', () async {
      register();
      expect(inviteTable.lookup(_hex(inviteId)), isNotNull);

      await redeem();

      final entry = inviteTable.lookup(_hex(inviteId))!;
      expect(entry.used, isTrue);
      expect(sentSignals, hasLength(1));
      expect(sentSignals.single.recipient, equals(aPubkey));
      final notif = sentSignals.single.message as InviteRedeemedMessage;
      expect(notif.inviteId, equals(inviteId));
      expect(notif.redeemerPubkey, equals(bPubkey));
    });

    test('second redemption of the same invite is rejected (single use)',
        () async {
      register();
      await redeem();
      await redeem(sender: _pubkey(3), mac: await claimMac(redeemer: _pubkey(3)));

      expect(sentSignals, hasLength(1)); // only the first notification
    });

    test('redemption with a bad MAC is rejected and the invite stays unused',
        () async {
      register();
      await redeem(mac: await claimMac(key: _pubkey(0xEE)));

      expect(inviteTable.lookup(_hex(inviteId))!.used, isFalse);
      expect(sentSignals, isEmpty);
    });

    test('MAC binds the redeemer: replay by another sender is rejected',
        () async {
      register();
      // Valid MAC computed for B, but submitted by C.
      await redeem(sender: _pubkey(3), mac: await claimMac(redeemer: bPubkey));

      expect(inviteTable.lookup(_hex(inviteId))!.used, isFalse);
      expect(sentSignals, isEmpty);
    });

    test('unknown and expired invites are rejected', () async {
      // Never registered.
      await redeem();
      expect(sentSignals, isEmpty);

      // Registered with an already-past expiry: dropped at registration.
      register(
          expiresAtMs:
              DateTime.now().millisecondsSinceEpoch - 1000);
      expect(inviteTable.lookup(_hex(inviteId)), isNull);
    });

    test('registration of an existing id by a different sender is rejected',
        () {
      register();
      handler.processSignaling(
        bPubkey,
        codec.encode(RegisterInviteMessage(
          inviteId: inviteId,
          inviteKey: _pubkey(0xDD),
          expiresAtMs: futureExpiry(),
        )),
      );

      final entry = inviteTable.lookup(_hex(inviteId))!;
      expect(entry.inviterPubkey, equals(aPubkey));
      expect(entry.inviteKey, equals(inviteKey));
    });

    test('inviter cannot redeem its own invite', () async {
      register();
      await redeem(sender: aPubkey, mac: await claimMac(redeemer: aPubkey));

      expect(inviteTable.lookup(_hex(inviteId))!.used, isFalse);
      expect(sentSignals, isEmpty);
    });

    test('notification is retained until acked, resent on retry, then stops',
        () async {
      register();
      handler.sendSignaling = (recipientPubkey, payload) async => false;
      await redeem();

      final entry = inviteTable.lookup(_hex(inviteId))!;
      expect(entry.used, isTrue);
      expect(entry.undeliveredRedeemer, equals(bPubkey));

      // Inviter comes back online; a cleanup sweep resends the notification.
      handler.sendSignaling = (recipientPubkey, payload) async {
        sentSignals.add(_SentSignal(recipientPubkey, codec.decode(payload)));
        return true;
      };
      await handler.retryUndeliveredInviteNotifications();
      // A successful write is NOT proof of receipt — still retained.
      expect(entry.undeliveredRedeemer, equals(bPubkey));
      expect(sentSignals, hasLength(1));

      // Inviter acks: the notification is cleared and no longer resent.
      handler.processSignaling(
        aPubkey,
        codec.encode(InviteRedeemedAckMessage(inviteId: inviteId)),
      );
      expect(entry.undeliveredRedeemer, isNull);
      await handler.retryUndeliveredInviteNotifications();
      expect(sentSignals, hasLength(1)); // no further resend
    });

    test('only the inviter can ack a redeemed notification', () async {
      register();
      handler.sendSignaling = (recipientPubkey, payload) async => false;
      await redeem();
      final entry = inviteTable.lookup(_hex(inviteId))!;

      // A stranger's ack is ignored.
      handler.processSignaling(
        _pubkey(9),
        codec.encode(InviteRedeemedAckMessage(inviteId: inviteId)),
      );
      expect(entry.undeliveredRedeemer, equals(bPubkey));
    });

    test('per-inviter registration cap rejects a flood', () {
      // Distinct 32-byte ids (the _pubkey helper only yields 256 values).
      String idHex(int n) {
        final b = Uint8List(32);
        b[0] = (n >> 8) & 0xff;
        b[1] = n & 0xff;
        return _hex(b);
      }

      for (var i = 0; i < InviteTable.maxInvitesPerInviter; i++) {
        expect(
          inviteTable.register(
            inviteIdHex: idHex(i),
            inviterPubkey: aPubkey,
            inviteKey: _pubkey(0xB0),
            expiresAtMs: futureExpiry(),
          ),
          isTrue,
        );
      }
      // One more from A is rejected...
      expect(
        inviteTable.register(
          inviteIdHex: idHex(9000),
          inviterPubkey: aPubkey,
          inviteKey: _pubkey(0xB0),
          expiresAtMs: futureExpiry(),
        ),
        isFalse,
      );
      // ...while a different inviter is unaffected.
      expect(
        inviteTable.register(
          inviteIdHex: idHex(9001),
          inviterPubkey: bPubkey,
          inviteKey: _pubkey(0xB0),
          expiresAtMs: futureExpiry(),
        ),
        isTrue,
      );
    });

    test('expired invites are pruned and free per-inviter capacity', () {
      register(
          expiresAtMs:
              DateTime.now().millisecondsSinceEpoch + 50);
      expect(inviteTable.count, equals(1));
      inviteTable.removeExpired(
          now: DateTime.now().add(const Duration(seconds: 1)));
      expect(inviteTable.count, equals(0));
      // Capacity freed: A can register again.
      expect(
        inviteTable.register(
          inviteIdHex: _hex(_pubkey(0xAB)),
          inviterPubkey: aPubkey,
          inviteKey: _pubkey(0xB0),
          expiresAtMs: futureExpiry(),
        ),
        isTrue,
      );
    });
  });
}
