import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking/src/models/identity.dart';
import 'package:grassroots_networking/src/models/packet.dart';
import 'package:grassroots_networking/src/models/peer.dart';
import 'package:grassroots_networking/src/session/noise_session_manager.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';

import '../helpers/sodium_test_bootstrap.dart';

void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  late SodiumSumo sodium;
  setUpAll(() async {
    sodium = await initTestSodium();
  });

  Future<GrassrootsIdentity> identity(String nickname) async {
    return GrassrootsIdentity.create(
      keyPair: await Ed25519().newKeyPair(),
      nickname: nickname,
    );
  }

  GrassrootsPacket handshakePacket(Uint8List payload) {
    return GrassrootsPacket(
      type: PacketType.noiseHandshake,
      payload: payload,
    );
  }

  test('establishes independent BLE and UDP sessions for the same peer',
      () async {
    final alice = await identity('Alice');
    final bob = await identity('Bob');
    final aliceSessions = NoiseSessionManager(identity: alice, sodium: sodium);
    final bobSessions = NoiseSessionManager(identity: bob, sodium: sodium);

    Future<void> completeHandshake(PeerTransport transport) async {
      final msg1 = await aliceSessions.startHandshake(
        transport,
        bob.publicKey,
      );
      expect(msg1, isNotNull);

      final msg2 = await bobSessions.handleHandshakePacket(
        handshakePacket(msg1!),
        transport: transport,
        peerId: 'alice-conn',
      );
      expect(msg2.remotePubkey, equals(alice.publicKey));
      expect(msg2.responsePayload, isNotNull);

      final msg3 = await aliceSessions.handleHandshakePacket(
        handshakePacket(msg2.responsePayload!),
        transport: transport,
        peerId: 'bob-conn',
      );
      expect(msg3.remotePubkey, equals(bob.publicKey));
      expect(msg3.sessionEstablished, isTrue);
      expect(msg3.responsePayload, isNotNull);

      final finished = await bobSessions.handleHandshakePacket(
        handshakePacket(msg3.responsePayload!),
        transport: transport,
        peerId: 'alice-conn',
      );
      expect(finished.remotePubkey, equals(alice.publicKey));
      expect(finished.sessionEstablished, isTrue);
      expect(aliceSessions.hasSession(transport, bob.publicKey), isTrue);
      expect(bobSessions.hasSession(transport, alice.publicKey), isTrue);
    }

    await completeHandshake(PeerTransport.bleDirect);
    await completeHandshake(PeerTransport.udp);

    final clear = GrassrootsPacket(
      type: PacketType.message,
      payload: Uint8List.fromList([1, 2, 3, 4]),
    );

    final bleEncrypted = await aliceSessions.encryptPacket(
      clear,
      transport: PeerTransport.bleDirect,
      remotePubkey: bob.publicKey,
    );
    final udpEncrypted = await aliceSessions.encryptPacket(
      clear,
      transport: PeerTransport.udp,
      remotePubkey: bob.publicKey,
    );

    expect(bleEncrypted.type, PacketType.secureMessage);
    expect(udpEncrypted.type, PacketType.secureMessage);
    expect(bleEncrypted.payload, isNot(equals(clear.payload)));
    expect(udpEncrypted.payload, isNot(equals(bleEncrypted.payload)));

    // BLE resolves the session via the connection→identity map learned at
    // handshake completion; UDP resolves via an explicit remote pubkey.
    final (bleDecrypted, bleSender) = await bobSessions.decryptPacket(
      bleEncrypted,
      transport: PeerTransport.bleDirect,
      peerId: 'alice-conn',
    );
    final (udpDecrypted, udpSender) = await bobSessions.decryptPacket(
      udpEncrypted,
      transport: PeerTransport.udp,
      remotePubkey: alice.publicKey,
    );

    expect(bleDecrypted.type, PacketType.message);
    expect(udpDecrypted.type, PacketType.message);
    expect(bleDecrypted.payload, clear.payload);
    expect(udpDecrypted.payload, clear.payload);
    expect(bleSender, equals(alice.publicKey));
    expect(udpSender, equals(alice.publicKey));
  });

  test(
      'aborts the handshake when the delivered static key does not match the '
      'claimed identity (impersonation)', () async {
    final alice = await identity('Alice');
    final bob = await identity('Bob');
    final mallory = await identity('Mallory');

    final aliceSessions = NoiseSessionManager(identity: alice, sodium: sodium);
    final mallorySessions =
        NoiseSessionManager(identity: mallory, sodium: sodium);

    const transport = PeerTransport.udp;

    // Alice initiates a handshake she believes is with Bob.
    final msg1 = await aliceSessions.startHandshake(transport, bob.publicKey);
    expect(msg1, isNotNull);

    // Mallory intercepts and answers it herself. Her message 2 honestly
    // claims Mallory and delivers Mallory's static.
    final msg2 = await mallorySessions.handleHandshakePacket(
      handshakePacket(msg1!),
      transport: transport,
      peerId: 'alice-conn',
    );
    expect(msg2.responsePayload, isNotNull);

    // In flight, the envelope's identity claim is rewritten to say "Bob".
    // Messages 2/3 bind claim to static via the Noise transcript, not a
    // signature, so the claim bytes (offset 2, after version + msgType) are
    // rewritable — the mismatch must be caught by the static-key check.
    final tampered = Uint8List.fromList(msg2.responsePayload!);
    tampered.setRange(2, 34, bob.publicKey);

    // Alice routes the message to her in-flight handshake with "Bob". The
    // delivered static is Mallory's and does not match the X25519 form of
    // Bob's identity, so Alice aborts and establishes no session.
    final result = await aliceSessions.handleHandshakePacket(
      handshakePacket(tampered),
      transport: transport,
      peerId: 'bob-conn',
    );

    expect(result.sessionEstablished, isFalse);
    expect(result.responsePayload, isNull);
    expect(aliceSessions.hasSession(transport, bob.publicKey), isFalse);
  });
}
