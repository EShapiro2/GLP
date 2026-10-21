import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:redux/redux.dart';
import 'package:sodium/sodium_sumo.dart';
import 'package:test/test.dart';

import 'package:grassroots_networking_core/grassroots_networking_core.dart';

/// The two handshakes of GNA §2.6, Session Establishment, at the protocol
/// level: the session manager and, for the ANNOUNCE a session carries, the
/// router. "When the initiator expects a specific responder key, it runs
/// Noise IK: it sends its own static key in the first message, encrypted to
/// the expected key, so only the holder of that key can read or answer it,
/// and the responder never transmits its static key"; "Only a first contact
/// with an unrecognized presence under Open trust has no expected key: there
/// the peers exchange ANNOUNCE first and run Noise XX"; "The responder
/// enforces the trust level"; the final message "(message 2 of IK, message 3
/// of XX)" retransmitted "until the other side's attestation message
/// arrives"; protocol version 7, version 6 refused at the handshake.
///
/// GATT is a connection id per leg, `central:` or `peripheral:`, as the BLE
/// carrier names its paths; the GATT carrier itself is not driven here, as
/// `GrassrootsNetwork` takes no fake BLE host (Implementation Notes).
///
/// Needs a native libsodium; self-skips when absent.
void main() {
  SodiumSumo? sodium;
  String? sodiumUnavailable;

  setUpAll(() async {
    try {
      sodium = await initHeadlessSodium();
    } on Object catch (e) {
      sodiumUnavailable = 'libsodium unavailable, skipping: $e';
    }
  });

  bool skipped() {
    if (sodiumUnavailable == null) return false;
    markTestSkipped(sodiumUnavailable!);
    return true;
  }

  Future<GrassrootsIdentity> identityFromSeed(int fill) async =>
      GrassrootsIdentity.create(
        keyPair: await Ed25519()
            .newKeyPairFromSeed(Uint8List.fromList(List.filled(32, fill))),
      );

  NoiseSessionManager manager(GrassrootsIdentity id) =>
      NoiseSessionManager(identity: id, sodium: sodium!);

  GrassrootsPacket handshake(Uint8List payload) =>
      GrassrootsPacket(type: PacketType.noiseHandshake, payload: payload);

  String hexOf(Uint8List bytes) =>
      bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  /// Whether [needle] occurs in [haystack].
  bool contains(Uint8List haystack, Uint8List needle) =>
      hexOf(haystack).contains(hexOf(needle));

  /// The protocol version and the message kind an envelope opens with.
  (int, int) envelopeHead(Uint8List payload) =>
      (ByteData.sublistView(payload, 0, 2).getUint16(0), payload[2]);

  bool Function(NoisePattern, Uint8List) knownOnly(Uint8List known) =>
      (pattern, key) =>
          pattern == NoisePattern.ik && hexOf(key) == hexOf(known);

  for (final transport in PeerTransport.values) {
    final (initiatorLeg, responderLeg) = transport == PeerTransport.bleDirect
        ? ('central:bob-adv', 'peripheral:alice-conn')
        : ('bob-hex', '127.0.0.1:9601');

    test(
        'IK between known peers over ${transport.name}: the initiator expects '
        'the key, and neither key travels in clear', () async {
      if (skipped()) return;
      final alice = await identityFromSeed(0x31);
      final bob = await identityFromSeed(0x32);
      final aliceSessions = manager(alice);
      final bobSessions = manager(bob);

      final m1 = (await aliceSessions.startHandshake(
        transport,
        bob.publicKey,
        pattern: NoisePattern.ik,
        peerId: initiatorLeg,
      ))!;
      expect(envelopeHead(m1), (7, 0x11));
      expect(contains(m1, alice.publicKey), isFalse,
          reason: 'the initiator\'s key travels encrypted to the expected one');
      expect(aliceSessions.handshakeRetransmitPayload(transport, bob.publicKey),
          m1,
          reason: 'message 1 is retransmitted until message 2 arrives');

      final asked = <(NoisePattern, String)>[];
      final atBob = await bobSessions.handleHandshakePacket(
        handshake(m1),
        transport: transport,
        peerId: responderLeg,
        acceptFirstMessage: (pattern, key) {
          asked.add((pattern, hexOf(key)));
          return knownOnly(alice.publicKey)(pattern, key);
        },
      );
      expect(asked, [(NoisePattern.ik, hexOf(alice.publicKey))],
          reason: 'the responder learns the initiator from message 1 alone');
      expect(atBob.remotePubkey, alice.publicKey);
      expect(atBob.pattern, NoisePattern.ik);
      expect(atBob.sessionEstablished, isTrue);
      expect(atBob.sentFinal, isTrue);
      final m2 = atBob.responsePayload!;
      expect(envelopeHead(m2), (7, 0x12));
      expect(contains(m2, bob.publicKey), isFalse,
          reason: 'the responder never transmits its static key');
      expect(bobSessions.finalHandshakePayload(transport, alice.publicKey), m2,
          reason: 'message 2 is retransmitted until the attestation arrives');

      final atAlice = await aliceSessions.handleHandshakePacket(
        handshake(m2),
        transport: transport,
        peerId: initiatorLeg,
        acceptFirstMessage: (_, __) => fail('message 2 is no first message'),
      );
      expect(atAlice.remotePubkey, bob.publicKey);
      expect(atAlice.sessionEstablished, isTrue);
      expect(atAlice.sentFinal, isFalse);
      expect(aliceSessions.finalHandshakePayload(transport, bob.publicKey),
          isNull);
      expect(aliceSessions.handshakeHashFor(transport, bob.publicKey),
          bobSessions.handshakeHashFor(transport, alice.publicKey));

      final clear = GrassrootsPacket(
          type: PacketType.fragment, payload: Uint8List.fromList([7, 7]));
      final sealed = await aliceSessions.encryptPacket(clear,
          transport: transport, remotePubkey: bob.publicKey);
      final (opened, sender, _) = await bobSessions.decryptPacket(sealed,
          transport: transport, peerId: responderLeg);
      expect(opened.payload, clear.payload);
      expect(sender, alice.publicKey);
    });
  }

  test(
      'over GATT a second leg is identified by the ANNOUNCE the session '
      'carries on it, which the router hands on as in-session', () async {
    if (skipped()) return;
    final alice = await identityFromSeed(0x33);
    final bob = await identityFromSeed(0x34);
    final aliceSessions = manager(alice);
    final bobSessions = manager(bob);
    const transport = PeerTransport.bleDirect;

    final m1 = (await aliceSessions.startHandshake(transport, bob.publicKey,
        pattern: NoisePattern.ik, peerId: 'central:bob-adv'))!;
    final atBob = await bobSessions.handleHandshakePacket(handshake(m1),
        transport: transport,
        peerId: 'peripheral:alice-conn',
        acceptFirstMessage: knownOnly(alice.publicKey));
    await aliceSessions.handleHandshakePacket(handshake(atBob.responsePayload!),
        transport: transport,
        peerId: 'central:bob-adv',
        acceptFirstMessage: (_, __) => false);

    // Bob's reverse leg: Bob dials Alice, and the leg reaches Alice as a
    // peripheral path no handshake ran on.
    final announce = GrassrootsPacket(
      type: PacketType.announce,
      payload: ProtocolHandler(identity: bob, sodium: sodium!)
          .createAnnouncePayload(),
    );
    final sealed = await bobSessions.encryptPacket(announce,
        transport: transport, remotePubkey: alice.publicKey);
    expect(sealed.type, PacketType.secureAnnounce);

    final store = Store<AppState>(appReducer, initialState: const AppState());
    final router = MessageRouter(
      store: store,
      protocolHandler: ProtocolHandler(identity: alice, sodium: sodium!),
      fragmentHandler: FragmentHandler(),
    );
    router.decryptSessionPacket = (packet, t, {String? peerId}) async {
      try {
        return await aliceSessions.decryptPacket(packet,
            transport: t, peerId: peerId);
      } catch (_) {
        return null;
      }
    };
    final announced = <(String, bool)>[];
    router.onPeerAnnounced =
        (data, t, {bool isNew = false, String? udpPeerId, bool inSession = false}) =>
            announced.add((hexOf(data.publicKey), inSession));

    await router.processPacket(
      GrassrootsPacket.deserialize(sealed.serialize()),
      transport: transport,
      bleDeviceId: 'peripheral:bob-reverse',
      bleRole: BleRole.peripheral,
    );
    expect(announced, [(hexOf(bob.publicKey), true)]);
    expect(
        store.state.peers.getPeerByPubkey(bob.publicKey)?.blePeripheralDeviceId,
        'peripheral:bob-reverse',
        reason: 'the leg is now Bob\'s');

    // A session ANNOUNCE whose record names another key than its session's
    // is dropped.
    final mallory = await identityFromSeed(0x35);
    final forged = await bobSessions.encryptPacket(
        GrassrootsPacket(
          type: PacketType.announce,
          payload: ProtocolHandler(identity: mallory, sodium: sodium!)
              .createAnnouncePayload(),
        ),
        transport: transport,
        remotePubkey: alice.publicKey);
    await router.processPacket(forged,
        transport: transport,
        bleDeviceId: 'peripheral:bob-reverse',
        bleRole: BleRole.peripheral);
    expect(announced, hasLength(1));
  });

  test(
      'XX for an Open first contact: three messages, each delivered key '
      'checked against the claim, the initiator sending the final one',
      () async {
    if (skipped()) return;
    final alice = await identityFromSeed(0x36);
    final bob = await identityFromSeed(0x37);
    final aliceSessions = manager(alice);
    final bobSessions = manager(bob);
    const transport = PeerTransport.udp;
    // After ANNOUNCE each holds the other's claimed key; Open accepts XX.
    bool openFor(NoisePattern pattern, Uint8List key) =>
        pattern == NoisePattern.xx && hexOf(key) == hexOf(alice.publicKey);

    final m1 = (await aliceSessions.startHandshake(transport, bob.publicKey,
        pattern: NoisePattern.xx, peerId: hexOf(bob.publicKey)))!;
    expect(envelopeHead(m1), (7, 0x01));
    final atBob = await bobSessions.handleHandshakePacket(handshake(m1),
        transport: transport,
        peerId: hexOf(alice.publicKey),
        acceptFirstMessage: openFor);
    expect(atBob.sessionEstablished, isFalse);
    final m2 = atBob.responsePayload!;
    expect(envelopeHead(m2), (7, 0x02));
    expect(bobSessions.handshakeRetransmitPayload(transport, alice.publicKey),
        m2);

    final atAlice = await aliceSessions.handleHandshakePacket(handshake(m2),
        transport: transport,
        peerId: hexOf(bob.publicKey),
        acceptFirstMessage: (_, __) => false);
    expect(atAlice.sessionEstablished, isTrue);
    expect(atAlice.sentFinal, isTrue);
    final m3 = atAlice.responsePayload!;
    expect(envelopeHead(m3), (7, 0x03));
    expect(aliceSessions.finalHandshakePayload(transport, bob.publicKey), m3);

    final done = await bobSessions.handleHandshakePacket(handshake(m3),
        transport: transport,
        peerId: hexOf(alice.publicKey),
        acceptFirstMessage: (_, __) => false);
    expect(done.sessionEstablished, isTrue);
    expect(done.remotePubkey, alice.publicKey);
    expect(aliceSessions.handshakeHashFor(transport, bob.publicKey),
        bobSessions.handshakeHashFor(transport, alice.publicKey));

    // A retransmitted message 2 after completion is answered with the same
    // message 3, establishing nothing again.
    final again = await aliceSessions.handleHandshakePacket(handshake(m2),
        transport: transport,
        peerId: hexOf(bob.publicKey),
        acceptFirstMessage: (_, __) => false);
    expect(again.sessionEstablished, isFalse);
    expect(again.responsePayload, m3);
  });

  test(
      'XX: a delivered static that is not the claimed key\'s aborts the '
      'handshake', () async {
    if (skipped()) return;
    final alice = await identityFromSeed(0x38);
    final bob = await identityFromSeed(0x39);
    final mallory = await identityFromSeed(0x3a);
    final aliceSessions = manager(alice);
    final mallorySessions = manager(mallory);
    const transport = PeerTransport.udp;

    final m1 = (await aliceSessions.startHandshake(transport, bob.publicKey,
        pattern: NoisePattern.xx))!;
    final atMallory = await mallorySessions.handleHandshakePacket(
        handshake(m1),
        transport: transport,
        acceptFirstMessage: (_, __) => true);
    // The claim (after version and kind) is rewritten to Bob's.
    final tampered = Uint8List.fromList(atMallory.responsePayload!)
      ..setRange(3, 35, bob.publicKey);
    final result = await aliceSessions.handleHandshakePacket(
        handshake(tampered),
        transport: transport,
        acceptFirstMessage: (_, __) => false);
    expect(result.sessionEstablished, isFalse);
    expect(aliceSessions.hasSession(transport, bob.publicKey), isFalse);
  });

  test(
      'IK: a first message sent to another key than the responder\'s does not '
      'open, and a responder answers only from what its trust check accepts',
      () async {
    if (skipped()) return;
    final alice = await identityFromSeed(0x3b);
    final bob = await identityFromSeed(0x3c);
    final carol = await identityFromSeed(0x3d);
    final aliceSessions = manager(alice);
    final bobSessions = manager(bob);
    final carolSessions = manager(carol);
    const transport = PeerTransport.udp;

    // Alice expects Carol's key; Bob, who holds another, cannot read it.
    final toCarol = (await aliceSessions.startHandshake(transport,
        carol.publicKey, pattern: NoisePattern.ik))!;
    await expectLater(
        bobSessions.handleHandshakePacket(handshake(toCarol),
            transport: transport, acceptFirstMessage: (_, __) => true),
        throwsFormatException);

    // Carol's trust check refuses Alice: no response, no state.
    var responded = false;
    await expectLater(
        carolSessions.handleHandshakePacket(handshake(toCarol),
            transport: transport,
            acceptFirstMessage: knownOnly(bob.publicKey),
            sendResponse: (_, __) async => responded = true),
        throwsStateError);
    expect(responded, isFalse);
    expect(carolSessions.hasSession(transport, alice.publicKey), isFalse);
    expect(carolSessions.finalHandshakePayload(transport, alice.publicKey),
        isNull);

    // And no XX first message, from any key, where the check refuses XX.
    final xx = (await bobSessions.startHandshake(transport, carol.publicKey,
        pattern: NoisePattern.xx))!;
    await expectLater(
        carolSessions.handleHandshakePacket(handshake(xx),
            transport: transport,
            acceptFirstMessage: (pattern, _) => pattern == NoisePattern.ik,
            sendResponse: (_, __) async => responded = true),
        throwsStateError);
    expect(responded, isFalse);
    expect(carolSessions.hasSession(transport, bob.publicKey), isFalse);
  });

  test(
      'IK: a retransmitted message 1 is answered with the same message 2, the '
      'session it established standing; a new message 1 replaces it',
      () async {
    if (skipped()) return;
    final alice = await identityFromSeed(0x3e);
    final bob = await identityFromSeed(0x3f);
    final aliceSessions = manager(alice);
    final bobSessions = manager(bob);
    const transport = PeerTransport.udp;

    final m1 = (await aliceSessions.startHandshake(transport, bob.publicKey,
        pattern: NoisePattern.ik))!;
    final first = await bobSessions.handleHandshakePacket(handshake(m1),
        transport: transport, acceptFirstMessage: knownOnly(alice.publicKey));
    final hash = bobSessions.handshakeHashFor(transport, alice.publicKey);
    final again = await bobSessions.handleHandshakePacket(handshake(m1),
        transport: transport, acceptFirstMessage: knownOnly(alice.publicKey));
    expect(again.sessionEstablished, isFalse);
    expect(again.responsePayload, first.responsePayload);
    expect(bobSessions.handshakeHashFor(transport, alice.publicKey), hash);

    // Alice restarts and runs a new handshake: it replaces the session.
    final restarted = manager(alice);
    final fresh = (await restarted.startHandshake(transport, bob.publicKey,
        pattern: NoisePattern.ik))!;
    final replaced = await bobSessions.handleHandshakePacket(handshake(fresh),
        transport: transport, acceptFirstMessage: knownOnly(alice.publicKey));
    expect(replaced.sessionEstablished, isTrue);
    expect(replaced.replacedSession, isTrue);
    expect(bobSessions.handshakeHashFor(transport, alice.publicKey),
        isNot(hash));
  });

  test('glare: crossed IK first messages leave the smaller key\'s handshake',
      () async {
    if (skipped()) return;
    final x = await identityFromSeed(0x41);
    final y = await identityFromSeed(0x42);
    final (small, large) =
        hexOf(x.publicKey).compareTo(hexOf(y.publicKey)) < 0 ? (x, y) : (y, x);
    final smallSessions = manager(small);
    final largeSessions = manager(large);
    const transport = PeerTransport.udp;
    bool accept(NoisePattern p, Uint8List k) => p == NoisePattern.ik;

    final fromSmall = (await smallSessions.startHandshake(
        transport, large.publicKey,
        pattern: NoisePattern.ik))!;
    final fromLarge = (await largeSessions.startHandshake(
        transport, small.publicKey,
        pattern: NoisePattern.ik))!;

    final atSmall = await smallSessions.handleHandshakePacket(
        handshake(fromLarge),
        transport: transport,
        acceptFirstMessage: accept);
    expect(atSmall.responsePayload, isNull,
        reason: 'the smaller key keeps its own handshake');
    final atLarge = await largeSessions.handleHandshakePacket(
        handshake(fromSmall),
        transport: transport,
        acceptFirstMessage: accept);
    expect(atLarge.sessionEstablished, isTrue,
        reason: 'the larger key answers the smaller\'s');
    final done = await smallSessions.handleHandshakePacket(
        handshake(atLarge.responsePayload!),
        transport: transport,
        acceptFirstMessage: accept);
    expect(done.sessionEstablished, isTrue);
    expect(smallSessions.handshakeHashFor(transport, large.publicKey),
        largeSessions.handshakeHashFor(transport, small.publicKey));
  });

  test('a version-6 handshake is refused at the handshake', () async {
    if (skipped()) return;
    final alice = await identityFromSeed(0x43);
    final bob = await identityFromSeed(0x44);
    final bobSessions = manager(bob);
    const transport = PeerTransport.udp;

    // A version-6 peer opens with XX message 1 in the envelope of that
    // version: handshake payload version 2, message 1, the claim, its
    // signature over the claim and the ephemeral, the ephemeral.
    final ephemeral = Uint8List.fromList(List.generate(32, (i) => i + 1));
    final secret = SecureKey.fromList(sodium!, alice.privateKey);
    final signature = sodium!.crypto.sign.detached(
        message: Uint8List.fromList([...alice.publicKey, ...ephemeral]),
        secretKey: secret);
    secret.dispose();
    final v6 = Uint8List.fromList(
        [2, 1, ...alice.publicKey, ...signature, ...ephemeral]);
    var asked = false;
    await expectLater(
        bobSessions.handleHandshakePacket(handshake(v6),
            transport: transport,
            acceptFirstMessage: (_, __) => asked = true),
        throwsFormatException);
    expect(asked, isFalse, reason: 'refused before it is read');

    // This version's envelope bearing version 6 is refused too.
    final m1 = (await manager(alice).startHandshake(transport, bob.publicKey,
        pattern: NoisePattern.ik))!;
    final relabelled = Uint8List.fromList(m1)..setRange(0, 2, [0, 6]);
    await expectLater(
        bobSessions.handleHandshakePacket(handshake(relabelled),
            transport: transport, acceptFirstMessage: (_, __) => true),
        throwsFormatException);
    expect(bobSessions.hasSession(transport, alice.publicKey), isFalse);

    // And a version-6 ANNOUNCE.
    final handler = ProtocolHandler(identity: alice, sodium: sodium!);
    final record = handler.createAnnouncePayload();
    record.setRange(32, 34, [0, 6]);
    final resigned = Uint8List.fromList(
        [...record.sublist(0, 34), ...handler.signBytes(record.sublist(0, 34))]);
    expect(() => handler.decodeAnnounce(resigned), throwsFormatException);
  });
}
