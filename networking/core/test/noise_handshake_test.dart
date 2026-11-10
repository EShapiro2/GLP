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
/// arrives"; protocol version 9, versions 8, 7 and 6 refused at the
/// handshake.
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
  /// The version an envelope declares and its message kind (GNA api.tex,
  /// "Versions": the declaration opens the envelope).
  (int, int) envelopeHead(Uint8List payload) => (
        VersionDeclaration.decode(payload).version,
        payload[VersionDeclaration.length]
      );

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
      expect(envelopeHead(m1), (1, 0x11));
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
      expect(envelopeHead(m2), (1, 0x12));
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
        (data, t, {String? udpPeerId, bool inSession = false}) =>
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
    const transport = PeerTransport.wan;
    // After ANNOUNCE each holds the other's claimed key; Open accepts XX.
    bool openFor(NoisePattern pattern, Uint8List key) =>
        pattern == NoisePattern.xx && hexOf(key) == hexOf(alice.publicKey);

    final m1 = (await aliceSessions.startHandshake(transport, bob.publicKey,
        pattern: NoisePattern.xx, peerId: hexOf(bob.publicKey)))!;
    expect(envelopeHead(m1), (1, 0x01));
    final atBob = await bobSessions.handleHandshakePacket(handshake(m1),
        transport: transport,
        peerId: hexOf(alice.publicKey),
        acceptFirstMessage: openFor);
    expect(atBob.sessionEstablished, isFalse);
    final m2 = atBob.responsePayload!;
    expect(envelopeHead(m2), (1, 0x02));
    expect(bobSessions.handshakeRetransmitPayload(transport, alice.publicKey),
        m2);

    final atAlice = await aliceSessions.handleHandshakePacket(handshake(m2),
        transport: transport,
        peerId: hexOf(bob.publicKey),
        acceptFirstMessage: (_, __) => false);
    expect(atAlice.sessionEstablished, isTrue);
    expect(atAlice.sentFinal, isTrue);
    final m3 = atAlice.responsePayload!;
    expect(envelopeHead(m3), (1, 0x03));
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
    const transport = PeerTransport.wan;

    final m1 = (await aliceSessions.startHandshake(transport, bob.publicKey,
        pattern: NoisePattern.xx))!;
    final atMallory = await mallorySessions.handleHandshakePacket(
        handshake(m1),
        transport: transport,
        acceptFirstMessage: (_, __) => true);
    // The claim (after the version declaration and the kind) is rewritten to
    // Bob's.
    const claimAt = VersionDeclaration.length + 1;
    final tampered = Uint8List.fromList(atMallory.responsePayload!)
      ..setRange(claimAt, claimAt + 32, bob.publicKey);
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
    const transport = PeerTransport.wan;

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
      'session it established standing; a replayed message 1 ends nothing, '
      'and a new handshake replaces the session once it completes',
      () async {
    if (skipped()) return;
    final alice = await identityFromSeed(0x3e);
    final bob = await identityFromSeed(0x3f);
    final aliceSessions = manager(alice);
    final bobSessions = manager(bob);
    const transport = PeerTransport.wan;
    final completed = <String>[];
    bobSessions.onSessionCompleted =
        (t, pk, peerId) => completed.add('${t.name} ${hexOf(pk)} $peerId');

    Future<Uint8List> sealFrom(NoiseSessionManager sessions, int byte) async =>
        (await sessions.encryptPacket(
                GrassrootsPacket(
                    type: PacketType.fragment,
                    payload: Uint8List.fromList([byte])),
                transport: transport,
                remotePubkey: bob.publicKey))
            .serialize();

    /// The payload Bob opens and the session it opens under, in hex.
    Future<(String, String)> openAtBob(Uint8List sealed) async {
      final (opened, _, hash) = await bobSessions.decryptPacket(
          GrassrootsPacket.deserialize(sealed),
          transport: transport,
          peerId: 'alice-leg',
          remotePubkey: alice.publicKey);
      return (hexOf(opened.payload), hexOf(hash));
    }

    // The first handshake; m1 is what a third party records.
    final m1 = (await aliceSessions.startHandshake(transport, bob.publicKey,
        pattern: NoisePattern.ik))!;
    final first = await bobSessions.handleHandshakePacket(handshake(m1),
        transport: transport, acceptFirstMessage: knownOnly(alice.publicKey));
    await aliceSessions.handleHandshakePacket(
        handshake(first.responsePayload!),
        transport: transport,
        acceptFirstMessage: (_, __) => false);
    final hash = bobSessions.handshakeHashFor(transport, alice.publicKey);
    final again = await bobSessions.handleHandshakePacket(handshake(m1),
        transport: transport, acceptFirstMessage: knownOnly(alice.publicKey));
    expect(again.sessionEstablished, isFalse);
    expect(again.responsePayload, first.responsePayload);
    expect(bobSessions.handshakeHashFor(transport, alice.publicKey), hash);

    // Alice restarts and runs a new handshake.  Bob answers it, and the
    // standing session stays until it completes.
    final restarted = manager(alice);
    final fresh = (await restarted.startHandshake(transport, bob.publicKey,
        pattern: NoisePattern.ik))!;
    final answered = await bobSessions.handleHandshakePacket(handshake(fresh),
        transport: transport, acceptFirstMessage: knownOnly(alice.publicKey));
    expect(answered.sessionEstablished, isFalse,
        reason: 'message 1 alone completes no handshake');
    expect(answered.replacedSession, isFalse);
    expect(answered.responsePayload, isNotNull);
    expect(bobSessions.handshakeHashFor(transport, alice.publicKey), hash,
        reason: 'the live session stands');
    expect(await openAtBob(await sealFrom(aliceSessions, 1)),
        ('01', hexOf(hash!)),
        reason: 'the standing session still carries the pair');
    final retransmitted = await bobSessions.handleHandshakePacket(
        handshake(fresh),
        transport: transport,
        acceptFirstMessage: knownOnly(alice.publicKey));
    expect(retransmitted.responsePayload, answered.responsePayload,
        reason: 'a retransmitted message 1 gets the same message 2');
    expect(completed, isEmpty);

    // The new handshake completes: Alice's first packet under it opens, and
    // its session takes the standing one's place.
    final atRestarted = await restarted.handleHandshakePacket(
        handshake(answered.responsePayload!),
        transport: transport,
        acceptFirstMessage: (_, __) => false);
    expect(atRestarted.sessionEstablished, isTrue);
    final newHash = restarted.handshakeHashFor(transport, bob.publicKey)!;
    expect(newHash, isNot(hash));
    expect(await openAtBob(await sealFrom(restarted, 2)), ('02', hexOf(newHash)));
    expect(completed, ['wan ${hexOf(alice.publicKey)} alice-leg'],
        reason: 'the completion is reported once, before the packet');
    expect(bobSessions.handshakeHashFor(transport, alice.publicKey), newHash);
    expect(bobSessions.finalHandshakePayload(transport, alice.publicKey),
        answered.responsePayload);

    // The first handshake's recorded message 1, replayed against the live
    // session: answered, and never completed, so it ends nothing.
    final replayed = await bobSessions.handleHandshakePacket(handshake(m1),
        transport: transport, acceptFirstMessage: knownOnly(alice.publicKey));
    expect(replayed.sessionEstablished, isFalse);
    expect(replayed.replacedSession, isFalse);
    expect(bobSessions.handshakeHashFor(transport, alice.publicKey), newHash,
        reason: 'a replayed message 1 leaves the live session standing');
    expect(await openAtBob(await sealFrom(restarted, 3)), ('03', hexOf(newHash)));
    expect(completed, hasLength(1));
  });

  test(
      'XX: a message 1 against a live session leaves it standing; a '
      'completed XX handshake replaces it', () async {
    if (skipped()) return;
    final alice = await identityFromSeed(0x3f);
    final bob = await identityFromSeed(0x40);
    final aliceSessions = manager(alice);
    final bobSessions = manager(bob);
    const transport = PeerTransport.wan;
    bool openFor(NoisePattern pattern, Uint8List key) =>
        pattern == NoisePattern.xx && hexOf(key) == hexOf(alice.publicKey);

    Future<Uint8List> sealFrom(NoiseSessionManager sessions, int byte) async =>
        (await sessions.encryptPacket(
                GrassrootsPacket(
                    type: PacketType.fragment,
                    payload: Uint8List.fromList([byte])),
                transport: transport,
                remotePubkey: bob.publicKey))
            .serialize();

    /// The payload Bob opens and the session it opens under, in hex.
    Future<(String, String)> openAtBob(Uint8List sealed) async {
      final (opened, _, hash) = await bobSessions.decryptPacket(
          GrassrootsPacket.deserialize(sealed),
          transport: transport,
          peerId: 'alice-leg',
          remotePubkey: alice.publicKey);
      return (hexOf(opened.payload), hexOf(hash));
    }

    Future<NoiseHandshakeResult> atBob(Uint8List payload) =>
        bobSessions.handleHandshakePacket(handshake(payload),
            transport: transport, peerId: 'alice-leg', acceptFirstMessage: openFor);

    Future<NoiseHandshakeResult> atAlice(
            NoiseSessionManager sessions, Uint8List payload) =>
        sessions.handleHandshakePacket(handshake(payload),
            transport: transport,
            peerId: 'bob-leg',
            acceptFirstMessage: (_, __) => false);

    // The first XX handshake; m1 is what a third party records.
    final m1 = (await aliceSessions.startHandshake(transport, bob.publicKey,
        pattern: NoisePattern.xx, peerId: 'bob-leg'))!;
    final m2 = (await atBob(m1)).responsePayload!;
    final m3 = (await atAlice(aliceSessions, m2)).responsePayload!;
    expect((await atBob(m3)).sessionEstablished, isTrue);
    final hash = bobSessions.handshakeHashFor(transport, alice.publicKey)!;
    expect(await openAtBob(await sealFrom(aliceSessions, 1)), ('01', hexOf(hash)));

    // The recorded message 1, replayed against the live session: answered,
    // and never completed, so it ends nothing.
    final replayed = await atBob(m1);
    expect(replayed.sessionEstablished, isFalse);
    expect(replayed.replacedSession, isFalse);
    expect(replayed.responsePayload, isNotNull);
    expect(bobSessions.hasSession(transport, alice.publicKey), isTrue);
    expect(bobSessions.handshakeHashFor(transport, alice.publicKey), hash,
        reason: 'a replayed message 1 leaves the live session standing');
    expect(await openAtBob(await sealFrom(aliceSessions, 2)), ('02', hexOf(hash)));
    // The standing session's own message 3, retransmitted, completes nothing.
    final staleFinal = await atBob(m3);
    expect(staleFinal.sessionEstablished, isFalse);
    expect(bobSessions.handshakeHashFor(transport, alice.publicKey), hash);

    // Alice restarts and runs a new XX handshake.  Bob answers it, and the
    // standing session stays until it completes.
    final restarted = manager(alice);
    final fresh = (await restarted.startHandshake(transport, bob.publicKey,
        pattern: NoisePattern.xx, peerId: 'bob-leg'))!;
    final answered = await atBob(fresh);
    expect(answered.sessionEstablished, isFalse,
        reason: 'message 1 alone completes no handshake');
    expect(answered.replacedSession, isFalse);
    final freshM2 = answered.responsePayload!;
    expect(bobSessions.handshakeHashFor(transport, alice.publicKey), hash,
        reason: 'the live session stands');
    expect(await openAtBob(await sealFrom(aliceSessions, 3)), ('03', hexOf(hash)),
        reason: 'the standing session still carries the pair');
    expect(bobSessions.handshakeRetransmitPayload(transport, alice.publicKey),
        freshM2,
        reason: 'message 2 is retransmitted until message 3 arrives');
    expect((await atBob(fresh)).responsePayload, freshM2,
        reason: 'a retransmitted message 1 gets the same message 2');

    // The new handshake completes with its message 3, and its session takes
    // the standing one's place.
    final atRestarted = await atAlice(restarted, freshM2);
    expect(atRestarted.sessionEstablished, isTrue);
    final freshM3 = atRestarted.responsePayload!;
    final done = await atBob(freshM3);
    expect(done.sessionEstablished, isTrue);
    expect(done.replacedSession, isTrue);
    expect(done.remotePubkey, alice.publicKey);
    final newHash = restarted.handshakeHashFor(transport, bob.publicKey)!;
    expect(newHash, isNot(hash));
    expect(bobSessions.handshakeHashFor(transport, alice.publicKey), newHash);
    expect(bobSessions.handshakeRetransmitPayload(transport, alice.publicKey),
        isNull);
    expect(await openAtBob(await sealFrom(restarted, 4)), ('04', hexOf(newHash)));
    await expectLater(
        () async => openAtBob(await sealFrom(aliceSessions, 5)),
        throwsA(anything),
        reason: 'the replaced session carries nothing');

    // Its message 3, retransmitted, completes nothing again.
    final again = await atBob(freshM3);
    expect(again.sessionEstablished, isFalse);
    expect(again.replacedSession, isFalse);
    expect(bobSessions.handshakeHashFor(transport, alice.publicKey), newHash);
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
    const transport = PeerTransport.wan;
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

  // GNA api.tex, "Versions": "In the handshake, and in an ANNOUNCE that
  // precedes it, each peer declares its kind, its version, its earliest
  // compatible version, and the version of the other's kind that it works
  // with.  Each peer checks that the version the other works with lies
  // between its own earliest compatible version and its own version; where
  // both checks pass the session is established, and each talks to the other
  // as the version the other works with, and otherwise the connection is
  // refused.  Thus a server can be updated ahead of the smartphones, and a
  // smartphone ahead of the servers, provided the updated one remains
  // compatible back to the version the other works with.  Two peers of one
  // kind do the same."
  group('versions (GNA, "Versions")', () {
    const transport = PeerTransport.wan;
    const phone1 = VersionDeclaration.smartphone;
    const server1 = VersionDeclaration.server;

    VersionDeclaration decl(LayerKind kind, int version, int earliest,
            {int phone = 1, int server = 1}) =>
        VersionDeclaration(
            kind: kind,
            version: version,
            earliest: earliest,
            worksWithSmartphone: phone,
            worksWithServer: server);

    /// An IK handshake from [a], declaring [da], to [b], declaring [db]:
    /// both managers, or a [FormatException] where one refuses the other.
    Future<(NoiseSessionManager, NoiseSessionManager)> ik(int seedA,
        VersionDeclaration da, int seedB, VersionDeclaration db) async {
      final a = await identityFromSeed(seedA);
      final b = await identityFromSeed(seedB);
      final atA =
          NoiseSessionManager(identity: a, sodium: sodium!, declaration: da);
      final atB =
          NoiseSessionManager(identity: b, sodium: sodium!, declaration: db);
      final m1 = (await atA.startHandshake(transport, b.publicKey,
          pattern: NoisePattern.ik))!;
      final r = await atB.handleHandshakePacket(handshake(m1),
          transport: transport, acceptFirstMessage: (_, __) => true);
      await atA.handleHandshakePacket(handshake(r.responsePayload!),
          transport: transport, acceptFirstMessage: (_, __) => true);
      return (atA, atB);
    }

    Future<void> connects(int s, VersionDeclaration da, VersionDeclaration db,
        {required int aTalksAs, required int bTalksAs}) async {
      final a = await identityFromSeed(s);
      final b = await identityFromSeed(s + 1);
      final (atA, atB) = await ik(s, da, s + 1, db);
      expect(atA.hasSession(transport, b.publicKey), isTrue);
      expect(atB.hasSession(transport, a.publicKey), isTrue);
      expect(atA.peerDeclaration(transport, b.publicKey), db);
      expect(atB.peerDeclaration(transport, a.publicKey), da);
      expect(atA.talksAs(transport, b.publicKey), aTalksAs,
          reason: 'each talks to the other as the version the other works '
              'with');
      expect(atB.talksAs(transport, a.publicKey), bTalksAs);
    }

    test(
        'a server one version ahead, still compatible back to the version '
        'the phone works with, connects, whichever dials', () async {
      if (skipped()) return;
      final server2 = decl(LayerKind.server, 2, 1, server: 2);
      await connects(0x51, phone1, server2, aTalksAs: 1, bTalksAs: 1);
      await connects(0x53, server2, phone1, aTalksAs: 1, bTalksAs: 1);
    });

    test(
        'a server one version ahead and compatible back to itself alone is '
        'refused by it: the phone works with server 1', () async {
      if (skipped()) return;
      final server2 = decl(LayerKind.server, 2, 2, server: 2);
      await expectLater(ik(0x55, phone1, 0x56, server2), throwsFormatException,
          reason: 'the server refuses message 1');
      await expectLater(ik(0x57, server2, 0x58, phone1), throwsFormatException,
          reason: 'the server refuses message 2');
    });

    test(
        'a phone one version ahead, still compatible back to the version '
        'the server works with, connects; one compatible back to itself alone '
        'is refused', () async {
      if (skipped()) return;
      final phone2 = decl(LayerKind.smartphone, 2, 1, phone: 2);
      await connects(0x59, phone2, server1, aTalksAs: 1, bTalksAs: 1);
      await connects(0x5b, server1, phone2, aTalksAs: 1, bTalksAs: 1);
      final phone2only = decl(LayerKind.smartphone, 2, 2, phone: 2);
      await expectLater(
          ik(0x5d, phone2only, 0x5e, server1), throwsFormatException);
      await expectLater(
          ik(0x5f, server1, 0x60, phone2only), throwsFormatException);
    });

    test(
        'two phones do the same, each declaring the smartphone version it '
        'works with', () async {
      if (skipped()) return;
      final phone2 = decl(LayerKind.smartphone, 2, 1, phone: 1);
      await connects(0x61, phone2, phone1, aTalksAs: 1, bTalksAs: 1);
      final phone2works2 = decl(LayerKind.smartphone, 2, 1, phone: 2);
      await expectLater(
          ik(0x63, phone2works2, 0x64, phone1), throwsFormatException,
          reason: 'phone 1 cannot talk as smartphone 2');
    });

    test(
        'the declarations are bound to the handshake: a responder\'s '
        'declaration changed on the way fails message 2, and no session '
        'stands', () async {
      if (skipped()) return;
      final a = await identityFromSeed(0x65);
      final b = await identityFromSeed(0x66);
      final atA = NoiseSessionManager(
          identity: a, sodium: sodium!, declaration: phone1);
      final atB = NoiseSessionManager(
          identity: b, sodium: sodium!, declaration: server1);
      final m1 = (await atA.startHandshake(transport, b.publicKey,
          pattern: NoisePattern.ik))!;
      final r = await atB.handleHandshakePacket(handshake(m1),
          transport: transport, acceptFirstMessage: (_, __) => true);
      // The server's version, which no check reads, from 1 to 3.
      final m2 = Uint8List.fromList(r.responsePayload!)..setRange(1, 3, [0, 3]);
      await expectLater(
          atA.handleHandshakePacket(handshake(m2),
              transport: transport, acceptFirstMessage: (_, __) => true),
          throwsFormatException,
          reason: 'message 2 does not decrypt under the hash it was not '
              'written to');
      expect(atA.hasSession(transport, b.publicKey), isFalse);
      // The message as sent still completes it.
      await atA.handleHandshakePacket(handshake(r.responsePayload!),
          transport: transport, acceptFirstMessage: (_, __) => true);
      expect(atA.hasSession(transport, b.publicKey), isTrue);
    });

    test(
        'an envelope and an ANNOUNCE declaring a smartphone version this '
        'layer is not are refused before either is read further', () async {
      if (skipped()) return;
      final alice = await identityFromSeed(0x67);
      final bob = await identityFromSeed(0x68);
      final bobSessions = manager(bob);
      final m1 = (await manager(alice)
          .startHandshake(transport, bob.publicKey, pattern: NoisePattern.ik))!;
      final relabelled = Uint8List.fromList(m1)
        ..setRange(0, VersionDeclaration.length,
            decl(LayerKind.smartphone, 3, 2, phone: 2).encode());
      var asked = false;
      await expectLater(
          bobSessions.handleHandshakePacket(handshake(relabelled),
              transport: transport,
              acceptFirstMessage: (_, __) => asked = true),
          throwsFormatException);
      expect(asked, isFalse, reason: 'refused before it is read');
      expect(bobSessions.hasSession(transport, alice.publicKey), isFalse);

      final handler = ProtocolHandler(identity: alice, sodium: sodium!);
      final record = Uint8List.fromList([
        ...alice.publicKey,
        ...decl(LayerKind.server, 3, 2, phone: 2).encode()
      ]);
      final resigned =
          Uint8List.fromList([...record, ...handler.signBytes(record)]);
      expect(() => handler.decodeAnnounce(resigned), throwsFormatException);
    });
  });
}
