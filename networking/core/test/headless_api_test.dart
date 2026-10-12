import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:sodium/sodium_sumo.dart';
import 'package:test/test.dart';

import 'package:grassroots_networking_core/grassroots_networking_core.dart';
import 'package:grassroots_networking_core/src/signaling/peer_link.dart';
import 'package:grassroots_networking_core/src/signaling/signaling_codec.dart';
import 'package:grassroots_networking_core/src/transport/hole_punch_service.dart';

/// The headless profile brought to the API's list for the WAN underlay (GNA,
/// 2026-10-11, Task C): `sign`, `verify`, `observedPeerAddress`, `punchUdp`,
/// `peerUnderlays`, `networkIdentity` / `onNetworkChanged`, the peer links,
/// and `onConnectivityStatusChanged` fired on startup.
///
/// Needs a native libsodium, as `headless_e2e_test.dart` does; self-skips
/// when absent.
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

  Future<GrassrootsIdentity> identityFromSeed(int fill, String name) async =>
      GrassrootsIdentity.create(
        keyPair: await Ed25519()
            .newKeyPairFromSeed(Uint8List.fromList(List.filled(32, fill))),
        nickname: name,
      );

  Future<HeadlessGrassrootsNetwork> node(
    int fill, {
    List<RendezvousServer> rendezvousServers = const [],
    ServerAddress? serverAddress,
    bool start = true,
  }) async {
    final layer = HeadlessGrassrootsNetwork(
      identity: await identityFromSeed(fill, 'node'),
      rendezvousServers: rendezvousServers,
      serverAddress: serverAddress,
      sodium: sodium!,
    );
    addTearDown(layer.dispose);
    if (start) expect(await layer.start(), isTrue);
    return layer;
  }

  Future<bool> waitFor(bool Function() predicate, {int seconds = 15}) async {
    final clock = Stopwatch()..start();
    while (!predicate() && clock.elapsed < Duration(seconds: seconds)) {
      await Future<void>.delayed(const Duration(milliseconds: 50));
    }
    return predicate();
  }

  /// [a] dials [b] and both report the other reachable.
  Future<void> connect(
    HeadlessGrassrootsNetwork a,
    HeadlessGrassrootsNetwork b,
  ) async {
    b.putKnownPeer(a.identity.publicKey);
    a.putPeerAddress(b.identity.publicKey, '127.0.0.1:${b.boundPort}');
    await a.send(b.identity.publicKey, Uint8List.fromList([1]));
    expect(
        await waitFor(() =>
            a.isPeerReachable(b.identity.publicKey) &&
            b.isPeerReachable(a.identity.publicKey)),
        isTrue);
  }

  void skipWithoutSodium() {
    if (sodiumUnavailable != null) markTestSkipped(sodiumUnavailable!);
  }

  test('sign and verify are the layer\'s, under its key', () async {
    skipWithoutSodium();
    if (sodiumUnavailable != null) return;
    final a = await node(0xa1, start: false);
    final b = await node(0xa2, start: false);
    final message = Uint8List.fromList(utf8.encode('attest(alice, bob)'));

    final signature = a.sign(message);
    expect(a.verify(a.getIdentity(), message, signature), isTrue);
    expect(b.verify(a.getIdentity(), message, signature), isTrue,
        reason: 'anyone verifies against the signer\'s key');
    expect(b.verify(b.getIdentity(), message, signature), isFalse);
    expect(a.verify(a.getIdentity(), Uint8List.fromList([0]), signature),
        isFalse);
    expect(a.verify(a.getIdentity(), message, Uint8List(3)), isFalse,
        reason: 'a malformed signature is a false, not an error');
  });

  test(
      'a connected peer: reachable over the WAN, observed at the source '
      'address of its session; an unconnected one neither', () async {
    skipWithoutSodium();
    if (sodiumUnavailable != null) return;
    final a = await node(0xa3);
    final b = await node(0xa4);
    final stranger = await identityFromSeed(0xa5, 'stranger');
    await connect(a, b);

    expect(a.peerUnderlays(b.identity.publicKey), [Underlay.wan]);
    expect(b.peerUnderlays(a.identity.publicKey), [Underlay.wan]);
    expect(a.observedPeerAddress(b.identity.publicKey),
        '127.0.0.1:${b.boundPort}');
    expect(b.observedPeerAddress(a.identity.publicKey),
        '127.0.0.1:${a.boundPort}',
        reason: 'the address the layer observes, which no one supplied');

    expect(a.peerUnderlays(stranger.publicKey), isEmpty);
    expect(a.observedPeerAddress(stranger.publicKey), isNull);
  });

  test('punchUdp sends punch datagrams toward the address', () async {
    skipWithoutSodium();
    if (sodiumUnavailable != null) return;
    final a = await node(0xa6);
    final target =
        await RawDatagramSocket.bind(InternetAddress.loopbackIPv4, 0);
    addTearDown(target.close);
    final punched = Completer<PunchPacket>();
    target.listen((event) {
      if (event != RawSocketEvent.read) return;
      final datagram = target.receive();
      final packet =
          datagram == null ? null : PunchPacket.tryParse(datagram.data);
      if (packet != null && !punched.isCompleted) punched.complete(packet);
    });

    a.punchUdp('127.0.0.1:${target.port}');
    final packet = await punched.future.timeout(const Duration(seconds: 5));
    expect(packet.senderPubkey, a.identity.publicKey);
    expect(() => a.punchUdp('nowhere'), throwsArgumentError);
  });

  test(
      'networkIdentity is the attached local network\'s fingerprint, '
      'reported by onNetworkChanged at start', () async {
    skipWithoutSodium();
    if (sodiumUnavailable != null) return;
    final a = await node(0xa7, start: false);
    final changes = <(String?, String?)>[];
    a.onNetworkChanged = (oldId, newId) => changes.add((oldId, newId));
    expect(a.networkIdentity(), isNull, reason: 'not read before start');
    expect(await a.start(), isTrue);

    final attached = (await readLocalNetwork()).networkId;
    expect(a.networkIdentity(), attached);
    expect(changes, attached == null ? isEmpty : [(null, attached)]);
  });

  test(
      'onConnectivityStatusChanged fires on startup: the static address, or '
      'none', () async {
    skipWithoutSodium();
    if (sodiumUnavailable != null) return;
    final server = await node(0xa8,
        serverAddress:
            ServerAddress(listenPort: 0, publicAddress: '203.0.113.9:9600'),
        start: false);
    final client = await node(0xa9, start: false);
    final serverReports = <(String?, String?)>[];
    final clientReports = <(String?, String?)>[];
    server.onConnectivityStatusChanged =
        (oldAddress, newAddress) => serverReports.add((oldAddress, newAddress));
    client.onConnectivityStatusChanged =
        (oldAddress, newAddress) => clientReports.add((oldAddress, newAddress));
    expect(await server.start(), isTrue);
    expect(await client.start(), isTrue);

    expect(serverReports, [(null, '203.0.113.9:9600')]);
    expect(clientReports, [(null, null)]);
  });

  group('peer links', () {
    test('generatePeerLink needs a rendezvous server', () async {
      skipWithoutSodium();
      if (sodiumUnavailable != null) return;
      final a = await node(0xb1);
      await expectLater(a.generatePeerLink(), throwsStateError);
    });

    test(
        'consumePeerLink refuses a link not signed by its inviter, an expired '
        'one and the agent\'s own', () async {
      skipWithoutSodium();
      if (sodiumUnavailable != null) return;
      final holder = await node(0xb2);
      final inviter = await node(0xb3, start: false);
      final forger = await node(0xb4, start: false);
      String link(HeadlessGrassrootsNetwork signer, Uint8List inviterKey,
              {Duration validFor = const Duration(hours: 1)}) =>
          PeerLinkInvite.create(
            identityPubkey: inviterKey,
            rvPubkey: forger.identity.publicKey,
            rvAddress: '127.0.0.1:9',
            nonce: Uint8List.fromList(List.filled(32, 7)),
            expiresAtMs:
                DateTime.now().add(validFor).millisecondsSinceEpoch,
            sign: signer.sign,
          ).toUri();

      await expectLater(
          holder.consumePeerLink(link(forger, inviter.identity.publicKey)),
          throwsFormatException,
          reason: 'signed by another than the inviter it names');
      await expectLater(
          holder.consumePeerLink(link(inviter, inviter.identity.publicKey,
              validFor: const Duration(minutes: -1))),
          throwsStateError,
          reason: 'expired');
      await expectLater(
          holder.consumePeerLink(link(holder, holder.identity.publicKey)),
          throwsStateError,
          reason: 'its own');
    });

    test(
        'a link registered with the inviter\'s rendezvous server is redeemed '
        'there by its holder; the inviter learns the redeemer, acknowledges '
        'and answers its first contact', () async {
      skipWithoutSodium();
      if (sodiumUnavailable != null) return;
      // A rendezvous server stood up on a headless layer, which records the
      // signaling it receives: the registration and the claim are what it
      // would act on.
      final rv = await node(0xb5);
      final rvSaw = <(String, SignalingMessage)>[];
      rv.debugOnSignaling = (sender, payload) => rvSaw.add((
            sender.map((b) => b.toRadixString(16).padLeft(2, '0')).join(),
            const SignalingCodec().decode(payload),
          ));
      final rvServer = RendezvousServer(
          '127.0.0.1:${rv.boundPort}', rv.identity.publicKey);

      // A headless layer answers the known-peer set alone, so this stand-in
      // is told both keys before the inviter, starting, dials it.
      final inviter =
          await node(0xb6, rendezvousServers: [rvServer], start: false);
      final holder = await node(0xb7);
      rv.putKnownPeer(inviter.identity.publicKey);
      rv.putKnownPeer(holder.identity.publicKey);
      expect(await inviter.start(), isTrue);
      final redeemed = Completer<Uint8List>();
      inviter.onPeerLinkRedeemed = redeemed.complete;
      final inviterHex = _hex(inviter.identity.publicKey);
      final holderHex = _hex(holder.identity.publicKey);

      // The inviter registers the invite over its session with the server.
      final link = await inviter.generatePeerLink();
      expect(
          await waitFor(() => rvSaw.any((s) =>
              s.$1 == inviterHex && s.$2 is RegisterInviteMessage)),
          isTrue);
      final registration = rvSaw
          .firstWhere((s) => s.$2 is RegisterInviteMessage)
          .$2 as RegisterInviteMessage;
      final invite = PeerLinkInvite.fromUri(link);
      expect(registration.inviteId, deriveInviteId(invite.nonce),
          reason: 'the opaque id, the nonce itself never sent');
      expect(invite.rvAddress, rvServer.address);

      // The holder redeems it at the server the link names.
      expect(await holder.consumePeerLink(link), inviter.identity.publicKey);
      expect(
          await waitFor(() => rvSaw.any(
              (s) => s.$1 == holderHex && s.$2 is RedeemInviteMessage)),
          isTrue);
      final claim = rvSaw
          .firstWhere((s) => s.$2 is RedeemInviteMessage)
          .$2 as RedeemInviteMessage;
      expect(claim.inviteId, registration.inviteId);
      expect(
          claim.mac,
          await computeRedemptionMac(
            inviteKey: registration.inviteKey,
            inviterPubkey: inviter.identity.publicKey,
            redeemerPubkey: holder.identity.publicKey,
            inviteId: claim.inviteId,
            redeemerNonce: claim.redeemerNonce,
          ),
          reason: 'the claim proves knowledge of the link');

      // The server forwards the redeemer to the inviter.
      expect(
          await rv.debugSendSignaling(
            inviter.identity.publicKey,
            const SignalingCodec().encode(InviteRedeemedMessage(
              inviteId: claim.inviteId,
              redeemerPubkey: holder.identity.publicKey,
            )),
          ),
          isTrue);
      expect(await redeemed.future.timeout(const Duration(seconds: 10)),
          holder.identity.publicKey);
      expect(
          await waitFor(() => rvSaw.any((s) =>
              s.$1 == inviterHex && s.$2 is InviteRedeemedAckMessage)),
          isTrue,
          reason: 'the inviter acknowledges, so the server stops retrying');

      // The link authorizes the redeemer's first contact: the inviter never
      // put its key in the known-peer set.
      final delivered = Completer<String>();
      inviter.onMessageReceived = (sender, payload, _, __) {
        if (_hex(sender) == holderHex && !delivered.isCompleted) {
          delivered.complete(utf8.decode(payload));
        }
      };
      holder.putPeerAddress(
          inviter.identity.publicKey, '127.0.0.1:${inviter.boundPort}');
      await holder.send(
          inviter.identity.publicKey, Uint8List.fromList(utf8.encode('hi')));
      expect(await delivered.future.timeout(const Duration(seconds: 15)),
          'hi');
      expect(inviter.store.state.knownPeers.known.containsKey(holderHex),
          isFalse);
    }, timeout: const Timeout(Duration(minutes: 1)));
  });
}

String _hex(Uint8List bytes) =>
    bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
