import 'dart:async';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:sodium/sodium_sumo.dart';
import 'package:test/test.dart';

import 'package:grassroots_networking_core/grassroots_networking_core.dart';

/// A datagram the relay carried: from the caller or from the callee, its
/// frame type, and for a handshake the kind of message its envelope names
/// (GNA §2.6; 0x11 and 0x12 IK messages 1 and 2, 0x01 to 0x03 XX's).
typedef _Carried = ({bool fromCaller, int type, int? kind});

/// A loopback UDP relay in front of the callee that records what it carries
/// in each direction, and drops what [drop] picks. The frame's type byte is in
/// the clear, and a handshake envelope opens with the version and the kind.
class _Relay {
  _Relay._(this._socket, this._calleePort) {
    _socket.listen((event) {
      if (event != RawSocketEvent.read) return;
      for (var d = _socket.receive(); d != null; d = _socket.receive()) {
        _relay(d);
      }
    });
  }

  static Future<_Relay> start(int calleePort) async => _Relay._(
        await RawDatagramSocket.bind(InternetAddress.loopbackIPv4, 0),
        calleePort,
      );

  final RawDatagramSocket _socket;
  final int _calleePort;
  InternetAddress? _callerAddress;
  int? _callerPort;
  final List<_Carried> carried = [];
  bool Function(_Carried datagram) drop = (_) => false;

  int get port => _socket.port;
  String get address => '127.0.0.1:$port';

  static _Carried _describe(bool fromCaller, Uint8List data) => (
        fromCaller: fromCaller,
        type: data.isEmpty ? -1 : data[0],
        kind: data.length > 7 && data[0] == PacketType.noiseHandshake.value
            ? data[7]
            : null,
      );

  void _relay(Datagram d) {
    final fromCallee = d.port == _calleePort &&
        d.address.address == InternetAddress.loopbackIPv4.address;
    final what = _describe(!fromCallee, d.data);
    if (fromCallee) {
      final address = _callerAddress;
      final port = _callerPort;
      carried.add(what);
      if (drop(what)) return;
      if (address != null && port != null) _socket.send(d.data, address, port);
      return;
    }
    _callerAddress = d.address;
    _callerPort = d.port;
    carried.add(what);
    if (drop(what)) return;
    _socket.send(d.data, InternetAddress.loopbackIPv4, _calleePort);
  }

  Iterable<int> kinds({required bool fromCaller}) => carried
      .where((c) => c.fromCaller == fromCaller && c.kind != null)
      .map((c) => c.kind!);

  Iterable<int> types({required bool fromCaller}) =>
      carried.where((c) => c.fromCaller == fromCaller).map((c) => c.type);

  void close() => _socket.close();
}

/// Noise IK on the headless profile, every connection of which is over the
/// WAN (GNA §2.6): "the initiator expects a key ... on every WAN connection,
/// since the layer connects over the WAN only to known peers"; the responder
/// answers an IK first message "on the WAN except on a rendezvous server ...
/// only from a key in the known-peer set, and answers no XX first message";
/// "ANNOUNCE is sent inside it" (GNA §2.5); the final handshake message
/// retransmitted "until the other side's attestation message arrives";
/// versions 7 and 6 refused at the handshake.
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

  Future<HeadlessGrassrootsNetwork> node(
    int fill, {
    List<RendezvousServer> rendezvousServers = const [],
    ServerAddress? serverAddress,
    bool rendezvousServer = false,
  }) async {
    final layer = HeadlessGrassrootsNetwork(
      outboundQueueStore: MemoryOutboundQueueStore(),
      identity: await identityFromSeed(fill),
      serverAddress: serverAddress,
      rendezvousServer: rendezvousServer,
      sodium: sodium!,
    );
    addTearDown(layer.dispose);
    // Declared as GLP declares them (GNA §4.2).
    for (final server in rendezvousServers) {
      layer.wan.putRendezvousServer(server.publicKey, server.address);
    }
    expect(await layer.start(), isTrue);
    return layer;
  }

  Future<_Relay> relayTo(HeadlessGrassrootsNetwork callee) async {
    final relay = await _Relay.start(callee.boundPort!);
    addTearDown(relay.close);
    return relay;
  }

  Future<bool> waitFor(bool Function() predicate, {int seconds = 15}) async {
    final clock = Stopwatch()..start();
    while (!predicate() && clock.elapsed < Duration(seconds: seconds)) {
      await Future<void>.delayed(const Duration(milliseconds: 50));
    }
    return predicate();
  }

  bool bothReachable(
          HeadlessGrassrootsNetwork a, HeadlessGrassrootsNetwork b) =>
      a.isPeerReachable(b.identity.publicKey) &&
      b.isPeerReachable(a.identity.publicKey);

  const ik1 = 0x11, ik2 = 0x12;
  final announce = PacketType.announce.value;
  final secureAnnounce = PacketType.secureAnnounce.value;

  test(
      'two known peers run IK over UDP: no ANNOUNCE before the handshake, '
      'ANNOUNCE inside the session', () async {
    if (skipped()) return;
    final caller = await node(0x51);
    final callee = await node(0x52);
    final relay = await relayTo(callee);
    callee.putKnownPeer(caller.identity.publicKey);
    caller.wan.putPeerAddress(callee.identity.publicKey, relay.address);

    await caller.send(callee.identity.publicKey, Uint8List.fromList([1]));
    expect(await waitFor(() => bothReachable(caller, callee)), isTrue);
    expect(
        await waitFor(() =>
            relay.types(fromCaller: true).contains(secureAnnounce) &&
            relay.types(fromCaller: false).contains(secureAnnounce)),
        isTrue,
        reason: 'ANNOUNCE is sent inside the session');

    expect(relay.kinds(fromCaller: true).toSet(), {ik1});
    expect(relay.kinds(fromCaller: false).toSet(), {ik2});
    expect(relay.types(fromCaller: true), isNot(contains(announce)),
        reason: 'no ANNOUNCE in clear');
    expect(relay.types(fromCaller: false), isNot(contains(announce)));
    expect(relay.carried.first.kind, ik1,
        reason: 'the handshake opens the connection');
  });

  test('an unknown key\'s IK first message is not answered', () async {
    if (skipped()) return;
    final stranger = await node(0x53);
    final callee = await node(0x54);
    final relay = await relayTo(callee);
    stranger.wan.putPeerAddress(callee.identity.publicKey, relay.address);

    await stranger.send(callee.identity.publicKey, Uint8List.fromList([2]));
    await Future<void>.delayed(const Duration(seconds: 4));
    expect(relay.kinds(fromCaller: true), contains(ik1));
    expect(relay.types(fromCaller: false), isEmpty,
        reason: 'the callee sends nothing to a key it does not know');
    expect(callee.isPeerReachable(stranger.identity.publicKey), isFalse);
    expect(stranger.isPeerReachable(callee.identity.publicKey), isFalse);
  });

  /// Send [payload] as one datagram to [layer] from a socket of our own, and
  /// answer whether anything came back within [wait].
  Future<bool> answered(HeadlessGrassrootsNetwork layer, Uint8List datagram,
      {Duration wait = const Duration(seconds: 3)}) async {
    final socket = await RawDatagramSocket.bind(InternetAddress.loopbackIPv4, 0);
    addTearDown(socket.close);
    var replies = 0;
    socket.listen((event) {
      if (event == RawSocketEvent.read && socket.receive() != null) replies++;
    });
    socket.send(datagram, InternetAddress.loopbackIPv4, layer.boundPort!);
    await Future<void>.delayed(wait);
    return replies > 0;
  }

  test('no XX first message is answered, from a known key or any other',
      () async {
    if (skipped()) return;
    final callee = await node(0x55);
    final knownId = await identityFromSeed(0x56);
    callee.putKnownPeer(knownId.publicKey);
    final xx1 = (await NoiseSessionManager(identity: knownId, sodium: sodium!)
        .startHandshake(PeerTransport.udp, callee.identity.publicKey,
            pattern: NoisePattern.xx))!;
    final datagram =
        GrassrootsPacket(type: PacketType.noiseHandshake, payload: xx1)
            .serialize();
    expect(await answered(callee, datagram), isFalse);
    expect(callee.isPeerReachable(knownId.publicKey), isFalse);
  });

  test(
      'a clear ANNOUNCE, which only an Open first contact sends, is ignored '
      'and its connection dropped', () async {
    if (skipped()) return;
    final callee = await node(0x57);
    final knownId = await identityFromSeed(0x58);
    callee.putKnownPeer(knownId.publicKey);
    final record = GrassrootsPacket(
      type: PacketType.announce,
      payload: ProtocolHandler(identity: knownId, sodium: sodium!)
          .createAnnouncePayload(),
    ).serialize();
    expect(await answered(callee, record), isFalse);
    expect(callee.isPeerReachable(knownId.publicKey), isFalse);
  });

  test('a rendezvous server answers any key\'s IK, and makes no key known',
      () async {
    if (skipped()) return;
    final server = await node(0x59,
        serverAddress:
            ServerAddress(listenPort: 0, publicAddress: '203.0.113.7:9600'),
        rendezvousServer: true);
    final relay = await relayTo(server);
    final client = await node(0x5a);
    client.wan.putPeerAddress(server.identity.publicKey, relay.address);

    await client.send(server.identity.publicKey, Uint8List.fromList([3]));
    expect(await waitFor(() => bothReachable(client, server)), isTrue);
    expect(relay.kinds(fromCaller: true).toSet(), {ik1});
    expect(relay.kinds(fromCaller: false).toSet(), {ik2});
    expect(server.store.state.knownPeers.known, isEmpty,
        reason: 'answering a stranger does not make it known');
  });

  test(
      'a client holds its session with a declared rendezvous server by IK, '
      'with the key GLP declared it by', () async {
    if (skipped()) return;
    final server = await node(0x5b,
        serverAddress:
            ServerAddress(listenPort: 0, publicAddress: '203.0.113.8:9600'),
        rendezvousServer: true);
    final relay = await relayTo(server);
    final client = await node(0x5c, rendezvousServers: [
      RendezvousServer(relay.address, server.identity.publicKey),
    ]);
    expect(await waitFor(() => bothReachable(client, server)), isTrue);
    expect(relay.kinds(fromCaller: true).toSet(), {ik1});
    expect(relay.types(fromCaller: true), isNot(contains(announce)));
  });

  test(
      'the final handshake message is retransmitted until the attestation '
      'arrives, and then no more', () async {
    if (skipped()) return;
    final caller = await node(0x5d);
    final callee = await node(0x5e);
    final relay = await relayTo(callee);
    callee.putKnownPeer(caller.identity.publicKey);
    caller.wan.putPeerAddress(callee.identity.publicKey, relay.address);

    // The caller's attestation fragments are lost for three seconds.
    final lossUntil = DateTime.now().add(const Duration(seconds: 3));
    relay.drop = (d) =>
        d.fromCaller &&
        d.type == PacketType.secureAttestationFragment.value &&
        DateTime.now().isBefore(lossUntil);

    await caller.send(callee.identity.publicKey, Uint8List.fromList([4]));
    expect(await waitFor(() => bothReachable(caller, callee), seconds: 30),
        isTrue);
    final sent = relay.kinds(fromCaller: false).where((k) => k == ik2).length;
    expect(sent, greaterThan(1),
        reason: 'message 2 again, while no attestation had arrived');

    await Future<void>.delayed(const Duration(seconds: 6));
    expect(relay.kinds(fromCaller: false).where((k) => k == ik2).length, sent,
        reason: 'not after the attestation arrived');
  }, timeout: const Timeout(Duration(minutes: 2)));

  test('a peer on version 6 or 7 is refused at the handshake', () async {
    if (skipped()) return;
    final callee = await node(0x5f);
    final v6Id = await identityFromSeed(0x60);
    callee.putKnownPeer(v6Id.publicKey);

    // A version-6 peer opens contact with its ANNOUNCE and an XX message 1
    // in the envelope of its version.
    final handler = ProtocolHandler(identity: v6Id, sodium: sodium!);
    final record = handler.createAnnouncePayload();
    final signed = record.sublist(0, 34)..setRange(32, 34, [0, 6]);
    final v6Announce = GrassrootsPacket(
      type: PacketType.announce,
      payload: Uint8List.fromList([...signed, ...handler.signBytes(signed)]),
    ).serialize();
    final ephemeral = Uint8List.fromList(List.generate(32, (i) => 0x80 + i));
    final v6Hello = GrassrootsPacket(
      type: PacketType.noiseHandshake,
      payload: Uint8List.fromList([
        2,
        1,
        ...v6Id.publicKey,
        ...handler.signBytes(Uint8List.fromList(
            [...v6Id.publicKey, ...ephemeral])),
        ...ephemeral,
      ]),
    ).serialize();
    expect(await answered(callee, v6Announce), isFalse);
    expect(await answered(callee, v6Hello), isFalse);

    // This version's IK message 1 bearing version 6 is refused too.
    final ik = (await NoiseSessionManager(identity: v6Id, sodium: sodium!)
        .startHandshake(PeerTransport.udp, callee.identity.publicKey,
            pattern: NoisePattern.ik))!;
    final relabelled = Uint8List.fromList(ik)..setRange(0, 2, [0, 6]);
    expect(
        await answered(
            callee,
            GrassrootsPacket(
                    type: PacketType.noiseHandshake, payload: relabelled)
                .serialize()),
        isFalse);
    // And bearing version 7, the version before the attestation message
    // carried the application's version number (GNA §2.6).
    final version7 = Uint8List.fromList(ik)..setRange(0, 2, [0, 7]);
    expect(
        await answered(
            callee,
            GrassrootsPacket(
                    type: PacketType.noiseHandshake, payload: version7)
                .serialize()),
        isFalse);
    expect(callee.isPeerReachable(v6Id.publicKey), isFalse);
  });
}
