import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:redux/redux.dart';
import 'package:sodium/sodium_sumo.dart';
import 'package:test/test.dart';

import 'package:grassroots_networking_core/grassroots_networking_core.dart';

/// Handshake glare against the headless layer (GNA §4.1: "When both
/// endpoints might initiate---mutual LAN discovery, simultaneous
/// reconnection---the deterministic initiator is the endpoint with the
/// lexicographically smaller public key"). Every WAN connection runs Noise
/// IK, each end expecting the other's key (GNA §2.6), so two ends that dial
/// each other at once send crossing first messages; the session manager keeps
/// the smaller key's handshake and the larger answers it.
///
/// A dialer built from the core internals and the headless layer each start
/// IK toward the other, the dialer holding its first message back until the
/// layer's arrives, so that the two cross; in both key orderings. Each run
/// must converge: session established, payload delivered, ACK returned.
///
/// Needs a native libsodium (`brew install libsodium` / `apt-get install
/// libsodium23`, or LIBSODIUM_PATH) — the suite self-skips when absent.
void main() {
  SodiumSumo? sodium;
  String? sodiumUnavailable;

  setUpAll(() async {
    try {
      sodium = await initHeadlessSodium();
    } on Object catch (e) {
      sodiumUnavailable = 'libsodium unavailable, skipping glare test: $e';
    }
  });

  Future<GrassrootsIdentity> identityFromSeed(int fill) async {
    final seed = Uint8List.fromList(List.filled(32, fill));
    return GrassrootsIdentity.create(
      keyPair: await Ed25519().newKeyPairFromSeed(seed),
    );
  }

  Future<void> runGlareCase({
    required int dialerSeed,
    required int acceptorSeed,
    required bool dialerKeySmaller,
  }) async {
    final dialerId = await identityFromSeed(dialerSeed);
    final acceptorId = await identityFromSeed(acceptorSeed);
    expect(
      _hex(dialerId.publicKey).compareTo(_hex(acceptorId.publicKey)) < 0,
      dialerKeySmaller,
      reason: 'seed choice must give the intended key ordering',
    );

    final acceptor = HeadlessGrassrootsNetwork(
      outboundQueueStore: MemoryOutboundQueueStore(),
      identity: acceptorId,
      sodium: sodium!,
    );
    final dialer = _CrossingDialer(dialerId, sodium!);
    addTearDown(() async {
      await dialer.dispose();
      await acceptor.dispose();
    });

    final delivered = <String>[];
    acceptor.onMessageReceived = (sender, payload, messageId, underlay) {
      expect(_hex(sender), _hex(dialerId.publicKey));
      delivered.add(utf8.decode(payload));
    };

    expect(await acceptor.start(), isTrue);
    await dialer.start();
    acceptor.putKnownPeer(dialerId.publicKey);
    acceptor.wan.putPeerAddress(dialerId.publicKey, dialer.address);

    // The dialer's first message waits for the layer's, and the layer
    // initiates by sending.
    final established = dialer.dialHoldingFirstMessage(
      acceptorId.publicKey,
      '127.0.0.1',
      acceptor.boundPort!,
    );
    await acceptor.send(
        dialerId.publicKey, Uint8List.fromList(utf8.encode('to the dialer')));
    expect(await established, isTrue,
        reason: 'glare must converge to an established session');
    expect(dialer.crossed, isTrue, reason: 'the first messages crossed');

    const messageId = '00000000-0000-4000-8000-000000000042';
    expect(
      await dialer.sendPayload(
        acceptorId.publicKey,
        messageId,
        Uint8List.fromList(utf8.encode('glare payload')),
      ),
      isTrue,
    );

    final deadline = DateTime.now().add(const Duration(seconds: 10));
    while ((delivered.isEmpty || !dialer.ackedMessageIds.contains(messageId)) &&
        DateTime.now().isBefore(deadline)) {
      await Future<void>.delayed(const Duration(milliseconds: 100));
    }
    expect(delivered, ['glare payload']);
    expect(dialer.ackedMessageIds, contains(messageId));
    expect(acceptor.isPeerReachable(dialerId.publicKey), isTrue);
  }

  test('glare converges when the dialer key is larger', () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    // 0x11 → d04a…, 0x22 → a09a…: dialer larger — the dialer answers the
    // layer's first message.
    await runGlareCase(
        dialerSeed: 0x11, acceptorSeed: 0x22, dialerKeySmaller: false);
  }, timeout: const Timeout(Duration(minutes: 2)));

  test('glare converges when the dialer key is smaller', () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    // 0x33 → 17cb…, 0x44 → d759…: dialer smaller — the layer answers the
    // dialer's.
    await runGlareCase(
        dialerSeed: 0x33, acceptorSeed: 0x44, dialerKeySmaller: true);
  }, timeout: const Timeout(Duration(minutes: 2)));
}

/// A WAN agent's UDP wiring reduced to its dial path, built from the same
/// core internals (store, router, session manager, UDP transport): it
/// initiates IK toward the layer, holding its first message back until the
/// layer's arrives, so that the two cross.
class _CrossingDialer {
  _CrossingDialer(this.identity, this.sodium) {
    store = Store<AppState>(
      appReducer,
      initialState: const AppState(),
    );
    protocol = ProtocolHandler(identity: identity, sodium: sodium);
    sessions = NoiseSessionManager(identity: identity, sodium: sodium);
    router = MessageRouter(
      store: store,
      protocolHandler: protocol,
      fragmentHandler: FragmentHandler(),
    );
    router.onUdpPeerIdentified = (senderPubkey, udpPeerId) =>
        udp?.mapIncomingConnectionToPubkey(udpPeerId, _hex(senderPubkey));
    router.onNoiseHandshakeReceived =
        (packet, transport, {String? peerId}) async {
      // The layer's message 1 has arrived: send ours across it first.
      final held = _heldFirstMessage;
      if (held != null) {
        _heldFirstMessage = null;
        crossed = true;
        await udp!.sendToPeer(_peerHex!, _framed(held));
      }
      try {
        final result = await sessions.handleHandshakePacket(
          packet,
          transport: transport,
          peerId: peerId,
          acceptFirstMessage: (pattern, _) => pattern == NoisePattern.ik,
          sendResponse: (payload, remotePubkey) async {
            await udp!.sendToPeer(peerId ?? _hex(remotePubkey), _framed(payload));
          },
        );
        if (result.sessionEstablished && peerId != null) {
          udp?.mapIncomingConnectionToPubkey(peerId, _hex(result.remotePubkey));
        }
      } catch (_) {}
    };
    router.decryptSessionPacket =
        (packet, transport, {String? peerId}) async {
      try {
        Uint8List? remotePubkey;
        if (peerId != null && peerId.length == 64 && !peerId.contains(':')) {
          remotePubkey = _bytes(peerId);
        }
        return await sessions.decryptPacket(
          packet,
          transport: transport,
          peerId: peerId,
          remotePubkey: remotePubkey,
        );
      } catch (_) {
        return null;
      }
    };
    router.onAckReceived = ackedMessageIds.add;
  }

  final GrassrootsIdentity identity;
  final SodiumSumo sodium;
  late final Store<AppState> store;
  late final ProtocolHandler protocol;
  late final NoiseSessionManager sessions;
  late final MessageRouter router;
  UdpTransportService? udp;
  final Set<String> ackedMessageIds = {};
  final Map<String, Future<void>> _rxChains = {};
  Uint8List? _heldFirstMessage;
  String? _peerHex;

  /// Whether our first message went out across the layer's.
  bool crossed = false;

  String get address =>
      '127.0.0.1:${udp!.localPortForAddressType(InternetAddressType.IPv4)}';

  static Uint8List _framed(Uint8List payload) =>
      GrassrootsPacket(type: PacketType.noiseHandshake, payload: payload)
          .serialize();

  Future<void> start() async {
    final service = UdpTransportService(
      identity: identity,
      store: store,
      protocolHandler: protocol,
    );
    if (!await service.initialize()) {
      throw StateError('dialer failed to bind a UDP socket');
    }
    service.onUdpDataReceived = (peerId, data) {
      // Per-connection arrival order (spec §Session Establishment).
      final previous = _rxChains[peerId] ?? Future<void>.value();
      _rxChains[peerId] = previous.then((_) async {
        try {
          await router.processPacket(
            GrassrootsPacket.deserialize(data),
            transport: PeerTransport.udp,
            udpPeerId: peerId,
          );
        } catch (_) {}
      });
    };
    await service.start();
    udp = service;
  }

  /// Open the path and start IK, holding message 1 back until the layer's
  /// message 1 arrives. Whichever side keeps its handshake, a waiter on the
  /// session is released when the session is made.
  Future<bool> dialHoldingFirstMessage(
    Uint8List peerPubkey,
    String host,
    int port,
  ) async {
    final hex = _hex(peerPubkey);
    _peerHex = hex;
    if (!await udp!.connectToPeer(hex, InternetAddress(host), port)) {
      return false;
    }
    _heldFirstMessage = await sessions.startHandshake(
      PeerTransport.udp,
      peerPubkey,
      pattern: NoisePattern.ik,
      peerId: hex,
    );
    final established =
        await sessions.waitForSession(PeerTransport.udp, peerPubkey);
    if (established) await sendAttestation(peerPubkey);
    return established;
  }

  /// The handshake is followed by the attestation exchange, and the peer is
  /// not reachable until it completes (spec §Session Establishment). This
  /// stand-in has no platform attestation, so it sends the absence — which is
  /// what a phone coordinator does on a platform that provides none — as a
  /// message of the Message Protocol: the absence is small enough for one
  /// self-contained attestation fragment.
  Future<void> sendAttestation(Uint8List peerPubkey) async {
    final handshakeHash =
        sessions.handshakeHashFor(PeerTransport.udp, peerPubkey);
    if (handshakeHash == null) return;
    final encrypted = await sessions.encryptPacket(
      GrassrootsPacket(
        type: PacketType.attestationFragment,
        payload: FragmentHandler.encodeFragment(
          messageId: '00000000-0000-4000-8000-0000000000a7',
          index: 0,
          count: 1,
          chunk: encodeAttestationPayload(null),
        ),
      ),
      transport: PeerTransport.udp,
      remotePubkey: peerPubkey,
    );
    await udp!.sendToPeer(_hex(peerPubkey), encrypted.serialize());
  }

  Future<bool> sendPayload(
    Uint8List peerPubkey,
    String messageId,
    Uint8List payload,
  ) async {
    // A small message travels as a single self-contained fragment (spec
    // §Message Transport).
    final encrypted = await sessions.encryptPacket(
      GrassrootsPacket(
        type: PacketType.fragment,
        payload: FragmentHandler.encodeFragment(
          messageId: messageId,
          index: 0,
          count: 1,
          chunk: payload,
        ),
      ),
      transport: PeerTransport.udp,
      remotePubkey: peerPubkey,
    );
    return udp!.sendToPeer(_hex(peerPubkey), encrypted.serialize());
  }

  Future<void> dispose() async {
    await udp?.dispose();
    sessions.dispose();
  }
}

String _hex(Uint8List bytes) =>
    bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

Uint8List _bytes(String hex) {
  final out = Uint8List(hex.length ~/ 2);
  for (var i = 0; i < out.length; i++) {
    out[i] = int.parse(hex.substring(2 * i, 2 * i + 2), radix: 16);
  }
  return out;
}
