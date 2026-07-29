import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:redux/redux.dart';
import 'package:sodium/sodium_sumo.dart';
import 'package:test/test.dart';

import 'package:grassroots_networking_core/grassroots_networking_core.dart';

/// Handshake glare against the headless acceptor, per the paper's Wire
/// mechanics note (Implementation Notes): the strict IP dial sequence has
/// the dialer ANNOUNCE and wait, but the phone coordinator's send path may
/// also initiate Noise from the dialer side opportunistically; the session
/// manager's deterministic glare rule (smaller key keeps the initiator
/// role) resolves the crossed handshakes, so the on-wire outcome conforms.
///
/// This test drives a phone-style dialer — connect, ANNOUNCE, and
/// immediately initiate — against [HeadlessGrassrootsNetwork], in both key
/// orderings (dialer smaller / larger than the acceptor). Each run must
/// converge: session established, payload delivered, ACK returned.
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

  Future<GrassrootsIdentity> identityFromSeed(int fill, String name) async {
    final seed = Uint8List.fromList(List.filled(32, fill));
    return GrassrootsIdentity.create(
      keyPair: await Ed25519().newKeyPairFromSeed(seed),
      nickname: name,
    );
  }

  Future<void> runGlareCase({
    required int dialerSeed,
    required int acceptorSeed,
    required bool dialerKeySmaller,
  }) async {
    final dialerId = await identityFromSeed(dialerSeed, 'dialer');
    final acceptorId = await identityFromSeed(acceptorSeed, 'acceptor');
    expect(
      _hex(dialerId.publicKey).compareTo(_hex(acceptorId.publicKey)) < 0,
      dialerKeySmaller,
      reason: 'seed choice must give the intended key ordering',
    );

    final acceptor = HeadlessGrassrootsNetwork(
      identity: acceptorId,
      sodium: sodium!,
    );
    final dialer = _PhoneStyleDialer(dialerId, sodium!);
    addTearDown(() async {
      await dialer.dispose();
      await acceptor.dispose();
    });

    acceptor.putKnownPeer(dialerId.publicKey);
    final delivered = <String>[];
    acceptor.onMessageReceived = (messageId, sender, payload, transport) {
      expect(_hex(sender), _hex(dialerId.publicKey));
      delivered.add(utf8.decode(payload));
    };

    expect(await acceptor.start(), isTrue);
    await dialer.start();

    // Phone-coordinator behavior: ANNOUNCE and immediately initiate Noise —
    // crossing with the acceptor-initiated handshake of the dial sequence.
    final established = await dialer.dialAnnounceAndInitiate(
      acceptorId.publicKey,
      '127.0.0.1',
      acceptor.boundPort!,
    );
    expect(established, isTrue,
        reason: 'glare must converge to an established session');

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
    // 0x11 → d04a…, 0x22 → a09a…: dialer larger — the dialer yields its
    // initiator role on the acceptor's crossed message 1.
    await runGlareCase(
        dialerSeed: 0x11, acceptorSeed: 0x22, dialerKeySmaller: false);
  }, timeout: const Timeout(Duration(minutes: 2)));

  test('glare converges when the dialer key is smaller', () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    // 0x33 → 17cb…, 0x44 → d759…: dialer smaller — the acceptor yields.
    await runGlareCase(
        dialerSeed: 0x33, acceptorSeed: 0x44, dialerKeySmaller: true);
  }, timeout: const Timeout(Duration(minutes: 2)));
}

/// The phone coordinator's UDP wiring reduced to its dial path, built from
/// the same core internals (store, router, session manager, UDP transport):
/// dials, ANNOUNCEs, and immediately initiates the Noise handshake.
class _PhoneStyleDialer {
  _PhoneStyleDialer(this.identity, this.sodium) {
    store = Store<AppState>(
      appReducer,
      initialState: const AppState(
        settings: SettingsState(bluetoothEnabled: false),
      ),
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
      await sessions.handleHandshakePacket(
        packet,
        transport: transport,
        peerId: peerId,
        sendResponse: (payload, remotePubkey) async {
          await udp!.sendToPeer(
            peerId ?? _hex(remotePubkey),
            GrassrootsPacket(
              type: PacketType.noiseHandshake,
              payload: payload,
            ).serialize(),
          );
        },
      );
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

  /// Connect, ANNOUNCE, and immediately initiate Noise (the glare trigger).
  /// On glare the larger-key dialer's first wait fails when it yields the
  /// initiator role; the responder handshake then completes — wait again,
  /// as the phone coordinator's queue-drain path effectively does.
  Future<bool> dialAnnounceAndInitiate(
    Uint8List peerPubkey,
    String host,
    int port,
  ) async {
    final hex = _hex(peerPubkey);
    if (!await udp!.connectToPeer(hex, InternetAddress(host), port)) {
      return false;
    }
    await udp!.sendToPeer(
      hex,
      GrassrootsPacket(
        type: PacketType.announce,
        payload: protocol.createAnnouncePayload(),
      ).serialize(),
    );
    final message1 =
        await sessions.startHandshake(PeerTransport.udp, peerPubkey);
    if (message1 != null) {
      await udp!.sendToPeer(
        hex,
        GrassrootsPacket(
          type: PacketType.noiseHandshake,
          payload: message1,
        ).serialize(),
      );
    }
    if (await sessions.waitForSession(PeerTransport.udp, peerPubkey)) {
      return true;
    }
    if (sessions.hasSession(PeerTransport.udp, peerPubkey)) return true;
    return sessions.waitForSession(PeerTransport.udp, peerPubkey);
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
    router.dispose();
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
