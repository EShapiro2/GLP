import 'dart:async';
import 'dart:io' show InternetAddressType;
import 'dart:typed_data';

import 'package:redux/redux.dart';
import 'package:sodium/sodium_sumo.dart';
import 'package:uuid/uuid.dart';

import '../models/identity.dart';
import '../models/packet.dart';
import '../models/peer.dart';
import '../models/rendezvous_server.dart';
import '../models/underlay.dart';
import '../platform/compat.dart';
import '../protocol/fragment_handler.dart';
import '../protocol/message_transport.dart';
import '../protocol/protocol_handler.dart';
import '../routing/message_router.dart';
import '../session/application_identity.dart';
import '../session/noise_session_manager.dart';
import '../session/platform_attestation.dart';
import '../store/store.dart';
import '../transport/address_utils.dart';
import '../transport/udp_transport_service.dart';
import 'known_peers_persistence.dart';

/// A server's listen port and static public `ip:port`, one optional pair (GNA
/// §Instantiation, "Server address: listen port and static public ip:port, or
/// none --- A rendezvous server's; none on a smartphone, which learns its
/// public address from a rendezvous server").  Only the headless profile takes
/// it (GNA, 2026-10-11, Q4).
@immutable
class ServerAddress {
  /// The UDP port the server binds: the port of its published address.
  final int listenPort;

  /// The server's static public `ip:port`, canonical; what
  /// [HeadlessGrassrootsNetwork.getPublicAddress] returns.
  final String publicAddress;

  const ServerAddress._(this.listenPort, this.publicAddress);

  /// Throws [ArgumentError] on a port outside 0..65535 or an unparseable
  /// address.
  factory ServerAddress({
    required int listenPort,
    required String publicAddress,
  }) {
    if (listenPort < 0 || listenPort > 65535) {
      throw ArgumentError.value(listenPort, 'listenPort', 'not a UDP port');
    }
    final parsed = parseAddressString(publicAddress);
    if (parsed == null) {
      throw ArgumentError.value(
          publicAddress, 'publicAddress', 'not a valid ip:port');
    }
    return ServerAddress._(listenPort, parsed.toAddressString());
  }
}

/// Pure-Dart, IP-only server profile of the Grassroots networking layer
/// (spec `GLP_Networking_API`; GPW cross-project request, Answer 2).
///
/// A headless embedding of the same session, framing, and router internals
/// the Flutter package runs — no reimplementation and no fork — exposing the
/// spec's API surface: construction with identity, [putKnownPeer] /
/// [removeKnownPeer] / [putPeerAddress], [send], [onMessageReceived],
/// [onPeerConnected] / [onPeerDisconnected], [getPublicAddress]. It realizes
/// the WAN underlay alone (spec §Networking API: "a server-side agent ...
/// realizes the WAN underlay alone, headless, with proximity discovery
/// absent"), so every callback reports [Underlay.wan]: no BLE, no mDNS, no
/// connectivity plugin.
///
/// Instantiation (spec §Instantiation): the key pair, the rendezvous servers,
/// the attestation, and a server's listen port and static public address as
/// one optional pair ([ServerAddress]).  It runs [Underlay.wan] alone, so it
/// takes no underlays; it has no location service, so it takes none (GNA,
/// 2026-10-11, Q3).  It holds a session with each rendezvous server, dialing
/// it at [start] and again on the heartbeat tick while none stands (Q2); a
/// rendezvous server passes an empty list.
///
/// This profile has no proximity underlay and therefore no trust level (spec
/// §Trust levels — a level is held per `ProximityUnderlay`, and this profile
/// has none). Unsolicited inbound IP contact is governed by no level: an inbound
/// ANNOUNCE or Noise handshake from a key neither in the known set nor a
/// configured rendezvous server's is rejected, so an unknown caller cannot be
/// answered at all. [putKnownPeer] registers a key, [removeKnownPeer] revokes
/// it; a service that must accept a caller registers its key first.
///
/// Known peers persist through [HeadlessKnownPeersStore] — file-backed
/// ([FileKnownPeersStore]) or in-memory ([MemoryKnownPeersStore]) — replacing
/// the Flutter package's SharedPreferences persistence.
class HeadlessGrassrootsNetwork {
  HeadlessGrassrootsNetwork({
    required this.identity,
    required List<RendezvousServer> rendezvousServers,
    this.attestation = const NoPlatformAttestation(),
    this.serverAddress,
    required this.sodium,
    HeadlessKnownPeersStore? knownPeersStore,
  })  : rendezvousServers = List.unmodifiable(rendezvousServers),
        _knownPeersStore = knownPeersStore ?? MemoryKnownPeersStore() {
    store = Store<AppState>(appReducer, initialState: const AppState());
    _protocolHandler = ProtocolHandler(identity: identity, sodium: sodium);
    _fragmentHandler = FragmentHandler();
    _noiseSessions = NoiseSessionManager(identity: identity, sodium: sodium);
    _messageRouter = MessageRouter(
      store: store,
      protocolHandler: _protocolHandler,
      fragmentHandler: _fragmentHandler,
    );
    _setupRouterCallbacks();
    _restoreKnownPeers();
    _storeSubscription = store.onChange.listen((state) {
      if (!identical(state.knownPeers.known, _lastPersistedKnown)) {
        _lastPersistedKnown = state.knownPeers.known;
        _knownPeersStore.save(state.knownPeers.known);
      }
    });
  }

  final GrassrootsIdentity identity;
  final SodiumSumo sodium;

  /// The rendezvous servers this agent holds a session with (spec
  /// §Instantiation); empty on a rendezvous server itself.
  final List<RendezvousServer> rendezvousServers;

  /// A server's listen port and static public address, or null: then an
  /// ephemeral port is bound and [getPublicAddress] answers null.
  final ServerAddress? serverAddress;

  /// This profile's platform attestation service. A headless server has none
  /// to offer (spec §Rendezvous Server: "Having no platform attestation to
  /// offer, a rendezvous server is reported unattested"), which is the
  /// default; the default still verifies what a peer offers.
  final PlatformAttestation attestation;

  late final Store<AppState> store;
  late final ProtocolHandler _protocolHandler;
  late final FragmentHandler _fragmentHandler;
  late final NoiseSessionManager _noiseSessions;
  late final MessageRouter _messageRouter;
  UdpTransportService? _udpService;

  /// The IP carrier of the shared message transport (spec §Message
  /// Transport): fragments sized to a datagram, per-fragment ACK,
  /// retransmit with backoff under a bounded per-peer window.
  late final MessageTransportSender _udpMessageSender = MessageTransportSender(
    maxChunk: FragmentHandler.udpMaxChunk,
    sendPacket: (peerHex, packet) async {
      final bytes = await _sessionPacketBytes(packet, _bytes(peerHex));
      if (bytes == null) return false;
      return await _udpService?.sendToPeer(peerHex, bytes) ?? false;
    },
  );
  StreamSubscription<AppState>? _storeSubscription;
  StreamSubscription<dynamic>? _connectionSub;
  Timer? _heartbeatTimer;

  /// Queued-message replay period (Implementation Notes: queued messages
  /// replay on the heartbeat tick).
  static const Duration heartbeatInterval = Duration(seconds: 30);

  final HeadlessKnownPeersStore _knownPeersStore;
  Map<String, String?>? _lastPersistedKnown;

  static const _uuid = Uuid();

  /// Peers with a live authenticated (Noise) IP session, by pubkey hex —
  /// drives [onPeerConnected]/[onPeerDisconnected] transitions.
  final Set<String> _reachablePeers = {};

  /// ANNOUNCEs already answered with our own, by connection-scoped peer id —
  /// keeps the reply out of an announce ping-pong.
  final Set<String> _announcedTo = {};

  /// Peers this endpoint dialed (by pubkey hex). The IP dial sequence (spec
  /// Implementation Notes) is asymmetric: the dialer opens the stream and
  /// ANNOUNCEs; the ACCEPTING endpoint initiates the Noise handshake. The
  /// dialer therefore never starts a handshake toward a peer in this set —
  /// it waits for the acceptor's.
  final Set<String> _dialedPeers = {};

  /// Sender-side fair-delivery queue: our own outbound messages awaiting a
  /// path to their recipient (spec §Networking Assumptions). Never holds
  /// another peer's traffic.
  final Map<String, List<({String messageId, Uint8List payload})>>
      _outboundQueue = {};

  // ===== Spec API surface =====

  /// Callback for incoming payloads (spec §Point-to-Point Communication,
  /// `onMessageReceived(sender, payload, messageId, underlay)`): the sender's
  /// public key, the byte payload, a layer-assigned `messageId`, and the
  /// underlay over which the message was delivered — [Underlay.wan] in this
  /// profile.
  void Function(
    Uint8List sender,
    Uint8List payload,
    String messageId,
    Underlay underlay,
  )? onMessageReceived;

  /// Fired when a peer becomes reachable (authenticated and attested session
  /// up). Receives the peer's public key, the underlay over which it
  /// connected — [Underlay.wan] in this profile — and the peer's attested
  /// application identity, null where the peer's platform provides none
  /// (spec §Connection and Reachability, `onPeerConnected(pk, underlay,
  /// identity?)`).
  void Function(
    Uint8List pk,
    Underlay underlay,
    ApplicationIdentity? identity,
  )? onPeerConnected;

  /// Fired when a peer is no longer reachable over an underlay —
  /// [Underlay.wan] in this profile (spec §Connection and Reachability,
  /// `onPeerDisconnected(pk, underlay)`).
  void Function(Uint8List pk, Underlay underlay)? onPeerDisconnected;

  /// The agent's current public `ip:port` — the static address of its
  /// [serverAddress] in this profile — or null when it was given none.
  String? getPublicAddress() => serverAddress?.publicAddress;

  /// The actually bound UDP port (after [start]); useful when no
  /// [serverAddress] was given and an ephemeral port was bound.
  int? get boundPort =>
      _udpService?.localPortForAddressType(InternetAddressType.IPv4) ??
      _udpService?.localPortForAddressType(InternetAddressType.IPv6);

  /// Supply a peer's public key: the peer joins the known set, which Closed
  /// trust recognizes (spec §Known Peers).
  void putKnownPeer(Uint8List pubkey) {
    store.dispatch(KnownPeerPutAction(_hex(pubkey)));
    store.dispatch(PeerIdentityRegisteredAction(publicKey: pubkey));
  }

  /// Withdraw a peer's key from the known set; under Closed trust the layer
  /// stops accepting its contact. Revocation is immediate: the session is
  /// reset, the connection torn down, and reachability dropped now — not
  /// when the transport's disconnect event arrives.
  void removeKnownPeer(Uint8List pubkey) {
    final hex = _hex(pubkey);
    store.dispatch(KnownPeerRemovedAction(hex));
    _noiseSessions.reset(PeerTransport.udp, pubkey);
    _udpMessageSender.abandonPeer(hex);
    unawaited(_udpService?.disconnectFromPeer(hex));
    if (_reachablePeers.remove(hex)) {
      onPeerDisconnected?.call(pubkey, Underlay.wan);
    }
  }

  /// Inform the layer of a peer's current public `ip:port` (spec
  /// §Connectivity and Address). Supplying an address also supplies the key.
  /// Throws [ArgumentError] on an unparseable address.
  void putPeerAddress(Uint8List pubkey, String address) {
    final parsed = parseAddressString(address);
    if (parsed == null) {
      throw ArgumentError.value(address, 'address', 'not a valid ip:port');
    }
    store.dispatch(AssociateUdpAddressAction(
      publicKey: pubkey,
      address: parsed.toAddressString(),
    ));
    store.dispatch(KnownPeerAddressUpdatedAction(
      pubkeyHex: _hex(pubkey),
      udpAddress: parsed.toAddressString(),
    ));
  }

  /// Whether the peer currently has an authenticated IP session.
  bool isPeerReachable(Uint8List pubkey) => _reachablePeers.contains(_hex(pubkey));

  // ===== Lifecycle =====

  /// Bind the UDP socket(s) (on the [serverAddress]'s listen port, or an
  /// ephemeral one) and start accepting and dialing; dial each rendezvous
  /// server. Returns false when no socket could be bound.
  Future<bool> start() async {
    if (_udpService != null) return true;
    final udp = UdpTransportService(
      identity: identity,
      store: store,
      protocolHandler: _protocolHandler,
      listenPort: serverAddress?.listenPort ?? 0,
    );
    if (!await udp.initialize()) {
      return false;
    }
    _udpService = udp;
    _setupUdpCallbacks(udp);
    await udp.start();
    _heartbeatTimer = Timer.periodic(heartbeatInterval, (_) {
      _holdRendezvousSessions();
      for (final hex in _outboundQueue.keys.toList()) {
        _drainQueueFor(_bytes(hex));
      }
    });
    debugPrint('[headless] listening on UDP port $boundPort '
        '(${_hex(identity.publicKey).substring(0, 8)}…)');
    _holdRendezvousSessions();
    return true;
  }

  /// Dial each rendezvous server with no connection standing (spec
  /// §Instantiation; §Connectivity and Address: "The heartbeat sweep
  /// therefore re-initiates connections to the known peers and the
  /// configured rendezvous servers").  The dial sequence is a send's: our
  /// signed ANNOUNCE, and the server, accepting, initiates the handshake.
  void _holdRendezvousSessions() {
    final udp = _udpService;
    if (udp == null) return;
    for (final server in rendezvousServers) {
      if (udp.getPeerIdForPubkey(server.publicKey) != null) continue;
      unawaited(() async {
        if (!await _dial(server.publicKey, server.address)) {
          debugPrint('[headless][rendezvous] could not dial $server');
        }
      }());
    }
  }

  /// Open a UDP path to [pubkey] at [address] and send our signed ANNOUNCE
  /// (the dial sequence, spec Implementation Notes): the accepting endpoint,
  /// having verified it, initiates the Noise handshake.
  Future<bool> _dial(Uint8List pubkey, String address) async {
    final udp = _udpService;
    if (udp == null) return false;
    final parsed = parseAddressString(address);
    if (parsed == null) return false;
    final hex = _hex(pubkey);
    _dialedPeers.add(hex);
    if (!await udp.connectToPeer(hex, parsed.ip, parsed.port)) {
      return false;
    }
    await udp.sendToPeer(hex, _signedAnnounceBytes());
    return true;
  }

  Future<void> stop() async {
    final udp = _udpService;
    _udpService = null;
    _heartbeatTimer?.cancel();
    _heartbeatTimer = null;
    await _connectionSub?.cancel();
    _connectionSub = null;
    _noiseSessions.resetTransport(PeerTransport.udp);
    for (final hex in _reachablePeers.toList()) {
      onPeerDisconnected?.call(_bytes(hex), Underlay.wan);
    }
    _reachablePeers.clear();
    _announcedTo.clear();
    _attestedOn.clear();
    for (final deadline in _attestationDeadlines.values) {
      deadline.timer.cancel();
    }
    _attestationDeadlines.clear();
    await udp?.dispose();
  }

  Future<void> dispose() async {
    await stop();
    await _storeSubscription?.cancel();
    _storeSubscription = null;
    _noiseSessions.dispose();
    _messageRouter.dispose();
  }

  // ===== Send =====

  /// Send [payload] to the peer with [recipientPubkey] (spec §Point-to-Point
  /// Communication). Held until the peer is reachable — its session
  /// established, authenticated and attested (spec §Session Establishment;
  /// GNA, 2026-10-11, Task A) — and delivered then: a peer not reachable is
  /// dialed at its dial-book address on demand, and the message queues
  /// (fair delivery, §Networking Assumptions) until the attestation exchange
  /// completes. Returns the message id, or null for invalid input.
  Future<String?> send(
    Uint8List recipientPubkey,
    Uint8List payload, {
    String? messageId,
  }) async {
    if (recipientPubkey.length != 32) {
      debugPrint('[headless] Cannot send: recipient key must be 32 bytes');
      return null;
    }
    messageId ??= _uuid.v4();
    if (await _trySendNow(recipientPubkey, payload, messageId)) {
      return messageId;
    }
    _outboundQueue
        .putIfAbsent(_hex(recipientPubkey), () => [])
        .add((messageId: messageId, payload: payload));
    debugPrint('[headless] Queued message $messageId for '
        '${_hex(recipientPubkey).substring(0, 8)}…');
    return messageId;
  }

  /// Hand [payload] to the message transport if the peer is reachable, and
  /// answer whether it did. A peer not reachable is connected to in the
  /// background — dialed, its session established, the exchange run — and
  /// the reachability that follows drains the queue ([_completeAttestation]).
  Future<bool> _trySendNow(
    Uint8List recipientPubkey,
    Uint8List payload,
    String messageId,
  ) async {
    final udp = _udpService;
    if (udp == null) return false;
    final hex = _hex(recipientPubkey);

    if (!_reachablePeers.contains(hex)) {
      unawaited(_connect(recipientPubkey));
      return false;
    }

    unawaited(
      _udpMessageSender.sendMessage(hex, messageId, payload).then((ok) {
        if (!ok && _udpService != null) {
          _outboundQueue
              .putIfAbsent(hex, () => [])
              .add((messageId: messageId, payload: payload));
          debugPrint('[headless] Transport gave up on $messageId; re-queued');
        }
      }),
    );
    return true;
  }

  /// Encrypt a session-secured packet, establishing the Noise session first
  /// if needed (recipient's key is the expected static; a mismatching
  /// responder cannot complete the handshake). Per the dial sequence (spec
  /// Implementation Notes) the handshake initiator is the ACCEPTING
  /// endpoint: toward a peer this endpoint dialed, it only waits.
  ///
  /// Where the session exists the packet is encrypted in the caller's
  /// synchronous step, with no await before the session is read: the message
  /// transport asks a bound message whether its session is still current in
  /// that same step (the attestation exchange), and an await between the two
  /// would let a session that replaced it carry the fragment.
  Future<Uint8List?> _sessionPacketBytes(
    GrassrootsPacket packet,
    Uint8List recipientPubkey,
  ) async {
    if (!packet.type.usesSessionSecurity) return packet.serialize();
    if (!_noiseSessions.hasSession(PeerTransport.udp, recipientPubkey) &&
        !await _ensureUdpSession(recipientPubkey)) {
      return null;
    }
    final encrypted = await _noiseSessions.encryptPacket(
      packet,
      transport: PeerTransport.udp,
      remotePubkey: recipientPubkey,
    );
    return encrypted.serialize();
  }

  /// Establish the Noise session with [recipientPubkey] if none exists.
  /// Per the dial sequence (spec Implementation Notes) the handshake
  /// initiator is the ACCEPTING endpoint: toward a peer this endpoint
  /// dialed, it only waits for the acceptor's handshake; otherwise it
  /// initiates (serialized per pair) and retransmits until the reply.
  Future<bool> _ensureUdpSession(Uint8List recipientPubkey) async {
    if (_noiseSessions.hasSession(PeerTransport.udp, recipientPubkey)) {
      return true;
    }
    if (!_dialedPeers.contains(_hex(recipientPubkey))) {
      final sent =
          await _serializedHandshake(_hex(recipientPubkey), () async {
        final payload = await _noiseSessions.startHandshake(
          PeerTransport.udp,
          recipientPubkey,
        );
        if (payload == null) return true;
        final handshakeBytes = GrassrootsPacket(
          type: PacketType.noiseHandshake,
          payload: payload,
        ).serialize();
        if (!await (_udpService?.sendToPeer(
                  _hex(recipientPubkey),
                  handshakeBytes,
                ) ??
                Future.value(false))) {
          _noiseSessions.reset(PeerTransport.udp, recipientPubkey);
          return false;
        }
        _scheduleHandshakeRetransmit(recipientPubkey);
        return true;
      });
      if (!sent) return false;
    }
    return _noiseSessions.waitForSession(PeerTransport.udp, recipientPubkey);
  }

  /// Connections in progress, by pubkey hex: one at a time per peer.
  final Map<String, Future<void>> _connecting = {};

  /// Bring the session with [pubkey] up: dial its dial-book address if no
  /// path stands, then establish the Noise session (the dialer waits for the
  /// acceptor's handshake per the dial sequence). The attestation exchange
  /// follows the session ([_onSessionEstablished]), and its completion makes
  /// the peer reachable.
  Future<void> _connect(Uint8List pubkey) {
    final hex = _hex(pubkey);
    final inFlight = _connecting[hex];
    if (inFlight != null) return inFlight;
    final connecting = () async {
      final udp = _udpService;
      if (udp == null) return;
      if (udp.getPeerIdForPubkey(pubkey) == null) {
        final address =
            store.state.peers.getPeerByPubkeyHex(hex)?.udpAddress ??
                store.state.knownPeers.known[hex];
        if (address == null) return;
        if (!await _dial(pubkey, address)) return;
      }
      await _ensureUdpSession(pubkey);
    }();
    _connecting[hex] = connecting;
    return connecting.whenComplete(() {
      if (identical(_connecting[hex], connecting)) _connecting.remove(hex);
    });
  }

  void _drainQueueFor(Uint8List pubkey) {
    final queued = _outboundQueue.remove(_hex(pubkey));
    if (queued == null || queued.isEmpty) return;
    unawaited(() async {
      for (final entry in queued) {
        if (!await _trySendNow(pubkey, entry.payload, entry.messageId)) {
          _outboundQueue
              .putIfAbsent(_hex(pubkey), () => [])
              .add(entry);
        }
      }
    }());
  }

  // ===== Trust =====

  bool _isKnown(Uint8List pubkey) =>
      store.state.knownPeers.known.containsKey(_hex(pubkey));

  bool _isRendezvousServer(Uint8List pubkey) {
    final hex = _hex(pubkey);
    return rendezvousServers.any((server) => server.pubkeyHex == hex);
  }

  /// Unsolicited inbound IP contact is governed by no trust level (spec §Trust
  /// levels): it is refused unless the peer is in the known-peer set or
  /// presents a valid invite. This profile has no proximity underlay — no BLE,
  /// no LAN discovery — and no invite support, so the known-peer set is the
  /// whole of it, with the configured rendezvous servers, infrastructure this
  /// agent itself dials.
  bool _acceptsContactFrom(Uint8List pubkey) =>
      _isKnown(pubkey) || _isRendezvousServer(pubkey);

  // ===== Wiring (mirrors the Flutter coordinator's IP path) =====

  void _setupRouterCallbacks() {
    _messageRouter.onMessageReceived =
        (messageId, senderPubkey, payload, arrivalTransport) {
      onMessageReceived?.call(senderPubkey, payload, messageId, Underlay.wan);
    };

    _messageRouter.shouldAcceptUdpAnnounce = (senderPubkey, {udpPeerId}) =>
        _acceptsContactFrom(senderPubkey);

    _messageRouter.onUdpAnnounceRejected = (senderPubkey, udpPeerId) {
      // Closed trust: unregistered keys cannot complete a session.
      unawaited(_udpService?.disconnectFromPeer(_hex(senderPubkey)));
    };

    _messageRouter.onUdpPeerIdentified = (senderPubkey, udpPeerId) {
      _udpService?.mapIncomingConnectionToPubkey(udpPeerId, _hex(senderPubkey));
    };

    _messageRouter.onPeerAnnounced =
        (data, transport, {bool isNew = false, String? udpPeerId}) {
      final hex = _hex(data.publicKey);
      // Answer with our own ANNOUNCE once per connection.
      if (_announcedTo.add(hex)) {
        unawaited(_udpService?.sendToPeer(hex, _signedAnnounceBytes()));
      }
      // The ACCEPTING endpoint initiates the Noise handshake (spec
      // Implementation Notes); toward a peer this endpoint dialed, the
      // handshake is the acceptor's to start.
      if (!_dialedPeers.contains(hex)) {
        unawaited(_ensureSessionWith(data.publicKey));
      }
    };

    _messageRouter.onNoiseHandshakeReceived =
        (packet, transport, {String? peerId}) async {
      try {
        // Serialized with any in-flight announce-triggered initiation for
        // the same pair; the pair key is the claimed sender, which
        // handleHandshakePacket verifies.
        final claimed = _peekHandshakeSender(packet, peerId);
        final result = await _serializedHandshake(
          claimed,
          () => _noiseSessions.handleHandshakePacket(
            packet,
            transport: transport,
            peerId: peerId,
            // The manager sends the response itself, before releasing
            // session waiters — and the trust gate runs first: no handshake
            // response ever goes to an unregistered key under Closed trust.
            sendResponse: (payload, remotePubkey) async {
              if (!_acceptsContactFrom(remotePubkey)) {
                throw StateError(
                  'unregistered key under Closed trust: '
                  '${_hex(remotePubkey).substring(0, 8)}…',
                );
              }
              final bytes = GrassrootsPacket(
                type: PacketType.noiseHandshake,
                payload: payload,
              ).serialize();
              await _udpService?.sendToPeer(
                peerId ?? _hex(remotePubkey),
                bytes,
              );
            },
          ),
        );
        // Trust gate at the session layer: refuse to complete a session
        // with an unregistered key under Closed trust.
        if (!_acceptsContactFrom(result.remotePubkey)) {
          debugPrint('[trust] Rejecting Noise handshake from unknown '
              '${_hex(result.remotePubkey).substring(0, 8)}…');
          _noiseSessions.reset(transport, result.remotePubkey);
          unawaited(
            _udpService?.disconnectFromPeer(_hex(result.remotePubkey)),
          );
          return;
        }
        if (result.sessionEstablished) {
          if (peerId != null) {
            _udpService?.mapIncomingConnectionToPubkey(
              peerId,
              _hex(result.remotePubkey),
            );
          }
          _onSessionEstablished(result.remotePubkey);
        } else if (_noiseSessions.handshakeRetransmitPayload(
                transport, result.remotePubkey) !=
            null) {
          // We just sent message 2 (or 1): retransmit it until its reply.
          _scheduleHandshakeRetransmit(result.remotePubkey, peerId: peerId);
        }
      } catch (e) {
        debugPrint('[headless][noise] Dropping handshake packet: $e');
      }
    };

    _messageRouter.onAttestationReceived =
        (senderPubkey, payload, transport, handshakeHash) {
      unawaited(_onAttestationReceived(senderPubkey, payload, handshakeHash));
    };

    _messageRouter.isSessionAttested = (senderPubkey, transport, handshakeHash) =>
        _sameBytes(_attestedOn[_hex(senderPubkey)], handshakeHash);

    _messageRouter.decryptSessionPacket =
        (packet, transport, {String? peerId}) async {
      try {
        Uint8List? remotePubkey;
        if (peerId != null && !peerId.contains(':') && peerId.length == 64) {
          remotePubkey = _bytes(peerId);
        }
        return await _noiseSessions.decryptPacket(
          packet,
          transport: transport,
          peerId: peerId,
          remotePubkey: remotePubkey,
        );
      } catch (e) {
        debugPrint('[headless][noise] Failed to decrypt packet: $e');
        return null;
      }
    };

    _messageRouter.onAckRequested = (transport, peerId, messageId) async {
      if (peerId == null) return;
      final recipientPubkey = _bytes(peerId);
      final bytes = await _sessionPacketBytes(
        _protocolHandler.createAckPacket(messageId: messageId),
        recipientPubkey,
      );
      if (bytes != null) {
        await _udpService?.sendToPeer(peerId, bytes);
      }
    };

    _messageRouter.onAckReceived = (messageId) {
      debugPrint('[headless] ACK received for $messageId');
      _udpMessageSender.handleMessageAck(messageId);
    };

    _messageRouter.onFragmentAckReceived =
        (senderPubkey, messageId, index, transport) {
      _udpMessageSender.handleFragmentAck(
          _hex(senderPubkey), messageId, index);
    };

    _messageRouter.onFragmentAckRequested =
        (transport, peerId, messageId, index) async {
      if (peerId == null) return;
      final bytes = await _sessionPacketBytes(
        GrassrootsPacket(
          type: PacketType.fragmentAck,
          payload: FragmentHandler.encodeFragmentAck(
              messageId: messageId, index: index),
        ),
        _bytes(peerId),
      );
      if (bytes != null) {
        await _udpService?.sendToPeer(peerId, bytes);
      }
    };
  }

  /// Per-connection receive chains (spec §Session Establishment: a
  /// receiver processes each connection's inbound bytes in arrival order).
  /// Without this, a handshake packet's async handler can suspend while
  /// the connection's next packet — its first session-encrypted message —
  /// is processed, and decryption fails because the session is not yet
  /// registered.
  final Map<String, Future<void>> _rxChains = {};

  void _enqueueRx(String connectionKey, Future<void> Function() task) {
    final chain = (_rxChains[connectionKey] ?? Future<void>.value())
        .then((_) => task());
    _rxChains[connectionKey] = chain;
    chain.whenComplete(() {
      if (identical(_rxChains[connectionKey], chain)) {
        _rxChains.remove(connectionKey);
      }
    });
  }

  /// Per-pair handshake serialization (spec §Session Establishment: one
  /// handshake per underlay per pair). The announce-triggered initiation and
  /// the inbound handshake handler both mutate the pair's handshake state
  /// across awaits; run concurrently they can each observe no handshake in
  /// flight and create one, clobbering the pair's single state machine —
  /// glare then stalls instead of resolving by the deterministic rule.
  final Map<String, Future<void>> _handshakeChains = {};

  Future<T> _serializedHandshake<T>(
    String pairKey,
    Future<T> Function() task,
  ) {
    final previous = _handshakeChains[pairKey] ?? Future<void>.value();
    final result = previous.then((_) => task());
    final chain = result.then((_) {}, onError: (_) {});
    _handshakeChains[pairKey] = chain;
    chain.whenComplete(() {
      if (identical(_handshakeChains[pairKey], chain)) {
        _handshakeChains.remove(pairKey);
      }
    });
    return result;
  }

  void _setupUdpCallbacks(UdpTransportService udp) {
    udp.onUdpDataReceived = (peerId, data) {
      _enqueueRx(peerId, () async {
        try {
          final packet = GrassrootsPacket.deserialize(data);
          await _messageRouter.processPacket(
            packet,
            transport: PeerTransport.udp,
            udpPeerId: peerId,
          );
        } catch (e) {
          debugPrint(
              '[headless] Failed to process packet from $peerId: $e');
        }
      });
    };

    _connectionSub = udp.connectionStream.listen((event) {
      if (!event.connected) {
        _announcedTo.remove(event.peerId);
        _dialedPeers.remove(event.peerId);
        if (event.peerId.length == 64 && !event.peerId.contains(':')) {
          final pubkey = _bytes(event.peerId);
          _noiseSessions.reset(PeerTransport.udp, pubkey);
          if (_reachablePeers.remove(event.peerId)) {
            onPeerDisconnected?.call(pubkey, Underlay.wan);
          }
        }
      }
    });
  }

  Future<void> _ensureSessionWith(Uint8List pubkey) async {
    if (_udpService?.getPeerIdForPubkey(pubkey) == null) return;
    if (_noiseSessions.hasSession(PeerTransport.udp, pubkey)) return;
    // The initiation is serialized with inbound handshake processing for
    // the same pair; the wait happens outside the pair's chain.
    final sent = await _serializedHandshake(_hex(pubkey), () async {
      final payload = await _noiseSessions.startHandshake(
        PeerTransport.udp,
        pubkey,
      );
      if (payload == null) return true;
      final bytes = GrassrootsPacket(
        type: PacketType.noiseHandshake,
        payload: payload,
      ).serialize();
      if (!await (_udpService?.sendToPeer(_hex(pubkey), bytes) ??
          Future.value(false))) {
        _noiseSessions.reset(PeerTransport.udp, pubkey);
        return false;
      }
      return true;
    });
    if (!sent) return;
    if (await _noiseSessions.waitForSession(PeerTransport.udp, pubkey)) {
      _onSessionEstablished(pubkey);
    }
  }

  /// Pair key for handshake serialization: the payload's claimed sender
  /// (verified inside handleHandshakePacket); falls back to the connection
  /// id for an undecodable payload, which the handler then rejects anyway.
  String _peekHandshakeSender(GrassrootsPacket packet, String? peerId) {
    try {
      return _hex(_noiseSessions.claimedHandshakeSender(packet.payload));
    } catch (_) {
      return peerId ?? '?';
    }
  }

  /// Retransmit the in-flight handshake message with backoff until its
  /// reply arrives (spec §Session Establishment: the handshake is
  /// self-ordering over a lossy carrier).
  void _scheduleHandshakeRetransmit(Uint8List pubkey, {String? peerId}) {
    var attempt = 0;
    void tick() {
      final payload = _noiseSessions.handshakeRetransmitPayload(
          PeerTransport.udp, pubkey);
      if (payload == null || attempt >= 5 || _udpService == null) return;
      attempt++;
      debugPrint('[headless][noise] Retransmitting handshake to '
          '${_hex(pubkey).substring(0, 8)} (attempt $attempt)');
      unawaited(_udpService?.sendToPeer(
        peerId ?? _hex(pubkey),
        GrassrootsPacket(type: PacketType.noiseHandshake, payload: payload)
            .serialize(),
      ));
      Timer(Duration(milliseconds: 600 * (1 << attempt)), tick);
    }

    Timer(const Duration(milliseconds: 800), tick);
  }

  void _onSessionEstablished(Uint8List pubkey) {
    store.dispatch(PeerUdpConnectionChangedAction(
      pubkeyHex: _hex(pubkey),
      connected: true,
    ));
    // The handshake is followed by the mutual attestation exchange, and
    // neither peer is reported reachable until it succeeds (spec §Session
    // Establishment). A profile with no platform attestation to offer sends
    // the absence — which the peer reports as unattested rather than
    // refusing, absence and failure being distinct.
    unawaited(_sendAttestation(pubkey));
    _awaitPeerAttestation(pubkey);
  }

  /// The session each peer's attestation was verified on, by its handshake
  /// hash: what decides whether a payload on a session is delivered (Task A)
  /// and what a later attestation on the same session is checked against.
  final Map<String, Uint8List> _attestedOn = {};

  /// The deadline by which each peer's attestation must arrive on its current
  /// session, by pubkey hex.
  final Map<String, ({Uint8List hash, Timer timer})> _attestationDeadlines =
      {};

  /// Wait for the peer's attestation on [pubkey]'s current session as long as
  /// the sender's attempts last, then tear the session down if it has not
  /// come (GNA, 2026-10-11, Attestation Q1). An attestation is never queued
  /// for the next session: the next session sends its own.
  void _awaitPeerAttestation(Uint8List pubkey) {
    final hash = _noiseSessions.handshakeHashFor(PeerTransport.udp, pubkey);
    if (hash == null) return;
    final hex = _hex(pubkey);
    final standing = _attestationDeadlines[hex];
    // A session is reported established by both the handshake handler and
    // the dial that awaited it: one deadline per session.
    if (standing != null && _sameBytes(standing.hash, hash)) return;
    standing?.timer.cancel();
    late final ({Uint8List hash, Timer timer}) deadline;
    deadline = (
      hash: hash,
      timer: Timer(_udpMessageSender.attemptWindow, () {
        if (identical(_attestationDeadlines[hex], deadline)) {
          _attestationDeadlines.remove(hex);
        }
        if (_udpService == null || !_isCurrentSession(pubkey, hash)) return;
        if (_sameBytes(_attestedOn[hex], hash)) return;
        debugPrint('[headless][attest] No attestation from '
            '${hex.substring(0, 8)} within the sender\'s attempts; tearing '
            'the session down');
        _tearDownSession(pubkey);
      }),
    );
    _attestationDeadlines[hex] = deadline;
  }

  /// The session each peer's attestation was last sent on, by its handshake
  /// hash: the exchange is once per session (spec §Session Establishment),
  /// and a session is reported established by both the handshake handler and
  /// the dial that awaited it.
  final Map<String, Uint8List> _attestationSentOn = {};

  /// Whether [pubkey]'s current session is the one whose handshake hash is
  /// [handshakeHash].
  bool _isCurrentSession(Uint8List pubkey, Uint8List handshakeHash) =>
      _sameBytes(
        _noiseSessions.handshakeHashFor(PeerTransport.udp, pubkey),
        handshakeHash,
      );

  /// Send this profile's attestation over the session's digest, or the
  /// explicit absence where it has none, as a message of the Message
  /// Protocol after handshake message 3 (spec §Session Establishment): its
  /// fragments acknowledged and retransmitted as a payload's are (§Message
  /// Protocol), and outside the fair-delivery queue, which waits on the
  /// reachability this exchange decides.
  ///
  /// The message is bound to its session: the signature names that session's
  /// handshake hash, so a fragment carried into a later session would be
  /// found invalid there and tear it down. Once its session is gone the
  /// message fails untransmitted; the next session sends its own, nothing
  /// being cached. When its attempts are exhausted while its session stands,
  /// the session is torn down (§Message Protocol).
  Future<void> _sendAttestation(Uint8List pubkey) async {
    final handshakeHash =
        _noiseSessions.handshakeHashFor(PeerTransport.udp, pubkey);
    if (handshakeHash == null) return;
    final hex = _hex(pubkey);
    if (_sameBytes(_attestationSentOn[hex], handshakeHash)) return;
    _attestationSentOn[hex] = handshakeHash;
    final digest = attestationDigest(
      identityPublicKey: identity.publicKey,
      handshakeHash: handshakeHash,
    );
    // Both halves or neither: a platform with no attestation key has neither
    // an attestation nor a signature to give, and sends the explicit absence.
    AttestationEvidence? evidence;
    try {
      final offered = await attestation.attestationFor(identity.publicKey);
      final signature = offered == null
          ? null
          : await attestation.signSessionDigest(
              identityPublicKey: identity.publicKey,
              digest: digest,
            );
      if (offered != null && signature != null) {
        evidence =
            AttestationEvidence(attestation: offered, signature: signature);
      }
    } catch (e) {
      // A platform that errs offers nothing; the peer reports us unattested
      // rather than refusing us, as the Flutter profile does.
      debugPrint('[headless][attest] Platform attestation failed: $e');
      evidence = null;
    }
    final delivered = await _udpMessageSender.sendMessage(
      hex,
      _uuid.v4(),
      encodeAttestationPayload(evidence),
      fragmentType: PacketType.attestationFragment,
      isCurrent: () =>
          _udpService != null && _isCurrentSession(pubkey, handshakeHash),
    );
    if (delivered || !_isCurrentSession(pubkey, handshakeHash)) return;
    debugPrint('[headless][attest] Our attestation to ${hex.substring(0, 8)} '
        'went unacknowledged; tearing the session down');
    _tearDownSession(pubkey);
  }

  /// Tear the UDP session with [pubkey] down: an attestation found invalid,
  /// or ours undeliverable on it. The disconnect event drops reachability
  /// where the peer had it.
  void _tearDownSession(Uint8List pubkey) {
    _attestedOn.remove(_hex(pubkey));
    _noiseSessions.reset(PeerTransport.udp, pubkey);
    store.dispatch(PeerUdpConnectionChangedAction(
      pubkeyHex: _hex(pubkey),
      connected: false,
    ));
    unawaited(_udpService?.disconnectFromPeer(_hex(pubkey)));
  }

  /// A peer's attestation arrived on the session whose handshake hash is
  /// [handshakeHash], the session that decrypted it (GNA, 2026-10-11, Task A).
  /// It is judged against that session's digest; an attestation that fails
  /// verification tears that session down and onPeerConnected does not fire.
  /// A verdict on a session no longer current concerns nothing that stands,
  /// and is dropped.
  Future<void> _onAttestationReceived(
    Uint8List senderPubkey,
    Uint8List payload,
    Uint8List handshakeHash,
  ) async {
    final peerShort = _hex(senderPubkey).substring(0, 8);
    if (!_isCurrentSession(senderPubkey, handshakeHash)) {
      debugPrint('[headless][attest] $peerShort attestation arrived on a '
          'session no longer current; dropped');
      return;
    }
    final digest = attestationDigest(
      identityPublicKey: senderPubkey,
      handshakeHash: handshakeHash,
    );

    AttestationVerdict verdict;
    try {
      verdict = await attestation.verify(
        evidence: decodeAttestationPayload(payload),
        digest: digest,
        peerIdentityKey: senderPubkey,
      );
    } on FormatException catch (e) {
      verdict = InvalidAttestation('malformed payload: $e');
    } catch (e) {
      verdict = InvalidAttestation('verification threw: $e');
    }
    // Verification awaits; the session it judged may have gone meanwhile.
    if (!_isCurrentSession(senderPubkey, handshakeHash)) return;

    switch (verdict) {
      case InvalidAttestation(:final reason):
        debugPrint('[headless][attest] $peerShort failed verification: '
            '$reason; tearing the session down');
        _tearDownSession(senderPubkey);
      case AttestedApplication(:final identity):
        debugPrint('[headless][attest] $peerShort attested as $identity');
        _completeAttestation(senderPubkey, handshakeHash,
            PeerAttestation(identity: identity));
      case UnattestedPlatform(:final reason):
        debugPrint('[headless][attest] $peerShort unattested: $reason');
        _completeAttestation(
            senderPubkey, handshakeHash, PeerAttestation.unattested);
    }
  }

  void _completeAttestation(
    Uint8List pubkey,
    Uint8List handshakeHash,
    PeerAttestation attestation,
  ) {
    _attestedOn[_hex(pubkey)] = handshakeHash;
    store.dispatch(PeerAttestedAction(
      publicKey: pubkey,
      underlay: Underlay.wan,
      attestation: attestation,
    ));
    if (_reachablePeers.add(_hex(pubkey))) {
      onPeerConnected?.call(pubkey, Underlay.wan, attestation.identity);
    }
    _drainQueueFor(pubkey);
  }

  void _restoreKnownPeers() {
    final persisted = _knownPeersStore.load();
    for (final entry in persisted.entries) {
      store.dispatch(KnownPeerPutAction(entry.key));
      final address = entry.value;
      if (address != null) {
        store.dispatch(KnownPeerAddressUpdatedAction(
          pubkeyHex: entry.key,
          udpAddress: address,
        ));
      }
    }
    _lastPersistedKnown = store.state.knownPeers.known;
  }

  Uint8List _signedAnnounceBytes() => GrassrootsPacket(
        type: PacketType.announce,
        payload: _protocolHandler.createAnnouncePayload(),
      ).serialize();

  static String _hex(Uint8List bytes) =>
      bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  static bool _sameBytes(Uint8List? a, Uint8List? b) {
    if (a == null || b == null || a.length != b.length) return false;
    for (var i = 0; i < a.length; i++) {
      if (a[i] != b[i]) return false;
    }
    return true;
  }

  static Uint8List _bytes(String hex) {
    final out = Uint8List(hex.length ~/ 2);
    for (var i = 0; i < out.length; i++) {
      out[i] = int.parse(hex.substring(2 * i, 2 * i + 2), radix: 16);
    }
    return out;
  }
}
