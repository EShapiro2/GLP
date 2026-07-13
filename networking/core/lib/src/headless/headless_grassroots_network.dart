import 'dart:async';
import 'dart:io' show InternetAddressType;
import 'dart:typed_data';

import 'package:redux/redux.dart';
import 'package:sodium/sodium_sumo.dart';
import 'package:uuid/uuid.dart';

import '../models/identity.dart';
import '../models/packet.dart';
import '../models/peer.dart';
import '../platform/compat.dart';
import '../protocol/fragment_handler.dart';
import '../protocol/message_transport.dart';
import '../protocol/protocol_handler.dart';
import '../routing/message_router.dart';
import '../session/noise_session_manager.dart';
import '../store/store.dart';
import '../transport/address_utils.dart';
import '../transport/udp_transport_service.dart';
import 'known_peers_persistence.dart';

/// Pure-Dart, IP-only server profile of the Grassroots networking layer
/// (spec `GLP_Networking_API`; GPW cross-project request, Answer 2).
///
/// A headless embedding of the same session, framing, and router internals
/// the Flutter package runs — no reimplementation and no fork — exposing the
/// spec's API surface: construction with identity, [putKnownPeer] /
/// [removeKnownPeer] / [putPeerAddress], [setTrustLevel], [send],
/// [onMessageReceived], [onPeerConnected] / [onPeerDisconnected],
/// [getPublicAddress]. IP is the only medium: no BLE, no mDNS, no
/// connectivity plugin — the public address is static configuration
/// ([staticPublicAddress]), as on a globally routable server.
///
/// Trust gating (spec §Cold-Call Trust Levels, applied to unsolicited IP
/// contact per the GPW answer): under Closed — the default — an inbound
/// ANNOUNCE or Noise handshake from a key not in the known set is rejected,
/// so unregistered keys cannot complete a session; [putKnownPeer] registers,
/// [removeKnownPeer] revokes. Under Open any agent may complete contact.
///
/// Known peers persist through [HeadlessKnownPeersStore] — file-backed
/// ([FileKnownPeersStore]) or in-memory ([MemoryKnownPeersStore]) — replacing
/// the Flutter package's SharedPreferences persistence.
class HeadlessGrassrootsNetwork {
  HeadlessGrassrootsNetwork({
    required this.identity,
    required this.sodium,
    this.listenPort = 0,
    this.staticPublicAddress,
    HeadlessKnownPeersStore? knownPeersStore,
  }) : _knownPeersStore = knownPeersStore ?? MemoryKnownPeersStore() {
    store = Store<AppState>(
      appReducer,
      initialState: const AppState(
        settings: SettingsState(bluetoothEnabled: false),
      ),
    );
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

  /// Fixed UDP port to bind (the port of the service's published address);
  /// 0 binds an ephemeral port (initiator profile).
  final int listenPort;

  /// The agent's static public `ip:port`, as configured for a globally
  /// routable server. Returned by [getPublicAddress]; no discovery runs.
  final String? staticPublicAddress;

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

  /// Callback for received message payloads:
  /// `(messageId, senderPubkey, payload, transport)`.
  void Function(String messageId, Uint8List senderPubkey, Uint8List payload,
      MessageTransport transport)? onMessageReceived;

  /// Fired when a peer becomes reachable over IP (authenticated session up).
  void Function(Uint8List pubkey, MessageTransport transport)? onPeerConnected;

  /// Fired when a peer stops being reachable over IP.
  void Function(Uint8List pubkey, MessageTransport transport)?
      onPeerDisconnected;

  /// The agent's current public `ip:port` — static configuration in this
  /// profile — or null when none was configured.
  String? getPublicAddress() => staticPublicAddress;

  /// The actually bound UDP port (after [start]); useful when [listenPort]
  /// was 0.
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
      onPeerDisconnected?.call(pubkey, MessageTransport.udp);
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

  /// Set the cold-call trust level (spec §Cold-Call Trust Levels). Until
  /// set, the level is Closed.
  void setTrustLevel(ColdCallTrustLevel level) {
    if (store.state.settings.coldCallTrustLevel == level) return;
    store.dispatch(SetColdCallTrustLevelAction(level));
  }

  /// Whether the peer currently has an authenticated IP session.
  bool isPeerReachable(Uint8List pubkey) => _reachablePeers.contains(_hex(pubkey));

  // ===== Lifecycle =====

  /// Bind the UDP socket(s) (on [listenPort]) and start accepting and
  /// dialing. Returns false when no socket could be bound.
  Future<bool> start() async {
    if (_udpService != null) return true;
    final udp = UdpTransportService(
      identity: identity,
      store: store,
      protocolHandler: _protocolHandler,
      listenPort: listenPort,
    );
    if (!await udp.initialize()) {
      return false;
    }
    _udpService = udp;
    _setupUdpCallbacks(udp);
    await udp.start();
    _heartbeatTimer = Timer.periodic(heartbeatInterval, (_) {
      for (final hex in _outboundQueue.keys.toList()) {
        _drainQueueFor(_bytes(hex));
      }
    });
    debugPrint('[headless] listening on UDP port $boundPort '
        '(${_hex(identity.publicKey).substring(0, 8)}…)');
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
      onPeerDisconnected?.call(_bytes(hex), MessageTransport.udp);
    }
    _reachablePeers.clear();
    _announcedTo.clear();
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
  /// Communication). Delivers over an existing session, dials the peer's
  /// dial-book address on demand, or queues the message until the peer is
  /// reachable (fair delivery). Returns the message id, or null for invalid
  /// input.
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

  Future<bool> _trySendNow(
    Uint8List recipientPubkey,
    Uint8List payload,
    String messageId,
  ) async {
    final udp = _udpService;
    if (udp == null) return false;
    final hex = _hex(recipientPubkey);

    // No existing UDX connection — dial the dial-book address, if any.
    if (udp.getPeerIdForPubkey(recipientPubkey) == null) {
      final address = store.state.peers.getPeerByPubkeyHex(hex)?.udpAddress ??
          store.state.knownPeers.known[hex];
      if (address == null) return false;
      final parsed = parseAddressString(address);
      if (parsed == null) return false;
      _dialedPeers.add(hex);
      if (!await udp.connectToPeer(hex, parsed.ip, parsed.port)) {
        return false;
      }
      // The dial sequence (spec Implementation Notes): the dialer sends its
      // signed ANNOUNCE; the accepting endpoint, having verified it,
      // initiates the Noise handshake.
      await udp.sendToPeer(hex, _signedAnnounceBytes());
    }

    // Establish the session up front (dialer waits for the acceptor's
    // handshake per the dial sequence) so the transport engine's fragments
    // all ride an existing session.
    if (!_noiseSessions.hasSession(PeerTransport.udp, recipientPubkey)) {
      final probe = await _sessionPacketBytes(
        _protocolHandler.createAckPacket(messageId: messageId),
        recipientPubkey,
      );
      if (probe == null) return false;
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
  Future<Uint8List?> _sessionPacketBytes(
    GrassrootsPacket packet,
    Uint8List recipientPubkey,
  ) async {
    if (!packet.type.usesSessionSecurity) return packet.serialize();
    if (!_noiseSessions.hasSession(PeerTransport.udp, recipientPubkey)) {
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
          return true;
        });
        if (!sent) return null;
      }
      if (!await _noiseSessions.waitForSession(
        PeerTransport.udp,
        recipientPubkey,
      )) {
        return null;
      }
    }
    final encrypted = await _noiseSessions.encryptPacket(
      packet,
      transport: PeerTransport.udp,
      remotePubkey: recipientPubkey,
    );
    return encrypted.serialize();
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

  bool _acceptsContactFrom(Uint8List pubkey) =>
      store.state.settings.coldCallTrustLevel == ColdCallTrustLevel.open ||
      _isKnown(pubkey);

  // ===== Wiring (mirrors the Flutter coordinator's IP path) =====

  void _setupRouterCallbacks() {
    _messageRouter.onMessageReceived =
        (messageId, senderPubkey, payload, arrivalTransport) {
      onMessageReceived?.call(
        messageId,
        senderPubkey,
        payload,
        MessageTransport.udp,
      );
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
  /// handshake per medium per pair). The announce-triggered initiation and
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
            onPeerDisconnected?.call(pubkey, MessageTransport.udp);
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
    if (_reachablePeers.add(_hex(pubkey))) {
      onPeerConnected?.call(pubkey, MessageTransport.udp);
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

  static Uint8List _bytes(String hex) {
    final out = Uint8List(hex.length ~/ 2);
    for (var i = 0; i < out.length; i++) {
      out[i] = int.parse(hex.substring(2 * i, 2 * i + 2), radix: 16);
    }
    return out;
  }
}
