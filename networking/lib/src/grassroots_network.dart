import 'dart:async';
import 'dart:io';
import 'package:connectivity_plus/connectivity_plus.dart';
import 'package:flutter/foundation.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';
import 'package:uuid/uuid.dart';
import 'ble/permission_handler.dart';
import 'platform/transport_foreground_service.dart';
import 'package:grassroots_networking_core/src/signaling/signaling_codec.dart';
import 'package:grassroots_networking_core/src/transport/address_utils.dart';
import 'package:grassroots_networking_core/src/transport/local_network.dart';
import 'package:grassroots_networking_core/src/places/place_registry.dart';
import 'transport/ble_transport_service.dart';
import 'transport/connection_service.dart';
import 'package:grassroots_networking_core/src/transport/hole_punch_service.dart';
import 'transport/lan_discovery_service.dart';
import 'package:grassroots_networking_core/src/transport/udp_transport_service.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/models/peer.dart';
import 'package:grassroots_networking_core/src/models/packet.dart';
import 'package:grassroots_networking_core/src/models/rendezvous_server.dart';
import 'package:grassroots_networking_core/src/models/underlay.dart';
import 'package:grassroots_networking_core/src/protocol/protocol_handler.dart';
import 'package:grassroots_networking_core/src/protocol/fragment_handler.dart';
import 'package:grassroots_networking_core/src/protocol/message_transport.dart';
import 'package:grassroots_networking_core/src/routing/message_router.dart';
import 'package:grassroots_networking_core/src/session/application_identity.dart';
import 'package:grassroots_networking_core/src/session/noise_session_manager.dart';
import 'package:grassroots_networking_core/src/session/platform_attestation.dart';
import 'package:grassroots_networking_core/src/store/store.dart';
import 'package:grassroots_networking_core/src/transport/transport_service.dart';

/// Configuration for Grassroots transport
class GrassrootsNetworkConfig {
  /// Whether to auto-connect to discovered peers
  final bool autoConnect;

  /// Whether to start scanning/advertising on init
  final bool autoStart;

  /// Scan duration (null for continuous)
  final Duration? scanDuration;

  /// Local name for BLE advertising
  final String? localName;

  /// Interval for sending periodic ANNOUNCE packets
  final Duration announceInterval;

  /// Interval for periodic BLE scanning (to discover new devices)
  final Duration scanInterval;

  /// How long to wait for an end-to-end ACK after a message is sent before
  /// treating it as lost and re-queueing it for the next drain. Covers two
  /// failure modes that `sendToPeer` cannot detect synchronously:
  ///
  /// - **Stale-link writes.** When the remote turns BLE off, Android keeps
  ///   the central GATT connection open until link-supervision times out
  ///   (~20–30 s). Writes during that window get queued at the OS layer and
  ///   the `_ble.send()` call returns success, but the bytes never leave the
  ///   radio. Without a watchdog the message would sit in `MessageStatus.sent`
  ///   forever.
  /// - **UDP loss.** UDX doesn't surface per-packet delivery; an ACK is the
  ///   only confirmation a message reached the recipient.
  ///
  /// Set high enough to tolerate slow links but low enough that the UI
  /// doesn't feel broken. BLE in-air ACKs are normally sub-second; UDP
  /// across the open Internet is typically <2 s.
  final Duration ackTimeout;

  const GrassrootsNetworkConfig({
    this.autoConnect = true,
    this.autoStart = true,
    this.scanDuration,
    this.localName,
    this.announceInterval = const Duration(seconds: 10),
    this.scanInterval = const Duration(seconds: 10),
    this.ackTimeout = const Duration(seconds: 5),
  });
}

class _QueuedOutboundMessage {
  final String messageId;
  final Uint8List recipientPubkey;
  final Uint8List payload;
  final DateTime queuedAt;
  final int attempts;

  _QueuedOutboundMessage({
    required this.messageId,
    required Uint8List recipientPubkey,
    required Uint8List payload,
    DateTime? queuedAt,
    this.attempts = 0,
  })  : recipientPubkey = Uint8List.fromList(recipientPubkey),
        payload = Uint8List.fromList(payload),
        queuedAt = queuedAt ?? DateTime.now();

  String get recipientPubkeyHex =>
      GrassrootsNetwork._pubkeyToHex(recipientPubkey);

  _QueuedOutboundMessage incrementAttempts() => _QueuedOutboundMessage(
        messageId: messageId,
        recipientPubkey: recipientPubkey,
        payload: payload,
        queuedAt: queuedAt,
        attempts: attempts + 1,
      );
}

/// Tracks an outgoing message that left the device but has not yet been
/// ACK'd. Drives both the ack-timeout watchdog and the BLE-disconnect
/// re-queue: when either fires, we push the payload back onto
/// [GrassrootsNetwork._outboundMessageQueue] so the next drain re-sends it.
///
/// Kept in plain memory rather than Redux because the payload bytes
/// themselves live here — Redux only stores delivery metadata
/// ([OutgoingMessage]). Lifecycle: created when `_trySendMessageNow`
/// dispatches `MessageSentAction`, dropped on `MessageDeliveredAction`
/// (ACK) or on re-queue (timeout / BLE disconnect).
class _AckPendingMessage {
  final String messageId;
  final Uint8List recipientPubkey;
  final Uint8List payload;

  /// The BLE pathId the message was written to, or null if it went via UDP.
  /// Used by the disconnect handler to find which pending messages were on
  /// a path that just dropped.
  final String? bleDeviceId;

  /// Watchdog timer; null while paused (e.g. during re-queue or shutdown).
  Timer? ackTimer;

  _AckPendingMessage({
    required this.messageId,
    required Uint8List recipientPubkey,
    required Uint8List payload,
    required this.bleDeviceId,
  })  : recipientPubkey = Uint8List.fromList(recipientPubkey),
        payload = Uint8List.fromList(payload);
}

/// Actions that mirror a known peer's live UDP address (peers slice, which
/// is not persisted) into the known-peers slice (which is), so the last-known
/// address survives an app restart and can seed reconnection — the hydration
/// path re-associates it on startup.
///
/// One action per known peer whose live address differs from the stored one.
/// A peer with no live address produces nothing: the stored address is the
/// peer's last known location and is never cleared unilaterally (only an
/// explicit peer report may clear it).
@visibleForTesting
List<KnownPeerAddressUpdatedAction> computeKnownPeerAddressMirrorActions({
  required KnownPeersState knownPeers,
  required PeersState peers,
}) {
  final actions = <KnownPeerAddressUpdatedAction>[];
  for (final entry in knownPeers.known.entries) {
    final peer = peers.getPeerByPubkeyHex(entry.key);
    final address = peer?.udpAddress;
    if (address == null || address.isEmpty) continue;
    if (address == entry.value) continue;
    actions.add(KnownPeerAddressUpdatedAction(
      pubkeyHex: entry.key,
      udpAddress: address,
    ));
  }
  return actions;
}

@visibleForTesting
Set<String> computeStaleUdpPeerPubkeys({
  required Iterable<PeerState> peers,
  required Set<String> connectedUdpPubkeys,
  required Duration staleThreshold,
  DateTime? now,
}) {
  final evaluationTime = now ?? DateTime.now();
  final stale = <String>{};

  for (final peer in peers) {
    if (!connectedUdpPubkeys.contains(peer.pubkeyHex)) continue;
    final lastUdpSeen = peer.lastUdpSeen;
    if (lastUdpSeen == null) continue;

    if (evaluationTime.difference(lastUdpSeen) > staleThreshold) {
      stale.add(peer.pubkeyHex);
    }
  }

  return stale;
}

/// Peers whose `bleCentralDeviceId` / `blePeripheralDeviceId` is still set but
/// who haven't surfaced any BLE traffic (ANNOUNCE, message, ACK, RSSI poll)
/// within [staleThreshold]. The BLE transport plugin is supposed to emit a
/// disconnect when a path drops, but in practice that signal is brittle: a
/// path that drifts through `failed` / `subscribed` (status-133 storms, GATT
/// reinit churn) without a clean `ready → dropped` transition never fires
/// `_emitDisconnect`. This sweep is the safety net — applies equally to
/// friends and strangers, so the UI's "Connected Peers" list stops showing
/// peers we have no live BLE link to.
@visibleForTesting
Set<String> computeStaleBlePeerPubkeys({
  required Iterable<PeerState> peers,
  required Duration staleThreshold,
  DateTime? now,
}) {
  final evaluationTime = now ?? DateTime.now();
  final stale = <String>{};

  for (final peer in peers) {
    if (!peer.hasBleConnection) continue;
    final lastBleSeen = peer.lastBleSeen;
    // Without a `lastBleSeen` timestamp we can't say anything. Treat as
    // fresh — the very next ANNOUNCE will populate the field, and any
    // explicit disconnect from the transport will clear the device-id
    // before this sweep matters.
    if (lastBleSeen == null) continue;

    if (evaluationTime.difference(lastBleSeen) > staleThreshold) {
      stale.add(peer.pubkeyHex);
    }
  }

  return stale;
}

/// Main Grassroots transport API.
///
/// This is the entry point for GSG to use Grassroots as a transport layer.
///
/// Usage:
/// ```dart
/// final identity = GrassrootsIdentity(
///   publicKey: myPubKey,
///   privateKey: myPrivKey,
///   nickname: 'Alice',
/// );
///
/// final grassroots = GrassrootsNetwork(
///   identity: identity,
///   underlays: {Underlay.pan, Underlay.lan, Underlay.wan},
///   rendezvousServers: [RendezvousServer('203.0.113.7:9514', serverKey)],
///   store: store,
///   sodium: sodium,
/// );
///
/// grassroots.onMessageReceived = (sender, payload, messageId, underlay) {
///   // Handle incoming GSG block
/// };
///
/// grassroots.onPeerConnected = (pk, underlay, identity) {
///   // peer reachable over `underlay` (PAN, LAN or WAN)
/// };
///
/// await grassroots.initialize();
/// await grassroots.start();
///
/// // Send a message
/// await grassroots.send(recipientPubkey, gsgBlockData);
/// ```
class GrassrootsNetwork {
  /// Our identity (from GSG layer)
  final GrassrootsIdentity identity;

  /// Configuration
  final GrassrootsNetworkConfig config;

  /// Redux store for app state
  final Store<AppState> store;

  /// Initialized libsodium (SUMO) instance for native Ed25519 sign/verify and
  /// the Ed25519↔X25519 conversion used to derive/verify Noise static keys.
  /// The caller (typically `main()`) is responsible for
  /// `await SodiumSumoInit.init()` once at app startup and passing the result
  /// here. SUMO is required for `crypto_sign_ed25519_*_to_curve25519`.
  final SodiumSumo sodium;

  /// The underlays this layer runs (spec §Instantiation, "a subset of PAN,
  /// LAN, WAN"): [Underlay.pan] runs BLE; [Underlay.lan] runs LAN discovery
  /// and IP sessions on the local network; [Underlay.wan] runs IP sessions
  /// with the Internet at large and the rendezvous servers.  An underlay not
  /// run is "simply absent from discovery and peerUnderlays" (§Networking
  /// API).
  final Set<Underlay> underlays;

  /// The rendezvous servers this layer connects to over the WAN (spec
  /// §Instantiation); fixed for the layer's lifetime.
  final List<RendezvousServer> rendezvousServers;

  /// Subscription for listening to store changes
  StreamSubscription<AppState>? _storeSubscription;

  /// Per-peer set of reachable underlays from the previous store tick. Used
  /// by the reachability subscriber to detect, per underlay, underlays that
  /// came up (fire onPeerConnected) or went down (fire onPeerDisconnected). A
  /// missing entry is treated as the empty set, so a peer that materializes
  /// already-reachable fires connect for each live underlay. Only non-empty
  /// sets are retained — an unreachable peer holds no entry.
  final Map<String, Set<Underlay>> _lastKnownUnderlays = {};

  /// Pubkeys (hex) already surfaced via [onPeerDiscovered] this session.
  /// Discovery is the identity-learned event on a proximity underlay — the
  /// first accepted ANNOUNCE carrying a nearby peer's public key, over the PAN
  /// or from a LAN source — which precedes the Noise session and therefore
  /// [onPeerConnected]. Fired once per identity; a peer going out of range
  /// and returning is a re-*connect*, not a re-discover.
  final Set<String> _discoveredPubkeyHexes = {};

  /// Permission handler
  final PermissionHandler _permissions = PermissionHandler();

  /// BLE transport service (null if BLE is disabled or unavailable)
  BleTransportService? _bleService;

  /// UDP transport service (null if UDP is disabled)
  UdpTransportService? _udpService;
  LanDiscoveryService? _lanDiscovery;

  /// Per-connection receive chains (spec §Session Establishment: a receiver
  /// processes each connection's inbound bytes in arrival order). Without
  /// this, a handshake packet's async handler can suspend while the
  /// connection's next packet — its first session-encrypted message — is
  /// processed, and decryption fails because the session is not yet
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
  /// Keyed per medium and pair; waiting for the session happens outside
  /// the chain.
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

  String _handshakePairKey(PeerTransport transport, Uint8List remotePubkey) =>
      '${transport.name}:${_pubkeyToHex(remotePubkey)}';

  /// The IP carrier of the shared message transport (spec §Message
  /// Transport): session-encrypted fragments in UDP datagrams, per-fragment
  /// ACK, retransmit-with-backoff under a bounded per-peer window —
  /// retransmission is the reliability mechanism.
  late final MessageTransportSender _udpMessageSender = MessageTransportSender(
    maxChunk: FragmentHandler.udpMaxChunk,
    sendPacket: (peerHex, packet) async {
      final bytes = await _packetBytesForTransport(
        packet: packet,
        transport: PeerTransport.udp,
        recipientPubkey: _hexToBytes(peerHex),
        peerId: peerHex,
      );
      if (bytes == null) return false;
      return await _udpService?.sendToPeer(peerHex, bytes) ?? false;
    },
  );

  /// The BLE carrier of the same transport: fragments ride GATT writes,
  /// which are link-acknowledged — retransmission is a backstop that rarely
  /// fires (spec §Message Transport).
  late final MessageTransportSender _bleMessageSender =
      MessageTransportSender.gatt(
    sendPacket: (peerHex, packet) async {
      final service = _bleService;
      if (service == null) return false;
      final deviceId = service.getPeerIdForPubkey(_hexToBytes(peerHex));
      if (deviceId == null) return false;
      final bytes = await _packetBytesForTransport(
        packet: packet,
        transport: PeerTransport.bleDirect,
        recipientPubkey: _hexToBytes(peerHex),
        peerId: deviceId,
      );
      if (bytes == null) return false;
      return await service.sendToPeer(deviceId, bytes);
    },
  );

  /// Retransmit the in-flight handshake message with backoff until its
  /// reply arrives (spec §Session Establishment: self-ordering handshake
  /// over the lossy IP carrier; BLE needs no timer — GATT writes are
  /// link-acknowledged).
  void _scheduleHandshakeRetransmit(Uint8List pubkey, {String? peerId}) {
    var attempt = 0;
    void tick() {
      final payload = _noiseSessions.handshakeRetransmitPayload(
          PeerTransport.udp, pubkey);
      if (payload == null || attempt >= 5 || _udpService == null) return;
      attempt++;
      debugPrint('[noise] Retransmitting handshake to '
          '${_pubkeyToHex(pubkey).substring(0, 8)} (attempt $attempt)');
      unawaited(_udpService?.sendToPeer(
        peerId ?? _pubkeyToHex(pubkey),
        GrassrootsPacket(type: PacketType.noiseHandshake, payload: payload)
            .serialize(),
      ));
      Timer(Duration(milliseconds: 600 * (1 << attempt)), tick);
    }

    Timer(const Duration(milliseconds: 800), tick);
  }

  /// Pair key for an inbound handshake packet: the payload's claimed sender
  /// (verified inside handleHandshakePacket); falls back to the connection
  /// id for an undecodable payload, which the handler then rejects anyway.
  String _peekHandshakePairKey(
    GrassrootsPacket packet,
    PeerTransport transport,
    String? peerId,
  ) {
    try {
      return _handshakePairKey(
        transport,
        _noiseSessions.claimedHandshakeSender(packet.payload),
      );
    } catch (_) {
      return '${transport.name}:${peerId ?? "?"}';
    }
  }

  /// Hole-punch services for NAT traversal, keyed by IP family.
  final Map<InternetAddressType, HolePunchService> _holePunchServices = {};

  /// Reads the agent's own interface addresses — its host candidates for
  /// dialing. Injectable so a hermetic test can name the addresses its nodes
  /// listen on; the default is the platform's interface list.
  final LocalHostAddressReader _readLocalHostAddresses;

  /// Our public address (ip:port), as a rendezvous server observed it.
  ///
  /// Null until the first rendezvous connection reflects one (spec
  /// §Connectivity and Address: an agent learns its public address from a
  /// rendezvous server, which observes the source address of its packets).
  String? _publicAddress;

  /// The agent's own interface addresses, last read.
  List<InternetAddress> _localHostAddresses = const [];

  final UdpConnectionService _connectionService = const UdpConnectionService();

  /// The in-flight background read of the local host addresses.
  Future<void>? _localHostAddressFuture;
  int _localHostAddressGeneration = 0;

  /// Timer for periodic ANNOUNCE broadcasts
  Timer? _announceTimer;

  /// Timer for periodic BLE scanning
  Timer? _scanTimer;

  /// Whether the coordinator has been initialized
  bool _initialized = false;

  /// Whether the coordinator has been started
  bool _started = false;

  /// Lock to serialize transport settings changes (prevents overlapping init/dispose)
  Future<void>? _transportUpdateLock;

  /// Subscription for network connectivity changes
  StreamSubscription<List<ConnectivityResult>>? _connectivitySubscription;

  /// Last known connectivity results (to detect actual changes)
  List<ConnectivityResult>? _lastConnectivityResults;

  /// Protocol handler for encoding/decoding packets
  late final ProtocolHandler _protocolHandler;

  /// Fragment handler for large BLE messages
  late final FragmentHandler _fragmentHandler;

  /// Message router for incoming packet processing
  late final MessageRouter _messageRouter;

  /// Per-peer, per-transport Noise XX sessions for payload encryption.
  late final NoiseSessionManager _noiseSessions;

  /// Deduplicate in-flight UDX connection attempts across all callers.
  final Map<String, Future<bool>> _udpConnectInFlight = {};

  /// Deduplicate proactive auto-UDP workflows kicked off by repeated ANNOUNCEs.
  final Map<String, Future<void>> _autoUdpConnectInFlight = {};
  final Map<String, String> _autoUdpLastAddress = {};
  final Map<String, DateTime> _autoUdpRetryAfter = {};

  /// Back off briefly after a failed proactive UDP attempt so repeated BLE
  /// ANNOUNCEs don't start a fresh UDX handshake every few seconds.
  static const _autoUdpRetryBackoff = Duration(seconds: 15);

  /// FIFO outbound message queues keyed by recipient public-key hex.
  ///
  /// These hold application payloads that could not be sent because no live
  /// BLE/UDP path was available. They drain when the peer announces or a UDP
  /// connection event reports the peer as connected.
  final Map<String, List<_QueuedOutboundMessage>> _outboundMessageQueue = {};
  final Set<String> _outboundQueueDrainsInProgress = {};

  /// Messages that left the device but have not been ACK'd yet. Keyed by
  /// `messageId`. The ack-timeout watchdog and the BLE-disconnect re-queue
  /// path both consult this map; ACK handling drops the entry. See
  /// [_AckPendingMessage] for the full rationale.
  final Map<String, _AckPendingMessage> _ackPendingMessages = {};

  /// Serialize rendezvous connect/re-announce work so a public-address update
  /// cannot race a save-triggered connect or a signaling-triggered re-register.
  Future<void> _rendezvousTaskQueue = Future.value();
  final Map<String, Future<bool>> _rendezvousSyncInFlight = {};
  final Map<String, DateTime> _rendezvousRetryAfter = {};
  final Map<String, UdpConnectFailureKind?> _rendezvousLastFailureKind = {};
  final Map<String, String> _lastRendezvousSuppressionLogKey = {};

  static const _rendezvousNetworkUnreachableBackoff = Duration(seconds: 15);
  static const _rendezvousHandshakeTimeoutBackoff = Duration(seconds: 20);

  // ===== Public callbacks =====

  /// Called for every incoming payload (spec §Point-to-Point Communication,
  /// `onMessageReceived(sender, payload, messageId, underlay)`). Receives the
  /// sender's public key, the byte payload, a layer-assigned `messageId`, and
  /// the [Underlay] over which the message was delivered, taken from the
  /// receive path.
  void Function(
    Uint8List sender,
    Uint8List payload,
    String messageId,
    Underlay underlay,
  )? onMessageReceived;

  /// Called for each nearby Grassroots agent found, with the underlay of
  /// discovery (spec §Concepts and Principles, Discovery:
  /// `onPeerDiscovered(pk, underlay)`). The agent's key is known once ANNOUNCE
  /// has run, so this fires on the first accepted ANNOUNCE from a nearby
  /// agent: over the PAN, or from a source within one of the agent's own
  /// local prefixes on the LAN. WAN contact is not discovery and does not
  /// fire it.
  void Function(Uint8List pk, ProximityUnderlay underlay)? onPeerDiscovered;

  /// Called when a peer becomes reachable over an underlay. Fires once per
  /// underlay: a peer reachable over the PAN and over the LAN or WAN fires
  /// twice, and an underlay that drops and later recovers fires again.
  /// Receives the peer's public key, the [Underlay] over which it connected,
  /// and the peer's attested application identity — null where the peer's
  /// platform provides no attestation (spec §Connection and Reachability,
  /// `onPeerConnected(pk, underlay, identity?)`; §Session Establishment). A
  /// peer is reachable only once its session is established, authenticated
  /// AND attested, so this never fires before the attestation exchange.
  void Function(
    Uint8List pk,
    Underlay underlay,
    ApplicationIdentity? identity,
  )? onPeerConnected;

  /// Called when an existing peer sends an ANNOUNCE update.
  void Function(PeerState peer)? onPeerUpdated;

  /// Called when a peer is no longer reachable over a given underlay. Fires
  /// per underlay: losing one of two live underlays fires here for the one
  /// that dropped while the peer stays reachable over the other. Receives the
  /// peer's public key and that [Underlay] (spec §Connection and
  /// Reachability, `onPeerDisconnected(pk, underlay)`).
  void Function(Uint8List pk, Underlay underlay)? onPeerDisconnected;

  /// Called when UDP transport becomes available
  void Function()? onUdpInitialized;

  /// Spec §Connectivity and Address, `onConnectivityStatusChanged(old?,
  /// new?)`: "Callback when the networking layer detects a change in the
  /// agent's public IP address ... Fired on startup and on every address
  /// change." Fired at [start] with the address as it stands, on each
  /// address a rendezvous server reflects, and on its loss.
  ///
  /// Receives `(oldAddress, newAddress)` as `ip:port` strings (or null when
  /// no public address is available). The caller derives the kind of
  /// change:
  ///   - startup / gain: `oldAddress == null && newAddress != null`
  ///   - loss:           `oldAddress != null && newAddress == null`
  ///   - update:         both non-null and `oldAddress != newAddress`
  void Function(String? oldAddress, String? newAddress)?
      onConnectivityStatusChanged;

  /// Spec: `docs/GLP_Networking_API/sections/ip.tex` §Local Network Identity —
  /// "Callback when the fingerprint changes. Receives `(oldId, newId)`, either
  /// of which may be null."
  ///
  /// This is not [onConnectivityStatusChanged]. That one reports the agent's
  /// public address; this one reports the local network it is attached to, and
  /// the two are independent in both directions: the attached local network can
  /// change while the public address does not, and the public address can
  /// change while the attached local network does not. GLP uses this to lower a
  /// LAN trust level upon leaving a trusted local network (spec §Trust levels).
  ///
  /// The fingerprint is best-effort and collision-prone. A match is evidence
  /// that the agent is attached to the same local network as before, not proof
  /// of it; what to do with that is GLP's decision, not the layer's.
  void Function(String? oldId, String? newId)? onNetworkChanged;

  // ===== Convenience accessors for Redux state =====

  PeersState get _peersState => store.state.peers;

  /// The declared places and their platform registrations, or null when no
  /// platform geofencing binding was supplied.
  final PlaceRegistry? _places;

  /// The platform's attestation service (spec §Session Establishment). The
  /// default offers none and verifies what a peer offers against the pinned
  /// roots; an app embedding supplies its platform's producer
  /// (`NativePlatformAttestation`).
  final PlatformAttestation _attestation;

  /// Instantiate the layer with everything it needs to run (spec
  /// §Instantiation: "The layer is created once per agent, with everything it
  /// needs to run; nothing in it changes for the layer's lifetime"): the key
  /// pair ([identity]), the [underlays] it runs, the [rendezvousServers] it
  /// connects to over the WAN (may be empty), the platform attestation or
  /// none, and the platform geofencing or none.  A server's listen port and
  /// static public address are the headless profile's alone (GNA, 2026-10-11,
  /// Q4).  The Redux [store], [sodium] and the host-address reader are
  /// implementation arguments.
  ///
  /// To change the underlays or the rendezvous servers, an embedding disposes
  /// the layer and instantiates another; there are no toggles and no call that
  /// adds or removes a server.
  GrassrootsNetwork({
    required this.identity,
    required Set<Underlay> underlays,
    required List<RendezvousServer> rendezvousServers,
    PlatformAttestation? platformAttestation,
    PlaceGeofenceBackend? placeGeofenceBackend,
    required this.store,
    required this.sodium,
    this.config = const GrassrootsNetworkConfig(),
    LocalHostAddressReader? localHostAddressReader,
  })  : underlays = Set.unmodifiable(underlays),
        rendezvousServers = List.unmodifiable(rendezvousServers),
        _readLocalHostAddresses =
            localHostAddressReader ?? readLocalHostAddresses,
        _attestation = platformAttestation ?? const NoPlatformAttestation(),
        _places = placeGeofenceBackend == null
            ? null
            : PlaceRegistry(backend: placeGeofenceBackend) {
    _protocolHandler = ProtocolHandler(identity: identity, sodium: sodium);
    _fragmentHandler = FragmentHandler();
    _noiseSessions = NoiseSessionManager(identity: identity, sodium: sodium);
    _messageRouter = MessageRouter(
      store: store,
      protocolHandler: _protocolHandler,
      fragmentHandler: _fragmentHandler,
    );
    _setupRouterCallbacks();

    // Listen to network connectivity changes (WiFi ↔ cellular, etc.)
    _connectivitySubscription = Connectivity().onConnectivityChanged.listen(
          _onConnectivityChanged,
        );
    _seedConnectivityState();
    unawaited(_refreshLocalNetwork());

    _storeSubscription = store.onChange.listen((state) {
      // Mirror known peers' live UDP addresses into the persisted known-peers
      // slice so the last-known address survives restart. Converges in one
      // extra change notification: the mirror dispatch updates the slice,
      // after which the addresses compare equal and no further action is
      // produced. (onChange is an async broadcast stream, so dispatching here
      // is not reentrant.)
      for (final action in computeKnownPeerAddressMirrorActions(
        knownPeers: state.knownPeers,
        peers: state.peers,
      )) {
        store.dispatch(action);
      }
      _processReachabilityTransitions(state.peers);
    });
  }

  /// Whether BLE transport is available (initialized and usable)
  bool get _bleAvailable =>
      _bleService != null && store.state.transports.bleState.isUsable;

  /// Whether UDP transport is available (initialized and usable)
  bool get _udpAvailable =>
      _udpService != null && store.state.transports.udpState.isUsable;

  /// The agent's own public key — its globally unique identity. Spec
  /// `docs/GLP_Networking_API/sections/api.tex` §Identity: `getIdentity() ->
  /// PubKey`. The private key is held internally and never returned here.
  Uint8List getIdentity() => identity.publicKey;

  /// The nearby Grassroots agents on the proximity underlays (spec §Concepts
  /// and Principles, Discovery, `getPeers() → List<Peer>`: "Scan and browse
  /// the proximity underlays for nearby Grassroots agents"), limited to them
  /// (GNA, 2026-10-11, Task C): one entry per peer and proximity underlay it
  /// is reachable over, PAN before LAN. A peer reachable over the WAN alone is
  /// not nearby. Until 2026-10-12 this answered every reachable peer.
  List<({Uint8List publicKey, ProximityUnderlay underlay})> getPeers() => [
        for (final peer in _peersState.peersList)
          for (final underlay in ProximityUnderlay.values)
            if (peer.reachableUnderlays
                .contains(Underlay.values.byName(underlay.name)))
              (publicKey: peer.publicKey, underlay: underlay),
      ];

  /// Check if a peer is reachable via any transport
  bool isPeerReachable(Uint8List pubkey) => _peersState.isPeerReachable(pubkey);

  /// The underlays by which [pk] is currently reachable, or an empty list if
  /// it is unreachable (spec §Connection and Reachability,
  /// `peerUnderlays(pk)`) — lets GLP make proximity-aware decisions (e.g.
  /// nearby friends = friends ∩ peers reachable over PAN). Returns a fresh
  /// list ordered PAN-before-IP.
  List<Underlay> peerUnderlays(Uint8List pk) =>
      _peersState.getPeerByPubkey(pk)?.reachableUnderlays.toList() ??
      const <Underlay>[];

  /// The agent's current public `ip:port`, or null if not yet known.
  /// Address changes are surfaced via [onConnectivityStatusChanged].
  String? getPublicAddress() => store.state.transports.publicAddress;

  /// An opaque fingerprint of the local network the agent is currently
  /// attached to, or null when it is attached to none or no constituent is
  /// readable. Spec `docs/GLP_Networking_API/sections/ip.tex` §Local Network
  /// Identity. Changes are surfaced via [onNetworkChanged].
  ///
  /// The same interface enumeration also yields the agent's own local address
  /// prefixes, held in `store.state.localNetwork`. They stay inside the layer:
  /// they decide whether an inbound source address is LAN contact (spec §Cold
  /// Calls), and GLP is given the fingerprint, never the addresses.
  String? networkIdentity() => store.state.localNetwork.networkId;

  /// The public `ip:port` at which the layer observes the connected peer — the
  /// source address of its live, attested UDP session — or null when the peer
  /// is reachable over no IP underlay (spec §Connectivity and Address,
  /// `observedPeerAddress(pk)`, behind `peer_address/2`). Not the dial-book
  /// address, which GLP supplies or a session observed earlier (GNA,
  /// 2026-10-11, Task C); until 2026-10-12 `peerAddress` answered that.
  String? observedPeerAddress(Uint8List pubkey) {
    final peer = _peersState.getPeerByPubkey(pubkey);
    if (peer == null ||
        !peer.reachableUnderlays.any((u) => u != Underlay.pan)) {
      return null;
    }
    final udp = _udpService;
    final peerId = udp?.getPeerIdForPubkey(pubkey);
    if (udp == null || peerId == null) return null;
    final remote = udp.getRemoteAddress(peerId);
    return remote == null
        ? null
        : observedAddressString(remote.ip, remote.port);
  }

  /// Feed the networking layer a peer's current public `ip:port`, learned at
  /// the GLP level (from a rendezvous agent or a BLE-adjacent friend). The
  /// layer records it in its dial book for [send] and reconnection, creating a
  /// dial-book entry even for a peer it has not yet met. Spec
  /// `docs/GLP_Networking_API/sections/ip.tex` §Connectivity and Address.
  ///
  /// [address] must be a non-empty `ip:port` (IPv4, or bracketed IPv6 as
  /// `[ip]:port`); it is canonicalized before storage. An unparseable address
  /// is rejected with an [ArgumentError]. This call only ever *supplies* an
  /// address — it never clears one; stored addresses are cleared only when the
  /// peer itself reports it no longer has one.
  void putPeerAddress(Uint8List pubkey, String address) {
    final parsed = parseAddressString(address);
    if (parsed == null) {
      throw ArgumentError.value(address, 'address', 'not a valid ip:port');
    }
    store.dispatch(AssociateUdpAddressAction(
      publicKey: pubkey,
      address: parsed.toAddressString(),
    ));
    // Supplying an address also supplies the key: the peer joins the known
    // set (recognition + persisted dial book).
    store.dispatch(KnownPeerAddressUpdatedAction(
      pubkeyHex: _pubkeyToHex(pubkey),
      udpAddress: parsed.toAddressString(),
    ));
    _rematchLanInstances();
  }

  /// Supply a peer's public key to the layer without an address. The key
  /// joins the known set: BLE Closed mode recognizes it (spec §Cold-Call
  /// Trust Levels — the layer recognizes only keys GLP supplies, never a
  /// friendship record), per-slot BLE scan filters cover it, and reconnection
  /// sweeps include it once an address is known.
  void putKnownPeer(Uint8List pubkey) {
    store.dispatch(KnownPeerPutAction(_pubkeyToHex(pubkey)));
    store.dispatch(PeerIdentityRegisteredAction(publicKey: pubkey));
    _rematchLanInstances();
  }

  /// Withdraw a peer's key from the known set. The layer stops recognizing
  /// it in Closed mode, drops its scan filters and dial-book entry, and no
  /// longer reconnects to it.
  void removeKnownPeer(Uint8List pubkey) {
    store.dispatch(KnownPeerRemovedAction(_pubkeyToHex(pubkey)));
  }

  // ===== Signing primitives =====
  //
  // Spec `docs/GLP_Networking_API/sections/system-predicates.tex`: signing and
  // verification are not themselves on the seam. The runtime realizes Secure
  // GLP's `sign(Term?, Sig)` and `verify(Term?, Sig?, Key?)` over these two
  // functions, which take and return bytes — the layer never learns about
  // modules, as it never learns about "friend". The layer holds the private
  // key, which is why the primitives live here and not above it.
  //
  // Both are synchronous: Ed25519 over native libsodium is pure CPU work
  // (~5-10 ms on Android, ~1-3 ms on iOS). A pure-Dart implementation was
  // measured at 150-200 ms per verification on Android, which is why the
  // native binding is kept rather than replaced.

  /// Detached Ed25519 signature over [message] under this layer's identity
  /// key. Spec summary table: `sign(bytes) -> signature`.
  Uint8List sign(Uint8List message) => _protocolHandler.signBytes(message);

  /// Whether [signature] is [signer]'s detached Ed25519 signature over
  /// [message]. Spec summary table: `verify(pk, bytes, signature) -> bool`.
  ///
  /// Returns false on any malformed input rather than throwing: a bad
  /// signature and an unparseable one are the same answer to the caller.
  bool verify(Uint8List signer, Uint8List message, Uint8List signature) =>
      _protocolHandler.verifyBytes(
        signature: signature,
        message: message,
        publicKey: signer,
      );

  /// Get peer by public key - from Redux store
  PeerState? getPeer(Uint8List pubkey) => _peersState.getPeerByPubkey(pubkey);

  /// Get latest RSSI for a peer (BLE only)
  int? getRssiForPeer(Uint8List pubkey) {
    final peer = _peersState.getPeerByPubkey(pubkey);
    return peer?.rssi;
  }

  /// Whether BLE — the PAN underlay's carrier — runs and is available.
  bool get isBleEnabled => _bleAvailable;

  /// Whether UDP — the LAN and WAN underlays' carrier — runs and is available.
  bool get isUdpEnabled => _udpAvailable;

  /// The cold-call trust level of [underlay] (spec §Trust levels). Closed
  /// until GLP sets it with [setTrustLevel]. The two proximity underlays carry
  /// their own levels, so an agent may answer strangers over one while
  /// ignoring them over the other.
  ColdCallTrustLevel trustLevelOf(ProximityUnderlay underlay) =>
      store.state.settings.trustLevelOf(underlay);

  /// Whether IP contact from [senderPubkey] is answered (spec §Cold Calls,
  /// Trust levels).
  ///
  /// Unsolicited inbound IP contact from outside the agent's own local network
  /// is governed by no trust level: it is refused unless the peer is in the
  /// known-peer set. LAN contact arrives on this
  /// same path, a LAN session being an IP session, so the two are told apart
  /// by the source address alone — a source within one of the agent's own
  /// local address prefixes is LAN contact, governed by the LAN level; any
  /// other source is governed by no level.
  ///
  /// Independent of every level: configured rendezvous servers (infrastructure
  /// this agent itself dials) and solicited contact — an ANNOUNCE arriving on a connection this
  /// agent dialed, which is keyed by the dialed key (unsolicited inbound
  /// carries a temp connection id until identified).
  ///
  /// Contact whose source falls under no underlay this layer runs is refused
  /// outright: there is no session to be had with it (GNA, 2026-10-11, Q1).
  bool _acceptsUdpContactFrom(Uint8List senderPubkey, {String? udpPeerId}) {
    final underlay = _ipUnderlayOfConnection(udpPeerId);
    if (underlay == null) return false;
    if (_isKnownPeerPubkey(senderPubkey)) return true;
    final hex = _pubkeyToHex(senderPubkey);
    if (_isRendezvousPubkeyHex(hex)) return true;
    if (udpPeerId == hex &&
        _udpService?.getPeerIdForPubkey(senderPubkey) != null) {
      return true;
    }
    if (underlay == Underlay.lan &&
        trustLevelOf(ProximityUnderlay.lan) == ColdCallTrustLevel.open) {
      return true;
    }
    return false;
  }

  bool get _runsPan => underlays.contains(Underlay.pan);
  bool get _runsLan => underlays.contains(Underlay.lan);
  bool get _runsWan => underlays.contains(Underlay.wan);

  /// Whether the layer runs an IP underlay, and so the UDP carrier.
  bool get _runsIp => _runsLan || _runsWan;

  /// The underlay of an IP session with a peer at [address] (GNA, 2026-10-11,
  /// Q1): [Underlay.lan] when the layer runs the LAN and the address lies
  /// within one of the agent's own local prefixes, else [Underlay.wan] when it
  /// runs the WAN, else none --- the layer holds no session with that peer.
  /// "A LAN connection and a WAN connection are both IP connections; the layer
  /// tells them apart by whether the peer's address is on the agent's own
  /// local network" (spec §Networking API); the prefixes come from the
  /// interface enumeration behind [networkIdentity], and an address not known
  /// is not known to be local.
  Underlay? _ipUnderlayFor(InternetAddress? address) {
    if (_runsLan &&
        address != null &&
        store.state.localNetwork.network.containsAddress(address)) {
      return Underlay.lan;
    }
    if (_runsWan) return Underlay.wan;
    return null;
  }

  /// The underlay of the IP contact carried on [udpPeerId], by its source
  /// address ([_ipUnderlayFor]).  This is the whole of how the layer tells
  /// LAN contact from contact from the Internet at large.
  Underlay? _ipUnderlayOfConnection(String? udpPeerId) => _ipUnderlayFor(
        udpPeerId == null ? null : _udpService?.getRemoteAddress(udpPeerId)?.ip,
      );

  /// The underlay of [pk]'s session on [carrier] (spec §Networking API): a
  /// GATT session is [Underlay.pan], and a UDP session is [_udpUnderlayOf].
  /// Null for a carrier that holds no session.
  Underlay? _underlayOf(PeerTransport carrier, Uint8List pk) =>
      switch (carrier) {
        PeerTransport.bleDirect => Underlay.pan,
        PeerTransport.udp => _udpUnderlayOf(pk),
        PeerTransport.webrtc => null,
      };

  /// The underlay of [pk]'s UDP session ([_ipUnderlayFor] of its address), or
  /// null when it falls under no underlay this layer runs.
  Underlay? _udpUnderlayOf(Uint8List pk) =>
      _ipUnderlayOfConnection(_udpService?.getPeerIdForPubkey(pk));

  /// Our public UDP address, as a rendezvous server observed it.
  ///
  /// Null until the first rendezvous connection reflects one — the agent
  /// cannot observe its own public address, and asks no external service for
  /// it (spec §Connectivity and Address).
  String? get udpAddress => _publicAddress;

  /// Whether currently scanning for BLE devices
  bool get isScanning => _bleService?.isScanning ?? false;

  // ===== Lifecycle =====

  /// Initialize the transport layer.
  ///
  /// This will:
  /// 1. Request required permissions
  /// 2. Initialize enabled transports (BLE and/or UDP)
  /// 3. Set up routing
  ///
  /// Call [start] after this to begin scanning/advertising.
  Future<bool> initialize() async {
    if (_initialized) {
      debugPrint('Already initialized');
      return _bleAvailable || _udpAvailable;
    }

    _initialized = true;
    debugPrint('Initializing Grassroots transport');

    bool anyTransportInitialized = false;

    try {
      // BLE carries the PAN.
      if (_runsPan) {
        anyTransportInitialized =
            await _initializeBle() || anyTransportInitialized;
      }

      // UDP carries the LAN and the WAN.
      if (_runsIp) {
        anyTransportInitialized =
            await _initializeUdp() || anyTransportInitialized;
      }

      if (!anyTransportInitialized) {
        debugPrint('No transports could be initialized');
        return false;
      }

      debugPrint(
        'Grassroots transport initialized (BLE: $_bleAvailable, UDP: $_udpAvailable)',
      );

      // Auto-start if configured
      if (config.autoStart) {
        await start();
      }

      return true;
    } catch (e) {
      debugPrint('Failed to initialize: $e');
      return false;
    }
  }

  /// Initialize BLE transport
  Future<bool> _initializeBle() async {
    try {
      debugPrint('Initializing BLE transport');

      // Reset Redux state so the service sees uninitialized
      store.dispatch(
        BleTransportStateChangedAction(TransportState.uninitialized),
      );

      // Request BLE permissions
      final permResult = await _permissions.requestPermissions();
      if (permResult != PermissionResult.granted) {
        debugPrint('BLE permissions not granted: $permResult');
        return false;
      }

      // Create BLE transport service (manages BLE manager + router)
      _bleService = BleTransportService(
        identity: identity,
        store: store,
        localName: config.localName ?? identity.nickname,
      );

      // Wire up callbacks BEFORE initialize — the connectionStream is a
      // broadcast stream that drops events with no listener. BLE connections
      // can arrive during initialize() (e.g. iOS central connecting to our
      // peripheral), so the listener must be in place first.
      _setupBleServiceCallbacks();

      // Initialize the service (dispatches state to Redux)
      final success = await _bleService!.initialize();
      if (!success) {
        debugPrint('BLE service initialization returned false');
        _bleService = null;
        return false;
      }

      debugPrint('BLE transport initialized successfully');
      return true;
    } catch (e, stack) {
      debugPrint('Failed to initialize BLE transport: $e');
      debugPrint('Stack trace: $stack');
      _bleService = null;
      return false;
    }
  }

  /// Initialize UDP transport
  Future<bool> _initializeUdp() async {
    try {
      debugPrint('Initializing UDP transport');

      // Reset Redux state so the service sees uninitialized
      store.dispatch(
        UdpTransportStateChangedAction(TransportState.uninitialized),
      );

      // Create UDP transport service
      _udpService = UdpTransportService(
        identity: identity,
        store: store,
        protocolHandler: _protocolHandler,
      );

      // Initialize the service (dispatches state to Redux)
      final success = await _udpService!.initialize();
      if (!success) {
        debugPrint('UDP service initialization returned false');
        _udpService = null;
        return false;
      }

      // Wire up callbacks
      _setupUdpServiceCallbacks();

      // Create hole-punch services using each raw socket.
      _holePunchServices
        ..clear()
        ..addEntries(
          _udpService!.rawSocketsByType.entries.map(
            (entry) => MapEntry(
              entry.key,
              HolePunchService(
                socket: entry.value,
                senderPubkey: identity.publicKey,
              ),
            ),
          ),
        );

      // Read our own interface addresses in the background; a dial needs one
      // of them for the local end of the pair. The public address is not read
      // here and cannot be: it arrives by rendezvous reflection.
      _localHostAddressFuture = _refreshLocalHostAddresses();

      // Treat the configured rendezvous server as a UDP peer we keep a
      // session with so it can immediately learn or refresh our address.
      _resetRendezvousBackoff();
      unawaited(_syncConfiguredRendezvous(reason: 'udp-initialized'));

      debugPrint('UDP transport initialized successfully');
      onUdpInitialized?.call();
      return true;
    } catch (e, stack) {
      debugPrint('Failed to initialize UDP transport: $e');
      debugPrint('Stack trace: $stack');
      _udpService = null;
      return false;
    }
  }

  /// Start scanning and advertising.
  Future<void> start() async {
    if (_started) {
      debugPrint('Already started');
      return;
    }
    if (!_bleAvailable && !_udpAvailable) {
      debugPrint('Cannot start: no transports available');
      return;
    }

    debugPrint('Starting Grassroots transport');

    // Start BLE if available
    if (_bleAvailable) {
      try {
        await _bleService!.start();
        debugPrint('BLE transport started');
      } catch (e) {
        debugPrint('Failed to start BLE: $e');
      }
    }

    // Start UDP if available
    if (_udpAvailable) {
      try {
        await _udpService!.start();
        debugPrint('UDP transport started');
        if (_runsLan) await _startLanDiscovery();
      } catch (e) {
        debugPrint('Failed to start UDP: $e');
      }
    }

    _started = true;
    // "Fired on startup" (spec §Connectivity and Address): the public address
    // as it stands, none until a rendezvous server reflects one.
    onConnectivityStatusChanged?.call(null, _publicAddress);
    // Keep the Android process unfrozen while transports run — a frozen Dart
    // VM goes ANNOUNCE-silent and never dials reverse BLE legs, while its
    // radio links stay up (peers then flap us through their stale sweeps).
    unawaited(TransportForegroundService.start());
    _startAnnounceTimer();
    _startScanTimer();
    if (_udpAvailable) {
      _resetRendezvousBackoff();
      unawaited(_syncConfiguredRendezvous(reason: 'transport-started'));
    }
  }

  /// Stop scanning and advertising.
  Future<void> stop() async {
    if (!_started) return;

    debugPrint('Stopping Grassroots transport');
    _started = false;
    unawaited(TransportForegroundService.stop());
    _announceTimer?.cancel();
    _announceTimer = null;
    _scanTimer?.cancel();
    _scanTimer = null;

    if (_bleService != null) {
      try {
        await _bleService!.stop();
      } catch (e) {
        debugPrint('Error stopping BLE: $e');
      }
      _noiseSessions.resetTransport(PeerTransport.bleDirect);
    }

    if (_udpService != null) {
      await _stopLanDiscovery();
      try {
        await _udpService!.stop();
      } catch (e) {
        debugPrint('Error stopping UDP: $e');
      }
      _noiseSessions.resetTransport(PeerTransport.udp);
    }
  }

  static Uint8List _hexToBytes(String hex) {
    final bytes = <int>[];
    for (var i = 0; i < hex.length; i += 2) {
      bytes.add(int.parse(hex.substring(i, i + 2), radix: 16));
    }
    return Uint8List.fromList(bytes);
  }

  /// The configured rendezvous server holding [pubkeyHex], or null.
  RendezvousServer? _rendezvousServerFor(String pubkeyHex) {
    for (final server in rendezvousServers) {
      if (server.pubkeyHex == pubkeyHex) return server;
    }
    return null;
  }

  bool _isRendezvousPubkeyHex(String pubkeyHex) =>
      _rendezvousServerFor(pubkeyHex.toLowerCase()) != null;

  String _rendezvousConfigKey(RendezvousServer server) =>
      '${server.pubkeyHex}@${server.address}';

  bool _canDialUdpAddress(AddressInfo address) =>
      _udpService != null &&
      _udpAvailable &&
      _ipUnderlayFor(address.ip) != null &&
      _connectionService.selectBestPairFromAddresses(
            localAddresses: _connectionLocalCandidates(),
            remoteAddresses: [address.toAddressString()],
          ) !=
          null;

  void _resetRendezvousBackoff([String? configKey]) {
    if (configKey == null) {
      _rendezvousRetryAfter.clear();
      _rendezvousLastFailureKind.clear();
      _lastRendezvousSuppressionLogKey.clear();
      return;
    }

    _rendezvousRetryAfter.remove(configKey);
    _rendezvousLastFailureKind.remove(configKey);
    _lastRendezvousSuppressionLogKey.remove(configKey);
  }

  /// Clear per-friend proactive-UDP backoff state.
  ///
  /// Invoked when our own public address changes, because the prior failures
  /// were observed through the previous network path: the OS UDP socket may
  /// have been rebound and NAT mappings invalidated, so the reason for those
  /// failures may no longer apply. Mirrors [_resetRendezvousBackoff].
  void _resetAutoUdpBackoff() {
    if (_autoUdpRetryAfter.isEmpty && _autoUdpLastAddress.isEmpty) return;
    debugPrint(
      '[auto-udp] Resetting per-friend backoff '
      '(${_autoUdpRetryAfter.length} entries) after public-address change',
    );
    _autoUdpRetryAfter.clear();
    _autoUdpLastAddress.clear();
  }

  void _logRendezvousSuppression(String configKey, String key, String message) {
    if (_lastRendezvousSuppressionLogKey[configKey] == key) return;
    _lastRendezvousSuppressionLogKey[configKey] = key;
    debugPrint(message);
  }

  Duration? _rendezvousBackoffForFailure(UdpConnectFailureKind? failureKind) {
    switch (failureKind) {
      case UdpConnectFailureKind.networkUnreachable:
        return _rendezvousNetworkUnreachableBackoff;
      case UdpConnectFailureKind.handshakeTimeout:
        return _rendezvousHandshakeTimeoutBackoff;
      case UdpConnectFailureKind.other:
      case null:
        return null;
    }
  }

  String _describeRendezvousFailure(UdpConnectFailureKind? failureKind) {
    switch (failureKind) {
      case UdpConnectFailureKind.networkUnreachable:
        return 'no usable UDP route';
      case UdpConnectFailureKind.handshakeTimeout:
        return 'UDX handshake timed out';
      case UdpConnectFailureKind.other:
      case null:
        return 'connect failed';
    }
  }

  bool _isRendezvousBackoffActive(RendezvousServer rendezvous, String reason) {
    final configKey = _rendezvousConfigKey(rendezvous);
    final retryAfter = _rendezvousRetryAfter[configKey];
    if (retryAfter == null) return false;

    final remaining = retryAfter.difference(DateTime.now());
    if (remaining <= Duration.zero) {
      _rendezvousRetryAfter.remove(configKey);
      _lastRendezvousSuppressionLogKey.remove(configKey);
      return false;
    }

    _logRendezvousSuppression(
      configKey,
      'cooldown:${_rendezvousLastFailureKind[configKey] ?? "unknown"}',
      '[rendezvous] Skipping $reason — '
          '${_describeRendezvousFailure(_rendezvousLastFailureKind[configKey])}; '
          'retrying in ${remaining.inSeconds}s',
    );
    return true;
  }

  Future<bool> _enqueueRendezvousTask(Future<bool> Function() task) {
    final completer = Completer<bool>();
    _rendezvousTaskQueue = _rendezvousTaskQueue.catchError((_) {}).then((
      _,
    ) async {
      try {
        completer.complete(await task());
      } catch (e) {
        debugPrint('[rendezvous] Task failed: $e');
        completer.complete(false);
      }
    });
    return completer.future;
  }

  Future<bool> _syncConfiguredRendezvous({
    List<RendezvousServer>? configs,
    String reason = 'sync',
  }) async {
    final targets = configs ?? rendezvousServers;
    if (targets.isEmpty) {
      return Future.value(false);
    }

    final results = <bool>[];
    for (final rendezvous in targets) {
      final configKey = _rendezvousConfigKey(rendezvous);
      final inFlight = _rendezvousSyncInFlight[configKey];
      if (inFlight != null) {
        results.add(await inFlight);
        continue;
      }

      late final Future<bool> syncFuture;
      syncFuture = _enqueueRendezvousTask(() async {
        if (_udpService == null || !_udpAvailable) {
          debugPrint('[rendezvous] Cannot $reason — UDP unavailable');
          return false;
        }

        final rendezvousAddress = _parseSupportedUdpAddress(
          rendezvous.address,
          context: 'rendezvous',
        );
        if (rendezvousAddress == null) {
          return false;
        }

        if (!_canDialUdpAddress(rendezvousAddress)) {
          _logRendezvousSuppression(
            configKey,
            'no-route:${rendezvousAddress.ip.type.name}',
            '[rendezvous] Skipping $reason for '
                '${rendezvous.pubkeyHex.substring(0, 8)}... — no usable '
                '${rendezvousAddress.ip.type == InternetAddressType.IPv6 ? "IPv6" : "IPv4"} route',
          );
          return false;
        }

        if (_isRendezvousBackoffActive(rendezvous, reason)) {
          return false;
        }

        _lastRendezvousSuppressionLogKey.remove(configKey);

        final announce = await _createSignedAnnounce();
        if (await _udpService!.sendToPeer(rendezvous.pubkeyHex, announce)) {
          _resetRendezvousBackoff(configKey);
          debugPrint(
            '[rendezvous] Re-announced via existing session '
            '(${rendezvous.pubkeyHex.substring(0, 8)}..., $reason)',
          );
          return true;
        }

        final success = await _sendViaUdp(
          rendezvous.pubkeyHex,
          rendezvous.address,
          announce,
          isRendezvous: true,
        );
        if (success) {
          _resetRendezvousBackoff(configKey);
          debugPrint(
            '[rendezvous] Connected and announced '
            '(${rendezvous.pubkeyHex.substring(0, 8)}..., $reason)',
          );
          return true;
        }

        final failureKind = _udpService!.lastConnectFailureKind;
        _rendezvousLastFailureKind[configKey] = failureKind;
        final backoff = _rendezvousBackoffForFailure(failureKind);
        if (backoff != null) {
          _rendezvousRetryAfter[configKey] = DateTime.now().add(backoff);
          debugPrint(
            '[rendezvous] Failed to connect '
            '(${rendezvous.pubkeyHex.substring(0, 8)}..., $reason): '
            '${_describeRendezvousFailure(failureKind)}; '
            'backing off for ${backoff.inSeconds}s',
          );
        } else {
          debugPrint(
            '[rendezvous] Failed to connect '
            '(${rendezvous.pubkeyHex.substring(0, 8)}..., $reason)',
          );
        }
        return false;
      }).whenComplete(() {
        if (identical(_rendezvousSyncInFlight[configKey], syncFuture)) {
          _rendezvousSyncInFlight.remove(configKey);
        }
      });

      _rendezvousSyncInFlight[configKey] = syncFuture;
      results.add(await syncFuture);
    }

    return results.any((result) => result);
  }

  /// Read the attached local network and, if it differs from the held one,
  /// record it and tell the runtime.
  ///
  /// The layer's own emission point for [onNetworkChanged], separate from the
  /// public-address one: the two facts are independent, so neither refresh
  /// implies the other. Called at construction, on every connectivity event
  /// (before the event is filtered — the local network can change while the
  /// connection type does not), and on the heartbeat tick, which catches a
  /// change the platform reports through no event at all.
  Future<void> _refreshLocalNetwork() async {
    final network = await readLocalNetwork();
    final previous = store.state.localNetwork.network;
    if (network == previous) return;

    store.dispatch(LocalNetworkChangedAction(network));
    debugPrint(
      '[local-network] ${previous.networkId} -> ${network.networkId} '
      '(prefixes: ${network.prefixes.join(', ')})',
    );
    onNetworkChanged?.call(previous.networkId, network.networkId);
  }

  void _seedConnectivityState() {
    unawaited(() async {
      try {
        final results = await Connectivity().checkConnectivity();
        final ipResults = _normalizeConnectivityResults(results);
        _lastConnectivityResults ??= ipResults;
        store.dispatch(
          NetworkConnectionTypeUpdatedAction(
            _connectionTypeFromResults(ipResults),
          ),
        );
      } catch (e) {
        debugPrint('Failed to read initial connectivity state: $e');
      }
    }());
  }

  List<ConnectivityResult> _normalizeConnectivityResults(
    List<ConnectivityResult> results,
  ) {
    final ipResults =
        results.where((r) => r != ConnectivityResult.bluetooth).toList();
    if (ipResults.isEmpty) {
      return [ConnectivityResult.none];
    }
    return ipResults;
  }

  NetworkConnectionType _connectionTypeFromResults(
    List<ConnectivityResult> results,
  ) {
    if (results.contains(ConnectivityResult.none)) {
      return NetworkConnectionType.offline;
    }
    if (results.contains(ConnectivityResult.wifi)) {
      return NetworkConnectionType.wifi;
    }
    if (results.contains(ConnectivityResult.mobile)) {
      return NetworkConnectionType.cellular;
    }
    if (results.contains(ConnectivityResult.ethernet)) {
      return NetworkConnectionType.ethernet;
    }
    if (results.contains(ConnectivityResult.vpn)) {
      return NetworkConnectionType.vpn;
    }
    return NetworkConnectionType.other;
  }

  void _clearDiscoveredPublicConnectivity() {
    _localHostAddressGeneration++;
    final lost = _publicAddress;
    _publicAddress = null;
    _localHostAddresses = const [];
    _localHostAddressFuture = null;
    store.dispatch(ClearPublicConnectivityAction());
    // Spec §Connectivity and Address: "loss: new=null" --- every address
    // change is reported, a loss included.
    if (lost != null) onConnectivityStatusChanged?.call(lost, null);
  }

  /// Handle network connectivity changes (WiFi ↔ cellular, etc.).
  ///
  /// When the network changes, our UDP socket is bound to the old interface
  /// and all UDX connections are dead. We need to:
  /// 1. Tear down the old UDP service (dead socket, dead connections)
  /// 2. Re-initialize with a new socket on the new interface
  /// 3. Re-discover public address (new IP from new network)
  /// 4. Re-register with the configured rendezvous servers
  /// 5. Re-connect to known peers
  void _onConnectivityChanged(List<ConnectivityResult> results) {
    // Filter out irrelevant connection types like bluetooth
    // which just indicate a BLE device connected/disconnected, not an IP network change.
    // If we don't filter this, every BLE connection change tears down the UDP transport!
    final ipResults = _normalizeConnectivityResults(results);
    store.dispatch(
      NetworkConnectionTypeUpdatedAction(_connectionTypeFromResults(ipResults)),
    );

    // Independent of everything below: the attached local network can change
    // while the connection type and the public address do not (one Wi-Fi
    // network swapped for another), so this refresh precedes every filter.
    unawaited(_refreshLocalNetwork());

    // Ignore the first notification (initial state, not a change)
    if (_lastConnectivityResults == null) {
      _lastConnectivityResults = ipResults;
      return;
    }

    // Ignore if nothing meaningful changed
    if (_connectivityResultsEqual(_lastConnectivityResults!, ipResults)) return;
    _lastConnectivityResults = ipResults;
    _clearDiscoveredPublicConnectivity();

    // If we lost all connectivity, nothing to do — connections will fail naturally.
    if (ipResults.contains(ConnectivityResult.none)) {
      debugPrint('Network lost — UDP connections will fail');
      return;
    }

    if (!_runsIp) {
      debugPrint(
        'Network changed while the layer runs no IP underlay — cleared '
        'cached public connectivity',
      );
      return;
    }

    debugPrint(
      'Network changed: $ipResults (raw: $results) — restarting UDP transport',
    );

    // Serialize with other transport updates to prevent overlapping init/dispose
    final previous = _transportUpdateLock ?? Future.value();
    _transportUpdateLock = previous.then(
      (_) => _restartUdpAfterNetworkChange(),
    );
  }

  /// Restart UDP transport after a network change.
  Future<void> _restartUdpAfterNetworkChange() async {
    if (!_runsIp) return;
    if (!_started) return;

    // Tear down old UDP service completely
    for (final service in _holePunchServices.values) {
      service.dispose();
    }
    _holePunchServices.clear();

    await _stopLanDiscovery();
    if (_udpService != null) {
      _noiseSessions.resetTransport(PeerTransport.udp);
      await _udpService!.dispose();
      _udpService = null;
    }

    _clearDiscoveredPublicConnectivity();
    store.dispatch(
      UdpTransportStateChangedAction(TransportState.uninitialized),
    );

    // Mark UDP peers as disconnected (connections are dead)
    for (final peer in _peersState.peersList) {
      if (peer.udpAddress != null) {
        store.dispatch(PeerUdpDisconnectedAction(peer.publicKey));
      }
    }

    // Re-initialize UDP on the new network interface
    final success = await _initializeUdp();
    if (!success) {
      debugPrint('Failed to re-initialize UDP after network change');
      return;
    }

    if (_udpAvailable) {
      await _udpService!.start();
      if (_runsLan) await _startLanDiscovery();
    }

    await _waitForLocalHostAddresses();

    await _reconnectKnownPeers(reason: 'connectivity-changed');
  }

  /// Walk every known peer with a dial-book address and bring it back online
  /// by dialing its recorded address (spec §Connectivity and Address,
  /// "Address-change recovery"). Idempotent —
  /// already-connected peers are skipped by [_connectToPeerViaUdp] and by the
  /// second-pass [getPeerIdForPubkey] guard. (Spec §Heartbeat: the heartbeat
  /// tick drives reconnection attempts toward unreachable peers the layer has
  /// addresses for; who those peers are is GLP's supply.)
  Future<void> _reconnectKnownPeers({required String reason}) async {
    if (!_udpAvailable) return;

    final dialBook = store.state.knownPeers.dialBook;
    if (dialBook.isEmpty) return;

    debugPrint(
      '[reconnect] Sweeping ${dialBook.length} known UDP peers ($reason)',
    );

    for (final entry in dialBook.entries) {
      final peerAddress =
          _peersState.getPeerByPubkeyHex(entry.key)?.udpAddress ?? entry.value;
      await _connectToPeerViaUdp(entry.key, peerAddress);
    }
  }

  /// Public entry point for triggering a known-peer UDP reconnection sweep.
  /// Chains on the transport update lock so it cannot overlap with a
  /// connectivity-driven or settings-driven restart.
  Future<void> reconnectKnownPeers({required String reason}) {
    final previous = _transportUpdateLock ?? Future.value();
    final next = previous.then((_) => _reconnectKnownPeers(reason: reason));
    _transportUpdateLock = next;
    return next;
  }

  // ===== LAN Discovery (spec §Proximity-Based Discovery, §LAN) =====

  /// Start DNS-SD advertising and browsing once UDP is up. The advertised
  /// SRV record carries the bound UDP port (spec §LAN: "the instance's SRV
  /// record carries the agent's LAN address and bound UDP port"); mDNS
  /// supplies the LAN address. Idempotent; a start failure (e.g. missing
  /// local-network permission on iOS) is logged and leaves the IP transport
  /// untouched.
  Future<void> _startLanDiscovery() async {
    if (_lanDiscovery?.isRunning ?? false) return;
    final udp = _udpService;
    if (udp == null) return;
    final port = udp.localPortForAddressType(InternetAddressType.IPv4) ??
        udp.localPortForAddressType(InternetAddressType.IPv6);
    if (port == null) {
      debugPrint('[lan] no bound UDP port; LAN discovery not started');
      return;
    }
    final lan = LanDiscoveryService(identity: identity)
      ..onInstanceResolved = _onLanInstanceResolved;
    _lanDiscovery = lan;
    try {
      await lan.start(port: port);
    } catch (e) {
      debugPrint('[lan] failed to start LAN discovery: $e');
      _lanDiscovery = null;
    }
  }

  Future<void> _stopLanDiscovery() async {
    _lanResolvedInstances.clear();
    final lan = _lanDiscovery;
    _lanDiscovery = null;
    if (lan == null) return;
    try {
      await lan.stop();
    } catch (e) {
      debugPrint('[lan] error stopping LAN discovery: $e');
    }
  }

  /// LAN instances resolved this session, token → (address, seen-at).
  /// A token outlives its resolution by at most one adjacent slot, so
  /// entries older than two slots are pruned. Kept so a key supplied
  /// *after* its instance resolved (GLP calling [putKnownPeer] while mDNS
  /// is already up) is still recognized — [_rematchLanInstances] replays
  /// the cache whenever the known set grows.
  final Map<String, ({String address, DateTime at})> _lanResolvedInstances =
      {};

  /// A browsed LAN instance resolved (spec §Proximity: recognition before
  /// contact). Cache it for late-arriving keys, then try contact.
  void _onLanInstanceResolved(String token, String address) {
    final cutoff = DateTime.now()
        .subtract(GrassrootsIdentity.bleSlotDuration * 2);
    _lanResolvedInstances
      ..removeWhere((_, entry) => entry.at.isBefore(cutoff))
      ..[token] = (address: address, at: DateTime.now());
    _tryLanContact(token, address);
  }

  /// Replay cached LAN instances against the (grown) known set. Called by
  /// [putKnownPeer] and [putPeerAddress]; already-connected peers short out
  /// in [_tryLanContact].
  void _rematchLanInstances() {
    if (_lanResolvedInstances.isEmpty) return;
    for (final entry in _lanResolvedInstances.entries.toList()) {
      _tryLanContact(entry.key, entry.value.address);
    }
  }

  /// A token matching a known peer's key feeds the observed address into
  /// dialing directly — never into distribution — and the deterministic
  /// initiator (lexicographically smaller public key, as everywhere) dials;
  /// the other side waits for the inbound connection. The recognized key is
  /// the expected static: [_connectToPeerViaUdp] dials that key, and the
  /// session aborts on a key mismatch.
  ///
  /// An unmatched instance is a cold call: under Open trust the layer contacts
  /// it as well as answering it (spec §Discovery and §Cold-Call Trust Levels —
  /// Open completes the ANNOUNCE exchange and the session handshake with any
  /// unknown contact on the medium). It is [_lanColdCall] that dials, with no
  /// expected key: the key is unset until the peer's ANNOUNCE claims it and the
  /// handshake binds it, exactly as BLE first contact works.
  void _tryLanContact(String token, String address) {
    final matched = matchLanToken(
      token,
      store.state.knownPeers.known.keys.map(_hexToBytes),
    );
    if (matched == null) {
      _lanColdCall(token, address);
      return;
    }
    final pubkeyHex = _pubkeyToHex(matched);
    final peerShort = pubkeyHex.substring(0, 8);
    final parsed = parseAddressString(address);
    if (parsed == null) {
      debugPrint('[lan] unusable address $address for $peerShort');
      return;
    }
    final canonical = parsed.toAddressString();
    debugPrint('[lan] recognized known peer $peerShort at $canonical');
    store.dispatch(
      AssociateUdpAddressAction(publicKey: matched, address: canonical),
    );
    if (_udpService?.getPeerIdForPubkey(matched) != null) return;
    final myHex = _pubkeyToHex(identity.publicKey);
    if (myHex.compareTo(pubkeyHex) < 0) {
      unawaited(_connectToPeerViaUdp(pubkeyHex, canonical));
    } else {
      debugPrint(
        '[lan] deterministic initiator is $peerShort; waiting for its dial',
      );
    }
  }

  /// Contact an unmatched LAN instance under Open trust.
  ///
  /// Until 2026-08-02 this was inbound-only — an unmatched peer who recognized
  /// us dialed and we accepted — on the ground that outbound paths are keyed by
  /// the peer's public key, which a cold call does not have. The consequence
  /// was that two mutual strangers on one local network never connected: each
  /// accepted, neither initiated. Sections §Discovery and §Cold-Call Trust
  /// Levels say Open contacts as well as answers, so that was a deviation.
  ///
  /// The key is not needed to dial. A UDP path is keyed by an opaque peer id,
  /// and an unsolicited *inbound* datagram already mints one from the source
  /// address and lets the sender's ANNOUNCE re-key it to their public key
  /// ([UdpTransportService.mapIncomingConnectionToPubkey]). An outbound cold
  /// call is the same thing in the other direction: open the path under the
  /// address, send our signed ANNOUNCE, and let theirs claim it. The expected
  /// static is therefore unset until ANNOUNCE supplies it and the Noise
  /// handshake binds it — the BLE first-contact rule, applied to LAN.
  ///
  /// Both sides dialing is not a problem to prevent: neither knows the other's
  /// key yet, so the deterministic-initiator rule has nothing to order them by.
  /// Each side's ANNOUNCE identifies it to the other, and the existing
  /// handshake glare resolution settles the pair from there.
  void _lanColdCall(String token, String address) {
    final trust = trustLevelOf(ProximityUnderlay.lan);
    if (trust != ColdCallTrustLevel.open) {
      debugPrint(
        '[lan] unmatched instance $token ignored (lan trust=${trust.name})',
      );
      return;
    }
    final parsed = parseAddressString(address);
    if (parsed == null) {
      debugPrint('[lan] unusable address $address for unmatched $token');
      return;
    }
    final udp = _udpService;
    if (udp == null) {
      debugPrint('[lan] UDP unavailable; cannot cold-call $token');
      return;
    }
    final canonical = parsed.toAddressString();
    if (_ipUnderlayFor(parsed.ip) == null) {
      debugPrint('[lan] $canonical falls under no underlay this layer runs');
      return;
    }
    // A path to that address already exists — identified, or a temp one from
    // their datagram having arrived first. Dialing again would only duplicate
    // it, and whichever ANNOUNCE lands first identifies the peer either way.
    if (udp.peerIdForAddress(canonical) != null) return;

    debugPrint('[lan] cold-calling unmatched instance $token at $canonical');
    unawaited(_lanColdCallAnnounce(canonical, parsed));
  }

  Future<void> _lanColdCallAnnounce(
    String canonical,
    AddressInfo parsed,
  ) async {
    final udp = _udpService;
    if (udp == null) return;
    // The temp peer id IS the canonical address, the same convention the
    // inbound path uses, so their ANNOUNCE re-keys this very path.
    final opened = await udp.connectToPeer(canonical, parsed.ip, parsed.port);
    if (!opened) {
      debugPrint('[lan] could not open a path to $canonical');
      return;
    }
    final announce = await _createSignedAnnounce();
    final sent = await udp.sendToPeer(canonical, announce);
    debugPrint(
      sent
          ? '[lan] ANNOUNCE sent to $canonical; awaiting theirs'
          : '[lan] ANNOUNCE to $canonical failed to send',
    );
  }

  /// Check if two connectivity result lists are equivalent.
  static bool _connectivityResultsEqual(
    List<ConnectivityResult> a,
    List<ConnectivityResult> b,
  ) {
    if (a.length != b.length) return false;
    final sortedA = List<ConnectivityResult>.from(a)
      ..sort((x, y) => x.index - y.index);
    final sortedB = List<ConnectivityResult>.from(b)
      ..sort((x, y) => x.index - y.index);
    for (var i = 0; i < sortedA.length; i++) {
      if (sortedA[i] != sortedB[i]) return false;
    }
    return true;
  }

  /// Apply a debug change to which BLE roles this device runs.
  /// Dispatches a Redux action and restarts the BLE transport so the new
  /// mode takes effect immediately.
  Future<void> setBleRoleMode(BleRoleMode mode) async {
    if (store.state.settings.bleRoleMode == mode) return;
    store.dispatch(SetBleRoleModeAction(mode));
    await _bleService?.applyRoleModeChange();
  }

  /// Set the cold-call trust level of the given [ProximityUnderlay] to Open
  /// or Closed (spec §Concepts and Principles, Discovery:
  /// `setTrustLevel(underlay, level)`; §Trust levels). Until set, both levels
  /// are Closed.
  ///
  /// A LAN session and a WAN session are both IP sessions, and only LAN
  /// contact — a source within one of the agent's own local prefixes — is
  /// governed by the LAN level; WAN contact is governed by no level. The two
  /// levels are independent — GLP raises one upon entering a trusted place or
  /// local network and lowers it upon leaving.
  Future<void> setTrustLevel(
    ProximityUnderlay underlay,
    ColdCallTrustLevel level,
  ) async {
    if (store.state.settings.trustLevelOf(underlay) == level) return;
    store.dispatch(SetColdCallTrustLevelAction(underlay, level));
  }

  // ===== Hole punching =====

  /// Open a NAT mapping toward [address] and return at once. Spec
  /// `docs/GLP_Networking_API/sections/system-predicates.tex`, backing
  /// `punch_udp/1`.
  ///
  /// [address] is an `ip:port` (IPv4, or bracketed IPv6 as `[ip]:port`); an
  /// unparseable one is rejected with an [ArgumentError], as [putPeerAddress]
  /// rejects it. No peer key is named and none is needed: punching is the
  /// mechanism of opening the mapping, and *who* to punch toward, *when*, and
  /// *whether* are GLP's decisions, taken above this line.
  ///
  /// Fire-and-forget by construction. The punch cannot report success — a NAT
  /// mapping opens silently and is observed only by the traffic that later
  /// crosses it — so this returns void and the datagrams continue in the
  /// background. GLP punches and then waits to hear from the peer; it does not
  /// wait on this call. A punch toward an address on an IP family this layer
  /// has no socket for is a no-op.
  void punchUdp(String address) {
    final parsed = parseAddressString(address);
    if (parsed == null) {
      throw ArgumentError.value(address, 'address', 'not a valid ip:port');
    }
    final service = _holePunchServiceFor(parsed.ip);
    if (service == null) {
      debugPrint(
        '[punch-udp] No hole-punch service for ${parsed.ip.type.name}; '
        'ignoring punch toward ${parsed.toAddressString()}',
      );
      return;
    }
    debugPrint('[punch-udp] Punching toward ${parsed.toAddressString()}');
    unawaited(service.punch(parsed.ip, parsed.port));
  }

  // ===== Places =====

  /// Register a geofence of [radiusMetres] around the device's **current**
  /// location under [place]. Spec
  /// `docs/GLP_Networking_API/sections/system-predicates.tex`:
  /// `declarePlace(place, radius)`.
  ///
  /// Returns false when the platform refuses, in which case [place] is left
  /// undeclared. A further declaration under the same name replaces the first.
  /// GLP supplies a name and a radius and receives no coordinates: there is no
  /// call here that returns a position.
  ///
  /// Platforms bound how many geofences an application may register, so a place
  /// no longer wanted is passed to [removePlace] rather than left in place;
  /// [dispose] releases whatever is still declared.
  Future<bool> declarePlace(String place, double radiusMetres) {
    final places = _places;
    if (places == null) {
      throw StateError(
        'declarePlace needs a PlaceGeofenceBackend; none was supplied to '
        'GrassrootsNetwork',
      );
    }
    return places.declarePlace(place, radiusMetres);
  }

  /// End the declaration of [place]: the geofence is unregistered with the
  /// platform. Spec: `removePlace(place)`. Removing a place that is not
  /// declared does nothing.
  Future<void> removePlace(String place) {
    final places = _places;
    if (places == null) {
      throw StateError(
        'removePlace needs a PlaceGeofenceBackend; none was supplied to '
        'GrassrootsNetwork',
      );
    }
    return places.removePlace(place);
  }

  /// Called for each event of a place still declared. Spec: `onPlaceEvent(cb)`.
  /// The runtime routes each onto the stream that place's
  /// `place_declare(Place?, Radius?, Events)` bound.
  ///
  /// Carries `entered` and `exited` as the platform reports crossings,
  /// `unobservable` when the platform stops reporting them, and `observable`
  /// when it resumes. Between the two, no crossing is seen and none is
  /// delivered late.
  ///
  /// A crossing the platform reports for a place already removed or superseded
  /// is dropped, the report being able to race the unregistration; it is
  /// neither queued nor does it reopen the declaration.
  set onPlaceEvent(void Function(String place, PlaceEvent event)? cb) {
    final places = _places;
    if (places == null) {
      throw StateError(
        'onPlaceEvent needs a PlaceGeofenceBackend; none was supplied to '
        'GrassrootsNetwork',
      );
    }
    places.onPlaceEvent = cb;
  }

  /// The places currently declared. Empty when no platform geofencing binding
  /// was supplied.
  Iterable<String> get declaredPlaces => _places?.declaredPlaces ?? const [];

  static const _uuid = Uuid();

  // ===== Messaging =====

  /// Send a message to a specific peer.
  ///
  /// Routes through the best available transport:
  /// 1. Bluetooth (if peer is nearby and BLE is enabled)
  /// 2. UDP (if peer has UDP address and UDP is enabled)
  ///
  /// Returns the message ID if the message was sent or queued, null only for
  /// invalid input.
  /// The message status can be tracked via store.state.messages.
  ///
  /// Transport selection: tries BLE first (preferred for nearby peers),
  /// falls back to UDP; a peer reachable over neither is queued for.
  /// Delivery is confirmed by an application-level ACK, not the transport write.
  Future<String?> send(
    Uint8List recipientPubkey,
    Uint8List payload, {
    String? messageId,
  }) async {
    if (recipientPubkey.length != 32) {
      debugPrint('Cannot send: recipient public key must be 32 bytes');
      return null;
    }

    // Use provided message ID or generate one. Full UUID required — the
    // MESSAGE payload carries it as a fixed 36-character prefix.
    messageId ??= _uuid.v4();

    // Dispatch sending action (clock icon)
    store.dispatch(
      MessageSendingAction(
        messageId: messageId,
        underlay: Underlay.pan, // Tentative — updated on actual send
        recipientPubkey: recipientPubkey,
        payloadSize: payload.length,
      ),
    );

    if (await _trySendMessageNow(
      recipientPubkey: recipientPubkey,
      payload: payload,
      messageId: messageId,
    )) {
      return messageId;
    }

    _queueOutboundMessage(
      recipientPubkey: recipientPubkey,
      payload: payload,
      messageId: messageId,
    );
    return messageId;
  }

  /// Hand [payload] to the carrier of an underlay over which the peer is
  /// reachable — its session there established, authenticated and attested —
  /// and answer whether it did (spec §Session Establishment; GNA, 2026-10-11,
  /// Task A: `send` is held until the peer is reachable). PAN first, then the
  /// peer's IP underlay. A peer reachable over none is connected to in the
  /// background ([_openPathsTo]), and the attestation exchange's completion
  /// drains the queue ([_onAttestationReceived]).
  Future<bool> _trySendMessageNow({
    required Uint8List recipientPubkey,
    required Uint8List payload,
    required String messageId,
  }) async {
    final peer = _peersState.getPeerByPubkey(recipientPubkey);
    if (peer == null) {
      debugPrint('Cannot send now: peer not found; message will remain queued');
      return false;
    }

    // --- The PAN, preferred for nearby peers ---
    final bleDeviceId = _connectedBleDeviceIdForPeer(peer);
    if (peer.reachableUnderlays.contains(Underlay.pan) &&
        _bleAvailable &&
        _bleService != null &&
        bleDeviceId != null) {
      debugPrint('Sending via BLE to ${peer.displayName}');

      final success = await _bleMessageSender.sendMessage(
        _pubkeyToHex(recipientPubkey),
        messageId,
        payload,
      );

      if (success) {
        _markSentAndTrackForAck(
          messageId: messageId,
          recipientPubkey: recipientPubkey,
          payload: payload,
          underlay: Underlay.pan,
          bleDeviceId: bleDeviceId,
        );
        // Delivery confirmed by ACK, not BLE write success.
        return true;
      }
      debugPrint('BLE send failed, falling back to UDP...');
    }

    // --- The LAN or the WAN: the peer's attested UDP session ---
    if (_udpAvailable && _udpService != null) {
      // Re-read peer — state may have changed during the BLE attempt.
      final resolvedPeer = _peersState.getPeerByPubkey(recipientPubkey) ?? peer;
      final ipUnderlay = resolvedPeer.reachableUnderlays
          .where((u) => u != Underlay.pan)
          .firstOrNull;
      if (ipUnderlay != null &&
          _udpService!.getPeerIdForPubkey(recipientPubkey) != null &&
          await _udpMessageSender.sendMessage(
            resolvedPeer.pubkeyHex,
            messageId,
            payload,
          )) {
        _markSentAndTrackForAck(
          messageId: messageId,
          recipientPubkey: recipientPubkey,
          payload: payload,
          underlay: ipUnderlay,
          bleDeviceId: null,
        );
        return true;
      }
    }

    debugPrint(
      '${peer.displayName} is not reachable over any underlay; holding the '
      'message until it is',
    );
    unawaited(_openPathsTo(recipientPubkey));
    return false;
  }

  /// Path openings in progress, by pubkey hex: one at a time per peer.
  final Map<String, Future<void>> _pathOpeningsInFlight = {};

  /// Bring a session with [pubkey] up where none is attested: over a GATT
  /// link already standing, start its handshake; over IP, connect on demand
  /// from the dial book, introduce ourselves by ANNOUNCE, and start the
  /// handshake.  A peer with no usable address waits for one: who learns an
  /// address is the social graph's, and it enters the layer by
  /// putPeerAddress (spec §Connectivity and Address). The attestation exchange follows each session
  /// ([_onNoiseSessionEstablished]); its completion makes the peer reachable
  /// and drains the queue.
  Future<void> _openPathsTo(Uint8List pubkey) {
    final pubkeyHex = _pubkeyToHex(pubkey);
    final inFlight = _pathOpeningsInFlight[pubkeyHex];
    if (inFlight != null) return inFlight;
    final opening = _openPaths(pubkey, pubkeyHex);
    _pathOpeningsInFlight[pubkeyHex] = opening;
    return opening.whenComplete(() {
      if (identical(_pathOpeningsInFlight[pubkeyHex], opening)) {
        _pathOpeningsInFlight.remove(pubkeyHex);
      }
    });
  }

  Future<void> _openPaths(Uint8List pubkey, String pubkeyHex) async {
    final peer = _peersState.getPeerByPubkey(pubkey);
    if (peer == null) return;
    final reachable = peer.reachableUnderlays;

    if (!reachable.contains(Underlay.pan) &&
        _connectedBleDeviceIdForPeer(peer) != null) {
      unawaited(_startNoiseHandshakeForPeer(
        transport: PeerTransport.bleDirect,
        peer: peer,
      ));
    }

    final udp = _udpService;
    if (udp == null || !_udpAvailable) return;
    if (reachable.any((u) => u != Underlay.pan)) return;

    if (udp.getPeerIdForPubkey(pubkey) != null) {
      // A path stands and its session is not attested yet: make sure its
      // handshake is under way.
      await _ensureNoiseSession(
        transport: PeerTransport.udp,
        recipientPubkey: pubkey,
        peerId: pubkeyHex,
      );
      return;
    }

    Future<bool> dial(PeerState target) async {
      final candidates = _udpCandidatesForPeer(target);
      if (candidates.isEmpty) return false;
      final address = target.udpAddress ?? candidates.first;
      debugPrint('Connecting via UDP to ${target.displayName} at $address');
      if (!await _ensureUdpConnection(target.pubkeyHex, address)) return false;
      // Introduce ourselves so the acceptor can verify us and initiate the
      // session (dial sequence, Implementation Notes); and initiate it
      // ourselves, the glare rule settling the pair.
      await _udpService?.sendToPeer(
          target.pubkeyHex, await _createSignedAnnounce());
      await _ensureNoiseSession(
        transport: PeerTransport.udp,
        recipientPubkey: pubkey,
        peerId: target.pubkeyHex,
      );
      return true;
    }

    if (await dial(peer)) return;
    debugPrint('[send] No direct path to ${peer.displayName}');
  }

  void _queueOutboundMessage({
    required Uint8List recipientPubkey,
    required Uint8List payload,
    required String messageId,
  }) {
    final pubkeyHex = _pubkeyToHex(recipientPubkey);
    final queue = _outboundMessageQueue.putIfAbsent(pubkeyHex, () => []);
    final existingIndex = queue.indexWhere((msg) => msg.messageId == messageId);
    final queued = _QueuedOutboundMessage(
      messageId: messageId,
      recipientPubkey: recipientPubkey,
      payload: payload,
      queuedAt: existingIndex >= 0 ? queue[existingIndex].queuedAt : null,
      attempts: existingIndex >= 0 ? queue[existingIndex].attempts : 0,
    );

    if (existingIndex >= 0) {
      queue[existingIndex] = queued;
    } else {
      queue.add(queued);
    }

    store.dispatch(MessageQueuedAction(messageId: messageId));
    debugPrint(
      '[queue] Queued message $messageId for ${pubkeyHex.substring(0, 8)} '
      '(${queue.length} pending)',
    );
  }

  void _drainQueuedMessagesForPeer(Uint8List recipientPubkey) {
    final pubkeyHex = _pubkeyToHex(recipientPubkey);
    final queue = _outboundMessageQueue[pubkeyHex];
    if (queue == null || queue.isEmpty) return;
    if (_outboundQueueDrainsInProgress.contains(pubkeyHex)) return;

    _outboundQueueDrainsInProgress.add(pubkeyHex);
    unawaited(() async {
      try {
        while (queue.isNotEmpty) {
          final queued = queue.first;

          // Skip messages whose Redux status is already past `sent` (i.e.
          // delivered/read/failed). The race that gets us here: an ACK
          // arrived after the watchdog timer fired and re-queued the
          // message, so the message is both `delivered` in Redux AND
          // present in the outbound queue. Re-sending would burn a round
          // trip and trigger duplicate-drop on the recipient.
          final current =
              store.state.messages.outgoingMessages[queued.messageId];
          if (current != null &&
              (current.status == MessageStatus.delivered ||
                  current.status == MessageStatus.read ||
                  current.status == MessageStatus.failed)) {
            debugPrint(
              '[queue] Skipping ${queued.messageId} for '
              '${pubkeyHex.substring(0, 8)} — already ${current.status.name}',
            );
            queue.removeAt(0);
            continue;
          }

          final attemptCounted = queued.incrementAttempts();
          queue[0] = attemptCounted;

          store.dispatch(
            MessageSendingAction(
              messageId: attemptCounted.messageId,
              underlay: Underlay.pan,
              recipientPubkey: attemptCounted.recipientPubkey,
              payloadSize: attemptCounted.payload.length,
            ),
          );

          final sent = await _trySendMessageNow(
            recipientPubkey: attemptCounted.recipientPubkey,
            payload: attemptCounted.payload,
            messageId: attemptCounted.messageId,
          );

          if (!sent) {
            store.dispatch(
                MessageQueuedAction(messageId: attemptCounted.messageId));
            debugPrint(
              '[queue] Drain paused for ${pubkeyHex.substring(0, 8)} after '
              '${attemptCounted.attempts} attempt(s)',
            );
            break;
          }

          queue.removeAt(0);
          debugPrint(
            '[queue] Sent queued message ${attemptCounted.messageId} to '
            '${pubkeyHex.substring(0, 8)} (${queue.length} remaining)',
          );
        }

        if (queue.isEmpty) {
          _outboundMessageQueue.remove(pubkeyHex);
        }
      } finally {
        _outboundQueueDrainsInProgress.remove(pubkeyHex);
      }
    }());
  }

  void _drainQueuedMessagesForLivePeers() {
    final queuedPeerHexes = List<String>.from(_outboundMessageQueue.keys);
    for (final pubkeyHex in queuedPeerHexes) {
      final peer = _peersState.getPeerByPubkeyHex(pubkeyHex);
      if (peer == null || !peer.isReachable) continue;
      _drainQueuedMessagesForPeer(peer.publicKey);
    }
  }

  // ===== ACK-pending tracker =====
  //
  // `sendToPeer` (BLE) and `_sendPacketViaUdp` return success the moment the
  // OS accepts the bytes — they cannot tell whether the recipient actually
  // got them. Two failure modes leak through:
  //
  //   - **Stale link.** The remote turns BLE off; Android keeps the central
  //     GATT connection "open" until link-supervision times out (~20–30 s).
  //     Writes during that window queue at the OS layer and silently die.
  //   - **UDP loss.** UDX doesn't surface per-packet delivery.
  //
  // For every successful send we register the messageId+payload in
  // `_ackPendingMessages` along with a watchdog timer. The recipient's ACK
  // cancels the timer; if it doesn't arrive, the timer fires and pushes the
  // payload back onto the outbound queue for the next drain. The BLE
  // disconnect listener also walks the map and re-queues anything that was
  // in flight on the dropped path — that path it doesn't even need to wait
  // for the timeout.
  //
  // Note: this is a watchdog in `GrassrootsNetwork` (the coordinator), not
  // a reducer-side heuristic. Per `CLAUDE.md`, reducers must only project
  // transport-emitted facts; inference about "I haven't heard back" lives
  // here and surfaces as an explicit `MessageQueuedAction` when it triggers.

  /// Dispatches `MessageSentAction` and registers the message in
  /// [_ackPendingMessages] with a fresh ack-timeout watchdog.
  void _markSentAndTrackForAck({
    required String messageId,
    required Uint8List recipientPubkey,
    required Uint8List payload,
    required Underlay underlay,
    required String? bleDeviceId,
  }) {
    store.dispatch(
      MessageSentAction(
        messageId: messageId,
        underlay: underlay,
        recipientPubkey: recipientPubkey,
        payloadSize: payload.length,
      ),
    );
    _trackAckPending(
      messageId: messageId,
      recipientPubkey: recipientPubkey,
      payload: payload,
      bleDeviceId: bleDeviceId,
    );
  }

  void _trackAckPending({
    required String messageId,
    required Uint8List recipientPubkey,
    required Uint8List payload,
    required String? bleDeviceId,
  }) {
    // Cancel any previous in-flight tracking for this messageId so a quick
    // resend-after-drain doesn't leak two timers for the same id.
    _ackPendingMessages.remove(messageId)?.ackTimer?.cancel();

    final pending = _AckPendingMessage(
      messageId: messageId,
      recipientPubkey: recipientPubkey,
      payload: payload,
      bleDeviceId: bleDeviceId,
    );
    pending.ackTimer = Timer(
      config.ackTimeout,
      () => _onAckTimedOut(messageId),
    );
    _ackPendingMessages[messageId] = pending;
  }

  /// ACK arrived for a message — drop the watchdog. Called from the ACK
  /// handler after `MessageDeliveredAction` is dispatched.
  void _clearAckPending(String messageId) {
    _ackPendingMessages.remove(messageId)?.ackTimer?.cancel();
  }

  void _onAckTimedOut(String messageId) {
    final pending = _ackPendingMessages.remove(messageId);
    if (pending == null) return;
    pending.ackTimer = null;

    // Race guard: if the ACK arrived between `_clearAckPending` and the
    // timer firing — or before `_markSentAndTrackForAck` even registered
    // this entry, in which case `_clearAckPending` was a no-op — the
    // message is already delivered in Redux. Re-queueing would silently
    // burn a redundant retry.
    final current = store.state.messages.outgoingMessages[messageId];
    if (current != null &&
        (current.status == MessageStatus.delivered ||
            current.status == MessageStatus.read)) {
      return;
    }

    debugPrint(
      '[ack-timeout] No ACK for $messageId within '
      '${config.ackTimeout.inMilliseconds}ms; re-queueing.',
    );
    _requeueAckPendingMessage(pending);
  }

  /// BLE path dropped — re-queue every message we sent on that path that
  /// is still waiting for an ACK. The watchdog would catch them eventually,
  /// but the disconnect is hard evidence the bytes never landed, so don't
  /// make the user wait.
  void _requeueMessagesOnBleDisconnect(String bleDeviceId) {
    // Snapshot before mutating: re-queueing pushes into a different map but
    // we also remove from `_ackPendingMessages`, and direct iteration would
    // throw on concurrent modification.
    final affected = _ackPendingMessages.values
        .where((p) => p.bleDeviceId == bleDeviceId)
        .toList(growable: false);
    for (final pending in affected) {
      _ackPendingMessages.remove(pending.messageId);
      pending.ackTimer?.cancel();
      pending.ackTimer = null;

      // Skip re-queue if the ACK already landed (Redux says delivered/read).
      // The disconnect fires after the link supervision detects the drop,
      // which can be several seconds late on Android — plenty of room for
      // an ACK to have arrived in the interim.
      final current =
          store.state.messages.outgoingMessages[pending.messageId];
      if (current != null &&
          (current.status == MessageStatus.delivered ||
              current.status == MessageStatus.read)) {
        continue;
      }

      debugPrint(
        '[ble-disconnect] Re-queueing in-flight message '
        '${pending.messageId} after $bleDeviceId dropped.',
      );
      _requeueAckPendingMessage(pending);
    }
  }

  void _requeueAckPendingMessage(_AckPendingMessage pending) {
    // Pushes back into `_outboundMessageQueue`, which also dispatches
    // `MessageQueuedAction` (status `sent` → `queued`). The next drain
    // trigger (ANNOUNCE / UDP connect / app resume / stale-peer tick) will
    // attempt redelivery.
    _queueOutboundMessage(
      recipientPubkey: pending.recipientPubkey,
      payload: pending.payload,
      messageId: pending.messageId,
    );
  }

  @visibleForTesting
  int queuedMessageCountForPeer(Uint8List pubkey) =>
      _outboundMessageQueue[_pubkeyToHex(pubkey)]?.length ?? 0;

  @visibleForTesting
  int get queuedMessageCount => _outboundMessageQueue.values.fold<int>(
        0,
        (count, queue) => count + queue.length,
      );

  @visibleForTesting
  int get ackPendingMessageCount => _ackPendingMessages.length;

  /// Feed a resolved LAN instance as the mDNS browser would, so the contact
  /// path can be exercised without a live responder on the machine.
  @visibleForTesting
  void debugResolveLanInstance(String token, String address) =>
      _onLanInstanceResolved(token, address);

  /// The agent's own candidates for the local end of a dial — its interface
  /// addresses at the bound UDP port, plus the reflected public address if one
  /// is known.
  ///
  /// A hermetic test reads the address its node listens on from here: there is
  /// no rendezvous server to reflect a public one, and `getPublicAddress()` is
  /// null until one does (spec §Connectivity and Address).
  @visibleForTesting
  Set<String> debugLocalCandidates() => _connectionLocalCandidates();

  /// The peer id bound to [address] on the UDP path, or null when none is —
  /// the address itself while the peer is unidentified, its pubkey hex after
  /// ANNOUNCE has claimed the path.
  @visibleForTesting
  String? debugUdpPeerIdForAddress(String address) =>
      _udpService?.peerIdForAddress(address);

  /// Send a read receipt to the original sender of a message.
  /// Call this when the user has read/viewed a message.
  /// Returns true if the read receipt was sent successfully.
  Future<bool> sendReadReceipt({
    required String messageId,
    required Uint8List senderPubkey,
  }) async {
    final peer = _peersState.getPeerByPubkey(senderPubkey);

    // Create read receipt packet at coordinator level. Signing happens after
    // session encryption, if available.
    final packet = _protocolHandler.createReadReceiptPacket(
      messageId: messageId,
    );

    // Try BLE first
    if (_bleAvailable && _bleService != null) {
      final bleDeviceId = _connectedBleDeviceIdForPeer(peer);
      if (peer != null && bleDeviceId != null) {
        final bytes = await _packetBytesForTransport(
          packet: packet,
          transport: PeerTransport.bleDirect,
          recipientPubkey: senderPubkey,
          peerId: bleDeviceId,
        );
        if (bytes != null &&
            await _bleService!.sendToPeer(bleDeviceId, bytes)) {
          return true;
        }
      }
    }

    // Fall back to UDP
    if (_udpAvailable && _udpService != null) {
      final udpCandidates = _udpCandidatesForPeer(peer);
      if (peer != null && udpCandidates.isNotEmpty) {
        if (await _sendPacketViaUdp(
          pubkeyHex: peer.pubkeyHex,
          udpAddress: peer.udpAddress ?? udpCandidates.first,
          packet: packet,
          recipientPubkey: senderPubkey,
        )) {
          return true;
        }
      }
    }

    debugPrint('No transport available to send read receipt');
    return false;
  }

  /// Broadcast a message to every peer over each underlay it is reachable
  /// over: as `send`, never into a session whose attestation exchange has not
  /// completed (GNA, 2026-10-11, Task A).
  Future<void> broadcast(Uint8List payload) async {
    // Broadcast as individually encrypted unicast packets, one Noise session
    // per peer per transport medium.
    if (_bleAvailable && _bleService != null) {
      try {
        for (final peerId in _bleService!.connectedPeerIds) {
          final pubkey = _bleService!.getPubkeyForPeerId(peerId);
          if (pubkey == null) continue;
          final peer = _peersState.getPeerByPubkey(pubkey);
          if (peer == null ||
              !peer.reachableUnderlays.contains(Underlay.pan)) {
            continue;
          }
          await _bleMessageSender.sendMessage(
            _pubkeyToHex(pubkey),
            _uuid.v4(),
            payload,
          );
        }
      } catch (e) {
        debugPrint('BLE broadcast failed: $e');
      }
    }

    if (_udpAvailable && _udpService != null) {
      try {
        for (final peer in _peersState.peersList) {
          if (_udpService!.getPeerIdForPubkey(peer.publicKey) == null ||
              !peer.reachableUnderlays.any((u) => u != Underlay.pan)) {
            continue;
          }
          await _udpMessageSender.sendMessage(
            peer.pubkeyHex,
            _uuid.v4(),
            payload,
          );
        }
      } catch (e) {
        debugPrint('UDP broadcast failed: $e');
      }
    }
  }

  // ===== Local Host Addresses =====

  /// Read the agent's own interface addresses — the host candidates a dial
  /// picks its local endpoint from.
  ///
  /// This is not the public address. The layer cannot observe its own public
  /// address (spec §Connectivity and Address); a rendezvous server observes it
  /// and reflects it back, which is the only thing that ever sets
  /// [_publicAddress].
  Future<void> _refreshLocalHostAddresses() async {
    final generation = _localHostAddressGeneration;
    final addresses = await _readLocalHostAddresses();
    if (_localHostAddressGeneration != generation) return;
    _localHostAddresses = addresses;
    debugPrint(
      '[local-host] Host candidates: '
      '${addresses.map((a) => a.address).join(', ')}',
    );
  }

  Future<void> _waitForLocalHostAddresses({
    Duration timeout = const Duration(seconds: 6),
  }) async {
    final inFlight = _localHostAddressFuture;
    if (inFlight == null) return;
    try {
      await inFlight.timeout(timeout);
    } catch (_) {}
  }

  AddressInfo? _parseSupportedUdpAddress(
    String udpAddress, {
    required String context,
    String? peerLabel,
  }) {
    final parsed = parseAddressString(udpAddress);
    if (parsed != null) return parsed;
    final label = peerLabel != null ? ' for $peerLabel' : '';
    debugPrint('[$context] Invalid UDP address$label: $udpAddress');
    return null;
  }

  String? _normalizeAnnouncedUdpAddress(
    String? udpAddress, {
    required String context,
  }) {
    if (udpAddress == null || udpAddress.isEmpty) return null;
    final parsed = _parseSupportedUdpAddress(udpAddress, context: context);
    return parsed?.toAddressString();
  }

  String? _connectedBleDeviceIdForPeer(PeerState? peer) {
    if (peer == null || _bleService == null || !_bleAvailable) {
      return null;
    }

    final centralId = peer.bleCentralDeviceId;
    if (centralId != null && _bleService!.isDeviceConnected(centralId)) {
      return centralId;
    }

    final peripheralId = peer.blePeripheralDeviceId;
    if (peripheralId != null && _bleService!.isDeviceConnected(peripheralId)) {
      return peripheralId;
    }

    return null;
  }

  static String _pubkeyToHex(Uint8List pubkey) =>
      pubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  /// Whether [pubkey] is in the known set — a key GLP supplied via the API
  /// (putKnownPeer / putPeerAddress). This is the Closed-mode recognition
  /// predicate (spec §Cold-Call Trust Levels: the layer recognizes only
  /// already-known peers, never a friendship record).
  bool _isKnownPeerPubkey(Uint8List pubkey) =>
      store.state.knownPeers.isKnown(_pubkeyToHex(pubkey));

  /// Fired when a Noise XX session for [transport] with [pubkey] completes
  /// authentication. Consolidated reachability — and therefore
  /// [onPeerConnected] — is gated on this: a transport counts toward
  /// `isReachable` only once its session is authenticated (spec
  /// `docs/GLP_Networking_API/sections/ip.tex` §IP Connection, "established and
  /// authenticated"). Also drains any queued messages now that an encrypted
  /// channel exists.
  void _onNoiseSessionEstablished(PeerTransport transport, Uint8List pubkey) {
    switch (transport) {
      case PeerTransport.udp:
        store.dispatch(
          PeerUdpConnectionChangedAction(
            pubkeyHex: _pubkeyToHex(pubkey),
            connected: true,
          ),
        );
        break;
      case PeerTransport.bleDirect:
        store.dispatch(PeerBleAuthenticatedAction(pubkey));
        break;
      case PeerTransport.webrtc:
        break;
    }
    // The handshake is followed by the mutual attestation exchange, and
    // neither peer is reported reachable until it succeeds (spec §Session
    // Establishment). Queued messages wait for it: a session that fails
    // attestation is torn down, and sending into it would be sending to a
    // peer that never becomes reachable.
    unawaited(_sendAttestation(transport, pubkey));
    _awaitPeerAttestation(transport, pubkey);
  }

  /// The session each peer's attestation was verified on, per medium, by its
  /// handshake hash: what decides whether a payload on a session is delivered
  /// (GNA, 2026-10-11, Task A).
  final Map<String, Uint8List> _attestedOn = {};

  /// The deadline by which each peer's attestation must arrive on its current
  /// session, per medium.
  final Map<String, ({Uint8List hash, Timer timer})> _attestationDeadlines =
      {};

  /// Wait for the peer's attestation on [pubkey]'s current [transport]
  /// session as long as the sender's attempts last, then tear the session
  /// down if it has not come (GNA, 2026-10-11, Attestation Q1). An
  /// attestation is never queued for the next session: the next session
  /// sends its own.
  void _awaitPeerAttestation(PeerTransport transport, Uint8List pubkey) {
    final sender = switch (transport) {
      PeerTransport.udp => _udpMessageSender,
      PeerTransport.bleDirect => _bleMessageSender,
      PeerTransport.webrtc => null,
    };
    if (sender == null) return;
    final hash = _noiseSessions.handshakeHashFor(transport, pubkey);
    if (hash == null) return;
    final key = _handshakePairKey(transport, pubkey);
    final standing = _attestationDeadlines[key];
    // A session may be reported established more than once: one deadline per
    // session.
    if (standing != null && listEquals(standing.hash, hash)) return;
    standing?.timer.cancel();
    late final ({Uint8List hash, Timer timer}) deadline;
    deadline = (
      hash: hash,
      timer: Timer(sender.attemptWindow, () {
        if (identical(_attestationDeadlines[key], deadline)) {
          _attestationDeadlines.remove(key);
        }
        if (!_isCurrentSession(transport, pubkey, hash)) return;
        final attestedOn = _attestedOn[key];
        if (attestedOn != null && listEquals(attestedOn, hash)) return;
        _tearDownForAttestation(
          transport,
          pubkey,
          "no attestation within the sender's attempts",
        );
      }),
    );
    _attestationDeadlines[key] = deadline;
  }

  // ===== Attestation (spec §Session Establishment) =====

  /// The session each peer's attestation was last sent on, per medium, by
  /// its handshake hash: the exchange is once per session (spec §Session
  /// Establishment), and a session may be reported established more than
  /// once.
  final Map<String, Uint8List> _attestationSentOn = {};

  /// Whether [pubkey]'s current session on [transport] is the one whose
  /// handshake hash is [handshakeHash].
  bool _isCurrentSession(
    PeerTransport transport,
    Uint8List pubkey,
    Uint8List handshakeHash,
  ) {
    final current = _noiseSessions.handshakeHashFor(transport, pubkey);
    return current != null && listEquals(current, handshakeHash);
  }

  /// Ask the platform to attest this agent over the session's digest, and
  /// send it. A platform with none sends the absence, which is distinct from
  /// a failure and is what the peer reports as unattested.
  ///
  /// It goes as a message of the Message Protocol after handshake message 3
  /// (spec §Session Establishment): fragmented, acknowledged and
  /// retransmitted by the medium's carrier as a `send` payload is (§Message
  /// Protocol), and outside the outbound queue, which waits on the
  /// reachability this exchange decides.
  ///
  /// The message is bound to its session: the signature names that session's
  /// handshake hash, so a fragment carried into a later session would be
  /// found invalid there and tear it down. Once its session is gone the
  /// message fails untransmitted, and the next session sends its own, nothing
  /// being cached. When its attempts are exhausted while its session stands,
  /// the session is torn down (§Message Protocol).
  Future<void> _sendAttestation(
    PeerTransport transport,
    Uint8List pubkey,
  ) async {
    final sender = switch (transport) {
      PeerTransport.udp => _udpMessageSender,
      PeerTransport.bleDirect => _bleMessageSender,
      PeerTransport.webrtc => null,
    };
    if (sender == null) return;
    final handshakeHash = _noiseSessions.handshakeHashFor(transport, pubkey);
    if (handshakeHash == null) {
      debugPrint('[attest] No handshake hash for '
          '${_pubkeyToHex(pubkey).substring(0, 8)}; cannot attest');
      return;
    }
    final sentKey = _handshakePairKey(transport, pubkey);
    final sentOn = _attestationSentOn[sentKey];
    if (sentOn != null && listEquals(sentOn, handshakeHash)) return;
    _attestationSentOn[sentKey] = handshakeHash;
    final digest = attestationDigest(
      identityPublicKey: identity.publicKey,
      handshakeHash: handshakeHash,
    );

    // Both halves or neither. The attestation is long-lived — the binding
    // obtains it once per install — while the signature is computed afresh
    // over this session's digest.
    AttestationEvidence? evidence;
    try {
      final attestation = await _attestation.attestationFor(identity.publicKey);
      final signature = attestation == null
          ? null
          : await _attestation.signSessionDigest(
              identityPublicKey: identity.publicKey,
              digest: digest,
            );
      if (attestation != null && signature != null) {
        evidence = AttestationEvidence(
          attestation: attestation,
          signature: signature,
        );
      }
    } catch (e) {
      // A platform that errs offers nothing; the peer reports us unattested
      // rather than refusing us.
      debugPrint('[attest] Platform attestation failed: $e');
      evidence = null;
    }

    // Over the medium the session belongs to, and no other: the digest binds
    // to that session's handshake hash, so the attestation means nothing
    // anywhere else. Sessions are per medium (spec §Session Establishment).
    final delivered = await sender.sendMessage(
      _pubkeyToHex(pubkey),
      _uuid.v4(),
      encodeAttestationPayload(evidence),
      fragmentType: PacketType.attestationFragment,
      isCurrent: () => _isCurrentSession(transport, pubkey, handshakeHash),
    );
    if (delivered || !_isCurrentSession(transport, pubkey, handshakeHash)) {
      return;
    }
    _tearDownForAttestation(
      transport,
      pubkey,
      'our attestation went unacknowledged',
    );
  }

  /// A peer's attestation arrived on the session whose handshake hash is
  /// [handshakeHash], the session that decrypted it (GNA, 2026-10-11, Task
  /// A). Verify it against the digest that session binds to; an attestation
  /// that fails tears that session down. A verdict on a session no longer
  /// current concerns nothing that stands, and is dropped.
  Future<void> _onAttestationReceived(
    Uint8List senderPubkey,
    Uint8List payload,
    PeerTransport transport,
    Uint8List handshakeHash,
  ) async {
    final peerShort = _pubkeyToHex(senderPubkey).substring(0, 8);
    if (!_isCurrentSession(transport, senderPubkey, handshakeHash)) {
      debugPrint('[attest] $peerShort attestation arrived on a session no '
          'longer current; dropped');
      return;
    }
    // Delivery is at least once, so a re-sent attestation can arrive again;
    // on a session already attested it is ignored (GNA, 2026-10-11, Task B).
    final attestedOn = _attestedOn[_handshakePairKey(transport, senderPubkey)];
    if (attestedOn != null && listEquals(attestedOn, handshakeHash)) {
      debugPrint('[attest] $peerShort attestation again on a session already '
          'attested; ignored');
      return;
    }
    final underlay = _underlayOf(transport, senderPubkey);
    if (underlay == null) {
      // An IP session whose peer's address falls under no underlay this layer
      // runs is no session (GNA, 2026-10-11, Q1).
      _tearDownForAttestation(
        transport,
        senderPubkey,
        'the session falls under no underlay this layer runs',
      );
      return;
    }

    final digest = attestationDigest(
      identityPublicKey: senderPubkey,
      handshakeHash: handshakeHash,
    );

    AttestationEvidence? offered;
    try {
      offered = decodeAttestationPayload(payload);
    } on FormatException catch (e) {
      debugPrint('[attest] Malformed attestation from $peerShort: $e');
      _tearDownForAttestation(transport, senderPubkey, 'malformed payload');
      return;
    }

    AttestationVerdict verdict;
    try {
      verdict = await _attestation.verify(
        evidence: offered,
        digest: digest,
        peerIdentityKey: senderPubkey,
      );
    } catch (e) {
      verdict = InvalidAttestation('verification threw: $e');
    }
    // Verification awaits; the session it judged may have gone meanwhile.
    if (!_isCurrentSession(transport, senderPubkey, handshakeHash)) return;

    final attestedKey = _handshakePairKey(transport, senderPubkey);
    switch (verdict) {
      case AttestedApplication(:final identity):
        debugPrint('[attest] $peerShort attested as $identity');
        _attestedOn[attestedKey] = handshakeHash;
        store.dispatch(PeerAttestedAction(
          publicKey: senderPubkey,
          underlay: underlay,
          attestation: PeerAttestation(identity: identity),
        ));
        _drainQueuedMessagesForPeer(senderPubkey);
      case UnattestedPlatform(:final reason):
        debugPrint('[attest] $peerShort unattested: $reason');
        _attestedOn[attestedKey] = handshakeHash;
        store.dispatch(PeerAttestedAction(
          publicKey: senderPubkey,
          underlay: underlay,
          attestation: PeerAttestation.unattested,
        ));
        _drainQueuedMessagesForPeer(senderPubkey);
      case InvalidAttestation(:final reason):
        debugPrint('[attest] $peerShort failed verification: $reason');
        _tearDownForAttestation(transport, senderPubkey, reason);
    }
  }

  /// The exchange failed on this session — the peer's attestation failed
  /// verification, or ours could not be delivered: the session is torn down,
  /// and onPeerConnected does not fire where reachability was never reported.
  void _tearDownForAttestation(
    PeerTransport transport,
    Uint8List pubkey,
    String reason,
  ) {
    debugPrint('[attest] Tearing down the ${transport.name} session with '
        '${_pubkeyToHex(pubkey).substring(0, 8)}: $reason');
    _attestedOn.remove(_handshakePairKey(transport, pubkey));
    _noiseSessions.reset(transport, pubkey);
    switch (transport) {
      case PeerTransport.udp:
        store.dispatch(PeerUdpConnectionChangedAction(
          pubkeyHex: _pubkeyToHex(pubkey),
          connected: false,
        ));
        unawaited(_udpService?.disconnectFromPeer(_pubkeyToHex(pubkey)));
      case PeerTransport.bleDirect:
        store.dispatch(PeerBleDisconnectedAction(pubkey));
      case PeerTransport.webrtc:
        break;
    }
  }

  Future<void> _startNoiseHandshakeForPeer({
    required PeerTransport transport,
    required PeerState peer,
    String? peerId,
  }) async {
    if (transport == PeerTransport.bleDirect) {
      peerId ??= _connectedBleDeviceIdForPeer(peer);
      if (peerId == null) return;
    } else if (transport == PeerTransport.udp) {
      if (_udpService?.getPeerIdForPubkey(peer.publicKey) == null) return;
      peerId ??= peer.pubkeyHex;
    } else {
      return;
    }

    await _ensureNoiseSession(
      transport: transport,
      recipientPubkey: peer.publicKey,
      peerId: peerId,
    );
  }

  Future<bool> _ensureNoiseSession({
    required PeerTransport transport,
    required Uint8List recipientPubkey,
    String? peerId,
  }) async {
    if (_noiseSessions.hasSession(transport, recipientPubkey)) return true;

    // Initiation is serialized with inbound handshake processing for the
    // same pair; the wait happens outside the pair's chain.
    final sent = await _serializedHandshake(
        _handshakePairKey(transport, recipientPubkey), () async {
      final payload = await _noiseSessions.startHandshake(
        transport,
        recipientPubkey,
      );
      if (payload == null) return true;
      final sent = await _sendNoiseHandshakePacket(
        transport: transport,
        recipientPubkey: recipientPubkey,
        peerId: peerId,
        payload: payload,
      );
      if (!sent) {
        _noiseSessions.reset(transport, recipientPubkey);
        return false;
      }
      if (transport == PeerTransport.udp) {
        _scheduleHandshakeRetransmit(recipientPubkey, peerId: peerId);
      }
      return true;
    });
    if (!sent) return false;

    return _noiseSessions.waitForSession(transport, recipientPubkey);
  }

  Future<bool> _sendNoiseHandshakePacket({
    required PeerTransport transport,
    required Uint8List recipientPubkey,
    required Uint8List payload,
    String? peerId,
  }) async {
    final packet = GrassrootsPacket(
      type: PacketType.noiseHandshake,
      payload: payload,
    );
    final bytes = packet.serialize();

    if (transport == PeerTransport.bleDirect) {
      final id = peerId ?? _bleService?.getPeerIdForPubkey(recipientPubkey);
      if (id == null) return false;
      final service = _bleService;
      if (service == null) return false;
      return service.sendToPeer(id, bytes);
    }

    if (transport == PeerTransport.udp) {
      final id = peerId ?? _pubkeyToHex(recipientPubkey);
      final service = _udpService;
      if (service == null) return false;
      return service.sendToPeer(id, bytes);
    }

    return false;
  }

  Future<Uint8List?> _packetBytesForTransport({
    required GrassrootsPacket packet,
    required PeerTransport transport,
    required Uint8List recipientPubkey,
    String? peerId,
  }) async {
    var outgoing = packet;
    if (packet.type.usesSessionSecurity) {
      // Where the session exists the packet is encrypted in the caller's
      // synchronous step, with no await before the session is read: the
      // message transport asks a bound message whether its session is still
      // current in that same step (the attestation exchange), and an await
      // between the two would let a session that replaced it carry the
      // fragment.
      if (!_noiseSessions.hasSession(transport, recipientPubkey) &&
          !await _ensureNoiseSession(
            transport: transport,
            recipientPubkey: recipientPubkey,
            peerId: peerId,
          )) {
        return null;
      }
      outgoing = await _noiseSessions.encryptPacket(
        packet,
        transport: transport,
        remotePubkey: recipientPubkey,
      );
    }
    return outgoing.serialize();
  }

  HolePunchService? _holePunchServiceFor(InternetAddress address) =>
      _holePunchServices[address.type];

  /// The agent's own candidates for the local end of a dial: its interface
  /// addresses at the bound UDP port of their family, plus the public address
  /// a rendezvous server reflected once one is known.
  ///
  /// Only families the UDP socket is actually bound to are offered, since the
  /// pair must be dialable from this socket.
  Set<String> _connectionLocalCandidates() {
    final udpService = _udpService;
    if (udpService == null) return const {};

    final candidates = <String>[];
    for (final address in _localHostAddresses) {
      if (!udpService.activeAddressTypes.contains(address.type)) continue;
      final localPort = udpService.localPortForAddressType(address.type);
      if (localPort == null) continue;
      candidates.add(AddressInfo(address, localPort).toAddressString());
    }

    final publicAddress = _publicAddress;
    if (publicAddress != null) {
      final parsed = parseAddressString(publicAddress);
      if (parsed != null &&
          udpService.activeAddressTypes.contains(parsed.ip.type)) {
        candidates.add(parsed.toAddressString());
      }
    }

    return normalizeAddressStrings(candidates);
  }

  /// The peer's dialable addresses: the single dial-book address (supplied
  /// via putPeerAddress or observed on a live session), plus an optional
  /// caller-provided fallback. ANNOUNCE carries no addresses (spec §ANNOUNCE
  /// and Liveness), so there is no advertised candidate set.
  Set<String> _udpCandidatesForPeer(
    PeerState? peer, {
    String? fallbackAddress,
  }) =>
      normalizeAddressStrings([
        peer?.udpAddress,
        fallbackAddress,
      ]);

  AddressCandidatePair? _selectUdpCandidatePair(
    Set<String> remoteCandidates, {
    required String context,
    String? peerLabel,
  }) {
    final localCandidates = _connectionLocalCandidates();
    final pair = _connectionService.selectBestPairFromAddresses(
      localAddresses: localCandidates,
      remoteAddresses: remoteCandidates,
    );
    if (pair == null) {
      final label = peerLabel != null ? ' for $peerLabel' : '';
      debugPrint(
        '[$context] No compatible UDP candidate pair$label: '
        'local=$localCandidates, '
        'remote=$remoteCandidates',
      );
      return null;
    }
    return pair;
  }

  // ===== UDP Connect-on-Demand =====

  /// Send data to a peer via UDP, connecting first if needed.
  ///
  /// UdpTransportService requires an active UDX connection before sending.
  /// This method handles the connect → ANNOUNCE → send flow transparently.
  Future<bool> _sendViaUdp(
    String pubkeyHex,
    String udpAddress,
    Uint8List data, {
    bool isRendezvous = false,
  }) async {
    if (_udpService == null) return false;
    final peerShort = pubkeyHex.substring(0, 8);

    // Already connected? Send directly.
    if (await _udpService!.sendToPeer(pubkeyHex, data)) {
      debugPrint('[udp-send] Sent to $peerShort via existing connection');
      return true;
    }

    final connected = await _ensureUdpConnection(
      pubkeyHex,
      udpAddress,
      isRendezvous: isRendezvous,
    );
    if (!connected) return false;

    debugPrint('[udp-send] Connected, sending data to $peerShort');
    return _udpService!.sendToPeer(pubkeyHex, data);
  }

  Future<bool> _sendPacketViaUdp({
    required String pubkeyHex,
    required String udpAddress,
    required GrassrootsPacket packet,
    required Uint8List recipientPubkey,
    bool isRendezvous = false,
  }) async {
    if (_udpService == null) return false;

    final connected =
        _udpService!.getPeerIdForPubkey(recipientPubkey) != null ||
            await _ensureUdpConnection(
              pubkeyHex,
              udpAddress,
              isRendezvous: isRendezvous,
            );
    if (!connected) return false;

    final bytes = await _packetBytesForTransport(
      packet: packet,
      transport: PeerTransport.udp,
      recipientPubkey: recipientPubkey,
      peerId: pubkeyHex,
    );
    if (bytes == null) return false;

    return _udpService!.sendToPeer(pubkeyHex, bytes);
  }

  Future<bool> _ensureUdpConnection(
    String pubkeyHex,
    String udpAddress, {
    bool isRendezvous = false,
  }) async {
    if (_udpService == null) return false;
    if (_udpService!.getPeerIdForPubkey(_hexToBytes(pubkeyHex)) != null) {
      return true;
    }
    final peerShort = pubkeyHex.substring(0, 8);
    final peer = store.state.peers.getPeerByPubkeyHex(pubkeyHex);

    final remoteCandidates = _udpCandidatesForPeer(
      peer,
      fallbackAddress: udpAddress,
    );
    // First dials can race the interface read: with no local candidates no
    // compatible pair exists and the dial fails spuriously — this is how a
    // rendezvous probe issued right after initialize()
    // used to fail. Wait for the in-flight read before giving up on candidate
    // selection.
    if (_connectionLocalCandidates().isEmpty) {
      debugPrint(
        '[udp-send] No local candidates yet for $peerShort — waiting for '
        'the local interface addresses',
      );
      await _waitForLocalHostAddresses(timeout: const Duration(seconds: 20));
    }
    final pair = _selectUdpCandidatePair(
      remoteCandidates,
      context: 'udp-send',
      peerLabel: peerShort,
    );
    if (pair == null) {
      return false;
    }
    final addr = pair.remote;
    final selectedAddress = addr.toAddressString();
    if (_ipUnderlayFor(addr.ip) == null) {
      // No session over an underlay this layer does not run (GNA, 2026-10-11,
      // Q1).
      debugPrint(
        '[udp-send] $selectedAddress for $peerShort falls under no underlay '
        'this layer runs ($underlays); not dialing',
      );
      return false;
    }

    // Datagrams need no stream (spec §IP Connection): associating the path
    // has no simultaneous-open hazard, so both sides may do it freely; the
    // deterministic-initiator rule applies to session initiation, where the
    // glare rule enforces it.

    final inFlight = _udpConnectInFlight[pubkeyHex];
    if (inFlight != null) {
      debugPrint('[udp-send] Reusing in-flight path setup to $peerShort...');
      return inFlight;
    }

    late final Future<bool> connectFuture;
    connectFuture = () async {
      // Hole-punch to open NAT mappings before the path is associated.
      // Skip for peers with publicly routable addresses — punching is wasted
      // when no NAT mapping is needed.
      if (!isRendezvous &&
          _holePunchServiceFor(addr.ip) != null &&
          peer != null &&
          !peer.hasPublicUdpAddress) {
        debugPrint(
          '[udp-send] Hole-punching to $selectedAddress before connecting...',
        );
        await _holePunchServiceFor(addr.ip)!.punch(addr.ip, addr.port);
      }

      debugPrint('[udp-send] Associating $peerShort at $selectedAddress...');
      return _udpService!.connectToPeer(pubkeyHex, addr.ip, addr.port);
    }();
    _udpConnectInFlight[pubkeyHex] = connectFuture;

    bool connected = false;
    try {
      connected = await connectFuture;
    } finally {
      if (identical(_udpConnectInFlight[pubkeyHex], connectFuture)) {
        _udpConnectInFlight.remove(pubkeyHex);
      }
    }

    if (connected) return true;

    debugPrint(
      '[udp-send] Path setup failed to $peerShort at $selectedAddress',
    );
    return false;
  }

  /// Proactively establish a UDP connection to a known peer at its dial-book
  /// address. Called by the reconnection sweeps (heartbeat tick, connectivity
  /// change, settings enable).
  ///
  /// Sends our own ANNOUNCE as the first message so the remote side learns
  /// our identity on the new UDP connection.
  Future<void> _connectToPeerViaUdp(
    String pubkeyHex,
    String udpAddress,
  ) async {
    final normalizedAddress =
        _normalizeAnnouncedUdpAddress(udpAddress, context: 'auto-udp') ??
            udpAddress;
    final peerShort = pubkeyHex.substring(0, 8);

    final retryAfter = _autoUdpRetryAfter[pubkeyHex];
    if (retryAfter != null &&
        _autoUdpLastAddress[pubkeyHex] == normalizedAddress &&
        DateTime.now().isBefore(retryAfter)) {
      debugPrint(
        '[auto-udp] Suppressing retry to $peerShort at '
        '$normalizedAddress until ${retryAfter.toIso8601String()}',
      );
      return;
    }

    final inFlight = _autoUdpConnectInFlight[pubkeyHex];
    if (inFlight != null) {
      debugPrint(
        '[auto-udp] Reusing in-flight proactive UDP attempt for '
        '$peerShort',
      );
      await inFlight;
      return;
    }

    _autoUdpLastAddress[pubkeyHex] = normalizedAddress;

    late final Future<void> task;
    task = () async {
      try {
        final announce = await _createSignedAnnounce();

        final success = await _sendViaUdp(
          pubkeyHex,
          normalizedAddress,
          announce,
        );
        if (success) {
          debugPrint(
            '[auto-udp] Proactive UDP connection to '
            '${pubkeyHex.substring(0, 8)} established',
          );
          _autoUdpRetryAfter.remove(pubkeyHex);
        } else {
          // Direct connection failed (likely NAT/firewall): the address
          // reaches the layer again from the social graph, by putPeerAddress
          // (spec §Connectivity and Address, "Address-change recovery").
          debugPrint(
            '[auto-udp] Proactive UDP connection to '
            '${pubkeyHex.substring(0, 8)} failed',
          );
          _autoUdpRetryAfter[pubkeyHex] = DateTime.now().add(
            _autoUdpRetryBackoff,
          );
        }
      } catch (e) {
        debugPrint(
          '[auto-udp] Error connecting to '
          '${pubkeyHex.substring(0, 8)}: $e',
        );
        _autoUdpRetryAfter[pubkeyHex] = DateTime.now().add(
          _autoUdpRetryBackoff,
        );
      }
    }();

    _autoUdpConnectInFlight[pubkeyHex] = task;
    try {
      await task;
    } finally {
      if (identical(_autoUdpConnectInFlight[pubkeyHex], task)) {
        _autoUdpConnectInFlight.remove(pubkeyHex);
      }
    }
  }

  /// Set up MessageRouter callbacks to dispatch to Redux and application layer
  void _setupRouterCallbacks() {
    // Message received from any transport
    _messageRouter.onMessageReceived =
        (messageId, senderPubkey, payload, arrivalTransport) {
      // The underlay of the session the message arrived on, from the receive
      // path (not inferred from the peer's preferred/active carrier).
      final underlay = _underlayOf(arrivalTransport, senderPubkey);
      if (underlay == null) return;

      store.dispatch(
        MessageReceivedAction(
          messageId: messageId,
          underlay: underlay,
          senderPubkey: senderPubkey,
          payloadSize: payload.length,
        ),
      );
      onMessageReceived?.call(senderPubkey, payload, messageId, underlay);
    };

    // ACK received (delivery confirmation)
    _messageRouter.onAckReceived = (messageId) {
      debugPrint('ACK received for message $messageId');
      store.dispatch(MessageDeliveredAction(messageId: messageId));
      _clearAckPending(messageId);
      _udpMessageSender.handleMessageAck(messageId);
      _bleMessageSender.handleMessageAck(messageId);
    };

    // Per-fragment ACK: settle the sender-side retransmit slot (spec
    // §Message Transport).
    _messageRouter.onFragmentAckReceived =
        (senderPubkey, messageId, index, transport) {
      final sender = transport == PeerTransport.udp
          ? _udpMessageSender
          : _bleMessageSender;
      sender.handleFragmentAck(_pubkeyToHex(senderPubkey), messageId, index);
    };

    // Inbound fragment: acknowledge it on the medium it arrived on.
    _messageRouter.onFragmentAckRequested =
        (transport, peerId, messageId, index) async {
      if (peerId == null) return;
      Uint8List? recipientPubkey;
      if (transport == PeerTransport.udp) {
        recipientPubkey = _hexToBytes(peerId);
      } else if (transport == PeerTransport.bleDirect) {
        recipientPubkey = _bleService?.getPubkeyForPeerId(peerId);
      }
      if (recipientPubkey == null) return;
      final bytes = await _packetBytesForTransport(
        packet: GrassrootsPacket(
          type: PacketType.fragmentAck,
          payload: FragmentHandler.encodeFragmentAck(
              messageId: messageId, index: index),
        ),
        transport: transport,
        recipientPubkey: recipientPubkey,
        peerId: peerId,
      );
      if (bytes == null) return;
      if (transport == PeerTransport.udp) {
        await _udpService?.sendToPeer(peerId, bytes);
      } else {
        await _bleService?.sendToPeer(peerId, bytes);
      }
    };

    // Read receipt received
    _messageRouter.onReadReceiptReceived = (messageId) {
      debugPrint('Read receipt received for message $messageId');
      store.dispatch(MessageReadAction(messageId: messageId));
    };

    // Map incoming UDP connections from any verified packet's senderPubkey.
    // Previously required ANNOUNCE as the first message on a stream; now any
    // verified packet identifies the sender via its header.
    _messageRouter.onUdpPeerIdentified = (senderPubkey, udpPeerId) {
      final pubkeyHex =
          senderPubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
      _udpService?.mapIncomingConnectionToPubkey(udpPeerId, pubkeyHex);
    };

    _messageRouter.onNoiseHandshakeReceived =
        (packet, transport, {String? peerId}) async {
      try {
        // Serialized with any in-flight announce-triggered initiation for
        // the same pair (spec §Session Establishment: one handshake per underlay per pair).
        final result = await _serializedHandshake(
          _peekHandshakePairKey(packet, transport, peerId),
          () => _noiseSessions.handleHandshakePacket(
            packet,
            transport: transport,
            peerId: peerId,
            // The manager transmits the response before releasing session
            // waiters, so a queued sender cannot race its first encrypted
            // packet ahead of handshake message 3.
            sendResponse: (payload, remotePubkey) async {
              // Trust gate (spec §Cold Calls, Trust levels): under Closed no
              // handshake response goes to an unanswerable key — the
              // handshake cannot complete.
              if (transport == PeerTransport.udp &&
                  !_acceptsUdpContactFrom(remotePubkey, udpPeerId: peerId)) {
                throw StateError(
                  'unsolicited handshake from unknown key '
                  '${_pubkeyToHex(remotePubkey).substring(0, 8)} under Closed '
                  'trust',
                );
              }
              await _sendNoiseHandshakePacket(
                transport: transport,
                recipientPubkey: remotePubkey,
                peerId: peerId,
                payload: payload,
              );
            },
          ),
        );
        if (transport == PeerTransport.udp &&
            !_acceptsUdpContactFrom(result.remotePubkey, udpPeerId: peerId)) {
          debugPrint(
            '[trust] Rejecting Noise handshake from unknown '
            '${_pubkeyToHex(result.remotePubkey).substring(0, 8)}',
          );
          _noiseSessions.reset(transport, result.remotePubkey);
          unawaited(
            _udpService?.disconnectFromPeer(_pubkeyToHex(result.remotePubkey)),
          );
          return;
        }
        if (result.sessionEstablished) {
          // The handshake authenticated the connection's identity — map any
          // still-anonymous inbound UDP connection to the peer's pubkey.
          if (transport == PeerTransport.udp && peerId != null) {
            _udpService?.mapIncomingConnectionToPubkey(
              peerId,
              _pubkeyToHex(result.remotePubkey),
            );
          }
          _onNoiseSessionEstablished(transport, result.remotePubkey);
        } else if (transport == PeerTransport.udp &&
            _noiseSessions.handshakeRetransmitPayload(
                    transport, result.remotePubkey) !=
                null) {
          // We just sent message 2 (or 1): retransmit until its reply.
          _scheduleHandshakeRetransmit(result.remotePubkey, peerId: peerId);
        }
      } catch (e) {
        debugPrint('[noise] Dropping handshake packet: $e');
      }
    };

    _messageRouter.onAttestationReceived =
        (senderPubkey, payload, transport, handshakeHash) {
      unawaited(_onAttestationReceived(
          senderPubkey, payload, transport, handshakeHash));
    };

    _messageRouter.isSessionAttested =
        (senderPubkey, transport, handshakeHash) {
      final attestedOn =
          _attestedOn[_handshakePairKey(transport, senderPubkey)];
      return attestedOn != null && listEquals(attestedOn, handshakeHash);
    };

    _messageRouter.decryptSessionPacket =
        (packet, transport, {String? peerId}) async {
      try {
        // Resolve the connection's identity when the transport already knows
        // it (BLE deviceId→pubkey from ANNOUNCE identification; UDP peerId is
        // the pubkey hex once the connection is mapped). The session manager
        // falls back to its own connection→identity map from the handshake.
        Uint8List? remotePubkey;
        if (peerId != null) {
          if (transport == PeerTransport.bleDirect) {
            remotePubkey = _bleService?.getPubkeyForPeerId(peerId);
          } else if (transport == PeerTransport.udp &&
              !peerId.contains(':') &&
              peerId.length == 64) {
            remotePubkey = _hexToBytes(peerId);
          }
        }
        return await _noiseSessions.decryptPacket(
          packet,
          transport: transport,
          peerId: peerId,
          remotePubkey: remotePubkey,
        );
      } catch (e) {
        debugPrint('[noise] Failed to decrypt session packet: $e');
        return null;
      }
    };

    // Peer ANNOUNCE processed
    _messageRouter.onPeerAnnounced =
        (data, transport, {bool isNew = false, String? udpPeerId}) {
      final pubkeyHex =
          data.publicKey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
      // debugPrint('Announce inc');
      // ANNOUNCE is identity beacon and heartbeat only (spec §ANNOUNCE and
      // Liveness): it carries no addresses, so nothing is registered from it.
      // Peer addresses reach the layer via putPeerAddress, fed by GLP;
      // reconnection toward known peers is driven by the heartbeat tick over
      // the dial book, not by announce contents.

      final announcedPeer = store.state.peers.getPeerByPubkeyHex(pubkeyHex);
      if (announcedPeer != null && !_isRendezvousPubkeyHex(pubkeyHex)) {
        unawaited(_startNoiseHandshakeForPeer(
          transport: transport,
          peer: announcedPeer,
          peerId: udpPeerId,
        ));
      }

      // onPeerDiscovered is the identity-learned event on a proximity
      // underlay: a nearby peer's public key arrives in the signed ANNOUNCE,
      // which is sent in the clear and precedes the Noise session. Because
      // onPeerConnected is gated on an authenticated Noise session (see
      // processReachabilityTransitions), discovery strictly precedes
      // connection — so we surface it here, once per identity, rather than
      // coupling it to the reachability transition. Discovery is over the
      // proximity underlays (spec §Concepts and Principles, Discovery): a BLE
      // ANNOUNCE is PAN discovery, a UDP ANNOUNCE from a source within one of
      // the agent's own local prefixes is LAN discovery, and WAN contact is
      // not discovery. The rendezvous server is infrastructure, not a
      // discoverable peer.
      final discoveredOn = switch (transport) {
        PeerTransport.bleDirect => ProximityUnderlay.pan,
        PeerTransport.udp => _ipUnderlayOfConnection(udpPeerId) == Underlay.lan
            ? ProximityUnderlay.lan
            : null,
        PeerTransport.webrtc => null,
      };
      if (discoveredOn != null &&
          !_isRendezvousPubkeyHex(pubkeyHex) &&
          _discoveredPubkeyHexes.add(pubkeyHex)) {
        onPeerDiscovered?.call(data.publicKey, discoveredOn);
      }

      // onPeerConnected is driven by the reachability subscriber (it fires once
      // the peer is reachable = an authenticated Noise session exists). We still
      // fire onPeerUpdated on subsequent ANNOUNCEs for callers that want every
      // update regardless of reachability transitions.
      final peerState = store.state.peers.getPeerByPubkey(data.publicKey);
      if (peerState != null && !isNew) {
        onPeerUpdated?.call(peerState);
      }

      _drainQueuedMessagesForPeer(data.publicKey);
    };

    // ACK request (router asks us to send ACK back to sender)
    _messageRouter.onAckRequested = (transport, peerId, messageId) async {
      if (peerId == null) {
        debugPrint('Cannot send ACK for $messageId: no peerId');
        return;
      }
      Uint8List? recipientPubkey;
      if (transport == PeerTransport.udp) {
        recipientPubkey = _hexToBytes(peerId);
      } else if (transport == PeerTransport.bleDirect) {
        recipientPubkey = _bleService?.getPubkeyForPeerId(peerId);
      }
      if (recipientPubkey == null) {
        debugPrint('Cannot send ACK for $messageId: unknown recipient pubkey');
        return;
      }

      final ackPacket = _protocolHandler.createAckPacket(
        messageId: messageId,
      );
      final bytes = await _packetBytesForTransport(
        packet: ackPacket,
        transport: transport,
        recipientPubkey: recipientPubkey,
        peerId: peerId,
      );
      if (bytes == null) return;

      if (transport == PeerTransport.udp) {
        await _udpService?.sendToPeer(peerId, bytes);
      } else if (transport == PeerTransport.bleDirect) {
        await _bleService?.sendToPeer(peerId, bytes);
      }
    };

    // Signaling packet received from a rendezvous server.
    _messageRouter.onSignalingReceived =
        (senderPubkey, payload, {observedIp, observedPort}) =>
            _onSignalingReceived(senderPubkey, payload);
  }

  /// A signaling payload arrived over a session.  Signaling is between an
  /// agent and its rendezvous servers alone, so it is taken from a configured
  /// rendezvous server and from no one else: the address the server observes
  /// this agent at (spec §Connectivity and Address).
  void _onSignalingReceived(Uint8List senderPubkey, Uint8List payload) {
    final senderHex = _pubkeyToHex(senderPubkey);
    if (!_isRendezvousPubkeyHex(senderHex)) {
      debugPrint(
        'Dropping signaling from untrusted sender '
        '${senderHex.substring(0, 8)}...',
      );
      return;
    }
    final SignalingMessage message;
    try {
      message = const SignalingCodec().decode(payload);
    } catch (e) {
      debugPrint('Failed to decode signaling message: $e');
      return;
    }
    switch (message) {
      case AddrReflectMessage(:final ip, :final port):
        _onAddrReflected(senderPubkey, ip, port);
    }
  }

  /// Address reflection: a rendezvous server told us the address it observes
  /// us at — the NAT-translated address, external port included (spec
  /// §Connectivity and Address).
  void _onAddrReflected(Uint8List senderPubkey, String ip, int port) {
    debugPrint(
      '[rendezvous] Server ${_pubkeyToHex(senderPubkey).substring(0, 8)}... '
      'reflected our address',
    );

    final reflectedIp = InternetAddress.tryParse(ip);
    if (reflectedIp == null) return;

    // Always update the display IP with the reflected address (IPv6 > IPv4).
    final currentIp = store.state.transports.publicIp;
    final isUpgrade =
        reflectedIp.type == InternetAddressType.IPv6 || currentIp == null;
    if (isUpgrade || ip == currentIp) {
      store.dispatch(PublicIpUpdatedAction(ip));
    }

    if (_udpService == null ||
        !_udpAvailable ||
        !_udpService!.canDialAddress(reflectedIp)) {
      debugPrint(
        'Reflected ${reflectedIp.type == InternetAddressType.IPv6 ? "IPv6" : "IPv4"} '
        'address $ip:$port — noted for display, but the current UDP '
        'socket cannot use that family.',
      );
      return;
    }

    final reflected = AddressInfo(reflectedIp, port).toAddressString();
    final previous = _publicAddress;
    if (reflected == previous) return; // No change

    debugPrint(
      'Public address updated via reflection: $previous → $reflected',
    );
    _publicAddress = reflected;
    store.dispatch(PublicAddressUpdatedAction(reflected));
    _resetRendezvousBackoff();
    _resetAutoUdpBackoff();
    unawaited(_syncConfiguredRendezvous(reason: 'reflected-address-updated'));
    // Spec onConnectivityStatus: RV-reflection-driven address change.
    onConnectivityStatusChanged?.call(previous, reflected);
  }

  /// Diff each peer's set of reachable transports against the previous store
  /// tick and fire the per-transport `onPeerConnected` / `onPeerDisconnected`
  /// callbacks for every medium that came up or went down. Delegates to the
  /// testable top-level [processReachabilityTransitions] function so the diff
  /// logic is unit-testable without a full `GrassrootsNetwork` harness.
  void _processReachabilityTransitions(PeersState peersState) {
    processReachabilityTransitions(
      peersState: peersState,
      lastKnownUnderlays: _lastKnownUnderlays,
      onConnected: onPeerConnected,
      onDisconnected: onPeerDisconnected,
    );
  }

  /// Set up callbacks for BLE transport service
  void _setupBleServiceCallbacks() {
    if (_bleService == null) return;

    // Forward BLE packets to the MessageRouter, preserving each device
    // connection's arrival order through async processing (spec §Session
    // Establishment).
    _bleService!.onBlePacketReceived =
        (packet, {String? bleDeviceId, int? rssi, BleRole? bleRole}) {
      _enqueueRx('ble:${bleDeviceId ?? "?"}', () async {
        await _messageRouter.processPacket(
          packet,
          transport: PeerTransport.bleDirect,
          bleDeviceId: bleDeviceId,
          bleRole: bleRole,
          rssi: rssi,
        );
      });
    };

    _messageRouter.shouldAcceptBleAnnounce =
        (senderPubkey, {String? bleDeviceId, BleRole? bleRole}) {
      // The BLE level alone governs a BLE encounter (spec §Trust levels).
      if (store.state.settings.isOpenOn(ProximityUnderlay.pan)) {
        return true;
      }
      // Closed: recognize only already-known peers — keys GLP supplied via
      // the API, never a friendship record (spec §Trust gating).
      return _isKnownPeerPubkey(senderPubkey);
    };

    _messageRouter.onBleAnnounceRejected = (senderPubkey, bleDeviceId) {
      if (bleDeviceId == null) return;
      unawaited(_bleService?.disconnectDevice(bleDeviceId));
    };

    // BLE-level disconnect cleans up the per-transport Noise session.
    // The application-level onPeerDisconnected for the BLE medium is fired by
    // the reachability subscriber when this peer's BLE session drops.
    _bleService!.onPeerDisconnected = (peer) {
      debugPrint('BLE Peer disconnected: ${peer.displayName}');
      _noiseSessions.reset(PeerTransport.bleDirect, peer.publicKey);
    };

    // Per spec (`docs/GLP_Networking_API/sections/ble.tex` §BLE Discovery):
    // "Upon successful BLE connection, an ANNOUNCE packet is exchanged."
    // Fire an immediate directed ANNOUNCE to the newly connected path so the
    // peer learns our identity within milliseconds rather than waiting up to
    // a full periodic-broadcast cycle. The periodic broadcast still runs as
    // a keep-alive and as a refresh after BLE-address rotation; receivers
    // treat repeated ANNOUNCEs from the same pubkey as idempotent.
    _bleService!.connectionStream.listen((event) {
      if (event.connected) {
        unawaited(_sendAnnounceToDevice(event.peerId));
      } else {
        debugPrint('BLE device disconnected: ${event.peerId}');
        _requeueMessagesOnBleDisconnect(event.peerId);
      }
    });
  }

  /// Set up callbacks for UDP transport service
  void _setupUdpServiceCallbacks() {
    if (_udpService == null) return;

    // Forward UDP data to the MessageRouter, preserving each connection's
    // arrival order through async processing (spec §Session Establishment).
    _udpService!.onUdpDataReceived = (peerId, data) {
      _enqueueRx('udp:$peerId', () async {
        try {
          final packet = GrassrootsPacket.deserialize(data);
          // Observed source address: the UDX remote, if known. Used by the
          // signaling matcher to learn cold-call senders' public addresses.
          final remote = _udpService?.getRemoteAddress(peerId);
          await _messageRouter.processPacket(
            packet,
            transport: PeerTransport.udp,
            udpPeerId: peerId,
            observedIp: remote?.ip.address,
            observedPort: remote?.port,
          );
        } catch (e) {
          debugPrint('Failed to process UDP packet from $peerId: $e');
        }
      });
    };

    // Trust gate for unsolicited IP contact (spec §Cold Calls, Trust
    // levels): an unknown key's ANNOUNCE is dropped unless it is LAN contact
    // under Open. Known peers, solicited contact (a connection this agent
    // dialed) and configured rendezvous servers pass.
    _messageRouter.shouldAcceptUdpAnnounce = (senderPubkey, {udpPeerId}) =>
        _acceptsUdpContactFrom(senderPubkey, udpPeerId: udpPeerId);

    _messageRouter.onUdpAnnounceRejected = (senderPubkey, udpPeerId) {
      unawaited(
        _udpService?.disconnectFromPeer(_pubkeyToHex(senderPubkey)),
      );
    };

    // Listen to connection events — update Redux state and log
    _udpService!.connectionStream.listen((event) {
      // A temp path — its id is the address, not a public key — belongs to a
      // peer the layer has not identified yet: an unsolicited inbound datagram,
      // or an outbound LAN cold call. There is no identity to record against,
      // and the Redux state is a projection of what the transports report, not
      // an inference from it, so nothing identity-keyed happens here. The
      // ANNOUNCE that names the peer re-keys the path and fires a second
      // connection event under the real key, and that one does this work.
      if (event.peerId.contains(':')) {
        debugPrint(
          'UDP path ${event.connected ? "opened" : "closed"} for the '
          'unidentified ${event.peerId}',
        );
        return;
      }
      if (event.connected) {
        debugPrint('UDP peer connected: ${event.peerId}');
        store.dispatch(PeerUdpSeenAction(_hexToBytes(event.peerId)));
        {
          // Incoming: a peer reached us at our public address without us
          // first opening a NAT mapping for them. Record that our current
          // address accepted unsolicited inbound.
          //
          // Outgoing: we reached the peer at their advertised address
          // without any punch coordination. Record that their address
          // accepted unsolicited inbound.
          final peer = _peersState.getPeerByPubkeyHex(event.peerId);
          final observedRemote = _udpService!.getRemoteAddress(event.peerId);
          final peerCandidates = _udpCandidatesForPeer(peer);
          final matchedAdvertisedAddress = observedRemote != null &&
              peerCandidates.any((candidate) {
                final parsed = parseAddressString(candidate);
                return parsed != null &&
                    observedRemote.ip.address == parsed.ip.address &&
                    observedRemote.port == parsed.port;
              });

          if (event.isIncoming) {
            // Only record inbound reachability if the peer reached us on the same
            // address they advertise publicly. This avoids treating LAN or
            // link-local paths as unsolicited public reachability.
            if (peer?.hasPublicUdpAddress == true && matchedAdvertisedAddress) {
              store.dispatch(UnsolicitedInboundObservedAction());
            } else {
              debugPrint(
                'Ignoring inbound reachability observation for ${event.peerId}: '
                'peer advertised=${peer?.udpAddress}, observed=$observedRemote',
              );
            }
          } else if (peer != null) {
            // Bind peer reachability observation to the exact advertised address
            // that succeeded, not just any direct path.
            if (peer.hasPublicUdpAddress && matchedAdvertisedAddress) {
              final remote = _udpService!.getRemoteAddress(event.peerId);
              final reachedAdvertisedAddress = remote != null &&
                  peerCandidates.any((candidate) {
                    final advertised = parseAddressString(candidate);
                    return advertised != null &&
                        remote.port == advertised.port &&
                        remote.ip.address == advertised.ip.address;
                  });
              if (reachedAdvertisedAddress) {
                store.dispatch(PeerDirectReachObservedAction(peer.publicKey));
              } else {
                debugPrint(
                  'Skipping direct-reach observation for ${event.peerId}: connected to '
                  '${remote?.ip.address ?? "unknown"}:${remote?.port ?? 0} '
                  'but advertised ${peer.udpAddress ?? "none"}',
                );
              }
            } else {
              debugPrint(
                'Ignoring peer direct-reach observation for ${event.peerId}: '
                'peer advertised=${peer.udpAddress}, observed=$observedRemote',
              );
            }
          }
        }
      } else {
        debugPrint('UDP peer disconnected: ${event.peerId}');
        final disconnectedPeer = _peersState.getPeerByPubkeyHex(event.peerId);
        if (disconnectedPeer != null) {
          _onUdpPeerDisconnected(disconnectedPeer);
        }
      }
      if (!event.connected) {
        // Disconnect is immediate — drop reachability now.
        store.dispatch(
          PeerUdpConnectionChangedAction(
            pubkeyHex: event.peerId,
            connected: false,
          ),
        );
      } else if (_isRendezvousPubkeyHex(event.peerId)) {
        // The rendezvous server runs no Noise handshake, so its raw UDX
        // connection is its reachability (gate exemption). Real peers become
        // reachable only once their Noise session authenticates — see
        // [_onNoiseSessionEstablished].
        store.dispatch(
          PeerUdpConnectionChangedAction(
            pubkeyHex: event.peerId,
            connected: true,
          ),
        );
      }
      if (event.connected) {
        _drainQueuedMessagesForPeer(_hexToBytes(event.peerId));
      }
    });
  }

  /// Fired when a UDP path ended or the ANNOUNCE keepalive timed out: the
  /// session goes with it.  Overall reachability is tracked by the
  /// reachability subscriber on the store, which fires the consolidated
  /// onPeerDisconnected when the last live transport drops; recovery is the
  /// heartbeat sweep's and the social graph's (spec §Connectivity and
  /// Address, "Address-change recovery").
  void _onUdpPeerDisconnected(PeerState peer) {
    _noiseSessions.reset(PeerTransport.udp, peer.publicKey);
  }

  /// Called by the host app when it returns to the foreground.
  ///
  /// Two steps, in order:
  ///   1. Probe the raw UDP sockets and rebind the transport if the OS
  ///      poisoned them while we were backgrounded (Android in particular
  ///      EPERMs background sends and leaves the socket FD in a permanently
  ///      broken state — fixable only by close + bind).
  ///   2. Re-sync configured rendezvous servers (existing UDX sessions may
  ///      be talking to a stale CID, or the rebind in step 1 just dropped
  ///      every session).
  Future<void> onAppResumed() async {
    debugPrint('[lifecycle] App resumed — probing sockets, re-syncing RVs');
    if (_udpService != null && _udpAvailable) {
      final rebound = await _udpService!.probeAndRebindIfDead();
      if (rebound) {
        debugPrint('[lifecycle] UDP transport rebound after foreground probe');
      }
    }
    unawaited(_syncConfiguredRendezvous(reason: 'app-resumed'));
    _drainQueuedMessagesForLivePeers();
  }

  /// Clean up resources
  Future<void> dispose() async {
    // Release every place declaration: platforms bound how many geofences an
    // application may register, so none may outlive the layer.
    await _places?.dispose();
    _connectivitySubscription?.cancel();
    _connectivitySubscription = null;
    _storeSubscription?.cancel();
    _storeSubscription = null;
    _announceTimer?.cancel();
    _scanTimer?.cancel();

    // Wait for any in-flight transport update to finish before disposing
    if (_transportUpdateLock != null) {
      await _transportUpdateLock;
      _transportUpdateLock = null;
    }

    await stop();

    _outboundMessageQueue.clear();
    _outboundQueueDrainsInProgress.clear();

    for (final pending in _ackPendingMessages.values) {
      pending.ackTimer?.cancel();
    }
    _ackPendingMessages.clear();
    for (final deadline in _attestationDeadlines.values) {
      deadline.timer.cancel();
    }
    _attestationDeadlines.clear();

    _noiseSessions.dispose();

    if (_bleService != null) {
      await _bleService!.dispose();
    }

    for (final service in _holePunchServices.values) {
      service.dispose();
    }
    _holePunchServices.clear();

    if (_udpService != null) {
      await _udpService!.dispose();
    }
  }

  /// Re-dial each rendezvous server with no connection standing (spec
  /// §Connectivity and Address: "The heartbeat sweep therefore re-initiates
  /// connections to the known peers and the configured rendezvous servers").
  /// The per-server backoff in [_syncConfiguredRendezvous] spaces the
  /// attempts.
  void _holdRendezvousSessions() {
    final udp = _udpService;
    if (udp == null || !_udpAvailable) return;
    final down = [
      for (final server in rendezvousServers)
        if (udp.getPeerIdForPubkey(server.publicKey) == null) server,
    ];
    if (down.isEmpty) return;
    unawaited(_syncConfiguredRendezvous(configs: down, reason: 'heartbeat'));
  }

  /// Start the periodic ANNOUNCE timer
  void _startAnnounceTimer() {
    _announceTimer?.cancel();
    _announceTimer = Timer.periodic(config.announceInterval, (_) {
      _broadcastAnnounce();
      _broadcastAnnounceViaUdp();
      _holdRendezvousSessions();
      _removeStalePeers();
      _drainQueuedMessagesForLivePeers();
      unawaited(_refreshLocalNetwork());
      // The same tick catches an interface address the platform reported
      // through no event at all; a dial needs one for its local end.
      unawaited(_refreshLocalHostAddresses());
    });
  }

  /// Start the periodic scan timer
  void _startScanTimer() {
    _scanTimer?.cancel();
    if (config.scanDuration == null) {
      // BLE start() already requested a continuous scan. Do not replace it
      // with the finite default scan window from BleTransportService.scan().
      return;
    }
    _scanTimer = Timer.periodic(config.scanInterval, (_) {
      // debugPrint('Scan timer is up! Scanning for new devices 📡');
      _periodicScan();
    });
  }

  /// Perform a periodic scan for new BLE devices
  Future<void> _periodicScan() async {
    if (!_bleAvailable) return;
    if (store.state.settings.bleRoleMode == BleRoleMode.peripheralOnly) {
      return;
    }
    try {
      store.dispatch(BleScanningChangedAction(true));
      await _bleService!.scan(timeout: config.scanDuration);
    } catch (e) {
      debugPrint('Periodic scan failed: $e');
    } finally {
      store.dispatch(BleScanningChangedAction(false));
    }
  }

  /// Send ANNOUNCE to all connected BLE devices.
  ///
  /// Each connected device is targeted individually so friend/non-friend
  /// address inclusion is decided using the current mapping for that exact BLE
  /// device ID. This avoids the old exclude-list logic that broke when BLE IDs
  /// rotated or when a peer had separate central/peripheral connections.
  Future<void> _broadcastAnnounce() async {
    if (_bleService == null || !_bleAvailable) return;

    for (final bleId in _bleService!.connectedPeerIds) {
      await _sendAnnounceToDevice(bleId);
    }
  }

  /// Broadcast ANNOUNCE via UDP to all connected peers.
  Future<void> _broadcastAnnounceViaUdp() async {
    if (_udpService == null || !_udpAvailable) return;

    final announce = await _createSignedAnnounce();
    await _udpService!.broadcast(announce);
  }

  /// Send ANNOUNCE directly to a specific BLE device ID.
  ///
  /// Called from the periodic [_broadcastAnnounce] loop, and fired
  /// immediately when a BLE path becomes connected (per spec §BLE Discovery:
  /// ANNOUNCE is exchanged upon successful BLE connection). Receivers treat
  /// repeated ANNOUNCEs from the same pubkey as idempotent, so racing with
  /// the periodic broadcast is harmless.
  ///
  /// Trust gating (spec §Cold-Call Trust Levels): under Closed trust, no
  /// ANNOUNCE is sent to an unknown device — only to devices mapped to a
  /// known key, or matching a known peer's derived per-slot UUID.
  Future<bool> _sendAnnounceToDevice(String deviceId) async {
    if (_bleService == null || !_bleAvailable) return false;

    // Check if this device ID already belongs to an authenticated known peer.
    final pubkey = _bleService!.getPubkeyForPeerId(deviceId);
    final isKnown = pubkey != null && _isKnownPeerPubkey(pubkey);

    final knownHint = _bleService!.getKnownPeerPubkeyHintForPeerId(deviceId);
    // A BLE device is a BLE encounter, so the BLE level governs it.
    final allowsColdCall = store.state.settings.isOpenOn(ProximityUnderlay.pan);
    if (!isKnown && !allowsColdCall && knownHint == null) {
      debugPrint(
        '[ble-announce] Suppressed ANNOUNCE to $deviceId '
        '(closed BLE trust, unknown peer)',
      );
      return false;
    }

    final announce = await _createSignedAnnounce();
    final sent = await _bleService!.sendToPeer(deviceId, announce);
    if (!sent) {
      debugPrint('[ble-announce] Failed to send ANNOUNCE to $deviceId');
    }
    return sent;
  }

  /// Create a signed ANNOUNCE packet — the irreducible identity record
  /// (spec §ANNOUNCE and Liveness): key + version + trailing signature.
  Future<Uint8List> _createSignedAnnounce() async {
    final payload = _protocolHandler.createAnnouncePayload();
    // The ANNOUNCE payload is a self-signed identity record; the frame needs
    // no further authentication.
    return GrassrootsPacket(
      type: PacketType.announce,
      payload: payload,
    ).serialize();
  }

  /// Remove peers that haven't sent an ANNOUNCE within the interval.
  ///
  /// BLE/general staleness uses [PeerState.lastSeen]. UDP liveness is tracked
  /// independently via [PeerState.lastUdpSeen] so a nearby BLE friend can age
  /// out of "Friends Online" without disappearing from "Nearby".
  void _removeStalePeers() {
    final staleThreshold = config.announceInterval * 2; // Give 2x grace period

    // Tear down quiet UDP sessions that have missed 2 announce cycles.
    final connectedUdpPubkeys = <String>{};
    if (_udpService != null) {
      for (final peer in _peersState.peersList) {
        if (_udpService!.getPeerIdForPubkey(peer.publicKey) != null) {
          connectedUdpPubkeys.add(peer.pubkeyHex);
        }
      }
    }

    final staleUdpPeers = computeStaleUdpPeerPubkeys(
      peers: _peersState.peersList,
      connectedUdpPubkeys: connectedUdpPubkeys,
      staleThreshold: staleThreshold,
    );
    if (_udpService != null) {
      for (final pubkeyHex in staleUdpPeers) {
        final peer = _peersState.getPeerByPubkeyHex(pubkeyHex);
        if (peer == null) continue;

        debugPrint(
          '[udp-stale] No UDP traffic from ${peer.displayName} for '
          '${staleThreshold.inSeconds}s; disconnecting stale session',
        );
        store.dispatch(PeerUdpDisconnectedAction(peer.publicKey));
        unawaited(_udpService!.disconnectFromPeer(pubkeyHex));
      }
    }

    // Sweep stale BLE attachments. The BLE plugin can fail to surface a
    // disconnect when the path drifts through `failed`/`subscribed` instead
    // of cleanly dropping from `ready`. Without this safety net the peer's
    // `bleCentralDeviceId` / `blePeripheralDeviceId` stay set indefinitely
    // and `nearbyBlePeers` keeps showing them. Friends and strangers are
    // treated identically — both should fall off "Connected Peers" once
    // they've been silent over BLE for two announce cycles.
    final staleBlePeers = computeStaleBlePeerPubkeys(
      peers: _peersState.peersList,
      staleThreshold: staleThreshold,
    );
    for (final pubkeyHex in staleBlePeers) {
      final peer = _peersState.getPeerByPubkeyHex(pubkeyHex);
      if (peer == null) continue;
      debugPrint(
        '[ble-stale] No BLE traffic from ${peer.displayName} for '
        '${staleThreshold.inSeconds}s; synthesizing disconnect',
      );
      // Redux is a strict projection of transport facts — so when we
      // synthesize a disconnect, make it a fact: physically tear down any
      // plugin paths still attached to this peer. An ANNOUNCE-quiet link is
      // often still alive at the radio level; clearing only the Redux
      // attachment blinds the identity-keyed dial guards, and the recovery
      // dial then opens a doomed SECOND link to a device we are still
      // linked with (20s connecting-wedge on iOS, GATT-133 churn on
      // Android). Tearing the radio down first makes the recovery dial a
      // legitimate first link.
      final centralId = peer.bleCentralDeviceId;
      if (centralId != null) {
        unawaited(_bleService?.disconnectDevice(centralId, forget: true));
      }
      final peripheralId = peer.blePeripheralDeviceId;
      if (peripheralId != null) {
        unawaited(_bleService?.disconnectDevice(peripheralId, forget: true));
      }
      // role: null clears both central and peripheral attachments.
      store.dispatch(PeerBleDisconnectedAction(peer.publicKey));
    }

    // Dispatch action to remove stale peers via Redux
    store.dispatch(StaleDiscoveredBlePeersRemovedAction(staleThreshold));
    store.dispatch(StalePeersRemovedAction(staleThreshold));
  }

}

/// Diff each peer's set of reachable underlays against the previous tick and
/// fire the per-underlay reachability callbacks for every underlay that came
/// up or went down. Pure-ish: the only side effects are calling [onConnected]
/// / [onDisconnected] and mutating [lastKnownUnderlays] in place.
///
/// Semantics (per underlay, independently — the PAN and IP sessions never
/// interfere):
///   - an underlay reachable now but not last tick: fire [onConnected] with
///     the peer's public key and that underlay. (Discovery — onPeerDiscovered
///     — is surfaced separately at ANNOUNCE receipt, because a peer's identity
///     is known before its Noise session authenticates.)
///   - an underlay reachable last tick but not now: fire [onDisconnected] with
///     the public key and that underlay. A peer that loses one of two live
///     underlays fires a disconnect for the lost one while staying reachable
///     over the other.
///   - a peer removed from [peersState.peersList] entirely (e.g.
///     PeerRemovedAction): fire [onDisconnected] for each underlay it still
///     held.
///   - no underlay changed: no fire.
@visibleForTesting
void processReachabilityTransitions({
  required PeersState peersState,
  required Map<String, Set<Underlay>> lastKnownUnderlays,
  required void Function(
    Uint8List pk,
    Underlay underlay,
    ApplicationIdentity? identity,
  )? onConnected,
  required void Function(Uint8List pk, Underlay underlay)? onDisconnected,
}) {
  final seenPubkeys = <String>{};

  for (final peer in peersState.peersList) {
    final pk = peer.pubkeyHex;
    seenPubkeys.add(pk);
    final previous = lastKnownUnderlays[pk] ?? const <Underlay>{};
    final current = peer.reachableUnderlays;
    if (setEquals(previous, current)) continue;

    for (final u in current.difference(previous)) {
      onConnected?.call(
        peer.publicKey,
        u,
        peer.attestationFor(u)?.identity,
      );
    }
    for (final u in previous.difference(current)) {
      onDisconnected?.call(peer.publicKey, u);
    }

    // Retain only non-empty sets so an unreachable peer holds no entry and the
    // removed-while-reachable sweep below stays simple.
    if (current.isEmpty) {
      lastKnownUnderlays.remove(pk);
    } else {
      lastKnownUnderlays[pk] = current;
    }
  }

  // Removed-while-reachable: the peer's entry vanished entirely, so surface a
  // disconnect on each underlay it still held.
  final missing = lastKnownUnderlays.keys
      .where((pk) => !seenPubkeys.contains(pk))
      .toList(growable: false);
  for (final pk in missing) {
    final underlays = lastKnownUnderlays.remove(pk)!;
    final cb = onDisconnected;
    if (cb == null) continue;
    final pubkey = Uint8List.fromList([
      for (var i = 0; i < pk.length; i += 2)
        int.parse(pk.substring(i, i + 2), radix: 16)
    ]);
    for (final u in underlays) {
      cb(pubkey, u);
    }
  }
}
