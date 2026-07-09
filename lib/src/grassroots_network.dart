import 'dart:async';
import 'dart:io';
import 'package:connectivity_plus/connectivity_plus.dart';
import 'package:flutter/foundation.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';
import 'package:uuid/uuid.dart';
import 'ble/permission_handler.dart';
import 'platform/transport_foreground_service.dart';
import 'signaling/peer_link.dart';
import 'signaling/signaling_codec.dart';
import 'signaling/signaling_service.dart';
import 'transport/address_utils.dart';
import 'transport/ble_transport_service.dart';
import 'transport/connection_service.dart';
import 'transport/hole_punch_service.dart';
import 'transport/public_address_discovery.dart';
import 'transport/udp_transport_service.dart';
import 'models/identity.dart';
import 'models/peer.dart';
import 'models/packet.dart';
import 'protocol/protocol_handler.dart';
import 'protocol/fragment_handler.dart';
import 'routing/message_router.dart';
import 'session/noise_session_manager.dart';
import 'store/store.dart';
import 'transport/transport_service.dart';

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

  /// Whether to enable BLE transport (can be overridden by TransportSettingsStore)
  final bool enableBle;

  /// Whether to enable UDP transport (can be overridden by TransportSettingsStore)
  final bool enableUdp;

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
    this.enableBle = true,
    this.enableUdp = true,
    this.ackTimeout = const Duration(seconds: 5),
  });
}

class _RendezvousConfig {
  final String address;
  final Uint8List pubkey;
  final String pubkeyHex;

  const _RendezvousConfig({
    required this.address,
    required this.pubkey,
    required this.pubkeyHex,
  });
}

/// Local record of an invite minted by [GrassrootsNetwork.generatePeerLink],
/// pending redemption ("records the invite locally as unused" — spec
/// §IP Cold-Call). Used to validate the rendezvous server's redeemed
/// notification and enforce local single-use.
class _OutstandingInvite {
  /// The rendezvous server the invite was registered with — the only sender
  /// whose redeemed notification is accepted for it.
  final String rvPubkeyHex;
  final int expiresAtMs;

  const _OutstandingInvite({
    required this.rvPubkeyHex,
    required this.expiresAtMs,
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

@visibleForTesting
bool shouldAcceptRendezvousReply(
  String pubkeyHex, {
  required SettingsState settings,
  required Iterable<String> pendingResponsePubkeys,
}) {
  final normalizedPubkeyHex = pubkeyHex.toLowerCase();

  for (final server in settings.configuredRendezvousServers) {
    if (server.pubkeyHex.isNotEmpty &&
        server.pubkeyHex.toLowerCase() == normalizedPubkeyHex) {
      return true;
    }
  }

  for (final pendingPubkeyHex in pendingResponsePubkeys) {
    if (pendingPubkeyHex == normalizedPubkeyHex) {
      return true;
    }
  }

  return false;
}

/// Whether a settings change that just brought a transport up must complete a
/// deferred [GrassrootsNetwork.start].
///
/// The wedge this guards against: with `autoStart` on, the first `initialize()`
/// can still find no usable transport (e.g. BLE permission denied and UDP off),
/// in which case it returns without ever calling `start()`, leaving `_started`
/// false. When the user later enables a transport from settings, the service
/// initializes but the warm-path per-transport `start()` calls are gated on the
/// stack already being started — so without this the transport is live but
/// inert (no advertising, scanning, or announce/scan timers) until the app
/// restarts.
///
/// True only when auto-start was requested, nothing was started yet, and a
/// transport is now usable. With `autoStart` off the caller manages `start()`
/// itself, so an unrelated settings change must not start on its behalf; the
/// disable paths (no transport available) and the warm path (already started)
/// also return false.
@visibleForTesting
bool shouldColdStartAfterSettingsChange({
  required bool autoStart,
  required bool wasStarted,
  required bool startedNow,
  required bool bleAvailable,
  required bool udpAvailable,
}) =>
    autoStart && !wasStarted && !startedNow && (bleAvailable || udpAvailable);

/// Actions that mirror an accepted friend's live UDP address (peers slice,
/// which is not persisted) into the friendship record (which is), so the
/// last-known address survives an app restart and can seed reconnection —
/// the hydration path in `main.dart` re-associates it on startup.
///
/// One action per friend whose live address differs from the stored one.
/// A friend with no live address produces nothing: the stored address is the
/// peer's last known location and is never cleared unilaterally (CLAUDE.md
/// "Peer Address Persistence" — only an explicit peer report may clear it).
@visibleForTesting
List<UpdateFriendshipUdpAddressAction> computeFriendUdpAddressMirrorActions({
  required FriendshipsState friendships,
  required PeersState peers,
}) {
  final actions = <UpdateFriendshipUdpAddressAction>[];
  for (final friendship in friendships.friends) {
    final peer =
        peers.getPeerByPubkeyHex(friendship.peerPubkeyHex.toLowerCase());
    final address = peer?.udpAddress;
    if (address == null || address.isEmpty) continue;
    if (address == friendship.udpAddress) continue;
    actions.add(UpdateFriendshipUdpAddressAction(
      peerPubkeyHex: friendship.peerPubkeyHex,
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
/// final grassroots = GrassrootsNetwork(identity: identity);
///
/// grassroots.onMessageReceived = (senderPubkey, payload) {
///   // Handle incoming GSG block
/// };
///
/// grassroots.onPeerConnected = (publicKey, transport) {
///   // peer reachable over `transport` (BLE or UDP) — start cordial dissemination
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

  /// Subscription for listening to store changes
  StreamSubscription<AppState>? _storeSubscription;

  /// Last known settings state for detecting changes
  SettingsState? _lastSettingsState;

  /// Last accepted friend set broadcast through FRIEND_LIST.
  Set<String> _lastFriendPubkeyHexes = const {};

  /// Per-peer set of reachable transports from the previous store tick. Used
  /// by the reachability subscriber to detect, per medium, transports that
  /// came up (fire onPeerConnected) or went down (fire onPeerDisconnected). A
  /// missing entry is treated as the empty set, so a peer that materializes
  /// already-reachable fires connect for each live medium. Only non-empty sets
  /// are retained — an unreachable peer holds no entry.
  final Map<String, Set<MessageTransport>> _lastKnownTransports = {};

  /// Pubkeys (hex) already surfaced via [onPeerDiscovered] this session.
  /// Discovery is the identity-learned event — the first accepted ANNOUNCE
  /// carrying a peer's public key + nickname — which precedes the Noise session
  /// and therefore [onPeerConnected]. Fired once per identity; a peer going out
  /// of range and returning is a re-*connect*, not a re-discover.
  final Set<String> _discoveredPubkeyHexes = {};

  /// Permission handler
  final PermissionHandler _permissions = PermissionHandler();

  /// BLE transport service (null if BLE is disabled or unavailable)
  BleTransportService? _bleService;

  /// UDP transport service (null if UDP is disabled)
  UdpTransportService? _udpService;

  /// Hole-punch services for NAT traversal, keyed by IP family.
  final Map<InternetAddressType, HolePunchService> _holePunchServices = {};

  /// Signaling service for address registration, queries, and hole-punch coordination
  late final SignalingService _signalingService;

  /// Public address discovery for finding our public ip:port
  final PublicAddressDiscovery _publicAddressDiscovery =
      PublicAddressDiscovery();

  /// Our discovered public address (ip:port), shared with friends
  String? _publicAddress;
  String? _linkLocalAddress;
  Set<String> _publicAddressCandidates = const {};

  final UdpConnectionService _connectionService = const UdpConnectionService();

  /// The in-flight background public-address discovery task.
  Future<void>? _publicAddressDiscoveryFuture;
  int _publicAddressDiscoveryGeneration = 0;

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

  /// Pending hole-punch completers: pubkeyHex → completer that resolves
  /// to true (connected) or false (failed) when the punch finishes.
  final Map<String, Completer<bool>> _holePunchCompleters = {};

  /// The target address we last punched toward for each peer.
  final Map<String, AddressInfo> _holePunchTargets = {};

  /// Whether we have finished our local punch for the peer.
  final Set<String> _holePunchLocalReady = {};

  /// Whether the remote peer has explicitly reported readiness.
  final Set<String> _holePunchRemoteReady = {};

  /// Prevent duplicate connect attempts while a punch is in flight.
  final Set<String> _holePunchConnectionInProgress = {};

  /// Keep punch traffic flowing while the initiator transitions from
  /// coordination into the actual UDX connect attempt.
  final Set<String> _holePunchKeepAliveInProgress = {};

  /// Deduplicate in-flight UDX connection attempts across all callers.
  final Map<String, Future<bool>> _udpConnectInFlight = {};

  /// Deduplicate proactive auto-UDP workflows kicked off by repeated ANNOUNCEs.
  final Map<String, Future<void>> _autoUdpConnectInFlight = {};
  final Map<String, String> _autoUdpLastAddress = {};
  final Map<String, DateTime> _autoUdpRetryAfter = {};

  /// Tracks when we last attempted discovery for each unreachable friend.
  /// Prevents hammering discovery every announce tick (10s) for the same peer.
  final Map<String, DateTime> _lastDiscoveryAttempt = {};

  /// Minimum interval between discovery attempts for the same peer.
  static const _discoveryRetryInterval = Duration(seconds: 60);

  /// When we last fan-ed out an AVAILABLE for each unreachable friend.
  /// Used to periodically re-fire AVAILABLE on the next announce tick so the
  /// pairing window at the friend's RV stays open while the friend is mid-
  /// reconnect (their stale-cache may stall their RECONNECT for tens of
  /// seconds). Cleared when the friend becomes UDP-reachable again.
  final Map<String, DateTime> _availableFanOutLastFiredAt = {};

  /// Minimum interval between AVAILABLE re-fires for the same friend while
  /// they remain UDP-unreachable.
  static const _availableRefireInterval = Duration(minutes: 5);

  /// Back off briefly after a failed proactive UDP attempt so repeated BLE
  /// ANNOUNCEs don't start a fresh UDX handshake every few seconds.
  static const _autoUdpRetryBackoff = Duration(seconds: 15);
  static const _holePunchKeepAliveDuration = Duration(seconds: 3);

  /// BLE device IDs that have already received a directed friend ANNOUNCE on
  /// the current connection. Cleared on disconnect so reconnects get a fresh
  /// addressed ANNOUNCE as soon as we know who is on the other side.
  final Set<String> _bleFriendAnnounceSent = {};

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
  final Map<String, Set<Completer<void>>> _rendezvousResponseWaiters = {};

  static const _rendezvousNetworkUnreachableBackoff = Duration(seconds: 15);
  static const _rendezvousHandshakeTimeoutBackoff = Duration(seconds: 20);

  // ===== Public callbacks =====

  /// Called when an application message is received.
  /// Parameters: messageId, senderPubkey, payload (raw GSG block data),
  /// transport (the transport the message actually arrived on).
  void Function(
    String messageId,
    Uint8List senderPubkey,
    Uint8List payload,
    MessageTransport transport,
  )? onMessageReceived;

  /// Spec-compliant receive callback
  /// (`docs/GLP_Networking_API/sections/api.tex` §onReceive).
  ///
  /// Fired alongside [onMessageReceived] for every accepted message. Use this
  /// when you only need the sender's pubkey and payload. The richer
  /// [onMessageReceived] variant additionally provides a transport-assigned
  /// `messageId` (useful for dedup / ACK correlation; also dispatched via the
  /// Redux `MessageReceivedAction`) and the transport the message actually
  /// arrived on (taken from the receive path).
  void Function(Uint8List senderPubkey, Uint8List payload)? onReceive;

  /// Spec: `docs/GLP_Networking_API/sections/api.tex` §onPeerDiscovered.
  /// Fires when a new peer's identity becomes known to us for the first
  /// time (the first ANNOUNCE arrives carrying their pubkey and nickname).
  /// Receives the peer's public key and nickname per the spec.
  void Function(Uint8List publicKey, String nickname)? onPeerDiscovered;

  /// Called when a peer becomes reachable over a given transport. Fires once
  /// per medium: a peer reachable over both BLE and IP fires twice (one BLE,
  /// one IP), and a medium that drops and later recovers fires again. Receives
  /// the peer's public key and the [MessageTransport] it connected over. Spec
  /// `docs/GLP_Networking_API/sections/api.tex` §Connection and Reachability.
  void Function(Uint8List publicKey, MessageTransport transport)?
      onPeerConnected;

  /// Called when an existing peer sends an ANNOUNCE update.
  void Function(PeerState peer)? onPeerUpdated;

  /// Called when a peer stops being reachable over a given transport. Fires
  /// per medium: losing one of two live transports fires here for the medium
  /// that dropped while the peer stays reachable over the other. Receives the
  /// peer's public key and the [MessageTransport] that went down. Spec
  /// `docs/GLP_Networking_API/sections/api.tex` §Connection and Reachability.
  void Function(Uint8List publicKey, MessageTransport transport)?
      onPeerDisconnected;

  /// Called when UDP transport becomes available
  void Function()? onUdpInitialized;

  /// Spec: `docs/GLP_Networking_API/sections/api.tex` §onConnectivityStatus
  /// (named `onConnectivityStatusChanged` here — the spec name reads like a
  /// status getter; this is a change-event callback).
  /// "Callback when the networking layer detects a change in the agent's
  /// public IP address. Triggers the rendezvous protocol. Fired on startup
  /// and on address change."
  ///
  /// Receives `(oldAddress, newAddress)` as `ip:port` strings (or null when
  /// no public address is available). The caller derives the kind of
  /// change:
  ///   - startup / gain: `oldAddress == null && newAddress != null`
  ///   - loss:           `oldAddress != null && newAddress == null`
  ///   - update:         both non-null and `oldAddress != newAddress`
  void Function(String? oldAddress, String? newAddress)?
      onConnectivityStatusChanged;

  /// Fired when a peer link minted by [generatePeerLink] is redeemed: the
  /// rendezvous server verified the holder's claim, atomically marked the
  /// invite used, and forwarded the redeemer's identity (spec §IP Cold-Call).
  /// Receives the redeemer's public key. Everything beyond this point —
  /// whether to accept, the signaling that follows, the resulting NAT
  /// traversal — is GLP-level and not the transport's concern.
  void Function(Uint8List redeemerPubkey)? onPeerLinkRedeemed;

  /// Outstanding invites minted by [generatePeerLink], keyed by lowercase
  /// InviteId hex. Held in-memory only: an invite is a short-lived (1 hour)
  /// secret-adjacent artifact — deliberately NOT in the Redux store, which is
  /// persisted to disk. Lost invites simply expire server-side.
  final Map<String, _OutstandingInvite> _outstandingInvites = {};

  // ===== Convenience accessors for Redux state =====

  PeersState get _peersState => store.state.peers;

  GrassrootsNetwork({
    required this.identity,
    this.config = const GrassrootsNetworkConfig(),
    required this.store,
    required this.sodium,
  }) {
    _protocolHandler = ProtocolHandler(identity: identity, sodium: sodium);
    _fragmentHandler = FragmentHandler();
    _noiseSessions = NoiseSessionManager(identity: identity, sodium: sodium);
    _messageRouter = MessageRouter(
      store: store,
      protocolHandler: _protocolHandler,
      fragmentHandler: _fragmentHandler,
    );
    _signalingService = SignalingService(store: store);
    _setupRouterCallbacks();
    _setupSignalingCallbacks();

    // Listen to network connectivity changes (WiFi ↔ cellular, etc.)
    _connectivitySubscription = Connectivity().onConnectivityChanged.listen(
          _onConnectivityChanged,
        );
    _seedConnectivityState();

    // Listen to Redux store changes for settings and friendship updates
    _lastSettingsState = store.state.settings;
    _lastFriendPubkeyHexes = store.state.peers.friendPubkeyHexes;
    _storeSubscription = store.onChange.listen((state) {
      final previousSettings = _lastSettingsState;
      if (previousSettings != null && state.settings != previousSettings) {
        _lastSettingsState = state.settings;
        _onTransportSettingsChanged(previousSettings, state.settings);
      }
      final friendPubkeyHexes = state.peers.friendPubkeyHexes;
      if (!setEquals(friendPubkeyHexes, _lastFriendPubkeyHexes)) {
        _lastFriendPubkeyHexes = friendPubkeyHexes;
        _broadcastFriendListToFriends(reason: 'friendship changed');
      }
      // Mirror friends' live UDP addresses into the persisted friendship
      // records so the last-known address survives restart. Converges in one
      // extra change notification: the mirror dispatch updates the friendship,
      // after which the addresses compare equal and no further action is
      // produced. (onChange is an async broadcast stream, so dispatching here
      // is not reentrant.)
      for (final action in computeFriendUdpAddressMirrorActions(
        friendships: state.friendships,
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

  /// Currently reachable peers — spec `docs/GLP_Networking_API/sections/api.tex`
  /// §getPeers: "Discover currently reachable peers." Returns only peers
  /// where `isReachable` is true (i.e. at least one transport is live).
  /// Callers that need the full list of known peers (including offline ones)
  /// can read `store.state.peers.peersList` directly.
  List<PeerState> getPeers() =>
      _peersState.peersList.where((p) => p.isReachable).toList();

  /// Check if a peer is reachable via any transport
  bool isPeerReachable(Uint8List pubkey) => _peersState.isPeerReachable(pubkey);

  /// The transports by which [pubkey] is currently reachable, or an empty list
  /// if it is unreachable. Spec `docs/GLP_Networking_API/sections/api.tex`
  /// §Connection and Reachability — lets GLP make proximity-aware decisions
  /// (e.g. nearby friends = friends ∩ peers reachable over BLE). Returns a
  /// fresh list ordered BLE-before-IP.
  List<MessageTransport> peerTransports(Uint8List pubkey) =>
      _peersState.getPeerByPubkey(pubkey)?.reachableTransports.toList() ??
      const <MessageTransport>[];

  /// The agent's current public `ip:port`, or null if not yet known.
  /// Address changes are surfaced via [onConnectivityStatusChanged].
  String? getPublicAddress() => store.state.transports.publicAddress;

  /// The last known public `ip:port` for [pubkey], or null if unknown.
  ///
  /// This is the observed/stored peer address (dial-book entry), not our own
  /// address. For the local agent's address use [getPublicAddress].
  String? peerAddress(Uint8List pubkey) =>
      _peersState.getPeerByPubkey(pubkey)?.udpAddress;

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
  }

  /// Produce a signed, one-time, rendezvous-mediated invite link and register
  /// the invite with a chosen rendezvous server. Spec
  /// `GLP_Networking_API` §IP Cold-Call: `generatePeerLink() -> URI`.
  ///
  /// Picks the first configured rendezvous server, mints a fresh 256-bit
  /// nonce, signs the invite record, derives the opaque InviteId/InviteKey,
  /// records the invite locally as unused, and registers it (id + proof key +
  /// expiry) with the server over the existing authenticated connection. The
  /// raw nonce travels only inside the returned link.
  ///
  /// Throws [StateError] when no rendezvous server is configured or the
  /// registration cannot be delivered (an unregistered link is unredeemable,
  /// so it is never returned).
  Future<String> generatePeerLink() async {
    // Drop links that expired without ever being redeemed so the map can't
    // grow unbounded over a long-lived session.
    final nowMs = DateTime.now().millisecondsSinceEpoch;
    _outstandingInvites.removeWhere((_, inv) => inv.expiresAtMs < nowMs);

    final servers = store.state.settings.configuredRendezvousServers;
    if (servers.isEmpty) {
      throw StateError(
        'generatePeerLink requires a configured rendezvous server',
      );
    }
    final rv = servers.first;
    final rvPubkey = _hexToBytes(rv.normalizedPubkeyHex);

    final nonce = sodium.randombytes.buf(32);
    final expiresAtMs =
        DateTime.now().add(inviteLifetime).millisecondsSinceEpoch;
    final invite = PeerLinkInvite.create(
      identityPubkey: identity.publicKey,
      rvPubkey: rvPubkey,
      rvAddress: rv.address,
      nonce: nonce,
      expiresAtMs: expiresAtMs,
      sign: _protocolHandler.signBytes,
    );

    final inviteId = deriveInviteId(nonce);
    final inviteKey = await deriveInviteKey(
      nonce: nonce,
      inviterPubkey: identity.publicKey,
      rvPubkey: rvPubkey,
      rvAddress: rv.address,
      expiresAtMs: expiresAtMs,
    );

    final registration = const SignalingCodec().encode(RegisterInviteMessage(
      inviteId: inviteId,
      inviteKey: inviteKey,
      expiresAtMs: expiresAtMs,
    ));
    final delivered =
        await _signalingService.sendSignaling?.call(rvPubkey, registration) ??
            false;
    if (!delivered) {
      throw StateError(
        'Could not register invite with rendezvous server ${rv.address}',
      );
    }

    final inviteIdHex = _pubkeyToHex(inviteId);
    _outstandingInvites[inviteIdHex] = _OutstandingInvite(
      rvPubkeyHex: rv.normalizedPubkeyHex,
      expiresAtMs: expiresAtMs,
    );
    debugPrint('[peer-link] registered invite '
        '${inviteIdHex.substring(0, 8)}... with RV ${rv.address}');
    return invite.toUri();
  }

  /// Redeem a peer link received out-of-band. Spec `GLP_Networking_API`
  /// §IP Cold-Call: `consumePeerLink(uri) -> PubKey`.
  ///
  /// Verifies the inviter's signature and the expiration, registers the
  /// inviter's identity locally, re-derives InviteId/InviteKey from the
  /// link's nonce, and submits the redemption claim (InviteId, our identity
  /// as the authenticated sender, MAC under InviteKey) to the rendezvous
  /// server named in the link. Returns the inviter's public key.
  ///
  /// Throws [FormatException] for a malformed or signature-invalid link and
  /// [StateError] for an expired/self-issued link or an unreachable
  /// rendezvous server.
  Future<Uint8List> consumePeerLink(String uri) async {
    final invite = PeerLinkInvite.fromUri(uri);
    if (!invite.verifySignature(sodium)) {
      throw const FormatException('Invite signature verification failed');
    }
    if (invite.isExpired) {
      throw StateError('Invite link has expired');
    }
    if (listEquals(invite.inviterPubkey, identity.publicKey)) {
      throw StateError('Cannot redeem an invite issued by this identity');
    }

    // Register the inviter's identity locally (spec) — reachability and
    // addresses follow later through the GLP-driven connection flow.
    store.dispatch(
      PeerIdentityRegisteredAction(publicKey: invite.inviterPubkey),
    );

    final inviteId = deriveInviteId(invite.nonce);
    final inviteKey = await deriveInviteKey(
      nonce: invite.nonce,
      inviterPubkey: invite.inviterPubkey,
      rvPubkey: invite.rvPubkey,
      rvAddress: invite.rvAddress,
      expiresAtMs: invite.expiresAtMs,
    );
    final redeemerNonce = sodium.randombytes.buf(32);
    final mac = await computeRedemptionMac(
      inviteKey: inviteKey,
      inviterPubkey: invite.inviterPubkey,
      redeemerPubkey: identity.publicKey,
      inviteId: inviteId,
      redeemerNonce: redeemerNonce,
    );

    final claim = const SignalingCodec().encode(RedeemInviteMessage(
      inviteId: inviteId,
      redeemerNonce: redeemerNonce,
      mac: mac,
    ));
    final delivered = await _signalingService.sendSignalingToAddress
            ?.call(invite.rvPubkey, invite.rvAddress, claim) ??
        false;
    if (!delivered) {
      throw StateError(
        'Could not submit redemption to rendezvous server ${invite.rvAddress}',
      );
    }
    debugPrint('[peer-link] redeemed invite from '
        '${_pubkeyToHex(invite.inviterPubkey).substring(0, 8)}... '
        'via RV ${invite.rvAddress}');
    return invite.inviterPubkey;
  }

  /// Get peer by public key - from Redux store
  PeerState? getPeer(Uint8List pubkey) => _peersState.getPeerByPubkey(pubkey);

  /// Get latest RSSI for a peer (BLE only)
  int? getRssiForPeer(Uint8List pubkey) {
    final peer = _peersState.getPeerByPubkey(pubkey);
    return peer?.rssi;
  }

  /// Whether BLE is currently enabled and available
  bool get isBleEnabled => _bleAvailable && _isBleEnabledInSettings;

  /// Whether UDP is currently enabled and available
  bool get isUdpEnabled => _udpAvailable && _isUdpEnabledInSettings;

  ColdCallTrustLevel get coldCallTrustLevel =>
      store.state.settings.coldCallTrustLevel;

  List<RendezvousServerSettings> get configuredRendezvousServers =>
      store.state.settings.configuredRendezvousServers;

  /// Our UDP address to share with friends.
  ///
  /// Returns the public UDP address discovered for the active IP family.
  /// Never returns a private LAN address. Returns null if public address
  /// discovery failed and we therefore have nothing to advertise.
  String? get udpAddress => _publicAddress;

  /// UDP address candidates to share with trusted peers.
  Set<String> get udpAddressCandidates => Set.unmodifiable(
        _candidateAddresses(includeLinkLocal: _linkLocalAddress != null),
      );

  /// Whether currently scanning for BLE devices
  bool get isScanning => _bleService?.isScanning ?? false;

  bool get _isBleEnabledInSettings => store.state.settings.bluetoothEnabled;

  bool get _isUdpEnabledInSettings => store.state.settings.udpEnabled;

  Future<bool> addRendezvousServer({
    required String address,
    required String pubkeyHex,
  }) async {
    final config = _parseRendezvousConfig(
      address: address,
      pubkeyHex: pubkeyHex,
    );
    if (config == null) return false;

    if (_hasConfiguredRendezvousServer(config)) {
      return true;
    }

    final responded = await _verifyRendezvousServerResponds(config);
    if (!responded) return false;

    store.dispatch(
      AddRendezvousServerAction(
        RendezvousServerSettings(
          address: config.address,
          pubkeyHex: config.pubkeyHex,
        ),
      ),
    );
    return true;
  }

  Future<void> removeRendezvousServer({
    required String address,
    required String pubkeyHex,
  }) async {
    store.dispatch(
      RemoveRendezvousServerAction(
        RendezvousServerSettings(address: address, pubkeyHex: pubkeyHex),
      ),
    );
  }

  /// Force a fresh public-address discovery attempt, bypassing the seeip cache.
  ///
  /// Invoked by the UI when the user taps "Retry" on the no-public-address
  /// warning. Friend/RV reflection runs on its own cadence and is not poked
  /// here; this only re-runs seeip-based discovery.
  Future<void> retryPublicAddressDiscovery() async {
    _publicAddressDiscovery.invalidateCache();
    await _discoverPublicAddress();
  }

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
      // Initialize BLE if enabled
      if (_isBleEnabledInSettings) {
        anyTransportInitialized =
            await _initializeBle() || anyTransportInitialized;
      }

      // Initialize UDP if enabled
      if (_isUdpEnabledInSettings) {
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

      // Start multiplexer immediately (punch packets can still be sent via raw socket)
      _udpService!.startMultiplexer();

      // Discover our public UDP address for the active IP family in the
      // background.
      _publicAddressDiscoveryFuture = _discoverPublicAddress();

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
      } catch (e) {
        debugPrint('Failed to start UDP: $e');
      }
    }

    _started = true;
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
    _bleFriendAnnounceSent.clear();

    if (_bleService != null) {
      try {
        await _bleService!.stop();
      } catch (e) {
        debugPrint('Error stopping BLE: $e');
      }
      _noiseSessions.resetTransport(PeerTransport.bleDirect);
    }

    if (_udpService != null) {
      try {
        await _udpService!.stop();
      } catch (e) {
        debugPrint('Error stopping UDP: $e');
      }
      _noiseSessions.resetTransport(PeerTransport.udp);
    }
  }

  /// Handle transport settings changes.
  /// Serializes updates so overlapping init/dispose sequences cannot occur.
  void _onTransportSettingsChanged(
    SettingsState previousSettings,
    SettingsState currentSettings,
  ) {
    debugPrint('Transport settings changed');
    final previous = _transportUpdateLock ?? Future.value();
    _transportUpdateLock = previous.then(
      (_) => _updateTransportsFromSettings(
        previousSettings: previousSettings,
        currentSettings: currentSettings,
      ),
    );
  }

  static Uint8List _hexToBytes(String hex) {
    final bytes = <int>[];
    for (var i = 0; i < hex.length; i += 2) {
      bytes.add(int.parse(hex.substring(i, i + 2), radix: 16));
    }
    return Uint8List.fromList(bytes);
  }

  _RendezvousConfig? _parseRendezvousConfig({
    required String address,
    required String pubkeyHex,
  }) {
    final normalizedAddress = _normalizeAnnouncedUdpAddress(
      address,
      context: 'rendezvous',
    );
    if (normalizedAddress == null) {
      debugPrint('[rendezvous] Ignoring invalid configured address: $address');
      return null;
    }

    try {
      final normalizedPubkeyHex = pubkeyHex.toLowerCase();
      return _RendezvousConfig(
        address: normalizedAddress,
        pubkey: _hexToBytes(normalizedPubkeyHex),
        pubkeyHex: normalizedPubkeyHex,
      );
    } catch (e) {
      debugPrint('[rendezvous] Ignoring invalid configured pubkey: $e');
      return null;
    }
  }

  List<_RendezvousConfig> _configuredRendezvousServers([
    SettingsState? settings,
  ]) {
    final configured = settings ?? store.state.settings;
    final configs = <_RendezvousConfig>[];
    final seen = <String>{};

    for (final server in configured.configuredRendezvousServers) {
      final parsed = _parseRendezvousConfig(
        address: server.address,
        pubkeyHex: server.pubkeyHex,
      );
      if (parsed == null) continue;

      final key = _rendezvousConfigKey(parsed);
      if (seen.add(key)) {
        configs.add(parsed);
      }
    }

    return configs;
  }

  _RendezvousConfig? _configuredRendezvousForPubkeyHex(
    String pubkeyHex, {
    SettingsState? settings,
  }) {
    for (final config in _configuredRendezvousServers(settings)) {
      if (config.pubkeyHex == pubkeyHex) {
        return config;
      }
    }
    return null;
  }

  bool _hasConfiguredRendezvousServer(
    _RendezvousConfig config, {
    SettingsState? settings,
  }) {
    final targetKey = _rendezvousConfigKey(config);
    for (final existing in _configuredRendezvousServers(settings)) {
      if (_rendezvousConfigKey(existing) == targetKey) {
        return true;
      }
    }
    return false;
  }

  bool _isRendezvousPubkeyHex(String pubkeyHex, {SettingsState? settings}) {
    return shouldAcceptRendezvousReply(
      pubkeyHex,
      settings: settings ?? store.state.settings,
      pendingResponsePubkeys: _rendezvousResponseWaiters.keys,
    );
  }

  String _rendezvousConfigKey(_RendezvousConfig config) =>
      '${config.pubkeyHex}@${config.address}';

  bool _canDialUdpAddress(AddressInfo address) =>
      _udpService != null &&
      _udpAvailable &&
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

  bool _isRendezvousBackoffActive(_RendezvousConfig rendezvous, String reason) {
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

  void _completeRendezvousResponseWaiters(String pubkeyHex) {
    final waiters = _rendezvousResponseWaiters.remove(pubkeyHex);
    if (waiters == null) return;

    for (final waiter in waiters) {
      if (!waiter.isCompleted) {
        waiter.complete();
      }
    }
  }

  void _removeRendezvousResponseWaiter(
    String pubkeyHex,
    Completer<void> waiter,
  ) {
    final waiters = _rendezvousResponseWaiters[pubkeyHex];
    if (waiters == null) return;
    waiters.remove(waiter);
    if (waiters.isEmpty) {
      _rendezvousResponseWaiters.remove(pubkeyHex);
    }
  }

  Future<bool> _verifyRendezvousServerResponds(
    _RendezvousConfig config, {
    Duration timeout = const Duration(seconds: 8),
    String reason = 'settings-save',
  }) async {
    final waiter = Completer<void>();
    _rendezvousResponseWaiters
        .putIfAbsent(config.pubkeyHex, () => <Completer<void>>{})
        .add(waiter);

    try {
      final synced = await _syncConfiguredRendezvous(
        config: config,
        reason: reason,
      );
      if (!synced) {
        return false;
      }

      await waiter.future.timeout(timeout);
      return true;
    } on TimeoutException {
      debugPrint(
        '[rendezvous] Timed out waiting for server response '
        '(${config.pubkeyHex.substring(0, 8)}...)',
      );
      return false;
    } finally {
      _removeRendezvousResponseWaiter(config.pubkeyHex, waiter);
    }
  }

  Future<bool> _syncConfiguredRendezvous({
    List<_RendezvousConfig>? configs,
    _RendezvousConfig? config,
    String reason = 'sync',
  }) async {
    final targets = config != null
        ? <_RendezvousConfig>[config]
        : (configs ?? _configuredRendezvousServers());
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

        final announce = await _createSignedAnnounce(address: udpAddress);
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

  Future<bool> _disconnectConfiguredRendezvous({
    List<_RendezvousConfig>? configs,
    _RendezvousConfig? config,
    String reason = 'disconnect',
  }) async {
    final targets = config != null
        ? <_RendezvousConfig>[config]
        : (configs ?? _configuredRendezvousServers());
    if (targets.isEmpty) {
      return Future.value(false);
    }

    final results = <bool>[];
    for (final rendezvous in targets) {
      results.add(
        await _enqueueRendezvousTask(() async {
          if (_udpService == null) return false;
          if (_udpService!.getPeerIdForPubkey(rendezvous.pubkey) == null) {
            return false;
          }

          debugPrint(
            '[rendezvous] Closing UDP session '
            '(${rendezvous.pubkeyHex.substring(0, 8)}..., $reason)',
          );
          await _udpService!.disconnectFromPeer(rendezvous.pubkeyHex);
          return true;
        }),
      );
    }

    return results.any((result) => result);
  }

  Future<void> _handleRendezvousSettingsChange({
    required SettingsState previousSettings,
    required SettingsState currentSettings,
  }) async {
    final previousRendezvous = _configuredRendezvousServers(previousSettings);
    final currentRendezvous = _configuredRendezvousServers(currentSettings);

    final previousByKey = <String, _RendezvousConfig>{
      for (final config in previousRendezvous)
        _rendezvousConfigKey(config): config,
    };
    final currentByKey = <String, _RendezvousConfig>{
      for (final config in currentRendezvous)
        _rendezvousConfigKey(config): config,
    };

    final removed = <_RendezvousConfig>[];
    for (final entry in previousByKey.entries) {
      if (!currentByKey.containsKey(entry.key)) {
        removed.add(entry.value);
      }
    }

    final added = <_RendezvousConfig>[];
    for (final entry in currentByKey.entries) {
      if (!previousByKey.containsKey(entry.key)) {
        added.add(entry.value);
      }
    }

    if (removed.isEmpty && added.isEmpty) return;

    for (final rendezvous in removed) {
      _resetRendezvousBackoff(_rendezvousConfigKey(rendezvous));
    }

    if (removed.isNotEmpty) {
      await _disconnectConfiguredRendezvous(
        configs: removed,
        reason: 'settings-changed',
      );
    }

    if (added.isNotEmpty && _udpService != null && _udpAvailable) {
      await _syncConfiguredRendezvous(configs: added, reason: 'settings-saved');
    }

    // Friends rely on us to tell them which RV agents to contact when we
    // disappear. Re-broadcast whenever the list changes.
    debugPrint("Broadcasting RV list to friends");
    _broadcastRvListToFriends();
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
    _publicAddressDiscoveryGeneration++;
    _publicAddress = null;
    _linkLocalAddress = null;
    _publicAddressCandidates = const {};
    _publicAddressDiscovery.invalidateCache();
    _publicAddressDiscoveryFuture = null;
    store.dispatch(ClearPublicConnectivityAction());
  }

  /// Handle network connectivity changes (WiFi ↔ cellular, etc.).
  ///
  /// When the network changes, our UDP socket is bound to the old interface
  /// and all UDX connections are dead. We need to:
  /// 1. Tear down the old UDP service (dead socket, dead connections)
  /// 2. Re-initialize with a new socket on the new interface
  /// 3. Re-discover public address (new IP from new network)
  /// 4. Re-register with well-connected friends
  /// 5. Re-connect to known peers
  ///
  /// Well-connected friends are reachable directly (public IP, no NAT),
  /// so we can always reconnect to them without a third party.
  void _onConnectivityChanged(List<ConnectivityResult> results) {
    // Filter out irrelevant connection types like bluetooth
    // which just indicate a BLE device connected/disconnected, not an IP network change.
    // If we don't filter this, every BLE connection change tears down the UDP transport!
    final ipResults = _normalizeConnectivityResults(results);
    store.dispatch(
      NetworkConnectionTypeUpdatedAction(_connectionTypeFromResults(ipResults)),
    );

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

    if (!_isUdpEnabledInSettings) {
      debugPrint(
        'Network changed while UDP is disabled — cleared cached '
        'public connectivity and will rediscover on re-enable',
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
    if (!_isUdpEnabledInSettings) return;
    if (!_started) return;

    for (final completer in _holePunchCompleters.values) {
      if (!completer.isCompleted) {
        completer.complete(false);
      }
    }
    _holePunchCompleters.clear();
    _holePunchTargets.clear();
    _holePunchLocalReady.clear();
    _holePunchRemoteReady.clear();
    _holePunchConnectionInProgress.clear();

    // Tear down old UDP service completely
    for (final service in _holePunchServices.values) {
      service.dispose();
    }
    _holePunchServices.clear();

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
      if (_udpCandidatesForPeer(peer).isNotEmpty) {
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
    }

    await _waitForPublicUdpAddress();

    await _reconnectUdpFriends(reason: 'connectivity-changed');
  }

  /// Walk every UDP-eligible friend and bring them back online.
  /// Direct-dial known addresses; fan out RECONNECT to facilitators for
  /// friends we couldn't reach directly. Idempotent — already-connected
  /// friends are skipped by [_connectToFriendViaUdp] and by the second-pass
  /// [getPeerIdForPubkey] guard.
  Future<void> _reconnectUdpFriends({required String reason}) async {
    if (!_udpAvailable) return;

    final udpFriends = _peersState.friends
        .where((peer) => _udpCandidatesForPeer(peer).isNotEmpty)
        .toList()
      ..sort((a, b) {
        if (a.isWellConnected == b.isWellConnected) return 0;
        return a.isWellConnected ? -1 : 1;
      });
    if (udpFriends.isEmpty) return;

    debugPrint(
      '[reconnect] Sweeping ${udpFriends.length} UDP friends ($reason)',
    );

    for (final friend in udpFriends) {
      final candidates = _udpCandidatesForPeer(friend);
      final friendAddress = friend.udpAddress ??
          (candidates.isNotEmpty ? candidates.first : null);
      if (friendAddress == null) continue;
      await _connectToFriendViaUdp(friend.pubkeyHex, friendAddress);
    }

    // Fan out RECONNECT for friends still unreachable. Rendezvous servers pair
    // this with the friend's AVAILABLE; eligible friend mediators are selected
    // through the friends-of-friends map and coordinate directly.
    final facilitatorCount = store.state.peers.wellConnectedFriends.length +
        _configuredRendezvousServers().length +
        store.state.friendships.friendRvServers.length;
    if (facilitatorCount == 0) return;

    for (final friend in udpFriends) {
      if (_udpService?.getPeerIdForPubkey(friend.publicKey) != null) continue;
      debugPrint(
        '[reconnect] Fanning out RECONNECT for ${friend.displayName} '
        'after IP change',
      );
      unawaited(
        _signalingService.fanOutReconnect(
          friend.publicKey,
          initiatorPubkey: identity.publicKey,
        ),
      );
    }
  }

  /// Public entry point for triggering a UDP friend-reconnection sweep.
  /// Chains on the transport update lock so it cannot overlap with a
  /// connectivity-driven or settings-driven restart.
  Future<void> reconnectUdpFriends({required String reason}) {
    final previous = _transportUpdateLock ?? Future.value();
    final next = previous.then((_) => _reconnectUdpFriends(reason: reason));
    _transportUpdateLock = next;
    return next;
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

  /// Update transports based on current settings
  Future<void> _updateTransportsFromSettings({
    required SettingsState previousSettings,
    required SettingsState currentSettings,
  }) async {
    final wasStarted = _started;

    // Handle BLE enable/disable
    if (_isBleEnabledInSettings && !_bleAvailable) {
      // BLE was enabled, try to initialize
      // Dispose old service first to clean up native state (GATT server, subscriptions)
      if (_bleService != null) {
        debugPrint('Disposing old BLE service before re-initialization');
        _noiseSessions.resetTransport(PeerTransport.bleDirect);
        await _bleService!.dispose();
        _bleService = null;
      }
      await _initializeBle();
      if (wasStarted && _bleAvailable) {
        await _bleService!.start();
      }
    } else if (!_isBleEnabledInSettings && _bleAvailable) {
      // BLE was disabled, dispose service and clean up
      debugPrint('BLE disabled from settings, cleaning up...');

      if (_bleService != null) {
        await _bleService!.dispose();
        _bleService = null;
      }
      _noiseSessions.resetTransport(PeerTransport.bleDirect);

      // Reset Redux state so _bleAvailable returns false
      store.dispatch(
        BleTransportStateChangedAction(TransportState.uninitialized),
      );

      // Clear all discovered BLE peers from Redux
      store.dispatch(ClearDiscoveredBlePeersAction());

      // Disconnect all peers that were connected via BLE
      for (final peer in _peersState.peersList) {
        if (peer.hasBleConnection) {
          store.dispatch(PeerBleDisconnectedAction(peer.publicKey));
        }
      }

      debugPrint('BLE cleanup complete');
    }

    // Handle UDP enable/disable
    if (_isUdpEnabledInSettings && !_udpAvailable) {
      // UDP was enabled, try to initialize
      await _initializeUdp();
      if (wasStarted && _udpAvailable) {
        await _udpService!.start();
        await _reconnectUdpFriends(reason: 'settings-enabled');
      }
    } else if (!_isUdpEnabledInSettings && _udpAvailable) {
      // UDP was disabled, dispose service and clean up
      debugPrint('UDP disabled from settings, cleaning up...');

      for (final service in _holePunchServices.values) {
        service.dispose();
      }
      _holePunchServices.clear();

      if (_udpService != null) {
        _noiseSessions.resetTransport(PeerTransport.udp);
        await _udpService!.dispose();
        _udpService = null;
      }

      _clearDiscoveredPublicConnectivity();

      // Reset Redux state so _udpAvailable returns false
      store.dispatch(
        UdpTransportStateChangedAction(TransportState.uninitialized),
      );

      // Disconnect all peers that were connected via UDP
      for (final peer in _peersState.peersList) {
        if (_udpCandidatesForPeer(peer).isNotEmpty) {
          store.dispatch(PeerUdpDisconnectedAction(peer.publicKey));
        }
      }

      debugPrint('UDP cleanup complete');
    }

    // Cold-start recovery: if the transport was never started (e.g. the first
    // initialize() found no usable transport because BLE permission was denied
    // and UDP was off, so it returned without calling start()) and a transport
    // has now come up via this settings change, do a full start. Otherwise the
    // service is live but inert — no advertising, no scanning, no announce/scan
    // timers, no started flag — until the app restarts. The per-transport
    // start() calls above only cover the warm path (wasStarted == true); start()
    // itself is guarded by _started, so this cannot double-start.
    if (shouldColdStartAfterSettingsChange(
      autoStart: config.autoStart,
      wasStarted: wasStarted,
      startedNow: _started,
      bleAvailable: _bleAvailable,
      udpAvailable: _udpAvailable,
    )) {
      debugPrint(
        'Transport came up from settings while never started — starting now',
      );
      await start();
      if (_udpAvailable) {
        await _reconnectUdpFriends(reason: 'settings-enabled-cold-start');
      }
    }

    await _handleRendezvousSettingsChange(
      previousSettings: previousSettings,
      currentSettings: currentSettings,
    );
  }

  // ===== Identity =====

  /// Update the user's nickname and broadcast to all peers
  Future<void> updateNickname(String newNickname) async {
    if (newNickname.isEmpty) return;

    debugPrint('Updating nickname to: $newNickname');
    identity.nickname = newNickname;

    // Broadcast ANNOUNCE with new nickname to all connected peers
    await _broadcastAnnounce();
  }

  /// Apply a debug change to which BLE roles this device runs.
  /// Dispatches a Redux action and restarts the BLE transport so the new
  /// mode takes effect immediately.
  Future<void> setBleRoleMode(BleRoleMode mode) async {
    if (store.state.settings.bleRoleMode == mode) return;
    store.dispatch(SetBleRoleModeAction(mode));
    await _bleService?.applyRoleModeChange();
  }

  /// Set the BLE cold-call trust level (Open/Closed). Spec
  /// `docs/GLP_Networking_API/sections/ble.tex` §Cold-Call Trust Levels:
  /// `setTrustLevel(level)`. Until set, the level is Closed.
  Future<void> setTrustLevel(ColdCallTrustLevel level) async {
    if (store.state.settings.coldCallTrustLevel == level) return;
    store.dispatch(SetColdCallTrustLevelAction(level));
  }

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
  /// falls back to UDP, then attempts discovery via well-connected friends.
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
        transport: MessageTransport.ble, // Tentative — updated on actual send
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

    // Create the message packet. The payload's messageId prefix is the id
    // the recipient's ACK echoes back, matching the entry we just stored
    // under `messageId` in `MessageSendingAction` so
    // `MessageDeliveredAction` can flip ✓ → ✓✓.
    final packet = _protocolHandler.createMessagePacket(
      payload: payload,
      messageId: messageId,
    );

    // --- Try BLE first (preferred for nearby peers) ---
    final bleDeviceId = _connectedBleDeviceIdForPeer(peer);
    if (_isBleEnabledInSettings &&
        _bleAvailable &&
        _bleService != null &&
        bleDeviceId != null) {
      debugPrint('Sending via BLE to ${peer.displayName}');

      bool success;
      if (_fragmentHandler.needsFragmentation(payload)) {
        success = await _sendFragmentedViaBle(
          payload: payload,
          recipientPubkey: recipientPubkey,
          bleDeviceId: bleDeviceId,
          messageId: messageId,
        );
      } else {
        final bytes = await _packetBytesForTransport(
          packet: packet,
          transport: PeerTransport.bleDirect,
          recipientPubkey: recipientPubkey,
          peerId: bleDeviceId,
        );
        if (bytes == null) {
          success = false;
        } else {
          success = await _bleService!.sendToPeer(bleDeviceId, bytes);
        }
      }

      if (success) {
        _markSentAndTrackForAck(
          messageId: messageId,
          recipientPubkey: recipientPubkey,
          payload: payload,
          transport: MessageTransport.ble,
          bleDeviceId: bleDeviceId,
        );
        // Delivery confirmed by ACK, not BLE write success.
        return true;
      }
      debugPrint('BLE send failed, falling back to UDP...');
    }

    // --- Try UDP (direct connection or connect-on-demand) ---
    if (_isUdpEnabledInSettings && _udpAvailable && _udpService != null) {
      // Re-read peer — state may have changed during BLE attempt.
      final resolvedPeer = _peersState.getPeerByPubkey(recipientPubkey) ?? peer;

      // Try existing UDX connection first
      if (_udpService!.getPeerIdForPubkey(recipientPubkey) != null) {
        final bytes = await _packetBytesForTransport(
          packet: packet,
          transport: PeerTransport.udp,
          recipientPubkey: recipientPubkey,
          peerId: resolvedPeer.pubkeyHex,
        );
        if (bytes != null &&
            await _udpService!.sendToPeer(resolvedPeer.pubkeyHex, bytes)) {
          // debugPrint(
          //   'Sent via existing UDP connection to ${resolvedPeer.displayName}',
          // );
          _markSentAndTrackForAck(
            messageId: messageId,
            recipientPubkey: recipientPubkey,
            payload: payload,
            transport: MessageTransport.udp,
            bleDeviceId: null,
          );
          return true;
        }
      }

      // No existing connection — try connect-on-demand if we have an address
      final udpCandidates = _udpCandidatesForPeer(resolvedPeer);
      if (udpCandidates.isNotEmpty) {
        final udpAddr = resolvedPeer.udpAddress ?? udpCandidates.first;
        debugPrint(
          'Sending via UDP to ${resolvedPeer.displayName} at $udpCandidates',
        );
        if (await _sendPacketViaUdp(
          pubkeyHex: resolvedPeer.pubkeyHex,
          udpAddress: udpAddr,
          packet: packet,
          recipientPubkey: recipientPubkey,
        )) {
          _markSentAndTrackForAck(
            messageId: messageId,
            recipientPubkey: recipientPubkey,
            payload: payload,
            transport: MessageTransport.udp,
            bleDeviceId: null,
          );
          return true;
        }
      }

      // No address — try discovery via well-connected friends
      if (resolvedPeer.isFriend) {
        debugPrint(
          '[send] No direct path to ${resolvedPeer.displayName}, '
          'attempting discovery via well-connected friends...',
        );
        final discovered = await _discoverPeerViaFriends(resolvedPeer);
        if (discovered) {
          // Re-read peer — discovery updated the address
          final freshPeer = _peersState.getPeerByPubkey(recipientPubkey);
          final freshCandidates = _udpCandidatesForPeer(freshPeer);
          if (freshPeer != null && freshCandidates.isNotEmpty) {
            debugPrint('[send] Discovery succeeded, sending via UDP');
            if (await _sendPacketViaUdp(
              pubkeyHex: freshPeer.pubkeyHex,
              udpAddress: freshPeer.udpAddress ?? freshCandidates.first,
              packet: packet,
              recipientPubkey: recipientPubkey,
            )) {
              _markSentAndTrackForAck(
                messageId: messageId,
                recipientPubkey: recipientPubkey,
                payload: payload,
                transport: MessageTransport.udp,
                bleDeviceId: null,
              );
              return true;
            }
          }
        }
        debugPrint('[send] Discovery failed for ${resolvedPeer.displayName}');
      }
    }

    debugPrint(
      'No transport currently available for ${peer.displayName}; queuing message',
    );
    return false;
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
              transport: MessageTransport.ble,
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
      if (peer == null || !_hasLiveSendPath(peer)) continue;
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
    required MessageTransport transport,
    required String? bleDeviceId,
  }) {
    store.dispatch(
      MessageSentAction(
        messageId: messageId,
        transport: transport,
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
    if (_isBleEnabledInSettings && _bleAvailable && _bleService != null) {
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
    if (_isUdpEnabledInSettings && _udpAvailable && _udpService != null) {
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

  /// Broadcast a message to all peers on all enabled transports.
  Future<void> broadcast(Uint8List payload) async {
    // Broadcast as individually encrypted unicast packets, one Noise session
    // per peer per transport medium.
    if (_isBleEnabledInSettings && _bleAvailable && _bleService != null) {
      try {
        for (final peerId in _bleService!.connectedPeerIds) {
          final pubkey = _bleService!.getPubkeyForPeerId(peerId);
          if (pubkey == null) continue;
          if (_fragmentHandler.needsFragmentation(payload)) {
            await _sendFragmentedViaBle(
              payload: payload,
              recipientPubkey: pubkey,
              bleDeviceId: peerId,
              messageId: _uuid.v4(),
            );
          } else {
            final packet = _protocolHandler.createMessagePacket(
              payload: payload,
              messageId: _uuid.v4(),
            );
            final bytes = await _packetBytesForTransport(
              packet: packet,
              transport: PeerTransport.bleDirect,
              recipientPubkey: pubkey,
              peerId: peerId,
            );
            if (bytes != null) {
              await _bleService!.sendToPeer(peerId, bytes);
            }
          }
        }
      } catch (e) {
        debugPrint('BLE broadcast failed: $e');
      }
    }

    if (_isUdpEnabledInSettings && _udpAvailable && _udpService != null) {
      try {
        for (final peer in _peersState.peersList) {
          if (_udpService!.getPeerIdForPubkey(peer.publicKey) == null) {
            continue;
          }
          final packet = _protocolHandler.createMessagePacket(
            payload: payload,
            messageId: _uuid.v4(),
          );
          final bytes = await _packetBytesForTransport(
            packet: packet,
            transport: PeerTransport.udp,
            recipientPubkey: peer.publicKey,
            peerId: peer.pubkeyHex,
          );
          if (bytes != null) {
            await _udpService!.sendToPeer(peer.pubkeyHex, bytes);
          }
        }
      } catch (e) {
        debugPrint('UDP broadcast failed: $e');
      }
    }
  }

  // ===== Public Address Discovery =====

  /// Discover our public UDP address and combine it with the bound UDP port.
  Future<void> _discoverPublicAddress() async {
    final udpService = _udpService;
    final discoveryGeneration = _publicAddressDiscoveryGeneration;
    if (udpService == null || udpService.activeAddressTypes.isEmpty) return;
    final previousAddress = _publicAddress;
    final previousCandidates = _publicAddressCandidates;
    final discoveredCandidates = <String>{};

    // Clear the failure flag while an attempt is in flight so the UI hides
    // the warning during retry.
    store.dispatch(PublicAddressDiscoveryFailedAction(false));

    for (final family in const [
      InternetAddressType.IPv6,
      InternetAddressType.IPv4,
    ]) {
      final localPort = udpService.localPortForAddressType(family);
      if (localPort == null) continue;

      final publicAddr = await _publicAddressDiscovery.getPublicAddress(
        localPort,
        type: family,
      );
      if (publicAddr != null) {
        discoveredCandidates.add(publicAddr);
      }
    }

    if (_publicAddressDiscoveryGeneration != discoveryGeneration ||
        _udpService != udpService ||
        !_isUdpEnabledInSettings) {
      return;
    }

    _publicAddressCandidates = normalizeAddressStrings(discoveredCandidates);
    final publicAddr = _preferredPublicAddress(_publicAddressCandidates);
    if (publicAddr != null) {
      _publicAddress = publicAddr;
      store.dispatch(PublicAddressUpdatedAction(publicAddr));
      debugPrint('Public UDP address: $_publicAddress');
      debugPrint('Public UDP candidates: $_publicAddressCandidates');
      if (publicAddr != previousAddress ||
          !setEquals(_publicAddressCandidates, previousCandidates)) {
        _resetRendezvousBackoff();
        _resetAutoUdpBackoff();
        unawaited(_syncConfiguredRendezvous(reason: 'public-address-updated'));
        // Spec onConnectivityStatus: fires on every public address change.
        // Startup case is `previousAddress == null`, gain case same.
        onConnectivityStatusChanged?.call(previousAddress, publicAddr);
      }
    } else {
      debugPrint(
        'Could not discover a public UDP address. '
        'No UDP address will be advertised.',
      );
      if (previousAddress != null) {
        // Spec onConnectivityStatus: address became unavailable.
        onConnectivityStatusChanged?.call(previousAddress, null);
      }
    }

    // Always update the display IP (even if no full address/port available).
    final bestIp = _publicAddressDiscovery.bestPublicIp;
    if (bestIp != null) {
      store.dispatch(PublicIpUpdatedAction(bestIp.address));
    }

    // If we still have neither a full public address nor any reflected/
    // discovered IP, flag discovery as failed so the UI can warn the user.
    final transports = store.state.transports;
    if (transports.publicAddress == null && transports.publicIp == null) {
      store.dispatch(PublicAddressDiscoveryFailedAction(true));
    }

    // Discover link-local IPv6 for same-LAN fallback.
    final ipv6Port = udpService.localPortForAddressType(
      InternetAddressType.IPv6,
    );
    final llAddr = ipv6Port != null
        ? await _publicAddressDiscovery.getLinkLocalAddress(ipv6Port)
        : null;
    if (_publicAddressDiscoveryGeneration != discoveryGeneration ||
        _udpService != udpService ||
        !_isUdpEnabledInSettings) {
      return;
    }
    if (llAddr != null) {
      _linkLocalAddress = llAddr;
      debugPrint('Link-local UDP address: $_linkLocalAddress');
    }
  }

  Future<void> _waitForPublicUdpAddress({
    Duration timeout = const Duration(seconds: 6),
  }) async {
    final inFlight = _publicAddressDiscoveryFuture;
    if (inFlight == null) return;
    try {
      await inFlight.timeout(timeout);
    } catch (_) {}
  }

  String? _preferredPublicAddress(Set<String> candidates) {
    String? ipv4;
    for (final candidate in candidates) {
      final parsed = parseAddressString(candidate);
      if (parsed == null || parsed.ip.isLinkLocal) continue;
      if (parsed.ip.type == InternetAddressType.IPv6) {
        return parsed.toAddressString();
      }
      if (parsed.ip.type == InternetAddressType.IPv4) {
        ipv4 ??= parsed.toAddressString();
      }
    }
    return ipv4;
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

  String? _normalizeAnnouncedLinkLocalAddress(
    String? udpAddress, {
    required String context,
  }) {
    if (udpAddress == null || udpAddress.isEmpty) return null;

    final parsed = parseIpv6AddressString(udpAddress);
    if (parsed == null) {
      debugPrint(
        '[$context] Ignoring non-link-local IPv6 address in link-local '
        'ANNOUNCE field: $udpAddress',
      );
      return null;
    }

    if (!parsed.ip.isLinkLocal) {
      debugPrint(
        '[$context] Ignoring non-link-local address in link-local '
        'ANNOUNCE field: $udpAddress',
      );
      return null;
    }

    return parsed.toAddressString();
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

  bool _hasLiveBlePath(PeerState? peer) =>
      _connectedBleDeviceIdForPeer(peer) != null;

  bool _hasLiveSendPath(PeerState? peer) {
    if (peer == null) return false;
    if (_hasLiveBlePath(peer)) return true;
    return _udpService?.getPeerIdForPubkey(peer.publicKey) != null;
  }

  static String _pubkeyToHex(Uint8List pubkey) =>
      pubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  bool _isAcceptedFriendPubkey(Uint8List pubkey) {
    final hex = _pubkeyToHex(pubkey);
    final peer = _peersState.getPeerByPubkeyHex(hex);
    if (peer?.isFriend == true) return true;
    return store.state.friendships.isFriend(hex);
  }

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
    _drainQueuedMessagesForPeer(pubkey);
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

    final payload = await _noiseSessions.startHandshake(
      transport,
      recipientPubkey,
    );
    if (payload != null) {
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
    }

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
      final ready = await _ensureNoiseSession(
        transport: transport,
        recipientPubkey: recipientPubkey,
        peerId: peerId,
      );
      if (!ready) return null;
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

  Set<String> _candidateAddresses({bool includeLinkLocal = false}) =>
      normalizeAddressStrings([
        ..._publicAddressCandidates,
        if (_publicAddress != null) _publicAddress,
        if (includeLinkLocal && _linkLocalAddress != null) _linkLocalAddress,
      ]);

  Set<String> _connectionLocalCandidates() {
    final udpService = _udpService;
    if (udpService == null) return const {};

    return normalizeAddressStrings(
      _candidateAddresses(includeLinkLocal: true).where((address) {
        final parsed = parseAddressString(address);
        return parsed != null &&
            udpService.activeAddressTypes.contains(parsed.ip.type);
      }),
    );
  }

  Set<String> _udpCandidatesForPeer(
    PeerState? peer, {
    String? fallbackAddress,
  }) =>
      normalizeAddressStrings([
        peer?.linkLocalAddress,
        peer?.udpAddress,
        if (peer != null) ...peer.udpAddressCandidates,
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

  GrassrootsPacket _createSignalingPacket(Uint8List signalingPayload) {
    return GrassrootsPacket(
      type: PacketType.signaling,
      payload: signalingPayload,
    );
  }

  Future<bool> _sendDirectSignalingOverLiveBle(
    Uint8List recipientPubkey,
    Uint8List signalingPayload,
  ) async {
    if (_bleService == null || !_bleAvailable) {
      return false;
    }

    final pubkeyHex =
        recipientPubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
    final peer = store.state.peers.getPeerByPubkeyHex(pubkeyHex);
    final deviceId = _connectedBleDeviceIdForPeer(peer);
    if (deviceId == null) {
      return false;
    }

    final packet = _createSignalingPacket(signalingPayload);
    final bytes = await _packetBytesForTransport(
      packet: packet,
      transport: PeerTransport.bleDirect,
      recipientPubkey: recipientPubkey,
      peerId: deviceId,
    );
    if (bytes == null) return false;
    return _bleService!.sendToPeer(deviceId, bytes);
  }

  Future<void> _sustainHolePunchTraffic(
    String peerHex,
    AddressInfo target, {
    required String phase,
  }) async {
    final punchService = _holePunchServiceFor(target.ip);
    if (punchService == null ||
        _holePunchKeepAliveInProgress.contains(peerHex)) {
      return;
    }

    _holePunchKeepAliveInProgress.add(peerHex);
    debugPrint(
      '[hole-punch] Sustaining punch traffic toward '
      '${target.toAddressString()} during $phase...',
    );
    try {
      await punchService.punch(
        target.ip,
        target.port,
        duration: _holePunchKeepAliveDuration,
      );
    } finally {
      _holePunchKeepAliveInProgress.remove(peerHex);
    }
  }

  void _beginHolePunchAttempt(String peerHex, {bool dispatchStarted = true}) {
    _holePunchTargets.remove(peerHex);
    _holePunchLocalReady.remove(peerHex);
    _holePunchRemoteReady.remove(peerHex);
    _holePunchConnectionInProgress.remove(peerHex);
    _holePunchKeepAliveInProgress.remove(peerHex);
    _holePunchCompleters.putIfAbsent(peerHex, () => Completer<bool>());
    if (dispatchStarted) {
      store.dispatch(HolePunchStartedAction(peerHex));
    }
  }

  void _clearHolePunchState(String peerHex, {bool clearCompleter = false}) {
    _holePunchTargets.remove(peerHex);
    _holePunchLocalReady.remove(peerHex);
    _holePunchRemoteReady.remove(peerHex);
    _holePunchConnectionInProgress.remove(peerHex);
    _holePunchKeepAliveInProgress.remove(peerHex);
    if (clearCompleter) {
      _holePunchCompleters.remove(peerHex);
    }
  }

  void _failHolePunchAttempt(String peerHex, String reason) {
    store.dispatch(HolePunchFailedAction(peerHex, reason));
    final completer = _holePunchCompleters.remove(peerHex);
    if (completer != null && !completer.isCompleted) {
      completer.complete(false);
    }
    _clearHolePunchState(peerHex);
  }

  Future<void> _maybeEstablishPunchConnection(String peerHex) async {
    if (!_holePunchLocalReady.contains(peerHex) ||
        !_holePunchRemoteReady.contains(peerHex)) {
      return;
    }
    if (_holePunchConnectionInProgress.contains(peerHex)) {
      return;
    }

    final target = _holePunchTargets[peerHex];
    if (target == null) {
      debugPrint(
        '[hole-punch] Both sides are ready for $peerHex but no target '
        'address is cached yet.',
      );
      return;
    }

    final myPubkeyHex = identity.publicKey
        .map((b) => b.toRadixString(16).padLeft(2, '0'))
        .join();
    final iAmInitiator = myPubkeyHex.compareTo(peerHex) < 0;
    if (!iAmInitiator) {
      unawaited(
        _sustainHolePunchTraffic(peerHex, target, phase: 'responder-wait'),
      );
      debugPrint(
        '[hole-punch] Both sides are ready; waiting for initiator '
        '$peerHex to connect.',
      );
      return;
    }
    if (_udpService == null) {
      _failHolePunchAttempt(
        peerHex,
        'UDP service unavailable during hole-punch connect',
      );
      return;
    }

    _holePunchConnectionInProgress.add(peerHex);
    debugPrint(
      '[hole-punch] Both sides ready; initiator connecting to '
      '${target.toAddressString()}...',
    );
    unawaited(
      _sustainHolePunchTraffic(peerHex, target, phase: 'initiator-connect'),
    );

    final announce = await _createSignedAnnounce(address: udpAddress);
    final connected = await _sendViaUdp(
      peerHex,
      target.toAddressString(),
      announce,
      allowBleAssistedFallback: false,
      performPreConnectPunch: false,
    );
    _holePunchConnectionInProgress.remove(peerHex);

    if (!connected) {
      _failHolePunchAttempt(
        peerHex,
        'UDX connection failed after both peers reported ready',
      );
    }
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
    bool allowBleAssistedFallback = true,
    bool performPreConnectPunch = true,
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
      allowBleAssistedFallback: allowBleAssistedFallback,
      performPreConnectPunch: performPreConnectPunch,
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
    bool allowBleAssistedFallback = true,
    bool performPreConnectPunch = true,
  }) async {
    if (_udpService == null) return false;

    final connected =
        _udpService!.getPeerIdForPubkey(recipientPubkey) != null ||
            await _ensureUdpConnection(
              pubkeyHex,
              udpAddress,
              isRendezvous: isRendezvous,
              allowBleAssistedFallback: allowBleAssistedFallback,
              performPreConnectPunch: performPreConnectPunch,
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
    bool allowBleAssistedFallback = true,
    bool performPreConnectPunch = true,
  }) async {
    if (_udpService == null) return false;
    if (_udpService!.getPeerIdForPubkey(_hexToBytes(pubkeyHex)) != null) {
      return true;
    }
    final peerShort = pubkeyHex.substring(0, 8);

    // Not connected — check if we should initiate or wait.
    // Only one side should call connectToPeer to avoid UDX simultaneous-open
    // (two independent socket pairs where data flows in only one direction).
    final myPubkeyHex = identity.publicKey
        .map((b) => b.toRadixString(16).padLeft(2, '0'))
        .join();
    final iAmInitiator = myPubkeyHex.compareTo(pubkeyHex) < 0;
    final peer = store.state.peers.getPeerByPubkeyHex(pubkeyHex);

    // After coordinated hole-punching, both sides punched a specific target.
    // Keep the UDX connect on that target instead of re-selecting a different
    // advertised candidate that did not just get its NAT mapping opened.
    final remoteCandidates = performPreConnectPunch
        ? _udpCandidatesForPeer(
            peer,
            fallbackAddress: udpAddress,
          )
        : normalizeAddressStrings([udpAddress]);
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

    if (!isRendezvous && !iAmInitiator) {
      // We're not the initiator — the other side should connect to us.
      // Wait briefly for their incoming connection to arrive.
      debugPrint(
        '[udp-send] Not initiator for $peerShort, waiting for incoming connection...',
      );
      for (var i = 0; i < 10; i++) {
        await Future.delayed(const Duration(milliseconds: 500));
        if (_udpService!.getPeerIdForPubkey(_hexToBytes(pubkeyHex)) != null) {
          debugPrint(
            '[udp-send] Incoming connection arrived for $peerShort',
          );
          return true;
        }
      }
      // Timed out waiting — fall through and try connecting ourselves as last resort
      debugPrint(
        '[udp-send] Timed out waiting for incoming connection from $peerShort, connecting ourselves...',
      );
    }

    final inFlight = _udpConnectInFlight[pubkeyHex];
    if (inFlight != null) {
      debugPrint('[udp-send] Reusing in-flight UDX connect to $peerShort...');
      return inFlight;
    }

    late final Future<bool> udxConnectFuture;
    udxConnectFuture = () async {
      // Hole-punch to open NAT mappings before UDX connection attempt.
      // Skip for peers with publicly routable addresses — punching is wasted
      // when no NAT mapping is needed. If the direct attempt fails, the
      // caller's fallback path will retry with a punch via signaling.
      if (performPreConnectPunch &&
          !isRendezvous &&
          _holePunchServiceFor(addr.ip) != null &&
          peer != null &&
          !peer.hasPublicUdpAddress) {
        debugPrint(
          '[udp-send] Hole-punching to $selectedAddress before connecting...',
        );
        await _holePunchServiceFor(addr.ip)!.punch(addr.ip, addr.port);
      }

      debugPrint('[udp-send] Connecting to $peerShort at $selectedAddress...');
      return _udpService!.connectToPeer(pubkeyHex, addr.ip, addr.port);
    }();
    _udpConnectInFlight[pubkeyHex] = udxConnectFuture;

    bool connected = false;
    try {
      connected = await udxConnectFuture;
    } finally {
      if (identical(_udpConnectInFlight[pubkeyHex], udxConnectFuture)) {
        _udpConnectInFlight.remove(pubkeyHex);
      }
    }

    if (connected) return true;

    debugPrint(
      '[udp-send] UDX connect failed to $peerShort at $selectedAddress',
    );

    if (allowBleAssistedFallback &&
        !isRendezvous &&
        peer != null &&
        peer.isFriend &&
        _hasLiveBlePath(peer)) {
      debugPrint(
        '[udp-send] Trying direct BLE-assisted hole-punch to $peerShort...',
      );
      return _attemptDirectPunchWithPeer(peer, pair);
    }

    return false;
  }

  /// Proactively establish a UDP connection to a friend.
  ///
  /// Called (fire-and-forget) when a friend's ANNOUNCE carries a UDP address
  /// and we don't yet have a live UDP connection to them. This keeps both
  /// transports active so disabling one doesn't lose the peer.
  ///
  /// Sends our own ANNOUNCE as the first message so the remote side learns
  /// our identity and address on the new UDP connection.
  Future<void> _connectToFriendViaUdp(
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
        final announce = await _createSignedAnnounce(address: this.udpAddress);

        // Try link-local first when peer is BLE-nearby (same LAN).
        // Link-local avoids AP client isolation and NAT issues.
        final peer = _peersState.getPeerByPubkeyHex(pubkeyHex);
        final llAddr = peer?.linkLocalAddress;
        if (llAddr != null && _hasLiveBlePath(peer)) {
          debugPrint(
            '[auto-udp] Trying link-local $llAddr for '
            '${pubkeyHex.substring(0, 8)}...',
          );
          final llSuccess = await _sendViaUdp(pubkeyHex, llAddr, announce);
          if (llSuccess) {
            debugPrint(
              '[auto-udp] Connected via link-local to '
              '${pubkeyHex.substring(0, 8)}',
            );
            _autoUdpRetryAfter.remove(pubkeyHex);
            return;
          }
          debugPrint('[auto-udp] Link-local failed, trying global address...');
        }

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
          // Direct connection failed (likely NAT/firewall). Try coordinated
          // hole-punch via a well-connected friend if one is reachable.
          final peer = _peersState.getPeerByPubkeyHex(pubkeyHex);
          if (peer != null && peer.isFriend) {
            if (_hasLiveBlePath(peer)) {
              final pair = _selectUdpCandidatePair(
                _udpCandidatesForPeer(
                  peer,
                  fallbackAddress: normalizedAddress,
                ),
                context: 'auto-udp-direct-punch',
                peerLabel: peer.displayName,
              );
              if (pair != null) {
                debugPrint(
                  '[auto-udp] Direct connect to '
                  '${pubkeyHex.substring(0, 8)} failed, trying direct BLE-assisted hole-punch...',
                );
                if (await _attemptDirectPunchWithPeer(peer, pair)) {
                  _autoUdpRetryAfter.remove(pubkeyHex);
                  return;
                }
              }
            }

            debugPrint(
              '[auto-udp] Direct connect to '
              '${pubkeyHex.substring(0, 8)} failed, trying hole-punch via friends...',
            );
            if (await _discoverPeerViaFriends(peer)) {
              _autoUdpRetryAfter.remove(pubkeyHex);
              return;
            }
          } else {
            debugPrint(
              '[auto-udp] Proactive UDP connection to '
              '${pubkeyHex.substring(0, 8)} failed',
            );
          }
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

  /// Ask a BLE-connected friend to start punching toward us, then punch locally.
  ///
  /// This is a direct friend-to-friend fallback for the case where we already
  /// have a control channel (usually BLE) to the target, but direct UDX to
  /// their advertised UDP address timed out.
  Future<bool> _attemptDirectPunchWithPeer(
    PeerState peer,
    AddressCandidatePair candidatePair,
  ) async {
    final peerHex = peer.pubkeyHex;
    final peerName = peer.displayName;
    final myAddr = candidatePair.local;
    final targetAddr = candidatePair.remote;

    if (_udpService == null || !_udpAvailable) {
      debugPrint(
        '[direct-punch] UDP unavailable, cannot coordinate with $peerName',
      );
      return false;
    }
    if (_holePunchServiceFor(targetAddr.ip) == null) {
      debugPrint(
        '[direct-punch] Hole-punch service unavailable, cannot coordinate with $peerName',
      );
      return false;
    }
    if (!_hasLiveBlePath(peer)) {
      debugPrint(
        '[direct-punch] No live BLE path to $peerName, skipping direct punch',
      );
      return false;
    }

    if (_holePunchCompleters.containsKey(peerHex)) {
      debugPrint(
        '[direct-punch] Reusing in-flight hole-punch attempt for $peerName',
      );
    } else {
      _beginHolePunchAttempt(peerHex);
    }

    debugPrint(
      '[direct-punch] Asking $peerName to punch toward '
      '${myAddr.toAddressString()} '
      'via direct friend signaling...',
    );
    final sent = await _signalingService.requestDirectPunch(
      peer.publicKey,
      requesterPubkey: identity.publicKey,
      requesterIp: myAddr.ip.address,
      requesterPort: myAddr.port,
      requireDirectTransport: true,
    );
    if (!sent) {
      _failHolePunchAttempt(peerHex, 'Could not signal target directly');
      return false;
    }

    await _executePunchInitiate(
      peer.publicKey,
      targetAddr.ip.address,
      targetAddr.port,
      readyRecipientPubkey: peer.publicKey,
    );

    final completer = _holePunchCompleters[peerHex];
    if (completer == null) return false;

    final succeeded = await completer.future.timeout(
      const Duration(seconds: 15),
      onTimeout: () {
        debugPrint(
          '[direct-punch] Timed out waiting for UDP connection to $peerName',
        );
        _failHolePunchAttempt(
          peerHex,
          'Timed out waiting for direct punch connection',
        );
        return false;
      },
    );

    if (succeeded) {
      debugPrint('[direct-punch] Established UDP path to $peerName');
    }
    return succeeded;
  }

  /// Execute the local side of a PUNCH_INITIATE instruction.
  Future<void> _executePunchInitiate(
    Uint8List peerPubkey,
    String ip,
    int port, {
    Uint8List? readyRecipientPubkey,
  }) async {
    final peerHex =
        peerPubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
    final peerShort = peerHex.substring(0, 8);
    final hasPendingSend = _holePunchCompleters.containsKey(peerHex);

    // Idempotency: a duplicate PUNCH_INITIATE arrives whenever both peers
    // independently requested punches for the same pair. If we're already
    // handling a punch toward the same target, skip — another round would
    // just produce redundant packets and duplicate PUNCH_READY messages.
    final incomingIp = InternetAddress.tryParse(ip);
    final existing = _holePunchTargets[peerHex];
    if (existing != null &&
        incomingIp != null &&
        existing.ip.address == incomingIp.address &&
        existing.port == port &&
        (_holePunchLocalReady.contains(peerHex) ||
            _holePunchConnectionInProgress.contains(peerHex))) {
      debugPrint(
        '[hole-punch] Ignoring duplicate PUNCH_INITIATE for '
        '$peerShort at $ip:$port — punch already in progress '
        '(pendingSend=$hasPendingSend)',
      );
      return;
    }

    debugPrint(
      '[hole-punch] PUNCH_INITIATE received: '
      'target=$peerShort at $ip:$port, '
      'pendingSend=$hasPendingSend',
    );

    store.dispatch(HolePunchPunchingAction(peerHex));

    final targetIp = incomingIp;
    if (targetIp == null) {
      debugPrint('[hole-punch] Invalid address in punch initiate: $ip:$port');
      _failHolePunchAttempt(peerHex, 'Invalid punch target address');
      return;
    }
    final targetAddress = AddressInfo(targetIp, port);
    if (_udpService == null ||
        _connectionService.selectBestPairFromAddresses(
              localAddresses: _connectionLocalCandidates(),
              remoteAddresses: [targetAddress.toAddressString()],
            ) ==
            null) {
      debugPrint(
        '[hole-punch] Unsupported address family in punch initiate: '
        '$ip:$port. No local advertised candidate can use '
        '${targetIp.type == InternetAddressType.IPv6 ? "IPv6" : "IPv4"}.',
      );
      _failHolePunchAttempt(peerHex, 'Unsupported address family');
      return;
    }

    final punchService = _holePunchServiceFor(targetIp);
    if (punchService == null) {
      debugPrint('[hole-punch] Hole-punch service unavailable');
      _failHolePunchAttempt(peerHex, 'Hole-punch service unavailable');
      return;
    }

    _holePunchTargets[peerHex] = targetAddress;

    // Send punch packets to open NAT mappings on both sides.
    debugPrint('[hole-punch] Sending punch packets to $ip:$port...');
    await punchService.punch(targetIp, port);
    debugPrint('[hole-punch] Punch packets sent.');

    _holePunchLocalReady.add(peerHex);

    if (readyRecipientPubkey != null) {
      final readyRecipientHex = readyRecipientPubkey
          .map((b) => b.toRadixString(16).padLeft(2, '0'))
          .join();
      debugPrint(
        '[hole-punch] Reporting local punch readiness for '
        '$peerShort...',
      );
      final readySent = await _signalingService.sendPunchReady(
        readyRecipientPubkey,
        identity.publicKey,
        requireDirectTransport: readyRecipientHex == peerHex,
      );
      if (!readySent) {
        _failHolePunchAttempt(
          peerHex,
          'Could not deliver PUNCH_READY to the remote coordinator',
        );
        return;
      }
    }

    // After punching, only the INITIATOR (smaller pubkey) establishes the
    // UDX connection, and only after the other side explicitly confirms
    // readiness via PUNCH_READY.
    final myPubkeyHex = identity.publicKey
        .map((b) => b.toRadixString(16).padLeft(2, '0'))
        .join();
    final iAmInitiator = myPubkeyHex.compareTo(peerHex) < 0;

    if (iAmInitiator) {
      debugPrint(
        '[hole-punch] Local punch complete for $peerShort; waiting for '
        'explicit PUNCH_READY before connecting...',
      );
      await _maybeEstablishPunchConnection(peerHex);
    } else if (!iAmInitiator) {
      // We punched but we're not the initiator — wait for incoming UDX connection.
      debugPrint('[hole-punch] Punched, waiting for initiator to connect...');
    }
  }

  /// Try to reach a peer through available rendezvous and FoF facilitators.
  ///
  /// Reconnected common friends receive explicit mediation requests. Public
  /// rendezvous servers receive RECONNECT and match it against the peer's
  /// AVAILABLE. We then wait for the coordinated punch to complete.
  ///
  /// Returns true if a UDP path to the peer was established.
  Future<bool> _discoverPeerViaFriends(PeerState peer) async {
    final pubkeyBytes = peer.publicKey;
    final pubkeyHex = peer.pubkeyHex;
    final name = peer.displayName;
    final mediators = store.state.peers.mediatorsForFriend(pubkeyHex);
    final directMediatorCount =
        mediators.where((mediator) => !mediator.isWellConnected).length;
    final trustedFriendCount = store.state.peers.wellConnectedFriends
        .where(
          (friend) =>
              store.state.peers.friendsOfFriends[friend.pubkeyHex]?.contains(
                pubkeyHex,
              ) ==
              true,
        )
        .length;
    final rendezvousCount = _configuredRendezvousServers().length;
    final facilitatorCount =
        directMediatorCount + trustedFriendCount + rendezvousCount;

    if (facilitatorCount == 0) {
      debugPrint('[discover] No signaling facilitators available');
      return false;
    }

    _beginHolePunchAttempt(pubkeyHex);

    var directMediatorSent = 0;
    for (final mediator in mediators) {
      if (mediator.isWellConnected) continue;
      final ok = await _signalingService.requestFriendMediation(
        mediatorPubkey: mediator.publicKey,
        targetPubkey: pubkeyBytes,
        initiatorPubkey: identity.publicKey,
      );
      if (ok) directMediatorSent++;
    }

    final sent = await _signalingService.fanOutReconnect(
      pubkeyBytes,
      initiatorPubkey: identity.publicKey,
    );
    if (sent == 0 && directMediatorSent == 0) {
      debugPrint('[discover] Could not reach any facilitator for $name');
      _failHolePunchAttempt(pubkeyHex, 'Could not reach any facilitator');
      return false;
    }

    final completer = _holePunchCompleters[pubkeyHex];
    if (completer == null) {
      debugPrint('[discover] Hole-punch state vanished for $name');
      return false;
    }

    debugPrint(
      '[discover] Mediation requested for $name '
      '(${directMediatorSent + sent} facilitator(s)); '
      'waiting for PUNCH_INITIATE (timeout: 15s)...',
    );

    final succeeded = await completer.future.timeout(
      const Duration(seconds: 15),
      onTimeout: () {
        debugPrint('[discover] Hole-punch timed out for $name');
        _failHolePunchAttempt(
          pubkeyHex,
          'Timed out waiting for coordinated hole-punch',
        );
        return false;
      },
    );

    if (succeeded) {
      debugPrint('[discover] Successfully established path to $name');
    }
    return succeeded;
  }

  // ===== Internal setup =====

  /// Periodically try to discover unreachable friends via rendezvous/FoF
  /// facilitators.
  ///
  /// On each announce tick, find friends that we know about but can't currently
  /// reach via any transport. Common reconnected friends are asked to mediate
  /// directly. Configured rendezvous servers still use the bootstrap
  /// RECONNECT/AVAILABLE match.
  ///
  /// Throttled: each peer is attempted at most once per [_discoveryRetryInterval].
  void _discoverUnreachableFriends() {
    // debugPrint("Discovering unreachable friends");
    if (!_udpAvailable) {
      // debugPrint("No UDP");
      return; // Need UDP to establish the connection
    }

    final rendezvousCount = _configuredRendezvousServers().length;

    final now = DateTime.now();
    final friends = _peersState.friends;

    // var len = friends.length;
    // debugPrint("Have $len friends");

    for (final friend in friends) {
      // Skip friends we can already reach
      if (_hasLiveBlePath(friend)) {
        // debugPrint("Have live BLE path to friend");
        continue;
      }
      if (_udpService?.getPeerIdForPubkey(friend.publicKey) != null) {
        // debugPrint("Have peer Id for public key in udp service");
        continue;
      }

      // Skip if we attempted discovery recently
      final lastAttempt = _lastDiscoveryAttempt[friend.pubkeyHex];
      if (lastAttempt != null &&
          now.difference(lastAttempt) < _discoveryRetryInterval) {
        // debugPrint("Skip because tried recently");
        continue;
      }

      final mediators =
          store.state.peers.mediatorsForFriend(friend.pubkeyHex).toList();
      final directMediatorCount =
          mediators.where((mediator) => !mediator.isWellConnected).length;
      final wellConnectedCount = store.state.peers.wellConnectedFriends
          .where(
            (wc) =>
                store.state.peers.friendsOfFriends[wc.pubkeyHex]?.contains(
                  friend.pubkeyHex,
                ) ==
                true,
          )
          .length;
      if (directMediatorCount == 0 &&
          wellConnectedCount == 0 &&
          rendezvousCount == 0) {
        continue;
      }

      // Skip if this friend IS one of our well-connected friends (they're
      // reachable — that's how we'd signal through them)
      // if (wellConnected.any((wc) => wc.pubkeyHex == friend.pubkeyHex)) {
      //   debugPrint("Skip because it's well connected");
      //   continue;
      // }

      debugPrint(
        '[discover] Friend ${friend.displayName} is unreachable, '
        'trying discovery via '
        '${directMediatorCount + wellConnectedCount + rendezvousCount} '
        'facilitator(s)...',
      );
      _lastDiscoveryAttempt[friend.pubkeyHex] = now;

      // Fire-and-forget — don't block the announce tick
      _discoverPeerViaFriends(friend).then((success) {
        if (success) {
          debugPrint(
            '[discover] Successfully reached ${friend.displayName} via friends',
          );
          _lastDiscoveryAttempt.remove(friend.pubkeyHex);
        } else {
          debugPrint(
            '[discover] Discovery failed for ${friend.displayName}, '
            'will retry in ${_discoveryRetryInterval.inSeconds}s',
          );
        }
      });
    }
  }

  /// Set up MessageRouter callbacks to dispatch to Redux and application layer
  void _setupRouterCallbacks() {
    // Message received from any transport
    _messageRouter.onMessageReceived =
        (messageId, senderPubkey, payload, arrivalTransport) {
      // Authoritative arrival transport from the receive path (not inferred
      // from the peer's preferred/active transport).
      final transport = arrivalTransport == PeerTransport.udp
          ? MessageTransport.udp
          : MessageTransport.ble;

      store.dispatch(
        MessageReceivedAction(
          messageId: messageId,
          transport: transport,
          senderPubkey: senderPubkey,
          payloadSize: payload.length,
        ),
      );
      onMessageReceived?.call(messageId, senderPubkey, payload, transport);
      onReceive?.call(senderPubkey, payload);
    };

    // ACK received (UDP delivery confirmation)
    _messageRouter.onAckReceived = (messageId) {
      debugPrint('ACK received for message $messageId');
      store.dispatch(MessageDeliveredAction(messageId: messageId));
      _clearAckPending(messageId);
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
        final result = await _noiseSessions.handleHandshakePacket(
          packet,
          transport: transport,
          peerId: peerId,
        );
        final response = result.responsePayload;
        if (response != null) {
          await _sendNoiseHandshakePacket(
            transport: transport,
            recipientPubkey: result.remotePubkey,
            peerId: peerId,
            payload: response,
          );
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
        }
      } catch (e) {
        debugPrint('[noise] Dropping handshake packet: $e');
      }
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
      if (_isRendezvousPubkeyHex(pubkeyHex)) {
        _completeRendezvousResponseWaiters(pubkeyHex);
      }

      // When we are well-connected and receive an ANNOUNCE from a friend
      // with a UDP address, register it in our address table. This is used
      // by the direct-punch path ([requestDirectPunch]) when we want a
      // friend already reachable over BLE to start punching toward us.
      //
      // Only friends are registered.
      //
      // UDP: use the observed address (NAT-translated, most reliable).
      // BLE: use the claimed address from the ANNOUNCE payload (no observed
      //      address available over BLE, but it's the only option — and for
      //      peers with public UDP reachability, the claimed address is correct).
      final announcedCandidates = normalizeAddressStrings([
        ...data.addressCandidates,
        data.udpAddress,
        data.linkLocalAddress,
      ]);
      if (store.state.transports.isWellConnected &&
          announcedCandidates.isNotEmpty) {
        final senderPeer = store.state.peers.getPeerByPubkeyHex(pubkeyHex);
        if (senderPeer != null && senderPeer.isFriend) {
          if (transport == PeerTransport.udp && _udpService != null) {
            final remote = _udpService!.getRemoteAddress(pubkeyHex);
            _signalingService.processAnnounceFromFriend(
              data.publicKey,
              claimedAddress: data.udpAddress,
              claimedAddresses: announcedCandidates,
              observedIp: remote?.ip.address,
              observedPort: remote?.port,
            );
          } else if (transport == PeerTransport.bleDirect) {
            _signalingService.processAnnounceFromFriend(
              data.publicKey,
              claimedAddress: data.udpAddress,
              claimedAddresses: announcedCandidates,
              // No observed address over BLE — claimed address only.
            );
          }
        }
      } else {
        // debugPrint(
        //     'Either we are not well connected, or peer has null udpAddress, this is expected');
      }

      if (transport == PeerTransport.bleDirect) {
        final senderPeer = store.state.peers.getPeerByPubkeyHex(pubkeyHex);
        if (senderPeer != null && senderPeer.isFriend) {
          _sendFriendAnnounceToConnectedBlePaths(senderPeer);
        }
      }

      final announcedPeer = store.state.peers.getPeerByPubkeyHex(pubkeyHex);
      if (announcedPeer != null && !_isRendezvousPubkeyHex(pubkeyHex)) {
        unawaited(_startNoiseHandshakeForPeer(
          transport: transport,
          peer: announcedPeer,
          peerId: udpPeerId,
        ));
      }

      // Proactive UDP connect: when a friend's ANNOUNCE arrives with a UDP
      // address (from any transport, including BLE), establish a UDP connection
      // so both transports are active simultaneously. This ensures disabling
      // BLE doesn't kill the peer — UDP keeps it alive.
      //
      // IMPORTANT: Only ONE side should initiate the connection to avoid
      // simultaneous-open issues in UDX (two independent socket pairs that
      // don't share streams, causing one-directional data flow). The device
      // with the lexicographically smaller pubkey initiates.
      if (announcedCandidates.isNotEmpty &&
          _udpService != null &&
          _udpAvailable) {
        debugPrint('[auto-udp] proactive UDP connect to $pubkeyHex');
        final senderPeer = store.state.peers.getPeerByPubkeyHex(pubkeyHex);
        if (senderPeer != null &&
            senderPeer.isFriend &&
            _udpService!.getPeerIdForPubkey(data.publicKey) == null) {
          debugPrint(
            '[auto-udp] proactive UDP connect to $pubkeyHex who is actually ${senderPeer.nickname}',
          );
          // Deterministic initiator: the peer with the smaller pubkey hex
          // initiates the connection. The other side waits for the incoming
          // connection to arrive via the multiplexer.
          final myPubkeyHex = identity.publicKey
              .map((b) => b.toRadixString(16).padLeft(2, '0'))
              .join();
          final iAmInitiator = myPubkeyHex.compareTo(pubkeyHex) < 0;

          if (iAmInitiator) {
            debugPrint(
              '[auto-udp] Friend ${data.nickname} has UDP address '
              '$announcedCandidates, connecting proactively (I am initiator)...',
            );
            _connectToFriendViaUdp(
              pubkeyHex,
              data.udpAddress ?? announcedCandidates.first,
            );
          } else {
            debugPrint(
              '[auto-udp] Friend ${data.nickname} has UDP address '
              '$announcedCandidates, waiting for them to connect (they are initiator)',
            );
          }
        }
      }

      // onPeerDiscovered is the identity-learned event: a peer's public key and
      // nickname arrive in the signed ANNOUNCE, which is sent in the clear and
      // precedes the Noise session. Because onPeerConnected is gated on an
      // authenticated Noise session (see processReachabilityTransitions),
      // discovery strictly precedes connection — so we surface it here, once per
      // identity, rather than coupling it to the reachability transition. The
      // rendezvous server is infrastructure, not a discoverable peer.
      if (!_isRendezvousPubkeyHex(pubkeyHex) &&
          _discoveredPubkeyHexes.add(pubkeyHex)) {
        onPeerDiscovered?.call(data.publicKey, data.nickname);
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

    // Signaling packet received — delegate to SignalingService.
    _messageRouter.onSignalingReceived =
        (senderPubkey, payload, {observedIp, observedPort}) {
      _signalingService.processSignaling(
        senderPubkey,
        payload,
        observedIp: observedIp,
        observedPort: observedPort,
      );
    };
  }

  /// Set up SignalingService callbacks
  void _setupSignalingCallbacks() {
    // SignalingService sends signaling payloads through us (wrapped in GrassrootsPacket)
    _signalingService.sendSignaling =
        (recipientPubkey, signalingPayload) async {
      final packet = _createSignalingPacket(signalingPayload);

      final pubkeyHex = _pubkeyToHex(recipientPubkey);
      final isRendezvous = _isRendezvousPubkeyHex(pubkeyHex);
      final rendezvousConfig =
          isRendezvous ? _configuredRendezvousForPubkeyHex(pubkeyHex) : null;

      // Rendezvous anchors are UDP-only and reached via a configured address.
      // They share the wire encryption (Noise-wrapped `secureSignaling`) with
      // peer-to-peer signaling — the only specialisation here is the
      // RV-aware connection-management path (`_syncConfiguredRendezvous`),
      // which knows about per-RV backoff and the ANNOUNCE primer.
      if (isRendezvous) {
        if (_udpService == null || !_udpAvailable) return false;
        final synced = await _syncConfiguredRendezvous(
          config: rendezvousConfig,
          reason: 'signaling-primer',
        );
        if (!synced) return false;
        final udpAddr = rendezvousConfig?.address;
        if (udpAddr == null || udpAddr.isEmpty) return false;
        return _sendPacketViaUdp(
          pubkeyHex: pubkeyHex,
          udpAddress: udpAddr,
          packet: packet,
          recipientPubkey: recipientPubkey,
          isRendezvous: true,
        );
      }

      // Try BLE first
      if (_bleService != null && _bleAvailable) {
        final peerId = _bleService!.getPeerIdForPubkey(recipientPubkey);
        if (peerId != null) {
          final bytes = await _packetBytesForTransport(
            packet: packet,
            transport: PeerTransport.bleDirect,
            recipientPubkey: recipientPubkey,
            peerId: peerId,
          );
          if (bytes != null && await _bleService!.sendToPeer(peerId, bytes)) {
            return true;
          }
        }
      }

      // Fall back to UDP
      if (_udpService != null && _udpAvailable) {
        if (_udpService!.getPeerIdForPubkey(recipientPubkey) != null) {
          final bytes = await _packetBytesForTransport(
            packet: packet,
            transport: PeerTransport.udp,
            recipientPubkey: recipientPubkey,
            peerId: pubkeyHex,
          );
          if (bytes != null &&
              await _udpService!.sendToPeer(pubkeyHex, bytes)) {
            return true;
          }
        }

        // Not connected via UDP yet — try connect-on-demand
        final peer = store.state.peers.getPeerByPubkeyHex(pubkeyHex);
        final candidates = _udpCandidatesForPeer(
          peer,
          fallbackAddress: rendezvousConfig?.address,
        );
        final udpAddr = peer?.udpAddress ??
            rendezvousConfig?.address ??
            (candidates.isNotEmpty ? candidates.first : null);
        if (udpAddr != null && udpAddr.isNotEmpty) {
          return _sendPacketViaUdp(
            pubkeyHex: pubkeyHex,
            udpAddress: udpAddr,
            packet: packet,
            recipientPubkey: recipientPubkey,
            isRendezvous: isRendezvous,
          );
        }
      }

      return false;
    };
    _signalingService.sendDirectSignaling = _sendDirectSignalingOverLiveBle;

    // Address-aware send: target an RV at an explicit ip:port even if we
    // haven't otherwise registered or connected to it. Used by AVAILABLE
    // fan-out for RVs we learned about from a friend (RV_LIST). The recipient
    // is always an RV here — always go through the Noise-encrypted
    // `_sendPacketViaUdp` path; the anchor only accepts secureSignaling.
    _signalingService.sendSignalingToAddress =
        (recipientPubkey, address, signalingPayload) async {
      if (_udpService == null || !_udpAvailable) return false;
      final packet = _createSignalingPacket(signalingPayload);
      final pubkeyHex = _pubkeyToHex(recipientPubkey);
      return _sendPacketViaUdp(
        pubkeyHex: pubkeyHex,
        udpAddress: address,
        packet: packet,
        recipientPubkey: recipientPubkey,
        isRendezvous: true,
      );
    };

    // A rendezvous server forwarded a redeemed cold-call invite: validate it
    // against our locally-recorded outstanding invites, register the
    // redeemer's identity, and surface it (spec §IP Cold-Call — everything
    // beyond this is GLP-level).
    _signalingService.onInviteRedeemed = (rvPubkey, inviteId, redeemerPubkey) {
      final inviteIdHex = _pubkeyToHex(inviteId);
      final shortId = inviteIdHex.substring(0, 8);

      // Acknowledge to the sender so it stops retrying, regardless of whether
      // this notification is actionable here (unknown/duplicate) — the sender
      // already cleared SignalingService's trust gate, and the ack carries no
      // secret. Without this, a lost first ack would loop the RV's retries.
      unawaited(_signalingService.sendSignaling?.call(
        rvPubkey,
        const SignalingCodec().encode(
          InviteRedeemedAckMessage(inviteId: inviteId),
        ),
      ));

      final outstanding = _outstandingInvites[inviteIdHex];
      if (outstanding == null) {
        debugPrint(
          '[peer-link] redeemed notification for unknown/already-handled '
          'invite $shortId... (acked)',
        );
        return;
      }
      final rvHex = _pubkeyToHex(rvPubkey);
      if (rvHex != outstanding.rvPubkeyHex) {
        debugPrint(
          '[peer-link] dropping redeemed notification for $shortId...: '
          'sender is not the rendezvous server the invite was registered with',
        );
        return;
      }
      // Single-use locally, mirroring the server-side atomic flip. Expiry is
      // the rendezvous server's call (it validated the redemption against the
      // signed ExpiresAt) — we do not second-guess it against our own clock,
      // which could differ.
      _outstandingInvites.remove(inviteIdHex);
      store.dispatch(
        PeerIdentityRegisteredAction(publicKey: redeemerPubkey),
      );
      debugPrint('[peer-link] invite $shortId... redeemed by '
          '${_pubkeyToHex(redeemerPubkey).substring(0, 8)}...');
      onPeerLinkRedeemed?.call(redeemerPubkey);
    };

    // Hole-punch initiation: a well-connected friend told us to start punching
    _signalingService.onPunchInitiate =
        (peerPubkey, ip, port, readyRecipientPubkey) async {
      await _executePunchInitiate(
        peerPubkey,
        ip,
        port,
        readyRecipientPubkey: readyRecipientPubkey,
      );
    };

    _signalingService.onPunchReady = (peerPubkey) async {
      final peerHex =
          peerPubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
      _holePunchRemoteReady.add(peerHex);
      await _maybeEstablishPunchConnection(peerHex);
    };

    // Address reflection: a well-connected friend told us our real public address.
    // This replaces the HTTP-discovered IP + guessed port with the actual
    // NAT-translated address the friend observed — correct external port included.
    // The corrected address will be broadcast to all friends on the next
    // periodic ANNOUNCE cycle.
    _signalingService.onAddrReflected = (senderPubkey, ip, port) {
      // TODO: why is server ANONUNNCE needed? why need to block verifyRendezvousServerResponds?
      // The GLP-spec response to a registration ANNOUNCE is an addrReflect.
      // If the sender is a configured (or in-flight) rendezvous server,
      // treat it as authoritative proof of the round-trip and unblock
      // _verifyRendezvousServerResponds. The server's separate ANNOUNCE-back
      // is informational — relying on it is fragile because that send path
      // is fire-and-forget and can be dropped on stream tear-down.
      final senderHex =
          senderPubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
      if (_isRendezvousPubkeyHex(senderHex)) {
        debugPrint(
          '[rendezvous] Server ${senderHex.substring(0, 8)}... acknowledged '
          'via addrReflect',
        );
        _completeRendezvousResponseWaiters(senderHex);
      }

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
    };
  }

  /// Diff each peer's set of reachable transports against the previous store
  /// tick and fire the per-transport `onPeerConnected` / `onPeerDisconnected`
  /// callbacks for every medium that came up or went down. Delegates to the
  /// testable top-level [processReachabilityTransitions] function so the diff
  /// logic is unit-testable without a full `GrassrootsNetwork` harness.
  void _processReachabilityTransitions(PeersState peersState) {
    processReachabilityTransitions(
      peersState: peersState,
      lastKnownTransports: _lastKnownTransports,
      onConnected: onPeerConnected,
      onDisconnected: onPeerDisconnected,
    );
  }

  /// Set up callbacks for BLE transport service
  void _setupBleServiceCallbacks() {
    if (_bleService == null) return;

    // Forward BLE packets to the MessageRouter for processing
    _bleService!.onBlePacketReceived =
        (packet, {String? bleDeviceId, int? rssi, BleRole? bleRole}) {
      _messageRouter.processPacket(
        packet,
        transport: PeerTransport.bleDirect,
        bleDeviceId: bleDeviceId,
        bleRole: bleRole,
        rssi: rssi,
      );
    };

    _messageRouter.shouldAcceptBleAnnounce =
        (senderPubkey, {String? bleDeviceId, BleRole? bleRole}) {
      if (store.state.settings.coldCallTrustLevel == ColdCallTrustLevel.open) {
        return true;
      }
      return _isAcceptedFriendPubkey(senderPubkey);
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
        _bleFriendAnnounceSent.remove(event.peerId);
        _requeueMessagesOnBleDisconnect(event.peerId);
      }
    });
  }

  /// Set up callbacks for UDP transport service
  void _setupUdpServiceCallbacks() {
    if (_udpService == null) return;

    // Forward UDP data to the MessageRouter for processing
    _udpService!.onUdpDataReceived = (peerId, data) {
      try {
        final packet = GrassrootsPacket.deserialize(data);
        // Observed source address: the UDX remote, if known. Used by the
        // signaling matcher to learn cold-call senders' public addresses.
        final remote = _udpService!.getRemoteAddress(peerId);
        _messageRouter.processPacket(
          packet,
          transport: PeerTransport.udp,
          udpPeerId: peerId,
          observedIp: remote?.ip.address,
          observedPort: remote?.port,
        );
      } catch (e) {
        debugPrint('Failed to deserialize UDP packet from $peerId: $e');
      }
    };

    // Listen to connection events — update Redux state and log
    _udpService!.connectionStream.listen((event) {
      if (event.connected) {
        debugPrint('UDP peer connected: ${event.peerId}');
        store.dispatch(PeerUdpSeenAction(_hexToBytes(event.peerId)));
        // The friend is reachable again — reset the AVAILABLE re-fire clock so
        // the next disconnect triggers a fresh fan-out immediately rather than
        // waiting out the previous-cycle backoff.
        _availableFanOutLastFiredAt.remove(event.peerId);
        _sendRvListToFriendIfEligible(event.peerId);
        _sendFriendListToFriendIfEligible(event.peerId);
        _mediateCommonFriendsFor(event.peerId);

        final wasPunching = _holePunchTargets.containsKey(event.peerId) ||
            _holePunchLocalReady.contains(event.peerId) ||
            _holePunchRemoteReady.contains(event.peerId);
        final completer = _holePunchCompleters.remove(event.peerId);
        if (wasPunching) {
          final remote = _udpService!.getRemoteAddress(event.peerId);
          if (remote != null) {
            store.dispatch(
              HolePunchSucceededAction(
                event.peerId,
                remote.ip.address,
                remote.port,
              ),
            );
          } else {
            final peer = _peersState.getPeerByPubkeyHex(event.peerId);
            final parsed = peer?.udpAddress != null
                ? parseAddressString(peer!.udpAddress!)
                : null;
            if (parsed != null) {
              store.dispatch(
                HolePunchSucceededAction(
                  event.peerId,
                  parsed.ip.address,
                  parsed.port,
                ),
              );
            }
          }
        } else {
          // Connection succeeded with no prior hole-punch coordination.
          //
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
        if (completer != null && !completer.isCompleted) {
          completer.complete(true);
        }
        _clearHolePunchState(event.peerId);
      } else {
        debugPrint('UDP peer disconnected: ${event.peerId}');
        final hadPendingPunch =
            _holePunchCompleters.containsKey(event.peerId) ||
                _holePunchTargets.containsKey(event.peerId) ||
                _holePunchLocalReady.contains(event.peerId) ||
                _holePunchRemoteReady.contains(event.peerId);
        if (hadPendingPunch) {
          _failHolePunchAttempt(
            event.peerId,
            event.reason ?? 'UDP disconnected during hole-punch',
          );
        } else {
          _clearHolePunchState(event.peerId);
        }
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

  /// Fired when a UDP stream ended or the ANNOUNCE keepalive timed out.
  ///
  /// Per the rendezvous reconnection algorithm, when we detect a friend went
  /// silent we should fan out AVAILABLE to our trusted facilitators. The peer
  /// (which presumably had its IP change) will send RECONNECT, the facilitator
  /// will match the pair, and a coordinated hole-punch follows.
  /// Send RV_LIST to a friend right after a UDP connection establishes,
  /// so they can target AVAILABLE at our exact rendezvous servers when our
  /// IP changes.
  void _sendRvListToFriendIfEligible(String pubkeyHex) {
    final peer = _peersState.getPeerByPubkeyHex(pubkeyHex);
    if (peer == null || !peer.isFriend) return;

    final rvServers = _ownRvServerEntries();
    if (rvServers.isEmpty) return;

    debugPrint(
      '[rv-list] Sending ${rvServers.length} rendezvous server entr(y/ies) to '
      '${peer.displayName}',
    );
    unawaited(_signalingService.sendRvList(peer.publicKey, rvServers));
  }

  /// Broadcast our current RV list to every UDP-reachable friend. Called
  /// when the local rendezvous server settings change.
  void _broadcastRvListToFriends() {
    final rvServers = _ownRvServerEntries();
    if (rvServers.isEmpty || _udpService == null) return;
    for (final friend in _peersState.friends) {
      if (_udpService!.getPeerIdForPubkey(friend.publicKey) == null) continue;
      debugPrint(
        '[rv-list] Re-broadcasting RV list to ${friend.displayName} '
        '(settings changed)',
      );
      unawaited(_signalingService.sendRvList(friend.publicKey, rvServers));
    }
  }

  /// Send our current accepted friend set to a friend after a live connection
  /// establishes so they can maintain their friends-of-friends map.
  void _sendFriendListToFriendIfEligible(String pubkeyHex) {
    final peer = _peersState.getPeerByPubkeyHex(pubkeyHex);
    if (peer == null || !peer.isFriend) return;

    final friendPubkeys = _ownFriendListEntries();
    debugPrint(
      '[fof] Sending ${friendPubkeys.length} accepted friend(s) to '
      '${peer.displayName}',
    );
    unawaited(_signalingService.sendFriendList(peer.publicKey, friendPubkeys));
  }

  /// Broadcast our current accepted friend set to every live friend.
  void _broadcastFriendListToFriends({required String reason}) {
    final friendPubkeys = _ownFriendListEntries();
    for (final friend in _peersState.friends) {
      if (!friend.isReachable) continue;
      debugPrint(
        '[fof] Broadcasting friend list to ${friend.displayName} ($reason)',
      );
      unawaited(
        _signalingService.sendFriendList(friend.publicKey, friendPubkeys),
      );
    }
  }

  List<Uint8List> _ownFriendListEntries() {
    final friends = _peersState.friendPubkeyHexes.toList()..sort();
    return [for (final hex in friends) _hexToBytes(hex)];
  }

  /// Proactively mediate between a reconnected friend and all common friends.
  ///
  /// This implements the bootstrap-friend step from the GLP friends-based
  /// rendezvous algorithm: when B reconnects to A, B consults its
  /// friends-of-friends map for A and immediately mediates for every common
  /// friend C that is currently live with B.
  void _mediateCommonFriendsFor(String reconnectedFriendHex) {
    final requester = _peersState.getPeerByPubkeyHex(reconnectedFriendHex);
    if (requester == null || !requester.isFriend) return;

    final commonFriendHexes = _peersState
        .commonFriendHexesWith(reconnectedFriendHex)
        .toList()
      ..sort();
    for (final commonHex in commonFriendHexes) {
      if (commonHex == reconnectedFriendHex) continue;
      final common = _peersState.getPeerByPubkeyHex(commonHex);
      if (common == null || !common.isFriend || !common.isReachable) {
        continue;
      }
      debugPrint(
        '[fof] Proactively mediating ${requester.displayName} <-> '
        '${common.displayName}',
      );
      _signalingService.mediateFriends(
        requesterPubkey: requester.publicKey,
        targetPubkey: common.publicKey,
      );
    }
  }

  List<RvServerEntry> _ownRvServerEntries() {
    return [
      for (final config in _configuredRendezvousServers())
        RvServerEntry(pubkey: config.pubkey, address: config.address),
    ];
  }

  void _onUdpPeerDisconnected(PeerState peer) {
    _noiseSessions.reset(PeerTransport.udp, peer.publicKey);
    if (!peer.isFriend) return;

    // Overall reachability is tracked by the reachability subscriber on the
    // store, which fires the consolidated onPeerDisconnected when the last
    // live transport drops. This handler only owns UDP-specific recovery —
    // fanning out AVAILABLE so signaling mediators can help re-establish.

    final facilitatorCount = store.state.peers.wellConnectedFriends.length +
        _configuredRendezvousServers().length +
        store.state.friendships.friendRvServers.length;
    if (facilitatorCount == 0) return;

    debugPrint(
      '[reconnect] UDP path to ${peer.displayName} dropped — fanning out AVAILABLE',
    );
    _availableFanOutLastFiredAt[peer.pubkeyHex] = DateTime.now();
    unawaited(_signalingService.fanOutAvailable(peer.publicKey));
  }

  /// Periodically re-fire AVAILABLE for friends that remain UDP-unreachable.
  ///
  /// The friend's RV holds AVAILABLE entries for a finite expiry window
  /// (~5 minutes). If the friend takes longer than that window to deliver
  /// their matching RECONNECT (e.g. their client is stalled on a stale-cache
  /// false-positive direct dial), the RV will have dropped our AVAILABLE and
  /// no pairing can complete. Re-firing every cycle keeps the pairing window
  /// open until the friend is reachable or we give up the path.
  void _refireAvailableForUnreachableFriends({bool force = false}) {
    if (_udpService == null || !_udpAvailable) return;
    final now = DateTime.now();
    for (final friend in _peersState.friends) {
      // Skip friends we already have a live UDP connection to.
      if (_udpService!.getPeerIdForPubkey(friend.publicKey) != null) continue;

      final last = _availableFanOutLastFiredAt[friend.pubkeyHex];
      if (!force &&
          last != null &&
          now.difference(last) < _availableRefireInterval) {
        continue;
      }
      debugPrint(
        '[reconnect] Re-firing AVAILABLE for ${friend.displayName} '
        '(UDP-unreachable for ${last == null ? "unknown" : "${now.difference(last).inSeconds}s"}${force ? ", forced" : ""})',
      );
      _availableFanOutLastFiredAt[friend.pubkeyHex] = now;
      unawaited(_signalingService.fanOutAvailable(friend.publicKey));
    }
  }

  /// Called by the host app when it returns to the foreground.
  ///
  /// Three steps, in order:
  ///   1. Probe the raw UDP sockets and rebind the transport if the OS
  ///      poisoned them while we were backgrounded (Android in particular
  ///      EPERMs background sends and leaves the socket FD in a permanently
  ///      broken state — fixable only by close + bind).
  ///   2. Re-sync configured rendezvous servers (existing UDX sessions may
  ///      be talking to a stale CID, or the rebind in step 1 just dropped
  ///      every session).
  ///   3. Force-refire AVAILABLE for unreachable friends, bypassing the
  ///      5-minute throttle. Waking up is a strong signal the user is about
  ///      to interact; latency matters more than RV bandwidth here.
  Future<void> onAppResumed() async {
    debugPrint('[lifecycle] App resumed — probing sockets, re-syncing RVs');
    if (_udpService != null && _udpAvailable) {
      final rebound = await _udpService!.probeAndRebindIfDead();
      if (rebound) {
        debugPrint('[lifecycle] UDP transport rebound after foreground probe');
      }
    }
    unawaited(_syncConfiguredRendezvous(reason: 'app-resumed'));
    _refireAvailableForUnreachableFriends(force: true);
    _drainQueuedMessagesForLivePeers();
  }

  /// Clean up resources
  Future<void> dispose() async {
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

    // Complete any pending hole-punch waiters so send() callers don't hang
    for (final completer in _holePunchCompleters.values) {
      if (!completer.isCompleted) completer.complete(false);
    }
    _holePunchCompleters.clear();
    _holePunchTargets.clear();
    _holePunchLocalReady.clear();
    _holePunchRemoteReady.clear();
    _holePunchConnectionInProgress.clear();
    _availableFanOutLastFiredAt.clear();
    _outboundMessageQueue.clear();
    _outboundQueueDrainsInProgress.clear();

    for (final pending in _ackPendingMessages.values) {
      pending.ackTimer?.cancel();
    }
    _ackPendingMessages.clear();

    _messageRouter.dispose();
    _signalingService.dispose();
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

  /// Start the periodic ANNOUNCE timer
  void _startAnnounceTimer() {
    _announceTimer?.cancel();
    _announceTimer = Timer.periodic(config.announceInterval, (_) {
      _broadcastAnnounce();
      _broadcastAnnounceViaUdp();
      _removeStalePeers();
      _discoverUnreachableFriends();
      _refireAvailableForUnreachableFriends();
      _drainQueuedMessagesForLivePeers();
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
  ///
  /// Always includes our address — all UDP peers are known (no strangers).
  Future<void> _broadcastAnnounceViaUdp() async {
    if (_udpService == null || !_udpAvailable) return;

    final announce = await _createSignedAnnounce(
      address: udpAddress,
      addressCandidates: _candidateAddresses(),
    );
    await _udpService!.broadcast(announce);
  }

  /// Send ANNOUNCE directly to a specific BLE device ID.
  ///
  /// Called from the periodic [_broadcastAnnounce] loop and from
  /// [_sendFriendAnnounceToConnectedBlePaths] (which fires when we receive
  /// an ANNOUNCE from a freshly-identified friend, to close the privacy
  /// gap where the previous periodic broadcast had to omit our address
  /// because we hadn't yet linked the device ID to a pubkey).
  ///
  /// Also fired immediately when a BLE path becomes connected (per spec
  /// §BLE Discovery: ANNOUNCE is exchanged upon successful BLE connection).
  /// Receivers treat repeated ANNOUNCEs from the same pubkey as idempotent,
  /// so racing with the periodic broadcast is harmless.
  Future<bool> _sendAnnounceToDevice(String deviceId) async {
    if (_bleService == null || !_bleAvailable) return false;

    // Check if this device ID already belongs to an authenticated friend.
    final pubkey = _bleService!.getPubkeyForPeerId(deviceId);
    final isFriend = pubkey != null && _isAcceptedFriendPubkey(pubkey);

    final friendHint = _bleService!.getFriendPubkeyHintForPeerId(deviceId);
    final allowsColdCall =
        store.state.settings.coldCallTrustLevel == ColdCallTrustLevel.open;
    if (!isFriend && !allowsColdCall && friendHint == null) {
      debugPrint(
        '[ble-announce] Suppressed ANNOUNCE to $deviceId '
        '(closed trust, unknown peer)',
      );
      return false;
    }

    // Authenticated friends get our address + link-local. Non-friends, and
    // derived-UUID friend hints that have not yet sent a signed ANNOUNCE, get
    // only identity. A spoofed derived UUID must not unlock friend metadata.
    final announce = isFriend
        ? await _createSignedAnnounce(
            address: udpAddress,
            linkLocalAddress: _linkLocalAddress,
          )
        : await _createSignedAnnounce();

    final sent = await _bleService!.sendToPeer(deviceId, announce);
    if (sent) {
      if (isFriend) {
        _bleFriendAnnounceSent.add(deviceId);
      } else {
        _bleFriendAnnounceSent.remove(deviceId);
      }
    } else {
      debugPrint('[ble-announce] Failed to send ANNOUNCE to $deviceId');
    }
    return sent;
  }

  /// Once a BLE peer identifies itself, send them a directed friend ANNOUNCE
  /// on every live BLE path we have for them. This closes the window where the
  /// connection-time ANNOUNCE had to omit our address because the device ID had
  /// not been mapped to a pubkey yet.
  void _sendFriendAnnounceToConnectedBlePaths(PeerState peer) {
    if (_bleService == null || !_bleAvailable) return;

    final candidateIds = <String>{
      if (peer.bleCentralDeviceId != null) peer.bleCentralDeviceId!,
      if (peer.blePeripheralDeviceId != null) peer.blePeripheralDeviceId!,
    };

    for (final deviceId in candidateIds) {
      if (_bleFriendAnnounceSent.contains(deviceId)) continue;
      if (!_bleService!.isDeviceConnected(deviceId)) continue;
      _sendAnnounceToDevice(deviceId);
    }
  }

  /// Create a signed ANNOUNCE packet, optionally with address.
  Future<Uint8List> _createSignedAnnounce({
    String? address,
    String? linkLocalAddress,
    Iterable<String> addressCandidates = const [],
  }) async {
    final normalizedAddress = _normalizeAnnouncedUdpAddress(
      address,
      context: 'announce',
    );
    final normalizedLinkLocal = _normalizeAnnouncedLinkLocalAddress(
      linkLocalAddress,
      context: 'announce',
    );
    final includeKnownCandidates = normalizedAddress != null ||
        normalizedLinkLocal != null ||
        addressCandidates.isNotEmpty;
    final normalizedCandidates = normalizeAddressStrings([
      if (includeKnownCandidates)
        ..._candidateAddresses(includeLinkLocal: normalizedLinkLocal != null),
      ...addressCandidates,
      normalizedAddress,
      normalizedLinkLocal,
    ]);
    final payload = _protocolHandler.createAnnouncePayload(
      address: normalizedAddress,
      linkLocalAddress: normalizedLinkLocal,
      addressCandidates: normalizedCandidates,
    );
    // The ANNOUNCE payload is a self-signed identity record; the frame needs
    // no further authentication.
    return GrassrootsPacket(
      type: PacketType.announce,
      payload: payload,
    ).serialize();
  }

  /// Send ANNOUNCE with address to a specific friend.
  ///
  /// This is the unified presence mechanism — friends receive our UDP address
  /// in the ANNOUNCE so they can connect to us over the internet.
  ///
  /// Works over both BLE and UDP transports.
  Future<bool> sendAnnounceToFriend({
    required Uint8List friendPubkey,
    String? myAddress,
  }) async {
    var sent = false;
    final friendPubkeyHex =
        friendPubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

    // Create signed ANNOUNCE packet with our address
    final normalizedAddress = _normalizeAnnouncedUdpAddress(
      myAddress,
      context: 'direct-announce',
    );
    final payload = _protocolHandler.createAnnouncePayload(
      address: normalizedAddress,
      addressCandidates: _candidateAddresses(),
    );
    final bytes = GrassrootsPacket(
      type: PacketType.announce,
      payload: payload,
    ).serialize();

    // Try BLE first if available
    if (_bleService != null && _bleAvailable) {
      final peerId = _bleService!.getPeerIdForPubkey(friendPubkey);
      if (peerId != null) {
        sent = await _bleService!.sendToPeer(peerId, bytes);
      }
    }

    // Also try UDP if available
    if (_udpService != null && _udpAvailable) {
      final peerId = _udpService!.getPeerIdForPubkey(friendPubkey);
      if (peerId != null) {
        final udpSent = await _udpService!.sendToPeer(peerId, bytes);
        sent = sent || udpSent;
      } else {
        final peer = _peersState.getPeerByPubkeyHex(friendPubkeyHex);
        final candidates = _udpCandidatesForPeer(peer);
        final friendAddress = peer?.udpAddress ??
            (candidates.isNotEmpty ? candidates.first : null);
        if (friendAddress != null && friendAddress.isNotEmpty) {
          final udpSent = await _sendViaUdp(
            friendPubkeyHex,
            friendAddress,
            bytes,
          );
          sent = sent || udpSent;
        }
      }
    }

    return sent;
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

  // ===== BLE Fragmentation Helpers =====

  /// Send a large payload via BLE using fragmentation.
  /// Each fragment is individually encrypted and signed.
  Future<bool> _sendFragmentedViaBle({
    required Uint8List payload,
    required Uint8List recipientPubkey,
    required String bleDeviceId,
    required String messageId,
  }) async {
    final fragmented = _fragmentHandler.fragment(
      payload: payload,
      messageId: messageId,
    );

    for (final fragment in fragmented.fragments) {
      final bytes = await _packetBytesForTransport(
        packet: fragment,
        transport: PeerTransport.bleDirect,
        recipientPubkey: recipientPubkey,
        peerId: bleDeviceId,
      );
      if (bytes == null) return false;
      final sent = await _bleService!.sendToPeer(bleDeviceId, bytes);
      if (!sent) return false;
      await Future.delayed(FragmentHandler.fragmentDelay);
    }
    return true;
  }
}

/// Diff each peer's set of reachable transports against the previous tick and
/// fire the per-transport reachability callbacks for every medium that came up
/// or went down. Pure-ish: the only side effects are calling [onConnected] /
/// [onDisconnected] and mutating [lastKnownTransports] in place.
///
/// Semantics (per medium, independently — BLE and IP never interfere):
///   - a transport reachable now but not last tick: fire [onConnected] with
///     the peer's public key and that transport. (Discovery — onPeerDiscovered
///     — is surfaced separately at ANNOUNCE receipt, because a peer's identity
///     is known before its Noise session authenticates.)
///   - a transport reachable last tick but not now: fire [onDisconnected] with
///     the public key and that transport. A peer that loses one of two live
///     media fires a disconnect for the lost medium while staying reachable
///     over the other.
///   - a peer removed from [peersState.peersList] entirely (e.g.
///     PeerRemovedAction): fire [onDisconnected] for each medium it still held.
///   - no medium changed: no fire.
@visibleForTesting
void processReachabilityTransitions({
  required PeersState peersState,
  required Map<String, Set<MessageTransport>> lastKnownTransports,
  required void Function(Uint8List publicKey, MessageTransport transport)?
      onConnected,
  required void Function(Uint8List publicKey, MessageTransport transport)?
      onDisconnected,
}) {
  final seenPubkeys = <String>{};

  for (final peer in peersState.peersList) {
    final pk = peer.pubkeyHex;
    seenPubkeys.add(pk);
    final previous = lastKnownTransports[pk] ?? const <MessageTransport>{};
    final current = peer.reachableTransports;
    if (setEquals(previous, current)) continue;

    for (final t in current.difference(previous)) {
      onConnected?.call(peer.publicKey, t);
    }
    for (final t in previous.difference(current)) {
      onDisconnected?.call(peer.publicKey, t);
    }

    // Retain only non-empty sets so an unreachable peer holds no entry and the
    // removed-while-reachable sweep below stays simple.
    if (current.isEmpty) {
      lastKnownTransports.remove(pk);
    } else {
      lastKnownTransports[pk] = current;
    }
  }

  // Removed-while-reachable: the peer's entry vanished entirely, so surface a
  // disconnect on each medium it still held.
  final missing = lastKnownTransports.keys
      .where((pk) => !seenPubkeys.contains(pk))
      .toList(growable: false);
  for (final pk in missing) {
    final transports = lastKnownTransports.remove(pk)!;
    final cb = onDisconnected;
    if (cb == null) continue;
    final pubkey = Uint8List.fromList([
      for (var i = 0; i < pk.length; i += 2)
        int.parse(pk.substring(i, i + 2), radix: 16)
    ]);
    for (final t in transports) {
      cb(pubkey, t);
    }
  }
}
