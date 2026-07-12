import 'dart:typed_data';
import '../platform/compat.dart';
import '../models/peer.dart';
import '../transport/address_utils.dart';
import 'messages_actions.dart';

/// A discovered BLE peer before identity (ANNOUNCE) is exchanged.
///
/// This is a strict projection of facts emitted by the grassroots_bluetooth_layer plugin:
/// the path stream tells us whether a path is connecting/ready/disconnected;
/// the advertisement stream tells us about RSSI and service UUIDs. Reducers
/// MUST NOT infer state — every field corresponds to a plugin event or to
/// explicit user intent (the blacklist).
@immutable
class DiscoveredPeerState {
  /// PathId from the plugin, e.g. `central:<remote-id>`.
  final String transportId;

  /// Advertised local name (informational only — most Grassroots devices omit it).
  final String? displayName;

  /// Latest signal strength reported by the plugin's advertisement stream.
  /// Always populated for `DiscoveredPeerState` because every advertisement carries RSSI.
  /// Signal strength in dBm at last observation. Always a real
  /// negative-dBm measurement: the BLE plugin drops any advertisement whose
  /// RSSI is non-negative (a platform-level "no measurement" sentinel) so
  /// only real measurements reach this field.
  final int rssi;

  /// Grassroots service UUID from the advertisement. With derived UUIDs this
  /// is the pre-connect identity hint used only for discovery decisions.
  final String? serviceUuid;

  /// First time we observed an advertisement matching this pathId.
  final DateTime discoveredAt;

  /// Most recent time we saw any plugin event (advertisement, path change).
  final DateTime lastSeen;

  /// Set during the window between calling `GrassrootsBluetooth.connect()` and the
  /// plugin emitting `connected`/`ready`/`failed`. Cleared by every other
  /// path lifecycle event.
  final bool isConnecting;

  /// True iff the plugin's last path state was `ready` with `canSend=true`.
  final bool isConnected;

  const DiscoveredPeerState({
    required this.transportId,
    this.displayName,
    required this.rssi,
    this.serviceUuid,
    required this.discoveredAt,
    required this.lastSeen,
    this.isConnecting = false,
    this.isConnected = false,
  });

  /// Signal quality indicator (0.0 - 1.0), derived from rssi.
  double get signalQuality {
    if (rssi >= -50) return 1.0;
    if (rssi <= -100) return 0.0;
    return (rssi + 100) / 50.0;
  }

  DiscoveredPeerState copyWith({
    String? transportId,
    String? displayName,
    int? rssi,
    String? serviceUuid,
    DateTime? discoveredAt,
    DateTime? lastSeen,
    bool? isConnecting,
    bool? isConnected,
  }) {
    return DiscoveredPeerState(
      transportId: transportId ?? this.transportId,
      displayName: displayName ?? this.displayName,
      rssi: rssi ?? this.rssi,
      serviceUuid: serviceUuid ?? this.serviceUuid,
      discoveredAt: discoveredAt ?? this.discoveredAt,
      lastSeen: lastSeen ?? this.lastSeen,
      isConnecting: isConnecting ?? this.isConnecting,
      isConnected: isConnected ?? this.isConnected,
    );
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is DiscoveredPeerState &&
          runtimeType == other.runtimeType &&
          transportId == other.transportId &&
          rssi == other.rssi &&
          serviceUuid == other.serviceUuid &&
          isConnecting == other.isConnecting &&
          isConnected == other.isConnected;

  @override
  int get hashCode => Object.hash(
        transportId,
        rssi,
        serviceUuid,
        isConnecting,
        isConnected,
      );

  @override
  String toString() =>
      'DiscoveredPeerState($transportId, rssi: $rssi, connected: $isConnected)';
}

/// Immutable peer state for identified peers (after ANNOUNCE).
@immutable
class PeerState {
  final Uint8List publicKey;
  final PeerConnectionState connectionState;
  final PeerTransport transport;

  /// Latest BLE signal strength in dBm.
  ///
  /// Null when the peer has no live BLE link or when the BLE stack cannot
  /// report a real measurement for the current role (for example GATT-server
  /// peripheral writes). Cleared by `PeerBleDisconnectedAction` when the last
  /// BLE path drops.
  final int? rssi;

  final int protocolVersion;
  final DateTime? lastSeen;

  /// PathId of our central → their peripheral path, when one is currently
  /// ready in the plugin. Set on ANNOUNCE receipt over a central path,
  /// cleared on `disconnected`/`failed` for that path.
  final String? bleCentralDeviceId;

  /// PathId of their central → our peripheral path, when one is currently
  /// ready in the plugin. Set on ANNOUNCE receipt over a peripheral path,
  /// cleared on `disconnected`/`failed` for that path.
  final String? blePeripheralDeviceId;

  /// When the last BLE ANNOUNCE was received from this peer.
  /// Used to detect stale BLE IDs (peer left BLE range but still on UDP).
  final DateTime? lastBleSeen;

  /// When the last verified UDP packet was received from this peer.
  /// Used to age out stale UDX sessions independently of BLE freshness.
  final DateTime? lastUdpSeen;

  /// The peer's dial-book address (ip:port format), supplied by GLP via
  /// putPeerAddress or observed on a live session. Never cleared
  /// unilaterally — it is the last known location and the only way to
  /// attempt reconnection.
  final String? udpAddress;

  /// When we last successfully reached this peer at [udpAddress] over UDP
  /// without a prior hole-punch coordination — i.e. the address accepted
  /// unsolicited inbound.
  ///
  /// Bound to [udpAddress]: cleared whenever the UDP address changes, since
  /// any prior observation was for a different network path.
  final DateTime? lastDirectReachAt;

  /// Whether there is an authenticated UDP path to this peer — a live UDX
  /// connection whose Noise XX session has completed. Set true when the Noise
  /// handshake authenticates (not on the bare UDX connect), false when the
  /// stream closes. Unlike [udpAddress] (preserved for reconnection), this
  /// reflects the actual authenticated-connection state right now.
  final bool hasLiveUdpConnection;

  /// Whether there is an authenticated BLE path to this peer — a live BLE link
  /// whose Noise XX session has completed. Set true when the BLE Noise
  /// handshake authenticates, cleared when the last BLE path drops. Distinct
  /// from [hasBleConnection] (the raw link, set on ANNOUNCE so we can route the
  /// handshake itself): a peer is only [isReachable] once authenticated.
  final bool bleAuthenticated;

  const PeerState({
    required this.publicKey,
    this.connectionState = PeerConnectionState.discovered,
    this.transport = PeerTransport.bleDirect,
    this.rssi,
    this.protocolVersion = 1,
    this.lastSeen,
    this.bleCentralDeviceId,
    this.blePeripheralDeviceId,
    this.lastBleSeen,
    this.lastUdpSeen,
    this.udpAddress,
    this.lastDirectReachAt,
    this.hasLiveUdpConnection = false,
    this.bleAuthenticated = false,
  });

  /// Hex representation of public key (for map keys)
  String get pubkeyHex =>
      publicKey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  /// Display name (truncated pubkey — nicknames are a GLP-level concern;
  /// the layer identifies peers by key only).
  String get displayName => '${pubkeyHex.substring(0, 8)}...';

  /// Whether this peer is currently connected.
  ///
  /// "Connected" means: we have received an ANNOUNCE from this peer over a
  /// transport that is still live AND we have completed our own ANNOUNCE
  /// exchange with them. Both sides reach this state within one ANNOUNCE
  /// round-trip of each other; there is no scenario where one side shows
  /// "connected" while the other shows "not connected" once both ANNOUNCEs
  /// have been received.
  bool get isConnected => connectionState == PeerConnectionState.connected;

  /// Whether this peer has any BLE connection (central or peripheral)
  bool get hasBleConnection =>
      bleCentralDeviceId != null || blePeripheralDeviceId != null;

  /// Convenience getter: preferred BLE device ID for sending.
  /// Prefers central (we initiated) since sendToPeer tries central service first.
  String? get bleDeviceId => bleCentralDeviceId ?? blePeripheralDeviceId;

  /// Whether we have an address for this peer that we could attempt to dial.
  /// For UDP, a stored dial-book address suffices; for BLE, a live path.
  bool get hasKnownAddress => hasBleConnection || udpAddress != null;

  /// Whether this peer's dial-book address is publicly routable.
  bool get hasPublicUdpAddress =>
      udpAddress != null && isGloballyRoutableAddress(udpAddress!);

  /// Whether this peer is reachable right now via any *authenticated*
  /// transport. This is the canonical "can a send succeed without queueing"
  /// predicate and the basis for [isReachable]'s public surface,
  /// `GrassrootsNetwork.isPeerReachable`.
  ///
  /// Reachability requires a completed Noise session — spec
  /// `docs/GLP_Networking_API/sections/ip.tex` §IP Connection: connected fires
  /// once the stream is "established and authenticated". A raw BLE/UDX link
  /// without a session does not count as reachable.
  bool get isReachable => bleAuthenticated || hasLiveUdpConnection;

  /// The transports over which this peer is reachable *right now* — one entry
  /// per medium that holds a completed Noise session. Empty when the peer is
  /// unreachable. This is the projection behind `GrassrootsNetwork.peerTransports`
  /// and the per-transport `onPeerConnected` / `onPeerDisconnected` callbacks.
  /// Iterated BLE-before-IP so callers observe a stable order. Spec
  /// `docs/GLP_Networking_API/sections/api.tex` §Connection and Reachability.
  Set<MessageTransport> get reachableTransports => {
        if (bleAuthenticated) MessageTransport.ble,
        if (hasLiveUdpConnection) MessageTransport.udp,
      };

  /// The currently active transport based on available connections.
  /// BLE is preferred when available; falls back to UDP, then stored value.
  PeerTransport get activeTransport {
    if (hasBleConnection) return PeerTransport.bleDirect;
    if (udpAddress != null) return PeerTransport.udp;
    return transport;
  }

  /// Signal quality (0.0 - 1.0). Returns null when no RSSI is known
  /// (UDP-only or BLE-disconnected peers). Callers in BLE-only contexts
  /// (e.g. the Nearby panel) can safely use `signalQuality!`.
  double? get signalQuality {
    final r = rssi;
    if (r == null) return null;
    if (r >= -50) return 1.0;
    if (r <= -100) return 0.0;
    return (r + 100) / 50.0;
  }

  PeerState copyWith({
    Uint8List? publicKey,
    PeerConnectionState? connectionState,
    PeerTransport? transport,
    int? rssi,
    int? protocolVersion,
    DateTime? lastSeen,
    String? bleCentralDeviceId,
    String? blePeripheralDeviceId,
    DateTime? lastBleSeen,
    DateTime? lastUdpSeen,
    String? udpAddress,
    DateTime? lastDirectReachAt,
    bool clearLastDirectReachAt = false,
    bool? hasLiveUdpConnection,
    bool? bleAuthenticated,
  }) {
    return PeerState(
      publicKey: publicKey ?? this.publicKey,
      connectionState: connectionState ?? this.connectionState,
      transport: transport ?? this.transport,
      rssi: rssi ?? this.rssi,
      protocolVersion: protocolVersion ?? this.protocolVersion,
      lastSeen: lastSeen ?? this.lastSeen,
      bleCentralDeviceId: bleCentralDeviceId ?? this.bleCentralDeviceId,
      blePeripheralDeviceId:
          blePeripheralDeviceId ?? this.blePeripheralDeviceId,
      lastBleSeen: lastBleSeen ?? this.lastBleSeen,
      lastUdpSeen: lastUdpSeen ?? this.lastUdpSeen,
      udpAddress: udpAddress ?? this.udpAddress,
      lastDirectReachAt: clearLastDirectReachAt
          ? null
          : lastDirectReachAt ?? this.lastDirectReachAt,
      hasLiveUdpConnection: hasLiveUdpConnection ?? this.hasLiveUdpConnection,
      bleAuthenticated: bleAuthenticated ?? this.bleAuthenticated,
    );
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is PeerState &&
          runtimeType == other.runtimeType &&
          pubkeyHex == other.pubkeyHex &&
          connectionState == other.connectionState &&
          transport == other.transport &&
          rssi == other.rssi &&
          bleCentralDeviceId == other.bleCentralDeviceId &&
          blePeripheralDeviceId == other.blePeripheralDeviceId &&
          udpAddress == other.udpAddress &&
          lastDirectReachAt == other.lastDirectReachAt &&
          hasLiveUdpConnection == other.hasLiveUdpConnection;

  @override
  int get hashCode => Object.hash(
        pubkeyHex,
        connectionState,
        transport,
        rssi,
        bleCentralDeviceId,
        blePeripheralDeviceId,
        udpAddress,
        lastDirectReachAt,
        hasLiveUdpConnection,
      );
}

/// Complete peers state for Redux store
@immutable
class PeersState {
  /// Discovered BLE peers (before ANNOUNCE), keyed by pathId.
  final Map<String, DiscoveredPeerState> discoveredBlePeers;

  /// Identified peers (after ANNOUNCE), keyed by pubkey hex
  final Map<String, PeerState> peers;

  const PeersState({
    this.discoveredBlePeers = const {},
    this.peers = const {},
  });

  static const PeersState initial = PeersState();

  // ===== Getters =====

  /// All discovered BLE peers as list
  List<DiscoveredPeerState> get discoveredBlePeersList =>
      discoveredBlePeers.values.toList();

  /// All identified peers as list
  List<PeerState> get peersList => peers.values.toList();

  /// Connected peers only
  List<PeerState> get connectedPeers =>
      peers.values.where((p) => p.isConnected).toList();

  /// Peers reachable via BLE
  List<PeerState> get blePeers =>
      peers.values.where((p) => p.hasBleConnection).toList();

  /// Nearby peers — anyone (friend or stranger) we currently hold a live BLE
  /// path to (central or peripheral). Used for the "Connected Peers" /
  /// "Nearby" UI section.
  ///
  /// Deliberately filters by `hasBleConnection` alone, NOT by
  /// `connectionState`. `connectionState` is a strict projection of
  /// transport-emitted facts and stays at `connected` until an explicit BLE
  /// disconnect surfaces — which can be missed when the path-state machine
  /// drifts through `failed`/`subscribed` without a clean `ready → dropped`
  /// transition. The BLE device-id fields are the ground truth of whether
  /// we still hold a path. The `_removeStalePeers` sweep in
  /// `GrassrootsNetwork` clears those ids on `lastBleSeen` staleness so a
  /// peer that's gone silent for two announce cycles falls off this list.
  List<PeerState> get nearbyBlePeers =>
      peers.values.where((p) => p.hasBleConnection).toList();

  /// Peers with a live UDP connection
  List<PeerState> get udpPeers =>
      peers.values.where((p) => p.hasLiveUdpConnection).toList();

  /// Count of connected peers
  int get connectedCount => connectedPeers.length;

  /// Count of all discovered BLE devices
  int get discoveredBleCount => discoveredBlePeers.length;

  /// Get peer by pubkey hex
  PeerState? getPeerByPubkeyHex(String pubkeyHex) => peers[pubkeyHex];

  /// Get peer by pubkey bytes
  PeerState? getPeerByPubkey(Uint8List pubkey) {
    final hex = pubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
    return peers[hex];
  }

  /// Get discovered BLE peer by device ID
  DiscoveredPeerState? getDiscoveredBlePeer(String deviceId) =>
      discoveredBlePeers[deviceId];

  /// Find every discovered BLE peer advertising the given service UUID,
  /// regardless of radio MAC / CBPeripheral identifier. The advertised UUID
  /// is the rotating per-slot beacon (Grassroots prefix + SHA-256("glp ble
  /// suffix" | pubkey | slot)[0..8]): stable within a slot — which is how we
  /// recognise the same logical peer when its radio identifier changes
  /// mid-slot — but different each slot, so slot-spanning recognition must
  /// query once per candidate UUID (the transport's `candidateServiceUuids`).
  ///
  /// Returns an empty iterable when `serviceUuid` is null/empty so callers
  /// don't have to null-check.
  Iterable<DiscoveredPeerState> getDiscoveredBlePeersByServiceUuid(
    String? serviceUuid,
  ) {
    if (serviceUuid == null || serviceUuid.isEmpty) {
      return const <DiscoveredPeerState>[];
    }
    final normalized = serviceUuid.toLowerCase();
    return discoveredBlePeers.values
        .where((p) => p.serviceUuid?.toLowerCase() == normalized);
  }

  /// Check if a peer is reachable by pubkey
  bool isPeerReachable(Uint8List pubkey) {
    final peer = getPeerByPubkey(pubkey);
    return peer?.isReachable ?? false;
  }

  // ===== Copy With =====

  PeersState copyWith({
    Map<String, DiscoveredPeerState>? discoveredBlePeers,
    Map<String, PeerState>? peers,
  }) {
    return PeersState(
      discoveredBlePeers: discoveredBlePeers ?? this.discoveredBlePeers,
      peers: peers ?? this.peers,
    );
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is PeersState &&
          runtimeType == other.runtimeType &&
          mapEquals(discoveredBlePeers, other.discoveredBlePeers) &&
          mapEquals(peers, other.peers);

  @override
  int get hashCode => Object.hash(
        _hashStringKeyedMap(discoveredBlePeers),
        _hashStringKeyedMap(peers),
      );
}

int _hashStringKeyedMap<T>(Map<String, T> map) {
  return Object.hashAll(
    (map.entries.toList()..sort((a, b) => a.key.compareTo(b.key))).map(
      (entry) => Object.hash(entry.key, entry.value),
    ),
  );
}

