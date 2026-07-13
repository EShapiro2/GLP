import 'dart:async';
import 'dart:io';
import 'dart:typed_data';
import 'package:redux/redux.dart';

import '../platform/compat.dart';
import '../transport/transport_service.dart';
import '../transport/address_utils.dart';
import '../models/identity.dart';
import '../models/packet.dart';
import '../protocol/protocol_handler.dart';
import '../store/store.dart';

/// Classification of an outbound connect refusal.
enum UdpConnectFailureKind {
  networkUnreachable,
  handshakeTimeout,
  other,
}

/// The IP carrier of the shared message transport (spec §Message Transport,
/// §IP Connection): datagrams need no stream. One Grassroots packet rides
/// one UDP datagram, sized within the path MTU; reliability (per-fragment
/// ACK + retransmission) and sessions live above this service. The UDP path
/// with its session is the connection in the liveness sense — this service
/// only binds sockets, associates peer addresses, and moves datagrams.
///
/// A "connection" here is soft state: a peer's dialable address, keyed by
/// pubkey hex once identified (a temp `ip:port` id until then). There is no
/// transport-level handshake and no stream; teardown is the coordinator's
/// liveness sweep (two silent ANNOUNCE intervals) or an explicit
/// [disconnectFromPeer].
class UdpTransportService extends TransportService {
  /// Datagram budget: one framed, session-encrypted packet must fit within
  /// a conservative path-MTU estimate.
  static const int maxDatagramBytes = 1200;

  final GrassrootsIdentity identity;
  final Store<AppState> store;
  final ProtocolHandler protocolHandler;

  /// Fixed UDP listen port for both address families; 0 (default) binds an
  /// ephemeral port per family. Server profiles bind their published port.
  final int listenPort;

  UdpTransportService({
    required this.identity,
    required this.store,
    required this.protocolHandler,
    this.listenPort = 0,
  });

  // --- Sockets ---

  final Map<InternetAddressType, RawDatagramSocket> _rawSockets = {};
  final Map<InternetAddressType, int> _localPorts = {};
  final Map<InternetAddressType, StreamSubscription> _socketSubs = {};
  RawDatagramSocket? _rawSocket;
  int? _localPort;
  InternetAddressType? _activeAddressType;

  // --- Peer paths ---

  /// Peer id (pubkey hex, or temp `ip:port` until identified) → address.
  final Map<String, AddressInfo> _peerPaths = {};

  /// Reverse map: `ip:port` → peer id currently associated with it.
  final Map<String, String> _addressToPeerId = {};

  /// Last inbound activity per temp (`ip:port`) path. Datagrams emit no
  /// close events, so unidentified paths — an ANNOUNCE that never completed,
  /// a mapped path's leftover alias, garbage from a scanner — are expired by
  /// [_sweepTempPaths] instead. Identified (pubkey-keyed) paths are the
  /// coordinator's to manage.
  final Map<String, DateTime> _tempPathLastSeen = {};
  Timer? _tempPathSweepTimer;

  /// A temp path unseen for this long is dropped. Long enough for any
  /// in-flight handler still holding the temp id, and for a slow ANNOUNCE →
  /// handshake bootstrap.
  static const Duration tempPathExpiry = Duration(minutes: 2);

  TransportState _state = TransportState.uninitialized;

  final _dataController = StreamController<TransportDataEvent>.broadcast();
  final _connectionController =
      StreamController<TransportConnectionEvent>.broadcast();

  /// Called when a packet-bearing datagram arrives from a peer.
  void Function(String peerId, Uint8List data)? onUdpDataReceived;

  UdpConnectFailureKind? _lastConnectFailureKind;

  // ===== Public getters =====

  /// The raw UDP socket — exposed for hole-punch sends.
  RawDatagramSocket? get rawSocket => _rawSocket;

  /// Raw UDP sockets keyed by IP family.
  Map<InternetAddressType, RawDatagramSocket> get rawSocketsByType =>
      Map.unmodifiable(_rawSockets);

  /// Our bound port (available after [initialize]).
  int? get localPort => _localPort;

  /// Bound local port for [type], if that family initialized successfully.
  int? localPortForAddressType(InternetAddressType type) => _localPorts[type];

  /// The preferred active IP family for the bound UDP socket.
  InternetAddressType? get activeAddressType => _activeAddressType;

  /// Every active IP family with a bound UDP socket.
  Set<InternetAddressType> get activeAddressTypes =>
      Set.unmodifiable(_rawSockets.keys);

  /// True once we've bound at least one UDP socket. The canonical "can we
  /// be reached" answer comes from the reflected public address, updated
  /// asynchronously via signaling.
  bool get hasUsableRoute => _activeAddressType != null;

  /// Classification of the most recent outbound connect refusal, if any.
  UdpConnectFailureKind? get lastConnectFailureKind => _lastConnectFailureKind;

  @override
  TransportType get type => TransportType.udp;

  @override
  TransportState get state => _state;

  @override
  Stream<TransportDataEvent> get dataStream => _dataController.stream;

  @override
  Stream<TransportConnectionEvent> get connectionStream =>
      _connectionController.stream;

  @override
  int get connectedCount =>
      _peerPaths.keys.where((id) => !id.contains(':')).length;

  @override
  bool get isActive => _state == TransportState.active;

  // ===== Lifecycle =====

  @override
  Future<bool> initialize() async {
    if (_state != TransportState.uninitialized) {
      debugPrint('UDP transport already initialized');
      return _state.isUsable;
    }

    _setState(TransportState.initializing);
    debugPrint('Initializing UDP transport');

    try {
      await _bindFamily(InternetAddressType.IPv6);
      await _bindFamily(InternetAddressType.IPv4);

      if (_rawSockets.isEmpty) {
        debugPrint('Failed to bind any UDP socket');
        _setState(TransportState.error);
        return false;
      }

      _refreshPreferredSocket();

      _setState(TransportState.ready);
      final bindings = _rawSockets.entries
          .map((entry) =>
              '${entry.key == InternetAddressType.IPv6 ? "IPv6" : "IPv4"}:${entry.value.port}')
          .join(', ');
      debugPrint('UDP transport bound ($bindings)');
      return true;
    } catch (e) {
      debugPrint('Failed to initialize UDP sockets: $e');
      _setState(TransportState.error);
      return false;
    }
  }

  @override
  Future<void> start() async {
    if (_rawSockets.isEmpty) {
      debugPrint('Cannot start: no bound socket. Call initialize() first.');
      return;
    }
    for (final entry in _rawSockets.entries) {
      final family = entry.key;
      if (_socketSubs.containsKey(family)) continue;
      final socket = entry.value;
      _socketSubs[family] = socket.listen((event) {
        if (event != RawSocketEvent.read) return;
        Datagram? datagram;
        while ((datagram = socket.receive()) != null) {
          _handleDatagram(datagram!);
        }
      });
    }
    _tempPathSweepTimer ??= Timer.periodic(
      const Duration(seconds: 30),
      (_) => _sweepTempPaths(),
    );
    _setState(TransportState.active);
    debugPrint(
      'UDP datagram receive started on '
      '${_rawSockets.keys.map((family) => family == InternetAddressType.IPv6 ? "IPv6" : "IPv4").join(", ")}',
    );
  }

  void _sweepTempPaths() {
    final cutoff = DateTime.now().subtract(tempPathExpiry);
    for (final tempKey in _peerPaths.keys
        .where((id) => id.contains(':'))
        .toList()) {
      final seen = _tempPathLastSeen[tempKey];
      if (seen != null && seen.isAfter(cutoff)) continue;
      final path = _peerPaths.remove(tempKey);
      _tempPathLastSeen.remove(tempKey);
      // Drop the reverse entry only if it still points at the temp id (an
      // identified path re-keyed it to the pubkey).
      if (path != null &&
          _addressToPeerId[path.toAddressString()] == tempKey) {
        _addressToPeerId.remove(path.toAddressString());
      }
    }
  }

  @override
  Future<void> stop() async {
    debugPrint('Stopping UDP transport');
    _tempPathSweepTimer?.cancel();
    _tempPathSweepTimer = null;
    for (final sub in _socketSubs.values) {
      await sub.cancel();
    }
    _socketSubs.clear();

    final peers = _peerPaths.keys.where((id) => !id.contains(':')).toList();
    _peerPaths.clear();
    _addressToPeerId.clear();
    _tempPathLastSeen.clear();
    for (final peerId in peers) {
      if (!_connectionController.isClosed) {
        _connectionController.add(TransportConnectionEvent(
          peerId: peerId,
          transport: TransportType.udp,
          connected: false,
          reason: 'Transport stopped',
        ));
      }
    }
    if (_state == TransportState.active) {
      _setState(TransportState.ready);
    }
  }

  @override
  Future<void> dispose() async {
    debugPrint('Disposing UDP transport');
    await stop();
    for (final socket in _rawSockets.values) {
      try {
        socket.close();
      } catch (_) {}
    }
    _rawSockets.clear();
    _localPorts.clear();
    _rawSocket = null;
    _localPort = null;
    _activeAddressType = null;
    await _dataController.close();
    await _connectionController.close();
    _setState(TransportState.disposed);
  }

  /// Probe each bound socket and rebuild the transport when the OS poisoned
  /// one (Android background EPERM state). Returns true if a rebind
  /// happened; the coordinator re-establishes sessions on the fresh
  /// sockets.
  Future<bool> probeAndRebindIfDead() async {
    if (_rawSockets.isEmpty) return false;

    final dead = <InternetAddressType>[];
    final probe = Uint8List.fromList(const [0]);
    for (final entry in _rawSockets.entries) {
      final family = entry.key;
      final socket = entry.value;
      final loopback = family == InternetAddressType.IPv6
          ? InternetAddress.loopbackIPv6
          : InternetAddress.loopbackIPv4;
      final familyLabel = family == InternetAddressType.IPv6 ? "IPv6" : "IPv4";
      try {
        final n = socket.send(probe, loopback, 1);
        if (n <= 0) {
          debugPrint('[probe] $familyLabel wrote $n bytes on probe — dead');
          dead.add(family);
        }
      } catch (e) {
        debugPrint('[probe] $familyLabel probe threw $e — dead');
        dead.add(family);
      }
    }

    if (dead.isEmpty) {
      debugPrint('[probe] UDP sockets healthy');
      return false;
    }

    debugPrint('[probe] Rebinding UDP transport — '
        '${dead.length} dead family/families');

    await stop();
    for (final s in _rawSockets.values) {
      try {
        s.close();
      } catch (_) {}
    }
    _rawSockets.clear();
    _localPorts.clear();
    _rawSocket = null;
    _localPort = null;
    _activeAddressType = null;

    _setState(TransportState.uninitialized);
    final ok = await initialize();
    if (!ok) {
      debugPrint('[probe] Re-initialize failed after socket death');
      return true;
    }
    await start();
    return true;
  }

  // ===== Peer paths =====

  bool canDialAddress(InternetAddress address) {
    if (!_rawSockets.containsKey(address.type)) return false;
    if (address.isLoopback) return true;
    return hasUsableRoute;
  }

  /// Associate [pubkeyHex] with a dialable address. There is no
  /// transport-level handshake (spec §IP Connection: datagrams need no
  /// stream) — the association is immediately usable and the session layer
  /// above defines reachability.
  Future<bool> connectToPeer(
      String pubkeyHex, InternetAddress addr, int port) async {
    _lastConnectFailureKind = null;
    if (!canDialAddress(addr)) {
      _lastConnectFailureKind = UdpConnectFailureKind.networkUnreachable;
      debugPrint('Cannot associate $pubkeyHex: no usable '
          '${addr.type == InternetAddressType.IPv6 ? "IPv6" : "IPv4"} '
          'route for ${addr.address}:$port');
      return false;
    }
    final isNew = !_peerPaths.containsKey(pubkeyHex);
    _setPath(pubkeyHex, AddressInfo(addr, port));
    if (isNew && !_connectionController.isClosed) {
      _connectionController.add(TransportConnectionEvent(
        peerId: pubkeyHex,
        transport: TransportType.udp,
        connected: true,
        isIncoming: false,
      ));
    }
    return true;
  }

  /// The coordinator identified the sender behind a temp connection id
  /// (verified ANNOUNCE or session-authenticated packet): re-key its path to
  /// the pubkey. The temp id is retained as an alias to the same address —
  /// the datagram receive loop can drain several packets from one peer
  /// before the identifying one is processed, so an in-flight handler still
  /// holding the temp id must keep resolving to a live path.
  void mapIncomingConnectionToPubkey(String tempKey, String pubkeyHex) {
    if (tempKey == pubkeyHex) return;
    final path = _peerPaths[tempKey];
    if (path == null) return;
    debugPrint('Mapped incoming path $tempKey → $pubkeyHex');
    final hadPath = _peerPaths.containsKey(pubkeyHex);
    // Re-key the reverse map to the identity, but keep the temp→address
    // forward entry as an alias for outbound sends.
    _peerPaths[pubkeyHex] = path;
    _peerPaths[tempKey] = path;
    _addressToPeerId[path.toAddressString()] = pubkeyHex;
    if (!hadPath && !_connectionController.isClosed) {
      _connectionController.add(TransportConnectionEvent(
        peerId: pubkeyHex,
        transport: TransportType.udp,
        connected: true,
        isIncoming: true,
      ));
    }
  }

  Future<void> disconnectFromPeer(String pubkeyHex) async {
    final path = _peerPaths.remove(pubkeyHex);
    if (path == null) return;
    if (_addressToPeerId[path.toAddressString()] == pubkeyHex) {
      _addressToPeerId.remove(path.toAddressString());
    }
    if (!_connectionController.isClosed) {
      _connectionController.add(TransportConnectionEvent(
        peerId: pubkeyHex,
        transport: TransportType.udp,
        connected: false,
        reason: 'Disconnected',
      ));
    }
    debugPrint('Disconnected from peer $pubkeyHex');
  }

  void _setPath(String peerId, AddressInfo path) {
    final previous = _peerPaths[peerId];
    if (previous != null) {
      _addressToPeerId.remove(previous.toAddressString());
    }
    _peerPaths[peerId] = path;
    _addressToPeerId[path.toAddressString()] = peerId;
  }

  @override
  void associatePeerWithPubkey(String peerId, Uint8List pubkey) {
    // Paths are re-keyed via mapIncomingConnectionToPubkey.
  }

  @override
  String? getPeerIdForPubkey(Uint8List pubkey) {
    final hex = pubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
    return _peerPaths.containsKey(hex) ? hex : null;
  }

  @override
  Uint8List? getPubkeyForPeerId(String peerId) {
    if (peerId.contains(':') || peerId.length != 64) return null;
    if (!_peerPaths.containsKey(peerId)) return null;
    final bytes = Uint8List(32);
    for (var i = 0; i < 32; i++) {
      bytes[i] = int.parse(peerId.substring(2 * i, 2 * i + 2), radix: 16);
    }
    return bytes;
  }

  /// The address a peer's datagrams come from / go to.
  ({InternetAddress ip, int port})? getRemoteAddress(String peerId) {
    final path = _peerPaths[peerId];
    if (path == null) return null;
    return (ip: path.ip, port: path.port);
  }

  // ===== Send / receive =====

  /// Send one framed packet as one datagram to [peerId]'s path.
  @override
  Future<bool> sendToPeer(String peerId, Uint8List data) async {
    final path = _peerPaths[peerId];
    if (path == null) {
      debugPrint('Cannot send to $peerId: no path');
      return false;
    }
    if (data.length > maxDatagramBytes) {
      debugPrint('Refusing oversized datagram to $peerId '
          '(${data.length} > $maxDatagramBytes) — fragment above this layer');
      return false;
    }
    final socket = _rawSockets[path.ip.type];
    if (socket == null) {
      debugPrint('Cannot send to $peerId: no '
          '${path.ip.type == InternetAddressType.IPv6 ? "IPv6" : "IPv4"} '
          'socket');
      return false;
    }
    try {
      return socket.send(data, path.ip, path.port) > 0;
    } catch (e) {
      debugPrint('Failed to send to peer $peerId: $e');
      return false;
    }
  }

  @override
  Future<void> broadcast(Uint8List data, {Set<String>? excludePeerIds}) async {
    for (final peerId in _peerPaths.keys.toList()) {
      if (peerId.contains(':')) continue; // unidentified paths
      if (excludePeerIds != null && excludePeerIds.contains(peerId)) continue;
      await sendToPeer(peerId, data);
    }
  }

  void _handleDatagram(Datagram datagram) {
    final data = datagram.data;
    // Punch packets (36 bytes, magic "BCPU") open NAT mappings; nothing to
    // route.
    if (data.length == 36 &&
        data[0] == 0x42 &&
        data[1] == 0x43 &&
        data[2] == 0x50 &&
        data[3] == 0x55) {
      return;
    }
    if (data.length < GrassrootsPacket.headerSize) return;

    final address = AddressInfo(datagram.address, datagram.port);
    final addressKey = address.toAddressString();
    var peerId = _addressToPeerId[addressKey];
    if (peerId == null) {
      // Unsolicited inbound. Contact bootstraps with ANNOUNCE or a Noise
      // handshake (spec §Session Establishment); only those earn a temp
      // path — anything else from an unknown source is undeliverable
      // upstream anyway (no session) and must not grow per-source state.
      final int typeByte = data[0];
      if (typeByte != PacketType.announce.value &&
          typeByte != PacketType.noiseHandshake.value) {
        return;
      }
      peerId = addressKey;
      _setPath(peerId, address);
      debugPrint('Inbound datagram path from $addressKey');
    }
    if (peerId.contains(':')) {
      _tempPathLastSeen[peerId] = DateTime.now();
    }

    if (!_dataController.isClosed) {
      _dataController.add(TransportDataEvent(
        peerId: peerId,
        transport: TransportType.udp,
        data: data,
      ));
    }
    onUdpDataReceived?.call(peerId, data);
  }

  // ===== Internal =====

  Future<void> _bindFamily(InternetAddressType type) async {
    final bindAddress = type == InternetAddressType.IPv6
        ? InternetAddress.anyIPv6
        : InternetAddress.anyIPv4;
    try {
      final socket = await RawDatagramSocket.bind(bindAddress, listenPort);
      _rawSockets[type] = socket;
      _localPorts[type] = socket.port;
    } catch (e) {
      debugPrint(
        'Failed to bind '
        '${type == InternetAddressType.IPv6 ? "IPv6" : "IPv4"} '
        'UDP socket: $e',
      );
    }
  }

  void _refreshPreferredSocket() {
    final preferredType = _rawSockets.containsKey(InternetAddressType.IPv6)
        ? InternetAddressType.IPv6
        : (_rawSockets.isNotEmpty ? _rawSockets.keys.first : null);
    _activeAddressType = preferredType;
    _rawSocket = preferredType != null ? _rawSockets[preferredType] : null;
    _localPort = preferredType != null ? _localPorts[preferredType] : null;
  }

  void _setState(TransportState newState) {
    if (_state == newState) return;
    _state = newState;
    store.dispatch(UdpTransportStateChangedAction(newState));
  }
}
