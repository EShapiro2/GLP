import 'dart:async';
import 'dart:io';
import 'dart:typed_data';
import 'package:redux/redux.dart';

import '../platform/compat.dart';
import '../transport/transport_service.dart';
import '../transport/address_utils.dart';
import '../transport/hole_punch_service.dart';
import '../models/identity.dart';
import '../models/packet.dart';
import '../models/peer.dart';
import '../protocol/protocol_handler.dart';
import '../store/store.dart';

/// Classification of an outbound connect refusal.
enum UdpConnectFailureKind {
  networkUnreachable,
  other,
}

/// The IP carrier of the shared message transport (spec §Message Transport,
/// §IP Connection): datagrams need no stream. One Grassroots packet rides
/// one UDP datagram, sized within the path MTU; reliability (per-fragment
/// ACK + retransmission) and sessions live above this service. The UDP path
/// with its session is the connection in the liveness sense — this service
/// only binds sockets, associates peer addresses, and moves datagrams.
///
/// A "connection" here is soft state: a peer's dialable address. There is no
/// transport-level handshake and no stream; teardown is the coordinator's
/// liveness sweep (two silent ANNOUNCE intervals) or an explicit
/// [disconnectFromPeer].
///
/// Sessions are per underlay (spec §Session Establishment), and the LAN and
/// the WAN share these sockets, so a peer may hold two paths at once: one for
/// its LAN session and one for its WAN session. Each path belongs to the IP
/// session [sessionOf] assigns its address when the path is opened (decision
/// 8), and an inbound datagram is attributed to the path its source address
/// is bound to. An identified path is keyed by [pathId] — its session and the
/// peer's pubkey hex; an unidentified one by its temp `ip:port` id.
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

  /// The IP session a path to [address] belongs to — [PeerTransport.lan] or
  /// [PeerTransport.wan] — or null when it falls under no underlay the layer
  /// runs, and no path is opened to it (decision 8). Asked once per path, when
  /// it is opened.
  final PeerTransport? Function(InternetAddress address) sessionOf;

  UdpTransportService({
    required this.identity,
    required this.store,
    required this.protocolHandler,
    required this.sessionOf,
    this.listenPort = 0,
  });

  /// The id of the identified path of [session] to the peer holding
  /// [pubkeyHex]: what the path is keyed by, and what its datagrams and its
  /// connection events carry as their peer id.
  static String pathId(PeerTransport session, String pubkeyHex) {
    assert(session.isIp, 'a UDP path belongs to an IP session');
    return '${session.name}/$pubkeyHex';
  }

  /// The session and the peer's pubkey hex an identified path id names, or
  /// null for a temp `ip:port` id or any other string.
  static ({PeerTransport session, String pubkeyHex})? parsePathId(
      String peerId) {
    final slash = peerId.indexOf('/');
    if (slash < 0) return null;
    final session = switch (peerId.substring(0, slash)) {
      'lan' => PeerTransport.lan,
      'wan' => PeerTransport.wan,
      _ => null,
    };
    final pubkeyHex = peerId.substring(slash + 1);
    if (session == null || pubkeyHex.length != 64) return null;
    return (session: session, pubkeyHex: pubkeyHex);
  }

  static bool _isTemp(String peerId) => peerId.contains(':');

  // --- Sockets ---

  final Map<InternetAddressType, RawDatagramSocket> _rawSockets = {};
  final Map<InternetAddressType, int> _localPorts = {};
  final Map<InternetAddressType, StreamSubscription> _socketSubs = {};
  RawDatagramSocket? _rawSocket;
  int? _localPort;
  InternetAddressType? _activeAddressType;

  // --- Peer paths ---

  /// Peer id ([pathId], or temp `ip:port` until identified) → path.
  final Map<String, _UdpPath> _peerPaths = {};

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

  /// Called when a packet-bearing datagram arrives from a peer, with the IP
  /// session of the path it arrived on.
  void Function(PeerTransport session, String peerId, Uint8List data)?
      onUdpDataReceived;

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
      _peerPaths.keys.where((id) => !_isTemp(id)).length;

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
    for (final tempKey in _peerPaths.keys.where(_isTemp).toList()) {
      final seen = _tempPathLastSeen[tempKey];
      if (seen != null && seen.isAfter(cutoff)) continue;
      final path = _peerPaths.remove(tempKey);
      _tempPathLastSeen.remove(tempKey);
      // Drop the reverse entry only if it still points at the temp id (an
      // identified path re-keyed it to the pubkey).
      if (path != null &&
          _addressToPeerId[path.addressKey] == tempKey) {
        _addressToPeerId.remove(path.addressKey);
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

    final peers = _peerPaths.keys.where((id) => !_isTemp(id)).toList();
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

  /// Associate [pubkeyHex] with a dialable address, on the IP session the
  /// address belongs to ([sessionOf]). There is no transport-level handshake
  /// (spec §IP Connection: datagrams need no stream) — the association is
  /// immediately usable and the session layer above defines reachability.
  ///
  /// Returns the path's id ([pathId]), or null when no path was opened: no
  /// usable route, or an address under no underlay the layer runs. A path of
  /// that session standing toward another address is moved to this one.
  Future<String?> connectToPeer(
      String pubkeyHex, InternetAddress addr, int port) async {
    final session = _openableSession(addr, port, pubkeyHex);
    if (session == null) return null;
    final id = pathId(session, pubkeyHex);
    final isNew = !_peerPaths.containsKey(id);
    _setPath(id, _UdpPath(AddressInfo(addr, port), session));
    if (isNew && !_connectionController.isClosed) {
      _connectionController.add(TransportConnectionEvent(
        peerId: id,
        transport: TransportType.udp,
        connected: true,
        isIncoming: false,
      ));
    }
    return id;
  }

  /// Open a temp path to [address] for a peer whose key is not known yet (an
  /// outbound LAN cold call): its id is the address itself, the convention
  /// the inbound path uses, so the ANNOUNCE that names the peer re-keys it
  /// ([mapIncomingConnectionToPubkey]). Returns whether the path is open.
  Future<bool> connectToAddress(AddressInfo address) async {
    final id = address.toAddressString();
    final session = _openableSession(address.ip, address.port, id);
    if (session == null) return false;
    final isNew = !_peerPaths.containsKey(id);
    _setPath(id, _UdpPath(address, session));
    if (isNew && !_connectionController.isClosed) {
      _connectionController.add(TransportConnectionEvent(
        peerId: id,
        transport: TransportType.udp,
        connected: true,
        isIncoming: false,
      ));
    }
    return true;
  }

  /// The session a path to [addr] would belong to, or null — with
  /// [lastConnectFailureKind] set — when none can be opened.
  PeerTransport? _openableSession(InternetAddress addr, int port, String to) {
    _lastConnectFailureKind = null;
    if (!canDialAddress(addr)) {
      _lastConnectFailureKind = UdpConnectFailureKind.networkUnreachable;
      debugPrint('Cannot associate $to: no usable '
          '${addr.type == InternetAddressType.IPv6 ? "IPv6" : "IPv4"} '
          'route for ${addr.address}:$port');
      return null;
    }
    final session = sessionOf(addr);
    if (session == null) {
      _lastConnectFailureKind = UdpConnectFailureKind.other;
      debugPrint('Cannot associate $to: ${addr.address}:$port falls under no '
          'underlay this layer runs');
    }
    return session;
  }

  /// The coordinator identified the sender behind [peerId] (verified
  /// ANNOUNCE or session-authenticated packet): key its path by [pathId] of
  /// the path's session and [pubkeyHex], and return that id. The temp id is
  /// retained as an alias to the same address — the datagram receive loop can
  /// drain several packets from one peer before the identifying one is
  /// processed, so an in-flight handler still holding the temp id must keep
  /// resolving to a live path. Returns [peerId] itself when it names no path.
  String mapIncomingConnectionToPubkey(String peerId, String pubkeyHex) {
    final path = _peerPaths[peerId];
    if (path == null) return peerId;
    final id = pathId(path.session, pubkeyHex);
    if (id == peerId) return id;
    debugPrint('Mapped incoming path $peerId → $id');
    final hadPath = _peerPaths.containsKey(id);
    // Re-key the reverse map to the identity, but keep the temp→address
    // forward entry as an alias for outbound sends.
    _peerPaths[id] = path;
    _addressToPeerId[path.addressKey] = id;
    if (!hadPath && !_connectionController.isClosed) {
      _connectionController.add(TransportConnectionEvent(
        peerId: id,
        transport: TransportType.udp,
        connected: true,
        isIncoming: true,
      ));
    }
    return id;
  }

  /// Drop the path [peerId] — a [pathId], or a temp id.
  Future<void> disconnectFromPeer(String peerId) async {
    final path = _peerPaths.remove(peerId);
    if (path == null) return;
    if (_addressToPeerId[path.addressKey] == peerId) {
      _addressToPeerId.remove(path.addressKey);
    }
    if (!_connectionController.isClosed) {
      _connectionController.add(TransportConnectionEvent(
        peerId: peerId,
        transport: TransportType.udp,
        connected: false,
        reason: 'Disconnected',
      ));
    }
    debugPrint('Disconnected from peer $peerId');
  }

  void _setPath(String peerId, _UdpPath path) {
    final previous = _peerPaths[peerId];
    if (previous != null) {
      _addressToPeerId.remove(previous.addressKey);
    }
    _peerPaths[peerId] = path;
    _addressToPeerId[path.addressKey] = peerId;
  }

  @override
  void associatePeerWithPubkey(String peerId, Uint8List pubkey) {
    // Paths are re-keyed via mapIncomingConnectionToPubkey.
  }

  /// The id of [pubkeyHex]'s identified path of [session], or null when it
  /// holds none.
  String? pathIdFor(PeerTransport session, String pubkeyHex) {
    final id = pathId(session, pubkeyHex);
    return _peerPaths.containsKey(id) ? id : null;
  }

  /// Whether [pubkeyHex] holds an identified path of [session].
  bool hasPath(PeerTransport session, String pubkeyHex) =>
      pathIdFor(session, pubkeyHex) != null;

  /// The IP session of the path [peerId], or null when no such path stands.
  PeerTransport? sessionOfPath(String peerId) => _peerPaths[peerId]?.session;

  @override
  Uint8List? getPubkeyForPeerId(String peerId) {
    final parsed = parsePathId(peerId);
    if (parsed == null || !_peerPaths.containsKey(peerId)) return null;
    final hex = parsed.pubkeyHex;
    final bytes = Uint8List(32);
    for (var i = 0; i < 32; i++) {
      bytes[i] = int.parse(hex.substring(2 * i, 2 * i + 2), radix: 16);
    }
    return bytes;
  }

  /// The peer id currently bound to [addressKey], or null when nothing is.
  ///
  /// A [pathId] once the peer at that address has been identified, and the
  /// address itself while the path is still a temp one. A caller about to open
  /// a path to an address uses this to see whether one already exists — under
  /// either name — rather than dialing a second time.
  String? peerIdForAddress(String addressKey) => _addressToPeerId[addressKey];

  /// The address a peer's datagrams come from / go to.
  ({InternetAddress ip, int port})? getRemoteAddress(String peerId) {
    final path = _peerPaths[peerId];
    if (path == null) return null;
    return (ip: path.address.ip, port: path.address.port);
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
    final address = path.address;
    final socket = _rawSockets[address.ip.type];
    if (socket == null) {
      debugPrint('Cannot send to $peerId: no '
          '${address.ip.type == InternetAddressType.IPv6 ? "IPv6" : "IPv4"} '
          'socket');
      return false;
    }
    try {
      return socket.send(data, address.ip, address.port) > 0;
    } catch (e) {
      debugPrint('Failed to send to peer $peerId: $e');
      return false;
    }
  }

  @override
  Future<void> broadcast(Uint8List data, {Set<String>? excludePeerIds}) async {
    for (final peerId in _peerPaths.keys.toList()) {
      if (_isTemp(peerId)) continue; // unidentified paths
      if (excludePeerIds != null && excludePeerIds.contains(peerId)) continue;
      await sendToPeer(peerId, data);
    }
  }

  void _handleDatagram(Datagram datagram) {
    final data = datagram.data;
    // Punch packets open NAT mappings; nothing to route.
    if (PunchPacket.isPunch(data)) return;
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
      // The source address decides the session the contact belongs to
      // (decision 8); one under no underlay the layer runs has none.
      final session = sessionOf(datagram.address);
      if (session == null) return;
      peerId = addressKey;
      _setPath(peerId, _UdpPath(address, session));
      debugPrint('Inbound datagram path from $addressKey (${session.name})');
    }
    final path = _peerPaths[peerId];
    if (path == null) return;
    if (_isTemp(peerId)) {
      _tempPathLastSeen[peerId] = DateTime.now();
    }

    if (!_dataController.isClosed) {
      _dataController.add(TransportDataEvent(
        peerId: peerId,
        transport: TransportType.udp,
        data: data,
      ));
    }
    onUdpDataReceived?.call(path.session, peerId, data);
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

/// A UDP path: the peer's address, and the IP session it belongs to, fixed
/// when the path is opened.
class _UdpPath {
  final AddressInfo address;
  final PeerTransport session;

  _UdpPath(this.address, this.session);

  String get addressKey => address.toAddressString();
}
