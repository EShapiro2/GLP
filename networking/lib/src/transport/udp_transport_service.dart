import 'dart:async';
import 'dart:io';
import 'dart:typed_data';
import 'package:flutter/material.dart';
import 'package:redux/redux.dart';
import 'package:grassroots_dart_udx/grassroots_dart_udx.dart';

import '../transport/transport_service.dart';
import '../transport/address_utils.dart';
import '../models/identity.dart';
import '../models/packet.dart';
import '../protocol/protocol_handler.dart';
import '../store/store.dart';

enum UdpConnectFailureKind {
  networkUnreachable,
  handshakeTimeout,
  other,
}

/// Display info for UDP transport
const _defaultUdpDisplayInfo = TransportDisplayInfo(
  icon: Icons.public,
  name: 'Internet',
  description: 'Direct UDP peer-to-peer transport',
  color: Colors.green,
);

/// UDP transport service using grassroots_dart_udx for reliable streams over UDP.
///
/// Uses Grassroots's Ed25519 identity directly. Addressing uses simple
/// ip:port strings.
///
/// ## Lifecycle
///
/// Two-phase initialization to support hole-punching:
///
/// 1. [initialize] — Binds a `RawDatagramSocket`. At this point the raw socket
///    is available for hole-punch packets via [rawSocket].
///
/// 2. [startMultiplexer] — Creates `UDXMultiplexer` on the same socket.
///    After this, all incoming UDP reads go through UDX. Stray non-UDX packets
///    (e.g. residual punch packets) are silently dropped.
///
/// For well-connected peers (no hole-punch needed), both phases happen immediately.
/// For NATed peers, phase 2 happens after hole-punch succeeds.
///
/// ## Connection Identity
///
/// The first message on any new UDX stream MUST be a GrassrootsPacket of type ANNOUNCE.
/// This allows the receiver to map the UDX connection to a Grassroots public key.
/// (Future: Noise XX handshake will replace this.)
///
/// ## No Store-and-Forward
///
/// Messages to unreachable peers fail immediately. No caching, no relaying.
class UdpTransportService extends TransportService {
  static const Duration _defaultHandshakeTimeout = Duration(seconds: 10);

  /// Our Grassroots identity (Ed25519 keypair)
  final GrassrootsIdentity identity;

  /// Redux store for peer state
  final Store<AppState> store;

  /// Protocol handler for encoding/decoding
  final ProtocolHandler protocolHandler;

  // --- Socket and UDX state ---

  /// Preferred raw UDP socket. We own it — UDX wraps it but doesn't create it.
  RawDatagramSocket? _rawSocket;

  /// Raw UDP sockets by address family.
  final Map<InternetAddressType, RawDatagramSocket> _rawSockets = {};

  /// UDX factory instance
  UDX? _udx;

  /// Multiplexers by address family.
  final Map<InternetAddressType, UDXMultiplexer> _multiplexers = {};

  /// Current transport state
  TransportState _state = TransportState.uninitialized;

  /// Our bound local port (available after [initialize])
  int? _localPort;
  InternetAddressType? _activeAddressType;
  final Map<InternetAddressType, int> _localPorts = {};

  // --- Peer connections ---

  /// Active UDX connections per peer, keyed by pubkey hex.
  final Map<String, _PeerConnection> _peerConnections = {};

  // Per-multiplexer monotonic stream-id counter. UDX peers refuse new SYNs
  // for a stream-id whose prior stream just closed (the id is in tear-down
  // state for some time). If we reused id=1 for every reconnect to the same
  // remote, the second add of a rendezvous server would be silently dropped
  // by the server's UDX layer. Bumping the id per connect avoids the
  // collision entirely.
  int _nextStreamId = 1;

  // --- Stream controllers ---

  final _stateController = StreamController<TransportState>.broadcast();
  final _dataController = StreamController<TransportDataEvent>.broadcast();
  final _connectionController =
      StreamController<TransportConnectionEvent>.broadcast();

  // --- Subscriptions ---

  final Map<InternetAddressType, StreamSubscription>
      _multiplexerConnectionsSubs = {};

  // --- Public callbacks ---

  /// Called when data is received from a UDP peer.
  /// The coordinator deserializes as GrassrootsPacket and routes via MessageRouter.
  void Function(String pubkeyHex, Uint8List data)? onUdpDataReceived;

  /// Timeout for UDX handshake completion.
  final Duration connectHandshakeTimeout;

  UdpConnectFailureKind? _lastConnectFailureKind;

  UdpTransportService({
    required this.identity,
    required this.store,
    required this.protocolHandler,
    this.connectHandshakeTimeout = _defaultHandshakeTimeout,
  });

  // ===== Public Getters =====

  /// The raw UDP socket — exposed for hole-punch service to send punch packets.
  /// Only use for raw sends; DO NOT read from this after [startMultiplexer].
  RawDatagramSocket? get rawSocket => _rawSocket;

  /// Raw UDP sockets keyed by IP family.
  Map<InternetAddressType, RawDatagramSocket> get rawSocketsByType =>
      Map.unmodifiable(_rawSockets);

  /// Our bound port (available after [initialize])
  int? get localPort => _localPort;

  /// Bound local port for [type], if that family initialized successfully.
  int? localPortForAddressType(InternetAddressType type) => _localPorts[type];

  /// The preferred active IP family for the bound UDP socket.
  InternetAddressType? get activeAddressType => _activeAddressType;

  /// Every active IP family with a bound UDP socket.
  Set<InternetAddressType> get activeAddressTypes =>
      Set.unmodifiable(_rawSockets.keys);

  /// Whether we currently have a usable local route for the active family.
  ///
  /// True once we've bound at least one UDP socket. We don't enumerate
  /// interfaces to "verify" reachability — that was unreliable on multi-homed
  /// devices (e.g. the first non-link-local address from NetworkInterface.list
  /// wasn't always the one the kernel actually uses for outbound traffic).
  /// The canonical "can we be reached" answer comes from the reflected
  /// public address, which is updated asynchronously via signaling.
  bool get hasUsableRoute => _activeAddressType != null;

  /// Classification of the most recent outbound connect failure, if any.
  UdpConnectFailureKind? get lastConnectFailureKind => _lastConnectFailureKind;

  /// Whether the UDX multiplexer is active (accepting streams)
  bool get isMultiplexerActive => _multiplexers.isNotEmpty;

  // ===== TransportService Implementation =====

  @override
  TransportType get type => TransportType.udp;
  @override
  TransportDisplayInfo get displayInfo => _defaultUdpDisplayInfo;

  @override
  TransportState get state => _state;

  @override
  Stream<TransportDataEvent> get dataStream => _dataController.stream;

  @override
  Stream<TransportConnectionEvent> get connectionStream =>
      _connectionController.stream;

  @override
  int get connectedCount => _peerConnections.length;

  @override
  bool get isActive => _state == TransportState.active;

  // ===== Lifecycle =====

  /// Phase 1: Bind the raw UDP socket.
  ///
  /// After this call:
  /// - [rawSocket] is available for sending hole-punch packets
  /// - [localPort] is known
  /// - The multiplexer is NOT yet created (call [startMultiplexer] for that)
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
      _udx = UDX();

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

  /// Phase 2: Create UDX multiplexer on the bound socket.
  ///
  /// After this call:
  /// - All incoming UDP reads go through UDX
  /// - DO NOT read from [rawSocket] directly (multiplexer owns reads)
  /// - You CAN still send raw bytes via [rawSocket.send()] (for punch packets)
  /// - Incoming non-UDX packets are silently dropped
  ///
  /// Call this:
  /// - Immediately after [initialize] if well-connected (no NAT)
  /// - After hole-punch succeeds if behind NAT
  void startMultiplexer() {
    if (_multiplexers.length == _rawSockets.length &&
        _multiplexers.isNotEmpty) {
      debugPrint('Multiplexer already started');
      return;
    }
    if (_rawSockets.isEmpty) {
      debugPrint(
          'Cannot start multiplexer: socket not bound. Call initialize() first.');
      return;
    }

    for (final entry in _rawSockets.entries) {
      final family = entry.key;
      if (_multiplexers.containsKey(family)) continue;

      final multiplexer = UDXMultiplexer(entry.value);
      _multiplexers[family] = multiplexer;

      // Handle non-UDX packets (raw UDP fallback for when UDX handshake fails)
      multiplexer.onRawPacket = _handleRawPacket;

      // Listen for incoming connections from remote peers
      _multiplexerConnectionsSubs[family] =
          multiplexer.connections.listen(_handleIncomingConnection);
    }

    _refreshPreferredSocket();

    _setState(TransportState.active);
    debugPrint(
      'UDX multiplexers started on '
      '${_multiplexers.keys.map((family) => family == InternetAddressType.IPv6 ? "IPv6" : "IPv4").join(", ")}',
    );
  }

  @override
  Future<void> start() async {
    // For compatibility with TransportService interface.
    // If multiplexer isn't started yet, start it now.
    if (_multiplexers.isEmpty && _rawSockets.isNotEmpty) {
      startMultiplexer();
    } else if (_state == TransportState.ready) {
      _setState(TransportState.active);
    }
  }

  @override
  Future<void> stop() async {
    debugPrint('Stopping UDP transport');

    // Close all peer connections (copy keys to avoid concurrent modification)
    final peerKeys = _peerConnections.keys.toList();
    for (final key in peerKeys) {
      final conn = _peerConnections.remove(key);
      try {
        await conn?.stream?.close();
      } catch (e) {
        debugPrint('Error closing stream for $key: $e');
      }
    }

    // Cancel multiplexer subscriptions
    for (final sub in _multiplexerConnectionsSubs.values) {
      await sub.cancel();
    }
    _multiplexerConnectionsSubs.clear();

    // Clear connection tracking maps
    _tempKeyToPubkey.clear();
    _addressToPubkey.clear();
    _pendingIncoming.clear();

    // We don't close the raw socket here — it might be reused.
    // The multiplexer is discarded; a new one can be created.
    _multiplexers.clear();

    if (_state == TransportState.active) {
      _setState(TransportState.ready);
    }
    debugPrint('UDP transport stopped');
  }

  @override
  Future<void> dispose() async {
    debugPrint('Disposing UDP transport');
    await stop();

    // Close the raw sockets
    for (final socket in _rawSockets.values) {
      socket.close();
    }
    _rawSockets.clear();
    _localPorts.clear();
    _rawSocket = null;
    _udx = null;
    _localPort = null;
    _activeAddressType = null;

    _state = TransportState.disposed;

    await _stateController.close();
    await _dataController.close();
    await _connectionController.close();
  }

  // ===== Connections =====

  /// Connect to a peer at a known ip:port.
  ///
  /// Creates a UDX connection (UDPSocket) and stream to the peer.
  /// The first message sent MUST be an ANNOUNCE packet (caller's responsibility).
  ///
  /// Returns true if the connection was established.
  Future<bool> connectToPeer(
      String pubkeyHex, InternetAddress addr, int port) async {
    _lastConnectFailureKind = null;

    final multiplexer = _multiplexers[addr.type];
    if (multiplexer == null) {
      debugPrint(
          'Cannot connect: ${addr.type == InternetAddressType.IPv6 ? "IPv6" : "IPv4"} '
          'multiplexer not started. Call startMultiplexer() first.');
      return false;
    }

    if (_peerConnections.containsKey(pubkeyHex)) {
      debugPrint('Already connected to $pubkeyHex');
      return true;
    }

    if (!canDialAddress(addr)) {
      _lastConnectFailureKind = UdpConnectFailureKind.networkUnreachable;
      debugPrint('Cannot connect to $pubkeyHex: no usable '
          '${addr.type == InternetAddressType.IPv6 ? "IPv6" : "IPv4"} '
          'route for ${addr.address}:$port');
      return false;
    }

    UDPSocket? udpSocket;
    StreamSubscription? socketErrorsSub;
    Object? lastSocketError;
    final connectCompleter = Completer<UDXStream>();
    try {
      final remoteHost = addr.address;

      // Store address → pubkey mapping so incoming connections from this
      // address can be immediately associated with the correct peer.
      _addressToPubkey['$remoteHost:$port'] = pubkeyHex;

      // Create UDX connection to the peer
      udpSocket = multiplexer.createSocket(_udx!, remoteHost, port);
      final socket = udpSocket;
      socketErrorsSub = udpSocket.on('error').listen((event) {
        final data = event.data;
        if (data is Map && data['error'] != null) {
          lastSocketError = data['error'];
          if (!connectCompleter.isCompleted) {
            connectCompleter.completeError(data['error']);
          }
        }
      });

      // Create the outgoing stream and wait for handshake inside a guarded
      // zone so low-level async socket send failures are captured as a normal
      // connect failure instead of escaping to Flutter's top-level handler.
      final streamId = _nextStreamId++;
      runZonedGuarded(() async {
        try {
          final stream = await UDXStream.createOutgoing(
            _udx!,
            socket,
            streamId,
            streamId,
            remoteHost,
            port,
          );

          // Wait for UDX handshake to complete, with timeout.
          // Without this timeout, the await hangs forever if the remote is
          // unreachable (firewall, wrong address), leaking UDX sockets and
          // preventing the auto-connect from ever succeeding.
          await socket.handshakeComplete.timeout(
            connectHandshakeTimeout,
            onTimeout: () {
              throw TimeoutException(
                'UDX handshake timed out after '
                '${connectHandshakeTimeout.inSeconds}s',
              );
            },
          );
          if (!connectCompleter.isCompleted) {
            connectCompleter.complete(stream);
          }
        } catch (error, stackTrace) {
          lastSocketError ??= error;
          if (!connectCompleter.isCompleted) {
            connectCompleter.completeError(error, stackTrace);
          }
        }
      }, (error, stackTrace) {
        lastSocketError ??= error;
        if (!connectCompleter.isCompleted) {
          connectCompleter.completeError(error, stackTrace);
        }
      });

      final stream = await connectCompleter.future;

      // Store the connection
      _peerConnections[pubkeyHex] = _PeerConnection(
        pubkeyHex: pubkeyHex,
        udpSocket: udpSocket,
        stream: stream,
        addr: addr,
        port: port,
      );

      // Listen for data on the outgoing stream (receives paired incoming data)
      _listenToStream(pubkeyHex, stream);

      // Also listen for additional incoming streams on this UDPSocket.
      // In a simultaneous open, the remote peer might send data on our
      // connection rather than creating their own.
      udpSocket.on('stream').listen((UDXEvent event) {
        final incomingStream = event.data as UDXStream;
        if (incomingStream != stream) {
          debugPrint(
              'Incoming stream ${incomingStream.id} on outgoing connection to $pubkeyHex');
          _listenToStream(pubkeyHex, incomingStream);
        }
      });
      udpSocket.flushStreamBuffer();

      debugPrint('Connected to peer $pubkeyHex at $remoteHost:$port');

      _connectionController.add(TransportConnectionEvent(
        peerId: pubkeyHex,
        transport: TransportType.udp,
        connected: true,
        isIncoming: false,
      ));

      _lastConnectFailureKind = null;
      return true;
    } catch (e) {
      _lastConnectFailureKind = _classifyConnectFailure(lastSocketError ?? e);
      debugPrint('Failed to connect to peer $pubkeyHex: $e');

      // Clean up the UDX socket on failure to prevent resource leaks.
      // Without this, each failed attempt leaves a dangling socket in the
      // multiplexer that never gets garbage collected.
      if (udpSocket != null) {
        try {
          await udpSocket.close();
        } catch (_) {}
      }
      // Remove stale address mapping
      _addressToPubkey.remove('${addr.address}:$port');

      return false;
    } finally {
      await socketErrorsSub?.cancel();
    }
  }

  @override
  Future<bool> sendToPeer(String peerId, Uint8List data) async {
    final conn = _peerConnections[peerId];
    if (conn == null || conn.stream == null) {
      debugPrint('Cannot send to $peerId: not connected');
      return false;
    }

    try {
      await conn.stream!.add(data);
      return true;
    } catch (e) {
      debugPrint('Failed to send to peer $peerId: $e');
      return false;
    }
  }

  // ===== Raw UDP fallback (bypasses UDX) =====

  /// Peers we communicate with via raw UDP (when UDX handshake fails).
  /// Key: pubkeyHex, Value: target address info.
  final Map<String, AddressInfo> _rawPeerAddresses = {};

  /// Send data via raw UDP to a peer, bypassing UDX entirely.
  ///
  /// Use this when UDX connectToPeer fails (e.g. hairpin routing on same LAN).
  /// No reliability — packets may be lost, duplicated, or reordered.
  /// The application layer handles retries via ACKs.
  bool sendRawTo(
      String pubkeyHex, InternetAddress ip, int port, Uint8List data) {
    final socket = _rawSockets[ip.type];
    if (socket == null) return false;
    try {
      final sent = socket.send(data, ip, port);
      if (sent > 0) {
        // Track this as a raw peer so broadcast can include them
        _rawPeerAddresses[pubkeyHex] = AddressInfo(ip, port);
        return true;
      }
      return false;
    } catch (e) {
      debugPrint('Raw UDP send failed to $pubkeyHex: $e');
      return false;
    }
  }

  /// Check if a peer has a raw UDP address (fallback mode).
  AddressInfo? getRawPeerAddress(String pubkeyHex) =>
      _rawPeerAddresses[pubkeyHex];

  @override
  Future<void> broadcast(Uint8List data, {Set<String>? excludePeerIds}) async {
    // Send via UDX connections
    for (final entry in _peerConnections.entries) {
      if (excludePeerIds != null && excludePeerIds.contains(entry.key)) {
        continue;
      }
      await sendToPeer(entry.key, data);
    }
    // Also send via raw UDP to peers where UDX failed
    for (final entry in _rawPeerAddresses.entries) {
      if (excludePeerIds != null && excludePeerIds.contains(entry.key)) {
        continue;
      }
      if (_peerConnections.containsKey(entry.key)) {
        continue; // Already sent via UDX
      }
      sendRawTo(entry.key, entry.value.ip, entry.value.port, data);
    }
  }

  /// Disconnect from a specific peer.
  Future<void> disconnectFromPeer(String pubkeyHex) async {
    final conn = _peerConnections.remove(pubkeyHex);
    if (conn == null) return;

    // Clean up address mapping
    _addressToPubkey.removeWhere((_, v) => v == pubkeyHex);
    // Clean up tempKey mapping
    _tempKeyToPubkey.removeWhere((_, v) => v == pubkeyHex);

    try {
      await conn.stream?.close();
    } catch (e) {
      debugPrint('Error closing stream for $pubkeyHex: $e');
    }

    _connectionController.add(TransportConnectionEvent(
      peerId: pubkeyHex,
      transport: TransportType.udp,
      connected: false,
      reason: 'Disconnected by request',
    ));

    debugPrint('Disconnected from peer $pubkeyHex');
  }

  @override
  void associatePeerWithPubkey(String peerId, Uint8List pubkey) {
    // Peer connections are already keyed by pubkey hex.
    // This is used for incoming connections where we learn the pubkey from ANNOUNCE.
    debugPrint('associatePeerWithPubkey: $peerId (managed via ANNOUNCE)');
  }

  @override
  String? getPeerIdForPubkey(Uint8List pubkey) {
    final hex = pubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
    return _peerConnections.containsKey(hex) ? hex : null;
  }

  @override
  Uint8List? getPubkeyForPeerId(String peerId) {
    final conn = _peerConnections[peerId];
    if (conn == null) return null;
    // peerId IS the pubkeyHex in our case
    final bytes = <int>[];
    for (var i = 0; i < peerId.length; i += 2) {
      bytes.add(int.parse(peerId.substring(i, i + 2), radix: 16));
    }
    return Uint8List.fromList(bytes);
  }

  /// Get the observed remote address for a connected peer.
  ///
  /// Returns the (ip, port) as seen on the UDX connection — this is the
  /// peer's NAT-translated public address. Used by the signaling service
  /// to reflect the peer's true external address back to them.
  ///
  /// Returns null if the peer is not connected.
  ({InternetAddress ip, int port})? getRemoteAddress(String pubkeyHex) {
    final conn = _peerConnections[pubkeyHex];
    if (conn == null) return null;
    return (ip: conn.addr, port: conn.port);
  }

  // ===== Incoming Connection Handling =====

  /// Handle a new incoming UDX connection (remote peer connected to us).
  void _handleIncomingConnection(UDPSocket socket) {
    // Normalize address string to match the format stored by connectToPeer.
    final addrStr = socket.remoteAddress.address;
    final remoteAddr = '$addrStr:${socket.remotePort}';
    debugPrint('Incoming UDX connection from $remoteAddr');

    // Check if we already know who this peer is (from a prior connectToPeer call).
    // If so, bypass the tempKey indirection and set up the listener with the
    // correct pubkey immediately. This avoids the timing race where data
    // arrives before ANNOUNCE processing can map the tempKey.
    final knownPubkey = _addressToPubkey[remoteAddr];
    if (knownPubkey != null) {
      debugPrint(
          'Known peer $knownPubkey at $remoteAddr, using direct stream listener');
      socket.on('stream').listen((UDXEvent event) {
        final stream = event.data as UDXStream;
        _listenToStream(knownPubkey, stream);
      });
      socket.flushStreamBuffer();
      return;
    }

    // Unknown peer — use tempKey-based handling until ANNOUNCE reveals pubkey
    socket.on('stream').listen((UDXEvent event) {
      final stream = event.data as UDXStream;
      _handleIncomingStream(socket, stream);
    });

    // Flush any buffered streams (race condition fix from grassroots_dart_udx)
    socket.flushStreamBuffer();
  }

  /// Handle an incoming stream on a connection.
  ///
  /// Any verified GrassrootsPacket identifies the sender via its header pubkey.
  /// The coordinator maps the connection after verifying the first packet.
  void _handleIncomingStream(UDPSocket socket, UDXStream stream) {
    debugPrint('Incoming UDX stream ${stream.id}');

    // Listen for data on this stream.
    // We don't know the pubkey yet, so we use a temporary key and let the
    // coordinator re-map after processing the first verified packet.
    final tempKey = '${socket.remoteAddress}:${socket.remotePort}:${stream.id}';

    // Store as pending until ANNOUNCE reveals the pubkey
    _pendingIncoming[tempKey] = _PeerConnection(
      pubkeyHex: '', // unknown yet
      udpSocket: socket,
      stream: stream,
      addr: socket.remoteAddress,
      port: socket.remotePort,
    );

    final framer = _PacketFramer();
    stream.data.listen(
      (Uint8List data) {
        if (data.isEmpty) return;

        // Use mapped pubkey hex if ANNOUNCE has been processed, otherwise tempKey.
        // This ensures ACKs and subsequent messages use the correct peer ID
        // that matches _peerConnections.
        final effectiveId = _tempKeyToPubkey[tempKey] ?? tempKey;

        for (final packetBytes in framer.ingest(data)) {
          _dataController.add(TransportDataEvent(
            peerId: effectiveId,
            transport: TransportType.udp,
            data: packetBytes,
          ));
          // Forward to coordinator for deserialization and routing. The
          // coordinator may call back with the pubkey after ANNOUNCE
          // processing, and we'll remap the connection.
          onUdpDataReceived?.call(effectiveId, packetBytes);
        }
      },
      onError: (e) {
        debugPrint('⚠️ UDX stream error from $tempKey: $e');
      },
      onDone: () {
        debugPrint('⚠️ UDX stream closed from $tempKey');
        framer.reset();
        // If we have a peer mapped to this temp key, clean up
        final pubkeyHex = _tempKeyToPubkey.remove(tempKey);
        if (pubkeyHex != null) {
          _peerConnections.remove(pubkeyHex);
          _connectionController.add(TransportConnectionEvent(
            peerId: pubkeyHex,
            transport: TransportType.udp,
            connected: false,
            reason: 'Stream closed',
          ));
        }
      },
    );
  }

  /// Reverse map: temp connection key → pubkey hex.
  /// Populated when ANNOUNCE is processed and we learn who connected to us.
  final Map<String, String> _tempKeyToPubkey = {};

  /// Reverse map: "remoteAddress:remotePort" → pubkey hex.
  /// Populated by [connectToPeer] so incoming connections from known addresses
  /// can be immediately associated with the correct pubkey, bypassing the
  /// tempKey indirection and avoiding the timing race where data arrives
  /// before ANNOUNCE processing completes.
  final Map<String, String> _addressToPubkey = {};

  /// Pending incoming connections not yet mapped to a pubkey.
  /// Keyed by tempKey, contains the UDPSocket + UDXStream.
  final Map<String, _PeerConnection> _pendingIncoming = {};

  /// Called by the coordinator after verifying a packet from an incoming connection.
  ///
  /// Maps the temporary connection key to the peer's pubkey hex.
  /// The coordinator calls this with the tempKey it received via [onUdpDataReceived]
  /// and the pubkey extracted from the verified packet's header.
  void mapIncomingConnectionToPubkey(String tempKey, String pubkeyHex) {
    _tempKeyToPubkey[tempKey] = pubkeyHex;

    final pending = _pendingIncoming.remove(tempKey);
    if (pending == null) return;

    // If a prior connection for this pubkey is still installed, the peer is
    // reconnecting from a new address. Replace it: tear the old one down
    // (close stream + socket, drop address mapping, fire a disconnect event
    // so application-level state — Noise sessions, hole-punch trackers,
    // PeerUdpConnectionChangedAction — sees the transition) and install the
    // fresh entry. The outgoing-side connectToPeer path already overwrites
    // unconditionally; the incoming side needs to match so a peer that moves
    // (wifi → cellular, etc.) and re-dials us doesn't leave us with two
    // half-live sessions — one orphan stream feeding receive-side
    // lastUdpSeen refreshes, the other a dead socket every sendToPeer
    // disappears into.
    final existing = _peerConnections[pubkeyHex];
    if (existing != null) {
      _addressToPubkey.remove('${existing.addr.address}:${existing.port}');

      // Tear down the prior session. The new incoming UDXSocket is a
      // distinct multiplexer entry (different source 5-tuple) so closing
      // the old socket can't take the new one with it; guard anyway.
      final closeStream = existing.stream;
      if (closeStream != null && closeStream != pending.stream) {
        unawaited(closeStream.close().catchError((Object e) {
          debugPrint('Error closing stale stream for $pubkeyHex: $e');
        }));
      }
      if (!identical(existing.udpSocket, pending.udpSocket)) {
        unawaited(existing.udpSocket.close().catchError((Object e) {
          debugPrint('Error closing stale UDPSocket for $pubkeyHex: $e');
        }));
      }

      _connectionController.add(TransportConnectionEvent(
        peerId: pubkeyHex,
        transport: TransportType.udp,
        connected: false,
        reason: 'Replaced by incoming reconnect from '
            '${pending.addr.address}:${pending.port}',
      ));
      debugPrint(
        'Replacing stale UDP session for $pubkeyHex '
        '(old=${existing.addr.address}:${existing.port}, '
        'new=${pending.addr.address}:${pending.port})',
      );
    }

    _peerConnections[pubkeyHex] = _PeerConnection(
      pubkeyHex: pubkeyHex,
      udpSocket: pending.udpSocket,
      stream: pending.stream,
      addr: pending.addr,
      port: pending.port,
    );
    _addressToPubkey['${pending.addr.address}:${pending.port}'] = pubkeyHex;

    _connectionController.add(TransportConnectionEvent(
      peerId: pubkeyHex,
      transport: TransportType.udp,
      connected: true,
      isIncoming: true,
    ));

    debugPrint('Mapped incoming connection $tempKey → $pubkeyHex');
  }

  // ===== Raw UDP Fallback Receive =====

  /// Handle a non-UDX packet received on the multiplexer socket.
  ///
  /// This fires for packets that don't parse as valid UDX (e.g. GrassrootsPackets
  /// sent via [sendRawTo] from a peer where UDX handshake failed).
  /// Skip hole-punch packets (36 bytes with BCPU magic).
  void _handleRawPacket(Uint8List data, InternetAddress address, int port) {
    // Skip punch packets (36 bytes, magic "BCPU")
    if (data.length == 36 &&
        data[0] == 0x42 &&
        data[1] == 0x43 &&
        data[2] == 0x50 &&
        data[3] == 0x55) {
      return;
    }

    // Skip tiny packets that aren't meaningful
    if (data.length < 50) return;

    final addrStr = '${address.address}:$port';
    debugPrint('Raw UDP packet: ${data.length} bytes from $addrStr');

    // Try to identify the sender from _rawPeerAddresses (reverse lookup)
    String? senderPubkeyHex;
    for (final entry in _rawPeerAddresses.entries) {
      if (entry.value.ip.address == address.address &&
          entry.value.port == port) {
        senderPubkeyHex = entry.key;
        break;
      }
    }

    // Forward to coordinator for deserialization — use the pubkeyHex if known,
    // otherwise use the address string as a temporary ID.
    final effectiveId = senderPubkeyHex ?? addrStr;
    onUdpDataReceived?.call(effectiveId, data);
  }

  // ===== Stream Listening =====

  /// Listen for data on an outgoing stream (peer we connected to).
  void _listenToStream(String pubkeyHex, UDXStream stream) {
    final framer = _PacketFramer();
    stream.data.listen(
      (Uint8List data) {
        if (data.isEmpty) return;

        for (final packetBytes in framer.ingest(data)) {
          _dataController.add(TransportDataEvent(
            peerId: pubkeyHex,
            transport: TransportType.udp,
            data: packetBytes,
          ));
          onUdpDataReceived?.call(pubkeyHex, packetBytes);
        }
      },
      onError: (e) {
        debugPrint('⚠️ UDX stream error from $pubkeyHex: $e');
      },
      onDone: () {
        debugPrint('⚠️ UDX stream closed from $pubkeyHex');
        framer.reset();
        _peerConnections.remove(pubkeyHex);

        if (!_connectionController.isClosed) {
          _connectionController.add(TransportConnectionEvent(
            peerId: pubkeyHex,
            transport: TransportType.udp,
            connected: false,
            reason: 'Stream closed',
          ));
        }
      },
    );
  }

  /// Probe each bound RawDatagramSocket to verify it still accepts sends.
  /// On Android in particular, a socket that was live when the app went to
  /// background can come back in a permanently-EPERM state — the file
  /// descriptor is "open" but every sendto() fails (errno 1, Operation not
  /// permitted) because the OS policy that blocked sends during background
  /// poisoned the socket session. The only fix is close + rebind.
  ///
  /// Call this on app foreground. If any family is dead, the whole UDP
  /// transport is torn down and re-initialized — peer connections that
  /// were riding the dead multiplexer are closed and a disconnect event is
  /// emitted per peer so `GrassrootsNetwork` can re-establish RV sessions
  /// on the fresh sockets.
  ///
  /// Returns true if any rebind happened.
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
      final familyLabel =
          family == InternetAddressType.IPv6 ? "IPv6" : "IPv4";
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
        '${dead.length} dead family/families: '
        '${dead.map((f) => f == InternetAddressType.IPv6 ? "IPv6" : "IPv4").join(", ")}');

    // Tear down peer connections + multiplexer (keeps _rawSockets around so
    // stop() doesn't try to use them; we close those ourselves next).
    final closedPeers = _peerConnections.keys.toList();
    await stop();

    // Close every raw socket — easier to rebuild the entire stack than to
    // surgically rebind just the dead families.
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

    // Notify the coordinator that everyone got disconnected so it can
    // re-establish (the dead sockets took their peers down with them).
    for (final pubkeyHex in closedPeers) {
      if (!_connectionController.isClosed) {
        _connectionController.add(TransportConnectionEvent(
          peerId: pubkeyHex,
          transport: TransportType.udp,
          connected: false,
          reason: 'Transport rebound after socket death',
        ));
      }
    }

    _setState(TransportState.uninitialized);
    final ok = await initialize();
    if (!ok) {
      debugPrint('[probe] Re-initialize failed after socket death');
      return true;
    }
    startMultiplexer();
    return true;
  }

  // ===== Internal =====

  Future<void> _bindFamily(InternetAddressType type) async {
    final bindAddress = type == InternetAddressType.IPv6
        ? InternetAddress.anyIPv6
        : InternetAddress.anyIPv4;
    try {
      final socket = await RawDatagramSocket.bind(bindAddress, 0);
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
    if (_state != newState) {
      _state = newState;
      store.dispatch(UdpTransportStateChangedAction(newState));
      if (!_stateController.isClosed) {
        _stateController.add(newState);
      }
    }
  }

  bool canDialAddress(InternetAddress address) {
    if (!_rawSockets.containsKey(address.type)) return false;
    if (address.isLoopback) return true;
    return hasUsableRoute;
  }

  UdpConnectFailureKind _classifyConnectFailure(Object error) {
    if (error is TimeoutException) {
      return UdpConnectFailureKind.handshakeTimeout;
    }

    if (error is SocketException) {
      final code = error.osError?.errorCode;
      final details =
          '${error.message} ${error.osError?.message ?? ""}'.toLowerCase();
      if (code == 101 ||
          code == 113 ||
          code == 65 ||
          details.contains('network is unreachable') ||
          details.contains('no route to host') ||
          details.contains('unreachable')) {
        return UdpConnectFailureKind.networkUnreachable;
      }
    }

    return UdpConnectFailureKind.other;
  }
}

/// Internal connection state for a single peer.
class _PeerConnection {
  final String pubkeyHex;
  final UDPSocket udpSocket;
  final UDXStream? stream;
  final InternetAddress addr;
  final int port;

  _PeerConnection({
    required this.pubkeyHex,
    required this.udpSocket,
    this.stream,
    required this.addr,
    required this.port,
  });
}

/// Per-UDX-stream byte accumulator that slices the byte stream into
/// complete GrassrootsPackets using the on-wire payload-length field as
/// the frame delimiter.
///
/// UDX is a reliable byte stream (a `stream.add(N bytes)` is fragmented
/// into MTU-sized stream-frames and delivered to the receiver as several
/// `_dataController.add(chunk)` events). Without framing, the receiver
/// can't tell where one application packet ends and the next begins.
/// GrassrootsPacket already carries its own length at
/// `payloadLengthOffset`, so all the receiver has to do is buffer until
/// `headerSize + payloadLength` bytes are available, slice that out as
/// one packet, and keep any trailing bytes for the next iteration.
class _PacketFramer {
  Uint8List _buffer = Uint8List(0);

  /// Append a UDX stream chunk and yield all complete packets it
  /// completes. Returned packets are independent copies; the framer
  /// retains any partial trailing bytes for the next call.
  List<Uint8List> ingest(Uint8List chunk) {
    if (chunk.isEmpty) return const [];
    _append(chunk);
    final packets = <Uint8List>[];
    while (true) {
      if (_buffer.length < GrassrootsPacket.headerSize) break;
      final payloadLength = GrassrootsPacket.peekPayloadLength(_buffer);
      if (payloadLength == null) break;
      final total = GrassrootsPacket.headerSize + payloadLength;
      if (_buffer.length < total) break;
      packets.add(Uint8List.fromList(_buffer.sublist(0, total)));
      _buffer = _buffer.length == total
          ? Uint8List(0)
          : Uint8List.fromList(_buffer.sublist(total));
    }
    return packets;
  }

  /// Drop any partial-packet bytes — call when the stream closes so a
  /// future stream id collision (rare, but possible) can't get a stale
  /// header prefix from the previous session.
  void reset() {
    _buffer = Uint8List(0);
  }

  void _append(Uint8List chunk) {
    if (_buffer.isEmpty) {
      _buffer = Uint8List.fromList(chunk);
      return;
    }
    final combined = Uint8List(_buffer.length + chunk.length)
      ..setRange(0, _buffer.length, _buffer)
      ..setRange(_buffer.length, _buffer.length + chunk.length, chunk);
    _buffer = combined;
  }
}
