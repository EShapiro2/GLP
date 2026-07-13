import 'dart:async';
import 'dart:io';
import 'dart:typed_data';
import 'package:sodium/sodium_sumo.dart' as libsodium;

import 'address_table.dart';
import 'identity.dart';
import 'invite_table.dart';
import 'libsodium_loader.dart';
import 'noise_session_manager.dart';
import 'packet.dart';
import 'peer_table.dart';
import 'protocol.dart';
import 'signaling_codec.dart';
import 'signaling_handler.dart';

/// The GLP rendezvous server — a lightweight, publicly-accessible agent
/// that coordinates hole-punching between peers.
///
/// Spec alignment (§7.1):
/// - Has its own independent Ed25519 keypair (generated once, persisted).
/// - Has no friends list and does not participate in the social graph.
/// - Accepts cold-call connections from any agent.
/// - Verifies friendship proofs to confirm requesting agents are friends.
/// - Observes connecting agents' public addresses (peer_address/2).
/// - Coordinates UDP hole-punches by relaying addresses.
/// - Never relays message content — only signaling metadata flows through.
///
/// The architecture is federated: anyone can run a rendezvous server,
/// and agents may use multiple servers for redundancy.
///
/// The anchor listens on IPv6 and IPv4 when the host supports both families.
class AnchorServer {
  final int ipv6Port;
  final String nickname;
  final String identityPath;
  final int announceIntervalSeconds;

  late AnchorIdentity _identity;
  late Protocol _protocol;
  late PeerTable _peerTable;
  late AddressTable _addressTable;
  late InviteTable _inviteTable;
  late SignalingHandler _signalingHandler;
  late SignalingCodec _codec;
  late NoiseSessionManager _noiseSessions;

  final List<_AnchorListener> _listeners = [];

  /// Active UDX connections per peer, keyed by pubkey hex.
  final Map<String, _PeerConnection> _peerConnections = {};

  /// Reverse map: "ip:port" → pubkey hex.
  final Map<String, String> _addressToPubkey = {};

  /// Pending incoming connections not yet mapped to a pubkey.
  final Map<String, _PeerConnection> _pendingIncoming = {};

  Timer? _announceTimer;
  Timer? _staleCleanupTimer;
  Timer? _statsTimer;

  AnchorServer({
    required this.nickname,
    required this.identityPath,
    this.announceIntervalSeconds = 30,
    this.ipv6Port = 9516,
  });

  Future<void> start() async {
    _log('Starting GLP Rendezvous Server...');

    // Load or generate identity
    _identity = await AnchorIdentity.loadOrCreate(
      path: identityPath,
      nickname: nickname,
    );
    _log('Identity pubkey: ${_identity.pubkeyHex}');

    _protocol = Protocol(identity: _identity);
    _peerTable = PeerTable();
    _addressTable = AddressTable();
    _inviteTable = InviteTable();
    _codec = const SignalingCodec();

    final sodium = await libsodium.SodiumSumoInit.init(loadLibsodium);
    _noiseSessions =
        NoiseSessionManager(identity: _identity, sodium: sodium);
    _log('Noise static pubkey: '
        '${_pubkeyToHex(await _noiseSessions.staticPublicKey())}');

    _signalingHandler = SignalingHandler(
      protocol: _protocol,
      peerTable: _peerTable,
      addressTable: _addressTable,
      inviteTable: _inviteTable,
      codec: _codec,
    );
    _signalingHandler.sendSignaling = _sendSignaling;

    _listeners
      ..clear()
      ..addAll(await _bindListeners());
    if (_listeners.isEmpty) {
      throw StateError('Failed to bind any UDP listener');
    }
    for (final listener in _listeners) {
      listener.socketSub = listener.rawSocket.listen((event) {
        if (event != RawSocketEvent.read) return;
        Datagram? datagram;
        while ((datagram = listener.rawSocket.receive()) != null) {
          _handleDatagram(listener, datagram!);
        }
      });
      _log('UDP datagram listener on port ${listener.port} '
          '(${_familyLabel(listener.family)})');
    }

    // Periodic ANNOUNCE to all connected peers
    _announceTimer = Timer.periodic(
      Duration(seconds: announceIntervalSeconds),
      (_) => _broadcastAnnounce(),
    );

    // Periodic stale entry cleanup
    _staleCleanupTimer = Timer.periodic(
      const Duration(seconds: 60),
      (_) {
        // Datagrams emit no close events (spec §IP Connection: the UDP path
        // with its session is the connection in the liveness sense); expire
        // connections whose packets went silent, and pending unidentified
        // sources that never completed ANNOUNCE — the anchor is public, so
        // this map must not grow with scanner spray.
        final silentSince = DateTime.now().subtract(const Duration(minutes: 5));
        for (final entry in _peerConnections.entries.toList()) {
          if (entry.value.lastSeen.isBefore(silentSince)) {
            _forgetPeerConnection(entry.key, entry.value);
          }
        }
        _pendingIncoming.removeWhere(
            (_, pending) => pending.lastSeen.isBefore(silentSince));
        _addressTable.removeStale(
          const Duration(minutes: 5),
          protectedPubkeys: _peerConnections.keys.toSet(),
        );
        _peerTable.removeStale(const Duration(minutes: 30));
        _inviteTable.removeExpired();
        unawaited(_signalingHandler.retryUndeliveredInviteNotifications());
      },
    );

    // Periodic stats
    _statsTimer = Timer.periodic(
      const Duration(seconds: 60),
      (_) => _printStats(),
    );

    _log('Rendezvous server ready');
    for (final listener in _listeners) {
      _log('  ${_familyLabel(listener.family)} address: '
          '${listener.publicAddress}');
    }
    _log('  Pubkey:   ${_identity.pubkeyHex}');
    _log('Waiting for connections...');
  }

  Future<void> stop() async {
    _announceTimer?.cancel();
    _staleCleanupTimer?.cancel();
    _statsTimer?.cancel();
    for (final listener in _listeners) {
      await listener.socketSub?.cancel();
    }

    _peerConnections.clear();

    for (final listener in _listeners) {
      listener.rawSocket.close();
    }
    _listeners.clear();
    _log('Rendezvous server stopped');
  }

  // ===== Public Address Discovery =====

  Future<List<_AnchorListener>> _bindListeners() async {
    final listeners = <_AnchorListener>[];
    final ipv6 = await _tryBindListener(InternetAddressType.IPv6);
    if (ipv6 != null) listeners.add(ipv6);
    final ipv4 = await _tryBindListener(InternetAddressType.IPv4);
    if (ipv4 != null) listeners.add(ipv4);
    return listeners;
  }

  Future<_AnchorListener?> _tryBindListener(InternetAddressType family) async {
    final bindAddress = family == InternetAddressType.IPv6
        ? InternetAddress.anyIPv6
        : InternetAddress.anyIPv4;
    try {
      final socket = await RawDatagramSocket.bind(bindAddress, ipv6Port);
      final publicAddress = family == InternetAddressType.IPv6
          ? await _discoverPublicIpv6Address(ipv6Port)
          : await _discoverPublicIpv4Address(ipv6Port);
      return _AnchorListener(
        family: family,
        port: ipv6Port,
        rawSocket: socket,
        publicAddress: publicAddress ??
            (family == InternetAddressType.IPv6
                ? '[::]:$ipv6Port'
                : '0.0.0.0:$ipv6Port'),
      );
    } catch (e) {
      _log('Failed to bind ${_familyLabel(family)} UDP socket on '
          'port $ipv6Port: $e');
      return null;
    }
  }

  Future<String?> _discoverPublicIpv6Address(int listenerPort) async {
    // GCE assigns global IPv6 addresses directly to the interface, so
    // NetworkInterface.list() works.
    try {
      final interfaces = await NetworkInterface.list(
        type: InternetAddressType.IPv6,
        includeLoopback: false,
      );
      for (final iface in interfaces) {
        for (final addr in iface.addresses) {
          if (addr.isLoopback || addr.isLinkLocal) continue;
          final discovered = '[${addr.address}]:$listenerPort';
          _log('Discovered public IPv6 address: $discovered');
          return discovered;
        }
      }
    } catch (e) {
      _log('Failed to enumerate IPv6 interfaces: $e');
    }

    // GCE metadata server — IPv6 /96 prefix (we strip the trailing /96).
    try {
      final client = HttpClient();
      try {
        final request = await client.getUrl(Uri.parse(
          'http://metadata.google.internal/computeMetadata/v1/instance/'
          'network-interfaces/0/ipv6s',
        ));
        request.headers.set('Metadata-Flavor', 'Google');
        final response = await request.close();
        if (response.statusCode == 200) {
          final body =
              await response.transform(const SystemEncoding().decoder).join();
          var ip = body.trim();
          if (ip.contains('/')) ip = ip.split('/').first;
          final parsed = InternetAddress.tryParse(ip);
          if (parsed != null && parsed.type == InternetAddressType.IPv6) {
            final discovered = '[${parsed.address}]:$listenerPort';
            _log(
                'Discovered public IPv6 address via GCE metadata: $discovered');
            return discovered;
          }
        }
      } finally {
        client.close();
      }
    } catch (e) {
      _log('GCE metadata unavailable for IPv6: $e');
    }

    // Fallback: external service.
    try {
      final client = HttpClient();
      try {
        final request =
            await client.getUrl(Uri.parse('https://ipv6.seeip.org'));
        final response = await request.close();
        final body =
            await response.transform(const SystemEncoding().decoder).join();
        final ip = body.trim();
        final parsed = InternetAddress.tryParse(ip);
        if (parsed != null && parsed.type == InternetAddressType.IPv6) {
          final discovered = '[${parsed.address}]:$listenerPort';
          _log('Discovered public IPv6 address via seeip.org: $discovered');
          return discovered;
        }
      } finally {
        client.close();
      }
    } catch (e) {
      _log('Failed to discover public IPv6 address via seeip: $e');
    }

    return null;
  }

  Future<String?> _discoverPublicIpv4Address(int listenerPort) async {
    try {
      final client = HttpClient();
      try {
        final request =
            await client.getUrl(Uri.parse('https://ipv4.seeip.org'));
        final response = await request.close();
        final body =
            await response.transform(const SystemEncoding().decoder).join();
        final ip = body.trim();
        final parsed = InternetAddress.tryParse(ip);
        if (parsed != null && parsed.type == InternetAddressType.IPv4) {
          final discovered = '${parsed.address}:$listenerPort';
          _log('Discovered public IPv4 address via seeip.org: $discovered');
          return discovered;
        }
      } finally {
        client.close();
      }
    } catch (e) {
      _log('Failed to discover public IPv4 address via seeip: $e');
    }

    return null;
  }

  // ===== Datagram Handling =====

  void _handleDatagram(_AnchorListener listener, Datagram datagram) {
    final data = datagram.data;
    // Skip punch packets (36 bytes, magic "BCPU")
    if (data.length == 36 &&
        data[0] == 0x42 &&
        data[1] == 0x43 &&
        data[2] == 0x50 &&
        data[3] == 0x55) {
      return;
    }
    if (data.length < 5) return;

    final addrKey = '${datagram.address.address}:${datagram.port}';
    var peerId = _addressToPubkey[addrKey];
    if (peerId == null) {
      peerId = addrKey;
      _pendingIncoming.putIfAbsent(
        addrKey,
        () => _PeerConnection(
          pubkeyHex: '',
          addr: datagram.address,
          port: datagram.port,
          listenerFamily: listener.family,
        ),
      );
    } else {
      _peerConnections[peerId]?.lastSeen = DateTime.now();
    }

    unawaited(_processIncomingData(peerId, data,
        observedIp: datagram.address.address,
        observedPort: datagram.port,
        observedFamily: datagram.address.type));
  }

  // ===== Packet Processing =====

  Future<void> _processIncomingData(
    String peerId,
    Uint8List data, {
    String? observedIp,
    int? observedPort,
    InternetAddressType? observedFamily,
  }) async {
    GrassrootsPacket packet;
    try {
      packet = GrassrootsPacket.deserialize(data);
    } catch (e) {
      _log('Failed to deserialize packet from $peerId: $e');
      return;
    }

    // The wire frame carries no identity or signature. Sender identity comes
    // from the payload layer: ANNOUNCE is a self-signed record, the Noise
    // handshake envelope carries an authenticated claim, and secureSignaling
    // is authenticated by the Noise session of the connection it arrived on.
    switch (packet.type) {
      case PacketType.announce:
        await _handleAnnounce(packet, peerId,
            observedIp: observedIp, observedPort: observedPort);
      case PacketType.noiseHandshake:
        await _handleNoiseHandshake(packet, peerId);
      case PacketType.secureSignaling:
        await _handleSecureSignaling(packet, peerId,
            observedIp: observedIp, observedPort: observedPort);
      case PacketType.signaling:
        // Plaintext signaling is no longer accepted — clients must wrap
        // signaling in Noise (secureSignaling). Dropping silently is the
        // intended behaviour after the legacy cutover.
        _log('Dropping plaintext signaling from $peerId '
            '(anchor requires Noise-encrypted secureSignaling)');
      case PacketType.message:
      case PacketType.fragmentStart:
      case PacketType.fragmentContinue:
      case PacketType.fragmentEnd:
        _log('Dropping ${packet.type} from $peerId '
            '(rendezvous server does not relay messages)');
      case PacketType.ack:
      case PacketType.nack:
      case PacketType.readReceipt:
      case PacketType.secureMessage:
      case PacketType.secureFragmentStart:
      case PacketType.secureFragmentContinue:
      case PacketType.secureFragmentEnd:
      case PacketType.secureAck:
      case PacketType.secureNack:
      case PacketType.secureReadReceipt:
        break;
    }
  }

  /// Process an inbound Noise XX handshake packet from a peer. The anchor is
  /// always responder — it never initiates a session. If the handshake
  /// completes (message 3), we send a fresh addrReflect over the freshly
  /// established session so the peer learns its public address without having
  /// to wait for another ANNOUNCE cycle.
  Future<void> _handleNoiseHandshake(GrassrootsPacket packet, String peerId) async {
    try {
      final result = await _noiseSessions.handleHandshakePacket(packet);
      final senderHex = _pubkeyToHex(result.remotePubkey);

      // Map the connection to the claimed identity only when the session
      // manager accepted the message (verified message-1 claim or completed
      // message 3) — an unexpected/garbage handshake must not rebind a
      // connection. Rebinding a freshly-arrived tempKey lets a reconnecting
      // peer (new NAT-mapped source port) replace its stale
      // `_peerConnections` entry.
      final accepted =
          result.responsePayload != null || result.sessionEstablished;
      if (accepted &&
          peerId != senderHex &&
          _pendingIncoming.containsKey(peerId)) {
        _mapIncomingConnectionToPubkey(peerId, senderHex);
      }

      final responsePayload = result.responsePayload;
      if (responsePayload != null) {
        _sendPacket(
          senderHex,
          GrassrootsPacket(
            type: PacketType.noiseHandshake,
            payload: responsePayload,
          ),
        );
      }
      if (result.sessionEstablished) {
        _log('Noise session established with ${senderHex.substring(0, 8)}...');
        _sendAddrReflectFor(senderHex);
      }
    } catch (e) {
      // Identity is unknown when decoding/verification fails, so there is no
      // session entry to reset; a retrying peer starts over with a fresh
      // message 1, which replaces any half-open responder state.
      _log('Failed to process Noise handshake from $peerId: $e');
    }
  }

  /// Decrypt an inbound `secureSignaling` packet and feed the plaintext into
  /// the signaling handler. The sender identity is the connection's mapped
  /// pubkey (established by the Noise handshake); an unmapped connection
  /// cannot have a session, so its packets are dropped.
  Future<void> _handleSecureSignaling(
    GrassrootsPacket packet,
    String peerId, {
    String? observedIp,
    int? observedPort,
  }) async {
    final senderHex = peerId;
    if (!_noiseSessions.hasSession(senderHex)) {
      _log('Dropping secureSignaling from $peerId (no Noise session)');
      return;
    }
    try {
      final clear = await _noiseSessions.decryptPacket(
        packet,
        remotePubkeyHex: senderHex,
      );
      _signalingHandler.processSignaling(
        _hexToBytes(senderHex),
        clear.payload,
        observedIp: observedIp,
        observedPort: observedPort,
      );
    } catch (e) {
      _log('Failed to decrypt secureSignaling from '
          '${senderHex.substring(0, 8)}...: $e');
      _noiseSessions.reset(senderHex);
    }
  }

  /// Send a fresh addrReflect to [pubkeyHex] using the currently-tracked
  /// observed address. Called right after a Noise session establishes so the
  /// peer learns its public address promptly rather than waiting for its next
  /// ANNOUNCE cycle.
  void _sendAddrReflectFor(String pubkeyHex) {
    final conn = _peerConnections[pubkeyHex];
    if (conn == null) return;
    final reflect = AddrReflectMessage(
      ip: conn.addr.address,
      port: conn.port,
    );
    final peer = _peerTable.lookupVerified(pubkeyHex);
    if (peer == null) return;
    _signalingHandler.sendSignaling?.call(
      peer.publicKey,
      _codec.encode(reflect),
    );
  }

  Future<void> _handleAnnounce(
    GrassrootsPacket packet,
    String peerId, {
    String? observedIp,
    int? observedPort,
  }) async {
    final AnnounceData data;
    try {
      // Decoding verifies the record's own trailing signature.
      data = await _protocol.decodeAnnounce(packet.payload);
    } catch (e) {
      _log('Dropping unverifiable ANNOUNCE from $peerId: $e');
      return;
    }
    final senderHex = data.pubkeyHex;

    // Map the connection to the verified identity. Always rebind a
    // freshly-arrived tempKey so that a reconnecting peer (new NAT-mapped
    // source port) replaces its stale `_peerConnections` entry.
    if (peerId != senderHex && _pendingIncoming.containsKey(peerId)) {
      _mapIncomingConnectionToPubkey(peerId, senderHex);
    }

    _refreshTrackedAddressFromAnnounce(
      senderHex,
      observedIp: observedIp,
      observedPort: observedPort,
    );

    _signalingHandler.processAnnounce(
      data,
      observedIp: observedIp,
      observedPort: observedPort,
    );

    _log('ANNOUNCE: ${senderHex.substring(0, 8)}...');
    // Send our ANNOUNCE back so they know who we are
    await _sendAnnounceTo(data.publicKey);
  }

  void _refreshTrackedAddressFromAnnounce(
    String pubkeyHex, {
    String? observedIp,
    int? observedPort,
  }) {
    final connection = _peerConnections[pubkeyHex];
    if (connection == null || observedIp == null || observedPort == null) {
      return;
    }

    // Only refresh the address-table timestamp when the observed endpoint still
    // matches the currently-tracked live UDX session.
    if (connection.addr.address != observedIp ||
        connection.port != observedPort) {
      return;
    }

    _addressTable.register(pubkeyHex, observedIp, observedPort);
  }

  void _mapIncomingConnectionToPubkey(String tempKey, String pubkeyHex) {
    final pending = _pendingIncoming.remove(tempKey);
    if (pending != null) {
      _trackPeerConnection(
        pubkeyHex: pubkeyHex,
        connection: _PeerConnection(
          pubkeyHex: pubkeyHex,
          addr: pending.addr,
          port: pending.port,
          listenerFamily: pending.listenerFamily,
        ),
      );
      _log('Mapped connection → ${pubkeyHex.substring(0, 8)}...');
    }
  }

  // ===== Sending =====

  Future<bool> _sendSignaling(
      Uint8List recipientPubkey, Uint8List signalingPayload) async {
    final senderHex = _identity.pubkeyHex;
    final recipientHex = _pubkeyToHex(recipientPubkey);
    final signalingSummary = _describeSignalingPayload(signalingPayload);

    // The anchor only ships signaling as Noise-encrypted `secureSignaling`. If
    // no session exists yet, drop silently — the client initiates Noise on its
    // first signaling attempt, and subsequent anchor-side replies (e.g. the
    // next addrReflect on the next ANNOUNCE cycle) will land once the session
    // is up.
    if (!_noiseSessions.hasSession(recipientHex)) {
      _log('Skipping signaling reply to ${recipientHex.substring(0, 8)}...: '
          'no Noise session yet ($signalingSummary)');
      return false;
    }

    _log('Preparing signaling reply $signalingSummary from '
        '${senderHex.substring(0, 8)}... to ${recipientHex.substring(0, 8)}... '
        '(payload=${signalingPayload.length}B)');

    final clearPacket = _protocol.createSignalingPacket(
      signalingPayload: signalingPayload,
    );

    GrassrootsPacket securePacket;
    try {
      securePacket = await _noiseSessions.encryptPacket(
        clearPacket,
        remotePubkeyHex: recipientHex,
      );
    } catch (e) {
      _log('Failed to encrypt signaling reply for '
          '${recipientHex.substring(0, 8)}...: $e');
      return false;
    }
    final serializedLength = securePacket.serialize().length;
    _log('Encrypted signaling reply $signalingSummary from '
        '${senderHex.substring(0, 8)}... to ${recipientHex.substring(0, 8)}... '
        '(wire=$serializedLength B)');

    final sent = _sendPacket(recipientHex, securePacket);
    _log('Signaling send path for ${recipientHex.substring(0, 8)}... '
        '${sent ? "accepted" : "rejected"} '
        '($signalingSummary)');
    return sent;
  }

  Future<void> _sendAnnounceTo(Uint8List recipientPubkey) async {
    final packet = await _protocol.createAnnouncePacket();
    _sendPacket(_pubkeyToHex(recipientPubkey), packet);
  }

  Future<void> _broadcastAnnounce() async {
    if (_peerConnections.isEmpty) return;

    final packet = await _protocol.createAnnouncePacket();
    for (final pubkeyHex in _peerConnections.keys.toList()) {
      _sendPacket(pubkeyHex, packet);
    }
  }

  bool _sendPacket(String pubkeyHex, GrassrootsPacket packet) {
    final conn = _peerConnections[pubkeyHex];
    if (conn == null) {
      _log('Cannot send ${packet.type.name} to '
          '${_shortHex(pubkeyHex)}: not connected');
      return false;
    }
    final listener = _listenerForFamily(conn.listenerFamily);
    if (listener == null) {
      _log('Cannot send ${packet.type.name} to ${_shortHex(pubkeyHex)}: '
          'no ${_familyLabel(conn.listenerFamily)} listener');
      return false;
    }
    try {
      final data = packet.serialize();
      final sent = listener.rawSocket.send(data, conn.addr, conn.port);
      return sent > 0;
    } catch (e) {
      _log('Failed to send to ${_shortHex(pubkeyHex)}: $e');
      return false;
    }
  }

  _AnchorListener? _listenerForFamily(InternetAddressType family) {
    for (final listener in _listeners) {
      if (listener.family == family) return listener;
    }
    return _listeners.isNotEmpty ? _listeners.first : null;
  }

  void _trackPeerConnection({
    required String pubkeyHex,
    required _PeerConnection connection,
  }) {
    final existing = _peerConnections[pubkeyHex];
    if (existing != null &&
        (existing.addr.address != connection.addr.address ||
            existing.port != connection.port ||
            existing.listenerFamily != connection.listenerFamily)) {
      _addressToPubkey.remove('${existing.addr.address}:${existing.port}');
    }

    _peerConnections[pubkeyHex] = connection;
    _addressToPubkey['${connection.addr.address}:${connection.port}'] =
        pubkeyHex;

    // The address table mirrors the live session. Drop any entries from a
    // prior session (possibly a different family) and register the current
    // one — so queries and punches can only ever return the address we're
    // actually exchanging packets with right now.
    _addressTable.remove(pubkeyHex);
    _addressTable.register(
      pubkeyHex,
      connection.addr.address,
      connection.port,
    );
    _log('Address registered: ${pubkeyHex.substring(0, 8)}... → '
        '${connection.addr.address}:${connection.port} '
        '(${_familyLabel(connection.listenerFamily)})');
  }

  /// Remove the peer's live-session tracking. Called when a UDX stream ends
  /// and no replacement has taken over. Keeps the address table aligned with
  /// the live connection set — once there's no live session, the address
  /// stops being reachable, so we stop advertising it.
  void _forgetPeerConnection(String pubkeyHex, _PeerConnection released) {
    final current = _peerConnections[pubkeyHex];
    if (!identical(current, released)) return;
    _peerConnections.remove(pubkeyHex);
    _addressToPubkey.remove('${released.addr.address}:${released.port}');
    _addressTable.remove(pubkeyHex);
    // A silent peer's Noise session is torn down; a fresh session is
    // negotiated on the next reconnect.
    _noiseSessions.reset(pubkeyHex);
    _log('Peer disconnected: ${pubkeyHex.substring(0, 8)}... '
        '(address table entry cleared)');
  }

  // ===== Stats =====

  void _printStats() {
    _log('--- Stats ---');
    _log('  Connected: ${_peerConnections.length} '
        '(verified: ${_peerTable.verifiedCount}, '
        'unverified: ${_peerTable.unverifiedCount})');
    _log('  Address table: ${_addressTable.length} entries');
    for (final peer in _peerTable.verifiedPeers) {
      final addresses = _addressTable.lookupAll(peer.pubkeyHex);
      final connected = _peerConnections.containsKey(peer.pubkeyHex);
      _log('  ${peer.pubkeyHex.substring(0, 8)}... '
          '${connected ? "LIVE" : "offline"}'
          '${addresses.isNotEmpty ? " addr=${addresses.map((entry) => "${entry.ip}:${entry.port}").join(",")}" : ""}');
    }
  }

  // ===== Helpers =====

  String _describeSignalingPayload(Uint8List signalingPayload) {
    try {
      final message = _codec.decode(signalingPayload);
      return switch (message) {
        PunchInitiateMessage() =>
          'punchInitiate peer=${_shortHex(_pubkeyToHex(message.peerPubkey))} '
              'addr=${message.ip}:${message.port}',
        PunchReadyMessage() =>
          'punchReady peer=${_shortHex(_pubkeyToHex(message.peerPubkey))}',
        AddrReflectMessage() =>
          'addrReflect addr=${message.ip}:${message.port}',
        ReconnectMessage() =>
          'reconnect initiator=${_shortHex(_pubkeyToHex(message.initiatorPubkey))} '
              'peer=${_shortHex(_pubkeyToHex(message.peerPubkey))}',
        AvailableMessage() =>
          'available peer=${_shortHex(_pubkeyToHex(message.peerPubkey))}',
        RegisterInviteMessage() =>
          'registerInvite id=${_shortHex(_pubkeyToHex(message.inviteId))}',
        RedeemInviteMessage() =>
          'redeemInvite id=${_shortHex(_pubkeyToHex(message.inviteId))}',
        InviteRedeemedMessage() =>
          'inviteRedeemed id=${_shortHex(_pubkeyToHex(message.inviteId))} '
              'redeemer=${_shortHex(_pubkeyToHex(message.redeemerPubkey))}',
        InviteRedeemedAckMessage() =>
          'inviteRedeemedAck id=${_shortHex(_pubkeyToHex(message.inviteId))}',
      };
    } catch (e) {
      return 'signaling-decode-failed payload=${signalingPayload.length}B error=$e';
    }
  }

  static String _shortHex(String hex) =>
      hex.length <= 8 ? hex : '${hex.substring(0, 8)}...';

  static String _familyLabel(InternetAddressType family) =>
      family == InternetAddressType.IPv6 ? 'IPv6' : 'IPv4';

  static String _pubkeyToHex(Uint8List pubkey) =>
      pubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  static Uint8List _hexToBytes(String hex) => Uint8List.fromList([
        for (var i = 0; i < hex.length; i += 2)
          int.parse(hex.substring(i, i + 2), radix: 16),
      ]);

  void _log(String message) {
    final ts = DateTime.now().toIso8601String().substring(11, 23);
    print('[$ts] $message');
  }
}

class _PeerConnection {
  final String pubkeyHex;
  final InternetAddress addr;
  final int port;
  final InternetAddressType listenerFamily;
  DateTime lastSeen = DateTime.now();

  _PeerConnection({
    required this.pubkeyHex,
    required this.addr,
    required this.port,
    required this.listenerFamily,
  });
}

class _AnchorListener {
  final InternetAddressType family;
  final int port;
  final RawDatagramSocket rawSocket;
  final String publicAddress;
  StreamSubscription? socketSub;

  _AnchorListener({
    required this.family,
    required this.port,
    required this.rawSocket,
    required this.publicAddress,
  });
}
