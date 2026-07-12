import 'package:redux/redux.dart';
import '../mesh/bloom_filter.dart';
import '../models/packet.dart';
import '../models/peer.dart';
import '../protocol/fragment_handler.dart';
import '../protocol/protocol_handler.dart';
import '../store/app_state.dart';
import '../store/peers_actions.dart';
import '../store/peers_state.dart';
import '../transport/address_utils.dart';
import 'package:flutter/foundation.dart';

/// Routes incoming packets from all transports to the appropriate handlers.
///
/// The wire frame carries no identity or signature, so sender identity comes
/// from the payload layer: ANNOUNCE payloads are self-signed identity records
/// (verified by [ProtocolHandler.decodeAnnounce]), Noise handshake envelopes
/// carry an authenticated identity claim (handled by the session manager),
/// and session-encrypted packets are authenticated by the Noise session that
/// decrypts them. Clear application-data packets carry no authentication and
/// are dropped.
///
/// Responsibilities:
/// - Message delivery deduplication (via BloomFilter, keyed by messageId)
/// - ANNOUNCE decoding and Redux dispatch
/// - Fragment reassembly delegation
/// - Callback dispatch to application layer
///
/// All transports feed into [processPacket] — one entry point, one format.
class MessageRouter {
  final Store<AppState> store;
  final ProtocolHandler protocolHandler;
  final FragmentHandler fragmentHandler;
  final BloomFilter _seenPackets = BloomFilter();

  /// Called when a message is received. [transport] is the transport the packet
  /// actually arrived on — authoritative, taken from the receive path rather
  /// than inferred from peer state.
  void Function(String id, Uint8List senderPubkey, Uint8List payload,
      PeerTransport transport)? onMessageReceived;

  /// Called when an ACK is received (delivery confirmation)
  void Function(String messageId)? onAckReceived;

  /// Called when a read receipt is received
  void Function(String messageId)? onReadReceiptReceived;

  /// Called when a peer ANNOUNCE is processed (new or updated peer).
  /// [udpPeerId] is the transport-level peer identifier (tempKey for incoming
  /// UDP connections) so the coordinator can map it to the peer's pubkey.
  void Function(AnnounceData data, PeerTransport transport,
      {bool isNew, String? udpPeerId})? onPeerAnnounced;

  /// Called when a message needs an ACK sent back to the sender
  void Function(PeerTransport transport, String? peerId, String messageId)?
      onAckRequested;

  /// Called when a signaling packet is received.
  /// The coordinator routes this to [SignalingService.processSignaling].
  /// [observedIp] / [observedPort] carry the UDP source address observed by
  /// the transport layer (null for BLE-arrived signaling).
  void Function(
    Uint8List senderPubkey,
    Uint8List payload, {
    String? observedIp,
    int? observedPort,
  })? onSignalingReceived;

  /// Called after payload-signature verification and before a BLE ANNOUNCE is
  /// applied. Return false to reject first contact from that sender.
  bool Function(
    Uint8List senderPubkey, {
    String? bleDeviceId,
    BleRole? bleRole,
  })? shouldAcceptBleAnnounce;

  /// Called when [shouldAcceptBleAnnounce] rejects a verified BLE ANNOUNCE.
  void Function(Uint8List senderPubkey, String? bleDeviceId)?
      onBleAnnounceRejected;

  /// Called when a packet with an authenticated sender identity (verified
  /// ANNOUNCE or session-decrypted packet) arrives over UDP, so the
  /// coordinator can map the connection to the peer's pubkey.
  void Function(Uint8List senderPubkey, String udpPeerId)? onUdpPeerIdentified;

  /// Called when a Noise handshake packet arrives. The coordinator owns
  /// session state and sends any handshake response over the same medium;
  /// the handshake envelope carries the peer's identity claim.
  Future<void> Function(
    GrassrootsPacket packet,
    PeerTransport transport, {
    String? peerId,
  })? onNoiseHandshakeReceived;

  /// Decrypts a session-encrypted packet before normal routing. Returns the
  /// clear packet together with the session-authenticated sender identity,
  /// or null when no session covers the packet.
  Future<(GrassrootsPacket, Uint8List)?> Function(
    GrassrootsPacket packet,
    PeerTransport transport, {
    String? peerId,
  })? decryptSessionPacket;

  /// Convenience accessor for peers state
  PeersState get _peersState => store.state.peers;

  MessageRouter({
    required this.store,
    required this.protocolHandler,
    required this.fragmentHandler,
  });

  // ===== Unified Packet Processing =====

  /// Process an incoming packet from any transport.
  ///
  /// Only three packet classes are accepted off the wire: self-signed
  /// ANNOUNCE records, Noise handshake packets (identity-claimed and
  /// self-authenticating), and session-encrypted packets (authenticated by
  /// the session AEAD on decrypt). Clear application-data packets carry no
  /// authentication and are dropped.
  Future<void> processPacket(
    GrassrootsPacket packet, {
    required PeerTransport transport,
    String? bleDeviceId,
    BleRole? bleRole,
    String? udpPeerId,
    int? rssi,
    String? observedIp,
    int? observedPort,
  }) async {
    if (packet.type == PacketType.noiseHandshake) {
      await onNoiseHandshakeReceived?.call(
        packet,
        transport,
        peerId: udpPeerId ?? bleDeviceId,
      );
      return;
    }

    // ANNOUNCE always processed (peer may have updated info). Decoding
    // verifies the payload's own signature; a forged record throws.
    if (packet.type == PacketType.announce) {
      final AnnounceData data;
      try {
        data = protocolHandler.decodeAnnounce(packet.payload);
      } catch (e) {
        debugPrint('Dropping unverifiable ANNOUNCE: $e');
        return;
      }
      if (transport == PeerTransport.bleDirect) {
        final accepted = shouldAcceptBleAnnounce?.call(
              data.publicKey,
              bleDeviceId: bleDeviceId,
              bleRole: bleRole,
            ) ??
            true;
        if (!accepted) {
          debugPrint(
            '[trust] Dropping BLE ANNOUNCE from '
            '${_pubkeyToHex(data.publicKey).substring(0, 8)}',
          );
          onBleAnnounceRejected?.call(data.publicKey, bleDeviceId);
          return;
        }
      }
      String? effectiveUdpPeerId = udpPeerId;
      if (transport == PeerTransport.udp && udpPeerId != null) {
        onUdpPeerIdentified?.call(data.publicKey, udpPeerId);
        effectiveUdpPeerId = _pubkeyToHex(data.publicKey);
      }
      _handleAnnounce(
        data,
        transport: transport,
        bleDeviceId: bleDeviceId,
        bleRole: bleRole,
        udpPeerId: effectiveUdpPeerId,
        rssi: rssi,
      );
      return;
    }

    if (!packet.type.isSessionEncrypted) {
      debugPrint(
          'Dropping unauthenticated clear ${packet.type.name} packet');
      return;
    }

    final decrypted = await decryptSessionPacket?.call(
      packet,
      transport,
      peerId: udpPeerId ?? bleDeviceId,
    );
    if (decrypted == null) {
      debugPrint('Dropping encrypted packet without session');
      return;
    }
    final (clearPacket, senderPubkey) = decrypted;
    packet = clearPacket;

    String? effectiveUdpPeerId = udpPeerId;
    if (transport == PeerTransport.udp) {
      if (udpPeerId != null) {
        onUdpPeerIdentified?.call(senderPubkey, udpPeerId);
        effectiveUdpPeerId = _pubkeyToHex(senderPubkey);
      }
      // Any session-authenticated packet over UDP counts as liveness traffic
      // for that peer, even if it is an ACK, read receipt, or retransmission.
      store.dispatch(PeerUdpSeenAction(senderPubkey));
    }

    // Refresh per-packet RSSI on every BLE packet from a known peer.
    // The plugin emits `payload.rssi` as null when the OS doesn't expose a
    // remote-RSSI measurement (peripheral-role writes on both platforms,
    // central paths before the first poll). For ANNOUNCE packets,
    // _handleAnnounce covers the RSSI update via PeerAnnounceReceivedAction.
    if (transport == PeerTransport.bleDirect && rssi != null) {
      final peer = _peersState.getPeerByPubkey(senderPubkey);
      if (peer != null) {
        store.dispatch(PeerRssiUpdatedAction(
          publicKey: senderPubkey,
          rssi: rssi,
        ));
      }
    }

    switch (packet.type) {
      case PacketType.message:
        _handleMessage(
          packet,
          senderPubkey: senderPubkey,
          transport: transport,
          peerId: effectiveUdpPeerId ?? bleDeviceId,
        );
      case PacketType.fragmentStart:
      case PacketType.fragmentContinue:
      case PacketType.fragmentEnd:
        _handleFragment(
          packet,
          senderPubkey: senderPubkey,
          transport: transport,
          peerId: effectiveUdpPeerId ?? bleDeviceId,
        );
      case PacketType.ack:
        _handleAck(packet);
      case PacketType.nack:
        // TODO: handle this
        break;
      case PacketType.readReceipt:
        _handleReadReceipt(packet);
      case PacketType.signaling:
        _handleSignaling(
          packet,
          senderPubkey: senderPubkey,
          observedIp: observedIp,
          observedPort: observedPort,
        );
      case PacketType.announce:
      case PacketType.noiseHandshake:
      case PacketType.secureMessage:
      case PacketType.secureFragmentStart:
      case PacketType.secureFragmentContinue:
      case PacketType.secureFragmentEnd:
      case PacketType.secureAck:
      case PacketType.secureNack:
      case PacketType.secureReadReceipt:
      case PacketType.secureSignaling:
        return;
    }
  }

  // ===== Handlers =====

  void _handleAnnounce(
    AnnounceData data, {
    required PeerTransport transport,
    String? bleDeviceId,
    BleRole? bleRole,
    String? udpPeerId,
    int? rssi,
  }) {
    final pubkey = data.publicKey;

    // Resolve BLE metadata only for packets that actually arrived over BLE.
    // UDP ANNOUNCEs can coincide with stale scan results; treating those as a
    // live BLE path makes UDP-only friends appear in the Nearby/Connected list.
    final isBleAnnounce = transport == PeerTransport.bleDirect;
    String? resolvedBleDeviceId = isBleAnnounce ? bleDeviceId : null;
    BleRole? resolvedBleRole = isBleAnnounce ? bleRole : null;
    DiscoveredPeerState? discoveredPeer;
    if (isBleAnnounce && bleDeviceId != null) {
      discoveredPeer = _peersState.getDiscoveredBlePeer(bleDeviceId);
    }

    // RSSI source priority for BLE-arrived ANNOUNCEs:
    //   1. Per-payload arrival RSSI (our own radio's measurement on this
    //      packet) — strongest source.
    //   2. Scan-time RSSI for the same pathId — also our own measurement,
    //      just slightly older.
    // Peripheral-only paths have no local measurement (the plugin emits
    // null) and leave effectiveRssi null; the UI shows "-- dBm" until the
    // reverse central dial fills it in.
    int? effectiveRssi;
    if (isBleAnnounce) {
      if (rssi != null) {
        effectiveRssi = rssi;
      } else if (discoveredPeer?.rssi != null) {
        effectiveRssi = discoveredPeer!.rssi;
      }
    }

    final isNew = _peersState.getPeerByPubkey(pubkey) == null;

    // Use the address from the ANNOUNCE payload only.
    // udpPeerId is the sender's hex pubkey, NOT an ip:port address —
    // using it as a fallback would corrupt the peer's stored udpAddress
    // and clear their well-connected status.
    final udpAddress = _normalizeUdpAddress(data.udpAddress);
    final linkLocalAddress = _normalizeLinkLocalAddress(data.linkLocalAddress);
    final udpAddressCandidates = _normalizeUdpAddressCandidates([
      ...data.addressCandidates,
      udpAddress,
      linkLocalAddress,
    ]);

    // Set the correct BLE device ID field based on role
    String? centralId;
    String? peripheralId;
    if (resolvedBleDeviceId != null && resolvedBleRole != null) {
      if (resolvedBleRole == BleRole.central) {
        centralId = resolvedBleDeviceId;
      } else {
        peripheralId = resolvedBleDeviceId;
      }
    }

    store.dispatch(PeerAnnounceReceivedAction(
      publicKey: pubkey,
      nickname: data.nickname,
      protocolVersion: data.protocolVersion,
      rssi: effectiveRssi,
      transport: transport,
      bleCentralDeviceId: centralId,
      blePeripheralDeviceId: peripheralId,
      udpAddress: udpAddress,
      linkLocalAddress: linkLocalAddress,
      udpAddressCandidates: udpAddressCandidates,
    ));

    if (resolvedBleDeviceId != null && resolvedBleRole != null) {
      store.dispatch(AssociateBleDeviceAction(
        publicKey: pubkey,
        deviceId: resolvedBleDeviceId,
        role: resolvedBleRole,
      ));
    }

    debugPrint(
        'Peer ${isNew ? "connected" : "updated"}: ${data.nickname} via ${transport.name}'
        '${data.udpAddress != null ? " addr=${data.udpAddress}" : ""}');

    // debugPrint('Peer announced!');

    onPeerAnnounced?.call(data, transport, isNew: isNew, udpPeerId: udpPeerId);
  }

  void _handleMessage(
    GrassrootsPacket packet, {
    required Uint8List senderPubkey,
    required PeerTransport transport,
    String? peerId,
  }) {
    // The MESSAGE payload opens with its messageId (the delivery identity,
    // stable across retries and media), followed by the body — mirroring
    // fragment payloads.
    if (packet.payload.length < ProtocolHandler.messageIdLength) {
      debugPrint('Dropping MESSAGE without a messageId prefix');
      return;
    }
    final messageId = String.fromCharCodes(
        packet.payload.sublist(0, ProtocolHandler.messageIdLength));
    final body = Uint8List.fromList(
        packet.payload.sublist(ProtocolHandler.messageIdLength));
    _deliverMessage(
      messageId,
      body,
      senderPubkey: senderPubkey,
      transport: transport,
      peerId: peerId,
    );
  }

  void _handleFragment(
    GrassrootsPacket packet, {
    required Uint8List senderPubkey,
    required PeerTransport transport,
    String? peerId,
  }) {
    final reassembled = fragmentHandler.processFragment(packet);
    if (reassembled == null) return;

    // Reassembly produced the original message's payload bytes; the
    // messageId travelled in the fragment payloads. Route through the same
    // delivery pipeline as single-packet messages.
    _deliverMessage(
      reassembled.messageId,
      reassembled.payload,
      senderPubkey: senderPubkey,
      transport: transport,
      peerId: peerId,
    );
  }

  void _deliverMessage(
    String messageId,
    Uint8List body, {
    required Uint8List senderPubkey,
    required PeerTransport transport,
    String? peerId,
  }) {
    // Dedup by messageId: only deliver to the app once, but ACK every time
    // so the sender's watchdog and BLE-disconnect re-queue stop retrying
    // when the original ACK got lost. Retries are fresh encryptions
    // (possibly over a different medium), so the session's replay window
    // cannot catch them — this id is what makes redelivery detectable.
    final firstSeen = !_seenPackets.checkAndAdd(messageId);
    if (firstSeen) {
      onMessageReceived?.call(messageId, senderPubkey, body, transport);
    } else {
      debugPrint(
        'Duplicate message ${messageId.length >= 8 ? messageId.substring(0, 8) : messageId}; '
        're-ACKing without re-delivering.',
      );
    }

    // Send ACK back to confirm delivery. The sender waits for this to
    // mark the message as "delivered" (2 checkmarks). Works over both
    // BLE (peerId = bleDeviceId) and UDP (peerId = udpPeerId).
    onAckRequested?.call(transport, peerId, messageId);
  }

  void _handleAck(GrassrootsPacket packet) {
    if (packet.payload.isEmpty) return;
    try {
      final messageId = String.fromCharCodes(packet.payload);
      // Validate: message IDs are short alphanumeric strings (UUID v4 prefix)
      if (messageId.length > 36) {
        debugPrint(
            'Ignoring ACK with invalid message ID length: ${messageId.length}');
        return;
      }
      onAckReceived?.call(messageId);
    } catch (e) {
      debugPrint('Failed to decode ACK payload: $e');
    }
  }

  void _handleSignaling(
    GrassrootsPacket packet, {
    required Uint8List senderPubkey,
    String? observedIp,
    int? observedPort,
  }) {
    onSignalingReceived?.call(
      senderPubkey,
      packet.payload,
      observedIp: observedIp,
      observedPort: observedPort,
    );
  }

  void _handleReadReceipt(GrassrootsPacket packet) {
    if (packet.payload.isEmpty) return;
    try {
      final messageId = String.fromCharCodes(packet.payload);
      if (messageId.length > 36) {
        debugPrint(
            'Ignoring read receipt with invalid message ID length: ${messageId.length}');
        return;
      }
      onReadReceiptReceived?.call(messageId);
    } catch (e) {
      debugPrint('Failed to decode read receipt payload: $e');
    }
  }

  // ===== Helpers =====

  static String _pubkeyToHex(Uint8List pubkey) =>
      pubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  String? _normalizeUdpAddress(String? udpAddress) {
    if (udpAddress == null || udpAddress.isEmpty) return null;

    final parsed = parseAddressString(udpAddress);
    if (parsed != null) return parsed.toAddressString();

    debugPrint('Ignoring malformed UDP address from ANNOUNCE: $udpAddress');
    return null;
  }

  Set<String> _normalizeUdpAddressCandidates(Iterable<String?> addresses) {
    final normalized = <String>{};
    for (final address in addresses) {
      final parsed = _normalizeUdpAddress(address);
      if (parsed != null) {
        normalized.add(parsed);
      }
    }
    return normalized;
  }

  String? _normalizeLinkLocalAddress(String? udpAddress) {
    final normalized = _normalizeUdpAddress(udpAddress);
    if (normalized == null) return null;

    final parsed = parseIpv6AddressString(normalized);
    if (parsed == null) return null;
    if (!parsed.ip.isLinkLocal) {
      debugPrint(
          'Ignoring non-link-local address in ANNOUNCE link-local field: '
          '$udpAddress');
      return null;
    }
    return parsed.toAddressString();
  }

  // ===== Deduplication API =====

  /// Mark a message ID as delivered (e.g., for locally-originated messages)
  void markSeen(String messageId) {
    _seenPackets.add(messageId);
  }

  /// Check if a message ID has been delivered before
  bool isDuplicate(String messageId) {
    return _seenPackets.mightContain(messageId);
  }

  // ===== Lifecycle =====

  /// Clean up resources
  void dispose() {
    _seenPackets.clear();
  }
}
