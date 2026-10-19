import 'package:redux/redux.dart';
import '../models/packet.dart';
import '../models/peer.dart';
import '../protocol/fragment_handler.dart';
import '../protocol/protocol_handler.dart';
import '../store/app_state.dart';
import '../store/peers_actions.dart';
import '../store/peers_state.dart';
import 'dart:typed_data';
import '../platform/compat.dart';

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
/// - ANNOUNCE decoding and Redux dispatch
/// - Fragment reassembly delegation
/// - Callback dispatch to application layer
///
/// All transports feed into [processPacket] — one entry point, one format.
///
/// Delivery is at least once (spec §Message Protocol: "A re-send after a lost
/// acknowledgment, possibly over another underlay, may deliver a message
/// multiple times, and madGLP dedupes repeated deliveries"; §Networking
/// Assumptions: "Every message passed to send() is eventually delivered to
/// the recipient at least once"): a message is delivered every time it is
/// reassembled, and acknowledged every time. There is no duplicate filter
/// (GNA, 2026-10-11, Task B): the Bloom filter that was one took a fresh
/// message for a duplicate about 1% of the time near its capacity, and
/// dropped it.
class MessageRouter {
  final Store<AppState> store;
  final ProtocolHandler protocolHandler;
  final FragmentHandler fragmentHandler;

  /// Called when a message is received. [transport] is the transport the packet
  /// actually arrived on — authoritative, taken from the receive path rather
  /// than inferred from peer state.
  void Function(String id, Uint8List senderPubkey, Uint8List payload,
      PeerTransport transport)? onMessageReceived;

  /// Called when an ACK is received (delivery confirmation)
  void Function(String messageId)? onAckReceived;

  /// Called when a per-fragment acknowledgment arrives (spec §Message
  /// Transport) — the coordinator settles the sender-side retransmit slot.
  void Function(Uint8List senderPubkey, String messageId, int index,
      PeerTransport transport)? onFragmentAckReceived;

  /// Called when an inbound fragment needs its per-fragment acknowledgment
  /// sent back.
  void Function(PeerTransport transport, String? peerId, String messageId,
      int index)? onFragmentAckRequested;

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
  /// The coordinator acts on what a rendezvous server sends.
  /// [observedIp] / [observedPort] carry the UDP source address observed by
  /// the transport layer (null for BLE-arrived signaling).
  void Function(
    Uint8List senderPubkey,
    Uint8List payload, {
    String? observedIp,
    int? observedPort,
  })? onSignalingReceived;

  /// Called when a peer's platform attestation arrives (spec §Session
  /// Establishment): the reassembled message of the attestation exchange,
  /// which rides the Message Protocol as `send` payloads do — its fragments
  /// acknowledged, the message de-duplicated and acknowledged whole — and is
  /// told from a payload by its fragment type alone. It rides the session, so
  /// the sender is session-authenticated.
  ///
  /// [handshakeHash] is that of the session that decrypted it (GNA,
  /// 2026-10-11, Task A): the coordinator verifies it against the digest that
  /// session binds to, not against whichever session is current when it
  /// judges, and tears that session down if it fails. Every fragment of one
  /// attestation is sent under one session, and a fragment decrypts only
  /// under its own, so the session that decrypted the completing fragment is
  /// the session of the whole message.
  void Function(
    Uint8List senderPubkey,
    Uint8List payload,
    PeerTransport transport,
    Uint8List handshakeHash,
  )? onAttestationReceived;

  /// Whether the attestation exchange on the session whose handshake hash is
  /// [handshakeHash] has completed: the peer's attestation verified on that
  /// session (spec §Session Establishment: "neither peer is reported
  /// reachable until it succeeds"). A `send` payload arriving on a session
  /// whose exchange has not completed is dropped unacknowledged, so the
  /// sender retransmits it (GNA, 2026-10-11, Task A). Unset, every session
  /// counts as completed.
  bool Function(
    Uint8List senderPubkey,
    PeerTransport transport,
    Uint8List handshakeHash,
  )? isSessionAttested;

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

  /// Called after payload-signature verification and before a UDP ANNOUNCE
  /// is applied. Return false to reject contact from that sender — the
  /// server profile's Closed-trust gate (spec answer: "unregistered keys
  /// cannot complete a session").
  bool Function(Uint8List senderPubkey, {String? udpPeerId})?
      shouldAcceptUdpAnnounce;

  /// Called when [shouldAcceptUdpAnnounce] rejects a verified UDP ANNOUNCE.
  void Function(Uint8List senderPubkey, String? udpPeerId)?
      onUdpAnnounceRejected;

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
  /// clear packet together with the session-authenticated sender identity and
  /// the handshake hash of the session that decrypted it, or null when no
  /// session covers the packet.
  Future<(GrassrootsPacket, Uint8List, Uint8List)?> Function(
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
      if (transport == PeerTransport.udp) {
        final accepted = shouldAcceptUdpAnnounce?.call(
              data.publicKey,
              udpPeerId: udpPeerId,
            ) ??
            true;
        if (!accepted) {
          debugPrint(
            '[trust] Dropping UDP ANNOUNCE from '
            '${_pubkeyToHex(data.publicKey).substring(0, 8)}',
          );
          onUdpAnnounceRejected?.call(data.publicKey, udpPeerId);
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
    final (clearPacket, senderPubkey, handshakeHash) = decrypted;
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
      case PacketType.fragment:
      case PacketType.attestationFragment:
        _handleFragment(
          packet,
          senderPubkey: senderPubkey,
          transport: transport,
          handshakeHash: handshakeHash,
          peerId: effectiveUdpPeerId ?? bleDeviceId,
        );
      case PacketType.fragmentAck:
        _handleFragmentAck(packet, senderPubkey, transport);
      case PacketType.ack:
        _handleAck(packet);
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
      case PacketType.secureFragment:
      case PacketType.secureFragmentAck:
      case PacketType.secureAck:
      case PacketType.secureReadReceipt:
      case PacketType.secureSignaling:
      case PacketType.secureAttestationFragment:
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

    // ANNOUNCE is identity beacon and heartbeat only (spec §ANNOUNCE and
    // Liveness): it carries no addresses. Peer addresses reach the layer via
    // putPeerAddress, fed by GLP.

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
      protocolVersion: data.protocolVersion,
      rssi: effectiveRssi,
      transport: transport,
      bleCentralDeviceId: centralId,
      blePeripheralDeviceId: peripheralId,
    ));

    if (resolvedBleDeviceId != null && resolvedBleRole != null) {
      store.dispatch(AssociateBleDeviceAction(
        publicKey: pubkey,
        deviceId: resolvedBleDeviceId,
        role: resolvedBleRole,
      ));
    }

    debugPrint(
        'Peer ${isNew ? "connected" : "updated"}: '
        '${data.pubkeyHex.substring(0, 8)}... via ${transport.name}');

    onPeerAnnounced?.call(data, transport, isNew: isNew, udpPeerId: udpPeerId);
  }

  void _handleFragment(
    GrassrootsPacket packet, {
    required Uint8List senderPubkey,
    required PeerTransport transport,
    required Uint8List handshakeHash,
    String? peerId,
  }) {
    final Fragment fragment;
    try {
      fragment =
          FragmentHandler.decodeFragment(packet.payload, type: packet.type);
    } catch (e) {
      debugPrint('Dropping malformed fragment: $e');
      return;
    }

    // A payload on a session whose attestation exchange has not completed is
    // dropped unacknowledged, so the sender retransmits it (GNA, 2026-10-11,
    // Task A); the exchange's own fragments are what complete it.
    if (fragment.type == PacketType.fragment &&
        !(isSessionAttested?.call(senderPubkey, transport, handshakeHash) ??
            true)) {
      debugPrint('Dropping payload fragment of ${fragment.messageId} '
          'unacknowledged: its session has not completed the attestation '
          'exchange');
      return;
    }

    // Acknowledge every fragment, duplicates included — the sender's
    // retransmit stops only on the ACK (spec §Message Transport).
    onFragmentAckRequested?.call(
      transport,
      peerId,
      fragment.messageId,
      fragment.index,
    );

    final reassembled = fragmentHandler.addFragment(fragment);
    if (reassembled == null) return;

    // Reassembly produced the original message's payload bytes; the
    // messageId travelled in the fragment payloads. Route through the same
    // delivery pipeline as single-packet messages.
    _deliverMessage(
      reassembled.messageId,
      reassembled.payload,
      type: reassembled.type,
      senderPubkey: senderPubkey,
      transport: transport,
      handshakeHash: handshakeHash,
      peerId: peerId,
    );
  }

  void _handleFragmentAck(
    GrassrootsPacket packet,
    Uint8List senderPubkey,
    PeerTransport transport,
  ) {
    final String messageId;
    final int index;
    try {
      (messageId, index) = FragmentHandler.decodeFragmentAck(packet.payload);
    } catch (e) {
      debugPrint('Dropping malformed fragment ACK: $e');
      return;
    }
    onFragmentAckReceived?.call(senderPubkey, messageId, index, transport);
  }

  void _deliverMessage(
    String messageId,
    Uint8List body, {
    required PacketType type,
    required Uint8List senderPubkey,
    required PeerTransport transport,
    required Uint8List handshakeHash,
    String? peerId,
  }) {
    // Delivered every time it is reassembled: a re-send after a lost
    // acknowledgment is delivered again (at least once, spec §Message
    // Protocol), and acknowledged again below so the sender stops. The
    // attestation exchange rides the same protocol as payloads and goes to
    // the coordinator, never to the application; a second attestation on a
    // session already attested is the coordinator's to ignore.
    if (type == PacketType.attestationFragment) {
      onAttestationReceived?.call(senderPubkey, body, transport, handshakeHash);
    } else {
      onMessageReceived?.call(messageId, senderPubkey, body, transport);
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
}
