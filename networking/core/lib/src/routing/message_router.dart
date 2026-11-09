import 'dart:async';
import 'dart:convert';
import 'package:cryptography/dart.dart' show DartSha256;
import 'package:redux/redux.dart';
import '../models/packet.dart';
import '../models/peer.dart';
import '../protocol/fragment_handler.dart';
import '../protocol/protocol_handler.dart';
import '../store/app_state.dart';
import '../store/peers_actions.dart';
import '../store/peers_state.dart';
import '../transport/udp_transport_service.dart';
import 'dart:typed_data';
import '../platform/compat.dart';

/// Routes incoming packets from all transports to the appropriate handlers.
///
/// The wire frame carries no identity or signature, so sender identity comes
/// from the payload layer: ANNOUNCE payloads are self-signed identity records
/// (verified by [ProtocolHandler.decodeAnnounce]), a Noise handshake
/// authenticates the peer it runs with (handled by the session manager), and
/// session-encrypted packets are authenticated by the Noise session that
/// decrypts them. Clear application-data packets carry no authentication and
/// are dropped.
///
/// ANNOUNCE arrives two ways (GNA §2.5): in clear, only before the XX
/// handshake of a first contact under Open trust, which the coordinator's
/// gates [shouldAcceptBleAnnounce] and [shouldAcceptUdpAnnounce] admit or
/// drop; and inside a session, as the heartbeat, its record naming the key
/// of the session that carried it.
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
/// the recipient at least once"). The receiver acknowledges each fragment,
/// and a message whose fragments are all acknowledged is settled at its
/// sender (GNA, 2026-11-08): every fragment is acknowledged on arrival except
/// a fixed one, the last by index ([FragmentHandler.heldIndex]), which is
/// acknowledged only once the whole message is reassembled, saved
/// ([keepDelivered]) and delivered, whatever order the fragments arrive in.
/// A receiver that lost its buffer therefore never acknowledges that fragment
/// alone: the sender retransmits it until it gives up, and sends the whole
/// message again.  No fragment is acknowledged that was not stored.
///
/// A message is delivered every time it is reassembled and saved, except
/// that a fragment of a message saved recently is acknowledged at once and
/// the message not delivered again: one released less than
/// [FragmentHandler.reassemblyTimeout] ago ([FragmentHandler.wasReleased]),
/// payload or attestation, or a payload kept, the runtime not having
/// acknowledged it yet ([isKeptDelivered]).  Its sender retransmits such a
/// fragment when the acknowledgment of the last one was lost, and without it
/// would retransmit until it gave up.  Neither is a duplicate filter in the
/// sense of GNA, 2026-10-11, Task B, whose Bloom filter took a fresh message
/// for a duplicate about 1% of the time near its capacity and dropped it:
/// both are exact records of messages already saved, keyed on the
/// authenticated sender and the wire messageId, and bounded, so a fresh
/// message is never taken for one, and at least once holds.
class MessageRouter {
  final Store<AppState> store;
  final ProtocolHandler protocolHandler;
  final FragmentHandler fragmentHandler;

  /// Called when a message is received. [transport] is the transport the packet
  /// actually arrived on — authoritative, taken from the receive path rather
  /// than inferred from peer state.
  void Function(String id, Uint8List senderPubkey, Uint8List payload,
      PeerTransport transport)? onMessageReceived;

  /// Called before a reassembled payload is delivered and its last fragment
  /// acknowledged: the coordinator keeps the message until the runtime
  /// acknowledges it (GNA §2.9, `acknowledge(messageId)`), and the router
  /// delivers it and acknowledges its last fragment once this completes, so
  /// that the sender settles a message only once its recipient keeps it.
  /// When it fails, the message is neither delivered nor its last fragment
  /// acknowledged, and its reassembly buffer is kept: the sender's
  /// retransmission of that fragment returns the message again and the save
  /// is retried.  Absent, the message is delivered and acknowledged at
  /// once.  The id is the layer-assigned one of [deliveryIdOf].
  Future<void> Function(String id, Uint8List senderPubkey, Uint8List payload,
      PeerTransport transport)? keepDelivered;

  /// Whether the message delivered under [id] (the layer-assigned one of
  /// [deliveryIdOf]) is kept: saved by [keepDelivered] and not yet
  /// acknowledged by the runtime.  A fragment of such a message, which its
  /// sender retransmits when the acknowledgment of the last fragment was
  /// lost, is acknowledged at once and the message is not delivered
  /// again: it was delivered when it was kept, and the kept messages are
  /// delivered again at restart.  Absent, no message counts as kept.
  bool Function(String id)? isKeptDelivered;

  /// The messages whose save [keepDelivered] is under way, by sender (hex)
  /// and wire messageId: the last fragment of one, retransmitted meanwhile,
  /// waits for that save to acknowledge it.
  final Set<({String senderHex, String messageId})> _saving = {};

  /// The layer-assigned id a message from [senderPubkey] is delivered under
  /// (GNA §2.9): the first 32 hex characters of SHA-256 over the sender's
  /// public key and the wire messageId. A re-send of the same message maps to
  /// the same id, and two senders using the same wire id map to different ones.
  static String deliveryIdOf(Uint8List senderPubkey, String messageId) {
    final digest = const DartSha256()
        .hashSync(
            Uint8List.fromList([...senderPubkey, ...utf8.encode(messageId)]))
        .bytes;
    return digest
        .sublist(0, 16)
        .map((b) => b.toRadixString(16).padLeft(2, '0'))
        .join();
  }

  /// Called when a per-fragment acknowledgment arrives from [senderPubkey],
  /// the session-authenticated peer that sent it (spec §Message Transport):
  /// the coordinator settles the retransmit slot on that peer's lane of the
  /// session it arrived on, and a message whose fragments are all
  /// acknowledged is settled.
  void Function(Uint8List senderPubkey, String messageId, int index,
      PeerTransport transport)? onFragmentAckReceived;

  /// Called when an inbound fragment needs its per-fragment acknowledgment
  /// sent back: at once for every fragment but the last of its message, and
  /// for that one once the message is reassembled, saved and delivered.
  void Function(
          PeerTransport transport, String? peerId, String messageId, int index)?
      onFragmentAckRequested;

  /// Called when a peer ANNOUNCE is processed (new or updated peer).
  /// [udpPeerId] is the transport-level peer identifier (tempKey for incoming
  /// UDP connections) so the coordinator can map it to the peer's pubkey.
  /// [inSession] tells the ANNOUNCE a session carried, the heartbeat, from the
  /// clear one of a first contact under Open trust (GNA §2.5).
  void Function(AnnounceData data, PeerTransport transport,
      {String? udpPeerId, bool inSession})? onPeerAnnounced;

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
  /// acknowledged, the last one once this has been called — and is
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
      if (transport.isIp) {
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
      if (transport.isIp && udpPeerId != null) {
        onUdpPeerIdentified?.call(data.publicKey, udpPeerId);
        effectiveUdpPeerId = UdpTransportService.pathId(
            transport, _pubkeyToHex(data.publicKey));
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
      debugPrint('Dropping unauthenticated clear ${packet.type.name} packet');
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
    if (transport.isIp) {
      if (udpPeerId != null) {
        onUdpPeerIdentified?.call(senderPubkey, udpPeerId);
        effectiveUdpPeerId =
            UdpTransportService.pathId(transport, _pubkeyToHex(senderPubkey));
      }
      // Any session-authenticated packet over UDP counts as liveness traffic
      // for that peer's session on this underlay, even if it is an ACK or a
      // retransmission.
      store.dispatch(PeerIpSeenAction(senderPubkey, transport.underlay));
    }
    // Any session-authenticated packet over BLE refreshes the stale sweep's
    // clock for that peer, not only ANNOUNCE. Unauthenticated packets never
    // reach this point.
    if (transport == PeerTransport.bleDirect) {
      store.dispatch(PeerBleSeenAction(senderPubkey));
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
      case PacketType.announce:
        _handleSessionAnnounce(
          packet,
          senderPubkey: senderPubkey,
          transport: transport,
          bleDeviceId: bleDeviceId,
          bleRole: bleRole,
          udpPeerId: effectiveUdpPeerId,
          rssi: rssi,
        );
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
      case PacketType.signaling:
        _handleSignaling(
          packet,
          senderPubkey: senderPubkey,
          observedIp: observedIp,
          observedPort: observedPort,
        );
      case PacketType.noiseHandshake:
      case PacketType.secureFragment:
      case PacketType.secureFragmentAck:
      case PacketType.secureSignaling:
      case PacketType.secureAttestationFragment:
      case PacketType.secureAnnounce:
        return;
    }
  }

  // ===== Handlers =====

  /// The ANNOUNCE a session carried: "Once the session is established,
  /// ANNOUNCE is sent inside it, and repeats periodically ... on every live
  /// connection of every underlay" (GNA §2.5, Heartbeat). Its record must
  /// name the key of the session that decrypted it; one that names another
  /// is dropped.
  void _handleSessionAnnounce(
    GrassrootsPacket packet, {
    required Uint8List senderPubkey,
    required PeerTransport transport,
    String? bleDeviceId,
    BleRole? bleRole,
    String? udpPeerId,
    int? rssi,
  }) {
    final AnnounceData data;
    try {
      data = protocolHandler.decodeAnnounce(packet.payload);
    } catch (e) {
      debugPrint('Dropping unverifiable session ANNOUNCE: $e');
      return;
    }
    if (_pubkeyToHex(data.publicKey) != _pubkeyToHex(senderPubkey)) {
      debugPrint('Dropping a session ANNOUNCE naming another key than its '
          'session\'s');
      return;
    }
    _handleAnnounce(
      data,
      transport: transport,
      bleDeviceId: bleDeviceId,
      bleRole: bleRole,
      udpPeerId: udpPeerId,
      rssi: rssi,
      inSession: true,
    );
  }

  void _handleAnnounce(
    AnnounceData data, {
    required PeerTransport transport,
    String? bleDeviceId,
    BleRole? bleRole,
    String? udpPeerId,
    int? rssi,
    bool inSession = false,
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

    debugPrint('Peer ${isNew ? "connected" : "updated"}: '
        '${data.pubkeyHex.substring(0, 8)}... via ${transport.name}');

    onPeerAnnounced?.call(data, transport,
        udpPeerId: udpPeerId, inSession: inSession);
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

    void acknowledge() => onFragmentAckRequested?.call(
          transport,
          peerId,
          fragment.messageId,
          fragment.index,
        );

    final senderHex = _pubkeyToHex(senderPubkey);
    final key = (senderHex: senderHex, messageId: fragment.messageId);

    // A fragment of a message saved and released lately, payload or
    // attestation: its sender retransmits because the acknowledgment of the
    // last fragment was lost. Acknowledged at once, so the sender settles;
    // not delivered again.
    if (fragmentHandler.wasReleased(
        senderHex: senderHex,
        messageId: fragment.messageId,
        type: fragment.type)) {
      acknowledge();
      return;
    }

    // A fragment of a payload already saved and not yet acknowledged by the
    // runtime: its sender retransmits because the acknowledgment of the last
    // fragment was lost. Acknowledged at once, so the sender settles; not
    // delivered again (see [isKeptDelivered]).
    final isKept = isKeptDelivered;
    if (isKept != null &&
        fragment.type == PacketType.fragment &&
        !_saving.contains(key) &&
        isKept(deliveryIdOf(senderPubkey, fragment.messageId))) {
      acknowledge();
      return;
    }

    // Every fragment but the last is acknowledged on arrival, duplicates
    // included — the sender's retransmit stops only on the acknowledgment
    // (spec §Message Transport). The fragment handler stores every fragment
    // (one of another attempt at the message replaces the stale buffer), so
    // no fragment is acknowledged that was not stored.
    final held = fragment.index == FragmentHandler.heldIndex(fragment.count);
    final reassembled =
        fragmentHandler.addFragment(fragment, senderHex: senderHex);
    if (!held) acknowledge();
    if (reassembled == null) return;

    // The message is complete; a save of it already under way acknowledges
    // the last fragment.
    if (!_saving.add(key)) return;

    // Reassembly produced the original message's payload bytes; the
    // messageId travelled in the fragment payloads. The last fragment is
    // acknowledged once the message is saved and delivered.
    _deliverMessage(
      reassembled.messageId,
      reassembled.payload,
      type: reassembled.type,
      senderPubkey: senderPubkey,
      transport: transport,
      handshakeHash: handshakeHash,
      peerId: peerId,
      heldIndex: FragmentHandler.heldIndex(fragment.count),
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

  /// Delivers a reassembled message, and then acknowledges its last fragment
  /// ([heldIndex]): a payload once [keepDelivered] has saved it, an
  /// attestation once the coordinator has it.
  void _deliverMessage(
    String messageId,
    Uint8List body, {
    required PacketType type,
    required Uint8List senderPubkey,
    required PeerTransport transport,
    required Uint8List handshakeHash,
    required int heldIndex,
    String? peerId,
  }) {
    final senderHex = _pubkeyToHex(senderPubkey);

    // The message is through: its buffer goes, and its last fragment is
    // acknowledged, under the wire messageId, on the session the completing
    // fragment arrived on (BLE: peerId = bleDeviceId; UDP: peerId =
    // udpPeerId).
    void settle() {
      fragmentHandler.release(
          senderHex: senderHex, messageId: messageId, type: type);
      _saving.remove((senderHex: senderHex, messageId: messageId));
      onFragmentAckRequested?.call(transport, peerId, messageId, heldIndex);
    }

    // The attestation exchange rides the same protocol as payloads and goes
    // to the coordinator, never to the application; a second attestation on
    // a session already attested is the coordinator's to ignore.
    if (type == PacketType.attestationFragment) {
      try {
        onAttestationReceived?.call(
            senderPubkey, body, transport, handshakeHash);
      } finally {
        settle();
      }
      return;
    }

    // Delivered every time it is reassembled and saved: a re-send of a
    // message no longer kept is delivered again (at least once, spec
    // §Message Protocol).
    final deliveryId = deliveryIdOf(senderPubkey, messageId);
    void deliverAndSettle() {
      try {
        onMessageReceived?.call(deliveryId, senderPubkey, body, transport);
      } finally {
        settle();
      }
    }

    final keep = keepDelivered;
    if (keep == null) {
      deliverAndSettle();
      return;
    }
    unawaited(keep(deliveryId, senderPubkey, body, transport).then(
      (_) => deliverAndSettle(),
      onError: (Object e) {
        // Neither delivered nor acknowledged, the buffer kept: the sender's
        // retransmission of the last fragment retries the save.
        _saving.remove((senderHex: senderHex, messageId: messageId));
        debugPrint('Message $messageId not kept, so neither delivered nor '
            'its last fragment acknowledged; its sender sends that fragment '
            'again: $e');
      },
    ));
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

  // ===== Helpers =====

  static String _pubkeyToHex(Uint8List pubkey) =>
      pubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
}
