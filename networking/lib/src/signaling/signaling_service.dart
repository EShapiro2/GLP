import 'package:redux/redux.dart';
import 'package:flutter/foundation.dart';

import '../store/app_state.dart';
import 'signaling_codec.dart';

/// Orchestrates signaling between this agent and rendezvous servers.
///
/// ## Reconnection protocol (spec §Rendezvous Server)
///
/// When agent A's IP changes, A and B reconnect through a rendezvous
/// server S as follows:
/// - A sends RECONNECT(initiatorPubkey=A, peerPubkey=B) to S. S observes A's
///   source address and verifies the initiator matches the signed packet sender.
/// - B (on detecting A went silent) sends AVAILABLE(peerPubkey=A) to S. S
///   observes B's source address.
/// - S matches the pair and sends each side a PUNCH_INITIATE carrying the
///   other side's observed address.
/// - Both sides punch their NATs; the deterministic UDX initiator opens the
///   stream once the path is open.
///
/// The RECONNECT/AVAILABLE matcher runs only on rendezvous servers
/// (bootstrap_anchor); this client fans requests out to the configured
/// servers and handles the resulting PUNCH_INITIATE / PUNCH_READY /
/// ADDR_REFLECT. The layer holds no social graph: it never mediates between
/// peers and never exchanges friend or rendezvous lists (spec: the transport
/// has no concept of friend; GLP holds the social graph).
///
/// ## Integration
///
/// The service doesn't send packets directly. Instead, it calls
/// [sendSignaling] which the coordinator provides. This keeps the
/// service transport-agnostic.
class SignalingService {
  final Store<AppState> store;
  final SignalingCodec codec;

  // ===== Callbacks (set by coordinator) =====

  /// Send a signaling payload wrapped in a GrassrootsPacket to a specific peer.
  /// The coordinator wraps the payload in a GrassrootsPacket(type: signaling),
  /// signs it, and sends it via the best available transport.
  Future<bool> Function(Uint8List recipientPubkey, Uint8List signalingPayload)?
  sendSignaling;

  /// Send a signaling payload only over an already-live direct control path.
  /// Used for peer-to-peer punch coordination where falling back to UDP would
  /// re-enter the very path we are trying to establish.
  Future<bool> Function(Uint8List recipientPubkey, Uint8List signalingPayload)?
  sendDirectSignaling;

  /// Send a signaling payload to a specific UDP address — used when redeeming
  /// a cold-call invite at the rendezvous server named in the peer link
  /// (spec §IP Cold-Call), which the redeemer need not have configured.
  Future<bool> Function(
    Uint8List recipientPubkey,
    String address,
    Uint8List signalingPayload,
  )?
  sendSignalingToAddress;

  /// Fired when a rendezvous server (or direct peer) tells us to start
  /// hole-punching. [readyRecipientPubkey] is where we should send PUNCH_READY
  /// after finishing our local punch — either the server or the peer.
  void Function(
    Uint8List peerPubkey,
    String ip,
    int port,
    Uint8List readyRecipientPubkey,
  )?
  onPunchInitiate;

  /// Fired when a hole-punch completes and we should connect to the peer.
  /// (Triggered by PUNCH_READY from the other side via the server.)
  void Function(Uint8List peerPubkey)? onPunchReady;

  /// Fired when a rendezvous server reflects our observed public address.
  /// The coordinator should update its public address with this value, and
  /// treat the [senderPubkey] as having authoritatively acknowledged a
  /// registration round-trip (the GLP spec response to a client ANNOUNCE).
  void Function(Uint8List senderPubkey, String ip, int port)? onAddrReflected;

  /// Fired when a rendezvous server forwards a redeemed cold-call invite
  /// (spec §IP Cold-Call): one of our outstanding peer links was redeemed.
  /// Receives the RV's pubkey (the authenticated sender), the InviteId, and
  /// the redeemer's public key. The coordinator validates the InviteId
  /// against its locally-recorded invites.
  void Function(
    Uint8List rvPubkey,
    Uint8List inviteId,
    Uint8List redeemerPubkey,
  )? onInviteRedeemed;

  SignalingService({required this.store, this.codec = const SignalingCodec()});

  // ===== Outgoing API (called by coordinator) =====

  /// Send RECONNECT(peerPubkey=target) to every configured rendezvous server.
  ///
  /// Called when this agent's connectivity changed (new public IP) and it
  /// wants to reach [targetPubkey]. Rendezvous servers match this against
  /// the target's AVAILABLE.
  ///
  /// Returns the number of servers the request was sent to.
  Future<int> fanOutReconnect(
    Uint8List targetPubkey, {
    required Uint8List initiatorPubkey,
  }) async {
    return _fanOutToRendezvous(
      targetPubkey,
      ReconnectMessage(
        initiatorPubkey: initiatorPubkey,
        peerPubkey: targetPubkey,
      ),
      intent: 'RECONNECT',
    );
  }

  /// Send AVAILABLE(peerPubkey=target) to every configured rendezvous server,
  /// so the servers can match the target's RECONNECT against our observed
  /// address.
  ///
  /// Returns the number of servers the message was sent to.
  Future<int> fanOutAvailable(Uint8List targetPubkey) async {
    return _fanOutToRendezvous(
      targetPubkey,
      AvailableMessage(peerPubkey: targetPubkey),
      intent: 'AVAILABLE',
    );
  }

  Future<int> _fanOutToRendezvous(
    Uint8List targetPubkey,
    SignalingMessage msg, {
    required String intent,
  }) async {
    final targetHex = _pubkeyToHex(targetPubkey);
    final servers = _configuredRendezvousPubkeys(excludePubkeyHex: targetHex);
    if (servers.isEmpty) {
      debugPrint(
        '[$intent] No rendezvous servers configured for ${targetHex.substring(0, 8)}...',
      );
      return 0;
    }

    final payload = codec.encode(msg);
    debugPrint(
      '[$intent] Fanning out for ${targetHex.substring(0, 8)}... to '
      '${servers.length} rendezvous server(s) (lex-ordered)',
    );

    var sent = 0;
    for (final server in servers) {
      final ok = await sendSignaling?.call(server, payload) ?? false;
      if (ok) {
        sent++;
      } else {
        debugPrint(
          '[$intent] Could not reach rendezvous server '
          '${_pubkeyToHex(server).substring(0, 8)}...',
        );
      }
    }
    return sent;
  }

  /// Directly ask a peer to start punching toward our advertised address.
  ///
  /// This is used when the target peer is already reachable over another
  /// transport such as BLE. Instead of relying on a rendezvous server, we
  /// send PUNCH_INITIATE straight to the target and then start punching
  /// locally.
  Future<bool> requestDirectPunch(
    Uint8List targetPubkey, {
    required Uint8List requesterPubkey,
    required String requesterIp,
    required int requesterPort,
    bool requireDirectTransport = false,
  }) async {
    final targetHex = _pubkeyToHex(targetPubkey);
    final targetPeer = store.state.peers.getPeerByPubkeyHex(targetHex);
    if (targetPeer == null) {
      debugPrint(
        'Cannot request direct punch from unknown peer ${targetHex.substring(0, 8)}...',
      );
      return false;
    }

    final msg = PunchInitiateMessage(
      peerPubkey: requesterPubkey,
      ip: requesterIp,
      port: requesterPort,
    );
    final payload = codec.encode(msg);
    final sendFn = requireDirectTransport ? sendDirectSignaling : sendSignaling;
    final sent = await sendFn?.call(targetPubkey, payload) ?? false;

    if (sent) {
      debugPrint(
        'Direct punch request sent to ${targetHex.substring(0, 8)}... '
        'for $requesterIp:$requesterPort',
      );
    } else {
      debugPrint(
        'Failed to send direct punch request to ${targetHex.substring(0, 8)}...',
      );
    }

    return sent;
  }

  /// Notify the rendezvous server or peer that our local punch completed.
  Future<bool> sendPunchReady(
    Uint8List recipientPubkey,
    Uint8List readyPeerPubkey, {
    bool requireDirectTransport = false,
  }) async {
    final msg = PunchReadyMessage(peerPubkey: readyPeerPubkey);
    final payload = codec.encode(msg);
    final sendFn = requireDirectTransport ? sendDirectSignaling : sendSignaling;
    return await sendFn?.call(recipientPubkey, payload) ?? false;
  }

  static Uint8List _hexToBytes(String hex) {
    final bytes = <int>[];
    for (var i = 0; i < hex.length; i += 2) {
      bytes.add(int.parse(hex.substring(i, i + 2), radix: 16));
    }
    return Uint8List.fromList(bytes);
  }

  // ===== Incoming processing (called by MessageRouter via coordinator) =====

  /// Process an incoming signaling packet.
  ///
  /// [senderPubkey] is the authenticated sender from the outer GrassrootsPacket.
  /// [payload] is the raw signaling payload (type byte + message data).
  void processSignaling(
    Uint8List senderPubkey,
    Uint8List payload, {
    String? observedIp,
    int? observedPort,
  }) {
    final senderHex = _pubkeyToHex(senderPubkey);
    if (!_isTrustedSignalingSender(senderHex)) {
      debugPrint(
        'Dropping signaling from untrusted sender '
        '${senderHex.substring(0, 8)}...',
      );
      return;
    }

    SignalingMessage msg;
    try {
      msg = codec.decode(payload);
    } catch (e) {
      debugPrint('Failed to decode signaling message: $e');
      return;
    }

    switch (msg) {
      case PunchInitiateMessage():
        _handlePunchInitiate(senderPubkey, msg);
      case PunchReadyMessage():
        _handlePunchReady(senderPubkey, msg);
      case AddrReflectMessage():
        _handleAddrReflect(senderPubkey, msg);
      case InviteRedeemedMessage():
        onInviteRedeemed?.call(senderPubkey, msg.inviteId, msg.redeemerPubkey);
      case ReconnectMessage():
      case AvailableMessage():
      case RegisterInviteMessage():
      case RedeemInviteMessage():
      case InviteRedeemedAckMessage():
        // Server-bound messages; an agent never legitimately receives them —
        // agents do not mediate between peers (the layer holds no social
        // graph; the RECONNECT/AVAILABLE matcher runs on rendezvous servers).
        debugPrint(
          'Ignoring ${msg.runtimeType} from ${senderHex.substring(0, 8)}... '
          '(rendezvous-server-bound message)',
        );
    }
  }

  /// Handle PUNCH_INITIATE: a rendezvous server (or direct peer) telling us
  /// to start punching.
  void _handlePunchInitiate(Uint8List senderPubkey, PunchInitiateMessage msg) {
    debugPrint(
      'Punch initiate: punch toward '
      '${_pubkeyToHex(msg.peerPubkey).substring(0, 8)}... at ${msg.ip}:${msg.port}',
    );
    onPunchInitiate?.call(msg.peerPubkey, msg.ip, msg.port, senderPubkey);
  }

  /// Handle PUNCH_READY: the other side finished its local punch.
  void _handlePunchReady(Uint8List senderPubkey, PunchReadyMessage msg) {
    final readyHex = _pubkeyToHex(msg.peerPubkey);
    debugPrint('Punch ready from ${readyHex.substring(0, 8)}...');
    onPunchReady?.call(msg.peerPubkey);
  }

  /// Handle ADDR_REFLECT: a rendezvous server telling us our observed address.
  ///
  /// This is the STUN-equivalent for Grassroots. The server saw our real
  /// NAT-translated address on the incoming UDP connection and is reflecting
  /// it back. We update our public address with this value — it has the
  /// correct external port, unlike our local-port-based guess.
  void _handleAddrReflect(Uint8List senderPubkey, AddrReflectMessage msg) {
    onAddrReflected?.call(senderPubkey, msg.ip, msg.port);
  }

  // ===== Lifecycle =====

  void dispose() {}

  // ===== Helpers =====

  static String _pubkeyToHex(Uint8List pubkey) =>
      pubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  /// Signaling is accepted from configured rendezvous servers and from peers
  /// the layer knows (identified peer records — populated by API supplies and
  /// authenticated contact). Direct punch coordination arrives from the peer
  /// itself over an authenticated session.
  bool _isTrustedSignalingSender(String senderHex) {
    if (store.state.peers.getPeerByPubkeyHex(senderHex) != null) {
      return true;
    }

    for (final server in store.state.settings.configuredRendezvousServers) {
      if (server.pubkeyHex.isNotEmpty &&
          server.pubkeyHex.toLowerCase() == senderHex) {
        return true;
      }
    }

    return false;
  }

  /// Configured rendezvous servers in deterministic lexicographic order by
  /// pubkey hex.
  List<Uint8List> _configuredRendezvousPubkeys({String? excludePubkeyHex}) {
    final servers = <String, Uint8List>{};

    for (final server in store.state.settings.configuredRendezvousServers) {
      final normalizedHex = server.pubkeyHex.toLowerCase();
      if (normalizedHex.isEmpty || normalizedHex == excludePubkeyHex) {
        continue;
      }

      try {
        servers.putIfAbsent(
          normalizedHex,
          () => _hexToBytes(normalizedHex),
        );
      } catch (e) {
        debugPrint('Ignoring invalid rendezvous pubkey in settings: $e');
      }
    }

    final orderedHexes = servers.keys.toList()..sort();
    return [for (final hex in orderedHexes) servers[hex]!];
  }
}
