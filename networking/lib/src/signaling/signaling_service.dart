import 'dart:async';
import 'dart:io';
import 'dart:typed_data';

import 'package:redux/redux.dart';
import 'package:flutter/foundation.dart';

import '../store/app_state.dart';
import '../store/friendships_actions.dart';
import '../store/peers_actions.dart';
import '../transport/address_utils.dart';
import 'address_table.dart';
import 'signaling_codec.dart';

/// Orchestrates signaling between peers via trusted rendezvous facilitators.
///
/// ## Reconnection protocol (matches the algorithm in the design spec)
///
/// When agent A's IP changes, A and B reconnect through a rendezvous
/// facilitator S as follows:
/// - A sends RECONNECT(initiatorPubkey=A, peerPubkey=B) to S. S observes A's
///   source address and verifies the initiator matches the signed packet sender.
/// - B (on detecting A went silent) sends AVAILABLE(peerPubkey=A) to S. S
///   observes B's source address.
/// - S matches the pair and sends each side a PUNCH_INITIATE carrying the
///   other side's observed address.
/// - Both sides punch their NATs; the deterministic UDX initiator opens the
///   stream once the path is open.
///
/// ## Facilitator roles
///
/// - **Rendezvous server** (bootstrap_anchor): runs the RECONNECT/AVAILABLE
///   matcher. The canonical facilitator implementation lives in
///   `bootstrap_anchor/lib/src/signaling_handler.dart`.
/// - **Friend mediator** (this client, when connected to both peers): handles
///   an explicit RECONNECT-style mediation request by looking up both friends'
///   addresses and sending each side a PUNCH_INITIATE. This matches the
///   friends-based rendezvous step in the GLP spec.
///
/// ## Integration
///
/// The service doesn't send packets directly. Instead, it calls
/// [sendSignaling] which the coordinator provides. This keeps the
/// service transport-agnostic.
class SignalingService {
  final Store<AppState> store;
  final SignalingCodec codec;

  /// Address table for friends we've observed via ANNOUNCE.
  ///
  /// Used to pick a target address for [requestDirectPunch] when we want a
  /// nearby friend to start punching toward our advertised address.
  final AddressTable addressTable = AddressTable();

  /// Timer for periodic stale-entry cleanup in the address table.
  Timer? _staleCleanupTimer;

  /// Counterpart map for forwarding PUNCH_READY between peers we coordinated.
  final Map<String, Uint8List> _pendingPunchCounterparts = {};

  /// Recently-coordinated punch sessions, keyed by canonical (sorted) pubkey
  /// pair, to drop duplicate matches inside the cooldown window.
  final Map<String, DateTime> _recentPunchCoordinations = {};

  static const _punchCoordinationCooldown = Duration(seconds: 15);

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

  /// Send a signaling payload to a specific UDP address — used when fanning
  /// out RECONNECT/AVAILABLE to rendezvous servers we've learned about from
  /// a friend (via RV_LIST) but haven't otherwise registered. The coordinator
  /// resolves the address via [_sendViaUdp].
  Future<bool> Function(
    Uint8List recipientPubkey,
    String address,
    Uint8List signalingPayload,
  )?
  sendSignalingToAddress;

  /// Fired when a rendezvous facilitator (or direct peer) tells us to start
  /// hole-punching. [readyRecipientPubkey] is where we should send PUNCH_READY
  /// after finishing our local punch — either the facilitator or the peer.
  void Function(
    Uint8List peerPubkey,
    String ip,
    int port,
    Uint8List readyRecipientPubkey,
  )?
  onPunchInitiate;

  /// Fired when a hole-punch completes and we should connect to the peer.
  /// (Triggered by PUNCH_READY from the other side via the facilitator.)
  void Function(Uint8List peerPubkey)? onPunchReady;

  /// Fired when a rendezvous facilitator reflects our observed public address.
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

  SignalingService({required this.store, this.codec = const SignalingCodec()}) {
    // Clean up stale address table entries every 60 seconds.
    _staleCleanupTimer = Timer.periodic(
      const Duration(seconds: 60),
      (_) => addressTable.removeStale(const Duration(minutes: 5)),
    );
  }

  // ===== Outgoing API (called by coordinator) =====

  /// Send RECONNECT(peerPubkey=target) to every trusted facilitator.
  ///
  /// Called when this agent's connectivity changed (new public IP) and it
  /// wants to reach [targetPubkey]. Rendezvous servers match this against
  /// the target's AVAILABLE. Friend mediators handle it only when they are
  /// already connected to the target and can look up both addresses.
  ///
  /// Returns the number of facilitators the request was sent to.
  Future<int> fanOutReconnect(
    Uint8List targetPubkey, {
    required Uint8List initiatorPubkey,
  }) async {
    return _fanOutCold(
      targetPubkey,
      ReconnectMessage(
        initiatorPubkey: initiatorPubkey,
        peerPubkey: targetPubkey,
      ),
      intent: 'RECONNECT',
    );
  }

  /// Ask one connected friend to mediate reconnection to [targetPubkey].
  Future<bool> requestFriendMediation({
    required Uint8List mediatorPubkey,
    required Uint8List targetPubkey,
    required Uint8List initiatorPubkey,
  }) async {
    final msg = ReconnectMessage(
      initiatorPubkey: initiatorPubkey,
      peerPubkey: targetPubkey,
    );
    return await sendSignaling?.call(mediatorPubkey, codec.encode(msg)) ??
        false;
  }

  /// Proactively mediate two connected friends from the local address table.
  void mediateFriends({
    required Uint8List requesterPubkey,
    required Uint8List targetPubkey,
  }) {
    final requesterHex = _pubkeyToHex(requesterPubkey);
    _handleRendezvous(
      senderPubkey: requesterPubkey,
      senderHex: requesterHex,
      targetPubkey: targetPubkey,
      observedIp: null,
      observedPort: null,
      intent: 'RECONNECT',
    );
  }

  /// Send AVAILABLE(peerPubkey=target) to the rendezvous facilitators that
  /// the target is known to use.
  ///
  /// Spec §3.5: when B detects A went silent, B contacts *A's* known
  /// rendezvous agents — not B's own. We learn the target's RV list (pubkey
  /// + address) via the RV_LIST signaling exchange. If the target hasn't
  /// told us their list yet, there is no rendezvous-server AVAILABLE target.
  /// Friends-based mediation is driven by RECONNECT-style mediation requests,
  /// not by the rendezvous server's two-sided AVAILABLE matcher.
  ///
  /// Returns the number of facilitators the message was sent to.
  Future<int> fanOutAvailable(Uint8List targetPubkey) async {
    final targetHex = _pubkeyToHex(targetPubkey);
    // RV lists are friendship-scoped (we only ever receive an RV_LIST from a
    // friend) and persisted with the friendship record, so they survive app
    // restart and are available before the friend's first reconnect.
    final friendship = store.state.friendships.getFriendship(targetHex);

    final payload = codec.encode(AvailableMessage(peerPubkey: targetPubkey));
    var sent = 0;

    // Primary: target's known rendezvous servers (spec-compliant). Send via
    // the address the friend told us — needed for RVs we don't ourselves
    // have configured.
    final rvHexes = <String>[];
    if (friendship != null) {
      for (final entry in friendship.knownRvServers.entries) {
        final hex = entry.key.toLowerCase();
        if (hex.isEmpty || hex == targetHex) continue;
        rvHexes.add(hex);
      }
      rvHexes.sort();
      for (final hex in rvHexes) {
        final address = friendship.knownRvServers[hex]!;
        Uint8List rvPubkey;
        try {
          rvPubkey = _hexToBytes(hex);
        } catch (_) {
          continue;
        }
        final ok =
            await sendSignalingToAddress?.call(rvPubkey, address, payload) ??
            false;
        if (ok) {
          sent++;
        } else {
          debugPrint(
            '[AVAILABLE] Could not reach RV ${hex.substring(0, 8)}... at '
            '$address',
          );
        }
      }
    }

    if (sent == 0) {
      debugPrint(
        '[AVAILABLE] No facilitators reached for ${targetHex.substring(0, 8)}'
        ' (target RV count=${rvHexes.length})',
      );
    } else {
      debugPrint(
        '[AVAILABLE] Sent for ${targetHex.substring(0, 8)} to '
        '${rvHexes.length} target RV(s)',
      );
    }
    return sent;
  }

  Future<int> _fanOutCold(
    Uint8List targetPubkey,
    SignalingMessage msg, {
    required String intent,
  }) async {
    final targetHex = _pubkeyToHex(targetPubkey);
    final facilitators = _trustedFacilitatorPubkeys(
      targetPubkeyHex: targetHex,
      excludePubkeyHex: targetHex,
    );
    if (facilitators.isEmpty) {
      debugPrint(
        '[$intent] No trusted facilitators to fan out to for ${targetHex.substring(0, 8)}...',
      );
      return 0;
    }

    final payload = codec.encode(msg);
    debugPrint(
      '[$intent] Fanning out for ${targetHex.substring(0, 8)}... to '
      '${facilitators.length} facilitator(s) (lex-ordered)',
    );

    var sent = 0;
    for (final facilitator in facilitators) {
      final ok = await sendSignaling?.call(facilitator, payload) ?? false;
      if (ok) {
        sent++;
      } else {
        debugPrint(
          '[$intent] Could not reach facilitator '
          '${_pubkeyToHex(facilitator).substring(0, 8)}...',
        );
      }
    }
    return sent;
  }

  /// Send our configured rendezvous server list to a friend so they can
  /// target AVAILABLE at exactly those servers when they detect us silent.
  Future<bool> sendRvList(
    Uint8List recipientPubkey,
    List<RvServerEntry> ownRvServers,
  ) async {
    final msg = RvListMessage(entries: ownRvServers);
    return await sendSignaling?.call(recipientPubkey, codec.encode(msg)) ??
        false;
  }

  /// Send our current accepted friend set to a friend.
  ///
  /// Recipients maintain this as their friends-of-friends map, which lets
  /// them choose common-friend mediators for friends-based rendezvous.
  Future<bool> sendFriendList(
    Uint8List recipientPubkey,
    List<Uint8List> friendPubkeys,
  ) async {
    final msg = FriendListMessage(friendPubkeys: friendPubkeys);
    return await sendSignaling?.call(recipientPubkey, codec.encode(msg)) ??
        false;
  }

  /// Directly ask a friend to start punching toward our advertised address.
  ///
  /// This is used when the target peer is already reachable over another
  /// transport such as BLE. Instead of relying on a third relay, we send
  /// PUNCH_INITIATE straight to the target and then start punching locally.
  Future<bool> requestDirectPunch(
    Uint8List targetPubkey, {
    required Uint8List requesterPubkey,
    required String requesterIp,
    required int requesterPort,
    bool requireDirectTransport = false,
  }) async {
    final targetHex = _pubkeyToHex(targetPubkey);
    final targetPeer = store.state.peers.getPeerByPubkeyHex(targetHex);
    if (targetPeer == null || !targetPeer.isFriend) {
      debugPrint(
        'Cannot request direct punch from non-friend ${targetHex.substring(0, 8)}...',
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

  /// Notify the facilitator or peer that our local punch completed.
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
  /// [observedIp] / [observedPort] carry the UDP source address observed by
  /// the transport layer — used when this client acts as a friend mediator.
  void processSignaling(
    Uint8List senderPubkey,
    Uint8List payload, {
    String? observedIp,
    int? observedPort,
  }) {
    final senderHex = _pubkeyToHex(senderPubkey);
    final senderPeer = store.state.peers.getPeerByPubkeyHex(senderHex);
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

    final senderLabel =
        senderPeer?.displayName ??
        'facilitator ${senderHex.substring(0, 8)}...';
    // debugPrint('Received signaling from $senderLabel: $msg');

    switch (msg) {
      case PunchInitiateMessage():
        _handlePunchInitiate(senderPubkey, msg);
      case PunchReadyMessage():
        _handlePunchReady(senderPubkey, msg);
      case AddrReflectMessage():
        _handleAddrReflect(senderPubkey, msg);
      case ReconnectMessage():
        if (!listEquals(msg.initiatorPubkey, senderPubkey)) {
          debugPrint(
            'Dropping RECONNECT from ${senderHex.substring(0, 8)}... — '
            'inner initiator does not match signed sender',
          );
          return;
        }
        _handleRendezvous(
          senderPubkey: senderPubkey,
          senderHex: senderHex,
          targetPubkey: msg.peerPubkey,
          observedIp: observedIp,
          observedPort: observedPort,
          intent: 'RECONNECT',
        );
      case AvailableMessage():
        _handleRendezvous(
          senderPubkey: senderPubkey,
          senderHex: senderHex,
          targetPubkey: msg.peerPubkey,
          observedIp: observedIp,
          observedPort: observedPort,
          intent: 'AVAILABLE',
        );
      case RvListMessage():
        _handleRvList(senderPubkey, msg);
      case FriendListMessage():
        _handleFriendList(senderPubkey, msg);
      case InviteRedeemedMessage():
        onInviteRedeemed?.call(senderPubkey, msg.inviteId, msg.redeemerPubkey);
      case RegisterInviteMessage():
      case RedeemInviteMessage():
      case InviteRedeemedAckMessage():
        // Server-bound messages; an agent never legitimately receives them.
        debugPrint(
          'Ignoring ${msg.runtimeType} from $senderLabel '
          '(rendezvous-server-bound message)',
        );
    }
  }

  void _handleRvList(Uint8List senderPubkey, RvListMessage msg) {
    final servers = <String, String>{};
    for (final entry in msg.entries) {
      if (entry.pubkey.length != 32) continue;
      if (entry.address.trim().isEmpty) continue;
      servers[_pubkeyToHex(entry.pubkey)] = entry.address;
    }
    final senderHex8 = _pubkeyToHex(senderPubkey).substring(0, 8);
    debugPrint(
      '[rv-list] $senderHex8 advertises ${servers.length} rendezvous server(s)',
    );
    final entries = servers.entries.toList()
      ..sort((a, b) => a.key.compareTo(b.key));
    for (final e in entries) {
      debugPrint('[rv-list]   ${e.key.substring(0, 8)} -> ${e.value}');
    }
    store.dispatch(
      FriendKnownRvServersUpdatedAction(
        publicKey: senderPubkey,
        rvServers: servers,
      ),
    );
  }

  void _handleFriendList(Uint8List senderPubkey, FriendListMessage msg) {
    final friends = <String>{};
    for (final pubkey in msg.friendPubkeys) {
      if (pubkey.length != 32) continue;
      friends.add(_pubkeyToHex(pubkey));
    }
    final senderHex8 = _pubkeyToHex(senderPubkey).substring(0, 8);
    debugPrint(
      '[fof] $senderHex8 advertises ${friends.length} accepted friend(s)',
    );
    store.dispatch(
      PeerFriendListUpdatedAction(
        publicKey: senderPubkey,
        friendPubkeyHexes: friends,
      ),
    );
  }

  /// Handle a RECONNECT-style friend mediation request by looking up both
  /// friends' addresses and dispatching PUNCH_INITIATE to both sides.
  ///
  /// The two-sided RECONNECT/AVAILABLE matcher belongs to rendezvous servers.
  /// A friend mediator follows the GLP friends-based rendezvous step: it must
  /// already be connected to the target friend, and it uses its friend address
  /// table rather than waiting for an AVAILABLE counterpart.
  void _handleRendezvous({
    required Uint8List senderPubkey,
    required String senderHex,
    required Uint8List targetPubkey,
    required String? observedIp,
    required int? observedPort,
    required String intent,
  }) {
    final targetHex = _pubkeyToHex(targetPubkey);

    if (intent != 'RECONNECT') {
      debugPrint(
        'Dropping $intent from ${senderHex.substring(0, 8)}... — friend '
        'mediators do not run the rendezvous-server matcher',
      );
      return;
    }

    if (senderHex == targetHex) {
      debugPrint(
        'Dropping $intent from ${senderHex.substring(0, 8)}... — '
        'sender targeting itself',
      );
      return;
    }

    final targetPeer = store.state.peers.getPeerByPubkeyHex(targetHex);
    if (targetPeer == null || !targetPeer.isFriend) {
      debugPrint(
        'Dropping $intent from ${senderHex.substring(0, 8)}... — target '
        '${targetHex.substring(0, 8)} is not a friend of this mediator',
      );
      return;
    }

    final senderAddress = _addressForMediation(
      senderHex,
      observedIp: observedIp,
      observedPort: observedPort,
    );
    if (senderAddress == null) {
      debugPrint(
        'Dropping $intent from ${senderHex.substring(0, 8)}... — no UDP '
        'address available for friends-based mediation',
      );
      return;
    }

    final now = DateTime.now();
    _pruneRecentPunchCoordinations(now);

    final sessionKey = _punchSessionKey(senderHex, targetHex);
    final lastCoordinated = _recentPunchCoordinations[sessionKey];
    if (lastCoordinated != null &&
        now.difference(lastCoordinated) < _punchCoordinationCooldown) {
      debugPrint(
        'Dropping duplicate $intent ${senderHex.substring(0, 8)}'
        ' ↔ ${targetHex.substring(0, 8)} — already coordinated '
        '${now.difference(lastCoordinated).inMilliseconds}ms ago',
      );
      return;
    }

    if (!targetPeer.isReachable) {
      debugPrint(
        'Dropping RECONNECT from ${senderHex.substring(0, 8)}... — target '
        '${targetHex.substring(0, 8)} is not live on this mediator',
      );
      return;
    }

    // Hole punching requires both peers to share an IP address family —
    // a UDP socket bound to IPv4 cannot reach an IPv6 destination, and vice
    // versa. Pick the target's address constrained to the sender's family
    // and bail out if no compatible candidate exists; coordinating a punch
    // across families produces 9 packets sent into the void on each side
    // and a 10s UDX handshake timeout for the user.
    final senderFamily = InternetAddress.tryParse(senderAddress.ip)?.type;
    final targetAddress = _addressForMediation(
      targetHex,
      observedIp: null,
      observedPort: null,
      requireFamily: senderFamily,
    );
    if (targetAddress == null) {
      debugPrint(
        'Dropping RECONNECT from ${senderHex.substring(0, 8)}... — no UDP '
        'address for target ${targetHex.substring(0, 8)} in the same '
        'address family ($senderFamily) as the sender '
        '(${senderAddress.ip})',
      );
      return;
    }

    _recentPunchCoordinations[sessionKey] = now;
    _pendingPunchCounterparts[senderHex] = targetPubkey;
    _pendingPunchCounterparts[targetHex] = senderPubkey;

    debugPrint(
      '[mediate] Coordinating single-step friend mediation: '
      '${senderHex.substring(0, 8)}'
      '(${senderAddress.ip}:${senderAddress.port}) <-> '
      '${targetHex.substring(0, 8)}(${targetAddress.ip}:${targetAddress.port})',
    );

    final initiateToSender = PunchInitiateMessage(
      peerPubkey: targetPubkey,
      ip: targetAddress.ip,
      port: targetAddress.port,
    );
    sendSignaling?.call(senderPubkey, codec.encode(initiateToSender));

    final initiateToTarget = PunchInitiateMessage(
      peerPubkey: senderPubkey,
      ip: senderAddress.ip,
      port: senderAddress.port,
    );
    sendSignaling?.call(targetPubkey, codec.encode(initiateToTarget));
  }

  /// Pick a UDP address suitable for use as a hole-punch target.
  ///
  /// When [requireFamily] is non-null, only candidates whose IP belongs to
  /// that family (IPv4 or IPv6) are considered. The mediator uses this to
  /// pair compatible families across the two punching peers.
  ({String ip, int port})? _addressForMediation(
    String pubkeyHex, {
    required String? observedIp,
    required int? observedPort,
    InternetAddressType? requireFamily,
  }) {
    bool matchesFamily(String ip) {
      if (requireFamily == null) return true;
      final parsed = InternetAddress.tryParse(ip);
      return parsed != null && parsed.type == requireFamily;
    }

    if (observedIp != null &&
        observedPort != null &&
        matchesFamily(observedIp)) {
      return (ip: observedIp, port: observedPort);
    }

    final registeredAddresses = addressTable.lookupAll(pubkeyHex);
    for (final registered in registeredAddresses) {
      if (!matchesFamily(registered.ip)) continue;
      final normalized = _normalizeMediationAddress(
        registered.ip,
        registered.port,
      );
      if (normalized == null || !isGloballyRoutableAddress(normalized)) {
        continue;
      }
      return (ip: registered.ip, port: registered.port);
    }

    for (final registered in registeredAddresses) {
      if (!matchesFamily(registered.ip)) continue;
      final normalized = _normalizeMediationAddress(
        registered.ip,
        registered.port,
      );
      if (normalized != null) {
        return (ip: registered.ip, port: registered.port);
      }
    }

    final peer = store.state.peers.getPeerByPubkeyHex(pubkeyHex);
    if (peer == null) return null;

    for (final candidate in peer.allUdpAddressCandidates) {
      if (!isGloballyRoutableAddress(candidate)) continue;
      final parsed = parseAddressString(candidate);
      if (parsed == null) continue;
      if (!matchesFamily(parsed.ip.address)) continue;
      return (ip: parsed.ip.address, port: parsed.port);
    }

    for (final candidate in peer.allUdpAddressCandidates) {
      final parsed = parseAddressString(candidate);
      if (parsed == null) continue;
      if (!matchesFamily(parsed.ip.address)) continue;
      return (ip: parsed.ip.address, port: parsed.port);
    }

    return null;
  }

  String? _normalizeMediationAddress(String ip, int port) {
    final parsedIp = InternetAddress.tryParse(ip);
    if (parsedIp == null) return null;
    return AddressInfo(parsedIp, port).toAddressString();
  }

  static String _punchSessionKey(String a, String b) =>
      a.compareTo(b) <= 0 ? '$a|$b' : '$b|$a';

  void _pruneRecentPunchCoordinations(DateTime now) {
    _recentPunchCoordinations.removeWhere(
      (_, ts) => now.difference(ts) >= _punchCoordinationCooldown,
    );
  }

  // ===== Incoming handlers =====

  /// Process an ANNOUNCE received over UDP from a friend.
  ///
  /// Records the friend's address candidates in the local address table
  /// (used to pick a punch target for [requestDirectPunch]) and reflects
  /// the observed external address back to the sender.
  void processAnnounceFromFriend(
    Uint8List senderPubkey, {
    String? claimedAddress,
    Iterable<String> claimedAddresses = const [],
    String? observedIp,
    int? observedPort,
  }) {
    final senderHex = _pubkeyToHex(senderPubkey);

    final normalizedClaimed = normalizeAddressStrings([
      claimedAddress,
      ...claimedAddresses,
    ]);

    for (final address in normalizedClaimed) {
      final parts = _parseAddress(address);
      if (parts == null) continue;
      addressTable.register(senderHex, parts.ip, parts.port);
    }

    if (observedIp != null && observedPort != null) {
      final observedAddress = AddressInfo(
        InternetAddress(observedIp),
        observedPort,
      ).toAddressString();
      final hasClaimedObserved = normalizedClaimed.contains(observedAddress);
      if (normalizedClaimed.isNotEmpty && !hasClaimedObserved) {
        debugPrint(
          'Address mismatch for ${senderHex.substring(0, 8)}...: '
          'claimed $normalizedClaimed, observed $observedIp:$observedPort — adding observed',
        );
      }
      addressTable.register(senderHex, observedIp, observedPort);
    }

    // Reflect the observed address back to the sender so they can learn
    // their true external address (especially the correct NAT port).
    if (observedIp != null && observedPort != null) {
      final reflect = AddrReflectMessage(ip: observedIp, port: observedPort);
      sendSignaling?.call(senderPubkey, codec.encode(reflect));
    }
  }

  /// Parse an address string in "[ip]:port" or "ip:port" format.
  static ({String ip, int port})? _parseAddress(String addr) {
    String ipStr;
    String portStr;

    if (addr.startsWith('[')) {
      final closeBracket = addr.indexOf(']');
      if (closeBracket < 0) return null;
      ipStr = addr.substring(1, closeBracket);
      final afterBracket = addr.substring(closeBracket + 1);
      if (!afterBracket.startsWith(':')) return null;
      portStr = afterBracket.substring(1);
    } else {
      final lastColon = addr.lastIndexOf(':');
      if (lastColon < 0) return null;
      ipStr = addr.substring(0, lastColon);
      portStr = addr.substring(lastColon + 1);
      if (ipStr.contains(':')) return null;
    }

    final port = int.tryParse(portStr);
    if (port == null) return null;

    return (ip: ipStr, port: port);
  }

  /// Handle PUNCH_INITIATE: a rendezvous facilitator telling us to start punching.
  void _handlePunchInitiate(Uint8List senderPubkey, PunchInitiateMessage msg) {
    debugPrint(
      'Punch initiate: punch toward '
      '${_pubkeyToHex(msg.peerPubkey).substring(0, 8)}... at ${msg.ip}:${msg.port}',
    );
    onPunchInitiate?.call(msg.peerPubkey, msg.ip, msg.port, senderPubkey);
  }

  /// Handle PUNCH_READY: either we coordinated this pair (forward to the
  /// counterpart) or it's addressed to us (fire onPunchReady).
  void _handlePunchReady(Uint8List senderPubkey, PunchReadyMessage msg) {
    final senderHex = _pubkeyToHex(senderPubkey);
    final readyHex = _pubkeyToHex(msg.peerPubkey);

    final counterpart = _pendingPunchCounterparts.remove(senderHex);
    if (counterpart != null) {
      debugPrint(
        '[mediate] Forwarding PUNCH_READY from ${senderHex.substring(0, 8)} '
        'to ${_pubkeyToHex(counterpart).substring(0, 8)}',
      );
      unawaited(sendSignaling?.call(counterpart, codec.encode(msg)));
      return;
    }

    debugPrint('Punch ready from ${readyHex.substring(0, 8)}...');
    onPunchReady?.call(msg.peerPubkey);
  }

  /// Handle ADDR_REFLECT: a facilitator telling us our observed address.
  ///
  /// This is the STUN-equivalent for Grassroots. The facilitator saw our real
  /// NAT-translated address on the incoming UDP connection and is reflecting
  /// it back. We update our public address with this value — it has the
  /// correct external port, unlike our local-port-based guess.
  void _handleAddrReflect(Uint8List senderPubkey, AddrReflectMessage msg) {
    // debugPrint('Address reflected by facilitator: ${msg.ip}:${msg.port}');
    onAddrReflected?.call(senderPubkey, msg.ip, msg.port);
  }

  // ===== Lifecycle =====

  void dispose() {
    _staleCleanupTimer?.cancel();
    _staleCleanupTimer = null;
    _pendingPunchCounterparts.clear();
    _recentPunchCoordinations.clear();
    addressTable.clear();
  }

  // ===== Helpers =====

  static String _pubkeyToHex(Uint8List pubkey) =>
      pubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  bool _isTrustedSignalingSender(String senderHex) {
    final senderPeer = store.state.peers.getPeerByPubkeyHex(senderHex);
    if (senderPeer != null && senderPeer.isFriend) {
      return true;
    }

    for (final server in store.state.settings.configuredRendezvousServers) {
      if (server.pubkeyHex.isNotEmpty &&
          server.pubkeyHex.toLowerCase() == senderHex) {
        return true;
      }
    }

    if (store.state.friendships.friendRvServers.containsKey(senderHex)) {
      return true;
    }

    return false;
  }

  /// Trusted facilitators in deterministic lexicographic order by pubkey hex.
  ///
  /// Both well-connected friends and configured rendezvous servers count as
  /// facilitators. Rendezvous servers run the two-sided matcher; friends run
  /// only the single-step friend mediation path when connected to the target.
  List<Uint8List> _trustedFacilitatorPubkeys({
    required String targetPubkeyHex,
    String? excludePubkeyHex,
  }) {
    final facilitators = <String, Uint8List>{};

    for (final friend in store.state.peers.wellConnectedFriends) {
      final friendHex = _pubkeyToHex(friend.publicKey);
      if (friendHex == excludePubkeyHex) continue;
      if (store.state.peers.friendsOfFriends[friendHex]?.contains(
            targetPubkeyHex,
          ) !=
          true) {
        continue;
      }
      facilitators[friendHex] = friend.publicKey;
    }

    for (final server in store.state.settings.configuredRendezvousServers) {
      final normalizedHex = server.pubkeyHex.toLowerCase();
      if (normalizedHex.isEmpty || normalizedHex == excludePubkeyHex) {
        continue;
      }

      try {
        facilitators.putIfAbsent(
          normalizedHex,
          () => _hexToBytes(normalizedHex),
        );
      } catch (e) {
        debugPrint('Ignoring invalid rendezvous pubkey in settings: $e');
      }
    }

    final orderedHexes = facilitators.keys.toList()..sort();
    return [for (final hex in orderedHexes) facilitators[hex]!];
  }
}
