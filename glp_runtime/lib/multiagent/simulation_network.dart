/// SimulationNetwork — the simulation realization of [GlpNetwork].
///
/// Per IGLP app:in-networking, Simulation realisation. Two classes:
///
/// - [SimulationRouter] (main isolate): owns the identifier–key directory, the
///   adjacency relation, per-pair queues, trust levels, the known-peer sets,
///   and messageId assignment. It is transport-agnostic: delivery and connectivity events go
///   through injected sinks, so it is unit-testable in-process and reusable by
///   `IsolateManager` (which wires the sinks to isolate ports).
/// - [SimulationNetworkClient] (agent isolate): implements [GlpNetwork] over the
///   isolate ports; forwards [send] to the router and surfaces deliveries as
///   [onMessageReceived].
///
/// Keys are real Ed25519, generated synchronously with `ed25519_edwards`
/// (`cryptography` is async-only and cannot back the synchronous `sign`/`verify`
/// of the interface).
///
/// The reported underlay is the PAN throughout (IGLP, Implementation Notes,
/// "Simulation realisation": "the reported underlay is the PAN throughout").
library;

import 'dart:typed_data';

import 'package:ed25519_edwards/ed25519_edwards.dart' as ed;

import 'glp_network.dart';

/// A bidirectional symbolic-identifier ⇄ public-key directory.
///
/// Global names and GLP programs keep symbolic identifiers; the directory
/// resolves them to keys at the seam (IGLP, "Simulation realisation": "agent
/// identifiers in global names remain symbolic and are resolved to keys at the
/// seam"; Remark "Agent Names").
class NetworkDirectory {
  final Map<String, PubKey> _idToPk = {};
  final Map<String, String> _hexToId = {};

  /// Register the mapping `id ⇄ pk`.
  void register(String id, PubKey pk) {
    _idToPk[id] = pk;
    _hexToId[pk.hex] = id;
  }

  /// The public key for symbolic id [id], or null if unknown.
  PubKey? pkOf(String id) => _idToPk[id];

  /// The symbolic id for public key [pk], or null if unknown.
  String? idOf(PubKey pk) => _hexToId[pk.hex];

  /// All registered symbolic ids.
  Iterable<String> get ids => _idToPk.keys;

  /// All registered public keys.
  Iterable<PubKey> get keys => _idToPk.values;

  int get length => _idToPk.length;
}

/// Generate a fresh Ed25519 key pair (synchronous).
///
/// Returns the public key as a [PubKey] and the private key bytes (the 64-byte
/// `ed25519_edwards` private key), the pair a [SimulationNetworkClient] is
/// instantiated with.
({PubKey pub, Uint8List priv}) generateKeyPair() {
  final kp = ed.generateKey();
  return (
    pub: PubKey.fromList(kp.publicKey.bytes),
    priv: Uint8List.fromList(kp.privateKey.bytes),
  );
}

/// A connectivity event the router emits to a client.
enum ConnectivityEvent { connected, disconnected, discovered }

/// A message queued in the router (while cut or held), preserving direction.
class _PendingDelivery {
  final String fromId;
  final String toId;
  final Uint8List payload;
  final String messageId;
  final Underlay underlay;

  _PendingDelivery(
      this.fromId, this.toId, this.payload, this.messageId, this.underlay);
}

/// The simulation router (main isolate).
///
/// Owns the directory, adjacency, trust, queues, and messageId assignment, and
/// applies all routing policy. Delivery and connectivity are emitted through
/// injected sinks set by the owner (`IsolateManager` in the live stack, or a
/// test in-process).
class SimulationRouter {
  final NetworkDirectory directory = NetworkDirectory();

  /// Adjacency is total by default; explicit pairs used only when [_total] is
  /// false. Stored as sorted unordered-pair keys.
  bool _total = true;
  final Set<String> _adjacentPairs = {};

  /// Each agent's cold-call trust level, per proximity underlay: "A level is
  /// held per ProximityUnderlay, and is set with setTrustLevel(underlay,
  /// level); both levels default to Closed" (GLP-Networking-API, Trust
  /// levels).  An agent's layer sets its own when the agent declares one with
  /// trust_declare/2 ([SimulationNetworkClient.setTrustLevel] forwards it
  /// here), and the boot harness sets the PAN level Open for the plays.
  final Map<String, Map<ProximityUnderlay, TrustLevel>> _trust = {};

  /// Per-agent set of ids this agent has contacted (sent to). Used for the
  /// Closed first-contact rule.
  final Map<String, Set<String>> _contacted = {};

  /// Each agent's known-peer set, by key hex: the keys its layer was given by
  /// putKnownPeer (GNA, "Known Peers": "This set is what Closed trust ...
  /// recognizes"), as peer_declare/1 gives them.  Used for the Closed
  /// first-contact rule: "under closed, first contact from an agent not
  /// declared by peer_declare is not answered" (IGLP, "Simulation
  /// realisation").
  final Map<String, Set<String>> _known = {};

  /// Pairs currently cut (visible disconnection) or held (invisible delay).
  final Set<String> _cutPairs = {};
  final Set<String> _heldPairs = {};

  /// Directed queues of deferred deliveries, keyed by `from>to`.
  final Map<String, List<_PendingDelivery>> _queues = {};

  int _nextMessageId = 1;

  // --- Injected sinks (set by the owner) ---

  /// Deliver an inbound message to agent [toId].
  void Function(String toId, PubKey fromPk, Uint8List payload, String messageId,
      Underlay underlay)? onDeliver;

  /// Emit a connectivity event to agent [toId] about peer [peerPk].
  void Function(String toId, PubKey peerPk, Underlay underlay,
      ConnectivityEvent event)? onConnectivity;

  // --- Directory & trust setup ---

  /// Register an agent's identity, both its trust levels Closed (paper, Trust
  /// levels: "until set, both levels are Closed").
  void register(String id, PubKey pk) {
    directory.register(id, pk);
    _trust.putIfAbsent(id, _closed);
    _contacted.putIfAbsent(id, () => <String>{});
  }

  static Map<ProximityUnderlay, TrustLevel> _closed() =>
      {for (final u in ProximityUnderlay.values) u: TrustLevel.closed};

  /// Set agent [id]'s cold-call trust level of [underlay] to [level]: what its
  /// layer does on setTrustLevel(underlay, level), as trust_declare/2 asks (IGLP,
  /// Definition "Seam Predicates"), or what the boot harness sets.
  void setTrustLevel(String id, ProximityUnderlay underlay, TrustLevel level) {
    _trust.putIfAbsent(id, _closed)[underlay] = level;
  }

  /// Agent [id]'s cold-call trust level of [underlay]; Closed for an agent the
  /// router does not hold.
  TrustLevel trustLevelOf(String id, ProximityUnderlay underlay) =>
      _trust[id]?[underlay] ?? TrustLevel.closed;

  // --- Known peers ---

  /// Add [pk] to agent [id]'s known-peer set: what its layer does on
  /// putKnownPeer(pk) (GNA, "Known Peers").
  void putKnownPeer(String id, PubKey pk) {
    _known.putIfAbsent(id, () => <String>{}).add(pk.hex);
  }

  /// Remove [pk] from agent [id]'s known-peer set: what its layer does on
  /// removeKnownPeer(pk).
  void removeKnownPeer(String id, PubKey pk) {
    _known[id]?.remove(pk.hex);
  }

  /// Whether [pk] is in agent [id]'s known-peer set.
  bool isKnownPeer(String id, PubKey pk) =>
      _known[id]?.contains(pk.hex) ?? false;

  // --- Adjacency ---

  static String _pair(String a, String b) =>
      (a.compareTo(b) <= 0) ? '$a|$b' : '$b|$a';

  /// Whether [a] and [b] are adjacent (reachable). Total by default.
  bool isAdjacent(String a, String b) {
    if (a == b) return false;
    return _total ? true : _adjacentPairs.contains(_pair(a, b));
  }

  /// Restrict adjacency to the explicit pairs added via [setAdjacent].
  void setPartialAdjacency() => _total = false;

  /// Mark [a] and [b] adjacent (only meaningful under partial adjacency).
  void setAdjacent(String a, String b) {
    _adjacentPairs.add(_pair(a, b));
  }

  /// Fire the post-boot connectivity: every ordered adjacent pair connects and
  /// is discovered (spec §3, "Once all agents have booted…").
  void bootComplete() {
    final ids = directory.ids.toList();
    for (final a in ids) {
      for (final b in ids) {
        if (a == b || !isAdjacent(a, b)) continue;
        final pkB = directory.pkOf(b)!;
        onConnectivity?.call(a, pkB, Underlay.pan, ConnectivityEvent.discovered);
        onConnectivity?.call(a, pkB, Underlay.pan, ConnectivityEvent.connected);
      }
    }
  }

  // --- Harness controls ---

  /// Visible disconnection: fires disconnect on both sides; subsequent sends
  /// queue and flush (in order) on [restore].
  void cut(String a, String b) {
    _cutPairs.add(_pair(a, b));
    final pkA = directory.pkOf(a);
    final pkB = directory.pkOf(b);
    if (pkB != null) {
      onConnectivity?.call(
          a, pkB, Underlay.pan, ConnectivityEvent.disconnected);
    }
    if (pkA != null) {
      onConnectivity?.call(
          b, pkA, Underlay.pan, ConnectivityEvent.disconnected);
    }
  }

  /// Reverse [cut]: fires connect on both sides and flushes queued messages in
  /// order (fair delivery).
  void restore(String a, String b) {
    _cutPairs.remove(_pair(a, b));
    final pkA = directory.pkOf(a);
    final pkB = directory.pkOf(b);
    if (pkB != null) {
      onConnectivity?.call(a, pkB, Underlay.pan, ConnectivityEvent.connected);
    }
    if (pkA != null) {
      onConnectivity?.call(b, pkA, Underlay.pan, ConnectivityEvent.connected);
    }
    _flush(a, b, reverse: false);
  }

  /// Invisible delay: peers stay reachable, no callbacks; queued deliveries are
  /// deferred until [releaseDelivery].
  void holdDelivery(String a, String b) {
    _heldPairs.add(_pair(a, b));
  }

  /// Release a [holdDelivery], flushing queued messages in REVERSE order — the
  /// reverse-order test hook for Issue 7 (spec §3, §7.3).
  void releaseDelivery(String a, String b) {
    _heldPairs.remove(_pair(a, b));
    _flush(a, b, reverse: true);
  }

  // --- Send / routing ---

  /// Route a payload from [fromId] to [toId], applying trust, adjacency, cut,
  /// and hold policy. Assigns a fresh messageId.
  void routeSend(String fromId, String toId, Uint8List payload) {
    _contacted.putIfAbsent(fromId, () => <String>{}).add(toId);

    final fromPk = directory.pkOf(fromId);
    final toPk = directory.pkOf(toId);
    if (fromPk == null || toPk == null) return; // unknown peer: drop

    if (!isAdjacent(fromId, toId)) return; // unreachable: drop

    // The Closed first-contact rule --- a Closed agent receives no first contact
    // from an agent it has never contacted --- under the level of the
    // underlay the encounter is on.  Every encounter here is a PAN one (IGLP
    // appendix-implementation-notes.tex, Simulation realisation: "every agent a
    // PAN peer of every other", "the reported underlay is the PAN
    // throughout"), so the PAN level governs it, and the LAN level, which
    // governs a LAN encounter alone, has none here to govern.  Until 2026-10-02
    // the router held one level per agent, the boot harness's, and a level an
    // agent declared never reached it; until 2026-10-09 the PAN was BLE.
    // A key the agent's layer was given by putKnownPeer is answered: "under
    // closed, first contact from an agent not declared by peer_declare is not
    // answered" (IGLP, "Simulation realisation"); until 2026-10-11 the
    // interface had no putKnownPeer and the router no known-peer set.
    if (trustLevelOf(toId, ProximityUnderlay.pan) == TrustLevel.closed &&
        !(_contacted[toId]?.contains(fromId) ?? false) &&
        !isKnownPeer(toId, fromPk)) {
      return;
    }

    final messageId = '${_nextMessageId++}';
    final pairKey = _pair(fromId, toId);

    if (_cutPairs.contains(pairKey) || _heldPairs.contains(pairKey)) {
      _queues
          .putIfAbsent('$fromId>$toId', () => [])
          .add(_PendingDelivery(fromId, toId, payload, messageId, Underlay.pan));
      return;
    }

    onDeliver?.call(toId, fromPk, payload, messageId, Underlay.pan);
  }

  /// Flush both directed queues of the pair {a, b}, skipping any still cut or
  /// held. [reverse] flushes LIFO (release hook); otherwise FIFO (restore).
  void _flush(String a, String b, {required bool reverse}) {
    for (final dir in ['$a>$b', '$b>$a']) {
      final q = _queues.remove(dir);
      if (q == null) continue;
      final pending = reverse ? q.reversed.toList() : q;
      for (final p in pending) {
        final pairKey = _pair(p.fromId, p.toId);
        if (_cutPairs.contains(pairKey) || _heldPairs.contains(pairKey)) {
          // Still blocked by the other control: re-queue and stop this dir.
          _queues.putIfAbsent(dir, () => []).add(p);
          continue;
        }
        final fromPk = directory.pkOf(p.fromId);
        if (fromPk == null) continue;
        onDeliver?.call(p.toId, fromPk, p.payload, p.messageId, p.underlay);
      }
    }
  }
}

/// The agent-side [GlpNetwork] (agent isolate).
///
/// Implements the interface over injected sinks: [send] forwards to the router,
/// reachability answers from the directory, identity/sign/verify use real
/// Ed25519. The owning isolate entry fires [onMessageReceived] on delivery and
/// the connectivity callbacks on router events.
///
/// Instantiated with the agent's key pair, as the layer is (GNA, "Identity":
/// "supplied to the layer's constructor when it is instantiated; it is
/// immutable for the layer's lifetime").  Until 2026-10-11 the pair was
/// installed after construction, by `putIdentity`.
class SimulationNetworkClient extends GlpNetwork {
  /// This agent's symbolic id (for routing through the directory).
  final String selfId;

  /// The shared identifier–key directory (published to every adapter).
  final NetworkDirectory directory;

  /// This agent's key pair: the public key [getIdentity] answers, and the
  /// 64-byte `ed25519_edwards` private key [sign] signs under.
  final ({PubKey pub, Uint8List priv}) keyPair;

  /// Forwards an outgoing send to the router.
  final void Function(String toId, Uint8List payload) sendToRouter;

  /// Forwards this agent's trust declaration to the router, which enforces it
  /// ([SimulationRouter.setTrustLevel]); null where no router routes this
  /// client's traffic.
  final void Function(ProximityUnderlay underlay, TrustLevel level)?
      trustToRouter;

  /// Forwards a change to this agent's known-peer set to the router, which
  /// consults it in the Closed first-contact rule
  /// ([SimulationRouter.putKnownPeer], [SimulationRouter.removeKnownPeer]):
  /// [known] true for putKnownPeer, false for removeKnownPeer.  Null where no
  /// router routes this client's traffic.
  final void Function(PubKey pk, bool known)? knownPeerToRouter;

  /// The cold-call trust level of each proximity underlay, both closed until
  /// set.
  final Map<ProximityUnderlay, TrustLevel> _trust = {
    for (final u in ProximityUnderlay.values) u: TrustLevel.closed,
  };

  /// The known-peer set, by key hex.
  final Set<String> _known = {};

  SimulationNetworkClient({
    required this.selfId,
    required this.directory,
    required this.keyPair,
    required this.sendToRouter,
    this.trustToRouter,
    this.knownPeerToRouter,
  });

  // --- Identity ---

  @override
  PubKey getIdentity() => keyPair.pub;

  // --- Known peers ---

  @override
  void putKnownPeer(PubKey pk) {
    _known.add(pk.hex);
    knownPeerToRouter?.call(pk, true);
  }

  @override
  void removeKnownPeer(PubKey pk) {
    _known.remove(pk.hex);
    knownPeerToRouter?.call(pk, false);
  }

  /// Whether [pk] is in this client's locally-recorded known-peer set.
  bool isKnownPeer(PubKey pk) => _known.contains(pk.hex);

  // --- Reachability (from the directory; total adjacency in simulation) ---

  @override
  bool isPeerReachable(PubKey pk) => directory.idOf(pk) != null;

  @override
  List<Underlay> peerUnderlays(PubKey pk) =>
      directory.idOf(pk) != null ? const [Underlay.pan] : const [];

  // --- Send ---

  @override
  void send(PubKey pk, Uint8List payload) {
    final toId = directory.idOf(pk);
    if (toId == null) {
      throw ArgumentError('send to unknown peer $pk');
    }
    sendToRouter(toId, payload);
  }

  // --- Signing (real Ed25519, synchronous) ---

  @override
  Uint8List sign(Uint8List message) =>
      ed.sign(ed.PrivateKey(keyPair.priv), message);

  @override
  bool verify(PubKey signer, Uint8List message, Uint8List signature) {
    return ed.verify(ed.PublicKey(signer.bytes), message, signature);
  }

  // --- Proximity (the PAN throughout) ---

  @override
  List<DiscoveredPeer> getPeers() => [
        for (final pk in directory.keys)
          if (directory.idOf(pk) != selfId)
            DiscoveredPeer(pk, ProximityUnderlay.pan),
      ];

  @override
  void setTrustLevel(ProximityUnderlay underlay, TrustLevel level) {
    // Trust is enforced router-side in the simulation ("The PAN's cold-call
    // trust level is enforced as trust_declare sets it", IGLP
    // appendix-implementation-notes.tex, Simulation realisation): the level is
    // recorded here and handed to the router, which applies it to the first
    // contacts it routes to this agent.  Until 2026-10-02 it was recorded here
    // alone and governed nothing.
    _trust[underlay] = level;
    trustToRouter?.call(underlay, level);
  }

  /// This client's locally-recorded trust level for [underlay].
  TrustLevel trustLevelOf(ProximityUnderlay underlay) => _trust[underlay]!;

  // --- WAN (not provided: IGLP, "Simulation realisation": "Not provided: the
  // WAN underlay and the rendezvous server, so peer_address/2, punch_udp/1 and
  // the effect of address_declare/2") ---

  @override
  String? getPublicAddress() =>
      throw UnsupportedError('the WAN underlay is not provided in '
          'SimulationNetwork');

  @override
  String? networkIdentity() =>
      throw UnsupportedError('the WAN underlay is not provided in '
          'SimulationNetwork');

  @override
  Future<String> generatePeerLink() =>
      throw UnsupportedError('peer links not supported in SimulationNetwork');

  @override
  Future<PubKey> consumePeerLink(String uri) =>
      throw UnsupportedError('peer links not supported in SimulationNetwork');

  /// Does nothing: "the effect of address_declare/2" is not provided (IGLP,
  /// "Simulation realisation"), the WAN underlay being absent, so an address
  /// is taken and has no effect.
  @override
  void putPeerAddress(PubKey pk, String address) {}

  @override
  String? observedPeerAddress(PubKey pk) =>
      throw UnsupportedError('peer_address/2 not provided in SimulationNetwork');

  @override
  void punchUdp(String address) =>
      throw UnsupportedError('punch_udp/1 not provided in SimulationNetwork');

  // --- Places (not provided: "the platform's geofencing, so place_declare/3
  // and place_remove/1"; [onPlaceEvent] never fires) ---

  @override
  Future<bool> declarePlace(String place, double radiusMetres) =>
      throw UnsupportedError('place_declare/3 not provided in SimulationNetwork');

  @override
  Future<void> removePlace(String place) =>
      throw UnsupportedError('place_remove/1 not provided in SimulationNetwork');
}
