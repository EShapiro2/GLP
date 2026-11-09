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
///   isolate ports; names each message it sends, forwards [send] to the router
///   and surfaces deliveries as [onMessageReceived], with the name its sender
///   gave the message as `senderMessageId`.
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
/// An agent is its public key, and a readable name is a variable standing for
/// one (IGLP, Remark "Agent Names"); "a destination, in a global name or in a
/// message, is a key or a readable name standing for one, and the seam
/// resolves both to the one agent" (IGLP 4f84612, Implementation Notes,
/// "Simulation realisation").  The directory holds each agent of the
/// simulation by its readable name, its id, and its key, and resolves either
/// to the other ([agentNamed], [pkOf], [idOf]).
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

  /// The one agent a destination [name] names, by its symbolic id, or null
  /// where it names none: [name] is the agent's readable name or its key, 64
  /// hex characters as self_key/1 answers it.  "A readable name stands for a
  /// key (Remark Agent Names), so a destination, in a global name or in a
  /// message, is a key or a readable name standing for one, and the seam
  /// resolves both to the one agent" (IGLP 4f84612, Implementation Notes,
  /// "Simulation realisation").
  String? agentNamed(String name) {
    if (_idToPk.containsKey(name)) return name;
    if (!RegExp(r'^[0-9a-fA-F]{64}$').hasMatch(name)) return null;
    return _hexToId[name.toLowerCase()];
  }

  /// All registered symbolic ids.
  Iterable<String> get ids => _idToPk.keys;

  /// All registered public keys.
  Iterable<PubKey> get keys => _idToPk.values;
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

/// A connectivity event the router emits to a client: a session's
/// connection and disconnection, and an agent's discovery and loss on a
/// proximity underlay, `onPeerDiscovered(pk)` and `onPeerLost(pk)` (GNA
/// cfbfeac, Section 3.1, Discovery).
enum ConnectivityEvent { connected, disconnected, discovered, lost }

/// A message queued in the router (while cut or held), preserving direction.
class _PendingDelivery {
  final String fromId;
  final String toId;
  final Uint8List payload;
  final String messageId;

  /// The id the sender's `send` returned for the message.
  final String senderMessageId;

  _PendingDelivery(this.fromId, this.toId, this.payload, this.messageId,
      this.senderMessageId);
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
  /// declare(trust(U, L)) ([SimulationNetworkClient.setTrustLevel] forwards it
  /// here); "The harness sets no trust level at spawn" (IGLP 45fabfc,
  /// "Simulation realisation").  Until 2026-11-08 the boot harness set the
  /// PAN level Open at every spawn.
  final Map<String, Map<ProximityUnderlay, TrustLevel>> _trust = {};

  /// Whether every agent of the boot is ready ([bootComplete]): before it no
  /// agent has an isolate to report a discovery to.
  bool _booted = false;

  /// The agents each agent has been reported discovered on the PAN and not
  /// since lost, as `a>b`, b discovered by a.  "A nearby declaration on the
  /// PAN is given seen for an adjacent agent the declaring agent has declared
  /// a peer, and for any other adjacent agent once the PAN trust levels of
  /// both are open, at the moment the second opens; it is given gone when the
  /// harness cuts the adjacency" (IGLP 45fabfc, "Simulation realisation"):
  /// the router reports discovery, `onPeerDiscovered(pk)`, at the moment the
  /// condition comes to hold, and loss, `onPeerLost(pk)`, when the harness
  /// cuts the pair (GNA cfbfeac, Section 3.1).
  final Set<String> _discovered = {};

  /// Per-agent set of ids this agent has contacted (sent to). Used for the
  /// Closed first-contact rule.
  final Map<String, Set<String>> _contacted = {};

  /// Each agent's known-peer set, by key hex: the keys its layer was given by
  /// putKnownPeer (GNA, "Known Peers": "This set is what Closed trust ...
  /// recognizes"), as declare(peer(P)) gives them.  Used for the Closed
  /// first-contact rule: "under closed, first contact from an agent not
  /// declared a peer is not answered" (IGLP, "Simulation
  /// realisation").
  final Map<String, Set<String>> _known = {};

  /// Pairs currently cut (visible disconnection) or held (invisible delay).
  final Set<String> _cutPairs = {};
  final Set<String> _heldPairs = {};

  /// Directed queues of deferred deliveries, keyed by `from>to`.
  final Map<String, List<_PendingDelivery>> _queues = {};

  /// Directed queues of first contacts the receiver has not answered, keyed
  /// by `from>to`: "under closed, first contact from an agent not declared a
  /// peer is not answered" (IGLP, "Simulation realisation").  Not answered is
  /// not lost: the sender's layer keeps a message it has saved for delivery
  /// and delivers it once a session with the peer exists (GNA, Section 2.9,
  /// send: "completes once the message is saved for delivery"; Section 2.11,
  /// fair message delivery), and the receiver comes to answer the sender when
  /// it declares it a peer, opens its PAN, or contacts it itself.  So a
  /// message a Closed receiver does not answer waits here, in order, and is
  /// routed when the receiver answers its sender ([_answer]).  Until
  /// 2026-11-08 it was dropped, so a first contact that reached the router
  /// before the receiver's declaration was lost, and the boot harness set
  /// every agent's PAN Open at its spawn.
  final Map<String, List<_PendingDelivery>> _unanswered = {};

  int _nextMessageId = 1;

  // --- Injected sinks (set by the owner) ---

  /// Deliver an inbound message to agent [toId], under the router-assigned
  /// messageId, with the senderMessageId its sender's `send` returned.
  void Function(String toId, PubKey fromPk, Uint8List payload, String messageId,
      String senderMessageId, Underlay underlay)? onDeliver;

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
  /// layer does on setTrustLevel(underlay, level), as declare(trust(U, L)) asks (IGLP,
  /// Definition "Seam Predicates"), or what the boot harness sets.
  void setTrustLevel(String id, ProximityUnderlay underlay, TrustLevel level) {
    _trust.putIfAbsent(id, _closed)[underlay] = level;
    // The PAN opened at [id]: every adjacent agent whose PAN is open too is
    // discovered by each, at the moment the second opens.
    if (underlay == ProximityUnderlay.pan && level == TrustLevel.open) {
      for (final other in directory.ids.toList()) {
        _discover(id, other);
        _discover(other, id);
      }
      // Open, [id] answers every first contact it left unanswered.
      for (final other in directory.ids.toList()) {
        _answer(other, id);
      }
    }
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
    // A peer [id] declared, adjacent, is discovered by [id] (IGLP 45fabfc).
    final other = directory.idOf(pk);
    if (other != null) {
      _discover(id, other);
      // A declared peer is answered: its first contacts [id] left
      // unanswered are routed.
      _answer(other, id);
    }
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

  /// Fire the post-boot connectivity: every ordered adjacent pair connects,
  /// once all agents have booted, and each agent discovers each adjacent
  /// agent the discovery condition already holds of ([_discover]).  Until
  /// 2026-11-08 every adjacent pair was discovered here too, whatever its
  /// trust and its known peers.
  void bootComplete() {
    _booted = true;
    final ids = directory.ids.toList();
    for (final a in ids) {
      for (final b in ids) {
        if (a == b || !isAdjacent(a, b)) continue;
        final pkB = directory.pkOf(b)!;
        onConnectivity?.call(a, pkB, Underlay.pan, ConnectivityEvent.connected);
      }
    }
    for (final a in ids) {
      for (final b in ids) {
        _discover(a, b);
      }
    }
  }

  /// Whether [a] discovers [b] on the PAN: [b] adjacent to [a] and the pair
  /// not cut, and [a] has declared [b] a peer, or the PAN trust levels of
  /// both are open (IGLP 45fabfc, "Simulation realisation").
  bool _discoverable(String a, String b) {
    if (a == b || !isAdjacent(a, b) || _cutPairs.contains(_pair(a, b))) {
      return false;
    }
    final pkB = directory.pkOf(b);
    if (pkB == null) return false;
    return isKnownPeer(a, pkB) ||
        (trustLevelOf(a, ProximityUnderlay.pan) == TrustLevel.open &&
            trustLevelOf(b, ProximityUnderlay.pan) == TrustLevel.open);
  }

  /// Report [b] discovered to [a] on the PAN, once, where the condition
  /// holds and the boot is complete.
  void _discover(String a, String b) {
    if (!_booted || _discovered.contains('$a>$b') || !_discoverable(a, b)) {
      return;
    }
    _discovered.add('$a>$b');
    onConnectivity?.call(
        a, directory.pkOf(b)!, Underlay.pan, ConnectivityEvent.discovered);
  }

  /// Report [b] lost to [a] on the PAN, where [a] discovered it.
  void _lose(String a, String b) {
    if (!_discovered.remove('$a>$b')) return;
    final pkB = directory.pkOf(b);
    if (pkB == null) return;
    onConnectivity?.call(a, pkB, Underlay.pan, ConnectivityEvent.lost);
  }

  // --- Harness controls ---

  /// Visible disconnection: fires disconnect on both sides; subsequent sends
  /// queue and flush (in order) on [restore].
  void cut(String a, String b) {
    _cutPairs.add(_pair(a, b));
    _emitBoth(a, b, ConnectivityEvent.disconnected);
    // "gone when the harness cuts the adjacency" (IGLP 45fabfc).
    _lose(a, b);
    _lose(b, a);
  }

  /// Reverse [cut]: fires connect on both sides and flushes queued messages in
  /// order (fair delivery).
  void restore(String a, String b) {
    _cutPairs.remove(_pair(a, b));
    _emitBoth(a, b, ConnectivityEvent.connected);
    // Adjacent again: each discovers the other where the condition holds.
    _discover(a, b);
    _discover(b, a);
    _flush(a, b, reverse: false);
  }

  /// Emit [event] to each of [a] and [b] about the other, where the other's
  /// key is in the directory: to [a] first.
  void _emitBoth(String a, String b, ConnectivityEvent event) {
    final pkA = directory.pkOf(a);
    final pkB = directory.pkOf(b);
    if (pkB != null) {
      onConnectivity?.call(a, pkB, Underlay.pan, event);
    }
    if (pkA != null) {
      onConnectivity?.call(b, pkA, Underlay.pan, event);
    }
  }

  /// Invisible delay: peers stay reachable, no callbacks; queued deliveries are
  /// deferred until [releaseDelivery].
  void holdDelivery(String a, String b) {
    _heldPairs.add(_pair(a, b));
  }

  /// Release a [holdDelivery], flushing queued messages in REVERSE order — the
  /// reverse-order test hook for Issue 7 (IGLP app:in-networking, Early
  /// messages).
  void releaseDelivery(String a, String b) {
    _heldPairs.remove(_pair(a, b));
    _flush(a, b, reverse: true);
  }

  // --- Send / routing ---

  /// Route a payload from [fromId] to [toId], applying trust, adjacency, cut,
  /// and hold policy. Assigns a fresh messageId; [senderMessageId] is the id
  /// the sender's `send` returned, delivered with the message.
  void routeSend(String fromId, String toId, Uint8List payload,
      String senderMessageId) {
    _contacted.putIfAbsent(fromId, () => <String>{}).add(toId);
    // [fromId] contacts [toId]: it answers [toId]'s first contacts it left
    // unanswered, before this send is routed.
    _answer(toId, fromId);

    final fromPk = directory.pkOf(fromId);
    final toPk = directory.pkOf(toId);
    if (fromPk == null || toPk == null) return; // unknown peer: drop

    if (!isAdjacent(fromId, toId)) return; // unreachable: drop

    // The Closed first-contact rule --- a Closed agent answers no first
    // contact from an agent it has never contacted --- under the level of the
    // underlay the encounter is on.  Every encounter here is a PAN one (IGLP
    // appendix-implementation-notes.tex, Simulation realisation: "every agent a
    // PAN peer of every other", "the reported underlay is the PAN
    // throughout"), so the PAN level governs it, and the LAN level, which
    // governs a LAN encounter alone, has none here to govern.  Until 2026-10-02
    // the router held one level per agent, the boot harness's, and a level an
    // agent declared never reached it; until 2026-10-09 the PAN was BLE.
    // A key the agent's layer was given by putKnownPeer is answered: "under
    // closed, first contact from an agent not declared a peer is not
    // answered" (IGLP, "Simulation realisation"); until 2026-10-11 the
    // interface had no putKnownPeer and the router no known-peer set.
    final messageId = '${_nextMessageId++}';
    // A first contact the receiver does not answer waits until it does, as
    // do the sender's later messages to it, in order ([_unanswered]).
    if (!_answers(toId, fromId) ||
        (_unanswered['$fromId>$toId']?.isNotEmpty ?? false)) {
      _unanswered
          .putIfAbsent('$fromId>$toId', () => [])
          .add(_PendingDelivery(
              fromId, toId, payload, messageId, senderMessageId));
      return;
    }
    _route(
        _PendingDelivery(fromId, toId, payload, messageId, senderMessageId));
  }

  /// Whether [toId] answers a contact from [fromId] on the PAN: its PAN is
  /// Open, or it has contacted [fromId], or it has declared [fromId] a peer.
  bool _answers(String toId, String fromId) {
    final fromPk = directory.pkOf(fromId);
    if (fromPk == null) return false;
    return trustLevelOf(toId, ProximityUnderlay.pan) == TrustLevel.open ||
        (_contacted[toId]?.contains(fromId) ?? false) ||
        isKnownPeer(toId, fromPk);
  }

  /// Route the first contacts [fromId] made to [toId] that [toId] left
  /// unanswered, in order, where [toId] now answers [fromId].
  void _answer(String fromId, String toId) {
    final key = '$fromId>$toId';
    final waiting = _unanswered[key];
    if (waiting == null || !_answers(toId, fromId)) return;
    _unanswered.remove(key);
    for (final p in waiting) {
      _route(p);
    }
  }

  /// A message answered: queued where its pair is cut or held, delivered
  /// otherwise.
  void _route(_PendingDelivery p) {
    final fromId = p.fromId, toId = p.toId, payload = p.payload;
    final messageId = p.messageId;
    final fromPk = directory.pkOf(fromId);
    if (fromPk == null) return;
    final pairKey = _pair(fromId, toId);

    if (_cutPairs.contains(pairKey) || _heldPairs.contains(pairKey)) {
      _queues.putIfAbsent('$fromId>$toId', () => []).add(p);
      return;
    }

    onDeliver?.call(
        toId, fromPk, payload, messageId, p.senderMessageId, Underlay.pan);
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
          // Still blocked by the other control: re-queue and go on.
          _queues.putIfAbsent(dir, () => []).add(p);
          continue;
        }
        final fromPk = directory.pkOf(p.fromId);
        if (fromPk == null) continue;
        onDeliver?.call(p.toId, fromPk, p.payload, p.messageId,
            p.senderMessageId, Underlay.pan);
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

  /// Forwards an outgoing send to the router, with the id [send] gave it.
  final void Function(String toId, Uint8List payload, String senderMessageId)
      sendToRouter;

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

  /// Forwards the runtime's acknowledgement of a delivered message to whoever delivered it ([acknowledge]); null where no one is told.
  final void Function(String messageId)? acknowledgeToRouter;

  /// The cold-call trust level of each proximity underlay, both closed until
  /// set.
  final Map<ProximityUnderlay, TrustLevel> _trust = {
    for (final u in ProximityUnderlay.values) u: TrustLevel.closed,
  };

  /// The known-peer set, by key hex.
  final Set<String> _known = {};

  /// The number of messages [send] has named, which names the next.
  int _sent = 0;

  SimulationNetworkClient({
    required this.selfId,
    required this.directory,
    required this.keyPair,
    required this.sendToRouter,
    this.trustToRouter,
    this.knownPeerToRouter,
    this.acknowledgeToRouter,
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

  /// The router queues what it cannot deliver yet and keeps no store, so the
  /// message is taken as soon as it is handed over, and a send is refused only
  /// to a key the directory names no agent by.  The id it returns is this
  /// agent's id and the number of messages it sent before, unique among the
  /// messages of the simulation, and the recipient receives it as
  /// `senderMessageId`.
  @override
  Future<String> send(PubKey pk, Uint8List payload) {
    final toId = directory.idOf(pk);
    if (toId == null) {
      return Future.error(ArgumentError('send to unknown peer $pk'));
    }
    final senderMessageId = '$selfId:${_sent++}';
    sendToRouter(toId, payload, senderMessageId);
    return Future.value(senderMessageId);
  }

  /// `acknowledge(messageId)` (GNA, Section 2.9): handed to [acknowledgeToRouter].  The router hands a message to the agent once, and keeps no delivered message across a restart, the simulation having no restart of its layer; so there is nothing for the acknowledgement to release here, and whoever delivered the message is told of it.
  @override
  void acknowledge(String messageId) => acknowledgeToRouter?.call(messageId);

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
    // trust level is enforced as the trust declaration sets it", IGLP
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
  // the effect of an address or a rendezvous declaration") ---

  @override
  String? getPublicAddress() =>
      throw UnsupportedError('the WAN underlay is not provided in '
          'SimulationNetwork');

  @override
  String? networkIdentity() =>
      throw UnsupportedError('the WAN underlay is not provided in '
          'SimulationNetwork');

  /// Does nothing: "the effect of an address ... declaration" is not provided
  /// (IGLP, "Simulation realisation"), the WAN underlay being absent, so an
  /// address is taken and has no effect.
  @override
  void putPeerAddress(PubKey pk, String address) {}

  /// Does nothing: "the effect of ... a rendezvous declaration" is not provided (IGLP, "Simulation realisation"), the rendezvous server being absent, so a server is taken and has no effect.
  @override
  void putRendezvousServer(PubKey pk, String address) {}

  /// Does nothing, as [putRendezvousServer] does nothing: no server was taken, and "a key not declared does nothing" (GNA, Section 4.2).
  @override
  void removeRendezvousServer(PubKey pk) {}

  @override
  String? observedPeerAddress(PubKey pk) =>
      throw UnsupportedError('peer_address/2 not provided in SimulationNetwork');

  @override
  void punchUdp(String address) =>
      throw UnsupportedError('punch_udp/1 not provided in SimulationNetwork');

  // --- Places (not provided: "the platform's geofencing, so a place
  // declaration and its removal"; [onPlaceEvent] never fires) ---

  @override
  Future<bool> declarePlace(String place, double radiusMetres) =>
      throw UnsupportedError('a place declaration is not provided in SimulationNetwork');

  @override
  Future<void> removePlace(String place) =>
      throw UnsupportedError('the removal of a place is not provided in SimulationNetwork');
}
