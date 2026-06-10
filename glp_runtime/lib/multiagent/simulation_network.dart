/// SimulationNetwork — the simulation realization of [GlpNetwork].
///
/// Per `docs/ma/networking-seam-spec.md` v0.2 Section 3. Two classes:
///
/// - [SimulationRouter] (main isolate): owns the identifier–key directory, the
///   adjacency relation, per-pair queues, trust levels, and messageId
///   assignment. It is transport-agnostic: delivery and connectivity events go
///   through injected sinks, so it is unit-testable in-process and reusable by
///   `IsolateManager` (which wires the sinks to isolate ports).
/// - [SimulationNetworkClient] (agent isolate): implements [GlpNetwork] over the
///   isolate ports; forwards [send] to the router and surfaces deliveries as
///   [onMessageReceived].
///
/// Keys are real Ed25519, generated synchronously with `ed25519_edwards`
/// (`cryptography` is async-only and cannot back the synchronous `sign`/`verify`
/// of Section 2).
library;

import 'dart:typed_data';

import 'package:ed25519_edwards/ed25519_edwards.dart' as ed;

import 'glp_network.dart';

/// A bidirectional symbolic-identifier ⇄ public-key directory (spec §4).
///
/// Global names and GLP programs keep symbolic identifiers; the directory
/// resolves them to keys at the seam.
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
/// `ed25519_edwards` private key) for [GlpNetwork.putIdentity].
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
  final Transport transport;

  _PendingDelivery(
      this.fromId, this.toId, this.payload, this.messageId, this.transport);
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

  /// Per-agent trust level. Default Closed per the paper; the boot harness sets
  /// Open for the plays.
  final Map<String, TrustLevel> _trust = {};

  /// Per-agent set of ids this agent has contacted (sent to). Used for the
  /// Closed first-contact rule.
  final Map<String, Set<String>> _contacted = {};

  /// Pairs currently cut (visible disconnection) or held (invisible delay).
  final Set<String> _cutPairs = {};
  final Set<String> _heldPairs = {};

  /// Directed queues of deferred deliveries, keyed by `from>to`.
  final Map<String, List<_PendingDelivery>> _queues = {};

  int _nextMessageId = 1;

  // --- Injected sinks (set by the owner) ---

  /// Deliver an inbound message to agent [toId].
  void Function(String toId, PubKey fromPk, Uint8List payload, String messageId,
      Transport t)? onDeliver;

  /// Emit a connectivity event to agent [toId] about peer [peerPk].
  void Function(
          String toId, PubKey peerPk, Transport t, ConnectivityEvent event)?
      onConnectivity;

  // --- Directory & trust setup ---

  /// Register an agent's identity. Default trust is Closed (paper §3).
  void register(String id, PubKey pk) {
    directory.register(id, pk);
    _trust.putIfAbsent(id, () => TrustLevel.closed);
    _contacted.putIfAbsent(id, () => <String>{});
  }

  /// Set agent [id]'s trust level (the boot harness sets Open for plays).
  void setTrustLevel(String id, TrustLevel level) {
    _trust[id] = level;
  }

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
        onConnectivity?.call(a, pkB, Transport.ble, ConnectivityEvent.discovered);
        onConnectivity?.call(a, pkB, Transport.ble, ConnectivityEvent.connected);
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
          a, pkB, Transport.ble, ConnectivityEvent.disconnected);
    }
    if (pkA != null) {
      onConnectivity?.call(
          b, pkA, Transport.ble, ConnectivityEvent.disconnected);
    }
  }

  /// Reverse [cut]: fires connect on both sides and flushes queued messages in
  /// order (fair delivery).
  void restore(String a, String b) {
    _cutPairs.remove(_pair(a, b));
    final pkA = directory.pkOf(a);
    final pkB = directory.pkOf(b);
    if (pkB != null) {
      onConnectivity?.call(a, pkB, Transport.ble, ConnectivityEvent.connected);
    }
    if (pkA != null) {
      onConnectivity?.call(b, pkA, Transport.ble, ConnectivityEvent.connected);
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

    // Closed first-contact rule: a Closed agent receives no first contact from
    // an agent it has never contacted.
    if (_trust[toId] == TrustLevel.closed &&
        !(_contacted[toId]?.contains(fromId) ?? false)) {
      return;
    }

    final messageId = '${_nextMessageId++}';
    final pairKey = _pair(fromId, toId);

    if (_cutPairs.contains(pairKey) || _heldPairs.contains(pairKey)) {
      _queues
          .putIfAbsent('$fromId>$toId', () => [])
          .add(_PendingDelivery(fromId, toId, payload, messageId, Transport.ble));
      return;
    }

    onDeliver?.call(toId, fromPk, payload, messageId, Transport.ble);
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
        onDeliver?.call(p.toId, fromPk, p.payload, p.messageId, p.transport);
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
class SimulationNetworkClient extends GlpNetwork {
  /// This agent's symbolic id (for routing through the directory).
  final String selfId;

  /// The shared identifier–key directory (published to every adapter, spec §4).
  final NetworkDirectory directory;

  /// Forwards an outgoing send to the router.
  final void Function(String toId, Uint8List payload) sendToRouter;

  PubKey? _pub;
  Uint8List? _priv;
  TrustLevel _trust = TrustLevel.closed;

  SimulationNetworkClient({
    required this.selfId,
    required this.directory,
    required this.sendToRouter,
  });

  // --- Identity ---

  @override
  void putIdentity(PubKey pub, Uint8List priv) {
    _pub = pub;
    _priv = priv;
  }

  @override
  PubKey getIdentity() {
    final p = _pub;
    if (p == null) throw StateError('Identity not set (call putIdentity first)');
    return p;
  }

  // --- Reachability (from the directory; total adjacency in simulation) ---

  @override
  bool isPeerReachable(PubKey pk) => directory.idOf(pk) != null;

  @override
  List<Transport> peerTransports(PubKey pk) =>
      directory.idOf(pk) != null ? const [Transport.ble] : const [];

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
  Uint8List sign(Uint8List message) {
    final priv = _priv;
    if (priv == null) throw StateError('Identity not set (call putIdentity first)');
    return ed.sign(ed.PrivateKey(priv), message);
  }

  @override
  bool verify(PubKey signer, Uint8List message, Uint8List signature) {
    return ed.verify(ed.PublicKey(signer.bytes), message, signature);
  }

  // --- BLE ---

  @override
  List<DiscoveredPeer> getPeers() => [
        for (final pk in directory.keys)
          if (directory.idOf(pk) != selfId) DiscoveredPeer(pk, Transport.ble),
      ];

  @override
  void setTrustLevel(TrustLevel level) {
    // Trust is enforced router-side in the simulation; record locally so
    // getIdentity-style queries stay consistent. The harness sets the router's
    // level at boot.
    _trust = level;
  }

  /// This client's locally-recorded trust level.
  TrustLevel get trustLevel => _trust;

  // --- IP (unsupported in simulation, spec §2/§8) ---

  @override
  String getPublicAddress() =>
      throw UnsupportedError('IP transport not supported in SimulationNetwork');

  @override
  String generatePeerLink() =>
      throw UnsupportedError('peer links not supported in SimulationNetwork');

  @override
  void consumePeerLink(String uri) =>
      throw UnsupportedError('peer links not supported in SimulationNetwork');
}
