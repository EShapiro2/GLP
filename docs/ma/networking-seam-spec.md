# madGLP Networking Seam Spec

**Version**: 0.1
**Date**: 2026-06-10
**Status**: DRAFT
**Source**: GLP Networking API Specification paper (`~/Grassroots/GLP-Networking-API`), Sections 2–3 and the Simulation Realization appendix; `madGLP-spec.md` v5.6; `known-issues.md` Issue 7.

---

## 1. Purpose

madGLP talks to networking through one Dart interface, `GlpNetwork`, transcribing the paper's API. Two realizations: `SimulationNetwork` (this spec; wraps the existing `IsolateManager`) and the real BLE/IP layer (Dan's, per the paper). Integration is a backend swap behind the same interface.

This spec covers: the interface (Section 2), the simulation realization (Section 3), the runtime adapter (Section 4), the hold mechanism (Section 5), the wire format (Section 6), and tests (Section 7).

---

## 2. The GlpNetwork Interface

File: `glp_runtime/lib/multiagent/glp_network.dart`. The interface transcribes the paper API verbatim; callbacks are settable fields, matching `MadContext.onMessageReady` house style.

```dart
enum Transport { ble, ip }
enum TrustLevel { open, closed }

/// 32-byte Ed25519 public key with value equality and a hex string form.
class PubKey { final Uint8List bytes; ... }

/// A device found by scanning, before its identity is known.
/// Carries transport-specific identification; in simulation, the pk.
class DiscoveredPeer { ... }

abstract class GlpNetwork {
  // Identity (paper Section 2.1)
  void putIdentity(PubKey pub, Uint8List priv);
  PubKey getIdentity();

  // Connection and reachability (Section 2.2)
  void Function(PubKey pk, Transport t)? onPeerConnected;
  void Function(PubKey pk, Transport t)? onPeerDisconnected;
  bool isPeerReachable(PubKey pk);
  List<Transport> peerTransports(PubKey pk);

  // Point-to-point communication (Section 2.5)
  void send(PubKey pk, Uint8List payload);
  void Function(PubKey sender, Uint8List payload,
                String messageId, Transport t)? onMessageReceived;

  // Signing primitives backing the system predicates (Section 2 / summary):
  // the layer holds the private key.
  Uint8List sign(Uint8List message);
  bool verify(PubKey signer, Uint8List message, Uint8List signature);

  // BLE (Section 3)
  List<DiscoveredPeer> getPeers();
  void Function(DiscoveredPeer p)? onPeerDiscovered;
  void setTrustLevel(TrustLevel level);

  // IP (Section 4) — UnsupportedError in SimulationNetwork
  void Function(String? oldAddr, String newAddr)? onConnectivityStatusChanged;
  String getPublicAddress();
  String generatePeerLink();
  void consumePeerLink(String uri);
}
```

Session establishment (paper Section 2.3) does not appear in the interface: it is internal to a realization. The GLP body kernels `sign/2` and `verify_attestation/4` are built on `sign`/`verify`; `peer_address/2` and `punch_udp/1` are IP-only and out of scope here.

---

## 3. SimulationNetwork

File: `glp_runtime/lib/multiagent/simulation_network.dart`. Two classes:

- **SimulationRouter** (main isolate): owns the identifier–key directory, the adjacency relation, per-pair queues, trust levels, and messageId assignment. `IsolateManager` keeps its boot duties (spawn, Ready/Start) and delegates all message routing to the router.
- **SimulationNetworkClient** (agent isolate): implements `GlpNetwork` over the existing isolate ports; forwards `send` to the router, surfaces router events as the interface callbacks.

**Boot.** The harness generates an Ed25519 key pair per agent (Dart `cryptography` package unless the repo already carries an Ed25519 implementation), installs it via `putIdentity`, and publishes the identifier–key directory to every adapter (Section 4).

**Adjacency and reachability.** The router holds an adjacency relation, total by default. Once all agents have booted, it fires `onPeerConnected(pk, ble)` for every ordered pair. `isPeerReachable` and `peerTransports` answer from the relation: `[ble]` if adjacent, `[]` otherwise. Harness controls:

- `cut(a, b)` / `restore(a, b)` — visible disconnection: fires `onPeerDisconnected` / `onPeerConnected` on both sides; messages sent while cut queue in the router and flush on restore (fair delivery).
- `holdDelivery(a, b)` / `releaseDelivery(a, b)` — invisible delay: peers stay reachable, delivery of queued messages is deferred until release. Release may deliver in any order; this is the reverse-order test hook.

**Discovery and trust.** Creating adjacency fires `onPeerDiscovered`; `getPeers` returns the adjacent peers. ANNOUNCE is subsumed: the router supplies the authenticated sender key on every delivery. `setTrustLevel` is enforced by the router: under Closed, an agent does not connect to, and receives no first contact from, agents it has never contacted; default Closed per the paper; the boot harness sets Open for the plays.

**Delivery.** `send(pk, payload)` resolves pk to the destination isolate via the directory and delivers the payload bytes with a router-assigned `messageId` and transport `ble`. No corruption, sender authenticated by construction; order arbitrary.

---

## 4. Runtime Adapter

The isolate entry and `MadContext` talk to `GlpNetwork` instead of `mainPort`:

- **Outgoing**: `ctx.onMessageReady(destId, msg)` becomes `network.send(directory[destId], msg.payload)`.
- **Incoming**: `network.onMessageReceived(senderPk, payload, ...)` deserializes `(globalName, value)` and calls `ctx.handleMadAssignment(globalName, value, directory.idOf(senderPk))`.

`AgentConfig` gains `keyPair` and `directory` fields. Global names and GLP programs keep symbolic agent identifiers; identifiers are resolved to keys only at the seam. Over a real network the agent identifier in global names will be the public key — deferred, per the paper appendix.

The body kernels `sign/2` and `verify_attestation/4` call `network.sign` / `network.verify`, with GLP-term encoding per the paper's system-predicate definitions.

`MessageType` disappears from the seam: the wire carries payload bytes only (Section 6). `MessageType.agentMessage` was already legacy and ignored.

---

## 5. Hold Mechanism (Issue 7)

Required by `madGLP-spec.md` v5.6 Section 8.3 (Early Messages) and recorded as `known-issues.md` Issue 7: in `mad_context.dart`, an `_r(p, i)` assignment arriving before its entry exists is stored in a hold table keyed `(remoteAgent, remoteIndex)` and delivered when `localize()` creates the entry, instead of throwing. Mandatory in the same change as the seam — the simulation router delivers in arbitrary order.

---

## 6. Wire Format

Unchanged: a payload is the serialized `(globalName, value)` assignment of `madGLP-spec.md` Section 8 (`payload_serializer.dart`). One message kind; a cold-call is an assignment to the index-0 serializer, distinguished by its global name, not by a type field. The seam carries payloads opaquely.

---

## 7. Tests

1. **Baseline**: the existing multiagent suite passes unchanged over the new seam (run before and after, per `GLP/CLAUDE.md`).
2. **Adjacency cut/restore**: messages sent while cut are queued and delivered on restore; callbacks fire on both sides.
3. **Reverse-order delivery**: hold a pair, send carrier and `_r` assignment, release in reverse order; the run completes with the same outcome (Issue 7 test).
4. **Trust level**: under Closed, a cold-call from an unknown agent is not delivered; under Open it is.
5. **Plays check**: the plays do not rely on per-sender cold-call order (Issue 7 Related Check).

---

## 8. Out of Scope

IP medium, rendezvous server, peer links, Noise sessions, rotating BLE suffix, public-key agent identifiers in global names.

---

## 9. Plan

Spec first (this document), then implementation in a GLP Claude Code session under the `GLP/CLAUDE.md` discipline: baseline tests green before commits, scope `glp_runtime/lib/multiagent/` + `test/multiagent/`.

New files: `glp_network.dart`, `simulation_network.dart`. Changed: `isolate_manager.dart`, `mad_context.dart`, `boot_loader.dart`, `agent_runtime.dart`, tests.

---

## Document History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 0.1 | 2026-06-10 | Claude | Initial draft: GlpNetwork interface transcribing the paper API; SimulationNetwork (router + client) wrapping IsolateManager; runtime adapter with identifier–key directory; Issue 7 hold mechanism mandatory; tests. |
