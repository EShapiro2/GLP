# Dan's Networking Layer vs GlpNetwork — Gap Analysis

**Version**: 0.6
**Date**: 2026-06-11
**Source**: https://github.com/danbachar/grassroots-networking (read 2026-06-11); GLP Networking API paper; `networking-seam-spec.md` v0.4.

## What it is

A Flutter app + library (`lib/src/`): `GrassrootsNetwork` facade over two transports — BLE (via his `grassroots_bluetooth_layer` package, central+peripheral dual mode) and UDP (via `grassroots_dart_udx`, his dart_udx fork) — with Noise sessions, hole punching, and Redux state. The README is stale (describes the abandoned bitchat mesh: relay, store-and-forward); his CLAUDE.md is the current design: direct delivery only, no relay, no queuing. ~22K lines Dart. Identity is generated above the layer and installed at construction — matches putIdentity.

## Matches the paper

- **Noise XX confirmed**: `Noise_XX_25519_ChaChaPoly_SHA256`, per transport medium + peer, handshake signed by the Ed25519 identity. Answers his open paper question 1.
- **UDX over UDP** for the IP medium; hole punching implemented.
- **API shape**: `onMessageReceived(messageId, senderPubkey, payload, transport)` — the paper signature. `onPeerConnected/Disconnected(Peer)`; `isPeerReachable`; `Peer.isReachableViaBle/Udp` ≈ `peerTransports`; `send(recipientPubkey, payload)` with messageId; `setColdCallTrustLevel(open|closed)` = `setTrustLevel`; public address discovery (seeip.org) + failure flag ≈ `getPublicAddress`/`onConnectivityStatusChanged`.
- **BLE service UUID**: fixed Grassroots prefix (first 8 bytes of SHA-256("grassroots")) + per-key suffix; UUID is a discovery hint, identity only via signed ANNOUNCE — the paper's design, except the suffix (below).

## Gaps — architectural, need decisions

1. **No queuing — fair delivery unrealized at his layer.** His core principle: a send to an unreachable peer fails immediately; the caller owns persistence and retry. The paper's assumption: the layer queues and delivers when connectivity is restored. Decision (Udi, 2026-06-11): deferred until real-network integration coding — in the multi-isolate simulation the router already queues, so nothing is affected today. The candidate resolution on file: the `GlpNetwork` adapter adds the queue and retry above his layer, leaving his code and the paper unchanged.
2. **Rendezvous by well-connected friends, not a dedicated server.** Decision (Udi, 2026-06-11): the paper stands. Friends are on smartphones and never have a stable public address, so friend-signaling is not viable; Dan's layer is to be revised to the paper's dedicated rendezvous server. His existing configured-servers support (settings, backoff) is the basis; friend-only signaling is dropped.
3. **Static BLE suffix.** Suffix = first 8 bytes of SHA-256(pubkey) — trackable, exactly what the paper's rotating time-slotted suffix (15-min slots) prevents. His code answers open paper question 2: not implemented. Needs his confirmation of the rotating scheme or a counterproposal.

## Gaps — small, wire/API level

4. **ANNOUNCE carries nickname**; the paper moved nickname to GLP. Fixed on branch `glp-api-alignment` (2026-06-11): nickname removed from the ANNOUNCE wire and the transport identity exchange; display names stay app-level, falling back to the key fingerprint.
5. **Trust default is Open** in his settings; paper says Closed default. Fixed on branch `glp-api-alignment` (2026-06-11): constructor default and persistence fallback now closed.
6. **No peer links**: `generatePeerLink`/`consumePeerLink` absent (no invite/link code anywhere). His friendship protocol may be the intended substitute — reconcile with the paper's peer-link section.
7. **No exposed sign/verify primitives** — packets are signed internally; the adapter builds GLP's `sign`/`verify` from the installed identity directly. Note: he uses libsodium FFI for Ed25519 because the `cryptography` package costs 150–200 ms per verify on Android — relevant to our kernels on phone later (our `ed25519_edwards` is pure Dart; interop unaffected, performance to revisit).
8. **Extras beyond the paper**: `broadcast`, ACK/read-receipts, fragmentation (BLE MTU), `onPeerUpdated` — harmless; adapter ignores or uses.
9. **ANNOUNCE carries UDP address candidates** (wire layout: candidateCount + candidates) — missed in v0.1. The paper's ANNOUNCE is the public key only; sharing the agent's address is GLP-level (IP section, Connectivity and Address). Pending the conversation with Dan — his proposed liveness section bundles address distribution into ANNOUNCE.

## Integration shape

His `GrassrootsNetwork` ≈ one-to-one under our `GlpNetwork`: a `RealNetworkAdapter implements GlpNetwork` that constructs `GrassrootsIdentity` from `putIdentity`, maps callbacks, adds the fair-delivery queue (gap 1), and exposes sign/verify. Blockers before coding: gap 1 decision, and the canonical serialization — pinned in the paper 2026-06-11 (the sign/2 row of the System Predicates section names the madGLP payload serialization as the realization-shared canonical bytes). pk-identifiers in global names (parked) become live at the same moment.

## Open paper questions, answered by the code

1. Noise XX over BLE — yes (XX, 25519, ChaChaPoly, SHA256); confirmed by Dan in the paper thread: one handshake per medium per pair, up to two concurrent sessions per pair (now stated in the paper's Session Establishment section). Closed.
2. Rotating suffix — not implemented; static SHA-256(pk) suffix.
3. Shared static UUID for background BLE — not used; per-key UUID advertising.

## Found during this analysis (ours, not Dan's)

The shipped `verify_attestation/4` body kernel (two keys, `Ok` output) mismatched the paper's four-input succeed/fail form — introduced in seam spec v0.4. Resolved (Udi, 2026-06-11): it becomes a *guard*, renamed `valid_attestation/4`, keeping the paper's four-input succeed/fail form — guard failure deselects the clause, so no dead agent and no `Ok` output; the statement is the term `attest(PkA, PkB)`, serialized canonically as for `sign/2`. Paper renamed (System Predicates, summary, simulation appendix); seam spec v0.5; kernel rework queued for a Code session.
