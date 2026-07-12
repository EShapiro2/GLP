# Dan's Networking Layer vs GlpNetwork — Gap Analysis

**Version**: 0.8
**Date**: 2026-07-12
**Source**: https://github.com/danbachar/grassroots-networking, branch `wip/per-transport-reachability` (read 2026-07-12; the branch matching Dan's latest paper threads — `main` and `glp-api-alignment` are behind it); GLP Networking API paper; `networking-seam-spec.md` v0.5.

## v0.8 — comparison against `wip/per-transport-reachability` (2026-07-12)

Closed since v0.7:

- **Wire frame** is the paper's minimal framing plus one type byte: type(1) + length(4), 5 bytes, shared by both media. TTL, timestamp, per-packet keys, per-packet Ed25519 signature, and the clear packet id are gone; the messageId opens the MESSAGE payload (36-char UUID prefix, as fragments always carried); receivers drop clear application-data packets (`message_router.dart`: only self-signed ANNOUNCE, Noise handshake, and session-encrypted types are accepted off the wire). Matches Dan's IP-section thread exactly.
- **ANNOUNCE is a self-contained signed record**: trailing Ed25519 signature over all preceding bytes, verified against the carried key before acceptance (`protocol_handler.dart`, protocolVersion 2). Matches the paper's relocated signed-ANNOUNCE property.
- **Gap 3 / open question 2 closed**: the rotating 15-minute slot suffix is implemented — `deriveServiceUuidForSlot`, a slot-rotation timer re-advertising each slot, ±1-slot matching, per-friend per-slot scan filters, and the first-dial rule re-based on lexicographically smaller public key (same rule as IP), per Dan's three implementation notes in the BLE thread.
- **Gap 6 closed**: `generatePeerLink`/`consumePeerLink` implemented (`signaling/peer_link.dart`), matching the paper's invite construction: 256-bit nonce, one-hour expiry, base64url URI `grassroots://invite/...`, Ed25519 signature over the canonical preceding bytes, InviteId = SHA-256("glp invite id" | Nonce256), InviteKey = HKDF(Nonce256, "glp invite proof" | A_pk | RvServer | ExpiresAt), raw nonce never on the IP network, invite recorded locally as unused.
- **Gap 5 closed on this branch too**: trust default Closed; Closed mode gates both the central dial and inbound BLE ANNOUNCE acceptance.
- **`putPeerAddress(pk, address)` implemented** (design B's API call).
- **Noise session details match**: `Noise_XX_25519_ChaChaPoly_SHA256`; the expected static key is recomputed from the peer's Ed25519 key via the standard map (`pkToCurve25519`); mismatch aborts; AAD binds ciphertext to its clear packet type.

Open on this branch:

1. **ANNOUNCE still carries nickname and address candidates** (format: pubkey + version + nickLen + nick + candidates + signature). The gap-4 nickname removal and gap-9 candidate shedding done on `glp-api-alignment` did not carry over to this branch; the paper's ANNOUNCE payload is key + version only. Receivers still register a friend's announced candidates in the layer's address table — ANNOUNCE still performs address distribution, contra design B.
2. **The layer still holds friendships** — persisted `FriendshipsState`, FRIEND_LIST broadcast to friends, RV-list broadcast to friends, per-friend AVAILABLE fan-out and friend-based peer discovery, friend-address mirroring, and BLE Closed mode keyed off friendship records rather than GLP-supplied known keys. Contra the paper ("the transport has no concept of friend"; GLP holds the social graph) and the gap-2 ruling that friend-signaling is dropped in favour of the dedicated rendezvous server. This is the main outstanding architectural gap.
3. **`putIdentity` absent as a call** — identity is installed at construction, as v0.7 noted; interface shape only, not a blocker.

## What it is

A Flutter app + library (`lib/src/`): `GrassrootsNetwork` facade over two transports — BLE (via his `grassroots_bluetooth_layer` package, central+peripheral dual mode) and UDP (via `grassroots_dart_udx`, his dart_udx fork) — with Noise sessions, hole punching, and Redux state. The README is stale (describes the abandoned bitchat mesh: relay, store-and-forward); his CLAUDE.md is the current design: direct delivery only, no relay, no queuing. ~22K lines Dart. Identity is generated above the layer and installed at construction — matches putIdentity.

## Matches the paper

- **Noise XX confirmed**: `Noise_XX_25519_ChaChaPoly_SHA256`, per transport medium + peer, handshake signed by the Ed25519 identity. Answers his open paper question 1.
- **UDX over UDP** for the IP medium; hole punching implemented.
- **API shape**: `onMessageReceived(messageId, senderPubkey, payload, transport)` — the paper signature. `onPeerConnected/Disconnected(Peer)`; `isPeerReachable`; `Peer.isReachableViaBle/Udp` ≈ `peerTransports`; `send(recipientPubkey, payload)` with messageId; `setColdCallTrustLevel(open|closed)` = `setTrustLevel`; public address discovery (seeip.org) + failure flag ≈ `getPublicAddress`/`onConnectivityStatusChanged`.
- **BLE service UUID**: fixed Grassroots prefix (first 8 bytes of SHA-256("grassroots")) + per-key suffix; UUID is a discovery hint, identity only via signed ANNOUNCE — the paper's design, except the suffix (below).

## Gaps — architectural, need decisions

1. **No queuing — fair delivery unrealized at his layer.** Closed (2026-06-12): Dan's new ANNOUNCE-and-Liveness section commits the layer to heartbeat-driven replay of queued messages to revived peers — fair delivery at the layer, as the paper assumes. The adapter no longer needs a queue. (Earlier record: his send failed on unreachable peers; deferred decision was an adapter-side queue.)
2. **Rendezvous by well-connected friends, not a dedicated server.** Decision (Udi, 2026-06-11): the paper stands. Friends are on smartphones and never have a stable public address, so friend-signaling is not viable; Dan's layer is to be revised to the paper's dedicated rendezvous server. His existing configured-servers support (settings, backoff) is the basis; friend-only signaling is dropped.
3. **Static BLE suffix.** Suffix = first 8 bytes of SHA-256(pubkey) — trackable, exactly what the paper's rotating time-slotted suffix (15-min slots) prevents. His code answers open paper question 2: not implemented. Needs his confirmation of the rotating scheme or a counterproposal.

## Gaps — small, wire/API level

4. **ANNOUNCE carries nickname**; the paper moved nickname to GLP. Fixed on branch `glp-api-alignment` (2026-06-11): nickname removed from the ANNOUNCE wire and the transport identity exchange; display names stay app-level, falling back to the key fingerprint.
5. **Trust default is Open** in his settings; paper says Closed default. Fixed on branch `glp-api-alignment` (2026-06-11): constructor default and persistence fallback now closed.
6. **No peer links**: `generatePeerLink`/`consumePeerLink` absent (no invite/link code anywhere). His friendship protocol may be the intended substitute — reconcile with the paper's peer-link section.
7. **No exposed sign/verify primitives** — packets are signed internally; the adapter builds GLP's `sign`/`verify` from the installed identity directly. Note: he uses libsodium FFI for Ed25519 because the `cryptography` package costs 150–200 ms per verify on Android — relevant to our kernels on phone later (our `ed25519_edwards` is pure Dart; interop unaffected, performance to revisit).
8. **Extras beyond the paper**: `broadcast`, ACK/read-receipts, fragmentation (BLE MTU), `onPeerUpdated` — harmless; adapter ignores or uses.
9. **ANNOUNCE carries UDP address candidates** (wire layout: candidateCount + candidates) — missed in v0.1. Ruled (Udi, 2026-06-12, design B): address distribution is GLP-level; ANNOUNCE sheds its candidates; the new API call `putPeerAddress(pk, address)` (paper, IP Connectivity + summary) is how GLP feeds the layer's dial book. Dan asked in-thread to revise his ANNOUNCE section accordingly (liveness content stands).

## Integration shape

His `GrassrootsNetwork` ≈ one-to-one under our `GlpNetwork`: a `RealNetworkAdapter implements GlpNetwork` that constructs `GrassrootsIdentity` from `putIdentity`, maps callbacks, and exposes sign/verify; the fair-delivery queue now lives in his layer (gap 1, closed). Blockers before coding: none architectural — the canonical serialization is pinned in the paper (sign/2 row), and `putPeerAddress` plus per-transport reachability events are agreed in-thread. pk-identifiers in global names (parked) become live at integration.

## Open paper questions, answered by the code

1. Noise XX over BLE — yes (XX, 25519, ChaChaPoly, SHA256); confirmed by Dan in the paper thread: one handshake per medium per pair, up to two concurrent sessions per pair (now stated in the paper's Session Establishment section). Closed.
2. Rotating suffix — not implemented; static SHA-256(pk) suffix.
3. Shared static UUID for background BLE — not used; per-key UUID advertising.

## Found during this analysis (ours, not Dan's)

The shipped `verify_attestation/4` body kernel (two keys, `Ok` output) mismatched the paper's four-input succeed/fail form — introduced in seam spec v0.4. Resolved (Udi, 2026-06-11): it becomes a *guard*, renamed `valid_attestation/4`, keeping the paper's four-input succeed/fail form — guard failure deselects the clause, so no dead agent and no `Ok` output; the statement is the term `attest(PkA, PkB)`, serialized canonically as for `sign/2`. Paper renamed (System Predicates, summary, simulation appendix); seam spec v0.5; kernel rework queued for a Code session.
