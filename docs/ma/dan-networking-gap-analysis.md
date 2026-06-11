# Dan's Networking Layer vs GlpNetwork — Gap Analysis

**Version**: 0.2
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

1. **No queuing — fair delivery unrealized at his layer.** His core principle: a send to an unreachable peer fails immediately; the caller owns persistence and retry. The paper's assumption: the layer queues and delivers when connectivity is restored. Resolution that respects both documents: our `GlpNetwork` adapter wraps his layer with the queue and retry (resend on `onPeerConnected`/address update), so his layer + our adapter together realize the paper's API. His code unchanged; paper unchanged.
2. **Rendezvous by well-connected friends, not a dedicated server.** Decision (Udi, 2026-06-11): the paper stands. Friends are on smartphones and never have a stable public address, so friend-signaling is not viable; Dan's layer is to be revised to the paper's dedicated rendezvous server. His existing configured-servers support (settings, backoff) is the basis; friend-only signaling is dropped.
3. **Static BLE suffix.** Suffix = first 8 bytes of SHA-256(pubkey) — trackable, exactly what the paper's rotating time-slotted suffix (15-min slots) prevents. His code answers open paper question 2: not implemented. Needs his confirmation of the rotating scheme or a counterproposal.

## Gaps — small, wire/API level

4. **ANNOUNCE carries nickname**; the paper moved nickname to GLP. Wire-format change on his side, or paper reverts. (Open paper comment 5 territory.)
5. **Trust default is Open** in his settings; paper says Closed default.
6. **No peer links**: `generatePeerLink`/`consumePeerLink` absent (no invite/link code anywhere). His friendship protocol may be the intended substitute — reconcile with the paper's peer-link section.
7. **No exposed sign/verify primitives** — packets are signed internally; the adapter builds GLP's `sign`/`verify` from the installed identity directly. Note: he uses libsodium FFI for Ed25519 because the `cryptography` package costs 150–200 ms per verify on Android — relevant to our kernels on phone later (our `ed25519_edwards` is pure Dart; interop unaffected, performance to revisit).
8. **Extras beyond the paper**: `broadcast`, ACK/read-receipts, fragmentation (BLE MTU), `onPeerUpdated` — harmless; adapter ignores or uses.

## Integration shape

His `GrassrootsNetwork` ≈ one-to-one under our `GlpNetwork`: a `RealNetworkAdapter implements GlpNetwork` that constructs `GrassrootsIdentity` from `putIdentity`, maps callbacks, adds the fair-delivery queue (gap 1), and exposes sign/verify. Blockers before coding: gap 1 decision, and the canonical serialization — pinned in the paper 2026-06-11 (the sign/2 row of the System Predicates section names the madGLP payload serialization as the realization-shared canonical bytes). pk-identifiers in global names (parked) become live at the same moment.

## Open paper questions, answered by the code

1. Noise XX over BLE — yes (XX, 25519, ChaChaPoly, SHA256).
2. Rotating suffix — not implemented; static SHA-256(pk) suffix.
3. Shared static UUID for background BLE — not used; per-key UUID advertising.

## Found during this analysis (ours, not Dan's)

The paper's `verify_attestation(Signer, PkA, PkB, Signature)` succeeds-or-fails over "the statement binding PkA and PkB"; the seam spec v0.4 and the shipped kernel have `verify_attestation(Signer?, Subject?, Sig?, Ok)` binding `Ok` to true/false over `attest(Signer, Subject)`. The mismatch entered with seam spec v0.4 (specified from the summary table, not the System Predicates section). Harmonisation ruling pending: align the paper to the implemented form, or the spec+code to the paper's.
