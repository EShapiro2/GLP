# Claude Instructions for Grassroots Networking

## Working Style
Always be precise, critical, and helpful. Prefer to ask rather than assume if you have unclarities.

## Project Philosophy

Grassroots Networking is a **peer-to-peer messaging transport** — a thin layer that moves packets between devices over Bluetooth (BLE) and the Internet (UDP). It is not an application; it is the plumbing that applications like GSG build on top of.

**Core principles:**

- **Direct delivery only.** Messages go straight from sender to recipient — never through an intermediary. The sender's transport MAY queue its own outbound messages locally when the recipient is temporarily unreachable and replay them once a path opens (madGLP "fair message delivery"), but no intermediary ever holds, caches, or relays another peer's traffic.
- **Identity is a key pair.** Every device holds an Ed25519 key pair. The public key *is* the peer's identity — nicknames are cosmetic. All trust decisions flow from cryptographic verification.
- **Two transports, one interface.** BLE covers nearby peers without Internet; UDP covers the globe. Both transports surface the same abstraction to the coordinator: connect, send, receive, disconnect. BLE is preferred when both are available.
- **Clean breaks, not compatibility shims.** When refactoring, fully replace old code. No legacy wrappers, no "kept for compatibility" comments, no dead code. Update every call site. There are no installed apps in the wild — you are free to rename, restructure, and break wire formats whenever it improves the design.

## No Legacy or Compatibility Code

When refactoring, DO NOT keep old code "for legacy" or "for compatibility". Fully replace old implementations, remove unused imports and dead code, and update all call sites. Use the Redux store (`AppState`) exclusively for shared state — no mutable singletons.

This applies to **wire-format decoders too**: when you add a field to a packet, do not write the decoder to "gracefully handle truncated/old payloads where the field is missing." There is no old version in the wild — the new field is required, and a payload that lacks it is malformed and must throw. Tolerance for a hypothetical previous version is a compatibility shim by another name.

## Local Queueing, No Relaying

Grassroots does NOT relay or forward messages on behalf of other peers — the transport never carries another peer's message through an intermediary. But the sender's own outbound messages MAY be queued locally when the recipient is temporarily unreachable, and re-sent automatically once a transport path opens. This satisfies the madGLP "fair message delivery" assumption (`docs/GLP_Networking_API/sections/api.tex`, §Networking Assumptions).

The boundary: the queue lives on the sender, holds only the sender's own messages, and replays them directly to the recipient when the recipient becomes reachable. No intermediary ever holds, caches, or rebroadcasts another peer's traffic.

## BLE Discovery & Identity

Every device advertises a public-key-derived Grassroots service UUID: a fixed Grassroots prefix plus the first 8 bytes of SHA-256(public key). The UUID is only a discovery hint, never an authorization proof. Identity is established by the signed ANNOUNCE handshake, which carries the full public key, nickname, and signature. In open cold-call mode, nearby unknown BLE peers may complete ANNOUNCE; in closed mode, unknown nearby peers do not get ANNOUNCE, and friend-only metadata is sent only after signed ANNOUNCE authenticates an accepted friend.

## Dual-Role BLE Is Mandatory

Every BLE pair must converge to a **dual-role connection**: two GATT legs, with each device central on one leg and peripheral on the other. Never ship a design that intentionally leaves a pair single-link. This requirement is inviolable.

Platform asymmetries are solved by choosing **who initiates each leg** — ordering, advertisement markers, pair reform — never by abandoning a leg. The one measured constraint (an iOS central cannot open the *second* link toward an Android it is already linked with; the connect wedges in `connecting` until timeout) is routed around by making iOS open the pair's *first* leg and the Android the reverse leg. iOS devices advertise the fixed `grs-ios` local name so peers can yield the first dial to them.

When a platform behavior is **unknown** (e.g. whether an iOS↔iOS reverse leg works), attempt it and let hardware decide — do not suppress it on extrapolation. A single-link pair is acceptable only as a *transient* state that the transport keeps trying to upgrade, or where hardware has *measurably* refused the second leg and the only remaining lever is initiator order.

## Well-Connected Friends & Hole-Punching

Most mobile devices sit behind NAT and cannot accept incoming UDP connections from the public Internet. A **well-connected** device is one that has a globally routable public address — it can be reached directly by anyone.

Well-connected friends play a special role: they act as **signaling relays** to help two NAT'd peers find each other. The flow is:

1. Each device registers its current address with its well-connected friends.
2. When peer A wants to reach peer B, A asks a mutual well-connected friend for B's address.
3. The friend coordinates a simultaneous hole-punch: it tells both A and B to send packets to each other at the same time, punching holes in both NATs.
4. Once the holes are open, A and B communicate directly — the well-connected friend is no longer in the path.

**Important:** Well-connected friends relay *signaling metadata* (addresses, punch timing), never message content. This preserves the direct-delivery principle.

**Signaling is friend-only.** A well-connected device only coordinates hole-punches between peers that are both its friends. It only registers friends' addresses in its address table, only responds to address queries for friends, and only sends PUNCH_INITIATE to friends. This is a trust boundary — we don't relay for arbitrary peers.

## Redux Architecture

All peer and transport state lives in an immutable Redux store (`AppState`). Key slices: `PeersState` (discovered BLE devices + identified peers), `TransportsState` (per-transport lifecycle + public address), `MessagesState`, `FriendshipsState`, `SettingsState`. UI reads from the store and subscribes to changes. Actions describe events; reducers produce the next state. No mutable singletons.

The Redux state is a strict projection of facts emitted by the transport layers — never an inference. Reducers must not synthesize state from "I haven't heard from X in N seconds" heuristics; that's the transport layer's job to surface as an explicit event (path failed, UDX session torn down, etc.).

## Transport Layer

Two transports are available, toggled independently in settings:

- **Bluetooth (BLE)** — local, no Internet required. Preferred when both are available.
- **Internet (UDP via UDX)** — global reach, requires Internet. Uses hole-punching for NAT traversal.

The `TransportState` lifecycle for each transport is: `uninitialized → initializing → ready → active` (plus `error` and `disposed`). A transport is "usable" when it is `ready` or `active`.

User-facing UI strings should say "Internet", not "UDP" or internal protocol names.

## One Address Per Connection, Multiple Candidates Per Peer

Per **connection**, exactly one address pair is in use — there is no per-message address selection or mid-stream address switching. But a device MAY advertise multiple address **candidates** in ANNOUNCE (e.g. public IPv4, public IPv6, link-local IPv6 for the same LAN), and each peer pair selects the candidate that actually works between them: link-local on the same LAN, public IPv6 across the Internet, IPv4 as a fallback. Once a connection is established on a candidate, the pair sticks to that candidate until the path breaks.

The primary public address is discovered via an external service (e.g. seeip.org). Link-local candidates are scoped to the local network and never reach the public Internet; they exist so two devices on the same LAN can connect directly without traversing NAT.

## Peer Address Persistence

Never unilaterally clear a peer's stored UDP address. Update it when a new valid address arrives (from ANNOUNCE, signaling, or observation), and clear it only when the peer explicitly tells us they no longer have one. Stale peer cleanup, our-side disconnects, and transport restarts must not null out `udpAddress` — it is the last known location and the only way to attempt reconnection. This applies to friends and non-friends alike.

GLP feeds peer addresses into the layer's dial book via `putPeerAddress(pk, address)` (spec `GLP_Networking_API` §Connectivity and Address): it validates the `ip:port`, **creates a dial-book entry even for a peer never seen before**, and is supply-only — it never clears an address (an unparseable address throws). The address is GLP-learned (from a rendezvous agent or a BLE-adjacent friend), so the layer stays friendship-agnostic: a `putPeerAddress`-created peer carries the address but `isFriend = false` and is not `isReachable` until a session authenticates.

## Transport Independence

BLE and UDP are independent transports. Disabling or losing one must have **zero effect** on the other's connection state, peer reachability, or online status. A peer connected via UDP remains online regardless of BLE state. The stale peer logic, the UI, and the reducer must all respect this: never let a BLE disconnection degrade UDP-derived state.

Application-level callbacks (`onPeerConnected`, `onPeerDisconnected`) report reachability **per transport**, matching the spec (`GLP_Networking_API` §Connection and Reachability): each fires with the peer's public key and the `MessageTransport` whose authenticated session came up or went down. A peer reachable over both BLE and IP fires two connects; losing one of two live transports fires a disconnect for that medium while the peer stays reachable over the other. `peerTransports(pubkey)` returns the set of media a peer is currently reachable over (empty if unreachable), and `isPeerReachable(pubkey)` stays the any-medium predicate. Exposing the transport is what lets GLP compute proximity (nearby friends = friends ∩ peers reachable over BLE). The per-transport independence still holds end-to-end: a BLE disconnect never degrades UDP-derived state.
