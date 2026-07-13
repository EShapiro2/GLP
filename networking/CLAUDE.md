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

Grassroots does NOT relay or forward messages on behalf of other peers — the transport never carries another peer's message through an intermediary. But the sender's own outbound messages MAY be queued locally when the recipient is temporarily unreachable, and re-sent automatically once a transport path opens. This satisfies the madGLP "fair message delivery" assumption (GLP-Networking-API paper, §Networking Assumptions).

The boundary: the queue lives on the sender, holds only the sender's own messages, and replays them directly to the recipient when the recipient becomes reachable. No intermediary ever holds, caches, or rebroadcasts another peer's traffic.

## BLE Discovery & Identity

Every device advertises a public-key-derived Grassroots service UUID: a fixed Grassroots prefix plus a rotating per-slot suffix derived from the public key and the current 15-minute time slot. The UUID is only a discovery hint, never an authorization proof. Identity is established by the signed ANNOUNCE record — exactly the peer's full public key, the protocol version, and a trailing Ed25519 signature over the preceding bytes (spec §ANNOUNCE and Liveness; nothing else travels in ANNOUNCE: no nickname, no addresses). In Open cold-call mode, nearby unknown BLE peers may complete ANNOUNCE; in Closed mode, the advertised suffix is used only to recognize already-known peers — keys GLP supplied via the API — and unmatched encounters are ignored.

## Dual-Role BLE Is Mandatory

Every BLE pair must converge to a **dual-role connection**: two GATT legs, with each device central on one leg and peripheral on the other. Never ship a design that intentionally leaves a pair single-link. This requirement is inviolable.

Platform asymmetries are solved by choosing **who initiates each leg** — ordering, advertisement markers, pair reform — never by abandoning a leg. The one measured constraint (an iOS central cannot open the *second* link toward an Android it is already linked with; the connect wedges in `connecting` until timeout) is routed around by making iOS open the pair's *first* leg and the Android the reverse leg. iOS devices advertise the fixed `grs-ios` local name so peers can yield the first dial to them.

When a platform behavior is **unknown** (e.g. whether an iOS↔iOS reverse leg works), attempt it and let hardware decide — do not suppress it on extrapolation. A single-link pair is acceptable only as a *transient* state that the transport keeps trying to upgrade, or where hardware has *measurably* refused the second leg and the only remaining lever is initiator order.

## No Social Graph in the Layer

**The transport has no concept of friend — GLP holds the social graph** (spec §Trust Gating). The layer recognizes only keys GLP supplies through its API (`putKnownPeer`, `putPeerAddress`); it holds no friendship records, exchanges no friend or rendezvous lists, and never mediates between peers. Who learns an agent's address is a trust decision that lives with the social graph, so address distribution is GLP's: addresses travel as GLP-level messages over authenticated sessions and enter the layer via `putPeerAddress`.

## Rendezvous Servers & Hole-Punching

Most mobile devices sit behind NAT and cannot accept incoming UDP connections from the public Internet. **Rendezvous servers** (spec §Rendezvous Server; implemented in `bootstrap_anchor/`) give NAT-bound agents a fixed point they can always reach outbound. The reconnection flow:

1. Agent A (whose IP changed) sends RECONNECT(peer=B) to the configured rendezvous servers; each server observes A's source address.
2. Agent B, on detecting A went silent, sends AVAILABLE(peer=A) to the same servers.
3. A server matches the pair and sends each side a PUNCH_INITIATE carrying the other side's observed address; both sides punch their NATs and the deterministic initiator connects.

A rendezvous server relays *signaling metadata* (addresses, punch timing), never message content — it is not a TURN relay. It has no friends list and accepts connections from any agent. A direct peer-to-peer PUNCH_INITIATE over an existing control path (usually BLE) covers the BLE-adjacent case without any third party.

## Redux Architecture

All peer and transport state lives in an immutable Redux store (`AppState`). Key slices: `PeersState` (discovered BLE devices + identified peers), `TransportsState` (per-transport lifecycle + public address), `MessagesState`, `KnownPeersState` (API-supplied keys + persisted dial book), `SettingsState`. UI reads from the store and subscribes to changes. Actions describe events; reducers produce the next state. No mutable singletons.

The Redux state is a strict projection of facts emitted by the transport layers — never an inference. Reducers must not synthesize state from "I haven't heard from X in N seconds" heuristics; that's the transport layer's job to surface as an explicit event (path failed, session torn down, etc.).

## Transport Layer

Two transports are available, toggled independently in settings:

- **Bluetooth (BLE)** — local, no Internet required. Preferred when both are available.
- **Internet (UDP)** — global reach, requires Internet. Datagrams carrying the shared message transport (paper §Message Transport); hole-punching for NAT traversal.

The `TransportState` lifecycle for each transport is: `uninitialized → initializing → ready → active` (plus `error` and `disposed`). A transport is "usable" when it is `ready` or `active`.

User-facing UI strings should say "Internet", not "UDP" or internal protocol names.

## One Address Per Peer

Per **connection**, exactly one address pair is in use — there is no per-message address selection or mid-stream address switching. Each peer has a single dial-book address, supplied by GLP via `putPeerAddress` or observed on a live session; ANNOUNCE carries no addresses (spec §ANNOUNCE and Liveness — address distribution is not its role). The agent's own local candidates (per IP family) are still used to pick a compatible local endpoint when dialing.

The primary public address is discovered via an external service (e.g. seeip.org) and corrected by rendezvous-server address reflection (ADDR_REFLECT).

## Duo on Two iPhones — Operational Notes

The on-device test bed is the GrassApp duo (`glp_multiagent/lib/main_grassapp_duo.dart`), one role per phone; success = both status banners reach `linked (…)` and the play's cross-phone messages flow.

| Phone | Role | flutter device id | devicectl UUID |
|---|---|---|---|
| iPhone 16 Pro ("iUdi 16pro", iOS 26.5) | `phone1` (bob + eve) | `00008140-001C38C83A12801C` | `2219A3DC-78B5-5721-9E5E-FC2686D6A402` |
| iPhone 11 Pro Max ("iUdi 11pro", iOS 17.4.1) | `phone2` (alice + dana) | `00008030-000275600A12802E` | `C1F923A2-54D9-54DA-912D-8D033E7E425F` |

Bundle id `com.eshapiro.grassapp`; launch after install with `xcrun devicectl device process launch --device <UUID> com.eshapiro.grassapp`.

Build + install (from `glp_multiagent`; first `bash tool/sync_glp_assets.sh`): `flutter build ios --release -t lib/main_grassapp_duo.dart --dart-define=DUO_ROLE=phone1 --dart-define=DUO_ANCHOR_ADDR=<mac-lan-ip>:9516 --dart-define=DUO_ANCHOR_PK=b83e2276d995c0a9afd74c939bc9390b2d3a3606f9110e5a5c91b9b62601d350`, then `xcrun devicectl device install app --device <UUID> build/ios/iphoneos/Runner.app`; repeat with `DUO_ROLE=phone2` for the other phone.  Anchor (optional fallback): `dart run bootstrap_anchor` in `GLP/networking/bootstrap_anchor` with identity file `{"seed":"7a8e99d6182579f0c8d9b39cca74c1bfd49bcfd796d53d9e47c4b8c92db2812b","nickname":"rendezvous"}`; check the Mac's LAN IP with `ifconfig en0`.

Gotchas, learned the hard way:

- Build and install roles **serially** — concurrent `flutter build`/`flutter run` invocations share the build dir and clobber each other's dart-defines; install phone1's app before building phone2's.
- iOS shows a **Local Network permission dialog** on first mDNS use — must be allowed on both phones.
- Udi's WiFi has **client isolation**: outbound to the Mac works, phone↔phone and Mac→phone are blocked, likely mDNS multicast too. Banners stuck at `pairing…` for over a minute after Allow → suspect the network, not the code; use a non-isolated LAN or a hotspot from a third device.
- Kill stale simulator apps before any anchor-mediated test — dead simulators with the same seeded identities poison the anchor's address table.
- A `[lan] unmatched instance` line with a plausible token usually means clock skew beyond the ±1-slot match — check both phones' clocks.
- The 11 Pro's provisioning, developer trust, and Developer Mode are done and should not recur.

## Peer Address Persistence

Never unilaterally clear a peer's stored UDP address. Update it when a new valid address arrives (from `putPeerAddress`, signaling, or observation), and clear it only when the peer explicitly tells us they no longer have one. Stale peer cleanup, our-side disconnects, and transport restarts must not null out `udpAddress` — it is the last known location and the only way to attempt reconnection.

GLP feeds peer addresses into the layer's dial book via `putPeerAddress(pk, address)` (spec `GLP_Networking_API` §Connectivity and Address): it validates the `ip:port`, **creates a dial-book entry even for a peer never seen before**, and is supply-only — it never clears an address (an unparseable address throws). Supplying an address also supplies the key: the peer joins the known set (`KnownPeersState`, persisted), which is what Closed-mode recognition, per-slot scan filters, and the reconnection sweeps range over. A `putPeerAddress`-created peer is not `isReachable` until a session authenticates.

## Transport Independence

BLE and UDP are independent transports. Disabling or losing one must have **zero effect** on the other's connection state, peer reachability, or online status. A peer connected via UDP remains online regardless of BLE state. The stale peer logic, the UI, and the reducer must all respect this: never let a BLE disconnection degrade UDP-derived state.

Application-level callbacks (`onPeerConnected`, `onPeerDisconnected`) report reachability **per transport**, matching the spec (`GLP_Networking_API` §Connection and Reachability): each fires with the peer's public key and the `MessageTransport` whose authenticated session came up or went down. A peer reachable over both BLE and IP fires two connects; losing one of two live transports fires a disconnect for that medium while the peer stays reachable over the other. `peerTransports(pubkey)` returns the set of media a peer is currently reachable over (empty if unreachable), and `isPeerReachable(pubkey)` stays the any-medium predicate. Exposing the transport is what lets GLP compute proximity (nearby friends = friends ∩ peers reachable over BLE). The per-transport independence still holds end-to-end: a BLE disconnect never degrades UDP-derived state.
