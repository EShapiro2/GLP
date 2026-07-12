# Bitchat Anchor Server — Session Summary

## Goal

Build a **personal cloud anchor** for the Bitchat P2P transport. The anchor is an always-on, globally-routable peer that helps a specific user's friends find each other via signaling (ADDR_QUERY, hole-punch coordination, address reflection). It never relays message content — only metadata.

## Key design decisions

1. **Owner-centric, not public.** Each anchor belongs to one user. It only serves that user and their explicit friends. Strangers that connect get logged and ignored.

2. **Friend list pushed from device, not configured on server.** The owner's mobile device is the source of truth. On first connect to the anchor (and whenever friendships change), it sends a `FRIENDS_SYNC` signaling message containing the full friend list. The server replaces its list and persists to `friends.json` for restart recovery only.

3. **Anchor identity is a deterministic subkey of the owner's key.**
   ```
   anchorSeed = SHA-256(ownerSeed || "bitchat-anchor")
   ```
   The owner's device always knows the anchor's pubkey — no manual configuration. The 32-byte seed is exported once and deployed to the server. Compromising the anchor seed does not reveal the owner's key (SHA-256 is one-way).

4. **Settings screen only needs the anchor address.** The pubkey is derived automatically.

## Wire protocol addition

New signaling type: `friendsSync(0x08)`

```
FRIENDS_SYNC: type(1) + count(2, big-endian) + [pubkey(32) + nickLen(1) + nick]*
```

Only the owner can send it; the server rejects it from anyone else.

## Files changed

### Client (`lib/`)

- **`src/models/identity.dart`** — Added `deriveAnchorKeyPair()`, `anchorPubkeyHex`, `anchorPublicKey`, `anchorSeed` to `BitchatIdentity`. Uses `Sha256` from the `cryptography` package.
- **`src/signaling/signaling_codec.dart`** — Added `SignalingType.friendsSync`, `FriendsSyncMessage`, `FriendsSyncEntry`, encode/decode.
- **`src/signaling/signaling_service.dart`** — Added `sendFriendsSync(anchorPubkey)` which reads `store.state.friendships.friends`, builds the message, sends via `sendSignaling` callback.
- **`src/bitchat.dart`** — Caches `_anchorPubkey` / `_anchorPubkeyHex` in `initialize()`. Tracks `_lastFriendshipsState` in the store listener; on change calls `_onFriendshipsChanged()` → `sendFriendsSync`. On UDX connect, if the connected peer's pubkey matches the derived anchor pubkey, calls `_syncFriendsToAnchor()`.
- **`src/store/settings_state.dart`** — Added `anchorAddress` field, removed `anchorPubkeyHex` (derived, not stored). Sentinel-based `copyWith` so `null` clears.
- **`src/store/settings_actions.dart`** — `SetAnchorServerAction` takes only `anchorAddress`.
- **`src/store/settings_reducer.dart`** — Handles `SetAnchorServerAction`.
- **`settings_screen.dart`** — New "Anchor Server" section with a single address text field, Save/Remove buttons, "Configured" badge, clear confirmation dialog.

### Server (`bootstrap_anchor/`)

- **`lib/src/identity.dart`** — Rewrote `AnchorIdentity`. Dropped `generate()` / `loadOrCreate()`. Now takes a seed via `fromSeedHex()` and reconstructs the Ed25519 keypair deterministically. No identity.json on disk.
- **`lib/src/peer_table.dart`** — Owner-centric `PeerTable` with `ownerPubkeyHex`, `_friends`, `_strangers`. `isFriend()` checks owner OR friend list. `loadFriendList` / `saveFriendList` for restart recovery. Public `FriendSpec` class.
- **`lib/src/signaling_codec.dart`** — Wire-identical to the client: all 7 signaling types including `friendsSync`.
- **`lib/src/signaling_handler.dart`** — `_handleFriendsSync` only accepts from owner, replaces full friend list, fires `onFriendsSynced` callback. All ADDR_QUERY/PUNCH_REQUEST handlers check `isFriend()`.
- **`lib/src/anchor_server.dart`** — Constructor takes `seedHex` instead of `identityPath`. On `start()`: derives identity from seed, adds owner as friend, loads persisted friends, wires `onFriendsSynced` → `saveFriendList(friendsPath)`.
- **`bin/server.dart`** — CLI: `--seed` (mandatory, 64-hex) + `--owner` (mandatory, 64-hex) + `--port` + `--nickname` + `--friends` + `--announce-interval`. Removed `--identity`.
- **`Dockerfile` + `Dockerfile.standalone`** — CMD updated to use `--seed ANCHOR_SEED_HEX` placeholder.
- **`deploy.sh`** — Takes `<ANCHOR_SEED_HEX> <OWNER_PUBKEY_HEX>` as first two args. Validates both are exactly 64 hex chars. Passes both via `--container-arg` to `gcloud compute instances create-with-container`.
- **`build.sh`** — Updated run hint to show `--seed` + `--owner` required.

## Deploy flow

1. Owner calls `identity.anchorSeed` on their device → 32-byte hex
2. `./deploy.sh <ANCHOR_SEED_HEX> <OWNER_PUBKEY_HEX> [PROJECT] [REGION] [ZONE]`
3. Script builds Docker image, pushes to Artifact Registry, creates e2-micro VM with IPv6, opens UDP port 9514, reports `[IPv6]:9514` address
4. On the phone: Settings → Anchor Server → paste address → Save
5. On first UDX connect, device pushes FRIENDS_SYNC. Server persists to `friends.json`. Thereafter any friendship change triggers re-sync.

## Architectural invariants preserved

- **No message relaying.** Anchor only handles signaling metadata.
- **Direct delivery only.** No caching, no store-and-forward.
- **Friend-only signaling.** Strangers get zero service.
- **Transport independence.** BLE and UDP remain independent; anchor only participates on UDP.
- **Peer address persistence.** Never unilaterally clears `udpAddress`.

## Outstanding / next steps

- Can't run `dart pub get` / `dart analyze` in the VM (no SDK), so all verification is code-level. Needs a build on a machine with Dart SDK.
- 10+ commits unpushed on the bitchat_transport repo (pinned from prior sessions).
- Other pinned items: BLE peripheral can't send to connected centrals; auto-udp retry too aggressive; BLE disconnect fix; stale friendship cleanup.
