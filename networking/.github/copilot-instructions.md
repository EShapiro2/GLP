# Claude Instructions for Bitchat Transport

## CRITICAL: NO Legacy or Compatibility Code

**IMPORTANT**: When refactoring, DO NOT keep old code "for legacy" or "for compatibility".

- ❌ **NO** `// Legacy - kept for compatibility` comments
- ❌ **NO** keeping both old and new implementations
- ✅ **DO** fully replace old code with new implementation
- ✅ **DO** remove unused imports and dead code
- ✅ **DO** update all call sites when changing APIs
- ❌ **NO** `PeerStore` - use Redux store (`AppState.peers`) only

---

## CRITICAL: NO Store-and-Forward / NO Relaying

**IMPORTANT**: Bitchat does NOT implement store-and-forward or message relaying.

- ❌ **NO caching** messages for offline peers
- ❌ **NO relaying** messages through intermediate peers
- ❌ **NO forwarding** packets on behalf of other peers

Messages are sent **directly** to the recipient:
- If the recipient is **online and reachable** → message is delivered
- If the recipient is **offline** → message **fails** and sender must retry later

This is by design - the application layer (GSG) handles message persistence and retry logic.

---

## BLE Service UUID Architecture

**IMPORTANT**: Each Bitchat device MUST advertise its own **unique** service UUID derived from its public key.

### Why Unique UUIDs?

1. **Identity**: The service UUID is derived from the device's Ed25519 public key (last 128 bits)
2. **Security**: Provides cryptographic binding between BLE identity and cryptographic identity
3. **Discovery**: Devices scan broadly and discover ALL devices with service UUIDs
4. **Verification**: After connection, devices exchange ANNOUNCE packets containing full public keys and verify identity

### How It Works

1. **Advertising (Peripheral Mode)**:
   - Each device advertises with UUID = `last_128_bits(publicKey)`
   - Example: Device A with pubkey `0x1234...` advertises UUID `12345678-9abc-def0-1234-567890abcdef`

2. **Scanning (Central Mode)**:
   - Devices scan for ALL devices advertising ANY service UUID
   - Do NOT filter by specific UUID during scan
   - The scan is opportunistic - we discover any device that might be a peer

3. **Connection & Service Discovery**:
   - After BLE connection established, perform GATT service discovery
   - Check if device has the Bitchat GATT characteristic (UUID: `0000ff01-0000-1000-8000-00805f9b34fb`)
   - If characteristic NOT found → disconnect (not a Bitchat peer, e.g., headphones)
   - If characteristic found → it's a Bitchat peer, proceed to step 4
   
   **IMPORTANT**: We cannot know if a device is a Bitchat peer until AFTER connection and service discovery.
   This is why we connect to all devices and disconnect from non-peers after discovery.

4. **ANNOUNCE Exchange & Verification**:
   - After service discovery confirms Bitchat peer, exchange ANNOUNCE packets
   - ANNOUNCE contains: full public key, nickname, signature
   - Receiving device verifies the signature and stores the mapping: `BLE_Device_ID -> PublicKey`

5. **Identity Mapping**:
   - BLE layer knows devices by MAC address / device ID
   - Application layer (GSG) knows peers by Ed25519 public key
   - Bitchat maintains the mapping between these identities

### DO NOT:
- ❌ Use a single fixed service UUID for all devices
- ❌ Filter scans to only look for specific UUIDs during discovery
- ❌ Assume a device is a Bitchat peer before service discovery
- ❌ Assume UUID uniqueness means peer uniqueness (verify via ANNOUNCE)

### DO:
- ✅ Derive unique service UUID from each device's public key
- ✅ Scan broadly for all devices with service UUIDs
- ✅ Connect to devices, then perform service discovery to check for Bitchat characteristic
- ✅ Disconnect from non-Bitchat devices after service discovery
- ✅ Exchange ANNOUNCE packets after confirming Bitchat peer
- ✅ Maintain BLE Device ID ↔ Public Key mapping

## NO Store-and-Forward

**IMPORTANT**: Bitchat does NOT implement store-and-forward messaging.

- Messages to offline peers simply **fail** - they are NOT cached
- The sender must retry later when the peer is online
- This is by design to keep the protocol simple and avoid message accumulation
- If a peer is unreachable via any enabled transport (BLE or libp2p), `send()` returns `false`

### DO NOT:
- ❌ Cache messages for offline peers
- ❌ Implement store-and-forward queues
- ❌ Automatically retry sending to offline peers
- ❌ Hold messages in memory waiting for peers to reconnect

### DO:
- ✅ Return `false` immediately if peer is unreachable
- ✅ Let the application layer handle retry logic if needed
- ✅ Try all available transports (BLE first, then libp2p) before failing

## Redux Architecture for Peers

All peer state is managed through Redux:

### State Structure
- `AppState.peers` → `PeersState`
  - `discoveredBlePeers: Map<String, DiscoveredPeerState>` - Pre-ANNOUNCE BLE devices
  - `peers: Map<String, PeerState>` - Post-ANNOUNCE identified peers

### Key Actions
- `BleDeviceDiscoveredAction` - BLE scan found a device
- `BleDeviceConnectedAction` / `BleDeviceDisconnectedAction` - Connection state
- `PeerAnnounceReceivedAction` - ANNOUNCE packet received (peer identified)
- `PeerRssiUpdatedAction` - Signal strength updated
- `StaleDiscoveredBlePeersRemovedAction` / `StalePeersRemovedAction` - Cleanup

### UI Pattern
```dart
// Read peers from Redux store
Map<String, Peer> get _peers {
  return {
    for (var p in appStore.state.peers.connectedPeers) 
      p.pubkeyHex: _peerStateToLegacy(p)
  };
}

// Subscribe to store changes
appStore.onChange.listen((_) => setState(() {}));
```

## Transport Layer Settings

### Transport Priority
When both transports are enabled and a peer is reachable via both:
1. **Bluetooth (BLE)** is preferred - faster, no Internet needed, works for nearby peers
2. **libp2p (Internet)** is fallback - works globally but requires Internet

### Disabling Transports
- When Bluetooth is disabled: stop advertising, stop scanning, no BLE communication
- When libp2p is disabled: stop libp2p host, no Internet communication
- At least one transport should remain enabled for the app to function

### Per-Peer Addresses
Each peer can have both a BLE address and a libp2p address stored:
- `PeerState.bleDeviceId` - BLE device ID (MAC on Android, UUID on iOS)
- `PeerState.libp2pAddress` - libp2p multiaddress
- Messages route through the best available transport based on peer availability

## Code References

- Service UUID derivation: `lib/src/models/identity.dart` → `bleServiceUuid` getter
- Peripheral advertising: `lib/src/ble/ble_peripheral_service.dart` → `startAdvertising()`
- Central scanning: `lib/src/ble/ble_central_service.dart` → `startScan()` and `_onScanResults()`
- ANNOUNCE handling: `lib/src/transport/ble_transport_service.dart` → `_handleAnnounce()`
- Redux store: `lib/src/store/` → `peers_state.dart`, `peers_actions.dart`, `peers_reducer.dart`
