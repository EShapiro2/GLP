# Bitchat Transport for GSG

A Flutter/Dart implementation of the Bitchat BLE mesh transport layer, designed for use with the Grassroots Social Graph (GSG) protocol.

## Overview

This package provides a BLE mesh networking layer based on the [Bitchat protocol](https://github.com/permissionlesstech/bitchat). It enables peer-to-peer communication between nearby devices without requiring internet connectivity.

### Features

- **Dual-mode BLE**: Operates as both Central (scanner) and Peripheral (advertiser) simultaneously
- **Mesh Routing**: Multi-hop message relay (up to 7 hops)
- **Fragmentation**: Automatic chunking and reassembly of large messages
- **Deduplication**: Bloom filter prevents duplicate packet processing
- **Store-and-Forward**: Caches messages for offline peers (12hr retention)
- **Protocol Compatible**: Works with other Bitchat implementations

### Architecture

```
┌─────────────────────────────────────────────────────────┐
│                      GSG Layer                          │
│   (Blocklace, Friendship Protocol, Cordial Dissem.)    │
├─────────────────────────────────────────────────────────┤
│                   Bitchat API                           │
│         (send, broadcast, onMessageReceived)            │
├─────────────────────────────────────────────────────────┤
│                   Mesh Router                           │
│   (Routing, Relay, Fragmentation, Store-and-Forward)   │
├─────────────────────────────────────────────────────────┤
│                   BLE Manager                           │
│           (Central + Peripheral Services)               │
├─────────────────────────────────────────────────────────┤
│              BLE Hardware (iOS/Android)                 │
└─────────────────────────────────────────────────────────┘
```

## Installation

Add to your `pubspec.yaml`:

```yaml
dependencies:
  bitchat_transport:
    path: ../bitchat_transport  # Or publish to pub.dev
```

### Platform Setup

#### Android

Add to `android/app/src/main/AndroidManifest.xml`:

```xml
<!-- Bluetooth permissions (Android 12+) -->
<uses-permission android:name="android.permission.BLUETOOTH_SCAN" />
<uses-permission android:name="android.permission.BLUETOOTH_CONNECT" />
<uses-permission android:name="android.permission.BLUETOOTH_ADVERTISE" />

<!-- Location (required for BLE scanning) -->
<uses-permission android:name="android.permission.ACCESS_FINE_LOCATION" />

<!-- Required features -->
<uses-feature android:name="android.hardware.bluetooth_le" android:required="true" />
```

#### iOS

Add to `ios/Runner/Info.plist`:

```xml
<key>NSBluetoothAlwaysUsageDescription</key>
<string>This app uses Bluetooth to communicate with nearby devices.</string>
<key>NSBluetoothPeripheralUsageDescription</key>
<string>This app uses Bluetooth to communicate with nearby devices.</string>
<key>NSLocationWhenInUseUsageDescription</key>
<string>Location is used to improve Bluetooth connectivity.</string>
<key>UIBackgroundModes</key>
<array>
    <string>bluetooth-central</string>
    <string>bluetooth-peripheral</string>
</array>
```

## Usage

### Basic Setup

```dart
import 'package:bitchat_transport/bitchat_transport.dart';

// Identity is provided by GSG layer
final identity = BitchatIdentity(
  publicKey: myEd25519PublicKey,   // 32 bytes
  privateKey: myEd25519PrivateKey, // 64 bytes
  nickname: 'Alice',
);

// Create Bitchat instance
final bitchat = Bitchat(identity: identity);

// Set up callbacks
bitchat.onMessageReceived = (senderPubkey, payload) {
  print('Received ${payload.length} bytes from ${senderPubkey}');
  // Handle GSG block
};

bitchat.onPeerConnected = (peer) {
  print('Peer connected: ${peer.displayName}');
  // Start cordial dissemination
};

bitchat.onPeerDisconnected = (peer) {
  print('Peer disconnected: ${peer.displayName}');
};

// Initialize (requests permissions, starts BLE)
final success = await bitchat.initialize();
if (!success) {
  print('Failed to initialize: ${bitchat.status}');
  return;
}

// Transport is now active and will auto-discover peers
```

### Sending Messages

```dart
// Send to specific peer
final sent = await bitchat.send(recipientPubkey, gsgBlockData);

// Broadcast to all peers
await bitchat.broadcast(gsgBlockData);
```

### Peer Management

```dart
// Get all known peers
final peers = bitchat.peers;

// Get connected peers only
final connected = bitchat.connectedPeers;

// Check if a peer is reachable
final reachable = bitchat.isPeerReachable(pubkey);

// Get specific peer
final peer = bitchat.getPeer(pubkey);
```

### Lifecycle

```dart
// Stop BLE activity (saves battery)
await bitchat.stop();

// Resume
await bitchat.start();

// Trigger new scan
await bitchat.scan(timeout: Duration(seconds: 30));

// Clean up
await bitchat.dispose();
```

## Protocol Details

### Packet Format

```
[0]      : Packet type (1 byte)
[1]      : TTL (1 byte, max 7)
[2-5]    : Timestamp (4 bytes)
[6-37]   : Sender public key (32 bytes)
[38-69]  : Recipient public key (32 bytes, zeros for broadcast)
[70-71]  : Payload length (2 bytes)
[72-87]  : Packet ID (16 bytes, UUID)
[88-151] : Signature (64 bytes)
[152-N]  : Payload (variable)
```

### Packet Types

| Type | Value | Description |
|------|-------|-------------|
| ANNOUNCE | 0x01 | Peer identity exchange |
| MESSAGE | 0x02 | Application data (GSG blocks) |
| FRAGMENT_START | 0x03 | Start of fragmented message |
| FRAGMENT_CONTINUE | 0x04 | Continuation fragment |
| FRAGMENT_END | 0x05 | Final fragment |
| ACK | 0x06 | Delivery acknowledgment |
| NACK | 0x07 | Request for data (GSG-level) |

### Fragmentation

Messages > 500 bytes are automatically fragmented:

1. `FRAGMENT_START`: Contains metadata (total size, count) + first chunk
2. `FRAGMENT_CONTINUE`: Intermediate chunks
3. `FRAGMENT_END`: Final chunk, triggers reassembly

Inter-fragment delay: 20ms

### Mesh Routing

- TTL starts at 7, decremented at each hop
- Bloom filter prevents duplicate processing
- Packets with TTL=0 are not relayed
- ANNOUNCE packets are not relayed (direct only)

## Future Transport Layers

The Nostr bridge from original Bitchat has been removed. See `TODO_TRANSPORT.md` for plans to implement:

- WebRTC for internet-based P2P
- STUN/TURN for NAT traversal
- Other transport options

## Dependencies

- `flutter_blue_plus` - BLE Central mode
- `ble_peripheral` - BLE Peripheral mode
- `permission_handler` - Runtime permissions
- `cryptography` - Ed25519 signing
- `uuid` - Packet ID generation
- `logger` - Logging

## License

MIT License - see LICENSE file
