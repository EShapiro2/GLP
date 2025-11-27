# BLE Protocol Implementation and Testing Plan

## Overview

This document outlines the implementation and testing plan for the Grassroots BLE communication protocol in Dart. The implementation follows the specification in `ble-paper.tex`.

## Implementation Phases

### Phase 1: Core Models and Constants (Week 1)

**Goal:** Define all data structures, enums, and constants.

**Files to create:**
```
lib/
├── src/
│   ├── models/
│   │   ├── peer_id.dart           # PeerID (8-byte truncated fingerprint)
│   │   ├── packet.dart            # BitchatPacket structure
│   │   ├── message_type.dart      # Enum for all 14 message types
│   │   ├── privacy_level.dart     # Enum for 4 privacy levels
│   │   ├── connection_state.dart  # Enum for state machine
│   │   ├── friend.dart            # Friend model
│   │   ├── message.dart           # Chat message model
│   │   └── payloads/
│   │       ├── chat_message.dart
│   │       ├── friend_request.dart
│   │       ├── friend_accept.dart
│   │       ├── introduction.dart
│   │       ├── friend_list_share.dart
│   │       └── fragment.dart
│   └── constants.dart             # All timeouts and limits
```

**Tasks:**
- [ ] Define `PeerID` class with hex encoding/decoding
- [ ] Define `MessageType` enum (0x01-0xF2)
- [ ] Define `PrivacyLevel` enum (1-4)
- [ ] Define `ConnectionState` enum
- [ ] Define `PacketFlags` (hasRecipient, hasSignature, etc.)
- [ ] Define all payload classes
- [ ] Define constants from spec (timeouts, limits)

**Tests:**
- Unit tests for PeerID serialization
- Unit tests for enum values match spec

---

### Phase 2: Cryptography (Week 2)

**Goal:** Implement key generation, Noise protocol, and signatures.

**Dependencies:** `pointycastle`, `cryptography` packages

**Files to create:**
```
lib/src/crypto/
├── key_pair.dart          # Curve25519 + Ed25519 key pairs
├── noise_protocol.dart    # Noise_XX_25519_ChaChaPoly_SHA256
├── noise_session.dart     # Session state (handshake + transport)
├── signature.dart         # Ed25519 signing/verification
└── fingerprint.dart       # SHA256 fingerprint derivation
```

**Tasks:**
- [ ] Generate Curve25519 key pair (Noise static key)
- [ ] Generate Ed25519 key pair (signing key)
- [ ] Derive fingerprint: `SHA256(NoiseStaticPublicKey)`
- [ ] Derive PeerID: first 8 bytes of fingerprint
- [ ] Derive Service UUID: last 16 bytes of public key
- [ ] Implement Noise XX handshake (3 messages)
- [ ] Implement Noise transport encryption/decryption
- [ ] Implement Ed25519 signing/verification

**Tests:**
- Key generation produces valid keys
- Fingerprint derivation matches test vectors
- Noise handshake completes between two parties
- Encrypted message round-trips correctly
- Signature verification works

---

### Phase 3: Binary Protocol (Week 3)

**Goal:** Implement packet encoding/decoding per spec.

**Files to create:**
```
lib/src/protocol/
├── binary_encoder.dart    # Packet → bytes
├── binary_decoder.dart    # Bytes → packet
├── padding.dart           # PKCS#7 padding to block sizes
├── compression.dart       # zlib compression (optional)
└── fragmentation.dart     # Fragment/reassemble large messages
```

**Tasks:**
- [ ] Implement 14-byte header encoding (big-endian)
- [ ] Implement variable field encoding (sender, recipient, payload, signature)
- [ ] Implement flags encoding/decoding
- [ ] Implement PKCS#7 padding to 256/512/1024/2048 bytes
- [ ] Implement payload encoding for each message type
- [ ] Implement fragmentation (split large messages)
- [ ] Implement reassembly (collect fragments)
- [ ] Implement zlib compression (when payload > 256 bytes)

**Tests:**
- Round-trip encoding/decoding for each message type
- Padding produces correct block sizes
- Fragmentation splits correctly at MTU boundary
- Reassembly handles out-of-order fragments
- Reassembly times out incomplete messages

---

### Phase 4: Bloom Filters (Week 3)

**Goal:** Implement Bloom filters for deduplication and friend list sharing.

**Files to create:**
```
lib/src/util/
├── bloom_filter.dart      # Generic Bloom filter
├── dedup_filter.dart      # Packet deduplication (8KB, k=7)
└── friend_filter.dart     # Friend list sharing (256B, k=5)
```

**Tasks:**
- [ ] Implement generic Bloom filter with configurable size and hash count
- [ ] Implement MurmurHash3 for double hashing
- [ ] Implement deduplication filter (8192 bits, k=7)
- [ ] Implement friend list filter (2048 bits, k=5)
- [ ] Implement filter rotation (clear every 5 minutes)

**Tests:**
- False positive rate within expected bounds
- No false negatives
- Filter rotation clears correctly
- Friend matching works across two filters

---

### Phase 5: Storage (Week 4)

**Goal:** Implement local database for social graph and messages.

**Dependencies:** `sqflite` or `drift` package

**Files to create:**
```
lib/src/storage/
├── database.dart          # Database initialization
├── friends_dao.dart       # Friends table CRUD
├── fof_dao.dart           # Friends-of-friends table CRUD
├── messages_dao.dart      # Messages table CRUD
└── keychain.dart          # Secure key storage
```

**Tasks:**
- [ ] Create database schema (3 tables)
- [ ] Implement Friends table CRUD
- [ ] Implement Friends-of-Friends table CRUD
- [ ] Implement Messages table CRUD (with status tracking)
- [ ] Implement secure keychain storage for private keys
- [ ] Implement message expiry cleanup (24 hours)
- [ ] Implement pending message queue (max 100)

**Tests:**
- CRUD operations work correctly
- Message status transitions work
- Expired messages are cleaned up
- Queue size limit enforced

---

### Phase 6: BLE Transport (Week 5-6)

**Goal:** Implement BLE advertising, scanning, and GATT operations.

**Dependencies:** `flutter_blue_plus` or `flutter_reactive_ble` package

**Files to create:**
```
lib/src/ble/
├── ble_service.dart       # Main BLE service coordinator
├── advertiser.dart        # BLE advertising (privacy-level aware)
├── scanner.dart           # BLE scanning (friend UUIDs or all)
├── gatt_server.dart       # GATT peripheral (expose characteristic)
├── gatt_client.dart       # GATT central (connect and write)
├── connection_manager.dart # Track active connections
└── duty_cycle.dart        # Advertise/scan alternation
```

**Tasks:**
- [ ] Derive Service UUID from public key
- [ ] Implement advertising with Service UUID (Levels 2-4)
- [ ] Implement advertising with name (Levels 3-4)
- [ ] Implement scanning for specific UUIDs (friend discovery)
- [ ] Implement scanning for all UUIDs (Level 3+ stranger discovery)
- [ ] Implement GATT server with single characteristic
- [ ] Implement GATT client (connect, discover, write, notify)
- [ ] Implement duty cycle (2s advertise, 5s scan)
- [ ] Implement connection timeout handling
- [ ] Implement RSSI threshold check for proximity friending

**Tests:**
- UUID derivation matches spec
- Advertisement data fits in 31 bytes
- GATT connection establishes
- Data exchange over characteristic works
- Duty cycle alternates correctly

---

### Phase 7: Session Management (Week 7)

**Goal:** Implement connection state machine and Noise session lifecycle.

**Files to create:**
```
lib/src/session/
├── session_manager.dart   # Manage all active sessions
├── session.dart           # Single peer session
├── handshake_handler.dart # Noise XX handshake state machine
└── transport_handler.dart # Post-handshake encrypted messaging
```

**Tasks:**
- [ ] Implement connection state machine (Idle → Connecting → Handshaking → Established → Disconnected)
- [ ] Implement Noise handshake as initiator
- [ ] Implement Noise handshake as responder
- [ ] Implement handshake timeout (10 seconds)
- [ ] Implement idle timeout (60 seconds)
- [ ] Implement session caching (reuse sessions with same peer)
- [ ] Implement retry logic with exponential backoff

**Tests:**
- State transitions are correct
- Handshake completes within timeout
- Handshake timeout triggers disconnect
- Session reuse works
- Retry backoff timing is correct

---

### Phase 8: Routing and Relay (Week 8)

**Goal:** Implement message routing, relay policy, and delivery tracking.

**Files to create:**
```
lib/src/routing/
├── router.dart            # Main routing logic
├── relay_policy.dart      # Privacy-level based relay decisions
├── delivery_tracker.dart  # Track pending deliveries and acks
├── store_forward.dart     # Queue messages for offline recipients
└── flood_controller.dart  # TTL and dedup management
```

**Tasks:**
- [ ] Implement TTL decrement and drop at 0
- [ ] Implement Bloom filter deduplication
- [ ] Implement relay policy (friend/FoF checks)
- [ ] Implement delivery acknowledgment sending
- [ ] Implement delivery acknowledgment receiving
- [ ] Implement retry with exponential backoff (2s, 4s, 8s, 16s)
- [ ] Implement store-and-forward queue
- [ ] Implement message expiry (24 hours)

**Tests:**
- TTL decrements correctly
- Duplicate packets are dropped
- Relay policy respects privacy levels
- Acks are sent and received
- Retries happen at correct intervals
- Expired messages are dropped

---

### Phase 9: Application Layer (Week 9)

**Goal:** Implement friendship and messaging features.

**Files to create:**
```
lib/src/app/
├── friend_manager.dart    # Friendship operations
├── message_manager.dart   # Send/receive messages
├── introduction_manager.dart # Handle introductions
└── notification_handler.dart # Callbacks for UI
```

**Tasks:**
- [ ] Implement friend request sending (with signature)
- [ ] Implement friend request receiving and validation
- [ ] Implement friend accept/reject
- [ ] Implement introduction request to mutual friend
- [ ] Implement introduction forwarding
- [ ] Implement friend list Bloom filter generation
- [ ] Implement friend list Bloom filter matching
- [ ] Implement chat message sending
- [ ] Implement chat message receiving
- [ ] Implement read receipts

**Tests:**
- Friend request signature validates
- Invalid signatures are rejected
- Friend list updates on accept
- Introduction flow completes
- Messages are delivered and acked

---

### Phase 10: Integration (Week 10)

**Goal:** Wire everything together and create public API.

**Files to create:**
```
lib/
├── grassroots_ble.dart    # Public API
└── src/
    └── grassroots_service.dart # Main service coordinator
```

**Tasks:**
- [ ] Create `GrassrootsService` class that coordinates all components
- [ ] Implement initialization (key generation, DB setup)
- [ ] Implement privacy level configuration
- [ ] Implement start/stop for BLE operations
- [ ] Implement public API for sending messages
- [ ] Implement public API for friend operations
- [ ] Implement event streams for UI callbacks
- [ ] Handle background/foreground transitions

**Tests:**
- Full initialization completes
- Privacy level changes affect behavior
- End-to-end message delivery works

---

## Testing Plan

### Unit Tests

Each module has unit tests covering:
- Normal operation (happy path)
- Edge cases (empty inputs, max sizes)
- Error handling (invalid data, timeouts)

**Test file structure:**
```
test/
├── models/
│   ├── peer_id_test.dart
│   ├── packet_test.dart
│   └── ...
├── crypto/
│   ├── key_pair_test.dart
│   ├── noise_protocol_test.dart
│   └── ...
├── protocol/
│   ├── binary_encoder_test.dart
│   ├── fragmentation_test.dart
│   └── ...
└── ...
```

### Integration Tests

**Two-device simulation:**
- Use mock BLE transport for local testing
- Test handshake between two `GrassrootsService` instances
- Test message exchange
- Test friend request flow
- Test relay through intermediate node

**Test scenarios:**
1. Direct communication (two friends in range)
2. Relay communication (message hops through third party)
3. Store-and-forward (recipient comes online later)
4. Stranger friending (Level 3+ proximity handshake)
5. Introduction (Level 4 mutual friend introduction)

### End-to-End Tests

**Real device testing:**
- Test on Android and iOS devices
- Test BLE advertising/scanning
- Test GATT connection establishment
- Test data exchange over GATT
- Test background operation

### Performance Tests

**Benchmarks:**
- Packet encoding/decoding throughput
- Noise handshake latency
- Message delivery latency (direct)
- Message delivery latency (1-hop relay)
- Bloom filter lookup time
- Database query time

**Stress tests:**
- Many concurrent connections
- Large message fragmentation
- High message rate
- Large friend list

---

## Dependencies

```yaml
dependencies:
  # Cryptography
  cryptography: ^2.5.0
  pointycastle: ^3.7.0

  # BLE
  flutter_blue_plus: ^1.31.0  # or flutter_reactive_ble

  # Storage
  sqflite: ^2.3.0
  path_provider: ^2.1.0
  flutter_secure_storage: ^9.0.0

  # Utilities
  uuid: ^4.2.0
  collection: ^1.18.0

dev_dependencies:
  test: ^1.24.0
  mockito: ^5.4.0
  fake_async: ^1.3.0
```

---

## Milestones

| Milestone | Week | Deliverables |
|-----------|------|--------------|
| M1: Foundation | 2 | Models, constants, crypto working |
| M2: Protocol | 4 | Binary encoding, Bloom filters, storage |
| M3: Transport | 6 | BLE advertising, scanning, GATT |
| M4: Sessions | 8 | Handshakes, routing, relay |
| M5: Complete | 10 | Full integration, public API |

---

## Risk Mitigation

| Risk | Mitigation |
|------|------------|
| BLE platform differences (iOS/Android) | Use well-maintained BLE package; test early on both platforms |
| Noise protocol complexity | Use existing Dart Noise library if available; fallback to manual implementation |
| Background BLE limitations | Document platform-specific limitations; test background mode early |
| Performance on low-end devices | Benchmark early; optimize hot paths |

---

## Next Steps

1. Set up Dart package structure
2. Implement Phase 1 (models and constants)
3. Write unit tests for Phase 1
4. Proceed to Phase 2 (cryptography)
