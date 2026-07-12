/// Grassroots Networking Layer for GSG Protocol
/// 
/// This package provides a BLE mesh transport layer based on the Grassroots protocol.
/// It handles:
/// - BLE Central/Peripheral dual-mode operation
/// - Packet fragmentation and reassembly
/// - Duplicate detection via Bloom filter
/// - Local queueing of our own outbound messages for offline peers
/// 
/// ## Usage
/// 
/// ```dart
/// import 'package:grassroots_networking/grassroots_networking.dart';
/// 
/// // Create identity (provided by GSG layer)
/// final identity = GrassrootsIdentity(
///   publicKey: myPubKey,
///   privateKey: myPrivKey,
///   nickname: 'Alice',
/// );
/// 
/// // Create GrassrootsNetwork instance
/// final grassroots = GrassrootsNetwork(identity: identity);
/// 
/// // Set up callbacks
/// grassroots.onMessageReceived = (senderPubkey, payload) {
///   // Handle incoming GSG block
/// };
/// 
/// grassroots.onPeerConnected = (publicKey, transport) {
///   // peer reachable over `transport` (BLE or UDP)
/// };
/// 
/// // Initialize and start
/// await grassroots.initialize();
/// 
/// // Send messages
/// await grassroots.send(recipientPubkey, gsgBlockData);
/// await grassroots.broadcast(gsgBlockData);
/// ```
/// 
/// ## Architecture
/// 
/// The package is structured as follows:
/// 
/// - `GrassrootsNetwork` - Main API class for GSG integration
/// - `GrassrootsIdentity` - Ed25519 identity provided by GSG
/// - `Peer` - Represents a connected peer
/// - `MeshRouter` - Handles routing, relay, and fragmentation
/// - `BleManager` - Manages BLE Central and Peripheral roles
/// - `TransportService` - Abstract interface for transport implementations
/// 
/// ## Protocol Compatibility
/// 
/// This implementation follows the Grassroots protocol specification for
/// BLE mesh networking, ensuring compatibility with other Grassroots clients.
/// 
/// ## Transport Abstraction
/// 
/// The transport layer is abstracted via the `TransportService` interface,
/// allowing multiple transport implementations:
/// - `BleTransportService` - Bluetooth Low Energy mesh (default)
/// - Future: WebRTC transport (STUN/TURN/TURNS)
library grassroots_networking;

// Main API
export 'src/grassroots_network.dart';

// Transport abstraction
export 'src/transport/transport.dart';

// Models
export 'src/models/identity.dart';
export 'src/identity_store.dart';
export 'src/models/peer.dart';
export 'src/models/packet.dart';
export 'src/models/block.dart';

// Redux Store (includes friendships_state, settings_state, persistence_service)
export 'src/store/store.dart';

// BLE (for advanced usage)
export 'src/ble/permission_handler.dart' show PermissionHandler, PermissionResult;

// Media (used by the chat UI for picture compression / file storage)
export 'src/services/media_service.dart';
