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

// Places: the layer's declarations and their crossings (spec §System
// Predicates), and the platform geofencing behind them.
export 'package:grassroots_networking_core/src/places/place_registry.dart';
export 'src/places/platform_place_geofence_backend.dart';

// Models
export 'package:grassroots_networking_core/src/models/identity.dart';
export 'src/identity_store.dart';
export 'package:grassroots_networking_core/src/models/peer.dart';
export 'package:grassroots_networking_core/src/models/packet.dart';
export 'package:grassroots_networking_core/src/models/block.dart';

// Redux Store (core states/actions/reducers; persistence is Flutter-side)
export 'package:grassroots_networking_core/src/store/store.dart';
export 'src/store/persistence_service.dart';

// UI adornments for core types (icons)
export 'src/transport/transport_display.dart';

// BLE (for advanced usage)
export 'src/ble/permission_handler.dart' show PermissionHandler, PermissionResult;

// Media (used by the chat UI for picture compression / file storage)
export 'src/services/media_service.dart';
