/// GNA's networking layer beneath madGLP, as a Flutter package.
///
/// One overlay over three underlays: the PAN over BLE (dual-role), the LAN
/// discovered by DNS-SD, and the WAN over UDP through rendezvous servers.
/// It handles:
/// - Noise XX sessions between Ed25519 keys, and attestation, on every underlay
/// - Packet fragmentation and reassembly
/// - Local queueing of our own outbound messages for offline peers
/// 
/// ## Usage
/// 
/// ```dart
/// import 'package:grassroots_networking/grassroots_networking.dart';
/// 
/// // The agent's key pair, generated once and persisted above the layer (spec §Identity)
/// final identity = await GrassrootsIdentity.generate();
/// 
/// // Create GrassrootsNetwork instance
/// final grassroots = GrassrootsNetwork(
///   identity: identity,
///   underlays: {Underlay.pan, Underlay.lan, Underlay.wan},
///   rendezvousServers: [RendezvousServer('203.0.113.7:9514', serverKey)],
///   store: store,
///   sodium: sodium,
/// );
/// 
/// // Set up callbacks
/// grassroots.onMessageReceived = (sender, payload, messageId, underlay) {
///   // Handle incoming GSG block
/// };
/// 
/// grassroots.onPeerConnected = (pk, underlay, identity) {
///   // peer reachable over `underlay` (PAN, LAN or WAN)
/// };
/// 
/// // Initialize and start
/// await grassroots.initialize();
/// await grassroots.start();
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
/// - `PeerState` - A peer's state in the Redux store
/// - `MessageRouter` - Routes incoming packets from every carrier to their handlers
/// - `TransportService` - Abstract interface of the carriers: `BleTransportService` for the PAN, `UdpTransportService` for the LAN and the WAN
library grassroots_networking;

// Main API
export 'src/grassroots_network.dart';

// Transport abstraction
export 'src/transport/transport.dart';

// Places: the layer's declarations and their crossings (spec §System
// Predicates), and the platform geofencing behind them.
export 'package:grassroots_networking_core/src/places/place_registry.dart';
export 'src/places/platform_place_geofence_backend.dart';

// Attestation: the seam, the attested application identity, the core's
// verifier, and the binding to the platform's native producer (spec §Session
// Establishment).
export 'package:grassroots_networking_core/src/session/platform_attestation.dart';
export 'package:grassroots_networking_core/src/session/application_identity.dart';
export 'package:grassroots_networking_core/src/session/attestation_verifier.dart';
export 'src/attestation/native_platform_attestation.dart';

// Models
export 'package:grassroots_networking_core/src/models/identity.dart';
export 'src/identity_store.dart';
export 'package:grassroots_networking_core/src/models/peer.dart';
export 'package:grassroots_networking_core/src/models/packet.dart';
export 'package:grassroots_networking_core/src/models/block.dart';
export 'package:grassroots_networking_core/src/models/underlay.dart';
export 'package:grassroots_networking_core/src/models/rendezvous_server.dart';

// Redux Store (core states/actions/reducers; persistence is Flutter-side)
export 'package:grassroots_networking_core/src/store/store.dart';
export 'src/store/persistence_service.dart';

// The fair-delivery queue's persistence (spec §Networking Assumptions): the
// seam and its stores, the core's and SharedPreferences'.
export 'package:grassroots_networking_core/src/protocol/outbound_queue_store.dart';
export 'src/store/outbound_queue_preferences.dart';

// UI adornments for core types (icons)
export 'src/transport/transport_display.dart';

// BLE (for advanced usage)
export 'src/ble/permission_handler.dart' show PermissionHandler, PermissionResult;

// Media (used by the chat UI for picture compression / file storage)
export 'src/services/media_service.dart';
