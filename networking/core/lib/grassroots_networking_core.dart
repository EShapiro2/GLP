/// Pure-Dart core of the Grassroots networking layer.
///
/// Everything protocol-bearing lives here — identity, wire format, Noise
/// sessions, message routing, Redux state, and the UDP/UDX transport — with
/// no Flutter dependency, so the same internals serve the Flutter package
/// (which re-exports this library) and headless server embeddings
/// (`dart compile exe`; see `HeadlessGrassrootsNetwork` and `bin/`).
library;

export 'src/models/identity.dart';
export 'src/models/packet.dart';
export 'src/models/peer.dart';
export 'src/models/block.dart';
export 'src/protocol/protocol_handler.dart';
export 'src/protocol/fragment_handler.dart';
export 'src/protocol/message_transport.dart';
export 'src/session/noise_session_manager.dart';
export 'src/routing/message_router.dart';
export 'src/store/store.dart';
export 'src/transport/transport_service.dart';
export 'src/transport/udp_transport_service.dart';
export 'src/transport/address_utils.dart';
export 'src/transport/local_network.dart';
export 'src/places/place_registry.dart';
export 'src/platform/compat.dart';
export 'src/headless/headless_grassroots_network.dart';
export 'src/headless/known_peers_persistence.dart';
export 'src/headless/libsodium_loader.dart';
