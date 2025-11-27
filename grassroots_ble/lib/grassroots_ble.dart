/// Resilient decentralized communication for Grassroots Social Networks using BLE.
///
/// This library implements a BLE-based peer-to-peer communication protocol
/// with customizable privacy levels, end-to-end encryption using the Noise
/// Protocol Framework, and support for friend discovery and message relay.
library grassroots_ble;

// Constants
export 'src/constants.dart';

// Models
export 'src/models/peer_id.dart';
export 'src/models/message_type.dart';
export 'src/models/privacy_level.dart';
export 'src/models/connection_state.dart';
export 'src/models/packet_flags.dart';
export 'src/models/packet.dart';
export 'src/models/friend.dart';
export 'src/models/message.dart';

// Payloads
export 'src/models/payloads/chat_payload.dart';
export 'src/models/payloads/delivery_ack_payload.dart';
export 'src/models/payloads/friend_request_payload.dart';
export 'src/models/payloads/friend_accept_payload.dart';
export 'src/models/payloads/introduction_payload.dart';
export 'src/models/payloads/friend_list_share_payload.dart';
export 'src/models/payloads/fragment_header.dart';
