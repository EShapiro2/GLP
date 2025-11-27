/// Delivery acknowledgment payload for the Grassroots BLE protocol.
library;

import 'dart:typed_data';

/// Payload for delivery acknowledgments (type 0x02) and read receipts (type 0x03).
///
/// Format:
/// - Message ID: 16 bytes (UUID of acknowledged message)
class DeliveryAckPayload {
  /// The message ID being acknowledged.
  final Uint8List messageId;

  /// Creates a delivery ack payload.
  DeliveryAckPayload({required this.messageId}) {
    if (messageId.length != 16) {
      throw ArgumentError('messageId must be 16 bytes');
    }
  }

  /// Encodes this payload to bytes.
  Uint8List encode() {
    return Uint8List.fromList(messageId);
  }

  /// Decodes a delivery ack payload from bytes.
  ///
  /// Throws [FormatException] if the data is invalid.
  static DeliveryAckPayload decode(Uint8List data) {
    if (data.length < 16) {
      throw FormatException(
        'Delivery ack payload too short: ${data.length} bytes (need 16)',
      );
    }
    return DeliveryAckPayload(
      messageId: Uint8List.fromList(data.sublist(0, 16)),
    );
  }

  /// Returns the message ID as a hex string.
  String get messageIdHex {
    return messageId.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
  }

  @override
  String toString() => 'DeliveryAckPayload(${messageIdHex.substring(0, 8)}...)';
}
