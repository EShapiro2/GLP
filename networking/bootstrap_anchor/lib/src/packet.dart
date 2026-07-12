import 'dart:typed_data';

/// Packet types matching Grassroots protocol.
///
/// Must be identical to the client-side PacketType enum values.
enum PacketType {
  announce(0x01),
  message(0x02),
  fragmentStart(0x03),
  fragmentContinue(0x04),
  fragmentEnd(0x05),
  ack(0x06),
  nack(0x07),
  readReceipt(0x08),
  signaling(0x09),
  noiseHandshake(0x0A),
  secureMessage(0x0B),
  secureFragmentStart(0x0C),
  secureFragmentContinue(0x0D),
  secureFragmentEnd(0x0E),
  secureAck(0x0F),
  secureNack(0x10),
  secureReadReceipt(0x11),
  secureSignaling(0x12);

  final int value;
  const PacketType(this.value);

  static PacketType fromValue(int value) {
    return PacketType.values.firstWhere(
      (t) => t.value == value,
      orElse: () => throw ArgumentError('Unknown packet type: $value'),
    );
  }

  /// Whether this packet type carries application data that must be wrapped in
  /// a Noise transport session before sending. Mirrors the client-side flag in
  /// lib/src/models/packet.dart.
  bool get usesSessionSecurity {
    switch (this) {
      case PacketType.message:
      case PacketType.fragmentStart:
      case PacketType.fragmentContinue:
      case PacketType.fragmentEnd:
      case PacketType.ack:
      case PacketType.nack:
      case PacketType.readReceipt:
      case PacketType.signaling:
        return true;
      case PacketType.announce:
      case PacketType.noiseHandshake:
      case PacketType.secureMessage:
      case PacketType.secureFragmentStart:
      case PacketType.secureFragmentContinue:
      case PacketType.secureFragmentEnd:
      case PacketType.secureAck:
      case PacketType.secureNack:
      case PacketType.secureReadReceipt:
      case PacketType.secureSignaling:
        return false;
    }
  }

  /// Whether this packet type is an encrypted Noise transport variant.
  bool get isSessionEncrypted {
    switch (this) {
      case PacketType.secureMessage:
      case PacketType.secureFragmentStart:
      case PacketType.secureFragmentContinue:
      case PacketType.secureFragmentEnd:
      case PacketType.secureAck:
      case PacketType.secureNack:
      case PacketType.secureReadReceipt:
      case PacketType.secureSignaling:
        return true;
      case PacketType.announce:
      case PacketType.message:
      case PacketType.fragmentStart:
      case PacketType.fragmentContinue:
      case PacketType.fragmentEnd:
      case PacketType.ack:
      case PacketType.nack:
      case PacketType.readReceipt:
      case PacketType.signaling:
      case PacketType.noiseHandshake:
        return false;
    }
  }

  PacketType get secureVariant {
    switch (this) {
      case PacketType.message:
        return PacketType.secureMessage;
      case PacketType.fragmentStart:
        return PacketType.secureFragmentStart;
      case PacketType.fragmentContinue:
        return PacketType.secureFragmentContinue;
      case PacketType.fragmentEnd:
        return PacketType.secureFragmentEnd;
      case PacketType.ack:
        return PacketType.secureAck;
      case PacketType.nack:
        return PacketType.secureNack;
      case PacketType.readReceipt:
        return PacketType.secureReadReceipt;
      case PacketType.signaling:
        return PacketType.secureSignaling;
      case PacketType.announce:
      case PacketType.noiseHandshake:
      case PacketType.secureMessage:
      case PacketType.secureFragmentStart:
      case PacketType.secureFragmentContinue:
      case PacketType.secureFragmentEnd:
      case PacketType.secureAck:
      case PacketType.secureNack:
      case PacketType.secureReadReceipt:
      case PacketType.secureSignaling:
        throw StateError('No secure variant for $this');
    }
  }

  PacketType get clearVariant {
    switch (this) {
      case PacketType.secureMessage:
        return PacketType.message;
      case PacketType.secureFragmentStart:
        return PacketType.fragmentStart;
      case PacketType.secureFragmentContinue:
        return PacketType.fragmentContinue;
      case PacketType.secureFragmentEnd:
        return PacketType.fragmentEnd;
      case PacketType.secureAck:
        return PacketType.ack;
      case PacketType.secureNack:
        return PacketType.nack;
      case PacketType.secureReadReceipt:
        return PacketType.readReceipt;
      case PacketType.secureSignaling:
        return PacketType.signaling;
      case PacketType.announce:
      case PacketType.message:
      case PacketType.fragmentStart:
      case PacketType.fragmentContinue:
      case PacketType.fragmentEnd:
      case PacketType.ack:
      case PacketType.nack:
      case PacketType.readReceipt:
      case PacketType.signaling:
      case PacketType.noiseHandshake:
        throw StateError('No clear variant for $this');
    }
  }
}

/// A Grassroots packet — wire-compatible with the Flutter client.
///
/// Binary format (5-byte header + variable payload):
/// ```
/// [0]       : Packet type (1 byte)
/// [1-4]     : Payload length (4 bytes, big-endian)
/// [5-N]     : Payload (variable length)
/// ```
///
/// The frame carries no identity, authentication, or identifiers of its
/// own. Sender identity and integrity come from the layer the payload rides
/// in: session-encrypted types are authenticated by the Noise session's
/// AEAD, ANNOUNCE payloads are self-signed identity records, and Noise
/// handshake payloads carry (and authenticate) the peer's identity claim.
class GrassrootsPacket {
  static const int headerSize = 5;
  static const int payloadLengthOffset = 1;
  static const int maxPayloadSize = 495;

  final PacketType type;
  final Uint8List payload;

  GrassrootsPacket({
    required this.type,
    required this.payload,
  });

  /// Serialize to binary format.
  Uint8List serialize() {
    final buffer = ByteData(headerSize + payload.length);
    buffer.setUint8(0, type.value);
    buffer.setUint32(1, payload.length, Endian.big);
    final bytes = buffer.buffer.asUint8List();
    bytes.setRange(headerSize, headerSize + payload.length, payload);
    return bytes;
  }

  /// Deserialize from binary format.
  static GrassrootsPacket deserialize(Uint8List data) {
    if (data.length < headerSize) {
      throw FormatException('Packet too small: ${data.length} < $headerSize');
    }

    final buffer = ByteData.view(data.buffer, data.offsetInBytes, data.length);
    final type = PacketType.fromValue(buffer.getUint8(0));
    final payloadLength = buffer.getUint32(1, Endian.big);

    if (data.length < headerSize + payloadLength) {
      throw FormatException(
          'Incomplete payload: expected $payloadLength bytes');
    }
    final payload = Uint8List.fromList(
        data.sublist(headerSize, headerSize + payloadLength));

    return GrassrootsPacket(type: type, payload: payload);
  }

  /// Copy this packet with one or more fields replaced. Used by the Noise
  /// session manager when swapping a clear packet for its encrypted variant
  /// (and vice versa).
  GrassrootsPacket copyWith({
    PacketType? type,
    Uint8List? payload,
  }) {
    return GrassrootsPacket(
      type: type ?? this.type,
      payload: payload ?? this.payload,
    );
  }

  @override
  String toString() =>
      'GrassrootsPacket($type, payload=${payload.length}b)';
}
