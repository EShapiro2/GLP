import 'dart:typed_data';

/// Packet types matching Grassroots protocol
enum PacketType {
  /// Peer identity announcement (sent periodically)
  announce(0x01),

  /// Application message (GSG blocks go here)
  message(0x02),

  /// Start of fragmented message
  fragmentStart(0x03),

  /// Continuation fragment
  fragmentContinue(0x04),

  /// Final fragment
  fragmentEnd(0x05),

  /// Delivery acknowledgment (for UDP transport)
  ack(0x06),

  /// Negative acknowledgment / request for data
  nack(0x07),

  /// Read receipt (recipient has read the message)
  readReceipt(0x08),

  /// Signaling (address registration, query, hole-punch coordination)
  signaling(0x09),

  /// Noise XX handshake message.
  noiseHandshake(0x0A),

  /// Session-encrypted application message.
  secureMessage(0x0B),

  /// Session-encrypted start of fragmented message.
  secureFragmentStart(0x0C),

  /// Session-encrypted continuation fragment.
  secureFragmentContinue(0x0D),

  /// Session-encrypted final fragment.
  secureFragmentEnd(0x0E),

  /// Session-encrypted delivery acknowledgment.
  secureAck(0x0F),

  /// Session-encrypted negative acknowledgment.
  secureNack(0x10),

  /// Session-encrypted read receipt.
  secureReadReceipt(0x11),

  /// Session-encrypted signaling packet.
  secureSignaling(0x12);

  final int value;
  const PacketType(this.value);

  static PacketType fromValue(int value) {
    return PacketType.values.firstWhere(
      (t) => t.value == value,
      orElse: () => throw ArgumentError('Unknown packet type: $value'),
    );
  }
}

extension PacketTypeSessionSecurity on PacketType {
  /// Whether this packet type should be payload-encrypted once a Noise session
  /// exists. ANNOUNCE and Noise handshake packets intentionally stay clear so
  /// peers can identify each other and bootstrap the session.
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
        throw StateError('Packet type $this has no secure variant');
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
        throw StateError('Packet type $this is not session encrypted');
    }
  }
}

/// A Grassroots packet — the wire frame shared by both transports.
///
/// Binary format:
/// ```
/// [0]       : Packet type (1 byte)
/// [1-4]     : Payload length (4 bytes, big-endian)
/// [5-N]     : Payload (variable length)
/// ```
///
/// Total header size: 5 bytes. The 4-byte payload length is the on-wire
/// framer: stream transports (UDP/UDX) accumulate bytes until
/// `headerSize + payloadLength` are available before treating a buffer as
/// one packet.
///
/// The frame carries no identity, authentication, or identifiers of its
/// own. Sender identity and integrity come from the layer the payload rides
/// in: session-encrypted types are authenticated by the Noise session's
/// AEAD, ANNOUNCE payloads are self-signed identity records, and Noise
/// handshake payloads carry (and authenticate) the peer's identity claim.
/// Message identity (delivery dedup, ACK correlation) is payload content:
/// MESSAGE payloads open with their messageId, as fragment payloads
/// always have.
class GrassrootsPacket {
  static const int headerSize = 5;
  static const int payloadLengthOffset = 1; // byte index of length field

  /// Soft target for fragmented payloads — chosen to keep a single
  /// encrypted packet under ~500 byte MTU on BLE.
  static const int maxPayloadSize = 495; // 500 - 5

  /// Packet type
  final PacketType type;

  /// Payload data (type-specific)
  final Uint8List payload;

  GrassrootsPacket({
    required this.type,
    required this.payload,
  });

  GrassrootsPacket copyWith({
    PacketType? type,
    Uint8List? payload,
  }) {
    return GrassrootsPacket(
      type: type ?? this.type,
      payload: payload ?? this.payload,
    );
  }

  /// Serialize to binary format for transmission
  Uint8List serialize() {
    final buffer = ByteData(headerSize + payload.length);
    buffer.setUint8(0, type.value);
    buffer.setUint32(1, payload.length, Endian.big);
    final bytes = buffer.buffer.asUint8List();
    bytes.setRange(headerSize, headerSize + payload.length, payload);
    return bytes;
  }

  /// Deserialize from binary format
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

  /// Peek the payload length from a serialized buffer without parsing the
  /// rest of the header. Used by stream-transport receive paths (UDP) to
  /// know when enough bytes have been accumulated to slice out one packet.
  /// Returns null when the buffer is shorter than the header.
  static int? peekPayloadLength(Uint8List data, [int offset = 0]) {
    if (data.length - offset < headerSize) return null;
    final view = ByteData.view(data.buffer, data.offsetInBytes + offset,
        data.length - offset);
    return view.getUint32(payloadLengthOffset, Endian.big);
  }

  @override
  String toString() =>
      'GrassrootsPacket($type, payload=${payload.length}b)';
}
