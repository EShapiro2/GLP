import 'dart:typed_data';

/// Signaling message types between an agent's layer and a rendezvous
/// server's: the address the server observes the agent at (spec
/// §Connectivity and Address).
///
/// These ride inside [GrassrootsPacket] payloads with [PacketType.signaling].
/// Authentication is handled by the outer GrassrootsPacket's Ed25519 signature.
///
/// Wire byte values are stable; gaps reflect retired message types
/// (ADDR_QUERY/ADDR_RESPONSE/PUNCH_REQUEST; RV_LIST/FRIEND_LIST at 0x0a/0x0b,
/// with friend-driven signaling, the layer holding no social graph; and
/// PUNCH_INITIATE, PUNCH_READY, RECONNECT and AVAILABLE at 0x05, 0x06, 0x08
/// and 0x09, the transport's reconnection protocol, retired 2026-10-13 for
/// the GLP rendezvous agent, GNA §4.2 and §6; REGISTER_INVITE,
/// REDEEM_INVITE, INVITE_REDEEMED and INVITE_REDEEMED_ACK at 0x0c to 0x0f,
/// the WAN cold-call invite, retired 2026-10-15, the layer having no invite,
/// GNA §4.4).
enum SignalingType {
  /// "Your actual public address is ip:port" — rendezvous server → agent
  /// (STUN-equivalent reflection, on the session coming up)
  addrReflect(0x07);

  final int value;
  const SignalingType(this.value);

  static SignalingType fromValue(int value) {
    return SignalingType.values.firstWhere(
      (t) => t.value == value,
      orElse: () => throw ArgumentError(
        'Unknown signaling type: 0x${value.toRadixString(16)}',
      ),
    );
  }
}

// ===== Message classes =====

/// Base class for decoded signaling messages.
sealed class SignalingMessage {
  SignalingType get type;
}

/// A rendezvous server reflects the agent's observed public address: the
/// source address of the agent's packets, NAT-translated, with the external
/// port (spec §Connectivity and Address).
class AddrReflectMessage extends SignalingMessage {
  @override
  SignalingType get type => SignalingType.addrReflect;

  /// The agent's observed public IP address.
  final String ip;

  /// The agent's observed public UDP port.
  final int port;

  AddrReflectMessage({required this.ip, required this.port});

  @override
  String toString() => 'AddrReflect($ip:$port)';
}

// ===== Codec =====

/// Binary encoder/decoder for signaling messages.
///
/// Wire formats:
///
/// ```
/// ADDR_REFLECT   : type(1) + ipLen(2) + ipBytes + port(2)
/// ```
class SignalingCodec {
  const SignalingCodec();

  // ===== Encoding =====

  Uint8List encode(SignalingMessage msg) {
    return switch (msg) {
      AddrReflectMessage() => _encodeAddrReflect(msg),
    };
  }

  Uint8List _encodeAddrReflect(AddrReflectMessage msg) {
    final ipBytes = Uint8List.fromList(msg.ip.codeUnits);
    final buffer = BytesBuilder();
    buffer.addByte(SignalingType.addrReflect.value);
    _writeUint16(buffer, ipBytes.length);
    buffer.add(ipBytes);
    _writeUint16(buffer, msg.port);
    return buffer.toBytes();
  }

  // ===== Decoding =====

  /// Decode a signaling payload into a [SignalingMessage].
  ///
  /// Throws [FormatException] if the payload is malformed.
  SignalingMessage decode(Uint8List data) {
    if (data.isEmpty) {
      throw const FormatException('Empty signaling payload');
    }

    final type = SignalingType.fromValue(data[0]);
    final payload = Uint8List.sublistView(data, 1);

    return switch (type) {
      SignalingType.addrReflect => _decodeAddrReflect(payload),
    };
  }

  AddrReflectMessage _decodeAddrReflect(Uint8List data) {
    var offset = 0;
    final ipLen = _readUint16(data, offset);
    offset += 2;
    final ip = String.fromCharCodes(data.sublist(offset, offset + ipLen));
    offset += ipLen;
    final port = _readUint16(data, offset);
    return AddrReflectMessage(ip: ip, port: port);
  }

  // ===== Helpers =====

  void _writeUint16(BytesBuilder buffer, int value) {
    buffer.addByte((value >> 8) & 0xFF);
    buffer.addByte(value & 0xFF);
  }

  int _readUint16(Uint8List data, int offset) {
    return (data[offset] << 8) | data[offset + 1];
  }
}
