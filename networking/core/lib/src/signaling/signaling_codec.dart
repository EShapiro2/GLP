import 'dart:typed_data';

/// Signaling message types between an agent's layer and a rendezvous
/// server's: the address the server observes the agent at, and the invite
/// registration and redemption of a WAN cold call (spec §WAN Cold-Call).
///
/// These ride inside [GrassrootsPacket] payloads with [PacketType.signaling].
/// Authentication is handled by the outer GrassrootsPacket's Ed25519 signature.
///
/// Wire byte values are stable; gaps reflect retired message types
/// (ADDR_QUERY/ADDR_RESPONSE/PUNCH_REQUEST; RV_LIST/FRIEND_LIST at 0x0a/0x0b,
/// with friend-driven signaling, the layer holding no social graph; and
/// PUNCH_INITIATE, PUNCH_READY, RECONNECT and AVAILABLE at 0x05, 0x06, 0x08
/// and 0x09, the transport's reconnection protocol, retired 2026-10-13 for
/// the GLP rendezvous agent, GNA §4.2 and §6).
enum SignalingType {
  /// "Your actual public address is ip:port" — rendezvous server → agent
  /// (STUN-equivalent reflection, on the session coming up)
  addrReflect(0x07),

  /// "Store this invite as unused" — inviter A → rendezvous server.
  ///
  /// IP cold-call registration (spec §IP Cold-Call): carries the derived
  /// InviteId, the InviteKey proof key, and the expiry. A's identity is the
  /// authenticated outer packet sender.
  registerInvite(0x0c),

  /// "Redeem this invite" — holder B → rendezvous server named in the link.
  ///
  /// Carries the InviteId, B's fresh nonce, and a MAC under InviteKey over
  /// (A_pk | B_pk | InviteId | nonce). B's identity is the authenticated
  /// outer packet sender; the raw invite nonce never travels over IP.
  redeemInvite(0x0d),

  /// "Your invite was redeemed by B" — rendezvous server → inviter A.
  ///
  /// Sent after the server atomically flips the invite to used; forwards the
  /// redeemer's identity per the spec.
  inviteRedeemed(0x0e),

  /// "I received the redeemed notification" — inviter A → rendezvous server.
  ///
  /// Acknowledges an INVITE_REDEEMED so the server stops retrying it (the
  /// notification is otherwise resent each cleanup cycle until the invite
  /// expires).
  inviteRedeemedAck(0x0f);

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

/// Inviter A registers a cold-call invite with a rendezvous server.
///
/// A's public key is the authenticated outer packet sender (the registration
/// travels over A's existing authenticated connection to the server, per the
/// spec) — it is deliberately not duplicated inside the payload.
class RegisterInviteMessage extends SignalingMessage {
  @override
  SignalingType get type => SignalingType.registerInvite;

  /// InviteId = SHA-256("glp invite id" | Nonce256), 32 bytes.
  final Uint8List inviteId;

  /// InviteKey = HKDF(Nonce256, "glp invite proof" | ...), 32 bytes. The
  /// proof key the server verifies redemption MACs against.
  final Uint8List inviteKey;

  /// Invite expiry, milliseconds since epoch.
  final int expiresAtMs;

  RegisterInviteMessage({
    required this.inviteId,
    required this.inviteKey,
    required this.expiresAtMs,
  }) {
    if (inviteId.length != 32) throw ArgumentError('inviteId must be 32 bytes');
    if (inviteKey.length != 32) {
      throw ArgumentError('inviteKey must be 32 bytes');
    }
  }

  @override
  String toString() => 'RegisterInvite(id: ${_hexPrefix(inviteId)})';
}

/// Holder B submits a redemption claim to the rendezvous server named in the
/// link. B's public key is the authenticated outer packet sender.
class RedeemInviteMessage extends SignalingMessage {
  @override
  SignalingType get type => SignalingType.redeemInvite;

  /// The invite being redeemed (32 bytes).
  final Uint8List inviteId;

  /// B's fresh 256-bit nonce, bound into the MAC.
  final Uint8List redeemerNonce;

  /// HMAC-SHA256 under InviteKey over (A_pk | B_pk | InviteId | nonce).
  final Uint8List mac;

  RedeemInviteMessage({
    required this.inviteId,
    required this.redeemerNonce,
    required this.mac,
  }) {
    if (inviteId.length != 32) throw ArgumentError('inviteId must be 32 bytes');
    if (redeemerNonce.length != 32) {
      throw ArgumentError('redeemerNonce must be 32 bytes');
    }
    if (mac.length != 32) throw ArgumentError('mac must be 32 bytes');
  }

  @override
  String toString() => 'RedeemInvite(id: ${_hexPrefix(inviteId)})';
}

/// Rendezvous server forwards the redeemer's identity to the inviter after
/// atomically marking the invite used.
class InviteRedeemedMessage extends SignalingMessage {
  @override
  SignalingType get type => SignalingType.inviteRedeemed;

  /// The invite that was redeemed (32 bytes).
  final Uint8List inviteId;

  /// The redeemer B's public key (32 bytes).
  final Uint8List redeemerPubkey;

  InviteRedeemedMessage({
    required this.inviteId,
    required this.redeemerPubkey,
  }) {
    if (inviteId.length != 32) throw ArgumentError('inviteId must be 32 bytes');
    if (redeemerPubkey.length != 32) {
      throw ArgumentError('redeemerPubkey must be 32 bytes');
    }
  }

  @override
  String toString() => 'InviteRedeemed(id: ${_hexPrefix(inviteId)})';
}

/// Inviter A acknowledges an INVITE_REDEEMED so the rendezvous server stops
/// retrying it.
class InviteRedeemedAckMessage extends SignalingMessage {
  @override
  SignalingType get type => SignalingType.inviteRedeemedAck;

  /// The invite whose redeemed notification is being acknowledged (32 bytes).
  final Uint8List inviteId;

  InviteRedeemedAckMessage({required this.inviteId}) {
    if (inviteId.length != 32) throw ArgumentError('inviteId must be 32 bytes');
  }

  @override
  String toString() => 'InviteRedeemedAck(id: ${_hexPrefix(inviteId)})';
}

String _hexPrefix(Uint8List bytes) => bytes
    .take(4)
    .map((b) => b.toRadixString(16).padLeft(2, '0'))
    .join();

// ===== Codec =====

/// Binary encoder/decoder for signaling messages.
///
/// Wire formats:
///
/// ```
/// ADDR_REFLECT   : type(1) + ipLen(2) + ipBytes + port(2)
/// REGISTER_INVITE: type(1) + inviteId(32) + inviteKey(32) + expiresAtMs(8)
/// REDEEM_INVITE  : type(1) + inviteId(32) + redeemerNonce(32) + mac(32)
/// INVITE_REDEEMED: type(1) + inviteId(32) + redeemerPubkey(32)
/// ```
class SignalingCodec {
  const SignalingCodec();

  // ===== Encoding =====

  Uint8List encode(SignalingMessage msg) {
    return switch (msg) {
      AddrReflectMessage() => _encodeAddrReflect(msg),
      RegisterInviteMessage() => _encodeRegisterInvite(msg),
      RedeemInviteMessage() => _encodeRedeemInvite(msg),
      InviteRedeemedMessage() => _encodeInviteRedeemed(msg),
      InviteRedeemedAckMessage() => _encodeInviteRedeemedAck(msg),
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

  Uint8List _encodeRegisterInvite(RegisterInviteMessage msg) {
    final buffer = BytesBuilder();
    buffer.addByte(SignalingType.registerInvite.value);
    buffer.add(msg.inviteId);
    buffer.add(msg.inviteKey);
    _writeUint64(buffer, msg.expiresAtMs);
    return buffer.toBytes();
  }

  Uint8List _encodeRedeemInvite(RedeemInviteMessage msg) {
    final buffer = BytesBuilder();
    buffer.addByte(SignalingType.redeemInvite.value);
    buffer.add(msg.inviteId);
    buffer.add(msg.redeemerNonce);
    buffer.add(msg.mac);
    return buffer.toBytes();
  }

  Uint8List _encodeInviteRedeemed(InviteRedeemedMessage msg) {
    final buffer = BytesBuilder();
    buffer.addByte(SignalingType.inviteRedeemed.value);
    buffer.add(msg.inviteId);
    buffer.add(msg.redeemerPubkey);
    return buffer.toBytes();
  }

  Uint8List _encodeInviteRedeemedAck(InviteRedeemedAckMessage msg) {
    final buffer = BytesBuilder();
    buffer.addByte(SignalingType.inviteRedeemedAck.value);
    buffer.add(msg.inviteId);
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
      SignalingType.registerInvite => _decodeRegisterInvite(payload),
      SignalingType.redeemInvite => _decodeRedeemInvite(payload),
      SignalingType.inviteRedeemed => _decodeInviteRedeemed(payload),
      SignalingType.inviteRedeemedAck => _decodeInviteRedeemedAck(payload),
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

  RegisterInviteMessage _decodeRegisterInvite(Uint8List data) {
    if (data.length != 72) {
      throw const FormatException('RegisterInvite payload must be 72 bytes');
    }
    return RegisterInviteMessage(
      inviteId: Uint8List.fromList(data.sublist(0, 32)),
      inviteKey: Uint8List.fromList(data.sublist(32, 64)),
      expiresAtMs: _readUint64(data, 64),
    );
  }

  RedeemInviteMessage _decodeRedeemInvite(Uint8List data) {
    if (data.length != 96) {
      throw const FormatException('RedeemInvite payload must be 96 bytes');
    }
    return RedeemInviteMessage(
      inviteId: Uint8List.fromList(data.sublist(0, 32)),
      redeemerNonce: Uint8List.fromList(data.sublist(32, 64)),
      mac: Uint8List.fromList(data.sublist(64, 96)),
    );
  }

  InviteRedeemedMessage _decodeInviteRedeemed(Uint8List data) {
    if (data.length != 64) {
      throw const FormatException('InviteRedeemed payload must be 64 bytes');
    }
    return InviteRedeemedMessage(
      inviteId: Uint8List.fromList(data.sublist(0, 32)),
      redeemerPubkey: Uint8List.fromList(data.sublist(32, 64)),
    );
  }

  InviteRedeemedAckMessage _decodeInviteRedeemedAck(Uint8List data) {
    if (data.length != 32) {
      throw const FormatException('InviteRedeemedAck payload must be 32 bytes');
    }
    return InviteRedeemedAckMessage(
      inviteId: Uint8List.fromList(data.sublist(0, 32)),
    );
  }

  // ===== Helpers =====

  void _writeUint16(BytesBuilder buffer, int value) {
    buffer.addByte((value >> 8) & 0xFF);
    buffer.addByte(value & 0xFF);
  }

  int _readUint16(Uint8List data, int offset) {
    return (data[offset] << 8) | data[offset + 1];
  }

  void _writeUint64(BytesBuilder buffer, int value) {
    for (var i = 7; i >= 0; i--) {
      buffer.addByte((value >> (8 * i)) & 0xFF);
    }
  }

  int _readUint64(Uint8List data, int offset) {
    var value = 0;
    for (var i = 0; i < 8; i++) {
      value = (value << 8) | data[offset + i];
    }
    return value;
  }
}
