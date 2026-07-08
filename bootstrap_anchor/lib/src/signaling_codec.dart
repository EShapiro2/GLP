import 'dart:typed_data';

/// Signaling message types — identical to the client-side SignalingType.
///
/// Wire byte values are stable; gaps reflect deprecated message types
/// (ADDR_QUERY/ADDR_RESPONSE/PUNCH_REQUEST were removed when the rendezvous
/// flow switched to RECONNECT/AVAILABLE matching).
enum SignalingType {
  punchInitiate(0x05),
  punchReady(0x06),
  addrReflect(0x07),
  reconnect(0x08),
  available(0x09),
  rvList(0x0a),
  // 0x0b (friendList) is agent↔friend only and never reaches the anchor.
  registerInvite(0x0c),
  redeemInvite(0x0d),
  inviteRedeemed(0x0e),
  inviteRedeemedAck(0x0f);

  final int value;
  const SignalingType(this.value);

  static SignalingType fromValue(int value) {
    return SignalingType.values.firstWhere(
      (t) => t.value == value,
      orElse: () => throw ArgumentError(
          'Unknown signaling type: 0x${value.toRadixString(16)}'),
    );
  }
}

// ===== Message classes =====

sealed class SignalingMessage {
  SignalingType get type;
}

class PunchInitiateMessage extends SignalingMessage {
  @override
  SignalingType get type => SignalingType.punchInitiate;
  final Uint8List peerPubkey;
  final String ip;
  final int port;
  PunchInitiateMessage(
      {required this.peerPubkey, required this.ip, required this.port});
}

class PunchReadyMessage extends SignalingMessage {
  @override
  SignalingType get type => SignalingType.punchReady;
  final Uint8List peerPubkey;
  PunchReadyMessage({required this.peerPubkey});
}

class AddrReflectMessage extends SignalingMessage {
  @override
  SignalingType get type => SignalingType.addrReflect;
  final String ip;
  final int port;
  AddrReflectMessage({required this.ip, required this.port});
}

class ReconnectMessage extends SignalingMessage {
  @override
  SignalingType get type => SignalingType.reconnect;
  final Uint8List initiatorPubkey;
  final Uint8List peerPubkey;
  ReconnectMessage({
    required this.initiatorPubkey,
    required this.peerPubkey,
  });
}

class AvailableMessage extends SignalingMessage {
  @override
  SignalingType get type => SignalingType.available;
  final Uint8List peerPubkey;
  AvailableMessage({required this.peerPubkey});
}

class RvServerEntry {
  final Uint8List pubkey;
  final String address;
  const RvServerEntry({required this.pubkey, required this.address});
}

class RvListMessage extends SignalingMessage {
  @override
  SignalingType get type => SignalingType.rvList;
  final List<RvServerEntry> entries;
  RvListMessage({required this.entries});
}

/// Inviter A registers a cold-call invite (spec §IP Cold-Call). A's identity
/// is the authenticated outer packet sender.
class RegisterInviteMessage extends SignalingMessage {
  @override
  SignalingType get type => SignalingType.registerInvite;
  final Uint8List inviteId;
  final Uint8List inviteKey;
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
}

/// Holder B's redemption claim. B's identity is the authenticated outer
/// packet sender; the MAC is HMAC-SHA256 under the registered InviteKey over
/// (A_pk | B_pk | InviteId | redeemerNonce).
class RedeemInviteMessage extends SignalingMessage {
  @override
  SignalingType get type => SignalingType.redeemInvite;
  final Uint8List inviteId;
  final Uint8List redeemerNonce;
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
}

/// Anchor forwards the redeemer's identity to the inviter after the invite
/// is atomically flipped to used.
class InviteRedeemedMessage extends SignalingMessage {
  @override
  SignalingType get type => SignalingType.inviteRedeemed;
  final Uint8List inviteId;
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
}

/// Inviter A acknowledges receipt of an INVITE_REDEEMED notification so the
/// anchor stops retrying it — makes the redeemer-forwarding an ack'd delivery
/// rather than fire-and-forget around the single-use flip.
class InviteRedeemedAckMessage extends SignalingMessage {
  @override
  SignalingType get type => SignalingType.inviteRedeemedAck;
  final Uint8List inviteId;
  InviteRedeemedAckMessage({required this.inviteId}) {
    if (inviteId.length != 32) throw ArgumentError('inviteId must be 32 bytes');
  }
}

// ===== Codec =====

/// Binary encoder/decoder for signaling messages.
class SignalingCodec {
  const SignalingCodec();

  Uint8List encode(SignalingMessage msg) {
    return switch (msg) {
      PunchInitiateMessage() => _encodePunchInitiate(msg),
      PunchReadyMessage() => _encodePunchReady(msg),
      AddrReflectMessage() => _encodeAddrReflect(msg),
      ReconnectMessage() => _encodeReconnect(msg),
      AvailableMessage() => _encodeAvailable(msg),
      RvListMessage() => _encodeRvList(msg),
      RegisterInviteMessage() => _encodeRegisterInvite(msg),
      RedeemInviteMessage() => _encodeRedeemInvite(msg),
      InviteRedeemedMessage() => _encodeInviteRedeemed(msg),
      InviteRedeemedAckMessage() => _encodeInviteRedeemedAck(msg),
    };
  }

  SignalingMessage decode(Uint8List data) {
    if (data.isEmpty) {
      throw const FormatException('Empty signaling payload');
    }
    final type = SignalingType.fromValue(data[0]);
    final payload = Uint8List.sublistView(data, 1);

    return switch (type) {
      SignalingType.punchInitiate => _decodePunchInitiate(payload),
      SignalingType.punchReady => _decodePunchReady(payload),
      SignalingType.addrReflect => _decodeAddrReflect(payload),
      SignalingType.reconnect => _decodeReconnect(payload),
      SignalingType.available => _decodeAvailable(payload),
      SignalingType.rvList => _decodeRvList(payload),
      SignalingType.registerInvite => _decodeRegisterInvite(payload),
      SignalingType.redeemInvite => _decodeRedeemInvite(payload),
      SignalingType.inviteRedeemed => _decodeInviteRedeemed(payload),
      SignalingType.inviteRedeemedAck => _decodeInviteRedeemedAck(payload),
    };
  }

  // ===== Encoding =====

  Uint8List _encodePunchInitiate(PunchInitiateMessage msg) {
    final buffer = BytesBuilder();
    buffer.addByte(SignalingType.punchInitiate.value);
    buffer.add(msg.peerPubkey);
    final ipBytes = Uint8List.fromList(msg.ip.codeUnits);
    _writeUint16(buffer, ipBytes.length);
    buffer.add(ipBytes);
    _writeUint16(buffer, msg.port);
    return buffer.toBytes();
  }

  Uint8List _encodePunchReady(PunchReadyMessage msg) {
    final buffer = BytesBuilder();
    buffer.addByte(SignalingType.punchReady.value);
    buffer.add(msg.peerPubkey);
    return buffer.toBytes();
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

  Uint8List _encodeReconnect(ReconnectMessage msg) {
    final buffer = BytesBuilder();
    buffer.addByte(SignalingType.reconnect.value);
    buffer.add(msg.initiatorPubkey);
    buffer.add(msg.peerPubkey);
    return buffer.toBytes();
  }

  Uint8List _encodeAvailable(AvailableMessage msg) {
    final buffer = BytesBuilder();
    buffer.addByte(SignalingType.available.value);
    buffer.add(msg.peerPubkey);
    return buffer.toBytes();
  }

  Uint8List _encodeRvList(RvListMessage msg) {
    final buffer = BytesBuilder();
    buffer.addByte(SignalingType.rvList.value);
    _writeUint16(buffer, msg.entries.length);
    for (final entry in msg.entries) {
      buffer.add(entry.pubkey);
      final addrBytes = Uint8List.fromList(entry.address.codeUnits);
      _writeUint16(buffer, addrBytes.length);
      buffer.add(addrBytes);
    }
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

  PunchInitiateMessage _decodePunchInitiate(Uint8List data) {
    if (data.length < 36) {
      throw const FormatException('PunchInitiate payload too short');
    }
    final peerPubkey = Uint8List.fromList(data.sublist(0, 32));
    var offset = 32;
    final ipLen = _readUint16(data, offset);
    offset += 2;
    final ip = String.fromCharCodes(data.sublist(offset, offset + ipLen));
    offset += ipLen;
    final port = _readUint16(data, offset);
    return PunchInitiateMessage(peerPubkey: peerPubkey, ip: ip, port: port);
  }

  PunchReadyMessage _decodePunchReady(Uint8List data) {
    if (data.length < 32) {
      throw const FormatException('PunchReady payload too short');
    }
    return PunchReadyMessage(
        peerPubkey: Uint8List.fromList(data.sublist(0, 32)));
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

  ReconnectMessage _decodeReconnect(Uint8List data) {
    if (data.length != 64) {
      throw const FormatException('Reconnect payload has invalid length');
    }
    return ReconnectMessage(
        initiatorPubkey: Uint8List.fromList(data.sublist(0, 32)),
        peerPubkey: Uint8List.fromList(data.sublist(32, 64)));
  }

  AvailableMessage _decodeAvailable(Uint8List data) {
    if (data.length < 32) {
      throw const FormatException('Available payload too short');
    }
    return AvailableMessage(
        peerPubkey: Uint8List.fromList(data.sublist(0, 32)));
  }

  RvListMessage _decodeRvList(Uint8List data) {
    final count = _readUint16(data, 0);
    var offset = 2;
    final entries = <RvServerEntry>[];
    for (var i = 0; i < count; i++) {
      if (offset + 34 > data.length) {
        throw const FormatException('RvList entry truncated');
      }
      final pubkey = Uint8List.fromList(data.sublist(offset, offset + 32));
      offset += 32;
      final addrLen = _readUint16(data, offset);
      offset += 2;
      if (offset + addrLen > data.length) {
        throw const FormatException('RvList address truncated');
      }
      final address =
          String.fromCharCodes(data.sublist(offset, offset + addrLen));
      offset += addrLen;
      entries.add(RvServerEntry(pubkey: pubkey, address: address));
    }
    return RvListMessage(entries: entries);
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
