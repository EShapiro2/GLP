import 'dart:typed_data';

/// Signaling message types for the rendezvous reconnection protocol.
///
/// These ride inside [GrassrootsPacket] payloads with [PacketType.signaling].
/// Authentication is handled by the outer GrassrootsPacket's Ed25519 signature.
///
/// Wire byte values are stable; gaps reflect deprecated message types
/// (ADDR_QUERY/ADDR_RESPONSE/PUNCH_REQUEST were removed when the rendezvous
/// flow switched to RECONNECT/AVAILABLE matching).
enum SignalingType {
  /// "Start sending UDP to ip:port Y for hole-punch" — facilitator → agent
  /// (also used for direct friend-to-friend punch coordination)
  punchInitiate(0x05),

  /// "I've opened my NAT, tell the other side" — agent → facilitator → peer
  punchReady(0x06),

  /// "Your actual public address is ip:port" — facilitator → agent
  /// (STUN-equivalent reflection, triggered by ANNOUNCE)
  addrReflect(0x07),

  /// "I want to reconnect to peer X" — agent → rendezvous facilitator
  ///
  /// Cold-call message sent when the agent's connectivity changed and it
  /// wants the facilitator to coordinate a hole-punch with peer X. The
  /// facilitator observes the sender's source IP/port and waits for a
  /// matching AVAILABLE from peer X.
  reconnect(0x08),

  /// "I'm available to peer X" — agent → rendezvous facilitator
  ///
  /// Cold-call message sent when the agent detects peer X went silent and
  /// wants to remain reachable for X's reconnect attempts. The facilitator
  /// observes the sender's source IP/port and matches against any pending
  /// RECONNECT from peer X.
  available(0x09),

  /// "Here is my list of rendezvous server pubkeys" — agent → friend
  ///
  /// Sent after friendship establishment, on new live connection to a friend,
  /// and whenever the agent's RV settings change. The recipient stores the
  /// list per-friend so that on detecting the friend went silent, it fans
  /// out AVAILABLE to those exact servers (per the spec: B contacts A's
  /// known rendezvous agents).
  rvList(0x0a),

  /// "Here is my current accepted friend set" — agent → friend.
  ///
  /// Used to maintain the friends-of-friends map that drives friend-mediated
  /// rendezvous.
  friendList(0x0b),

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

/// Well-connected friend tells agent to start hole-punching to a peer.
class PunchInitiateMessage extends SignalingMessage {
  @override
  SignalingType get type => SignalingType.punchInitiate;

  /// The public key of the peer to punch toward (32 bytes).
  final Uint8List peerPubkey;

  /// The peer's IP address to send punch packets to.
  final String ip;

  /// The peer's UDP port to send punch packets to.
  final int port;

  PunchInitiateMessage({
    required this.peerPubkey,
    required this.ip,
    required this.port,
  });

  @override
  String toString() =>
      'PunchInitiate(peer: ${peerPubkey.sublist(0, 4).map((b) => b.toRadixString(16).padLeft(2, '0')).join()}..., '
      '$ip:$port)';
}

/// Agent tells a well-connected friend that its NAT is open.
class PunchReadyMessage extends SignalingMessage {
  @override
  SignalingType get type => SignalingType.punchReady;

  /// The public key of the peer we're punching toward (32 bytes).
  final Uint8List peerPubkey;

  PunchReadyMessage({required this.peerPubkey});

  @override
  String toString() =>
      'PunchReady(peer: ${peerPubkey.sublist(0, 4).map((b) => b.toRadixString(16).padLeft(2, '0')).join()}...)';
}

/// Well-connected friend reflects the agent's observed public address.
///
/// When a well-connected friend receives an ANNOUNCE over UDP, it compares
/// the claimed address in the payload with the observed source address on the
/// UDX connection. If they differ, it sends ADDR_REFLECT back with the actual
/// address — letting the agent learn its true NAT-translated address,
/// including the correct external port.
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

/// Agent asks a rendezvous facilitator to coordinate reconnection with a peer.
///
/// The agent's source address is observed by the facilitator from the
/// containing UDP packet — no address is carried in the payload.
class ReconnectMessage extends SignalingMessage {
  @override
  SignalingType get type => SignalingType.reconnect;

  /// The public key of the reconnect initiator (32 bytes).
  ///
  /// This duplicates the authenticated outer packet sender for protocol
  /// clarity. Receivers must reject it if it does not match the outer sender.
  final Uint8List initiatorPubkey;

  /// The public key of the peer we want to reconnect to (32 bytes).
  final Uint8List peerPubkey;

  ReconnectMessage({required this.initiatorPubkey, required this.peerPubkey});

  @override
  String toString() =>
      'Reconnect(initiator: ${initiatorPubkey.sublist(0, 4).map((b) => b.toRadixString(16).padLeft(2, '0')).join()}, peer: ${peerPubkey.sublist(0, 4).map((b) => b.toRadixString(16).padLeft(2, '0')).join()}...)';
}

/// Agent declares availability to a peer at a rendezvous facilitator.
///
/// The agent's source address is observed by the facilitator from the
/// containing UDP packet — no address is carried in the payload.
class AvailableMessage extends SignalingMessage {
  @override
  SignalingType get type => SignalingType.available;

  /// The public key of the peer we want to be reachable from (32 bytes).
  final Uint8List peerPubkey;

  AvailableMessage({required this.peerPubkey});

  @override
  String toString() =>
      'Available(peer: ${peerPubkey.sublist(0, 4).map((b) => b.toRadixString(16).padLeft(2, '0')).join()}...)';
}

/// One rendezvous server's identity: pubkey + reachable address.
class RvServerEntry {
  /// 32-byte Ed25519 public key of the rendezvous server.
  final Uint8List pubkey;

  /// "ip:port" address where the rendezvous server can be reached.
  final String address;

  const RvServerEntry({required this.pubkey, required this.address});

  String get pubkeyHex =>
      pubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  @override
  String toString() =>
      'RvServerEntry($address, ${pubkeyHex.substring(0, 8)}...)';
}

/// Agent informs a friend about its configured rendezvous servers.
///
/// The friend stores this list keyed by the sender's pubkey so that, on
/// detecting the sender went silent, it can send AVAILABLE to these exact
/// servers — matching the sender's RECONNECT (which goes to the same set).
class RvListMessage extends SignalingMessage {
  @override
  SignalingType get type => SignalingType.rvList;

  /// Rendezvous server entries (pubkey + address pairs).
  final List<RvServerEntry> entries;

  RvListMessage({required this.entries});

  @override
  String toString() => 'RvList(count: ${entries.length})';
}

/// Agent informs a friend about its current accepted friends.
class FriendListMessage extends SignalingMessage {
  @override
  SignalingType get type => SignalingType.friendList;

  /// Friend public keys (32 bytes each).
  final List<Uint8List> friendPubkeys;

  FriendListMessage({required this.friendPubkeys});

  @override
  String toString() => 'FriendList(count: ${friendPubkeys.length})';
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
/// PUNCH_INITIATE : type(1) + peerPubkey(32) + ipLen(2) + ipBytes + port(2)
/// PUNCH_READY    : type(1) + peerPubkey(32)
/// ADDR_REFLECT   : type(1) + ipLen(2) + ipBytes + port(2)
/// RECONNECT      : type(1) + initiatorPubkey(32) + peerPubkey(32)
/// AVAILABLE      : type(1) + peerPubkey(32)
/// RV_LIST        : type(1) + count(2) +
///                  repeated(pubkey(32) + addrLen(2) + addrBytes)
/// FRIEND_LIST    : type(1) + count(2) + repeated(pubkey(32))
/// REGISTER_INVITE: type(1) + inviteId(32) + inviteKey(32) + expiresAtMs(8)
/// REDEEM_INVITE  : type(1) + inviteId(32) + redeemerNonce(32) + mac(32)
/// INVITE_REDEEMED: type(1) + inviteId(32) + redeemerPubkey(32)
/// ```
class SignalingCodec {
  const SignalingCodec();

  // ===== Encoding =====

  Uint8List encode(SignalingMessage msg) {
    return switch (msg) {
      PunchInitiateMessage() => _encodePunchInitiate(msg),
      PunchReadyMessage() => _encodePunchReady(msg),
      AddrReflectMessage() => _encodeAddrReflect(msg),
      ReconnectMessage() => _encodeReconnect(msg),
      AvailableMessage() => _encodeAvailable(msg),
      RvListMessage() => _encodeRvList(msg),
      FriendListMessage() => _encodeFriendList(msg),
      RegisterInviteMessage() => _encodeRegisterInvite(msg),
      RedeemInviteMessage() => _encodeRedeemInvite(msg),
      InviteRedeemedMessage() => _encodeInviteRedeemed(msg),
      InviteRedeemedAckMessage() => _encodeInviteRedeemedAck(msg),
    };
  }

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

  Uint8List _encodeFriendList(FriendListMessage msg) {
    final buffer = BytesBuilder();
    buffer.addByte(SignalingType.friendList.value);
    _writeUint16(buffer, msg.friendPubkeys.length);
    for (final pubkey in msg.friendPubkeys) {
      if (pubkey.length != 32) {
        throw ArgumentError('FriendList pubkey must be 32 bytes');
      }
      buffer.add(pubkey);
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
      SignalingType.punchInitiate => _decodePunchInitiate(payload),
      SignalingType.punchReady => _decodePunchReady(payload),
      SignalingType.addrReflect => _decodeAddrReflect(payload),
      SignalingType.reconnect => _decodeReconnect(payload),
      SignalingType.available => _decodeAvailable(payload),
      SignalingType.rvList => _decodeRvList(payload),
      SignalingType.friendList => _decodeFriendList(payload),
      SignalingType.registerInvite => _decodeRegisterInvite(payload),
      SignalingType.redeemInvite => _decodeRedeemInvite(payload),
      SignalingType.inviteRedeemed => _decodeInviteRedeemed(payload),
      SignalingType.inviteRedeemedAck => _decodeInviteRedeemedAck(payload),
    };
  }

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
      peerPubkey: Uint8List.fromList(data.sublist(0, 32)),
    );
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
      peerPubkey: Uint8List.fromList(data.sublist(32, 64)),
    );
  }

  AvailableMessage _decodeAvailable(Uint8List data) {
    if (data.length < 32) {
      throw const FormatException('Available payload too short');
    }
    return AvailableMessage(
      peerPubkey: Uint8List.fromList(data.sublist(0, 32)),
    );
  }

  RvListMessage _decodeRvList(Uint8List data) {
    if (data.length < 2) {
      throw const FormatException('RvList payload too short');
    }
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
      final address = String.fromCharCodes(
        data.sublist(offset, offset + addrLen),
      );
      offset += addrLen;
      entries.add(RvServerEntry(pubkey: pubkey, address: address));
    }
    return RvListMessage(entries: entries);
  }

  FriendListMessage _decodeFriendList(Uint8List data) {
    if (data.length < 2) {
      throw const FormatException('FriendList payload too short');
    }
    final count = _readUint16(data, 0);
    var offset = 2;
    final friendPubkeys = <Uint8List>[];
    for (var i = 0; i < count; i++) {
      if (offset + 32 > data.length) {
        throw const FormatException('FriendList entry truncated');
      }
      friendPubkeys.add(Uint8List.fromList(data.sublist(offset, offset + 32)));
      offset += 32;
    }
    if (offset != data.length) {
      throw const FormatException('FriendList payload has trailing bytes');
    }
    return FriendListMessage(friendPubkeys: friendPubkeys);
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
