import 'dart:typed_data';
import 'package:cryptography/cryptography.dart';

import 'identity.dart';
import 'packet.dart';

/// Protocol handler for the bootstrap anchor.
///
/// Handles the self-signed ANNOUNCE record and record-level Ed25519
/// signing/verification. Wire-compatible with the Flutter client's
/// ProtocolHandler.
class Protocol {
  final AnchorIdentity identity;
  static const int protocolVersion = 2;

  /// Length of the trailing Ed25519 signature on an ANNOUNCE payload.
  static const int announceSignatureLength = 64;

  const Protocol({required this.identity});

  // ===== Signing & Verification =====

  /// Detached Ed25519 signature over arbitrary [message] bytes under the
  /// anchor identity. Used for self-contained signed records (ANNOUNCE).
  Future<Uint8List> signBytes(Uint8List message) async {
    final signature =
        await Ed25519().sign(message, keyPair: identity.keyPair);
    return Uint8List.fromList(signature.bytes);
  }

  /// Verify a detached Ed25519 [signature] over [message] against
  /// [publicKey]. Returns false on any error.
  Future<bool> verifyBytes({
    required Uint8List signature,
    required Uint8List message,
    required Uint8List publicKey,
  }) async {
    try {
      return await Ed25519().verify(
        message,
        signature: Signature(
          signature,
          publicKey: SimplePublicKey(publicKey, type: KeyPairType.ed25519),
        ),
      );
    } catch (e) {
      return false;
    }
  }

  // ===== ANNOUNCE =====

  /// Create a self-signed ANNOUNCE payload. Must match the client's format:
  ///
  /// Format: pubkey(32) + version(2) + nickLen(1) + nick
  /// + candidateCount(2) + repeated(candidateLen(2) + candidate)
  /// + signature(64) over all preceding bytes
  Future<Uint8List> createAnnouncePayload({
    String? address,
    String? linkLocalAddress,
    Iterable<String> addressCandidates = const [],
  }) async {
    final nicknameBytes = Uint8List.fromList(identity.nickname.codeUnits);
    final candidates = <String>{
      if (address != null && address.isNotEmpty) address,
      if (linkLocalAddress != null && linkLocalAddress.isNotEmpty)
        linkLocalAddress,
      ...addressCandidates.where((candidate) => candidate.isNotEmpty),
    };
    final buffer = BytesBuilder();

    // Pubkey (32 bytes)
    buffer.add(identity.publicKey);

    // Protocol version (2 bytes, big-endian)
    final versionBytes = ByteData(2);
    versionBytes.setUint16(0, protocolVersion, Endian.big);
    buffer.add(versionBytes.buffer.asUint8List());

    // Nickname length (1 byte) + nickname
    buffer.addByte(nicknameBytes.length);
    buffer.add(nicknameBytes);

    final candidateCountBytes = ByteData(2);
    candidateCountBytes.setUint16(0, candidates.length, Endian.big);
    buffer.add(candidateCountBytes.buffer.asUint8List());
    for (final candidate in candidates) {
      final candidateBytes = Uint8List.fromList(candidate.codeUnits);
      final candidateLenBytes = ByteData(2);
      candidateLenBytes.setUint16(0, candidateBytes.length, Endian.big);
      buffer.add(candidateLenBytes.buffer.asUint8List());
      buffer.add(candidateBytes);
    }

    buffer.add(await signBytes(buffer.toBytes()));
    return buffer.toBytes();
  }

  /// Decode and verify a self-signed ANNOUNCE payload. Throws
  /// [FormatException] on malformed input or a bad signature.
  Future<AnnounceData> decodeAnnounce(Uint8List data) async {
    var offset = 0;

    final pubkey = data.sublist(offset, offset + 32);
    offset += 32;

    final version = ByteData.view(data.buffer, data.offsetInBytes + offset, 2)
        .getUint16(0, Endian.big);
    offset += 2;

    final nicknameLength = data[offset];
    offset += 1;
    final nickname =
        String.fromCharCodes(data.sublist(offset, offset + nicknameLength));
    offset += nicknameLength;

    if (offset + 2 > data.length) {
      throw const FormatException('ANNOUNCE payload missing candidates');
    }

    final addressCandidates = <String>{};
    final candidateCount =
        ByteData.view(data.buffer, data.offsetInBytes + offset, 2)
            .getUint16(0, Endian.big);
    offset += 2;
    for (var i = 0; i < candidateCount; i++) {
      if (offset + 2 > data.length) {
        throw const FormatException('ANNOUNCE candidate length missing');
      }
      final candidateLength =
          ByteData.view(data.buffer, data.offsetInBytes + offset, 2)
              .getUint16(0, Endian.big);
      offset += 2;
      if (offset + candidateLength > data.length) {
        throw const FormatException('ANNOUNCE candidate truncated');
      }
      if (candidateLength > 0) {
        addressCandidates.add(String.fromCharCodes(
          data.sublist(offset, offset + candidateLength),
        ));
      }
      offset += candidateLength;
    }

    if (data.length != offset + announceSignatureLength) {
      throw const FormatException('ANNOUNCE signature missing or malformed');
    }
    final signature = data.sublist(offset, offset + announceSignatureLength);
    final signedBytes = data.sublist(0, offset);
    if (!await verifyBytes(
      signature: signature,
      message: signedBytes,
      publicKey: Uint8List.fromList(pubkey),
    )) {
      throw const FormatException('ANNOUNCE signature verification failed');
    }

    final address = _firstNonLinkLocalCandidate(addressCandidates);
    final linkLocalAddress = _firstLinkLocalCandidate(addressCandidates);

    return AnnounceData(
      publicKey: Uint8List.fromList(pubkey),
      nickname: nickname,
      protocolVersion: version,
      udpAddress: address,
      linkLocalAddress: linkLocalAddress,
      addressCandidates: addressCandidates,
    );
  }

  String? _firstNonLinkLocalCandidate(Iterable<String> candidates) {
    for (final candidate in candidates) {
      if (!_isLinkLocalCandidate(candidate)) return candidate;
    }
    return null;
  }

  String? _firstLinkLocalCandidate(Iterable<String> candidates) {
    for (final candidate in candidates) {
      if (_isLinkLocalCandidate(candidate)) return candidate;
    }
    return null;
  }

  bool _isLinkLocalCandidate(String candidate) {
    final lower = candidate.toLowerCase();
    if (lower.startsWith('[')) {
      final end = lower.indexOf(']');
      final host = end == -1 ? lower.substring(1) : lower.substring(1, end);
      return host.startsWith('fe80:');
    }
    final colon = lower.lastIndexOf(':');
    final host = colon == -1 ? lower : lower.substring(0, colon);
    return host.startsWith('169.254.');
  }

  /// Create an ANNOUNCE packet (self-signed payload).
  Future<GrassrootsPacket> createAnnouncePacket({String? address}) async {
    return GrassrootsPacket(
      type: PacketType.announce,
      payload: await createAnnouncePayload(address: address),
    );
  }

  /// Create ACK packet.
  GrassrootsPacket createAckPacket({required String messageId}) {
    return GrassrootsPacket(
      type: PacketType.ack,
      payload: Uint8List.fromList(messageId.codeUnits),
    );
  }

  /// Create a signaling packet.
  GrassrootsPacket createSignalingPacket({
    required Uint8List signalingPayload,
  }) {
    return GrassrootsPacket(
      type: PacketType.signaling,
      payload: signalingPayload,
    );
  }
}

/// Decoded ANNOUNCE data.
class AnnounceData {
  final Uint8List publicKey;
  final String nickname;
  final int protocolVersion;
  final String? udpAddress;
  final String? linkLocalAddress;
  final Set<String> addressCandidates;

  const AnnounceData({
    required this.publicKey,
    required this.nickname,
    required this.protocolVersion,
    this.udpAddress,
    this.linkLocalAddress,
    this.addressCandidates = const {},
  });

  String get pubkeyHex =>
      publicKey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  @override
  String toString() => 'AnnounceData($nickname, v$protocolVersion'
      '${udpAddress != null ? ", addr: $udpAddress" : ""}'
      '${linkLocalAddress != null ? ", ll: $linkLocalAddress" : ""}'
      '${addressCandidates.isNotEmpty ? ", candidates: $addressCandidates" : ""})';
}
