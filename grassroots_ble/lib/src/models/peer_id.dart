/// Peer identifier for the Grassroots BLE protocol.
///
/// A PeerID is the first 8 bytes (64 bits) of the SHA-256 fingerprint
/// of a user's Noise static public key.
library;

import 'dart:typed_data';

import '../constants.dart';

/// A unique identifier for a peer in the network.
///
/// PeerIDs are derived from the SHA-256 hash of a peer's Noise static
/// public key. Only the first 8 bytes are used for routing efficiency.
class PeerId {
  /// The raw 8-byte identifier.
  final Uint8List bytes;

  /// Creates a PeerId from raw bytes.
  ///
  /// Throws [ArgumentError] if bytes is not exactly 8 bytes.
  PeerId(Uint8List bytes) : bytes = Uint8List.fromList(bytes) {
    if (bytes.length != peerIdSize) {
      throw ArgumentError(
        'PeerId must be exactly $peerIdSize bytes, got ${bytes.length}',
      );
    }
  }

  /// Creates a PeerId from a hex string.
  ///
  /// Throws [FormatException] if the string is not valid hex.
  /// Throws [ArgumentError] if the decoded bytes are not 8 bytes.
  factory PeerId.fromHex(String hex) {
    final cleanHex = hex.toLowerCase().replaceAll(RegExp(r'[^0-9a-f]'), '');
    if (cleanHex.length != peerIdSize * 2) {
      throw ArgumentError(
        'Hex string must represent $peerIdSize bytes (${peerIdSize * 2} hex chars), '
        'got ${cleanHex.length ~/ 2} bytes',
      );
    }

    final bytes = Uint8List(peerIdSize);
    for (var i = 0; i < peerIdSize; i++) {
      bytes[i] = int.parse(cleanHex.substring(i * 2, i * 2 + 2), radix: 16);
    }
    return PeerId(bytes);
  }

  /// Creates a PeerId from the first 8 bytes of a fingerprint.
  ///
  /// Throws [ArgumentError] if fingerprint is less than 8 bytes.
  factory PeerId.fromFingerprint(Uint8List fingerprint) {
    if (fingerprint.length < peerIdSize) {
      throw ArgumentError(
        'Fingerprint must be at least $peerIdSize bytes',
      );
    }
    return PeerId(Uint8List.fromList(fingerprint.sublist(0, peerIdSize)));
  }

  /// The broadcast PeerId (all 0xFF bytes).
  ///
  /// Used as recipient ID to indicate a broadcast message.
  static final PeerId broadcast = PeerId(
    Uint8List.fromList(List.filled(peerIdSize, 0xFF)),
  );

  /// Returns the hex string representation (lowercase).
  String toHex() {
    return bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
  }

  /// Returns a shortened display string (first 4 bytes as hex).
  String toShortString() {
    return bytes.sublist(0, 4).map((b) => b.toRadixString(16).padLeft(2, '0')).join();
  }

  /// Whether this is the broadcast PeerId.
  bool get isBroadcast {
    for (final b in bytes) {
      if (b != 0xFF) return false;
    }
    return true;
  }

  @override
  bool operator ==(Object other) {
    if (identical(this, other)) return true;
    if (other is! PeerId) return false;
    for (var i = 0; i < peerIdSize; i++) {
      if (bytes[i] != other.bytes[i]) return false;
    }
    return true;
  }

  @override
  int get hashCode {
    // Use first 4 bytes as hash
    return bytes[0] |
        (bytes[1] << 8) |
        (bytes[2] << 16) |
        (bytes[3] << 24);
  }

  @override
  String toString() => 'PeerId(${ toHex()})';
}
