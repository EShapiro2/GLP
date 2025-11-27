/// Friend model for the Grassroots BLE protocol.
///
/// Represents a peer in the user's friend list with their cryptographic
/// keys and metadata.
library;

import 'dart:typed_data';

import 'peer_id.dart';

/// A friend in the user's social graph.
class Friend {
  /// The friend's peer ID (truncated fingerprint).
  final PeerId peerId;

  /// The friend's Curve25519 public key (Noise static key).
  final Uint8List noisePublicKey;

  /// The friend's Ed25519 public key (signing key).
  final Uint8List signPublicKey;

  /// The friend's display name.
  final String displayName;

  /// When this friend was added (milliseconds since epoch).
  final int addedAt;

  /// When this friend was last seen (milliseconds since epoch).
  final int lastSeen;

  /// Whether the friend's identity has been verified out-of-band.
  final bool isVerified;

  /// Creates a new friend.
  Friend({
    required this.peerId,
    required this.noisePublicKey,
    required this.signPublicKey,
    required this.displayName,
    int? addedAt,
    int? lastSeen,
    this.isVerified = false,
  })  : addedAt = addedAt ?? DateTime.now().millisecondsSinceEpoch,
        lastSeen = lastSeen ?? DateTime.now().millisecondsSinceEpoch {
    if (noisePublicKey.length != 32) {
      throw ArgumentError('noisePublicKey must be 32 bytes');
    }
    if (signPublicKey.length != 32) {
      throw ArgumentError('signPublicKey must be 32 bytes');
    }
  }

  /// Returns a copy with updated lastSeen timestamp.
  Friend touch() {
    return Friend(
      peerId: peerId,
      noisePublicKey: noisePublicKey,
      signPublicKey: signPublicKey,
      displayName: displayName,
      addedAt: addedAt,
      lastSeen: DateTime.now().millisecondsSinceEpoch,
      isVerified: isVerified,
    );
  }

  /// Returns a copy with verified status set.
  Friend verify() {
    return Friend(
      peerId: peerId,
      noisePublicKey: noisePublicKey,
      signPublicKey: signPublicKey,
      displayName: displayName,
      addedAt: addedAt,
      lastSeen: lastSeen,
      isVerified: true,
    );
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Friend &&
          runtimeType == other.runtimeType &&
          peerId == other.peerId;

  @override
  int get hashCode => peerId.hashCode;

  @override
  String toString() => 'Friend($displayName, ${peerId.toShortString()})';
}

/// A friend-of-friend in the extended social graph.
class FriendOfFriend {
  /// The peer ID of the friend-of-friend.
  final PeerId peerId;

  /// The peer ID of the mutual friend through whom we know them.
  final PeerId viaFriend;

  /// The Curve25519 public key (if known).
  final Uint8List? noisePublicKey;

  /// The display name (if known).
  final String? displayName;

  /// Creates a friend-of-friend entry.
  FriendOfFriend({
    required this.peerId,
    required this.viaFriend,
    this.noisePublicKey,
    this.displayName,
  });

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is FriendOfFriend &&
          runtimeType == other.runtimeType &&
          peerId == other.peerId &&
          viaFriend == other.viaFriend;

  @override
  int get hashCode => Object.hash(peerId, viaFriend);

  @override
  String toString() =>
      'FriendOfFriend(${displayName ?? peerId.toShortString()} via ${viaFriend.toShortString()})';
}
