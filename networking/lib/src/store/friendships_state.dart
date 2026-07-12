import 'package:flutter/foundation.dart';

/// Status of a friendship
enum FriendshipStatus {
  /// Friend request sent, waiting for acceptance
  pending,

  /// Friend request received, waiting for user action
  received,

  /// Friendship established
  accepted,

  /// Friend request declined
  declined,
}

/// Immutable friendship state for Redux store
@immutable
class FriendshipState {
  /// The friend's public key in hex format
  final String peerPubkeyHex;

  /// The friend's UDP address (if known)
  final String? udpAddress;

  /// The friend's nickname (if known)
  final String? nickname;

  /// The current status of the friendship
  final FriendshipStatus status;

  /// When the friend request was sent/received
  final DateTime createdAt;

  /// When the friendship was last updated
  final DateTime updatedAt;

  /// Optional message sent with the friend request
  final String? message;

  /// Rendezvous servers this friend uses, learned via the RV_LIST signaling
  /// exchange. Keyed by lowercase RV pubkey hex; value is the "ip:port"
  /// address. Stable, friendship-scoped data — persisted alongside the rest
  /// of the friendship record so it survives app restart and is available
  /// before the friend's first reconnect (avoids a `target RV count=0`
  /// window where AVAILABLE fan-out has nowhere to go).
  final Map<String, String> knownRvServers;

  const FriendshipState({
    required this.peerPubkeyHex,
    this.udpAddress,
    this.nickname,
    required this.status,
    required this.createdAt,
    required this.updatedAt,
    this.message,
    this.knownRvServers = const {},
  });

  /// Whether this is an established friendship
  bool get isAccepted => status == FriendshipStatus.accepted;

  /// Whether this is a pending friend request we sent
  bool get isPendingOutgoing => status == FriendshipStatus.pending;

  /// Whether this is a pending friend request we received
  bool get isPendingIncoming => status == FriendshipStatus.received;

  /// Display name for the friend
  String get displayName =>
      nickname?.isNotEmpty == true ? nickname! : 'Peer ${peerPubkeyHex.substring(0, 8)}...';

  FriendshipState copyWith({
    String? udpAddress,
    String? nickname,
    FriendshipStatus? status,
    String? message,
    DateTime? updatedAt,
    Map<String, String>? knownRvServers,
  }) {
    return FriendshipState(
      peerPubkeyHex: peerPubkeyHex,
      udpAddress: udpAddress ?? this.udpAddress,
      nickname: nickname ?? this.nickname,
      status: status ?? this.status,
      createdAt: createdAt,
      updatedAt: updatedAt ?? DateTime.now(),
      message: message ?? this.message,
      knownRvServers: knownRvServers ?? this.knownRvServers,
    );
  }

  Map<String, dynamic> toJson() => {
        'peerPubkeyHex': peerPubkeyHex,
        'udpAddress': udpAddress,
        'nickname': nickname,
        'status': status.index,
        'createdAt': createdAt.toIso8601String(),
        'updatedAt': updatedAt.toIso8601String(),
        'message': message,
        if (knownRvServers.isNotEmpty) 'knownRvServers': knownRvServers,
      };

  factory FriendshipState.fromJson(Map<String, dynamic> json) {
    final rawRvServers = json['knownRvServers'] as Map<String, dynamic>?;
    final knownRvServers = rawRvServers == null
        ? const <String, String>{}
        : {for (final e in rawRvServers.entries) e.key: e.value as String};
    return FriendshipState(
      peerPubkeyHex: json['peerPubkeyHex'] as String,
      udpAddress: json['udpAddress'] as String?,
      nickname: json['nickname'] as String?,
      status: FriendshipStatus.values[json['status'] as int],
      createdAt: DateTime.parse(json['createdAt'] as String),
      updatedAt: DateTime.parse(json['updatedAt'] as String),
      message: json['message'] as String?,
      knownRvServers: knownRvServers,
    );
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is FriendshipState &&
          runtimeType == other.runtimeType &&
          peerPubkeyHex == other.peerPubkeyHex &&
          udpAddress == other.udpAddress &&
          nickname == other.nickname &&
          status == other.status &&
          message == other.message &&
          mapEquals(knownRvServers, other.knownRvServers);

  @override
  int get hashCode => Object.hash(
        peerPubkeyHex,
        udpAddress,
        nickname,
        status,
        message,
        Object.hashAllUnordered(knownRvServers.entries
            .map((e) => Object.hash(e.key, e.value))),
      );

  @override
  String toString() =>
      'FriendshipState($peerPubkeyHex, status: $status, nickname: $nickname)';
}

/// Friendships state for Redux store
@immutable
class FriendshipsState {
  /// All friendships keyed by peerPubkeyHex
  final Map<String, FriendshipState> friendships;

  const FriendshipsState({this.friendships = const {}});

  static const FriendshipsState initial = FriendshipsState();

  // ===== Getters =====

  /// All friendships as list
  List<FriendshipState> get all => friendships.values.toList();

  /// Accepted friends only
  List<FriendshipState> get friends =>
      friendships.values.where((f) => f.isAccepted).toList();

  /// Pending incoming friend requests
  List<FriendshipState> get pendingIncoming =>
      friendships.values.where((f) => f.isPendingIncoming).toList();

  /// Pending outgoing friend requests
  List<FriendshipState> get pendingOutgoing =>
      friendships.values.where((f) => f.isPendingOutgoing).toList();

  /// Get friendship by peer public key hex
  FriendshipState? getFriendship(String peerPubkeyHex) =>
      friendships[peerPubkeyHex];

  /// Check if peer is an accepted friend
  bool isFriend(String peerPubkeyHex) =>
      friendships[peerPubkeyHex]?.isAccepted ?? false;

  /// Check if there's a pending request with this peer
  bool hasPendingRequest(String peerPubkeyHex) {
    final f = friendships[peerPubkeyHex];
    return f != null && (f.isPendingIncoming || f.isPendingOutgoing);
  }

  /// Get all friend public key hexes
  List<String> get friendPubkeyHexes =>
      friends.map((f) => f.peerPubkeyHex).toList();

  /// Get all friend UDP addresses
  List<String> get friendUdpAddresses => friends
      .where((f) => f.udpAddress != null)
      .map((f) => f.udpAddress!)
      .toList();

  /// Rendezvous servers advertised by accepted friends via RV_LIST.
  ///
  /// Keyed by lowercase rendezvous pubkey hex; value is the advertised
  /// "ip:port" address. These servers are trusted only as reconnect
  /// facilitators because a friend explicitly told us to use them.
  Map<String, String> get friendRvServers {
    final servers = <String, String>{};
    for (final friend in friends) {
      for (final entry in friend.knownRvServers.entries) {
        final hex = entry.key.toLowerCase();
        final address = entry.value.trim();
        if (hex.isEmpty || address.isEmpty) continue;
        servers[hex] = address;
      }
    }
    return Map.unmodifiable(servers);
  }

  // ===== Copy With =====

  FriendshipsState copyWith({
    Map<String, FriendshipState>? friendships,
  }) {
    return FriendshipsState(
      friendships: friendships ?? this.friendships,
    );
  }

  // ===== Persistence =====

  Map<String, dynamic> toJson() => {
        'friendships': friendships.map(
          (key, value) => MapEntry(key, value.toJson()),
        ),
      };

  factory FriendshipsState.fromJson(Map<String, dynamic> json) {
    final friendshipsJson = json['friendships'] as Map<String, dynamic>?;
    if (friendshipsJson == null) return const FriendshipsState();

    final friendships = friendshipsJson.map(
      (key, value) => MapEntry(
        key,
        FriendshipState.fromJson(value as Map<String, dynamic>),
      ),
    );
    return FriendshipsState(friendships: friendships);
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is FriendshipsState &&
          runtimeType == other.runtimeType &&
          mapEquals(friendships, other.friendships);

  @override
  int get hashCode => friendships.length.hashCode;
}
