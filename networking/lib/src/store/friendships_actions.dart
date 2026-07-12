import 'dart:typed_data';

import 'friendships_state.dart';

/// Base class for friendship-related actions
abstract class FriendshipAction {}

/// Create a new outgoing friend request
class CreateFriendRequestAction extends FriendshipAction {
  final String peerPubkeyHex;
  final String? nickname;
  final String? message;

  CreateFriendRequestAction({
    required this.peerPubkeyHex,
    this.nickname,
    this.message,
  });
}

/// Record a received friend request
class ReceiveFriendRequestAction extends FriendshipAction {
  final String peerPubkeyHex;
  final String? nickname;
  final String? message;

  ReceiveFriendRequestAction({
    required this.peerPubkeyHex,
    this.nickname,
    this.message,
  });
}

/// Accept an incoming friend request
class AcceptFriendRequestAction extends FriendshipAction {
  final String peerPubkeyHex;

  AcceptFriendRequestAction(this.peerPubkeyHex);
}

/// Process a friendship accept message from the other party
class ProcessFriendshipAcceptAction extends FriendshipAction {
  final String peerPubkeyHex;
  final String? nickname;

  ProcessFriendshipAcceptAction({
    required this.peerPubkeyHex,
    this.nickname,
  });
}

/// Decline a friend request
class DeclineFriendRequestAction extends FriendshipAction {
  final String peerPubkeyHex;

  DeclineFriendRequestAction(this.peerPubkeyHex);
}

/// Remove a friendship (unfriend)
class RemoveFriendshipAction extends FriendshipAction {
  final String peerPubkeyHex;

  RemoveFriendshipAction(this.peerPubkeyHex);
}

/// Handle being unfriended by someone
class HandleUnfriendedByAction extends FriendshipAction {
  final String peerPubkeyHex;

  HandleUnfriendedByAction(this.peerPubkeyHex);
}

/// Hydrate friendships from persistence
class HydrateFriendshipsAction extends FriendshipAction {
  final Map<String, FriendshipState> friendships;

  HydrateFriendshipsAction(this.friendships);
}

/// Update friendship's UDP address (e.g., after reconnection)
class UpdateFriendshipUdpAddressAction extends FriendshipAction {
  final String peerPubkeyHex;
  final String? udpAddress;

  UpdateFriendshipUdpAddressAction({
    required this.peerPubkeyHex,
    this.udpAddress,
  });
}

/// Replace a friend's known rendezvous servers (learned via the RV_LIST
/// signaling exchange). Only friends ever receive an RV_LIST — strangers
/// don't — so this action is friendship-scoped by construction and the data
/// lives alongside the rest of the persisted friendship record.
class FriendKnownRvServersUpdatedAction extends FriendshipAction {
  /// Friend's pubkey (Ed25519 32-byte raw bytes).
  final Uint8List publicKey;

  /// Lowercase RV pubkey hex → "ip:port".
  final Map<String, String> rvServers;

  FriendKnownRvServersUpdatedAction({
    required this.publicKey,
    required this.rvServers,
  });
}
