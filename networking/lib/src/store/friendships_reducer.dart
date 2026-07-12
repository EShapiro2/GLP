import 'package:flutter/foundation.dart';

import 'friendships_state.dart';
import 'friendships_actions.dart';

/// Reducer for friendships state
FriendshipsState friendshipsReducer(FriendshipsState state, FriendshipAction action) {
  if (action is CreateFriendRequestAction) {
    final now = DateTime.now();
    final friendship = FriendshipState(
      peerPubkeyHex: action.peerPubkeyHex,
      nickname: action.nickname,
      status: FriendshipStatus.pending,
      message: action.message,
      createdAt: now,
      updatedAt: now,
    );
    return state.copyWith(
      friendships: Map.from(state.friendships)..[action.peerPubkeyHex] = friendship,
    );
  }

  if (action is ReceiveFriendRequestAction) {
    final existing = state.friendships[action.peerPubkeyHex];
    final now = DateTime.now();

    // Auto-accept if we already have pending outgoing (mutual friend requests)
    if (existing != null && existing.isPendingOutgoing) {
      final friendship = existing.copyWith(
        status: FriendshipStatus.accepted,
        nickname: action.nickname ?? existing.nickname,
        updatedAt: now,
      );
      return state.copyWith(
        friendships: Map.from(state.friendships)..[action.peerPubkeyHex] = friendship,
      );
    }

    // Already friends - just update nickname
    if (existing != null && existing.isAccepted) {
      final friendship = existing.copyWith(
        nickname: action.nickname ?? existing.nickname,
        updatedAt: now,
      );
      return state.copyWith(
        friendships: Map.from(state.friendships)..[action.peerPubkeyHex] = friendship,
      );
    }

    // New incoming request
    final friendship = FriendshipState(
      peerPubkeyHex: action.peerPubkeyHex,
      nickname: action.nickname,
      status: FriendshipStatus.received,
      message: action.message,
      createdAt: now,
      updatedAt: now,
    );
    return state.copyWith(
      friendships: Map.from(state.friendships)..[action.peerPubkeyHex] = friendship,
    );
  }

  if (action is AcceptFriendRequestAction) {
    final existing = state.friendships[action.peerPubkeyHex];
    if (existing == null || !existing.isPendingIncoming) return state;

    final friendship = existing.copyWith(
      status: FriendshipStatus.accepted,
      updatedAt: DateTime.now(),
    );
    return state.copyWith(
      friendships: Map.from(state.friendships)..[action.peerPubkeyHex] = friendship,
    );
  }

  if (action is ProcessFriendshipAcceptAction) {
    final existing = state.friendships[action.peerPubkeyHex];
    final now = DateTime.now();

    if (existing == null) {
      // Strange case - they accepted but we never sent request
      // Create a new accepted friendship
      final friendship = FriendshipState(
        peerPubkeyHex: action.peerPubkeyHex,
        nickname: action.nickname,
        status: FriendshipStatus.accepted,
        createdAt: now,
        updatedAt: now,
      );
      return state.copyWith(
        friendships: Map.from(state.friendships)..[action.peerPubkeyHex] = friendship,
      );
    }

    final friendship = existing.copyWith(
      status: FriendshipStatus.accepted,
      nickname: action.nickname ?? existing.nickname,
      updatedAt: now,
    );
    return state.copyWith(
      friendships: Map.from(state.friendships)..[action.peerPubkeyHex] = friendship,
    );
  }

  if (action is DeclineFriendRequestAction) {
    final existing = state.friendships[action.peerPubkeyHex];
    if (existing == null || !existing.isPendingIncoming) return state;

    final friendship = existing.copyWith(
      status: FriendshipStatus.declined,
      updatedAt: DateTime.now(),
    );
    return state.copyWith(
      friendships: Map.from(state.friendships)..[action.peerPubkeyHex] = friendship,
    );
  }

  if (action is RemoveFriendshipAction) {
    final newMap = Map<String, FriendshipState>.from(state.friendships);
    newMap.remove(action.peerPubkeyHex);
    return state.copyWith(friendships: newMap);
  }

  if (action is HandleUnfriendedByAction) {
    final newMap = Map<String, FriendshipState>.from(state.friendships);
    newMap.remove(action.peerPubkeyHex);
    return state.copyWith(friendships: newMap);
  }

  if (action is HydrateFriendshipsAction) {
    return state.copyWith(friendships: action.friendships);
  }

  if (action is UpdateFriendshipUdpAddressAction) {
    final existing = state.friendships[action.peerPubkeyHex];
    if (existing == null) return state;

    // A null action address preserves the stored one (never clear
    // unilaterally); an unchanged address is a no-op so it doesn't churn
    // updatedAt or trigger a persistence write.
    final newAddress = action.udpAddress ?? existing.udpAddress;
    if (newAddress == existing.udpAddress) return state;

    final friendship = existing.copyWith(
      udpAddress: newAddress,
      updatedAt: DateTime.now(),
    );
    return state.copyWith(
      friendships: Map.from(state.friendships)..[action.peerPubkeyHex] = friendship,
    );
  }

  if (action is FriendKnownRvServersUpdatedAction) {
    final pubkeyHex = action.publicKey
        .map((b) => b.toRadixString(16).padLeft(2, '0'))
        .join();
    final existing = state.friendships[pubkeyHex];
    // We only ever receive an RV_LIST from an accepted friend, so a missing
    // friendship record means the sender isn't (or no longer is) one of our
    // friends; drop the update silently.
    if (existing == null || !existing.isAccepted) return state;

    final normalized = <String, String>{
      for (final entry in action.rvServers.entries)
        entry.key.toLowerCase(): entry.value,
    };
    if (mapEquals(existing.knownRvServers, normalized)) return state;

    final friendship = existing.copyWith(
      knownRvServers: normalized,
      updatedAt: DateTime.now(),
    );
    return state.copyWith(
      friendships: Map.from(state.friendships)..[pubkeyHex] = friendship,
    );
  }

  return state;
}
