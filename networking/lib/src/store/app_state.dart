import 'package:flutter/foundation.dart';
import 'peers_state.dart';
import 'messages_state.dart';
import 'friendships_state.dart';
import 'settings_state.dart';
import 'signaling_state.dart';
import 'transports_state.dart';

/// Main application state for redux
@immutable
class AppState {
  /// Per-transport lifecycle state
  final TransportsState transports;

  /// Peers state (discovered and connected peers)
  final PeersState peers;

  /// Messages state (sent and received messages, conversations)
  final MessagesState messages;

  /// Friendships state (friends, pending requests)
  final FriendshipsState friendships;

  /// Settings state (transport settings)
  final SettingsState settings;

  /// Signaling state (address registration, hole-punch attempts)
  final SignalingState signaling;

  const AppState({
    this.transports = const TransportsState(),
    this.peers = const PeersState(),
    this.messages = const MessagesState(),
    this.friendships = const FriendshipsState(),
    this.settings = const SettingsState(),
    this.signaling = const SignalingState(),
  });

  /// Initial state
  static const AppState initial = AppState();

  // ===== Convenience getters that derive from peers state =====

  /// Number of nearby peers (discovered BLE devices)
  int get nearbyPeerCount => peers.discoveredBleCount;

  /// Number of connected peers (after ANNOUNCE)
  int get connectedPeerCount => peers.connectedCount;

  /// Number of online friends (friends connected via UDP only)
  int get onlineFriendsCount => peers.onlineFriends.length;

  // ===== Convenience getters derived from transports state =====

  /// Get display string for connection status
  String get statusDisplayString => transports.statusDisplayString;

  /// Whether status indicates a healthy/running state
  bool get isHealthy => transports.isHealthy;

  /// Create a copy with updated values
  AppState copyWith({
    TransportsState? transports,
    PeersState? peers,
    MessagesState? messages,
    FriendshipsState? friendships,
    SettingsState? settings,
    SignalingState? signaling,
  }) {
    return AppState(
      transports: transports ?? this.transports,
      peers: peers ?? this.peers,
      messages: messages ?? this.messages,
      friendships: friendships ?? this.friendships,
      settings: settings ?? this.settings,
      signaling: signaling ?? this.signaling,
    );
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is AppState &&
          runtimeType == other.runtimeType &&
          transports == other.transports &&
          peers == other.peers &&
          messages == other.messages &&
          friendships == other.friendships &&
          settings == other.settings &&
          signaling == other.signaling;

  @override
  int get hashCode => Object.hash(
        transports,
        peers,
        messages,
        friendships,
        settings,
        signaling,
      );
}
