import 'dart:async';
import 'dart:convert';
import 'package:flutter/foundation.dart';
import 'package:shared_preferences/shared_preferences.dart';

import 'app_state.dart';
import 'friendships_state.dart';
import 'settings_state.dart';
import 'messages_state.dart';

/// Service for persisting Redux state to SharedPreferences
class PersistenceService {
  static const String _friendshipsKey = 'grassroots_friendships_v2';
  static const String _settingsKey = 'grassroots_settings_v2';
  static const String _conversationsKey = 'grassroots_conversations_v2';
  static const String _unreadCountsKey = 'grassroots_unread_counts_v2';

  /// Debounce timer for batching writes
  Timer? _debounceTimer;
  static const Duration _debounceDelay = Duration(milliseconds: 500);

  /// Last state that was persisted (to avoid unnecessary writes)
  AppState? _lastPersistedState;

  /// Pending persistence flags
  bool _pendingFriendships = false;
  bool _pendingSettings = false;
  bool _pendingConversations = false;

  SharedPreferences? _prefs;

  Future<SharedPreferences> get _preferences async {
    _prefs ??= await SharedPreferences.getInstance();
    return _prefs!;
  }

  // ===== Load Methods =====

  /// Load friendships from storage
  Future<FriendshipsState> loadFriendships() async {
    final prefs = await _preferences;
    final data = prefs.getString(_friendshipsKey);

    if (data == null) return const FriendshipsState();

    try {
      return FriendshipsState.fromJson(jsonDecode(data) as Map<String, dynamic>);
    } catch (e) {
      debugPrint('Failed to load friendships: $e');
      return const FriendshipsState();
    }
  }

  /// Load settings from storage
  Future<SettingsState> loadSettings() async {
    final prefs = await _preferences;
    final data = prefs.getString(_settingsKey);

    if (data == null) return const SettingsState();

    try {
      return SettingsState.fromJson(jsonDecode(data) as Map<String, dynamic>);
    } catch (e) {
      debugPrint('Failed to load settings: $e');
      return const SettingsState();
    }
  }

  /// Load conversations from storage
  Future<(Map<String, List<ChatMessageState>>, Map<String, int>)>
      loadConversations() async {
    final prefs = await _preferences;

    Map<String, List<ChatMessageState>> conversations = {};
    Map<String, int> unreadCounts = {};

    // Load conversations
    final convData = prefs.getString(_conversationsKey);
    if (convData != null) {
      try {
        final json = jsonDecode(convData) as Map<String, dynamic>;
        conversations = json.map((key, value) => MapEntry(
              key,
              (value as List<dynamic>)
                  .map((m) =>
                      ChatMessageState.fromJson(m as Map<String, dynamic>))
                  .toList(),
            ));
      } catch (e) {
        debugPrint('Failed to load conversations: $e');
      }
    }

    // Load unread counts
    final unreadData = prefs.getString(_unreadCountsKey);
    if (unreadData != null) {
      try {
        final json = jsonDecode(unreadData) as Map<String, dynamic>;
        unreadCounts = json.map((key, value) => MapEntry(key, value as int));
      } catch (e) {
        debugPrint('Failed to load unread counts: $e');
      }
    }

    return (conversations, unreadCounts);
  }

  // ===== Save Methods =====

  /// Called when state changes - schedules debounced persistence
  void onStateChanged(AppState state) {
    // Check what changed
    if (_lastPersistedState == null ||
        state.friendships != _lastPersistedState!.friendships) {
      _pendingFriendships = true;
    }
    if (_lastPersistedState == null ||
        state.settings != _lastPersistedState!.settings) {
      _pendingSettings = true;
    }
    if (_lastPersistedState == null ||
        state.messages.conversations != _lastPersistedState!.messages.conversations ||
        state.messages.unreadCounts != _lastPersistedState!.messages.unreadCounts) {
      _pendingConversations = true;
    }

    // Schedule debounced write
    if (_pendingFriendships || _pendingSettings || _pendingConversations) {
      _debounceTimer?.cancel();
      _debounceTimer = Timer(_debounceDelay, () => _persistState(state));
    }
  }

  /// Actually persist the state to storage
  Future<void> _persistState(AppState state) async {
    final prefs = await _preferences;

    if (_pendingFriendships) {
      try {
        await prefs.setString(
          _friendshipsKey,
          jsonEncode(state.friendships.toJson()),
        );
        _pendingFriendships = false;
        debugPrint('Persisted ${state.friendships.friendships.length} friendships');
      } catch (e) {
        debugPrint('Failed to persist friendships: $e');
      }
    }

    if (_pendingSettings) {
      try {
        await prefs.setString(
          _settingsKey,
          jsonEncode(state.settings.toJson()),
        );
        _pendingSettings = false;
        debugPrint('Persisted settings');
      } catch (e) {
        debugPrint('Failed to persist settings: $e');
      }
    }

    if (_pendingConversations) {
      try {
        // Persist conversations
        final convJson = state.messages.conversations.map(
          (key, value) => MapEntry(key, value.map((m) => m.toJson()).toList()),
        );
        await prefs.setString(_conversationsKey, jsonEncode(convJson));

        // Persist unread counts
        await prefs.setString(
          _unreadCountsKey,
          jsonEncode(state.messages.unreadCounts),
        );

        _pendingConversations = false;
        debugPrint('Persisted ${state.messages.conversations.length} conversations');
      } catch (e) {
        debugPrint('Failed to persist conversations: $e');
      }
    }

    _lastPersistedState = state;
  }

  /// Force immediate persistence (call on app exit)
  Future<void> flush(AppState state) async {
    _debounceTimer?.cancel();
    _pendingFriendships = true;
    _pendingSettings = true;
    _pendingConversations = true;
    await _persistState(state);
  }

  /// Clean up resources
  void dispose() {
    _debounceTimer?.cancel();
  }
}
