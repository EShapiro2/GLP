/// Privacy levels for the Grassroots BLE protocol.
///
/// Users choose one of four privacy levels that determine their
/// advertising, scanning, and sharing behavior.
library;

/// Enumeration of privacy levels.
///
/// Each level includes all capabilities of previous levels.
enum PrivacyLevel {
  /// Level 1: Silent.
  ///
  /// - Does not advertise
  /// - Can scan for known friends' UUIDs only
  /// - Cannot be discovered by strangers
  /// - Does not relay messages
  silent(1),

  /// Level 2: Visible.
  ///
  /// - Advertises with derived Service UUID
  /// - Can be discovered by friends who know their PK
  /// - Participates in message relay for friends and friends-of-friends
  visible(2),

  /// Level 3: Open.
  ///
  /// - Advertises with Service UUID and display name
  /// - Scans for all BLE devices (not just friends)
  /// - Can be befriended by strangers in close proximity
  open(3),

  /// Level 4: Social.
  ///
  /// - Shares friend list (Bloom filter) upon connection
  /// - Enables introductions through mutual friends
  social(4);

  /// The numeric value for this privacy level.
  final int value;

  const PrivacyLevel(this.value);

  /// Returns the [PrivacyLevel] for the given numeric value.
  ///
  /// Throws [ArgumentError] if the value is not valid (1-4).
  static PrivacyLevel fromValue(int value) {
    for (final level in PrivacyLevel.values) {
      if (level.value == value) {
        return level;
      }
    }
    throw ArgumentError('Invalid privacy level: $value (must be 1-4)');
  }

  /// Whether this level advertises.
  bool get advertises => this != PrivacyLevel.silent;

  /// Whether this level scans for all devices (not just friends).
  bool get scansAll =>
      this == PrivacyLevel.open || this == PrivacyLevel.social;

  /// Whether this level shares the display name in advertisements.
  bool get sharesName =>
      this == PrivacyLevel.open || this == PrivacyLevel.social;

  /// Whether this level shares the friend list.
  bool get sharesFriends => this == PrivacyLevel.social;

  /// Whether this level relays messages.
  bool get relays => this != PrivacyLevel.silent;

  /// Whether this level can be befriended by strangers.
  bool get acceptsStrangers =>
      this == PrivacyLevel.open || this == PrivacyLevel.social;
}
