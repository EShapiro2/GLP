/// Connection states for the Grassroots BLE protocol.
///
/// Each BLE connection follows a state machine from idle through
/// established, with possible disconnection and retry.
library;

/// Enumeration of connection states.
enum ConnectionState {
  /// No connection; scanning or advertising.
  idle,

  /// BLE GATT connection in progress.
  connecting,

  /// Noise XX handshake in progress.
  handshaking,

  /// Secure session active; can exchange messages.
  established,

  /// Connection lost; may retry.
  disconnected;

  /// Whether the connection is active (connected or handshaking).
  bool get isActive =>
      this == ConnectionState.connecting ||
      this == ConnectionState.handshaking ||
      this == ConnectionState.established;

  /// Whether messages can be sent.
  bool get canSendMessages => this == ConnectionState.established;

  /// Whether the connection can transition to the given state.
  bool canTransitionTo(ConnectionState target) {
    switch (this) {
      case ConnectionState.idle:
        return target == ConnectionState.connecting;
      case ConnectionState.connecting:
        return target == ConnectionState.handshaking ||
            target == ConnectionState.disconnected;
      case ConnectionState.handshaking:
        return target == ConnectionState.established ||
            target == ConnectionState.disconnected;
      case ConnectionState.established:
        return target == ConnectionState.disconnected;
      case ConnectionState.disconnected:
        return target == ConnectionState.idle;
    }
  }
}

/// Friendship states for tracking relationship with a peer.
enum FriendshipState {
  /// Unknown peer (stranger).
  stranger,

  /// Friend request sent, awaiting response.
  pending,

  /// Friendship established.
  friend,

  /// Friend request was rejected.
  rejected;

  /// Whether messages can be exchanged.
  bool get canMessage => this == FriendshipState.friend;
}

/// Message delivery status.
enum MessageStatus {
  /// Message is queued but not yet sent.
  pending(0),

  /// Message has been sent but not acknowledged.
  sent(1),

  /// Message delivery has been acknowledged.
  delivered(2),

  /// Message has been read by recipient.
  read(3);

  /// The numeric value for database storage.
  final int value;

  const MessageStatus(this.value);

  /// Returns the [MessageStatus] for the given numeric value.
  static MessageStatus fromValue(int value) {
    for (final status in MessageStatus.values) {
      if (status.value == value) {
        return status;
      }
    }
    throw ArgumentError('Invalid message status: $value');
  }
}
