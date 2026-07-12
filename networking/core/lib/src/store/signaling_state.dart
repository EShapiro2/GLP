import 'package:flutter/foundation.dart';

/// Status of a hole-punch attempt.
enum HolePunchStatus {
  /// PUNCH_REQUEST sent to a well-connected friend.
  requested,

  /// PUNCH_INITIATE received — actively sending punch packets.
  punching,

  /// Punch succeeded — direct UDP path established.
  succeeded,

  /// Punch timed out or failed.
  failed,
}

/// Redux state for signaling and hole-punch coordination.
@immutable
class SignalingState {
  /// Active hole-punch attempts.
  /// Key: target peer pubkey hex, value: current status.
  final Map<String, HolePunchStatus> holePunchAttempts;

  const SignalingState({
    this.holePunchAttempts = const {},
  });

  static const SignalingState initial = SignalingState();

  SignalingState copyWith({
    Map<String, HolePunchStatus>? holePunchAttempts,
  }) {
    return SignalingState(
      holePunchAttempts: holePunchAttempts ?? this.holePunchAttempts,
    );
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is SignalingState &&
          runtimeType == other.runtimeType &&
          mapEquals(holePunchAttempts, other.holePunchAttempts);

  @override
  int get hashCode => holePunchAttempts.length.hashCode;

  @override
  String toString() =>
      'SignalingState(punches: ${holePunchAttempts.length})';
}
