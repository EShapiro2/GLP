/// Base class for signaling actions.
abstract class SignalingAction {}

/// A hole-punch attempt was initiated.
class HolePunchStartedAction extends SignalingAction {
  final String targetPubkeyHex;
  HolePunchStartedAction(this.targetPubkeyHex);
}

/// We received PUNCH_INITIATE and started sending punch packets.
class HolePunchPunchingAction extends SignalingAction {
  final String targetPubkeyHex;
  HolePunchPunchingAction(this.targetPubkeyHex);
}

/// Hole-punch succeeded — direct UDP path established.
class HolePunchSucceededAction extends SignalingAction {
  final String targetPubkeyHex;
  final String ip;
  final int port;
  HolePunchSucceededAction(this.targetPubkeyHex, this.ip, this.port);
}

/// Hole-punch failed or timed out.
class HolePunchFailedAction extends SignalingAction {
  final String targetPubkeyHex;
  final String reason;
  HolePunchFailedAction(this.targetPubkeyHex, this.reason);
}
