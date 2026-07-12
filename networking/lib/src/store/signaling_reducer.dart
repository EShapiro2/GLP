import 'signaling_actions.dart';
import 'signaling_state.dart';

/// Reducer for signaling state.
SignalingState signalingReducer(SignalingState state, SignalingAction action) {
  if (action is HolePunchStartedAction) {
    final updated = Map<String, HolePunchStatus>.from(state.holePunchAttempts);
    updated[action.targetPubkeyHex] = HolePunchStatus.requested;
    return state.copyWith(holePunchAttempts: updated);
  }

  if (action is HolePunchPunchingAction) {
    final updated = Map<String, HolePunchStatus>.from(state.holePunchAttempts);
    updated[action.targetPubkeyHex] = HolePunchStatus.punching;
    return state.copyWith(holePunchAttempts: updated);
  }

  if (action is HolePunchSucceededAction) {
    final updated = Map<String, HolePunchStatus>.from(state.holePunchAttempts);
    updated[action.targetPubkeyHex] = HolePunchStatus.succeeded;
    return state.copyWith(holePunchAttempts: updated);
  }

  if (action is HolePunchFailedAction) {
    final updated = Map<String, HolePunchStatus>.from(state.holePunchAttempts);
    updated[action.targetPubkeyHex] = HolePunchStatus.failed;
    return state.copyWith(holePunchAttempts: updated);
  }

  return state;
}
