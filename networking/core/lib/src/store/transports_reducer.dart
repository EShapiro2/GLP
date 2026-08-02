import 'transports_state.dart';
import 'transports_actions.dart';

TransportsState transportsReducer(
  TransportsState state,
  TransportAction action,
) {
  if (action is BleTransportStateChangedAction) {
    return state.copyWith(bleState: action.state, bleError: action.error);
  }

  if (action is UdpTransportStateChangedAction) {
    return state.copyWith(udpState: action.state, udpError: action.error);
  }

  if (action is BleScanningChangedAction) {
    return state.copyWith(bleScanning: action.scanning);
  }

  if (action is PublicAddressUpdatedAction) {
    if (action.publicAddress == null) {
      return state.clearPublicAddress();
    }
    if (action.publicAddress == state.publicAddress) {
      return state;
    }
    // Address changed — invalidate the prior reachability observation, since
    // it was bound to the previous address/network path.
    return state.withNewPublicAddress(action.publicAddress!);
  }

  if (action is PublicIpUpdatedAction) {
    return state.copyWith(publicIp: action.publicIp);
  }

  if (action is ClearPublicConnectivityAction) {
    return state.clearPublicConnectivity();
  }

  if (action is NetworkConnectionTypeUpdatedAction) {
    return state.copyWith(networkConnectionType: action.connectionType);
  }

  if (action is UnsolicitedInboundObservedAction) {
    // Only meaningful if we have a public address to bind the observation to.
    if (state.publicAddress == null) return state;
    return state.copyWith(lastUnsolicitedInboundAt: action.observedAt);
  }

  return state;
}
