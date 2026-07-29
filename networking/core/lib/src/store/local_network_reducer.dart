import 'local_network_actions.dart';
import 'local_network_state.dart';

LocalNetworkState localNetworkReducer(
  LocalNetworkState state,
  LocalNetworkAction action,
) {
  if (action is LocalNetworkChangedAction) {
    return state.copyWith(network: action.network);
  }
  return state;
}
