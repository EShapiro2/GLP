import 'app_state.dart';
import 'peers_actions.dart';
import 'peers_reducer.dart';
import 'messages_actions.dart';
import 'messages_reducer.dart';
import 'known_peers_actions.dart';
import 'known_peers_reducer.dart';
import 'local_network_actions.dart';
import 'local_network_reducer.dart';
import 'settings_actions.dart';
import 'settings_reducer.dart';
import 'signaling_actions.dart';
import 'signaling_reducer.dart';
import 'transports_actions.dart';
import 'transports_reducer.dart';

/// Root reducer that handles all actions
AppState appReducer(AppState state, dynamic action) {
  // Handle peer actions via peersReducer
  if (action is PeerAction) {
    return state.copyWith(
      peers: peersReducer(state.peers, action),
    );
  }

  // Handle message actions via messagesReducer
  if (action is MessageAction) {
    return state.copyWith(
      messages: messagesReducer(state.messages, action),
    );
  }

  // Handle known-peer actions via knownPeersReducer
  if (action is KnownPeerAction) {
    return state.copyWith(
      knownPeers: knownPeersReducer(state.knownPeers, action),
    );
  }

  // Handle settings actions via settingsReducer
  if (action is SettingsAction) {
    return state.copyWith(
      settings: settingsReducer(state.settings, action),
    );
  }

  // Handle transport state actions via transportsReducer
  if (action is TransportAction) {
    return state.copyWith(
      transports: transportsReducer(state.transports, action),
    );
  }

  // Handle local-network actions via localNetworkReducer
  if (action is LocalNetworkAction) {
    return state.copyWith(
      localNetwork: localNetworkReducer(state.localNetwork, action),
    );
  }

  // Handle signaling actions via signalingReducer
  if (action is SignalingAction) {
    return state.copyWith(
      signaling: signalingReducer(state.signaling, action),
    );
  }

  return state;
}
