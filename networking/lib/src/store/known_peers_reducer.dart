import 'known_peers_actions.dart';
import 'known_peers_state.dart';

/// Reducer for the known-peers slice (API-supplied keys + dial addresses).
KnownPeersState knownPeersReducer(KnownPeersState state, dynamic action) {
  if (action is KnownPeerPutAction) {
    final hex = action.pubkeyHex.toLowerCase();
    if (state.known.containsKey(hex)) return state;
    return state.copyWith(
      known: Map<String, String?>.from(state.known)..[hex] = null,
    );
  }

  if (action is KnownPeerRemovedAction) {
    final hex = action.pubkeyHex.toLowerCase();
    if (!state.known.containsKey(hex)) return state;
    return state.copyWith(
      known: Map<String, String?>.from(state.known)..remove(hex),
    );
  }

  if (action is KnownPeerAddressUpdatedAction) {
    final hex = action.pubkeyHex.toLowerCase();
    if (action.udpAddress.isEmpty) return state;
    if (state.known[hex] == action.udpAddress) return state;
    return state.copyWith(
      known: Map<String, String?>.from(state.known)..[hex] = action.udpAddress,
    );
  }

  if (action is KnownPeersLoadedAction) {
    return KnownPeersState(
      known: {
        for (final e in action.known.entries) e.key.toLowerCase(): e.value,
      },
    );
  }

  return state;
}
