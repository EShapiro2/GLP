/// Base class for known-peer actions.
///
/// Known peers are keys GLP supplied via the layer's API (`putKnownPeer`,
/// `putPeerAddress`) — the layer's recognition set and dial book. See
/// [KnownPeersState].
abstract class KnownPeerAction {}

/// GLP supplied a key: add it to the known set (a no-op when already known —
/// an existing dial address is preserved).
class KnownPeerPutAction extends KnownPeerAction {
  final String pubkeyHex;

  KnownPeerPutAction(this.pubkeyHex);
}

/// GLP withdrew a key: remove it from the known set (and with it the stored
/// dial address).
class KnownPeerRemovedAction extends KnownPeerAction {
  final String pubkeyHex;

  KnownPeerRemovedAction(this.pubkeyHex);
}

/// A known peer's dial address was supplied (putPeerAddress) or observed on
/// a live session (address mirror). Creates the entry when the key is not
/// yet known — putPeerAddress creates a dial-book entry even for a peer
/// never seen before.
class KnownPeerAddressUpdatedAction extends KnownPeerAction {
  final String pubkeyHex;
  final String udpAddress;

  KnownPeerAddressUpdatedAction({
    required this.pubkeyHex,
    required this.udpAddress,
  });
}

/// Replace the whole slice with a persisted snapshot (startup hydration).
class KnownPeersLoadedAction extends KnownPeerAction {
  final Map<String, String?> known;

  KnownPeersLoadedAction(this.known);
}
