/// The underlays of the layer (GNA §Networking API): "An `Underlay` is one of
/// `PAN`, `LAN` or `WAN`; a peer may be reachable over more than one underlay
/// at a time."
///
/// The underlay is the scope a session runs at, not its carrier.  A GATT
/// session is [Underlay.pan]; a UDP session is [Underlay.lan] when the peer's
/// address lies within one of the agent's own local prefixes and
/// [Underlay.wan] otherwise --- "A LAN connection and a WAN connection are both
/// IP connections; the layer tells them apart by whether the peer's address is
/// on the agent's own local network" (§Networking API), by the test the LAN
/// trust level uses (§Cold Calls, Trust levels).
library;

/// One of the three underlays: Bluetooth Low Energy in radio range (GNA
/// §PAN), the local network (§LAN), or the Internet at large (§WAN Underlay).
enum Underlay {
  /// Bluetooth Low Energy: a GATT session.
  pan,

  /// The local network: a UDP session with a peer whose address lies within
  /// one of the agent's own local prefixes.
  lan,

  /// The Internet at large: any other UDP session.
  wan,
}

/// A proximity underlay, on which a nearby agent is discovered (GNA
/// §Networking API: "The proximity underlays are PAN and LAN ...; a
/// ProximityUnderlay is one of the two").  A cold-call trust level is held per
/// proximity underlay and set with `setTrustLevel(underlay, level)` (§Cold
/// Calls, Trust levels); discovery reports the proximity underlay a nearby
/// agent was found on (§Concepts and Principles, Discovery).
enum ProximityUnderlay {
  /// Discovered by BLE advertisement.
  pan,

  /// Discovered by DNS-SD on the local network.
  lan,
}
