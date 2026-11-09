import 'underlay.dart';

/// Connection state of a peer
enum PeerConnectionState {
  /// Discovered via BLE scan but not connected
  discovered,

  /// BLE connection established, awaiting ANNOUNCE
  connecting,

  /// ANNOUNCE exchanged, peer identity known
  connected,

  /// Connection lost, may still have cached identity
  disconnected,
}

/// The session a packet travels on, one per underlay (spec §Session
/// Establishment: "Sessions are per underlay: one handshake per underlay per
/// pair, so a pair reachable over more than one underlay holds a session on
/// each").  Sessions, their handshakes and their message lanes are keyed by
/// this together with the peer's key.
///
/// [lan] and [wan] share one carrier, UDP on the same sockets; which of the
/// two a UDP path's session belongs to is decided by the path's address when
/// the path is opened (decision 8: [lan] when the layer runs the LAN and the
/// address lies within one of the agent's own local prefixes, else [wan]
/// when it runs the WAN, else no session).
enum PeerTransport {
  /// The GATT session, over a direct BLE connection (Central or Peripheral
  /// role): the PAN.
  bleDirect,

  /// A UDP session on the local network: the LAN.
  lan,

  /// A UDP session with the Internet at large: the WAN.
  wan,
}

/// Extension to get display info for peer transport
/// (the UI icon lives in the Flutter package: `transport_display.dart`)
extension PeerTransportDisplay on PeerTransport {
  /// Get the display name for this transport
  String get displayName {
    switch (this) {
      case PeerTransport.bleDirect:
        return 'Bluetooth';
      case PeerTransport.lan:
        return 'Local network';
      case PeerTransport.wan:
        return 'Internet';
    }
  }

  /// Whether this session rides the UDP carrier: [PeerTransport.lan] or
  /// [PeerTransport.wan].
  bool get isIp => this != PeerTransport.bleDirect;

  /// The underlay this session runs on.
  Underlay get underlay => switch (this) {
        PeerTransport.bleDirect => Underlay.pan,
        PeerTransport.lan => Underlay.lan,
        PeerTransport.wan => Underlay.wan,
      };
}

/// The session of each underlay.
extension UnderlaySession on Underlay {
  /// The session that runs on this underlay.
  PeerTransport get session => switch (this) {
        Underlay.pan => PeerTransport.bleDirect,
        Underlay.lan => PeerTransport.lan,
        Underlay.wan => PeerTransport.wan,
      };
}

/// The IP sessions, in the order `send` prefers them: the LAN before the WAN.
const List<PeerTransport> ipSessions = [PeerTransport.lan, PeerTransport.wan];
