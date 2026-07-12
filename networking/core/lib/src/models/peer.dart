import '../transport/transport_service.dart';

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

/// Transport type for peer communication
enum PeerTransport {
  /// Direct BLE connection (Central or Peripheral role)
  bleDirect,

  /// WebRTC P2P connection
  webrtc,

  /// UDP P2P connection
  udp,
}

/// Extension to get display info for peer transport
/// (the UI icon lives in the Flutter package: `transport_display.dart`)
extension PeerTransportDisplay on PeerTransport {
  /// Get the display name for this transport
  String get displayName {
    switch (this) {
      case PeerTransport.bleDirect:
        return 'Bluetooth';
      case PeerTransport.webrtc:
        return 'WebRTC';
      case PeerTransport.udp:
        return 'UDP';
    }
  }

  /// Convert from TransportType
  static PeerTransport fromTransportType(TransportType type,
      {bool isMesh = false}) {
    switch (type) {
      case TransportType.ble:
        return PeerTransport.bleDirect;
      case TransportType.webrtc:
        return PeerTransport.webrtc;
      case TransportType.udp:
        return PeerTransport.udp;
    }
  }
}
