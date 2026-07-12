import 'package:flutter/material.dart';
import 'package:grassroots_networking_core/src/models/peer.dart';

/// UI adornments for transports — Flutter-side only; the core keeps
/// transports free of widget types.
extension PeerTransportIcon on PeerTransport {
  /// Icon shown next to a peer for this transport type.
  Icon get icon {
    switch (this) {
      case PeerTransport.bleDirect:
        return const Icon(Icons.bluetooth_connected,
            size: 16, color: Colors.blue);
      case PeerTransport.webrtc:
        return const Icon(Icons.public, size: 16, color: Colors.blue);
      case PeerTransport.udp:
        return const Icon(Icons.public, size: 16, color: Colors.green);
    }
  }
}
