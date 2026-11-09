import 'dart:io';

import 'dart:typed_data';

import '../platform/compat.dart';

/// A UDP hole-punch datagram.
///
/// Format: `[magic(4)]` = 4 bytes total, the magic alone and no payload.
///
/// The magic bytes identify this as a Grassroots punch datagram so receivers
/// can distinguish it from the layer's packets and other UDP traffic. A punch
/// carries no key: its only purpose is to create a NAT mapping, and the peer
/// is identified by the Noise handshake that follows.
class PunchPacket {
  /// Magic bytes: "GRPU" (Grassroots Punch UDP)
  static const List<int> magic = [0x47, 0x52, 0x50, 0x55];

  /// Expected total datagram size: the magic alone.
  static const int packetSize = 4;

  /// The wire form of a punch datagram.
  static Uint8List serialize() => Uint8List.fromList(magic);

  /// Whether a received datagram is a punch: exactly the magic.
  static bool isPunch(Uint8List data) {
    if (data.length != packetSize) return false;
    for (var i = 0; i < packetSize; i++) {
      if (data[i] != magic[i]) return false;
    }
    return true;
  }
}

/// Simple simultaneous UDP hole-punch service.
///
/// Sends punch packets to a target address at regular intervals to open
/// NAT mappings. Both peers must punch each other simultaneously for the
/// NAT traversal to succeed.
///
/// ## How it works
///
/// 1. Alice sends UDP punch packets to Bob's public ip:port.
/// 2. Each outgoing packet creates an outbound NAT mapping on Alice's router.
/// 3. Bob simultaneously sends punch packets to Alice's public ip:port.
/// 4. Once both NATs have mappings, packets can flow in both directions.
/// 5. Once the path is open, the peers establish their Noise session over it, IK
///    with the expected key (GNA §2.6).
///
/// [punch] is fire-and-forget: it sends packets for a fixed duration, and
/// both peers punch at once so that their bursts overlap.
///
/// ## Design for extensibility
///
/// This class handles simple cone-NAT punching. For symmetric NATs,
/// a birthday-paradox approach (sending to multiple predicted ports) can be
/// added as a subclass or strategy without changing the interface.
class HolePunchService {

  /// The raw UDP socket punch packets are sent from.
  final RawDatagramSocket socket;

  /// Whether this service has been disposed.
  bool _disposed = false;

  /// Serialized punch packet (same for all targets).
  final Uint8List _punchBytes = PunchPacket.serialize();

  HolePunchService({
    required this.socket,
  });

  /// Send punch packets for a fixed duration, fire-and-forget: the socket is
  /// the one a [UdpTransportService] reads, which drops punch packets on
  /// arrival.
  ///
  /// The caller should attempt [UdpTransportService.connectToPeer] after
  /// this completes.
  Future<void> punch(
    InternetAddress targetIp,
    int targetPort, {
    Duration duration = const Duration(seconds: 2),
    Duration interval = const Duration(milliseconds: 200),
  }) async {
    debugPrint('Punching ${targetIp.address}:$targetPort for ${duration.inMilliseconds}ms');

    final endTime = DateTime.now().add(duration);
    var sent = 0;

    while (!_disposed && DateTime.now().isBefore(endTime)) {
      socket.send(_punchBytes, targetIp, targetPort);
      sent++;
      await Future.delayed(interval);
    }

    debugPrint('Punch complete: sent $sent packets to ${targetIp.address}:$targetPort');
  }

  /// Cancel any in-progress punch operations.
  void dispose() {
    _disposed = true;
  }
}
