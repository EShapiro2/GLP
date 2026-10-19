import 'dart:io';

import 'dart:typed_data';

import '../platform/compat.dart';

/// A UDP hole-punch packet.
///
/// Format: `[magic(4)] [senderPubkey(32)]` = 36 bytes total.
///
/// The magic bytes identify this as a Grassroots punch packet so receivers
/// can distinguish it from the layer's packets and other UDP traffic.
///
/// Punch packets serve one purpose: to create a NAT mapping. The receiver doesn't need to process them — the act of sending opens our NAT.
class PunchPacket {
  /// Magic bytes: "GRPU" (Grassroots Punch UDP)
  static const List<int> magic = [0x47, 0x52, 0x50, 0x55];

  /// Expected total packet size
  static const int packetSize = 4 + 32;

  /// The sender's Ed25519 public key (32 bytes)
  final Uint8List senderPubkey;

  PunchPacket._(this.senderPubkey);

  /// Create a punch packet with our public key.
  factory PunchPacket.create(Uint8List senderPubkey) {
    assert(senderPubkey.length == 32);
    return PunchPacket._(Uint8List.fromList(senderPubkey));
  }

  /// Serialize to wire format.
  Uint8List serialize() {
    final bytes = Uint8List(packetSize);
    bytes.setRange(0, 4, magic);
    bytes.setRange(4, 36, senderPubkey);
    return bytes;
  }

  /// Try to parse a received datagram as a punch packet.
  ///
  /// Returns null if the data doesn't have the correct magic or is too short.
  static PunchPacket? tryParse(Uint8List data) {
    if (data.length < packetSize) return null;

    // Check magic
    if (data[0] != magic[0] ||
        data[1] != magic[1] ||
        data[2] != magic[2] ||
        data[3] != magic[3]) {
      return null;
    }

    return PunchPacket._(Uint8List.fromList(data.sublist(4, 36)));
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
/// 5. Once the path is open, the peers establish their Noise XX session over it.
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

  /// Our public key, included in punch packets so the peer can identify us.
  final Uint8List senderPubkey;

  /// Whether this service has been disposed.
  bool _disposed = false;

  /// Cached serialized punch packet (same for all targets).
  late final Uint8List _punchBytes;

  HolePunchService({
    required this.socket,
    required this.senderPubkey,
  }) {
    _punchBytes = PunchPacket.create(senderPubkey).serialize();
  }

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
