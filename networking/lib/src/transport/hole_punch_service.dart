import 'dart:async';
import 'dart:io';

import 'package:flutter/foundation.dart';

/// A UDP hole-punch packet.
///
/// Format: `[magic(4)] [senderPubkey(32)]` = 36 bytes total.
///
/// The magic bytes identify this as a Grassroots punch packet so receivers
/// can distinguish it from UDX traffic or other UDP packets.
///
/// Punch packets serve one purpose: to create a NAT mapping. The receiver
/// doesn't need to process them — the act of sending opens our NAT.
/// But if the receiver's multiplexer hasn't started yet, it can read them
/// to detect that the hole is open.
class PunchPacket {
  /// Magic bytes: "BCPU" (BitChat Punch UDP)
  static const List<int> magic = [0x42, 0x43, 0x50, 0x55];

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
/// 5. After punching, a UDX connection can be established through the open path.
///
/// ## Two modes
///
/// - [punch]: Fire-and-forget. Sends packets for a fixed duration. Simple but
///   requires both peers to punch simultaneously and hope for overlap.
///
/// - [punchUntilResponse]: Sends packets and listens for incoming punch packets
///   from the target. Returns true when a response is detected. More reliable
///   but requires the multiplexer to NOT be running (so we can read raw packets).
///
/// ## Design for extensibility
///
/// This class handles simple cone-NAT punching. For symmetric NATs,
/// a birthday-paradox approach (sending to multiple predicted ports) can be
/// added as a subclass or strategy without changing the interface.
class HolePunchService {

  /// The raw UDP socket used for sending (and optionally receiving) punch packets.
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

  /// Send punch packets for a fixed duration.
  ///
  /// This is the simple fire-and-forget mode. Use when the UDX multiplexer
  /// is already running (so we can't read raw packets).
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

  /// Send punch packets and wait for a response from the target.
  ///
  /// Returns true if a punch packet was received from the target before timeout.
  /// Returns false if the timeout expired without a response.
  ///
  /// **Important:** This mode requires the UDX multiplexer to NOT be running,
  /// since the multiplexer takes over socket reads. Use [punch] instead if
  /// the multiplexer is already active.
  Future<bool> punchUntilResponse(
    InternetAddress targetIp,
    int targetPort, {
    Duration timeout = const Duration(seconds: 5),
    Duration interval = const Duration(milliseconds: 200),
  }) async {
    debugPrint('Punching ${targetIp.address}:$targetPort (waiting for response, '
        'timeout: ${timeout.inMilliseconds}ms)');

    final completer = Completer<bool>();
    var sent = 0;

    // Listen for incoming punch packets from the target
    final sub = socket.listen((event) {
      if (event == RawSocketEvent.read && !completer.isCompleted) {
        final dg = socket.receive();
        if (dg == null) return;

        // Check if it's a punch packet from the target
        final parsed = PunchPacket.tryParse(dg.data);
        if (parsed != null) {
          debugPrint('Received punch response from '
              '${dg.address.address}:${dg.port}');
          if (!completer.isCompleted) {
            completer.complete(true);
          }
        }
      }
    });

    // Send punch packets at intervals
    final sendTimer = Timer.periodic(interval, (_) {
      if (!_disposed && !completer.isCompleted) {
        socket.send(_punchBytes, targetIp, targetPort);
        sent++;
      }
    });

    // Also send one immediately
    socket.send(_punchBytes, targetIp, targetPort);
    sent++;

    // Timeout
    final timeoutTimer = Timer(timeout, () {
      if (!completer.isCompleted) {
        debugPrint('Punch timed out after sending $sent packets');
        completer.complete(false);
      }
    });

    final result = await completer.future;

    sendTimer.cancel();
    timeoutTimer.cancel();
    await sub.cancel();

    debugPrint('Punch result: ${result ? 'SUCCESS' : 'TIMEOUT'} '
        '(sent $sent packets)');
    return result;
  }

  /// Cancel any in-progress punch operations.
  void dispose() {
    _disposed = true;
  }
}
