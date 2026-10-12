import 'dart:async';
import 'dart:typed_data';

import '../models/packet.dart';
import '../platform/compat.dart';
import 'fragment_handler.dart';

/// Transmits one (session-encrypted, framed) packet to a peer over the
/// carrier. Returns false when the carrier cannot take the packet now.
typedef PacketSender = Future<bool> Function(
    String peerHex, GrassrootsPacket packet);

/// The sending half of the shared message transport (spec §Message
/// Transport): a message is split into self-contained fragments sized to
/// the carrier; the receiver acknowledges fragments; the sender retransmits
/// unacknowledged fragments with backoff, under a bounded send window per
/// peer.  No order is kept across messages — window slots are filled
/// round-robin across a peer's active messages, so a lost fragment delays
/// only its own message.
///
/// One instance per medium; the carrier differences are the [maxChunk], the
/// backoff profile (over IP retransmission is the reliability mechanism;
/// over link-acknowledged GATT writes it is a backstop), and the
/// [sendPacket] closure that encrypts and transmits.
class MessageTransportSender {
  MessageTransportSender({
    required this.maxChunk,
    required this.sendPacket,
    this.windowPerPeer = 16,
    this.initialBackoff = const Duration(milliseconds: 400),
    this.maxBackoff = const Duration(seconds: 6),
    this.maxAttempts = 8,
  });

  /// The GATT carrier (spec §Message Protocol: over BLE a fragment rides a
  /// GATT write without response, sized to the negotiated MTU): fragments of
  /// [FragmentHandler.bleMaxChunk], a window of four, and retransmission a
  /// backstop to the link's own acknowledgment. One profile, so that a test
  /// drives the carrier the coordinator runs.
  factory MessageTransportSender.gatt({required PacketSender sendPacket}) =>
      MessageTransportSender(
        maxChunk: FragmentHandler.bleMaxChunk,
        windowPerPeer: 4,
        initialBackoff: const Duration(seconds: 2),
        maxAttempts: 3,
        sendPacket: sendPacket,
      );

  final int maxChunk;
  final PacketSender sendPacket;

  /// Bounded send window per peer: at most this many fragments in flight
  /// (sent, unacknowledged) toward one peer at any moment.
  final int windowPerPeer;

  final Duration initialBackoff;
  final Duration maxBackoff;

  /// Retransmissions per fragment before the whole message fails (the
  /// caller re-queues it under fair delivery).
  final int maxAttempts;

  final FragmentHandler _fragmenter = FragmentHandler();
  final Map<String, _PeerLane> _lanes = {};
  bool _disposed = false;

  /// Send [payload] as [messageId] to [peerHex]. Completes true when every
  /// fragment has been acknowledged (or the whole message was), false when
  /// a fragment exhausted its retransmissions, the peer was abandoned, or
  /// [isCurrent] said the message no longer belongs on the wire.
  ///
  /// [fragmentType] is what the fragments travel as: a `send` payload's
  /// [PacketType.fragment], or [PacketType.attestationFragment] for the
  /// attestation exchange, which rides the same protocol (spec §Message
  /// Protocol).
  ///
  /// [isCurrent], where given, binds the message to something that can end
  /// before the message is through — the attestation to the session whose
  /// handshake hash its signature names (spec §Session Establishment). It is
  /// asked before every transmission, in the same synchronous step that hands
  /// the fragment to [sendPacket]; once it answers false the message fails
  /// untransmitted, and nothing of it reaches a session it does not belong
  /// to.
  Future<bool> sendMessage(
    String peerHex,
    String messageId,
    Uint8List payload, {
    PacketType fragmentType = PacketType.fragment,
    bool Function()? isCurrent,
  }) {
    if (_disposed) return Future.value(false);
    final lane = _lanes.putIfAbsent(peerHex, () => _PeerLane(peerHex));
    // A retry can race the original (e.g. the coordinator's ACK watchdog
    // re-queues while the engine is still retransmitting): join the
    // in-flight message instead of transmitting it twice.
    final existing = lane.messageById(messageId);
    if (existing != null) return existing.completer.future;
    final fragmented = _fragmenter.fragment(
      payload: payload,
      messageId: messageId,
      maxChunk: maxChunk,
      type: fragmentType,
    );
    final message = _OutboundMessage(
      messageId: messageId,
      fragments: fragmented.fragments,
      isCurrent: isCurrent,
    );
    lane.messages.add(message);
    _pump(lane);
    return message.completer.future;
  }

  /// A per-fragment acknowledgment arrived.
  void handleFragmentAck(String peerHex, String messageId, int index) {
    final lane = _lanes[peerHex];
    if (lane == null) return;
    final message = lane.messageById(messageId);
    if (message == null) return;
    final inFlight = message.inFlight.remove(index);
    if (inFlight != null) {
      inFlight.timer?.cancel();
      lane.slotsInUse--;
    }
    message.pending.remove(index);
    _completeIfDone(lane, message);
    _pump(lane);
  }

  /// A whole-message acknowledgment arrived (delivery receipt): everything
  /// still outstanding for that message is settled — the receiver has it.
  void handleMessageAck(String messageId) {
    for (final lane in _lanes.values) {
      final message = lane.messageById(messageId);
      if (message == null) continue;
      for (final inFlight in message.inFlight.values) {
        inFlight.timer?.cancel();
        lane.slotsInUse--;
      }
      message.inFlight.clear();
      message.pending.clear();
      message.complete(true);
      lane.messages.remove(message);
      _pump(lane);
      return;
    }
  }

  /// Drop all state toward [peerHex]; outstanding messages fail (and fall
  /// back to the caller's fair-delivery queue).
  void abandonPeer(String peerHex) {
    final lane = _lanes.remove(peerHex);
    if (lane == null) return;
    for (final message in lane.messages.toList()) {
      for (final inFlight in message.inFlight.values) {
        inFlight.timer?.cancel();
      }
      message.complete(false);
    }
    lane.messages.clear();
  }

  void dispose() {
    _disposed = true;
    for (final peer in _lanes.keys.toList()) {
      abandonPeer(peer);
    }
    _fragmenter.dispose();
  }

  /// Fill free window slots, round-robin across the peer's active messages
  /// (spec: a lost fragment delays only its own message — one stalled
  /// message must not starve the others).
  void _pump(_PeerLane lane) {
    if (_disposed) return;
    while (lane.slotsInUse < windowPerPeer && lane.messages.isNotEmpty) {
      final message = lane.nextMessageWithPending();
      if (message == null) return;
      final index = message.pending.first;
      message.pending.remove(index);
      final inFlight = _InFlightFragment(index: index);
      message.inFlight[index] = inFlight;
      lane.slotsInUse++;
      _transmit(lane, message, inFlight);
    }
  }

  void _transmit(
    _PeerLane lane,
    _OutboundMessage message,
    _InFlightFragment inFlight,
  ) {
    if (_disposed || message.completer.isCompleted) return;
    if (inFlight.attempts >= maxAttempts) {
      _failMessage(lane, message);
      return;
    }
    if (message.isCurrent != null && !message.isCurrent!()) {
      // What the message was bound to has ended: fail it before anything of
      // it is sent, rather than let a carrier carry it into whatever has
      // taken that thing's place.
      debugPrint('[transport] ${message.messageId} no longer current; '
          'failing it untransmitted');
      _failMessage(lane, message);
      return;
    }
    inFlight.attempts++;
    unawaited(() async {
      final ok = await sendPacket(
        lane.peerHex,
        message.fragments[inFlight.index],
      );
      if (!ok && inFlight.attempts == 1) {
        // The carrier refused the very first transmission (no session, no
        // path): fail fast so the caller can queue the message rather than
        // burn the backoff schedule.
        _failMessage(lane, message);
        return;
      }
    }());
    if (message.completer.isCompleted) return;
    final delay = _backoffFor(inFlight.attempts);
    inFlight.timer = Timer(delay, () {
      if (message.completer.isCompleted) return;
      if (!message.inFlight.containsKey(inFlight.index)) return;
      debugPrint('[transport] Retransmitting ${message.messageId} '
          'fragment ${inFlight.index} (attempt ${inFlight.attempts + 1})');
      _transmit(lane, message, inFlight);
    });
  }

  /// How long a message's attempts last: from its first transmission to the
  /// moment its last retransmission goes unacknowledged and the message
  /// fails. The receiver of the attestation exchange waits this long for the
  /// peer's attestation before it tears the session down (GNA, 2026-10-11,
  /// Attestation Q1: "the receiver waits for the peer's attestation as long as
  /// the sender's attempts last").
  Duration get attemptWindow {
    var window = Duration.zero;
    for (var attempt = 1; attempt <= maxAttempts; attempt++) {
      window += _backoffFor(attempt);
    }
    return window;
  }

  /// The backoff schedule, for tests. What it guards against is invisible
  /// from the outside — a spinning CPU, not an error — so it is asserted
  /// directly rather than through the sender's behaviour.
  @visibleForTesting
  Duration backoffForAttempt(int attempts) => _backoffFor(attempts);

  /// Exponential backoff, clamped at [maxBackoff].
  ///
  /// The doubling is iterated and stopped at the clamp rather than computed
  /// as `initialBackoff * (1 << (attempts - 1))`, which overflows: at
  /// attempts 63 the shift is large enough that the Duration multiplication
  /// wraps to ZERO, and from there every retransmit timer fires immediately —
  /// a hot loop flooding the carrier instead of a backoff. It is unreachable
  /// at the attempt limits in use (3 in the coordinator, 8 by default), which
  /// is why it had never been seen; raising either would have found it as a
  /// spinning CPU and not as an error.
  Duration _backoffFor(int attempts) {
    var delay = initialBackoff;
    for (var doubling = 1; doubling < attempts; doubling++) {
      if (delay >= maxBackoff) return maxBackoff;
      delay *= 2;
    }
    return delay > maxBackoff ? maxBackoff : delay;
  }

  void _failMessage(_PeerLane lane, _OutboundMessage message) {
    for (final inFlight in message.inFlight.values) {
      inFlight.timer?.cancel();
      lane.slotsInUse--;
    }
    message.inFlight.clear();
    message.complete(false);
    lane.messages.remove(message);
    _pump(lane);
  }

  void _completeIfDone(_PeerLane lane, _OutboundMessage message) {
    if (message.pending.isEmpty && message.inFlight.isEmpty) {
      message.complete(true);
      lane.messages.remove(message);
    }
  }
}

class _PeerLane {
  _PeerLane(this.peerHex);

  final String peerHex;
  final List<_OutboundMessage> messages = [];
  int slotsInUse = 0;
  int _rrCursor = 0;

  _OutboundMessage? messageById(String messageId) {
    for (final message in messages) {
      if (message.messageId == messageId) return message;
    }
    return null;
  }

  /// Round-robin over messages that still have unsent fragments.
  _OutboundMessage? nextMessageWithPending() {
    if (messages.isEmpty) return null;
    for (var i = 0; i < messages.length; i++) {
      final candidate = messages[(_rrCursor + i) % messages.length];
      if (candidate.pending.isNotEmpty) {
        _rrCursor = (_rrCursor + i + 1) % messages.length;
        return candidate;
      }
    }
    return null;
  }
}

class _OutboundMessage {
  _OutboundMessage({
    required this.messageId,
    required this.fragments,
    this.isCurrent,
  }) : pending = {for (var i = 0; i < fragments.length; i++) i};

  final String messageId;
  final List<GrassrootsPacket> fragments;

  /// Whether the message still belongs on the wire (see
  /// [MessageTransportSender.sendMessage]); null for an unbound message.
  final bool Function()? isCurrent;

  /// Fragment indices not yet transmitted (or returned by a failed slot).
  final Set<int> pending;

  /// Fragment indices transmitted and awaiting acknowledgment.
  final Map<int, _InFlightFragment> inFlight = {};

  final Completer<bool> completer = Completer<bool>();

  void complete(bool value) {
    if (!completer.isCompleted) completer.complete(value);
  }
}

class _InFlightFragment {
  _InFlightFragment({required this.index});

  final int index;
  int attempts = 0;
  Timer? timer;
}
