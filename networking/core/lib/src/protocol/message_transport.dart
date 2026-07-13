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
  /// a fragment exhausted its retransmissions or the peer was abandoned.
  Future<bool> sendMessage(
    String peerHex,
    String messageId,
    Uint8List payload,
  ) {
    if (_disposed) return Future.value(false);
    final fragmented = _fragmenter.fragment(
      payload: payload,
      messageId: messageId,
      maxChunk: maxChunk,
    );
    final lane = _lanes.putIfAbsent(peerHex, () => _PeerLane(peerHex));
    final message = _OutboundMessage(
      messageId: messageId,
      fragments: fragmented.fragments,
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
    message.acked.add(index);
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
    if (inFlight.attempts > maxAttempts) {
      _failMessage(lane, message);
      return;
    }
    final delay = _backoffFor(inFlight.attempts);
    inFlight.timer = Timer(delay, () {
      if (message.completer.isCompleted) return;
      if (!message.inFlight.containsKey(inFlight.index)) return;
      debugPrint('[transport] Retransmitting ${message.messageId} '
          'fragment ${inFlight.index} (attempt ${inFlight.attempts + 1})');
      _transmit(lane, message, inFlight);
    });
  }

  Duration _backoffFor(int attempts) {
    var delay = initialBackoff * (1 << (attempts - 1));
    if (delay > maxBackoff) delay = maxBackoff;
    return delay;
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
  _OutboundMessage({required this.messageId, required this.fragments})
      : pending = {for (var i = 0; i < fragments.length; i++) i};

  final String messageId;
  final List<GrassrootsPacket> fragments;

  /// Fragment indices not yet transmitted (or returned by a failed slot).
  final Set<int> pending;

  /// Fragment indices transmitted and awaiting acknowledgment.
  final Map<int, _InFlightFragment> inFlight = {};

  final Set<int> acked = {};
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
