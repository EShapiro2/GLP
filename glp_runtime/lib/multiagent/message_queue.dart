/// Message Queue (M_p) for madGLP
///
/// Manages outbound messages from an agent to other agents.
/// Messages are queued per destination with FIFO ordering.
///
/// Specification: IGLP Definition madGLP Local State (M_p, the outgoing
/// messages).
library;

import 'dart:collection';
import 'dart:typed_data';

import 'package:glp_runtime/runtime/saved_state.dart';
import 'package:glp_runtime/wire/codec.dart' show messageWithSenderEpoch;

import 'mad_helpers.dart' show GlobalName;

/// Type of outbound message
enum MessageType {
  /// Assignment: (G := T, destination)
  /// G is a global name (_w(p,i) or _r(p,i)), T is the assigned term
  assignment,

  /// Request: req(_r(p, i)) — sent to the anchor p by an agent that localized
  /// a forwarded reader name (spec §app:requests-acks). Carries no term.
  request,

  /// Acknowledgement: ack(_r(p, i)) — sent to a reader-name value's sender
  /// after applying it; releases the pending value and closes the link.
  acknowledgement,

  /// Gone: gone(G) — answers traffic for an end this agent lost, and the
  /// receiver retires its own end (IGLP db0a117, "Requests and
  /// Acknowledgements").  Carries no term.
  gone,
}

/// An outbound message to another agent
class OutboundMessage {
  /// Destination agent ID
  final String destination;

  /// Type of message
  final MessageType type;

  /// Serialized payload (opaque bytes), the message in its canonical encoding,
  /// its sender's epoch after its kind byte ([withSenderEpoch]).
  List<int> payload;

  /// The link this message belongs to, by its global name, where it belongs
  /// to one: a value, a request or an acknowledgement on a link, and so a
  /// message a retirement of that link's end removes (IGLP db0a117,
  /// Definition "Retire": "from M_q every message of the link").  Null for a
  /// serializer send, a cold call, and for a gone, which belong to no link
  /// the agent holds.
  final GlobalName? link;

  /// Whether this message is held (Definition Held Link): it stays in M_p and
  /// is not eligible for Send until `authorise_link/2` authorises it, or is
  /// removed outright if the link is refused.
  ///
  /// Mutable: authorisation clears the mark, and the message then goes out on
  /// the next Send — it is the same message, released, not a new one.
  bool held;

  OutboundMessage({
    required this.destination,
    required this.type,
    required this.payload,
    this.link,
    this.held = false,
  });

  /// Rewrite the sender's epoch this message carries to [epoch]: a message
  /// queued before a restart after a crash is sent in the epoch the restart
  /// opened, "the epoch its sender is in when it sends it" (IGLP db0a117).
  void withSenderEpoch(int epoch) {
    payload = messageWithSenderEpoch(
        payload is Uint8List ? payload as Uint8List : Uint8List.fromList(payload),
        epoch);
  }

  @override
  String toString() {
    final h = held ? ', HELD' : '';
    return 'OutboundMessage(to=$destination, type=$type, '
        '${payload.length} bytes$h)';
  }
}

/// Message Queue (M_p): the outgoing messages, in a FIFO queue per
/// destination, each with its held mark ([OutboundMessage.held]).
/// 
/// [poll] takes a destination's oldest sendable message, passing over the
/// held ones, which stay in place until authorisation clears their mark or a
/// refusal removes them ([remove]).
class MessageQueue {
  /// Queues indexed by destination agent ID
  final Map<String, Queue<OutboundMessage>> _queuesByDestination = {};
  
  /// Add a message to the queue
  /// 
  /// Messages are queued per destination with FIFO ordering.
  void add(OutboundMessage message) {
    final queue = _queuesByDestination.putIfAbsent(
      message.destination,
      () => Queue<OutboundMessage>(),
    );
    queue.add(message);
  }
  
  /// Poll (remove and return) the next *sendable* message for a destination
  ///
  /// Returns null if no sendable message is queued for this destination.
  /// Maintains FIFO ordering among sendable messages - oldest first.
  ///
  /// Held messages are skipped and left in place: Send is "enabled when
  /// (m, q) ∈ M_p is unsent **and not held**" (Definition madGLP Send), and a
  /// held message "remains in M_p, marked held" (Definition Held Link).
  OutboundMessage? poll(String destination) {
    final queue = _queuesByDestination[destination];
    if (queue == null || queue.isEmpty) {
      return null;
    }

    OutboundMessage? message;
    if (!queue.first.held) {
      message = queue.removeFirst();
    } else {
      // Some held message is at the head; take the first sendable one behind
      // it, preserving order among sendable messages.
      message = _takeFirst(queue, (m) => !m.held);
      if (message == null) return null;
    }

    // Clean up empty queue
    if (queue.isEmpty) {
      _queuesByDestination.remove(destination);
    }

    return message;
  }

  /// Every message currently in M_p, held or not, across all destinations.
  Iterable<OutboundMessage> get all =>
      _queuesByDestination.values.expand((q) => q);

  /// The held message for the link named [link], or null.
  OutboundMessage? findHeld(GlobalName link) {
    for (final m in all) {
      if (m.held && m.link == link) return m;
    }
    return null;
  }

  /// Remove every message of the link named [link], held or not (IGLP
  /// db0a117, Definition "Retire"); the number removed.
  int removeOfLink(GlobalName link) {
    var n = 0;
    for (final m in all.where((m) => m.link == link).toList()) {
      if (remove(m)) n++;
    }
    return n;
  }

  /// Rewrite the sender's epoch of every message to [epoch]
  /// ([OutboundMessage.withSenderEpoch]).
  void withSenderEpoch(int epoch) {
    for (final m in all) {
      m.withSenderEpoch(epoch);
    }
  }

  /// Remove [message] from M_p wherever it sits. Used when a link is refused.
  bool remove(OutboundMessage message) {
    for (final entry in _queuesByDestination.entries) {
      if (_takeFirst(entry.value, (m) => m == message) != null) {
        if (entry.value.isEmpty) {
          _queuesByDestination.remove(entry.key);
        }
        return true;
      }
    }
    return false;
  }

  /// Take out of [queue] the first message satisfying [test], keeping the
  /// others in order, and return it; null, and [queue] unchanged, where none
  /// does.
  static OutboundMessage? _takeFirst(
      Queue<OutboundMessage> queue, bool Function(OutboundMessage) test) {
    final rest = queue.toList();
    final idx = rest.indexWhere(test);
    if (idx < 0) return null;
    final taken = rest.removeAt(idx);
    queue
      ..clear()
      ..addAll(rest);
    return taken;
  }

  /// Number of held messages across all destinations.
  int get heldCount => all.where((m) => m.held).length;

  /// Number of messages eligible for Send across all destinations.
  int get sendableLength => all.where((m) => !m.held).length;

  /// Whether some message is eligible for Send: unsent and not held
  /// (Definition madGLP Send).
  bool get hasSendable => all.any((m) => !m.held);

  /// Get all destinations that have queued messages
  List<String> get destinations {
    return _queuesByDestination.keys.toList();
  }
  
  /// Check if queue is empty (no messages for any destination)
  bool get isEmpty {
    return _queuesByDestination.isEmpty;
  }
  
  /// Total number of messages across all destinations
  int get totalLength {
    return _queuesByDestination.values
        .fold(0, (sum, queue) => sum + queue.length);
  }

  /// Write the outgoing messages into the agent's saved state, by destination
  /// and in order, each with its held mark (IGLP, Implementation Notes,
  /// "Persistence": "the outgoing messages with their pending and held
  /// marks"; the pending values are MadContext's).
  void saveState(StateWriter w) {
    w.uint(_queuesByDestination.length);
    for (final MapEntry(key: destination, value: queue)
        in _queuesByDestination.entries) {
      w.str(destination);
      w.uint(queue.length);
      for (final m in queue) {
        w.u8(m.type.index);
        w.bytes(m.payload);
        final link = m.link;
        w.boolean(link != null);
        if (link != null) link.save(w);
        w.boolean(m.held);
      }
    }
  }

  /// Read back what [saveState] wrote, into a queue fresh from construction.
  void restoreState(StateReader r) {
    _queuesByDestination.clear();
    for (var i = r.uint(); i > 0; i--) {
      final destination = r.str();
      for (var j = r.uint(); j > 0; j--) {
        final type = MessageType.values[r.u8()];
        final payload = r.bytes();
        final link = r.boolean() ? GlobalName.restore(r) : null;
        add(OutboundMessage(
          destination: destination,
          type: type,
          payload: payload,
          link: link,
          held: r.boolean(),
        ));
      }
    }
  }
  
  @override
  String toString() {
    if (isEmpty) {
      return 'MessageQueue: empty';
    }
    
    final buffer = StringBuffer('MessageQueue:\n');
    for (final destination in destinations) {
      final count = _queuesByDestination[destination]!.length;
      buffer.writeln('  $destination: $count message(s)');
    }
    return buffer.toString();
  }
}
