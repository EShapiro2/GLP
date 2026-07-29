/// Message Queue (M_p) for madGLP
///
/// Manages outbound messages from an agent to other agents.
/// Messages are queued per destination with FIFO ordering.
///
/// Specification: /docs/ma/madGLP-spec.md Section 6.1
library;

import 'dart:collection';

/// Type of outbound message
enum MessageType {
  /// Assignment: (G := T, destination)
  /// G is a global name (_w(p,i) or _r(p,i)), T is the assigned term
  assignment,

  /// Agent message: structured term sent between agents
  /// Used for friend-to-friend communication (msg/3 terms)
  agentMessage,

  /// Request: req(_r(p, i)) — sent to the anchor p by an agent that localized
  /// a forwarded reader name (spec §app:requests-acks). Carries no term.
  request,

  /// Acknowledgement: ack(_r(p, i)) — sent to a reader-name value's sender
  /// after applying it; releases the pending value and closes the link.
  acknowledgement,
}

/// An outbound message to another agent
class OutboundMessage {
  /// Destination agent ID
  final String destination;

  /// Type of message
  final MessageType type;

  /// Serialized payload (opaque bytes)
  final List<int> payload;

  /// The link this message belongs to, as `(anchor, index, isWriter)`, when the
  /// message can be held. Null for messages that never hold (serializer sends).
  final (String, int, bool)? link;

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

  @override
  String toString() {
    final h = held ? ', HELD' : '';
    return 'OutboundMessage(to=$destination, type=$type, '
        '${payload.length} bytes$h)';
  }
}

/// Message Queue (M_p) for managing outbound messages
/// 
/// Maintains FIFO ordering per destination and ensures at-most-once delivery.
/// 
/// Properties:
/// - FIFO per destination: Messages to same agent delivered in order
/// - At-most-once: Each message delivered at most once
/// - Eventual delivery: All queued messages eventually delivered (assuming connectivity)
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
      final rest = queue.toList();
      final idx = rest.indexWhere((m) => !m.held);
      if (idx < 0) return null;
      message = rest.removeAt(idx);
      queue
        ..clear()
        ..addAll(rest);
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

  /// The held message for link `(anchor, index, isWriter)`, or null.
  OutboundMessage? findHeld(String anchor, int index, bool isWriter) {
    for (final m in all) {
      if (m.held && m.link == (anchor, index, isWriter)) return m;
    }
    return null;
  }

  /// Remove [message] from M_p wherever it sits. Used when a link is refused.
  bool remove(OutboundMessage message) {
    for (final entry in _queuesByDestination.entries) {
      final rest = entry.value.toList();
      if (rest.remove(message)) {
        entry.value
          ..clear()
          ..addAll(rest);
        if (entry.value.isEmpty) {
          _queuesByDestination.remove(entry.key);
        }
        return true;
      }
    }
    return false;
  }

  /// Number of held messages across all destinations.
  int get heldCount => all.where((m) => m.held).length;

  /// Number of messages eligible for Send across all destinations.
  int get sendableLength => all.where((m) => !m.held).length;
  
  /// Peek at the next message for a destination without removing it
  /// 
  /// Returns null if no messages are queued for this destination.
  OutboundMessage? peek(String destination) {
    final queue = _queuesByDestination[destination];
    if (queue == null || queue.isEmpty) {
      return null;
    }
    return queue.first;
  }
  
  /// Get the number of messages queued for a destination
  int countFor(String destination) {
    final queue = _queuesByDestination[destination];
    return queue?.length ?? 0;
  }
  
  /// Get all destinations that have queued messages
  List<String> get destinations {
    return _queuesByDestination.keys.toList();
  }
  
  /// Check if queue is empty (no messages for any destination)
  bool get isEmpty {
    return _queuesByDestination.isEmpty;
  }
  
  /// Check if queue has messages
  bool get isNotEmpty {
    return _queuesByDestination.isNotEmpty;
  }
  
  /// Total number of messages across all destinations
  int get totalLength {
    return _queuesByDestination.values
        .fold(0, (sum, queue) => sum + queue.length);
  }
  
  /// Clear all messages for all destinations
  void clear() {
    _queuesByDestination.clear();
  }
  
  /// Clear all messages for a specific destination
  void clearFor(String destination) {
    _queuesByDestination.remove(destination);
  }
  
  /// Get all messages for a destination (without removing them)
  /// 
  /// Returns an empty list if no messages are queued.
  List<OutboundMessage> peekAll(String destination) {
    final queue = _queuesByDestination[destination];
    if (queue == null) {
      return [];
    }
    return List.unmodifiable(queue);
  }
  
  /// Poll all messages for a destination
  /// 
  /// Returns an empty list if no messages are queued.
  /// Removes all messages from the queue.
  List<OutboundMessage> pollAll(String destination) {
    final queue = _queuesByDestination[destination];
    if (queue == null || queue.isEmpty) {
      return [];
    }
    
    final messages = List<OutboundMessage>.from(queue);
    _queuesByDestination.remove(destination);
    return messages;
  }
  
  @override
  String toString() {
    if (isEmpty) {
      return 'MessageQueue: empty';
    }
    
    final buffer = StringBuffer('MessageQueue:\n');
    for (final destination in destinations) {
      final count = countFor(destination);
      buffer.writeln('  $destination: $count message(s)');
    }
    return buffer.toString();
  }
}
