/// Message Queue (M_p) for irmaGLP
/// 
/// Manages outbound messages from an agent to other agents.
/// Messages are queued per destination with FIFO ordering.
/// 
/// Specification: /docs/ma/irmaGLP-spec.md Section 3.3
library;

import 'dart:collection';

/// Type of outbound message
enum MessageType {
  /// Assignment: (X?:=T, destination)
  /// Reader X? is local to destination agent
  assignment,
  
  /// Read request: (request(X?, requester), destination)
  /// Agent requests value from creator
  readRequest,
  
  /// Abandon notification: (abandon(Y), destination)
  /// Variable became unreachable
  abandon,
  
  /// Agent message: structured term sent between agents
  /// Used for friend-to-friend communication (msg/3 terms)
  agentMessage,
}

/// An outbound message to another agent
class OutboundMessage {
  /// Destination agent ID
  final String destination;
  
  /// Type of message
  final MessageType type;
  
  /// Serialized payload (opaque bytes)
  final List<int> payload;
  
  OutboundMessage({
    required this.destination,
    required this.type,
    required this.payload,
  });
  
  @override
  String toString() {
    return 'OutboundMessage(to=$destination, type=$type, ${payload.length} bytes)';
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
  
  /// Poll (remove and return) the next message for a destination
  /// 
  /// Returns null if no messages are queued for this destination.
  /// Maintains FIFO ordering - oldest message is returned first.
  OutboundMessage? poll(String destination) {
    final queue = _queuesByDestination[destination];
    if (queue == null || queue.isEmpty) {
      return null;
    }
    
    final message = queue.removeFirst();
    
    // Clean up empty queue
    if (queue.isEmpty) {
      _queuesByDestination.remove(destination);
    }
    
    return message;
  }
  
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
