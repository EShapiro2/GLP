/// IsolateRouter — routes MAD messages between agent isolates via SendPort.
///
/// Replaces the previous MadRouter which used desktop_multi_window method
/// channels between separate OS windows. Now routes via Dart isolate ports.
///
/// Messages sent to unregistered agents are buffered and delivered when the
/// target agent registers (handles initialization race conditions).
library;

import 'dart:isolate';
import 'dart:typed_data';

import 'package:flutter/foundation.dart';

import 'isolate_protocol.dart';

/// Routes MAD messages between agent isolates via SendPort.
class IsolateRouter {
  static final IsolateRouter _instance = IsolateRouter._();
  static IsolateRouter get instance => _instance;

  IsolateRouter._();

  final Map<String, SendPort> _agentPorts = {}; // agentId -> SendPort
  final Map<String, List<DeliverMad>> _pendingMessages = {}; // buffered msgs
  final List<String> _routingLog = [];
  VoidCallback? onLogUpdate;

  List<String> get routingLog => List.unmodifiable(_routingLog);

  void _log(String message) {
    final timestamp = DateTime.now().toIso8601String().substring(11, 19);
    _routingLog.add('[$timestamp] $message');
    onLogUpdate?.call();
  }

  void clearLog() {
    _routingLog.clear();
    _pendingMessages.clear();
    onLogUpdate?.call();
  }

  /// Register an agent with its command SendPort.
  /// Delivers any buffered messages that arrived before registration.
  void register(String agentId, SendPort port) {
    final key = agentId.toLowerCase();
    _agentPorts[key] = port;
    _log('Registered $agentId');

    // Deliver any buffered messages
    final pending = _pendingMessages.remove(key);
    if (pending != null && pending.isNotEmpty) {
      _log('Delivering ${pending.length} buffered messages to $agentId');
      for (final msg in pending) {
        port.send(msg);
      }
    }
  }

  /// Unregister an agent.
  void unregister(String agentId) {
    final key = agentId.toLowerCase();
    _agentPorts.remove(key);
    _pendingMessages.remove(key);
    _log('Unregistered $agentId');
  }

  /// Route a MAD message to the target agent's isolate.
  /// If the target is not yet registered, buffers the message for later delivery.
  void route(String from, String to, Uint8List payload) {
    _log('Route: $from -> $to (${payload.length} bytes)');

    final key = to.toLowerCase();
    final targetPort = _agentPorts[key];
    if (targetPort == null) {
      // Buffer for later delivery when target registers
      _log('Buffering message for unregistered agent $to');
      _pendingMessages.putIfAbsent(key, () => []).add(DeliverMad(from, payload));
      return;
    }

    targetPort.send(DeliverMad(from, payload));
    _log('Delivered to $to');
  }

  bool isRegistered(String agentId) {
    return _agentPorts.containsKey(agentId.toLowerCase());
  }

  List<String> get registeredAgents => _agentPorts.keys.toList();
}
