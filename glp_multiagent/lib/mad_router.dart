/// IsolateRouter — routes MAD messages between agent isolates via SendPort.
///
/// Replaces the previous MadRouter which used desktop_multi_window method
/// channels between separate OS windows. Now routes via Dart isolate ports.
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
    onLogUpdate?.call();
  }

  /// Register an agent with its command SendPort.
  void register(String agentId, SendPort port) {
    _agentPorts[agentId.toLowerCase()] = port;
    _log('Registered $agentId');
  }

  /// Unregister an agent.
  void unregister(String agentId) {
    _agentPorts.remove(agentId.toLowerCase());
    _log('Unregistered $agentId');
  }

  /// Route a MAD message to the target agent's isolate.
  void route(String from, String to, Uint8List payload) {
    _log('Route: $from -> $to (${payload.length} bytes)');

    final targetPort = _agentPorts[to.toLowerCase()];
    if (targetPort == null) {
      _log('ERROR: Unknown recipient $to');
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
