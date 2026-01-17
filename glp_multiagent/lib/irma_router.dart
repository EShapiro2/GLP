/// IrmaRouter - Routes opaque byte payloads between agent windows
///
/// Replaces SimpleRouter for irmaGLP integration.
/// Handles Uint8List payloads instead of JSON for inter-agent messages.
library;

import 'dart:convert';
import 'dart:typed_data';
import 'package:flutter/foundation.dart';
import 'package:desktop_multi_window/desktop_multi_window.dart';

/// Router for opaque byte payloads between agent windows
class IrmaRouter {
  static final IrmaRouter _instance = IrmaRouter._();
  static IrmaRouter get instance => _instance;

  IrmaRouter._();

  final Map<String, int> _agentWindows = {}; // agentId -> windowId
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

  /// Register an agent with its window ID
  void register(String agentId, int windowId) {
    _agentWindows[agentId.toLowerCase()] = windowId;
    _log('Registered $agentId (window $windowId)');
  }

  /// Unregister an agent
  void unregister(String agentId) {
    _agentWindows.remove(agentId.toLowerCase());
    _log('Unregistered $agentId');
  }

  /// Route a binary message from one agent to another
  /// 
  /// [from] - sender agent ID
  /// [to] - recipient agent ID  
  /// [payload] - opaque bytes (serialized irmaGLP message)
  Future<void> route(String from, String to, Uint8List payload) async {
    _log('Route: $from -> $to (${payload.length} bytes)');

    final targetWindowId = _agentWindows[to.toLowerCase()];
    if (targetWindowId == null) {
      _log('ERROR: Unknown recipient $to');
      return;
    }

    try {
      // Encode payload as base64 for JSON transport
      // (DesktopMultiWindow requires JSON-serializable arguments)
      final encodedPayload = base64Encode(payload);
      
      await DesktopMultiWindow.invokeMethod(
        targetWindowId,
        'deliver_irma',
        jsonEncode({
          'from': from,
          'payload': encodedPayload,
        }),
      );
      _log('Delivered to $to');
    } catch (e) {
      _log('ERROR delivering to $to: $e');
    }
  }

  /// Get window ID for an agent (for direct access if needed)
  int? getWindowId(String agentId) {
    return _agentWindows[agentId.toLowerCase()];
  }

  /// Check if an agent is registered
  bool isRegistered(String agentId) {
    return _agentWindows.containsKey(agentId.toLowerCase());
  }

  /// Get list of registered agent IDs
  List<String> get registeredAgents => _agentWindows.keys.toList();
}
