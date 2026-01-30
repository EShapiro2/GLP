/// IrmaAgent - Agent wrapper for irmaGLP multiagent integration
///
/// Wraps a GlpRuntime with IrmaContext for multiagent communication.
/// Manages V_p, M_p, and message routing via coordinator.
library;

import 'dart:convert';
import 'dart:typed_data';

import '../runtime/runtime.dart';
import '../runtime/terms.dart';
import '../bytecode/runner.dart';
import '../runtime/scheduler.dart';
import 'irma_context.dart';
import 'variable_table.dart';
import 'message_queue.dart';
import 'payload_serializer.dart';

/// Callback for sending messages to coordinator
typedef SendToCoordinator = Future<void> Function(String destination, Uint8List payload);

/// Callback for receiving messages from coordinator  
typedef OnMessageReceived = void Function(String from, Uint8List payload);

/// IrmaAgent wraps a GLP runtime with irmaGLP multiagent support
class IrmaAgent {
  final String agentId;
  final GlpRuntime runtime;
  late final IrmaContext context;
  
  /// Callback to send messages to coordinator
  SendToCoordinator? onSendToCoordinator;
  
  /// Debug logging callback
  void Function(String)? onLog;
  
  IrmaAgent({
    required this.agentId,
    GlpRuntime? runtime,
  }) : runtime = runtime ?? GlpRuntime() {
    // Create context with the actual runtime instance
    context = IrmaContext(
      agentId: agentId,
      runtime: this.runtime,
    );
    // Set up message flush callback
    context.onMessageReady = _handleOutboundMessage;
  }
  
  /// Handle outbound message from IrmaContext
  void _handleOutboundMessage(String destination, OutboundMessage msg) {
    _log('Outbound ${msg.type.name} to $destination');
    
    // Serialize the message
    final payload = _serializeMessage(msg);
    
    // Send via coordinator
    if (onSendToCoordinator != null) {
      onSendToCoordinator!(destination, payload);
    } else {
      _log('WARNING: No coordinator callback set, message dropped');
    }
  }
  
  /// Serialize an outbound message to bytes
  Uint8List _serializeMessage(OutboundMessage msg) {
    final serializer = PayloadSerializer(agentId);
    return serializer.serializeMessage(msg);
  }
  
  /// Handle incoming message from coordinator
  void handleIncomingMessage(String from, Uint8List payload) {
    _log('Incoming from $from (${payload.length} bytes)');
    
    // Deserialize the message
    final serializer = PayloadSerializer(agentId);
    final msg = serializer.deserializeMessage(payload);
    
    // Route to appropriate handler based on type
    switch (msg.type) {
      case MessageType.assignment:
        _handleAssignment(msg);
        break;
      case MessageType.readRequest:
        _handleReadRequest(msg, from);
        break;
      case MessageType.abandon:
        _handleAbandon(msg);
        break;
      case MessageType.agentMessage:
        // Agent messages are handled at the Flutter app level, not here
        _log('WARNING: agentMessage received in IrmaAgent - should be handled by app');
        break;
    }
  }
  
  void _handleAssignment(OutboundMessage msg) {
    // Extract globalVarId and value from payload
    // Pass heap allocation callbacks to properly import nested variables
    final serializer = PayloadSerializer(agentId);
    final (globalId, value) = serializer.deserializeAssignmentPayload(
      msg.payload,
      // Allocator callback: create single-cell for imported variable
      (bool isReader) {
        if (isReader) {
          return runtime.heap.allocateImportedReader();
        } else {
          return runtime.heap.allocateImportedWriter();
        }
      },
      // Entry creator callback: attach VariableEntry to imported variables
      onVariableImported: (int localAddr, bool isReader, GlobalVarId nestedGlobalId, int? pairedReaderCreatorLocalId) {
        context.attachImportedVariableEntry(
          localAddr, isReader, nestedGlobalId, nestedGlobalId.creator,
          pairedReaderCreatorLocalId: pairedReaderCreatorLocalId);
      },
    );

    _log('Assignment: ${globalId.creator}:${globalId.localId} = ${_formatTerm(value)}');

    // Apply to context with full global ID for V_p lookup
    context.handleAssignment(globalId.creator, globalId.localId, value);
  }
  
  void _handleReadRequest(OutboundMessage msg, String requester) {
    // Extract varId from payload
    final serializer = PayloadSerializer(agentId);
    final varId = serializer.deserializeReadRequestPayload(msg.payload);
    
    _log('ReadRequest: var $varId from $requester');
    
    // Apply to context
    context.handleReadRequest(varId, requester);
  }
  
  void _handleAbandon(OutboundMessage msg) {
    // Extract varId from payload
    final serializer = PayloadSerializer(agentId);
    final varId = serializer.deserializeAbandonPayload(msg.payload);
    
    _log('Abandon: var $varId');
    
    // Apply to context
    context.handleAbandon(varId);
  }
  
  /// Flush all pending messages to coordinator
  /// Call this after agent reaches quiescence
  int flushMessages() {
    return context.flushMessages();
  }
  
  /// Export a term for sending to another agent
  /// Registers local variables in V_p and returns term with global IDs
  Term exportTerm(Term term) {
    return context.exportTerm(term);
  }
  
  /// Import a term received from another agent
  /// Registers remote variables in V_p
  void importTerm(Term term, String sender) {
    context.importTerm(term, sender);
  }
  
  /// Register a writer variable for export
  void registerWriter(int varId) {
    context.registerWriter(varId);
  }
  
  /// Register a created reader for export
  void registerCreatedReader(int varId) {
    context.registerCreatedReader(varId);
  }
  
  /// Register an imported writer (e.g., from introduction protocol)
  void registerImportedWriter(int varId, String creator) {
    context.registerImportedWriter(varId, creator);
  }
  
  /// Get current V_p state for debugging
  VariableTable get vp => context.vp;
  
  /// Get current M_p state for debugging
  MessageQueue get mp => context.mp;
  
  void _log(String message) {
    onLog?.call('[$agentId] $message');
  }
  
  String _formatTerm(Term term) {
    if (term is ConstTerm) {
      return term.value?.toString() ?? 'nil';
    } else if (term is VarRef) {
      // Per irmaGLP-spec.md Section 3.2.1: use heap to check isReader
      final isReaderVar = context.runtime.heap.isReader(term.addr);
      return isReaderVar ? 'X${term.addr}?' : 'X${term.addr}';
    } else if (term is StructTerm) {
      final args = term.args.map(_formatTerm).join(', ');
      return '${term.functor}($args)';
    }
    return term.toString();
  }
}
