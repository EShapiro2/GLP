/// irmaGLP Agent Context
/// 
/// Extends GLP runtime with V_p (Variable Table) and M_p (Message Queue)
/// for multiagent communication.
/// 
/// Specification: /docs/ma/irmaGLP-spec.md
/// 
/// Integration approach: Uses heap onBind callbacks to observe variable bindings.
/// When a writer in V_p is bound, the callback queues assignment messages.
/// This decouples the GLP runtime from network transport.
library;

import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/multiagent/variable_table.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';
import 'package:glp_runtime/multiagent/helpers.dart';
import 'package:glp_runtime/multiagent/payload_serializer.dart';

/// Callback for delivering messages to other agents
typedef MessageDeliveryCallback = void Function(String destination, OutboundMessage message);

/// irmaGLP Agent Context
/// 
/// Wraps a GlpRuntime with V_p and M_p for multiagent operation.
/// Each agent has its own IrmaContext with unique agentId.
/// 
/// Uses heap onBind callbacks to observe variable bindings and trigger
/// message queuing automatically, without modifying the GLP runtime.
class IrmaContext {
  /// Unique identifier for this agent (e.g., "alice", "bob")
  final String agentId;
  
  /// Underlying GLP runtime
  final GlpRuntime runtime;
  
  /// Variable table V_p: tracks variables with non-local counterparts
  final VariableTable vp;
  
  /// Message queue M_p: outbound messages to other agents
  final MessageQueue mp;
  
  /// Helper routines for irmaGLP transactions
  final IrmaHelpers helpers;
  
  /// Payload serializer for message encoding
  final PayloadSerializer _serializer = PayloadSerializer();
  
  /// Optional callback for message delivery (set by coordinator)
  MessageDeliveryCallback? onMessageReady;
  
  IrmaContext({
    required this.agentId,
    required this.runtime,
  }) : vp = VariableTable(agentId),
       mp = MessageQueue(),
       helpers = IrmaHelpers();
  
  // =========================================================================
  // Writer Binding Observation (Heap Callback Approach)
  // =========================================================================
  
  /// Register a writer in V_p and set up binding callback
  /// 
  /// When this writer is bound, the callback will:
  /// 1. Check if there's a requester for the paired reader
  /// 2. If so, queue an assignment message to the requester
  void registerWriter(int varId) {
    // Add to V_p as writer
    vp.add(varId, VariableEntry(
      varId: varId,
      creator: agentId,
      role: VariableRole.writer,
    ));
    
    // Register heap callback to observe when this writer is bound
    runtime.heap.onBind(varId, (Term value) {
      _onWriterBound(varId, value);
    });
  }
  
  /// Called when a writer in V_p is bound to a value
  void _onWriterBound(int writerId, Term value) {
    final entry = vp.lookup(writerId);
    if (entry == null) return;
    
    // Check if there's a requester for the paired reader
    // The requester is stored in the reader entry's state field
    // But wait - for writers, we need to find the paired reader entry
    // In our design, writer and reader share the same varId
    // The reader entry would have role=createdReader if we created it
    
    // Actually, when we export a variable:
    // - If we export the writer, we add (varId, agentId, writer) to V_p
    // - The remote agent gets the reader
    // - When remote agent requests, we get a read request message
    // - We record the requester in the writer entry's state
    
    if (entry.role == VariableRole.writer && entry.state != null) {
      // Writer has a requester - send assignment
      final requester = entry.state as String;
      _queueAssignment(writerId, value, requester);
    }
  }
  
  /// Register a created reader in V_p
  /// 
  /// A created reader is one we created locally but exported to another agent.
  /// When the paired writer (also local) is bound, we need to send the value
  /// to whoever requested this reader.
  void registerCreatedReader(int varId) {
    vp.add(varId, VariableEntry(
      varId: varId,
      creator: agentId,
      role: VariableRole.createdReader,
    ));
    
    // Register heap callback on the paired writer
    // When it's bound, send value to requester (if any)
    runtime.heap.onBind(varId, (Term value) {
      _onCreatedReaderWriterBound(varId, value);
    });
  }
  
  /// Called when the writer paired with a created reader is bound
  void _onCreatedReaderWriterBound(int varId, Term value) {
    final entry = vp.lookup(varId);
    if (entry == null) return;
    
    if (entry.role == VariableRole.createdReader && entry.state != null) {
      // Someone requested this reader - send them the value
      final requester = entry.state as String;
      _queueAssignment(varId, value, requester);
    }
  }
  
  // =========================================================================
  // Message Queuing
  // =========================================================================
  
  /// Queue an assignment message for a remote reader
  void _queueAssignment(int varId, Term value, String destination) {
    final payload = _serializer.serializeTerm(value, agentId);
    mp.add(OutboundMessage(
      destination: destination,
      type: MessageType.assignment,
      payload: payload,
    ));
  }
  
  // =========================================================================
  // Abandonment Handling
  // =========================================================================
  
  /// Abandon a reader
  /// 
  /// Called when a reader becomes unreachable in the local computation.
  void abandonReader(int readerId) {
    helpers.abandon(readerId, vp, mp);
  }
  
  /// Handle suspension: send read requests for blocking readers
  /// 
  /// For each X? ∈ W (suspension set):
  /// - Call request(X?) to send read request to creator
  void processSuspension(Set<int> blockingReaders) {
    for (final readerId in blockingReaders) {
      helpers.request(readerId, agentId, vp, mp);
    }
  }
  
  // =========================================================================
  // Message Flushing
  // =========================================================================
  
  /// Flush all pending messages via callback
  /// 
  /// Returns number of messages flushed.
  int flushMessages() {
    if (onMessageReady == null) return 0;
    
    int count = 0;
    for (final destination in mp.destinations) {
      while (true) {
        final msg = mp.poll(destination);
        if (msg == null) break;
        onMessageReady!(destination, msg);
        count++;
      }
    }
    return count;
  }
  
  // =========================================================================
  // Term Import/Export
  // =========================================================================
  
  /// Import a term received from another agent
  /// 
  /// For each variable Y in term where (Y, ·, ·) ∉ V_p:
  /// - Add (Y, creator, ⊥) to V_p as imported reader
  void importTerm(Term term, String fromAgent) {
    final varIds = _extractVariables(term);
    for (final varId in varIds) {
      if (!vp.contains(varId)) {
        // Variable not in V_p - add as imported reader
        vp.add(varId, VariableEntry(
          varId: varId,
          creator: fromAgent,
          role: VariableRole.importedReader,
        ));
      }
    }
  }
  
  /// Export a term being sent to another agent
  /// 
  /// For each local variable in term:
  /// - Add to V_p and register binding callback
  /// 
  /// Returns modified term (with relay variables if needed).
  Term exportTerm(Term term) {
    final varIds = _extractVariables(term);
    
    for (final varId in varIds) {
      if (!vp.contains(varId)) {
        // Local variable being exported for first time
        // Determine if it's a writer or reader based on the term
        // For now, assume we're exporting the writer (reader goes to remote)
        registerWriter(varId);
      }
    }
    
    // For relay handling, use the helpers.export method
    final relayGoals = <GoalRef>[];
    final result = helpers.export(
      term,
      agentId,
      vp,
      relayGoals,
      (_, __) => [runtime.heap.allocateVariable(), runtime.heap.allocateVariable()],
    );
    
    // TODO: Add relay goals to active queue
    for (final goal in relayGoals) {
      // runtime.gq.enqueue(goal); // Need proper PC lookup for relay/2
    }
    
    return result.term;
  }
  
  // =========================================================================
  // Incoming Message Handlers
  // =========================================================================
  
  /// Handle incoming assignment message
  /// 
  /// Called by coordinator when (X?:=T) arrives from another agent.
  void handleAssignment(int varId, Term value) {
    // 1. Apply assignment to heap
    final activations = runtime.heap.bindVariable(varId, value);
    
    // 2. Enqueue reactivated goals
    for (final act in activations) {
      runtime.gq.enqueue(act);
    }
    
    // 3. Remove from V_p (variable is now fully local)
    vp.remove(varId);
    
    // 4. Import variables from value
    // (handled by the coordinator when deserializing)
  }
  
  /// Handle incoming read request message
  /// 
  /// Called by coordinator when request(X?, requester) arrives.
  void handleReadRequest(int varId, String requester) {
    final entry = vp.lookup(varId);
    if (entry == null) return;
    
    // Check if variable exists in heap and is already bound
    Term? value;
    if (runtime.heap.varTable.containsKey(varId)) {
      value = runtime.heap.getValue(varId);
    }
    
    if (value != null) {
      // Already bound - send value immediately
      _queueAssignment(varId, value, requester);
    } else {
      // Not yet bound - record requester
      // When the variable is bound, the onBind callback will send the value
      vp.updateState(varId, requester);
    }
  }
  
  /// Handle incoming abandon notification
  /// 
  /// Called by coordinator when abandon(Y) arrives.
  void handleAbandon(int varId) {
    // Remove from V_p
    vp.remove(varId);
    
    // Remove any pending bind callback
    runtime.heap.removeBindCallback(varId);
    
    // TODO: Reactivate any goals suspended on this variable
    // (They will fail since the remote counterpart is gone)
  }
  
  // =========================================================================
  // Private Helpers
  // =========================================================================
  
  Set<int> _extractVariables(Term term) {
    final result = <int>{};
    _extractVariablesRecursive(term, result);
    return result;
  }
  
  void _extractVariablesRecursive(Term term, Set<int> result) {
    if (term is VarRef) {
      result.add(term.varId);
    } else if (term is StructTerm) {
      for (final arg in term.args) {
        _extractVariablesRecursive(arg, result);
      }
    }
    // ConstTerm has no variables
  }
  
  // =========================================================================
  // Legacy API (for backward compatibility with tests)
  // =========================================================================
  
  /// Process bindings from σ̂? (reader substitution) after Reduce
  /// 
  /// DEPRECATED: Use heap callbacks instead. This is kept for test compatibility.
  void processReaderBindings(Map<int, Term> sigmaHatReader) {
    for (final entry in sigmaHatReader.entries) {
      final varId = entry.key;
      final value = entry.value;
      
      final vpEntry = vp.lookup(varId);
      if (vpEntry == null) continue;
      
      if (vpEntry.role == VariableRole.createdReader && 
          vpEntry.creator == agentId &&
          vpEntry.state != null) {
        final requester = vpEntry.state as String;
        _queueAssignment(varId, value, requester);
      }
      else if (vpEntry.role == VariableRole.importedReader &&
               vpEntry.creator != agentId &&
               vpEntry.state == null) {
        vp.updateState(varId, vpEntry.creator);
      }
    }
  }
  
  /// Detect and handle abandoned readers after reduction
  /// 
  /// DEPRECATED: Use abandonReader() directly.
  void processAbandonedReaders({
    required Set<int> readersInGoal,
    required Set<int> assignedReaders,
    required Set<int> readersInBody,
  }) {
    for (final readerId in readersInGoal) {
      if (!assignedReaders.contains(readerId) && 
          !readersInBody.contains(readerId)) {
        helpers.abandon(readerId, vp, mp);
      }
    }
  }
  
  /// Handle goal failure: abandon all readers in the goal
  /// 
  /// DEPRECATED: Use abandonReader() for each reader.
  void processFailure(Set<int> readersInGoal) {
    for (final readerId in readersInGoal) {
      helpers.abandon(readerId, vp, mp);
    }
  }
}
