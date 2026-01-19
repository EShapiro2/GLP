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
  late final PayloadSerializer _serializer;
  
  /// Optional callback for message delivery (set by coordinator)
  MessageDeliveryCallback? onMessageReady;
  
  IrmaContext({
    required this.agentId,
    required this.runtime,
  }) : vp = VariableTable(agentId),
       mp = MessageQueue(),
       helpers = IrmaHelpers(agentId) {
    _serializer = PayloadSerializer(agentId);
  }
  
  // =========================================================================
  // Writer Binding Observation (Heap Callback Approach)
  // =========================================================================
  
  /// Register a writer in V_p and set up binding callback
  /// 
  /// When this writer is bound, the callback will:
  /// 1. Check if there's a requester for the paired reader
  /// 2. If so, queue an assignment message to the requester
  void registerWriter(int varId) {
    // Add to V_p as createdWriter
    vp.add(varId, VariableEntry(
      varId: varId,
      creator: agentId,
      role: VariableRole.createdWriter,
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
    
    if (entry.role == VariableRole.createdWriter && entry.state != null) {
      // Created writer has a requester - send assignment directly
      final requester = entry.state as String;
      _queueAssignment(writerId, value, requester);
      // Update entry state to store the value
      vp.updateState(writerId, value);
    } else if (entry.role == VariableRole.importedWriter) {
      // Imported writer - notify creator (creator routes to requester)
      _queueAssignment(writerId, value, entry.creator);
      // Update entry state to store the value
      vp.updateState(writerId, value);
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
  
  /// Register an imported writer in V_p
  /// 
  /// An imported writer is one created by another agent but transferred to us
  /// (e.g., via friend introduction). When we bind it, we notify the creator.
  void registerImportedWriter(int varId, String creator) {
    vp.add(varId, VariableEntry(
      varId: varId,
      creator: creator,
      role: VariableRole.importedWriter,
    ));
    
    // Register heap callback to notify creator when bound
    runtime.heap.onBind(varId, (Term value) {
      _onWriterBound(varId, value);
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
  /// - Add to V_p as imported reader or writer based on VarRef.isReader
  void importTerm(Term term, String fromAgent) {
    _importTermRecursive(term, fromAgent);
  }
  
  void _importTermRecursive(Term term, String fromAgent) {
    if (term is VarRef) {
      if (!vp.contains(term.varId)) {
        // Variable not in V_p - add based on type
        if (term.isReader) {
          vp.add(term.varId, VariableEntry(
            varId: term.varId,
            creator: fromAgent,
            role: VariableRole.importedReader,
          ));
        } else {
          // Imported writer - register with callback
          registerImportedWriter(term.varId, fromAgent);
        }
      }
    } else if (term is StructTerm) {
      for (final arg in term.args) {
        _importTermRecursive(arg, fromAgent);
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
  /// 
  /// Three cases per spec:
  /// 1. Reader is local (not in V_p) → apply directly
  /// 2. Created reader with pending request → forward to requester, store value
  /// 3. Created reader, no request yet → store value for later
  void handleAssignment(int varId, Term value) {
    final entry = vp.lookup(varId);
    
    if (entry == null) {
      // Case 1: Reader is local (not in V_p) - apply directly
      final activations = runtime.heap.bindVariable(varId, value);
      for (final act in activations) {
        runtime.gq.enqueue(act);
      }
    } else if (entry.role == VariableRole.createdReader) {
      // This agent created the reader - check for pending requester
      final storedState = entry.state;
      
      if (storedState != null && storedState is String) {
        // Case 2: Created reader with pending request - forward to requester
        _queueAssignment(varId, value, storedState);
        // Update entry to store the value
        vp.updateState(varId, value);
      } else {
        // Case 3: Created reader, no request yet - store value
        vp.updateState(varId, value);
      }
    } else if (entry.role == VariableRole.importedReader) {
      // We imported this reader - apply directly and remove from V_p
      final activations = runtime.heap.bindVariable(varId, value);
      for (final act in activations) {
        runtime.gq.enqueue(act);
      }
      vp.remove(varId);
    }
  }
  
  /// Handle incoming read request message
  /// 
  /// Called by coordinator when request(X?, requester) arrives.
  /// 
  /// Per spec:
  /// - If value already stored → reply immediately
  /// - Else if created reader with no request → record requester
  /// - Else if created writer → reply if bound, else record requester
  void handleReadRequest(int varId, String requester) {
    final entry = vp.lookup(varId);
    if (entry == null) return;
    
    final storedState = entry.state;
    
    if (entry.role == VariableRole.createdReader) {
      if (storedState != null && storedState is Term) {
        // Value already stored - reply immediately
        _queueAssignment(varId, storedState, requester);
      } else if (storedState == null) {
        // No request yet - record requester
        vp.updateState(varId, requester);
      }
      // If storedState is a String (another requester), ignore duplicate
    } else if (entry.role == VariableRole.createdWriter) {
      // Check if variable is already bound in heap
      Term? value;
      if (runtime.heap.varTable.containsKey(varId)) {
        value = runtime.heap.getValue(varId);
      }
      
      if (value != null) {
        // Already bound - send value immediately
        _queueAssignment(varId, value, requester);
      } else {
        // Not yet bound - record requester
        vp.updateState(varId, requester);
      }
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
