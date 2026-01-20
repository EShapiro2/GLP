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
import 'package:glp_runtime/runtime/suspension.dart';
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
    final key = VarKey(varId, false); // writer
    // Add to V_p as createdWriter
    vp.add(key, VariableEntry(
      varId: varId,
      isReader: false,
      creator: agentId,
      role: VariableRole.createdWriter,
    ));
    
    // Register heap callback to observe when this writer is bound
    runtime.heap.onBind(varId, (Term value) {
      _onWriterBound(varId, value);
    });
  }
  
  /// Called when a writer in V_p is bound to a value
  /// 
  /// Phase 6: For imported writers with heap-attached entries, also stores
  /// value in entry.state so derefAddr() can return it.
  void _onWriterBound(int writerId, Term value) {
    print('[DEBUG IRMA $agentId] _onWriterBound: writerId=$writerId, value=$value');
    final key = VarKey(writerId, false); // writer
    final entry = vp.lookup(key);
    if (entry == null) {
      print('[DEBUG IRMA $agentId] _onWriterBound: NO ENTRY in V_p for $writerId');
      return;
    }
    print('[DEBUG IRMA $agentId] _onWriterBound: entry.role=${entry.role}, entry.state=${entry.state}, entry.creator=${entry.creator}');
    
    if (entry.role == VariableRole.createdWriter && entry.state != null) {
      // Created writer has a requester - send assignment directly
      final requester = entry.state as String;
      print('[DEBUG IRMA $agentId] _onWriterBound: CREATED WRITER with requester=$requester, sending assignment');
      _queueAssignmentFromEntry(entry, value, requester);
      // Update entry state to store the value (Phase 6: updates both V_p and heap cell)
      entry.state = value;
      vp.updateState(key, value);
    } else if (entry.role == VariableRole.importedWriter) {
      // Imported writer - notify creator (creator routes to requester)
      print('[DEBUG IRMA $agentId] _onWriterBound: IMPORTED WRITER, notifying creator=${entry.creator}');
      _queueAssignmentFromEntry(entry, value, entry.creator);
      // Update entry state to store the value (Phase 6: updates both V_p and heap cell)
      entry.state = value;
      vp.updateState(key, value);
    } else {
      print('[DEBUG IRMA $agentId] _onWriterBound: NO ACTION (role=${entry.role}, state=${entry.state})');
    }
  }
  
  /// Register a created reader in V_p
  /// 
  /// A created reader is one we created locally but exported to another agent.
  /// When the paired writer (also local) is bound, we need to send the value
  /// to whoever requested this reader.
  void registerCreatedReader(int varId) {
    final key = VarKey(varId, true); // reader
    vp.add(key, VariableEntry(
      varId: varId,
      isReader: true,
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
  /// 
  /// [varId] - Our local heap ID for this variable
  /// [creator] - The agent who created this variable
  /// [creatorLocalId] - The creator's original local ID for this variable
  void registerImportedWriter(int varId, String creator, {int? creatorLocalId}) {
    print('[DEBUG IRMA $agentId] registerImportedWriter: varId=$varId, creator=$creator, creatorLocalId=${creatorLocalId ?? varId}');
    final key = VarKey(varId, false); // writer
    vp.add(key, VariableEntry(
      varId: varId,
      isReader: false,
      creator: creator,
      role: VariableRole.importedWriter,
      creatorLocalId: creatorLocalId ?? varId,
    ));
    
    // Register heap callback to notify creator when bound
    runtime.heap.onBind(varId, (Term value) {
      print('[DEBUG IRMA $agentId] HEAP CALLBACK fired for imported writer $varId, value=$value');
      _onWriterBound(varId, value);
    });
  }
  
  /// Called when the writer paired with a created reader is bound
  void _onCreatedReaderWriterBound(int varId, Term value) {
    final key = VarKey(varId, true); // reader
    final entry = vp.lookup(key);
    if (entry == null) return;
    
    if (entry.role == VariableRole.createdReader && entry.state != null) {
      // Someone requested this reader - send them the value
      final requester = entry.state as String;
      _queueAssignmentFromEntry(entry, value, requester);
    }
  }
  
  // =========================================================================
  // Message Queuing
  // =========================================================================
  
  /// Queue an assignment message for a remote reader
  /// 
  /// Uses the creator's local ID (creatorLocalId) in the global ID format,
  /// not our local varId. This ensures the creator can look it up in their V_p.
  void _queueAssignmentFromEntry(VariableEntry entry, Term value, String destination) {
    // Use creator's local ID for the global variable ID
    final creatorLocalId = entry.creatorLocalId;
    final creator = entry.creator;
    
    print('[DEBUG IRMA $agentId] _queueAssignment: varId=${entry.varId}, creatorLocalId=$creatorLocalId, creator=$creator, value=$value, destination=$destination');
    
    // Create assignment payload with proper global ID
    final globalIdSerializer = PayloadSerializer(creator);
    final payload = globalIdSerializer.createAssignmentPayload(creatorLocalId, value);
    
    mp.add(OutboundMessage(
      destination: destination,
      type: MessageType.assignment,
      payload: payload,
    ));
    print('[DEBUG IRMA $agentId] _queueAssignment: message queued, mp.totalLength=${mp.totalLength}');
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
  /// 
  /// This version uses heap dereference to get VariableEntry directly
  /// instead of V_p lookup, per Phase 4 of implementation plan.
  void processSuspension(Set<int> blockingReaders) {
    for (final readerId in blockingReaders) {
      _requestFromHeap(readerId);
    }
  }
  
  /// Send read request for an imported reader using heap-based lookup
  /// 
  /// Dereferences the reader's heap cell to get the VariableEntry directly,
  /// eliminating the need for V_p lookup during request routing.
  /// 
  /// Falls back to legacy V_p lookup for variables created via legacy importTerm.
  void _requestFromHeap(int readerAddr) {
    // Check if this address exists in the heap
    // (Legacy importTerm doesn't allocate heap cells, so we need to fall back)
    if (readerAddr >= runtime.heap.cells.length) {
      print('[DEBUG IRMA $agentId] _requestFromHeap: addr $readerAddr out of bounds, using legacy request');
      helpers.request(readerAddr, agentId, vp, mp);
      return;
    }
    
    // Dereference to get either Term (if bound) or VariableEntry (if imported unbound)
    final result = runtime.heap.derefAddr(readerAddr);
    
    if (result is VariableEntry) {
      final entry = result;
      
      // Only send request for imported readers that haven't been requested yet
      if (entry.role == VariableRole.importedReader && 
          entry.creator != agentId && 
          entry.state == null) {
        print('[DEBUG IRMA $agentId] _requestFromHeap: sending request for reader $readerAddr to ${entry.creator}');
        
        // Update state to mark request sent (in both V_p and heap cell)
        final readerKey = VarKey(entry.varId, true);
        vp.updateState(readerKey, entry.creator);
        entry.state = entry.creator;  // Also update the heap cell's entry
        
        // Queue read request message using creator's ID namespace
        final creatorSerializer = PayloadSerializer(entry.creator);
        final payload = creatorSerializer.createReadRequestPayload(
          entry.creatorLocalId,  // Use creator's original ID
          agentId,
        );
        mp.add(OutboundMessage(
          destination: entry.creator,
          type: MessageType.readRequest,
          payload: payload,
        ));
      } else {
        print('[DEBUG IRMA $agentId] _requestFromHeap: skipping reader $readerAddr (role=${entry.role}, state=${entry.state})');
      }
    } else if (result is VarRef) {
      // Local unbound variable - use legacy V_p lookup
      print('[DEBUG IRMA $agentId] _requestFromHeap: local unbound VarRef, using legacy request');
      helpers.request(readerAddr, agentId, vp, mp);
    } else {
      // Already bound - no request needed
      print('[DEBUG IRMA $agentId] _requestFromHeap: reader $readerAddr already bound to $result');
    }
  }
  
  // =========================================================================
  // Message Flushing
  // =========================================================================
  
  /// Flush all pending messages via callback
  /// 
  /// Returns number of messages flushed.
  int flushMessages() {
    if (onMessageReady == null) {
      print('[DEBUG IRMA $agentId] flushMessages: NO CALLBACK SET');
      return 0;
    }
    
    print('[DEBUG IRMA $agentId] flushMessages: mp.totalLength=${mp.totalLength}, destinations=${mp.destinations}');
    int count = 0;
    for (final destination in mp.destinations) {
      while (true) {
        final msg = mp.poll(destination);
        if (msg == null) break;
        print('[DEBUG IRMA $agentId] flushMessages: sending ${msg.type} to $destination');
        onMessageReady!(destination, msg);
        count++;
      }
    }
    print('[DEBUG IRMA $agentId] flushMessages: flushed $count messages');
    return count;
  }
  
  // =========================================================================
  // Term Import/Export
  // =========================================================================

  /// Deserialize and import a term received from another agent
  /// 
  /// This is the preferred method for receiving terms from other agents.
  /// It uses the new heap allocators to create single-cell representations
  /// for imported variables, with VariableEntry attached directly to the cell.
  /// 
  /// [payload] - The serialized term bytes
  /// [fromAgent] - The agent who sent this term (usually the creator)
  /// 
  /// Returns the deserialized term with local variable IDs.
  Term deserializeAndImportTerm(List<int> payload, String fromAgent) {
    final (term, globalIdMapping) = PayloadSerializer.deserializeAgentMessagePayloadWithMapping(
      payload,
      // Allocator callback: create single-cell for imported variable
      (bool isReader) {
        if (isReader) {
          return runtime.heap.allocateImportedReader();
        } else {
          return runtime.heap.allocateImportedWriter();
        }
      },
      // Entry creator callback: create and attach VariableEntry to cell
      onVariableImported: (int localAddr, bool isReader, GlobalVarId globalId) {
        _attachImportedVariableEntry(localAddr, isReader, globalId, fromAgent);
      },
    );
    
    return term;
  }
  
  /// Attach a VariableEntry to an imported variable's heap cell
  /// 
  /// Creates the entry and stores it both in V_p and in the heap cell content.
  void _attachImportedVariableEntry(
    int localAddr, 
    bool isReader, 
    GlobalVarId globalId,
    String fromAgent,
  ) {
    final creator = globalId.creator;
    final creatorLocalId = globalId.localId;
    
    final entry = VariableEntry(
      varId: localAddr,
      isReader: isReader,
      creator: creator,
      role: isReader ? VariableRole.importedReader : VariableRole.importedWriter,
      creatorLocalId: creatorLocalId,
    );
    
    // Add to V_p
    final key = VarKey(localAddr, isReader);
    vp.add(key, entry);
    
    // Attach entry to heap cell content
    runtime.heap.cells[localAddr].content = entry;
    
    print('[DEBUG IRMA $agentId] _attachImportedVariableEntry: localAddr=$localAddr, isReader=$isReader, creator=$creator, creatorLocalId=$creatorLocalId');
    
    // For imported writers, register binding callback
    if (!isReader) {
      runtime.heap.onBind(localAddr, (Term value) {
        print('[DEBUG IRMA $agentId] HEAP CALLBACK fired for imported writer $localAddr, value=$value');
        _onWriterBound(localAddr, value);
      });
    }
  }

  /// Import a term received from another agent (legacy method)
  ///
  /// For each variable Y in term where (Y, ·, ·) ∉ V_p:
  /// - Add to V_p as imported reader or writer based on VarRef.isReader
  /// 
  /// [term] - The deserialized term
  /// [fromAgent] - The agent who sent this term (usually the creator)
  /// [globalIdMapping] - Optional mapping from local varId -> GlobalVarId
  ///   This maps our local heap IDs to the creator's global IDs.
  void importTerm(Term term, String fromAgent, {Map<int, GlobalVarId>? globalIdMapping}) {
    _importTermRecursive(term, fromAgent, globalIdMapping ?? {});
  }
  
  void _importTermRecursive(Term term, String fromAgent, Map<int, GlobalVarId> globalIdMapping) {
    if (term is VarRef) {
      final key = VarKey(term.varId, term.isReader);
      if (!vp.contains(key)) {
        // Look up the global ID for this variable
        final globalId = globalIdMapping[term.varId];
        final creator = globalId?.creator ?? fromAgent;
        final creatorLocalId = globalId?.localId;
        
        // Variable not in V_p - add based on type
        if (term.isReader) {
          print('[DEBUG IRMA $agentId] _importTermRecursive: registering imported reader ${term.varId} from $creator (creatorLocalId=$creatorLocalId)');
          vp.add(key, VariableEntry(
            varId: term.varId,
            isReader: true,
            creator: creator,
            role: VariableRole.importedReader,
            creatorLocalId: creatorLocalId,
          ));
          // NOTE: Do NOT send request here. Per spec section 5.2, request() is
          // called when a goal SUSPENDS on this reader, not at import time.
        } else {
          // Imported writer - register with callback
          registerImportedWriter(term.varId, creator, creatorLocalId: creatorLocalId);
        }
      }
    } else if (term is StructTerm) {
      for (final arg in term.args) {
        _importTermRecursive(arg, fromAgent, globalIdMapping);
      }
    }
  }
  
  /// Export a term being sent to another agent
  /// 
  /// For each local variable in term:
  /// - Add to V_p and register binding callback
  /// 
  /// For requested readers being re-exported:
  /// - Create relay pair (Z, Z?) per spec Section 4.3
  /// - Set up forwarding callback: when Y? is bound, bind Z
  /// - This implements: export_reader(Y?, Z) :- Z = Y?.
  /// 
  /// Returns modified term (with relay variables substituted if needed).
  Term exportTerm(Term term) {
    // For relay handling, use the helpers.export method
    final relaySetups = <RelaySetup>[];
    final result = helpers.export(
      term,
      agentId,
      vp,
      relaySetups,
      (_, __) {
        final (writerAddr, readerAddr) = runtime.heap.allocateVariable();
        return [writerAddr, readerAddr];
      },
    );
    
    // Set up relay forwarding callbacks
    // This implements: export_reader(Y?, Z) :- Z = Y?.
    for (final relay in relaySetups) {
      _setupRelayForwarding(relay);
    }
    
    return result.term;
  }
  
  /// Set up forwarding callback for a relay
  /// 
  /// When the original reader (Y?) receives a value, bind the relay writer (Z)
  /// to the same value. This propagates the value to whoever holds Z?.
  /// 
  /// Implements: export_reader(Y?, Z) :- Z = Y?.
  void _setupRelayForwarding(RelaySetup relay) {
    print('[DEBUG IRMA $agentId] _setupRelayForwarding: Y?=${relay.originalReaderId} -> Z=${relay.relayWriterId}, Z?=${relay.relayReaderId}');
    
    // Register callback: when Y? is bound, bind Z to same value
    runtime.heap.onBind(relay.originalReaderId, (Term value) {
      print('[DEBUG IRMA $agentId] RELAY FORWARD: Y?=${relay.originalReaderId} bound to $value, binding Z=${relay.relayWriterId}');
      
      // Bind the relay writer Z to the same value
      // This will trigger _onWriterBound if Z has a requester
      final activations = runtime.heap.bindVariable(relay.relayWriterId, value);
      for (final act in activations) {
        runtime.gq.enqueue(act);
      }
    });
    
    // Also register the relay writer's callback for message routing
    // (The relay writer Z is in V_p and needs to send assignments when bound)
    runtime.heap.onBind(relay.relayWriterId, (Term value) {
      _onWriterBound(relay.relayWriterId, value);
    });
  }
  
  // =========================================================================
  // Incoming Message Handlers
  // =========================================================================
  
  /// Handle incoming assignment message
  /// 
  /// Called by coordinator when (X?:=T) arrives from another agent.
  /// Per spec Section 5.3 Type 1, assignments are always for readers (X?).
  /// 
  /// The assignment contains a global ID (creator:creatorLocalId) which we
  /// must translate to our local varId via V_p lookup.
  /// 
  /// Cases:
  /// 1. Imported reader found in V_p → translate to local varId, apply
  /// 2. Created reader with pending request → forward to requester
  /// 3. Created reader, no request yet → store value
  /// 4. Not in V_p but we're creator → local variable, apply directly
  /// 
  /// Phase 5: For imported readers with heap-attached entries, stores value
  /// in entry.state so derefAddr() can return it.
  void handleAssignment(String creator, int creatorLocalId, Term value) {
    print('[DEBUG IRMA $agentId] handleAssignment: creator=$creator, creatorLocalId=$creatorLocalId, value=$value');
    
    // Search V_p for entry matching this global ID
    final entry = vp.findByCreatorLocalId(creator, creatorLocalId, isReader: true);
    print('[DEBUG IRMA $agentId] handleAssignment: entry=${entry?.role}, localVarId=${entry?.varId}, state=${entry?.state}');
    
    if (entry != null) {
      if (entry.role == VariableRole.importedReader) {
        // Imported reader - store value in entry and activate suspensions
        print('[DEBUG IRMA $agentId] handleAssignment: IMPORTED READER - binding local varId=${entry.varId}');
        
        // Phase 5: Store value in entry.state (updates both V_p and heap cell
        // since they share the same VariableEntry object)
        entry.state = value;
        
        // Activate suspensions from VariableEntry ("virtual writer")
        // Per irmaGLP spec Section 3.1.2: V_p serves as virtual writer for imported readers
        final activations = _activateSuspensionsFromEntry(entry);
        print('[DEBUG IRMA $agentId] handleAssignment: activated ${activations.length} goals from VariableEntry');
        for (final act in activations) {
          runtime.gq.enqueue(act);
        }
        
        // Also bind on heap for normal variable semantics (may return 0 activations
        // since suspensions were in VariableEntry, not heap cell)
        final heapActivations = runtime.heap.bindVariable(entry.varId, value);
        print('[DEBUG IRMA $agentId] handleAssignment: bindVariable returned ${heapActivations.length} heap activations');
        for (final act in heapActivations) {
          runtime.gq.enqueue(act);
        }
        vp.remove(entry.key);
      } else if (entry.role == VariableRole.createdReader) {
        // We created this reader - check for pending requester
        final storedState = entry.state;
        
        if (storedState != null && storedState is String) {
          // Created reader with pending request - forward to requester
          print('[DEBUG IRMA $agentId] handleAssignment: CREATED READER with requester=$storedState, forwarding');
          _queueAssignmentFromEntry(entry, value, storedState);
          entry.state = value;  // Phase 5: Update entry directly
          vp.updateState(entry.key, value);
        } else {
          // No requester yet - store value
          print('[DEBUG IRMA $agentId] handleAssignment: CREATED READER, no requester yet, storing value');
          entry.state = value;  // Phase 5: Update entry directly
          vp.updateState(entry.key, value);
        }
      } else {
        print('[DEBUG IRMA $agentId] handleAssignment: UNHANDLED ROLE - ${entry.role}');
      }
    } else if (creator == agentId) {
      // Not in V_p, but we created it - local variable, apply directly
      print('[DEBUG IRMA $agentId] handleAssignment: LOCAL (not in V_p) - binding varId=$creatorLocalId');
      final activations = runtime.heap.bindVariable(creatorLocalId, value);
      for (final act in activations) {
        runtime.gq.enqueue(act);
      }
    } else {
      // Not in V_p and we didn't create it - should not happen
      print('[DEBUG IRMA $agentId] handleAssignment: ERROR - no entry for $creator:$creatorLocalId and we are not creator');
    }
  }
  
  /// Handle incoming read request message
  /// 
  /// Called by coordinator when request(X?, requester) arrives.
  /// 
  /// Per spec Section 5.3 Type 2:
  /// - If (X?, q, T) ∈ V_q where T ∈ 𝒯 → reply immediately with stored value
  /// - Else if (X?, q, ⊥) ∈ V_q → record requester
  /// - Else if (X, q, T) ∈ V_q → reply with writer's value (direct communication case)
  /// 
  /// Note: varId is the creator's local ID (creatorLocalId), not necessarily our local varId.
  /// We must use findByCreatorLocalId to look up entries, matching handleAssignment's approach.
  void handleReadRequest(int varId, String requester) {
    print('[DEBUG IRMA $agentId] handleReadRequest: varId=$varId, requester=$requester');
    
    // First check reader entry - use findByCreatorLocalId since varId is creatorLocalId
    // Note: For created readers, we are the creator, so use agentId
    final readerEntry = vp.findByCreatorLocalId(agentId, varId, isReader: true);
    
    if (readerEntry != null) {
      print('[DEBUG IRMA $agentId] handleReadRequest: found reader entry, role=${readerEntry.role}, state=${readerEntry.state}');
      
      if (readerEntry.role == VariableRole.createdReader) {
        final storedState = readerEntry.state;
        if (storedState != null && storedState is Term) {
          // Value already stored - reply immediately
          print('[DEBUG IRMA $agentId] handleReadRequest: created reader has value, replying immediately');
          _queueAssignmentFromEntry(readerEntry, storedState, requester);
        } else if (storedState == null) {
          // No request yet - record requester
          print('[DEBUG IRMA $agentId] handleReadRequest: created reader, recording requester=$requester');
          readerEntry.state = requester;  // Update entry directly (same object in V_p)
          vp.updateState(readerEntry.key, requester);
        } else {
          print('[DEBUG IRMA $agentId] handleReadRequest: created reader, already has requester=$storedState, ignoring');
        }
        return;
      }
    }
    
    // Check writer entry (direct communication case per spec)
    // Use findByCreatorLocalId for consistency with reader lookup
    final writerEntry = vp.findByCreatorLocalId(agentId, varId, isReader: false);
    
    if (writerEntry != null && writerEntry.role == VariableRole.createdWriter) {
      print('[DEBUG IRMA $agentId] handleReadRequest: found writer entry, state=${writerEntry.state}');
      
      // Check if variable is already bound in heap
      // Phase 3: Use isWriterBound instead of varTable (varId == writerAddr)
      Term? value;
      if (runtime.heap.isWriterBound(varId)) {
        value = runtime.heap.getValue(varId);
      }
      print('[DEBUG IRMA $agentId] handleReadRequest: created writer, heap value=$value');
      
      if (value != null) {
        // Already bound - send value immediately
        print('[DEBUG IRMA $agentId] handleReadRequest: writer already bound, sending immediately');
        _queueAssignmentFromEntry(writerEntry, value, requester);
      } else {
        // Not yet bound - record requester
        print('[DEBUG IRMA $agentId] handleReadRequest: writer not bound, recording requester=$requester');
        writerEntry.state = requester;  // Update entry directly
        vp.updateState(writerEntry.key, requester);
      }
      return;
    }
    
    print('[DEBUG IRMA $agentId] handleReadRequest: NO ENTRY in V_p for reader or writer');
  }
  
  /// Handle incoming abandon notification
  /// 
  /// Called by coordinator when abandon(Y) arrives.
  void handleAbandon(int varId) {
    // Remove both reader and writer entries if present
    vp.remove(VarKey(varId, true));
    vp.remove(VarKey(varId, false));
    
    // Remove any pending bind callback
    runtime.heap.removeBindCallback(varId);
    
    // TODO: Reactivate any goals suspended on this variable
    // (They will fail since the remote counterpart is gone)
  }
  
  // =========================================================================
  // Private Helpers
  // =========================================================================
  
  /// Activate suspensions from a VariableEntry ("virtual writer")
  /// 
  /// Walks the suspension list in the entry, activates armed records,
  /// and returns GoalRefs for reactivation.
  /// 
  /// Per irmaGLP spec Section 3.1.2: For imported readers, V_p serves as
  /// the virtual writer that holds suspensions.
  List<GoalRef> _activateSuspensionsFromEntry(VariableEntry entry) {
    final activations = <GoalRef>[];
    var current = entry.suspensions;
    
    while (current != null) {
      if (current.armed) {
        activations.add(GoalRef(current.goalId!, current.resumePC));
        current.record.disarm();
      }
      current = current.next;
    }
    
    // Clear the suspension list
    entry.suspensions = null;
    
    return activations;
  }
  
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
      
      final readerKey = VarKey(varId, true);
      final vpEntry = vp.lookup(readerKey);
      if (vpEntry == null) continue;
      
      if (vpEntry.role == VariableRole.createdReader && 
          vpEntry.creator == agentId &&
          vpEntry.state != null) {
        final requester = vpEntry.state as String;
        _queueAssignmentFromEntry(vpEntry, value, requester);
      }
      else if (vpEntry.role == VariableRole.importedReader &&
               vpEntry.creator != agentId &&
               vpEntry.state == null) {
        vp.updateState(readerKey, vpEntry.creator);
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
