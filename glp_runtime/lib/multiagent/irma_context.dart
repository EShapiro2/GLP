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
///
/// The callback receives raw OutboundMessage and must switch on message.type
/// to deserialize appropriately using PayloadSerializer methods:
///
/// ```dart
/// ctx.onMessageReady = (destination, message) {
///   switch (message.type) {
///     case MessageType.assignment:
///       final (globalId, value) = serializer.deserializeAssignmentPayload(message.payload);
///       targetCtx.handleAssignment(globalId.creator, globalId.localId, value);
///     case MessageType.readRequest:
///       final varId = serializer.deserializeReadRequestPayload(message.payload);
///       targetCtx.handleReadRequest(varId, sourceAgentId);
///     case MessageType.abandon:
///       final varId = serializer.deserializeAbandonPayload(message.payload);
///       targetCtx.handleAbandon(varId);
///   }
/// };
/// ```
///
/// Design note: The single callback with message type switching was chosen over
/// separate typed callbacks (onAssignment, onReadRequest, onAbandon) to keep
/// the API simpler and allow coordinators flexibility in message routing.
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

  // =========================================================================
  // Network Stream Tracking (Section 5.4)
  // =========================================================================

  /// Current tail writer of network input stream (for appending incoming messages)
  int? _netInTailWriter;

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
      onWriterBound(varId, value);
    });
  }
  
  /// Called when a writer in V_p is bound to a value
  ///
  /// Phase 6: For imported writers with heap-attached entries, also stores
  /// value in entry.state so derefAddr() can return it.
  void onWriterBound(int writerId, Term value) {
    print('[DEBUG IRMA $agentId] onWriterBound: writerId=$writerId, value=$value');
    final key = VarKey(writerId, false); // writer
    final entry = vp.lookup(key);
    if (entry == null) {
      print('[DEBUG IRMA $agentId] onWriterBound: NO ENTRY in V_p for $writerId');
      return;
    }
    print('[DEBUG IRMA $agentId] onWriterBound: entry.role=${entry.role}, entry.requester=${entry.requester}, entry.creator=${entry.creator}');

    if (entry.role == VariableRole.createdWriter && entry.requester != null) {
      // Created writer has a requester - send assignment directly
      final requester = entry.requester!;
      print('[DEBUG IRMA $agentId] onWriterBound: CREATED WRITER with requester=$requester, sending assignment');
      _queueAssignmentFromEntry(entry, value, requester);
      // Update entry to store the bound value
      entry.boundValue = value;
      vp.updateBoundValue(key, value);
    } else if (entry.role == VariableRole.importedWriter) {
      // Imported writer - notify creator (creator routes to requester)
      print('[DEBUG IRMA $agentId] onWriterBound: IMPORTED WRITER, notifying creator=${entry.creator}');
      _queueAssignmentFromEntry(entry, value, entry.creator);
      // Update entry to store the bound value
      entry.boundValue = value;
      vp.updateBoundValue(key, value);
    } else {
      print('[DEBUG IRMA $agentId] onWriterBound: NO ACTION (role=${entry.role}, requester=${entry.requester})');
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
      onWriterBound(varId, value);
    });
  }
  
  /// Called when the writer paired with a created reader is bound
  void _onCreatedReaderWriterBound(int varId, Term value) {
    final key = VarKey(varId, true); // reader
    final entry = vp.lookup(key);
    if (entry == null) return;

    if (entry.role == VariableRole.createdReader && entry.requester != null) {
      // Someone requested this reader - send them the value
      final requester = entry.requester!;
      _queueAssignmentFromEntry(entry, value, requester);
    }
  }

  // =========================================================================
  // Network Transaction (Section 5.4)
  // =========================================================================

  /// Register network input stream for receiving messages
  ///
  /// [netInTailWriter] is the writer address of the current tail of the
  /// network input stream. When messages arrive, they are appended here.
  void registerNetworkInput(int netInTailWriter) {
    _netInTailWriter = netInTailWriter;
    print('[DEBUG IRMA $agentId] registerNetworkInput: tail=$netInTailWriter');
  }

  /// Register network output stream for monitoring
  ///
  /// Per spec Section 5.4: When msg(destination, content) appears on the
  /// network output stream, trigger the Network Transaction.
  ///
  /// [netOutWriter] is the writer address of the network output stream.
  void registerNetworkOutput(int netOutWriter) {
    print('[DEBUG IRMA $agentId] registerNetworkOutput: writer=$netOutWriter');
    runtime.heap.onBind(netOutWriter, (Term term) {
      _processNetworkOutput(term);
    });
  }

  /// Process network output when stream is bound
  ///
  /// Checks if term is a cons cell [msg(Dest, X) | Rest] and triggers
  /// Network Transaction for each message found.
  void _processNetworkOutput(Term term) {
    print('[DEBUG IRMA $agentId] _processNetworkOutput: term=$term');

    // Dereference if VarRef
    if (term is VarRef) {
      final derefed = runtime.heap.derefAddr(term.addr);
      if (derefed is Term) {
        term = derefed;
      } else {
        print('[DEBUG IRMA $agentId] _processNetworkOutput: unbound VarRef, ignoring');
        return;
      }
    }

    // Check for cons cell: .(Head, Tail)
    if (term is StructTerm && term.functor == '.' && term.args.length == 2) {
      var head = term.args[0];
      final tail = term.args[1];

      // Dereference head if needed
      if (head is VarRef) {
        final derefedHead = runtime.heap.derefAddr(head.addr);
        if (derefedHead is VarRef) {
          // Head is still unbound - register callback for when it gets bound
          // This happens when the cons cell is created before its elements are bound
          final headAddr = (derefedHead as VarRef).addr;
          print('[DEBUG IRMA $agentId] _processNetworkOutput: head is unbound at $headAddr, registering callback');
          runtime.heap.onBind(headAddr, (Term boundHead) {
            print('[DEBUG IRMA $agentId] _processNetworkOutput: head now bound to $boundHead');
            _processNetworkOutputMessage(boundHead, tail);
          });
          // Continue monitoring tail separately
          _monitorTail(tail);
          return;
        } else if (derefedHead is Term) {
          head = derefedHead;
        }
      }

      // Check if head is msg(Dest, Content)
      if (head is StructTerm && head.functor == 'msg' && head.args.length == 2) {
        var dest = head.args[0];
        final content = head.args[1];

        // Dereference dest if needed
        if (dest is VarRef) {
          final derefedDest = runtime.heap.derefAddr(dest.addr);
          if (derefedDest is Term) {
            dest = derefedDest;
          }
        }

        // Extract destination agent ID
        if (dest is ConstTerm && dest.value is String) {
          final destAgentId = dest.value as String;
          print('[DEBUG IRMA $agentId] _processNetworkOutput: found msg($destAgentId, $content)');
          _triggerNetworkTransaction(destAgentId, content);
        } else {
          print('[DEBUG IRMA $agentId] _processNetworkOutput: dest is not atom: $dest');
        }
      } else {
        print('[DEBUG IRMA $agentId] _processNetworkOutput: head is not msg/2: $head');
      }

      // Continue monitoring tail for more messages
      _monitorTail(tail);
    } else if (term is ConstTerm && term.value == 'nil') {
      print('[DEBUG IRMA $agentId] _processNetworkOutput: stream ended (nil)');
    } else {
      print('[DEBUG IRMA $agentId] _processNetworkOutput: unexpected term type: $term');
    }
  }

  /// Process a message head once it's bound
  ///
  /// Called when the head of a network output cons cell becomes bound.
  void _processNetworkOutputMessage(Term head, Term tail) {
    // Check if head is msg(Dest, Content)
    if (head is StructTerm && head.functor == 'msg' && head.args.length == 2) {
      var dest = head.args[0];
      final content = head.args[1];

      // Dereference dest if needed
      if (dest is VarRef) {
        final derefedDest = runtime.heap.derefAddr(dest.addr);
        if (derefedDest is Term) {
          dest = derefedDest;
        }
      }

      // Extract destination agent ID
      if (dest is ConstTerm && dest.value is String) {
        final destAgentId = dest.value as String;
        print('[DEBUG IRMA $agentId] _processNetworkOutputMessage: found msg($destAgentId, $content)');
        _triggerNetworkTransaction(destAgentId, content);
      } else {
        print('[DEBUG IRMA $agentId] _processNetworkOutputMessage: dest is not atom: $dest');
      }
    } else {
      print('[DEBUG IRMA $agentId] _processNetworkOutputMessage: not msg/2: $head');
    }
  }

  /// Monitor a tail VarRef for more messages
  void _monitorTail(Term tail) {
    if (tail is VarRef) {
      final tailAddr = tail.addr;
      final writerAddr = runtime.heap.tryWriterForReader(tailAddr);
      if (writerAddr != null) {
        print('[DEBUG IRMA $agentId] _monitorTail: monitoring tail writer $writerAddr');
        runtime.heap.onBind(writerAddr, (Term t) {
          _processNetworkOutput(t);
        });
      } else if (runtime.heap.isWriter(tailAddr)) {
        print('[DEBUG IRMA $agentId] _monitorTail: monitoring tail (already writer) $tailAddr');
        runtime.heap.onBind(tailAddr, (Term t) {
          _processNetworkOutput(t);
        });
      }
    }
  }

  /// Trigger Network Transaction per spec Section 5.4
  ///
  /// 1. X' := export(X) for this agent
  /// 2. Queue message for delivery to destination's network input
  /// 3. Destination imports variables when receiving
  void _triggerNetworkTransaction(String destination, Term content) {
    print('[DEBUG IRMA $agentId] _triggerNetworkTransaction: to=$destination, content=$content');

    // Step 1: export(content) - modifies V_p and M_p
    final exportedContent = exportTerm(content);
    print('[DEBUG IRMA $agentId] _triggerNetworkTransaction: exported=$exportedContent');

    // Step 2: Serialize and queue for delivery
    final payload = _serializer.createAgentMessagePayload(
      exportedContent,
      runtime.heap.isReader,
      lookupVariable: _lookupVariableForSerialization,
    );

    mp.add(OutboundMessage(
      destination: destination,
      type: MessageType.agentMessage,
      payload: payload,
    ));
    print('[DEBUG IRMA $agentId] _triggerNetworkTransaction: queued agentMessage to $destination');
  }

  /// Handle incoming network message per spec Section 5.4
  ///
  /// 1. Deserialize content X'
  /// 2. For each variable Y in X' not local to this agent: add (Y, creator, ⊥) to V_p
  /// 3. Append X' to network input stream
  void handleNetworkMessage(String sender, List<int> payload) {
    print('[DEBUG IRMA $agentId] handleNetworkMessage: from=$sender, ${payload.length} bytes');

    // Step 1 & 2: Deserialize and import variables
    final content = deserializeAndImportTerm(payload, sender);
    print('[DEBUG IRMA $agentId] handleNetworkMessage: content=$content');

    // Step 3: Append to network input stream
    if (_netInTailWriter == null) {
      print('[DEBUG IRMA $agentId] handleNetworkMessage: ERROR - no NetIn tail registered');
      return;
    }

    // Wrap content in msg(sender, content) for GLP protocol
    // The receiving agent expects msg(From, Content) to know who sent the message
    final msgTerm = StructTerm('msg', [ConstTerm(sender), content]);

    // Create new cons cell [msg(sender, content) | newTail]
    final (newTailWriter, newTailReader) = runtime.heap.allocateVariable();
    final consCell = StructTerm('.', [msgTerm, VarRef(newTailReader)]);

    // Bind current tail to the cons cell
    final activations = runtime.heap.bindVariable(_netInTailWriter!, consCell);
    print('[DEBUG IRMA $agentId] handleNetworkMessage: bound NetIn tail, ${activations.length} activations');

    // Update tail pointer
    _netInTailWriter = newTailWriter;

    // Enqueue any reactivated goals (use enqueueReactivatedGoal for proper cleanup)
    for (final act in activations) {
      runtime.enqueueReactivatedGoal(act);
    }
  }

  // =========================================================================
  // Message Queuing
  // =========================================================================
  
  /// Queue an assignment message for a remote reader
  ///
  /// Uses the creator's local ID (creatorLocalId) in the global ID format,
  /// not our local varId. This ensures the creator can look it up in their V_p.
  ///
  /// For imported variables in the value term, uses V_p to get the correct
  /// (creator, creatorLocalId) per irmaGLP-spec.md Section 8.1.
  void _queueAssignmentFromEntry(VariableEntry entry, Term value, String destination) {
    // Use creator's local ID for the global variable ID
    // Per spec Section 5.3: assignments are addressed to the reader (W?:=T)
    // For imported writers, use pairedReaderCreatorLocalId (the reader's ID)
    final int assignmentTargetId;
    if (entry.role == VariableRole.importedWriter && entry.pairedReaderCreatorLocalId != null) {
      assignmentTargetId = entry.pairedReaderCreatorLocalId!;
    } else {
      assignmentTargetId = entry.creatorLocalId;
    }
    final creator = entry.creator;

    // Per spec Section 4.3: export the term before sending
    // This registers any new local variables (like Zs in [a|Zs?]) in V_p
    // with proper onBind callbacks, so nested writers can communicate
    final exportedValue = exportTerm(value);

    // Deep dereference the exported value to resolve all VarRefs to their bound values
    // This is necessary because VarRefs contain local heap addresses that are
    // meaningless to other agents
    final derefedValue = _deepDeref(exportedValue);

    print('[DEBUG IRMA $agentId] _queueAssignment: varId=${entry.varId}, assignmentTargetId=$assignmentTargetId, creator=$creator, value=$derefedValue, destination=$destination');

    // Create assignment payload with proper global ID
    // Use V2 method with isReader callback and lookupVariable callback
    // to correctly serialize imported variables in the value term
    final globalIdSerializer = PayloadSerializer(creator);
    final payload = globalIdSerializer.createAssignmentPayloadV2(
      assignmentTargetId,
      derefedValue,
      runtime.heap.isReader,
      lookupVariable: _lookupVariableForSerialization,
    );

    mp.add(OutboundMessage(
      destination: destination,
      type: MessageType.assignment,
      payload: payload,
    ));
    print('[DEBUG IRMA $agentId] _queueAssignment: message queued, mp.totalLength=${mp.totalLength}');
  }

  /// Lookup variable info for serialization
  ///
  /// For imported variables, returns the original creator's (creator, creatorLocalId).
  /// For local variables, returns (agentId, addr).
  ({String creator, int creatorLocalId, bool isReader}) _lookupVariableForSerialization(int addr) {
    final isReader = runtime.heap.isReader(addr);
    final key = VarKey(addr, isReader);
    final entry = vp.lookup(key);

    if (entry != null) {
      // Found in V_p - use original creator's ID
      return (
        creator: entry.creator,
        creatorLocalId: entry.creatorLocalId,
        isReader: entry.isReader,
      );
    } else {
      // Not in V_p - local variable, use our agentId
      return (
        creator: agentId,
        creatorLocalId: addr,
        isReader: isReader,
      );
    }
  }

  /// Deeply dereference a term, resolving all VarRefs to their bound values.
  ///
  /// This is necessary before serializing a term for inter-agent transport,
  /// as VarRefs contain local heap addresses that are meaningless to other agents.
  ///
  /// - ConstTerm: returned as-is
  /// - VarRef: dereferenced, and if bound to another term, recursively processed
  /// - StructTerm: args are recursively dereferenced
  ///
  /// If a VarRef is unbound, it remains as a VarRef (will be serialized with global ID).
  Term _deepDeref(Term term, {Set<int>? visited}) {
    visited ??= {};

    if (term is ConstTerm) {
      return term;
    } else if (term is VarRef) {
      // Prevent infinite loops
      if (visited.contains(term.addr)) {
        return term;
      }
      visited.add(term.addr);

      // Dereference the VarRef
      final derefed = runtime.heap.derefAddr(term.addr);
      if (derefed is Term && derefed != term) {
        // Recursively dereference the result
        return _deepDeref(derefed, visited: visited);
      }
      // Unbound or self-referential - return original VarRef
      return term;
    } else if (term is StructTerm) {
      // Recursively dereference all args
      final derefedArgs = term.args.map((arg) => _deepDeref(arg, visited: visited)).toList();
      return StructTerm(term.functor, derefedArgs);
    }

    return term;
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
  /// IMPORTANT: The readerAddr MUST exist in the heap with a VariableEntry.
  /// All imported variables should be created via allocateImportedReader(),
  /// which creates the heap cell.
  void _requestFromHeap(int readerAddr) {
    // Validate address exists in heap
    if (readerAddr >= runtime.heap.cells.length) {
      throw StateError(
        '_requestFromHeap: addr $readerAddr out of bounds. '
        'Imported readers must be created via allocateImportedReader() '
        'which creates the heap cell.'
      );
    }

    // Dereference to get either Term (if bound) or VariableEntry (if imported unbound)
    final result = runtime.heap.derefAddr(readerAddr);

    if (result is VariableEntry) {
      final entry = result;

      // Only send request for imported readers that haven't been requested yet
      if (entry.role == VariableRole.importedReader &&
          entry.creator != agentId &&
          !entry.requestSent) {
        print('[DEBUG IRMA $agentId] _requestFromHeap: sending request for reader $readerAddr to ${entry.creator}');

        // Mark request sent (in both V_p and heap cell entry)
        final readerKey = VarKey(entry.varId, true);
        vp.markRequestSent(readerKey);
        entry.requestSent = true;  // Also update the heap cell's entry

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
        print('[DEBUG IRMA $agentId] _requestFromHeap: skipping reader $readerAddr (role=${entry.role}, requestSent=${entry.requestSent})');
      }
    } else if (result is VarRef) {
      // Local unbound variable - should not happen for imported readers
      // If we get here, the caller passed a local variable to a method
      // designed for imported readers
      print('[DEBUG IRMA $agentId] _requestFromHeap: WARNING - got local VarRef at $readerAddr, expected VariableEntry');
      // Fall back to legacy request for compatibility
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
      onVariableImported: (int localAddr, bool isReader, GlobalVarId globalId, int? pairedReaderCreatorLocalId) {
        attachImportedVariableEntry(localAddr, isReader, globalId, fromAgent,
            pairedReaderCreatorLocalId: pairedReaderCreatorLocalId);
      },
    );

    return term;
  }
  
  /// Attach a VariableEntry to an imported variable's heap cell
  ///
  /// Creates the entry and stores it both in V_p and in the heap cell content.
  ///
  /// [pairedReaderCreatorLocalId] - For imported writers, the creator's local ID
  ///   of the paired reader. Used when sending assignments per spec Section 5.3.
  void attachImportedVariableEntry(
    int localAddr,
    bool isReader,
    GlobalVarId globalId,
    String fromAgent,
    {int? pairedReaderCreatorLocalId}
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

    // For imported writers, store the paired reader's ID for assignment routing
    if (!isReader && pairedReaderCreatorLocalId != null) {
      entry.pairedReaderCreatorLocalId = pairedReaderCreatorLocalId;
    }

    // Add to V_p
    final key = VarKey(localAddr, isReader);
    vp.add(key, entry);

    // Attach entry to heap cell content
    runtime.heap.cells[localAddr].content = entry;

    print('[DEBUG IRMA $agentId] attachImportedVariableEntry: localAddr=$localAddr, isReader=$isReader, creator=$creator, creatorLocalId=$creatorLocalId');

    // For imported writers, register binding callback
    if (!isReader) {
      runtime.heap.onBind(localAddr, (Term value) {
        print('[DEBUG IRMA $agentId] HEAP CALLBACK fired for imported writer $localAddr, value=$value');
        onWriterBound(localAddr, value);
      });
    }
  }

  /// Import a term received from another agent (legacy method)
  ///
  /// For each variable Y in term where (Y, ·, ·) ∉ V_p:
  /// - Add to V_p as imported reader or writer based on heap cell tag
  ///
  /// [term] - The deserialized term
  /// [fromAgent] - The agent who sent this term (usually the creator)
  /// [globalIdMapping] - Optional mapping from local addr -> GlobalVarId
  ///   This maps our local heap addresses to the creator's global IDs.
  void importTerm(Term term, String fromAgent, {Map<int, GlobalVarId>? globalIdMapping}) {
    _importTermRecursive(term, fromAgent, globalIdMapping ?? {});
  }

  void _importTermRecursive(Term term, String fromAgent, Map<int, GlobalVarId> globalIdMapping) {
    if (term is VarRef) {
      // Per irmaGLP-spec.md Section 3.2.1: use heap to check isReader
      final addr = term.addr;
      final isReaderVar = runtime.heap.isReader(addr);
      final key = VarKey(addr, isReaderVar);
      if (!vp.contains(key)) {
        // Look up the global ID for this variable
        final globalId = globalIdMapping[addr];
        final creator = globalId?.creator ?? fromAgent;
        final creatorLocalId = globalId?.localId;

        // Variable not in V_p - add based on type
        if (isReaderVar) {
          print('[DEBUG IRMA $agentId] _importTermRecursive: registering imported reader $addr from $creator (creatorLocalId=$creatorLocalId)');
          vp.add(key, VariableEntry(
            varId: addr,
            isReader: true,
            creator: creator,
            role: VariableRole.importedReader,
            creatorLocalId: creatorLocalId,
          ));
          // NOTE: Do NOT send request here. Per spec section 5.2, request() is
          // called when a goal SUSPENDS on this reader, not at import time.
        } else {
          // Imported writer - register with callback
          registerImportedWriter(addr, creator, creatorLocalId: creatorLocalId);
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
      () {
        final (writerAddr, readerAddr) = runtime.heap.allocateVariable();
        return [writerAddr, readerAddr];
      },
      runtime.heap.isReader,  // Per irmaGLP-spec.md Section 3.2.1
    );

    // Set up relay forwarding callbacks
    // This implements: export_reader(Y?, Z) :- Z = Y?.
    for (final relay in relaySetups) {
      _setupRelayForwarding(relay);
    }

    // Register heap callbacks for newly exported writers (Issue 17)
    // Per spec Section 5.2: when created writers are bound and have requesters,
    // assignments should be sent. The callback enables message routing.
    for (final writerAddr in result.newlyExportedWriters) {
      runtime.heap.onBind(writerAddr, (Term value) {
        onWriterBound(writerAddr, value);
      });
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
      // This will trigger onWriterBound if Z has a requester
      final activations = runtime.heap.bindVariable(relay.relayWriterId, value);
      for (final act in activations) {
        runtime.enqueueReactivatedGoal(act);
      }
    });
    
    // Also register the relay writer's callback for message routing
    // (The relay writer Z is in V_p and needs to send assignments when bound)
    runtime.heap.onBind(relay.relayWriterId, (Term value) {
      onWriterBound(relay.relayWriterId, value);
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
    print('[DEBUG IRMA $agentId] handleAssignment: entry=${entry?.role}, localVarId=${entry?.varId}, requester=${entry?.requester}, requestSent=${entry?.requestSent}');
    
    if (entry != null) {
      if (entry.role == VariableRole.importedReader) {
        // Imported reader - per irmaGLP spec Section 5.3:
        // - Reactivate suspended goals
        // - Apply {X?:=T} substitution (by updating heap cell)
        // - Remove entry from V_p
        // - Add V_p entries for non-local variables in T (TODO)
        print('[DEBUG IRMA $agentId] handleAssignment: IMPORTED READER - binding readerAddr=${entry.varId}');

        // Bind imported reader: updates heap cell, gets activations from VariableEntry.suspensions
        // Note: entry.state is NOT used here - bindImportedReader uses entry.suspensions
        final readerAddr = entry.varId;
        final activations = runtime.heap.bindImportedReader(readerAddr, value, entry);
        print('[DEBUG IRMA $agentId] handleAssignment: bindImportedReader returned ${activations.length} activations');
        for (final act in activations) {
          runtime.enqueueReactivatedGoal(act);
        }

        // Remove from V_p - variable is now bound, entry no longer needed
        vp.remove(entry.key);
      } else if (entry.role == VariableRole.createdReader) {
        // We created this reader - check for pending requester
        if (entry.requester != null) {
          // Created reader with pending request - forward to requester
          print('[DEBUG IRMA $agentId] handleAssignment: CREATED READER with requester=${entry.requester}, forwarding');
          _queueAssignmentFromEntry(entry, value, entry.requester!);
          entry.boundValue = value;
          vp.updateBoundValue(entry.key, value);
        } else {
          // No requester yet - store value for later
          print('[DEBUG IRMA $agentId] handleAssignment: CREATED READER, no requester yet, storing value');
          entry.boundValue = value;
          vp.updateBoundValue(entry.key, value);
        }
      } else {
        print('[DEBUG IRMA $agentId] handleAssignment: UNHANDLED ROLE - ${entry.role}');
      }
    } else if (creator == agentId) {
      // Not in V_p, but we created it - local variable, apply directly
      // Per spec Section 5.3: assignments are addressed to the reader (W?:=T)
      // If this is a reader cell, we need to bind the paired writer instead
      int targetAddr = creatorLocalId;
      if (runtime.heap.isReader(creatorLocalId)) {
        // Assignment addressed to reader, bind the paired writer (reader - 1)
        targetAddr = creatorLocalId - 1;
        print('[DEBUG IRMA $agentId] handleAssignment: LOCAL reader - binding paired writer at $targetAddr');
      } else {
        print('[DEBUG IRMA $agentId] handleAssignment: LOCAL (not in V_p) - binding varId=$creatorLocalId');
      }
      final activations = runtime.heap.bindVariable(targetAddr, value);
      for (final act in activations) {
        runtime.enqueueReactivatedGoal(act);
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
  /// ## varId Parameter Semantics
  ///
  /// The [varId] parameter is the **creator's local ID** (creatorLocalId), NOT the
  /// requester's local heap address. Here's the flow:
  ///
  /// 1. Alice imports reader from Bob with: creatorLocalId=42, creator=bob
  /// 2. Alice's local heap has the reader at address 100
  /// 3. Alice sends read request using creatorLocalId=42 (not 100)
  /// 4. Bob receives varId=42
  /// 5. Bob looks up entry where: creator=bob, creatorLocalId=42
  ///
  /// For created variables, varId == creatorLocalId == local heap address.
  /// We use findByCreatorLocalId to handle both cases uniformly.
  void handleReadRequest(int varId, String requester) {
    print('[DEBUG IRMA $agentId] handleReadRequest: varId=$varId (creatorLocalId), requester=$requester');

    // Look up reader entry by creatorLocalId
    // For created readers, we are the creator, so creator=agentId
    final readerEntry = vp.findByCreatorLocalId(agentId, varId, isReader: true);
    
    if (readerEntry != null) {
      print('[DEBUG IRMA $agentId] handleReadRequest: found reader entry, role=${readerEntry.role}, requester=${readerEntry.requester}, boundValue=${readerEntry.boundValue}');

      if (readerEntry.role == VariableRole.createdReader) {
        if (readerEntry.boundValue != null) {
          // Value already stored - reply immediately
          print('[DEBUG IRMA $agentId] handleReadRequest: created reader has value, replying immediately');
          _queueAssignmentFromEntry(readerEntry, readerEntry.boundValue!, requester);
        } else if (readerEntry.requester == null) {
          // No request yet - record requester
          print('[DEBUG IRMA $agentId] handleReadRequest: created reader, recording requester=$requester');
          readerEntry.requester = requester;
          vp.updateRequester(readerEntry.key, requester);
        } else {
          print('[DEBUG IRMA $agentId] handleReadRequest: created reader, already has requester=${readerEntry.requester}, ignoring');
        }
        return;
      }
    }
    
    // Check writer entry (direct communication case per spec)
    // Use findByCreatorLocalId for consistency with reader lookup
    final writerEntry = vp.findByCreatorLocalId(agentId, varId, isReader: false);
    
    if (writerEntry != null && writerEntry.role == VariableRole.createdWriter) {
      print('[DEBUG IRMA $agentId] handleReadRequest: found writer entry, requester=${writerEntry.requester}');

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
        writerEntry.requester = requester;
        vp.updateRequester(writerEntry.key, requester);
      }
      return;
    }
    
    print('[DEBUG IRMA $agentId] handleReadRequest: NO ENTRY in V_p for reader or writer');
  }
  
  /// Handle incoming abandon notification
  ///
  /// Called by coordinator when abandon(Y) arrives.
  /// Per spec Section 5.1: "Abandoned variables cause dependent suspended
  /// goals to fail rather than wait indefinitely."
  ///
  /// Implementation: Bind the abandoned variable to a special '$abandoned$'
  /// term. This triggers reactivation of suspended goals, which will then
  /// fail in unification (since '$abandoned$' won't match their expected values).
  void handleAbandon(int varId) {
    print('[DEBUG IRMA $agentId] handleAbandon: varId=$varId');

    // For imported readers, bind to poison value to trigger goal failure
    final readerKey = VarKey(varId, true);
    final entry = vp.lookup(readerKey);

    if (entry != null && entry.role == VariableRole.importedReader) {
      // Get the VariableEntry from heap cell for suspensions
      final cell = runtime.heap.cells[varId];
      if (cell.content is VariableEntry) {
        final heapEntry = cell.content as VariableEntry;

        // Bind to poison value - this reactivates suspended goals
        // which will then fail when they try to unify with '$abandoned$'
        final poisonValue = ConstTerm('\$abandoned\$');
        final activations = runtime.heap.bindImportedReader(varId, poisonValue, heapEntry);
        print('[DEBUG IRMA $agentId] handleAbandon: bound to poison, ${activations.length} goals reactivated');

        for (final act in activations) {
          runtime.enqueueReactivatedGoal(act);
        }
      }
    }

    // Remove both reader and writer entries if present
    vp.remove(VarKey(varId, true));
    vp.remove(VarKey(varId, false));

    // Remove any pending bind callback
    runtime.heap.removeBindCallback(varId);
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
      // Per irmaGLP-spec.md Section 3.2.1: use raw addr as identifier
      result.add(term.addr);
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
          vpEntry.requester != null) {
        final requester = vpEntry.requester!;
        _queueAssignmentFromEntry(vpEntry, value, requester);
      }
      else if (vpEntry.role == VariableRole.importedReader &&
               vpEntry.creator != agentId &&
               !vpEntry.requestSent) {
        vp.markRequestSent(readerKey);
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
