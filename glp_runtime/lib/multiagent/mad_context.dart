/// madGLP Agent Context
///
/// Provides agent-level context for multiagent GLP communication.
/// Each agent has W_p (global writers table) and M_p (message queue).
///
/// Specification: /docs/ma/madGLP-spec.md
library;

import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';
import 'package:glp_runtime/multiagent/payload_serializer.dart';
import 'package:glp_runtime/multiagent/global_send.dart';
import 'package:glp_runtime/multiagent/global_writers_table.dart';
import 'package:glp_runtime/multiagent/mad_helpers.dart';

/// Callback for delivering messages to other agents
typedef MessageDeliveryCallback = void Function(String destination, OutboundMessage message);

/// madGLP Agent Context
///
/// Wraps a GlpRuntime with W_p and M_p for multiagent operation.
/// Each agent has its own MadContext with unique agentId.
class MadContext {
  /// Unique identifier for this agent (e.g., "alice", "bob")
  final String agentId;

  /// Underlying GLP runtime
  final GlpRuntime runtime;

  /// Global writers table W_p: tracks writers awaiting incoming assignments
  final GlobalWritersTable wp;

  /// Message queue M_p: outbound messages to other agents
  final MessageQueue mp;

  /// Registry for pending global_send goals (watches readers, sends when known)
  final GlobalSendRegistry globalSendRegistry;

  /// Payload serializer for message encoding
  late final PayloadSerializer _serializer;

  /// Optional callback for message delivery (set by coordinator)
  MessageDeliveryCallback? onMessageReady;

  MadContext({
    required this.agentId,
    required this.runtime,
  })  : wp = GlobalWritersTable(agentId),
        mp = MessageQueue(),
        globalSendRegistry = GlobalSendRegistry(agentId) {
    _serializer = PayloadSerializer(agentId);
  }

  // =========================================================================
  // Writer Binding Observation
  // =========================================================================

  /// Called when a writer is bound to a value
  ///
  /// Checks for global_send goals watching this writer's reader and fires
  /// them if found (per madGLP-spec.md Section 4).
  void onWriterBound(int writerId, Term value) {
    print('[MAD $agentId] onWriterBound: writerId=$writerId, value=$value');
    _fireGlobalSendGoalIfExists(writerId, value);
  }

  // =========================================================================
  // Message Queue Operations (Send Transaction)
  // =========================================================================

  /// Flush all queued messages via the onMessageReady callback
  ///
  /// Returns the number of messages flushed.
  int flushMessages() {
    if (onMessageReady == null) return 0;

    int count = 0;
    final destinations = List<String>.from(mp.destinations);
    print('[MAD $agentId] flushMessages: mp.totalLength=${mp.totalLength}, destinations=$destinations');

    for (final dest in destinations) {
      while (true) {
        final msg = mp.poll(dest);
        if (msg == null) break;
        print('[MAD $agentId] flushMessages: sending ${msg.type} to $dest');
        onMessageReady!(dest, msg);
        count++;
      }
    }
    print('[MAD $agentId] flushMessages: flushed $count messages');
    return count;
  }

  // =========================================================================
  // madGLP global_send Mechanism (Phase 3)
  // =========================================================================

  /// Fire global_send goal if one is watching this writer's reader
  ///
  /// When a writer is bound, its paired reader becomes "known". If there's
  /// a global_send goal watching that reader, fire it now.
  void _fireGlobalSendGoalIfExists(int writerAddr, Term value) {
    // Check if there's a global_send goal watching this writer's reader
    final result = globalSendRegistry.onWriterBound(
      writerAddr: writerAddr,
      value: value,
      table: wp,
      extractVariables: (val) {
        final vars = <TermVar>[];
        if (val is Term) {
          _extractTermVarsRecursive(val, vars);
        }
        return vars;
      },
    );

    if (result == null) {
      return;
    }

    print('[MAD $agentId] global_send FIRED: ${result.globalName} -> ${result.destination}');

    // Queue the assignment message
    final payload = _serializer.createGlobalSendPayload(
      result.globalName,
      result.value as Term,
      runtime.heap.isReader,
      lookupVariable: _lookupVariableForSerialization,
    );

    mp.add(OutboundMessage(
      destination: result.destination,
      type: MessageType.assignment,
      payload: payload,
    ));

    // Register any new goals spawned for nested variables
    for (final newGoal in result.newGoals) {
      print('[MAD $agentId] global_send spawned new goal: ${newGoal.globalName}');
      globalSendRegistry.register(newGoal);
    }
  }

  /// Lookup variable info for serialization
  ({String creator, int creatorLocalId, bool isReader}) _lookupVariableForSerialization(int addr) {
    // For local variables, use the current agent as creator
    // This is a simplified version - extend as needed for imported vars
    return (creator: agentId, creatorLocalId: addr, isReader: runtime.heap.isReader(addr));
  }

  /// Extract TermVars from a term for globalization
  void _extractTermVarsRecursive(Term term, List<TermVar> result) {
    if (term is VarRef) {
      final isReader = runtime.heap.isReader(term.addr);
      if (isReader) {
        result.add(TermVar.reader(term.addr));
      } else {
        result.add(TermVar.writer(term.addr));
      }
    } else if (term is StructTerm) {
      for (final arg in term.args) {
        _extractTermVarsRecursive(arg, result);
      }
    }
    // ConstTerm has no variables
  }

  /// Register global_send goals from GlobalSendSpawn info
  ///
  /// Called after globalize() or localize() to register any spawned goals.
  void registerGlobalSendSpawns(List<GlobalSendSpawn> spawns) {
    globalSendRegistry.registerSpawns(spawns);
    for (final spawn in spawns) {
      print('[MAD $agentId] registered global_send goal: ${spawn.globalName} -> ${spawn.destAgent}');
    }
  }

  // =========================================================================
  // madGLP Receive Transaction (Phase 4)
  // =========================================================================

  /// Handle incoming madGLP assignment message
  ///
  /// Per spec Section 8.3, handles three cases:
  ///
  /// **Case `_w(p, 0) := [T | _w(p,0)]` (Serializer)**: Cold-call to our network input.
  /// **Case `_w(p, i) := T` with i > 0**: We localized _w(p,i), search for entry (X_q, p, i).
  /// **Case `_r(p, i) := T`**: We globalized Y?, find entry (Y, q) at index i.
  ///
  /// Both non-serializer cases: localize T, bind writer, register spawned goals, remove entry.
  /// Serializer case: extend network input stream, update entry (don't remove).
  void handleMadAssignment({
    required GlobalName globalName,
    required Term value,
    required String fromAgent,
  }) {
    print('[MAD $agentId] handleMadAssignment: $globalName := $value from $fromAgent');

    if (globalName.isWriter && globalName.index == 0) {
      // Case _w(p, 0) := [T | _w(p,0)] - serializer (cold-call to network input)
      _handleSerializerAssignment(value, fromAgent);
    } else if (globalName.isWriter) {
      // Case _w(p, i) := T with i > 0 - we localized this, search for entry
      _handleWriterAssignment(globalName, value, fromAgent);
    } else {
      // Case _r(p, i) := T - we globalized this, direct lookup
      _handleReaderAssignment(globalName, value, fromAgent);
    }
  }

  /// Handle serializer assignment _w(p, 0) := [T | _w(p,0)]
  ///
  /// This is a cold-call to our network input stream.
  /// Spec Section 8.3: "Agent q finds the permanent entry `(N_q, *)` at index 0.
  /// Localize T↑ by q to get T_q↓. Assign N_q := [T_q↓? | N'_q] where N'_q is a
  /// fresh writer. Update the entry to `(N'_q, *)` at index 0."
  void _handleSerializerAssignment(Term value, String fromAgent) {
    print('[MAD $agentId] _handleSerializerAssignment: cold-call from $fromAgent');

    // Get current serializer writer from index-0 entry
    final currentWriter = wp.serializerWriterAddr;
    if (currentWriter == null) {
      throw StateError('Serializer entry not initialized at index 0');
    }
    print('[MAD $agentId] _handleSerializerAssignment: current serializer writer=$currentWriter');

    // The value should be [T | serializer_marker] - extract the content T
    // The serializer marker is a special constant we recognize
    Term content;
    if (value is StructTerm && value.functor == '.' && value.args.length == 2) {
      content = value.args[0];  // Head is the actual content
      // Tail (value.args[1]) should be the serializer marker, we ignore it
      print('[MAD $agentId] _handleSerializerAssignment: extracted content from list cell');
    } else {
      // If not wrapped in list cell, use the value directly (for compatibility)
      content = value;
      print('[MAD $agentId] _handleSerializerAssignment: using value directly (no list wrapper)');
    }

    // Allocate fresh writer for stream continuation
    final (freshWriter, freshReader) = runtime.heap.allocateVariable();
    print('[MAD $agentId] _handleSerializerAssignment: fresh stream continuation ($freshWriter,$freshReader)');

    // Build the list cell [content | freshReader]
    // This extends the network input stream by one element
    final listCell = StructTerm('.', [content, VarRef(freshReader)]);

    // Bind current writer to extend the stream: N_p := [content | N'_p?]
    final activations = runtime.heap.bindVariable(currentWriter, listCell);
    print('[MAD $agentId] _handleSerializerAssignment: bound stream, ${activations.length} activations');

    // Update the serializer entry to point to the fresh writer
    // This entry is permanent - never removed
    wp.updateSerializerWriter(freshWriter);
    print('[MAD $agentId] _handleSerializerAssignment: updated serializer entry to writer=$freshWriter');

    // Reactivate suspended goals waiting on the network input stream
    for (final act in activations) {
      runtime.enqueueReactivatedGoal(act);
    }
  }

  /// Handle _w(p, i) := T assignment with i > 0 (we localized _w(p,i))
  ///
  /// Search for LocalizeEntry with (remoteAgent=p, remoteIndex=i).
  void _handleWriterAssignment(GlobalName globalName, Term value, String fromAgent) {
    final entry = wp.findByRemote(globalName.agent, globalName.index);
    if (entry == null) {
      throw StateError(
        'No LocalizeEntry for $globalName: expected entry with '
        '(remoteAgent=${globalName.agent}, remoteIndex=${globalName.index})',
      );
    }

    print('[MAD $agentId] _handleWriterAssignment: found entry, writerAddr=${entry.writerAddr}');

    // Bind the writer
    final activations = runtime.heap.bindVariable(entry.writerAddr, value);
    print('[MAD $agentId] _handleWriterAssignment: bound writer, ${activations.length} activations');

    // Reactivate suspended goals
    for (final act in activations) {
      runtime.enqueueReactivatedGoal(act);
    }

    // Remove the entry
    wp.removeLocalizeEntry(globalName.agent, globalName.index);
    print('[MAD $agentId] _handleWriterAssignment: entry removed');
  }

  /// Handle _r(p, i) := T assignment (we globalized Y?)
  ///
  /// Lookup GlobalizeEntry at index i (we are agent p).
  void _handleReaderAssignment(GlobalName globalName, Term value, String fromAgent) {
    final entry = wp.lookupByIndex(globalName.index);
    if (entry == null) {
      throw StateError(
        'No GlobalizeEntry at index ${globalName.index} for $globalName',
      );
    }

    print('[MAD $agentId] _handleReaderAssignment: found entry, writerAddr=${entry.writerAddr}');

    // Bind the writer
    final activations = runtime.heap.bindVariable(entry.writerAddr, value);
    print('[MAD $agentId] _handleReaderAssignment: bound writer, ${activations.length} activations');

    // Reactivate suspended goals
    for (final act in activations) {
      runtime.enqueueReactivatedGoal(act);
    }

    // Remove the entry
    wp.removeGlobalizeEntry(globalName.index);
    print('[MAD $agentId] _handleReaderAssignment: entry removed');
  }

  /// Handle madGLP assignment with nested global names
  ///
  /// Extended version that handles nested global names in the value term,
  /// creating LocalizeEntries and spawning global_send goals as needed.
  void handleMadAssignmentWithGlobalNames({
    required GlobalName globalName,
    required Term value,
    required List<GlobalName> nestedGlobalNames,
    required String fromAgent,
  }) {
    print('[MAD $agentId] handleMadAssignmentWithGlobalNames: $globalName with ${nestedGlobalNames.length} nested names');

    // Localize the nested global names
    final localizeResult = localize(
      globalNames: nestedGlobalNames,
      localAgent: agentId,
      table: wp,
      freshAddrAllocator: () {
        final (w, _) = runtime.heap.allocateVariable();
        return w;
      },
    );

    // Register spawned goals from localization
    registerGlobalSendSpawns(localizeResult.spawns);

    // Now handle the main assignment
    handleMadAssignment(
      globalName: globalName,
      value: value,
      fromAgent: fromAgent,
    );
  }

  // =========================================================================
  // Export/Import Operations (for Flutter app compatibility)
  // =========================================================================

  /// Export a term for sending to another agent
  ///
  /// Extracts variables and registers them in W_p as needed.
  /// For madGLP, this prepares the term for serialization by setting up
  /// onBind callbacks for writers so assignments can be routed.
  void exportTerm(Term term) {
    final vars = <TermVar>[];
    _extractTermVarsRecursive(term, vars);

    // Register onBind callbacks for any writers in the term
    for (final v in vars) {
      if (v.isWriter) {
        // Set up callback so when this writer is bound, we can route the assignment
        runtime.heap.onBind(v.addr, (Term value) {
          onWriterBound(v.addr, value);
        });
        print('[MAD $agentId] exportTerm: registered callback for writer ${v.addr}');
      }
    }
  }

  /// Process suspension - in madGLP, blocking readers don't trigger immediate requests
  ///
  /// The madGLP model is push-based: assignments are sent when writers are bound,
  /// not when readers request. However, for compatibility with the UI, we log
  /// blocking readers.
  void processSuspension(Set<int> blockingReaders) {
    // In madGLP, suspension means we're waiting for assignments to arrive
    // The push model means we don't send read requests - we just wait
    for (final readerId in blockingReaders) {
      print('[MAD $agentId] processSuspension: waiting for assignment to reader $readerId');
    }
    // No explicit request messages needed in madGLP push model
  }

  // =========================================================================
  // Cold-Call Send (via Index-0 Serializer)
  // =========================================================================

  /// Perform cold-call send via the index-0 serializer
  ///
  /// Per madGLP-spec.md Section 4.1 and 12.2:
  /// 1. Use the well-known serializer address _w(destAgent, 0)
  /// 2. Globalize T for destAgent (spawns global_send goals for writers, creates entries for readers)
  /// 3. Queue assignment message _w(destAgent, 0) := [T↑ | _w(destAgent, 0)]
  ///
  /// This uses the same mechanism as established global links, but targets
  /// the serializer address which has many-to-one semantics.
  ///
  /// This is called by the '_cold_send'/2 kernel predicate when a cold-call
  /// message is ready to be sent.
  void coldSend(Term term, String destAgent) {
    print('[MAD $agentId] coldSend: $term -> $destAgent (via serializer _w($destAgent,0))');

    // Extract variables from the term
    final vars = <TermVar>[];
    _extractTermVarsRecursive(term, vars);
    print('[MAD $agentId] coldSend: found ${vars.length} variables in term');

    // Globalize the term for the destination agent
    // This allocates global names for all variables and spawns global_send goals for writers
    final globalizeResult = globalize(
      variables: vars,
      localAgent: agentId,
      remoteAgent: destAgent,
      table: wp,
    );

    // Register the spawned global_send goals for nested writers
    for (final spawn in globalizeResult.spawns) {
      print('[MAD $agentId] coldSend: registering global_send goal for ${spawn.globalName}');
      globalSendRegistry.register(GlobalSendGoal.fromSpawn(spawn));

      // Set up callback so when the writer's paired reader becomes known, the goal fires
      runtime.heap.onBind(spawn.readerAddr, (Term value) {
        onWriterBound(spawn.readerAddr, value);
      });
    }

    // Use the well-known serializer address _w(destAgent, 0)
    // No index allocation needed - index 0 is the permanent serializer
    final serializerGlobalName = GlobalName.writer(destAgent, 0);
    print('[MAD $agentId] coldSend: using serializer address $serializerGlobalName');

    // Serialize the term with list wrapping for serializer: [T | _w(destAgent, 0)]
    // Spec Section 4.1: "This sends the assignment `_w(q,0) := [T↑ | _w(q,0)]`"
    final payload = _serializer.createSerializerPayload(
      serializerGlobalName,
      term,
      runtime.heap.isReader,
      lookupVariable: _lookupVariableForSerialization,
    );

    // Queue the message for delivery
    mp.add(OutboundMessage(
      destination: destAgent,
      type: MessageType.assignment,
      payload: payload,
    ));

    print('[MAD $agentId] coldSend: queued serializer message to $destAgent, mp.totalLength=${mp.totalLength}');
  }
}
