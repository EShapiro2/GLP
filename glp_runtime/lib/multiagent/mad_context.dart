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

  /// Optional trace sink for MAD infrastructure output.
  /// When set, MAD debug output goes through this callback instead of print().
  void Function(String)? traceSink;

  /// Send MAD trace output to traceSink if set, otherwise silent.
  void _trace(String msg) {
    if (traceSink != null) {
      traceSink!(msg);
    }
  }

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
    _trace('[MAD $agentId] onWriterBound: writerId=$writerId, value=$value');
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
    _trace('[MAD $agentId] flushMessages: mp.totalLength=${mp.totalLength}, destinations=$destinations');

    for (final dest in destinations) {
      while (true) {
        final msg = mp.poll(dest);
        if (msg == null) break;
        _trace('[MAD $agentId] flushMessages: sending ${msg.type} to $dest');
        onMessageReady!(dest, msg);
        count++;
      }
    }
    _trace('[MAD $agentId] flushMessages: flushed $count messages');
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

    _trace('[MAD $agentId] global_send FIRED: ${result.globalName} -> ${result.destination}');

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
      _trace('[MAD $agentId] global_send spawned new goal: ${newGoal.globalName}');
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
  ///
  /// Each TermVar carries both the writer and reader addresses of its pair,
  /// looked up via the heap's cross-pointers.
  void _extractTermVarsRecursive(Term term, List<TermVar> result) {
    if (term is VarRef) {
      final isReader = runtime.heap.isReader(term.addr);
      if (isReader) {
        final writerAddr = runtime.heap.tryWriterForReader(term.addr);
        result.add(TermVar.reader(term.addr, writerAddr: writerAddr ?? term.addr));
      } else {
        final readerAddr = runtime.heap.pairedReaderAddr(term.addr);
        result.add(TermVar.writer(term.addr, readerAddr: readerAddr ?? term.addr));
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
  /// Per spec Section 4: global_send(T, G, Q) fires when T (the reader) becomes
  /// known, i.e., when the paired writer is bound. We register both the goal in
  /// the GlobalSendRegistry and an onBind callback on the heap so that
  /// onWriterBound is called when the writer is assigned.
  void registerGlobalSendSpawns(List<GlobalSendSpawn> spawns) {
    for (final spawn in spawns) {
      globalSendRegistry.register(GlobalSendGoal.fromSpawn(spawn));
      runtime.heap.onBind(spawn.readerAddr, (Term value) {
        onWriterBound(spawn.readerAddr, value);
      });
      _trace('[MAD $agentId] registered global_send goal: ${spawn.globalName} -> ${spawn.destAgent}');
    }
  }

  // =========================================================================
  // Write-back for localized _w variables
  // =========================================================================

  /// Register write-back callbacks for localized _w variables
  ///
  /// When localize processes _w(p, i), it creates a fresh pair (Y_q, Y_q?).
  /// The reader Y_q? goes into the term. If the local agent later binds
  /// the writer Y_q, the value must be sent back to agent p as _w(p, i) := T↑.
  ///
  /// This method registers onBind callbacks on the fresh writers so that
  /// when they are bound locally, a global_send fires to deliver the value.
  void _registerWriteBackCallbacks(
    List<GlobalName> globalNames,
    LocalizeResult localizeResult,
  ) {
    for (var i = 0; i < globalNames.length; i++) {
      final gn = globalNames[i];
      if (!gn.isWriter) continue; // Only _w names need write-back

      final pair = localizeResult.freshPairs[i];
      final writerAddr = pair.writerAddr;
      final destAgent = gn.agent;

      _trace('[MAD $agentId] registering write-back callback for $gn on writer=$writerAddr');

      runtime.heap.onBind(writerAddr, (Term value) {
        _trace('[MAD $agentId] write-back FIRED: $gn := $value (writer=$writerAddr)');
        _sendWriteBack(gn, value, destAgent);
      });
    }
  }

  /// Send a write-back message for a localized _w variable
  ///
  /// When the local agent binds a writer that was created by localizing _w(p, i),
  /// globalize the value and send _w(p, i) := T↑ to agent p.
  void _sendWriteBack(GlobalName globalName, Term value, String destAgent) {
    // Globalize the value (may contain nested variables)
    final vars = <TermVar>[];
    _extractTermVarsRecursive(value, vars);

    if (vars.isNotEmpty) {
      _trace('[MAD $agentId] write-back: globalizing ${vars.length} nested variables');
      final globalizeResult = globalize(
        variables: vars,
        localAgent: agentId,
        remoteAgent: destAgent,
        table: wp,
      );

      // Register global_send goals for nested writers
      registerGlobalSendSpawns(globalizeResult.spawns);

      // Transform the value to use global names
      value = globalizeTermWithResult(value, vars, globalizeResult);
    }

    _trace('[MAD $agentId] write-back: sending $globalName := $value to $destAgent');

    // Create and queue the assignment message
    final payload = _serializer.createGlobalSendPayload(
      globalName,
      value,
      runtime.heap.isReader,
      lookupVariable: _lookupVariableForSerialization,
    );

    mp.add(OutboundMessage(
      destination: destAgent,
      type: MessageType.assignment,
      payload: payload,
    ));
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
    _trace('[MAD $agentId] handleMadAssignment: $globalName := $value from $fromAgent');

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
  /// Localize T↑ by q to get T_q↓. Assign N_q := [T_q↓ | N'_q] where N'_q is a
  /// fresh writer. Update the entry to `(N'_q, *)` at index 0."
  void _handleSerializerAssignment(Term value, String fromAgent) {
    _trace('[MAD $agentId] _handleSerializerAssignment: cold-call from $fromAgent');

    // Get current serializer writer from index-0 entry
    final currentWriter = wp.serializerWriterAddr;
    if (currentWriter == null) {
      throw StateError('Serializer entry not initialized at index 0');
    }
    _trace('[MAD $agentId] _handleSerializerAssignment: current serializer writer=$currentWriter');

    // The value should be [T | serializer_marker] - extract the content T
    // The serializer marker is a special constant we recognize
    Term content;
    if (value is StructTerm && value.functor == '.' && value.args.length == 2) {
      content = value.args[0];  // Head is the actual content
      // Tail (value.args[1]) should be the serializer marker, we ignore it
      _trace('[MAD $agentId] _handleSerializerAssignment: extracted content from list cell');
    } else {
      // If not wrapped in list cell, use the value directly (for compatibility)
      content = value;
      _trace('[MAD $agentId] _handleSerializerAssignment: using value directly (no list wrapper)');
    }

    // Localize the content: replace global names with local variables
    // Per spec Section 8.3: "Localize T↑ by q to get T_q↓"
    final globalNames = extractGlobalNames(content);
    if (globalNames.isNotEmpty) {
      _trace('[MAD $agentId] _handleSerializerAssignment: found ${globalNames.length} global names to localize');
      final localizeResult = localize(
        globalNames: globalNames,
        localAgent: agentId,
        table: wp,
        freshAddrAllocator: () => runtime.heap.allocateVariable(),
      );

      // Register spawned goals from localization (_r cases)
      registerGlobalSendSpawns(localizeResult.spawns);

      // Register write-back callbacks for _w names: if the local agent
      // binds the fresh writer, send the value back to the originating agent.
      _registerWriteBackCallbacks(globalNames, localizeResult);

      // Replace global names with local variables in content
      content = localizeTermWithResult(content, globalNames, localizeResult);
      _trace('[MAD $agentId] _handleSerializerAssignment: localized content = $content');
    }

    // Allocate fresh writer for stream continuation
    final (freshWriter, freshReader) = runtime.heap.allocateVariable();
    _trace('[MAD $agentId] _handleSerializerAssignment: fresh stream continuation ($freshWriter,$freshReader)');

    // Build the list cell [content | freshReader]
    // This extends the network input stream by one element
    final listCell = StructTerm('.', [content, VarRef(freshReader)]);

    // Bind current writer to extend the stream: N_p := [content | N'_p?]
    final activations = runtime.heap.bindVariable(currentWriter, listCell);
    _trace('[MAD $agentId] _handleSerializerAssignment: bound stream, ${activations.length} activations');

    // Update the serializer entry to point to the fresh writer
    // This entry is permanent - never removed
    wp.updateSerializerWriter(freshWriter);
    _trace('[MAD $agentId] _handleSerializerAssignment: updated serializer entry to writer=$freshWriter');

    // Reactivate suspended goals waiting on the network input stream
    for (final act in activations) {
      runtime.enqueueReactivatedGoal(act);
    }
  }

  /// Handle _w(p, i) := T assignment with i > 0 (we localized _w(p,i))
  ///
  /// Search for LocalizeEntry with (remoteAgent=p, remoteIndex=i).
  /// Per spec Section 8.3: "Localize T↑ by q from p to get T_q↓, assign X_q := T_q↓"
  void _handleWriterAssignment(GlobalName globalName, Term value, String fromAgent) {
    final entry = wp.findByRemote(globalName.agent, globalName.index);
    if (entry == null) {
      throw StateError(
        'No LocalizeEntry for $globalName: expected entry with '
        '(remoteAgent=${globalName.agent}, remoteIndex=${globalName.index})',
      );
    }

    _trace('[MAD $agentId] _handleWriterAssignment: found entry, writerAddr=${entry.writerAddr}');

    // Localize the value: replace global names with local variables
    // Per spec Section 8.3: "Localize T↑ by q from p to get T_q↓"
    Term localizedValue = value;
    final globalNames = extractGlobalNames(value);
    if (globalNames.isNotEmpty) {
      _trace('[MAD $agentId] _handleWriterAssignment: localizing ${globalNames.length} nested global names');
      final localizeResult = localize(
        globalNames: globalNames,
        localAgent: agentId,
        table: wp,
        freshAddrAllocator: () => runtime.heap.allocateVariable(),
      );
      registerGlobalSendSpawns(localizeResult.spawns);
      _registerWriteBackCallbacks(globalNames, localizeResult);
      localizedValue = localizeTermWithResult(value, globalNames, localizeResult);
      _trace('[MAD $agentId] _handleWriterAssignment: localized value = $localizedValue');
    }

    // Remove any onBind callback before binding — the value is arriving
    // from a remote assignment, so we must NOT echo it back via write-back.
    runtime.heap.removeBindCallback(entry.writerAddr);

    // Bind the writer with localized value
    final activations = runtime.heap.bindVariable(entry.writerAddr, localizedValue);
    _trace('[MAD $agentId] _handleWriterAssignment: bound writer, ${activations.length} activations');

    // Reactivate suspended goals
    for (final act in activations) {
      runtime.enqueueReactivatedGoal(act);
    }

    // Remove the entry
    wp.removeLocalizeEntry(globalName.agent, globalName.index);
    _trace('[MAD $agentId] _handleWriterAssignment: entry removed');
  }

  /// Handle _r(p, i) := T assignment
  ///
  /// Two cases:
  /// 1. We globalized Y? as _r(p, i): lookup GlobalizeEntry at index i (we are agent p)
  /// 2. We localized _r(p, i): lookup LocalizeReaderEntry by (p, i) (we are agent q)
  ///
  /// Per spec Section 8.3: "Localize T↑, assign X := T↓"
  void _handleReaderAssignment(GlobalName globalName, Term value, String fromAgent) {
    // Case 1: Check for GlobalizeEntry (we globalized this reader)
    final entry = wp.lookupByIndex(globalName.index);

    // Case 2: Check for LocalizeReaderEntry (we localized this _r name)
    final localizeWriterAddr = wp.findLocalizeReaderEntry(globalName.agent, globalName.index);

    if (entry == null && localizeWriterAddr == null) {
      throw StateError(
        'No GlobalizeEntry at index ${globalName.index} and no '
        'LocalizeReaderEntry for ($globalName.agent, $globalName.index) for $globalName',
      );
    }

    final writerAddr = entry?.writerAddr ?? localizeWriterAddr!;
    final isLocalizeCase = entry == null;
    _trace('[MAD $agentId] _handleReaderAssignment: ${isLocalizeCase ? "localize-reader" : "globalize-reader"} entry, writerAddr=$writerAddr');

    // Localize the value: replace global names with local variables
    // Per spec Section 8.3: "Localize T↑ by p from q to get T_p↓"
    Term localizedValue = value;
    final globalNames = extractGlobalNames(value);
    if (globalNames.isNotEmpty) {
      _trace('[MAD $agentId] _handleReaderAssignment: localizing ${globalNames.length} nested global names');
      final localizeResult = localize(
        globalNames: globalNames,
        localAgent: agentId,
        table: wp,
        freshAddrAllocator: () => runtime.heap.allocateVariable(),
      );
      registerGlobalSendSpawns(localizeResult.spawns);
      _registerWriteBackCallbacks(globalNames, localizeResult);
      localizedValue = localizeTermWithResult(value, globalNames, localizeResult);
      _trace('[MAD $agentId] _handleReaderAssignment: localized value = $localizedValue');
    }

    // Remove any onBind callback before binding — the value is arriving
    // from a remote assignment, so we must NOT echo it back via global_send.
    runtime.heap.removeBindCallback(writerAddr);

    // Bind the writer with localized value
    final activations = runtime.heap.bindVariable(writerAddr, localizedValue);
    _trace('[MAD $agentId] _handleReaderAssignment: bound writer, ${activations.length} activations');

    // Reactivate suspended goals
    for (final act in activations) {
      runtime.enqueueReactivatedGoal(act);
    }

    // Remove the entry
    if (isLocalizeCase) {
      wp.removeLocalizeReaderEntry(globalName.agent, globalName.index);
    } else {
      wp.removeGlobalizeEntry(globalName.index);
    }
    _trace('[MAD $agentId] _handleReaderAssignment: entry removed');
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
    _trace('[MAD $agentId] handleMadAssignmentWithGlobalNames: $globalName with ${nestedGlobalNames.length} nested names');

    // Localize the nested global names
    final localizeResult = localize(
      globalNames: nestedGlobalNames,
      localAgent: agentId,
      table: wp,
      freshAddrAllocator: () => runtime.heap.allocateVariable(),
    );

    // Register spawned goals from localization
    registerGlobalSendSpawns(localizeResult.spawns);
    _registerWriteBackCallbacks(nestedGlobalNames, localizeResult);

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
        _trace('[MAD $agentId] exportTerm: registered callback for writer ${v.addr}');
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
      _trace('[MAD $agentId] processSuspension: waiting for assignment to reader $readerId');
    }
    // No explicit request messages needed in madGLP push model
  }

  // =========================================================================
  // Unified Send ('_send' builtin implementation)
  // =========================================================================

  /// Unified send operation per spec Section 11.5
  ///
  /// The `'_send'(T, G, Q)` builtin behavior depends on whether G is a serializer
  /// address (index 0) or a normal global name (index > 0):
  ///
  /// **Case G = `_w(q, 0)` (Serializer)**:
  /// 1. Globalizes term T for remote agent Q
  /// 2. Adds message `(_w(q,0) := [T↑ | _w(q,0)], Q)` to M_p — content wrapped in list cell
  ///
  /// **Case G = `_w(p, i)` or `_r(p, i)` with i > 0 (Normal)**:
  /// 1. Globalizes term T for remote agent Q
  /// 2. Adds message `(G := T↑, Q)` to M_p — content sent directly
  void send(Term term, bool isWriter, String gnAgent, int gnIndex, String destAgent) {
    _trace('[MAD $agentId] send: term=$term, isWriter=$isWriter, gnAgent=$gnAgent, gnIndex=$gnIndex, dest=$destAgent');

    // Extract variables from the term for globalization
    final vars = <TermVar>[];
    _extractTermVarsRecursive(term, vars);
    _trace('[MAD $agentId] send: found ${vars.length} variables in term');

    // Globalize the term for the destination agent
    // This allocates global names for all variables and spawns global_send goals for writers
    final globalizeResult = globalize(
      variables: vars,
      localAgent: agentId,
      remoteAgent: destAgent,
      table: wp,
    );

    // Register the spawned global_send goals for nested writers
    registerGlobalSendSpawns(globalizeResult.spawns);

    // Register onBind callbacks for globalize-reader entries:
    // When the paired writer is bound locally, send _r(p, i) := T↑ to dest.
    for (var i = 0; i < vars.length; i++) {
      if (vars[i].isReader) {
        final gn = globalizeResult.globalNames[i];
        final writerAddr = vars[i].writerAddr;
        _trace('[MAD $agentId] send: registering onBind for globalize-reader $gn on writer=$writerAddr');
        runtime.heap.onBind(writerAddr, (Term value) {
          _trace('[MAD $agentId] globalize-reader onBind FIRED: $gn := $value (writer=$writerAddr)');
          _sendWriteBack(gn, value, destAgent);
        });
      }
    }

    // Transform the term to use global names
    final globalizedTerm = globalizeTermWithResult(term, vars, globalizeResult);
    _trace('[MAD $agentId] send: globalized term = $globalizedTerm');

    // Build the global name structure
    final globalName = isWriter
        ? GlobalName.writer(gnAgent, gnIndex)
        : GlobalName.reader(gnAgent, gnIndex);

    // Create payload based on serializer (index 0) vs normal (index > 0)
    List<int> payload;
    if (isWriter && gnIndex == 0) {
      // Serializer case: wrap in list cell [T↑ | _w(q,0)]
      _trace('[MAD $agentId] send: serializer case, wrapping in list cell');
      payload = _serializer.createSerializerPayload(
        globalName,
        globalizedTerm,
        runtime.heap.isReader,
        lookupVariable: _lookupVariableForSerialization,
      );
    } else {
      // Normal case: send directly
      _trace('[MAD $agentId] send: normal case, sending directly');
      payload = _serializer.createGlobalSendPayload(
        globalName,
        globalizedTerm,
        runtime.heap.isReader,
        lookupVariable: _lookupVariableForSerialization,
      );
    }

    // Queue the message for delivery
    mp.add(OutboundMessage(
      destination: destAgent,
      type: MessageType.assignment,
      payload: payload,
    ));

    _trace('[MAD $agentId] send: queued message to $destAgent, mp.totalLength=${mp.totalLength}');
  }
}
