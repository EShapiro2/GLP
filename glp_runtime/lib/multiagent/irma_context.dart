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
/// Each agent has its own IrmaContext with unique agentId.
class IrmaContext {
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

  IrmaContext({
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
    print('[DEBUG IRMA $agentId] onWriterBound: writerId=$writerId, value=$value');
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
    print('[DEBUG IRMA $agentId] flushMessages: mp.totalLength=${mp.totalLength}, destinations=$destinations');

    for (final dest in destinations) {
      while (true) {
        final msg = mp.poll(dest);
        if (msg == null) break;
        print('[DEBUG IRMA $agentId] flushMessages: sending ${msg.type} to $dest');
        onMessageReady!(dest, msg);
        count++;
      }
    }
    print('[DEBUG IRMA $agentId] flushMessages: flushed $count messages');
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

    print('[DEBUG IRMA $agentId] global_send FIRED: ${result.globalName} -> ${result.destination}');

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
      print('[DEBUG IRMA $agentId] global_send spawned new goal: ${newGoal.globalName}');
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
      print('[DEBUG IRMA $agentId] registered global_send goal: ${spawn.globalName} -> ${spawn.destAgent}');
    }
  }

  // =========================================================================
  // madGLP Receive Transaction (Phase 4)
  // =========================================================================

  /// Handle incoming madGLP assignment message
  ///
  /// Per spec Section 8.3, handles two cases:
  ///
  /// **Case `_w(p, i) := T`**: We localized _w(p,i), search for entry (X_q, p, i).
  /// **Case `_r(p, i) := T`**: We globalized Y?, find entry (Y, q) at index i.
  ///
  /// Both cases: localize T, bind writer, register spawned goals, remove entry.
  void handleMadAssignment({
    required GlobalName globalName,
    required Term value,
    required String fromAgent,
  }) {
    print('[DEBUG IRMA $agentId] handleMadAssignment: $globalName := $value from $fromAgent');

    if (globalName.isWriter) {
      // Case _w(p, i) := T - we localized this, search for entry
      _handleWriterAssignment(globalName, value, fromAgent);
    } else {
      // Case _r(p, i) := T - we globalized this, direct lookup
      _handleReaderAssignment(globalName, value, fromAgent);
    }
  }

  /// Handle _w(p, i) := T assignment (we localized _w(p,i))
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

    print('[DEBUG IRMA $agentId] _handleWriterAssignment: found entry, writerAddr=${entry.writerAddr}');

    // Bind the writer
    final activations = runtime.heap.bindVariable(entry.writerAddr, value);
    print('[DEBUG IRMA $agentId] _handleWriterAssignment: bound writer, ${activations.length} activations');

    // Reactivate suspended goals
    for (final act in activations) {
      runtime.enqueueReactivatedGoal(act);
    }

    // Remove the entry
    wp.removeLocalizeEntry(globalName.agent, globalName.index);
    print('[DEBUG IRMA $agentId] _handleWriterAssignment: entry removed');
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

    print('[DEBUG IRMA $agentId] _handleReaderAssignment: found entry, writerAddr=${entry.writerAddr}');

    // Bind the writer
    final activations = runtime.heap.bindVariable(entry.writerAddr, value);
    print('[DEBUG IRMA $agentId] _handleReaderAssignment: bound writer, ${activations.length} activations');

    // Reactivate suspended goals
    for (final act in activations) {
      runtime.enqueueReactivatedGoal(act);
    }

    // Remove the entry
    wp.removeGlobalizeEntry(globalName.index);
    print('[DEBUG IRMA $agentId] _handleReaderAssignment: entry removed');
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
    print('[DEBUG IRMA $agentId] handleMadAssignmentWithGlobalNames: $globalName with ${nestedGlobalNames.length} nested names');

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
}
