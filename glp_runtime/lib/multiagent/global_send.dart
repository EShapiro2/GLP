/// Global Send mechanism for madGLP
///
/// Implements the `global_send` goal that watches a reader and sends
/// its value to a remote agent when it becomes known.
///
/// See: madGLP-spec.md Section 4 (The global_send Predicate)
library;

import 'mad_helpers.dart';
import 'global_writers_table.dart';

/// A pending global_send goal waiting for a reader to become known
///
/// Spawned symmetrically when:
/// - Globalizing a reader Y?: watches Y?, sends to destination
/// - Localizing _w(p,i): watches Y_q?, sends back to p
///
/// See: madGLP-spec.md Section 4
class GlobalSendGoal {
  /// Address of the reader to watch
  final int readerAddr;

  /// Global name identifying the link (_w(p,i) or _r(p,i))
  final GlobalName globalName;

  /// Agent to send the message to
  final String destination;

  GlobalSendGoal({
    required this.readerAddr,
    required this.globalName,
    required this.destination,
  });

  /// Create from a GlobalSendSpawn (conversion from spawn info to goal)
  factory GlobalSendGoal.fromSpawn(GlobalSendSpawn spawn) {
    return GlobalSendGoal(
      readerAddr: spawn.readerAddr,
      globalName: spawn.globalName,
      destination: spawn.destAgent,
    );
  }

  @override
  String toString() =>
      'GlobalSendGoal(reader=$readerAddr, name=$globalName, dest=$destination)';
}

/// Result of firing a global_send goal
///
/// Contains the message to be sent and any new goals spawned
/// for nested variables in the value.
class GlobalSendFiredResult {
  /// The global name for the message (G in "G := T")
  final GlobalName globalName;

  /// Destination agent
  final String destination;

  /// The original value (before globalization)
  final Object? value;

  /// New goals spawned for nested variables in the value
  final List<GlobalSendGoal> newGoals;

  /// The extracted variables from the value (for globalize-term-with-result)
  final List<TermVar> extractedVariables;

  /// The globalize result (for globalize-term-with-result)
  final GlobalizeResult globalizeResult;

  GlobalSendFiredResult({
    required this.globalName,
    required this.destination,
    required this.value,
    required this.newGoals,
    required this.extractedVariables,
    required this.globalizeResult,
  });
}

/// Registry for pending global_send goals
///
/// Maps reader addresses to goals waiting for those readers to become known.
/// When a writer is bound, call onWriterBound() to fire any matching goals.
///
/// See: madGLP-spec.md Section 4, Implementation Plan Section 3.2
class GlobalSendRegistry {
  /// Agent ID for this registry (used when globalizing values)
  final String agentId;

  /// Pending goals indexed by reader address
  final Map<int, GlobalSendGoal> _goals = {};

  GlobalSendRegistry(this.agentId);

  /// Register a goal to watch a reader
  ///
  /// If the reader is already known (bound), the goal should fire immediately.
  /// This is handled by the caller checking the reader state before registering.
  void register(GlobalSendGoal goal) {
    _goals[goal.readerAddr] = goal;
  }

  /// Register multiple goals from spawn information
  void registerSpawns(List<GlobalSendSpawn> spawns) {
    for (final spawn in spawns) {
      register(GlobalSendGoal.fromSpawn(spawn));
    }
  }

  /// Check if there's a goal watching this reader address
  bool hasGoalFor(int readerAddr) => _goals.containsKey(readerAddr);

  /// Get the goal watching this reader address (if any)
  GlobalSendGoal? getGoalFor(int readerAddr) => _goals[readerAddr];

  /// Called when a writer is bound to a value.
  ///
  /// If there's a goal watching this writer's reader, fires the goal:
  /// 1. Removes the goal (one-shot)
  /// 2. Globalizes the value (may produce new goals for nested variables)
  /// 3. Returns the result with message info and new goals
  ///
  /// The caller is responsible for:
  /// - Actually sending the message to the destination
  /// - Registering any new goals returned
  ///
  /// Returns null if no goal was watching this writer.
  ///
  /// Spec Section 12 (Goal Atomicity): The globalization and message creation
  /// must happen atomically. New goals for nested variables must be registered
  /// before the current operation completes.
  GlobalSendFiredResult? onWriterBound({
    required int writerAddr,
    required Object? value,
    required GlobalWritersTable table,
    required List<TermVar> Function(Object?) extractVariables,
  }) {
    // The writer and reader share the same address in our model
    final goal = _goals.remove(writerAddr);
    if (goal == null) return null;

    // Globalize the value (may spawn new goals for nested variables)
    final variables = extractVariables(value);
    final globalizeResult = globalize(
      variables: variables,
      localAgent: agentId,
      remoteAgent: goal.destination,
      table: table,
    );

    // Convert spawns to goals
    final newGoals = globalizeResult.spawns
        .map((s) => GlobalSendGoal.fromSpawn(s))
        .toList();

    return GlobalSendFiredResult(
      globalName: goal.globalName,
      destination: goal.destination,
      value: value,
      newGoals: newGoals,
      extractedVariables: variables,
      globalizeResult: globalizeResult,
    );
  }

  /// Number of pending goals (for testing)
  int get pendingCount => _goals.length;

  /// Clear all pending goals (for testing)
  void clear() => _goals.clear();

  @override
  String toString() {
    final buf = StringBuffer('GlobalSendRegistry($agentId)\n');
    for (final entry in _goals.entries) {
      buf.writeln('  [${entry.key}] ${entry.value}');
    }
    return buf.toString();
  }
}
