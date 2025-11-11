/// System predicate execution infrastructure for GLP
///
/// System predicates are external functions (implemented in Dart) that can be
/// called from GLP programs via the `execute` instruction. They provide:
/// - I/O operations (file, terminal, network)
/// - Arithmetic evaluation
/// - System information (time, IDs, etc.)
/// - Any operation requiring side effects or host interaction
///
/// Inspired by FCP's execute mechanism but adapted for Dart.

import 'runtime.dart';
import 'terms.dart';

/// Result of executing a system predicate
enum SystemResult {
  /// Predicate succeeded - continue execution
  success,

  /// Predicate failed - try next clause or fail goal
  failure,

  /// Predicate suspended - waiting for readers to be bound
  suspend,
}

/// System predicate call context
/// Contains arguments and collects suspended readers if predicate blocks
class SystemCall {
  /// Name of the predicate being called
  final String name;

  /// Arguments passed to the predicate (may be writers, readers, or ground terms)
  final List<Object?> args;

  /// Readers that caused suspension (filled by predicate if it suspends)
  final Set<int> suspendedReaders = {};

  SystemCall(this.name, this.args);
}

/// System predicate function signature
///
/// Takes:
/// - GlpRuntime: access to heap, writer/reader operations
/// - SystemCall: arguments and suspension tracking
///
/// Returns:
/// - SystemResult indicating success/failure/suspend
///
/// Can modify:
/// - Writer bindings (via rt.bindWriter)
/// - call.suspendedReaders (if suspending)
typedef SystemPredicate = SystemResult Function(
  GlpRuntime rt,
  SystemCall call,
);

/// Registry of system predicates
///
/// System predicates are registered at runtime initialization and can be
/// called via the Execute opcode.
class SystemPredicateRegistry {
  final Map<String, SystemPredicate> _predicates = {};

  /// Register a system predicate
  void register(String name, SystemPredicate predicate) {
    _predicates[name] = predicate;
  }

  /// Look up a system predicate by name
  SystemPredicate? lookup(String name) => _predicates[name];

  /// Check if a predicate is registered
  bool has(String name) => _predicates.containsKey(name);

  /// Get all registered predicate names
  Iterable<String> get names => _predicates.keys;
}
