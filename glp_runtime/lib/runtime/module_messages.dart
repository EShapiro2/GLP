/// Module system message types following FCP design
/// Based on FCP domain_server.cp message format

import 'package:glp_runtime/runtime/terms.dart';

/// Call information for debugging/tracing
/// Following FCP's call_info structure
class CallInfo {
  final String sourceModule;
  final int line;
  final int column;

  CallInfo(this.sourceModule, this.line, this.column);

  @override
  String toString() => 'CallInfo($sourceModule:$line:$column)';
}

/// RPC message format following FCP's export message
/// Structure: export(CallInfo, Scope, Goal, CCC)
///
/// In trust mode (v1), CCC is empty map and ignored.
class ExportMessage {
  final CallInfo callInfo;
  final List<String> scope;  // Module path for hierarchical resolution
  final String functor;      // Goal functor
  final int arity;           // Goal arity
  final List<Term> args;     // Goal arguments
  final Map<String, dynamic> ccc;  // Computation Control Context (ignored in trust mode)

  ExportMessage({
    required this.callInfo,
    required this.scope,
    required this.functor,
    required this.arity,
    required this.args,
    this.ccc = const {},
  });

  /// Create a message with minimal info (trust mode)
  factory ExportMessage.trust({
    required String sourceModule,
    required String functor,
    required int arity,
    required List<Term> args,
  }) {
    return ExportMessage(
      callInfo: CallInfo(sourceModule, 0, 0),
      scope: [],
      functor: functor,
      arity: arity,
      args: args,
      ccc: const {},
    );
  }

  String get signature => '$functor/$arity';

  @override
  String toString() => 'ExportMessage($signature from ${callInfo.sourceModule})';
}
