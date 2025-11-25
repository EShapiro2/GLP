import '../bytecode/opcodes.dart' show Op;

/// A compiled module ready for execution.
///
/// Each module has its own instruction list and label table,
/// providing namespace separation from other modules.
class LoadedModule {
  /// Module name (e.g., 'math', 'utils.list')
  final String name;

  /// Exported procedure signatures ("proc/arity")
  /// Empty set means all procedures are exported
  final Set<String> exports;

  /// Imported module names
  final List<String> imports;

  /// This module's instructions
  final List<Op> instructions;

  /// Label table: label name → PC offset
  final Map<String, int> labels;

  /// Procedure entry points: "proc/arity" → PC offset
  final Map<String, int> procOffsets;

  LoadedModule({
    required this.name,
    required this.exports,
    required this.imports,
    required this.instructions,
    required this.labels,
    required this.procOffsets,
  });

  /// Check if procedure is exported.
  /// Empty exports set means all procedures are exported (legacy mode).
  bool isExported(String proc, int arity) {
    return exports.isEmpty || exports.contains('$proc/$arity');
  }

  /// Get procedure entry point PC, or null if not found.
  int? getProcOffset(String proc, int arity) {
    return procOffsets['$proc/$arity'];
  }

  /// Get all procedure signatures defined in this module.
  Iterable<String> get procedures => procOffsets.keys;

  @override
  String toString() => 'LoadedModule($name, ${procOffsets.length} procedures)';
}
