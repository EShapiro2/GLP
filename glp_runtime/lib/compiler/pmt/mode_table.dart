/// PMT Mode Table: Stores mode declarations for type checking
///
/// Maps predicate signatures (e.g., "merge/3") to their argument modes
/// (reader/writer for each position).

import '../ast.dart';

/// Argument mode in a PMT declaration
enum Mode {
  reader,  // Input: value flows from caller to callee
  writer,  // Output: value flows from callee to caller
}

/// Error thrown when a duplicate mode declaration is detected
class DuplicateModeDeclarationError implements Exception {
  final String signature;
  final int line;
  final int column;

  DuplicateModeDeclarationError(this.signature, this.line, this.column);

  @override
  String toString() =>
      'Duplicate mode declaration for $signature at line $line, column $column';
}

/// Mode table: stores and retrieves mode declarations by predicate signature
class ModeTable {
  final Map<String, List<Mode>> _modes = {};
  final Map<String, ModeDeclaration> _declarations = {};

  /// Add a mode declaration to the table
  ///
  /// Throws [DuplicateModeDeclarationError] if a declaration for the same
  /// predicate/arity already exists.
  void addDeclaration(ModeDeclaration decl) {
    final signature = decl.signature;

    if (_modes.containsKey(signature)) {
      throw DuplicateModeDeclarationError(signature, decl.line, decl.column);
    }

    // Extract modes from moded arguments
    final modes = decl.args.map((arg) => arg.isReader ? Mode.reader : Mode.writer).toList();

    _modes[signature] = modes;
    _declarations[signature] = decl;
  }

  /// Get modes for a predicate with given arity
  ///
  /// Returns null if no declaration exists.
  List<Mode>? getModes(String predicate, int arity) {
    return _modes['$predicate/$arity'];
  }

  /// Check if a declaration exists for predicate/arity
  bool hasDeclaration(String predicate, int arity) {
    return _modes.containsKey('$predicate/$arity');
  }

  /// Get the original declaration for a predicate/arity
  ///
  /// Returns null if no declaration exists.
  ModeDeclaration? getDeclaration(String predicate, int arity) {
    return _declarations['$predicate/$arity'];
  }

  /// Get all declared signatures
  Iterable<String> get signatures => _modes.keys;

  /// Get number of declarations
  int get length => _modes.length;

  /// Check if table is empty
  bool get isEmpty => _modes.isEmpty;

  /// Build a mode table from a list of mode declarations
  static ModeTable fromDeclarations(List<ModeDeclaration> declarations) {
    final table = ModeTable();
    for (final decl in declarations) {
      table.addDeclaration(decl);
    }
    return table;
  }
}
