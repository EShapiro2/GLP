/// PMT Mode Table: Stores mode declarations for type checking
///
/// Maps predicate signatures (e.g., "merge/3") to their argument modes
/// (reader/writer for each position).
///
/// Supports multiple mode alternatives for the same predicate/arity
/// (union of mode declarations).

import '../ast.dart';

/// Argument mode in a PMT declaration
enum Mode {
  reader,  // Input: value flows from caller to callee
  writer,  // Output: value flows from callee to caller
}

/// Mode table: stores and retrieves mode declarations by predicate signature
///
/// Supports multiple mode alternatives per predicate/arity for union declarations:
/// ```
/// Observe := observe(Any?, Any, Any?) | observe(Any, Any?, Any?).
/// ```
class ModeTable {
  /// Maps predicate/arity to list of mode alternatives
  final Map<String, List<List<Mode>>> _modes = {};

  /// Maps predicate/arity to list of declarations (one per alternative)
  final Map<String, List<ModeDeclaration>> _declarations = {};

  /// Add a mode declaration to the table
  ///
  /// If a declaration for the same predicate/arity already exists,
  /// adds this as an alternative mode (for union declarations).
  void addDeclaration(ModeDeclaration decl) {
    final signature = decl.signature;

    // Extract modes from moded arguments
    final modes = decl.args.map((arg) => arg.isReader ? Mode.reader : Mode.writer).toList();

    // Add to existing alternatives or create new list
    _modes.putIfAbsent(signature, () => []).add(modes);
    _declarations.putIfAbsent(signature, () => []).add(decl);
  }

  /// Get the first/primary mode for a predicate with given arity
  ///
  /// Returns null if no declaration exists.
  /// For predicates with multiple modes, use [getAllModes].
  List<Mode>? getModes(String predicate, int arity) {
    final allModes = _modes['$predicate/$arity'];
    return allModes?.isNotEmpty == true ? allModes!.first : null;
  }

  /// Get all mode alternatives for a predicate with given arity
  ///
  /// Returns null if no declaration exists.
  /// Returns list of mode signatures (each is a List<Mode>).
  List<List<Mode>>? getAllModes(String predicate, int arity) {
    return _modes['$predicate/$arity'];
  }

  /// Check if a declaration exists for predicate/arity
  bool hasDeclaration(String predicate, int arity) {
    return _modes.containsKey('$predicate/$arity');
  }

  /// Check if predicate has multiple mode alternatives
  bool hasMultipleModes(String predicate, int arity) {
    final allModes = _modes['$predicate/$arity'];
    return allModes != null && allModes.length > 1;
  }

  /// Get the first/primary declaration for a predicate/arity
  ///
  /// Returns null if no declaration exists.
  ModeDeclaration? getDeclaration(String predicate, int arity) {
    final allDecls = _declarations['$predicate/$arity'];
    return allDecls?.isNotEmpty == true ? allDecls!.first : null;
  }

  /// Get all declarations for a predicate/arity
  ///
  /// Returns null if no declaration exists.
  List<ModeDeclaration>? getAllDeclarations(String predicate, int arity) {
    return _declarations['$predicate/$arity'];
  }

  /// Get a declaration by its type name (e.g., "And" for "And := and(...)")
  ///
  /// Returns null if no declaration with that type name exists.
  ModeDeclaration? getDeclarationByTypeName(String typeName) {
    for (final decls in _declarations.values) {
      for (final decl in decls) {
        if (decl.typeName == typeName) {
          return decl;
        }
      }
    }
    return null;
  }

  /// Get all declared signatures
  Iterable<String> get signatures => _modes.keys;

  /// Get number of unique predicates (not counting alternatives)
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
