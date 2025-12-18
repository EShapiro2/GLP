/// Type table for Moded Type definitions
///
/// Stores user-defined type definitions parsed from GLP source.

import '../ast.dart';

/// Table mapping type names to their definitions
class TypeTable {
  final Map<String, TypeDefinition> _types = {};

  TypeTable();

  /// Add a type definition to the table
  /// If type already exists, merges constructors
  void addDefinition(TypeDefinition def) {
    final existing = _types[def.typeName];
    if (existing != null) {
      // Merge constructors
      final mergedConstructors = [...existing.constructors, ...def.constructors];
      _types[def.typeName] = TypeDefinition(
        def.typeName,
        existing.typeParams,  // Keep params from first definition
        mergedConstructors,
        existing.line,
        existing.column,
      );
    } else {
      _types[def.typeName] = def;
    }
  }

  /// Add a single constructor to an existing or new type
  void addConstructor(String typeName, TypeConstructor ctor, {List<String>? typeParams, int line = 0, int col = 0}) {
    final existing = _types[typeName];
    if (existing != null) {
      _types[typeName] = TypeDefinition(
        typeName,
        existing.typeParams,
        [...existing.constructors, ctor],
        existing.line,
        existing.column,
      );
    } else {
      _types[typeName] = TypeDefinition(
        typeName,
        typeParams ?? [],
        [ctor],
        line,
        col,
      );
    }
  }

  /// Get a type definition by name
  TypeDefinition? getType(String name) => _types[name];

  /// Check if a type exists
  bool hasType(String name) => _types.containsKey(name);

  /// Get all type names
  Iterable<String> get typeNames => _types.keys;

  /// Get all type definitions
  Iterable<TypeDefinition> get definitions => _types.values;

  /// Number of types in the table
  int get length => _types.length;

  /// Check if table is empty
  bool get isEmpty => _types.isEmpty;

  /// Create a TypeTable from a list of TypeDefinitions
  static TypeTable fromDefinitions(List<TypeDefinition> defs) {
    final table = TypeTable();
    for (final def in defs) {
      table.addDefinition(def);
    }
    return table;
  }

  /// Create a TypeTable from a Module's type definitions
  static TypeTable fromModule(Module module) {
    return fromDefinitions(module.typeDefinitions);
  }

  @override
  String toString() {
    final buffer = StringBuffer('TypeTable(\n');
    for (final def in _types.values) {
      buffer.writeln('  $def');
    }
    buffer.write(')');
    return buffer.toString();
  }
}
