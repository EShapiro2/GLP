// lib/analysis/type_checker/type_environment_builder.dart
//
// Builds TypeEnvironment from a parsed Module.
// Loads prelude, merges with user definitions, validates.
//
// Specification: docs/modules/type-environment.md v0.6

import 'type_ast.dart';
import 'prelude.dart';
import '../../compiler/ast.dart' as ast;
import '../../compiler/lexer.dart';
import '../../compiler/parser.dart';

/// Error for illegal type redefinition
class RedefinitionError implements Exception {
  final String message;
  final int line;
  final int column;

  RedefinitionError(this.message, this.line, this.column);

  @override
  String toString() => '$message at line $line, column $column';
}

/// Error for type alias (no new structure introduced)
class TypeAliasError implements Exception {
  final String message;
  final int line;
  final int column;

  TypeAliasError(this.message, this.line, this.column);

  @override
  String toString() => '$message at line $line, column $column';
}

/// Error for non-deterministic type (overlapping alternatives)
class NonDeterministicTypeError implements Exception {
  final String message;
  final int line;
  final int column;

  NonDeterministicTypeError(this.message, this.line, this.column);

  @override
  String toString() => '$message at line $line, column $column';
}

/// Build TypeEnvironment from prelude
TypeEnvironment buildPreludeEnvironment() {
  final lexer = Lexer(typePrelude);
  final tokens = lexer.tokenize();
  final parser = Parser(tokens);
  final module = parser.parseModule();

  return _buildEnvironmentFromModule(module, checkRedefinitions: false);
}

/// Build TypeEnvironment from a parsed Module
///
/// Loads prelude first, then merges user definitions.
/// Throws RedefinitionError if user redefines predefined types/procedures.
TypeEnvironment buildTypeEnvironment(ast.Module module) {
  // Load prelude
  final preludeEnv = buildPreludeEnvironment();

  // Build user environment
  final userEnv = _buildEnvironmentFromModule(module, checkRedefinitions: true);

  // Merge: prelude first, then user (user can shadow non-predefined)
  return preludeEnv.merge(userEnv);
}

/// Build TypeEnvironment from Module's type definitions and procedure declarations
TypeEnvironment _buildEnvironmentFromModule(
  ast.Module module, {
  required bool checkRedefinitions,
}) {
  final types = <String, TypeDef>{};
  final procedures = <String, ProcDecl>{};

  // Add type definitions
  for (final typeDef in module.typeDefs) {
    if (checkRedefinitions && isPredefinedType(typeDef.name)) {
      throw RedefinitionError(
        'Cannot redefine predefined type: ${typeDef.name}',
        typeDef.line,
        typeDef.column,
      );
    }
    // Check for type aliases (must introduce structure)
    if (_isTypeAlias(typeDef)) {
      throw TypeAliasError(
        'Type definition must introduce structure, not alias: ${typeDef.name}',
        typeDef.line,
        typeDef.column,
      );
    }
    // Check for determinism (alternatives must be distinguishable)
    _checkDeterminism(typeDef);
    types[typeDef.name] = typeDef;
  }

  // Add procedure declarations
  for (final procDecl in module.procDeclarations) {
    if (checkRedefinitions && isPredefinedProcedure(procDecl.name)) {
      throw RedefinitionError(
        'Cannot redefine predefined procedure: ${procDecl.name}/${procDecl.arity}',
        procDecl.line,
        procDecl.column,
      );
    }
    procedures[procDecl.key] = procDecl;
  }

  return TypeEnvironment(types, procedures);
}

/// Extract all clauses from a Module's procedures
List<ast.Clause> extractClauses(ast.Module module) {
  final clauses = <ast.Clause>[];
  for (final proc in module.procedures) {
    clauses.addAll(proc.clauses);
  }
  return clauses;
}

/// Check if a type definition is an alias (no new structure introduced)
/// 
/// Per spec (type-environment.md v0.5):
/// Type definitions must introduce new structure, not alias existing types.
/// 
/// Illegal aliases:
/// - Output ::= _.         (alias for primitive wildcard)
/// - Input ::= _?.          (alias for primitive wildcard complement)
/// - MyList ::= List.       (alias for defined type)
/// - MyStream ::= Stream?.  (alias for complement of defined type)
bool _isTypeAlias(TypeDef def) {
  // Multiple alternatives = not an alias (introduces structure through alternatives)
  if (def.alternatives.length != 1) return false;

  final alt = def.alternatives.first;

  // Single PrimitiveModeAlt (_ or _?) = alias
  if (alt is PrimitiveModeAlt) return true;

  // Single TypeRef (T or T?) = alias
  if (alt is TypeRef) return true;

  // Compound structures introduce new structure:
  // ConstantAlt, ListNilAlt, ListConsAlt, StructAlt, DiffListAlt
  return false;
}

/// Check if type alternatives are deterministic (distinguishable)
///
/// Per spec (type-environment.md v0.5):
/// Type definitions must be deterministic: alternatives must be distinguishable
/// by their top-level functor or, for primitive types, by disjoint type membership.
///
/// Throws NonDeterministicTypeError for:
/// - Two alternatives with same functor/arity
/// - Two constant alternatives with same value
/// - Primitive type alternatives that overlap (e.g., _ with anything, Number with Integer/Real)
void _checkDeterminism(TypeDef def) {
  final functors = <String>{};      // "functor/arity" keys
  final constants = <String>{};     // constant values
  final primitives = <String>{};    // primitive type names
  bool hasWildcard = false;

  for (final alt in def.alternatives) {
    if (alt is ConstantAlt) {
      final key = alt.value.toString();
      if (constants.contains(key)) {
        throw NonDeterministicTypeError(
          'Duplicate constant alternative: $key in ${def.name}',
          def.line, def.column);
      }
      constants.add(key);

    } else if (alt is ListNilAlt) {
      if (functors.contains('[]/0')) {
        throw NonDeterministicTypeError(
          'Duplicate [] alternative in ${def.name}',
          def.line, def.column);
      }
      functors.add('[]/0');

    } else if (alt is ListConsAlt) {
      if (functors.contains('[|]/2')) {
        throw NonDeterministicTypeError(
          'Duplicate [|] alternative in ${def.name}',
          def.line, def.column);
      }
      functors.add('[|]/2');

    } else if (alt is StructAlt) {
      final key = '${alt.functor}/${alt.args.length}';
      if (functors.contains(key)) {
        throw NonDeterministicTypeError(
          'Duplicate functor alternative: $key in ${def.name}',
          def.line, def.column);
      }
      functors.add(key);

    } else if (alt is DiffListAlt) {
      if (functors.contains('\\/2')) {
        throw NonDeterministicTypeError(
          'Duplicate \\ alternative in ${def.name}',
          def.line, def.column);
      }
      functors.add('\\/2');

    } else if (alt is PrimitiveModeAlt) {
      // _ or _? - wildcards overlap with everything
      if (hasWildcard || primitives.isNotEmpty) {
        throw NonDeterministicTypeError(
          'Wildcard _ overlaps with other alternatives in ${def.name}',
          def.line, def.column);
      }
      hasWildcard = true;

    } else if (alt is TypeRef) {
      // TypeRef in alternative position = primitive type reference
      final name = alt.name;
      if ({'Integer', 'Real', 'Number', 'String'}.contains(name)) {
        _checkPrimitiveOverlap(name, primitives, hasWildcard, def);
        primitives.add(name);
      }
    }
  }
}

void _checkPrimitiveOverlap(String newPrimitive, Set<String> existing, bool hasWildcard, TypeDef def) {
  // Wildcard overlaps with everything
  if (hasWildcard) {
    throw NonDeterministicTypeError(
      'Wildcard _ overlaps with $newPrimitive in ${def.name}',
      def.line, def.column);
  }

  // Number overlaps with Integer and Real
  if (newPrimitive == 'Number' &&
      (existing.contains('Integer') || existing.contains('Real'))) {
    throw NonDeterministicTypeError(
      'Number overlaps with Integer/Real in ${def.name}',
      def.line, def.column);
  }
  if ((newPrimitive == 'Integer' || newPrimitive == 'Real') &&
      existing.contains('Number')) {
    throw NonDeterministicTypeError(
      '$newPrimitive overlaps with Number in ${def.name}',
      def.line, def.column);
  }

  // Direct duplicate
  if (existing.contains(newPrimitive)) {
    throw NonDeterministicTypeError(
      'Duplicate primitive type $newPrimitive in ${def.name}',
      def.line, def.column);
  }
}
