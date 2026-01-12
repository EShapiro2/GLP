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
