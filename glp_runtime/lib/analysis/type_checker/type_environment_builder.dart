// lib/analysis/type_checker/type_environment_builder.dart
//
// Builds TypeEnvironment from a parsed Module.
// Loads prelude, merges with user definitions, validates.
// Resolves type aliases at preprocessing time.
//
// Specification: docs/modules/type-environment.md v0.7

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

/// Error for circular alias chain
class CircularAliasError implements Exception {
  final String message;
  final int line;
  final int column;

  CircularAliasError(this.message, this.line, this.column);

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

  // Add type definitions (including aliases - will be resolved later)
  for (final typeDef in module.typeDefs) {
    if (checkRedefinitions && isPredefinedType(typeDef.name)) {
      throw RedefinitionError(
        'Cannot redefine predefined type: ${typeDef.name}',
        typeDef.line,
        typeDef.column,
      );
    }
    // Note: Aliases are allowed (v0.7) - determinism check skipped for them
    if (!_isTypeAlias(typeDef)) {
      _checkDeterminism(typeDef);
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
    // Mark procedure as builtin if it's a true builtin (implemented in Dart)
    final isBuiltin = isBuiltinProcedure(procDecl.key);
    if (isBuiltin && !procDecl.isBuiltin) {
      // Create new ProcDecl with isBuiltin flag set
      procedures[procDecl.key] = ProcDecl(
        procDecl.name,
        procDecl.argTypes,
        procDecl.line,
        procDecl.column,
        isBuiltin: true,
      );
    } else {
      procedures[procDecl.key] = procDecl;
    }
  }

  // Resolve aliases (preprocessing step per v0.7 spec)
  _resolveAliases(types, procedures);

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
/// Per spec (type-environment.md v0.7):
/// Type aliases are permitted for documentation and readability.
/// They are resolved at preprocessing time.
/// 
/// Aliases are:
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

/// Resolve type aliases at preprocessing time.
///
/// Per spec (type-environment.md v0.7):
/// Aliases are fully resolved during preprocessing. Every occurrence of an
/// alias name is replaced by its target type. The type checker operates only
/// on resolved types. Circular alias chains are detected and rejected.
void _resolveAliases(Map<String, TypeDef> types, Map<String, ProcDecl> procedures) {
  // Step 1: Identify aliases
  final aliases = <String, TypeDef>{};
  for (final entry in types.entries) {
    if (_isTypeAlias(entry.value)) {
      aliases[entry.key] = entry.value;
    }
  }

  if (aliases.isEmpty) return;  // No aliases to resolve

  // Step 2: Resolve aliases transitively, detecting cycles
  final resolved = <String, TypeExpr>{};  // name -> final resolved TypeExpr
  final visiting = <String>{};  // Currently being resolved (for cycle detection)

  TypeExpr resolveAlias(String name) {
    if (resolved.containsKey(name)) {
      return resolved[name]!;
    }

    final aliasDef = aliases[name];
    if (aliasDef == null) {
      // Not an alias - return a TypeRef to it
      return TypeRef(name, 0, 0);
    }

    if (visiting.contains(name)) {
      throw CircularAliasError(
        'Circular alias chain detected: $name',
        aliasDef.line,
        aliasDef.column,
      );
    }

    visiting.add(name);

    final target = aliasDef.alternatives.first;
    TypeExpr result;

    if (target is TypeRef) {
      // Target is another type reference - check if it's also an alias
      if (aliases.containsKey(target.name)) {
        // Resolve transitively
        final resolvedTarget = resolveAlias(target.name);
        // Apply complement if needed: (T?)? = T
        result = _applyComplement(resolvedTarget, target.isInput, target.line, target.column);
      } else {
        // Target is a real type, keep as TypeRef
        result = target;
      }
    } else if (target is PrimitiveModeAlt) {
      // Target is _ or _?
      result = target;
    } else {
      // Shouldn't happen if _isTypeAlias is correct
      result = target;
    }

    visiting.remove(name);
    resolved[name] = result;
    return result;
  }

  // Resolve all aliases
  for (final name in aliases.keys) {
    resolveAlias(name);
  }

  // Step 3: Replace alias references in type definitions
  final nonAliasTypes = types.entries
      .where((e) => !aliases.containsKey(e.key))
      .toList();

  for (final entry in nonAliasTypes) {
    final newAlternatives = <TypeExpr>[];
    for (final alt in entry.value.alternatives) {
      newAlternatives.add(_replaceAliasReferences(alt, resolved));
    }
    types[entry.key] = TypeDef(
      entry.value.name,
      newAlternatives,
      entry.value.line,
      entry.value.column,
    );
  }

  // Step 4: Replace alias references in procedure declarations
  for (final entry in procedures.entries.toList()) {
    final newArgTypes = <TypeExpr>[];
    for (final argType in entry.value.argTypes) {
      newArgTypes.add(_replaceAliasReferences(argType, resolved));
    }
    procedures[entry.key] = ProcDecl(
      entry.value.name,
      newArgTypes,
      entry.value.line,
      entry.value.column,
      isBuiltin: entry.value.isBuiltin,
    );
  }

  // Step 5: Remove alias definitions from types map
  for (final name in aliases.keys) {
    types.remove(name);
  }
}

/// Apply complement to a TypeExpr if needed.
/// Implements the involution (T?)? = T.
TypeExpr _applyComplement(TypeExpr expr, bool applyComplement, int line, int column) {
  if (!applyComplement) return expr;

  if (expr is TypeRef) {
    return TypeRef(expr.name, line, column, isInput: !expr.isInput);
  } else if (expr is PrimitiveModeAlt) {
    return PrimitiveModeAlt(!expr.isInput, line, column);
  }
  return expr;
}

/// Replace alias references in a TypeExpr recursively.
TypeExpr _replaceAliasReferences(TypeExpr expr, Map<String, TypeExpr> resolved) {
  if (expr is TypeRef) {
    final resolvedTarget = resolved[expr.name];
    if (resolvedTarget != null) {
      // Replace with resolved target, applying complement if needed
      return _applyComplement(resolvedTarget, expr.isInput, expr.line, expr.column);
    }
    return expr;  // Not an alias, keep as-is
  }

  if (expr is PrimitiveModeAlt) {
    return expr;  // Primitives don't reference other types
  }

  if (expr is ConstantAlt) {
    return expr;  // Constants don't reference other types
  }

  if (expr is ListNilAlt) {
    return expr;  // Empty list doesn't reference other types
  }

  if (expr is ListConsAlt) {
    return ListConsAlt(
      _replaceAliasReferences(expr.head, resolved),
      _replaceAliasReferences(expr.tail, resolved),
      expr.line,
      expr.column,
    );
  }

  if (expr is StructAlt) {
    return StructAlt(
      expr.functor,
      expr.args.map((a) => _replaceAliasReferences(a, resolved)).toList(),
      expr.line,
      expr.column,
    );
  }

  if (expr is DiffListAlt) {
    return DiffListAlt(
      _replaceAliasReferences(expr.content, resolved),
      _replaceAliasReferences(expr.hole, resolved),
      expr.line,
      expr.column,
    );
  }

  return expr;  // Unknown type, return as-is
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
