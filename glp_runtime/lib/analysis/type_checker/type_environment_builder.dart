// lib/analysis/type_checker/type_environment_builder.dart
//
// Builds TypeEnvironment from a parsed Module.
// Loads root scope, merges with user definitions, validates.
// Resolves type aliases at preprocessing time.
//
// Specification: docs/modules/type-environment.md v0.8

import 'type_ast.dart';
import 'root_scope.dart';
import 'param_expansion.dart';
import 'program_dfa.dart' show primitiveClosure;
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

/// Error for union alias expansion failure
class AliasExpansionError implements Exception {
  final String message;
  final int line;
  final int column;

  AliasExpansionError(this.message, this.line, this.column);

  @override
  String toString() => '$message at line $line, column $column';
}

/// The root scope's type definitions, recorded when the root scope is built.
/// The determinism check consults them so that a module writing
/// `T ::= Number ; Integer.` is caught even though `Number` is defined in the
/// root `self.glp` rather than in the module itself.
Map<String, TypeDef> rootScopeTypeDefs = const {};

/// Source for the root scope environment (set by engine from programs/self.glp).
String? _rootScopeEnvironmentSource;

/// Set the source from which the root scope environment is built.
/// Call this once during engine initialization with the content of programs/self.glp.
void setRootScopeEnvironmentSource(String source) {
  _rootScopeEnvironmentSource = source;
}

/// Build TypeEnvironment from root scope
TypeEnvironment buildRootScopeEnvironment() {
  final source = _rootScopeEnvironmentSource ?? rootScopeTypes;
  if (source.isEmpty) {
    return TypeEnvironment({}, {});
  }

  final lexer = Lexer(source);
  final tokens = lexer.tokenize();
  final parser = Parser(tokens);
  final module = parser.parseModule();

  // Extract templates before expansion removes them.
  // These are passed to downstream modules so they can expand references
  // to root scope-defined parameterized types (e.g., Stream(X), Channel(X,Y)).
  final rootScopeTemplates = <String, TypeDef>{};
  for (final td in module.typeDefs) {
    if (td.isParameterized) {
      rootScopeTemplates[td.name] = td;
    }
  }

  // Expand parameterized types before building the environment.
  // Templates (e.g., Stream(X)) are removed; only concrete expansions remain.
  final expandedModule = expandParameterizedTypes(module);

  final env = _buildEnvironmentFromModule(expandedModule, checkRedefinitions: false, resolveAliasesNow: true);
  rootScopeTypeDefs = Map<String, TypeDef>.unmodifiable(env.types);
  return TypeEnvironment(env.types, env.procedures,
      paramProcDecls: env.paramProcDecls,
      typeTemplates: rootScopeTemplates);
}

/// Build TypeEnvironment from a parsed Module
///
/// Loads root scope first, then merges user definitions.
/// Throws RedefinitionError if user redefines predefined types/procedures.
///
/// If [ancestorScope] is provided, it is used as the base environment
/// instead of just the root scope. The ancestor scope should already include
/// the root scope and all ancestor self.glp definitions (built by
/// assembleTypeScope in module_hierarchy.dart). The module's own
/// definitions are then merged on top (shadowing ancestors).
TypeEnvironment buildTypeEnvironment(ast.Module module, {TypeEnvironment? ancestorScope}) {
  // Base environment: ancestor scope if provided, otherwise just root scope
  final baseEnv = ancestorScope ?? buildRootScopeEnvironment();

  // Build user environment WITHOUT resolving aliases yet
  final userEnv = _buildEnvironmentFromModule(module, checkRedefinitions: ancestorScope == null, resolveAliasesNow: false);

  // Merge: base first, then user (user can shadow non-predefined)
  final merged = baseEnv.merge(userEnv);

  // Now resolve aliases on the merged environment (so user aliases can reference root scope types)
  final types = Map<String, TypeDef>.from(merged.types);
  final procedures = Map<String, ProcDecl>.from(merged.procedures);
  _resolveAliases(types, procedures);

  // Innermost-first shadowing (spec §3.2/§3.3): a module-local MONOMORPHIC
  // procedure shadows an inherited parameterized template of the same key — so
  // the template must not survive in paramProcDecls (else call-site inference,
  // Case B, would fire for calls the local definition resolves).  A module's
  // OWN parameterized procedure keeps its template (it is in userEnv's
  // paramProcDecls).  This is the defining-module dual of the exposed-scope
  // shadowing in the program linker.
  final paramProcDecls = Map<String, ProcDecl>.from(merged.paramProcDecls);
  for (final key in userEnv.procedures.keys) {
    if (!userEnv.paramProcDecls.containsKey(key)) {
      paramProcDecls.remove(key);
    }
  }

  return TypeEnvironment(types, procedures, paramProcDecls: paramProcDecls);
}

/// Build TypeEnvironment from Module's type definitions and procedure declarations
TypeEnvironment _buildEnvironmentFromModule(
  ast.Module module, {
  required bool checkRedefinitions,
  required bool resolveAliasesNow,
}) {
  final types = <String, TypeDef>{};
  final procedures = <String, ProcDecl>{};
  final paramProcDecls = <String, ProcDecl>{};

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
      _checkDeterminism(typeDef, types);
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
      procedures[procDecl.qualifiedKey] = ProcDecl(
        procDecl.name,
        procDecl.argTypes,
        procDecl.line,
        procDecl.column,
        isBuiltin: true,
        exported: procDecl.exported,
        imported: procDecl.imported,
        modulePath: procDecl.modulePath,
      );
    } else {
      procedures[procDecl.qualifiedKey] = procDecl;
    }
  }

  // Add parameterized proc decl templates (for call-site inference)
  for (final paramDecl in module.paramProcDecls) {
    paramProcDecls[paramDecl.qualifiedKey] = paramDecl;
  }

  // Resolve aliases (preprocessing step per v0.8 spec) - only if requested
  if (resolveAliasesNow) {
    _resolveAliases(types, procedures);
  }

  return TypeEnvironment(types, procedures, paramProcDecls: paramProcDecls);
}

/// Extract all clauses from a Module's procedures
List<ast.Clause> extractClauses(ast.Module module) {
  final clauses = <ast.Clause>[];
  for (final proc in module.procedures) {
    clauses.addAll(proc.clauses);
  }
  return clauses;
}

/// Check if a type definition is a simple alias (single type reference)
/// 
/// Per spec (type-environment.md v0.8):
/// Simple aliases have a single alternative that is a TypeRef or PrimitiveModeAlt:
/// - Output ::= _.         (alias for primitive wildcard)
/// - Input ::= _?.          (alias for primitive wildcard complement)  
/// - MyList ::= List.       (alias for defined type)
/// - MyStream ::= Stream?.  (alias for complement of defined type)
bool _isSimpleAlias(TypeDef def) {
  if (def.alternatives.length != 1) return false;

  final alt = def.alternatives.first;

  // Single PrimitiveModeAlt (_ or _?) = simple alias
  if (alt is PrimitiveModeAlt) return true;

  // Single TypeRef (T or T?) = simple alias
  if (alt is TypeRef) return true;

  return false;
}

/// Check if a type definition is a union alias (multiple type references to defined types)
///
/// Per spec (type-environment.md v0.8):
/// Union aliases have multiple alternatives, all of which are TypeRefs to
/// user-defined types (not predefined primitives):
/// - Msg ::= NetMsg ; UserMsg.   (union of two user-defined message types)
///
/// NOT a union alias:
/// - Key ::= Number ; String.  (references predefined primitives)
bool _isUnionAlias(TypeDef def) {
  // Must have multiple alternatives
  if (def.alternatives.length < 2) return false;

  // All alternatives must be TypeRefs to non-predefined types
  for (final alt in def.alternatives) {
    if (alt is! TypeRef) return false;
    // If it references a predefined type, it's not a union alias
    final typeName = (alt as TypeRef).name;
    if (isPredefinedType(typeName)) {
      return false;
    }
  }

  return true;
}

/// Check if a type definition is any kind of alias
bool _isTypeAlias(TypeDef def) {
  return _isSimpleAlias(def) || _isUnionAlias(def);
}

/// Resolve type aliases at preprocessing time.
///
/// Per spec (type-environment.md v0.8):
/// - Simple aliases are resolved transitively and replaced everywhere
/// - Union aliases are expanded by collecting alternatives from referenced types
/// - Circular alias chains are detected and rejected
void _resolveAliases(Map<String, TypeDef> types, Map<String, ProcDecl> procedures) {
  // Step 1: Identify simple and union aliases
  final simpleAliases = <String, TypeDef>{};
  final unionAliases = <String, TypeDef>{};
  
  for (final entry in types.entries) {
    if (_isSimpleAlias(entry.value)) {
      simpleAliases[entry.key] = entry.value;
    } else if (_isUnionAlias(entry.value)) {
      unionAliases[entry.key] = entry.value;
    }
  }

  if (simpleAliases.isEmpty && unionAliases.isEmpty) return;

  // Step 2: Resolve simple aliases transitively, detecting cycles
  final resolved = <String, TypeExpr>{};  // name -> final resolved TypeExpr
  final visiting = <String>{};  // Currently being resolved (for cycle detection)

  TypeExpr resolveSimpleAlias(String name) {
    if (resolved.containsKey(name)) {
      return resolved[name]!;
    }

    final aliasDef = simpleAliases[name];
    if (aliasDef == null) {
      // Not a simple alias - return a TypeRef to it
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
      if (simpleAliases.containsKey(target.name)) {
        // Resolve transitively
        final resolvedTarget = resolveSimpleAlias(target.name);
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
      // Shouldn't happen if _isSimpleAlias is correct
      result = target;
    }

    visiting.remove(name);
    resolved[name] = result;
    return result;
  }

  // Resolve all simple aliases
  for (final name in simpleAliases.keys) {
    resolveSimpleAlias(name);
  }

  // Step 3: Expand union aliases
  for (final entry in unionAliases.entries) {
    final name = entry.key;
    final def = entry.value;
    final expandedAlts = <TypeExpr>[];

    for (final alt in def.alternatives) {
      final ref = alt as TypeRef;  // Already verified by _isUnionAlias
      
      // Check: union alias cannot reference another alias
      if (simpleAliases.containsKey(ref.name) || unionAliases.containsKey(ref.name)) {
        throw AliasExpansionError(
          'Union alias cannot reference another alias: ${ref.name}',
          def.line,
          def.column,
        );
      }

      // Check if this is a predefined type - if so, keep the TypeRef as an alternative
      if (isPredefinedType(ref.name)) {
        expandedAlts.add(ref);
        continue;
      }

      // Get the target type definition
      final targetDef = types[ref.name];
      if (targetDef == null) {
        throw AliasExpansionError(
          'Union alias references undefined type: ${ref.name}',
          def.line,
          def.column,
        );
      }

      // Collect alternatives from the target type, applying complement if needed
      for (final targetAlt in targetDef.alternatives) {
        expandedAlts.add(_applyComplementToAlt(targetAlt, ref.isInput, def.line, def.column));
      }
    }

    // Replace union alias definition with expanded definition
    final expandedDef = TypeDef(name, expandedAlts, def.line, def.column);
    
    // Check determinism of expanded type
    _checkDeterminism(expandedDef, types);
    
    types[name] = expandedDef;
  }

  // Step 4: Replace simple alias references in all type definitions
  final nonSimpleAliasTypes = types.entries
      .where((e) => !simpleAliases.containsKey(e.key))
      .toList();

  for (final entry in nonSimpleAliasTypes) {
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

  // Step 5: Replace alias references in procedure declarations
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
      exported: entry.value.exported,
      imported: entry.value.imported,
      modulePath: entry.value.modulePath,
    );
  }

  // Step 6: Remove simple alias definitions from types map
  // (Union aliases are already expanded in place, so keep them)
  for (final name in simpleAliases.keys) {
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

/// Apply complement to all type references within a type alternative.
/// Used for union alias expansion when the reference is complemented (e.g., Msg ::= NetMsg?).
TypeExpr _applyComplementToAlt(TypeExpr alt, bool applyComplement, int line, int column) {
  if (!applyComplement) return alt;

  if (alt is TypeRef) {
    return TypeRef(alt.name, line, column, isInput: !alt.isInput);
  } else if (alt is PrimitiveModeAlt) {
    return PrimitiveModeAlt(!alt.isInput, line, column);
  } else if (alt is ConstantAlt) {
    return alt;  // Constants don't have modes
  } else if (alt is ListNilAlt) {
    return alt;  // [] doesn't have modes
  } else if (alt is ListConsAlt) {
    return ListConsAlt(
      _applyComplementToAlt(alt.head, true, line, column),
      _applyComplementToAlt(alt.tail, true, line, column),
      line,
      column,
    );
  } else if (alt is StructAlt) {
    return StructAlt(
      alt.functor,
      alt.args.map((a) => _applyComplementToAlt(a, true, line, column)).toList(),
      line,
      column,
    );
  } else if (alt is DiffListAlt) {
    return DiffListAlt(
      _applyComplementToAlt(alt.content, true, line, column),
      _applyComplementToAlt(alt.hole, true, line, column),
      line,
      column,
    );
  }
  return alt;
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
/// - Bare type-name alternatives that overlap (e.g., _ with anything, or
///   `Number` with `Integer`, since `Number ::= Integer ; Real.`)
///
/// [scope] supplies the type definitions a bare type-name alternative may
/// refer to, so overlap is decided by what each alternative actually accepts
/// rather than by a built-in table of type names.
void _checkDeterminism(TypeDef def, [Map<String, TypeDef> scope = const {}]) {
  final functors = <String>{};      // "functor/arity" keys
  final constants = <String>{};     // constant values
  final primitives = <String>{};    // primitives reached by bare type-name alternatives
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
      // A bare type-name alternative stands for everything the named type
      // accepts.  Two such alternatives clash when the primitives they reach
      // intersect — `Number` and `Integer` clash because `Number` reaches
      // `Integer`.
      final reached = _reachedPrimitives(alt, scope);
      if (reached.isNotEmpty) {
        _checkPrimitiveOverlap(alt.name, reached, primitives, hasWildcard, def);
        primitives.addAll(reached);
      }
    }
  }
}

/// The primitive types a bare type-name alternative accepts, following its
/// definition transitively.  Empty when the name is not a type reference this
/// scope can resolve.
Set<String> _reachedPrimitives(TypeRef alt, Map<String, TypeDef> scope) {
  if (alt.isParameterized) return const {};
  if (TypeRef.builtins.contains(alt.name)) return {alt.name};
  final def = scope[alt.name] ?? rootScopeTypeDefs[alt.name];
  if (def == null) return const {};
  return primitiveClosure(def, {...rootScopeTypeDefs, ...scope});
}

void _checkPrimitiveOverlap(String altName, Set<String> reached,
    Set<String> existing, bool hasWildcard, TypeDef def) {
  // Wildcard overlaps with everything
  if (hasWildcard) {
    throw NonDeterministicTypeError(
      'Wildcard _ overlaps with $altName in ${def.name}',
      def.line, def.column);
  }

  final clash = reached.where(existing.contains);
  if (clash.isNotEmpty) {
    throw NonDeterministicTypeError(
      '$altName overlaps with an earlier alternative on ${clash.join('/')} in ${def.name}',
      def.line, def.column);
  }
}
