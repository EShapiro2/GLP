// lib/analysis/type_checker/type_environment_builder.dart
//
// Builds TypeEnvironment from a parsed Module.
// Merges its definitions over the scope passed in, validates.
// Resolves type aliases at preprocessing time.
//
// Specification: TGLP, the scope a module is checked in (Definition "Root,
// Scope") and its appendix "Type Aliases".

import 'type_ast.dart';
import 'root_scope.dart';
import 'param_expansion.dart';
import 'program_dfa.dart' show primitiveClosure, constantLabel;
import '../../compiler/ast.dart' as ast;

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

/// Build TypeEnvironment from a parsed Module
///
/// Merges the module's definitions over [ancestorScope], which they shadow:
/// the root self.glp is "the outermost ancestor scope, shadowable like any
/// other" (TGLP appendix-root-self.tex).  Until 2026-10-02 a module checked
/// with no ancestor scope could not redefine Integer ... Stream, OpenStream or
/// the built-in guard names: a RedefinitionError no paper stated.
///
/// [ancestorScope] is the scope the module is checked in, passed in: the
/// language primitives, the root self.glp and every ancestor self.glp (built
/// by buildAncestorScope in module_hierarchy.dart).  With none, the module is
/// checked against the language primitives alone (TGLP Definition "Root,
/// Scope", Π): until 2026-10-04 the default was a root-scope environment built
/// from a source the engine set once for the whole process.
/// [typeTemplates] are the module's own parameterised type definitions, which
/// [expandParameterizedTypes] removed from [module] and which survive this
/// environment to be expanded later. They are merged over the base scope's and
/// carried on the returned environment with their simple-alias references
/// resolved, so a later expansion of one cannot name an alias this build erases.
TypeEnvironment buildTypeEnvironment(ast.Module module,
    {TypeEnvironment? ancestorScope,
    Map<String, TypeDef> typeTemplates = const {}}) {
  // Base environment: the scope passed in, otherwise the language primitives.
  final baseEnv = ancestorScope ?? TypeEnvironment.empty();

  // Build user environment WITHOUT resolving aliases yet.  A bare type-name
  // alternative is checked for determinism against what it names in the
  // module or in its scope.
  final userEnv =
      _buildEnvironmentFromModule(module, scopeTypes: baseEnv.types);

  // Merge: base first, then user, which shadows any definition of the base
  final merged = baseEnv.merge(userEnv);

  // Now resolve aliases on the merged environment (so user aliases can reference root scope types)
  final types = Map<String, TypeDef>.from(merged.types);
  final procedures = Map<String, ProcDecl>.from(merged.procedures);
  final templates = <String, TypeDef>{
    ...merged.typeTemplates,
    ...typeTemplates,
  };
  // Innermost-first shadowing: a module-local MONOMORPHIC procedure shadows
  // an inherited parameterized template of the same key — so the template
  // must not survive in paramProcDecls (else call-site inference would fire
  // for calls the local definition resolves).  A module's
  // OWN parameterized procedure keeps its template (it is in userEnv's
  // paramProcDecls).  This is the defining-module dual of the exposed-scope
  // shadowing in the program linker.
  final paramProcDecls = Map<String, ProcDecl>.from(merged.paramProcDecls);
  for (final key in userEnv.procedures.keys) {
    if (!userEnv.paramProcDecls.containsKey(key)) {
      paramProcDecls.remove(key);
    }
  }

  _resolveAliases(types, procedures,
      typeTemplates: templates, paramProcDecls: paramProcDecls);

  return TypeEnvironment(types, procedures,
      paramProcDecls: paramProcDecls,
      typeTemplates: templates,
      typeOrigins: {
        for (final e in merged.typeOrigins.entries)
          if (types.containsKey(e.key)) e.key: e.value
      },
      scopeLayers: merged.scopeLayers);
}

/// Build TypeEnvironment from Module's type definitions and procedure declarations
TypeEnvironment _buildEnvironmentFromModule(
  ast.Module module, {
  Map<String, TypeDef> scopeTypes = const {},
}) {
  final types = <String, TypeDef>{};
  final procedures = <String, ProcDecl>{};
  final paramProcDecls = <String, ProcDecl>{};

  // Add type definitions (including aliases - will be resolved later)
  for (final typeDef in module.typeDefs) {
    // Aliases are allowed; the determinism check is skipped for them.
    if (!_isTypeAlias(typeDef)) {
      _checkDeterminism(typeDef, {...scopeTypes, ...types});
    }
    types[typeDef.name] = typeDef;
  }

  // Add procedure declarations
  for (final procDecl in module.procDeclarations) {
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

  return TypeEnvironment(types, procedures, paramProcDecls: paramProcDecls);
}

/// Check if a type definition is a simple alias (single type reference)
///
/// TGLP appendix "Type Aliases": "A simple alias has a single alternative
/// that is a type reference: an alias for a defined type, or for the dual of
/// one. ... The referenced types must be defined---not aliases themselves,
/// and not primitives."
/// - Output ::= _.         (alias for primitive wildcard)
/// - Input ::= _?.          (alias for primitive wildcard complement)
/// - MyList ::= List.       (alias for defined type)
/// - MyStream ::= Stream?.  (alias for complement of defined type)
///
/// NOT a simple alias:
/// - Key ::= String.        (references a primitive: an ordinary type
///                           definition, whose automaton inherits String's)
///
/// Until 2026-09-18 a single `TypeRef` counted whatever it named, so the root
/// `self.glp`'s `Key ::= String.` was erased from every environment and a
/// descendant module's `NetMsg ::= msg(Key, _)` failed with "Unresolved type:
/// Key" in every linked program (IGLP, 2026-09-18).
bool _isSimpleAlias(TypeDef def) {
  if (def.alternatives.length != 1) return false;

  final alt = def.alternatives.first;

  // Single PrimitiveModeAlt (_ or _?) = simple alias
  if (alt is PrimitiveModeAlt) return true;

  // Single TypeRef to a defined type (T or T?) = simple alias; a reference
  // to a primitive type is a definition, not an alias.
  if (alt is TypeRef) return !TypeRef.builtins.contains(alt.name);

  return false;
}

/// Check if a type definition is a union alias (multiple type references to defined types)
///
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
    final typeName = (alt).name;
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
/// - Simple aliases are resolved transitively and replaced everywhere
/// - Union aliases are expanded by collecting alternatives from referenced types
/// - Circular alias chains are detected and rejected
///
/// [typeTemplates], when given, is rewritten alongside [types]: a parameterised
/// type template is expanded AFTER this environment is built (by the abstract
/// instance of a parametric declaration, or by an instantiation a clause body
/// induces), and step 6 erases each simple alias from [types]. A template body
/// naming a simple alias would therefore expand into a definition referring to a
/// type no longer in the environment — `Agent ::= Constant.` beside
/// `NetEnd(C) ::= net_end(Agent, ...)` in one `self.glp` gave
/// "Unresolved type: Agent". Templates are rewritten here so no expansion of
/// one can name an erased alias.
///
/// [paramProcDecls] are rewritten for the same reason: a parameterised
/// procedure declaration is instantiated after this build — at a call site, or
/// as the abstract instance the parametricity check builds — and one whose
/// signature names a simple alias gave "Unresolved type: Agent?".
void _resolveAliases(Map<String, TypeDef> types, Map<String, ProcDecl> procedures,
    {Map<String, TypeDef>? typeTemplates,
    Map<String, ProcDecl>? paramProcDecls}) {
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
        expandedAlts.add(ref.isInput
            ? _complementAlt(targetAlt, def.line, def.column)
            : targetAlt);
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

  TypeExpr replace(TypeExpr e) => _replaceAliasReferences(e, resolved);
  for (final entry in nonSimpleAliasTypes) {
    types[entry.key] = entry.value.mapAlternatives(replace);
  }

  // Step 4b: Replace simple alias references in the parameterised type
  // templates, which survive this environment and are expanded later.
  if (typeTemplates != null) {
    for (final entry in typeTemplates.entries.toList()) {
      typeTemplates[entry.key] = entry.value.mapAlternatives(replace);
    }
  }

  // Step 5: Replace alias references in procedure declarations
  for (final entry in procedures.entries.toList()) {
    procedures[entry.key] = entry.value.mapArgTypes(replace);
  }

  // Step 5b: Same for the parameterised procedure declarations, which survive
  // this environment and are instantiated later.
  if (paramProcDecls != null) {
    for (final entry in paramProcDecls.entries.toList()) {
      paramProcDecls[entry.key] = entry.value.mapArgTypes(replace);
    }
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
/// Every node of the result is placed at [line], [column].
TypeExpr _complementAlt(TypeExpr alt, int line, int column) {
  if (alt is TypeRef || alt is PrimitiveModeAlt) {
    return _applyComplement(alt, true, line, column);
  } else if (alt is ConstantAlt) {
    return alt;  // Constants don't have modes
  } else if (alt is ListNilAlt) {
    return alt;  // [] doesn't have modes
  } else if (alt is ListConsAlt) {
    return ListConsAlt(
      _complementAlt(alt.head, line, column),
      _complementAlt(alt.tail, line, column),
      line,
      column,
    );
  } else if (alt is StructAlt) {
    return StructAlt(
      alt.functor,
      alt.args.map((a) => _complementAlt(a, line, column)).toList(),
      line,
      column,
    );
  } else if (alt is DiffListAlt) {
    return DiffListAlt(
      _complementAlt(alt.content, line, column),
      _complementAlt(alt.hole, line, column),
      line,
      column,
    );
  }
  return alt;
}

/// Replace alias references in a TypeExpr recursively.
TypeExpr _replaceAliasReferences(
        TypeExpr expr, Map<String, TypeExpr> resolved) =>
    mapTypeExpr(expr, (e) {
      if (e is TypeRef) {
        final resolvedTarget = resolved[e.name];
        if (resolvedTarget != null) {
          // Replace with resolved target, applying complement if needed
          return _applyComplement(resolvedTarget, e.isInput, e.line, e.column);
        }
        if (e.typeArgs.isEmpty) return e; // Not an alias, keep as-is
        // A still-parameterised reference — `Stream(Agent)` in a type
        // template, which expansion has not yet reached. Its arguments may
        // name aliases.  (In `types` and `procedures` expansion has already
        // replaced every parameterised reference by its expanded name, so
        // typeArgs is empty there and this is a no-op.)
        return TypeRef(e.name, e.line, e.column,
            isInput: e.isInput,
            typeArgs: e.typeArgs
                .map((a) => _replaceAliasReferences(a, resolved))
                .toList());
      }
      // PrimitiveModeAlt, ConstantAlt, ListNilAlt: no reference to another
      // type.
      return e;
    });

/// Check if type alternatives are deterministic (distinguishable)
///
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

  // Claim the functor [key] for an alternative, refused with [message] where
  // an earlier alternative claimed it.
  void claim(String key, String message) {
    if (functors.contains(key)) {
      throw NonDeterministicTypeError(message, def.line, def.column);
    }
    functors.add(key);
  }

  for (final alt in def.alternatives) {
    if (alt is ConstantAlt) {
      // Keyed by kind and value ([constantLabel]): `0 ; '0'` are two
      // alternatives.  Until 2026-10-10 by text, and refused as one.
      final key = constantLabel(alt.value);
      if (constants.contains(key)) {
        throw NonDeterministicTypeError(
          'Duplicate constant alternative: $key in ${def.name}',
          def.line, def.column);
      }
      constants.add(key);

    } else if (alt is ListNilAlt) {
      claim('[]/0', 'Duplicate [] alternative in ${def.name}');

    } else if (alt is ListConsAlt) {
      claim('[|]/2', 'Duplicate [|] alternative in ${def.name}');

    } else if (alt is StructAlt) {
      final key = '${alt.functor}/${alt.args.length}';
      claim(key, 'Duplicate functor alternative: $key in ${def.name}');

    } else if (alt is DiffListAlt) {
      claim('\\/2', 'Duplicate \\ alternative in ${def.name}');

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
  final def = scope[alt.name];
  if (def == null) return const {};
  return primitiveClosure(def, scope);
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
