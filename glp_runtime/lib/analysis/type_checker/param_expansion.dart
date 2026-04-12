// lib/analysis/type_checker/param_expansion.dart
//
// Expand parameterized types to monomorphic equivalents.
// Runs after parsing and before type automaton construction.
//
// Spec: docs/type system/typed-program.md, section "Parameterized Types"
// Paper: Section 8, Definition 8.1

import '../../compiler/ast.dart' as ast;
import 'type_ast.dart';

/// Expand all parameterized types in a module to monomorphic equivalents.
/// Returns a new Module with only monomorphic type definitions and
/// procedure declarations. The original Module is not modified.
///
/// If the module has no parameterized types, returns it unchanged.
ast.Module expandParameterizedTypes(ast.Module module, {
    Set<String> knownTypeNames = const {},
    Map<String, TypeDef> externalTemplates = const {},
}) {
  // Step 1: Separate templates from monomorphic types
  final templates = <String, TypeDef>{};
  final monoTypeDefs = <TypeDef>[];

  for (final td in module.typeDefs) {
    if (td.isParameterized) {
      templates[td.name] = td;
    } else {
      monoTypeDefs.add(td);
    }
  }

  // Merge external templates (from prelude/ancestor scopes).
  // Local templates take precedence over external ones.
  for (final entry in externalTemplates.entries) {
    templates.putIfAbsent(entry.key, () => entry.value);
  }

  // Note: don't return early if templates is empty — proc decls may reference
  // prelude templates (e.g., Stream(X)) and still need type param detection.

  // Known monomorphic type names: used to collapse all-wildcard expansions
  // (e.g., Stream(_) → Stream) when a monomorphic version exists.
  final monoNames = <String>{
    ...monoTypeDefs.map((td) => td.name),
    ...knownTypeNames,
  };

  // Step 2: Collect all instantiations from type defs and proc decls
  final instantiations = <String, List<TypeExpr>>{}; // expanded name -> type args

  // Scan monomorphic type def bodies
  for (final td in monoTypeDefs) {
    for (final alt in td.alternatives) {
      _collectInstantiations(alt, templates, instantiations);
    }
  }

  // Scan procedure declarations.
  // For parameterized proc decls (those with bare type params), skip
  // instantiations that contain type parameter names — those are templates,
  // not concrete instantiations.
  for (final pd in module.procDeclarations) {
    final procTypeParams = _detectProcTypeParams(pd, templates, monoTypeDefs, knownTypeNames);
    if (procTypeParams.isEmpty) {
      // Non-parameterized proc decl: collect all instantiations normally
      for (final arg in pd.argTypes) {
        _collectInstantiations(arg, templates, instantiations);
      }
    } else {
      // Parameterized proc decl: only collect instantiations with concrete args
      for (final arg in pd.argTypes) {
        _collectInstantiationsInTemplate(arg, templates, instantiations, procTypeParams);
      }
    }
  }

  // Scan template bodies for cross-references
  for (final td in templates.values) {
    for (final alt in td.alternatives) {
      _collectInstantiationsInTemplate(alt, templates, instantiations, td.typeParams);
    }
  }

  // Step 3: Expand each instantiation using a worklist
  final expandedDefs = <TypeDef>[];
  final expanded = <String>{};

  while (instantiations.length > expanded.length) {
    for (final entry in Map.of(instantiations).entries) {
      if (expanded.contains(entry.key)) continue;
      final expandedName = entry.key;
      final typeArgs = entry.value;
      final templateName = _templateNameFromExpanded(expandedName);
      final template = templates[templateName];
      if (template == null) {
        // Referenced template not found — skip (will be caught by type checker)
        expanded.add(expandedName);
        continue;
      }
      // Check arity
      if (template.typeParams.length != typeArgs.length) {
        expanded.add(expandedName);
        continue;
      }
      // Substitute parameters
      final substitution = Map<String, TypeExpr>.fromIterables(template.typeParams, typeArgs);
      final newAlts = template.alternatives
          .map((alt) => _substituteTypeExpr(alt, substitution, templates, instantiations, monoNames: monoNames))
          .toList();
      final processedAlts = newAlts
          .map((alt) => _replaceParamRefs(alt, templates, monoNames: monoNames))
          .toList();
      expandedDefs.add(TypeDef(expandedName, processedAlts, template.line, template.column));
      expanded.add(expandedName);
    }
  }

  // Step 4: Replace references in monomorphic type defs
  final replacedTypeDefs = monoTypeDefs.map((td) {
    final newAlts = td.alternatives
        .map((alt) => _replaceParamRefs(alt, templates, monoNames: monoNames))
        .toList();
    return TypeDef(td.name, newAlts, td.line, td.column);
  }).toList();

  // Step 5: Replace references in procedure declarations.
  // Parameterized proc decls: generate wildcard-instantiated concrete version
  // (for checking own clauses) AND preserve the parameterized template
  // (for call-site inference in Case B).
  final replacedProcDecls = <ProcDecl>[];
  final paramProcDeclTemplates = <ProcDecl>[];

  for (final pd in module.procDeclarations) {
    final procTypeParams = _detectProcTypeParams(pd, templates, monoTypeDefs, knownTypeNames);
    if (procTypeParams.isNotEmpty) {
      // Preserve parameterized template for call-site inference
      final paramTemplate = ProcDecl(pd.name, pd.argTypes, pd.line, pd.column,
          typeParams: procTypeParams,
          exported: pd.exported, imported: pd.imported, modulePath: pd.modulePath);
      paramProcDeclTemplates.add(paramTemplate);

      // Generate wildcard-instantiated concrete version:
      // Substitute each type param with PrimitiveModeAlt(false) (i.e., _)
      final wildcardSubst = <String, TypeExpr>{
        for (final tp in procTypeParams)
          tp: PrimitiveModeAlt(false, 0, 0),
      };
      final wildcardArgTypes = pd.argTypes.map((arg) {
        final substituted = _substituteTypeExpr(arg, wildcardSubst, templates, instantiations, monoNames: monoNames);
        return _replaceParamRefs(substituted, templates, monoNames: monoNames);
      }).toList();

      replacedProcDecls.add(ProcDecl(pd.name, wildcardArgTypes, pd.line, pd.column,
          exported: pd.exported, imported: pd.imported, modulePath: pd.modulePath));
    } else {
      // Non-parameterized: expand as before
      final newArgTypes = pd.argTypes
          .map((arg) => _replaceParamRefs(arg, templates, monoNames: monoNames))
          .toList();
      replacedProcDecls.add(ProcDecl(pd.name, newArgTypes, pd.line, pd.column,
          exported: pd.exported, imported: pd.imported, modulePath: pd.modulePath));
    }
  }

  // Expand any new instantiations generated by wildcard substitution
  while (instantiations.length > expanded.length) {
    for (final entry in Map.of(instantiations).entries) {
      if (expanded.contains(entry.key)) continue;
      final expandedName = entry.key;
      final typeArgs = entry.value;
      final templateName = _templateNameFromExpanded(expandedName);
      final template = templates[templateName];
      if (template == null) {
        expanded.add(expandedName);
        continue;
      }
      if (template.typeParams.length != typeArgs.length) {
        expanded.add(expandedName);
        continue;
      }
      final substitution = Map<String, TypeExpr>.fromIterables(template.typeParams, typeArgs);
      final newAlts = template.alternatives
          .map((alt) => _substituteTypeExpr(alt, substitution, templates, instantiations, monoNames: monoNames))
          .toList();
      final processedAlts = newAlts
          .map((alt) => _replaceParamRefs(alt, templates, monoNames: monoNames))
          .toList();
      expandedDefs.add(TypeDef(expandedName, processedAlts, template.line, template.column));
      expanded.add(expandedName);
    }
  }

  return ast.Module(
    declaration: module.declaration,
    typeDefs: [...replacedTypeDefs, ...expandedDefs],
    procDeclarations: replacedProcDecls,
    paramProcDecls: paramProcDeclTemplates,
    procedures: module.procedures,
    compileMode: module.compileMode,
    line: module.line,
    column: module.column,
  );
}

/// Detect type parameters in a procedure declaration.
/// A type parameter is a name that:
///  1. Appears as a bare typeArg inside a TypeRef with typeArgs (e.g., X in Stream(X))
///  2. Is not a known defined type
/// Bare top-level unknown types (e.g., MyUndefinedType) are NOT type params —
/// they are only type params if the same name also appears inside a typeArg.
List<String> _detectProcTypeParams(ProcDecl pd, Map<String, TypeDef> templates,
    List<TypeDef> monoTypeDefs, Set<String> externalKnownTypes) {
  final knownTypes = <String>{
    ...templates.keys,
    ...monoTypeDefs.map((td) => td.name),
    ...TypeRef.builtins,
    ...externalKnownTypes,
    'Constant', // also a known type
  };
  // Pass 1: collect names that appear as typeArgs of any TypeRef with typeArgs.
  // These are the "inner" type parameter candidates.
  final innerCandidates = <String>{};
  for (final arg in pd.argTypes) {
    _collectInnerTypeParamCandidates(arg, knownTypes, innerCandidates);
  }
  return innerCandidates.toList();
}

/// Collect type parameter names from inside parameterized type refs.
/// A candidate is a bare TypeRef name that appears as a typeArg of any
/// TypeRef with typeArgs, and is not a known type.
void _collectInnerTypeParamCandidates(TypeExpr expr,
    Set<String> knownTypes, Set<String> candidates) {
  if (expr is TypeRef) {
    if (expr.typeArgs.isNotEmpty) {
      // Check each typeArg for bare unknown names
      for (final arg in expr.typeArgs) {
        if (arg is TypeRef && arg.typeArgs.isEmpty && !knownTypes.contains(arg.name)) {
          candidates.add(arg.name);
        }
        // Recurse into nested type args
        _collectInnerTypeParamCandidates(arg, knownTypes, candidates);
      }
    }
    return;
  }
  // Recurse into structural types
  if (expr is StructAlt) {
    for (final arg in expr.args) {
      _collectInnerTypeParamCandidates(arg, knownTypes, candidates);
    }
  }
  if (expr is ListConsAlt) {
    _collectInnerTypeParamCandidates(expr.head, knownTypes, candidates);
    _collectInnerTypeParamCandidates(expr.tail, knownTypes, candidates);
  }
  if (expr is DiffListAlt) {
    _collectInnerTypeParamCandidates(expr.content, knownTypes, candidates);
    _collectInnerTypeParamCandidates(expr.hole, knownTypes, candidates);
  }
}

/// Generate expanded name: Stream + [Integer] -> "Stream<Integer>"
/// For nested parameterized refs, recursively uses expanded notation.
String _expandedName(String templateName, List<TypeExpr> typeArgs) {
  return '$templateName<${typeArgs.map(_typeExprToCanonical).join(',')}>';
}

/// Convert a TypeExpr to its canonical string for expanded-name purposes.
/// Nested parameterized TypeRefs use angle-bracket notation recursively.
String _typeExprToCanonical(TypeExpr expr) {
  if (expr is TypeRef) {
    if (expr.typeArgs.isNotEmpty) {
      // Nested parameterized ref: recursively expand
      return '${expr.name}<${expr.typeArgs.map(_typeExprToCanonical).join(',')}>${expr.isInput ? '?' : ''}';
    }
    return expr.toString(); // simple ref: "Integer", "Msg?", etc.
  }
  return expr.toString();
}

/// Extract template name from expanded name: "Stream<Integer>" -> "Stream"
String _templateNameFromExpanded(String expandedName) {
  final idx = expandedName.indexOf('<');
  if (idx < 0) return expandedName;
  return expandedName.substring(0, idx);
}

/// Check if a TypeRef references a template with matching arity.
/// Returns true only if the name is a template AND the type arg count matches.
bool _isTemplateRef(TypeRef expr, Map<String, TypeDef> templates) {
  if (expr.typeArgs.isEmpty) return false;
  final template = templates[expr.name];
  if (template == null) return false;
  return expr.typeArgs.length == template.typeParams.length;
}

/// Collect parameterized type references from a TypeExpr
void _collectInstantiations(TypeExpr expr, Map<String, TypeDef> templates,
    Map<String, List<TypeExpr>> instantiations) {
  if (expr is TypeRef) {
    if (_isTemplateRef(expr, templates)) {
      final name = _expandedName(expr.name, expr.typeArgs);
      instantiations.putIfAbsent(name, () => expr.typeArgs);
      // Recurse into type args (for nested parameterized types)
      for (final arg in expr.typeArgs) {
        _collectInstantiations(arg, templates, instantiations);
      }
    }
    // Also recurse into typeArgs even if not a template (might contain nested refs)
    for (final arg in expr.typeArgs) {
      _collectInstantiations(arg, templates, instantiations);
    }
    return;
  }
  if (expr is StructAlt) {
    for (final arg in expr.args) {
      _collectInstantiations(arg, templates, instantiations);
    }
  }
  if (expr is ListConsAlt) {
    _collectInstantiations(expr.head, templates, instantiations);
    _collectInstantiations(expr.tail, templates, instantiations);
  }
  if (expr is DiffListAlt) {
    _collectInstantiations(expr.content, templates, instantiations);
    _collectInstantiations(expr.hole, templates, instantiations);
  }
}

/// Collect instantiations from template bodies, skipping type parameter names.
/// For example, in `Stream(X) ::= [] ; [X | Stream(X)]`, `Stream(X)` in the body
/// is a recursive self-reference with parameter X, not an instantiation to collect.
/// But `Pair(Integer, String)` in a template body IS an instantiation.
void _collectInstantiationsInTemplate(TypeExpr expr, Map<String, TypeDef> templates,
    Map<String, List<TypeExpr>> instantiations, List<String> templateParams) {
  if (expr is TypeRef) {
    if (_isTemplateRef(expr, templates)) {
      // Check if all args are just bare type parameters — if so, it's a recursive
      // self-reference, not an instantiation to collect separately.
      final allParamRefs = expr.typeArgs.every((arg) =>
          arg is TypeRef && arg.typeArgs.isEmpty && templateParams.contains(arg.name));
      if (!allParamRefs) {
        // Contains concrete types — collect any nested instantiations
        // Use template-aware version to preserve param awareness through nesting
        for (final arg in expr.typeArgs) {
          _collectInstantiationsInTemplate(arg, templates, instantiations, templateParams);
        }
      }
    }
    for (final arg in expr.typeArgs) {
      _collectInstantiationsInTemplate(arg, templates, instantiations, templateParams);
    }
    return;
  }
  if (expr is StructAlt) {
    for (final arg in expr.args) {
      _collectInstantiationsInTemplate(arg, templates, instantiations, templateParams);
    }
  }
  if (expr is ListConsAlt) {
    _collectInstantiationsInTemplate(expr.head, templates, instantiations, templateParams);
    _collectInstantiationsInTemplate(expr.tail, templates, instantiations, templateParams);
  }
  if (expr is DiffListAlt) {
    _collectInstantiationsInTemplate(expr.content, templates, instantiations, templateParams);
    _collectInstantiationsInTemplate(expr.hole, templates, instantiations, templateParams);
  }
}

/// Substitute type parameters in a TypeExpr.
/// [monoNames]: set of known monomorphic type names. When all substituted args
/// are wildcards AND the base name is in monoNames, use the base name directly.
TypeExpr _substituteTypeExpr(TypeExpr expr, Map<String, TypeExpr> substitution,
    Map<String, TypeDef> templates, Map<String, List<TypeExpr>> instantiations,
    {Set<String> monoNames = const {}}) {
  if (expr is TypeRef) {
    // If this is a type parameter, substitute it
    if (substitution.containsKey(expr.name) && expr.typeArgs.isEmpty) {
      final replacement = substitution[expr.name]!;
      // Apply isInput from the original reference
      if (expr.isInput && replacement is TypeRef) {
        return TypeRef(replacement.name, replacement.line, replacement.column,
            isInput: true, typeArgs: replacement.typeArgs);
      }
      if (expr.isInput && replacement is PrimitiveModeAlt) {
        return PrimitiveModeAlt(true, replacement.line, replacement.column);
      }
      return replacement;
    }
    // If this is a parameterized reference to a template with matching arity, record and replace
    if (_isTemplateRef(expr, templates)) {
      final substArgs = expr.typeArgs
          .map((a) => _substituteTypeExpr(a, substitution, templates, instantiations, monoNames: monoNames))
          .toList();
      // If all substituted args are wildcards (_) AND a monomorphic type with
      // the base name exists, use the base name directly.
      // Stream(_) ≡ Stream when a monomorphic Stream type exists.
      final allWildcards = substArgs.every((a) => a is PrimitiveModeAlt);
      if (allWildcards && monoNames.contains(expr.name)) {
        return TypeRef(expr.name, expr.line, expr.column, isInput: expr.isInput);
      }
      final expandedName = _expandedName(expr.name, substArgs);
      instantiations.putIfAbsent(expandedName, () => substArgs);
      return TypeRef(expandedName, expr.line, expr.column, isInput: expr.isInput);
    }
    return expr;
  }
  if (expr is StructAlt) {
    return StructAlt(expr.functor,
        expr.args.map((a) => _substituteTypeExpr(a, substitution, templates, instantiations, monoNames: monoNames)).toList(),
        expr.line, expr.column);
  }
  if (expr is ListConsAlt) {
    return ListConsAlt(
        _substituteTypeExpr(expr.head, substitution, templates, instantiations, monoNames: monoNames),
        _substituteTypeExpr(expr.tail, substitution, templates, instantiations, monoNames: monoNames),
        expr.line, expr.column);
  }
  if (expr is DiffListAlt) {
    return DiffListAlt(
        _substituteTypeExpr(expr.content, substitution, templates, instantiations, monoNames: monoNames),
        _substituteTypeExpr(expr.hole, substitution, templates, instantiations, monoNames: monoNames),
        expr.line, expr.column);
  }
  // PrimitiveModeAlt, ConstantAlt, ListNilAlt — no substitution needed
  return expr;
}

/// Replace parameterized type refs with expanded names (for non-template types and proc decls).
/// [monoNames]: when all args are wildcards AND base name is in monoNames, use base name.
TypeExpr _replaceParamRefs(TypeExpr expr, Map<String, TypeDef> templates,
    {Set<String> monoNames = const {}}) {
  if (expr is TypeRef) {
    if (_isTemplateRef(expr, templates)) {
      // Replace args recursively first
      final replacedArgs = expr.typeArgs
          .map((a) => _replaceParamRefs(a, templates, monoNames: monoNames))
          .toList();
      // If all args are wildcards (_) AND base name exists as monomorphic, use base name.
      final allWildcards = replacedArgs.every((a) => a is PrimitiveModeAlt);
      if (allWildcards && monoNames.contains(expr.name)) {
        return TypeRef(expr.name, expr.line, expr.column, isInput: expr.isInput);
      }
      final expandedName = _expandedName(expr.name, replacedArgs);
      return TypeRef(expandedName, expr.line, expr.column, isInput: expr.isInput);
    }
    // Recurse into typeArgs even if not a template ref
    if (expr.typeArgs.isNotEmpty) {
      final replacedArgs = expr.typeArgs
          .map((a) => _replaceParamRefs(a, templates, monoNames: monoNames))
          .toList();
      return TypeRef(expr.name, expr.line, expr.column, isInput: expr.isInput, typeArgs: replacedArgs);
    }
    return expr;
  }
  if (expr is StructAlt) {
    return StructAlt(expr.functor,
        expr.args.map((a) => _replaceParamRefs(a, templates, monoNames: monoNames)).toList(),
        expr.line, expr.column);
  }
  if (expr is ListConsAlt) {
    return ListConsAlt(
        _replaceParamRefs(expr.head, templates, monoNames: monoNames),
        _replaceParamRefs(expr.tail, templates, monoNames: monoNames),
        expr.line, expr.column);
  }
  if (expr is DiffListAlt) {
    return DiffListAlt(
        _replaceParamRefs(expr.content, templates, monoNames: monoNames),
        _replaceParamRefs(expr.hole, templates, monoNames: monoNames),
        expr.line, expr.column);
  }
  return expr;
}
