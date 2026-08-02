// lib/analysis/type_checker/param_expansion.dart
//
// Expand parameterized types to monomorphic equivalents.
// Runs after parsing and before type automaton construction.
//
// Spec: TGLP (Moded-Types), sections/appendix-implementation-notes.tex §Parameterised-Type Expansion
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

  // Merge external templates (from root scope/ancestor scopes).
  // Local templates take precedence over external ones.
  for (final entry in externalTemplates.entries) {
    templates.putIfAbsent(entry.key, () => entry.value);
  }

  // Enforce the bound that keeps the reachable type set finite, so expansion
  // (and the parameterized-procedure instantiation closure) terminates.
  _checkNoGrowingTypeRecursion(templates);

  // Note: don't return early if templates is empty — proc decls may reference
  // root scope templates (e.g., Stream(X)) and still need type param detection.

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
  // For parameterized proc decls (those naming type parameters), skip
  // instantiations that contain type parameter names — those are templates,
  // not concrete instantiations.
  final declKnownTypes = _declKnownTypes(templates, monoTypeDefs, knownTypeNames);
  for (final pd in module.procDeclarations) {
    final procTypeParams = _procTypeParams(pd, declKnownTypes);
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
    final procTypeParams = _procTypeParams(pd, declKnownTypes);
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
    typeDefs: [...replacedTypeDefs, ...expandedDefs],
    procDeclarations: replacedProcDecls,
    paramProcDecls: paramProcDeclTemplates,
    procedures: module.procedures,
    compileMode: module.compileMode,
    line: module.line,
    column: module.column,
  );
}

/// Materialize the monomorphic type definitions named by [neededNames] — the
/// expanded names of type instantiations that arise only through the
/// procedure-instantiation closure (e.g. `Stream<Box<Msg>>`), which the initial
/// declaration-driven expansion did not produce. Uses [templates]; recurses into
/// nested arguments; skips names already in [have] or non-parameterized (no `<`).
/// Returns name → fresh TypeDef for the newly materialized definitions.
///
/// The set of such names is finite because recursion among parameterised
/// procedures is monomorphic (typed-program: Parameterised Procedure
/// Declarations), so this terminates without any size bound.
Map<String, TypeDef> materializeInstantiations(
    Iterable<String> neededNames,
    Map<String, TypeDef> templates,
    Set<String> have) {
  final out = <String, TypeDef>{};
  final work = <String>[...neededNames];
  while (work.isNotEmpty) {
    var name = work.removeLast();
    if (name.endsWith('?')) name = name.substring(0, name.length - 1);
    if (have.contains(name) || out.containsKey(name)) continue;
    final lt = name.indexOf('<');
    if (lt < 0) continue; // base/monomorphic name — defined elsewhere
    final templateName = name.substring(0, lt);
    final template = templates[templateName];
    if (template == null) continue; // unknown template — leave to the checker
    final argNames = _splitTopLevelArgs(name.substring(lt + 1, name.length - 1));
    if (template.typeParams.length != argNames.length) continue;
    final subst = <String, TypeExpr>{};
    for (var i = 0; i < argNames.length; i++) {
      subst[template.typeParams[i]] = TypeRef(argNames[i], 0, 0);
      work.add(argNames[i]); // materialize nested instantiations too
    }
    final alts = template.alternatives
        .map((a) => _substituteToExpandedNames(a, subst, templates, work))
        .toList();
    out[name] = TypeDef(name, alts, template.line, template.column);
  }
  return out;
}

// ============================================================================
// Abstract instance and routing (typed-program: Modular Checking via Abstract
// Parameters). A parameterised procedure that does not inspect a parameter and
// uses no parameter as a type-definition alternative is checked once, against an
// abstract instance, and certified for every instantiation.
// ============================================================================

/// The abstract instance of a parameterised procedure declaration: each type
/// parameter is replaced by a distinct *abstract type* — a synthesized
/// zero-alternative type whose automaton is empty, so a variable at a parameter
/// position is consistent while a functor or constant is not. [decl] is the
/// monomorphic declaration; [typeDefs] are the synthesized type definitions (the
/// abstract types and the expanded types referencing them) to add to the
/// checking environment; [abstractTypeNames] are the abstract-type names (used to
/// recognise parametric callee instantiations).
class AbstractInstance {
  final ProcDecl decl;
  final Map<String, TypeDef> typeDefs;
  final Set<String> abstractTypeNames;
  AbstractInstance(this.decl, this.typeDefs, this.abstractTypeNames);
}

/// Build the abstract instance of [paramTemplate] with parameters [typeParams].
AbstractInstance buildAbstractInstance(
    ProcDecl paramTemplate, List<String> typeParams, Map<String, TypeDef> templates,
    {Set<String> knownMonoTypes = const {}}) {
  final abstractOf = <String, String>{
    for (final tp in typeParams) tp: '\$abstract_$tp',
  };
  final subst = <String, TypeExpr>{
    for (final e in abstractOf.entries) e.key: TypeRef(e.value, 0, 0),
  };
  final instantiations = <String, List<TypeExpr>>{};
  final argTypes = paramTemplate.argTypes.map((arg) {
    final s = _substituteTypeExpr(arg, subst, templates, instantiations,
        monoNames: knownMonoTypes);
    return _replaceParamRefs(s, templates, monoNames: knownMonoTypes);
  }).toList();
  final decl = ProcDecl(paramTemplate.name, argTypes,
      paramTemplate.line, paramTemplate.column,
      exported: paramTemplate.exported,
      imported: paramTemplate.imported,
      modulePath: paramTemplate.modulePath);

  final typeDefs = <String, TypeDef>{};
  for (final name in abstractOf.values) {
    typeDefs[name] = TypeDef(name, const [], 0, 0); // zero alternatives = abstract type
  }
  final have = <String>{...knownMonoTypes, ...templates.keys, ...abstractOf.values};
  typeDefs.addAll(materializeInstantiations(instantiations.keys, templates, have));
  return AbstractInstance(decl, typeDefs, abstractOf.values.toSet());
}

/// Does some clause head place a functor or constant at a parameter position?
/// Such a procedure inspects a parameter and is not parametrically well-typed; it
/// takes the per-instantiation route.
bool procInspectsParameter(List<ast.Clause> clauses, ProcDecl paramTemplate,
    List<String> typeParams, Map<String, TypeDef> templates) {
  final params = typeParams.toSet();
  for (final clause in clauses) {
    final args = clause.head.args;
    for (var i = 0;
        i < paramTemplate.argTypes.length && i < args.length;
        i++) {
      if (_termInspectsParamAt(
          args[i], paramTemplate.argTypes[i], params, templates)) {
        return true;
      }
    }
  }
  return false;
}

bool _termInspectsParamAt(ast.Term term, TypeExpr type, Set<String> params,
    Map<String, TypeDef> templates) {
  // Bare parameter position: a non-variable term inspects the parameter.
  if (type is TypeRef && type.typeArgs.isEmpty && params.contains(type.name)) {
    return !(term is ast.VarTerm || term is ast.UnderscoreTerm);
  }
  // A variable covers anything and descends nowhere.
  if (term is ast.VarTerm || term is ast.UnderscoreTerm) return false;
  // Parameterised/named type: descend by matching the term's constructor to a
  // template alternative, substituting the type arguments.
  if (type is TypeRef) {
    final template = templates[type.name];
    if (template == null) return false; // monomorphic/primitive: no parameter reachable
    if (template.typeParams.length != type.typeArgs.length) return false;
    final subst = <String, TypeExpr>{
      for (var i = 0; i < template.typeParams.length; i++)
        template.typeParams[i]: type.typeArgs[i],
    };
    for (final alt in template.alternatives) {
      final pairs = _matchAltToTerm(alt, term, subst);
      if (pairs != null) {
        return pairs
            .any((p) => _termInspectsParamAt(p.$1, p.$2, params, templates));
      }
    }
    return false; // no alternative matches the term's constructor — not a parameter inspection
  }
  return false;
}

/// If [alt] structurally matches [term]'s top constructor, return the (subterm,
/// subtype) pairs to recurse on, with the template substitution applied to the
/// subtypes; otherwise null. Type-valued alternatives (a bare parameter or
/// another type) are not descended — the parameter-as-alternative case routes to
/// per-instantiation independently.
List<(ast.Term, TypeExpr)>? _matchAltToTerm(
    TypeExpr alt, ast.Term term, Map<String, TypeExpr> subst) {
  if (alt is ListNilAlt) {
    return (term is ast.ListTerm && term.isNil) ? const [] : null;
  }
  if (alt is ListConsAlt) {
    if (term is ast.ListTerm &&
        !term.isNil &&
        term.head != null &&
        term.tail != null) {
      return [
        (term.head!, _substParam(alt.head, subst)),
        (term.tail!, _substParam(alt.tail, subst)),
      ];
    }
    return null;
  }
  if (alt is StructAlt) {
    if (term is ast.StructTerm &&
        term.functor == alt.functor &&
        term.args.length == alt.args.length) {
      return [
        for (var i = 0; i < alt.args.length; i++)
          (term.args[i], _substParam(alt.args[i], subst)),
      ];
    }
    return null;
  }
  if (alt is ConstantAlt) {
    return (term is ast.ConstTerm) ? const [] : null;
  }
  // DiffListAlt, TypeRef, PrimitiveModeAlt: not descended structurally here.
  return null;
}

/// Substitute bare template parameters in [e] using [subst].
TypeExpr _substParam(TypeExpr e, Map<String, TypeExpr> subst) {
  if (e is TypeRef) {
    if (e.typeArgs.isEmpty && subst.containsKey(e.name)) {
      final r = subst[e.name]!;
      if (e.isInput && r is TypeRef) {
        return TypeRef(r.name, r.line, r.column,
            isInput: true, typeArgs: r.typeArgs);
      }
      return r;
    }
    if (e.typeArgs.isNotEmpty) {
      return TypeRef(e.name, e.line, e.column,
          isInput: e.isInput,
          typeArgs: e.typeArgs.map((a) => _substParam(a, subst)).toList());
    }
    return e;
  }
  if (e is ListConsAlt) {
    return ListConsAlt(
        _substParam(e.head, subst), _substParam(e.tail, subst), e.line, e.column);
  }
  if (e is StructAlt) {
    return StructAlt(e.functor,
        e.args.map((a) => _substParam(a, subst)).toList(), e.line, e.column);
  }
  if (e is DiffListAlt) {
    return DiffListAlt(_substParam(e.content, subst), _substParam(e.hole, subst),
        e.line, e.column);
  }
  return e;
}

/// Does any type parameter occur as a top-level alternative of a type definition
/// reachable from the procedure's argument types? Such a procedure takes the
/// per-instantiation route (its determinism rests on the instantiation).
bool paramUsedAsTypeAlternative(
    ProcDecl paramTemplate, Map<String, TypeDef> templates) {
  final referenced = <String>{};
  void collect(TypeExpr e) {
    if (e is TypeRef) {
      if (templates.containsKey(e.name)) referenced.add(e.name);
      for (final a in e.typeArgs) {
        collect(a);
      }
      return;
    }
    for (final c in _typeExprChildren(e)) {
      collect(c);
    }
  }

  for (final arg in paramTemplate.argTypes) {
    collect(arg);
  }
  // Transitive closure over template references.
  final work = [...referenced];
  while (work.isNotEmpty) {
    final t = templates[work.removeLast()];
    if (t == null) continue;
    for (final alt in t.alternatives) {
      final names = <String>{};
      _collectTemplateRefNames(alt, templates, names);
      for (final n in names) {
        if (referenced.add(n)) work.add(n);
      }
    }
  }
  // Does any referenced template carry one of its own parameters as a bare
  // top-level alternative?
  for (final name in referenced) {
    final t = templates[name];
    if (t == null) continue;
    final p = t.typeParams.toSet();
    for (final alt in t.alternatives) {
      if (alt is TypeRef && alt.typeArgs.isEmpty && p.contains(alt.name)) {
        return true;
      }
    }
  }
  return false;
}

/// Split top-level comma-separated arguments of an expanded-name body, respecting
/// nested angle brackets: `Box<Msg>,Integer` → [`Box<Msg>`, `Integer`].
List<String> _splitTopLevelArgs(String s) {
  final result = <String>[];
  var depth = 0, start = 0;
  for (var i = 0; i < s.length; i++) {
    final c = s[i];
    if (c == '<') {
      depth++;
    } else if (c == '>') {
      depth--;
    } else if (c == ',' && depth == 0) {
      result.add(s.substring(start, i).trim());
      start = i + 1;
    }
  }
  if (start < s.length) result.add(s.substring(start).trim());
  return result;
}

/// Substitute parameters in [expr] with their argument refs, and rewrite every
/// parameterized template reference to its expanded name, queuing that name (and
/// nested ones) into [work] for materialization.
TypeExpr _substituteToExpandedNames(TypeExpr expr, Map<String, TypeExpr> subst,
    Map<String, TypeDef> templates, List<String> work) {
  if (expr is TypeRef) {
    if (expr.typeArgs.isEmpty && subst.containsKey(expr.name)) {
      final r = subst[expr.name]!;
      if (expr.isInput && r is TypeRef) {
        return TypeRef(r.name, r.line, r.column,
            isInput: true, typeArgs: r.typeArgs);
      }
      return r;
    }
    if (expr.typeArgs.isNotEmpty && templates.containsKey(expr.name)) {
      final newArgs = expr.typeArgs
          .map((a) => _substituteToExpandedNames(a, subst, templates, work))
          .toList();
      final expanded = _expandedName(expr.name, newArgs);
      work.add(expanded);
      return TypeRef(expanded, expr.line, expr.column, isInput: expr.isInput);
    }
    return expr;
  }
  if (expr is StructAlt) {
    return StructAlt(
        expr.functor,
        expr.args
            .map((a) => _substituteToExpandedNames(a, subst, templates, work))
            .toList(),
        expr.line,
        expr.column);
  }
  if (expr is ListConsAlt) {
    return ListConsAlt(
        _substituteToExpandedNames(expr.head, subst, templates, work),
        _substituteToExpandedNames(expr.tail, subst, templates, work),
        expr.line,
        expr.column);
  }
  if (expr is DiffListAlt) {
    return DiffListAlt(
        _substituteToExpandedNames(expr.content, subst, templates, work),
        _substituteToExpandedNames(expr.hole, subst, templates, work),
        expr.line,
        expr.column);
  }
  return expr;
}

/// Enforce the finiteness bound on reachable types (typed-program spec / paper
/// §Parameterised Types): "Where a parameterised type refers to itself, directly
/// or transitively, no parameter may occur as a proper subterm of an argument."
///
/// `Stream(X) ::= [] ; [X | Stream(X)]` is fine — the self-reference's argument
/// is the bare parameter `X`. `Bad(X) ::= leaf ; node(Bad(Box(X)))` is rejected
/// — `X` is a proper subterm of `Box(X)`, so expansion would generate
/// `Bad<Box<...Box<X>...>>` without bound. Detected statically here, at the
/// type-parsing/expansion stage, before any expansion runs.
void _checkNoGrowingTypeRecursion(Map<String, TypeDef> templates) {
  // Direct template→template reference edges.
  final refs = <String, Set<String>>{};
  for (final t in templates.values) {
    final s = <String>{};
    for (final alt in t.alternatives) {
      _collectTemplateRefNames(alt, templates, s);
    }
    refs[t.name] = s;
  }

  // Transitive reachability over the reference graph.
  Set<String> reachableFrom(String start) {
    final seen = <String>{};
    final stack = [...?refs[start]];
    while (stack.isNotEmpty) {
      final n = stack.removeLast();
      if (!seen.add(n)) continue;
      stack.addAll(refs[n] ?? const <String>{});
    }
    return seen;
  }
  final reach = {for (final name in templates.keys) name: reachableFrom(name)};

  // For each template T, every parameterised reference U(args) in T's body that
  // can reach back to T (a reference on a cycle through T) must not carry a
  // parameter of T as a proper subterm of an argument.
  for (final t in templates.values) {
    for (final alt in t.alternatives) {
      _checkRefArgsOnCycle(alt, t, templates, reach);
    }
  }
}

/// Collect the names of templates referenced anywhere within [expr].
void _collectTemplateRefNames(
    TypeExpr expr, Map<String, TypeDef> templates, Set<String> out) {
  if (expr is TypeRef) {
    if (templates.containsKey(expr.name)) out.add(expr.name);
    for (final a in expr.typeArgs) {
      _collectTemplateRefNames(a, templates, out);
    }
    return;
  }
  for (final c in _typeExprChildren(expr)) {
    _collectTemplateRefNames(c, templates, out);
  }
}

/// Walk [expr]; for each parameterised reference `U(args)` on a cycle through
/// [t], reject any argument carrying a parameter of [t] as a proper subterm.
void _checkRefArgsOnCycle(TypeExpr expr, TypeDef t,
    Map<String, TypeDef> templates, Map<String, Set<String>> reach) {
  if (expr is TypeRef) {
    if (expr.typeArgs.isNotEmpty && templates.containsKey(expr.name)) {
      final u = expr.name;
      final onCycle = u == t.name || (reach[u]?.contains(t.name) ?? false);
      if (onCycle) {
        for (final arg in expr.typeArgs) {
          for (final p in t.typeParams) {
            if (_paramOccursIn(p, arg) && !_isBareParam(arg, p)) {
              throw Exception(
                  'Type checking failed: parameterised type "${t.name}" refers '
                  'to "$u" on a recursive cycle with parameter "$p" occurring as '
                  'a proper subterm of argument "$arg"; this would make the set '
                  'of reachable types infinite. In a recursive parameterised '
                  'type, no parameter may occur as a proper subterm of an '
                  'argument (typed-program: Parameterised Types).');
            }
          }
        }
      }
    }
    for (final a in expr.typeArgs) {
      _checkRefArgsOnCycle(a, t, templates, reach);
    }
    return;
  }
  for (final c in _typeExprChildren(expr)) {
    _checkRefArgsOnCycle(c, t, templates, reach);
  }
}

/// Immediate TypeExpr children of a non-TypeRef node.
Iterable<TypeExpr> _typeExprChildren(TypeExpr expr) {
  if (expr is StructAlt) return expr.args;
  if (expr is ListConsAlt) return [expr.head, expr.tail];
  if (expr is DiffListAlt) return [expr.content, expr.hole];
  return const <TypeExpr>[];
}

/// Does the type parameter named [p] occur anywhere within [expr]?
bool _paramOccursIn(String p, TypeExpr expr) {
  if (expr is TypeRef) {
    if (expr.name == p && expr.typeArgs.isEmpty) return true;
    return expr.typeArgs.any((a) => _paramOccursIn(p, a));
  }
  return _typeExprChildren(expr).any((c) => _paramOccursIn(p, c));
}

/// Is [arg] exactly the bare parameter named [p] (so [p] is not a *proper*
/// subterm of it)?
bool _isBareParam(TypeExpr arg, String p) =>
    arg is TypeRef && arg.name == p && arg.typeArgs.isEmpty;

/// The type names a procedure declaration may use without declaring them: the
/// templates and monomorphic definitions in scope, the primitives, and the
/// names the ancestor scope supplies.
Set<String> _declKnownTypes(Map<String, TypeDef> templates,
        List<TypeDef> monoTypeDefs, Set<String> externalKnownTypes) =>
    <String>{
      ...templates.keys,
      ...monoTypeDefs.map((td) => td.name),
      ...TypeRef.builtins,
      ...TypeRef.systemTypes,
      ...externalKnownTypes,
    };

/// The type parameters of a procedure declaration: exactly those its parameter
/// list names.
///
/// Spec: Moded-Types, sections/parameterized-types.tex, paragraph "Declaration
/// parameters" — "The parameters of a procedure declaration are exactly those
/// its parameter list names."  Naming them is what lets a misspelt type name be
/// rejected instead of read as a parameter: `=(X, X?)` and `pass(Strem?, Strem)`
/// are the same declaration up to renaming, so no rule reading either alone
/// tells them apart.
///
/// TRANSITIONAL, until every declaration in the tree carries its list.  A
/// declaration that names its parameters gets the rule above, and its undefined
/// names are rejected.  A declaration that names none falls back to the
/// inference the rule replaces, because the strict reading applied to an
/// unswept tree rejects root self.glp's `=(X, X?)` and every load with it.  The
/// sweep is per owner (GLP-Spec's root self.glp, SGSG's routers under
/// social/graph/routing, IGLP's fixtures under programs/tests/); when it is
/// done, delete [_inferProcTypeParamsTransitional] and its two helpers and call
/// [_checkDeclTypeNames] unconditionally.
List<String> _procTypeParams(ProcDecl pd, Set<String> knownTypes) {
  if (pd.typeParams.isNotEmpty) {
    _checkDeclTypeNames(pd, knownTypes);
    return pd.typeParams;
  }
  return _inferProcTypeParamsTransitional(pd, knownTypes);
}

/// The inference the named parameter list replaces: a type parameter is a name
/// that is not a known defined type and either appears as a bare typeArg inside
/// a TypeRef with typeArgs (X in `Stream(X)`), or appears as a bare top-level
/// argument type (M in `p(M?, Stream(Ent)?)`).
///
/// It cannot separate a parameter from a typo, which is why the paper replaced
/// it.  Reached only by a declaration that names no parameters; see
/// [_procTypeParams] for what removes it.
List<String> _inferProcTypeParamsTransitional(ProcDecl pd, Set<String> knownTypes) {
  final candidates = <String>{};
  // Names appearing as typeArgs of any TypeRef with typeArgs (inner positions).
  for (final arg in pd.argTypes) {
    _collectInnerTypeParamCandidates(arg, knownTypes, candidates);
  }
  // Bare top-level argument names that are not known types.
  for (final arg in pd.argTypes) {
    if (arg is TypeRef && arg.typeArgs.isEmpty && !knownTypes.contains(arg.name)) {
      candidates.add(arg.name);
    }
  }
  return candidates.toList();
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
  for (final c in _typeExprChildren(expr)) {
    _collectInnerTypeParamCandidates(c, knownTypes, candidates);
  }
}

/// Reject an undefined type name occurring in [pd] and not in its parameter
/// list.
///
/// Spec: Moded-Types, "Declaration parameters" — "An undefined type name
/// occurring in a declaration and not in its parameter list is an error, so a
/// misspelt type name is rejected rather than read as a parameter."
///
/// A qualified name (`mod#T`) names a type in another module and is resolved by
/// the module system, so it is left to it.
void _checkDeclTypeNames(ProcDecl pd, Set<String> knownTypes) {
  final params = pd.typeParams.toSet();
  final undefined = <TypeRef>[];
  for (final arg in pd.argTypes) {
    _collectUndefinedTypeNames(arg, knownTypes, params, undefined);
  }
  if (undefined.isEmpty) return;
  final r = undefined.first;
  final where = 'line ${r.line > 0 ? r.line : pd.line}, '
      'column ${r.line > 0 ? r.column : pd.column}';
  final list = pd.typeParams.isEmpty
      ? 'the declaration names no type parameters'
      : 'its type parameters are ${pd.typeParams.join(', ')}';
  throw Exception(
      'Type checking failed: undefined type "${r.name}" in the declaration of '
      '${pd.name}/${pd.arity} at $where, and $list. An undefined type name in a '
      'declaration and not in its parameter list is an error; if "${r.name}" is '
      'meant as a type parameter, name it — procedure(${r.name}) ${pd.name}(...) '
      '(typed-program: Declaration parameters).');
}

/// Collect every named type reference in [expr] that is neither a known type
/// nor a declared parameter.  Qualified names are skipped.
void _collectUndefinedTypeNames(TypeExpr expr, Set<String> knownTypes,
    Set<String> params, List<TypeRef> undefined) {
  if (expr is TypeRef) {
    if (!params.contains(expr.name) &&
        !knownTypes.contains(expr.name) &&
        !expr.name.contains('#')) {
      undefined.add(expr);
    }
    for (final arg in expr.typeArgs) {
      _collectUndefinedTypeNames(arg, knownTypes, params, undefined);
    }
    return;
  }
  for (final c in _typeExprChildren(expr)) {
    _collectUndefinedTypeNames(c, knownTypes, params, undefined);
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
