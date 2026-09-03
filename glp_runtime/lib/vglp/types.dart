// glp_runtime/lib/vglp/types.dart
//
// The types the canonical compilation adds.
// Spec: vGLP, sections/elicitation.tex, Definition "Canonical Compilation",
// the "types" item, and sections/vglp.tex: "vGLP is typed as GLP is ... a
// program carries type definitions and procedure declarations, and a
// volition-guarded clause is typed as its ordinary clause is, its answer
// writers by their occurrences".
//
// The compilation is typed at both ends: it takes a typed vGLP program and
// produces a typed GLP program, which must typecheck as its source does.  What
// it adds:
//
//   - per volition-guarded clause C, the answer type Xs_C ::= xs(t1,...,ti) at
//     the types of C's answer writers, and the context type
//     Ctx_C ::= ctx(t'1,...,t'j) at the types of its context readers;
//   - the program's answer type A ::= Xs_C1 ; ... ; Xs_Ck and context type
//     X ::= Ctx_C1 ; ... ; Ctx_Ck over its volition-guarded clauses;
//   - the mediator's vocabulary, instantiated at A and X;
//   - each procedure declaration of M with the added arguments, the mediator
//     channel first and then the slots, in the order of the compiled head.
//
// The answer writers' types are not declared anywhere in M — a writer of a
// volition guard occurs in the clause's guard and body, not necessarily in its
// head — so they are READ OFF THE TYPE CHECKER: the ordinary clause of C is
// checked in M's own environment and each writer's inferred type is taken from
// the result.  This is why the compilation needs M typed rather than merely
// declared.

import '../compiler/ast.dart' as ast;
import '../compiler/glp_printer.dart';
import '../analysis/type_checker/type_ast.dart';
import '../analysis/type_checker/type_environment_builder.dart';
import '../analysis/type_checker/param_expansion.dart';

/// The names a compiled program gives its answer type, its escrow type and
/// its context type.
const answerTypeName = 'Answer';
const escrowTypeName = 'Escrow';
const contextTypeName = 'Context';

/// One volition-guarded clause of M, with the identity the compilation gives
/// it: the name its ask carries, and the types of its answer and context.
class ClauseTypes {
  /// The name the ask carries, `<predicate>_<j>` for the j-th
  /// volition-guarded clause of the procedure.
  final String name;

  /// `Xs_C ::= xs_C(t1, ..., ti)`, or `Xs_C ::= xs_C` where the question is
  /// empty.
  final TypeDef answer;

  /// `Reply_C ::= then(Xs_C) ; else` where C has an else-branch, and
  /// `Reply_C ::= then(Xs_C)` where not: a slot admits exactly the replies
  /// its clause can receive.
  final TypeDef reply;

  /// `Ctx_C ::= ctx_C(t'1, ..., t'j)`; null where the clause has no context,
  /// in which case the ask carries the bare constant `ctx_C`.
  final TypeDef? context;

  ClauseTypes(this.name, this.answer, this.reply, this.context);
}

/// The types the compilation of [module] adds, and the rewritten procedure
/// declarations.
class CompiledTypes {
  final List<TypeDef> typeDefs;
  final List<ProcDecl> procDecls;
  final Map<String, ClauseTypes> byClause;

  CompiledTypes(this.typeDefs, this.procDecls, this.byClause);
}

/// Build the compiled program's types.
///
/// [slotCountOf] gives the number of volition-guarded clauses of a procedure of
/// M, which is the number of slot arguments its compiled declaration gains.
CompiledTypes compileTypes(ast.Module module,
    {List<ast.Module> ancestors = const [], TypeEnvironment? scope}) {
  final env = _environmentOf(module, ancestors, scope);

  final byClause = <String, ClauseTypes>{};
  final added = <TypeDef>[];
  final answerAlts = <TypeExpr>[];
  final escrowAlts = <TypeExpr>[];
  final contextAlts = <TypeExpr>[];

  for (final proc in module.procedures) {
    var j = 0;
    for (final c in proc.clauses) {
      if (!c.isVolitionGuarded) continue;
      j++;
      final name = '${proc.name}_$j';

      final q = c.volitionGuard!.question;
      final ctx = c.volitionGuard!.context;

      // The answer and context terms carry the clause's own functor, xs_C and
      // ctx_C: the program's answer type is a union over its clauses, and a
      // union's top-level functors must be distinct, so two clauses whose
      // questions have the same length cannot both contribute `xs/i`.
      // An empty question still asks, carrying the bare constant xs_C, which
      // is then the type's one alternative.
      final answer = TypeDef('Xs_$name', [
        q.isEmpty
            ? ConstantAlt('xs_$name', c.line, c.column)
            : StructAlt('xs_$name', [
                for (final pos in q)
                  pos.writer == null
                      ? _typeOfValue(pos.value!, c)
                      : _typeOfVariable(pos.writer!.name, c, env, name)
              ], c.line, c.column)
      ], c.line, c.column);
      added.add(answer);
      answerAlts.add(TypeRef('Xs_$name', c.line, c.column));

      // The reply type admits exactly what the clause can receive: the
      // then-branch with its answer, and else only where it has an else-branch.
      final reply = TypeDef('Reply_$name', [
        StructAlt('then', [TypeRef('Xs_$name', c.line, c.column)],
            c.line, c.column),
        if (c.elseBranch != null) ConstantAlt('else', c.line, c.column),
      ], c.line, c.column);
      added.add(reply);
      // The escrow names the clause and holds the writer of its reply.
      escrowAlts.add(StructAlt('esc_$name',
          [TypeRef('Reply_$name', c.line, c.column, isInput: true)],
          c.line, c.column));

      TypeDef? context;
      if (ctx.isNotEmpty) {
        final typeName = 'Ctx_$name';
        context = TypeDef(typeName, [
          StructAlt('ctx_$name', [
            for (final v in ctx) _typeOfVariable(v.name, c, env, name)
          ], c.line, c.column)
        ], c.line, c.column);
        added.add(context);
        contextAlts.add(TypeRef(typeName, c.line, c.column));
      } else {
        contextAlts.add(ConstantAlt('ctx_$name', c.line, c.column));
      }

      byClause[name] = ClauseTypes(name, answer, reply, context);
    }
  }

  if (byClause.isNotEmpty) {
    added.add(TypeDef(answerTypeName, answerAlts, 0, 0));
    added.add(TypeDef(escrowTypeName, escrowAlts, 0, 0));
    added.add(TypeDef(contextTypeName, contextAlts, 0, 0));
  }

  return CompiledTypes(added, _rewriteDeclarations(module), byClause);
}

/// Each procedure declaration of M with the added arguments: the mediator
/// channel first, then one slot per volition-guarded clause of the procedure,
/// in the order of the compiled head, the j-th typed `Slot(Reply_Cj)` by its
/// own clause's reply type.  Both are input positions: the caller supplies
/// the channel it holds and the slots the goal carries.
List<ProcDecl> _rewriteDeclarations(ast.Module module) {
  final slots = <String, int>{
    for (final p in module.procedures)
      '${p.name}/${p.arity}':
          p.clauses.where((c) => c.isVolitionGuarded).length
  };

  final out = <ProcDecl>[];
  for (final d in module.procDeclarations) {
    final m = slots['${d.name}/${d.arity}'];
    if (m == null) {
      out.add(d);  // a declaration with no clauses here — a builtin or import
      continue;
    }
    out.add(ProcDecl(
      d.name,
      [
        medChannelType(isInput: true),
        ...d.argTypes,
        for (var k = 0; k < m; k++)
          TypeRef('Slot', 0, 0, isInput: true, typeArgs: [
            TypeRef('Reply_${d.name}_${k + 1}', 0, 0),
          ]),
      ],
      d.line,
      d.column,
      typeParams: d.typeParams,
      isBuiltin: d.isBuiltin,
      exported: d.exported,
      imported: d.imported,
      modulePath: d.modulePath,
    ));
  }
  return out;
}

/// `Channel(Closed, Stream(AgentMsg))`, the compiled goal's end of the
/// mediator channel: it only ever sends, so its read side is closed.  AgentMsg
/// is the vocabulary's, instantiated at the program's types: monomorphic.
TypeExpr medChannelType({required bool isInput}) => TypeRef('Channel', 0, 0,
        isInput: isInput,
        typeArgs: [
          TypeRef('Closed', 0, 0),
          TypeRef('Stream', 0, 0, typeArgs: [TypeRef('AgentMsg', 0, 0)])
        ]);

// ---------------------------------------------------------------------------
// Reading the answer and context types off the declared types
// ---------------------------------------------------------------------------

/// The environment of M, built as the type checker builds it: parameterised
/// types expanded, the root scope beneath, and beneath M itself the program's
/// own ancestor `self.glp` modules.
///
/// The ancestors are not optional in practice: a .vglp source calls procedures
/// its program declares in `self.glp` — `inject_enrol_response`, `send_net` —
/// and an answer writer is typed by the position it occurs at, which may be an
/// argument of one of those.  Without them the writer has no type and the
/// compilation stops on a defect that is not in the source.
///
/// [scope] is the module's ancestor scope as the loader built it, which is the
/// complete answer: it carries the root scope, every ancestor `self.glp`, and
/// the modules an ancestor `-expose`s — `send_net` and the rest of
/// `social/graph/routing`, which a .vglp source calls and no ancestor of it
/// declares.  Given it, [ancestors] is redundant and is for callers with no
/// loader.
TypeEnvironment _environmentOf(ast.Module module, List<ast.Module> ancestors,
    TypeEnvironment? scope) {
  var base = scope ?? buildRootScopeEnvironment();
  for (final a in ancestors) {
    final expandedAncestor = expandParameterizedTypes(a,
        knownTypeNames: base.types.keys.toSet(),
        externalTemplates: base.typeTemplates);
    base = buildTypeEnvironment(expandedAncestor, ancestorScope: base,
        typeTemplates: {
          for (final td in a.typeDefs)
            if (td.isParameterized) td.name: td,
        });
  }
  final expanded = expandParameterizedTypes(module,
      knownTypeNames: base.types.keys.toSet(),
      externalTemplates: base.typeTemplates);
  return buildTypeEnvironment(expanded, ancestorScope: base, typeTemplates: {
    for (final td in module.typeDefs)
      if (td.isParameterized) td.name: td,
  });
}

/// The type of an anonymous answer position, which is the type of the ground
/// term the volition guard writes there: `yes` is a Constant, `3` an Integer,
/// and a compound term takes the primitive type `_`, since it has no declared
/// one.  (`Any` is the checker's internal name and is not source syntax.)
TypeExpr _typeOfValue(ast.Term value, ast.Clause c) {
  if (value is ast.ConstTerm) {
    final v = value.value;
    if (v is int) return TypeRef('Integer', c.line, c.column);
    if (v is double) return TypeRef('Real', c.line, c.column);
    if (v is String && v.startsWith('"')) {
      return TypeRef('String', c.line, c.column);
    }
    return TypeRef('Constant', c.line, c.column);
  }
  return PrimitiveModeAlt(false, c.line, c.column);
}

/// The type of the answer writer or context reader [variable] of clause [c].
///
/// It is NOT read off a clause check.  A volition guard's answer writers have
/// no writer occurrence in the clause — the guard is where they are written
/// (vGLP, Definition "Guarded Clause, ..."), so the ordinary clause of C does
/// not satisfy SRSW on its own and the checker gives it no verdict and no
/// types.  The type is therefore resolved by POSITION: the first occurrence of
/// the variable in the head or a body goal is found, and the declared type of
/// that argument is walked down to the sub-position the variable sits at.
///
/// A writer whose only occurrence is one side of a guard `X? =?= T` has the
/// type of the other side, since the guard compares two terms of one type.
///
/// A parameterised body goal --- `send_friend(Constant?, M?, Stream(Ent)?,
/// Stream(Ent))` and the rest of `lib/routing` --- is instantiated at the call
/// from the term's shape: a position declared as a bare type parameter has no
/// type of its own, so the term there is typed by the program's types that
/// accept it, and the variable takes the type of its position in them.  The
/// head and the monomorphic goals are tried first, so that a variable with an
/// occurrence at a declared position is typed by it and never by a shape.
TypeExpr _typeOfVariable(String? variable, ast.Clause c, TypeEnvironment env,
    String clauseName, {Set<String> visiting = const {}}) {
  if (variable == null) return TypeRef('Any', c.line, c.column);

  final headDecl = env.getProcedure(c.head.functor, c.head.arity);
  if (headDecl != null) {
    for (var k = 0; k < c.head.args.length && k < headDecl.argTypes.length;
        k++) {
      final found =
          _walk(c.head.args[k], headDecl.argTypes[k], variable, env);
      if (found != null) return found;
    }
  }

  final parameterised = <(ast.Goal, ProcDecl)>[];
  for (final g in c.body ?? const <ast.Goal>[]) {
    final decl = _declarationOf(g, env);
    if (decl == null) continue;
    if (decl.isParameterized) {
      parameterised.add((g, decl));
      continue;
    }
    for (var k = 0; k < g.args.length && k < decl.argTypes.length; k++) {
      final found = _walk(g.args[k], decl.argTypes[k], variable, env);
      if (found != null) return found;
    }
  }

  for (final guard in c.guards ?? const <ast.Guard>[]) {
    if (guard.predicate != '=?=' || guard.args.length != 2) continue;
    final other = _otherSideOf(guard, variable);
    if (other == null) continue;
    final found = _typeOfTerm(other, c, env, clauseName,
        visiting: {...visiting, variable});
    if (found != null) return found;
  }

  for (final (g, decl) in parameterised) {
    final params = decl.typeParams.toSet();
    for (var k = 0; k < g.args.length && k < decl.argTypes.length; k++) {
      final found = _walk(g.args[k], decl.argTypes[k], variable, env,
          params: params, clauseName: clauseName);
      if (found != null) return found;
    }
  }

  throw StateError(
      'The type of "$variable" in the volition guard of $clauseName cannot be '
      'resolved: it occurs at no argument position of the clause head or of a '
      'declared body goal.  A volition guard names writers whose readers occur '
      'in the clause (vGLP, Definition "Guarded Clause, ...").');
}

/// The side of the guard `A =?= B` that [variable] is not, where [variable]
/// is one side of it; null otherwise.
ast.Term? _otherSideOf(ast.Guard guard, String variable) {
  final a = guard.args[0];
  final b = guard.args[1];
  if (a is ast.VarTerm && a.name == variable) return b;
  if (b is ast.VarTerm && b.name == variable) return a;
  return null;
}

/// The type of a whole term of the clause, for the other side of a guard: a
/// variable's by the clause, a constant's by its value, and a compound term's
/// the one type of the program that accepts it.  Null where it has none, or
/// where the variable is already being resolved (two writers compared only
/// with each other type neither).
TypeExpr? _typeOfTerm(ast.Term t, ast.Clause c, TypeEnvironment env,
    String clauseName, {required Set<String> visiting}) {
  if (t is ast.VarTerm) {
    if (visiting.contains(t.name)) return null;
    try {
      return _typeOfVariable(t.name, c, env, clauseName, visiting: visiting);
    } on StateError {
      return null;
    }
  }
  if (t is ast.ConstTerm) return _typeOfValue(t, c);
  if (t is ast.StructTerm || t is ast.ListTerm) {
    final accepting = [
      for (final def in env.types.values)
        if (!def.isParameterized &&
            def.name != 'Any' &&
            _accepts(t, TypeRef(def.name, 0, 0), env, 0))
          def.name
    ];
    if (accepting.length == 1) return TypeRef(accepting.single, c.line, c.column);
  }
  return null;
}

/// The declaration a body goal is checked against: the parameterised template
/// where there is one, since that is the one whose type parameters are known,
/// else the monomorphic declaration.
ProcDecl? _declarationOf(ast.Goal g, TypeEnvironment env) =>
    env.paramProcDecls['${g.functor}/${g.args.length}'] ??
    env.getProcedure(g.functor, g.args.length);

/// Walk [term] against [type], looking for [variable]; return the type at the
/// position it sits at, or null if it is not in this argument.  [params] are
/// the type parameters of the declaration being walked; a position typed by
/// one is instantiated from the term's shape.
TypeExpr? _walk(ast.Term term, TypeExpr type, String variable,
    TypeEnvironment env,
    {Set<String> params = const {}, String clauseName = ''}) {
  final atParameter =
      type is TypeRef && type.typeArgs.isEmpty && params.contains(type.name);

  if (term is ast.VarTerm) {
    if (term.name != variable) return null;
    // A variable that is the whole argument at a bare type parameter gives the
    // parameter no shape to be instantiated from, and the position no type.
    return atParameter ? null : _bare(type);
  }

  if (atParameter) {
    if (!_mentions(term, variable)) return null;
    return _typeByShape(term, variable, env, clauseName);
  }

  if (term is ast.StructTerm) {
    for (final alt in _alternatives(type, env)) {
      if (alt is StructAlt &&
          alt.functor == term.functor &&
          alt.args.length == term.args.length) {
        for (var k = 0; k < term.args.length; k++) {
          final found = _walk(term.args[k], alt.args[k], variable, env);
          if (found != null) return found;
        }
      }
    }
    return null;
  }

  if (term is ast.ListTerm) {
    if (term.isNil) return null;
    for (final alt in _alternatives(type, env)) {
      if (alt is ListConsAlt) {
        if (term.head != null) {
          final found = _walk(term.head!, alt.head, variable, env);
          if (found != null) return found;
        }
        if (term.tail != null) {
          final found = _walk(term.tail!, alt.tail, variable, env);
          if (found != null) return found;
        }
      }
    }
    return null;
  }

  return null;
}

/// The type of [variable]'s position in [term], where [term] stands at a
/// position declared as a bare type parameter: every monomorphic type of the
/// program that accepts the term is a candidate instantiation, and the
/// variable's type is the type of its position in them, which they must
/// agree on.  A term of no type, or of types that disagree on the position,
/// leaves the variable untyped, and the compilation stops on it.
TypeExpr _typeByShape(
    ast.Term term, String variable, TypeEnvironment env, String clauseName) {
  final accepting = <String>[];
  final found = <String, TypeExpr>{};
  for (final def in env.types.values) {
    if (def.isParameterized) continue;
    final ref = TypeRef(def.name, 0, 0);
    if (!_accepts(term, ref, env, 0)) continue;
    accepting.add(def.name);
    final t = _walk(term, ref, variable, env);
    if (t != null) found[t.toString()] = t;
  }
  if (found.length == 1) return found.values.single;

  final text = GlpPrinter().printTerm(term);
  final where = 'The type of "$variable" in the volition guard of $clauseName '
      'cannot be resolved: it occurs in $text at a position declared as a type '
      'parameter, ';
  if (found.isEmpty) {
    throw StateError(accepting.isEmpty
        ? '${where}and $text is a term of no type of the program.'
        : '${where}and no type of the program that $text is a term of '
            '(${accepting.join(', ')}) types the position of "$variable".');
  }
  throw StateError('${where}and the types of the program that $text is a term '
      'of (${accepting.join(', ')}) disagree on the type of "$variable": '
      '${found.keys.join(', ')}.');
}

/// Whether [term] is a term of [type]: a variable is a term of every type, a
/// constant of a type with it as an alternative or of its primitive type, and
/// a compound term of a type with an alternative of its functor whose
/// arguments accept its own.
bool _accepts(ast.Term term, TypeExpr type, TypeEnvironment env, int depth) {
  if (term is ast.VarTerm || term is ast.UnderscoreTerm) return true;
  if (depth > 64) return false;
  if (type is PrimitiveModeAlt) return true;
  if (type is TypeRef) {
    switch (type.name) {
      case 'Any':
      case '_':
        return true;
      case 'Constant':
        return term is ast.ConstTerm && _isAtom(term);
      case 'String':
        return term is ast.ConstTerm && _isString(term);
      case 'Integer':
        return term is ast.ConstTerm && term.value is int;
      case 'Real':
        return term is ast.ConstTerm && term.value is double;
      case 'Number':
        return term is ast.ConstTerm &&
            (term.value is int || term.value is double);
    }
    return _alternatives(type, env)
        .any((a) => _accepts(term, a, env, depth + 1));
  }
  if (type is ConstantAlt) {
    return term is ast.ConstTerm && term.value == type.value;
  }
  if (type is StructAlt) {
    if (term is! ast.StructTerm ||
        term.functor != type.functor ||
        term.args.length != type.args.length) {
      return false;
    }
    for (var k = 0; k < term.args.length; k++) {
      if (!_accepts(term.args[k], type.args[k], env, depth + 1)) return false;
    }
    return true;
  }
  if (type is ListNilAlt) return term is ast.ListTerm && term.isNil;
  if (type is ListConsAlt) {
    return term is ast.ListTerm &&
        !term.isNil &&
        (term.head == null || _accepts(term.head!, type.head, env, depth + 1)) &&
        (term.tail == null || _accepts(term.tail!, type.tail, env, depth + 1));
  }
  return false;
}

bool _isAtom(ast.ConstTerm t) {
  final v = t.value;
  return v is String && !v.startsWith('"');
}

bool _isString(ast.ConstTerm t) {
  final v = t.value;
  return v is String && v.startsWith('"');
}

/// Whether [variable] occurs in [term].
bool _mentions(ast.Term term, String variable) {
  if (term is ast.VarTerm) return term.name == variable;
  if (term is ast.StructTerm) return term.args.any((a) => _mentions(a, variable));
  if (term is ast.ListTerm) {
    return (term.head != null && _mentions(term.head!, variable)) ||
        (term.tail != null && _mentions(term.tail!, variable));
  }
  return false;
}

/// The alternatives a type expression offers, following one named type.  A
/// reference to a parameterised type, `Stream(FriendMsg)`, offers the
/// template's alternatives with its arguments in place of the parameters ---
/// or those of the instantiation the environment already holds under its
/// expanded name, `Stream<FriendMsg>`.
List<TypeExpr> _alternatives(TypeExpr type, TypeEnvironment env) {
  if (type is! TypeRef) return const [];
  final def = env.types[type.name];
  if (def != null && (def.typeParams.isEmpty || type.typeArgs.isEmpty)) {
    return def.alternatives;
  }
  final template = def ?? env.typeTemplates[type.name];
  if (template != null && template.typeParams.length == type.typeArgs.length) {
    final subst = {
      for (var i = 0; i < template.typeParams.length; i++)
        template.typeParams[i]: type.typeArgs[i]
    };
    return [for (final a in template.alternatives) _subst(a, subst)];
  }
  final expanded = env.types[_expandedName(type)];
  if (expanded != null) return expanded.alternatives;
  return const [];
}

/// `Stream<FriendMsg>`, the environment's name for an instantiation.
String _expandedName(TypeRef t) => t.typeArgs.isEmpty
    ? t.name
    : '${t.name}<${t.typeArgs.map((a) => a is TypeRef ? _expandedName(a) : a.toString()).join(', ')}>';

/// [e] with the type parameters in [subst] replaced.
TypeExpr _subst(TypeExpr e, Map<String, TypeExpr> subst) {
  if (e is TypeRef) {
    final bound = subst[e.name];
    if (bound != null && e.typeArgs.isEmpty) {
      return bound is TypeRef
          ? TypeRef(bound.name, e.line, e.column,
              isInput: e.isInput, typeArgs: bound.typeArgs)
          : bound;
    }
    return TypeRef(e.name, e.line, e.column,
        isInput: e.isInput,
        typeArgs: [for (final a in e.typeArgs) _subst(a, subst)]);
  }
  if (e is StructAlt) {
    return StructAlt(e.functor, [for (final a in e.args) _subst(a, subst)],
        e.line, e.column);
  }
  if (e is ListConsAlt) {
    return ListConsAlt(
        _subst(e.head, subst), _subst(e.tail, subst), e.line, e.column);
  }
  if (e is DiffListAlt) {
    return DiffListAlt(
        _subst(e.content, subst), _subst(e.hole, subst), e.line, e.column);
  }
  return e;
}

/// The type at a position, without its mode: the answer's type is the type of
/// the value, and the ask carries it in one direction only.
TypeExpr _bare(TypeExpr type) {
  if (type is TypeRef) {
    return TypeRef(type.name, type.line, type.column,
        isInput: false, typeArgs: type.typeArgs);
  }
  return type;
}
