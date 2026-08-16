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
import '../analysis/type_checker/type_ast.dart';
import '../analysis/type_checker/type_environment_builder.dart';
import '../analysis/type_checker/param_expansion.dart';

/// The name a compiled program gives its answer type and its context type.
const answerTypeName = 'Answer';
const contextTypeName = 'Context';

/// One volition-guarded clause of M, with the identity the compilation gives
/// it: the name its ask carries, and the types of its answer and context.
class ClauseTypes {
  /// The name the ask carries, `<predicate>_<j>` for the j-th
  /// volition-guarded clause of the procedure.
  final String name;

  /// `Xs_C ::= xs(t1, ..., ti)`; null where the clause's question is empty, in
  /// which case the ask carries `xs` and contributes no alternative.
  final TypeDef? answer;

  /// `Ctx_C ::= ctx(t'1, ..., t'j)`; null where the clause has no context.
  final TypeDef? context;

  ClauseTypes(this.name, this.answer, this.context);
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
    {List<ast.Module> ancestors = const []}) {
  final env = _environmentOf(module, ancestors);

  final byClause = <String, ClauseTypes>{};
  final added = <TypeDef>[];
  final answerAlts = <TypeExpr>[];
  final contextAlts = <TypeExpr>[];

  for (final proc in module.procedures) {
    var j = 0;
    for (final c in proc.clauses) {
      if (!c.isVolitionGuarded) continue;
      j++;
      final name = '${proc.name}_$j';

      final q = c.volitionGuard!.question;
      final ctx = c.volitionGuard!.context;

      TypeDef? answer;
      if (q.isNotEmpty) {
        final typeName = 'Xs_$name';
        answer = TypeDef(typeName, [
          StructAlt('xs', [
            for (final pos in q)
              _typeOfVariable(pos.writer?.name, c, env, name)
          ], c.line, c.column)
        ], c.line, c.column);
        added.add(answer);
        answerAlts.add(TypeRef(typeName, c.line, c.column));
      }

      TypeDef? context;
      if (ctx.isNotEmpty) {
        final typeName = 'Ctx_$name';
        context = TypeDef(typeName, [
          StructAlt('ctx', [
            for (final v in ctx) _typeOfVariable(v.name, c, env, name)
          ], c.line, c.column)
        ], c.line, c.column);
        added.add(context);
        contextAlts.add(TypeRef(typeName, c.line, c.column));
      }

      byClause[name] = ClauseTypes(name, answer, context);
    }
  }

  // A clause with an empty question or an empty context still asks, carrying
  // the bare constant `xs` or `ctx`; the program's type therefore has that
  // constant as an alternative whenever some clause omits one.
  if (answerAlts.length < byClause.length) {
    answerAlts.add(ConstantAlt('xs', 0, 0));
  }
  if (contextAlts.length < byClause.length) {
    contextAlts.add(ConstantAlt('ctx', 0, 0));
  }

  if (byClause.isNotEmpty) {
    added.add(TypeDef(answerTypeName, answerAlts, 0, 0));
    added.add(TypeDef(contextTypeName, contextAlts, 0, 0));
  }

  return CompiledTypes(added, _rewriteDeclarations(module), byClause);
}

/// Each procedure declaration of M with the added arguments: the mediator
/// channel first, then one slot per volition-guarded clause of the procedure,
/// in the order of the compiled head.  Both are input positions: the caller
/// supplies the channel it holds and the slots the goal carries.
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
          TypeRef('Slot', 0, 0,
              isInput: true, typeArgs: [TypeRef(answerTypeName, 0, 0)]),
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

/// `Channel(Closed, Stream(AgentMsg(Answer, Context)))`, the compiled goal's
/// end of the mediator channel: it only ever sends, so its read side is closed.
TypeExpr medChannelType({required bool isInput}) => TypeRef('Channel', 0, 0,
        isInput: isInput,
        typeArgs: [
          TypeRef('Closed', 0, 0),
          TypeRef('Stream', 0, 0, typeArgs: [
            TypeRef('AgentMsg', 0, 0, typeArgs: [
              TypeRef(answerTypeName, 0, 0),
              TypeRef(contextTypeName, 0, 0),
            ])
          ])
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
TypeEnvironment _environmentOf(ast.Module module, List<ast.Module> ancestors) {
  var base = buildRootScopeEnvironment();
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

/// The type of the answer writer or context reader [variable] of clause [c].
///
/// It is NOT read off a clause check.  A volition guard's answer writers have
/// no writer occurrence in the clause — the guard is where they are written
/// (vGLP, Definition "Guarded Clause, ..."), so the ordinary clause of C does
/// not satisfy SRSW on its own and the checker gives it no verdict and no
/// types.  The type is therefore resolved by POSITION: the first occurrence of
/// the variable in the head or a body goal is found, and the declared type of
/// that argument is walked down to the sub-position the variable sits at.
TypeExpr _typeOfVariable(String? variable, ast.Clause c, TypeEnvironment env,
    String clauseName) {
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

  for (final g in c.body ?? const <ast.Goal>[]) {
    final decl = env.getProcedure(g.functor, g.args.length);
    if (decl == null) continue;
    for (var k = 0; k < g.args.length && k < decl.argTypes.length; k++) {
      final found = _walk(g.args[k], decl.argTypes[k], variable, env);
      if (found != null) return found;
    }
  }

  throw StateError(
      'The type of "$variable" in the volition guard of $clauseName cannot be '
      'resolved: it occurs at no argument position of the clause head or of a '
      'declared body goal.  A volition guard names writers whose readers occur '
      'in the clause (vGLP, Definition "Guarded Clause, ...").');
}

/// Walk [term] against [type], looking for [variable]; return the type at the
/// position it sits at, or null if it is not in this argument.
TypeExpr? _walk(ast.Term term, TypeExpr type, String variable,
    TypeEnvironment env) {
  if (term is ast.VarTerm) {
    return term.name == variable ? _bare(type) : null;
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

/// The alternatives a type expression offers, following one named type.
List<TypeExpr> _alternatives(TypeExpr type, TypeEnvironment env) {
  if (type is TypeRef) {
    final def = env.types[type.name];
    if (def != null) return def.alternatives;
  }
  return const [];
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
