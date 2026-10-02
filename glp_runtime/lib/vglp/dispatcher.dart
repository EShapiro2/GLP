// glp_runtime/lib/vglp/dispatcher.dart
//
// The dispatcher and the generic part of the construct processes: read from
// programs/vglp/dispatcher.glp and instantiated into a compiled program.
// Spec: vGLP, sections/elicitation.tex, Definition "Canonical Compilation" and
// the paragraph before it; vGLP's code task of 2026-10-02 00:13 UTC, Part 2,
// with the answers of 2026-10-01 23:55 UTC (E) and 2026-10-02 08:26 UTC.
//
// The canonical compilation of M consists, besides M's clauses so extended
// and the asking clauses, of "the construct process of T" for each interactive
// type T and "the dispatcher".  Both are GLP written once, in programs/vglp/,
// and emitted into every compiled program, as the mediator of the old design
// was: a module path resolves from the program root downward, so a compiled
// program cannot name programs/vglp/ (mediator.dart).
//
// The generic source names three types and one procedure it does not define,
// which the compilation supplies: Ask, the asks on the ask stream; Question,
// the union of the program's questions; Handle, withdraw; and construct/5,
// the construct process of each interactive type (constructs.dart).  Every
// type and procedure the source does define is emitted under a name fresh
// against the program's, so that no name of the program is taken.

import 'dart:io';

import '../compiler/ast.dart' as ast;
import '../compiler/lexer.dart';
import '../compiler/parser.dart';
import '../analysis/type_checker/type_ast.dart';

/// The types the compilation supplies to the generic source, by the names
/// the source uses for them.
const askTypeRef = 'Ask';
const questionTypeRef = 'Question';
const handleTypeRef = 'Handle';

/// The procedure the compilation supplies: the construct process of each
/// interactive type, construct(Id?, Q?, W?, Gs?, Ds).
const constructHook = 'construct';

/// The dispatcher's entry point, dispatch(Asks?, PersonCh?, MCh), which the
/// bridge or a play spawns beside the initial goal.
const dispatchEntry = 'dispatch';

/// The generic source of the dispatcher and the construct processes.
class DispatcherSource {
  static const fileName = 'dispatcher.glp';

  final ast.Module module;

  DispatcherSource(this.module);

  /// The source in [directory] --- `programs/vglp/` in the tree.
  factory DispatcherSource.fromDirectory(String directory) {
    final file = File('$directory${Platform.pathSeparator}$fileName');
    if (!file.existsSync()) {
      throw StateError(
          'The dispatcher\'s generic source is not at ${file.path}: the '
          'canonical compilation emits the dispatcher and the construct '
          'processes into every compiled program and reads them from there '
          '(vGLP, Definition "Canonical Compilation")');
    }
    return DispatcherSource.fromText(file.readAsStringSync());
  }

  factory DispatcherSource.fromText(String text) =>
      DispatcherSource(Parser(Lexer(text).tokenize()).parseModule());

  /// The source in [directory], or null where it has none.
  static DispatcherSource? inDirectory(String directory) {
    final file = File('$directory${Platform.pathSeparator}$fileName');
    return file.existsSync() ? DispatcherSource.fromDirectory(directory) : null;
  }
}

/// The generic source as one program emits it.
class InstantiatedDispatcher {
  /// The source's own types, renamed.
  final List<TypeDef> typeDefs;

  /// The declarations and procedures emitted: those reachable from the
  /// dispatcher's entry point and from the procedures the construct processes
  /// call, renamed.
  final List<ProcDecl> procDecls;
  final List<ast.Procedure> procedures;

  /// Generic name to emitted name, for the procedures and the types the
  /// source defines, the construct hook included.
  final Map<String, String> procNames;
  final Map<String, String> typeNames;

  InstantiatedDispatcher(this.typeDefs, this.procDecls, this.procedures,
      this.procNames, this.typeNames);

  /// The emitted name of the generic procedure [name].
  String proc(String name) {
    final n = procNames[name];
    if (n == null) {
      throw StateError('The dispatcher\'s generic source defines no procedure '
          '$name, which the construct processes call');
    }
    return n;
  }

  /// The emitted name of the generic type [name].
  String type(String name) {
    final n = typeNames[name];
    if (n == null) {
      throw StateError(
          'The dispatcher\'s generic source defines no type $name');
    }
    return n;
  }
}

/// Instantiate [source] into a program.
///
/// [supplied] gives the emitted type of each of Ask, Question and Handle;
/// [params] are the type parameters these carry, which every declaration
/// mentioning them takes.  [freshType] and [freshProc] give a name fresh
/// against the program's for a stem, and [constructName] is the name the
/// construct hook is emitted under.  [used] are the generic procedures the
/// construct processes call; with the dispatcher's entry point they are the
/// roots from which the emitted procedures are reached.
InstantiatedDispatcher instantiateDispatcher(
  DispatcherSource source, {
  required Map<String, TypeExpr> supplied,
  required List<String> params,
  required String Function(String stem) freshType,
  required String Function(String stem) freshProc,
  required String constructName,
}) {
  final m = source.module;

  // The types the source defines, renamed fresh; the supplied ones replaced.
  final typeNames = <String, String>{
    for (final td in m.typeDefs) td.name: freshType(td.name)
  };

  // The procedures the source defines, renamed fresh; the hook named as the
  // compilation names it.
  final defined = <String>{for (final p in m.procedures) p.name};
  final procNames = <String, String>{
    for (final name in defined) name: freshProc(name),
    constructHook: constructName,
  };

  TypeExpr retype(TypeExpr e) => _retype(e, typeNames, supplied);

  final typeDefs = [
    for (final td in m.typeDefs)
      TypeDef(typeNames[td.name]!, [for (final a in td.alternatives) retype(a)],
          td.line, td.column,
          typeParams: td.typeParams)
  ];

  final procDecls = [
    for (final d in m.procDeclarations)
      ProcDecl(
        procNames[d.name] ?? d.name,
        [for (final t in d.argTypes) retype(t)],
        d.line,
        d.column,
        typeParams: [
          ...d.typeParams,
          if (d.argTypes.any((t) => _mentionsSupplied(t, _parametrised)))
            for (final p in params) if (!d.typeParams.contains(p)) p,
        ],
        exported: d.exported,
      )
  ];

  final procedures = [
    for (final p in m.procedures)
      ast.Procedure(procNames[p.name]!, p.arity,
          [for (final c in p.clauses) _renameClause(c, procNames)],
          p.line, p.column)
  ];

  return InstantiatedDispatcher(
      typeDefs, procDecls, procedures, procNames, typeNames);
}

/// The generic procedures reachable from [roots], by their generic names,
/// over the calls of [source]'s clauses.
Set<String> reachableGeneric(DispatcherSource source, Iterable<String> roots) {
  final byName = <String, List<ast.Procedure>>{};
  for (final p in source.module.procedures) {
    byName.putIfAbsent(p.name, () => []).add(p);
  }
  final seen = <String>{};
  final todo = [...roots.where(byName.containsKey)];
  while (todo.isNotEmpty) {
    final name = todo.removeLast();
    if (!seen.add(name)) continue;
    for (final p in byName[name]!) {
      for (final c in p.clauses) {
        for (final g in c.body ?? const <ast.Goal>[]) {
          if (byName.containsKey(g.functor) && !seen.contains(g.functor)) {
            todo.add(g.functor);
          }
        }
      }
    }
  }
  return seen;
}

TypeExpr _retype(TypeExpr e, Map<String, String> typeNames,
    Map<String, TypeExpr> supplied) {
  if (e is TypeRef) {
    final s = supplied[e.name];
    if (s != null && e.typeArgs.isEmpty) {
      if (s is TypeRef) {
        return TypeRef(s.name, e.line, e.column,
            isInput: e.isInput, typeArgs: s.typeArgs);
      }
      return s;
    }
    return TypeRef(typeNames[e.name] ?? e.name, e.line, e.column,
        isInput: e.isInput,
        typeArgs: [
          for (final a in e.typeArgs) _retype(a, typeNames, supplied)
        ]);
  }
  if (e is StructAlt) {
    return StructAlt(e.functor,
        [for (final a in e.args) _retype(a, typeNames, supplied)],
        e.line, e.column);
  }
  if (e is ListConsAlt) {
    return ListConsAlt(_retype(e.head, typeNames, supplied),
        _retype(e.tail, typeNames, supplied), e.line, e.column);
  }
  if (e is DiffListAlt) {
    return DiffListAlt(_retype(e.content, typeNames, supplied),
        _retype(e.hole, typeNames, supplied), e.line, e.column);
  }
  return e;
}

/// The supplied types that carry the program's type parameters.
const _parametrised = {askTypeRef, questionTypeRef};

bool _mentionsSupplied(TypeExpr e, Set<String> names) {
  if (e is TypeRef) {
    return names.contains(e.name) ||
        e.typeArgs.any((a) => _mentionsSupplied(a, names));
  }
  if (e is StructAlt) return e.args.any((a) => _mentionsSupplied(a, names));
  if (e is ListConsAlt) {
    return _mentionsSupplied(e.head, names) || _mentionsSupplied(e.tail, names);
  }
  return false;
}

ast.Clause _renameClause(ast.Clause c, Map<String, String> names) {
  ast.Goal rename(ast.Goal g) => names.containsKey(g.functor) &&
          g is! ast.RemoteGoal &&
          g is! ast.SpawnGoal
      ? ast.Goal(names[g.functor]!, g.args, g.line, g.column)
      : g;
  return ast.Clause(
      ast.Atom(names[c.head.functor] ?? c.head.functor, c.head.args,
          c.head.line, c.head.column),
      guards: c.guards,
      body: c.body?.map(rename).toList(),
      line: c.line,
      column: c.column);
}
