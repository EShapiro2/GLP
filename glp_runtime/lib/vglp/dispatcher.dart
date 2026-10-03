// glp_runtime/lib/vglp/dispatcher.dart
//
// The dispatcher and the generic part of the construct processes: read from
// programs/vglp/dispatcher.glp and instantiated into a compiled program.
// Spec: vGLP, sections/elicitation.tex, Definition "Canonical Compilation" and
// the paragraph before it; vGLP's code task of 2026-10-02 00:13 UTC, Part 2,
// with the answers of 2026-10-01 23:55 UTC (E) and 2026-10-02 08:26 UTC; the
// generic source's form of 2026-10-02 21:02 UTC (B) without the handle (vGLP
// #5 Cowork, 2026-10-03 08:16 UTC, item 2).
//
// The canonical compilation of M consists, besides M's clauses so extended
// and the asking clauses, of "the construct process of T" for each interactive
// type T and "the dispatcher".  Both are GLP written once, in programs/vglp/,
// and emitted into every compiled program, as the mediator of the old design
// was: a module path resolves from the program root downward, so a compiled
// program cannot name programs/vglp/ (mediator.dart).
//
// The generic source is a module that loads and type-checks by itself: it is
// parameterised in the program's questions, Q of Ask(Q), Spawn(Q),
// dispatch/4 and serve/8, and calls no construct process, writing a spawn on
// dispatch/4's fourth argument instead.  The compilation supplies the
// questions, Question, and the construct process of each interactive type,
// construct/4, with constructs/1, which reads the spawns and calls
// construct/4 on each, and dispatch/3, which spawns dispatch/4 and
// constructs/1 (constructs.dart).  Every type and procedure the source
// defines is emitted under a name fresh against the program's, so that no
// name of the program is taken, and no declaration of it exported: the
// compiled program's entry point is its dispatch/3.

import 'dart:io';

import '../compiler/ast.dart' as ast;
import '../compiler/lexer.dart';
import '../compiler/parser.dart';
import '../analysis/type_checker/type_ast.dart';

/// The generic source's types the compilation names: the asks on the ask
/// stream, Ask(Q); the spawns of the construct processes, Spawn(Q); and a
/// grant, Input.
const askTypeName = 'Ask';
const spawnTypeName = 'Spawn';
const inputTypeName = 'Input';

/// The dispatcher's entry point, dispatch(Asks?, PersonCh?, MCh, Spawns); the
/// compiled program's dispatch/3, which the bridge or a play spawns beside
/// the initial goal, takes the same name.
const dispatchEntry = 'dispatch';

/// The procedures the compilation supplies, by the stems of their names: the
/// construct process of each interactive type, construct(Id?, Q?, Gs?, Ds),
/// and constructs(Spawns?), which reads the dispatcher's spawns and calls it
/// on each.
const constructHook = 'construct';
const constructsHook = 'constructs';

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
  /// source defines.
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
/// [freshType] and [freshProc] give a name fresh against the program's for a
/// stem.  Every declaration is emitted unexported.
InstantiatedDispatcher instantiateDispatcher(
  DispatcherSource source, {
  required String Function(String stem) freshType,
  required String Function(String stem) freshProc,
}) {
  final m = source.module;

  // The types the source defines, renamed fresh.
  final typeNames = <String, String>{
    for (final td in m.typeDefs) td.name: freshType(td.name)
  };

  // The procedures the source defines, renamed fresh.
  final defined = <String>{for (final p in m.procedures) p.name};
  final procNames = <String, String>{
    for (final name in defined) name: freshProc(name),
  };

  TypeExpr retype(TypeExpr e) => _retype(e, typeNames);

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
        typeParams: d.typeParams,
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

TypeExpr _retype(TypeExpr e, Map<String, String> typeNames) {
  if (e is TypeRef) {
    return TypeRef(typeNames[e.name] ?? e.name, e.line, e.column,
        isInput: e.isInput,
        typeArgs: [for (final a in e.typeArgs) _retype(a, typeNames)]);
  }
  if (e is StructAlt) {
    return StructAlt(e.functor, [for (final a in e.args) _retype(a, typeNames)],
        e.line, e.column);
  }
  if (e is ListConsAlt) {
    return ListConsAlt(_retype(e.head, typeNames), _retype(e.tail, typeNames),
        e.line, e.column);
  }
  if (e is DiffListAlt) {
    return DiffListAlt(_retype(e.content, typeNames),
        _retype(e.hole, typeNames), e.line, e.column);
  }
  return e;
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
