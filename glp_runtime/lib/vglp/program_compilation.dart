// glp_runtime/lib/vglp/program_compilation.dart
//
// The canonical compilation as a whole: a vGLP module becomes one
// self-contained GLP module.
// Spec: vGLP, sections/elicitation.tex, Definition "Canonical Compilation".
//
// ⌈M⌉ is the compiled procedures "together with the mediator", so the mediator
// is part of the compiled program and not a library it imports — which the module system forces anyway, a module path resolving from
// the program root downward.
//
// The emission is ONE FILE.  The mediator's vocabulary types the compiled
// agent's own slots and channel, and a GLP module cannot see the types of
// another module below it in the tree — only of one above.  Emitting the agent
// and the mediator together is what makes the compiled module self-contained,
// and it is why the compiled program needs nothing of programs/vglp/ at run
// time.

import 'dart:io';

import '../compiler/ast.dart' as ast;
import '../compiler/error.dart';
import '../compiler/lexer.dart';
import '../compiler/parser.dart';
import '../compiler/glp_printer.dart';
import '../compiler/token.dart';
import '../analysis/type_checker/type_ast.dart';
import '../runtime/module_hierarchy.dart' show discoverSelfChain;
import 'canonical.dart';
import 'clause_compilation.dart';
import 'constructs.dart' show InteractiveType;
import 'mediator.dart';
import 'types.dart';

/// The first line of every emitted module, by which the emitter tells its own
/// output from a hand-written module it must not overwrite.
const compiledHeader =
    '%% Compiled from vGLP by the canonical compilation\n'
    '%% (vGLP, Definition "Canonical Compilation").  Do not edit: edit\n'
    '%% the .vglp source and compile again.\n';

/// The compiled program: one GLP module's source text, and the pieces it was
/// built from, for tests to inspect.
class CompiledProgram {
  final String source;
  final CompiledTypes types;
  final List<CompiledProcedure> procedures;

  CompiledProgram(this.source, this.types, this.procedures);
}

/// Compile a vGLP module against the generic mediator source.
///
/// [scope] is the module's ancestor scope as the loader built it — the root
/// scope, the ancestor `self.glp` chain, and whatever an ancestor `-expose`s.
/// A .vglp source calls procedures none of its own declarations names, and an
/// answer writer may be typed by one of them, so the loader supplies this.
/// [ancestors] is the same thing for a caller with no loader.
CompiledProgram compileProgram(ast.Module module, MediatorSource mediator,
    {List<ast.Module> ancestors = const [], TypeEnvironment? scope}) {
  final types = compileTypes(module, ancestors: ancestors, scope: scope);
  final med = instantiate(mediator);

  final declsByKey = <String, ProcDecl>{
    for (final d in module.procDeclarations) d.key: d
  };
  final compiledDecls = <String, ProcDecl>{
    for (final d in types.procDecls) d.key: d
  };
  final defined = <String>{
    for (final p in module.procedures) '${p.name}/${p.arity}'
  };
  final slotCounts = <String, int>{
    for (final p in module.procedures)
      '${p.name}/${p.arity}':
          p.clauses.where((c) => c.isVolitionGuarded).length
  };

  final compiled = <CompiledProcedure>[];
  for (final p in module.procedures) {
    final decl = declsByKey['${p.name}/${p.arity}'];
    if (decl == null) {
      throw StateError(
          'The procedure ${p.name}/${p.arity} has no declaration.  The '
          'compilation is typed at both ends and cannot compile an undeclared '
          'procedure: the ask clause passes each argument of the head by mode '
          '(Definition "Canonical Compilation").');
    }
    compiled.add(compileProcedure(p,
        decl: decl,
        isProcedureOfM: (n, a) => defined.contains('$n/$a'),
        clauseName: (proc, j) => '${proc.name}_$j',
        slotCountOf: (n, a) => slotCounts['$n/$a'] ?? 0));
  }

  final pending = pendingTableClauses(
      module.procedures, (proc, j) => '${proc.name}_$j');

  return CompiledProgram(
      _emit(module, types, med, compiled, compiledDecls, pending),
      types,
      compiled);
}

String _emit(ast.Module module, CompiledTypes types, InstantiatedMediator med,
    List<CompiledProcedure> compiled, Map<String, ProcDecl> compiledDecls,
    PendingTableClauses pending) {
  final b = StringBuffer();
  final printer = GlpPrinter();

  b.write(compiledHeader);
  b.writeln();

  b.writeln('%% --- the source\'s own types ---');
  for (final td in module.typeDefs) {
    b.writeln(printTypeDef(td));
  }
  b.writeln();

  b.writeln('%% --- the types the compilation adds ---');
  for (final td in types.typeDefs) {
    b.writeln(printTypeDef(td));
  }
  b.writeln();

  b.writeln('%% --- the mediator\'s vocabulary, instantiated ---');
  for (final td in med.typeDefs) {
    b.writeln(printTypeDef(td));
  }
  b.writeln();

  if (module.displayDecls.isNotEmpty) {
    b.writeln('%% --- the display declarations, carried through ---');
    for (final d in module.displayDecls) {
      b.writeln(printDisplayDecl(d));
    }
    b.writeln();
  }

  b.writeln('%% --- the compiled agent ---');
  for (final cp in compiled) {
    final decl = compiledDecls['${cp.name}/${cp.arity}'];
    if (decl != null) b.writeln(printProcDecl(decl));
    for (final c in cp.clauses) {
      b.writeln(printer.printClause(c));
    }
    b.writeln();
  }

  b.writeln('%% --- the mediator, and the pending table with the program\'s '
      'clauses ahead of the search clauses ---');
  final medDecls = <String, ProcDecl>{for (final d in med.procDecls) d.key: d};
  for (final p in med.procedures) {
    final decl = medDecls['${p.name}/${p.arity}'];
    if (decl != null) b.writeln(printProcDecl(decl));
    // The pending table's answer and close clauses are the program's, one per
    // volition-guarded clause; the mediator source carries the search clauses
    // that follow them (Definition "Canonical Compilation").
    final own = p.name == 'answer' && p.arity == 4
        ? pending.answer
        : p.name == 'close' && p.arity == 3
            ? pending.close
            : const <ast.Clause>[];
    for (final c in own) {
      b.writeln(printer.printClause(c));
    }
    for (final c in p.clauses) {
      b.writeln(printer.printClause(c));
    }
    b.writeln();
  }

  return b.toString();
}

/// Compile the text of a `.vglp` source to the text of its GLP module.
///
/// A source in the paper's syntax --- `procedure (T)*p(...)`, `(A)*p(...)` ---
/// compiles by the canonical compilation of vGLP at db03e2d (canonical.dart),
/// against the dispatcher's generic source in the [mediator]'s directory and
/// in its [scope].  A source in the old syntax, with volition guards `*(...)`,
/// keeps its old compilation, against the generic [mediator], until its owner
/// ports it (vGLP's code task of 2026-10-01, item 6); it has none to compile
/// against where [mediator] is null.  A source in the paper's syntax at
/// [path] is compiled with the widget declarations of its scope, read from
/// the self.vglp beside each self.glp from the root down
/// (scopeWidgetDeclarations), the root being the parent of the [mediator]'s
/// directory, programs/.
///
/// A source at [path] is compiled with the exports of the modules it imports
/// from read beside it (_SiblingExports): an import of a procedure of
/// another module that reaches a question is "compiled as its export is"
/// (vGLP, Definition "Canonical Compilation", the import sentence), and a
/// source in neither syntax that has such an import reaches a question
/// through it, and is compiled by the canonical compilation too
/// (importsReachingProcedure).  Modules that import from each other
/// procedures that reach a question are compiled together, their
/// interactive types joined once (the Definition, its next sentence).
String compileVglpSource(String text,
    {MediatorSource? mediator, TypeEnvironment? scope, String? path}) {
  final canonical = path == null
      ? (isPaperSyntaxSource(text)
          ? compileCanonical(text,
              dispatcher: mediator?.dispatcher, scope: scope)
          : null)
      : _SiblingExports(mediator, scope).compile(path, text);
  if (canonical != null) return canonical.source;
  if (mediator == null) {
    throw StateError('${path ?? 'The source'} is in the old syntax, and the '
        'generic mediator source it compiles against is missing');
  }
  final module =
      Parser(Lexer(text).tokenize(), vglp: true).parseModule();
  return compileProgram(module, mediator, scope: scope).source;
}

/// The exports of the modules a `.vglp` source imports from, as its
/// compilation reads them: "compiled as their exports are" (vGLP, Definition
/// "Canonical Compilation", the import sentence).
///
/// The qualifier N of an import is "a single child directory or module file
/// relative to the caller's directory" (TGLP modules.tex, "Cross-module type
/// checking").  A module file N.vglp beside the source, which the loader
/// compiles, is read by its own canonical compilation, in the same scope ---
/// the ancestor chain of one directory --- with the same generic source and
/// the same widget declarations of that scope; so is one whose N.glp beside
/// it is its compiled module, which the loader links in its place.  A module
/// N.glp written by hand, which stands, is a GLP module, as is any qualifier
/// naming no N.vglp beside the source --- a directory, entered through its
/// self.glp, or a path of several segments, which TGLP leaves to future
/// work: no export is read of it, and no procedure of it reaches a question.
///
/// Each source is compiled once per call.  "Modules that import from each
/// other procedures that reach a question are compiled together, their
/// interactive types joined once" (vGLP at 66b6500, the Definition; vGLP #7
/// Cowork, 2026-10-11 14:19 UTC, item 2): a source that imports, directly or
/// through others, from a module that imports from it is compiled with all
/// of them (_compileComponent), and a source that does not is compiled as
/// its own, reading the export of each module it imports from.
class _SiblingExports {
  final MediatorSource? mediator;
  final TypeEnvironment? scope;
  final _compiled = <String, CanonicalProgram?>{};

  /// The modules of a component compiled together that are refused, each
  /// with its refusal, which every reading of its export meets.
  final _refused = <String, CompileError>{};
  final _compiling = <String>{};

  _SiblingExports(this.mediator, this.scope);

  /// The canonical compilation of the source [text] at [path], or null where
  /// it is not compiled by it: in the old syntax, or in neither syntax with
  /// no import of a procedure that reaches a question.
  CanonicalProgram? compile(String path, String text) {
    final key = File(path).absolute.path;
    if (_compiled.containsKey(key)) return _compiled[key];
    final refused = _refused[key];
    if (refused != null) throw refused;
    if (!_compiling.add(key)) {
      // The modules that import from each other are compiled together
      // (_compileComponent), and none of them is read through the export of
      // another on the way, so no compilation reads its own.
      throw StateError('The compilation of $path reads its own export, and '
          'the modules that import from each other are compiled together');
    }
    try {
      final component = _component(key, text, path);
      if (component != null) {
        _compileComponent(component);
        final r = _refused[key];
        if (r != null) throw r;
        return _compiled[key];
      }
      CanonicalProgram? reader(String module) => _exportOf(key, module);
      CanonicalProgram? result;
      if (isPaperSyntaxSource(text) || importsReachingProcedure(text, reader)) {
        result = _compileOne(path, text, reader);
      }
      return _compiled[key] = result;
    } finally {
      _compiling.remove(key);
    }
  }

  CanonicalProgram _compileOne(String path, String text, ExportReader exports,
      {Set<String> ownTogether = const {}}) {
    final dir = mediator?.directory;
    return compileCanonical(text,
        dispatcher: mediator?.dispatcher,
        scope: scope,
        scopeWidgets: dir != null
            ? scopeWidgetDeclarations(path, Directory(dir).parent.path)
            : const {},
        exports: exports,
        ownTogether: ownTogether);
  }

  /// The module file [module].vglp beside the source at [importer], by its
  /// absolute path, or null where [module] is no such vGLP module.
  String? _sibling(String importer, String module) {
    if (module.contains('#')) return null;
    final dir = File(importer).parent.path;
    final sep = Platform.pathSeparator;
    final vglp = File('$dir$sep$module.vglp');
    if (!vglp.existsSync()) return null;
    final glp = File('$dir$sep$module.glp');
    if (glp.existsSync() &&
        !glp.readAsStringSync().startsWith(compiledHeader)) {
      return null;
    }
    return vglp.absolute.path;
  }

  /// The export of [module] as the source at [importer] reads it: the
  /// canonical compilation of the module file [module].vglp beside it, or
  /// null where [module] is no such vGLP module.
  CanonicalProgram? _exportOf(String importer, String module) {
    final path = _sibling(importer, module);
    return path == null ? null : compile(path, File(path).readAsStringSync());
  }

  /// The modules that import from each other with the source [text] at
  /// [key], directly or through others, by the module files their imports
  /// name beside them, or null where it imports from none that imports from
  /// it.  An import is followed whatever it imports, which procedures reach
  /// a question being decided only by all of them together.
  _Component? _component(String key, String text, String path) {
    final texts = <String, String>{key: text};
    final fronts = <String, ModuleFront?>{};
    final edges = <String, List<String>>{};
    void visit(String k) {
      if (edges.containsKey(k)) return;
      final out = edges[k] = <String>[];
      ModuleFront? front;
      try {
        front = readModuleFront(texts[k]!);
      } on CompileError {
        // Refused by its own compilation, which reads no export of it.
        front = null;
      }
      fronts[k] = front;
      for (final i in front?.imports ?? const <FrontImport>[]) {
        final t = _sibling(k, i.module);
        if (t == null) continue;
        if (!out.contains(t)) out.add(t);
        texts.putIfAbsent(t, () => File(t).readAsStringSync());
        visit(t);
      }
    }

    visit(key);
    final scc = _components(edges).firstWhere((c) => c.contains(key));
    if (scc.length == 1 && !edges[key]!.contains(key)) return null;
    return _Component(
        {...scc}, {for (final k in scc) k: k == key ? path : k}, texts, fronts);
  }

  /// Compile the modules of [c], which import from each other.
  ///
  /// Which procedures of each reach a question is the least fixpoint over
  /// the calls of all of them, an import of a procedure of one of them
  /// reaching one where that procedure does.  The modules that import from
  /// each other procedures that reach a question are then compiled together,
  /// each such set once its imports of procedures reaching a question of the
  /// others are compiled: their interactive types joined once
  /// (joinComponentTypes), and each compiled by the canonical compilation,
  /// an import of another of them compiled as its export is, by what was read
  /// of it.  One that imports none from a module that imports from it is
  /// compiled as its own.
  void _compileComponent(_Component c) {
    final members = c.members.toList()..sort();
    final er = {for (final k in members) k: <String>{}};
    final reach = {for (final k in members) k: <String>{}};

    bool reaches(String k, FrontImport i) {
      final t = _sibling(k, i.module);
      if (t != null && c.members.contains(t)) {
        return er[t]!.contains(i.signature);
      }
      final CanonicalProgram? n;
      try {
        n = _exportOf(k, i.module);
      } on CompileError {
        // Its compilation refuses the importer when it reads the export.
        return false;
      }
      return n != null && n.exportedReaching.contains(i.signature);
    }

    for (var changed = true; changed;) {
      changed = false;
      for (final k in members) {
        final f = c.fronts[k];
        if (f == null) continue;
        final r = f.reaching((i) => reaches(k, i));
        final e = f.exportedReaching(r);
        reach[k] = r;
        if (e.length != er[k]!.length) {
          er[k] = e;
          changed = true;
        }
      }
    }

    final canonical = {
      for (final k in members)
        k: isPaperSyntaxSource(c.texts[k]!) ||
            (c.fronts[k]?.imports.any((i) => reaches(k, i)) ?? false)
    };

    // The imports of procedures that reach a question, between them.
    final real = <String, List<String>>{
      for (final k in members)
        k: [
          if (canonical[k]!)
            for (final i in c.fronts[k]?.imports ?? const <FrontImport>[])
              if (_sibling(k, i.module) case final t?
                  when c.members.contains(t) &&
                      (i.volitional || er[t]!.contains(i.signature)))
                t
        ]
    };

    // The export of [module] as the module [k] of the component reads it: of
    // one compiled before it, its compilation; of one compiled after it,
    // which it imports no procedure reaching a question from, what was read
    // of it.
    ExportReader readerOf(String k) => (module) {
          final t = _sibling(k, module);
          if (t == null || !c.members.contains(t)) return _exportOf(k, module);
          final r = _refused[t];
          if (r != null) throw r;
          if (_compiled.containsKey(t)) return _compiled[t];
          return canonical[t]! ? _standIn(c, t, reach[t]!, er[t]!) : null;
        };

    for (final set in _components(real)) {
      final k = set.first;
      if (set.length == 1 && !real[k]!.contains(k)) {
        try {
          _compiled[k] = canonical[k]!
              ? _compileOne(c.paths[k]!, c.texts[k]!, readerOf(k))
              : null;
        } on CompileError catch (e) {
          _refused[k] = e;
        }
        continue;
      }
      _compileTogether(c, (set.toList()..sort()), reach, er, readerOf);
    }
  }

  /// Compile the modules [together], which import from each other
  /// procedures that reach a question: "compiled together, their interactive
  /// types joined once" (vGLP at 66b6500, Definition "Canonical
  /// Compilation").  Their interactive types are joined before any of them
  /// is compiled, two of two of them with one functor told apart in import
  /// order (_importOrder; vGLP #8 Cowork, 2026-10-11 16:44 UTC, Q1), and each
  /// import of a procedure of another of them is compiled as that one's
  /// export is, by what was read of it with those types; the compiled modules
  /// are held to it, and one refused refuses them all.
  ///
  /// "The module that defines a type declares its widget, and the others
  /// declare none for it" (vGLP #8 Cowork, Q2): a type of theirs has the
  /// widget the modules that define it draw it with, by their widget
  /// declarations --- of the type, or of a type inside it --- and their
  /// scope, and every module of them has it so, as a type imported from
  /// outside them has the widget its export gives it.  So each is compiled
  /// twice: first with the widget of every type of theirs drawn by its own
  /// declarations, which gives the widget each module that defines a type
  /// draws it with; then with those widgets, each module's declaration for a
  /// type it does not define refused as one for a type it imports.  Of two
  /// modules that define one type, one whose declarations draw it as its
  /// scope does declares nothing for it and takes the other's; two that draw
  /// it apart are refused, one type with two widgets.
  void _compileTogether(
      _Component c,
      List<String> together,
      Map<String, Set<String>> reach,
      Map<String, Set<String>> er,
      ExportReader Function(String k) readerOf) {
    final errors = <String, CompileError>{};
    String name(String k) {
      final file = File(k).uri.pathSegments.last;
      return file.substring(0, file.length - '.vglp'.length);
    }

    // The members, each with the exports of the modules outside them that
    // its imports of procedures reaching a question are of.
    final members = <ComponentMember>[];
    for (final k in together) {
      final read = readerOf(k);
      final outside = <(FrontImport, CanonicalProgram)>[];
      for (final i in c.fronts[k]!.imports) {
        if (together.contains(_sibling(k, i.module))) continue;
        final CanonicalProgram? n;
        try {
          n = read(i.module);
        } on CompileError {
          // Its compilation refuses the importer when it reads the export.
          continue;
        }
        if (n == null) continue;
        if (i.volitional || n.exportedReaching.contains(i.signature)) {
          outside.add((i, n));
        }
      }
      members.add(ComponentMember(name(k), c.fronts[k]!, outside));
    }

    ({List<InteractiveType> types, Map<String, List<String>> definers})?
        joined;
    try {
      final (order, undecided) = _importOrder(together, name);
      joined = joinComponentTypes(members,
          importOrder: order, undecided: undecided);
    } on ComponentRefusal catch (r) {
      errors[together.firstWhere((k) => name(k) == r.member)] = r.error;
    }

    var results = <String, CanonicalProgram>{};
    if (joined != null) {
      final functors = {for (final t in joined.types) t.written: t.functor};
      final byName = {for (final k in together) name(k): k};
      final definers = {
        for (final MapEntry(key: w, value: ns) in joined.definers.entries)
          w: [for (final n in ns) byName[n]!]
      };

      // Each module compiled with the interactive types [types], the types
      // of theirs [own] gives it its own.
      Map<String, CanonicalProgram> compileAll(List<InteractiveType> types,
          Set<String> Function(String k) own, Map<String, CompileError> failed,
          {Map<String, String>? texts}) {
        final standIns = {
          for (final k in together)
            k: _standIn(c, k, reach[k]!, er[k]!,
                interactive: types, functors: functors)
        };
        final out = <String, CanonicalProgram>{};
        for (final k in (texts?.keys ?? together)) {
          final read = readerOf(k);
          try {
            out[k] = _compileOne(c.paths[k]!, texts?[k] ?? c.texts[k]!,
                (module) {
              final t = _sibling(k, module);
              return together.contains(t) ? standIns[t] : read(module);
            }, ownTogether: own(k));
          } on CompileError catch (e) {
            failed[k] = e;
          }
        }
        return out;
      }

      String? widgetOf(CanonicalProgram? p, String w) {
        for (final t in p?.interactive ?? const <InteractiveType>[]) {
          if (t.written == w) return t.widget;
        }
        return null;
      }

      // First, every type of theirs each module's own: the widget each
      // module that defines a type draws it with, and, of one with widget
      // declarations, the widget its scope alone draws it with.
      final all = definers.keys.toSet();
      final drawn = compileAll(joined.types, (_) => all, {});
      final declaring = [
        for (final k in together)
          if (drawn.containsKey(k) &&
              c.fronts[k]!.widgets.positions.isNotEmpty)
            k
      ];
      final undeclared = compileAll(joined.types, (_) => all, {},
          texts: {for (final k in declaring) k: c.fronts[k]!.widgets.stripped});

      final widgets = <String, String?>{};
      final conflicts = <String, CompileError>{};
      for (final MapEntry(key: w, value: ks) in definers.entries) {
        final ds = [for (final k in ks) if (drawn.containsKey(k)) k];
        if (ds.isEmpty) continue;
        final declared = [
          for (final k in ds)
            if (declaring.contains(k) &&
                (!undeclared.containsKey(k) ||
                    widgetOf(undeclared[k], w) != widgetOf(drawn[k], w)))
              k
        ];
        final by = declared.isEmpty ? ds : declared;
        final widget = widgetOf(drawn[by.first], w);
        widgets[w] = widget;
        final other = by.skip(1).where((k) => widgetOf(drawn[k], w) != widget);
        if (other.isEmpty) continue;
        final k = other.first;
        final front = c.fronts[k]!;
        final at = front.widgets.positionOf(w) ??
            (declared.contains(k)
                ? front.widgets.positions.first
                : (() {
                    final v = front.volitional.values.firstWhere(
                        (v) => typeSource(v.interactiveType) == w);
                    return (v.line, v.column);
                  })());
        conflicts[k] = CompileError(
            'The interactive type $w of the modules compiled together, '
            '${together.map(name).join(', ')}, is defined by '
            '${name(by.first)} and ${name(k)} and has the widget $widget in '
            '${name(by.first)} and ${widgetOf(drawn[k], w)} in ${name(k)}'
            '${declared.isEmpty ? '' : ', by their widget declarations'}: '
            '"the module that defines a type declares its widget, and the '
            'others declare none for it" (vGLP #8 Cowork, 2026-10-11 16:44 '
            'UTC, Q2), and their interactive types are joined once, each '
            'with one widget (vGLP, Definition "Canonical Compilation": "each '
            'with the functor and the widget its export gives it"; '
            '$_togetherText)',
            at.$1,
            at.$2,
            phase: 'analyzer');
      }

      // Then each with those widgets, a module's own types those it
      // defines.
      final joint = [
        for (final t in joined.types)
          widgets[t.written] != null ? t.withWidget(widgets[t.written]) : t
      ];
      final failed = <String, CompileError>{};
      results = compileAll(
          joint,
          (k) => {
                for (final MapEntry(key: w, value: ks) in definers.entries)
                  if (ks.contains(k)) w
              },
          failed);
      for (final k in together) {
        final e = failed[k] ?? conflicts[k];
        if (e != null) errors[k] = e;
      }

      // Each compiled as it was read: the procedures that reach a question,
      // and the interactive types with their functors and widgets, are those
      // joined.
      if (errors.isEmpty) {
        final joinedWidgets = {for (final t in joint) t.written: t.widget};
        for (final k in together) {
          final p = results[k]!;
          final types = {for (final t in p.interactive) t.written: t.functor};
          if (p.exportedReaching.length != er[k]!.length ||
              !p.exportedReaching.containsAll(er[k]!) ||
              types.length != functors.length ||
              types.entries.any((e) => functors[e.key] != e.value) ||
              p.interactive.any((t) =>
                  definers.containsKey(t.written) &&
                  t.widget != joinedWidgets[t.written])) {
            throw StateError('${name(k)}, compiled together with '
                '${together.map(name).join(', ')}, is not compiled as it was '
                'read before: its procedures that reach a question '
                '${p.exportedReaching}, read ${er[k]}; its interactive types '
                '$types, joined $functors; their widgets '
                '${{for (final t in p.interactive) t.written: t.widget}}, '
                'joined $joinedWidgets');
          }
        }
      }
    }

    if (errors.isEmpty) {
      for (final k in together) {
        _compiled[k] = results[k];
      }
      return;
    }
    final failed = together.firstWhere(errors.containsKey);
    final e = errors[failed]!;
    for (final k in together) {
      if (errors.containsKey(k)) {
        _refused[k] = errors[k]!;
        continue;
      }
      final at = c.fronts[k]!.imports.firstWhere(
          (i) => together.contains(_sibling(k, i.module)));
      _refused[k] = CompileError(
          'The module ${name(failed)}, compiled together with this one '
          '(vGLP, Definition "Canonical Compilation": $_togetherText), is not '
          'compiled: ${e.message} (in ${name(failed)} at line ${e.line}, '
          'column ${e.column})',
          at.line,
          at.column,
          category: e.category);
    }
  }

  /// The modules [together], by their names, in import order, or null where
  /// it is undecided, with why: "the module first in import order keeps the
  /// functor and the other takes _2" (vGLP #8 Cowork, 2026-10-11 16:44 UTC,
  /// Q1), the module first in import order being "the one the program's
  /// entry imports first" (Code #7, 2026-10-11 16:19 UTC, Q1).
  ///
  /// The program's entry, as the compilation finds it, is the self.glp
  /// beside them: their qualifiers name module files of one directory, and a
  /// module outside it reaches them only through it, a directory exporting
  /// through its self.glp (TGLP, Section "Compilation", the fourth step), so
  /// it is the program's entry where the directory is the program and stands
  /// in their order for the program's where the directory is in it.  The
  /// order is the order in which its imports reach them, directly or through
  /// the module files beside it, each file's imports in source order and
  /// followed as they are met.  It is undecided where there is no self.glp
  /// beside them, or one whose imports reach none of them.
  (List<String>?, String) _importOrder(
      List<String> together, String Function(String k) name) {
    final dir = File(together.first).parent.path;
    final sep = Platform.pathSeparator;
    final entry = File('$dir${sep}self.glp');
    if (!entry.existsSync()) {
      return (null, 'there is no self.glp beside them, the program\'s entry');
    }
    final names = {for (final k in together) name(k)};
    final order = <String>[];
    final seen = <String>{};
    void visit(File f) {
      for (final n in _importedModules(
          f.readAsStringSync(), f.path.endsWith('.vglp'))) {
        if (!seen.add(n)) continue;
        if (names.contains(n)) order.add(n);
        final g = _moduleFile(dir, n);
        if (g != null) visit(g);
      }
    }

    visit(entry);
    if (order.isEmpty) {
      return (
        null,
        'the self.glp beside them, the program\'s entry, imports none of '
            'them, directly or through the modules beside it'
      );
    }
    return (order, '');
  }

  /// The module file [module] of the directory [dir], as a qualifier names
  /// it, or null where it names none: N.vglp, or the N.glp beside it where
  /// that is written by hand and stands (_sibling), or an N.glp alone.
  File? _moduleFile(String dir, String module) {
    final sep = Platform.pathSeparator;
    final vglp = File('$dir$sep$module.vglp');
    final glp = File('$dir$sep$module.glp');
    if (vglp.existsSync()) {
      return glp.existsSync() &&
              !glp.readAsStringSync().startsWith(compiledHeader)
          ? glp
          : vglp;
    }
    return glp.existsSync() ? glp : null;
  }

  /// The export of the module [k] of [c] as the compilation reads it before
  /// it is compiled: its volitional procedures, the procedures it exports
  /// that reach a question, and, of modules compiled together, their
  /// interactive types joined once with their functors.
  CanonicalProgram _standIn(
          _Component c, String k, Set<String> reach, Set<String> er,
          {List<InteractiveType> interactive = const [],
          Map<String, String> functors = const {}}) =>
      CanonicalProgram('', c.fronts[k]!.module,
          c.fronts[k]!.volitional.values.toList(), reach, '',
          questionType: '',
          functors: functors,
          interactive: interactive,
          exportedReaching: er);
}

const _togetherText = '"Modules that import from each other procedures that '
    'reach a question are compiled together, their interactive types joined '
    'once"';

/// The modules a source's text names as qualifiers, in source order, each
/// once: N in a declaration `imported procedure N#p(...)` or a call
/// `N # p(...)`, and M in a directive `-expose(M).`, each of one segment, a
/// module file or a child directory of the source's directory (TGLP, Section
/// "Compilation", the fourth step).  A `.vglp` source is read with its widget
/// declarations set aside; a text the lexer refuses names none, its own
/// compilation refusing it.
List<String> _importedModules(String text, bool vglp) {
  final List<Token> ts;
  try {
    ts = Lexer(vglp && text.contains('=::=')
            ? extractWidgetDeclarations(text).stripped
            : text)
        .tokenize();
  } on CompileError {
    return const [];
  }
  bool at(int i, TokenType t) => i >= 0 && i < ts.length && ts[i].type == t;
  final out = <String>[];
  void add(String n) {
    if (!out.contains(n)) out.add(n);
  }

  for (var i = 0; i < ts.length; i++) {
    if (at(i, TokenType.MINUS) &&
        at(i + 1, TokenType.ATOM) &&
        ts[i + 1].lexeme == 'expose' &&
        at(i + 2, TokenType.LPAREN)) {
      final parts = <String>[];
      var j = i + 3;
      while (at(j, TokenType.ATOM)) {
        parts.add(ts[j].lexeme);
        j++;
        if (!at(j, TokenType.HASH)) break;
        j++;
      }
      if (parts.length == 1) add(parts.single);
      i = j;
      continue;
    }
    if (at(i, TokenType.ATOM) &&
        at(i + 1, TokenType.HASH) &&
        at(i + 2, TokenType.ATOM) &&
        !at(i - 1, TokenType.HASH) &&
        !at(i + 3, TokenType.HASH)) {
      add(ts[i].lexeme);
    }
  }
  return out;
}

/// Modules that import from each other, directly or through others: each by
/// its absolute path, with the path it is compiled at, its text and its
/// front.
class _Component {
  final Set<String> members;
  final Map<String, String> paths;
  final Map<String, String> texts;
  final Map<String, ModuleFront?> fronts;
  _Component(this.members, this.paths, this.texts, this.fronts);
}

/// The strongly connected components of the graph [edges], each before any
/// that reaches it (Tarjan).
List<List<String>> _components(Map<String, List<String>> edges) {
  final index = <String, int>{};
  final low = <String, int>{};
  final stack = <String>[];
  final onStack = <String>{};
  final out = <List<String>>[];
  var next = 0;
  void connect(String v) {
    index[v] = low[v] = next++;
    stack.add(v);
    onStack.add(v);
    for (final w in edges[v] ?? const <String>[]) {
      if (!index.containsKey(w)) {
        connect(w);
        low[v] = low[v]! < low[w]! ? low[v]! : low[w]!;
      } else if (onStack.contains(w)) {
        low[v] = low[v]! < index[w]! ? low[v]! : index[w]!;
      }
    }
    if (low[v] == index[v]) {
      final scc = <String>[];
      String w;
      do {
        w = stack.removeLast();
        onStack.remove(w);
        scc.add(w);
      } while (w != v);
      out.add(scc);
    }
  }

  for (final v in edges.keys) {
    if (!index.containsKey(v)) connect(v);
  }
  return out;
}

/// Emit the compiled GLP beside each `.vglp` source under [rootDir], and return
/// the paths written.
///
/// This is the flag's half of the load: the loader compiles a `.vglp` in memory
/// and runs it, and this writes the same text to disc, which is what the paper's
/// platform section exhibits.
///
/// It never clobbers a hand-written module.  The emitted file is `<stem>.glp`,
/// and a `<stem>.glp` that exists and does not carry the compiler's header is
/// left alone and reported: switching a deployed program onto its compiled
/// agent is its own change.
List<String> emitCompiledVglp(String rootDir, MediatorSource? mediator,
    {required TypeEnvironment Function(String vglpPath) scopeFor,
    void Function(String message)? onSkip}) {
  final root = Directory(rootDir);
  if (!root.existsSync()) {
    throw ArgumentError('Program root directory not found: $rootDir');
  }

  final written = <String>[];
  final sources = root
      .listSync(recursive: true)
      .whereType<File>()
      .where((f) => f.path.endsWith('.vglp'))
      .toList();

  for (final file in sources) {
    // A self.vglp holds the widget declarations of its self.glp's scope and
    // is no module (scopeWidgetDeclarations).
    if (file.uri.pathSegments.last == selfVglp) continue;
    final target = '${file.path.substring(0, file.path.length - 5)}.glp';
    final existing = File(target);
    if (existing.existsSync() &&
        !existing.readAsStringSync().startsWith(compiledHeader)) {
      onSkip?.call('$target is hand-written; not overwritten');
      continue;
    }

    final text = file.readAsStringSync();
    existing.writeAsStringSync(compileVglpSource(text,
        mediator: mediator,
        // The old compilation reads types off the checker in this scope; the
        // canonical compilation builds the construct processes from it.
        scope: scopeFor(file.path),
        path: file.path));
    written.add(target);
  }
  return written;
}

/// The file of a directory's widget declarations, beside its self.glp.
const selfVglp = 'self.vglp';

/// The widget declarations in scope of the `.vglp` source at [path]: those of
/// the self.vglp beside each self.glp from the root, [programsDir], down to
/// the source's own directory, each more local one overriding a more global
/// one (Definition "Widget Declaration, Default Widget": "Widget declarations
/// are scoped as type declarations are: a declaration at the root holds for
/// every program, one in a module holds in that module, and a local
/// declaration overrides a global one"; vGLP #5 Cowork, 2026-10-03 08:16 UTC,
/// item 7).  The chain is the one the source's type scope is built from (TGLP
/// modules.tex, Definition "Root, Scope"), a directory with no self.glp
/// adding nothing; the source's own declarations override these in turn
/// (compileCanonical).
Map<String, String> scopeWidgetDeclarations(String path, String programsDir) {
  final chain = [
    '$programsDir${Platform.pathSeparator}self.glp',
    ...discoverSelfChain(
        targetFile: path,
        rootDir: File(path).parent.path,
        programsDir: programsDir),
  ];
  final out = <String, String>{};
  for (final selfGlp in chain) {
    final file = File(
        '${File(selfGlp).parent.path}${Platform.pathSeparator}$selfVglp');
    if (!File(selfGlp).existsSync() || !file.existsSync()) continue;
    out.addAll(readSelfVglp(file.path));
  }
  return out;
}

/// The widget declarations of the self.vglp at [path], a file of `T =::= W.`
/// declarations and comments only; anything else in it is refused, naming
/// the file.
Map<String, String> readSelfVglp(String path) {
  final WidgetDeclarations w;
  try {
    w = extractWidgetDeclarations(File(path).readAsStringSync());
  } on CompileError catch (e) {
    throw CompileError('$path: ${e.message}', e.line, e.column,
        category: e.category);
  }
  final rest = Lexer(w.stripped)
      .tokenize()
      .where((t) => t.type != TokenType.EOF)
      .toList();
  if (rest.isNotEmpty) {
    throw CompileError(
        '$path holds the widget declarations of its self.glp\'s scope, '
        '"T =::= W.", and nothing else (vGLP #5 Cowork, 2026-10-03 08:16 UTC, '
        'item 7)',
        rest.first.line,
        rest.first.column,
        phase: 'parser');
  }
  return w.byModedType;
}
