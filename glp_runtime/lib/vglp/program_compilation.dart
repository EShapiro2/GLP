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
String compileVglpSource(String text,
    {MediatorSource? mediator, TypeEnvironment? scope, String? path}) {
  if (isPaperSyntaxSource(text)) {
    final dir = mediator?.directory;
    return compileCanonical(text,
            dispatcher: mediator?.dispatcher,
            scope: scope,
            scopeWidgets: path != null && dir != null
                ? scopeWidgetDeclarations(path, Directory(dir).parent.path)
                : const {})
        .source;
  }
  if (mediator == null) {
    throw StateError('${path ?? 'The source'} is in the old syntax, and the '
        'generic mediator source it compiles against is missing');
  }
  final module =
      Parser(Lexer(text).tokenize(), vglp: true).parseModule();
  return compileProgram(module, mediator, scope: scope).source;
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
