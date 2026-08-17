// glp_runtime/lib/vglp/program_compilation.dart
//
// The canonical compilation as a whole: a vGLP module becomes one
// self-contained GLP module.
// Spec: vGLP, sections/elicitation.tex, Definition "Canonical Compilation".
//
// ⌈M⌉ is the compiled procedures "together with the mediator ... and the
// timer", so the mediator is part of the compiled program and not a library it
// imports — which the module system forces anyway, a module path resolving from
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
import '../compiler/lexer.dart';
import '../compiler/parser.dart';
import '../compiler/glp_printer.dart';
import '../analysis/type_checker/type_ast.dart';
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

  return CompiledProgram(
      _emit(module, types, med, compiled, compiledDecls), types, compiled);
}

String _emit(ast.Module module, CompiledTypes types, InstantiatedMediator med,
    List<CompiledProcedure> compiled, Map<String, ProcDecl> compiledDecls) {
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

  b.writeln('%% --- the mediator ---');
  final medDecls = <String, ProcDecl>{for (final d in med.procDecls) d.key: d};
  for (final p in med.procedures) {
    final decl = medDecls['${p.name}/${p.arity}'];
    if (decl != null) b.writeln(printProcDecl(decl));
    for (final c in p.clauses) {
      b.writeln(printer.printClause(c));
    }
    b.writeln();
  }

  return b.toString();
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
List<String> emitCompiledVglp(String rootDir, MediatorSource mediator,
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
      .where((f) => f.path.endsWith('.vglp'));

  for (final file in sources) {
    final target = '${file.path.substring(0, file.path.length - 5)}.glp';
    final existing = File(target);
    if (existing.existsSync() &&
        !existing.readAsStringSync().startsWith(compiledHeader)) {
      onSkip?.call('$target is hand-written; not overwritten');
      continue;
    }

    final module = Parser(Lexer(file.readAsStringSync()).tokenize(), vglp: true)
        .parseModule();
    final compiled =
        compileProgram(module, mediator, scope: scopeFor(file.path));
    existing.writeAsStringSync(compiled.source);
    written.add(target);
  }
  return written;
}
