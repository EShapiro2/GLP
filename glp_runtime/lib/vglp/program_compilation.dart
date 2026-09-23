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
import '../compiler/lexer.dart';
import '../compiler/parser.dart';
import '../compiler/error.dart';
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
  checkDisplayDecls(module);
  checkQuestionParameters(module);
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

/// The question parameters of each procedure of [module] are exactly the
/// writers its volition guards name, or the compilation rejects the source.
///
/// "A procedure declaration carries its question parameters after its argument
/// list, procedure p(...) *(X1, ..., Xm)., being every writer that a volition
/// guard of the procedure names, each of them once; a volition guard's writers
/// are matched to them by name" (vGLP, sections/vglp.tex, Section
/// "Volition-Guarded GLP").  EVERY writer, so a guard writer the declaration
/// does not name is refused; every writer A VOLITION GUARD NAMES, so a
/// declared parameter no guard of the procedure names is refused as well.
/// Each of them once is the parser's, the list being where the repetition
/// would be.
///
/// The check is vGLP's: a GLP program has no volition guards, and the
/// declarations the compilation carries into the compiled program keep their
/// parameters with no guard left to match them against (Definition "Canonical
/// Compilation": the compiled program carries the procedure declarations of
/// M).  It is therefore run here, on the source, and not on the module the
/// compilation emits.
///
/// A procedure with no declaration is passed over: compileProgram refuses it
/// by itself, and with its own message.
void checkQuestionParameters(ast.Module module) {
  final declsByKey = <String, ProcDecl>{
    for (final d in module.procDeclarations) '${d.name}/${d.arity}': d
  };

  for (final proc in module.procedures) {
    final decl = declsByKey['${proc.name}/${proc.arity}'];
    if (decl == null) continue;

    // The writers the procedure's volition guards name, in source order, each
    // once, with the line of the first guard that names each.
    final namedAt = <String, int>{};
    for (final c in proc.clauses) {
      final g = c.volitionGuard;
      if (g == null) continue;
      for (final pos in g.question) {
        final w = pos.writer;
        if (w == null) continue;  // an anonymous writer names nothing
        namedAt.putIfAbsent(w.name, () => g.line);
      }
    }

    final declared = decl.questionParams;
    final subject = '${proc.name}/${proc.arity}';
    final list = declared.isEmpty
        ? 'no question parameters'
        : '*(${declared.join(', ')})';

    for (final entry in namedAt.entries) {
      if (declared.contains(entry.key)) continue;
      throw CompileError(
          'The volition guard names the writer "${entry.key}", which the '
          'declaration of $subject does not: it carries $list.  The question '
          'parameters of a procedure are EVERY writer a volition guard of it '
          'names, each of them once, and a guard\'s writers are matched to '
          'them by name (vGLP, Section "Volition-Guarded GLP").  Write '
          '"procedure ${proc.name}(...) '
          '*(${([...declared, ...namedAt.keys.where((k) => !declared.contains(k))]).join(', ')}).".',
          entry.value,
          0,
          phase: 'analyzer');
    }

    for (final param in declared) {
      if (namedAt.containsKey(param)) continue;
      throw CompileError(
          'The declaration of $subject names the question parameter '
          '"$param", which no volition guard of the procedure names'
          '${namedAt.isEmpty ? '' : ': its guards name '
              '${namedAt.keys.join(', ')}'}.  The question parameters are '
          'every writer a volition guard of the procedure names, and nothing '
          'besides (vGLP, Section "Volition-Guarded GLP").',
          decl.line,
          decl.column,
          phase: 'analyzer');
    }
  }
}

/// Every clause-form display declaration of [module] names a volition-guarded
/// clause the module has, or the compilation rejects the source.
///
/// A display declaration is "for a volition-guarded clause of predicate p with
/// volition guard *(...)", and it "names its clause's volition guard, so an
/// else-branch has none of its own" (vGLP, Definition "Display Declaration,
/// Default Display").  A declaration whose predicate and guard no clause of the
/// module carries therefore names no clause: there is nothing it is the display
/// of, and nothing the bridge would render by it.  It is an error in the
/// source, and the compilation says so instead of carrying the declaration
/// verbatim into the compiled program, which is what `:emit` and the load both
/// did until 2026-09-20.  A declaration may also carry a clause index, naming
/// "the n-th of several clauses of p with that volition guard" (same
/// definition), so the match is by predicate, guard and index: an index beyond
/// the number of clauses of the predicate with that guard names no clause
/// either.
void checkDisplayDecls(ast.Module module) {
  final guardsOf = <String, List<String>>{};
  final predicates = <String>{};
  for (final p in module.procedures) {
    predicates.add(p.name);
    for (final c in p.clauses) {
      if (c.volitionGuard != null) {
        guardsOf
            .putIfAbsent(p.name, () => <String>[])
            .add(printVolitionGuard(c.volitionGuard!));
      }
    }
  }

  for (final d in module.displayDecls) {
    if (!d.isClauseForm) continue;
    final p = d.predicate!;
    final guard = printVolitionGuard(d.guard!);
    final have = guardsOf[p] ?? const <String>[];
    // The declaration names the n-th of the clauses of p with that guard where
    // it carries an index, and one such clause where it does not.
    final matching = have.where((g) => g == guard).length;
    final wanted = d.index ?? 1;
    if (matching >= wanted) continue;

    final subject =
        'display $p $guard${d.index == null ? '' : ' ${d.index}'}';
    final String missing;
    if (!predicates.contains(p)) {
      missing = 'the program has no procedure $p';
    } else if (have.isEmpty) {
      missing = 'no clause of $p carries a volition guard';
    } else if (matching == 0) {
      missing = 'the volition-guarded clauses of $p carry '
          '${have.toSet().join(', ')}';
    } else {
      missing = '$p has $matching '
          '${matching == 1 ? 'clause' : 'clauses'} with that volition guard';
    }
    throw CompileError(
        'The display declaration "$subject : ..." names no clause: '
        '$missing.  A display declaration is for a volition-guarded clause and '
        'names that clause\'s volition guard, an else-branch having none of its '
        'own, and its clause index the n-th of several clauses of the '
        'predicate with that guard (vGLP, Definition "Display Declaration, '
        'Default Display").',
        d.line,
        d.column,
        phase: 'analyzer');
  }
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
