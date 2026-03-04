/// Project linker: static linking of multi-module GLP projects.
///
/// Given a project root directory, discovers all modules, type-checks each
/// independently, then produces a single flat Program AST where all
/// inter-module calls are resolved to renamed local procedures.
///
/// Specification: docs/modules/glp-project-compilation-spec.md
/// Plan: docs/modules/project-compilation-implementation-plan.md
library;

import 'dart:io';
import 'ast.dart';
import 'lexer.dart';
import 'parser.dart';
import 'partial_evaluator.dart';
import '../analysis/type_checker/type_ast.dart';
import '../analysis/type_checker/type_environment_builder.dart';
import '../analysis/type_checker/type_checker.dart';
import '../runtime/module_hierarchy.dart';

/// A discovered module in the project tree.
class DiscoveredModule {
  final String filePath;
  final String moduleName;
  final Module ast;
  final TypeEnvironment ancestorScope;

  DiscoveredModule({
    required this.filePath,
    required this.moduleName,
    required this.ast,
    required this.ancestorScope,
  });
}

/// Result of linking a project.
class LinkResult {
  final Program program;
  final List<ProcDecl> procDeclarations;

  LinkResult(this.program, this.procDeclarations);
}

/// Walk the project directory tree and discover all modules.
///
/// For each `.glp` file (excluding `self.glp` and `boot_direct.glp`):
/// - Parse into Module AST
/// - Extract module name (from `-module(M).` or filename)
/// - Build ancestor type scope chain
///
/// `self.glp` files are parsed for type definitions but produce no module.
List<DiscoveredModule> discoverProject(String rootDir) {
  final root = Directory(rootDir);
  if (!root.existsSync()) {
    throw ArgumentError('Project root directory not found: $rootDir');
  }

  final modules = <DiscoveredModule>[];

  // Recursively find all .glp files
  final glpFiles = root
      .listSync(recursive: true)
      .whereType<File>()
      .where((f) => f.path.endsWith('.glp'))
      .toList();

  for (final file in glpFiles) {
    final filename = file.path.split(Platform.pathSeparator).last;

    // Skip self.glp (type definitions only, no procedures)
    if (filename == 'self.glp') continue;

    // Skip boot_direct.glp (copy of boot.glp with direct calls, not a module)
    if (filename == 'boot_direct.glp') continue;

    // Parse the module
    final source = file.readAsStringSync();
    final lexer = Lexer(source);
    final tokens = lexer.tokenize();
    final parser = Parser(tokens);
    final module = parser.parseModule();

    // Extract module name
    final moduleName = module.name ?? _moduleNameFromFilename(filename);

    // Build ancestor scope chain
    final chain = discoverSelfChain(
      targetFile: file.absolute.path,
      rootDir: root.absolute.path,
    );
    final ancestorScope = _buildAncestorScope(chain);

    modules.add(DiscoveredModule(
      filePath: file.path,
      moduleName: moduleName,
      ast: module,
      ancestorScope: ancestorScope,
    ));
  }

  return modules;
}

/// Type-check each module independently against its ancestor scope.
///
/// Throws on type errors with module name and error details.
void typeCheckProject(List<DiscoveredModule> modules) {
  for (final mod in modules) {
    // Skip modules with no procedure declarations (untyped orchestration)
    if (mod.ast.procDeclarations.isEmpty) continue;

    // Only type-check modules that have non-imported declarations
    final hasOwnDecls = mod.ast.procDeclarations.any((d) => !d.imported);
    if (!hasOwnDecls) continue;

    // Run partial evaluation before type checking (same pipeline as compiler)
    final program = Program(mod.ast.procedures, mod.ast.line, mod.ast.column);
    final pe = PartialEvaluator();
    final transformed = pe.transformDefinedGuards(program);

    final result = checkModule(
      mod.ast,
      transformedProcedures: transformed.procedures,
      ancestorScope: mod.ancestorScope,
    );

    if (!result.isWellTyped) {
      final errors = result.errors
          .map((e) => '  ${e.message} at line ${e.line}')
          .join('\n');
      throw Exception(
          'Type checking failed for ${mod.moduleName} (${mod.filePath}):\n$errors');
    }
  }
}

/// Link all modules into a single flat Program.
///
/// Renames procedures (`p/n` → `M:p/n`), resolves all calls, and generates
/// entry point aliases for the top module's procedures.
///
/// Returns a [LinkResult] with the linked program and renamed proc declarations
/// (needed for SRSW type-based relaxation during compilation).
LinkResult linkProject(List<DiscoveredModule> modules, String topModuleName) {
  // Build procedure registry: module name → set of procedure signatures
  final registry = <String, Set<String>>{};
  for (final mod in modules) {
    final sigs = <String>{};
    for (final proc in mod.ast.procedures) {
      sigs.add('${proc.name}/${proc.arity}');
    }
    registry[mod.moduleName] = sigs;
  }

  final allProcedures = <Procedure>[];

  // Process each module
  for (final mod in modules) {
    final localSigs = registry[mod.moduleName]!;

    for (final proc in mod.ast.procedures) {
      final renamedName = '${mod.moduleName}:${proc.name}';
      final renamedClauses = <Clause>[];

      for (final clause in proc.clauses) {
        // Rename clause head
        final renamedHead = Atom(
          '${mod.moduleName}:${clause.head.functor}',
          clause.head.args,
          clause.head.line,
          clause.head.column,
        );

        // Resolve calls in body
        final resolvedBody = clause.body
            ?.map((g) => _resolveGoal(g, mod.moduleName, localSigs))
            .toList();

        renamedClauses.add(Clause(
          renamedHead,
          guards: clause.guards,
          body: resolvedBody,
          line: clause.line,
          column: clause.column,
        ));
      }

      allProcedures.add(Procedure(
        renamedName,
        proc.arity,
        renamedClauses,
        proc.line,
        proc.column,
      ));
    }
  }

  // Generate entry point aliases for the top module's procedures
  final topModule = modules.where((m) => m.moduleName == topModuleName);
  if (topModule.isNotEmpty) {
    for (final proc in topModule.first.ast.procedures) {
      final aliasClause = _makeAliasClause(
        proc.name,
        proc.arity,
        '$topModuleName:${proc.name}',
      );
      allProcedures.add(Procedure(
        proc.name,
        proc.arity,
        [aliasClause],
        0,
        0,
      ));
    }
  }

  // Collect and rename proc declarations for SRSW relaxation
  final allDecls = <ProcDecl>[];
  for (final mod in modules) {
    for (final decl in mod.ast.procDeclarations) {
      if (decl.imported) continue; // Skip imported — they're in other modules
      allDecls.add(ProcDecl(
        '${mod.moduleName}:${decl.name}',
        decl.argTypes,
        decl.line,
        decl.column,
        isBuiltin: decl.isBuiltin,
      ));
    }
  }

  return LinkResult(Program(allProcedures, 0, 0), allDecls);
}

/// Resolve a single goal in a clause body.
Goal _resolveGoal(Goal goal, String moduleName, Set<String> localSigs) {
  // RemoteGoal: M' # p(...) → M':p(...)
  if (goal is RemoteGoal) {
    final targetModule = goal.staticModuleName;
    if (targetModule != null) {
      // Static dispatch: replace with renamed goal
      return Goal(
        '$targetModule:${goal.goal.functor}',
        goal.goal.args,
        goal.line,
        goal.column,
      );
    }
    // Dynamic dispatch — can't resolve statically, leave as-is
    return goal;
  }

  // SpawnGoal: resolve inner goal, keep wrapper
  if (goal is SpawnGoal) {
    final resolvedInner =
        _resolveGoal(goal.innerGoal, moduleName, localSigs);
    if (!identical(resolvedInner, goal.innerGoal)) {
      return SpawnGoal(resolvedInner, goal.agentId, goal.line, goal.column);
    }
    return goal;
  }

  // Regular goal: check if it matches a local procedure
  final sig = '${goal.functor}/${goal.arity}';
  if (localSigs.contains(sig)) {
    return Goal(
      '$moduleName:${goal.functor}',
      goal.args,
      goal.line,
      goal.column,
    );
  }

  // Prelude/stdlib/body kernel — leave unchanged
  return goal;
}

/// Create an alias clause: p(A0, ..., An-1) :- M:p(A0?, ..., An-1?).
Clause _makeAliasClause(String name, int arity, String targetName) {
  if (arity == 0) {
    // Zero-arity: p :- M:p.
    final head = Atom(name, [], 0, 0);
    final body = [Goal(targetName, [], 0, 0)];
    return Clause(head, body: body, line: 0, column: 0);
  }

  // Generate variable names (V prefix — underscore prefix causes issues in codegen)
  final headArgs = List.generate(
      arity, (i) => VarTerm('V$i', false, 0, 0) as Term);
  final bodyArgs = List.generate(
      arity, (i) => VarTerm('V$i', true, 0, 0) as Term);

  final head = Atom(name, headArgs, 0, 0);
  final body = [Goal(targetName, bodyArgs, 0, 0)];
  return Clause(head, body: body, line: 0, column: 0);
}

/// Extract module name from filename (without .glp extension).
String _moduleNameFromFilename(String filename) {
  if (filename.endsWith('.glp')) {
    return filename.substring(0, filename.length - 4);
  }
  return filename;
}

/// Build ancestor type scope from self.glp chain.
TypeEnvironment _buildAncestorScope(List<String> chain) {
  var env = buildPreludeEnvironment();

  for (final selfGlpPath in chain) {
    final source = File(selfGlpPath).readAsStringSync();
    final lexer = Lexer(source);
    final tokens = lexer.tokenize();
    final parser = Parser(tokens);
    final selfModule = parser.parseModule();

    final types = <String, TypeDef>{};
    for (final t in selfModule.typeDefs) {
      types[t.name] = t;
    }
    final procs = <String, ProcDecl>{};
    for (final p in selfModule.procDeclarations) {
      procs[p.qualifiedKey] = p;
    }
    env = env.merge(TypeEnvironment(types, procs));
  }

  return env;
}
