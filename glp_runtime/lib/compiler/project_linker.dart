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
import '../analysis/type_checker/param_expansion.dart';
import '../analysis/type_checker/type_checker.dart';
import '../runtime/module_hierarchy.dart';
import '../analysis/type_checker/type_environment_builder.dart';

/// A discovered module in the project tree.
class DiscoveredModule {
  final String filePath;
  final String moduleName;
  final Module ast;
  final TypeEnvironment ancestorScope;
  final bool isSelfGlp;

  DiscoveredModule({
    required this.filePath,
    required this.moduleName,
    required this.ast,
    required this.ancestorScope,
    this.isSelfGlp = false,
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
/// For each `.glp` file (excluding `boot_direct.glp`):
/// - Parse into Module AST
/// - Extract module name (from `-module(M).` or filename; for `self.glp` without
///   `-module()`, derives name from parent directory)
/// - Build ancestor type scope chain
///
/// `self.glp` files contribute both types AND procedures to the ancestor scope.
/// Their procedures are compiled to bytecode and renamed like any other module.
List<DiscoveredModule> discoverProject(String rootDir,
    {String? rootSelfGlpPath}) {
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

    // Skip boot_direct.glp (copy of boot.glp with direct calls, not a module)
    if (filename == 'boot_direct.glp') continue;

    // Skip mad_boot.glp and files in mad_boot/ directory
    // (madGLP boot procedures, loaded on top of linked project)
    if (filename == 'mad_boot.glp') continue;
    if (file.parent.path.endsWith('${Platform.pathSeparator}mad_boot') ||
        file.parent.path.endsWith('/mad_boot')) continue;

    // Parse the module
    final source = file.readAsStringSync();
    final lexer = Lexer(source);
    final tokens = lexer.tokenize();
    final parser = Parser(tokens);
    final module = parser.parseModule();

    // Extract module name: for self.glp without -module(), derive from parent dir
    final moduleName = module.name ??
        (filename == 'self.glp'
            ? _moduleNameFromDirPath(file.parent.path)
            : _moduleNameFromFilename(filename));

    // Build ancestor scope chain
    final chain = discoverSelfChain(
      targetFile: file.absolute.path,
      rootDir: root.absolute.path,
    );
    final ancestorScope =
        _buildAncestorScope(chain, rootSelfGlpPath: rootSelfGlpPath);

    modules.add(DiscoveredModule(
      filePath: file.path,
      moduleName: moduleName,
      ast: module,
      ancestorScope: ancestorScope,
      isSelfGlp: filename == 'self.glp',
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

  // Build ancestor self.glp procedure map for each module.
  // Maps module name → { sig → ancestorModuleName } (inner-most ancestor wins).
  final selfGlpModules = modules.where((m) => m.isSelfGlp).toList();
  final ancestorSelfProcs = <String, Map<String, String>>{};

  for (final mod in modules) {
    final modDir = File(mod.filePath).parent.absolute.path;
    final procs = <String, String>{}; // sig → ancestorModuleName

    // Walk self.glp modules from inner-most to outer-most.
    // Inner-most wins (first entry in putIfAbsent).
    // Sort by path length descending (longer path = more nested = inner).
    final ancestors = selfGlpModules
        .where((s) {
          if (identical(s, mod)) return false; // skip self
          final selfDir = File(s.filePath).parent.absolute.path;
          return modDir.startsWith(selfDir);
        })
        .toList()
      ..sort((a, b) => b.filePath.length.compareTo(a.filePath.length));

    for (final selfMod in ancestors) {
      for (final proc in selfMod.ast.procedures) {
        final sig = '${proc.name}/${proc.arity}';
        procs.putIfAbsent(sig, () => selfMod.moduleName);
      }
    }

    ancestorSelfProcs[mod.moduleName] = procs;
  }

  final allProcedures = <Procedure>[];

  // Process each module
  for (final mod in modules) {
    final localSigs = registry[mod.moduleName]!;
    final modAncestorProcs = ancestorSelfProcs[mod.moduleName] ?? {};

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
            ?.map((g) =>
                _resolveGoal(g, mod.moduleName, localSigs, modAncestorProcs))
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

  // Build a project-wide procedure declaration index for mode-aware aliases.
  // Maps 'name/arity' → ProcDecl, collecting from all modules' non-imported decls.
  final declIndex = <String, ProcDecl>{};
  for (final mod in modules) {
    for (final d in mod.ast.procDeclarations) {
      if (d.imported) continue;
      final sig = '${d.name}/${d.arity}';
      // First declaration wins (could also prefer exported, but any is fine)
      declIndex.putIfAbsent(sig, () => d);
    }
  }

  // Generate entry point aliases.
  // Top module: ALL procedures get aliases (for REPL invocation).
  // Other modules: only EXPORTED procedures get aliases (for cross-module calls
  // from code loaded on top, e.g., madGLP boot source calling agent/4).
  final aliasedSigs = <String, String>{}; // sig → owning module (for conflict detection)
  for (final mod in modules) {
    final isTop = mod.moduleName == topModuleName;
    for (final proc in mod.ast.procedures) {
      final sig = '${proc.name}/${proc.arity}';

      // Top module: alias all. Others: only exported.
      if (!isTop) {
        final isExported = mod.ast.procDeclarations
            .any((d) => d.exported && d.name == proc.name && d.arity == proc.arity);
        if (!isExported) continue;
      }

      // Skip if an alias already exists (top module wins)
      if (aliasedSigs.containsKey(sig)) continue;

      aliasedSigs[sig] = mod.moduleName;

      // Look up ProcDecl for mode-aware alias generation.
      // First check the owning module, then the project-wide index.
      final decl = _findProcDecl(mod, proc.name, proc.arity)
          ?? declIndex[sig];

      final aliasClause = _makeAliasClause(
        proc.name,
        proc.arity,
        '${mod.moduleName}:${proc.name}',
        declaration: decl,
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
///
/// Resolution order: local procedure → ancestor self.glp chain → prelude/stdlib.
Goal _resolveGoal(Goal goal, String moduleName, Set<String> localSigs,
    Map<String, String> ancestorSelfProcs) {
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
        _resolveGoal(goal.innerGoal, moduleName, localSigs, ancestorSelfProcs);
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

  // Check ancestor self.glp procedures
  final ancestorModule = ancestorSelfProcs[sig];
  if (ancestorModule != null) {
    return Goal(
      '$ancestorModule:${goal.functor}',
      goal.args,
      goal.line,
      goal.column,
    );
  }

  // Prelude/stdlib/body kernel — leave unchanged
  return goal;
}

/// Find the ProcDecl for a procedure in a module (non-imported only).
ProcDecl? _findProcDecl(DiscoveredModule mod, String name, int arity) {
  for (final d in mod.ast.procDeclarations) {
    if (!d.imported && d.name == name && d.arity == arity) return d;
  }
  return null;
}

/// Create an alias clause with mode-aware argument forwarding.
///
/// Given a procedure declaration, generates:
///   p(V0, V1, V2) :- M:p(V0?, V1, V2).
/// where input args (declared with ?) get reader annotation in the body,
/// and output args (no ?) get writer annotation (pass-through).
///
/// Without a declaration, falls back to all-reader body args:
///   p(V0, V1, V2) :- M:p(V0?, V1?, V2?).
Clause _makeAliasClause(String name, int arity, String targetName,
    {ProcDecl? declaration}) {
  if (arity == 0) {
    // Zero-arity: p :- M:p.
    final head = Atom(name, [], 0, 0);
    final body = [Goal(targetName, [], 0, 0)];
    return Clause(head, body: body, line: 0, column: 0);
  }

  // Generate variable names (V prefix — underscore prefix causes issues in codegen)
  final headArgs = List.generate(
      arity, (i) => VarTerm('V$i', false, 0, 0) as Term);

  // Body args: use mode from declaration if available.
  // Input args (T?) → isReader: true  (forward the value as reader)
  // Output args (T) → isReader: false (forward the writer so callee can write)
  final bodyArgs = List.generate(arity, (i) {
    final isInput = declaration != null && i < declaration.argTypes.length
        ? declaration.isInputArg(i)
        : true; // Fallback: assume input (reader) when no declaration
    return VarTerm('V$i', isInput, 0, 0) as Term;
  });

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

/// Extract module name from directory path (last component).
String _moduleNameFromDirPath(String dirPath) {
  final parts = dirPath.split(Platform.pathSeparator);
  return parts.last;
}

/// Build ancestor type scope from self.glp chain.
///
/// If [rootSelfGlpPath] is provided, it is prepended to the chain as the
/// outermost ancestor (the root self.glp, e.g. programs/self.glp).
///
/// Uses the same expansion pattern as assembleTypeScope in module_hierarchy.dart:
/// each self.glp's parameterized types are expanded before merging, and templates
/// are tracked for downstream modules.
TypeEnvironment _buildAncestorScope(List<String> chain,
    {String? rootSelfGlpPath}) {
  // Start from prelude (which includes root self.glp), matching
  // the individual-load path in assembleTypeScope.
  var env = buildPreludeEnvironment();

  // Chain contains only project-local self.glp files.
  // Root self.glp is already included in the prelude environment.
  for (final selfGlpPath in chain) {
    final source = File(selfGlpPath).readAsStringSync();
    final lexer = Lexer(source);
    final tokens = lexer.tokenize();
    final parser = Parser(tokens);
    final selfModule = parser.parseModule();

    // Extract templates before expansion removes them.
    final selfTemplates = <String, TypeDef>{};
    for (final td in selfModule.typeDefs) {
      if (td.isParameterized) {
        selfTemplates[td.name] = td;
      }
    }

    // Expand parameterized types before building scope.
    final expandedSelfModule = expandParameterizedTypes(selfModule,
        knownTypeNames: env.types.keys.toSet(),
        externalTemplates: env.typeTemplates);

    // Build environment from this self.glp (shadowing allowed).
    final selfEnv = buildScopeFromModule(expandedSelfModule);

    // Merge: later entries shadow earlier ones. Include templates for descendants.
    env = env.merge(TypeEnvironment(selfEnv.types, selfEnv.procedures,
        paramProcDecls: selfEnv.paramProcDecls,
        typeTemplates: selfTemplates));
  }

  return env;
}
