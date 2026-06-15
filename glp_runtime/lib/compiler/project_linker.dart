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
import 'package:path/path.dart' as ppath;
import 'ast.dart';
import 'lexer.dart';
import 'parser.dart';
import 'partial_evaluator.dart';
import '../analysis/type_checker/type_ast.dart';
import '../analysis/type_checker/param_expansion.dart';
import '../analysis/type_checker/type_checker.dart';
import '../analysis/type_checker/well_typed_clause.dart' show InstantiationCollector;
import '../runtime/module_hierarchy.dart';
import '../analysis/type_checker/type_environment_builder.dart';

/// A discovered module in the project tree.
class DiscoveredModule {
  final String filePath;
  final String moduleName;
  final Module ast;
  TypeEnvironment ancestorScope;
  final bool isSelfGlp;

  /// If this module was collected because an ancestor `self.glp` `-expose`d it,
  /// the normalized directory of that exposing `self.glp`. Its EXPORTED
  /// procedures lift into that directory's subtree scope. Null for ordinary
  /// modules.
  final String? exposingDir;

  DiscoveredModule({
    required this.filePath,
    required this.moduleName,
    required this.ast,
    required this.ancestorScope,
    this.isSelfGlp = false,
    this.exposingDir,
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

  // The root `programs/` directory bounds the ancestor scope chain. When known,
  // discovery extends above the project root up to (excluding) this directory.
  final programsDir = rootSelfGlpPath != null
      ? File(rootSelfGlpPath).parent.absolute.path
      : null;

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

    // Build ancestor scope chain (extends up to programs/ when known)
    final chain = discoverSelfChain(
      targetFile: file.absolute.path,
      rootDir: root.absolute.path,
      programsDir: programsDir,
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

  // Discover ancestor `self.glp` files ABOVE the project root, up to (but not
  // including) `programs/`. Ancestor directories contribute only their
  // `self.glp`, never their other modules (project-compilation spec §3.1). The
  // root `programs/self.glp` is excluded — it is realised by the root-scope
  // mechanism. These ancestor self.glp modules are linked like any other, so
  // their (multi-clause, parameterised) procedures resolve for descendants.
  if (programsDir != null) {
    for (final selfPath
        in _ancestorSelfGlpFiles(root.absolute.path, programsDir)) {
      final source = File(selfPath).readAsStringSync();
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final selfModule = parser.parseModule();

      final moduleName = selfModule.name ??
          _moduleNameFromDirPath(File(selfPath).parent.path);

      final chain = discoverSelfChain(
        targetFile: selfPath,
        rootDir: File(selfPath).parent.path,
        programsDir: programsDir,
      );
      final ancestorScope =
          _buildAncestorScope(chain, rootSelfGlpPath: rootSelfGlpPath);

      modules.add(DiscoveredModule(
        filePath: selfPath,
        moduleName: moduleName,
        ast: selfModule,
        ancestorScope: ancestorScope,
        isSelfGlp: true,
      ));
    }
  }

  // Root programs/self.glp is excluded from the linkable module list above, but
  // it is still a self.glp and may carry -expose directives (module-system spec
  // §3.3). Parse it as an exposer-only seed so its exposures are resolved like
  // any other self.glp's — its exposing directory is programs/, whose subtree is
  // every discovered module.
  final extraExposers = <DiscoveredModule>[];
  if (rootSelfGlpPath != null && File(rootSelfGlpPath).existsSync()) {
    final rootModule =
        Parser(Lexer(File(rootSelfGlpPath).readAsStringSync()).tokenize())
            .parseModule();
    if (rootModule.exposes.isNotEmpty) {
      extraExposers.add(DiscoveredModule(
        filePath: rootSelfGlpPath,
        moduleName: _moduleNameFromDirPath(File(rootSelfGlpPath).parent.path),
        ast: rootModule,
        ancestorScope: buildRootScopeEnvironment(),
        isSelfGlp: true,
      ));
    }
  }

  // Resolve `-expose(M).` directives: collect the named module files (tagged
  // with their exposing directory), check collisions, and lift their EXPORTED
  // declarations/types into the exposing subtree's scope.
  _resolveExposes(modules, programsDir, rootSelfGlpPath,
      extraExposers: extraExposers);

  return modules;
}

/// Normalize a path: absolute, `..`/`.` resolved, no trailing slash.
String _normPath(String p) {
  var n = ppath.normalize(Directory(p).absolute.path);
  if (n.length > 1 && n.endsWith(Platform.pathSeparator)) {
    n = n.substring(0, n.length - 1);
  }
  return n;
}

/// True if [childDir] is [ancestorDir] or below it.
bool _dirUnder(String childDir, String ancestorDir) =>
    childDir == ancestorDir ||
    childDir.startsWith('$ancestorDir${Platform.pathSeparator}');

/// Resolve `-expose` directives among [modules] (mutates the list).
///
/// For each exposing module, each `-expose(a#b#c).` names the module file
/// `<exposing self.glp dir>/a/b/c.glp`. That file is parsed, added as a linkable
/// module tagged with the exposing directory, and its `-expose` directives are
/// followed transitively. Two modules exposed at one level that share an
/// exported name/arity is a compile-time error. Finally, each exposed module's
/// EXPORTED declarations and the types it defines are merged into the
/// ancestorScope of every module in the exposing directory's subtree.
void _resolveExposes(List<DiscoveredModule> modules, String? programsDir,
    String? rootSelfGlpPath,
    {List<DiscoveredModule> extraExposers = const []}) {
  // [extraExposers] are self.glp files that carry -expose directives but are not
  // themselves linkable modules — specifically root programs/self.glp, which is
  // excluded from [modules] (realised by the root-scope mechanism) yet may
  // expose like any other self.glp (module-system spec §3.3; root "is not
  // otherwise special"). They seed the worklist but are never linked.
  final pending = <DiscoveredModule>[
    ...modules.where((m) => m.ast.exposes.isNotEmpty),
    ...extraExposers.where((m) => m.ast.exposes.isNotEmpty),
  ];
  final collectedFiles = <String>{};
  // exposingDir(norm) -> exported sig -> exposed module name (collision check)
  final perDirSig = <String, Map<String, String>>{};

  while (pending.isNotEmpty) {
    final exposer = pending.removeLast();
    final exposerDir = File(exposer.filePath).parent.path;
    final exposingDirNorm = _normPath(exposerDir);
    final sigMap = perDirSig.putIfAbsent(exposingDirNorm, () => {});

    for (final path in exposer.ast.exposes) {
      final rel = path.split('#').join(Platform.pathSeparator);
      final file = File('$exposerDir${Platform.pathSeparator}$rel.glp');
      if (!file.existsSync()) {
        throw Exception('-expose: module file not found: ${file.path}\n'
            '  from -expose($path) in ${exposer.filePath}');
      }

      final exposedAst =
          Parser(Lexer(file.readAsStringSync()).tokenize()).parseModule();
      final exposedName = exposedAst.name ??
          _moduleNameFromFilename(file.path.split(Platform.pathSeparator).last);

      // Collision: exported sigs unique among modules exposed at this level.
      for (final d in exposedAst.procDeclarations) {
        if (!d.exported) continue;
        final sig = '${d.name}/${d.arity}';
        final prev = sigMap[sig];
        if (prev != null && prev != exposedName) {
          throw Exception(
              '-expose collision at $exposingDirNorm: procedure $sig is '
              'exposed by both "$prev" and "$exposedName".');
        }
        sigMap[sig] = exposedName;
      }

      if (collectedFiles.contains(_normPath(file.path))) continue;
      collectedFiles.add(_normPath(file.path));

      final chain = discoverSelfChain(
        targetFile: file.absolute.path,
        rootDir: file.parent.path,
        programsDir: programsDir,
      );
      final exposedDM = DiscoveredModule(
        filePath: file.path,
        moduleName: exposedName,
        ast: exposedAst,
        ancestorScope:
            _buildAncestorScope(chain, rootSelfGlpPath: rootSelfGlpPath),
        isSelfGlp: false,
        exposingDir: exposingDirNorm,
      );
      modules.add(exposedDM);
      if (exposedAst.exposes.isNotEmpty) pending.add(exposedDM);
    }
  }

  // Type-env lift: merge exposed EXPORTED declarations/types into the scope of
  // every module in the exposing subtree.
  final exposed = modules.where((m) => m.exposingDir != null).toList();
  if (exposed.isEmpty) return;
  for (final m in modules) {
    if (m.exposingDir != null) continue;
    final modDir = _normPath(File(m.filePath).parent.path);
    for (final e in exposed) {
      if (!_dirUnder(modDir, e.exposingDir!)) continue;
      m.ancestorScope =
          _mergeExposed(m.ancestorScope, _exposedExportScope(e.ast, m.ancestorScope));
    }
  }
}

/// Merge an exposed module's [exposed] scope into [base] WITHOUT overriding any
/// name already present nearer the use site.  Innermost-first shadowing (spec
/// §3.2/§3.3: "a definition nearer the use site shadows an exposed one"):
/// exposed names only fill gaps.  A name defined nearer — whether as an ordinary
/// procedure or as a parameterized template — shadows an exposed entry of the
/// same key in BOTH maps, so a shadowed parameterized template is dropped
/// entirely and never drives call-site instantiation (Case B).  This is the
/// behaviour the platform routers rely on before the per-platform copies are
/// removed: the local monomorphic router shadows the exposed parameterised one.
TypeEnvironment _mergeExposed(TypeEnvironment base, TypeEnvironment exposed) {
  bool definedNearer(String key) =>
      base.procedures.containsKey(key) || base.paramProcDecls.containsKey(key);

  final procedures = <String, ProcDecl>{...base.procedures};
  for (final e in exposed.procedures.entries) {
    if (!definedNearer(e.key)) procedures[e.key] = e.value;
  }
  final paramProcDecls = <String, ProcDecl>{...base.paramProcDecls};
  for (final e in exposed.paramProcDecls.entries) {
    if (!definedNearer(e.key)) paramProcDecls[e.key] = e.value;
  }
  final types = <String, TypeDef>{...base.types};
  for (final e in exposed.types.entries) {
    types.putIfAbsent(e.key, () => e.value);
  }
  return TypeEnvironment(types, procedures,
      paramProcDecls: paramProcDecls,
      typeTemplates: {...base.typeTemplates, ...exposed.typeTemplates});
}

/// A TypeEnvironment of a module's EXPORTED procedure declarations plus the
/// types it defines, for type-checking exposed signatures in the subtree.
///
/// [base] supplies the exposing subtree's known type names and parameterised
/// templates (`Stream`, `Channel`, …), so the exposed signatures' parameterised
/// types are recognised and routed to `paramProcDecls` (exactly as an ordinary
/// ancestor `self.glp` would be processed).
TypeEnvironment _exposedExportScope(Module m, TypeEnvironment base) {
  final exported = m.procDeclarations.where((d) => d.exported).toList();
  final synthetic = Module(
    typeDefs: m.typeDefs,
    procDeclarations: exported,
    line: m.line,
    column: m.column,
  );
  final expanded = expandParameterizedTypes(synthetic,
      knownTypeNames: base.types.keys.toSet(),
      externalTemplates: base.typeTemplates);
  return buildScopeFromModule(expanded);
}

/// Collect `self.glp` files in ancestor directories ABOVE [rootDir], walking up
/// to but NOT including [programsDir]. Returns absolute paths, innermost-first.
List<String> _ancestorSelfGlpFiles(String rootDir, String programsDir) {
  // Normalize for comparison: absolute + resolve `..`/`.` (callers may pass
  // paths containing `..`) + strip trailing slash.
  String norm(String p) {
    var n = ppath.normalize(Directory(p).absolute.path);
    if (n.endsWith('/')) n = n.substring(0, n.length - 1);
    return n;
  }

  final programsNorm = norm(programsDir);
  final result = <String>[];
  var dir = Directory(rootDir).parent.absolute.path;

  while (true) {
    final dn = norm(dir);
    if (dn == programsNorm) break; // exclude programs/self.glp
    if (!dn.startsWith(programsNorm)) break; // above programs/ — stop
    final selfGlp = File('$dir${Platform.pathSeparator}self.glp');
    if (selfGlp.existsSync()) result.add(selfGlp.absolute.path);
    final parent = Directory(dir).parent.path;
    if (parent == dir) break; // filesystem root safety
    dir = parent;
  }

  return result;
}

/// Type-check a project.
///
/// Each module is checked against its own ancestor scope (scoping-correct,
/// collision-free — there is no merged environment). A parameterized procedure
/// is the one cross-module case: its declaration is a clause template, and per
/// the clause-template rule (docs/type system/typed-program.md; plan Step 1) its
/// defining clauses must be well-typed against each instantiation inferred from
/// a call site. Because that instantiation's monomorphic declaration carries its
/// concrete types from the caller's scope, the defining clauses are re-checked
/// there — never by flattening all modules into one environment.
///
/// Pass 1: per-module checks (verdicts for the non-parameterized case), while a
/// shared collector records every call-site instantiation and parameterized
/// procedures defer their wildcard self-check.
/// Phase 2: each distinct instantiation's defining clauses are checked against
/// its monomorphic declaration in the scope it was inferred in.
/// Fallback: a parameterized procedure with zero collected instantiations is
/// checked once under its wildcard declaration.
///
/// Throws on type errors with module name and error details.
void typeCheckProject(List<DiscoveredModule> modules) {
  final collector = InstantiationCollector();
  final deferred = <DeferredParamProc>[];

  // Pass 1: per-module checks + instantiation collection.
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
      collector: collector,
      deferredSink: deferred,
    );

    if (!result.isWellTyped) {
      final errors = result.errors
          .map((e) => '  ${e.message} at line ${e.line}')
          .join('\n');
      throw Exception(
          'Type checking failed for ${mod.moduleName} (${mod.filePath}):\n$errors');
    }
  }

  // Phase 2: per-instantiation defining-clause checking.
  final instantiatedKeys = <String>{};
  for (final inst in collector.all) {
    instantiatedKeys.add(inst.procKey);
    final clauses = _definingClausesForKey(modules, inst.procKey);
    if (clauses == null) continue; // defined outside the project (e.g. root scope)

    // Focused env: the caller scope where the instantiation was inferred (it
    // already holds the concrete types), with the procedure rebound to the
    // monomorphic declaration this instantiation produces.
    final focusedEnv = TypeEnvironment(
      inst.env.types,
      {...inst.env.procedures, inst.monoDecl.key: inst.monoDecl},
      paramProcDecls: inst.env.paramProcDecls,
      typeTemplates: inst.env.typeTemplates,
    );

    final res = TypeChecker(focusedEnv).checkSingleProcedure(inst.monoDecl, clauses);
    if (!res.isWellTyped) {
      final errors =
          res.errors.map((e) => '  ${e.message} at line ${e.line}').join('\n');
      throw Exception(
          'Type checking failed for ${inst.procKey} at instantiation '
          '${inst.monoDecl.argTypes.join(', ')}:\n$errors');
    }
  }

  // Fallback: parameterized procedures with no collected instantiation are
  // checked once under their wildcard declaration.
  final seenFallback = <String>{};
  for (final d in deferred) {
    if (instantiatedKeys.contains(d.procKey)) continue;
    if (!seenFallback.add(d.procKey)) continue;

    final focusedEnv = TypeEnvironment(
      d.env.types,
      {...d.env.procedures, d.wildcardDecl.key: d.wildcardDecl},
      paramProcDecls: d.env.paramProcDecls,
      typeTemplates: d.env.typeTemplates,
    );

    final res =
        TypeChecker(focusedEnv).checkSingleProcedure(d.wildcardDecl, d.clauses);
    if (!res.isWellTyped) {
      final errors =
          res.errors.map((e) => '  ${e.message} at line ${e.line}').join('\n');
      throw Exception(
          'Type checking failed for parameterized procedure ${d.procKey} '
          '(no in-scope instantiation; wildcard fallback):\n$errors');
    }
  }
}

/// Find the defining clauses for a procedure "name/arity" among the project's
/// modules. Returns null if the procedure is defined outside the project (e.g.
/// in the root scope), in which case it is not the project's responsibility to
/// check. If more than one module defines it, the clauses are concatenated.
List<Clause>? _definingClausesForKey(
    List<DiscoveredModule> modules, String procKey) {
  List<Clause>? found;
  for (final mod in modules) {
    for (final proc in mod.ast.procedures) {
      if ('${proc.name}/${proc.arity}' == procKey) {
        (found ??= <Clause>[]).addAll(proc.clauses);
      }
    }
  }
  return found;
}

/// Link all modules into a single flat Program.
///
/// Renames procedures (`p/n` → `M:p/n`), resolves all calls, and generates
/// entry-point aliases for the exported procedures of root-level modules
/// (project-compilation spec §3.4). [rootDir] is the loaded project root: a
/// module is "root-level" when its nearest enclosing `self.glp` directory is
/// that root, i.e. it is not contained in any descendant `self.glp` subtree.
///
/// Returns a [LinkResult] with the linked program and renamed proc declarations
/// (needed for SRSW type-based relaxation during compilation).
LinkResult linkProject(List<DiscoveredModule> modules, {required String rootDir}) {
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

    // Exposed procedures: a `self.glp` that `-expose`s a module lifts that
    // module's EXPORTED procedures into its subtree. Real ancestor `self.glp`
    // definitions (added above) and local definitions (checked first in
    // `_resolveGoal`) take precedence over exposed ones.
    final modDirNorm = _normPath(File(mod.filePath).parent.path);
    for (final em in modules) {
      if (em.exposingDir == null) continue;
      if (identical(em, mod)) continue;
      if (!_dirUnder(modDirNorm, em.exposingDir!)) continue;
      for (final d in em.ast.procDeclarations) {
        if (!d.exported) continue;
        procs.putIfAbsent('${d.name}/${d.arity}', () => em.moduleName);
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

  // Generate entry-point aliases (project-compilation spec §3.4).
  // Only EXPORTED procedures of ROOT-LEVEL modules receive unprefixed aliases.
  // A module is root-level when its nearest enclosing self.glp directory is the
  // loaded root itself — i.e. it is not contained in any descendant self.glp
  // subtree (a nested sub-project, e.g. secure/, village/). Nested-subtree
  // modules keep prefixed names only. There is no "top module".
  final rootNorm = _normPath(rootDir);
  final descendantSelfDirs = <String>{};
  for (final s in selfGlpModules) {
    final sDir = _normPath(File(s.filePath).parent.path);
    if (sDir != rootNorm && _dirUnder(sDir, rootNorm)) {
      descendantSelfDirs.add(sDir);
    }
  }
  bool isRootLevel(DiscoveredModule mod) {
    if (mod.exposingDir != null) return false; // exposed, not part of root surface
    final modDir = _normPath(File(mod.filePath).parent.path);
    if (!_dirUnder(modDir, rootNorm)) return false; // ancestor self.glp above root
    for (final s in descendantSelfDirs) {
      if (_dirUnder(modDir, s)) return false; // inside a nested sub-project
    }
    return true;
  }

  final aliasedSigs = <String, String>{}; // sig → owning module (conflict check)
  for (final mod in modules) {
    if (!isRootLevel(mod)) continue;
    for (final proc in mod.ast.procedures) {
      final isExported = mod.ast.procDeclarations.any(
          (d) => d.exported && d.name == proc.name && d.arity == proc.arity);
      if (!isExported) continue;

      final sig = '${proc.name}/${proc.arity}';
      final owner = aliasedSigs[sig];
      if (owner != null && owner != mod.moduleName) {
        throw Exception(
            'Entry-point conflict: procedure $sig is exported by both '
            'root-level modules "$owner" and "${mod.moduleName}".');
      }
      if (owner != null) continue;
      aliasedSigs[sig] = mod.moduleName;

      // Look up ProcDecl for mode-aware alias generation.
      // First check the owning module, then the project-wide index.
      final decl = _findProcDecl(mod, proc.name, proc.arity) ?? declIndex[sig];

      final aliasClause = _makeAliasClause(
        proc.name,
        proc.arity,
        '${mod.moduleName}:${proc.name}',
        declaration: decl,
      );
      allProcedures.add(Procedure(proc.name, proc.arity, [aliasClause], 0, 0));
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
/// Resolution order: local procedure → ancestor self.glp chain → root scope/stdlib.
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

  // Root scope/stdlib/body kernel — leave unchanged
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
  // Start from root scope (which includes root self.glp), matching
  // the individual-load path in assembleTypeScope.
  var env = buildRootScopeEnvironment();

  // Chain contains only project-local self.glp files.
  // Root self.glp is already included in the root scope environment.
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
