/// Program linker: static linking of multi-module GLP programs.
///
/// Given a program root directory, discovers all modules, type-checks each
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
import 'primitive_layer.dart';
import '../analysis/type_checker/type_ast.dart';
import '../analysis/type_checker/param_expansion.dart';
import '../analysis/type_checker/type_checker.dart';
import '../analysis/type_checker/type_identity.dart';
import '../runtime/module_hierarchy.dart';
import '../analysis/type_checker/type_environment_builder.dart';

/// A discovered module in the program tree.
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

/// Result of linking a program.
class LinkResult {
  final Program program;
  final List<ProcDecl> procDeclarations;

  LinkResult(this.program, this.procDeclarations);
}

/// Walk the program directory tree and discover all modules.
///
/// For each `.glp` file (excluding `boot_direct.glp`):
/// - Parse into Module AST
/// - Extract module name (from `-module(M).` or filename; for `self.glp` without
///   `-module()`, derives name from parent directory)
/// - Build ancestor type scope chain
///
/// `self.glp` files contribute both types AND procedures to the ancestor scope.
/// Their procedures are compiled to bytecode and renamed like any other module.
List<DiscoveredModule> discoverProgram(String rootDir,
    {String? rootSelfGlpPath}) {
  final root = Directory(rootDir);
  if (!root.existsSync()) {
    throw ArgumentError('Program root directory not found: $rootDir');
  }

  final modules = <DiscoveredModule>[];

  // The root `programs/` directory bounds the ancestor scope chain. When known,
  // discovery extends above the program root up to (excluding) this directory.
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
    // (madGLP boot procedures, loaded on top of linked program)
    if (filename == 'mad_boot.glp') continue;
    if (file.parent.path.endsWith('${Platform.pathSeparator}mad_boot') ||
        file.parent.path.endsWith('/mad_boot')) continue;

    // Parse the module
    final source = file.readAsStringSync();
    final lexer = Lexer(source);
    final tokens = lexer.tokenize();
    final parser = Parser(tokens);
    final module = parser.parseModule();

    // Enforce "Admission to the Primitive Layer" (Rule A / Rule B) at load time.
    enforcePrimitiveLayer(file.path, module, rootSelfGlpPath);

    // Module name is derived from the path: a self.glp takes its parent dir's
    // name, any other module its file name (-module removed).
    final moduleName = filename == 'self.glp'
        ? _moduleNameFromDirPath(file.parent.path)
        : _moduleNameFromFilename(filename);

    // Build ancestor scope chain (extends up to programs/ when known)
    final chain = discoverSelfChain(
      targetFile: file.absolute.path,
      rootDir: root.absolute.path,
      programsDir: programsDir,
    );
    final ancestorScope =
        buildAncestorScope(chain: chain, rootSelfGlpPath: rootSelfGlpPath);

    modules.add(DiscoveredModule(
      filePath: file.path,
      moduleName: moduleName,
      ast: module,
      ancestorScope: ancestorScope,
      isSelfGlp: filename == 'self.glp',
    ));
  }

  // Add the program's filesystem context (ancestor self.glp above the root) and
  // resolve -expose directives.
  _addAncestorContextAndExposes(
      modules, root.absolute.path, programsDir, rootSelfGlpPath);
  return modules;
}

/// Discover a single self-contained module as a one-module program (modules.tex
/// §Design: "A Typed GLP program is either a self-contained module or a
/// directory with a self.glp module"). The module is the program's only own
/// module — it has no self.glp of its own, so every one of its procedures is an
/// entry point (§Static Linking). Its filesystem context (ancestor self.glp
/// above its directory, up to programs/) is added, so it links and runs through
/// the same pipeline as a directory program.
List<DiscoveredModule> discoverSingleModule(String filePath,
    {String? rootSelfGlpPath}) {
  final file = File(filePath);
  if (!file.existsSync()) {
    throw ArgumentError('Module file not found: $filePath');
  }
  final programsDir = rootSelfGlpPath != null
      ? File(rootSelfGlpPath).parent.absolute.path
      : null;

  final module =
      Parser(Lexer(file.readAsStringSync()).tokenize()).parseModule();
  enforcePrimitiveLayer(file.path, module, rootSelfGlpPath);

  final dir = file.parent.absolute.path;
  final chain = discoverSelfChain(
      targetFile: file.absolute.path, rootDir: dir, programsDir: programsDir);

  final modules = <DiscoveredModule>[
    DiscoveredModule(
      filePath: file.path,
      moduleName: _moduleNameFromFilename(
          file.path.split(Platform.pathSeparator).last),
      ast: module,
      ancestorScope:
          buildAncestorScope(chain: chain, rootSelfGlpPath: rootSelfGlpPath),
      isSelfGlp: false,
    ),
  ];

  // The module's own-directory self.glp is its nearest scope: it is in scope for
  // type checking, so it must also be linked, or its procedures are unresolved
  // at runtime. (The ancestor self.glp ABOVE the directory are added below.)
  final ownSelf = File('$dir${Platform.pathSeparator}self.glp');
  if (file.path.split(Platform.pathSeparator).last != 'self.glp' &&
      ownSelf.existsSync()) {
    final selfModule =
        Parser(Lexer(ownSelf.readAsStringSync()).tokenize()).parseModule();
    final selfChain = discoverSelfChain(
        targetFile: ownSelf.absolute.path, rootDir: dir, programsDir: programsDir);
    modules.add(DiscoveredModule(
      filePath: ownSelf.path,
      moduleName: _moduleNameFromDirPath(dir),
      ast: selfModule,
      ancestorScope:
          buildAncestorScope(chain: selfChain, rootSelfGlpPath: rootSelfGlpPath),
      isSelfGlp: true,
    ));
  }

  _addAncestorContextAndExposes(modules, dir, programsDir, rootSelfGlpPath);
  return modules;
}

/// Add a program's filesystem context to [modules]: the ancestor `self.glp`
/// files ABOVE [rootAbsPath] (up to but excluding `programs/`), linked like any
/// other module so their (multi-clause, parameterised) procedures resolve for
/// descendants; then resolve `-expose` directives (including the root
/// `programs/self.glp`'s, which is itself realised by the root-scope mechanism).
void _addAncestorContextAndExposes(List<DiscoveredModule> modules,
    String rootAbsPath, String? programsDir, String? rootSelfGlpPath) {
  if (programsDir != null) {
    for (final selfPath in _ancestorSelfGlpFiles(rootAbsPath, programsDir)) {
      final selfModule =
          Parser(Lexer(File(selfPath).readAsStringSync()).tokenize())
              .parseModule();
      final chain = discoverSelfChain(
        targetFile: selfPath,
        rootDir: File(selfPath).parent.path,
        programsDir: programsDir,
      );
      modules.add(DiscoveredModule(
        filePath: selfPath,
        moduleName: _moduleNameFromDirPath(File(selfPath).parent.path),
        ast: selfModule,
        ancestorScope:
            buildAncestorScope(chain: chain, rootSelfGlpPath: rootSelfGlpPath),
        isSelfGlp: true,
      ));
    }
  }

  // Root programs/self.glp is excluded from the linkable module list, but it is
  // still a self.glp and may carry -expose directives (module-system spec §3.3).
  // Parse it as an exposer-only seed so its exposures resolve like any other
  // self.glp's — its exposing directory is programs/, whose subtree is every
  // discovered module.
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

  _resolveExposes(modules, programsDir, rootSelfGlpPath,
      extraExposers: extraExposers);
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
      final exposedName =
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
            buildAncestorScope(chain: chain, rootSelfGlpPath: rootSelfGlpPath),
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

/// Step 2 of static linking (modules.tex §Static Linking): "each module is
/// type-checked independently against its ancestor scope, exactly as for
/// single-file compilation". It runs after discovery (step 1) and before
/// renaming (step 3), so every error names the module's own file and line, and
/// it covers every discovered module — including one that no entry point
/// reaches, which step 5 (dead-code elimination) drops before the linked check
/// ever sees it. The linked check that follows is an addition to this one, not
/// a replacement: it is more stringent where a call supplies concrete types
/// across a `#` boundary, and blind where a module is unreachable.
///
/// Two points the per-module check settles, both as the paper puts them:
///
/// - A parameterised procedure with no instantiation in its own module is not
///   rejected here — [checkModule] is called with
///   `rejectUninstantiatedInspecting: false`. A procedure that never inspects a
///   parameter is certified once for all instantiations by the abstract-
///   parameter route (parameterized-types.tex §Modular Checking via Abstract
///   Parameters), which [checkModule] runs regardless; one that does inspect a
///   parameter has no well-typing of its own and acquires one only per
///   instantiation, which the linked check supplies.
/// - Defined guards are unfolded per module before checking, as on the
///   single-file path (`GlpEngine.loadSource`): guard unfolding precedes type
///   checking, so input coverage is checked on the unfolded head.
///
/// A module with no procedure declarations is not checked, matching the
/// single-file path — a `self.glp` that carries only type definitions has
/// nothing to check.
///
/// Throws on type errors, naming each offending module's file path.
void checkModulesIndependently(List<DiscoveredModule> modules) {
  final failures = <String>[];

  for (final mod in modules) {
    if (mod.ast.procDeclarations.isEmpty) continue;

    final pe = PartialEvaluator();
    final transformed = pe.transformDefinedGuards(
        Program(mod.ast.procedures, mod.ast.line, mod.ast.column));

    final result = checkModule(
      mod.ast,
      transformedProcedures: transformed.procedures,
      ancestorScope: mod.ancestorScope,
      rejectUninstantiatedInspecting: false,
    );
    if (result.isWellTyped) continue;

    for (final e in result.errors) {
      failures.add('  ${mod.filePath}:${e.line}: ${e.message}');
    }
  }

  if (failures.isNotEmpty) {
    throw Exception('Type checking failed for module(s) of the program:\n'
        '${failures.join('\n')}');
  }
}

/// Type-check a program on its LINKED program (paper: modules §Module-System
/// Design "Self-contained type checking", §Static Linking; def:program —
/// soundness is established on the linked program). Linking renames every
/// procedure to `M:p` and resolves every call (a cross-module `M' # p` becomes a
/// local `M':p`); the whole program is then one program in which a cross-module
/// call is an ordinary local call. The instantiation closure (§Parameterised
/// Procedure Declarations) therefore induces and checks a parameterised callee's
/// clauses at every instantiation a call supplies — in both directions and
/// through parametric intermediaries — which a per-module check, stopping at the
/// `#` boundary, does not. Renaming makes procedure names unambiguous across
/// modules and type identity is structural, so no merged-environment juggling is
/// needed. A parameterised procedure with no instantiation goes unchecked, not
/// rejected (typed-program.md "Programs and Modules").
///
/// This is the SECOND of the two checks the paper specifies. Step 2 — each
/// module against its ancestor scope — runs first, in
/// [checkModulesIndependently]; a module no entry point reaches is checked
/// there and nowhere else, since step-5 dead-code elimination drops it before
/// the linked check.
///
/// Throws on type errors with details.
LinkResult checkedLinkedProgram(List<DiscoveredModule> modules,
    {required String rootDir, bool rejectUninstantiated = false}) {
  // Step 2 (modules.tex §Static Linking): after discovery, before renaming,
  // each module is type-checked independently against its ancestor scope. The
  // linked check below is an addition to it, not a replacement.
  checkModulesIndependently(modules);

  // Soundness is established on the LINKED program (paper: modules §Module-System
  // Design "Self-contained type checking", §Static Linking; def:program). We link
  // first — renaming every procedure to `M:p` and resolving every call, including
  // each cross-module `M' # p` to a local `M':p` — then type-check the single
  // linked program. In it a cross-module call is an ordinary local call, so the
  // instantiation closure (§Parameterised Procedure Declarations) induces and
  // checks the callee's clauses at every instantiation the call supplies — in both
  // directions and through parametric intermediaries. Renaming makes every
  // procedure name unambiguous across modules, and type identity is structural, so
  // no per-module environment juggling is needed.
  // linkProgram applies all five steps, including step-5 DCE, so the program
  // type-checked and compiled below is restricted to its reachable procedures.
  final linked = linkProgram(modules, rootDir: rootDir);
  final flat = linkedFlatModule(modules, linked);

  final pe = PartialEvaluator();
  final transformed = pe.transformDefinedGuards(linked.program);

  // rejectUninstantiatedInspecting: false — at load time the program's concrete
  // initial goals (def:program) are not yet known, and they are what instantiate
  // the parametric entry procedures and routers. A parametric procedure left
  // uninstantiated here is bound by the goal at run; rejecting it at load would
  // refuse every program whose routers are instantiated only through its goals.
  // The free-type-parameter (no-linked-program) check belongs where the goal
  // completes the program, not here.
  final result = checkModule(
    flat,
    transformedProcedures: transformed.procedures,
    rejectUninstantiatedInspecting: rejectUninstantiated,
  );

  if (!result.isWellTyped) {
    final errors = result.errors
        .map((e) => '  ${e.message} at line ${e.line}')
        .join('\n');
    throw Exception('Type checking failed for linked program:\n$errors');
  }

  // What the verdict above does NOT cover. A parameterised procedure that
  // inspects a type parameter and that no call in this program instantiates is
  // checked by nothing (parameterized-types.tex sec:programs-and-modules), and
  // until 2026-08-03 a program said so nowhere: it printed a clean verdict and
  // the unchecked clauses were indistinguishable from checked ones. That is how
  // typed_actors.glp carried an untagged value at a tagged-union position for
  // months. One line, naming them, so the gap is visible at the point the
  // program is pronounced well-typed rather than only to whoever reads the
  // checker.
  final unchecked = [
    for (final w in result.warnings)
      if (w.procedure != null) w.procedure!
  ]..sort();
  if (unchecked.isNotEmpty) {
    print('[TYPE] ${unchecked.length} parameterized procedure(s) unchecked in '
        'this program — no instantiation: ${unchecked.join(', ')}');
  }

  return linked;
}

/// The single flat Module the linked program is type-checked and compiled as:
/// the linked program's procedures, every module's own type definitions, and the
/// linked declarations.
///
/// The type definitions are the union of every module's own, deduplicated by
/// name (structural identity makes duplicates the same type). Root-scope types
/// are supplied by the root scope, not here.
///
/// A module may redefine a root-scope operation (e.g. send/receive/new_channel/
/// merge) with local clauses but no local declaration, relying on the root
/// declaration. Linking renames those clauses to `M:p` while the root
/// declaration stays bare, leaving the renamed procedure undeclared in the
/// linked program. A renamed declaration is supplied from the root scope so the
/// procedure is checked. (An unqualified entry-point alias carries its exporting
/// module's declaration from linking — shadowing a root-scope declaration of the
/// same name/arity; only an alias whose export has no declaration is left
/// undeclared.)
Module linkedFlatModule(List<DiscoveredModule> modules, LinkResult linked) {
  final typeDefs = <String, TypeDef>{};
  for (final mod in modules) {
    for (final td in mod.ast.typeDefs) {
      typeDefs.putIfAbsent(td.name, () => td);
    }
  }

  final rootEnv = buildRootScopeEnvironment();
  final procDecls = [...linked.procDeclarations];
  final declKeys = {for (final d in procDecls) d.key};
  for (final p in linked.program.procedures) {
    final key = '${p.name}/${p.arity}';
    if (declKeys.contains(key)) continue;
    final colon = p.name.lastIndexOf(':');
    if (colon < 0) continue; // unqualified entry-point alias
    final bareKey = '${p.name.substring(colon + 1)}/${p.arity}';
    // Prefer the parametric template over the wildcard-instantiated version in
    // `procedures`: a redefined root op (send/receive/new_channel/merge) is
    // parametric, and the renamed copy must carry the template so call-site
    // inference (Case B) concretises it rather than leaving wildcard types.
    final rd = rootEnv.paramProcDecls[bareKey] ?? rootEnv.procedures[bareKey];
    if (rd != null) {
      procDecls.add(ProcDecl(p.name, rd.argTypes, rd.line, rd.column,
          exported: rd.exported, isBuiltin: rd.isBuiltin));
      declKeys.add(key);
    }
  }

  return Module(
    typeDefs: typeDefs.values.toList(),
    procDeclarations: procDecls,
    procedures: linked.program.procedures,
    line: 0,
    column: 0,
  );
}

/// The type-identity tables of a linked program (modules.tex §Dynamic
/// Activation): declared `p/n` → identity, which `find_type/2` reads, and
/// exported `p/n` → identity, the table a `Module` value carries.
///
/// Built on demand from the same flat module the program was type-checked as,
/// so the automata are the checked program's. Nothing on the load path calls
/// this: the kernels that consume the tables (`'_find_type'`, `'_run'`/3) are
/// step 2 of `/Grassroots/docs/typed-dynamic-activation-plan.md` and are IGLP's.
TypeIdentityTables linkedTypeIdentityTables(
        List<DiscoveredModule> modules, LinkResult linked) =>
    typeIdentityTablesForModule(linkedFlatModule(modules, linked));

/// Whole-program type-check gate (paper: modules §Static Linking — "the unit of
/// compilation and execution is a program ... only a well-typed program is
/// compiled and run"). Throws unless the linked program is well-typed. For
/// callers that need only the verdict (e.g. a single-file gate that compiles the
/// source unrenamed); callers that compile the linked program use
/// [checkedLinkedProgram].
void typeCheckProgram(List<DiscoveredModule> modules, {required String rootDir}) {
  checkedLinkedProgram(modules, rootDir: rootDir);
}

/// Static linking of all modules into a single flat Program (modules.tex
/// sec:static-linking, all five steps).
///
/// Steps 1–4 ([linkAndResolveModules]) rename procedures (`p/n` → `M:p/n`),
/// resolve all calls, and generate entry-point aliases for root-level exports;
/// step 5 ([eliminateDeadCode]) restricts the result to the reachable
/// procedures. This is the program of def:program that is type-checked and
/// compiled.
///
/// Between steps 4 and 5, a directory with no entry points is rejected
/// ([_requireEntryPoints]).
LinkResult linkProgram(List<DiscoveredModule> modules,
    {required String rootDir, String? singleModulePath}) {
  final linked = linkAndResolveModules(modules,
      rootDir: rootDir, singleModulePath: singleModulePath);
  if (singleModulePath == null) {
    _requireEntryPoints(modules, linked, rootDir);
  }
  return eliminateDeadCode(linked);
}

/// A directory with no entry points is not a program (modules.tex §Static
/// Linking, "Entry and the absence of a boot module"): "A root `self.glp` that
/// exports no procedure therefore gives a program with no entry points, which
/// the fifth step restricts to the empty set of procedures. No initial goal
/// resolves against it, so it is not a program in the sense of def:program, and
/// the loader rejects it rather than linking it and reporting success."
///
/// The entry points are the bare (unprefixed) procedures step 4 generated: a
/// directory's are the aliases of its root `self.glp`'s exports, by definition
/// or by forwarding (§External access). Every other procedure carries a renamed
/// `M:p`, so an empty bare set is an empty entry-point set, which is what step 5
/// would restrict the program to. Rejecting here rather than after step 5 is
/// what the paper asks for: the loader rejects it rather than linking it.
///
/// Two things this is NOT. It is not the reachability check — a program with one
/// entry point that reaches nothing else is a program, and step 5 keeps it. And
/// it does not apply to a single module: a single-module program has no
/// `self.glp`, "exports all its procedures, so every one is an entry point"
/// (§Static Linking), and the linker keeps them bare rather than aliasing them,
/// so [linkProgram] tests it only on the directory path.
///
/// An `-expose`d procedure is not an entry point either (§`-expose`: it "is not
/// thereby exported by the root `self.glp`, so it is an entry point only if the
/// root `self.glp` exports it in its own right"), so a root `self.glp` whose
/// only exports are exposed ones is rejected here as well.
void _requireEntryPoints(
    List<DiscoveredModule> modules, LinkResult linked, String rootDir) {
  final hasEntryPoint =
      linked.program.procedures.any((p) => !p.name.contains(':'));
  if (hasEntryPoint) return;

  final rootNorm = _normPath(rootDir);
  final rootSelfPaths = modules
      .where((m) =>
          m.isSelfGlp && _normPath(File(m.filePath).parent.path) == rootNorm)
      .map((m) => m.filePath)
      .toList();

  final cause = rootSelfPaths.isEmpty
      ? 'it has no root self.glp, and no module at its root exports a procedure'
      : '${rootSelfPaths.first} exports no procedure';

  throw Exception(
      'Not a program: $rootDir has no entry points — $cause. A procedure is an '
      'entry point exactly when the root self.glp exports it, by declaring it '
      'exported and either defining it or forwarding it to the module that '
      'does (modules.tex §External access); an exposed procedure is not one. '
      'With no entry point no initial goal resolves against the directory, so '
      'it is not a program by def:program and is rejected rather than linked.');
}

/// Steps 1–4 of static linking: the pure rename-and-resolve transform, without
/// dead-code elimination.
///
/// Renames procedures (`p/n` → `M:p/n`), resolves all calls, and generates
/// entry-point aliases for the exported procedures of root-level modules
/// (project-compilation spec §3.4). [rootDir] is the loaded program root: a
/// module is "root-level" when its nearest enclosing `self.glp` directory is
/// that root, i.e. it is not contained in any descendant `self.glp` subtree.
///
/// Returns a [LinkResult] with the renamed program and renamed proc declarations
/// (needed for SRSW type-based relaxation during compilation). This is the stage
/// to inspect when checking renaming/resolution/aliasing in isolation; the
/// program actually compiled is [linkProgram] (which also applies step 5).
LinkResult linkAndResolveModules(List<DiscoveredModule> modules,
    {required String rootDir, String? singleModulePath}) {
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

  // The loaded module of a single-module program keeps its procedures under
  // their bare names: they are the program's entry points, "called by plain
  // name by a goal posted at the root" (modules.tex §Static Linking), so the
  // bare name is a plain-name handle on the real head — no forwarder clause,
  // whose flat arguments cannot carry a structured-mode term's nested holes.
  // Every SCOPE module (ancestor self.glp, own-dir self.glp, exposed module) is
  // still renamed to M:p, so its internal calls resolve to its OWN procedures
  // and the bare loaded module never hijacks an ancestor's same-named call.
  final singleNorm =
      singleModulePath != null ? _normPath(singleModulePath) : null;

  // Process each module
  for (final mod in modules) {
    final localSigs = registry[mod.moduleName]!;
    final modAncestorProcs = ancestorSelfProcs[mod.moduleName] ?? {};
    final keepBare =
        singleNorm != null && _normPath(mod.filePath) == singleNorm;

    for (final proc in mod.ast.procedures) {
      // Step 3 (modules.tex §Static Linking): rename every procedure p/n to
      // M:p/n, eliminating name collisions — except the loaded module's own
      // procedures, kept bare as the program's plain-name entry points.
      final renamedName = keepBare ? proc.name : '${mod.moduleName}:${proc.name}';
      final renamedClauses = <Clause>[];

      for (final clause in proc.clauses) {
        final renamedHead = keepBare
            ? clause.head
            : Atom('${mod.moduleName}:${clause.head.functor}',
                clause.head.args, clause.head.line, clause.head.column);

        // Step 4: resolve every body call in this module's scope — local → p
        // becomes M:p (bare in the loaded module), ancestor self.glp →
        // ancestor:p, static cross-module M' # p → M':p (a local Spawn, not a
        // Distribute; manual §19.7).
        final resolvedBody = clause.body
            ?.map((g) => _resolveGoal(
                g, mod.moduleName, localSigs, modAncestorProcs,
                keepLocalBare: keepBare))
            .toList();

        // A defined guard calls a user unit-clause procedure, which step 3
        // renames to M:g. Resolve the guard call in the same scope so it points
        // at the renamed unit clause; the partial evaluator unfolds it by that
        // name. Builtin guards and root-scope guards match no module procedure
        // and stay bare (root unit clauses are collected unrenamed).
        final resolvedGuards = clause.guards
            ?.map((g) => _resolveGuard(
                g, mod.moduleName, localSigs, modAncestorProcs,
                keepLocalBare: keepBare))
            .toList();

        renamedClauses.add(Clause(
          renamedHead,
          guards: resolvedGuards,
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

  // Build a program-wide procedure declaration index for mode-aware aliases.
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

  // Generate entry-point aliases (modules.tex sec:static-linking step 5,
  // §External access). For a DIRECTORY program, the entry points are the
  // EXPORTED procedures of the ROOT self.glp — the self.glp at the loaded
  // program root — each given an unqualified forwarding alias so an external
  // goal calls it by plain name. (A directory with no root self.glp falls back
  // to its root-level modules' exported procedures.)
  //
  // A SINGLE-MODULE program generates NO aliases: its own procedures are kept
  // bare above (keepBare), and those bare names ARE the entry points, a
  // plain-name handle directly on each real head. A forwarding alias is avoided
  // on purpose — its flat arguments cannot carry a structured-mode term's nested
  // reader/writer holes.
  final aliasDecls = <ProcDecl>[];
  if (singleNorm == null) {
    final rootNorm = _normPath(rootDir);
    final rootSelfMods = modules
        .where((m) =>
            m.isSelfGlp && _normPath(File(m.filePath).parent.path) == rootNorm)
        .toList();

    Iterable<DiscoveredModule> aliasSourceModules;
    if (rootSelfMods.isNotEmpty) {
      aliasSourceModules = rootSelfMods;
    } else {
      final descendantSelfDirs = <String>{};
      for (final s in selfGlpModules) {
        final sDir = _normPath(File(s.filePath).parent.path);
        if (sDir != rootNorm && _dirUnder(sDir, rootNorm)) {
          descendantSelfDirs.add(sDir);
        }
      }
      bool isRootLevel(DiscoveredModule mod) {
        if (mod.exposingDir != null) return false; // exposed, not root surface
        final modDir = _normPath(File(mod.filePath).parent.path);
        if (!_dirUnder(modDir, rootNorm)) return false; // ancestor above root
        for (final s in descendantSelfDirs) {
          if (_dirUnder(modDir, s)) return false; // inside a nested sub-program
        }
        return true;
      }
      aliasSourceModules = modules.where(isRootLevel);
    }

    final aliasedSigs = <String, String>{}; // sig → owning module (conflict check)
    for (final mod in aliasSourceModules) {
      for (final proc in mod.ast.procedures) {
        final isExported = mod.ast.procDeclarations.any(
            (d) => d.exported && d.name == proc.name && d.arity == proc.arity);
        if (!isExported) continue;

        final sig = '${proc.name}/${proc.arity}';
        final owner = aliasedSigs[sig];
        if (owner != null && owner != mod.moduleName) {
          throw Exception(
              'Entry-point conflict: procedure $sig is exported by both '
              '"$owner" and "${mod.moduleName}".');
        }
        if (owner != null) continue;
        aliasedSigs[sig] = mod.moduleName;

        // Look up ProcDecl for mode-aware alias generation.
        // First check the owning module, then the program-wide index.
        final decl = _findProcDecl(mod, proc.name, proc.arity) ?? declIndex[sig];

        // The alias carries the exporting declaration under its bare name
        // (collected into the returned declarations below), so the linked-
        // program check uses the exporting module's declaration — shadowing a
        // root-scope declaration of the same name/arity (e.g. the root
        // self.glp's run/2), exactly as the module's own declaration shadows
        // it in the per-module check.
        if (decl != null) {
          aliasDecls.add(ProcDecl(proc.name, decl.argTypes, decl.line,
              decl.column, exported: decl.exported, isBuiltin: decl.isBuiltin));
        }

        final aliasClause = _makeAliasClause(
          proc.name,
          proc.arity,
          '${mod.moduleName}:${proc.name}',
          declaration: decl,
        );
        allProcedures.add(Procedure(proc.name, proc.arity, [aliasClause], 0, 0));
      }
    }
  }

  // Collect and rename proc declarations for SRSW relaxation. The loaded
  // module's declarations stay bare, matching its bare procedures.
  //
  // A kept-bare declaration is an entry point of the program and carries the
  // exported flag, whatever the source wrote: "a single-module program, having
  // no self.glp, exports all its procedures, so every one is an entry point"
  // (modules.tex sec:static-linking). A renamed `M:p` declaration is not an
  // entry point — an external goal cannot name it — so it carries the flag not
  // at all, and a directory program's entry points come from `aliasDecls`
  // below. This is what the exported type-identity table is built over
  // (analysis/type_checker/type_identity.dart), and it is the same set the
  // artefact's interface table records.
  final allDecls = <ProcDecl>[];
  for (final mod in modules) {
    final keepBare =
        singleNorm != null && _normPath(mod.filePath) == singleNorm;
    for (final decl in mod.ast.procDeclarations) {
      if (decl.imported) continue; // Skip imported — they're in other modules
      allDecls.add(ProcDecl(
        keepBare ? decl.name : '${mod.moduleName}:${decl.name}',
        decl.argTypes,
        decl.line,
        decl.column,
        isBuiltin: decl.isBuiltin,
        exported: keepBare,
      ));
    }
  }

  allDecls.addAll(aliasDecls);

  return LinkResult(Program(allProcedures, 0, 0), allDecls);
}

/// Dead-code elimination: the linker's step 5 (modules.tex sec:static-linking).
///
/// Returns the linked program restricted to its \emph{reachable} procedures:
/// the root's exported procedures (the entry-point aliases — the bare,
/// unprefixed procedures the linker generated — and the renamed procedures they
/// call) and the transitive closure of procedures called in the body of a
/// reachable one. Guards are followed too: a defined guard's call site is
/// renamed to `M:g` in step with its procedure (so the partial evaluator unfolds
/// it after linking), and a guard left bare is also followed by base name as a
/// safeguard. Restricting the program to its reachable procedures is
/// semantically equivalent to the whole; everything else is pruned.
/// The reachability seed is the bare (unprefixed) entry-point aliases the linker
/// generated: a directory's are the root self.glp's exported procedures, a
/// single module's are every one of its own procedures. Every other procedure
/// carries a renamed `M:p` name, so the unprefixed aliases are exactly the
/// entry points.
LinkResult eliminateDeadCode(LinkResult linked) {
  final procedures = linked.program.procedures;

  final byFullName = <String, Procedure>{};
  for (final p in procedures) {
    byFullName['${p.name}/${p.arity}'] = p;
  }
  // base 'name/arity' → full keys, for resolving unqualified guard call sites.
  final byBaseName = <String, List<String>>{};
  for (final fk in byFullName.keys) {
    final p = byFullName[fk]!;
    final ci = p.name.lastIndexOf(':');
    final base = ci < 0 ? p.name : p.name.substring(ci + 1);
    byBaseName.putIfAbsent('$base/${p.arity}', () => <String>[]).add(fk);
  }

  final reachable = <String>{};
  final work = <String>[];
  void markFull(String key) {
    if (byFullName.containsKey(key) && reachable.add(key)) work.add(key);
  }
  void markBase(String baseKey) {
    for (final fk in byBaseName[baseKey] ?? const <String>[]) {
      if (reachable.add(fk)) work.add(fk);
    }
  }

  void collectFromGoal(Goal g) {
    if (g is RemoteGoal) {
      // A static M' # p was already rewritten to a Goal by _resolveGoal; a
      // residual RemoteGoal is dynamic — record its target by base name.
      markBase('${g.goal.functor}/${g.goal.arity}');
      return;
    }
    if (g is SpawnGoal) {
      collectFromGoal(g.innerGoal);
      return;
    }
    // Body calls carry resolved names: M:p for local/ancestor procedures (exact
    // match keeps the target), unqualified for root-scope calls (no procedure
    // here — left to the separately merged root self.glp).
    markFull('${g.functor}/${g.arity}');
  }

  // Seed: the bare (unprefixed) entry-point aliases the linker generated.
  for (final p in procedures) {
    if (!p.name.contains(':')) markFull('${p.name}/${p.arity}');
  }
  while (work.isNotEmpty) {
    final proc = byFullName[work.removeLast()]!;
    for (final clause in proc.clauses) {
      for (final g in clause.body ?? const <Goal>[]) {
        collectFromGoal(g);
      }
      for (final gd in clause.guards ?? const <Guard>[]) {
        // A defined guard now carries its resolved name (M:g) — keep it exactly;
        // markBase additionally covers any guard left bare that names a renamed
        // procedure by base name (defensive; never under-keeps).
        markFull('${gd.predicate}/${gd.args.length}');
        markBase('${gd.predicate}/${gd.args.length}');
      }
    }
  }

  final keptProcedures = procedures
      .where((p) => reachable.contains('${p.name}/${p.arity}'))
      .toList();
  final keptDecls = linked.procDeclarations
      .where((d) => reachable.contains('${d.name}/${d.arity}'))
      .toList();

  return LinkResult(Program(keptProcedures, 0, 0), keptDecls);
}

/// Resolve a defined-guard call in a clause's guard list, mirroring
/// [_resolveGoal]'s scope order: a guard `g/n` that names a local or ancestor
/// `self.glp` unit-clause procedure is renamed to `M:g/n` so it matches the
/// renamed procedure; a builtin guard or a root-scope guard (no matching module
/// procedure) is left bare. Guards never cross module boundaries (no `M' # g`),
/// so there is no remote case.
Guard _resolveGuard(Guard guard, String moduleName, Set<String> localSigs,
    Map<String, String> ancestorSelfProcs,
    {bool keepLocalBare = false}) {
  final sig = '${guard.predicate}/${guard.args.length}';
  if (localSigs.contains(sig)) {
    // Loaded module keeps bare names: a local guard stays bare.
    if (keepLocalBare) return guard;
    return Guard('$moduleName:${guard.predicate}', guard.args,
        guard.line, guard.column,
        negated: guard.negated);
  }
  final ancestorModule = ancestorSelfProcs[sig];
  if (ancestorModule != null) {
    return Guard('$ancestorModule:${guard.predicate}', guard.args,
        guard.line, guard.column,
        negated: guard.negated);
  }
  return guard;
}

/// Resolve a single goal in a clause body.
///
/// Resolution order: local procedure → ancestor self.glp chain → root scope/stdlib.
Goal _resolveGoal(Goal goal, String moduleName, Set<String> localSigs,
    Map<String, String> ancestorSelfProcs,
    {bool keepLocalBare = false}) {
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
    final resolvedInner = _resolveGoal(
        goal.innerGoal, moduleName, localSigs, ancestorSelfProcs,
        keepLocalBare: keepLocalBare);
    if (!identical(resolvedInner, goal.innerGoal)) {
      return SpawnGoal(resolvedInner, goal.agentId, goal.line, goal.column);
    }
    return goal;
  }

  // Regular goal: check if it matches a local procedure
  final sig = '${goal.functor}/${goal.arity}';
  if (localSigs.contains(sig)) {
    // Loaded module keeps bare names: a local call stays bare.
    if (keepLocalBare) return goal;
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

  bool isInputArg(int i) => declaration != null && i < declaration.argTypes.length
      ? declaration.isInputArg(i)
      : true; // Fallback: assume input when no declaration

  // Head args (V prefix — underscore prefix causes issues in codegen).
  // Input arg (T?): the head captures the caller's value as a writer.
  // Output arg (T): the head is a reader hole that the body's writer fills.
  // (For arity>0 procedures with output args, a head writer there would pair
  // with a body writer — an SRSW violation; the head must be the reader.)
  final headArgs = List.generate(
      arity, (i) => VarTerm('V$i', !isInputArg(i), 0, 0) as Term);

  // Body args: input → reader (forward the value), output → writer (so the
  // callee fills it).
  final bodyArgs = List.generate(arity, (i) {
    final isInput = isInputArg(i);
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

// Ancestor-scope assembly lives in module_hierarchy.dart (buildAncestorScope)
// — the one shared implementation; no linker-local copy.
