/// Module hierarchy: self.glp chain discovery and type scope assembly.
///
/// Implements GLP module scoping per docs/modules/glp-module-system-spec.md:
/// - Directory-based hierarchy (Section 2)
/// - Implicit ancestor scoping (Section 3.1)
/// - Shadowing (Section 3.2)
/// - Sibling isolation (Section 3.3)
///
/// Specification: docs/modules/glp-module-system-spec.md Sections 2-3

import 'dart:io';
import 'package:path/path.dart' as ppath;
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/ast.dart' as ast;
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/param_expansion.dart';

/// The name of the module held by the file at [filePath]: its path from the
/// root [rootDir] (TGLP modules.tex, Compilation, third step: every procedure
/// and type is renamed `M:p/n` and `M:T`, "where M is the module's path from
/// the root").  A directory's `self.glp` is named by the directory's path, any
/// other module by its directory's path and its file name, the extension
/// dropped: `secure/app.glp` is `secure/app`, `secure/self.glp` is `secure`.
/// Segments are joined by `/`; the root's own `self.glp` is the empty path.
///
/// A file outside [rootDir] --- a program loaded from outside the root, which
/// TGLP's Definition "Root, Scope" does not cover --- is named by its relative
/// path from the root, `..` segments included, which no file under the root
/// shares.
String modulePathName(String filePath, String rootDir) {
  final file = ppath.normalize(File(filePath).absolute.path);
  final root = ppath.normalize(Directory(rootDir).absolute.path);
  final target = ppath.basename(file) == 'self.glp'
      ? ppath.dirname(file)
      : ppath.withoutExtension(file);
  final rel = ppath.relative(target, from: root);
  if (rel == '.') return '';
  return ppath.split(rel).join('/');
}

/// The path from the root of the directory holding the module named [name]:
/// the module itself where it is a `self.glp` ([isSelfGlp]), otherwise its
/// name without the last segment.  The root is the empty path.
String moduleDirectoryName(String name, {required bool isSelfGlp}) {
  if (isSelfGlp) return name;
  final slash = name.lastIndexOf('/');
  return slash < 0 ? '' : name.substring(0, slash);
}

/// Discover the self.glp chain from root to target file's directory.
///
/// Walks up from the target file's directory to the root directory,
/// collecting self.glp files at each level. Returns them in root-first
/// order (outermost ancestor first, innermost last).
///
/// If the target file IS self.glp, the chain includes only ancestors
/// above it (not itself — the module's own definitions come from parsing it).
///
/// [targetFile]: absolute path to the .glp file being compiled
/// [rootDir]: absolute path to the project root directory
/// [programsDir]: absolute path to the root `programs/` directory. When given,
///   the chain extends ABOVE the project root up to (but excluding) this
///   directory — so intermediate ancestor `self.glp` files between the load
///   point and `programs/` are included. The root `programs/self.glp` itself is
///   NOT collected here (it is realised by the root-scope mechanism). When null,
///   the legacy bound applies: the walk stops at `rootDir` (inclusive).
///
/// Returns: list of absolute paths to self.glp files, root-first order
List<String> discoverSelfChain({
  required String targetFile,
  required String rootDir,
  String? programsDir,
}) {
  // Normalize paths
  final root = Directory(rootDir).absolute.path;
  final target = File(targetFile).absolute.path;
  final targetName = target.split(Platform.pathSeparator).last;

  // Determine the starting directory for the walk.
  // If the target IS self.glp, start from its parent (don't include itself).
  // Otherwise, start from the target's directory.
  String startDir;
  if (targetName == 'self.glp') {
    // Target is self.glp — start from its parent directory
    startDir = File(target).parent.parent.path;
  } else {
    // Target is a regular module — start from its directory
    startDir = File(target).parent.path;
  }

  // Normalize a path for comparison: make absolute, resolve `..`/`.` segments
  // lexically (callers may pass paths containing `..`, e.g. `GLP/test/..`), and
  // strip any trailing slash.
  String norm(String p) {
    var n = ppath.normalize(Directory(p).absolute.path);
    if (n.endsWith('/')) n = n.substring(0, n.length - 1);
    return n;
  }

  final rootNorm = norm(root);
  final programsNorm = programsDir != null ? norm(programsDir) : null;

  // Walk from startDir upward, collecting self.glp files at each level.
  final chain = <String>[];
  var currentDir = startDir;

  while (true) {
    final currentNorm = norm(currentDir);

    if (programsNorm != null) {
      // Extended bound: stop at programsDir WITHOUT collecting its self.glp,
      // and never walk above it.
      if (currentNorm == programsNorm) break;
      if (!currentNorm.startsWith(programsNorm)) break;
    } else {
      // Legacy bound: stop once we have gone above rootDir.
      if (!currentNorm.startsWith(rootNorm)) break;
    }

    final selfGlp = File('$currentDir${Platform.pathSeparator}self.glp');
    if (selfGlp.existsSync()) {
      chain.add(selfGlp.absolute.path);
    }

    // Legacy inclusive stop at rootDir (only when not extending to programsDir).
    if (programsNorm == null && currentNorm == rootNorm) break;

    final parent = Directory(currentDir).parent.path;
    if (parent == currentDir) break; // filesystem root safety
    currentDir = parent;
  }

  // Reverse: collected target-to-outermost, but callers want outermost-first.
  return chain.reversed.toList();
}

/// Merge a parsed module's types and declarations into a scope environment.
///
/// The single scope-merge primitive (modules.tex §Scope construction: later
/// definitions shadow earlier ones). Extracts the module's parameterized
/// templates before expansion removes them, so descendant scopes can expand
/// references to them; expands the module's own parameterized types against
/// the accumulated environment (known type names are not mistaken for type
/// parameters); merges with shadowing.
/// [typesFillGapsOnly] keeps the module's type definitions from shadowing what
/// [env] already defines: they fill gaps and nothing more. This is the rule for
/// a module of a DESCENDANT directory in the goal-check environment — a goal is
/// posted to the program's entry points and is therefore checked in the program
/// ROOT's scope, so a deeper `self.glp`'s redefinition of one of its type names
/// is that subtree's business and not the goal's. (Reported by Currencies Code,
/// 2026-09-03, against the linked program; the goal check took the deeper
/// definition the same way.)
///
/// [label] is the scope the module's types are recorded as defined in
/// ([TypeEnvironment.typeOrigins]): the prefix a definition is kept under
/// once a nearer scope defines its name, in either direction of shadowing.
TypeEnvironment mergeModuleIntoScope(TypeEnvironment env, ast.Module module,
    {bool typesFillGapsOnly = false, String? label}) {
  final templates = <String, TypeDef>{};
  for (final td in module.typeDefs) {
    if (td.isParameterized) {
      templates[td.name] = td;
    }
  }
  final expanded = expandParameterizedTypes(module,
      knownTypeNames: env.types.keys.toSet(),
      externalTemplates: env.typeTemplates);
  final moduleEnv = buildScopeFromModule(expanded);
  // The layer's clauses join the scope's, by key: a defined guard of the
  // layer is unfolded, and its parameterised procedures certified, by them
  // ([TypeEnvironment.scopeLayers]).
  final clauses = <String, List<ast.Clause>>{};
  for (final proc in module.procedures) {
    for (final c in proc.clauses) {
      clauses
          .putIfAbsent('${c.head.functor}/${c.head.arity}', () => [])
          .add(c);
    }
  }
  final scopeLayer = ScopeLayer(clauses);
  final layer = TypeEnvironment(moduleEnv.types, moduleEnv.procedures,
      paramProcDecls: moduleEnv.paramProcDecls,
      typeTemplates: templates,
      scopeLayers: {for (final k in clauses.keys) k: scopeLayer});
  final merged =
      _mergeLayer(env, layer, typesFillGapsOnly: typesFillGapsOnly, label: label);
  // The scope the layer's procedures were declared in.
  scopeLayer.env = merged;
  return merged;
}

/// [layer], a module's environment, merged into [env] ([mergeModuleIntoScope]).
TypeEnvironment _mergeLayer(TypeEnvironment env, TypeEnvironment layer,
    {bool typesFillGapsOnly = false, String? label}) {
  if (!typesFillGapsOnly) {
    // Innermost-first shadowing, as [buildTypeEnvironment] applies it to a
    // module's own scope (modules.tex, "Scope construction": later
    // definitions shadow earlier): a procedure the module declares
    // monomorphic shadows an inherited parameterised template of the same
    // key, which therefore does not survive in paramProcDecls, else a call
    // the module's declaration types is read as a call to the template.
    // Until 2026-10-02 the template survived here, beside the module's
    // declaration, and a goal posted to book's merge_ordered.glp,
    // merge([1,3,5], [2,4,6], Zop), was read as a call to the root's
    // procedure(X) merge(Stream(X)?, Stream(X)?, Stream(X)).
    final merged = env.merge(layer, label: label);
    final shadowed = {
      for (final key in layer.procedures.keys)
        if (!layer.paramProcDecls.containsKey(key) &&
            merged.paramProcDecls.containsKey(key))
          key
    };
    if (shadowed.isEmpty) return merged;
    return TypeEnvironment(merged.types, merged.procedures,
        paramProcDecls: {
          for (final e in merged.paramProcDecls.entries)
            if (!shadowed.contains(e.key)) e.key: e.value
        },
        typeTemplates: merged.typeTemplates,
        typeOrigins: merged.typeOrigins,
        scopeLayers: merged.scopeLayers);
  }
  // The module under the scope rather than over it: its types and its
  // declarations fill gaps, and a type the scope already defines survives
  // from the module under `<label>:T`, the module's own declarations meaning
  // it (TypeEnvironment.shadowedBy) --- a declaration's types are those of
  // the scope it was declared in, whichever way the shadowing runs. Its
  // declarations fill gaps for the same reason its types do: "the procedures
  // that may be posted are exactly the entry points" (TGLP "Compilation",
  // entry and the absence of a boot module), the program root's exports, so
  // a descendant's procedure of the same name and arity as one in the root's
  // scope is not what a goal names. Until 2026-09-18 it shadowed the root's
  // declaration while its types were read as the root's, which passed a goal
  // against a declaration of the same shape and would have rejected any other.
  final under = layer.shadowedBy(env, ownLabel: label);
  // The scope's own declaration of a key shadows the module's, so a template
  // the module carries for a key the scope declares monomorphic does not
  // survive either.
  return TypeEnvironment(
      {...under.types, ...env.types},
      {...under.procedures, ...env.procedures},
      paramProcDecls: {
        for (final e in under.paramProcDecls.entries)
          if (!env.procedures.containsKey(e.key) ||
              env.paramProcDecls.containsKey(e.key))
            e.key: e.value,
        ...env.paramProcDecls
      },
      typeTemplates: {...under.typeTemplates, ...env.typeTemplates},
      typeOrigins: {...under.originsUnder(label), ...env.typeOrigins},
      scopeLayers: {...layer.scopeLayers, ...env.scopeLayers});
}

/// Merge a self.glp file into a scope environment: parse, then
/// [mergeModuleIntoScope], labelled by the directory the `self.glp` is the
/// scope of --- the name the linker gives that module, its path from [root]
/// ([modulePathName]); with no [root] given, the directory's last segment.
///
/// The types the `self.glp`'s `-expose` directives lift are in [env] before
/// the `self.glp` is merged ([liftExposedTypes]), so its own declarations and
/// every later layer resolve them.
TypeEnvironment mergeSelfGlpFileIntoScope(TypeEnvironment env, String path,
    {String? root, String? label}) {
  final source = File(path).readAsStringSync();
  final module = Parser(Lexer(source).tokenize()).parseModule();
  final lifted =
      liftExposedTypes(env, module, File(path).parent.path, root: root);
  try {
    return mergeModuleIntoScope(lifted, module,
        label: label ??
            (root != null ? modulePathName(path, root) : _directoryLabel(path)));
  } on UndefinedDeclarationTypeError catch (e) {
    // The declaration is the self.glp's: the error names its file.
    throw e.inFile(path);
  }
}

/// [env] with the types that [exposer]'s `-expose` directives lift into the
/// scope of its directory [exposerDir].
///
/// TGLP modules.tex, "The -expose directive": `-expose(M).` "lifts the
/// exported procedures of module M (and the types their signatures carry) into
/// that directory's scope, as if defined in its self.glp".  A scope is layered
/// one `self.glp` at a time (Definition (Root, Scope)), and a layer's
/// declarations, and those of every layer after it, are expanded against the
/// types known when it is merged; so a type an ancestor exposes must be known
/// then, or a declaration naming it is refused as naming an undefined type, or,
/// naming no parameters, reads it as one (`NetStream`, which the root's
/// `-expose(system#mad_predicates)` gave every scope until 2026-10-04;
/// programs/tests/expose/root_types_decl).
///
/// Only the types are lifted here.  The exposed procedures, their collisions
/// and their entry-point status are the linker's (program_linker.dart,
/// `_resolveExposes`), which lifts them into each module of the exposing
/// subtree with the same [exposedExportScope], so the two agree on every type
/// they share.  A type [env] already defines is not replaced: a definition
/// nearer the use site shadows an exposed one.  A module path whose file is
/// missing lifts nothing here; the linker reports it.  The lifted types are
/// labelled by the exposed module's name, its path from [root]
/// ([modulePathName]); with no [root] given, its file name.
TypeEnvironment liftExposedTypes(
    TypeEnvironment env, ast.Module exposer, String exposerDir,
    {String? root}) {
  if (exposer.exposes.isEmpty) return env;
  var types = env.types;
  var origins = env.typeOrigins;
  for (final path in exposer.exposes) {
    final file =
        File('${ppath.joinAll([exposerDir, ...path.split('#')])}.glp');
    if (!file.existsSync()) continue;
    final exposed = Parser(Lexer(file.readAsStringSync()).tokenize())
        .parseModule();
    final TypeEnvironment lifted;
    try {
      lifted = exposedExportScope(exposed,
          TypeEnvironment(types, env.procedures,
              paramProcDecls: env.paramProcDecls,
              typeTemplates: env.typeTemplates,
              typeOrigins: origins,
              scopeLayers: env.scopeLayers),
          exposerTypeDefs: exposer.typeDefs);
    } on UndefinedDeclarationTypeError catch (e) {
      throw e.inFile(file.path);
    }
    final label = root != null
        ? modulePathName(file.path, root)
        : ppath.basenameWithoutExtension(file.path);
    final added = <String, TypeDef>{
      for (final e in lifted.types.entries)
        if (!types.containsKey(e.key)) e.key: e.value,
    };
    if (added.isEmpty) continue;
    types = {...types, ...added};
    origins = {...origins, for (final t in added.keys) t: label};
  }
  if (identical(types, env.types)) return env;
  return TypeEnvironment(types, env.procedures,
      paramProcDecls: env.paramProcDecls,
      typeTemplates: env.typeTemplates,
      typeOrigins: origins,
      scopeLayers: env.scopeLayers);
}

/// A TypeEnvironment of a module's EXPORTED procedure declarations plus the
/// types it defines, for type-checking exposed signatures in the subtree.
///
/// [base] supplies the exposing subtree's known type names and parameterised
/// templates (`Stream`, `Channel`, …), so the exposed signatures' parameterised
/// types are recognised and routed to `paramProcDecls` (exactly as an ordinary
/// ancestor `self.glp` would be processed).
///
/// [exposerTypeDefs] adds the definitions of the module that exposed [m].  An
/// exposed declaration is read "as if defined in its `self.glp`", so its type
/// names resolve in the exposing module's scope, which carries that module's
/// own definitions (Definition (Root, Scope): the scope of M ends in M).
/// [base] is the scope of the module RECEIVING the lift, and a `self.glp`'s own
/// ancestor scope excludes itself, so a type the exposing `self.glp` defines is
/// otherwise undefined at the very declaration naming it.  They are made known
/// here and not merged into the returned scope: what `-expose` lifts is the
/// exposed module's procedures and the types their signatures carry.
///
/// The known names of the lift are both scopes', the root's among [base] and
/// the exposing module's in [exposerTypeDefs] --- not the latter instead of the
/// former --- and each enters as what it is: a monomorphic definition as a
/// known type name, a parameterised one as a template, exactly as [base]
/// carries them (`types` against `typeTemplates`).  A template entered as a
/// known monomorphic name makes the expansion collapse a wildcard instance of
/// it to the bare name (`Stream(_)` to `Stream`, param_expansion.dart), which
/// no scope defines: that is how a lifted `Stream(C)`, `C` a parameter, became
/// "Unresolved type: Stream" wherever the root `self.glp` both defines `Stream`
/// and exposes the declaration.  A parameter of the lifted declaration is in
/// neither and stays bare (parameterized-types.tex, "Declaration parameters").
TypeEnvironment exposedExportScope(ast.Module m, TypeEnvironment base,
    {List<TypeDef> exposerTypeDefs = const []}) {
  final exported = m.procDeclarations.where((d) => d.exported).toList();
  final synthetic = ast.Module(
    typeDefs: m.typeDefs,
    procDeclarations: exported,
    line: m.line,
    column: m.column,
  );
  final expanded = expandParameterizedTypes(synthetic,
      knownTypeNames: {
        ...base.types.keys,
        for (final td in exposerTypeDefs)
          if (td.typeParams.isEmpty) td.name,
      },
      externalTemplates: {
        ...base.typeTemplates,
        for (final td in exposerTypeDefs)
          if (td.typeParams.isNotEmpty) td.name: td,
      });
  return buildScopeFromModule(expanded);
}

/// The last segment of the directory holding [path].
String _directoryLabel(String path) {
  final dir = File(path).absolute.parent.path;
  final parts = dir.split(Platform.pathSeparator).where((p) => p.isNotEmpty);
  return parts.isEmpty ? dir : parts.last;
}

/// The language primitives, `Π` of TGLP Definition "Root, Scope": the base of
/// every scope, for every module of every program, and not an ancestor
/// directory (modules.tex, "Two things are named self.glp and they enter a
/// scope by different routes").  By GLP-Spec appendix-guards.tex, "Predefined
/// types", they are the primitive types `Integer`, `Real`, `String`, `Module`
/// and `MutualRef`, which the checker builds into every automaton
/// (analysis/type_checker/program_dfa.dart), and the kernels and builtin
/// guards the runtime implements (root_scope.dart, `builtinProcedures`), whose
/// declarations the root self.glp states in GLP; everything else in the root
/// is GLP and reaches a scope by the chain (GLP #3 Cowork, 2026-10-03 21:18
/// UTC).  As an environment it therefore defines nothing: the empty scope.
TypeEnvironment primitiveScope() => TypeEnvironment.empty();

/// The scope a module directly under the root is checked in, `Π ⊔ d_1`, the
/// root self.glp at [rootSelfGlpPath] its one layer: what a module of no
/// further ancestor sees, a file-less source and a posted goal among them.
/// [primitiveScope] where there is no root.
TypeEnvironment rootScope(String? rootSelfGlpPath) =>
    buildAncestorScope(chain: const [], rootSelfGlpPath: rootSelfGlpPath);

/// Build the ancestor scope for a self.glp chain (root-first order).
///
/// TGLP Definition "Root, Scope": the scope begins with the language
/// primitives ([primitiveScope]); the root self.glp at [rootSelfGlpPath], d_1,
/// is layered next like any self.glp, under the empty path its renaming gives
/// it, and a chain entry equal to it is skipped; then each chain self.glp in
/// order, later shadowing earlier.  The target module itself is NOT merged
/// here — [assembleTypeScope] adds it; on the engine path checkModule adds it.
/// Until 2026-10-04 the base was a root-scope environment built from a source
/// the engine set once for the whole process.
///
/// This is the ONE implementation of ancestor-scope assembly, shared by the
/// linker, the engine's module check, and the engine's goal-check environment.
///
/// [rootScope] is `Π ⊔ d_1` built already ([rootScope] of this file) for the
/// same root: the scopes of one load, and of one engine, are built over it,
/// so the root's layer is one and its procedures are certified once
/// ([ScopeLayer]).  Each scope returned has maps of its own.
TypeEnvironment buildAncestorScope({
  required List<String> chain,
  String? rootSelfGlpPath,
  TypeEnvironment? rootScope,
}) {
  var env = primitiveScope();
  File? rootSelf;
  if (rootSelfGlpPath != null) {
    final f = File(rootSelfGlpPath);
    if (f.existsSync()) {
      rootSelf = f;
      env = rootScope != null
          ? rootScope.copy()
          : mergeSelfGlpFileIntoScope(env, f.path,
              root: f.parent.path, label: '');
    }
  }
  for (final selfGlpPath in chain) {
    if (rootSelf != null &&
        File(selfGlpPath).absolute.path == rootSelf.absolute.path) {
      continue;
    }
    env = mergeSelfGlpFileIntoScope(env, selfGlpPath,
        root: rootSelf?.parent.path);
  }
  return env;
}

/// Assemble the type scope for a module by layering ancestor definitions.
///
/// Builds a TypeEnvironment by:
/// 1. Starting with the language primitives and the root self.glp at
///    [rootSelfGlpPath], where one is given ([buildAncestorScope])
/// 2. Merging each self.glp in the chain (root-first, so children shadow parents)
/// 3. Merging the target module's own types and declarations (shadows all ancestors)
///
/// [chain]: list of self.glp file paths, root-first order (from discoverSelfChain)
/// [module]: the parsed Module AST of the target file
///
/// Returns: the assembled TypeEnvironment with all visible types and procedures
TypeEnvironment assembleTypeScope({
  required List<String> chain,
  required ast.Module module,
  String? rootSelfGlpPath,
}) {
  return mergeModuleIntoScope(
      buildAncestorScope(chain: chain, rootSelfGlpPath: rootSelfGlpPath),
      module);
}

/// Build a TypeEnvironment from a Module's types and procedure declarations.
///
/// Unlike _buildEnvironmentFromModule in type_environment_builder.dart,
/// this does NOT check for predefined type redefinition (because shadowing
/// ancestor types is allowed) and does NOT resolve aliases (that happens
/// after all scopes are assembled).
TypeEnvironment buildScopeFromModule(ast.Module module) {
  final types = <String, TypeDef>{};
  final procedures = <String, ProcDecl>{};
  final paramProcDecls = <String, ProcDecl>{};

  for (final typeDef in module.typeDefs) {
    types[typeDef.name] = typeDef;
  }

  for (final procDecl in module.procDeclarations) {
    procedures[procDecl.qualifiedKey] = procDecl;
  }

  for (final paramDecl in module.paramProcDecls) {
    paramProcDecls[paramDecl.qualifiedKey] = paramDecl;
  }

  return TypeEnvironment(types, procedures, paramProcDecls: paramProcDecls);
}
