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
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart';

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
  final layer = TypeEnvironment(moduleEnv.types, moduleEnv.procedures,
      paramProcDecls: moduleEnv.paramProcDecls, typeTemplates: templates);
  if (!typesFillGapsOnly) return env.merge(layer, label: label);
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
  return TypeEnvironment(
      {...under.types, ...env.types},
      {...under.procedures, ...env.procedures},
      paramProcDecls: {...under.paramProcDecls, ...env.paramProcDecls},
      typeTemplates: {...under.typeTemplates, ...env.typeTemplates},
      typeOrigins: {...under.originsUnder(label), ...env.typeOrigins});
}

/// Merge a self.glp file into a scope environment: parse, then
/// [mergeModuleIntoScope], labelled by the directory the `self.glp` is the
/// scope of --- the name the linker gives that module.
///
/// The types the `self.glp`'s `-expose` directives lift are in [env] before
/// the `self.glp` is merged ([liftExposedTypes]), so its own declarations and
/// every later layer resolve them.
TypeEnvironment mergeSelfGlpFileIntoScope(TypeEnvironment env, String path) {
  final source = File(path).readAsStringSync();
  final module = Parser(Lexer(source).tokenize()).parseModule();
  final lifted = liftExposedTypes(env, module, File(path).parent.path);
  return mergeModuleIntoScope(lifted, module, label: _directoryLabel(path));
}

/// [env] with the types that [exposer]'s `-expose` directives lift into the
/// scope of its directory [exposerDir].
///
/// TGLP modules.tex, "The -expose directive": `-expose(M).` "lifts the
/// exported procedures of module M (and the types their signatures carry) into
/// that directory's scope, as if defined in its self.glp".  A scope is layered
/// one `self.glp` at a time (Definition (Root, Scope)), and a layer's
/// declarations, and those of every layer after it, are expanded against the
/// types known when it is merged; so a type an ancestor exposes --- the root's
/// `-expose(system#mad_predicates)` gives every scope `NetStream` --- must be
/// known then, or a declaration naming it is refused as naming an undefined
/// type, or, naming no parameters, reads it as one.
///
/// Only the types are lifted here.  The exposed procedures, their collisions
/// and their entry-point status are the linker's (program_linker.dart,
/// `_resolveExposes`), which lifts them into each module of the exposing
/// subtree with the same [exposedExportScope], so the two agree on every type
/// they share.  A type [env] already defines is not replaced: a definition
/// nearer the use site shadows an exposed one.  A module path whose file is
/// missing lifts nothing here; the linker reports it.
TypeEnvironment liftExposedTypes(
    TypeEnvironment env, ast.Module exposer, String exposerDir) {
  if (exposer.exposes.isEmpty) return env;
  var types = env.types;
  var origins = env.typeOrigins;
  for (final path in exposer.exposes) {
    final file =
        File('${ppath.joinAll([exposerDir, ...path.split('#')])}.glp');
    if (!file.existsSync()) continue;
    final exposed = Parser(Lexer(file.readAsStringSync()).tokenize())
        .parseModule();
    final lifted = exposedExportScope(exposed,
        TypeEnvironment(types, env.procedures,
            paramProcDecls: env.paramProcDecls,
            typeTemplates: env.typeTemplates,
            typeOrigins: origins),
        exposerTypeDefs: exposer.typeDefs);
    final label = ppath.basenameWithoutExtension(file.path);
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
      typeOrigins: origins);
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

/// Build the ancestor scope for a self.glp chain (root-first order).
///
/// modules.tex §Scope construction: the scope begins with the GLP language
/// primitives (root scope); if [rootSelfGlpPath] is given and exists, the root
/// self.glp (programs/self.glp) is layered next, and chain entries equal to it
/// are skipped; then each chain self.glp in order, later shadowing earlier.
/// The target module itself is NOT merged here — [assembleTypeScope] adds it;
/// on the engine path checkModule adds it.
///
/// This is the ONE implementation of ancestor-scope assembly, shared by the
/// linker, the engine's module check, and the engine's goal-check environment.
TypeEnvironment buildAncestorScope({
  required List<String> chain,
  String? rootSelfGlpPath,
}) {
  var env = buildRootScopeEnvironment();
  File? rootSelf;
  if (rootSelfGlpPath != null) {
    final f = File(rootSelfGlpPath);
    if (f.existsSync()) {
      rootSelf = f;
      // The root self.glp is one layer, d_1 of every scope (TGLP Definition
      // "Root, Scope"), and the root-scope environment already is it when it
      // was built from this file's text: layering the file again would make
      // every type of the root a second definition of itself. It is layered
      // here only when the root-scope environment was built from something
      // else.
      final source = f.readAsStringSync();
      if (!isRootScopeEnvironmentSource(source)) {
        env = mergeSelfGlpFileIntoScope(env, f.path);
      } else {
        // The root-scope environment realises the root's definitions but not
        // its `-expose` directives, which are d_1's as much as its
        // definitions are (modules.tex, "The -expose directive").
        env = liftExposedTypes(
            env, Parser(Lexer(source).tokenize()).parseModule(), f.parent.path);
      }
    }
  }
  for (final selfGlpPath in chain) {
    if (rootSelf != null &&
        File(selfGlpPath).absolute.path == rootSelf.absolute.path) {
      continue;
    }
    env = mergeSelfGlpFileIntoScope(env, selfGlpPath);
  }
  return env;
}

/// Assemble the type scope for a module by layering ancestor definitions.
///
/// Builds a TypeEnvironment by:
/// 1. Starting with the root scope (root of all type chains)
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
}) {
  return mergeModuleIntoScope(buildAncestorScope(chain: chain), module);
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
