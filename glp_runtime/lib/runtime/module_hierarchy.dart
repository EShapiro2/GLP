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
TypeEnvironment mergeModuleIntoScope(TypeEnvironment env, ast.Module module,
    {bool typesFillGapsOnly = false}) {
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
  final merged = env.merge(TypeEnvironment(moduleEnv.types, moduleEnv.procedures,
      paramProcDecls: moduleEnv.paramProcDecls, typeTemplates: templates));
  if (!typesFillGapsOnly) return merged;
  return TypeEnvironment(
      {...merged.types, ...env.types}, merged.procedures,
      paramProcDecls: merged.paramProcDecls,
      typeTemplates: {...merged.typeTemplates, ...env.typeTemplates});
}

/// Merge a self.glp file into a scope environment: parse, then
/// [mergeModuleIntoScope].
TypeEnvironment mergeSelfGlpFileIntoScope(TypeEnvironment env, String path) {
  final source = File(path).readAsStringSync();
  final module = Parser(Lexer(source).tokenize()).parseModule();
  return mergeModuleIntoScope(env, module);
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
      env = mergeSelfGlpFileIntoScope(env, f.path);
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
