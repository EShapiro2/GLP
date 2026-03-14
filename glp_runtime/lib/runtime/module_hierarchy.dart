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
///
/// Returns: list of absolute paths to self.glp files, root-first order
List<String> discoverSelfChain({
  required String targetFile,
  required String rootDir,
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

  // Walk from startDir up to root, collecting self.glp files
  final chain = <String>[];
  var currentDir = startDir;

  while (true) {
    // Normalize for comparison, stripping trailing slashes for consistency
    var currentNorm = Directory(currentDir).absolute.path;
    var rootNorm = Directory(root).absolute.path;
    if (currentNorm.endsWith('/')) currentNorm = currentNorm.substring(0, currentNorm.length - 1);
    if (rootNorm.endsWith('/')) rootNorm = rootNorm.substring(0, rootNorm.length - 1);

    // Check if we've gone above the root
    if (!currentNorm.startsWith(rootNorm)) {
      break;
    }

    final selfGlp = File('$currentDir${Platform.pathSeparator}self.glp');
    if (selfGlp.existsSync()) {
      chain.add(selfGlp.absolute.path);
    }

    // If we've reached the root, stop
    if (currentNorm == rootNorm) {
      break;
    }

    // Walk up
    currentDir = Directory(currentDir).parent.path;
  }

  // Reverse: we collected from target-to-root, but want root-first
  return chain.reversed.toList();
}

/// Assemble the type scope for a module by layering ancestor definitions.
///
/// Builds a TypeEnvironment by:
/// 1. Starting with the prelude (root of all type chains)
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
  // Start with prelude
  var env = buildPreludeEnvironment();

  // Layer each self.glp in order (root first, children shadow parents)
  for (final selfGlpPath in chain) {
    final source = File(selfGlpPath).readAsStringSync();
    final lexer = Lexer(source);
    final tokens = lexer.tokenize();
    final parser = Parser(tokens);
    final selfModule = parser.parseModule();

    // Extract templates from this self.glp before expansion removes them.
    // These chain to descendant modules so they can expand references.
    final selfTemplates = <String, TypeDef>{};
    for (final td in selfModule.typeDefs) {
      if (td.isParameterized) {
        selfTemplates[td.name] = td;
      }
    }

    // Expand parameterized types before building scope
    // Pass accumulated env type names so earlier types aren't mistaken for type params.
    // Pass ancestor templates so this self.glp can expand references to prelude templates.
    final expandedSelfModule = expandParameterizedTypes(selfModule,
        knownTypeNames: env.types.keys.toSet(),
        externalTemplates: env.typeTemplates);

    // Build environment from this self.glp (without prelude check — ancestors
    // can define types with same names, shadowing is allowed)
    final selfEnv = buildScopeFromModule(expandedSelfModule);

    // Merge: later entries overwrite earlier ones (shadowing).
    // Include this self.glp's templates in the environment for descendants.
    env = env.merge(TypeEnvironment(selfEnv.types, selfEnv.procedures,
        paramProcDecls: selfEnv.paramProcDecls,
        typeTemplates: selfTemplates));
  }

  // Finally, merge the target module's own definitions (shadows all ancestors)
  final expandedModule = expandParameterizedTypes(module,
      knownTypeNames: env.types.keys.toSet(),
      externalTemplates: env.typeTemplates);
  final moduleEnv = buildScopeFromModule(expandedModule);
  env = env.merge(moduleEnv);

  return env;
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
