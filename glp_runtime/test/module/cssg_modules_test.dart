/// Validation test: cssg_modules project against Phase 1-3 module system.
///
/// Validates that each module file in programs/cssg_modules/ parses and
/// type-checks correctly using the real pipeline from GlpEngine.loadSource:
///   1. Parse → 2. Partial evaluation → 3. Type check with ancestor scope
///
/// This test does NOT modify any .glp files or runtime code.

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/ast.dart' as ast;
import 'package:glp_runtime/compiler/partial_evaluator.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart';
import 'package:glp_runtime/runtime/module_hierarchy.dart';

void main() {
  // Set prelude sources from programs/self.glp (same as GlpEngine constructor)
  final rootSelfGlp = File('../programs/self.glp');
  if (rootSelfGlp.existsSync()) {
    final source = rootSelfGlp.readAsStringSync();
    setPreludeUnitClauseSource(source);
    setPreludeEnvironmentSource(source);
  }

  // Locate cssg_modules relative to glp_runtime/
  final cssgRoot = '../programs/cssg_modules';

  // Verify the project directory exists
  if (!Directory(cssgRoot).existsSync()) {
    throw StateError('cssg_modules directory not found at $cssgRoot');
  }

  /// Parse a .glp file into a Module AST.
  ast.Module parseFile(String path) {
    final source = File(path).readAsStringSync();
    final lexer = Lexer(source);
    final tokens = lexer.tokenize();
    final parser = Parser(tokens);
    return parser.parseModule();
  }

  /// Build the ancestor type scope for a target file.
  ///
  /// Uses discoverSelfChain to find the self.glp chain, then builds
  /// the ancestor scope (prelude + chain self.glp files) WITHOUT the
  /// target module's own definitions — those are added by checkModule
  /// via buildTypeEnvironment(module, ancestorScope: ...).
  TypeEnvironment buildAncestorScope(String targetFile) {
    final chain = discoverSelfChain(
      targetFile: targetFile,
      rootDir: cssgRoot,
    );

    // Start with prelude
    var env = buildPreludeEnvironment();

    // Layer each self.glp in chain order (root first)
    for (final selfGlpPath in chain) {
      final selfModule = parseFile(selfGlpPath);

      // Build scope from this self.glp: types and procedure declarations
      // (same logic as _buildScopeFromModule in module_hierarchy.dart)
      final types = <String, TypeDef>{};
      final procedures = <String, ProcDecl>{};
      for (final typeDef in selfModule.typeDefs) {
        types[typeDef.name] = typeDef;
      }
      for (final procDecl in selfModule.procDeclarations) {
        procedures[procDecl.qualifiedKey] = procDecl;
      }
      final selfEnv = TypeEnvironment(types, procedures);

      // Merge: later entries shadow earlier ones
      env = env.merge(selfEnv);
    }

    return env;
  }

  /// Full pipeline: parse → PE → type check with ancestor scope.
  /// Matches GlpEngine.loadSource.
  TypeCheckResult typeCheckFile(String path) {
    final module = parseFile(path);
    final ancestorScope = buildAncestorScope(path);

    // Partial evaluation: eliminate defined guards
    final program = ast.Program(module.procedures, module.line, module.column);
    final pe = PartialEvaluator();
    final transformedAst = pe.transformDefinedGuards(program);

    // Type check with PE-transformed procedures and ancestor scope
    return checkModule(module,
        transformedProcedures: transformedAst.procedures,
        ancestorScope: ancestorScope);
  }

  group('cssg_modules end-to-end', () {
    test('self.glp parses and type-checks', () {
      final selfPath = '$cssgRoot/self.glp';
      expect(File(selfPath).existsSync(), isTrue,
          reason: 'self.glp must exist');

      final module = parseFile(selfPath);

      // self.glp should have type definitions
      expect(module.typeDefs, isNotEmpty,
          reason: 'self.glp should define shared types');

      // self.glp is types-only — no procedures, but run through pipeline
      // to verify type environment builds without error
      final result = typeCheckFile(selfPath);

      if (!result.isWellTyped) {
        fail('self.glp type errors:\n${result.errors.join('\n')}');
      }
    });

    test('agent.glp type-checks with PE and ancestor scope', () {
      final agentPath = '$cssgRoot/agent.glp';
      expect(File(agentPath).existsSync(), isTrue,
          reason: 'agent.glp must exist');

      final result = typeCheckFile(agentPath);

      if (!result.isWellTyped) {
        fail('agent.glp type errors:\n${result.errors.join('\n')}');
      }
    });

    test('ui/mediator.glp type-checks with PE and ancestor scope', () {
      final mediatorPath = '$cssgRoot/ui/mediator.glp';
      expect(File(mediatorPath).existsSync(), isTrue,
          reason: 'ui/mediator.glp must exist');

      final result = typeCheckFile(mediatorPath);

      if (!result.isWellTyped) {
        fail('ui/mediator.glp type errors:\n${result.errors.join('\n')}');
      }
    });

    test('ui/actors.glp type-checks with PE and ancestor scope', () {
      final actorsPath = '$cssgRoot/ui/actors.glp';
      expect(File(actorsPath).existsSync(), isTrue,
          reason: 'ui/actors.glp must exist');

      final result = typeCheckFile(actorsPath);

      if (!result.isWellTyped) {
        fail('ui/actors.glp type errors:\n${result.errors.join('\n')}');
      }
    });

    test('boot.glp parses (untyped orchestration)', () {
      final bootPath = '$cssgRoot/boot.glp';
      expect(File(bootPath).existsSync(), isTrue,
          reason: 'boot.glp must exist');

      // boot.glp is untyped orchestration — no procedure declarations
      // (only imported declarations). Just verify it parses successfully.
      final module = parseFile(bootPath);

      // Should have imported procedure declarations
      final importedDecls =
          module.procDeclarations.where((d) => d.imported).toList();
      expect(importedDecls, isNotEmpty,
          reason: 'boot.glp should have imported procedure declarations');

      // Should have procedures (bare clauses for play1-7, network2/3, etc.)
      expect(module.procedures, isNotEmpty,
          reason: 'boot.glp should have orchestration procedures');
    });
  });
}
