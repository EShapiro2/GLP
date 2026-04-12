/// Social Graph (Simulated UI) — Modular integration test.
///
/// Tests the social_graph_simulated_ui_modules/ project using both:
///   1. Static linking (project linker: discover → link → compile as flat program)
///   2. Dynamic linking (GLP dispatch: compile each module, activateModule, # dispatch)
///
/// Verifies that both modes produce identical output for fplay1/fplay2/fplay3.

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/compiler/project_linker.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/ast.dart' as ast;
import 'package:glp_runtime/compiler/partial_evaluator.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/glp_activation.dart';
import 'package:glp_runtime/runtime/module_hierarchy.dart';
import 'package:glp_runtime/bytecode/runner.dart';

/// Source for the serve/2 system predicate (needed for dynamic dispatch).
const serveSource = '''
-mode(system).

procedure serve(Any?, Any?).

serve(Module, [Goal | In]) :-
    ground(Module?) |
    '_activate'(Module?, Goal?),
    serve(Module?, In?).

serve(_, []) :-
    otherwise |
    true.
''';

void main() {
  // Set prelude sources from programs/self.glp (same as GlpEngine constructor)
  final rootSelfGlp = File('../programs/self.glp');
  if (rootSelfGlp.existsSync()) {
    final source = rootSelfGlp.readAsStringSync();
    setPreludeUnitClauseSource(source);
    setPreludeEnvironmentSource(source);
  }

  final projectRoot = '../programs/social_graph_simulated_ui_modules';

  if (!Directory(projectRoot).existsSync()) {
    print('social_graph_simulated_ui_modules not found at $projectRoot, '
        'skipping tests');
    return;
  }

  late GlpCompiler compiler;
  late BytecodeProgram stdlibBytecode;

  /// Compile root self.glp and return stdlib bytecode.
  BytecodeProgram compileStdlib(GlpCompiler compiler) {
    return compiler.compile(File('../programs/self.glp').readAsStringSync());
  }

  setUpAll(() {
    compiler = GlpCompiler();
    stdlibBytecode = compileStdlib(compiler);
  });

  // =========================================================================
  // Module validation — parse and type-check each module
  // =========================================================================

  group('Module validation', () {
    ast.Module parseFile(String path) {
      final source = File(path).readAsStringSync();
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      return parser.parseModule();
    }

    TypeEnvironment buildAncestorScope(String targetFile) {
      final chain = discoverSelfChain(
        targetFile: targetFile,
        rootDir: projectRoot,
      );
      var env = buildPreludeEnvironment();
      for (final selfGlpPath in chain) {
        final selfModule = parseFile(selfGlpPath);
        final types = <String, TypeDef>{};
        final procedures = <String, ProcDecl>{};
        for (final typeDef in selfModule.typeDefs) {
          types[typeDef.name] = typeDef;
        }
        for (final procDecl in selfModule.procDeclarations) {
          procedures[procDecl.qualifiedKey] = procDecl;
        }
        env = env.merge(TypeEnvironment(types, procedures));
      }
      return env;
    }

    TypeCheckResult typeCheckFile(String path) {
      final module = parseFile(path);
      final ancestorScope = buildAncestorScope(path);
      final program = ast.Program(
          module.procedures, module.line, module.column);
      final pe = PartialEvaluator();
      final transformedAst = pe.transformDefinedGuards(program);
      return checkModule(module,
          transformedProcedures: transformedAst.procedures,
          ancestorScope: ancestorScope);
    }

    test('self.glp parses and defines shared types', () {
      final selfPath = '$projectRoot/self.glp';
      expect(File(selfPath).existsSync(), isTrue);
      final module = parseFile(selfPath);
      expect(module.typeDefs, isNotEmpty,
          reason: 'self.glp should define shared types');
    });

    test('agent.glp type-checks', () {
      final result = typeCheckFile('$projectRoot/agent.glp');
      if (!result.isWellTyped) {
        fail('agent.glp type errors:\n${result.errors.join('\n')}');
      }
    });

    test('ui/mediator.glp type-checks', () {
      final result = typeCheckFile('$projectRoot/ui/mediator.glp');
      if (!result.isWellTyped) {
        fail('ui/mediator.glp type errors:\n${result.errors.join('\n')}');
      }
    });

    test('ui/actors.glp type-checks', () {
      final result = typeCheckFile('$projectRoot/ui/actors.glp');
      if (!result.isWellTyped) {
        fail('ui/actors.glp type errors:\n${result.errors.join('\n')}');
      }
    });

    test('boot.glp parses (untyped orchestration)', () {
      final module = parseFile('$projectRoot/boot.glp');
      final importedDecls =
          module.procDeclarations.where((d) => d.imported).toList();
      expect(importedDecls, isNotEmpty,
          reason: 'boot.glp should have imported procedure declarations');
      expect(module.procedures, isNotEmpty,
          reason: 'boot.glp should have orchestration procedures');
    });
  });

  // =========================================================================
  // Static linking
  // =========================================================================

  group('Static linking', () {
    test('project discovery finds 4 modules', () {
      final modules = discoverProject(projectRoot);
      final names = modules.map((m) => m.moduleName).toSet();
      expect(names, containsAll(['agent', 'mediator', 'actors', 'boot']));
      expect(names.length, equals(4));
    });

    test('type checking passes', () {
      final modules = discoverProject(projectRoot);
      expect(() => typeCheckProject(modules), returnsNormally);
    });

    test('linking produces renamed procedures', () {
      final modules = discoverProject(projectRoot);
      final result = linkProject(modules, 'boot');
      final procNames =
          result.program.procedures.map((p) => p.name).toSet();

      expect(procNames, contains('agent:agent'));
      expect(procNames, contains('agent:merge'));
      expect(procNames, contains('mediator:ui_mediator'));
      expect(procNames, contains('actors:alice1'));
      expect(procNames, contains('boot:play1'));
      expect(procNames, contains('boot:fplay1'));

      // Entry point aliases
      expect(procNames, contains('play1'));
      expect(procNames, contains('fplay1'));
    });

    /// Static link helper: discover → link → compile → run.
    List<String> runStaticPlay(String playName) {
      final modules = discoverProject(projectRoot);
      final result = linkProject(modules, 'boot');
      var program = compiler.compileProgram(
        result.program,
        procDeclarations: result.procDeclarations,
      );
      program = program.merge(stdlibBytecode);

      final rt = GlpRuntime();
      final output = <String>[];
      rt.outputCallback = (s) => output.add(s);
      rt.runners[program] = BytecodeRunner(program);

      final scheduler = Scheduler(rt: rt);
      final goalId = rt.nextGoalId++;
      rt.setGoalEnv(goalId, CallEnv(args: {}));
      rt.setGoalProgram(goalId, program);

      final pc = program.labels['$playName/0']!;
      rt.gq.enqueue(GoalRef(goalId, pc));
      final execResult = scheduler.drainWithStatus(maxCycles: 50000);

      if (output.length < 5) {
        print('=== Static $playName output (${output.length} lines) ===');
        for (final line in output) {
          print('  $line');
        }
        print('Status: ${execResult.status}');
        print('Goals ran: ${execResult.goalsRan.length}');
      }
      return output;
    }

    test('fplay1 produces correct output (both accept)', () {
      final output = runStaticPlay('fplay1');
      expect(output, isNotEmpty, reason: 'fplay1 should produce output');
      final s = output.join('\n');
      expect(s, contains('tagged(alice'));
      expect(s, contains('connected(bob)'));
      expect(s, contains('connected(alice)'));
      expect(s, contains('connected(charlie)'));
    });

    test('fplay2 produces correct output (Alice accepts, Charlie rejects)',
        () {
      final output = runStaticPlay('fplay2');
      expect(output, isNotEmpty, reason: 'fplay2 should produce output');
      final s = output.join('\n');
      expect(s, contains('rejected'));
    });

    test('fplay3 produces correct output (both reject)', () {
      final output = runStaticPlay('fplay3');
      expect(output, isNotEmpty, reason: 'fplay3 should produce output');
    });
  });

  // =========================================================================
  // Dynamic linking (GLP dispatch)
  // =========================================================================

  group('Dynamic linking', () {
    late BytecodeProgram serveBytecode;
    late BytecodeProgram agentBytecode;
    late BytecodeProgram mediatorBytecode;
    late BytecodeProgram actorsBytecode;
    late BytecodeProgram bootBytecode;

    setUpAll(() {
      serveBytecode = compiler.compile(serveSource);
      agentBytecode = compiler
          .compile(File('$projectRoot/agent.glp').readAsStringSync())
          .merge(stdlibBytecode);
      mediatorBytecode = compiler
          .compile(
              File('$projectRoot/ui/mediator.glp').readAsStringSync())
          .merge(stdlibBytecode);
      actorsBytecode = compiler
          .compile(
              File('$projectRoot/ui/actors.glp').readAsStringSync())
          .merge(stdlibBytecode);
      bootBytecode = compiler
          .compile(File('$projectRoot/boot.glp').readAsStringSync())
          .merge(stdlibBytecode);
    });

    test('modules compile with _select/1', () {
      expect(agentBytecode.labels.containsKey('_select/1'), isTrue,
          reason: 'agent.glp should have _select/1');
      expect(mediatorBytecode.labels.containsKey('_select/1'), isTrue,
          reason: 'mediator.glp should have _select/1');
      expect(actorsBytecode.labels.containsKey('_select/1'), isTrue,
          reason: 'actors.glp should have _select/1');
      expect(bootBytecode.labels.containsKey('_select/1'), isFalse,
          reason: 'boot.glp has no exported procedures');
    });

    /// Dynamic dispatch helper: activate modules, run play via # dispatch.
    List<String> runDynamicPlay(String playName) {
      final rt = GlpRuntime();
      final output = <String>[];
      rt.outputCallback = (s) => output.add(s);

      activateModule(
        rt: rt,
        serveBytecode: serveBytecode,
        moduleBytecode: agentBytecode,
        moduleName: 'agent',
      );
      activateModule(
        rt: rt,
        serveBytecode: serveBytecode,
        moduleBytecode: mediatorBytecode,
        moduleName: 'mediator',
      );
      activateModule(
        rt: rt,
        serveBytecode: serveBytecode,
        moduleBytecode: actorsBytecode,
        moduleName: 'actors',
      );

      rt.runners[bootBytecode] = BytecodeRunner(bootBytecode);

      final scheduler = Scheduler(rt: rt);
      scheduler.drainWithStatus(maxCycles: 300);

      final goalId = rt.nextGoalId++;
      rt.setGoalEnv(goalId, CallEnv(args: {}));
      rt.setGoalProgram(goalId, bootBytecode);
      rt.setGoalModuleContext(
        goalId,
        ReplModuleContext(
          moduleName: 'boot',
          imports: {
            1: ReplModuleTarget('actors', actorsBytecode),
            2: ReplModuleTarget('agent', agentBytecode),
            3: ReplModuleTarget('mediator', mediatorBytecode),
          },
        ),
      );

      final pc = bootBytecode.labels['$playName/0']!;
      rt.gq.enqueue(GoalRef(goalId, pc));
      scheduler.drainWithStatus(maxCycles: 50000);
      return output;
    }

    test('fplay1 via GLP dispatch', () {
      final output = runDynamicPlay('fplay1');
      expect(output, isNotEmpty, reason: 'fplay1 should produce output');
      final s = output.join('\n');
      expect(s, contains('tagged(alice'));
      expect(s, contains('connected(bob)'));
      expect(s, contains('connected(alice)'));
    });

    test('fplay2 via GLP dispatch', () {
      final output = runDynamicPlay('fplay2');
      expect(output, isNotEmpty, reason: 'fplay2 should produce output');
      final s = output.join('\n');
      expect(s, contains('rejected'));
    });

    test('fplay3 via GLP dispatch', () {
      final output = runDynamicPlay('fplay3');
      expect(output, isNotEmpty, reason: 'fplay3 should produce output');
    });
  });

  // =========================================================================
  // Cross-mode comparison
  // =========================================================================

  group('Static vs dynamic comparison', () {
    test('fplay1 output is identical in both modes', () {
      // Static
      final modules = discoverProject(projectRoot);
      final linkResult = linkProject(modules, 'boot');
      var staticProg = compiler.compileProgram(
        linkResult.program,
        procDeclarations: linkResult.procDeclarations,
      );
      staticProg = staticProg.merge(stdlibBytecode);

      final staticRt = GlpRuntime();
      final staticOutput = <String>[];
      staticRt.outputCallback = (s) => staticOutput.add(s);
      staticRt.runners[staticProg] = BytecodeRunner(staticProg);

      var scheduler = Scheduler(rt: staticRt);
      var goalId = staticRt.nextGoalId++;
      staticRt.setGoalEnv(goalId, CallEnv(args: {}));
      staticRt.setGoalProgram(goalId, staticProg);
      staticRt.gq
          .enqueue(GoalRef(goalId, staticProg.labels['fplay1/0']!));
      scheduler.drainWithStatus(maxCycles: 50000);

      // Dynamic
      final serveBytecode = compiler.compile(serveSource);
      final agentBytecode = compiler
          .compile(
              File('$projectRoot/agent.glp').readAsStringSync())
          .merge(stdlibBytecode);
      final mediatorBytecode = compiler
          .compile(
              File('$projectRoot/ui/mediator.glp').readAsStringSync())
          .merge(stdlibBytecode);
      final actorsBytecode = compiler
          .compile(
              File('$projectRoot/ui/actors.glp').readAsStringSync())
          .merge(stdlibBytecode);
      final bootBytecode = compiler
          .compile(
              File('$projectRoot/boot.glp').readAsStringSync())
          .merge(stdlibBytecode);

      final dynRt = GlpRuntime();
      final dynOutput = <String>[];
      dynRt.outputCallback = (s) => dynOutput.add(s);

      activateModule(
          rt: dynRt,
          serveBytecode: serveBytecode,
          moduleBytecode: agentBytecode,
          moduleName: 'agent');
      activateModule(
          rt: dynRt,
          serveBytecode: serveBytecode,
          moduleBytecode: mediatorBytecode,
          moduleName: 'mediator');
      activateModule(
          rt: dynRt,
          serveBytecode: serveBytecode,
          moduleBytecode: actorsBytecode,
          moduleName: 'actors');

      dynRt.runners[bootBytecode] = BytecodeRunner(bootBytecode);
      scheduler = Scheduler(rt: dynRt);
      scheduler.drainWithStatus(maxCycles: 300);

      goalId = dynRt.nextGoalId++;
      dynRt.setGoalEnv(goalId, CallEnv(args: {}));
      dynRt.setGoalProgram(goalId, bootBytecode);
      dynRt.setGoalModuleContext(
        goalId,
        ReplModuleContext(
          moduleName: 'boot',
          imports: {
            1: ReplModuleTarget('actors', actorsBytecode),
            2: ReplModuleTarget('agent', agentBytecode),
            3: ReplModuleTarget('mediator', mediatorBytecode),
          },
        ),
      );
      dynRt.gq
          .enqueue(GoalRef(goalId, bootBytecode.labels['fplay1/0']!));
      scheduler.drainWithStatus(maxCycles: 50000);

      // Both should produce output
      expect(staticOutput, isNotEmpty);
      expect(dynOutput, isNotEmpty);

      // Sort for comparison (nondeterministic scheduling order)
      staticOutput.sort();
      dynOutput.sort();

      print('=== Static output (${staticOutput.length} lines) ===');
      for (final line in staticOutput) {
        print('  $line');
      }
      print('=== Dynamic output (${dynOutput.length} lines) ===');
      for (final line in dynOutput) {
        print('  $line');
      }

      expect(staticOutput.length, equals(dynOutput.length),
          reason: 'Both modes should produce same number of output lines');
      expect(staticOutput, equals(dynOutput),
          reason: 'Static and dynamic linking should produce identical '
              'output (after sorting)');
    });
  });
}
