/// End-to-end module execution tests
/// Tests the full RPC flow: Distribute → serve_import → dispatcher → execute → result

import 'dart:async';
import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/analyzer.dart';
import 'package:glp_runtime/compiler/codegen.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/compiler/ast.dart' show Module, Program;
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/loaded_module.dart';
import 'package:glp_runtime/runtime/module_registry.dart';
import 'package:glp_runtime/runtime/module_runtime.dart';
import 'package:glp_runtime/runtime/import_vector.dart';
import 'package:glp_runtime/runtime/serve_import.dart';
import 'package:glp_runtime/runtime/dispatcher.dart';
import 'package:glp_runtime/runtime/module_messages.dart';

/// Compile module source to LoadedModule
LoadedModule compileModule(String source, String name) {
  final lexer = Lexer(source);
  final tokens = lexer.tokenize();
  final parser = Parser(tokens);
  final module = parser.parseModule();

  final analyzer = Analyzer();
  final annotatedProgram = analyzer.analyze(module.toProgram());
  final generator = CodeGenerator();
  final bytecode = generator.generate(annotatedProgram);

  final exports = <String>{};
  for (final exportDecl in module.exports) {
    for (final procRef in exportDecl.exports) {
      exports.add(procRef.signature);
    }
  }

  return LoadedModule(
    name: name,
    bytecode: bytecode,
    exports: exports,
    imports: module.importedModules,
  );
}

// Extension to convert Module AST to Program for analyzer
extension ModuleToProgram on Module {
  Program toProgram() {
    return Program(procedures, line, column);
  }
}

void main() {
  group('End-to-End Module Execution', () {
    late GlpRuntime rt;
    late FcpModuleRegistry registry;

    setUp(() {
      rt = GlpRuntime();
      registry = FcpModuleRegistry();
    });

    /// Load stdlib bytecode for arithmetic operations
    BytecodeProgram loadStdlib() {
      final stdlibSource = File('/home/user/GLP/glp/stdlib/assign.glp').readAsStringSync();
      final compiler = GlpCompiler();
      return compiler.compile(stdlibSource);
    }

    test('direct RPC: send double(5, W) message, math binds W = 10', () async {
      // Create math module - fact binds writer arg to constant
      final mathSource = '''
-module(math).
-export([double/2]).

double(5, 10).
''';
      final mathModule = compileModule(mathSource, 'math');
      registry.register(mathModule);

      // Create result variable (writer for RPC output)
      final resultVarId = rt.heap.allocateVariable();
      final resultWriter = VarRef(resultVarId, isReader: false);
      print('[TEST] Created result writer W$resultVarId');

      // Manually create args for double(5, W) - constant 5 and writer W
      final constVarId = rt.heap.allocateVariable();
      rt.heap.bindWriterConst(constVarId, 5);
      final constArg = VarRef(constVarId, isReader: true);  // Reader bound to 5

      // Start dispatcher for math module with proper executor
      final mathDispatcher = startDispatcher(
        module: mathModule,
        executor: (message, module) async {
          print('[DISPATCH] Received: ${message.signature} args=${message.args}');

          final signature = message.signature;
          final entryPc = module.getEntryPoint(signature);
          if (entryPc == null) {
            print('[DISPATCH] No entry point for $signature');
            return;
          }
          print('[DISPATCH] Entry at PC $entryPc');

          // Create env with message args
          final env = CallEnv(
            args: {for (int i = 0; i < message.args.length; i++) i: message.args[i]},
          );

          final goalId = rt.nextGoalId++;
          rt.setGoalEnv(goalId, env);
          rt.setGoalProgram(goalId, module.bytecode);
          rt.setGoalModuleContext(goalId, ModuleGoalContext(
            module: module,
            registry: registry,
          ));

          final runner = BytecodeRunner(module.bytecode);
          final ctx = RunnerContext(
            rt: rt,
            goalId: goalId,
            kappa: entryPc,
            env: env,
            debugOutput: true,
            moduleContext: ModuleGoalContext(module: module, registry: registry),
          );
          print('[DISPATCH] Executing ${message.signature}');
          runner.runWithStatus(ctx);
          print('[DISPATCH] Done');
        },
        onError: (error) => print('[DISPATCH ERROR] $error'),
      );
      print('[TEST] Dispatcher started');

      // Directly send message to math module's input
      final message = ExportMessage.trust(
        sourceModule: 'test',
        functor: 'double',
        arity: 2,
        args: [constArg, resultWriter],  // Pass WRITER for second arg
      );
      print('[TEST] Sending message: double/2 args=[$constArg, $resultWriter]');
      mathModule.inputSink.add(message);

      // Allow async processing
      await Future.delayed(Duration(milliseconds: 100));

      // Check result
      final resultValue = rt.heap.valueOfWriter(resultVarId);
      print('[TEST] Result W$resultVarId = $resultValue');

      // Cleanup
      mathDispatcher.stop();

      // Verify
      expect(resultValue, isNotNull, reason: 'Writer should be bound');
      expect(resultValue, isA<ConstTerm>(), reason: 'Should be ConstTerm(10)');
      expect((resultValue as ConstTerm).value, equals(10), reason: 'Should be 10');
    });

    test('recursive RPC: factorial(5, W) → W = 120', () async {
      // Load stdlib for := arithmetic
      final stdlib = loadStdlib();

      // Create math module with factorial
      // Using SRSW-compliant pattern: F? in head (reader), F := in body (writer)
      final mathSource = '''
-module(math).
-export([factorial/2]).

factorial(0, 1).
factorial(N, F?) :- N? > 0 | N1 := N? - 1, factorial(N1?, F1), F := N? * F1?.
''';

      // Compile math module
      final lexer = Lexer(mathSource);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final module = parser.parseModule();

      final analyzer = Analyzer();
      final annotatedProgram = analyzer.analyze(module.toProgram());
      final generator = CodeGenerator();
      var mathBytecode = generator.generate(annotatedProgram);

      // Merge with stdlib so := works
      mathBytecode = mathBytecode.merge(stdlib);
      print('[TEST] Math module compiled with ${mathBytecode.ops.length} ops');

      final exports = <String>{'factorial/2'};
      final mathModule = LoadedModule(
        name: 'math',
        bytecode: mathBytecode,
        exports: exports,
        imports: [],
      );
      registry.register(mathModule);

      // Create result variable (writer for RPC output)
      final resultVarId = rt.heap.allocateVariable();
      final resultWriter = VarRef(resultVarId, isReader: false);
      print('[TEST] Created result writer W$resultVarId');

      // Create arg for N=5
      final nVarId = rt.heap.allocateVariable();
      rt.heap.bindWriterConst(nVarId, 5);
      final nArg = VarRef(nVarId, isReader: true);  // Reader bound to 5

      // Track spawned goals for recursive execution
      final spawnedGoals = <int>[];

      // Start dispatcher with recursive execution support
      final mathDispatcher = startDispatcher(
        module: mathModule,
        executor: (message, module) async {
          print('[DISPATCH] Received: ${message.signature} args=${message.args}');

          final signature = message.signature;
          final entryPc = module.getEntryPoint(signature);
          if (entryPc == null) {
            print('[DISPATCH] No entry point for $signature');
            return;
          }

          // Create env with message args
          final env = CallEnv(
            args: {for (int i = 0; i < message.args.length; i++) i: message.args[i]},
          );

          final goalId = rt.nextGoalId++;
          spawnedGoals.add(goalId);
          rt.setGoalEnv(goalId, env);
          rt.setGoalProgram(goalId, module.bytecode);
          rt.setGoalModuleContext(goalId, ModuleGoalContext(
            module: module,
            registry: registry,
          ));

          final runner = BytecodeRunner(module.bytecode);
          final ctx = RunnerContext(
            rt: rt,
            goalId: goalId,
            kappa: entryPc,
            env: env,
            debugOutput: false,  // Less verbose for recursion
            moduleContext: ModuleGoalContext(module: module, registry: registry),
          );
          runner.runWithStatus(ctx);
        },
        onError: (error) => print('[DISPATCH ERROR] $error'),
      );
      print('[TEST] Dispatcher started');

      // Send factorial(5, W) message
      final message = ExportMessage.trust(
        sourceModule: 'test',
        functor: 'factorial',
        arity: 2,
        args: [nArg, resultWriter],
      );
      print('[TEST] Sending: factorial/2 args=[$nArg, $resultWriter]');
      mathModule.inputSink.add(message);

      // Allow async processing - factorial(5) needs multiple recursive calls
      // Give enough time for recursion to complete
      await Future.delayed(Duration(milliseconds: 500));

      // Run scheduler to process any spawned goals
      print('[TEST] Processing ${rt.gq.length} queued goals...');
      var iterations = 0;
      while (!rt.gq.isEmpty && iterations < 100) {
        final goalRef = rt.gq.dequeue();
        if (goalRef == null) break;
        final goalId = goalRef.id;
        final env = rt.getGoalEnv(goalId);
        final prog = rt.getGoalProgram(goalId);
        if (env != null && prog != null) {
          final runner = BytecodeRunner(prog as BytecodeProgram);
          final ctx = RunnerContext(
            rt: rt,
            goalId: goalId,
            kappa: goalRef.pc,  // Resume at saved PC
            env: env,
            debugOutput: false,
            moduleContext: rt.getGoalModuleContext(goalId),
          );
          runner.runWithStatus(ctx);
        }
        iterations++;
      }
      print('[TEST] Processed $iterations goals');

      // Check result
      final resultValue = rt.heap.valueOfWriter(resultVarId);
      print('[TEST] Result W$resultVarId = $resultValue');

      // Cleanup
      mathDispatcher.stop();

      // Verify factorial(5) = 120
      expect(resultValue, isNotNull, reason: 'Writer should be bound after factorial(5)');
      expect(resultValue, isA<ConstTerm>(), reason: 'Should be ConstTerm(120)');
      expect((resultValue as ConstTerm).value, equals(120), reason: 'factorial(5) should be 120');
    });
  });
}
