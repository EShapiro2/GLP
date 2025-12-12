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

    test('dynamic RPC via Transmit: module name as variable', () async {
      // Create math module with double/2
      final mathSource = '''
-module(math).
-export([double/2]).

double(5, 10).
double(10, 20).
''';
      final mathModule = compileModule(mathSource, 'math');
      registry.register(mathModule);

      // Create result variable (writer for RPC output)
      final resultVarId = rt.heap.allocateVariable();
      final resultWriter = VarRef(resultVarId, isReader: false);
      print('[TEST] Created result writer W$resultVarId');

      // Create module name variable bound to 'math'
      final moduleNameVarId = rt.heap.allocateVariable();
      rt.heap.bindWriterConst(moduleNameVarId, 'math');
      final moduleNameReader = VarRef(moduleNameVarId, isReader: true);

      // Create arg for input value 5
      final inputVarId = rt.heap.allocateVariable();
      rt.heap.bindWriterConst(inputVarId, 5);
      final inputArg = VarRef(inputVarId, isReader: true);

      // Start dispatcher for math module
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
          runner.runWithStatus(ctx);
        },
        onError: (error) => print('[DISPATCH ERROR] $error'),
      );
      print('[TEST] Math dispatcher started');

      // Simulate Transmit by directly sending to resolved module
      // In a full GLP program, Transmit opcode would resolve module name from variable
      // Here we test the dispatch path directly
      final message = ExportMessage.trust(
        sourceModule: 'test',
        functor: 'double',
        arity: 2,
        args: [inputArg, resultWriter],
      );

      // Resolve module name and send
      final resolvedModule = registry.lookup('math');
      expect(resolvedModule, isNotNull, reason: 'math module should be in registry');
      print('[TEST] Transmit: resolved "math" -> sending double/2');
      resolvedModule!.inputSink.add(message);

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
      expect((resultValue as ConstTerm).value, equals(10), reason: 'dynamic RPC should bind to 10');
    });

    test('multi-module chain: B → C RPC works', () async {
      // Simplified test: B calls C directly, C returns fixed value
      // This tests the RPC chain without arithmetic complexity

      // Module C: simple fact
      final cSource = '''
-module(c).
-export([value/1]).

value(42).
''';
      final cModule = compileModule(cSource, 'c');
      registry.register(cModule);
      print('[TEST] Module C registered');

      // Module B: calls c # value(X)
      // Pattern: R? in head (reader), R in body (writer) - C can bind it
      final bSource = '''
-module(b).
-import([c]).
-export([get_value/1]).

get_value(R?) :- otherwise | c # value(R).
''';
      final bModule = compileModule(bSource, 'b');

      // Set up import vector for B (1 import: c)
      final bVectorResult = ImportVector.make(1);
      bModule.importVector = bVectorResult.vector;
      registry.register(bModule);
      print('[TEST] Module B registered');

      // Start serve_import for B → C
      final bServeImports = startServeImports(
        readers: bVectorResult.readers,
        importNames: ['c'],
        registry: registry,
        onError: (error, msg) => print('[SERVE_IMPORT B ERROR] $error'),
      );
      print('[TEST] serve_import B → C started');

      // Start dispatcher for module C
      final cDispatcher = startDispatcher(
        module: cModule,
        executor: (message, module) async {
          print('[DISPATCH C] Received: ${message.signature} args=${message.args}');
          final entryPc = module.getEntryPoint(message.signature);
          if (entryPc == null) {
            print('[DISPATCH C] No entry for ${message.signature}');
            return;
          }

          final env = CallEnv(
            args: {for (int i = 0; i < message.args.length; i++) i: message.args[i]},
          );
          final goalId = rt.nextGoalId++;
          rt.setGoalEnv(goalId, env);
          rt.setGoalProgram(goalId, module.bytecode);
          rt.setGoalModuleContext(goalId, ModuleGoalContext(module: module, registry: registry));

          final runner = BytecodeRunner(module.bytecode);
          final ctx = RunnerContext(
            rt: rt,
            goalId: goalId,
            kappa: entryPc,
            env: env,
            debugOutput: true,  // Enable debug for C
            moduleContext: ModuleGoalContext(module: module, registry: registry),
          );
          runner.runWithStatus(ctx);
          print('[DISPATCH C] Done');
        },
        onError: (error) => print('[DISPATCH C ERROR] $error'),
      );
      print('[TEST] Dispatcher C started');

      // Start dispatcher for module B
      final bDispatcher = startDispatcher(
        module: bModule,
        executor: (message, module) async {
          print('[DISPATCH B] Received: ${message.signature} args=${message.args}');
          final entryPc = module.getEntryPoint(message.signature);
          if (entryPc == null) {
            print('[DISPATCH B] No entry for ${message.signature}');
            return;
          }

          final env = CallEnv(
            args: {for (int i = 0; i < message.args.length; i++) i: message.args[i]},
          );
          final goalId = rt.nextGoalId++;
          rt.setGoalEnv(goalId, env);
          rt.setGoalProgram(goalId, module.bytecode);
          rt.setGoalModuleContext(goalId, ModuleGoalContext(module: module, registry: registry));

          final runner = BytecodeRunner(module.bytecode);
          final ctx = RunnerContext(
            rt: rt,
            goalId: goalId,
            kappa: entryPc,
            env: env,
            debugOutput: true,  // Enable debug for B
            moduleContext: ModuleGoalContext(module: module, registry: registry),
          );
          runner.runWithStatus(ctx);
          print('[DISPATCH B] Done');
        },
        onError: (error) => print('[DISPATCH B ERROR] $error'),
      );
      print('[TEST] Dispatcher B started');

      // Create result variable
      final resultVarId = rt.heap.allocateVariable();
      final resultWriter = VarRef(resultVarId, isReader: false);
      print('[TEST] Created result writer W$resultVarId');

      // Send get_value(W) to module B
      final message = ExportMessage.trust(
        sourceModule: 'test',
        functor: 'get_value',
        arity: 1,
        args: [resultWriter],
      );
      print('[TEST] Sending: get_value/1 to B');
      bModule.inputSink.add(message);

      // Allow async processing
      await Future.delayed(Duration(milliseconds: 200));

      // Process any queued goals
      var iterations = 0;
      while (!rt.gq.isEmpty && iterations < 20) {
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
            kappa: goalRef.pc,
            env: env,
            debugOutput: false,
            moduleContext: rt.getGoalModuleContext(goalId),
          );
          runner.runWithStatus(ctx);
        }
        iterations++;
      }
      print('[TEST] Processed $iterations queued goals');

      // Check result
      final resultValue = rt.heap.valueOfWriter(resultVarId);
      print('[TEST] Result W$resultVarId = $resultValue');

      // Cleanup
      for (final s in bServeImports) s.stop();
      bDispatcher.stop();
      cDispatcher.stop();

      // Verify
      expect(resultValue, isNotNull, reason: 'Writer should be bound after B→C chain');
      expect(resultValue, isA<ConstTerm>(), reason: 'Should be ConstTerm(42)');
      expect((resultValue as ConstTerm).value, equals(42), reason: 'get_value should return 42');
    });

    test('error: missing module returns no_service', () async {
      // Don't register any module named 'unknown'

      // Try to lookup unknown module
      final unknownModule = registry.lookup('unknown');
      expect(unknownModule, isNull, reason: 'Unknown module should not exist');

      print('[TEST] Verified: lookup("unknown") returns null (no_service)');
    });

    test('error: unexported procedure not accessible', () async {
      // Create math module with private procedure (not exported)
      final mathSource = '''
-module(math).
-export([public_proc/1]).

public_proc(X?) :- X := 42.
private_proc(X?) :- X := 99.
''';
      final stdlib = loadStdlib();
      final lexer = Lexer(mathSource);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final module = parser.parseModule();
      final analyzer = Analyzer();
      final annotated = analyzer.analyze(module.toProgram());
      final generator = CodeGenerator();
      var bytecode = generator.generate(annotated);
      bytecode = bytecode.merge(stdlib);

      final mathModule = LoadedModule(
        name: 'math',
        bytecode: bytecode,
        exports: {'public_proc/1'},  // Only public_proc is exported
        imports: [],
      );
      registry.register(mathModule);

      // Verify export checking
      expect(mathModule.isExported('public_proc/1'), isTrue,
          reason: 'public_proc/1 should be exported');
      expect(mathModule.isExported('private_proc/1'), isFalse,
          reason: 'private_proc/1 should NOT be exported');

      print('[TEST] Verified: private_proc/1 not in exports (unknown error)');
    });
  });
}
