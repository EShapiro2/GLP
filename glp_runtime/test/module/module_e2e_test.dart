/// End-to-end module execution tests
/// Tests the full RPC flow: Distribute → serve_import → dispatcher → execute → result

import 'dart:async';
import 'package:test/test.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/analyzer.dart';
import 'package:glp_runtime/compiler/codegen.dart';
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
  });
}
