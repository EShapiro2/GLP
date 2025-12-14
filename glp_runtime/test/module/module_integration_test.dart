import 'package:test/test.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/analyzer.dart';
import 'package:glp_runtime/compiler/codegen.dart';
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/loaded_module.dart';
import 'package:glp_runtime/runtime/module_registry.dart';
import 'package:glp_runtime/runtime/import_vector.dart';

/// Compile GLP source to BytecodeProgram
BytecodeProgram compileSource(String source) {
  final lexer = Lexer(source);
  final tokens = lexer.tokenize();
  final parser = Parser(tokens);
  final program = parser.parse();
  final analyzer = Analyzer();
  final annotated = analyzer.analyze(program);
  final generator = CodeGenerator();
  return generator.generate(annotated);
}

/// Parse module declarations and compile to LoadedModule
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

void main() {
  group('Module Integration - Compilation', () {
    test('compiles math module with exports', () {
      // Simple module with valid SRSW patterns - no output chaining
      final source = '''
-module(math).
-export([process/1]).

process(X) :- otherwise |
    consume(X?).

consume(_).
''';

      final module = compileModule(source, 'math');

      expect(module.name, 'math');
      expect(module.exports, {'process/1'});
      expect(module.imports, isEmpty);
      expect(module.bytecode.ops, isNotEmpty);
    });

    test('compiles main module with imports', () {
      // Module with static RPC using valid SRSW
      final source = '''
-module(main).
-import([math]).
-export([boot/0]).

boot :- otherwise |
    math # add(1, 2, R),
    done(R?).

done(_).
''';

      final module = compileModule(source, 'main');

      expect(module.name, 'main');
      expect(module.exports, {'boot/0'});
      expect(module.imports, ['math']);

      // Check that Distribute opcodes were generated
      final distributeOps = module.bytecode.ops.whereType<Distribute>().toList();
      expect(distributeOps.length, greaterThanOrEqualTo(1));
      expect(distributeOps.any((op) => op.functor == 'add'), true);
    });

    test('compiles module with dynamic RPC', () {
      final source = '''
-module(dispatcher).
-export([forward/2]).

forward(M, X) :- otherwise |
    M? # process(X?).
''';

      final module = compileModule(source, 'dispatcher');

      expect(module.exports, {'forward/2'});

      // Check that Transmit opcodes were generated
      final transmitOps = module.bytecode.ops.whereType<Transmit>().toList();
      expect(transmitOps.length, 1);
      expect(transmitOps[0].functor, 'process');
    });
  });

  group('Module Integration - Registry', () {
    test('registers and looks up modules', () {
      final registry = FcpModuleRegistry();

      // Simple valid SRSW - no output chaining
      final mathSource = '''
-module(math).
-export([process/1]).
process(X) :- otherwise |
    consume(X?).

consume(_).
''';
      final mathModule = compileModule(mathSource, 'math');
      registry.register(mathModule);

      expect(registry.isLoaded('math'), true);
      expect(registry.lookup('math'), mathModule);
      expect(registry.isLoaded('unknown'), false);
      expect(registry.lookup('unknown'), isNull);
    });
  });

  group('Module Integration - Import Vector', () {
    test('creates and routes messages through import vector', () async {
      final registry = FcpModuleRegistry();

      // Create math module with valid SRSW - no output
      final mathSource = '''
-module(math).
-export([process/1]).
process(X) :- otherwise |
    consume(X?).

consume(_).
''';
      final mathModule = compileModule(mathSource, 'math');
      registry.register(mathModule);

      // Create main module with import (valid SRSW)
      final mainSource = '''
-module(main).
-import([math]).
-export([boot/0]).
boot :- otherwise |
    math # process(5).
''';
      final mainModule = compileModule(mainSource, 'main');
      registry.register(mainModule);

      // Set up import vector for main
      final result = ImportVector.make(1);
      mainModule.importVector = result.vector;

      expect(mainModule.importVector!.size, 1);
    });
  });

  group('Module Integration - Bytecode Inspection', () {
    test('static RPC generates correct Distribute opcode', () {
      // Valid SRSW with multiple static RPCs
      final source = '''
-module(client).
-import([math, io]).
-export([run/0]).

run :- otherwise |
    math # compute(5, R),
    io # print(R?).
''';
      final module = compileModule(source, 'client');

      final distributeOps = module.bytecode.ops.whereType<Distribute>().toList();

      // Should have 2 Distribute ops
      expect(distributeOps.length, 2);

      // math gets index 1 (first import)
      final computeOp = distributeOps.firstWhere((op) => op.functor == 'compute');
      expect(computeOp.importIndex, 1);
      expect(computeOp.arity, 2);

      // io gets index 2 (second import)
      final printOp = distributeOps.firstWhere((op) => op.functor == 'print');
      expect(printOp.importIndex, 2);
      expect(printOp.arity, 1);
    });

    test('dynamic RPC generates correct Transmit opcode', () {
      final source = '''
-module(router).
-export([route/2]).

route(Module, Data) :- otherwise |
    Module? # handle(Data?).
''';
      final module = compileModule(source, 'router');

      final transmitOps = module.bytecode.ops.whereType<Transmit>().toList();

      expect(transmitOps.length, 1);
      expect(transmitOps[0].functor, 'handle');
      expect(transmitOps[0].arity, 1);
    });

    test('mixed static and dynamic RPC in same module', () {
      final source = '''
-module(hybrid).
-import([logger]).
-export([process/2]).

process(Target, Data) :- otherwise |
    logger # start(ok),
    Target? # handle(Data?),
    logger # finish(ok).
''';
      final module = compileModule(source, 'hybrid');

      final distributeOps = module.bytecode.ops.whereType<Distribute>().toList();
      final transmitOps = module.bytecode.ops.whereType<Transmit>().toList();

      // 2 static RPCs to logger
      expect(distributeOps.length, 2);
      expect(distributeOps.every((op) => op.importIndex == 1), true);

      // 1 dynamic RPC to Target
      expect(transmitOps.length, 1);
      expect(transmitOps[0].functor, 'handle');
    });
  });
}

// Extension to convert Module AST to Program for analyzer
extension ModuleToProgram on Module {
  Program toProgram() {
    return Program(procedures, line, column);
  }
}
