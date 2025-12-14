import 'package:test/test.dart';
import 'dart:async';
import 'package:glp_runtime/runtime/import_vector.dart';
import 'package:glp_runtime/runtime/module_messages.dart';
import 'package:glp_runtime/runtime/loaded_module.dart';
import 'package:glp_runtime/runtime/module_registry.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/terms.dart';

void main() {
  group('ImportVector', () {
    test('make creates vector with correct size', () {
      final result = ImportVector.make(3);
      expect(result.vector.size, 3);
      expect(result.readers.length, 3);
    });

    test('write sends message to correct stream (1-indexed)', () async {
      final result = ImportVector.make(3);
      final received = <int>[];

      // Listen to all streams
      for (int i = 0; i < 3; i++) {
        result.readers[i].listen((msg) {
          received.add(i);
        });
      }

      // Write to stream 2 (1-indexed)
      final message = ExportMessage.trust(
        sourceModule: 'test',
        functor: 'foo',
        arity: 1,
        args: [],
      );
      result.vector.write(2, message);

      // Allow async processing
      await Future.delayed(Duration(milliseconds: 10));

      // Should have received on stream index 1 (0-indexed)
      expect(received, [1]);
    });

    test('write throws RangeError for invalid index', () {
      final result = ImportVector.make(3);
      final message = ExportMessage.trust(
        sourceModule: 'test',
        functor: 'foo',
        arity: 1,
        args: [],
      );

      expect(() => result.vector.write(0, message), throwsRangeError);  // 0 is invalid (1-indexed)
      expect(() => result.vector.write(4, message), throwsRangeError);  // Out of range
    });

    test('close closes all streams', () {
      final result = ImportVector.make(2);
      result.vector.close();
      // No exception thrown = success
    });
  });

  group('ExportMessage', () {
    test('trust factory creates message with minimal info', () {
      final msg = ExportMessage.trust(
        sourceModule: 'math',
        functor: 'factorial',
        arity: 2,
        args: [ConstTerm(5), VarRef(1, isReader: false)],
      );

      expect(msg.callInfo.sourceModule, 'math');
      expect(msg.functor, 'factorial');
      expect(msg.arity, 2);
      expect(msg.signature, 'factorial/2');
      expect(msg.args.length, 2);
      expect(msg.ccc, isEmpty);
    });

    test('callInfo stores source location', () {
      final msg = ExportMessage(
        callInfo: CallInfo('main', 10, 5),
        scope: ['main', 'helper'],
        functor: 'foo',
        arity: 0,
        args: [],
      );

      expect(msg.callInfo.sourceModule, 'main');
      expect(msg.callInfo.line, 10);
      expect(msg.callInfo.column, 5);
      expect(msg.scope, ['main', 'helper']);
    });
  });

  group('LoadedModule', () {
    test('creates module with exports and imports', () {
      final bytecode = BytecodeProgram([]);
      final module = LoadedModule(
        name: 'math',
        bytecode: bytecode,
        exports: {'factorial/2', 'gcd/3'},
        imports: ['io', 'utils'],
      );

      expect(module.name, 'math');
      expect(module.exports, {'factorial/2', 'gcd/3'});
      expect(module.imports, ['io', 'utils']);
    });

    test('isExported checks export set', () {
      final bytecode = BytecodeProgram([]);
      final module = LoadedModule(
        name: 'math',
        bytecode: bytecode,
        exports: {'factorial/2', 'gcd/3'},
        imports: [],
      );

      expect(module.isExported('factorial/2'), true);
      expect(module.isExported('gcd/3'), true);
      expect(module.isExported('unknown/1'), false);
    });

    test('inputStream receives messages', () async {
      final bytecode = BytecodeProgram([]);
      final module = LoadedModule(
        name: 'test',
        bytecode: bytecode,
        exports: {},
        imports: [],
      );

      final received = <ExportMessage>[];
      module.inputStream.listen(received.add);

      final msg = ExportMessage.trust(
        sourceModule: 'caller',
        functor: 'test',
        arity: 0,
        args: [],
      );
      module.inputSink.add(msg);

      await Future.delayed(Duration(milliseconds: 10));

      expect(received.length, 1);
      expect(received[0].functor, 'test');
    });
  });

  group('FcpModuleRegistry', () {
    test('register and lookup module', () {
      final registry = FcpModuleRegistry();
      final bytecode = BytecodeProgram([]);
      final module = LoadedModule(
        name: 'math',
        bytecode: bytecode,
        exports: {'factorial/2'},
        imports: [],
      );

      registry.register(module);

      expect(registry.lookup('math'), module);
      expect(registry.lookup('unknown'), isNull);
      expect(registry.isLoaded('math'), true);
      expect(registry.isLoaded('unknown'), false);
    });

    test('moduleCount returns number of loaded modules', () {
      final registry = FcpModuleRegistry();
      expect(registry.moduleCount, 0);

      final bytecode = BytecodeProgram([]);
      registry.register(LoadedModule(
        name: 'a',
        bytecode: bytecode,
        exports: {},
        imports: [],
      ));
      expect(registry.moduleCount, 1);

      registry.register(LoadedModule(
        name: 'b',
        bytecode: bytecode,
        exports: {},
        imports: [],
      ));
      expect(registry.moduleCount, 2);
    });

    test('close clears all modules', () {
      final registry = FcpModuleRegistry();
      final bytecode = BytecodeProgram([]);

      registry.register(LoadedModule(
        name: 'a',
        bytecode: bytecode,
        exports: {},
        imports: [],
      ));
      registry.register(LoadedModule(
        name: 'b',
        bytecode: bytecode,
        exports: {},
        imports: [],
      ));

      registry.close();
      expect(registry.moduleCount, 0);
    });
  });
}
