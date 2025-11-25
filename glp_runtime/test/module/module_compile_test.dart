import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/runtime/loaded_module.dart';
import 'package:glp_runtime/runtime/service_registry.dart';

void main() {
  group('LoadedModule', () {
    test('isExported returns true for empty exports (all exported)', () {
      final module = LoadedModule(
        name: 'test',
        exports: {},  // Empty = all exported
        imports: [],
        instructions: [],
        labels: {},
        procOffsets: {'foo/1': 0, 'bar/2': 10},
      );

      expect(module.isExported('foo', 1), isTrue);
      expect(module.isExported('bar', 2), isTrue);
      expect(module.isExported('baz', 3), isTrue);  // Even undefined
    });

    test('isExported respects explicit exports', () {
      final module = LoadedModule(
        name: 'test',
        exports: {'foo/1'},  // Only foo/1 exported
        imports: [],
        instructions: [],
        labels: {},
        procOffsets: {'foo/1': 0, 'bar/2': 10},
      );

      expect(module.isExported('foo', 1), isTrue);
      expect(module.isExported('bar', 2), isFalse);
    });

    test('getProcOffset returns correct offset', () {
      final module = LoadedModule(
        name: 'test',
        exports: {},
        imports: [],
        instructions: [],
        labels: {},
        procOffsets: {'foo/1': 0, 'bar/2': 10},
      );

      expect(module.getProcOffset('foo', 1), equals(0));
      expect(module.getProcOffset('bar', 2), equals(10));
      expect(module.getProcOffset('baz', 3), isNull);
    });
  });

  group('ServiceRegistry', () {
    test('register and lookup module', () {
      final registry = ServiceRegistry();
      final module = LoadedModule(
        name: 'math',
        exports: {},
        imports: [],
        instructions: [],
        labels: {},
        procOffsets: {},
      );

      registry.register(module);

      expect(registry.isLoaded('math'), isTrue);
      expect(registry.lookup('math'), equals(module));
      expect(registry.lookup('unknown'), isNull);
    });

    test('register throws on duplicate', () {
      final registry = ServiceRegistry();
      final module1 = LoadedModule(
        name: 'math',
        exports: {},
        imports: [],
        instructions: [],
        labels: {},
        procOffsets: {},
      );
      final module2 = LoadedModule(
        name: 'math',
        exports: {},
        imports: [],
        instructions: [],
        labels: {},
        procOffsets: {},
      );

      registry.register(module1);
      expect(() => registry.register(module2), throwsStateError);
    });

    test('registerOrReplace allows duplicate', () {
      final registry = ServiceRegistry();
      final module1 = LoadedModule(
        name: 'math',
        exports: {'old/1'},
        imports: [],
        instructions: [],
        labels: {},
        procOffsets: {},
      );
      final module2 = LoadedModule(
        name: 'math',
        exports: {'new/1'},
        imports: [],
        instructions: [],
        labels: {},
        procOffsets: {},
      );

      registry.registerOrReplace(module1);
      registry.registerOrReplace(module2);

      expect(registry.lookup('math')?.exports, equals({'new/1'}));
    });

    test('unload removes module', () {
      final registry = ServiceRegistry();
      final module = LoadedModule(
        name: 'math',
        exports: {},
        imports: [],
        instructions: [],
        labels: {},
        procOffsets: {},
      );

      registry.register(module);
      expect(registry.isLoaded('math'), isTrue);

      final removed = registry.unload('math');
      expect(removed, isTrue);
      expect(registry.isLoaded('math'), isFalse);

      final removedAgain = registry.unload('math');
      expect(removedAgain, isFalse);
    });

    test('reload replaces module', () {
      final registry = ServiceRegistry();
      final module1 = LoadedModule(
        name: 'math',
        exports: {'v1/0'},
        imports: [],
        instructions: [],
        labels: {},
        procOffsets: {},
      );
      final module2 = LoadedModule(
        name: 'math',
        exports: {'v2/0'},
        imports: [],
        instructions: [],
        labels: {},
        procOffsets: {},
      );

      registry.register(module1);
      registry.reload(module2);

      expect(registry.lookup('math')?.exports, equals({'v2/0'}));
    });
  });

  group('GlpCompiler.compileModule', () {
    test('compiles simple fact to LoadedModule', () {
      final compiler = GlpCompiler();
      final module = compiler.compileModule(
        'foo(42).',
        moduleName: 'test',
      );

      expect(module.name, equals('test'));
      expect(module.procedures, contains('foo/1'));
      expect(module.instructions, isNotEmpty);
    });

    test('compiles multiple procedures', () {
      final compiler = GlpCompiler();
      final source = '''
        foo(1).
        bar(X?) :- foo(X?).
      ''';

      final module = compiler.compileModule(source, moduleName: 'multi');

      expect(module.name, equals('multi'));
      expect(module.procedures, contains('foo/1'));
      expect(module.procedures, contains('bar/1'));
    });

    test('respects exports parameter', () {
      final compiler = GlpCompiler();
      final module = compiler.compileModule(
        'foo(1). bar(2).',
        moduleName: 'restricted',
        exports: {'foo/1'},
      );

      expect(module.isExported('foo', 1), isTrue);
      expect(module.isExported('bar', 1), isFalse);
    });

    test('uses anonymous name when not specified', () {
      final compiler = GlpCompiler();
      final module = compiler.compileModule('test(1).');

      expect(module.name, equals('_anonymous'));
    });

    test('records procedure offsets', () {
      final compiler = GlpCompiler();
      final module = compiler.compileModule(
        'first(1). second(2).',
        moduleName: 'offsets',
      );

      // First procedure should start at 0 (after any setup)
      expect(module.getProcOffset('first', 1), isNotNull);
      expect(module.getProcOffset('second', 1), isNotNull);

      // Second should be after first
      final firstOffset = module.getProcOffset('first', 1)!;
      final secondOffset = module.getProcOffset('second', 1)!;
      expect(secondOffset, greaterThan(firstOffset));
    });
  });

  group('Module integration', () {
    test('compile and register module', () {
      final compiler = GlpCompiler();
      final registry = ServiceRegistry();

      final mathModule = compiler.compileModule('''
        double(X?, Y) :- Y := X? * 2.
        triple(X?, Y) :- Y := X? * 3.
      ''', moduleName: 'math', exports: {'double/2', 'triple/2'});

      registry.register(mathModule);

      expect(registry.isLoaded('math'), isTrue);
      expect(registry.lookup('math')?.isExported('double', 2), isTrue);
    });

    test('multiple modules in registry', () {
      final compiler = GlpCompiler();
      final registry = ServiceRegistry();

      final math = compiler.compileModule('add(X?, Y?, Z) :- Z := X? + Y?.', moduleName: 'math');
      final list = compiler.compileModule('len([], 0).', moduleName: 'list');

      registry.register(math);
      registry.register(list);

      expect(registry.moduleCount, equals(2));
      expect(registry.moduleNames, containsAll(['math', 'list']));
    });
  });
}
