import 'package:test/test.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/ast.dart';

void main() {
  group('PMT Parser - Mode Declarations', () {
    test('parses simple declaration without type params: Not := not(Num?, Num).', () {
      const source = '''
Not := not(Num?, Num).

not(0, 1).
not(1, 0).
''';

      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final module = parser.parseModule();

      expect(module.modeDeclarations.length, 1);

      final decl = module.modeDeclarations[0];
      expect(decl.typeName, 'Not');
      expect(decl.typeParams, isEmpty);
      expect(decl.predicate, 'not');
      expect(decl.arity, 2);
      expect(decl.signature, 'not/2');

      expect(decl.args.length, 2);
      expect(decl.args[0].typeName, 'Num');
      expect(decl.args[0].isReader, isTrue);
      expect(decl.args[1].typeName, 'Num');
      expect(decl.args[1].isReader, isFalse);

      expect(module.procedures.length, 1);
      expect(module.procedures[0].name, 'not');
    });

    test('parses declaration with type params: Merge(A) := merge(List(A)?, List(A)?, List(A)).', () {
      const source = '''
Merge(A) := merge(List(A)?, List(A)?, List(A)).

merge([], Ys, Ys?).
merge([X|Xs], Ys, [X?|Zs]) :- merge(Ys?, Xs?, Zs).
''';

      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final module = parser.parseModule();

      expect(module.modeDeclarations.length, 1);

      final decl = module.modeDeclarations[0];
      expect(decl.typeName, 'Merge');
      expect(decl.typeParams, ['A']);
      expect(decl.predicate, 'merge');
      expect(decl.arity, 3);
      expect(decl.signature, 'merge/3');

      expect(decl.args.length, 3);

      // First arg: List(A)?
      expect(decl.args[0].typeName, 'List');
      expect(decl.args[0].typeParams, ['A']);
      expect(decl.args[0].isReader, isTrue);

      // Second arg: List(A)?
      expect(decl.args[1].typeName, 'List');
      expect(decl.args[1].typeParams, ['A']);
      expect(decl.args[1].isReader, isTrue);

      // Third arg: List(A)
      expect(decl.args[2].typeName, 'List');
      expect(decl.args[2].typeParams, ['A']);
      expect(decl.args[2].isReader, isFalse);
    });

    test('parses multiple declarations', () {
      const source = '''
-module(math).
-export([half_adder/4, xor/3, and/3]).

HalfAdder := half_adder(Num?, Num?, Num, Num).
Xor := xor(Num?, Num?, Num).
And := and(Num?, Num?, Num).

half_adder(A, B, Sum, Carry) :-
    ground(A?), ground(B?) |
    xor(A?, B?, Sum), and(A?, B?, Carry).
''';

      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final module = parser.parseModule();

      expect(module.declaration?.name, 'math');
      expect(module.exports.length, 1);
      expect(module.modeDeclarations.length, 3);

      expect(module.modeDeclarations[0].predicate, 'half_adder');
      expect(module.modeDeclarations[0].arity, 4);

      expect(module.modeDeclarations[1].predicate, 'xor');
      expect(module.modeDeclarations[1].arity, 3);

      expect(module.modeDeclarations[2].predicate, 'and');
      expect(module.modeDeclarations[2].arity, 3);

      expect(module.procedures.length, 1);
      expect(module.procedures[0].name, 'half_adder');
    });

    test('parses declaration with multiple type params: BiMap(K, V) := bimap(Map(K, V)?, Map(K, V)).', () {
      const source = '''
BiMap(K, V) := bimap(Map(K, V)?, Map(K, V)).

bimap(In, Out).
''';

      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final module = parser.parseModule();

      expect(module.modeDeclarations.length, 1);

      final decl = module.modeDeclarations[0];
      expect(decl.typeName, 'BiMap');
      expect(decl.typeParams, ['K', 'V']);
      expect(decl.predicate, 'bimap');
      expect(decl.arity, 2);

      expect(decl.args[0].typeName, 'Map');
      expect(decl.args[0].typeParams, ['K', 'V']);
      expect(decl.args[0].isReader, isTrue);

      expect(decl.args[1].typeName, 'Map');
      expect(decl.args[1].typeParams, ['K', 'V']);
      expect(decl.args[1].isReader, isFalse);
    });

    test('ModeDeclaration toString formats correctly', () {
      final decl = ModeDeclaration(
        'Merge',
        ['A'],
        'merge',
        [
          ModedArg('List', ['A'], isReader: true),
          ModedArg('List', ['A'], isReader: true),
          ModedArg('List', ['A'], isReader: false),
        ],
        1,
        1,
      );

      expect(decl.toString(), 'Merge(A) := merge(List(A)?, List(A)?, List(A)).');
    });

    test('ModedArg toString formats correctly', () {
      final readerArg = ModedArg('List', ['A'], isReader: true);
      final writerArg = ModedArg('List', ['A'], isReader: false);
      final simpleReader = ModedArg('Num', [], isReader: true);
      final simpleWriter = ModedArg('Num', [], isReader: false);

      expect(readerArg.toString(), 'List(A)?');
      expect(writerArg.toString(), 'List(A)');
      expect(simpleReader.toString(), 'Num?');
      expect(simpleWriter.toString(), 'Num');
    });

    test('module without declarations has empty modeDeclarations list', () {
      const source = '''
-module(simple).

foo(X).
''';

      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final module = parser.parseModule();

      expect(module.modeDeclarations, isEmpty);
      expect(module.procedures.length, 1);
    });

    test('declarations come after exports but before clauses', () {
      const source = '''
-module(streams).
-export([merge/3]).
-import([utils]).

Merge(A) := merge(List(A)?, List(A)?, List(A)).

merge([], Ys, Ys?).
''';

      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final module = parser.parseModule();

      expect(module.declaration?.name, 'streams');
      expect(module.exports.length, 1);
      expect(module.imports.length, 1);
      expect(module.modeDeclarations.length, 1);
      expect(module.procedures.length, 1);

      // Verify order is preserved
      expect(module.modeDeclarations[0].predicate, 'merge');
      expect(module.procedures[0].name, 'merge');
    });

    test('parses union of mode declarations', () {
      const source = '''
Observe := observe(Any?, Any, Any?) | observe(Any, Any?, Any?).

observe(X?, Y, Z?) :- observe(Y?, X, Z).
observe(X, Out1?, Out2?) :- ground(X?) | true.
''';

      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final module = parser.parseModule();

      // Should produce 2 mode declarations for the same predicate
      expect(module.modeDeclarations.length, 2);

      // First mode: observe(Any?, Any, Any?)
      final decl1 = module.modeDeclarations[0];
      expect(decl1.typeName, 'Observe');
      expect(decl1.predicate, 'observe');
      expect(decl1.arity, 3);
      expect(decl1.args[0].isReader, isTrue);   // Any?
      expect(decl1.args[1].isReader, isFalse);  // Any
      expect(decl1.args[2].isReader, isTrue);   // Any?

      // Second mode: observe(Any, Any?, Any?)
      final decl2 = module.modeDeclarations[1];
      expect(decl2.typeName, 'Observe');
      expect(decl2.predicate, 'observe');
      expect(decl2.arity, 3);
      expect(decl2.args[0].isReader, isFalse);  // Any
      expect(decl2.args[1].isReader, isTrue);   // Any?
      expect(decl2.args[2].isReader, isTrue);   // Any?
    });

    test('union of modes with type parameters', () {
      const source = '''
BiMap(K, V) := bimap(Map(K, V)?, Map(V, K)) | bimap(Map(K, V), Map(V, K)?).

bimap(In, Out).
''';

      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final module = parser.parseModule();

      // Should produce 2 mode declarations
      expect(module.modeDeclarations.length, 2);

      // First mode: bimap(Map(K, V)?, Map(V, K))
      final decl1 = module.modeDeclarations[0];
      expect(decl1.predicate, 'bimap');
      expect(decl1.args[0].isReader, isTrue);   // Map(K, V)?
      expect(decl1.args[1].isReader, isFalse);  // Map(V, K)

      // Second mode: bimap(Map(K, V), Map(V, K)?)
      final decl2 = module.modeDeclarations[1];
      expect(decl2.predicate, 'bimap');
      expect(decl2.args[0].isReader, isFalse);  // Map(K, V)
      expect(decl2.args[1].isReader, isTrue);   // Map(V, K)?
    });
  });
}
