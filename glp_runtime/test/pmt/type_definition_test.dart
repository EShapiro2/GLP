import 'package:test/test.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/pmt/type_table.dart';

void main() {
  group('Type Definition Parser', () {
    Module parseModule(String source) {
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      return parser.parseModule();
    }

    test('simple atom type', () {
      final source = 'BinaryDigit := zero.';
      final module = parseModule(source);

      expect(module.typeDefinitions.length, 1);
      final typeDef = module.typeDefinitions[0];
      expect(typeDef.typeName, 'BinaryDigit');
      expect(typeDef.typeParams, isEmpty);
      expect(typeDef.constructors.length, 1);
      expect(typeDef.constructors[0], isA<AtomConstructor>());
      expect((typeDef.constructors[0] as AtomConstructor).name, 'zero');
    });

    test('union type', () {
      final source = 'BinaryDigit := zero | one.';
      final module = parseModule(source);

      expect(module.typeDefinitions.length, 1);
      final typeDef = module.typeDefinitions[0];
      expect(typeDef.typeName, 'BinaryDigit');
      expect(typeDef.constructors.length, 2);
      expect((typeDef.constructors[0] as AtomConstructor).name, 'zero');
      expect((typeDef.constructors[1] as AtomConstructor).name, 'one');
    });

    test('multi-line same type', () {
      final source = '''
BinaryDigit := zero.
BinaryDigit := one.
''';
      final module = parseModule(source);

      expect(module.typeDefinitions.length, 1);
      final typeDef = module.typeDefinitions[0];
      expect(typeDef.typeName, 'BinaryDigit');
      expect(typeDef.constructors.length, 2);
      expect((typeDef.constructors[0] as AtomConstructor).name, 'zero');
      expect((typeDef.constructors[1] as AtomConstructor).name, 'one');
    });

    test('recursive list type', () {
      final source = 'List := [] | [_ | List].';
      final module = parseModule(source);

      expect(module.typeDefinitions.length, 1);
      final typeDef = module.typeDefinitions[0];
      expect(typeDef.typeName, 'List');
      expect(typeDef.constructors.length, 2);

      // First constructor: []
      expect(typeDef.constructors[0], isA<ListConstructor>());
      final nil = typeDef.constructors[0] as ListConstructor;
      expect(nil.isNil, true);

      // Second constructor: [_|List]
      expect(typeDef.constructors[1], isA<ListConstructor>());
      final cons = typeDef.constructors[1] as ListConstructor;
      expect(cons.head!.typeName, '_');
      expect(cons.head!.isReader, false);
      expect(cons.tail!.typeName, 'List');
      expect(cons.tail!.isReader, false);
    });

    test('embedded reader mode in list', () {
      final source = 'BBList := [] | [_? | BBList].';
      final module = parseModule(source);

      expect(module.typeDefinitions.length, 1);
      final typeDef = module.typeDefinitions[0];
      expect(typeDef.constructors.length, 2);

      final cons = typeDef.constructors[1] as ListConstructor;
      expect(cons.head!.typeName, '_');
      expect(cons.head!.isReader, true);  // _? has reader mode
      expect(cons.tail!.typeName, 'BBList');
    });

    test('struct type with embedded modes', () {
      final source = 'DiffList := dl(List, List).';
      final module = parseModule(source);

      expect(module.typeDefinitions.length, 1);
      final typeDef = module.typeDefinitions[0];
      expect(typeDef.constructors.length, 1);
      expect(typeDef.constructors[0], isA<StructConstructor>());

      final ctor = typeDef.constructors[0] as StructConstructor;
      expect(ctor.functor, 'dl');
      expect(ctor.args.length, 2);
      expect(ctor.args[0].typeName, 'List');
      expect(ctor.args[0].isReader, false);
      expect(ctor.args[1].typeName, 'List');
      expect(ctor.args[1].isReader, false);
    });

    test('mixed union', () {
      final source = 'Cmd := inc | dec | get(Num).';
      final module = parseModule(source);

      expect(module.typeDefinitions.length, 1);
      final typeDef = module.typeDefinitions[0];
      expect(typeDef.constructors.length, 3);

      expect((typeDef.constructors[0] as AtomConstructor).name, 'inc');
      expect((typeDef.constructors[1] as AtomConstructor).name, 'dec');
      expect(typeDef.constructors[2], isA<StructConstructor>());
      final get = typeDef.constructors[2] as StructConstructor;
      expect(get.functor, 'get');
      expect(get.args.length, 1);
      expect(get.args[0].typeName, 'Num');
    });

    test('mode declaration still works', () {
      final source = 'And := and(List?, List?, List).';
      final module = parseModule(source);

      expect(module.modeDeclarations.length, 1);
      expect(module.typeDefinitions.length, 0);

      final modeDecl = module.modeDeclarations[0];
      expect(modeDecl.typeName, 'And');
      expect(modeDecl.predicate, 'and');
      expect(modeDecl.args.length, 3);
      expect(modeDecl.args[0].isReader, true);
      expect(modeDecl.args[1].isReader, true);
      expect(modeDecl.args[2].isReader, false);
    });

    test('coexists with mode declaration', () {
      final source = '''
BinaryDigit := zero | one.
And := and(List?, List?, List).
''';
      final module = parseModule(source);

      expect(module.typeDefinitions.length, 1);
      expect(module.modeDeclarations.length, 1);

      expect(module.typeDefinitions[0].typeName, 'BinaryDigit');
      expect(module.modeDeclarations[0].typeName, 'And');
    });

    test('parameterized type', () {
      final source = 'Option(T) := none | some(T).';
      final module = parseModule(source);

      expect(module.typeDefinitions.length, 1);
      final typeDef = module.typeDefinitions[0];
      expect(typeDef.typeName, 'Option');
      expect(typeDef.typeParams, ['T']);
      expect(typeDef.constructors.length, 2);
    });

    test('nullary predicate is mode declaration', () {
      final source = 'Halt := halt().';
      final module = parseModule(source);

      // halt() with empty parens is a mode declaration for halt/0
      expect(module.modeDeclarations.length, 1);
      expect(module.typeDefinitions.length, 0);
      expect(module.modeDeclarations[0].predicate, 'halt');
      expect(module.modeDeclarations[0].arity, 0);
    });

    test('nullary atom is type constructor', () {
      final source = 'Halt := halt.';
      final module = parseModule(source);

      // halt without parens is a type constructor
      expect(module.typeDefinitions.length, 1);
      expect(module.modeDeclarations.length, 0);
      expect((module.typeDefinitions[0].constructors[0] as AtomConstructor).name, 'halt');
    });

    test('coexists with clauses', () {
      final source = '''
BinaryDigit := zero | one.
And := and(List?, List?, List).

and([], [], []).
and([X|Xs], [Y|Ys], [Z|Zs?]) :- and(Xs?, Ys?, Zs).
''';
      final module = parseModule(source);

      expect(module.typeDefinitions.length, 1);
      expect(module.modeDeclarations.length, 1);
      expect(module.procedures.length, 1);
      expect(module.procedures[0].name, 'and');
      expect(module.procedures[0].clauses.length, 2);
    });
  });

  group('TypeTable', () {
    test('fromDefinitions', () {
      final source = '''
BinaryDigit := zero | one.
Cmd := inc | dec.
''';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final module = parser.parseModule();

      final table = TypeTable.fromDefinitions(module.typeDefinitions);

      expect(table.length, 2);
      expect(table.hasType('BinaryDigit'), true);
      expect(table.hasType('Cmd'), true);
      expect(table.hasType('Unknown'), false);
    });

    test('fromModule', () {
      final source = '''
BinaryDigit := zero | one.
And := and(List?, List?, List).
''';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final module = parser.parseModule();

      final table = TypeTable.fromModule(module);

      expect(table.length, 1);  // Only type definitions, not mode declarations
      expect(table.hasType('BinaryDigit'), true);
      expect(table.hasType('And'), false);  // Mode declaration, not type
    });

    test('addConstructor merges', () {
      final table = TypeTable();
      table.addConstructor('Bool', const AtomConstructor('true'));
      table.addConstructor('Bool', const AtomConstructor('false'));

      expect(table.length, 1);
      expect(table.getType('Bool')!.constructors.length, 2);
    });
  });
}
