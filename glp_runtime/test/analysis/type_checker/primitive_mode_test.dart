// test/analysis/type_checker/primitive_mode_test.dart
//
// Tests for primitive mode types (_ and _?)
// Note: Types like `BiMode ::= _ ; _?` are ILLEGAL because they create
// non-deterministic DFAs (both _ and _? match any value at that position)

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/type_compiler.dart';

void main() {
  group('Primitive Mode Type Parsing', () {
    test('Parse primitive mode output (_)', () {
      final source = 'MyType ::= _.';
      final env = parseTypes(source);

      expect(env.types.containsKey('MyType'), isTrue);
      final typeDef = env.types['MyType'];
      expect(typeDef, isNotNull);
      expect(typeDef!.alternatives, hasLength(1));

      final alt = typeDef.alternatives[0];
      expect(alt, isA<PrimitiveModeAlt>());
      expect((alt as PrimitiveModeAlt).isInput, isFalse);
      expect(alt.toString(), equals('_'));
    });

    test('Parse primitive mode input (_?)', () {
      final source = 'MyType ::= _?.';
      final env = parseTypes(source);

      final typeDef = env.types['MyType'];
      expect(typeDef, isNotNull);

      final alt = typeDef!.alternatives[0];
      expect(alt, isA<PrimitiveModeAlt>());
      expect((alt as PrimitiveModeAlt).isInput, isTrue);
      expect(alt.toString(), equals('_?'));
    });

    test('System type prelude defines Output and Input', () {
      // Even with empty source, prelude should define Output and Input
      final env = parseTypes('');

      expect(env.types['Output'], isNotNull);
      expect(env.types['Input'], isNotNull);

      final outputType = env.types['Output']!;
      expect(outputType.alternatives, hasLength(1));
      expect(outputType.alternatives[0], isA<PrimitiveModeAlt>());
      expect((outputType.alternatives[0] as PrimitiveModeAlt).isInput, isFalse);

      final inputType = env.types['Input']!;
      expect(inputType.alternatives, hasLength(1));
      expect(inputType.alternatives[0], isA<PrimitiveModeAlt>());
      expect((inputType.alternatives[0] as PrimitiveModeAlt).isInput, isTrue);
    });

    test('System type prelude defines List with primitive head', () {
      final env = parseTypes('');

      expect(env.types['List'], isNotNull);
      final listType = env.types['List']!;

      expect(listType.alternatives, hasLength(2));

      // First alternative: [_ | List]
      final consAlt = listType.alternatives[0] as ListConsAlt;
      expect(consAlt.head, isA<PrimitiveModeAlt>());
      expect((consAlt.head as PrimitiveModeAlt).isInput, isFalse);
      expect(consAlt.tail, isA<TypeRef>());
      expect((consAlt.tail as TypeRef).name, equals('List'));

      // Second alternative: []
      expect(listType.alternatives[1], isA<ListNilAlt>());
    });
  });

  group('Type Definition Syntax', () {
    test('Parse ::= as type definition', () {
      final source = 'Nat ::= 0 ; s(Nat).';
      final env = parseTypes(source);

      final typeDef = env.types['Nat'];
      expect(typeDef, isNotNull);
      expect(typeDef.toString(), contains('::='));
    });

    test('Parse structural type with multiple alternatives', () {
      final source = 'SmallNat ::= 0 ; 1 ; 2.';
      final env = parseTypes(source);

      final typeDef = env.types['SmallNat'];
      expect(typeDef, isNotNull);
      expect(typeDef!.alternatives, hasLength(3));
    });
  });

  group('DFA Compilation with Primitive Modes', () {
    test('Compile Output type creates DFA with primitive state', () {
      final env = parseTypes('');
      final compiler = TypeCompiler(env);

      final outputDFA = compiler.compile('Output');

      expect(outputDFA.primitiveStateModes, isNotEmpty);
      expect(outputDFA.isPrimitiveState(outputDFA.startState), isTrue);
    });

    test('Type with primitive mode creates primitive state', () {
      final source = 'MyType ::= _.';
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final dfa = compiler.compile('MyType');

      expect(dfa.primitiveStateModes, isNotEmpty);
    });

    test('Structural type does not create primitive states', () {
      final source = 'Nat ::= 0 ; s(Nat).';
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final dfa = compiler.compile('Nat');

      expect(dfa.primitiveStateModes, isEmpty);
    });
  });
}
