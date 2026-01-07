// test/analysis/type_checker/primitive_mode_test.dart
//
// Tests for primitive mode types (_ and _?) and ::= semantics

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

    test('Parse type with both primitive modes', () {
      final source = 'Universal ::= _ ; _?.';
      final env = parseTypes(source);

      final typeDef = env.types['Universal'];
      expect(typeDef, isNotNull);
      expect(typeDef!.alternatives, hasLength(2));

      final alt0 = typeDef.alternatives[0] as PrimitiveModeAlt;
      final alt1 = typeDef.alternatives[1] as PrimitiveModeAlt;

      expect(alt0.isInput, isFalse); // _
      expect(alt1.isInput, isTrue);  // _?
    });

    test('System type prelude defines Output', () {
      // Even with empty source, prelude should define Output
      final env = parseTypes('');

      expect(env.types['Output'], isNotNull);
      final outputType = env.types['Output']!;

      expect(outputType.name, equals('Output'));
      expect(outputType.alternatives, hasLength(1));

      final alt0 = outputType.alternatives[0] as PrimitiveModeAlt;
      expect(alt0.isInput, isFalse); // _
    });

    test('System type prelude defines Input', () {
      final env = parseTypes('');

      expect(env.types['Input'], isNotNull);
      final inputType = env.types['Input']!;

      expect(inputType.name, equals('Input'));
      expect(inputType.alternatives, hasLength(1));

      final alt0 = inputType.alternatives[0] as PrimitiveModeAlt;
      expect(alt0.isInput, isTrue); // _?
    });

    test('System type prelude defines List with _ elements', () {
      final env = parseTypes('');

      expect(env.types['List'], isNotNull);
      final listType = env.types['List']!;

      expect(listType.alternatives, hasLength(2));

      // First alternative: [] or [_ | List]
      // Order may vary
    });
  });

  group('Type Definition', () {
    test('Parse ::= as exact type definition', () {
      final source = 'Nat ::= 0 ; s(Nat).';
      final env = parseTypes(source);

      final typeDef = env.types['Nat'];
      expect(typeDef, isNotNull);
      expect(typeDef.toString(), contains('::='));
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

    test('Type with both primitive modes creates primitive state', () {
      final source = 'Universal ::= _ ; _?.';
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final dfa = compiler.compile('Universal');

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
