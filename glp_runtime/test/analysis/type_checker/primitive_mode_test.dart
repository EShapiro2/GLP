// test/analysis/type_checker/primitive_mode_test.dart
//
// Tests for primitive mode types (_ and _?) and ::= vs ::< semantics

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/type_compiler.dart';

void main() {
  group('Primitive Mode Type Parsing', () {
    test('Parse primitive mode output (_)', () {
      final source = 'MyType ::= _.';
      final env = parseTypes(source);

      // Should have MyType + Any + List from prelude
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

    test('System type prelude defines Any', () {
      // Even with empty source, prelude should define Any
      final env = parseTypes('');

      expect(env.types['Any'], isNotNull);
      final anyType = env.types['Any']!;

      expect(anyType.name, equals('Any'));
      expect(anyType.alternatives, hasLength(2));

      final alt0 = anyType.alternatives[0] as PrimitiveModeAlt;
      final alt1 = anyType.alternatives[1] as PrimitiveModeAlt;

      expect(alt0.isInput, isFalse); // _
      expect(alt1.isInput, isTrue);  // _?
    });

    test('System type prelude defines List with Any', () {
      final env = parseTypes('');

      expect(env.types['List'], isNotNull);
      final listType = env.types['List']!;

      expect(listType.alternatives, hasLength(2));

      // First alternative: []
      expect(listType.alternatives[0], isA<ListNilAlt>());

      // Second alternative: [Any | List]
      final consAlt = listType.alternatives[1] as ListConsAlt;
      expect(consAlt.head, isA<TypeRef>());
      expect((consAlt.head as TypeRef).name, equals('Any'));
      expect(consAlt.tail, isA<TypeRef>());
      expect((consAlt.tail as TypeRef).name, equals('List'));
    });
  });

  group('Type Definition vs Subtype Declaration', () {
    test('Parse ::= as exact type definition', () {
      final source = 'Nat ::= 0 ; s(Nat).';
      final env = parseTypes(source);

      final typeDef = env.types['Nat'];
      expect(typeDef, isNotNull);
      expect(typeDef!.isExact, isTrue);
      expect(typeDef.toString(), contains('::='));
    });

    test('Parse ::< as subtype declaration with single constant', () {
      final source = 'SmallNat ::< 0.';
      final env = parseTypes(source);

      final typeDef = env.types['SmallNat'];
      expect(typeDef, isNotNull);
      expect(typeDef!.isExact, isFalse);
      expect(typeDef.toString(), contains('::<'));
    });

    test('Parse ::< with multiple constants', () {
      final source = 'SmallNat ::< 0 ; 1 ; 2.';
      final env = parseTypes(source);

      final typeDef = env.types['SmallNat'];
      expect(typeDef, isNotNull);
      expect(typeDef!.isExact, isFalse);
      expect(typeDef!.alternatives, hasLength(3));
    });

    test('Parse file with both ::= and ::<', () {
      final source = '''
Nat ::= 0 ; s(Nat).
SmallNat ::< 0.
''';
      final env = parseTypes(source);

      expect(env.types['Nat']!.isExact, isTrue);
      expect(env.types['SmallNat']!.isExact, isFalse);
    });
  });

  group('DFA Compilation with Primitive Modes', () {
    test('Compile Any type creates DFA with any-value states', () {
      final env = parseTypes('');
      final compiler = TypeCompiler(env);

      final anyDFA = compiler.compile('Any');

      expect(anyDFA.anyValueStates, isNotEmpty);
      expect(anyDFA.anyValueStates.contains(anyDFA.startState), isTrue);
    });

    test('Type with primitive mode creates any-value state', () {
      final source = 'MyType ::= _.';
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final dfa = compiler.compile('MyType');

      expect(dfa.anyValueStates, isNotEmpty);
    });

    test('Type with both primitive modes creates any-value state', () {
      final source = 'Universal ::= _ ; _?.';
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final dfa = compiler.compile('Universal');

      expect(dfa.anyValueStates, isNotEmpty);
    });

    test('Structural type does not create any-value states', () {
      final source = 'Nat ::= 0 ; s(Nat).';
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final dfa = compiler.compile('Nat');

      expect(dfa.anyValueStates, isEmpty);
    });
  });

  group('Type Definition Properties', () {
    test('Exact type definition (::=) has isExact = true', () {
      final source = 'MyType ::= 0 ; 1.';
      final env = parseTypes(source);

      expect(env.types['MyType']!.isExact, isTrue);
    });

    test('Subtype declaration (::&lt;) has isExact = false', () {
      final source = 'MyType ::< 0.';
      final env = parseTypes(source);

      expect(env.types['MyType']!.isExact, isFalse);
    });

    test('System types Any and List are exact (::=)', () {
      final env = parseTypes('');

      expect(env.types['Any']!.isExact, isTrue);
      expect(env.types['List']!.isExact, isTrue);
    });
  });
}
