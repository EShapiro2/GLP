// test/analysis/type_checker/type_compiler_test.dart
//
// Tests for TypeCompiler module
// Spec: docs/modules/type-dfa.md

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_compiler.dart';
import 'package:glp_runtime/analysis/type_checker/type_parser.dart';

void main() {
  group('TypeCompiler', () {
    group('No Type Aliasing (Epsilon Transitions)', () {
      // Spec: docs/modules/type-dfa.md "No Type Aliasing" section
      // Type aliasing is illegal because it creates epsilon transitions (NFA)

      test('NEGATIVE: rejects simple type alias (A ::= B)', () {
        final source = '''
          BaseType ::= [] ; [_ | BaseType].
          AliasType ::= BaseType.
        ''';

        final typeEnv = parseTypes(source);
        final compiler = TypeCompiler(typeEnv);

        expect(
          () => compiler.compile('AliasType'),
          throwsA(isA<TypeCompileError>().having(
            (e) => e.message,
            'message',
            contains('type alias'),
          )),
          reason: 'Type alias creates epsilon transition, making DFA into NFA',
        );
      });

      test('NEGATIVE: rejects type union (A ::= B ; C)', () {
        final source = '''
          List1 ::= [] ; [_ | List1].
          List2 ::= [] ; [_ | List2].
          Combined ::= List1 ; List2.
        ''';

        final typeEnv = parseTypes(source);
        final compiler = TypeCompiler(typeEnv);

        expect(
          () => compiler.compile('Combined'),
          throwsA(isA<TypeCompileError>()),
          reason: 'Merging types creates NFA with epsilon transitions',
        );
      });

      test('POSITIVE: accepts type reference inside constructor', () {
        final source = '''
          Other ::= a ; b.
          Wrapper ::= wrap(Other).
        ''';

        final typeEnv = parseTypes(source);
        final compiler = TypeCompiler(typeEnv);

        // Should not throw - Other is inside a constructor, not a bare reference
        final dfa = compiler.compile('Wrapper');
        expect(dfa.states, isNotEmpty);
      });

      test('POSITIVE: accepts recursive type reference', () {
        final source = '''
          MyList ::= [] ; [_ | MyList].
        ''';

        final typeEnv = parseTypes(source);
        final compiler = TypeCompiler(typeEnv);

        // Should not throw - MyList reference is inside [_|MyList] constructor
        final dfa = compiler.compile('MyList');
        expect(dfa.states, isNotEmpty);
      });
    });
  });
}
