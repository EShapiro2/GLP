// test/analysis/type_checker/nested_structure_parser_test.dart
//
// Tests for parsing nested structures in type definitions
// Verifies that functor arguments can be arbitrary type expressions

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_parser.dart';

void main() {
  group('Nested Structure Parsing', () {

    test('Simple nested functor: s(s(EvenNat))', () {
      final source = '''
EvenNat ::= 0 ; s(s(EvenNat)).
''';

      expect(() => parseTypes(source), returnsNormally,
          reason: 's(s(EvenNat)) should parse successfully');
    });

    test('Binary tree with nested structure: node(Tree, Tree)', () {
      final source = '''
Tree ::= leaf ; node(Tree, Tree).
''';

      expect(() => parseTypes(source), returnsNormally,
          reason: 'node(Tree, Tree) should parse successfully');
    });

    test('Mixed nested and constants: pair(s(Nat), 0)', () {
      final source = '''
Nat ::= 0 ; s(Nat).
MixedPair ::= pair(s(Nat), 0).
''';

      expect(() => parseTypes(source), returnsNormally,
          reason: 'pair(s(Nat), 0) should parse successfully');
    });

    test('Triple nesting: s(s(s(Nat)))', () {
      final source = '''
Nat ::= 0 ; s(Nat).
TripleNat ::= s(s(s(Nat))).
''';

      expect(() => parseTypes(source), returnsNormally,
          reason: 's(s(s(Nat))) should parse successfully');
    });

    test('Nested with list: node(Tree, [Tree | Trees])', () {
      final source = '''
Tree ::= leaf ; node(Tree, [Tree | Trees]).
Trees ::= [] ; [Tree | Trees].
''';

      expect(() => parseTypes(source), returnsNormally,
          reason: 'node(Tree, [Tree | Trees]) should parse successfully');
    });

    test('Verify parsed structure for EvenNat', () {
      final source = '''
EvenNat ::= 0 ; s(s(EvenNat)).
''';

      final env = parseTypes(source);
      expect(env.types.containsKey('EvenNat'), isTrue);

      final evenNat = env.types['EvenNat']!;
      expect(evenNat.alternatives.length, equals(2),
          reason: 'EvenNat should have 2 alternatives: 0 and s(s(EvenNat))');
    });
  });
}
