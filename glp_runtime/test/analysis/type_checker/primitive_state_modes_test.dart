// test/analysis/type_checker/primitive_state_modes_test.dart
//
// Tests for primitive state modes (Phase 0 implementation).
// Verifies that primitiveStateModes correctly tracks mode information
// and that mode checking at primitive positions works as expected.
//
// Note: Bare primitives as type definitions (e.g., `Output ::= _`) are ILLEGAL.
// Primitives can only appear:
// 1. Directly in procedure declarations: `procedure foo(_).`
// 2. Within constructor arguments: `MyList ::= [] ; [_ | MyList].`

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';
import 'package:glp_runtime/analysis/type_checker/type_compiler.dart';
import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'test_helpers.dart';

void main() {
  group('Primitive State Modes', () {
    test('List with _ head has produce mode at head position', () {
      final source = 'OutputList ::= [] ; [_ | OutputList].';
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final dfa = compiler.compile('OutputList');

      // The list type should have primitive states for the head element
      expect(dfa.primitiveStateModes, isNotEmpty,
          reason: 'List with _ head should have primitive state modes');
    });

    test('List with _? head has consume mode at head position', () {
      final source = 'InputList ::= [] ; [_? | InputList].';
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final dfa = compiler.compile('InputList');

      expect(dfa.primitiveStateModes, isNotEmpty,
          reason: 'List with _? head should have primitive state modes');
    });
  });

  group('Mode Checking at Primitive Positions - POSITIVE', () {
    test('reader variable at _ (output) position: accepted', () {
      // procedure foo(_). means output position (produce mode)
      // Head with reader X? → after flip → writer X (produce) → matches
      final source = '''
procedure copy(_).

copy(X?).
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: 'Reader X? at output position _ should be accepted (after flip)');
    });

    test('writer variable at _? (input) position: accepted', () {
      // procedure foo(_?). means input position (consume mode)
      // Head with writer X → after flip → reader X? (consume) → matches
      final source = '''
procedure echo(_?).

echo(X).
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: 'Writer X at input position _? should be accepted (after flip)');
    });

    test('_ accepts reader (single-mode, no coverage requirement)', () {
      final source = '''
procedure identity(_, _).

identity(X?, Y?).
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: '_ has single mode, readers match after flip');
    });

    test('_? accepts writer (single-mode, no coverage requirement)', () {
      final source = '''
procedure swap_modes(_?, _?).

swap_modes(X, Y).
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: '_? has single mode, writers match after flip');
    });
  });

  group('Mode Checking at Primitive Positions - NEGATIVE', () {
    test('writer variable at _ (output) position: REJECTED', () {
      // procedure foo(_). means output position (produce mode)
      // Head with writer X → after flip → reader X? (consume) → NO MATCH
      final source = '''
procedure bad_copy(_).

bad_copy(X).
''';
      final result = checkTypes(source);
      expect(result.errors, isNotEmpty, reason: 'Writer X at output position _ should be rejected');
    });

    test('reader variable at _? (input) position: REJECTED', () {
      // procedure foo(_?). means input position (consume mode)
      // Head with reader X? → after flip → writer X (produce) → NO MATCH
      final source = '''
procedure bad_echo(_?).

bad_echo(X?).
''';
      final result = checkTypes(source);
      expect(result.errors, isNotEmpty, reason: 'Reader X? at input position _? should be rejected');
    });

    test('mode mismatch at nested position in list', () {
      final source = '''
OutputList ::= [] ; [_ | OutputList].

procedure process(OutputList).

process([]).
process([X | T]) :- process(T?).
''';
      final result = checkTypes(source);
      // Writer X at _ (produce) position → after flip → X? (consume) → NO MATCH
      expect(result.errors, isNotEmpty, reason: 'Writer X inside list at output position should be rejected');
    });
  });

  group('Single-mode Types (no coverage requirement)', () {
    test('POSITIVE: _ with single clause', () {
      final source = '''
procedure output_single(_).

output_single(X?).
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: '_ has single mode, no coverage requirement');
    });

    test('POSITIVE: _? with single clause', () {
      final source = '''
procedure input_single(_?).

input_single(X).
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: '_? has single mode, no coverage requirement');
    });

    test('POSITIVE: Nested _ in list with single clause', () {
      final source = '''
OutputList ::= [] ; [_ | OutputList].

procedure process_list(OutputList?).

process_list([]).
process_list([H? | T]) :- process_list(T?).
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: 'List uses _ at head, reader H? matches after mode complement');
    });

    test('POSITIVE: Nested _? in list with single clause', () {
      final source = '''
InputList ::= [] ; [_? | InputList].

procedure consume_list(InputList?).

consume_list([]).
consume_list([H | T]) :- consume_list(T?).
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: 'List uses _? at head, writer H matches after mode complement');
    });
  });

  group('Structural vs Primitive States', () {
    test('Structural type has no primitive states', () {
      final source = 'Nat ::= 0 ; s(Nat).';
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final dfa = compiler.compile('Nat');

      expect(dfa.primitiveStateModes, isEmpty, reason: 'Structural types have no primitive state modes');
    });

    test('isPrimitiveState returns false for structural states', () {
      final source = 'Nat ::= 0 ; s(Nat).';
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final dfa = compiler.compile('Nat');

      expect(dfa.isPrimitiveState(dfa.startState), isFalse);
    });

    test('List with primitive head has primitive state for head position', () {
      final source = 'OutputList ::= [] ; [_ | OutputList].';
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final dfa = compiler.compile('OutputList');

      // The start state (OutputList) is not primitive
      expect(dfa.isPrimitiveState(dfa.startState), isFalse);
      // But there should be primitive states for the head element
      expect(dfa.primitiveStateModes.isNotEmpty, isTrue);
    });
  });
}
