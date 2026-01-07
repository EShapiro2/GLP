// test/analysis/type_checker/primitive_state_modes_test.dart
//
// Tests for primitive state modes (Phase 0 implementation).
// Verifies that primitiveStateModes correctly tracks mode information
// and that mode checking at primitive positions works as expected.

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';
import 'package:glp_runtime/analysis/type_checker/type_compiler.dart';
import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'test_helpers.dart';

void main() {
  group('Primitive State Modes', () {
    test('_ state has {Mode.produce}', () {
      final source = 'OutputOnly ::= _.';
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final dfa = compiler.compile('OutputOnly');

      expect(dfa.primitiveStateModes, isNotEmpty);
      final modes = dfa.getModesAt(dfa.startState);
      expect(modes, contains(Mode.produce));
      expect(modes.contains(Mode.consume), isFalse);
    });

    test('_? state has {Mode.consume}', () {
      final source = 'InputOnly ::= _?.';
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final dfa = compiler.compile('InputOnly');

      expect(dfa.primitiveStateModes, isNotEmpty);
      final modes = dfa.getModesAt(dfa.startState);
      expect(modes, contains(Mode.consume));
      expect(modes.contains(Mode.produce), isFalse);
    });

    test('Bimodal state has {Mode.produce, Mode.consume}', () {
      final source = 'Bimodal ::= _ ; _?.';
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final dfa = compiler.compile('Bimodal');

      expect(dfa.primitiveStateModes, isNotEmpty);
      final modes = dfa.getModesAt(dfa.startState);
      expect(modes, contains(Mode.produce));
      expect(modes, contains(Mode.consume));
    });
  });

  group('Mode Checking at Primitive Positions - POSITIVE', () {
    test('writer variable at _ position: accepted', () {
      final source = '''
OutputOnly ::= _.

procedure copy(OutputOnly).

copy(X).
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: 'Writer X at output position _ should be accepted');
    });

    test('reader variable at _? position: accepted', () {
      final source = '''
InputOnly ::= _?.

procedure echo(InputOnly).

echo(X?).
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: 'Reader X? at input position _? should be accepted');
    });

    test('_ accepts writer variable', () {
      final source = '''
procedure identity(_, _).

identity(X, X?).
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: 'Primitive _ accepts writer');
    });

    test('_ accepts reader and writer in different args', () {
      final source = '''
procedure swap_modes(_, _).

swap_modes(X?, Y).
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: 'Primitive _ accepts any variable');
    });
  });

  group('Mode Checking at Primitive Positions - NEGATIVE', () {
    test('writer variable at _? position: REJECTED', () {
      final source = '''
InputOnly ::= _?.

procedure bad_copy(InputOnly).

bad_copy(X).
''';
      final result = checkTypes(source);
      expect(result.errors, isNotEmpty, reason: 'Writer X at input position _? should be rejected');
    });

    test('reader variable at _ position: REJECTED', () {
      final source = '''
OutputOnly ::= _.

procedure bad_echo(OutputOnly).

bad_echo(X?).
''';
      final result = checkTypes(source);
      expect(result.errors, isNotEmpty, reason: 'Reader X? at output position _ should be rejected');
    });

    test('mode mismatch at nested position', () {
      final source = '''
OutputOnly ::= _.
Container ::= box(OutputOnly).

procedure process(Container).

process(box(X?)).
''';
      final result = checkTypes(source);
      expect(result.errors, isNotEmpty, reason: 'Reader X? inside box at output position should be rejected');
    });
  });

  group('Mode Coverage for Bimodal type (::= semantics)', () {
    // === POSITIVE CONTROLS ===

    test('POSITIVE: Bimodal with two clauses covering both modes', () {
      final source = '''
Bimodal ::= _ ; _?.

procedure full_coverage(Bimodal).

full_coverage(X).
full_coverage(X?).
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: 'Two clauses cover both mode alternatives');
    });

    test('POSITIVE: Bimodal with two clauses, two args, all combinations', () {
      final source = '''
Bimodal ::= _ ; _?.

procedure binary_coverage(Bimodal, Bimodal).

binary_coverage(X, Y?).
binary_coverage(X?, Y).
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: 'Two clauses cover all mode combinations');
    });

    test('POSITIVE: _ with single clause (no coverage required for primitive)', () {
      final source = '''
procedure prim_single(_).

prim_single(X).
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: 'Primitive _ has no coverage requirement');
    });

    // === NEGATIVE CONTROLS ===

    test('NEGATIVE: Bimodal with single clause - incomplete coverage', () {
      final source = '''
Bimodal ::= _ ; _?.

procedure incomplete(Bimodal).

incomplete(X).
''';
      final result = checkTypes(source);
      expect(result.errors, isNotEmpty, reason: 'Bimodal requires both _ and _? coverage');
    });

    test('NEGATIVE: Bimodal with two clauses but same mode - still incomplete', () {
      final source = '''
Bimodal ::= _ ; _?.

procedure same_mode(Bimodal).

same_mode(X).
same_mode(Y).
''';
      final result = checkTypes(source);
      expect(result.errors, isNotEmpty, reason: 'Both clauses are writers, missing reader');
    });

    test('NEGATIVE: Bimodal binary with single clause - incomplete', () {
      final source = '''
Bimodal ::= _ ; _?.

procedure binary_incomplete(Bimodal, Bimodal).

binary_incomplete(X, X?).
''';
      final result = checkTypes(source);
      expect(result.errors, isNotEmpty, reason: 'Arg1 missing reader, Arg2 missing writer');
    });

    test('NEGATIVE: known guard does NOT satisfy coverage', () {
      final source = '''
Bimodal ::= _ ; _?.

procedure known_not_ground(Bimodal).

known_not_ground(X?) :- known(X?) | true.
''';
      final result = checkTypes(source);
      expect(result.errors, isNotEmpty, reason: 'known does not imply ground');
    });

    test('NEGATIVE: Nested Bimodal with incomplete coverage', () {
      final source = '''
Bimodal ::= _ ; _?.
BimodalList ::= [] ; [Bimodal | BimodalList].

procedure process(BimodalList).

process([]).
process([H? | T]) :- process(T?).
''';
      final result = checkTypes(source);
      expect(result.errors, isNotEmpty, reason: 'Head position has Bimodal, needs both modes');
    });

    // === POSITIVE: Nested _ (no coverage required) ===

    test('POSITIVE: Nested _ with single clause', () {
      final source = '''
PrimList ::= [] ; [_ | PrimList].

procedure process_list(PrimList).

process_list([]).
process_list([H? | T]) :- process_list(T?).
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: 'PrimList uses _ at head, no coverage requirement');
    });
  });

  group('Mode Intersection', () {
    test('OutputOnly ∩ InputOnly = ∅ (empty)', () {
      final source = 'OutputOnly ::= _. InputOnly ::= _?.';
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final out = compiler.compile('OutputOnly');
      final inp = compiler.compile('InputOnly');

      final intersection = out.intersect(inp);

      expect(intersection.isEmpty, isTrue, reason: 'Output-only ∩ Input-only = empty set');
    });

    test('Bimodal ∩ OutputOnly = OutputOnly', () {
      final source = 'Bimodal ::= _ ; _?. OutputOnly ::= _.';
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final bimodal = compiler.compile('Bimodal');
      final out = compiler.compile('OutputOnly');

      final intersection = bimodal.intersect(out);

      // Intersection should have only output mode
      final modes = intersection.getModesAt(intersection.startState);
      expect(modes, contains(Mode.produce));
      expect(modes.contains(Mode.consume), isFalse);
    });

    test('Bimodal ∩ InputOnly = InputOnly', () {
      final source = 'Bimodal ::= _ ; _?. InputOnly ::= _?.';
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final bimodal = compiler.compile('Bimodal');
      final inp = compiler.compile('InputOnly');

      final intersection = bimodal.intersect(inp);

      // Intersection should have only input mode
      final modes = intersection.getModesAt(intersection.startState);
      expect(modes, contains(Mode.consume));
      expect(modes.contains(Mode.produce), isFalse);
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

    test('isPrimitiveState returns true for primitive type states', () {
      final source = 'OutputOnly ::= _.';
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final dfa = compiler.compile('OutputOnly');

      expect(dfa.isPrimitiveState(dfa.startState), isTrue);
    });
  });
}
