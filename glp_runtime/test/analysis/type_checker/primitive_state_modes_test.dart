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

    test('BiMode state has {Mode.produce, Mode.consume}', () {
      final source = 'BiMode ::= _ ; _?.';
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final dfa = compiler.compile('BiMode');

      expect(dfa.primitiveStateModes, isNotEmpty);
      final modes = dfa.getModesAt(dfa.startState);
      expect(modes, contains(Mode.produce));
      expect(modes, contains(Mode.consume));
    });

    test('Output type has produce mode', () {
      final source = ''; // Output is predefined
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final outputDFA = compiler.compile('Output');

      final outputModes = outputDFA.getModesAt(outputDFA.startState);
      expect(outputModes, contains(Mode.produce));
      expect(outputModes.contains(Mode.consume), isFalse);
    });

    test('Input type has consume mode', () {
      final source = ''; // Input is predefined
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final inputDFA = compiler.compile('Input');

      final inputModes = inputDFA.getModesAt(inputDFA.startState);
      expect(inputModes, contains(Mode.consume));
      expect(inputModes.contains(Mode.produce), isFalse);
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

    test('Output accepts writer (single-mode, no coverage requirement)', () {
      final source = '''
procedure identity(Output, Output).

identity(X, Y).
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: 'Output has single mode, no coverage requirement');
    });

    test('Input accepts reader (single-mode, no coverage requirement)', () {
      final source = '''
procedure swap_modes(Input, Input).

swap_modes(X?, Y?).
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: 'Input has single mode, no coverage requirement');
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

  group('Mode Coverage for BiMode (::= semantics)', () {
    // === POSITIVE CONTROLS ===

    test('POSITIVE: BiMode with two clauses covering both modes', () {
      final source = '''
BiMode ::= _ ; _?.

procedure full_coverage(BiMode).

full_coverage(X).   % writer covers _
full_coverage(X?).  % reader covers _?
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: 'Two clauses cover both mode alternatives');
    });

    test('POSITIVE: BiMode with two clauses, two args, all combinations', () {
      final source = '''
BiMode ::= _ ; _?.

procedure binary_coverage(BiMode, BiMode).

binary_coverage(X, Y?).   % writer/reader
binary_coverage(X?, Y).   % reader/writer
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: 'Two clauses cover all mode combinations');
    });

    test('POSITIVE: Output with single clause (no coverage required)', () {
      final source = '''
procedure output_single(Output).

output_single(X).
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: 'Output has single mode, no coverage requirement');
    });

    // === NEGATIVE CONTROLS ===

    test('NEGATIVE: BiMode with single clause - incomplete coverage', () {
      final source = '''
BiMode ::= _ ; _?.

procedure incomplete(BiMode).

incomplete(X).
''';
      final result = checkTypes(source);
      expect(result.errors, isNotEmpty, reason: 'BiMode requires both _ and _? coverage');
    });

    test('NEGATIVE: BiMode with two clauses but same mode - still incomplete', () {
      final source = '''
BiMode ::= _ ; _?.

procedure same_mode(BiMode).

same_mode(X).
same_mode(Y).
''';
      final result = checkTypes(source);
      expect(result.errors, isNotEmpty, reason: 'Both clauses are writers, missing reader');
    });

    test('NEGATIVE: BiMode binary with single clause - incomplete', () {
      final source = '''
BiMode ::= _ ; _?.

procedure binary_incomplete(BiMode, BiMode).

binary_incomplete(X, X?).
''';
      final result = checkTypes(source);
      expect(result.errors, isNotEmpty, reason: 'Arg1 missing reader, Arg2 missing writer');
    });

    test('NEGATIVE: known guard does NOT satisfy coverage', () {
      final source = '''
BiMode ::= _ ; _?.

procedure known_not_ground(BiMode).

known_not_ground(X?) :- known(X?) | true.
''';
      final result = checkTypes(source);
      expect(result.errors, isNotEmpty, reason: 'known does not imply ground');
    });

    test('NEGATIVE: Nested BiMode with incomplete coverage', () {
      final source = '''
BiMode ::= _ ; _?.
BiModeList ::= [] ; [BiMode | BiModeList].

procedure process(BiModeList).

process([]).
process([H? | T]) :- process(T?).
''';
      final result = checkTypes(source);
      expect(result.errors, isNotEmpty, reason: 'Head position has BiMode, needs both modes');
    });

    // === POSITIVE: Nested single-mode (no coverage required) ===

    test('POSITIVE: Nested Output with single clause', () {
      final source = '''
OutputList ::= [] ; [Output | OutputList].

procedure process_list(OutputList).

process_list([]).
process_list([H | T]) :- process_list(T?).
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: 'OutputList uses Output at head, no coverage requirement');
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

    test('BiMode ∩ OutputOnly = OutputOnly', () {
      final source = 'BiMode ::= _ ; _?. OutputOnly ::= _.';
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final biMode = compiler.compile('BiMode');
      final out = compiler.compile('OutputOnly');

      final intersection = biMode.intersect(out);

      // Intersection should have only produce mode
      final modes = intersection.getModesAt(intersection.startState);
      expect(modes, contains(Mode.produce));
      expect(modes.contains(Mode.consume), isFalse);
    });

    test('BiMode ∩ InputOnly = InputOnly', () {
      final source = 'BiMode ::= _ ; _?. InputOnly ::= _?.';
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final biMode = compiler.compile('BiMode');
      final inp = compiler.compile('InputOnly');

      final intersection = biMode.intersect(inp);

      // Intersection should have only consume mode
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
