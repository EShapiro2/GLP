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
    test('_ state has {Mode.output}', () {
      final source = 'OutputOnly ::= _.';
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final dfa = compiler.compile('OutputOnly');

      expect(dfa.primitiveStateModes, isNotEmpty);
      final modes = dfa.getModesAt(dfa.startState);
      expect(modes, contains(Mode.output));
      expect(modes.contains(Mode.input), isFalse);
    });

    test('_? state has {Mode.input}', () {
      final source = 'InputOnly ::= _?.';
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final dfa = compiler.compile('InputOnly');

      expect(dfa.primitiveStateModes, isNotEmpty);
      final modes = dfa.getModesAt(dfa.startState);
      expect(modes, contains(Mode.input));
      expect(modes.contains(Mode.output), isFalse);
    });

    test('Every state has {Mode.output, Mode.input}', () {
      final source = ''; // Every is predefined
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final dfa = compiler.compile('Every');

      expect(dfa.primitiveStateModes, isNotEmpty);
      final modes = dfa.getModesAt(dfa.startState);
      expect(modes, contains(Mode.output));
      expect(modes, contains(Mode.input));
    });

    test('Any inherits Every modes via ::<', () {
      final source = ''; // Any ::< Every is predefined
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final anyDFA = compiler.compile('Any');
      final everyDFA = compiler.compile('Every');

      // Any should have same modes as Every (inherited)
      final anyModes = anyDFA.getModesAt(anyDFA.startState);
      final everyModes = everyDFA.getModesAt(everyDFA.startState);
      expect(anyModes, equals(everyModes));
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
      expect(result, isEmpty, reason: 'Writer X at output position _ should be accepted');
    });

    test('reader variable at _? position: accepted', () {
      final source = '''
InputOnly ::= _?.

procedure echo(InputOnly).

echo(X?).
''';
      final result = checkTypes(source);
      expect(result, isEmpty, reason: 'Reader X? at input position _? should be accepted');
    });

    test('Every accepts both writer and reader', () {
      final source = '''
procedure identity(Every, Every).

identity(X, X?).
''';
      final result = checkTypes(source);
      expect(result, isEmpty, reason: 'Every accepts both modes');
    });

    test('Every with reader then writer', () {
      final source = '''
procedure swap_modes(Every, Every).

swap_modes(X?, Y).
''';
      final result = checkTypes(source);
      expect(result, isEmpty, reason: 'Every accepts reader at arg1, writer at arg2');
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
      expect(result, isNotEmpty, reason: 'Writer X at input position _? should be rejected');
    });

    test('reader variable at _ position: REJECTED', () {
      final source = '''
OutputOnly ::= _.

procedure bad_echo(OutputOnly).

bad_echo(X?).
''';
      final result = checkTypes(source);
      expect(result, isNotEmpty, reason: 'Reader X? at output position _ should be rejected');
    });

    test('mode mismatch at nested position', () {
      final source = '''
OutputOnly ::= _.
Container ::= box(OutputOnly).

procedure process(Container).

process(box(X?)).
''';
      final result = checkTypes(source);
      expect(result, isNotEmpty, reason: 'Reader X? inside box at output position should be rejected');
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

    test('Every ∩ OutputOnly = OutputOnly', () {
      final source = 'OutputOnly ::= _.';
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final every = compiler.compile('Every');
      final out = compiler.compile('OutputOnly');

      final intersection = every.intersect(out);

      // Intersection should have only output mode
      final modes = intersection.getModesAt(intersection.startState);
      expect(modes, contains(Mode.output));
      expect(modes.contains(Mode.input), isFalse);
    });

    test('Every ∩ InputOnly = InputOnly', () {
      final source = 'InputOnly ::= _?.';
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final every = compiler.compile('Every');
      final inp = compiler.compile('InputOnly');

      final intersection = every.intersect(inp);

      // Intersection should have only input mode
      final modes = intersection.getModesAt(intersection.startState);
      expect(modes, contains(Mode.input));
      expect(modes.contains(Mode.output), isFalse);
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
