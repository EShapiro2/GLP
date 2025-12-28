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

    test('Any state has {Mode.output, Mode.input}', () {
      final source = ''; // Any is predefined
      final env = parseTypes(source);
      final compiler = TypeCompiler(env);

      final dfa = compiler.compile('Any');

      expect(dfa.primitiveStateModes, isNotEmpty);
      final modes = dfa.getModesAt(dfa.startState);
      expect(modes, contains(Mode.output));
      expect(modes, contains(Mode.input));
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

    test('Any accepts both writer and reader (requires coverage)', () {
      final source = '''
procedure identity(Any, Any).

identity(X, X?).
identity(Y?, Y).
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: 'Both clauses together cover all modes');
    });

    test('Any with single clause - incomplete coverage', () {
      final source = '''
procedure swap_modes(Any, Any).

swap_modes(X?, Y).
''';
      final result = checkTypes(source);
      expect(result.errors, isNotEmpty, reason: 'Any requires coverage of both modes');
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

  group('Mode Coverage for Any (::= semantics)', () {
    // === POSITIVE CONTROLS ===

    test('POSITIVE: Any with two clauses covering both modes', () {
      final source = '''
procedure full_coverage(Any).

full_coverage(X).   % writer covers _
full_coverage(X?).  % reader covers _?
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: 'Two clauses cover both mode alternatives');
    });

    test('POSITIVE: Any with two clauses, two args, all combinations', () {
      final source = '''
procedure binary_coverage(Any, Any).

binary_coverage(X, Y?).   % writer/reader
binary_coverage(X?, Y).   % reader/writer
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: 'Two clauses cover all mode combinations');
    });

    // === NEGATIVE CONTROLS ===

    test('NEGATIVE: Any with single clause - incomplete coverage', () {
      final source = '''
procedure incomplete(Any).

incomplete(X).
''';
      final result = checkTypes(source);
      expect(result.errors, isNotEmpty, reason: 'Any requires both _ and _? coverage');
    });

    test('NEGATIVE: Any with two clauses but same mode - still incomplete', () {
      final source = '''
procedure same_mode(Any).

same_mode(X).
same_mode(Y).
''';
      final result = checkTypes(source);
      expect(result.errors, isNotEmpty, reason: 'Both clauses are writers, missing reader');
    });

    test('NEGATIVE: Any binary with single clause - incomplete', () {
      final source = '''
procedure binary_incomplete(Any, Any).

binary_incomplete(X, X?).
''';
      final result = checkTypes(source);
      expect(result.errors, isNotEmpty, reason: 'Arg1 missing reader, Arg2 missing writer');
    });

    test('NEGATIVE: known guard does NOT satisfy coverage', () {
      final source = '''
procedure known_not_ground(Any).

known_not_ground(X?) :- known(X?) | true.
''';
      final result = checkTypes(source);
      expect(result.errors, isNotEmpty, reason: 'known does not imply ground');
    });

    test('NEGATIVE: Nested Any with incomplete coverage', () {
      final source = '''
AnyList ::= [] ; [Any | AnyList].

procedure process(AnyList).

process([]).
process([H? | T]) :- process(T?).
''';
      final result = checkTypes(source);
      expect(result.errors, isNotEmpty, reason: 'Head position has Any, needs both modes');
    });

    // === POSITIVE: List at OUTPUT position (containment, not coverage) ===

    test('POSITIVE: List at output position with single clause', () {
      final source = '''
procedure process_list(List).

process_list([]).
process_list([H? | T]) :- process_list(T?).
''';
      final result = checkTypes(source);
      expect(result.errors, isEmpty, reason: 'Output position uses containment, not coverage');
    });
  });

  // Note: Mode intersection tests removed - intersect() method not implemented

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
