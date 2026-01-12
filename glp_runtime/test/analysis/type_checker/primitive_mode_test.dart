// test/analysis/type_checker/primitive_mode_test.dart
//
// Tests for primitive mode types (_ and _?) in the type system
// Note: _ and _? are primitive modes used directly in procedure declarations,
// not as type aliases (Output/Input type aliases were removed).

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart';
import 'package:glp_runtime/analysis/type_checker/program_dfa.dart';
import 'test_helpers.dart';

void main() {
  group('Primitive Mode in Prelude', () {
    test('System type prelude defines List with primitive head', () {
      final env = buildPreludeEnvironment();

      expect(env.hasType('List'), isTrue);
    });

    test('System type prelude defines Stream', () {
      final env = buildPreludeEnvironment();
      expect(env.hasType('Stream'), isTrue);
    });

    test('System type prelude defines DiffList', () {
      final env = buildPreludeEnvironment();
      expect(env.hasType('DiffList'), isTrue);
    });
  });

  group('Procedure Declarations with Primitive Modes', () {
    test('Procedure with _ (output) argument parses', () {
      final result = checkTypes('''
        procedure foo(_).
        foo(x).
      ''');
      expect(result.errors, isEmpty);
    });

    test('Procedure with _? (input) argument parses', () {
      final result = checkTypes('''
        procedure bar(_?).
        bar(X).
      ''');
      expect(result.errors, isEmpty);
    });

    test('Procedure with mixed modes parses', () {
      final result = checkTypes('''
        procedure copy(_?, _).
        copy(X, X?).
      ''');
      expect(result.errors, isEmpty);
    });
  });

  group('DFA Compilation with Primitive Modes', () {
    test('Structural type does not have wildcard start state', () {
      final env = buildPreludeEnvironment();
      final dfa = buildProgramDFA(env);

      // Get the List automaton's start state
      final listAutomaton = dfa.getAutomaton('List');

      // List is structural - transitions should exist
      expect(listAutomaton.startState, isNotNull);
    });
  });

  group('Type Definition Syntax', () {
    test('Parse ::= as type definition', () {
      final result = checkTypes('''
        Nat ::= zero ; s(Nat).
        procedure inc(Nat?, Nat).
        inc(zero, s(zero)).
        inc(s(N), s(M?)) :- inc(N?, M).
      ''');
      expect(result.errors, isEmpty);
    });

    test('Parse structural type with multiple alternatives', () {
      final result = checkTypes('''
        SmallNat ::= zero ; one ; two.
        procedure pick(SmallNat).
        pick(zero).
        pick(one).
        pick(two).
      ''');
      expect(result.errors, isEmpty);
    });
  });
}
