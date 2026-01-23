// glp_runtime/test/analysis/type_checker/well_typed_term_test.dart
//
// Unit tests for well-typed moded term checking.
// Spec: docs/type system/well-typed-term.md
// Paper: Definition 5.4

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/well_typed_term.dart';
import 'package:glp_runtime/analysis/type_checker/moded_term.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';
import 'package:glp_runtime/analysis/type_checker/program_dfa.dart';

void main() {
  group('checkModedTerm', () {
    // Helper to create states
    DFAState state(String name, {bool isDual = false, bool isFinal = false}) =>
        DFAState(name, isDual: isDual, isFinal: isFinal);

    DFAState wildcardProduce() => state('_', isDual: false, isFinal: true);
    DFAState wildcardConsume() => state('_', isDual: true, isFinal: true);
    DFAState integerState({bool isDual = false}) =>
        state('Integer', isDual: isDual, isFinal: true);
    DFAState streamState({bool isDual = false}) =>
        state('Stream', isDual: isDual, isFinal: false);
    DFAState finalState() => state('_FINAL_', isDual: false, isFinal: true);

    group('constant at primitive type position', () {
      test('integer constant at Integer type is well-typed', () {
        // Type: Integer (accepts integer literals)
        // Term: ↑42
        final startState = integerState();
        final transitions = <(DFAState, TransitionLabel), DFAState>{};

        final automaton = Automaton(startState, transitions);
        final dfa = ProgramDFA(
          {'Integer': startState},
          {'Integer': automaton},
        );

        final term = ModedConstant(Mode.produce, 42);
        final result = checkModedTerm(term, automaton, dfa);

        expect(result.isWellTyped, isTrue);
        expect(result.errors, isEmpty);
      });

      test('string constant at Integer type fails', () {
        final startState = integerState();
        final transitions = <(DFAState, TransitionLabel), DFAState>{};
        final automaton = Automaton(startState, transitions);
        final dfa = ProgramDFA(
          {'Integer': startState},
          {'Integer': automaton},
        );

        final term = ModedConstant(Mode.produce, 'hello');
        final result = checkModedTerm(term, automaton, dfa);

        expect(result.isWellTyped, isFalse);
        expect(result.errors, isNotEmpty);
      });
    });

    group('variable at wildcard position', () {
      test('writer at _ (produce wildcard) is well-typed', () {
        // Type: _ (accepts any produced term)
        // Term: X (writer)
        final startState = wildcardProduce();
        final transitions = <(DFAState, TransitionLabel), DFAState>{};
        final automaton = Automaton(startState, transitions);
        final dfa = ProgramDFA(
          {'_': startState},
          {'_': automaton},
        );

        final term = ModedVariable.writer('X', structuralMode: Mode.produce);
        final result = checkModedTerm(term, automaton, dfa);

        expect(result.isWellTyped, isTrue);
        expect(result.variableTypes.containsKey('X'), isTrue);
      });

      test('reader at _? (consume wildcard) is well-typed', () {
        // Type: _? (accepts any consumed term)
        // Term: X? (reader)
        final startState = wildcardConsume();
        final transitions = <(DFAState, TransitionLabel), DFAState>{};
        final automaton = Automaton(startState, transitions);
        final dfa = ProgramDFA(
          {'_?': startState},
          {'_?': automaton},
        );

        final term = ModedVariable.reader('X', structuralMode: Mode.consume);
        final result = checkModedTerm(term, automaton, dfa);

        expect(result.isWellTyped, isTrue);
        expect(result.variableTypes.containsKey('X?'), isTrue);
      });
    });

    group('variable pair duality', () {
      test('writer type _ and reader type _? are dual', () {
        // Term with both X (writer) and X? (reader)
        // X at _ position, X? at _? position
        final prodWild = wildcardProduce();
        final consWild = wildcardConsume();

        // Compound term with two args
        final compoundState = state('test', isDual: false);
        final transitions = <(DFAState, TransitionLabel), DFAState>{
          (compoundState, TransitionLabel.functor('test', 2, 1, mode: Mode.produce)): prodWild,
          (compoundState, TransitionLabel.functor('test', 2, 2, mode: Mode.consume)): consWild,
        };
        final automaton = Automaton(compoundState, transitions);
        final dfa = ProgramDFA(
          {'test': compoundState, '_': prodWild, '_?': consWild},
          {'test': automaton, '_': Automaton(prodWild, {}), '_?': Automaton(consWild, {})},
        );

        // ↓test(↑X, ↓X?)
        final term = ModedCompound(Mode.consume, 'test', 2, [
          ModedVariable.writer('X', structuralMode: Mode.produce),
          ModedVariable.reader('X', structuralMode: Mode.consume),
        ]);

        final result = checkModedTerm(term, automaton, dfa);

        expect(result.isWellTyped, isTrue);
        expect(result.variableTypes.containsKey('X'), isTrue);
        expect(result.variableTypes.containsKey('X?'), isTrue);
      });

      test('same type for both writer and reader fails duality', () {
        // Both X and X? at _ (produce) positions - not dual
        final prodWild = wildcardProduce();

        final compoundState = state('test', isDual: false);
        final transitions = <(DFAState, TransitionLabel), DFAState>{
          (compoundState, TransitionLabel.functor('test', 2, 1, mode: Mode.produce)): prodWild,
          (compoundState, TransitionLabel.functor('test', 2, 2, mode: Mode.produce)): prodWild,
        };
        final automaton = Automaton(compoundState, transitions);
        final dfa = ProgramDFA(
          {'test': compoundState, '_': prodWild},
          {'test': automaton, '_': Automaton(prodWild, {})},
        );

        // ↓test(↑X, ↑X?) - reader X? at produce position (mode mismatch)
        final term = ModedCompound(Mode.consume, 'test', 2, [
          ModedVariable.writer('X', structuralMode: Mode.produce),
          ModedVariable.reader('X', structuralMode: Mode.produce),  // wrong mode for reader
        ]);

        final result = checkModedTerm(term, automaton, dfa);

        // Should fail due to duality check
        expect(result.isWellTyped, isFalse);
      });
    });

    group('path consistency', () {
      test('no transition for functor fails', () {
        // Type has no transition for 'foo' functor
        final startState = state('T', isDual: false);
        final transitions = <(DFAState, TransitionLabel), DFAState>{};
        final automaton = Automaton(startState, transitions);
        final dfa = ProgramDFA(
          {'T': startState},
          {'T': automaton},
        );

        // ↑foo(↑X) - but type T has no foo transition
        final term = ModedCompound(Mode.produce, 'foo', 1, [
          ModedVariable.writer('X', structuralMode: Mode.produce),
        ]);

        final result = checkModedTerm(term, automaton, dfa);

        expect(result.isWellTyped, isFalse);
        expect(result.errors.any((e) => e is InconsistentPathError), isTrue);
      });
    });
  });
}
