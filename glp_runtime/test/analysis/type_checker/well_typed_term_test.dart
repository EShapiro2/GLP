// test/analysis/type_checker/well_typed_term_test.dart
//
// Tests for well_typed_term.dart
// Specification: docs/modules/well-typed-term.md v0.3
// Paper Reference: Definition 4.3, Definition 4.5

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';
import 'package:glp_runtime/analysis/type_checker/moded_term.dart';
import 'package:glp_runtime/analysis/type_checker/well_typed_term.dart';
import 'package:glp_runtime/analysis/type_checker/type_dfa.dart';

void main() {
  group('WellTypedTerm', () {
    // =========================================================================
    // Helper: Create Simple DFAs for Testing
    // =========================================================================

    /// Create a DFA for type: T ::= _ (primitive output only)
    TypeDFA createPrimitiveOutputDFA() {
      final state = DFAState('T');
      return TypeDFA(
        states: {state},
        startState: state,
        finalStates: {},
        transitions: {},
        primitiveStateModes: {state: {Mode.produce}},
      );
    }

    /// Create a DFA for type: T ::= _? (primitive input only)
    TypeDFA createPrimitiveInputDFA() {
      final state = DFAState('T');
      return TypeDFA(
        states: {state},
        startState: state,
        finalStates: {},
        transitions: {},
        primitiveStateModes: {state: {Mode.consume}},
      );
    }

    /// Create a DFA for type: T ::= _ ; _? (bi-moded primitive)
    TypeDFA createBiModedPrimitiveDFA() {
      final state = DFAState('Any');
      return TypeDFA(
        states: {state},
        startState: state,
        finalStates: {},
        transitions: {},
        primitiveStateModes: {state: {Mode.produce, Mode.consume}},
      );
    }

    /// Create a DFA for type: Stream ::= [] ; [_|Stream]
    TypeDFA createStreamDFA() {
      final streamState = DFAState('Stream');
      final primitiveState = DFAState('_primitive', isFinal: true);
      final finalState = DFAState('_FINAL_', isFinal: true);

      return TypeDFA(
        states: {streamState, primitiveState, finalState},
        startState: streamState,
        finalStates: {finalState},
        transitions: {
          // [] → final (empty list)
          (streamState, PathElement.nil()): finalState,
          // [|](2,1) → primitive (head position accepts any value)
          (streamState, PathElement.listHead()): primitiveState,
          // [|](2,2) → Stream (tail is recursive)
          (streamState, PathElement.listTail()): streamState,
        },
        primitiveStateModes: {
          primitiveState: {Mode.produce, Mode.consume}, // bi-moded
        },
      );
    }

    // =========================================================================
    // Basic Variable Tests
    // =========================================================================

    group('Basic Variable Checks', () {
      test('writer at produce position is well-typed', () {
        // Type: T ::= _ (output only)
        // Term: X (writer)
        final dfa = createPrimitiveOutputDFA();
        final term = ModedVariable.writer('X');

        final result = checkModedTerm(term, dfa);

        expect(result.isWellTyped, isTrue);
        expect(result.errors, isEmpty);
        expect(result.variableTypes, contains('X'));
        expect(result.variableTypes['X']!.mode, equals(Mode.produce));
      });

      test('reader at consume position is well-typed', () {
        // Type: T ::= _? (input only)
        // Term: X? (reader)
        final dfa = createPrimitiveInputDFA();
        final term = ModedVariable.reader('X');

        final result = checkModedTerm(term, dfa);

        expect(result.isWellTyped, isTrue);
        expect(result.errors, isEmpty);
        expect(result.variableTypes, contains('X?'));
        expect(result.variableTypes['X?']!.mode, equals(Mode.consume));
      });

      test('writer at bi-moded position is well-typed', () {
        // Type: Any ::= _ ; _? (bi-moded)
        // Term: X (writer)
        final dfa = createBiModedPrimitiveDFA();
        final term = ModedVariable.writer('X');

        final result = checkModedTerm(term, dfa);

        expect(result.isWellTyped, isTrue);
      });

      test('reader at bi-moded position is well-typed', () {
        // Type: Any ::= _ ; _? (bi-moded)
        // Term: X? (reader)
        final dfa = createBiModedPrimitiveDFA();
        final term = ModedVariable.reader('X');

        final result = checkModedTerm(term, dfa);

        expect(result.isWellTyped, isTrue);
      });
    });

    // =========================================================================
    // Negative Controls: Mode Mismatches
    // =========================================================================

    group('Negative: Mode Mismatches', () {
      test('writer at input-only position is NOT well-typed', () {
        // Type: T ::= _? (input only)
        // Term: X (writer) - WRONG! needs produce but type only has consume
        final dfa = createPrimitiveInputDFA();
        final term = ModedVariable.writer('X');

        final result = checkModedTerm(term, dfa);

        expect(result.isWellTyped, isFalse);
        expect(result.errors, hasLength(1));
        expect(result.errors.first, isA<InconsistentPathError>());
      });

      test('reader at output-only position is NOT well-typed', () {
        // Type: T ::= _ (output only)
        // Term: X? (reader) - WRONG! needs consume but type only has produce
        final dfa = createPrimitiveOutputDFA();
        final term = ModedVariable.reader('X');

        final result = checkModedTerm(term, dfa);

        expect(result.isWellTyped, isFalse);
        expect(result.errors, hasLength(1));
        expect(result.errors.first, isA<InconsistentPathError>());
      });
    });

    // =========================================================================
    // List Structure Tests
    // =========================================================================

    group('List Structure Checks', () {
      test('nil at Stream position is well-typed', () {
        // Type: Stream ::= [] ; [_|Stream]
        // Term: ↓[] (empty list)
        final dfa = createStreamDFA();
        final term = ModedConstant.nil(Mode.consume);

        final result = checkModedTerm(term, dfa);

        expect(result.isWellTyped, isTrue);
      });

      test('cons with variables at Stream position', () {
        // Type: Stream (with bi-moded head)
        // Term: ↓[↓X?|Xs?] (list with reader head and reader tail)
        final dfa = createStreamDFA();

        final term = ModedCompound.listCons(
          Mode.consume,
          ModedVariable.reader('X'),
          ModedVariable.reader('Xs'),
        );

        final result = checkModedTerm(term, dfa);

        expect(result.isWellTyped, isTrue);
        expect(result.variableTypes, contains('X?'));
        expect(result.variableTypes, contains('Xs?'));
      });
    });

    // =========================================================================
    // Complementarity Tests
    // =========================================================================

    group('Variable Complementarity', () {
      test('X and X? at same type are complementary', () {
        // Type: Any (bi-moded)
        // Term: ↓f(↓X?, ↑X) - X? reads, X writes at same type
        final anyState = DFAState('Any');
        final dfa = TypeDFA(
          states: {anyState},
          startState: anyState,
          finalStates: {},
          transitions: {
            (anyState, PathElement.functor('f', 2, 1)): anyState,
            (anyState, PathElement.functor('f', 2, 2)): anyState,
          },
          primitiveStateModes: {anyState: {Mode.produce, Mode.consume}},
        );

        final term = ModedCompound(Mode.consume, 'f', 2, [
          ModedVariable.reader('X'),
          ModedVariable.writer('X'),
        ]);

        final result = checkModedTerm(term, dfa);

        expect(result.isWellTyped, isTrue);
        expect(result.variableTypes, contains('X?'));
        expect(result.variableTypes, contains('X'));

        // Both should be at same state
        expect(result.variableTypes['X?']!.typeState,
            equals(result.variableTypes['X']!.typeState));
      });

      test('X and X? at different types are NOT complementary', () {
        // Create DFA with two different type states
        final stateA = DFAState('A');
        final stateB = DFAState('B');
        final dfa = TypeDFA(
          states: {stateA, stateB},
          startState: stateA,
          finalStates: {},
          transitions: {
            (stateA, PathElement.functor('f', 2, 1)): stateA,
            (stateA, PathElement.functor('f', 2, 2)): stateB,
          },
          primitiveStateModes: {
            stateA: {Mode.produce, Mode.consume},
            stateB: {Mode.produce, Mode.consume},
          },
        );

        // X? at position 1 (state A), X at position 2 (state B)
        final term = ModedCompound(Mode.consume, 'f', 2, [
          ModedVariable.reader('X'),
          ModedVariable.writer('X'),
        ]);

        final result = checkModedTerm(term, dfa);

        expect(result.isWellTyped, isFalse);
        expect(result.errors, hasLength(1));
        expect(result.errors.first, isA<NonComplementaryError>());
      });
    });

    // =========================================================================
    // PathCheckResult Tests
    // =========================================================================

    group('checkPathAgainstDFA', () {
      test('single-step variable path', () {
        final dfa = createBiModedPrimitiveDFA();
        final path = ModedPath([
          PathStep(
            symbol: 'X?',
            argIndex: 0,
            mode: Mode.consume,
            isVariable: true,
            isReader: true,
          ),
        ]);

        final result = checkPathAgainstDFA(path, dfa);

        expect(result.isConsistent, isTrue);
        expect(result.variableAssignment, isNotNull);
        expect(result.variableAssignment!.mode, equals(Mode.consume));
      });

      test('two-step path through structure', () {
        final dfa = createStreamDFA();

        // Path: [|]/2 at root → X? at head position
        final path = ModedPath([
          PathStep(symbol: '[|]/2', argIndex: 0, mode: Mode.produce),
          PathStep(
            symbol: 'X?',
            argIndex: 1,
            mode: Mode.consume,
            isVariable: true,
            isReader: true,
          ),
        ]);

        final result = checkPathAgainstDFA(path, dfa);

        // Should find transition [|](2,1) from Stream state
        expect(result.isConsistent, isTrue);
      });

      test('invalid path - no transition', () {
        final dfa = createStreamDFA();

        // Path with invalid functor
        final path = ModedPath([
          PathStep(symbol: 'invalid/1', argIndex: 0, mode: Mode.produce),
          PathStep(
            symbol: 'X',
            argIndex: 1,
            mode: Mode.produce,
            isVariable: true,
            isReader: false,
          ),
        ]);

        final result = checkPathAgainstDFA(path, dfa);

        expect(result.isConsistent, isFalse);
        expect(result.reason, contains('No transition'));
      });
    });

    // =========================================================================
    // VariableTypeInfo Tests
    // =========================================================================

    group('VariableTypeInfo', () {
      test('equality based on state, mode, and reader status', () {
        final state = DFAState('T');

        final info1 = VariableTypeInfo(
          typeState: state,
          mode: Mode.consume,
          isReader: true,
        );
        final info2 = VariableTypeInfo(
          typeState: state,
          mode: Mode.consume,
          isReader: true,
        );
        final info3 = VariableTypeInfo(
          typeState: state,
          mode: Mode.produce,
          isReader: false,
        );

        expect(info1, equals(info2));
        expect(info1, isNot(equals(info3)));
      });

      test('toString formats nicely', () {
        final state = DFAState('Stream');
        final info = VariableTypeInfo(
          typeState: state,
          mode: Mode.consume,
          isReader: true,
        );

        expect(info.toString(), contains('Stream'));
        expect(info.toString(), contains('↓'));
      });
    });

    // =========================================================================
    // Error Type Tests
    // =========================================================================

    group('Error Types', () {
      test('InconsistentPathError message includes path', () {
        final path = ModedPath([
          PathStep(symbol: 'f/1', argIndex: 0, mode: Mode.consume),
          PathStep(
            symbol: 'X',
            argIndex: 1,
            mode: Mode.produce,
            isVariable: true,
            isReader: false,
          ),
        ]);

        final error = InconsistentPathError(path, 'test reason');

        expect(error.message, contains('test reason'));
        expect(error.message, contains('Path:'));
      });

      test('NonComplementaryError message includes both types', () {
        final state1 = DFAState('A');
        final state2 = DFAState('B');

        final error = NonComplementaryError(
          'X',
          VariableTypeInfo(typeState: state1, mode: Mode.produce, isReader: false),
          VariableTypeInfo(typeState: state2, mode: Mode.consume, isReader: true),
        );

        expect(error.message, contains('X'));
        expect(error.message, contains('X?'));
        expect(error.message, contains('complementary'));
      });
    });

    // =========================================================================
    // Complex Structure Tests
    // =========================================================================

    group('Complex Structures', () {
      test('nested list cons', () {
        // Type: Stream
        // Term: ↓[↓X?|↓[↓Y?|Zs?]] (nested list)
        final dfa = createStreamDFA();

        final inner = ModedCompound.listCons(
          Mode.consume,
          ModedVariable.reader('Y'),
          ModedVariable.reader('Zs'),
        );

        final term = ModedCompound.listCons(
          Mode.consume,
          ModedVariable.reader('X'),
          inner,
        );

        final result = checkModedTerm(term, dfa);

        expect(result.isWellTyped, isTrue);
        expect(result.variableTypes, contains('X?'));
        expect(result.variableTypes, contains('Y?'));
        expect(result.variableTypes, contains('Zs?'));
      });

      test('multiple occurrences of same variable', () {
        // Same variable appears twice - should have consistent type
        final anyState = DFAState('Any');
        final dfa = TypeDFA(
          states: {anyState},
          startState: anyState,
          finalStates: {},
          transitions: {
            (anyState, PathElement.functor('f', 2, 1)): anyState,
            (anyState, PathElement.functor('f', 2, 2)): anyState,
          },
          primitiveStateModes: {anyState: {Mode.produce, Mode.consume}},
        );

        // f(X?, X?) - same reader variable twice
        final term = ModedCompound(Mode.consume, 'f', 2, [
          ModedVariable.reader('X'),
          ModedVariable.reader('X'),
        ]);

        final result = checkModedTerm(term, dfa);

        expect(result.isWellTyped, isTrue);
        // Should only have one entry for X?
        expect(result.variableTypes.length, equals(1));
        expect(result.variableTypes, contains('X?'));
      });
    });

    // =========================================================================
    // WellTypedResult Factory Tests
    // =========================================================================

    group('WellTypedResult Factories', () {
      test('success factory creates well-typed result', () {
        final result = WellTypedResult.success({
          'X': VariableTypeInfo(
            typeState: DFAState('T'),
            mode: Mode.produce,
            isReader: false,
          ),
        });

        expect(result.isWellTyped, isTrue);
        expect(result.errors, isEmpty);
        expect(result.variableTypes, isNotEmpty);
      });

      test('failure factory creates ill-typed result', () {
        final error = InconsistentPathError(
          ModedPath([
            PathStep(
              symbol: 'X',
              argIndex: 0,
              mode: Mode.produce,
              isVariable: true,
              isReader: false,
            ),
          ]),
          'test',
        );

        final result = WellTypedResult.failure([error]);

        expect(result.isWellTyped, isFalse);
        expect(result.errors, hasLength(1));
        expect(result.variableTypes, isEmpty);
      });
    });
  });
}
