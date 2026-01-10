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

    /// Create a DFA for type: Stream ::= [] ; [_|Stream]
    /// Head position has produce mode (_)
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
          // [|](2,1) → primitive (head position: output mode)
          (streamState, PathElement.listHead()): primitiveState,
          // [|](2,2) → Stream (tail is recursive)
          (streamState, PathElement.listTail()): streamState,
        },
        primitiveStateModes: {
          primitiveState: {Mode.produce}, // Single mode: produce
        },
      );
    }

    /// Create a DFA for type: HollowStream ::= [] ; [_?|HollowStream]
    /// Head position has consume mode (_?)
    TypeDFA createHollowStreamDFA() {
      final streamState = DFAState('HollowStream');
      final primitiveState = DFAState('_primitive_input', isFinal: true);
      final finalState = DFAState('_FINAL_', isFinal: true);

      return TypeDFA(
        states: {streamState, primitiveState, finalState},
        startState: streamState,
        finalStates: {finalState},
        transitions: {
          (streamState, PathElement.nil()): finalState,
          (streamState, PathElement.listHead()): primitiveState,
          (streamState, PathElement.listTail()): streamState,
        },
        primitiveStateModes: {
          primitiveState: {Mode.consume}, // Single mode: consume
        },
      );
    }

    /// Create a DFA for type: Pair ::= pair(_, _?)
    /// Arg1 has produce mode, Arg2 has consume mode
    TypeDFA createPairDFA() {
      final pairState = DFAState('Pair');
      final outputState = DFAState('_output', isFinal: true);
      final inputState = DFAState('_input', isFinal: true);

      return TypeDFA(
        states: {pairState, outputState, inputState},
        startState: pairState,
        finalStates: {},
        transitions: {
          (pairState, PathElement.functor('pair', 2, 1)): outputState,
          (pairState, PathElement.functor('pair', 2, 2)): inputState,
        },
        primitiveStateModes: {
          outputState: {Mode.produce},
          inputState: {Mode.consume},
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

      test('writer at stream head (produce position) is well-typed', () {
        // Type: Stream ::= [] ; [_|Stream]
        // Term: [X|Xs] - X at produce position
        final dfa = createStreamDFA();
        final term = ModedCompound.listCons(
          Mode.produce,
          ModedVariable.writer('X'),
          ModedVariable.writer('Xs'),
        );

        final result = checkModedTerm(term, dfa);

        expect(result.isWellTyped, isTrue);
      });

      test('reader at hollow stream head (consume position) is well-typed', () {
        // Type: HollowStream ::= [] ; [_?|HollowStream]
        // Term: [X?|Xs?] - X? at consume position
        final dfa = createHollowStreamDFA();
        final term = ModedCompound.listCons(
          Mode.consume,
          ModedVariable.reader('X'),
          ModedVariable.reader('Xs'),
        );

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

      test('reader at stream head (produce position) is NOT well-typed', () {
        // Type: Stream ::= [] ; [_|Stream]
        // Term: [X?|Xs] - X? at produce position is wrong
        final dfa = createStreamDFA();
        final term = ModedCompound.listCons(
          Mode.produce,
          ModedVariable.reader('X'), // Wrong! produce position needs writer
          ModedVariable.writer('Xs'),
        );

        final result = checkModedTerm(term, dfa);

        expect(result.isWellTyped, isFalse);
      });
    });

    // =========================================================================
    // List Structure Tests
    // =========================================================================

    group('List Structure Checks', () {
      test('nil at Stream position is well-typed', () {
        // Type: Stream ::= [] ; [_|Stream]
        // Term: [] (empty list)
        final dfa = createStreamDFA();
        final term = ModedConstant.nil(Mode.produce);

        final result = checkModedTerm(term, dfa);

        expect(result.isWellTyped, isTrue);
      });

      test('cons with writer head at Stream position', () {
        // Type: Stream (head is produce mode)
        // Term: [X|Xs] (list with writer head and writer tail)
        final dfa = createStreamDFA();

        final term = ModedCompound.listCons(
          Mode.produce,
          ModedVariable.writer('X'),
          ModedVariable.writer('Xs'),
        );

        final result = checkModedTerm(term, dfa);

        expect(result.isWellTyped, isTrue);
        expect(result.variableTypes, contains('X'));
        expect(result.variableTypes, contains('Xs'));
      });
    });

    // =========================================================================
    // Complementarity Tests
    // =========================================================================

    group('Variable Complementarity', () {
      test('X and X? at complementary positions in same type are well-typed', () {
        // Type: Pair ::= pair(_, _?)
        // Term: pair(X, X?) - X at produce position, X? at consume position
        final dfa = createPairDFA();

        final term = ModedCompound(Mode.produce, 'pair', 2, [
          ModedVariable.writer('X'),  // arg1: produce position
          ModedVariable.reader('X'),  // arg2: consume position
        ]);

        final result = checkModedTerm(term, dfa);

        expect(result.isWellTyped, isTrue);
        expect(result.variableTypes, contains('X'));
        expect(result.variableTypes, contains('X?'));
      }, skip: 'Nested primitive mode checking not yet implemented');

      test('X and X? at wrong positions are NOT well-typed', () {
        // Type: Pair ::= pair(_, _?)
        // Term: pair(X?, X) - X? at produce position (wrong), X at consume position (wrong)
        final dfa = createPairDFA();

        final term = ModedCompound(Mode.produce, 'pair', 2, [
          ModedVariable.reader('X'),  // arg1: produce position - WRONG
          ModedVariable.writer('X'),  // arg2: consume position - WRONG
        ]);

        final result = checkModedTerm(term, dfa);

        expect(result.isWellTyped, isFalse);
      });

      test('X and X? at different types are NOT complementary', () {
        // Create DFA with two different type states for arg positions
        final stateA = DFAState('A');
        final stateB = DFAState('B');
        final rootState = DFAState('Root');
        final dfa = TypeDFA(
          states: {rootState, stateA, stateB},
          startState: rootState,
          finalStates: {},
          transitions: {
            (rootState, PathElement.functor('f', 2, 1)): stateA,
            (rootState, PathElement.functor('f', 2, 2)): stateB,
          },
          primitiveStateModes: {
            stateA: {Mode.consume},  // arg1 is consume
            stateB: {Mode.produce},  // arg2 is produce
          },
        );

        // f(X?, X) - X? at state A, X at state B (different states)
        final term = ModedCompound(Mode.produce, 'f', 2, [
          ModedVariable.reader('X'),
          ModedVariable.writer('X'),
        ]);

        final result = checkModedTerm(term, dfa);

        // Well-typed but X and X? are at different type states
        expect(result.isWellTyped, isTrue);
        expect(result.variableTypes['X?']!.typeState,
            isNot(equals(result.variableTypes['X']!.typeState)));
      }, skip: 'Nested primitive mode checking not yet implemented');
    });

    // =========================================================================
    // PathCheckResult Tests
    // =========================================================================

    group('checkPathAgainstDFA', () {
      test('single-step variable path at consume position', () {
        final dfa = createPrimitiveInputDFA();
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

      test('single-step variable path at produce position', () {
        final dfa = createPrimitiveOutputDFA();
        final path = ModedPath([
          PathStep(
            symbol: 'X',
            argIndex: 0,
            mode: Mode.produce,
            isVariable: true,
            isReader: false,
          ),
        ]);

        final result = checkPathAgainstDFA(path, dfa);

        expect(result.isConsistent, isTrue);
        expect(result.variableAssignment, isNotNull);
        expect(result.variableAssignment!.mode, equals(Mode.produce));
      });

      test('two-step path through structure', () {
        final dfa = createStreamDFA();

        // Path: [|]/2 at root → X at head position (produce)
        final path = ModedPath([
          PathStep(symbol: '[|]/2', argIndex: 0, mode: Mode.produce),
          PathStep(
            symbol: 'X',
            argIndex: 1,
            mode: Mode.produce,
            isVariable: true,
            isReader: false,
          ),
        ]);

        final result = checkPathAgainstDFA(path, dfa);

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
      test('nested list cons with writers', () {
        // Type: Stream ::= [] ; [_|Stream]
        // Term: [X|[Y|Zs]] (nested list with writers)
        final dfa = createStreamDFA();

        final inner = ModedCompound.listCons(
          Mode.produce,
          ModedVariable.writer('Y'),
          ModedVariable.writer('Zs'),
        );

        final term = ModedCompound.listCons(
          Mode.produce,
          ModedVariable.writer('X'),
          inner,
        );

        final result = checkModedTerm(term, dfa);

        expect(result.isWellTyped, isTrue);
        expect(result.variableTypes, contains('X'));
        expect(result.variableTypes, contains('Y'));
        expect(result.variableTypes, contains('Zs'));
      });

      test('multiple occurrences of same variable at same type', () {
        // Create type with same primitive state for both args
        final rootState = DFAState('Root');
        final primState = DFAState('Prim');
        final dfa = TypeDFA(
          states: {rootState, primState},
          startState: rootState,
          finalStates: {},
          transitions: {
            (rootState, PathElement.functor('f', 2, 1)): primState,
            (rootState, PathElement.functor('f', 2, 2)): primState,
          },
          primitiveStateModes: {primState: {Mode.consume}},
        );

        // f(X?, X?) - same reader variable twice at consume positions
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
