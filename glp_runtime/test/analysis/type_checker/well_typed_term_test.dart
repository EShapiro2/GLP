// test/analysis/type_checker/well_typed_term_test.dart
//
// Tests for well_typed_term.dart
// Specification: docs/modules/well-typed-term.md v0.4
// Paper Reference: Definition 4.3, Definition 4.5

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';
import 'package:glp_runtime/analysis/type_checker/moded_term.dart';
import 'package:glp_runtime/analysis/type_checker/well_typed_term.dart';
import 'package:glp_runtime/analysis/type_checker/program_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';

void main() {
  group('WellTypedTerm', () {
    // =========================================================================
    // Helper: Create Type Environment and DFA
    // =========================================================================

    /// Create a simple Stream type environment
    TypeEnvironment createStreamEnv() {
      final env = TypeEnvironment.empty();
      // Stream ::= [] ; [_|Stream]
      env.addType(TypeDef(
        'Stream',
        [
          ListNilAlt(0, 0),
          ListConsAlt(
            PrimitiveModeAlt(false, 0, 0), // _ for head (produce)
            TypeRef('Stream', 0, 0),
            0,
            0,
          ),
        ],
        0,
        0,
      ));
      return env;
    }

    /// Create a HollowStream type environment
    TypeEnvironment createHollowStreamEnv() {
      final env = TypeEnvironment.empty();
      // HollowStream ::= [] ; [_?|HollowStream?]
      // Note: tail is also input (HollowStream?) so entire structure is hollow
      env.addType(TypeDef(
        'HollowStream',
        [
          ListNilAlt(0, 0),
          ListConsAlt(
            PrimitiveModeAlt(true, 0, 0), // _? for head (consume)
            TypeRef('HollowStream', 0, 0, isInput: true), // HollowStream? for tail (consume)
            0,
            0,
          ),
        ],
        0,
        0,
      ));
      return env;
    }

    /// Create a Pair type environment
    TypeEnvironment createPairEnv() {
      final env = TypeEnvironment.empty();
      // Pair ::= pair(_, _?)
      env.addType(TypeDef(
        'Pair',
        [
          StructAlt('pair', [
            PrimitiveModeAlt(false, 0, 0), // _ output
            PrimitiveModeAlt(true, 0, 0),  // _? input
          ], 0, 0),
        ],
        0,
        0,
      ));
      return env;
    }

    // =========================================================================
    // Basic Variable Tests
    // =========================================================================

    group('Basic Variable Checks', () {
      test('writer at produce position (_) is well-typed', () {
        // Type: _ (output only)
        // Term: X (writer)
        final dfa = buildProgramDFA(TypeEnvironment.empty());
        final automaton = dfa.getAutomaton('_');
        final term = ModedVariable.writer('X');

        final result = checkModedTerm(term, automaton, dfa);

        expect(result.isWellTyped, isTrue);
        expect(result.errors, isEmpty);
        expect(result.variableTypes, contains('X'));
        expect(result.variableTypes['X']!.mode, equals(Mode.produce));
      });

      test('reader at consume position (_?) is well-typed', () {
        // Type: _? (input only)
        // Term: X? (reader)
        final dfa = buildProgramDFA(TypeEnvironment.empty());
        final automaton = dfa.getAutomaton('_?');
        final term = ModedVariable.reader('X');

        final result = checkModedTerm(term, automaton, dfa);

        expect(result.isWellTyped, isTrue);
        expect(result.errors, isEmpty);
        expect(result.variableTypes, contains('X?'));
        expect(result.variableTypes['X?']!.mode, equals(Mode.consume));
      });

      test('writer at stream head (produce position) is well-typed', () {
        // Type: Stream ::= [] ; [_|Stream]
        // Term: [X|Xs] - X at produce position
        final env = createStreamEnv();
        final dfa = buildProgramDFA(env);
        final automaton = dfa.getAutomaton('Stream');
        final term = ModedCompound.listCons(
          Mode.produce,
          ModedVariable.writer('X'),
          ModedVariable.writer('Xs'),
        );

        final result = checkModedTerm(term, automaton, dfa);

        expect(result.isWellTyped, isTrue);
      });

      test('reader at hollow stream head (consume position) is well-typed', () {
        // Type: HollowStream ::= [] ; [_?|HollowStream]
        // Term: [X?|Xs?] - X? at consume position
        final env = createHollowStreamEnv();
        final dfa = buildProgramDFA(env);
        final automaton = dfa.getAutomaton('HollowStream');
        final term = ModedCompound.listCons(
          Mode.consume,
          ModedVariable.reader('X'),
          ModedVariable.reader('Xs'),
        );

        final result = checkModedTerm(term, automaton, dfa);

        expect(result.isWellTyped, isTrue);
      });
    });

    // =========================================================================
    // Negative Controls: Mode Mismatches
    // =========================================================================

    group('Negative: Mode Mismatches', () {
      test('NEGATIVE: writer at _? (input-only) position is NOT well-typed', () {
        // Type: _? (input only)
        // Term: X (writer) - WRONG! needs consume but writer has produce
        final dfa = buildProgramDFA(TypeEnvironment.empty());
        final automaton = dfa.getAutomaton('_?');
        final term = ModedVariable.writer('X');

        final result = checkModedTerm(term, automaton, dfa);

        expect(result.isWellTyped, isFalse);
        expect(result.errors, hasLength(1));
        expect(result.errors.first, isA<InconsistentPathError>());
      });

      test('NEGATIVE: reader at _ (output-only) position is NOT well-typed', () {
        // Type: _ (output only)
        // Term: X? (reader) - WRONG! needs produce but reader has consume
        final dfa = buildProgramDFA(TypeEnvironment.empty());
        final automaton = dfa.getAutomaton('_');
        final term = ModedVariable.reader('X');

        final result = checkModedTerm(term, automaton, dfa);

        expect(result.isWellTyped, isFalse);
        expect(result.errors, hasLength(1));
        expect(result.errors.first, isA<InconsistentPathError>());
      });

      test('NEGATIVE: reader at stream head (produce position) is NOT well-typed', () {
        // Type: Stream ::= [] ; [_|Stream]
        // Term: [X?|Xs] - X? at produce position is wrong
        final env = createStreamEnv();
        final dfa = buildProgramDFA(env);
        final automaton = dfa.getAutomaton('Stream');
        final term = ModedCompound.listCons(
          Mode.produce,
          ModedVariable.reader('X'), // Wrong! produce position needs writer
          ModedVariable.writer('Xs'),
        );

        final result = checkModedTerm(term, automaton, dfa);

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
        final env = createStreamEnv();
        final dfa = buildProgramDFA(env);
        final automaton = dfa.getAutomaton('Stream');
        final term = ModedConstant.nil(Mode.produce);

        final result = checkModedTerm(term, automaton, dfa);

        expect(result.isWellTyped, isTrue);
      });

      test('cons with writer head at Stream position', () {
        // Type: Stream (head is produce mode)
        // Term: [X|Xs] (list with writer head and writer tail)
        final env = createStreamEnv();
        final dfa = buildProgramDFA(env);
        final automaton = dfa.getAutomaton('Stream');

        final term = ModedCompound.listCons(
          Mode.produce,
          ModedVariable.writer('X'),
          ModedVariable.writer('Xs'),
        );

        final result = checkModedTerm(term, automaton, dfa);

        expect(result.isWellTyped, isTrue);
        expect(result.variableTypes, contains('X'));
        expect(result.variableTypes, contains('Xs'));
      });
    });

    // =========================================================================
    // Complementarity Tests
    // =========================================================================

    group('Variable Complementarity', () {
      test('X and X? at complementary positions in Pair are well-typed', () {
        // Type: Pair ::= pair(_, _?)
        // Term: pair(X, X?) - X at produce position, X? at consume position
        final env = createPairEnv();
        final dfa = buildProgramDFA(env);
        final automaton = dfa.getAutomaton('Pair');

        final term = ModedCompound(Mode.produce, 'pair', 2, [
          ModedVariable.writer('X'),  // arg1: produce position
          ModedVariable.reader('X'),  // arg2: consume position
        ]);

        final result = checkModedTerm(term, automaton, dfa);

        expect(result.isWellTyped, isTrue);
        expect(result.variableTypes, contains('X'));
        expect(result.variableTypes, contains('X?'));
      });

      test('NEGATIVE: X and X? at wrong positions in Pair', () {
        // Type: Pair ::= pair(_, _?)
        // Term: pair(X?, X) - X? at produce position (wrong), X at consume position (wrong)
        final env = createPairEnv();
        final dfa = buildProgramDFA(env);
        final automaton = dfa.getAutomaton('Pair');

        final term = ModedCompound(Mode.produce, 'pair', 2, [
          ModedVariable.reader('X'),  // arg1: produce position - WRONG
          ModedVariable.writer('X'),  // arg2: consume position - WRONG
        ]);

        final result = checkModedTerm(term, automaton, dfa);

        expect(result.isWellTyped, isFalse);
      });
    });

    // =========================================================================
    // PathCheckResult Tests
    // =========================================================================

    group('checkPathAgainstAutomaton', () {
      test('single-step reader variable path at _? state', () {
        final dfa = buildProgramDFA(TypeEnvironment.empty());
        final automaton = dfa.getAutomaton('_?');
        final path = ModedPath([
          PathStep(
            symbol: 'X?',
            argIndex: 0,
            mode: Mode.consume,
            isVariable: true,
            isReader: true,
          ),
        ]);

        final result = checkPathAgainstAutomaton(path, automaton, dfa);

        expect(result.isConsistent, isTrue);
        expect(result.variableAssignment, isNotNull);
        expect(result.variableAssignment!.mode, equals(Mode.consume));
      });

      test('single-step writer variable path at _ state', () {
        final dfa = buildProgramDFA(TypeEnvironment.empty());
        final automaton = dfa.getAutomaton('_');
        final path = ModedPath([
          PathStep(
            symbol: 'X',
            argIndex: 0,
            mode: Mode.produce,
            isVariable: true,
            isReader: false,
          ),
        ]);

        final result = checkPathAgainstAutomaton(path, automaton, dfa);

        expect(result.isConsistent, isTrue);
        expect(result.variableAssignment, isNotNull);
        expect(result.variableAssignment!.mode, equals(Mode.produce));
      });

      test('two-step path through Stream structure', () {
        final env = createStreamEnv();
        final dfa = buildProgramDFA(env);
        final automaton = dfa.getAutomaton('Stream');

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

        final result = checkPathAgainstAutomaton(path, automaton, dfa);

        expect(result.isConsistent, isTrue);
      });

      test('NEGATIVE: invalid path - no transition', () {
        final env = createStreamEnv();
        final dfa = buildProgramDFA(env);
        final automaton = dfa.getAutomaton('Stream');

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

        final result = checkPathAgainstAutomaton(path, automaton, dfa);

        expect(result.isConsistent, isFalse);
        expect(result.reason, contains('No transition'));
      });
    });

    // =========================================================================
    // VariableTypeInfo Tests
    // =========================================================================

    group('VariableTypeInfo', () {
      test('equality based on state, mode, and reader status', () {
        final state = DFAState('_', isComplement: false, isFinal: true);

        final info1 = VariableTypeInfo(
          typeState: state,
          mode: Mode.produce,
          isReader: false,
        );
        final info2 = VariableTypeInfo(
          typeState: state,
          mode: Mode.produce,
          isReader: false,
        );
        final info3 = VariableTypeInfo(
          typeState: state,
          mode: Mode.consume,
          isReader: true,
        );

        expect(info1, equals(info2));
        expect(info1, isNot(equals(info3)));
      });

      test('toString formats nicely', () {
        final state = DFAState('Stream', isComplement: false, isFinal: false);
        final info = VariableTypeInfo(
          typeState: state,
          mode: Mode.produce,
          isReader: false,
        );

        expect(info.toString(), contains('Stream'));
        expect(info.toString(), contains('↑'));
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
        final state1 = DFAState('_', isComplement: false, isFinal: true);
        final state2 = DFAState('_', isComplement: true, isFinal: true);

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
        final env = createStreamEnv();
        final dfa = buildProgramDFA(env);
        final automaton = dfa.getAutomaton('Stream');

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

        final result = checkModedTerm(term, automaton, dfa);

        expect(result.isWellTyped, isTrue);
        expect(result.variableTypes, contains('X'));
        expect(result.variableTypes, contains('Y'));
        expect(result.variableTypes, contains('Zs'));
      });
    });

    // =========================================================================
    // WellTypedResult Factory Tests
    // =========================================================================

    group('WellTypedResult Factories', () {
      test('success factory creates well-typed result', () {
        final result = WellTypedResult.success({
          'X': VariableTypeInfo(
            typeState: DFAState('_', isComplement: false, isFinal: true),
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
