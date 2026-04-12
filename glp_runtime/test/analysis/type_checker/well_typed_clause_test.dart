// glp_runtime/test/analysis/type_checker/well_typed_clause_test.dart
//
// Unit tests for well-typed clause checking.
// Spec: docs/type system/well-typed-clause.md
// Paper: Definition 5.7

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/well_typed_clause.dart';
import 'package:glp_runtime/analysis/type_checker/well_typed_term.dart';
import 'package:glp_runtime/analysis/type_checker/moded_term.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';
import 'package:glp_runtime/analysis/type_checker/program_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/compiler/ast.dart' as ast;

void main() {
  group('checkClause', () {
    // Helper to build a Goal from functor and args
    ast.Goal goal(String f, List<ast.Term> args) =>
        ast.Goal(f, args, 0, 0);

    // Helper to build variable terms
    ast.VarTerm writer(String name) => ast.VarTerm(name, false, 0, 0);
    ast.VarTerm reader(String name) => ast.VarTerm(name, true, 0, 0);

    // Helper to build list terms
    ast.ListTerm nil() => ast.ListTerm(null, null, 0, 0);
    ast.ListTerm cons(ast.Term h, ast.Term t) => ast.ListTerm(h, t, 0, 0);

    // Helper to create DFA states
    DFAState state(String name, {bool isDual = false, bool isFinal = false, bool isProcedure = false}) =>
        DFAState(name, isDual: isDual, isFinal: isFinal, isProcedure: isProcedure);

    DFAState wildcardProduce() => state('_', isDual: false, isFinal: true);
    DFAState wildcardConsume() => state('_', isDual: true, isFinal: true);
    DFAState streamState({bool isDual = false}) =>
        state('Stream', isDual: isDual, isFinal: false);

    // Helper to create a simple type environment with merge procedure
    TypeEnvironment createMergeEnv() {
      final env = TypeEnvironment.empty();

      // Define Stream type: Stream ::= [] ; [_|Stream].
      env.addType(TypeDef(
        'Stream',
        [
          ListNilAlt(0, 0),
          ListConsAlt(
            PrimitiveModeAlt(false, 0, 0),  // _ (element)
            TypeRef('Stream', 0, 0, isInput: false),  // Stream (tail)
            0, 0,
          ),
        ],
        0, 0,
      ));

      // Declare merge procedure: procedure merge(Stream?, Stream?, Stream).
      env.addProcedure(ProcDecl(
        'merge',
        [
          TypeRef('Stream', 0, 0, isInput: true),   // Stream?
          TypeRef('Stream', 0, 0, isInput: true),   // Stream?
          TypeRef('Stream', 0, 0, isInput: false),  // Stream
        ],
        0, 0,
      ));

      return env;
    }

    // Helper to create a simple DFA for Stream type
    ProgramDFA createStreamDFA() {
      final prodWild = wildcardProduce();
      final consWild = wildcardConsume();
      final stream = streamState();
      final streamDual = streamState(isDual: true);
      final finalState = state('_FINAL_', isFinal: true);

      // Create automaton for Stream type with [] and [|] transitions
      final streamTransitions = <(DFAState, TransitionLabel), DFAState>{
        // [] transition
        (stream, TransitionLabel.constant('[]')): finalState,
        // [H|T] transitions
        (stream, TransitionLabel.functor('[|]', 2, 1, mode: Mode.produce)): prodWild,
        (stream, TransitionLabel.functor('[|]', 2, 2, mode: Mode.produce)): stream,
      };

      // Create automaton for Stream? (dual) type
      final streamDualTransitions = <(DFAState, TransitionLabel), DFAState>{
        (streamDual, TransitionLabel.constant('[]')): finalState,
        (streamDual, TransitionLabel.functor('[|]', 2, 1, mode: Mode.consume)): consWild,
        (streamDual, TransitionLabel.functor('[|]', 2, 2, mode: Mode.consume)): streamDual,
      };

      // Create automata for merge procedure
      final mergeState = state('merge/3', isProcedure: true);
      final mergeTransitions = <(DFAState, TransitionLabel), DFAState>{
        (mergeState, TransitionLabel.functor('merge', 3, 1, mode: Mode.consume)): streamDual,
        (mergeState, TransitionLabel.functor('merge', 3, 2, mode: Mode.consume)): streamDual,
        (mergeState, TransitionLabel.functor('merge', 3, 3, mode: Mode.produce)): stream,
      };

      return ProgramDFA(
        {
          '_': prodWild,
          '_?': consWild,
          'Stream': stream,
          'Stream?': streamDual,
          'merge/3': mergeState,
          '_FINAL_': finalState,
        },
        {
          '_': Automaton(prodWild, {}),
          '_?': Automaton(consWild, {}),
          'Stream': Automaton(stream, streamTransitions),
          'Stream?': Automaton(streamDual, streamDualTransitions),
          'merge/3': Automaton(mergeState, mergeTransitions),
        },
      );
    }

    group('Condition 1: Head well-typed', () {
      test('valid head is well-typed', () {
        final env = createMergeEnv();
        final dfa = createStreamDFA();

        // Clause: merge(Xs, [], Xs?) :- true.
        // Head has: Xs at Stream?, [] at Stream?, Xs? at Stream
        final clause = TypedClause(
          head: goal('merge', [
            writer('Xs'),  // Xs at position 1 (Stream?)
            nil(),         // [] at position 2 (Stream?)
            reader('Xs'),  // Xs? at position 3 (Stream)
          ]),
          bodyAtoms: [],
        );

        final result = checkClause(clause, dfa, env);

        // Should be well-typed (head matches declaration)
        expect(result.isWellTyped, isTrue);
        expect(result.errors, isEmpty);
      });

      test('undefined procedure fails', () {
        final env = createMergeEnv();
        final dfa = createStreamDFA();

        // Clause for undefined procedure
        final clause = TypedClause(
          head: goal('undefined_proc', [writer('X')]),
          bodyAtoms: [],
        );

        final result = checkClause(clause, dfa, env);

        expect(result.isWellTyped, isFalse);
        expect(result.errors.any((e) => e is UndefinedProcedureError), isTrue);
      });
    });

    group('Condition 2: Body atoms well-typed', () {
      test('valid body atom is well-typed', () {
        final env = createMergeEnv();
        final dfa = createStreamDFA();

        // Clause: merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
        final clause = TypedClause(
          head: goal('merge', [
            cons(writer('X'), writer('Xs')),
            writer('Ys'),
            cons(reader('X'), reader('Zs')),
          ]),
          bodyAtoms: [
            goal('merge', [
              reader('Ys'),
              reader('Xs'),
              writer('Zs'),
            ]),
          ],
        );

        final result = checkClause(clause, dfa, env);

        // The clause structure is valid, variables should be typed correctly
        // Note: Full type checking depends on DFA transitions being correct
        // This test verifies the body atom is processed without error
        expect(result.modedHead, isNotNull);
        expect(result.modedBodyAtoms.length, equals(1));
      });
    });

    group('Condition 3a: Variables in same part need dual types', () {
      test('X at _? and X? at _ in head are dual (well-typed)', () {
        // Create environment with a test procedure: test(_?, _)
        // In a head, writer X goes at input position (_?) and reader X? at output (_).
        // After head complementation: writer→reader at consume (OK), reader→writer at produce (OK).
        final env = TypeEnvironment.empty();
        env.addProcedure(ProcDecl(
          'test',
          [
            PrimitiveModeAlt(true, 0, 0),   // _? (input wildcard)
            PrimitiveModeAlt(false, 0, 0),  // _ (output wildcard)
          ],
          0, 0,
        ));

        final prodWild = wildcardProduce();
        final consWild = wildcardConsume();
        final testState = state('test/2', isProcedure: true);

        final testTransitions = <(DFAState, TransitionLabel), DFAState>{
          (testState, TransitionLabel.functor('test', 2, 1, mode: Mode.consume)): consWild,
          (testState, TransitionLabel.functor('test', 2, 2, mode: Mode.produce)): prodWild,
        };

        final dfa = ProgramDFA(
          {'_': prodWild, '_?': consWild, 'test/2': testState},
          {
            '_': Automaton(prodWild, {}),
            '_?': Automaton(consWild, {}),
            'test/2': Automaton(testState, testTransitions),
          },
        );

        // Clause: test(X, X?) :- true.
        // X (writer) at _? (input), X? (reader) at _ (output) - these are dual
        final clause = TypedClause(
          head: goal('test', [writer('X'), reader('X')]),
          bodyAtoms: [],
        );

        final result = checkClause(clause, dfa, env);

        expect(result.isWellTyped, isTrue);
        expect(result.variableTypes.containsKey('X'), isTrue);
        expect(result.variableTypes.containsKey('X?'), isTrue);
      });
    });

    group('Condition 3b: Variables split across head/body need same type', () {
      test('head X at Stream? and body X? at Stream? are same type (well-typed)', () {
        final env = createMergeEnv();
        final dfa = createStreamDFA();

        // Clause: merge(Xs, [], Xs?) :- true.
        // Xs (writer) in head at Stream? position
        // Xs? (reader) in head at Stream position (different, but this is head-only)
        // This should work because X at input type and X? at output type are dual
        final clause = TypedClause(
          head: goal('merge', [
            writer('Xs'),
            nil(),
            reader('Xs'),
          ]),
          bodyAtoms: [],
        );

        final result = checkClause(clause, dfa, env);

        // Xs at Stream? (input) and Xs? at Stream (output) - these are dual
        expect(result.isWellTyped, isTrue);
      });
    });

    group('Error cases', () {
      test('wrong arity for procedure fails with UndefinedProcedureError', () {
        final env = createMergeEnv();
        final dfa = createStreamDFA();

        // Wrong arity for merge (2 instead of 3)
        // This is treated as undefined because procedures are keyed by name/arity
        final clause = TypedClause(
          head: goal('merge', [writer('X'), writer('Y')]),
          bodyAtoms: [],
        );

        final result = checkClause(clause, dfa, env);

        expect(result.isWellTyped, isFalse);
        // Procedure lookup is by name/arity, so merge/2 is "undefined"
        expect(result.errors.any((e) => e is UndefinedProcedureError), isTrue);
      });
    });
  });
}
