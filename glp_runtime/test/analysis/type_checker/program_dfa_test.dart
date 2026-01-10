// test/analysis/type_checker/program_dfa_test.dart
//
// Tests for ProgramDFA per spec: docs/modules/type-dfa.md v0.7
// Paper Reference: Section 4.1 (lines 32-44), Definition 4.3 (lines 247-262)

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/program_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';

void main() {
  group('ProgramDFA Construction', () {
    test('ProgramDFA has final states _ and _?', () {
      final env = TypeEnvironment.empty();
      final dfa = buildProgramDFA(env);

      expect(dfa.getState('_').isFinal, isTrue);
      expect(dfa.getState('_?').isFinal, isTrue);
      expect(dfa.getState('_FINAL_').isFinal, isTrue);
    });

    test('ProgramDFA has type states Integer and String (not final)', () {
      final env = TypeEnvironment.empty();
      final dfa = buildProgramDFA(env);

      expect(dfa.getState('Integer').isFinal, isFalse);
      expect(dfa.getState('String').isFinal, isFalse);
    });

    test('Defined type creates state in DFA', () {
      final env = TypeEnvironment.empty();
      env.addType(TypeDef('Stream', [
        ListNilAlt(0, 0),
        ListConsAlt(PrimitiveModeAlt(false, 0, 0), TypeRef('Stream', 0, 0), 0, 0),
      ], 0, 0));

      final dfa = buildProgramDFA(env);

      expect(dfa.getState('Stream'), isNotNull);
      expect(dfa.getState('Stream').isFinal, isFalse);
    });

    test('Procedure declaration creates state in DFA', () {
      final env = TypeEnvironment.empty();
      env.addType(TypeDef('Stream', [ListNilAlt(0, 0)], 0, 0));
      env.addProcedure(ProcDecl('merge', [
        TypeRef('Stream', 0, 0, isInput: true),
        TypeRef('Stream', 0, 0, isInput: true),
        TypeRef('Stream', 0, 0),
      ], 0, 0));

      final dfa = buildProgramDFA(env);

      expect(dfa.getState('merge/3'), isNotNull);
    });

    test('List type creates transitions for nil and cons', () {
      final env = TypeEnvironment.empty();
      env.addType(TypeDef('Stream', [
        ListNilAlt(0, 0),
        ListConsAlt(PrimitiveModeAlt(false, 0, 0), TypeRef('Stream', 0, 0), 0, 0),
      ], 0, 0));

      final dfa = buildProgramDFA(env);
      final streamState = dfa.getState('Stream');

      // [] → _FINAL_
      final nilLabel = TransitionLabel.constant('[]');
      expect(dfa.transition(streamState, nilLabel)?.name, equals('_FINAL_'));

      // [|](2,1):↑ → _
      final headLabel = TransitionLabel.functor('[|]', 2, 1, mode: Mode.produce);
      expect(dfa.transition(streamState, headLabel)?.name, equals('_'));

      // [|](2,2):↑ → Stream
      final tailLabel = TransitionLabel.functor('[|]', 2, 2, mode: Mode.produce);
      expect(dfa.transition(streamState, tailLabel)?.name, equals('Stream'));
    });

    test('Procedure state has transitions to argument type states', () {
      final env = TypeEnvironment.empty();
      env.addType(TypeDef('Stream', [ListNilAlt(0, 0)], 0, 0));
      env.addProcedure(ProcDecl('merge', [
        TypeRef('Stream', 0, 0, isInput: true),
        TypeRef('Stream', 0, 0, isInput: true),
        TypeRef('Stream', 0, 0),
      ], 0, 0));

      final dfa = buildProgramDFA(env);
      final procState = dfa.getState('merge/3');

      final arg1Label = TransitionLabel.functor('merge', 3, 1);
      final arg2Label = TransitionLabel.functor('merge', 3, 2);
      final arg3Label = TransitionLabel.functor('merge', 3, 3);

      expect(dfa.transition(procState, arg1Label)?.name, equals('Stream'));
      expect(dfa.transition(procState, arg2Label)?.name, equals('Stream'));
      expect(dfa.transition(procState, arg3Label)?.name, equals('Stream'));
    });

    test('Complemented type in definition flips transition mode', () {
      final env = TypeEnvironment.empty();
      // HollowStream ::= [] ; [_?|HollowStream].
      env.addType(TypeDef('HollowStream', [
        ListNilAlt(0, 0),
        ListConsAlt(PrimitiveModeAlt(true, 0, 0), TypeRef('HollowStream', 0, 0), 0, 0),
      ], 0, 0));

      final dfa = buildProgramDFA(env);
      final state = dfa.getState('HollowStream');

      // [|](2,1):↓ → _? (mode flipped because _?)
      final headLabel = TransitionLabel.functor('[|]', 2, 1, mode: Mode.consume);
      expect(dfa.transition(state, headLabel)?.name, equals('_?'));
    });

    test('NEGATIVE: Unknown type in procedure declaration throws error', () {
      final env = TypeEnvironment.empty();
      env.addProcedure(ProcDecl('foo', [TypeRef('Unknown', 0, 0)], 0, 0));

      expect(() => buildProgramDFA(env), throwsA(isA<UnknownTypeError>()));
    });
  });

  group('Leaf Consistency', () {
    test('Writer variable at _ state with produce mode is consistent', () {
      final dfa = buildProgramDFA(TypeEnvironment.empty());
      final state = dfa.getState('_');
      final leaf = LeafTerm.writer('X', mode: Mode.produce);

      final result = checkLeafConsistency(leaf, state, dfa, complement: false);

      expect(result.isConsistent, isTrue);
      expect(result.type?.name, equals('_'));
    });

    test('NEGATIVE: Reader variable at _ state is inconsistent', () {
      final dfa = buildProgramDFA(TypeEnvironment.empty());
      final state = dfa.getState('_');
      final leaf = LeafTerm.reader('X?', mode: Mode.consume);

      final result = checkLeafConsistency(leaf, state, dfa, complement: false);

      expect(result.isConsistent, isFalse);
    });

    test('Reader variable at _? state with consume mode is consistent', () {
      final dfa = buildProgramDFA(TypeEnvironment.empty());
      final state = dfa.getState('_?');
      final leaf = LeafTerm.reader('X?', mode: Mode.consume);

      final result = checkLeafConsistency(leaf, state, dfa, complement: false);

      expect(result.isConsistent, isTrue);
      expect(result.type?.name, equals('_?'));
    });

    test('NEGATIVE: Writer variable at _? state is inconsistent', () {
      final dfa = buildProgramDFA(TypeEnvironment.empty());
      final state = dfa.getState('_?');
      final leaf = LeafTerm.writer('X', mode: Mode.produce);

      final result = checkLeafConsistency(leaf, state, dfa, complement: false);

      expect(result.isConsistent, isFalse);
    });

    test('Integer literal at Integer state is consistent', () {
      final dfa = buildProgramDFA(TypeEnvironment.empty());
      final state = dfa.getState('Integer');
      final leaf = LeafTerm.integerConstant(42);

      final result = checkLeafConsistency(leaf, state, dfa, complement: false);

      expect(result.isConsistent, isTrue);
      expect(result.type?.name, equals('_FINAL_'));
    });

    test('NEGATIVE: String literal at Integer state is inconsistent', () {
      final dfa = buildProgramDFA(TypeEnvironment.empty());
      final state = dfa.getState('Integer');
      final leaf = LeafTerm.stringConstant('hello');

      final result = checkLeafConsistency(leaf, state, dfa, complement: false);

      expect(result.isConsistent, isFalse);
    });

    test('Writer variable at Integer state with produce mode is consistent', () {
      final dfa = buildProgramDFA(TypeEnvironment.empty());
      final state = dfa.getState('Integer');
      final leaf = LeafTerm.writer('N', mode: Mode.produce);

      final result = checkLeafConsistency(leaf, state, dfa, complement: false);

      expect(result.isConsistent, isTrue);
      expect(result.type?.name, equals('Integer'));
    });

    test('Reader variable at Integer state with consume mode is consistent', () {
      final dfa = buildProgramDFA(TypeEnvironment.empty());
      final state = dfa.getState('Integer');
      final leaf = LeafTerm.reader('N?', mode: Mode.consume);

      final result = checkLeafConsistency(leaf, state, dfa, complement: false);

      expect(result.isConsistent, isTrue);
      expect(result.type?.name, equals('Integer'));
    });

    test('Writer variable at type state with produce mode is consistent', () {
      final env = TypeEnvironment.empty();
      env.addType(TypeDef('Stream', [ListNilAlt(0, 0)], 0, 0));
      final dfa = buildProgramDFA(env);
      final state = dfa.getState('Stream');
      final leaf = LeafTerm.writer('Xs', mode: Mode.produce);

      final result = checkLeafConsistency(leaf, state, dfa, complement: false);

      expect(result.isConsistent, isTrue);
      expect(result.type?.name, equals('Stream'));
    });

    test('NEGATIVE: Reader variable at type state with produce mode is inconsistent', () {
      final env = TypeEnvironment.empty();
      env.addType(TypeDef('Stream', [ListNilAlt(0, 0)], 0, 0));
      final dfa = buildProgramDFA(env);
      final state = dfa.getState('Stream');
      final leaf = LeafTerm.reader('Xs?', mode: Mode.produce);

      final result = checkLeafConsistency(leaf, state, dfa, complement: false);

      expect(result.isConsistent, isFalse);
    });
  });

  group('Complement Flag', () {
    test('With complement, reader at _ is consistent (mode flips to produce)', () {
      final dfa = buildProgramDFA(TypeEnvironment.empty());
      final state = dfa.getState('_');
      final leaf = LeafTerm.reader('X?', mode: Mode.consume);

      final result = checkLeafConsistency(leaf, state, dfa, complement: true);

      expect(result.isConsistent, isTrue);
    });

    test('With complement, writer at _? is consistent (mode flips to consume)', () {
      final dfa = buildProgramDFA(TypeEnvironment.empty());
      final state = dfa.getState('_?');
      final leaf = LeafTerm.writer('X', mode: Mode.produce);

      final result = checkLeafConsistency(leaf, state, dfa, complement: true);

      expect(result.isConsistent, isTrue);
    });

    test('NEGATIVE: With complement, writer at _ is inconsistent', () {
      final dfa = buildProgramDFA(TypeEnvironment.empty());
      final state = dfa.getState('_');
      final leaf = LeafTerm.writer('X', mode: Mode.produce);

      final result = checkLeafConsistency(leaf, state, dfa, complement: true);

      expect(result.isConsistent, isFalse);
    });
  });

  group('DFAState properties', () {
    test('isWildcard returns true for _ and _?', () {
      final dfa = buildProgramDFA(TypeEnvironment.empty());

      expect(dfa.getState('_').isWildcard, isTrue);
      expect(dfa.getState('_?').isWildcard, isTrue);
      expect(dfa.getState('Integer').isWildcard, isFalse);
    });

    test('isProducedWildcard and isConsumedWildcard', () {
      final dfa = buildProgramDFA(TypeEnvironment.empty());

      expect(dfa.getState('_').isProducedWildcard, isTrue);
      expect(dfa.getState('_').isConsumedWildcard, isFalse);
      expect(dfa.getState('_?').isProducedWildcard, isFalse);
      expect(dfa.getState('_?').isConsumedWildcard, isTrue);
    });

    test('isIntegerType and isStringType', () {
      final dfa = buildProgramDFA(TypeEnvironment.empty());

      expect(dfa.getState('Integer').isIntegerType, isTrue);
      expect(dfa.getState('Integer').isStringType, isFalse);
      expect(dfa.getState('String').isIntegerType, isFalse);
      expect(dfa.getState('String').isStringType, isTrue);
    });

    test('isAnonymousFinal', () {
      final dfa = buildProgramDFA(TypeEnvironment.empty());

      expect(dfa.getState('_FINAL_').isAnonymousFinal, isTrue);
      expect(dfa.getState('_').isAnonymousFinal, isFalse);
    });
  });
}
