// test/analysis/type_checker/program_dfa_test.dart
//
// Tests for ProgramDFA per spec: docs/modules/type-dfa.md v1.0
// Paper Reference: Section 4.1 (lines 19-24, 47-53), Definition 4.3 (lines 283-298)

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/program_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';

void main() {
  group('DFAState', () {
    test('name getter returns baseName for non-complement state', () {
      final state = DFAState('Stream', isComplement: false, isFinal: false);
      expect(state.name, equals('Stream'));
    });

    test('name getter returns baseName? for complement state', () {
      final state = DFAState('Stream', isComplement: true, isFinal: false);
      expect(state.name, equals('Stream?'));
    });

    test('complement returns opposite state', () {
      final state = DFAState('Stream', isComplement: false, isFinal: false);
      final comp = state.complement;
      expect(comp.baseName, equals('Stream'));
      expect(comp.isComplement, isTrue);
      expect(comp.isFinal, isFalse);
    });

    test('double complement returns original state', () {
      final state = DFAState('Stream', isComplement: false, isFinal: false);
      final doubleComp = state.complement.complement;
      expect(doubleComp.baseName, equals(state.baseName));
      expect(doubleComp.isComplement, equals(state.isComplement));
    });

    test('isWildcard returns true for _ and _?', () {
      final produced = DFAState('_', isComplement: false, isFinal: true);
      final consumed = DFAState('_', isComplement: true, isFinal: true);
      expect(produced.isWildcard, isTrue);
      expect(consumed.isWildcard, isTrue);
    });

    test('isProducedWildcard and isConsumedWildcard', () {
      final produced = DFAState('_', isComplement: false, isFinal: true);
      final consumed = DFAState('_', isComplement: true, isFinal: true);

      expect(produced.isProducedWildcard, isTrue);
      expect(produced.isConsumedWildcard, isFalse);
      expect(consumed.isProducedWildcard, isFalse);
      expect(consumed.isConsumedWildcard, isTrue);
    });

    test('isIntegerType and isStringType', () {
      final intState = DFAState('Integer', isComplement: false, isFinal: false);
      final strState = DFAState('String', isComplement: false, isFinal: false);

      expect(intState.isIntegerType, isTrue);
      expect(intState.isStringType, isFalse);
      expect(strState.isIntegerType, isFalse);
      expect(strState.isStringType, isTrue);
    });

    test('isAnonymousFinal', () {
      final anonFinal = DFAState('_FINAL_', isComplement: false, isFinal: true);
      final produced = DFAState('_', isComplement: false, isFinal: true);

      expect(anonFinal.isAnonymousFinal, isTrue);
      expect(produced.isAnonymousFinal, isFalse);
    });

    test('equality based on baseName and isComplement', () {
      final s1 = DFAState('Stream', isComplement: false, isFinal: false);
      final s2 = DFAState('Stream', isComplement: false, isFinal: false);
      final s3 = DFAState('Stream', isComplement: true, isFinal: false);

      expect(s1 == s2, isTrue);
      expect(s1 == s3, isFalse);
      expect(s1.hashCode, equals(s2.hashCode));
    });
  });

  group('TransitionLabel', () {
    test('functor label with mode', () {
      final label = TransitionLabel.functor('[|]', 2, 1, mode: Mode.produce);
      expect(label.symbol, equals('[|]'));
      expect(label.arity, equals(2));
      expect(label.argIndex, equals(1));
      expect(label.mode, equals(Mode.produce));
    });

    test('constant label has arity 0 and argIndex 0', () {
      final label = TransitionLabel.constant(42);
      expect(label.symbol, equals('42'));
      expect(label.arity, equals(0));
      expect(label.argIndex, equals(0));
      expect(label.mode, isNull);
    });

    test('complement flips mode', () {
      final label = TransitionLabel.functor('[|]', 2, 1, mode: Mode.produce);
      final comp = label.complement;
      expect(comp.symbol, equals('[|]'));
      expect(comp.mode, equals(Mode.consume));
    });

    test('complement with null mode stays null', () {
      final label = TransitionLabel.functor('merge', 3, 1, mode: null);
      final comp = label.complement;
      expect(comp.mode, isNull);
    });

    test('equality includes mode', () {
      final l1 = TransitionLabel.functor('[|]', 2, 1, mode: Mode.produce);
      final l2 = TransitionLabel.functor('[|]', 2, 1, mode: Mode.produce);
      final l3 = TransitionLabel.functor('[|]', 2, 1, mode: Mode.consume);

      expect(l1 == l2, isTrue);
      expect(l1 == l3, isFalse);
    });
  });

  group('ProgramDFA Construction', () {
    test('system states exist in pairs', () {
      final env = TypeEnvironment.empty();
      final dfa = buildProgramDFA(env);

      // Wildcard pair
      expect(dfa.getState('_').isFinal, isTrue);
      expect(dfa.getState('_').isComplement, isFalse);
      expect(dfa.getState('_?').isFinal, isTrue);
      expect(dfa.getState('_?').isComplement, isTrue);

      // Integer pair
      expect(dfa.getState('Integer').isFinal, isFalse);
      expect(dfa.getState('Integer?').isFinal, isFalse);

      // String pair
      expect(dfa.getState('String').isFinal, isFalse);
      expect(dfa.getState('String?').isFinal, isFalse);

      // Anonymous final
      expect(dfa.getState('_FINAL_').isFinal, isTrue);
    });

    test('defined type creates state pair', () {
      final env = TypeEnvironment.empty();
      env.addType(TypeDef('Stream', [ListNilAlt(0, 0)], 0, 0));

      final dfa = buildProgramDFA(env);

      expect(dfa.getState('Stream').isComplement, isFalse);
      expect(dfa.getState('Stream?').isComplement, isTrue);
    });

    test('defined type creates automaton pair', () {
      final env = TypeEnvironment.empty();
      env.addType(TypeDef('Stream', [ListNilAlt(0, 0)], 0, 0));

      final dfa = buildProgramDFA(env);

      expect(dfa.getAutomaton('Stream'), isNotNull);
      expect(dfa.getAutomaton('Stream?'), isNotNull);
    });

    test('procedure creates single state (no complement)', () {
      final env = TypeEnvironment.empty();
      env.addType(TypeDef('Stream', [ListNilAlt(0, 0)], 0, 0));
      env.addProcedure(ProcDecl('merge', [
        TypeRef('Stream', 0, 0, isInput: true),
        TypeRef('Stream', 0, 0, isInput: true),
        TypeRef('Stream', 0, 0),
      ], 0, 0));

      final dfa = buildProgramDFA(env);

      expect(dfa.getState('merge/3'), isNotNull);
      expect(() => dfa.getState('merge/3?'), throwsStateError);
    });
  });

  group('Stream Automaton', () {
    late ProgramDFA dfa;

    setUp(() {
      final env = TypeEnvironment.empty();
      // Stream ::= [] ; [_|Stream]
      env.addType(TypeDef('Stream', [
        ListNilAlt(0, 0),
        ListConsAlt(PrimitiveModeAlt(false, 0, 0), TypeRef('Stream', 0, 0), 0, 0),
      ], 0, 0));
      dfa = buildProgramDFA(env);
    });

    test('Stream automaton has produce modes', () {
      final automaton = dfa.getAutomaton('Stream');
      final state = automaton.startState;

      // [] → _FINAL_
      final nilLabel = TransitionLabel.constant('[]');
      expect(automaton.transition(state, nilLabel)?.name, equals('_FINAL_'));

      // [|](2,1):↑ → _
      final headLabel = TransitionLabel.functor('[|]', 2, 1, mode: Mode.produce);
      expect(automaton.transition(state, headLabel)?.name, equals('_'));

      // [|](2,2):↑ → Stream
      final tailLabel = TransitionLabel.functor('[|]', 2, 2, mode: Mode.produce);
      expect(automaton.transition(state, tailLabel)?.name, equals('Stream'));
    });

    test('Stream? automaton has consume modes', () {
      final automaton = dfa.getAutomaton('Stream?');
      final state = automaton.startState;

      // [] → _FINAL_
      final nilLabel = TransitionLabel.constant('[]');
      expect(automaton.transition(state, nilLabel)?.name, equals('_FINAL_'));

      // [|](2,1):↓ → _?
      final headLabel = TransitionLabel.functor('[|]', 2, 1, mode: Mode.consume);
      expect(automaton.transition(state, headLabel)?.name, equals('_?'));

      // [|](2,2):↓ → Stream?
      final tailLabel = TransitionLabel.functor('[|]', 2, 2, mode: Mode.consume);
      expect(automaton.transition(state, tailLabel)?.name, equals('Stream?'));
    });

    test('Stream and Stream? automata have different start states', () {
      final streamAuto = dfa.getAutomaton('Stream');
      final streamCompAuto = dfa.getAutomaton('Stream?');

      expect(streamAuto.startState.name, equals('Stream'));
      expect(streamCompAuto.startState.name, equals('Stream?'));
    });
  });

  group('HollowStream (nested complement)', () {
    late ProgramDFA dfa;

    setUp(() {
      final env = TypeEnvironment.empty();
      // HollowStream ::= [] ; [_?|HollowStream]
      env.addType(TypeDef('HollowStream', [
        ListNilAlt(0, 0),
        ListConsAlt(PrimitiveModeAlt(true, 0, 0), TypeRef('HollowStream', 0, 0), 0, 0),
      ], 0, 0));
      dfa = buildProgramDFA(env);
    });

    test('HollowStream automaton has consume mode for head', () {
      final automaton = dfa.getAutomaton('HollowStream');
      final state = automaton.startState;

      // [|](2,1):↓ → _? (isInput=true flips to consume)
      final headLabel = TransitionLabel.functor('[|]', 2, 1, mode: Mode.consume);
      expect(automaton.transition(state, headLabel)?.name, equals('_?'));

      // [|](2,2):↑ → HollowStream
      final tailLabel = TransitionLabel.functor('[|]', 2, 2, mode: Mode.produce);
      expect(automaton.transition(state, tailLabel)?.name, equals('HollowStream'));
    });

    test('HollowStream? automaton has produce mode for head (XOR)', () {
      final automaton = dfa.getAutomaton('HollowStream?');
      final state = automaton.startState;

      // [|](2,1):↑ → _ (XOR: isInput ^ isComplement = true ^ true = false)
      final headLabel = TransitionLabel.functor('[|]', 2, 1, mode: Mode.produce);
      expect(automaton.transition(state, headLabel)?.name, equals('_'));

      // [|](2,2):↓ → HollowStream?
      final tailLabel = TransitionLabel.functor('[|]', 2, 2, mode: Mode.consume);
      expect(automaton.transition(state, tailLabel)?.name, equals('HollowStream?'));
    });
  });

  group('Procedure Automaton', () {
    late ProgramDFA dfa;

    setUp(() {
      final env = TypeEnvironment.empty();
      env.addType(TypeDef('Stream', [ListNilAlt(0, 0)], 0, 0));
      // procedure merge(Stream?, Stream?, Stream)
      env.addProcedure(ProcDecl('merge', [
        TypeRef('Stream', 0, 0, isInput: true),
        TypeRef('Stream', 0, 0, isInput: true),
        TypeRef('Stream', 0, 0),
      ], 0, 0));
      dfa = buildProgramDFA(env);
    });

    test('procedure automaton transitions to declared types directly', () {
      final automaton = dfa.getAutomaton('merge/3');
      final state = automaton.startState;

      // merge(3,1) → Stream? (declared as Stream?)
      final arg1Label = TransitionLabel.functor('merge', 3, 1, mode: null);
      expect(automaton.transition(state, arg1Label)?.name, equals('Stream?'));

      // merge(3,2) → Stream? (declared as Stream?)
      final arg2Label = TransitionLabel.functor('merge', 3, 2, mode: null);
      expect(automaton.transition(state, arg2Label)?.name, equals('Stream?'));

      // merge(3,3) → Stream (declared as Stream)
      final arg3Label = TransitionLabel.functor('merge', 3, 3, mode: null);
      expect(automaton.transition(state, arg3Label)?.name, equals('Stream'));
    });
  });

  group('Leaf Consistency', () {
    late ProgramDFA dfa;

    setUp(() {
      dfa = buildProgramDFA(TypeEnvironment.empty());
    });

    test('writer at _ state with produce mode is consistent', () {
      final state = dfa.getState('_');
      final leaf = LeafTerm.writer('X', mode: Mode.produce);

      final result = checkLeafConsistency(leaf, state, dfa);

      expect(result.isConsistent, isTrue);
      expect(result.type?.name, equals('_'));
    });

    test('NEGATIVE: reader at produce mode is inconsistent (at _ state)', () {
      final state = dfa.getState('_');
      // With Mode Correspondence Property, we check mode matches variable type
      // Reader requires consume mode, not produce mode
      final leaf = LeafTerm.reader('X?', mode: Mode.produce);

      final result = checkLeafConsistency(leaf, state, dfa);

      expect(result.isConsistent, isFalse);
      expect(result.reason, contains('reader'));
    });

    test('reader at _? state with consume mode is consistent', () {
      final state = dfa.getState('_?');
      final leaf = LeafTerm.reader('X?', mode: Mode.consume);

      final result = checkLeafConsistency(leaf, state, dfa);

      expect(result.isConsistent, isTrue);
      expect(result.type?.name, equals('_?'));
    });

    test('NEGATIVE: writer at consume mode is inconsistent (at _? state)', () {
      final state = dfa.getState('_?');
      // With Mode Correspondence Property, we check mode matches variable type
      // Writer requires produce mode, not consume mode
      final leaf = LeafTerm.writer('X', mode: Mode.consume);

      final result = checkLeafConsistency(leaf, state, dfa);

      expect(result.isConsistent, isFalse);
      expect(result.reason, contains('writer'));
    });

    test('integer literal at Integer state is consistent', () {
      final state = dfa.getState('Integer');
      final leaf = LeafTerm.integerConstant(42);

      final result = checkLeafConsistency(leaf, state, dfa);

      expect(result.isConsistent, isTrue);
      expect(result.type?.name, equals('_FINAL_'));
    });

    test('NEGATIVE: string literal at Integer state is inconsistent', () {
      final state = dfa.getState('Integer');
      final leaf = LeafTerm.stringConstant('hello');

      final result = checkLeafConsistency(leaf, state, dfa);

      expect(result.isConsistent, isFalse);
    });

    test('writer at Integer state (non-complement) with produce is consistent', () {
      final state = dfa.getState('Integer');
      final leaf = LeafTerm.writer('N', mode: Mode.produce);

      final result = checkLeafConsistency(leaf, state, dfa);

      expect(result.isConsistent, isTrue);
      expect(result.type?.name, equals('Integer'));
    });

    test('reader at Integer? state (complement) with consume is consistent', () {
      final state = dfa.getState('Integer?');
      final leaf = LeafTerm.reader('N?', mode: Mode.consume);

      final result = checkLeafConsistency(leaf, state, dfa);

      expect(result.isConsistent, isTrue);
      expect(result.type?.name, equals('Integer?'));
    });

    // Note: With Mode Correspondence Property, reader at consume is always consistent
    // (the state's complement status is already encoded in the structural mode)
    test('NEGATIVE: reader at produce mode is inconsistent', () {
      final state = dfa.getState('Integer');
      final leaf = LeafTerm.reader('N?', mode: Mode.produce);

      final result = checkLeafConsistency(leaf, state, dfa);

      expect(result.isConsistent, isFalse,
          reason: 'Reader requires consume mode, not produce mode');
    });

    test('writer at type state (non-complement) with produce is consistent', () {
      final env = TypeEnvironment.empty();
      env.addType(TypeDef('Stream', [ListNilAlt(0, 0)], 0, 0));
      final dfa = buildProgramDFA(env);

      final state = dfa.getState('Stream');
      final leaf = LeafTerm.writer('Xs', mode: Mode.produce);

      final result = checkLeafConsistency(leaf, state, dfa);

      expect(result.isConsistent, isTrue);
      expect(result.type?.name, equals('Stream'));
    });

    test('reader at type? state (complement) with consume is consistent', () {
      final env = TypeEnvironment.empty();
      env.addType(TypeDef('Stream', [ListNilAlt(0, 0)], 0, 0));
      final dfa = buildProgramDFA(env);

      final state = dfa.getState('Stream?');
      final leaf = LeafTerm.reader('Xs?', mode: Mode.consume);

      final result = checkLeafConsistency(leaf, state, dfa);

      expect(result.isConsistent, isTrue);
      expect(result.type?.name, equals('Stream?'));
    });

    // Note: With Mode Correspondence Property, reader at consume is always consistent
    // (the state's complement status is already encoded in the structural mode)
    test('NEGATIVE: reader at produce mode at type state is inconsistent', () {
      final env = TypeEnvironment.empty();
      env.addType(TypeDef('Stream', [ListNilAlt(0, 0)], 0, 0));
      final dfa = buildProgramDFA(env);

      final state = dfa.getState('Stream');
      final leaf = LeafTerm.reader('Xs?', mode: Mode.produce);

      final result = checkLeafConsistency(leaf, state, dfa);

      expect(result.isConsistent, isFalse,
          reason: 'Reader requires consume mode, not produce mode');
    });
  });

  group('Error conditions', () {
    test('NEGATIVE: unknown type in procedure throws error', () {
      final env = TypeEnvironment.empty();
      env.addProcedure(ProcDecl('foo', [TypeRef('Unknown', 0, 0)], 0, 0));

      expect(() => buildProgramDFA(env), throwsA(isA<UnknownTypeError>()));
    });

    test('NEGATIVE: unknown state name throws StateError', () {
      final dfa = buildProgramDFA(TypeEnvironment.empty());

      expect(() => dfa.getState('NotAState'), throwsStateError);
    });

    test('NEGATIVE: unknown automaton name throws StateError', () {
      final dfa = buildProgramDFA(TypeEnvironment.empty());

      expect(() => dfa.getAutomaton('NotAType'), throwsStateError);
    });
  });
}
