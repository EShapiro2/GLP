// test/analysis/type_checker/nfa_construction_test.dart
//
// Tests for NFA construction and subset construction.
// Phase 1: Verify NFA/DFA pipeline independently of type checking.

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';
import 'package:glp_runtime/analysis/type_checker/moded_label.dart';
import 'package:glp_runtime/analysis/type_checker/type_nfa.dart';
import 'package:glp_runtime/analysis/type_checker/nfa_compiler.dart';
import 'package:glp_runtime/analysis/type_checker/nfa_to_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/type_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/type_parser.dart';

void main() {
  group('ModedLabel', () {
    test('equality includes mode', () {
      final l1 = ModedLabel('ch(2,1)', mode: Mode.input);
      final l2 = ModedLabel('ch(2,1)', mode: Mode.output);
      final l3 = ModedLabel('ch(2,1)', mode: Mode.input);
      final l4 = ModedLabel('ch(2,1)');

      expect(l1, equals(l3));
      expect(l1, isNot(equals(l2)));
      expect(l1, isNot(equals(l4)));
      expect(l2, isNot(equals(l4)));
    });

    test('factory constructors', () {
      final functor = ModedLabel.functor('foo', 2, 1, mode: Mode.input);
      expect(functor.pathElement, equals('foo(2,1)'));
      expect(functor.mode, equals(Mode.input));

      final head = ModedLabel.listHead(mode: Mode.output);
      expect(head.pathElement, equals('[|](2,1)'));
      expect(head.mode, equals(Mode.output));

      final nil = ModedLabel.nil();
      expect(nil.pathElement, equals('[]'));
      expect(nil.mode, isNull);
    });
  });

  group('NFA Construction', () {
    test('POSITIVE: Primitive type _ creates output-mode state', () {
      // Use predefined Every from prelude
      final env = parseTypes('');  // Empty, will use prelude

      final compiler = TypeNFACompiler(env);
      final nfa = compiler.compile('Every');

      // Start state should be primitive with both modes
      expect(nfa.isPrimitiveState(nfa.startState), isTrue);
      expect(nfa.getModesAt(nfa.startState), equals({Mode.output, Mode.input}));
    });

    test('POSITIVE: List creates correct transitions', () {
      // Use predefined List from prelude
      final env = parseTypes('');  // Empty, will use prelude

      final compiler = TypeNFACompiler(env);
      final nfa = compiler.compile('List');

      // Should have transitions for [] and [|]
      final labels = nfa.collectLabels({nfa.startState});
      final labelStrings = labels.map((l) => l.pathElement).toSet();

      expect(labelStrings, contains('[]'));
      expect(labelStrings, contains('[|](2,1)'));
      expect(labelStrings, contains('[|](2,2)'));
    });

    test('POSITIVE: Channel creates four distinct transitions', () {
      // Channel is predefined as: ch(Stream?, Stream)
      // But we need both alternatives - let's define our own test type
      final env = parseTypes('''
        MyStream ::< List.
        MyChannel ::= ch(MyStream?, MyStream) ; ch(MyStream, MyStream?).
      ''');

      final compiler = TypeNFACompiler(env);
      final nfa = compiler.compile('MyChannel');

      // Collect all transitions from start state
      final startClosure = nfa.epsilonClosure(nfa.startState);
      final labels = nfa.collectLabels(startClosure);

      // Should have 4 distinct labels (2 positions × 2 mode configurations)
      // ch(2,1)? and ch(2,1) - position 1 with input and output
      // ch(2,2)? and ch(2,2) - position 2 with input and output
      final pos1Input = labels.where((l) =>
          l.pathElement == 'ch(2,1)' && l.mode == Mode.input);
      final pos1Output = labels.where((l) =>
          l.pathElement == 'ch(2,1)' && l.mode == Mode.output);
      final pos2Input = labels.where((l) =>
          l.pathElement == 'ch(2,2)' && l.mode == Mode.input);
      final pos2Output = labels.where((l) =>
          l.pathElement == 'ch(2,2)' && l.mode == Mode.output);

      expect(pos1Input.length, equals(1), reason: 'Should have ch(2,1)?');
      expect(pos1Output.length, equals(1), reason: 'Should have ch(2,1)');
      expect(pos2Input.length, equals(1), reason: 'Should have ch(2,2)?');
      expect(pos2Output.length, equals(1), reason: 'Should have ch(2,2)');
    });

    test('POSITIVE: DiffList encodes mode in labels', () {
      // Use predefined DiffList from prelude
      final env = parseTypes('');  // Empty, will use prelude

      final compiler = TypeNFACompiler(env);
      final nfa = compiler.compile('DiffList');

      final startClosure = nfa.epsilonClosure(nfa.startState);
      final labels = nfa.collectLabels(startClosure);

      // Content position: \(2,1) with output mode (List without ?)
      final content = labels.where((l) =>
          l.pathElement == '\\(2,1)' && l.mode == Mode.output);
      expect(content.length, equals(1), reason: 'Content should be output mode');

      // Hole position: \(2,2) with input mode (List?)
      final hole = labels.where((l) =>
          l.pathElement == '\\(2,2)' && l.mode == Mode.input);
      expect(hole.length, equals(1), reason: 'Hole should be input mode');
    });

    test('POSITIVE: Subtype creates epsilon transition', () {
      // Stream is predefined as: Stream ::< List
      final env = parseTypes('');  // Empty, will use prelude

      final compiler = TypeNFACompiler(env);
      final nfa = compiler.compile('Stream');

      // Stream should have ε-transition to List
      final epsTargets = nfa.epsilonTransitions[nfa.startState] ?? {};
      final targetNames = epsTargets.map((s) => s.name).toSet();

      expect(targetNames, contains('List'));
    });
  });

  group('Subset Construction', () {
    test('POSITIVE: Every produces bi-moded DFA start state', () {
      // Use predefined Every from prelude
      final env = parseTypes('');  // Empty, will use prelude

      final compiler = TypeNFACompiler(env);
      final nfa = compiler.compile('Every');
      final converter = NFAToDFAConverter(nfa);
      final dfa = converter.convert();

      // DFA start state should be bi-moded
      expect(dfa.isPrimitiveState(dfa.startState), isTrue);
      expect(dfa.getModesAt(dfa.startState), equals({Mode.output, Mode.input}));
    });

    test('POSITIVE: List DFA accepts nil and cons paths', () {
      // Use predefined List from prelude
      final env = parseTypes('');  // Empty, will use prelude

      final compiler = TypeNFACompiler(env);
      final nfa = compiler.compile('List');
      final converter = NFAToDFAConverter(nfa);
      final dfa = converter.convert();

      // Should accept empty list path
      final nilPath = [ModedLabel.nil()];
      expect(dfa.acceptsModedPath(nilPath, Mode.output), isTrue);

      // Should accept cons paths (head then tail)
      final headTransition = dfa.transitions.keys.where((k) =>
          k.$1 == dfa.startState && k.$2.pathElement == '[|](2,1)');
      expect(headTransition.isNotEmpty, isTrue, reason: 'Should have head transition');
    });

    test('POSITIVE: Channel DFA preserves mode distinctions', () {
      // Define our own bi-directional channel type
      final env = parseTypes('''
        MyStream ::< List.
        MyChannel ::= ch(MyStream?, MyStream) ; ch(MyStream, MyStream?).
      ''');

      final compiler = TypeNFACompiler(env);
      final nfa = compiler.compile('MyChannel');
      final converter = NFAToDFAConverter(nfa);
      final dfa = converter.convert();

      // DFA should have transitions for all 4 mode configurations
      final transitionsFromStart = dfa.transitions.entries
          .where((e) => e.key.$1 == dfa.startState)
          .map((e) => e.key.$2)
          .toList();

      final pos1Labels = transitionsFromStart.where((l) => l.pathElement == 'ch(2,1)');
      final pos2Labels = transitionsFromStart.where((l) => l.pathElement == 'ch(2,2)');

      // Should have both input and output modes for each position
      expect(pos1Labels.map((l) => l.mode).toSet(),
          equals({Mode.input, Mode.output}));
      expect(pos2Labels.map((l) => l.mode).toSet(),
          equals({Mode.input, Mode.output}));
    });

    test('POSITIVE: DFA accepts matching moded path', () {
      // Define our own bi-directional channel type
      final env = parseTypes('''
        MyStream ::< List.
        MyChannel ::= ch(MyStream?, MyStream) ; ch(MyStream, MyStream?).
      ''');

      final compiler = TypeNFACompiler(env);
      final nfa = compiler.compile('MyChannel');
      final converter = NFAToDFAConverter(nfa);
      final dfa = converter.convert();

      // Path for first alternative: ch(MyStream?, MyStream)
      // Position 1 is input, position 2 is output
      final path1 = [ModedLabel.functor('ch', 2, 1, mode: Mode.input)];

      // Find if this path leads to a valid state
      final state1 = dfa.transitions[(dfa.startState, path1[0])];
      expect(state1, isNotNull, reason: 'Should have transition for ch(2,1)?');
    });
  });

  group('DFA Operations', () {
    test('POSITIVE: bi-moded type is superset of everything', () {
      // Use predefined types from prelude
      final env = parseTypes('');  // Empty, will use prelude

      final compiler = TypeNFACompiler(env);

      final everyNFA = compiler.compile('Every');
      final everyDFA = NFAToDFAConverter(everyNFA).convert();

      // Recompile for List
      final listCompiler = TypeNFACompiler(env);
      final listNFA = listCompiler.compile('List');
      final listDFA = NFAToDFAConverter(listNFA).convert();

      // List ⊆ Every should be true (Every accepts everything)
      expect(listDFA.isSubsetOf(everyDFA), isTrue);
    });

    test('POSITIVE: moded empty check', () {
      final emptyDFA = TypeDFA.empty();
      expect(emptyDFA.isModedEmpty, isTrue);

      // Use predefined Every from prelude
      final env = parseTypes('');  // Empty, will use prelude
      final compiler = TypeNFACompiler(env);
      final nfa = compiler.compile('Every');
      final dfa = NFAToDFAConverter(nfa).convert();
      expect(dfa.isModedEmpty, isFalse);
    });
  });

  group('Negative Controls', () {
    test('NEGATIVE: Undefined type reference fails', () {
      final env = parseTypes('''
        Foo ::= bar(Undefined).
      ''');

      final compiler = TypeNFACompiler(env);
      // Should not throw during compilation (Undefined is just a state name)
      // But the NFA will have a dangling reference
      final nfa = compiler.compile('Foo');
      expect(nfa.states.map((s) => s.name), contains('Undefined'));
    });
  });
}
