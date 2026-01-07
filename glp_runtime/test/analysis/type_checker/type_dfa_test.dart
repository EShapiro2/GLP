/// Tests for TypeDFA compilation and operations
/// Specification: docs/modules/type-dfa.md
/// Paper Reference: Section 4.1 (lines 7-35), lines 213-226

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';

void main() {
  group('TypeDFA', () {
    late TypeEnvironment env;

    setUp(() {
      env = TypeEnvironment();
    });

    group('compileType', () {
      // Positive controls
      test('compiles Stream type with nil and cons alternatives', () {
        // Stream ::= [] ; [_|Stream].
        env.addType(TypeDef(
          'Stream',
          [ListNilAlt(), ListConsAlt(PrimitiveType('_'), TypeRef('Stream'))]
        ));

        final dfa = compileType('Stream', env);

        expect(dfa.startState.name, equals('Stream'));
        expect(dfa.states, isNotEmpty);
        // Should have transitions for [] and [|]
        expect(dfa.transitions, isNotEmpty);
      });

      test('compiled Stream DFA has correct primitive state modes', () {
        env.addType(TypeDef(
          'Stream',
          [ListNilAlt(), ListConsAlt(PrimitiveType('_'), TypeRef('Stream'))]
        ));

        final dfa = compileType('Stream', env);

        // The _ primitive should have produce mode
        final primState = dfa.states.firstWhere(
          (s) => dfa.primitiveStateModes.containsKey(s),
          orElse: () => throw StateError('No primitive state found')
        );
        expect(
          dfa.primitiveStateModes[primState],
          contains(Mode.produce)
        );
      });

      test('compiles HollowStream with consume mode for _?', () {
        // HollowStream ::= [] ; [_?|HollowStream].
        env.addType(TypeDef(
          'HollowStream',
          [ListNilAlt(), ListConsAlt(PrimitiveType('_?'), TypeRef('HollowStream'))]
        ));

        final dfa = compileType('HollowStream', env);

        // The _? primitive should have consume mode
        final primState = dfa.states.firstWhere(
          (s) => dfa.primitiveStateModes.containsKey(s),
          orElse: () => throw StateError('No primitive state found')
        );
        expect(
          dfa.primitiveStateModes[primState],
          contains(Mode.consume)
        );
      });

      // Negative controls
      test('throws NonDeterministicTypeError for Any ::= _ ; _?', () {
        env.addType(TypeDef(
          'Any',
          [PrimitiveModeAlt(isInput: false), PrimitiveModeAlt(isInput: true)]
        ));

        expect(
          () => compileType('Any', env),
          throwsA(isA<NonDeterministicTypeError>())
        );
      });

      test('throws NonDeterministicTypeError for AnyOne ::= 1 ; 1?', () {
        // This would be 1 with mode produce vs 1 with mode consume
        // Same leading symbol (1), different modes - illegal
        env.addType(TypeDef(
          'AnyOne',
          [ConstantAlt(1, isInput: false), ConstantAlt(1, isInput: true)]
        ));

        expect(
          () => compileType('AnyOne', env),
          throwsA(isA<NonDeterministicTypeError>())
        );
      });

      test('throws UndefinedTypeError for reference to undefined type', () {
        env.addType(TypeDef(
          'BadStream',
          [ListConsAlt(PrimitiveType('_'), TypeRef('UndefinedType'))]
        ));

        expect(
          () => compileType('BadStream', env),
          throwsA(isA<UndefinedTypeError>())
        );
      });
    });

    group('applyModeComplement', () {
      // Positive controls
      test('flips all transition modes', () {
        env.addType(TypeDef(
          'Stream',
          [ListNilAlt(), ListConsAlt(PrimitiveType('_'), TypeRef('Stream'))]
        ));

        final original = compileType('Stream', env);
        final complemented = applyModeComplement(original);

        // All produce modes should become consume
        for (final entry in complemented.transitions.entries) {
          final label = entry.key.$2;
          if (label.mode != null) {
            // Find corresponding original transition
            final originalLabel = original.transitions.keys
              .firstWhere((k) => k.$2.symbol == label.symbol)
              .$2;
            expect(label.mode, equals(originalLabel.mode?.flip));
          }
        }
      });

      test('flips primitive state modes', () {
        env.addType(TypeDef(
          'Stream',
          [ListNilAlt(), ListConsAlt(PrimitiveType('_'), TypeRef('Stream'))]
        ));

        final original = compileType('Stream', env);
        final complemented = applyModeComplement(original);

        // Primitive modes should be flipped
        for (final entry in complemented.primitiveStateModes.entries) {
          final state = entry.key;
          final modes = entry.value;

          if (original.primitiveStateModes.containsKey(state)) {
            final originalModes = original.primitiveStateModes[state]!;
            for (final mode in modes) {
              expect(originalModes, contains(mode.flip));
            }
          }
        }
      });

      test('complement is involution', () {
        env.addType(TypeDef(
          'Stream',
          [ListNilAlt(), ListConsAlt(PrimitiveType('_'), TypeRef('Stream'))]
        ));

        final original = compileType('Stream', env);
        final doubleComplemented = applyModeComplement(applyModeComplement(original));

        // Should be equivalent to original
        expect(doubleComplemented.transitions.length, equals(original.transitions.length));
      });
    });

    group('DFA operations', () {
      late TypeDFA streamDfa;

      setUp(() {
        env.addType(TypeDef(
          'Stream',
          [ListNilAlt(), ListConsAlt(PrimitiveType('_'), TypeRef('Stream'))]
        ));
        streamDfa = compileType('Stream', env);
      });

      test('isEmpty returns false for non-empty DFA', () {
        expect(isEmpty(streamDfa), isFalse);
      });

      test('isSubsetOf returns true for same DFA', () {
        expect(isSubsetOf(streamDfa, streamDfa), isTrue);
      });

      test('union of DFA with itself equals itself', () {
        final unioned = union(streamDfa, streamDfa);
        expect(isSubsetOf(streamDfa, unioned), isTrue);
        expect(isSubsetOf(unioned, streamDfa), isTrue);
      });

      test('intersection of DFA with itself equals itself', () {
        final intersected = intersect(streamDfa, streamDfa);
        expect(isSubsetOf(streamDfa, intersected), isTrue);
        expect(isSubsetOf(intersected, streamDfa), isTrue);
      });

      test('intersection with complement is empty', () {
        final complemented = complement(streamDfa);
        final intersected = intersect(streamDfa, complemented);
        expect(isEmpty(intersected), isTrue);
      });

      test('findAcceptingPath returns path for non-empty DFA', () {
        final path = findAcceptingPath(streamDfa);
        expect(path, isNotNull);
      });

      test('findAcceptingPath returns null for empty DFA', () {
        final emptyDfa = emptyDFA();
        final path = findAcceptingPath(emptyDfa);
        expect(path, isNull);
      });
    });

    group('accepts', () {
      test('Stream DFA accepts path to nil', () {
        env.addType(TypeDef(
          'Stream',
          [ListNilAlt(), ListConsAlt(PrimitiveType('_'), TypeRef('Stream'))]
        ));
        final dfa = compileType('Stream', env);

        // Path: Stream --[]-> final
        final path = TypePath([
          PathStep(0, Mode.produce, 'Stream'),
          PathStep(1, Mode.produce, '[]')
        ]);

        expect(accepts(dfa, path), isTrue);
      });

      test('Stream DFA accepts path to element', () {
        env.addType(TypeDef(
          'Stream',
          [ListNilAlt(), ListConsAlt(PrimitiveType('_'), TypeRef('Stream'))]
        ));
        final dfa = compileType('Stream', env);

        // Path: Stream --[|](2,1)-> _
        final path = TypePath([
          PathStep(0, Mode.produce, 'Stream'),
          PathStep(1, Mode.produce, '[|](2,1)'),
          PathStep(1, Mode.produce, '_')
        ]);

        expect(accepts(dfa, path), isTrue);
      });
    });
  });
}
