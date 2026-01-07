/// Tests for well-typed moded term checking
/// Specification: docs/modules/well-typed-term.md
/// Paper Reference: Definition 4.5

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/well_typed_term.dart';
import 'package:glp_runtime/analysis/type_checker/moded_term.dart';
import 'package:glp_runtime/analysis/type_checker/type_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';

void main() {
  group('Well-Typed Term', () {
    late TypeEnvironment env;
    late TypeDFA streamDfa;
    late TypeDFA mergeDfa;

    setUp(() {
      env = TypeEnvironment();

      // Stream ::= [] ; [_|Stream].
      env.addType(TypeDef(
        'Stream',
        [ListNilAlt(), ListConsAlt(PrimitiveType('_'), TypeRef('Stream'))]
      ));

      // procedure merge(Stream?, Stream?, Stream)
      env.addProcedure(ProcDecl('merge', 3, [
        TypeRef('Stream', isComplement: true),
        TypeRef('Stream', isComplement: true),
        TypeRef('Stream', isComplement: false)
      ]));

      streamDfa = compileType('Stream', env);
      mergeDfa = compileProcedureType('merge', 3, env);
    });

    group('checkWellTyped', () {
      // Positive controls
      test('merge moded head is well-typed', () {
        // H' = ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
        final modedHead = ModedCompound(
          Mode.consume,
          'merge',
          3,
          [
            ModedCompound(Mode.consume, '.', 2, [
              ModedVariable('X', isReader: true),
              ModedVariable('Xs', isReader: true)
            ]),
            ModedVariable('Ys', isReader: true),
            ModedCompound(Mode.produce, '.', 2, [
              ModedVariable('X', isReader: false),
              ModedVariable('Zs', isReader: false)
            ])
          ]
        );

        final result = checkWellTyped(modedHead, mergeDfa);

        expect(result.isWellTyped, isTrue);
        expect(result.errors, isEmpty);
      });

      test('variable types are complementary', () {
        // Same test as above but check variable assignments
        final modedHead = ModedCompound(
          Mode.consume,
          'merge',
          3,
          [
            ModedCompound(Mode.consume, '.', 2, [
              ModedVariable('X', isReader: true),
              ModedVariable('Xs', isReader: true)
            ]),
            ModedVariable('Ys', isReader: true),
            ModedCompound(Mode.produce, '.', 2, [
              ModedVariable('X', isReader: false),
              ModedVariable('Zs', isReader: false)
            ])
          ]
        );

        final result = checkWellTyped(modedHead, mergeDfa);

        // X? : _? and X : _ should be complements
        expect(result.variableTypes['X?']?.typeName, equals('_?'));
        expect(result.variableTypes['X']?.typeName, equals('_'));
      });

      // Negative controls
      test('writer at consumed position is not well-typed', () {
        // ↓foo(↓X) where X is writer at consumed position
        env.addProcedure(ProcDecl('foo', 1, [
          TypeRef('Stream', isComplement: true)  // Input = consumed
        ]));
        final fooDfa = compileProcedureType('foo', 1, env);

        final term = ModedCompound(
          Mode.consume,
          'foo',
          1,
          [ModedVariable('X', isReader: false)]  // Writer at consume position!
        );

        final result = checkWellTyped(term, fooDfa);

        expect(result.isWellTyped, isFalse);
        expect(result.errors, isNotEmpty);
        expect(result.errors.first, isA<InconsistentPathError>());
      });

      test('reader at produced position is not well-typed', () {
        env.addProcedure(ProcDecl('bar', 1, [
          TypeRef('Stream', isComplement: false)  // Output = produced
        ]));
        final barDfa = compileProcedureType('bar', 1, env);

        final term = ModedCompound(
          Mode.consume,
          'bar',
          1,
          [
            ModedCompound(Mode.produce, '.', 2, [
              ModedVariable('X', isReader: true),  // Reader at produce position!
              ModedVariable('Xs', isReader: false)
            ])
          ]
        );

        final result = checkWellTyped(term, barDfa);

        expect(result.isWellTyped, isFalse);
        expect(result.errors.any((e) => e is InconsistentPathError), isTrue);
      });

      test('non-complementary variable types is not well-typed', () {
        // ↓bar(↓X?, ↑X) where X? gets type T and X gets type S (T ≠ S)
        env.addType(TypeDef('T', [ConstantAlt(1)]));
        env.addType(TypeDef('S', [ConstantAlt(2)]));
        env.addProcedure(ProcDecl('bar', 2, [
          TypeRef('T', isComplement: true),   // X? gets T
          TypeRef('S', isComplement: false)   // X gets S
        ]));
        final barDfa = compileProcedureType('bar', 2, env);

        final term = ModedCompound(
          Mode.consume,
          'bar',
          2,
          [
            ModedVariable('X', isReader: true),
            ModedVariable('X', isReader: false)
          ]
        );

        final result = checkWellTyped(term, barDfa);

        expect(result.isWellTyped, isFalse);
        expect(
          result.errors.any((e) => e is NonComplementaryVariablesError),
          isTrue
        );
      });
    });

    group('isWellTyped', () {
      test('returns true for well-typed term', () {
        final term = ModedCompound(
          Mode.consume,
          'merge',
          3,
          [
            ModedVariable('Xs', isReader: true),
            ModedVariable('Ys', isReader: true),
            ModedVariable('Zs', isReader: false)
          ]
        );

        expect(isWellTyped(term, mergeDfa), isTrue);
      });
    });
  });
}
