/// Tests for well-typed clause checking
/// Specification: docs/modules/well-typed-clause.md
/// Paper Reference: Definition 4.8

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/well_typed_clause.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment.dart';

void main() {
  group('Well-Typed Clause', () {
    late TypeEnvironment env;

    setUp(() {
      env = TypeEnvironment();

      env.addType(TypeDef(
        'Stream',
        [ListNilAlt(), ListConsAlt(PrimitiveType('_'), TypeRef('Stream'))]
      ));

      env.addProcedure(ProcDecl('merge', 3, [
        TypeRef('Stream', isComplement: true),
        TypeRef('Stream', isComplement: true),
        TypeRef('Stream', isComplement: false)
      ]));
    });

    group('checkClause', () {
      // Positive controls
      test('merge clause 1 is well-typed', () {
        // merge([], Ys, Ys?).
        final clause = Clause(
          Compound('merge', [
            Constant([]),
            Variable('Ys', isReader: false),
            Variable('Ys', isReader: true)
          ]),
          []  // No body
        );

        final result = checkClause(clause, env);

        expect(result.isWellTyped, isTrue);
        expect(result.errors, isEmpty);
      });

      test('merge clause 2 is well-typed', () {
        // merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
        final clause = Clause(
          Compound('merge', [
            Compound('.', [Variable('X', isReader: false), Variable('Xs', isReader: false)]),
            Variable('Ys', isReader: false),
            Compound('.', [Variable('X', isReader: true), Variable('Zs', isReader: true)])
          ]),
          [
            Compound('merge', [
              Variable('Ys', isReader: true),
              Variable('Xs', isReader: true),
              Variable('Zs', isReader: false)
            ])
          ]
        );

        final result = checkClause(clause, env);

        expect(result.isWellTyped, isTrue);
        expect(result.errors, isEmpty);
      });

      test('returns complementary variable types', () {
        final clause = Clause(
          Compound('merge', [
            Compound('.', [Variable('X', isReader: false), Variable('Xs', isReader: false)]),
            Variable('Ys', isReader: false),
            Compound('.', [Variable('X', isReader: true), Variable('Zs', isReader: true)])
          ]),
          [
            Compound('merge', [
              Variable('Ys', isReader: true),
              Variable('Xs', isReader: true),
              Variable('Zs', isReader: false)
            ])
          ]
        );

        final result = checkClause(clause, env);

        // X/X? should have complementary types
        expect(result.variableTypes['X'], isNotNull);
        expect(result.variableTypes['X?'], isNotNull);
      });

      // Negative controls
      test('clause with non-complementary variable types is not well-typed', () {
        // bad(X, Y, [X?|Y?]) where X and Y get different types
        env.addProcedure(ProcDecl('bad', 3, [
          TypeRef('Integer', isComplement: true),
          TypeRef('String', isComplement: true),
          TypeRef('Stream', isComplement: false)
        ]));
        env.addType(TypeDef('Integer', [ConstantAlt(0)]));
        env.addType(TypeDef('String', [ConstantAlt('')]));

        final clause = Clause(
          Compound('bad', [
            Variable('X', isReader: false),
            Variable('Y', isReader: false),
            Compound('.', [Variable('X', isReader: true), Variable('Y', isReader: true)])
          ]),
          []
        );

        final result = checkClause(clause, env);

        // This should fail because X in position 1 gets Integer
        // but X? in position 3.head gets _? (from Stream element)
        // These are not complementary
        expect(result.isWellTyped, isFalse);
      });

      test('clause with reader at output position in body is not well-typed', () {
        // foo(X) :- bar(X?, X?).  where bar(S?, S) - second arg is output
        env.addProcedure(ProcDecl('foo', 1, [
          TypeRef('Stream', isComplement: true)
        ]));
        env.addProcedure(ProcDecl('bar', 2, [
          TypeRef('Stream', isComplement: true),   // input
          TypeRef('Stream', isComplement: false)   // output
        ]));

        final clause = Clause(
          Compound('foo', [Variable('X', isReader: false)]),
          [
            Compound('bar', [
              Variable('X', isReader: true),
              Variable('X', isReader: true)  // Reader at output position!
            ])
          ]
        );

        final result = checkClause(clause, env);

        expect(result.isWellTyped, isFalse);
        expect(result.errors.any((e) => e is BodyAtomNotWellTypedError), isTrue);
      });

      test('throws UndeclaredProcedureError for undeclared procedure', () {
        final clause = Clause(
          Compound('undeclared', [Variable('X', isReader: false)]),
          []
        );

        expect(
          () => checkClause(clause, env),
          throwsA(isA<UndeclaredProcedureError>())
        );
      });
    });

    group('accepts', () {
      // Positive controls
      test('merge clause 1 accepts path to nil', () {
        // merge([], Ys, Ys?).
        final clause = Clause(
          Compound('merge', [
            Constant([]),
            Variable('Ys', isReader: false),
            Variable('Ys', isReader: true)
          ]),
          []
        );

        // Input path to [] at argument 1
        final path = TypePath([
          PathStep(0, Mode.consume, 'merge/3'),
          PathStep(1, Mode.consume, 'Stream?'),
          PathStep(1, Mode.consume, '[]')
        ]);

        expect(accepts(clause, path, env), isTrue);
      });

      test('merge clause 2 accepts path to cons head', () {
        // merge([X|Xs], Ys, [X?|Zs?]) :- ...
        final clause = Clause(
          Compound('merge', [
            Compound('.', [Variable('X', isReader: false), Variable('Xs', isReader: false)]),
            Variable('Ys', isReader: false),
            Compound('.', [Variable('X', isReader: true), Variable('Zs', isReader: true)])
          ]),
          [
            Compound('merge', [
              Variable('Ys', isReader: true),
              Variable('Xs', isReader: true),
              Variable('Zs', isReader: false)
            ])
          ]
        );

        // Input path to element at argument 1
        final path = TypePath([
          PathStep(0, Mode.consume, 'merge/3'),
          PathStep(1, Mode.consume, 'Stream?'),
          PathStep(1, Mode.consume, '.'),
          PathStep(1, Mode.consume, '_?')
        ]);

        expect(accepts(clause, path, env), isTrue);
      });

      // Negative controls
      test('merge clause 2 does not accept path to nil', () {
        final clause = Clause(
          Compound('merge', [
            Compound('.', [Variable('X', isReader: false), Variable('Xs', isReader: false)]),
            Variable('Ys', isReader: false),
            Compound('.', [Variable('X', isReader: true), Variable('Zs', isReader: true)])
          ]),
          [
            Compound('merge', [
              Variable('Ys', isReader: true),
              Variable('Xs', isReader: true),
              Variable('Zs', isReader: false)
            ])
          ]
        );

        // Input path to [] at argument 1 - clause 2 has [X|Xs], not []
        final path = TypePath([
          PathStep(0, Mode.consume, 'merge/3'),
          PathStep(1, Mode.consume, 'Stream?'),
          PathStep(1, Mode.consume, '[]')
        ]);

        expect(accepts(clause, path, env), isFalse);
      });
    });
  });
}
