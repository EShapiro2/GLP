/// Tests for well-typed program checking
/// Specification: docs/modules/well-typed-program.md
/// Paper Reference: Definition 4.10

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/well_typed_program.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment.dart';

void main() {
  group('Well-Typed Program', () {
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

    group('checkProgram - Covariance', () {
      // Positive controls
      test('well-typed merge program passes covariance', () {
        final clauses = [
          // merge([], Ys, Ys?).
          Clause(
            Compound('merge', [
              Constant([]),
              Variable('Ys', isReader: false),
              Variable('Ys', isReader: true)
            ]),
            []
          ),
          // merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
          Clause(
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
          )
        ];

        final result = checkProgram(clauses, env);

        expect(result.isWellTyped, isTrue);
        expect(result.clauseResults.every((r) => r.isWellTyped), isTrue);
      });

      // Negative controls
      test('program with ill-typed clause fails covariance', () {
        // Reader in output position
        env.addProcedure(ProcDecl('bad', 1, [
          TypeRef('Stream', isComplement: false)
        ]));

        final clauses = [
          // bad([X?|Xs?]).  - readers in output position!
          Clause(
            Compound('bad', [
              Compound('.', [
                Variable('X', isReader: true),  // Reader at produce position
                Variable('Xs', isReader: true)
              ])
            ]),
            []
          )
        ];

        final result = checkProgram(clauses, env);

        expect(result.isWellTyped, isFalse);
        expect(result.errors.any((e) => e.contains('not well-typed')), isTrue);
      });
    });

    group('checkProgram - Contravariance', () {
      // Positive controls
      test('complete merge program passes contravariance', () {
        final clauses = [
          // merge([], Ys, Ys?).
          Clause(
            Compound('merge', [
              Constant([]),
              Variable('Ys', isReader: false),
              Variable('Ys', isReader: true)
            ]),
            []
          ),
          // merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
          Clause(
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
          ),
          // merge(Xs, [], Xs?).
          Clause(
            Compound('merge', [
              Variable('Xs', isReader: false),
              Constant([]),
              Variable('Xs', isReader: true)
            ]),
            []
          )
        ];

        final result = checkProgram(clauses, env);

        expect(result.isWellTyped, isTrue);
        expect(result.uncoveredPaths, isEmpty);
      });

      // Negative controls
      test('incomplete merge program fails contravariance', () {
        // Missing clause for []
        final clauses = [
          // merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
          Clause(
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
          )
        ];

        final result = checkProgram(clauses, env);

        expect(result.isWellTyped, isFalse);
        expect(result.uncoveredPaths, isNotEmpty);
        // Should report uncovered path to []
      });

      test('uncovered input path provides witness', () {
        // Missing clause for [] in argument 1
        final clauses = [
          Clause(
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
          )
        ];

        final result = checkProgram(clauses, env);

        expect(result.uncoveredPaths.length, greaterThan(0));
        // The witness should indicate [] is not covered
        final witness = result.uncoveredPaths.first;
        expect(witness.toString(), contains('[]'));
      });
    });

    group('checkProgram - combined', () {
      test('program with Nat type and pred procedure', () {
        // Nat ::= 0 ; s(_).
        env.addType(TypeDef(
          'Nat',
          [ConstantAlt(0), StructAlt('s', 1, [PrimitiveType('_')])]
        ));

        // procedure pred(Nat?, Nat).
        env.addProcedure(ProcDecl('pred', 2, [
          TypeRef('Nat', isComplement: true),
          TypeRef('Nat', isComplement: false)
        ]));

        // Complete coverage requires clause for 0
        final clauses = [
          // pred(s(N), N?).
          Clause(
            Compound('pred', [
              Compound('s', [Variable('N', isReader: false)]),
              Variable('N', isReader: true)
            ]),
            []
          )
        ];

        final result = checkProgram(clauses, env);

        // Should fail - missing clause for pred(0, ???)
        expect(result.isWellTyped, isFalse);
        expect(result.uncoveredPaths, isNotEmpty);
      });
    });
  });
}
