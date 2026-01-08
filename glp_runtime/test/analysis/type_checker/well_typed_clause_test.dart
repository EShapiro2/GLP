// test/analysis/type_checker/well_typed_clause_test.dart
//
// Tests for well_typed_clause.dart
// Specification: docs/modules/well-typed-clause.md v0.1
// Paper Reference: Definition 4.8
//
// Note: Types like `BiMode ::= _ ; _?` are ILLEGAL because they create
// non-deterministic transitions (NFA instead of DFA). Each position
// has exactly ONE mode.

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/type_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/type_compiler.dart';
import 'package:glp_runtime/analysis/type_checker/well_typed_clause.dart';
import 'package:glp_runtime/compiler/ast.dart' as ast;

void main() {
  group('WellTypedClause', () {
    // =========================================================================
    // Helper: Create Type Environment
    // =========================================================================

    /// Create a simple type environment with basic types
    TypeEnvironment createBasicEnvironment() {
      final env = TypeEnvironment.empty();

      // Define Output type: Output ::= _ (produce mode only)
      env.addType(TypeDef(
        'Output',
        [PrimitiveModeAlt(false, 0, 0)], // _
        0,
        0,
      ));

      // Define Input type: Input ::= _? (consume mode only)
      env.addType(TypeDef(
        'Input',
        [PrimitiveModeAlt(true, 0, 0)], // _?
        0,
        0,
      ));

      // Define Stream type: Stream ::= [] ; [_|Stream]
      env.addType(TypeDef(
        'Stream',
        [
          ListNilAlt(0, 0),
          ListConsAlt(
            PrimitiveModeAlt(false, 0, 0), // _ for head
            TypeRef('Stream', 0, 0),
            0,
            0,
          ),
        ],
        0,
        0,
      ));

      // Define HollowStream type: HollowStream ::= [] ; [_?|HollowStream]
      env.addType(TypeDef(
        'HollowStream',
        [
          ListNilAlt(0, 0),
          ListConsAlt(
            PrimitiveModeAlt(true, 0, 0), // _? for head
            TypeRef('HollowStream', 0, 0),
            0,
            0,
          ),
        ],
        0,
        0,
      ));

      // Define Nat type: Nat ::= 0 ; s(Nat)
      env.addType(TypeDef(
        'Nat',
        [
          ConstantAlt(0, 0, 0),
          StructAlt('s', [TypeRef('Nat', 0, 0)], 0, 0),
        ],
        0,
        0,
      ));

      // Define Pair type: Pair ::= pair(_, _?)
      // First arg is output, second is input
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

    /// Create AST variable term
    ast.VarTerm varTerm(String name, {bool isReader = false}) {
      return ast.VarTerm(name, isReader, 0, 0);
    }

    /// Create AST constant term
    ast.ConstTerm constTerm(Object value) {
      return ast.ConstTerm(value, 0, 0);
    }

    /// Create AST struct term
    ast.StructTerm structTerm(String functor, List<ast.Term> args) {
      return ast.StructTerm(functor, args, 0, 0);
    }

    /// Create AST list (nil)
    ast.ListTerm nilTerm() {
      return ast.ListTerm(null, null, 0, 0);
    }

    /// Create AST list (cons)
    ast.ListTerm consTerm(ast.Term head, ast.Term tail) {
      return ast.ListTerm(head, tail, 0, 0);
    }

    /// Create AST goal
    ast.Goal goal(String functor, List<ast.Term> args) {
      return ast.Goal(functor, args, 0, 0);
    }

    // =========================================================================
    // Basic Well-Typed Clause Tests
    // =========================================================================

    group('Basic Well-Typed Clauses', () {
      test('simple fact with output variable is well-typed', () {
        // procedure foo(Output).
        // foo(X).
        final env = createBasicEnvironment();
        env.addProcedure(ProcDecl(
          'foo',
          [TypeRef('Output', 0, 0)],
          0,
          0,
        ));

        final compiler = TypeCompiler(env);
        final clause = TypedClause(
          head: goal('foo', [varTerm('X')]),
        );

        final result = checkClause(clause, env, compiler);

        expect(result.isWellTyped, isTrue);
        expect(result.errors, isEmpty);
      });

      test('simple fact with input variable is well-typed', () {
        // procedure bar(Input?).
        // bar(X?).
        final env = createBasicEnvironment();
        env.addProcedure(ProcDecl(
          'bar',
          [TypeRef('Input', 0, 0, isInput: true)],
          0,
          0,
        ));

        final compiler = TypeCompiler(env);
        final clause = TypedClause(
          head: goal('bar', [varTerm('X', isReader: true)]),
        );

        final result = checkClause(clause, env, compiler);

        expect(result.isWellTyped, isTrue);
        expect(result.errors, isEmpty);
      });

      test('constant in output position is well-typed', () {
        // procedure nat(Nat).
        // nat(0).
        final env = createBasicEnvironment();
        env.addProcedure(ProcDecl(
          'nat',
          [TypeRef('Nat', 0, 0)],
          0,
          0,
        ));

        final compiler = TypeCompiler(env);
        final clause = TypedClause(
          head: goal('nat', [constTerm(0)]),
        );

        final result = checkClause(clause, env, compiler);

        expect(result.isWellTyped, isTrue);
      });
    });

    // =========================================================================
    // Mode Mismatch Tests
    // =========================================================================

    group('Mode Mismatches', () {
      test('reader in output-only position is NOT well-typed', () {
        // procedure foo(Output).  -- output position, only produces
        // foo(X?).  -- reader needs consume mode but Output only has produce
        final env = createBasicEnvironment();
        env.addProcedure(ProcDecl(
          'foo',
          [TypeRef('Output', 0, 0)],
          0,
          0,
        ));

        final compiler = TypeCompiler(env);
        final clause = TypedClause(
          head: goal('foo', [varTerm('X', isReader: true)]), // reader
        );

        final result = checkClause(clause, env, compiler);

        expect(result.isWellTyped, isFalse);
        expect(result.errors, hasLength(1));
        expect(result.errors.first, isA<HeadError>());
      });

      test('writer in input-only position is NOT well-typed', () {
        // procedure bar(Input?).  -- input position, only consumes
        // bar(X).  -- writer needs produce mode but Input only has consume
        final env = createBasicEnvironment();
        env.addProcedure(ProcDecl(
          'bar',
          [TypeRef('Input', 0, 0, isInput: true)],
          0,
          0,
        ));

        final compiler = TypeCompiler(env);
        final clause = TypedClause(
          head: goal('bar', [varTerm('X')]), // writer
        );

        final result = checkClause(clause, env, compiler);

        expect(result.isWellTyped, isFalse);
        expect(result.errors, hasLength(1));
        expect(result.errors.first, isA<HeadError>());
      });
    });

    // =========================================================================
    // Clauses with Body Atoms
    // =========================================================================

    group('Clauses with Body Atoms', () {
      test('clause with well-typed body atom', () {
        // procedure append(Stream?, Stream?, Stream).
        // append([], Ys?, Ys).
        // append([X|Xs], Ys?, [X|Zs]) :- append(Xs?, Ys?, Zs).
        final env = createBasicEnvironment();
        env.addProcedure(ProcDecl(
          'append',
          [
            TypeRef('Stream', 0, 0, isInput: true),  // input
            TypeRef('Stream', 0, 0, isInput: true),  // input
            TypeRef('Stream', 0, 0),                  // output
          ],
          0,
          0,
        ));

        final compiler = TypeCompiler(env);

        // Test first clause: append([], Ys?, Ys).
        final clause1 = TypedClause(
          head: goal('append', [
            nilTerm(),
            varTerm('Ys', isReader: true),
            varTerm('Ys'),
          ]),
        );

        final result1 = checkClause(clause1, env, compiler);
        expect(result1.isWellTyped, isTrue);

        // Test second clause with body atom
        final clause2 = TypedClause(
          head: goal('append', [
            consTerm(varTerm('X'), varTerm('Xs')),
            varTerm('Ys', isReader: true),
            consTerm(varTerm('X', isReader: true), varTerm('Zs')),
          ]),
          bodyAtoms: [
            goal('append', [
              varTerm('Xs', isReader: true),
              varTerm('Ys', isReader: true),
              varTerm('Zs'),
            ]),
          ],
        );

        final result2 = checkClause(clause2, env, compiler);
        expect(result2.isWellTyped, isTrue);
      });

      test('undefined procedure in body is caught', () {
        final env = createBasicEnvironment();
        env.addProcedure(ProcDecl(
          'foo',
          [TypeRef('Output', 0, 0)],
          0,
          0,
        ));

        final compiler = TypeCompiler(env);
        final clause = TypedClause(
          head: goal('foo', [varTerm('X')]),
          bodyAtoms: [
            goal('undefined', [varTerm('X', isReader: true)]),
          ],
        );

        final result = checkClause(clause, env, compiler);

        expect(result.isWellTyped, isFalse);
        expect(result.errors.any((e) => e is BodyAtomError), isTrue);
      });
    });

    // =========================================================================
    // Variable Complementarity Tests
    // =========================================================================

    group('Variable Complementarity', () {
      test('X and X? at complementary positions in Pair are well-typed', () {
        // Pair ::= pair(_, _?)
        // procedure test(Pair).
        // test(pair(X, X?)).  -- X writer at _ position, X? reader at _? position
        final env = createBasicEnvironment();
        env.addProcedure(ProcDecl(
          'test',
          [TypeRef('Pair', 0, 0)],
          0,
          0,
        ));

        final compiler = TypeCompiler(env);
        final clause = TypedClause(
          head: goal('test', [
            structTerm('pair', [
              varTerm('X'),
              varTerm('X', isReader: true),
            ]),
          ]),
        );

        final result = checkClause(clause, env, compiler);

        expect(result.isWellTyped, isTrue);
        expect(result.variableTypes, contains('X'));
        expect(result.variableTypes, contains('X?'));
      });

      test('X and X? at different types are NOT complementary', () {
        // procedure mismatch(Nat, Stream?).
        // mismatch(X, X?).  -- X at Nat, X? at Stream - NOT complementary
        final env = createBasicEnvironment();
        env.addProcedure(ProcDecl(
          'mismatch',
          [
            TypeRef('Nat', 0, 0),               // output Nat
            TypeRef('Stream', 0, 0, isInput: true), // input Stream
          ],
          0,
          0,
        ));

        final compiler = TypeCompiler(env);
        final clause = TypedClause(
          head: goal('mismatch', [
            varTerm('X'),
            varTerm('X', isReader: true),
          ]),
        );

        final result = checkClause(clause, env, compiler);

        expect(result.isWellTyped, isFalse);
        expect(result.errors.any((e) => e is ClauseComplementaryError), isTrue);
      });
    });

    // =========================================================================
    // Error Handling Tests
    // =========================================================================

    group('Error Handling', () {
      test('undefined procedure returns error', () {
        final env = createBasicEnvironment();
        final compiler = TypeCompiler(env);

        final clause = TypedClause(
          head: goal('undefined', [varTerm('X')]),
        );

        final result = checkClause(clause, env, compiler);

        expect(result.isWellTyped, isFalse);
        expect(result.errors.first, isA<UndefinedProcedureError>());
      });

      test('wrong arity returns undefined procedure error', () {
        // Note: Procedures are looked up by name+arity, so calling foo/1
        // when only foo/2 is defined results in UndefinedProcedureError
        final env = createBasicEnvironment();
        env.addProcedure(ProcDecl(
          'foo',
          [TypeRef('Output', 0, 0), TypeRef('Output', 0, 0)], // foo/2
          0,
          0,
        ));

        final compiler = TypeCompiler(env);
        final clause = TypedClause(
          head: goal('foo', [varTerm('X')]), // foo/1 - not defined
        );

        final result = checkClause(clause, env, compiler);

        expect(result.isWellTyped, isFalse);
        expect(result.errors.first, isA<UndefinedProcedureError>());
        expect((result.errors.first as UndefinedProcedureError).procedureName, equals('foo'));
        expect((result.errors.first as UndefinedProcedureError).arity, equals(1));
      });
    });

    // =========================================================================
    // ClauseCheckResult Factory Tests
    // =========================================================================

    group('ClauseCheckResult Factories', () {
      test('success factory creates well-typed result', () {
        final result = ClauseCheckResult.success({});

        expect(result.isWellTyped, isTrue);
        expect(result.errors, isEmpty);
      });

      test('failure factory creates ill-typed result', () {
        final error = UndefinedProcedureError('foo', 1);
        final result = ClauseCheckResult.failure([error]);

        expect(result.isWellTyped, isFalse);
        expect(result.errors, hasLength(1));
      });
    });

    // =========================================================================
    // TypeDFA.applyModeComplement Tests
    // =========================================================================

    group('TypeDFA.applyModeComplement', () {
      test('flips consume to produce', () {
        final state = DFAState('T');
        final dfa = TypeDFA(
          states: {state},
          startState: state,
          finalStates: {},
          transitions: {},
          primitiveStateModes: {state: {Mode.consume}},
        );

        final complemented = dfa.applyModeComplement();

        expect(complemented.getModesAt(state), equals({Mode.produce}));
      });

      test('flips produce to consume', () {
        final state = DFAState('T');
        final dfa = TypeDFA(
          states: {state},
          startState: state,
          finalStates: {},
          transitions: {},
          primitiveStateModes: {state: {Mode.produce}},
        );

        final complemented = dfa.applyModeComplement();

        expect(complemented.getModesAt(state), equals({Mode.consume}));
      });
    });
  });
}
