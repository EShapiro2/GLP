// test/analysis/type_checker/well_typed_clause_test.dart
//
// Tests for well_typed_clause.dart
// Specification: docs/modules/well-typed-clause.md v0.1
// Paper Reference: Definition 4.8

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

      // Define Any type: Any ::= _ ; _?
      env.addType(TypeDef(
        'Any',
        [
          PrimitiveModeAlt(false, 0, 0), // _
          PrimitiveModeAlt(true, 0, 0),  // _?
        ],
        0,
        0,
      ));

      // Define Stream type: Stream ::= [] ; [Any|Stream]
      env.addType(TypeDef(
        'Stream',
        [
          ListNilAlt(0, 0),
          ListConsAlt(
            TypeRef('Any', 0, 0),
            TypeRef('Stream', 0, 0),
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
        // procedure foo(Any).
        // foo(X).
        final env = createBasicEnvironment();
        env.addProcedure(ProcDecl(
          'foo',
          [TypeRef('Any', 0, 0)],
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
        // procedure bar(Any?).
        // bar(X?).
        final env = createBasicEnvironment();
        env.addProcedure(ProcDecl(
          'bar',
          [TypeRef('Any', 0, 0, isInput: true)],
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
      test('writer in output-only position is NOT well-typed', () {
        // procedure foo(OutOnly).  -- output position, only produces
        // foo(X).  -- writer in source → reader in moded head → needs consume → FAILS
        //
        // Per Definition 4.6: modedHead flips variables
        // Source writer X → moded reader X? → needs consume mode
        // But OutOnly only has produce mode → MISMATCH
        final env = createBasicEnvironment();

        // Create type with only output mode (produce)
        env.addType(TypeDef(
          'OutOnly',
          [PrimitiveModeAlt(false, 0, 0)], // only _ (produce)
          0,
          0,
        ));

        env.addProcedure(ProcDecl(
          'foo',
          [TypeRef('OutOnly', 0, 0)],
          0,
          0,
        ));

        final compiler = TypeCompiler(env);
        final clause = TypedClause(
          head: goal('foo', [varTerm('X')]), // writer in source
        );

        final result = checkClause(clause, env, compiler);

        expect(result.isWellTyped, isFalse);
        expect(result.errors, hasLength(1));
        expect(result.errors.first, isA<HeadError>());
      });

      test('reader in input-only position is NOT well-typed', () {
        // procedure bar(InOnly?).  -- input position, only consumes
        // bar(X?).  -- reader in source → writer in moded head → needs produce → FAILS
        //
        // Per Definition 4.6: modedHead flips variables
        // Source reader X? → moded writer X → needs produce mode
        // But InOnly only has consume mode → MISMATCH
        final env = createBasicEnvironment();

        // Create type with only input mode (consume)
        env.addType(TypeDef(
          'InOnly',
          [PrimitiveModeAlt(true, 0, 0)], // only _? (consume)
          0,
          0,
        ));

        env.addProcedure(ProcDecl(
          'bar',
          [TypeRef('InOnly', 0, 0, isInput: true)],
          0,
          0,
        ));

        final compiler = TypeCompiler(env);
        final clause = TypedClause(
          head: goal('bar', [varTerm('X', isReader: true)]), // reader in source
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
          [TypeRef('Any', 0, 0)],
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
      test('X and X? at same type are complementary', () {
        // procedure pair(Any, Any?).
        // pair(X, X?).  -- X writer, X? reader at same Any type
        final env = createBasicEnvironment();
        env.addProcedure(ProcDecl(
          'pair',
          [
            TypeRef('Any', 0, 0),            // output
            TypeRef('Any', 0, 0, isInput: true), // input
          ],
          0,
          0,
        ));

        final compiler = TypeCompiler(env);
        final clause = TypedClause(
          head: goal('pair', [
            varTerm('X'),
            varTerm('X', isReader: true),
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
        // (not ArityMismatchClauseError)
        final env = createBasicEnvironment();
        env.addProcedure(ProcDecl(
          'foo',
          [TypeRef('Any', 0, 0), TypeRef('Any', 0, 0)], // foo/2
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

      test('flips both modes in bi-moded state', () {
        final state = DFAState('Any');
        final dfa = TypeDFA(
          states: {state},
          startState: state,
          finalStates: {},
          transitions: {},
          primitiveStateModes: {state: {Mode.consume, Mode.produce}},
        );

        final complemented = dfa.applyModeComplement();

        // Both flipped means same set (complement of {↓,↑} = {↑,↓})
        expect(complemented.getModesAt(state), equals({Mode.consume, Mode.produce}));
      });
    });
  });
}
