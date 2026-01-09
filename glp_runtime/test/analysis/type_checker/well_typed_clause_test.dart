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

      // Define Any type: Any ::= _ (produce mode)
      // Used at output position: Any → no complement → produce mode
      // Used at input position: Any? → complement → consume mode
      env.addType(TypeDef(
        'Any',
        [PrimitiveModeAlt(false, 0, 0)], // _
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
      // Per paper lines 282-283:
      // - At INPUT positions (T?): head should have WRITER (receives value from goal)
      // - At OUTPUT positions (T): head should have READER (provides value to goal)
      //
      // After variable flip in moded head:
      // - Writer X → Reader X? (consume mode)
      // - Reader X? → Writer X (produce mode)
      //
      // Type Any ::= _ has produce mode.
      // - At output position (Any): no complement → produce mode
      // - At input position (Any?): complement → consume mode

      test('POSITIVE: reader at output position is well-typed', () {
        // procedure foo(Any).  -- OUTPUT position, type has produce mode
        // foo(X?).  -- reader provides value to goal
        // After flip: X (produce) matches Any's produce mode (no complement)
        final env = createBasicEnvironment();
        env.addProcedure(ProcDecl(
          'foo',
          [TypeRef('Any', 0, 0)],  // OUTPUT position
          0,
          0,
        ));

        final compiler = TypeCompiler(env);
        final clause = TypedClause(
          head: goal('foo', [varTerm('X', isReader: true)]),
        );

        final result = checkClause(clause, env, compiler);

        expect(result.isWellTyped, isTrue);
        expect(result.errors, isEmpty);
      });

      test('POSITIVE: writer at input position is well-typed', () {
        // procedure bar(Any?).  -- INPUT position, type has produce mode
        // bar(X).  -- writer receives value from goal
        // After flip: X? (consume) matches Any's produce mode after complement (consume)
        final env = createBasicEnvironment();
        env.addProcedure(ProcDecl(
          'bar',
          [TypeRef('Any', 0, 0, isInput: true)],  // INPUT position
          0,
          0,
        ));

        final compiler = TypeCompiler(env);
        final clause = TypedClause(
          head: goal('bar', [varTerm('X')]),
        );

        final result = checkClause(clause, env, compiler);

        expect(result.isWellTyped, isTrue);
        expect(result.errors, isEmpty);
      });

      test('NEGATIVE: writer at output position is NOT well-typed', () {
        // procedure foo(Any).  -- OUTPUT position, type has produce mode
        // foo(X).  -- writer is WRONG for output position
        // After flip: X? (consume) does NOT match Any's produce mode
        final env = createBasicEnvironment();
        env.addProcedure(ProcDecl(
          'foo',
          [TypeRef('Any', 0, 0)],  // OUTPUT position
          0,
          0,
        ));

        final compiler = TypeCompiler(env);
        final clause = TypedClause(
          head: goal('foo', [varTerm('X')]),
        );

        final result = checkClause(clause, env, compiler);

        expect(result.isWellTyped, isFalse);
        expect(result.errors, hasLength(1));
        expect(result.errors.first, isA<HeadError>());
      });

      test('NEGATIVE: reader at input position is NOT well-typed', () {
        // procedure bar(Any?).  -- INPUT position, type has produce mode
        // bar(X?).  -- reader is WRONG for input position
        // After flip: X (produce) does NOT match Any's produce mode after complement (consume)
        final env = createBasicEnvironment();
        env.addProcedure(ProcDecl(
          'bar',
          [TypeRef('Any', 0, 0, isInput: true)],  // INPUT position
          0,
          0,
        ));

        final compiler = TypeCompiler(env);
        final clause = TypedClause(
          head: goal('bar', [varTerm('X', isReader: true)]),
        );

        final result = checkClause(clause, env, compiler);

        expect(result.isWellTyped, isFalse);
        expect(result.errors, hasLength(1));
        expect(result.errors.first, isA<HeadError>());
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
    // Clauses with Body Atoms
    // =========================================================================

    group('Clauses with Body Atoms', () {
      // Per paper semantics:
      // - HEAD at INPUT position (T?): WRITER receives value from goal
      // - HEAD at OUTPUT position (T): READER provides value to goal
      // - BODY at INPUT position (T?): READER provides value to callee
      // - BODY at OUTPUT position (T): WRITER receives value from callee

      test('POSITIVE: clause with well-typed head and body', () {
        // procedure append(Stream?, Stream?, Stream).
        // CORRECT: append([], Ys, Ys?).
        // CORRECT: append([X|Xs], Ys, [X?|Zs?]) :- append(Xs?, Ys?, Zs).
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

        // Test first clause: append([], Ys, Ys?).
        // Arg 1 (input): [] constant - OK
        // Arg 2 (input): Ys writer - receives from goal
        // Arg 3 (output): Ys? reader - provides to goal
        final clause1 = TypedClause(
          head: goal('append', [
            nilTerm(),
            varTerm('Ys'),                    // writer at input position
            varTerm('Ys', isReader: true),    // reader at output position
          ]),
        );

        final result1 = checkClause(clause1, env, compiler);
        expect(result1.isWellTyped, isTrue);

        // Test second clause: append([X|Xs], Ys, [X?|Zs?]) :- append(Xs?, Ys?, Zs).
        // Head arg 1 (input): [X|Xs] writers - receives from goal
        // Head arg 2 (input): Ys writer - receives from goal
        // Head arg 3 (output): [X?|Zs?] readers - provides to goal
        // Body arg 1 (input): Xs? reader - provides to callee
        // Body arg 2 (input): Ys? reader - provides to callee
        // Body arg 3 (output): Zs writer - receives from callee
        final clause2 = TypedClause(
          head: goal('append', [
            consTerm(varTerm('X'), varTerm('Xs')),              // writers at input
            varTerm('Ys'),                                      // writer at input
            consTerm(varTerm('X', isReader: true), varTerm('Zs', isReader: true)), // readers at output
          ]),
          bodyAtoms: [
            goal('append', [
              varTerm('Xs', isReader: true),   // reader provides to callee
              varTerm('Ys', isReader: true),   // reader provides to callee
              varTerm('Zs'),                   // writer receives from callee
            ]),
          ],
        );

        final result2 = checkClause(clause2, env, compiler);
        expect(result2.isWellTyped, isTrue);
      });

      test('NEGATIVE: clause with wrong modes in nested list', () {
        // procedure append(Stream?, Stream?, Stream).
        // WRONG: append([X?|Xs?], Ys, [X|Zs]).
        // - Arg 1 (input): [X?|Xs?] readers - should be writers [X|Xs]
        // - Arg 3 (output): [X|Zs] has X writer - should be X? reader
        // Mode checking happens at the list element level (_ position in Stream)
        final env = createBasicEnvironment();
        env.addProcedure(ProcDecl(
          'append',
          [
            TypeRef('Stream', 0, 0, isInput: true),
            TypeRef('Stream', 0, 0, isInput: true),
            TypeRef('Stream', 0, 0),
          ],
          0,
          0,
        ));

        final compiler = TypeCompiler(env);

        final clause = TypedClause(
          head: goal('append', [
            consTerm(varTerm('X', isReader: true), varTerm('Xs', isReader: true)),  // WRONG: readers at input
            varTerm('Ys'),
            consTerm(varTerm('X'), varTerm('Zs')),  // WRONG: writer X at output
          ]),
        );

        final result = checkClause(clause, env, compiler);
        expect(result.isWellTyped, isFalse);
      });

      test('undefined procedure in body is caught', () {
        final env = createBasicEnvironment();
        env.addProcedure(ProcDecl(
          'foo',
          [TypeRef('Any', 0, 0)],  // output position
          0,
          0,
        ));

        final compiler = TypeCompiler(env);
        final clause = TypedClause(
          head: goal('foo', [varTerm('X', isReader: true)]),  // reader at output
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
      // Pair ::= pair(_, _?)
      // - Position 1: _ (produce mode)
      // - Position 2: _? (consume mode)
      //
      // For OUTPUT arg in head (procedure test(Pair)):
      // - Head should have READER (provides value to goal)
      // - After flip: reader X? → writer X (produce)
      // - After flip: writer X → reader X? (consume)
      //
      // CORRECT: test(pair(X?, X))
      // - X? at pos 1 → after flip X (produce) matches _ (produce) ✓
      // - X at pos 2 → after flip X? (consume) matches _? (consume) ✓

      test('POSITIVE: X? and X at complementary positions in Pair', () {
        // Pair ::= pair(_, _?)
        // procedure test(Pair).  -- OUTPUT position
        // CORRECT: test(pair(X?, X)).
        // - X? reader at produce position → after flip: X (produce) ✓
        // - X writer at consume position → after flip: X? (consume) ✓
        final env = createBasicEnvironment();
        env.addProcedure(ProcDecl(
          'test',
          [TypeRef('Pair', 0, 0)],  // OUTPUT position
          0,
          0,
        ));

        final compiler = TypeCompiler(env);
        final clause = TypedClause(
          head: goal('test', [
            structTerm('pair', [
              varTerm('X', isReader: true),  // reader at produce position
              varTerm('X'),                  // writer at consume position
            ]),
          ]),
        );

        final result = checkClause(clause, env, compiler);

        expect(result.isWellTyped, isTrue);
        expect(result.variableTypes, contains('X'));
        expect(result.variableTypes, contains('X?'));
      });

      test('NEGATIVE: X and X? at wrong positions in Pair', () {
        // Pair ::= pair(_, _?)
        // procedure test(Pair).  -- OUTPUT position
        // WRONG: test(pair(X, X?)).
        // - X writer at produce position → after flip: X? (consume) ≠ _ (produce)
        // - X? reader at consume position → after flip: X (produce) ≠ _? (consume)
        final env = createBasicEnvironment();
        env.addProcedure(ProcDecl(
          'test',
          [TypeRef('Pair', 0, 0)],  // OUTPUT position
          0,
          0,
        ));

        final compiler = TypeCompiler(env);
        final clause = TypedClause(
          head: goal('test', [
            structTerm('pair', [
              varTerm('X'),                  // WRONG: writer at produce position
              varTerm('X', isReader: true),  // WRONG: reader at consume position
            ]),
          ]),
        );

        final result = checkClause(clause, env, compiler);

        expect(result.isWellTyped, isFalse);
      });

      test('NEGATIVE: X and X? at different types are NOT complementary', () {
        // procedure mismatch(Nat, Stream?).
        // mismatch(X?, X).
        // - Arg 1 (output Nat): X? reader - provides to goal ✓
        // - Arg 2 (input Stream?): X writer - receives from goal ✓
        // But X and X? have different types (Nat vs Stream) - NOT complementary
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
            varTerm('X', isReader: true),  // reader at output position
            varTerm('X'),                  // writer at input position
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
          [TypeRef('Any', 0, 0), TypeRef('Any', 0, 0)], // foo/2
          0,
          0,
        ));

        final compiler = TypeCompiler(env);
        final clause = TypedClause(
          head: goal('foo', [varTerm('X', isReader: true)]), // foo/1 - not defined
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
