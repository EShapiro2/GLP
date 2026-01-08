// test/analysis/type_checker/type_checker_new_test.dart
//
// Tests for the new type_checker.dart implementation
// Specification: docs/modules/well-typed-program.md v0.5
// Paper Reference: Definition 4.10

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart';
import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/ast.dart' as ast;

void main() {
  group('TypeChecker (New Implementation)', () {
    // =========================================================================
    // Helper: Parse and check source
    // =========================================================================

    TypeCheckResult checkTypes(String source) {
      // Separate type declarations from clauses
      final lines = source.split('\n');
      final clauseLines = <String>[];

      for (final line in lines) {
        final trimmed = line.trim();
        if (trimmed.contains('::=') ||
            trimmed.startsWith('procedure ')) {
          continue;
        }
        if (trimmed.isNotEmpty && !trimmed.startsWith('%')) {
          clauseLines.add(line);
        }
      }

      final clauseSource = clauseLines.join('\n');

      // Parse clauses
      List<ast.Clause> clauses = [];
      if (clauseSource.trim().isNotEmpty) {
        final lexer = Lexer(clauseSource);
        final tokens = lexer.tokenize();
        final parser = Parser(tokens);
        final program = parser.parse();
        clauses = program.procedures.expand((p) => p.clauses).toList();
      }

      // Parse type declarations and check
      final typeEnv = parseTypes(source);
      final checker = TypeChecker(typeEnv);
      return checker.check(clauses);
    }

    // =========================================================================
    // Well-Typed Programs (Positive Controls)
    // =========================================================================

    group('Well-Typed Programs', () {
      test('identity procedure', () {
        // Using single-mode primitive type
        final source = '''
procedure id(_?, _).

id(X, X?).
''';
        final result = checkTypes(source);
        expect(result.isWellTyped, isTrue, reason: result.toString());
      });

      test('simple list append', () {
        // Use MyList to avoid conflict with predefined List
        final source = '''
MyList ::= [] ; [_|MyList].
procedure append(MyList?, MyList?, MyList).

append([], Ys, Ys?).
append([X|Xs], Ys, [X?|Zs?]) :- append(Xs?, Ys?, Zs).
''';
        final result = checkTypes(source);
        expect(result.isWellTyped, isTrue, reason: result.toString());
      });

      test('merge example from paper', () {
        // Use MyStream to avoid conflict with predefined Stream
        final source = '''
MyStream ::= [] ; [_|MyStream].
procedure merge(MyStream?, MyStream?, MyStream).

merge([], Ys, Ys?).
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
''';
        final result = checkTypes(source);
        expect(result.isWellTyped, isTrue, reason: result.toString());
      });

      test('Nat successor', () {
        // Using = to bind output values
        final source = '''
Nat ::= 0 ; s(Nat).
procedure succ(Nat?, Nat).

succ(0, R) :- R = s(0).
succ(s(N), R) :- succ(N?, M), R = s(M?).
''';
        final result = checkTypes(source);
        expect(result.isWellTyped, isTrue, reason: result.toString());
      });

      test('fact clause only (no body)', () {
        // Using = to bind output values
        final source = '''
Nat ::= 0 ; s(Nat).
procedure zero(Nat).

zero(X) :- X = 0.
''';
        final result = checkTypes(source);
        expect(result.isWellTyped, isTrue, reason: result.toString());
      });

      test('single output argument', () {
        // Using = to bind output values
        final source = '''
MyBool ::= mytrue ; myfalse.
procedure yes(MyBool).

yes(X) :- X = mytrue.
''';
        final result = checkTypes(source);
        expect(result.isWellTyped, isTrue, reason: result.toString());
      });
    });

    // =========================================================================
    // Covariance Failures (Clause Not Well-Typed)
    // =========================================================================

    group('Covariance Failures', () {
      test('wrong constant at output position', () {
        // 42 is not a valid MyStream value
        final source = '''
MyStream ::= [] ; [_|MyStream].
procedure head(MyStream?, MyStream).

head([X|Xs], 42).
''';
        final result = checkTypes(source);
        expect(result.isWellTyped, isFalse);
        expect(result.errors, isNotEmpty);
      });

      test('wrong functor in head', () {
        // foo is not a valid Nat functor
        final source = '''
Nat ::= 0 ; s(Nat).
procedure inc(Nat?, Nat).

inc(foo(X), s(X?)).
''';
        final result = checkTypes(source);
        expect(result.isWellTyped, isFalse);
        expect(result.errors, isNotEmpty);
      });

      test('wrong type in body atom', () {
        // 'hello' is an atom, not a Nat - unification should fail type check
        final source = '''
Nat ::= 0 ; s(Nat).
procedure double(Nat?, Nat).

double(N, M?) :- H = hello, double(H?, M).
''';
        final result = checkTypes(source);
        // Should fail because 'hello' is not a Nat
        expect(result.isWellTyped, isFalse);
      });
    });

    // =========================================================================
    // Contravariance Failures (Input Not Covered)
    // =========================================================================

    group('Contravariance Failures', () {
      test('missing nil case for list', () {
        final source = '''
MyStream ::= [] ; [_|MyStream].
procedure merge(MyStream?, MyStream?, MyStream).

merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
''';
        // Missing: merge([], Ys, Ys?).
        final result = checkTypes(source);
        expect(result.isWellTyped, isFalse);
        expect(result.errors.any((e) => e.message.contains('[]')), isTrue,
            reason: 'Should report uncovered [] alternative\n${result.toString()}');
      });

      test('missing cons case for list', () {
        final source = '''
MyList ::= [] ; [_|MyList].
procedure length(MyList?, Nat).

Nat ::= 0 ; s(Nat).

length([], R) :- R = 0.
''';
        // Missing: length([X|Xs], ...).
        final result = checkTypes(source);
        expect(result.isWellTyped, isFalse);
        expect(result.errors.any((e) => e.message.contains('[|]')), isTrue,
            reason: 'Should report uncovered [|] alternative\n${result.toString()}');
      });

      test('missing zero case for Nat', () {
        final source = '''
Nat ::= 0 ; s(Nat).
procedure pred(Nat?, Nat).

pred(s(N), N?).
''';
        // Missing: pred(0, ...).
        final result = checkTypes(source);
        expect(result.isWellTyped, isFalse);
        expect(result.errors.any((e) => e.message.contains('0')), isTrue,
            reason: 'Should report uncovered 0 alternative\n${result.toString()}');
      });

      test('missing successor case for Nat', () {
        final source = '''
Nat ::= 0 ; s(Nat).
procedure iszero(Nat?, Bool).

Bool ::= true ; false.

iszero(0, R) :- R = true.
''';
        // Missing: iszero(s(N), R) :- R = false.
        final result = checkTypes(source);
        expect(result.isWellTyped, isFalse);
        expect(result.errors.any((e) => e.message.contains('s')), isTrue,
            reason: 'Should report uncovered s alternative\n${result.toString()}');
      });
    });

    // =========================================================================
    // Variable Wildcards
    // =========================================================================

    group('Variable Wildcards', () {
      test('variable at input covers all alternatives', () {
        final source = '''
Nat ::= 0 ; s(Nat).
procedure copy(Nat?, Nat).

copy(X, X?).
''';
        // Variable X covers both 0 and s(Nat)
        final result = checkTypes(source);
        expect(result.isWellTyped, isTrue, reason: result.toString());
      });

      test('variable at list input covers nil and cons', () {
        final source = '''
MyList ::= [] ; [_|MyList].
procedure last(MyList?, _).

last(Xs, Y?).
''';
        // Variable Xs covers both [] and [_|MyList]
        // (Though semantically this would loop forever on nil)
        final result = checkTypes(source);
        expect(result.isWellTyped, isTrue, reason: result.toString());
      });
    });

    // =========================================================================
    // Warnings
    // =========================================================================

    group('Warnings', () {
      test('declared but not defined', () {
        final source = '''
Nat ::= 0 ; s(Nat).
procedure undefined_proc(Nat?, Nat).
''';
        final result = checkTypes(source);
        expect(result.warnings, isNotEmpty);
        expect(result.warnings.any((w) => w.message.contains('not defined')), isTrue);
      });

      test('defined but not declared', () {
        final source = '''
Nat ::= 0 ; s(Nat).

undeclared(X, X?).
''';
        final result = checkTypes(source);
        expect(result.warnings, isNotEmpty);
        expect(result.warnings.any((w) => w.message.contains('no type declaration')), isTrue);
      });
    });

    // =========================================================================
    // Complex Types
    // =========================================================================

    group('Complex Types', () {
      test('binary tree', () {
        final source = '''
Tree ::= leaf ; node(Tree, Tree).
procedure mirror(Tree?, Tree).

mirror(leaf, R) :- R = leaf.
mirror(node(L, R), node(R2?, L2?)) :- mirror(L?, L2), mirror(R?, R2).
''';
        final result = checkTypes(source);
        expect(result.isWellTyped, isTrue, reason: result.toString());
      });

      test('option type', () {
        final source = '''
Option ::= none ; some(_).
procedure get(Option?, _).

get(some(X), X?).
''';
        // Missing: get(none, ...) - but this might be intentional (partial function)
        final result = checkTypes(source);
        // Should report missing none case
        expect(result.errors.any((e) => e.message.contains('none')), isTrue);
      });
    });

    // =========================================================================
    // Multiple Input Arguments
    // =========================================================================

    group('Multiple Input Arguments', () {
      test('both arguments covered', () {
        final source = '''
MyBool ::= mytrue ; myfalse.
procedure myand(MyBool?, MyBool?, MyBool).

myand(mytrue, mytrue, R) :- R = mytrue.
myand(mytrue, myfalse, R) :- R = myfalse.
myand(myfalse, mytrue, R) :- R = myfalse.
myand(myfalse, myfalse, R) :- R = myfalse.
''';
        final result = checkTypes(source);
        expect(result.isWellTyped, isTrue, reason: result.toString());
      });

      test('first argument incomplete', () {
        final source = '''
MyBool ::= mytrue ; myfalse.
procedure myand(MyBool?, MyBool?, MyBool).

myand(mytrue, mytrue, R) :- R = mytrue.
myand(mytrue, myfalse, R) :- R = myfalse.
''';
        // Missing myfalse cases for first argument
        final result = checkTypes(source);
        expect(result.isWellTyped, isFalse);
      });
    });

    // =========================================================================
    // Edge Cases
    // =========================================================================

    group('Edge Cases', () {
      test('empty program', () {
        final source = '''
Nat ::= 0 ; s(Nat).
procedure test(Nat?, Nat).
''';
        final result = checkTypes(source);
        // No clauses defined - should warn but not error
        expect(result.warnings, isNotEmpty);
      });

      test('output only procedure', () {
        final source = '''
Nat ::= 0 ; s(Nat).
procedure zero(Nat).

zero(R) :- R = 0.
''';
        // No input arguments - contravariance check should be vacuous
        final result = checkTypes(source);
        expect(result.isWellTyped, isTrue, reason: result.toString());
      });
    });
  });
}
