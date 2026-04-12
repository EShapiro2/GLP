// glp_runtime/test/compiler/partial_evaluator_test.dart
//
// Tests for partial evaluator guard validation.
// Verifies that:
//   - Single-unit-clause procedures can be called in guard position (unfolded)
//   - Builtin guards are kept as-is
//   - Non-unit-clause procedures (multiple clauses or has body/guards) are rejected
//
// Spec: docs/typed-glp-manual.md Section 8 (Single-Unit-Clause Procedures)
// Spec: docs/guards-reference.md

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/compiler/partial_evaluator.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/error.dart';

void main() {
  // Set prelude unit clause source from programs/self.glp
  final rootSelfGlp = File('../programs/self.glp');
  if (rootSelfGlp.existsSync()) {
    setPreludeUnitClauseSource(rootSelfGlp.readAsStringSync());
  }
  group('PartialEvaluator guard validation', () {
    late PartialEvaluator pe;

    setUp(() {
      pe = PartialEvaluator();
    });

    /// Helper to parse GLP source and run partial evaluator.
    /// Uses parseModule() to properly handle procedure declarations.
    void runPE(String source) {
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final module = parser.parseModule();
      final program = Program(module.procedures, module.line, module.column);
      pe.transformDefinedGuards(program);
    }

    test('accepts single-unit-clause procedure in guard position', () {
      // my_guard/2 is a single unit clause - should be unfolded successfully
      const source = '''
        procedure my_guard(_, _).
        my_guard(foo(X?, Y), bar(X, Y?)).

        procedure test(_).
        test(R) :- my_guard(foo(A, B?), R) | work(A?, B).

        procedure work(_, _).
        work(X?, Y?) :- true.
      ''';

      // Should not throw
      expect(() => runPE(source), returnsNormally);
    });

    test('accepts builtin guard (integer/1)', () {
      // integer/1 is a builtin - should be kept as-is
      const source = '''
        procedure test(_).
        test(X?) :- integer(X?) | process(X?).

        procedure process(_).
        process(X?) :- true.
      ''';

      expect(() => runPE(source), returnsNormally);
    });

    test('accepts builtin guard (ground/1)', () {
      const source = '''
        procedure test(_).
        test(X?) :- ground(X?) | process(X?).

        procedure process(_).
        process(X?) :- true.
      ''';

      expect(() => runPE(source), returnsNormally);
    });

    test('accepts builtin guard (number/1)', () {
      const source = '''
        procedure test(_).
        test(X?) :- number(X?) | process(X?).

        procedure process(_).
        process(X?) :- true.
      ''';

      expect(() => runPE(source), returnsNormally);
    });

    test('accepts builtin comparison guards', () {
      const source = '''
        procedure test(_, _).
        test(X?, Y?) :- X? < Y? | less(X?, Y?).
        test(X?, Y?) :- X? > Y? | greater(X?, Y?).
        test(X?, Y?) :- X? =:= Y? | equal(X?, Y?).

        procedure less(_, _).
        less(X?, Y?) :- true.

        procedure greater(_, _).
        greater(X?, Y?) :- true.

        procedure equal(_, _).
        equal(X?, Y?) :- true.
      ''';

      expect(() => runPE(source), returnsNormally);
    });

    test('rejects procedure with multiple clauses in guard position', () {
      // multi/1 has two clauses - cannot be used in guard position
      const source = '''
        procedure multi(_).
        multi(a).
        multi(b).

        procedure test(_).
        test(X?) :- multi(X?) | process(X?).

        procedure process(_).
        process(X?) :- true.
      ''';

      expect(
        () => runPE(source),
        throwsA(
          isA<CompileError>().having(
            (e) => e.message,
            'message',
            contains('Cannot call "multi/1" in guard position'),
          ),
        ),
      );
    });

    test('rejects procedure with body in guard position', () {
      // has_body/1 has a body (not a unit clause) - cannot be used in guard
      const source = '''
        procedure helper(_).
        helper(x).

        procedure has_body(_).
        has_body(X) :- helper(X?).

        procedure test(_).
        test(X?) :- has_body(X?) | process(X?).

        procedure process(_).
        process(X?) :- true.
      ''';

      expect(
        () => runPE(source),
        throwsA(
          isA<CompileError>().having(
            (e) => e.message,
            'message',
            contains('Cannot call "has_body/1" in guard position'),
          ),
        ),
      );
    });

    test('rejects procedure with guards in guard position', () {
      // has_guard/1 has a guard (not a unit clause) - cannot be used in guard
      const source = '''
        procedure has_guard(_).
        has_guard(X) :- integer(X?) | true.

        procedure test(_).
        test(X?) :- has_guard(X?) | process(X?).

        procedure process(_).
        process(X?) :- true.
      ''';

      expect(
        () => runPE(source),
        throwsA(
          isA<CompileError>().having(
            (e) => e.message,
            'message',
            contains('Cannot call "has_guard/1" in guard position'),
          ),
        ),
      );
    });

    test('error message mentions multiple clauses or non-unit clauses', () {
      const source = '''
        procedure bad(_).
        bad(a).
        bad(b).

        procedure test(_).
        test(X?) :- bad(X?) | ok.

        procedure ok.
        ok :- true.
      ''';

      expect(
        () => runPE(source),
        throwsA(
          isA<CompileError>().having(
            (e) => e.message,
            'message',
            contains('multiple clauses or non-unit clauses'),
          ),
        ),
      );
    });

    test('mixed guards: builtin and single-unit-clause both accepted', () {
      // my_check/1 is a unit clause guard that matches a constant
      // integer/1 is a builtin guard
      // Both should work together
      const source = '''
        procedure my_check(_).
        my_check(foo(a)).

        procedure test(_).
        test(X?) :- integer(X?), my_check(foo(a)) | process(X?).

        procedure process(_).
        process(X?) :- true.
      ''';

      expect(() => runPE(source), returnsNormally);
    });

    test('unit clause (new_channel pattern) accepted in guard', () {
      // This test verifies that unit clauses for channel creation work
      // The unit clause new_channel/2 should be accepted and unfolded
      const source = '''
        procedure new_channel(_, _).
        new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).

        procedure play.
        play :- new_channel(AliceCh, BobCh) | alice(AliceCh?), bob(BobCh?).

        procedure alice(_).
        alice(Ch?) :- true.

        procedure bob(_).
        bob(Ch?) :- true.
      ''';

      expect(() => runPE(source), returnsNormally);
    });

    test('rejects negated defined guard', () {
      // Defined guards cannot be negated
      const source = '''
        procedure my_guard(_).
        my_guard(foo).

        procedure test(_).
        test(X?) :- ~my_guard(X?) | process(X?).

        procedure process(_).
        process(X?) :- true.
      ''';

      expect(
        () => runPE(source),
        throwsA(
          isA<CompileError>().having(
            (e) => e.message,
            'message',
            contains('cannot be negated'),
          ),
        ),
      );
    });
  });
}
