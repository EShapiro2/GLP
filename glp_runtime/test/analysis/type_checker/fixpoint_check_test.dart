// test/analysis/type_checker/fixpoint_check_test.dart
//
// Tests for fixpoint checking: verify T_P^α(S) = S

import 'package:test/test.dart';
import 'test_helpers.dart';

void main() {
  group('Fixpoint Checking', () {

    // =========================================================================
    // POSITIVE CONTROLS - Should PASS
    // =========================================================================

    group('Complete Definitions', () {

      test('Nat: both base and recursive case', () {
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).
          procedure test(Nat?).
          test(0).
          test(s(N)) :- test(N?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Complete Nat definition should pass');
      });

      test('List: both nil and cons case', () {
        final result = checkTypes('''
          Atom ::= a ; b ; c.
          MyList ::= [] ; [Atom | MyList].
          procedure test(MyList?).
          test([]).
          test([X | Xs]) :- test(Xs?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Complete List definition should pass');
      });

      test('Color: all three alternatives', () {
        final result = checkTypes('''
          Color ::= red ; green ; blue.
          procedure test(Color?).
          test(red).
          test(green).
          test(blue).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'All Color alternatives covered');
      });

      test('Binary tree: leaf and node', () {
        final result = checkTypes('''
          Atom ::= a ; b ; c.
          Tree ::= leaf ; node(Tree, Atom, Tree).
          procedure test(Tree?).
          test(leaf).
          test(node(L, X, R)) :- test(L?), test(R?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Complete Tree definition');
      });

      test('Identity: single clause for single type alternative', () {
        final result = checkTypes('''
          Unit ::= unit.
          procedure test(Unit?).
          test(unit).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Single alternative fully covered');
      });

    });

    // =========================================================================
    // NEGATIVE CONTROLS - Should FAIL
    // =========================================================================

    group('Incomplete Definitions', () {

      test('Nat: missing base case', () {
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).
          procedure test(Nat?).
          test(s(N)) :- test(N?).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Missing 0 case should fail');
        expect(result.errors.any((e) =>
            e.message.contains('incomplete') ||
            e.message.contains('not cover')), isTrue,
            reason: 'Should report incomplete definition');
      });

      test('Nat: missing recursive case', () {
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).
          procedure test(Nat?).
          test(0).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Missing s(Nat) case should fail');
        expect(result.errors.any((e) =>
            e.message.contains('do not cover')), isTrue);
      });

      test('Color: missing middle alternative', () {
        final result = checkTypes('''
          Color ::= red ; green ; blue.
          procedure test(Color?).
          test(red).
          test(blue).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Missing green case should fail');
      });

      test('List: missing nil case', () {
        final result = checkTypes('''
          Atom ::= a ; b ; c.
          MyList ::= [] ; [Atom | MyList].
          procedure test(MyList?).
          test([X | Xs]) :- test(Xs?).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Missing [] case should fail');
      });

      test('Tree: missing leaf case', () {
        final result = checkTypes('''
          Atom ::= a ; b ; c.
          Tree ::= leaf ; node(Tree, Atom, Tree).
          procedure test(Tree?).
          test(node(L, X, R)) :- test(L?), test(R?).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Missing leaf case should fail');
      });

    });

    group('Over-broad Definitions', () {

      test('Nat: clause produces non-Nat value', () {
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).
          procedure test(Nat?).
          test(0).
          test(s(N)) :- test(N?).
          test(foo).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'foo is not in Nat');
        expect(result.errors.any((e) =>
            e.message.contains('not a subset') ||
            e.message.contains('outside') ||
            e.message.contains('not in type')), isTrue,
            reason: 'Should report containment violation');
      });

      test('Nat: wrong constructor', () {
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).
          procedure double(Nat?, Nat).
          double(0, 0).
          double(s(N), p(M)) :- double(N?, M).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'p(M) not in Nat');
        expect(result.errors.any((e) =>
            e.message.contains('not a subset') ||
            e.message.contains('outside') ||
            e.message.contains('not in type')), isTrue);
      });

      test('Color: extra alternative', () {
        final result = checkTypes('''
          Color ::= red ; green ; blue.
          procedure test(Color?).
          test(red).
          test(green).
          test(blue).
          test(yellow).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'yellow not in Color');
        expect(result.errors, isNotEmpty,
            reason: 'Pattern does not match type');
      });

    });

    // =========================================================================
    // STREAM SEMANTICS - Stream has no [] constructor
    // =========================================================================

    group('Stream Definitions', () {

      test('Stream: no nil case required (Stream has no [] constructor)', () {
        final result = checkTypes('''
          Atom ::= a ; b ; c.
          MyStream ::= [Atom | MyStream].
          procedure test(MyStream?).
          test([X | Xs]) :- test(Xs?).
        ''');
        // Stream has no [] constructor, so only cons case needed
        expect(result.isWellTyped, isTrue,
            reason: 'Stream has no [] constructor');
      });

    });

    // =========================================================================
    // EDGE CASES
    // =========================================================================

    group('Edge Cases', () {

      test('Empty type: no clauses needed', () {
        final result = checkTypes('''
          Empty ::= dummy.
          procedure test(Empty?).
        ''');
        // Empty type with no clauses is vacuously true
        // All clauses useless warning may be issued
        expect(result.errors.isEmpty, isTrue,
            reason: 'No clauses for empty input type is valid');
      });

      test('Multiple argument positions', () {
        final result = checkTypes('''
          Bool ::= true ; false.
          procedure and(Bool?, Bool?, Bool).
          and(true, true, true).
          and(true, false, false).
          and(false, true, false).
          and(false, false, false).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'All 4 combinations covered');
      });

      test('Multiple argument positions - incomplete per-argument', () {
        final result = checkTypes('''
          Bool ::= true ; false.
          procedure and(Bool?, Bool?, Bool).
          and(true, false, false).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Missing false in first argument position');
      });

    });

    // =========================================================================
    // ASYMMETRIC SEMANTICS - Output needs no coverage
    // =========================================================================

    group('Asymmetric Semantics', () {

      test('POSITIVE: output produces subset (no coverage needed)', () {
        // Second argument is OUTPUT - only needs containment, not coverage
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).
          procedure increment(Nat?, Nat).
          increment(0, s(0)).
          increment(s(N), s(s(N?))).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Output only needs containment: s(Nat) ⊆ Nat');
      });

      test('NEGATIVE: input incomplete despite full output', () {
        // First argument is INPUT - must cover all of Nat
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).
          procedure decrement(Nat?, Nat).
          decrement(s(N), N?).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Input must be covered: 0 not handled');
      });

    });

  });
}
