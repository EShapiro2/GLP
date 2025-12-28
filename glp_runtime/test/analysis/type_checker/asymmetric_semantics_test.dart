// test/analysis/type_checker/asymmetric_semantics_test.dart
//
// Tests for asymmetric well-typing semantics (spec v1.13 Section 6.1)
// Output positions: only containment required (inferred ⊆ declared)
// Input positions: coverage required (declared ⊆ inferred)

import 'package:test/test.dart';
import 'test_helpers.dart';

void main() {
  group('Asymmetric Well-Typing Semantics', () {

    // =========================================================================
    // OUTPUT POSITIONS - Only containment required, no coverage
    // =========================================================================

    group('Output Positions (Containment Only)', () {

      test('POSITIVE: succ produces subset of Nat at output position', () {
        // succ(N, s(N?)) produces only s(Nat), not full Nat
        // But s(Nat) ⊆ Nat, so output containment is satisfied
        // No coverage check at output position
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).
          procedure succ(Nat?, Nat).
          succ(N, s(N?)).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Output containment: s(Nat) ⊆ Nat is satisfied');
      });

      test('POSITIVE: generate only positive numbers', () {
        // Produces s(Nat) which is subset of Nat
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).
          procedure generate_positive(Nat).
          generate_positive(s(N?)) :- generate_positive(N).
          generate_positive(s(0)).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Output containment: s(Nat) ⊆ Nat');
      });

      test('POSITIVE: always return true (output needs no coverage)', () {
        // Produces only 'true', which is subset of Bool
        // No coverage required at output position
        final result = checkTypes('''
          Bool ::= true ; false.
          procedure always_true(Bool).
          always_true(true).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Output containment: {true} ⊆ Bool, no coverage needed');
      });

      test('POSITIVE: output position needs no structural coverage', () {
        // Only produces 'red', not full Color - should PASS
        final result = checkTypes('''
          Color ::= red ; green ; blue.
          procedure make_red(Color).
          make_red(red).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Output containment: {red} ⊆ Color, no coverage needed');
      });

      test('NEGATIVE: output produces value outside declared type', () {
        // Produces 'foo' which is not in Nat
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).
          procedure bad_output(Nat).
          bad_output(foo).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Output containment violated: foo ∉ Nat');
      });

      test('NEGATIVE: output produces wrong constructor', () {
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).
          procedure bad_succ(Nat?, Nat).
          bad_succ(N, p(N?)).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Output containment violated: p(Nat) ∉ Nat');
      });

    });

    // =========================================================================
    // INPUT POSITIONS - Coverage required
    // =========================================================================

    group('Input Positions (Coverage Required)', () {

      test('POSITIVE: pred covers all Nat inputs', () {
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).
          procedure pred(Nat?, Nat).
          pred(0, 0).
          pred(s(N), N?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Input coverage: both 0 and s(Nat) handled');
      });

      test('NEGATIVE: pred missing zero case at input', () {
        // Only handles s(N), doesn't handle 0
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).
          procedure pred(Nat?, Nat).
          pred(s(N), N?).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Input coverage violated: 0 not handled');
      });

      test('NEGATIVE: missing input alternative', () {
        final result = checkTypes('''
          Color ::= red ; green ; blue.
          procedure process_color(Color?, Color).
          process_color(red, red).
          process_color(green, green).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Input coverage violated: blue not handled');
      });

      test('POSITIVE: all input alternatives covered', () {
        final result = checkTypes('''
          Color ::= red ; green ; blue.
          procedure process_color(Color?, Color).
          process_color(red, red).
          process_color(green, green).
          process_color(blue, blue).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Input coverage: all alternatives handled');
      });

    });

    // =========================================================================
    // MIXED POSITIONS - Asymmetric behavior
    // =========================================================================

    group('Mixed Input/Output Positions', () {

      test('POSITIVE: input fully covered, output partial', () {
        // First arg (input): must cover all Nat
        // Second arg (output): can produce subset
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).
          procedure increment(Nat?, Nat).
          increment(0, s(0)).
          increment(s(N), s(s(N?))).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Input covered, output containment satisfied');
      });

      test('NEGATIVE: input incomplete even if output complete', () {
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).
          procedure decrement(Nat?, Nat).
          decrement(s(N), N?).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Input coverage violated even though output could produce all Nat');
      });

    });

    // =========================================================================
    // LIST EXAMPLES
    // =========================================================================

    group('List Examples', () {

      test('POSITIVE: copy covers all List inputs', () {
        final result = checkTypes('''
          Elem ::= a ; b.
          MyList ::= [] ; [Elem | MyList].
          procedure copy(MyList?, MyList).
          copy([], []).
          copy([X | Xs], [X? | Ys]) :- copy(Xs?, Ys).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Input coverage: [] and [_|_] both handled');
      });

      test('NEGATIVE: missing nil case at input', () {
        final result = checkTypes('''
          Elem ::= a ; b.
          MyList ::= [] ; [Elem | MyList].
          procedure copy(MyList?, MyList).
          copy([X | Xs], [X? | Ys]) :- copy(Xs?, Ys).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Input coverage violated: [] not handled');
      });

      test('POSITIVE: generate only non-empty lists (output)', () {
        // Output can produce subset (only cons, no nil)
        final result = checkTypes('''
          Elem ::= a ; b.
          MyList ::= [] ; [Elem | MyList].
          procedure make_nonempty(Elem?, MyList).
          make_nonempty(X, [X? | []]).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Output containment: [Elem|[]] ⊆ MyList');
      });

    });

  });
}
