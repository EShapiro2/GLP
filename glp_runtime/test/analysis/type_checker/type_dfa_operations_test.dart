// test/analysis/type_checker/type_dfa_operations_test.dart
//
// Tests for TypeDFA operations required for fixpoint checking

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/moded_label.dart';

void main() {
  group('TypeDFA Operations', () {

    group('empty and singleton', () {
      test('empty DFA accepts nothing', () {
        final dfa = TypeDFA.empty();
        expect(dfa.isEmpty, isTrue);
      });

      test('singleton accepts exactly one constant', () {
        final dfa = TypeDFA.singleton('foo');
        expect(dfa.isEmpty, isFalse);

        // Should accept path with just 'foo'
        final fooPath = TermPath([ModedLabel.constant('foo')]);
        expect(dfa.acceptsPath(fooPath), isTrue);

        // Should reject different constant
        final barPath = TermPath([ModedLabel.constant('bar')]);
        expect(dfa.acceptsPath(barPath), isFalse);

        // Should reject longer paths
        final longerPath = TermPath([
          ModedLabel.constant('foo'),
          ModedLabel.constant('bar'),
        ]);
        expect(dfa.acceptsPath(longerPath), isFalse);
      });
    });

    group('union', () {
      test('union of disjoint singletons', () {
        final a = TypeDFA.singleton('a');
        final b = TypeDFA.singleton('b');
        final ab = a.union(b);

        final pathA = TermPath([ModedLabel.constant('a')]);
        final pathB = TermPath([ModedLabel.constant('b')]);
        final pathC = TermPath([ModedLabel.constant('c')]);

        expect(ab.acceptsPath(pathA), isTrue);
        expect(ab.acceptsPath(pathB), isTrue);
        expect(ab.acceptsPath(pathC), isFalse);
      });

      test('union with empty is identity', () {
        final a = TypeDFA.singleton('a');
        final empty = TypeDFA.empty();
        final result = a.union(empty);

        expect(result.isEquivalent(a), isTrue);
      });

      test('union is commutative', () {
        final a = TypeDFA.singleton('a');
        final b = TypeDFA.singleton('b');

        final ab = a.union(b);
        final ba = b.union(a);

        expect(ab.isEquivalent(ba), isTrue);
      });
    });

    group('complement', () {
      test('complement of singleton rejects that constant', () {
        final a = TypeDFA.singleton('a');
        final notA = a.complete().flip();

        final pathA = TermPath([ModedLabel.constant('a')]);
        expect(notA.acceptsPath(pathA), isFalse);
      });

      test('double complement is identity', () {
        final a = TypeDFA.singleton('a');
        final doubleComp = a.complete().flip().flip();

        expect(doubleComp.isEquivalent(a.complete()), isTrue);
      });

      test('complement requires complete DFA', () {
        final a = TypeDFA.singleton('a');
        final completed = a.complete();

        // Complete DFA should have sink state
        expect(completed.states.length, greaterThan(a.states.length));
      });
    });

    group('isSubsetOf', () {
      test('singleton is subset of union', () {
        final a = TypeDFA.singleton('a');
        final b = TypeDFA.singleton('b');
        final ab = a.union(b);

        expect(a.isSubsetOf(ab), isTrue);
        expect(b.isSubsetOf(ab), isTrue);
        expect(ab.isSubsetOf(a), isFalse);
      });

      test('empty is subset of everything', () {
        final empty = TypeDFA.empty();
        final a = TypeDFA.singleton('a');

        expect(empty.isSubsetOf(a), isTrue);
        expect(empty.isSubsetOf(empty), isTrue);
      });

      test('DFA is subset of itself', () {
        final a = TypeDFA.singleton('a');
        expect(a.isSubsetOf(a), isTrue);
      });

      test('union is superset of components', () {
        final a = TypeDFA.singleton('a');
        final b = TypeDFA.singleton('b');
        final c = TypeDFA.singleton('c');

        final ab = a.union(b);
        final abc = ab.union(c);

        expect(ab.isSubsetOf(abc), isTrue);
        expect(abc.isSubsetOf(ab), isFalse);
      });
    });

    group('isEquivalent', () {
      test('same DFA is equivalent to itself', () {
        final a = TypeDFA.singleton('a');
        expect(a.isEquivalent(a), isTrue);
      });

      test('union is commutative (equivalence)', () {
        final a = TypeDFA.singleton('a');
        final b = TypeDFA.singleton('b');

        final ab = a.union(b);
        final ba = b.union(a);

        expect(ab.isEquivalent(ba), isTrue);
      });

      test('different singletons are not equivalent', () {
        final a = TypeDFA.singleton('a');
        final b = TypeDFA.singleton('b');

        expect(a.isEquivalent(b), isFalse);
      });

      test('empty DFAs are equivalent', () {
        final empty1 = TypeDFA.empty();
        final empty2 = TypeDFA.empty();

        expect(empty1.isEquivalent(empty2), isTrue);
      });

      test('union is associative (equivalence)', () {
        final a = TypeDFA.singleton('a');
        final b = TypeDFA.singleton('b');
        final c = TypeDFA.singleton('c');

        final ab_c = a.union(b).union(c);
        final a_bc = a.union(b.union(c));

        expect(ab_c.isEquivalent(a_bc), isTrue);
      });
    });

    group('isEmpty', () {
      test('empty DFA has empty language', () {
        final empty = TypeDFA.empty();
        expect(empty.isEmpty, isTrue);
      });

      test('singleton has non-empty language', () {
        final a = TypeDFA.singleton('a');
        expect(a.isEmpty, isFalse);
      });

      test('union of non-empty DFAs is non-empty', () {
        final a = TypeDFA.singleton('a');
        final b = TypeDFA.singleton('b');
        final ab = a.union(b);

        expect(ab.isEmpty, isFalse);
      });

      test('union with empty is non-empty', () {
        final a = TypeDFA.singleton('a');
        final empty = TypeDFA.empty();
        final result = a.union(empty);

        expect(result.isEmpty, isFalse);
      });
    });

    group('complete', () {
      test('complete adds sink state', () {
        final a = TypeDFA.singleton('a');
        final completed = a.complete();

        // Should have more states (sink added)
        expect(completed.states.length, greaterThan(a.states.length));
      });

      test('complete is idempotent', () {
        final a = TypeDFA.singleton('a');
        final completed1 = a.complete();
        final completed2 = completed1.complete();

        // Completing again should not add more states
        expect(completed2.states.length, equals(completed1.states.length));
      });

      test('completed DFA is equivalent to original', () {
        final a = TypeDFA.singleton('a');
        final completed = a.complete();

        final pathA = TermPath([ModedLabel.constant('a')]);
        final pathB = TermPath([ModedLabel.constant('b')]);

        // Same acceptance behavior
        expect(completed.acceptsPath(pathA), equals(a.acceptsPath(pathA)));
        expect(completed.acceptsPath(pathB), equals(a.acceptsPath(pathB)));
      });
    });

    group('intersection', () {
      test('intersection of disjoint singletons is empty', () {
        final a = TypeDFA.singleton('a');
        final b = TypeDFA.singleton('b');
        final intersection = a.intersect(b);

        expect(intersection.isEmpty, isTrue);
      });

      test('intersection with itself is identity', () {
        final a = TypeDFA.singleton('a');
        final intersection = a.intersect(a);

        expect(intersection.isEquivalent(a), isTrue);
      });

      test('intersection is commutative', () {
        final a = TypeDFA.singleton('a');
        final b = TypeDFA.singleton('b');

        final ab = a.intersect(b);
        final ba = b.intersect(a);

        expect(ab.isEquivalent(ba), isTrue);
      });

      test('intersection with empty is empty', () {
        final a = TypeDFA.singleton('a');
        final empty = TypeDFA.empty();
        final result = a.intersect(empty);

        expect(result.isEmpty, isTrue);
      });
    });

    group('DeMorgan laws', () {
      test('complement of union equals intersection of complements', () {
        final a = TypeDFA.singleton('a');
        final b = TypeDFA.singleton('b');

        // ¬(A ∪ B) = ¬A ∩ ¬B
        // Must use combined alphabet for all operations
        final combinedAlphabet = a.alphabet.union(b.alphabet);
        final left = a.union(b).complete(combinedAlphabet).flip();
        final right = a.complete(combinedAlphabet).flip()
            .intersect(b.complete(combinedAlphabet).flip());

        expect(left.isEquivalent(right), isTrue);
      });

      test('complement of intersection equals union of complements', () {
        final a = TypeDFA.singleton('a');
        final b = TypeDFA.singleton('b');

        // ¬(A ∩ B) = ¬A ∪ ¬B
        // Must use combined alphabet for all operations
        final combinedAlphabet = a.alphabet.union(b.alphabet);
        final left = a.intersect(b).complete(combinedAlphabet).flip();
        final right = a.complete(combinedAlphabet).flip()
            .union(b.complete(combinedAlphabet).flip());

        expect(left.isEquivalent(right), isTrue);
      });
    });
  });
}
