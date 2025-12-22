// test/analysis/type_checker/clause_contribution_test.dart
//
// Tests for clause contribution computation

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/clause_contribution.dart';
import 'package:glp_runtime/analysis/type_checker/type_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'package:glp_runtime/analysis/type_checker/type_compiler.dart';
import 'package:glp_runtime/compiler/ast.dart' as ast;

void main() {
  group('ClauseContributionComputer', () {

    group('constant patterns', () {
      test('atom constant produces singleton DFA', () {
        final typeEnv = parseTypes('');
        final computer = ClauseContributionComputer(typeEnv);

        final pattern = ast.ConstTerm('foo', 0, 0);
        final result = computer.computeArgContribution(pattern, {});

        final expected = TypeDFA.singleton('foo');
        expect(result.isEquivalent(expected), isTrue);
      });

      test('integer constant produces singleton DFA', () {
        final typeEnv = parseTypes('');
        final computer = ClauseContributionComputer(typeEnv);

        final pattern = ast.ConstTerm(42, 0, 0);
        final result = computer.computeArgContribution(pattern, {});

        final expected = TypeDFA.singleton('42');
        expect(result.isEquivalent(expected), isTrue);
      });

      test('double constant produces singleton DFA', () {
        final typeEnv = parseTypes('');
        final computer = ClauseContributionComputer(typeEnv);

        final pattern = ast.ConstTerm(3.14, 0, 0);
        final result = computer.computeArgContribution(pattern, {});

        final expected = TypeDFA.singleton('3.14');
        expect(result.isEquivalent(expected), isTrue);
      });

      test('empty list produces singleton DFA', () {
        final typeEnv = parseTypes('');
        final computer = ClauseContributionComputer(typeEnv);

        final pattern = ast.ListTerm(null, null, 0, 0);
        final result = computer.computeArgContribution(pattern, {});

        final expected = TypeDFA.singleton('[]');
        expect(result.isEquivalent(expected), isTrue);
      });
    });

    group('variable patterns', () {
      test('variable produces its inferred type DFA', () {
        final typeEnv = parseTypes('''
Nat ::= 0 ; s(Nat).
''');
        final compiler = TypeCompiler(typeEnv);
        final computer = ClauseContributionComputer(typeEnv);

        final natDFA = compiler.compile('Nat');
        final pattern = ast.VarTerm('X', false, 0, 0);
        final varTypes = {'X': natDFA};

        final result = computer.computeArgContribution(pattern, varTypes);

        expect(result.isEquivalent(natDFA), isTrue);
      });

      test('reader variable produces same type as writer', () {
        final typeEnv = parseTypes('''
Nat ::= 0 ; s(Nat).
''');
        final compiler = TypeCompiler(typeEnv);
        final computer = ClauseContributionComputer(typeEnv);

        final natDFA = compiler.compile('Nat');
        final writerPattern = ast.VarTerm('X', false, 0, 0);
        final readerPattern = ast.VarTerm('X', true, 0, 0);
        final varTypes = {'X': natDFA};

        final writerResult = computer.computeArgContribution(writerPattern, varTypes);
        final readerResult = computer.computeArgContribution(readerPattern, varTypes);

        expect(writerResult.isEquivalent(readerResult), isTrue);
      });

      test('unknown variable produces empty DFA', () {
        final typeEnv = parseTypes('');
        final computer = ClauseContributionComputer(typeEnv);

        final pattern = ast.VarTerm('Unknown', false, 0, 0);
        final result = computer.computeArgContribution(pattern, {});

        expect(result.isEmpty, isTrue);
      });
    });

    group('structured patterns', () {
      test('nullary functor acts like constant', () {
        final typeEnv = parseTypes('');
        final computer = ClauseContributionComputer(typeEnv);

        final pattern = ast.StructTerm('zero', [], 0, 0);
        final result = computer.computeArgContribution(pattern, {});

        final expected = TypeDFA.singleton('zero');
        expect(result.isEquivalent(expected), isTrue);
      });

      test('s(X) with X:empty produces empty (no ground instances)', () {
        final typeEnv = parseTypes('');
        final computer = ClauseContributionComputer(typeEnv);

        final pattern = ast.StructTerm('s', [ast.VarTerm('X', false, 0, 0)], 0, 0);
        final varTypes = {'X': TypeDFA.empty()};

        final result = computer.computeArgContribution(pattern, varTypes);

        // If X has empty type, s(X) also has empty language
        expect(result.isEmpty, isTrue);
      });

      test('f(0, 1) produces singleton with two constants', () {
        final typeEnv = parseTypes('');
        final computer = ClauseContributionComputer(typeEnv);

        final pattern = ast.StructTerm('f', [
          ast.ConstTerm(0, 0, 0),
          ast.ConstTerm(1, 0, 0),
        ], 0, 0);
        final result = computer.computeArgContribution(pattern, {});

        // Should produce a DFA that accepts f(0,1)
        // Check it's not empty
        expect(result.isEmpty, isFalse);

        // The DFA should have transitions for f/2 arg positions
        expect(result.transitions.isNotEmpty, isTrue);
      });

      test('pair(X, Y) with X:a, Y:b produces singleton pair', () {
        final typeEnv = parseTypes('');
        final computer = ClauseContributionComputer(typeEnv);

        final xDFA = TypeDFA.singleton('a');
        final yDFA = TypeDFA.singleton('b');

        final pattern = ast.StructTerm('pair', [
          ast.VarTerm('X', false, 0, 0),
          ast.VarTerm('Y', false, 0, 0),
        ], 0, 0);
        final varTypes = {'X': xDFA, 'Y': yDFA};

        final result = computer.computeArgContribution(pattern, varTypes);

        // Should produce DFA accepting pair(a, b)
        expect(result.isEmpty, isFalse);
      });
    });

    group('list patterns', () {
      test('[X|Xs] with X:empty produces empty', () {
        final typeEnv = parseTypes('');
        final computer = ClauseContributionComputer(typeEnv);

        final pattern = ast.ListTerm(
          ast.VarTerm('X', false, 0, 0),
          ast.VarTerm('Xs', false, 0, 0),
          0, 0,
        );
        final varTypes = {
          'X': TypeDFA.empty(),
          'Xs': TypeDFA.singleton('[]'),
        };

        final result = computer.computeArgContribution(pattern, varTypes);

        // If head type is empty, list is empty
        expect(result.isEmpty, isTrue);
      });

      test('[a|[]] produces non-empty DFA', () {
        final typeEnv = parseTypes('');
        final computer = ClauseContributionComputer(typeEnv);

        final pattern = ast.ListTerm(
          ast.ConstTerm('a', 0, 0),
          ast.ListTerm(null, null, 0, 0),
          0, 0,
        );
        final result = computer.computeArgContribution(pattern, {});

        // [a] should produce non-empty DFA
        expect(result.isEmpty, isFalse);
      });

      test('[X|Xs] with X:singleton, Xs:[] produces singleton list', () {
        final typeEnv = parseTypes('');
        final computer = ClauseContributionComputer(typeEnv);

        final xDFA = TypeDFA.singleton('a');
        final xsDFA = TypeDFA.singleton('[]');

        final pattern = ast.ListTerm(
          ast.VarTerm('X', false, 0, 0),
          ast.VarTerm('Xs', false, 0, 0),
          0, 0,
        );
        final varTypes = {'X': xDFA, 'Xs': xsDFA};

        final result = computer.computeArgContribution(pattern, varTypes);

        // Should produce [a] = [a|[]]
        expect(result.isEmpty, isFalse);
      });
    });

    group('union of contributions', () {
      test('two constants produce union with two elements', () {
        final typeEnv = parseTypes('');
        final computer = ClauseContributionComputer(typeEnv);

        final pattern1 = ast.ConstTerm('a', 0, 0);
        final pattern2 = ast.ConstTerm('b', 0, 0);

        final result1 = computer.computeArgContribution(pattern1, {});
        final result2 = computer.computeArgContribution(pattern2, {});
        final union = result1.union(result2);

        // Union should accept both 'a' and 'b'
        final pathA = TermPath([PathElement.constant('a')]);
        final pathB = TermPath([PathElement.constant('b')]);
        final pathC = TermPath([PathElement.constant('c')]);

        expect(union.acceptsPath(pathA), isTrue);
        expect(union.acceptsPath(pathB), isTrue);
        expect(union.acceptsPath(pathC), isFalse);
      });

      test('empty union empty is empty', () {
        final empty1 = TypeDFA.empty();
        final empty2 = TypeDFA.empty();
        final union = empty1.union(empty2);

        expect(union.isEmpty, isTrue);
      });

      test('singleton union singleton is both', () {
        final a = TypeDFA.singleton('a');
        final b = TypeDFA.singleton('b');
        final ab = a.union(b);

        expect(ab.isEmpty, isFalse);

        final pathA = TermPath([PathElement.constant('a')]);
        final pathB = TermPath([PathElement.constant('b')]);

        expect(ab.acceptsPath(pathA), isTrue);
        expect(ab.acceptsPath(pathB), isTrue);
      });
    });

  });
}
