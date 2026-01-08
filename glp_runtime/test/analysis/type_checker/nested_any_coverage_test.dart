// test/analysis/type_checker/nested_any_coverage_test.dart
//
// Tests for mode coverage checking with nested primitive positions in lists

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'package:glp_runtime/analysis/type_checker/mode_checker.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/ast.dart';

/// Helper to parse GLP source and extract clauses for first procedure
List<Clause> parseClauses(String source) {
  final lexer = Lexer(source);
  final tokens = lexer.tokenize();
  final parser = Parser(tokens);
  final program = parser.parse();
  if (program.procedures.isEmpty) {
    return [];
  }
  return program.procedures[0].clauses;
}

void main() {
  group('Nested primitive coverage in List', () {

    test('PrimList with output-only head - should PASS', () {
      // PrimList has _ at head position (single mode, no coverage required)
      final typeDecl = '''
PrimList ::= [] ; [_ | PrimList].
procedure copy(PrimList?, PrimList).
''';
      final clauseCode = '''
copy([], []).
copy([H? | In], [H | Out?]) :- copy(In?, Out).
''';

      final clauses = parseClauses(clauseCode);
      final typeEnv = parseTypes(typeDecl);
      final checker = ModeChecker(typeEnv);
      final errors = checker.checkProcedure('copy', 2, clauses);

      print('\n=== Test: PrimList with output-only head ===');
      print('Errors found: ${errors.length}');
      for (var error in errors) {
        print('  - ${error.message}');
      }

      // Should pass - PrimList head is _ only
      final hasCoverageError = errors.any((e) =>
        e.message.contains('coverage') || e.message.contains('Coverage'));

      expect(hasCoverageError, isFalse,
        reason: 'PrimList with single mode head should not require full coverage');
    });
  });
}
