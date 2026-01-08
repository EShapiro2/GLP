// test/analysis/type_checker/list_copy_coverage_test.dart
//
// Tests for mode coverage checking with subtype declarations

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
  group('Mode coverage for subtype declarations', () {

    test('Subtype declaration skips coverage check', () {
      // Partial ::< _ means subtype, no coverage required
      // Declared OUTPUT (Partial) → callee sees INPUT → expects READER
      final typeDecl = '''
Partial ::< _.
procedure test(Partial).
''';
      final clauseCode = 'test(X?).';  // Reader at callee INPUT position

      final clauses = parseClauses(clauseCode);
      final typeEnv = parseTypes(typeDecl);
      final checker = ModeChecker(typeEnv);
      final errors = checker.checkProcedure('test', 1, clauses);

      print('\n=== Test: Subtype declaration ===');
      print('Type decl: $typeDecl');
      print('Clauses: $clauseCode');
      print('Errors found: ${errors.length}');
      for (var error in errors) {
        print('  - ${error.message}');
      }

      final hasCoverageError = errors.any((e) =>
        e.message.contains('coverage') || e.message.contains('Coverage'));

      expect(hasCoverageError, isFalse,
        reason: '::< types should not require mode coverage');
    });
  });
}
