// test/analysis/type_checker/list_copy_coverage_test.dart
//
// Tests for mode coverage checking with direct Any arguments

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
  group('Direct Any argument mode coverage', () {

    test('procedure with Every argument - single reader clause should FAIL', () {
      final typeDecl = 'procedure echo(Every?, Every).';
      final clauseCode = '''
echo(X?, Y).
''';

      final clauses = parseClauses(clauseCode);
      final typeEnv = parseTypes(typeDecl);
      final checker = ModeChecker(typeEnv);
      final errors = checker.checkProcedure('echo', 2, clauses);

      print('\n=== Test: Single reader clause ===');
      print('Type: $typeDecl');
      print('Clauses: $clauseCode');
      print('Errors found: ${errors.length}');
      for (var error in errors) {
        print('  - ${error.message}');
      }

      // Should have mode coverage error - only covers reader mode for arg 1
      final hasCoverageError = errors.any((e) =>
        e.message.contains('coverage') || e.message.contains('Coverage'));

      expect(hasCoverageError, isTrue,
        reason: 'Should detect missing writer mode for Every? argument 1');
    });

    test('procedure with Every argument - two clauses covering both modes should PASS', () {
      final typeDecl = 'procedure echo(Every?, Every).';
      final clauseCode = '''
echo(X?, Y).
echo(X, Y?).
''';

      final clauses = parseClauses(clauseCode);
      final typeEnv = parseTypes(typeDecl);
      final checker = ModeChecker(typeEnv);
      final errors = checker.checkProcedure('echo', 2, clauses);

      print('\n=== Test: Two clauses covering both modes ===');
      print('Type: $typeDecl');
      print('Clauses: $clauseCode');
      print('Errors found: ${errors.length}');
      for (var error in errors) {
        print('  - ${error.message}');
      }

      // Should pass - both modes covered
      final hasCoverageError = errors.any((e) =>
        e.message.contains('coverage') || e.message.contains('Coverage'));

      expect(hasCoverageError, isFalse,
        reason: 'Both reader and writer modes covered');
    });

    test('procedure with custom universal type - should require coverage', () {
      final typeDecl = '''
Universal ::= _ ; _?.
procedure transform(Universal?, Universal).
''';
      final clauseCode = '''
transform(X?, Y).
''';

      final clauses = parseClauses(clauseCode);
      final typeEnv = parseTypes(typeDecl);
      final checker = ModeChecker(typeEnv);
      final errors = checker.checkProcedure('transform', 2, clauses);

      print('\n=== Test: Custom universal type ===');
      print('Type decl: $typeDecl');
      print('Clauses: $clauseCode');
      print('Errors found: ${errors.length}');
      for (var error in errors) {
        print('  - ${error.message}');
      }

      // Should fail - missing writer mode
      final hasCoverageError = errors.any((e) =>
        e.message.contains('coverage') || e.message.contains('Coverage'));

      expect(hasCoverageError, isTrue,
        reason: 'Custom universal type should require coverage');
    });

    test('subtype declaration should skip coverage check', () {
      final typeDecl = '''
Partial ::< _.
procedure test(Partial).
''';
      final clauseCode = '''
test(X).
''';

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

      // Should pass - ::< types don't require coverage
      final hasCoverageError = errors.any((e) =>
        e.message.contains('coverage') || e.message.contains('Coverage'));

      expect(hasCoverageError, isFalse,
        reason: '::< types should not require mode coverage');
    });
  });
}
