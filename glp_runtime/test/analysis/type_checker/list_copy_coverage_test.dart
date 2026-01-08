// test/analysis/type_checker/list_copy_coverage_test.dart
//
// Tests for mode coverage checking with bi-mode types

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
  group('Mode coverage for bi-mode types', () {

    test('BiMode INPUT arg - single writer clause, incomplete coverage FAILS', () {
      // BiMode ::= _ ; _? requires BOTH modes covered
      // Declared INPUT (BiMode?) → callee sees OUTPUT → expects WRITER
      final typeDecl = '''
BiMode ::= _ ; _?.
procedure test(BiMode?).
''';
      final clauseCode = 'test(X).';  // Only writer, missing reader

      final clauses = parseClauses(clauseCode);
      final typeEnv = parseTypes(typeDecl);
      final checker = ModeChecker(typeEnv);
      final errors = checker.checkProcedure('test', 1, clauses);

      print('\n=== Test: Single writer clause ===');
      print('Type: $typeDecl');
      print('Clauses: $clauseCode');
      print('Errors found: ${errors.length}');
      for (var error in errors) {
        print('  - ${error.message}');
      }

      final hasCoverageError = errors.any((e) =>
        e.message.contains('coverage') || e.message.contains('Coverage'));

      expect(hasCoverageError, isTrue,
        reason: 'BiMode requires both modes, only writer provided');
    });

    test('BiMode INPUT arg - two clauses covering both modes PASSES', () {
      // Declared INPUT (BiMode?) → callee sees OUTPUT
      // Need both writer X and reader X? across clauses
      final typeDecl = '''
BiMode ::= _ ; _?.
procedure test(BiMode?).
''';
      final clauseCode = '''
test(X).
test(X?).
''';

      final clauses = parseClauses(clauseCode);
      final typeEnv = parseTypes(typeDecl);
      final checker = ModeChecker(typeEnv);
      final errors = checker.checkProcedure('test', 1, clauses);

      print('\n=== Test: Two clauses covering both modes ===');
      print('Type: $typeDecl');
      print('Clauses: $clauseCode');
      print('Errors found: ${errors.length}');
      for (var error in errors) {
        print('  - ${error.message}');
      }

      final hasCoverageError = errors.any((e) =>
        e.message.contains('coverage') || e.message.contains('Coverage'));

      expect(hasCoverageError, isFalse,
        reason: 'Both writer and reader modes covered');
    });

    test('Custom universal type requires coverage', () {
      // Universal ::= _ ; _? same as BiMode
      final typeDecl = '''
Universal ::= _ ; _?.
procedure test(Universal?).
''';
      final clauseCode = 'test(X).';  // Only writer

      final clauses = parseClauses(clauseCode);
      final typeEnv = parseTypes(typeDecl);
      final checker = ModeChecker(typeEnv);
      final errors = checker.checkProcedure('test', 1, clauses);

      print('\n=== Test: Custom universal type ===');
      print('Type decl: $typeDecl');
      print('Clauses: $clauseCode');
      print('Errors found: ${errors.length}');
      for (var error in errors) {
        print('  - ${error.message}');
      }

      final hasCoverageError = errors.any((e) =>
        e.message.contains('coverage') || e.message.contains('Coverage'));

      expect(hasCoverageError, isTrue,
        reason: 'Custom universal type should require coverage');
    });

    test('Single-mode type does not require coverage', () {
      // OutputOnly ::= _ means only output mode, no coverage required
      // Declared OUTPUT (OutputOnly) → callee sees INPUT → expects READER
      final typeDecl = '''
OutputOnly ::= _.
procedure test(OutputOnly).
''';
      final clauseCode = 'test(X?).';  // Reader at callee INPUT position

      final clauses = parseClauses(clauseCode);
      final typeEnv = parseTypes(typeDecl);
      final checker = ModeChecker(typeEnv);
      final errors = checker.checkProcedure('test', 1, clauses);

      print('\n=== Test: Single-mode type ===');
      print('Type decl: $typeDecl');
      print('Clauses: $clauseCode');
      print('Errors found: ${errors.length}');
      for (var error in errors) {
        print('  - ${error.message}');
      }

      final hasCoverageError = errors.any((e) =>
        e.message.contains('coverage') || e.message.contains('Coverage'));

      expect(hasCoverageError, isFalse,
        reason: 'Single-mode types should not require multi-mode coverage');
    });
  });
}
