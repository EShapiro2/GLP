// test/analysis/type_checker/list_copy_coverage_test.dart
//
// Tests for mode coverage checking with Every type

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
  group('Mode coverage for Every type', () {

    test('Every INPUT arg - single writer clause, incomplete coverage FAILS', () {
      // Every ::= _ ; _? requires BOTH modes covered
      // Declared INPUT (Every?) → callee sees OUTPUT → expects WRITER
      final typeDecl = 'procedure test(Every?).';
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
        reason: 'Every requires both modes, only writer provided');
    });

    test('Every INPUT arg - two clauses covering both modes PASSES', () {
      // Declared INPUT (Every?) → callee sees OUTPUT
      // Need both writer X and reader X? across clauses
      final typeDecl = 'procedure test(Every?).';
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
      // Universal ::= _ ; _? same as Every
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
