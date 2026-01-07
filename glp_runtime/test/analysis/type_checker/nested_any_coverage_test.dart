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
  group('Nested Bimodal coverage in List', () {

    test('BimodalList copy single clause - should FAIL (missing writer at head)', () {
      // BimodalList has Bimodal at head position, requiring coverage
      final typeDecl = '''
Bimodal ::= _ ; _?.
BimodalList ::= [] ; [Bimodal | BimodalList].
procedure copy(BimodalList?, BimodalList).
''';
      final clauseCode = '''
copy([], []).
copy([H? | In], [H | Out?]) :- copy(In?, Out).
''';

      final clauses = parseClauses(clauseCode);
      final typeEnv = parseTypes(typeDecl);
      final checker = ModeChecker(typeEnv);
      final errors = checker.checkProcedure('copy', 2, clauses);

      print('\n=== Test: BimodalList copy single clause ===');
      print('Errors found: ${errors.length}');
      for (var error in errors) {
        print('  - ${error.message}');
      }

      // Should detect missing writer mode at head position
      final hasCoverageError = errors.any((e) =>
        e.message.contains('coverage') || e.message.contains('Coverage'));

      expect(hasCoverageError, isTrue,
        reason: 'Should detect missing writer mode at BimodalList head position');
    });

    test('BimodalList copy two clauses - should PASS (both modes covered)', () {
      final typeDecl = '''
Bimodal ::= _ ; _?.
BimodalList ::= [] ; [Bimodal | BimodalList].
procedure copy(BimodalList?, BimodalList).
''';
      final clauseCode = '''
copy([], []).
copy([H? | In], [H | Out?]) :- copy(In?, Out).
copy([H | In], [H? | Out?]) :- copy(In?, Out).
''';

      final clauses = parseClauses(clauseCode);
      final typeEnv = parseTypes(typeDecl);
      final checker = ModeChecker(typeEnv);
      final errors = checker.checkProcedure('copy', 2, clauses);

      print('\n=== Test: BimodalList copy two clauses ===');
      print('Errors found: ${errors.length}');
      for (var error in errors) {
        print('  - ${error.message}');
      }

      // Should pass - both modes covered at head position
      final hasCoverageError = errors.any((e) =>
        e.message.contains('coverage') || e.message.contains('Coverage'));

      expect(hasCoverageError, isFalse,
        reason: 'Both reader and writer modes covered at head position');
    });

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

    test('List with ::< should skip nested coverage', () {
      // PartialList is subtype, no coverage required
      final typeDecl = '''
Bimodal ::= _ ; _?.
PartialList ::< [] ; [Bimodal | PartialList].
procedure process(PartialList?, PartialList).
''';
      final clauseCode = '''
process([], []).
process([H? | In], [H | Out?]) :- process(In?, Out).
''';

      final clauses = parseClauses(clauseCode);
      final typeEnv = parseTypes(typeDecl);
      final checker = ModeChecker(typeEnv);
      final errors = checker.checkProcedure('process', 2, clauses);

      print('\n=== Test: ::< PartialList ===');
      print('Errors found: ${errors.length}');
      for (var error in errors) {
        print('  - ${error.message}');
      }

      // Should pass - ::< types skip coverage
      final hasCoverageError = errors.any((e) =>
        e.message.contains('coverage') || e.message.contains('Coverage'));

      expect(hasCoverageError, isFalse,
        reason: '::< types should not require mode coverage');
    });
  });
}
