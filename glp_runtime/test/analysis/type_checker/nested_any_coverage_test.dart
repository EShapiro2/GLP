// test/analysis/type_checker/nested_any_coverage_test.dart
//
// Tests for mode coverage checking with nested primitive positions in List

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

    test('List copy with output-only head - should PASS', () {
      final typeDecl = '''
List1 ::= [] ; [_ | List1].
procedure copy(List1?, List1).
''';
      final clauseCode = '''
copy([], []).
copy([H | In], [H? | Out?]) :- copy(In?, Out).
''';

      final clauses = parseClauses(clauseCode);
      final typeEnv = parseTypes(typeDecl);
      final checker = ModeChecker(typeEnv);
      final errors = checker.checkProcedure('copy', 2, clauses);

      print('\n=== Test: List1 with output-only head ===');
      print('Errors found: ${errors.length}');
      for (var error in errors) {
        print('  - ${error.message}');
      }

      // Should pass - List1 head is _ only (single mode)
      final hasCoverageError = errors.any((e) =>
        e.message.contains('coverage'));

      expect(hasCoverageError, isFalse,
        reason: 'List1 with single mode head should not require full coverage');
    });

    test('List with input-only head - should PASS', () {
      final typeDecl = '''
InvList ::= [] ; [_? | InvList].
procedure fill(InvList?, _).
''';
      final clauseCode = '''
fill([], _).
fill([Slot? | Rest], Val) :- Slot = Val?, fill(Rest?, Val?).
''';

      final clauses = parseClauses(clauseCode);
      final typeEnv = parseTypes(typeDecl);
      final checker = ModeChecker(typeEnv);
      final errors = checker.checkProcedure('fill', 2, clauses);

      print('\n=== Test: InvList with input-only head ===');
      print('Errors found: ${errors.length}');
      for (var error in errors) {
        print('  - ${error.message}');
      }

      // Should pass - InvList head is _? only (single mode)
      final hasCoverageError = errors.any((e) =>
        e.message.contains('coverage'));

      expect(hasCoverageError, isFalse,
        reason: 'InvList with single mode head should not require full coverage');
    });
  });
}
