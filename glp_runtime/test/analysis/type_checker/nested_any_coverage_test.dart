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

    test('NEGATIVE: InvList with wrong variable placement fails', () {
      // InvList ::= [] ; [_? | InvList] - elements are _? (input mode)
      // procedure fill(InvList?, _) - arg1 is input, arg2 is output
      // At input position (InvList?), clause head needs writers
      // But [Slot? | Rest] has reader Slot? - WRONG
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

      print('\n=== Test: NEGATIVE InvList wrong placement ===');
      print('Errors found: ${errors.length}');
      for (var error in errors) {
        print('  - ${error.message}');
      }

      // Should FAIL - reader Slot? at input position is wrong
      expect(errors.isNotEmpty, isTrue,
        reason: 'Reader at input position should fail');
    });

    test('POSITIVE: InvList with correct variable placement passes', () {
      // InvList ::= [] ; [_? | InvList] - elements are _? (input mode)
      // procedure fill(InvList?, _?) - arg1 is input (list), arg2 is input (value)
      // At input position, clause head needs writers
      // [Slot | Rest] has writer Slot - CORRECT for list
      // Val is writer - CORRECT for value
      // Body: Slot = Val? reads Val and writes to Slot
      final typeDecl = '''
InvList ::= [] ; [_? | InvList].
procedure fill(InvList?, _?).
''';
      final clauseCode = '''
fill([], _).
fill([Slot | Rest], Val) :- Slot = Val?, fill(Rest?, Val?).
''';

      final clauses = parseClauses(clauseCode);
      final typeEnv = parseTypes(typeDecl);
      final checker = ModeChecker(typeEnv);
      final errors = checker.checkProcedure('fill', 2, clauses);

      print('\n=== Test: POSITIVE InvList correct placement ===');
      print('Errors found: ${errors.length}');
      for (var error in errors) {
        print('  - ${error.message}');
      }

      // Should PASS - correct variable placement
      expect(errors, isEmpty,
        reason: 'Correct variable placement should pass');
    });
  });
}
