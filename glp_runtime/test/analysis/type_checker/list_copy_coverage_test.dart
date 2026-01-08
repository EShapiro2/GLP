// test/analysis/type_checker/list_copy_coverage_test.dart
//
// Tests for mode checking with single-mode types.
//
// Note: Types like `BiMode ::= _ ; _?` are ILLEGAL because they create
// non-deterministic transitions (NFA instead of DFA). Each position
// has exactly ONE mode. This file tests valid single-mode types.

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
  group('Mode checking for single-mode types', () {

    test('Output type accepts writer (no coverage requirement)', () {
      // OutputOnly ::= _ means only output mode
      // procedure test(OutputOnly) → callee position is output → expects writer
      final typeDecl = '''
OutputOnly ::= _.
procedure test(OutputOnly).
''';
      final clauseCode = 'test(X).';  // Writer at output position

      final clauses = parseClauses(clauseCode);
      final typeEnv = parseTypes(typeDecl);
      final checker = ModeChecker(typeEnv);
      final errors = checker.checkProcedure('test', 1, clauses);

      print('\n=== Test: Output type with writer ===');
      print('Type: $typeDecl');
      print('Clauses: $clauseCode');
      print('Errors found: ${errors.length}');
      for (var error in errors) {
        print('  - ${error.message}');
      }

      expect(errors, isEmpty,
        reason: 'OutputOnly accepts writer, single-mode no coverage requirement');
    });

    test('Input type accepts reader (no coverage requirement)', () {
      // InputOnly ::= _? means only input mode
      // procedure test(InputOnly) → callee position is input → expects reader
      final typeDecl = '''
InputOnly ::= _?.
procedure test(InputOnly).
''';
      final clauseCode = 'test(X?).';  // Reader at input position

      final clauses = parseClauses(clauseCode);
      final typeEnv = parseTypes(typeDecl);
      final checker = ModeChecker(typeEnv);
      final errors = checker.checkProcedure('test', 1, clauses);

      print('\n=== Test: Input type with reader ===');
      print('Type: $typeDecl');
      print('Clauses: $clauseCode');
      print('Errors found: ${errors.length}');
      for (var error in errors) {
        print('  - ${error.message}');
      }

      expect(errors, isEmpty,
        reason: 'InputOnly accepts reader, single-mode no coverage requirement');
    });

    test('Output type rejects reader (mode mismatch)', () {
      // OutputOnly ::= _ means only output mode
      // procedure test(OutputOnly) → callee position is output → expects writer, not reader
      final typeDecl = '''
OutputOnly ::= _.
procedure test(OutputOnly).
''';
      final clauseCode = 'test(X?).';  // Reader at output position - WRONG

      final clauses = parseClauses(clauseCode);
      final typeEnv = parseTypes(typeDecl);
      final checker = ModeChecker(typeEnv);
      final errors = checker.checkProcedure('test', 1, clauses);

      print('\n=== Test: Output type with reader (should fail) ===');
      print('Type: $typeDecl');
      print('Clauses: $clauseCode');
      print('Errors found: ${errors.length}');
      for (var error in errors) {
        print('  - ${error.message}');
      }

      expect(errors, isNotEmpty,
        reason: 'OutputOnly requires writer, reader is mode mismatch');
    });

    test('Input type rejects writer (mode mismatch)', () {
      // InputOnly ::= _? means only input mode
      // procedure test(InputOnly) → callee position is input → expects reader, not writer
      final typeDecl = '''
InputOnly ::= _?.
procedure test(InputOnly).
''';
      final clauseCode = 'test(X).';  // Writer at input position - WRONG

      final clauses = parseClauses(clauseCode);
      final typeEnv = parseTypes(typeDecl);
      final checker = ModeChecker(typeEnv);
      final errors = checker.checkProcedure('test', 1, clauses);

      print('\n=== Test: Input type with writer (should fail) ===');
      print('Type: $typeDecl');
      print('Clauses: $clauseCode');
      print('Errors found: ${errors.length}');
      for (var error in errors) {
        print('  - ${error.message}');
      }

      expect(errors, isNotEmpty,
        reason: 'InputOnly requires reader, writer is mode mismatch');
    });

    test('List with output head accepts writer elements', () {
      // OutputList ::= [] ; [_ | OutputList] - head is output mode
      final typeDecl = '''
OutputList ::= [] ; [_ | OutputList].
procedure copy(OutputList).
''';
      final clauseCode = '''
copy([]).
copy([H | T]) :- copy(T?).
''';

      final clauses = parseClauses(clauseCode);
      final typeEnv = parseTypes(typeDecl);
      final checker = ModeChecker(typeEnv);
      final errors = checker.checkProcedure('copy', 1, clauses);

      print('\n=== Test: OutputList with writer head ===');
      print('Type: $typeDecl');
      print('Clauses: $clauseCode');
      print('Errors found: ${errors.length}');
      for (var error in errors) {
        print('  - ${error.message}');
      }

      expect(errors, isEmpty,
        reason: 'OutputList head is _ so writer H is accepted');
    });

    test('List with input head accepts reader elements', () {
      // InputList ::= [] ; [_? | InputList] - head is input mode
      final typeDecl = '''
InputList ::= [] ; [_? | InputList].
procedure consume(InputList).
''';
      final clauseCode = '''
consume([]).
consume([H? | T]) :- consume(T?).
''';

      final clauses = parseClauses(clauseCode);
      final typeEnv = parseTypes(typeDecl);
      final checker = ModeChecker(typeEnv);
      final errors = checker.checkProcedure('consume', 1, clauses);

      print('\n=== Test: InputList with reader head ===');
      print('Type: $typeDecl');
      print('Clauses: $clauseCode');
      print('Errors found: ${errors.length}');
      for (var error in errors) {
        print('  - ${error.message}');
      }

      expect(errors, isEmpty,
        reason: 'InputList head is _? so reader H? is accepted');
    });
  });
}
