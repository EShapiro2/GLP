import 'package:test/test.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/pmt/type_table.dart';
import 'package:glp_runtime/compiler/pmt/mode_table.dart';
import 'package:glp_runtime/compiler/pmt/type_checker.dart';

void main() {
  group('TypeChecker', () {
    Module parseModule(String source) {
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      return parser.parseModule();
    }

    TypeChecker createChecker(Module module) {
      final typeTable = TypeTable.fromModule(module);
      final modeTable = ModeTable.fromDeclarations(module.modeDeclarations);
      return TypeChecker(typeTable, modeTable);
    }

    group('constant type checking', () {
      test('valid constant matches type', () {
        final source = '''
BinaryDigit := zero | one.
List(A) := [] | [A | List(A)].
And := and(List(BinaryDigit)?, List(BinaryDigit)?, List(BinaryDigit)).
and([one|Xs], [one|Ys], [one|Zs?]) :- and(Xs?, Ys?, Zs).
''';
        final module = parseModule(source);
        final checker = createChecker(module);

        final errors = checker.checkModule(module);
        expect(errors, isEmpty);
      });

      test('invalid constant fails type check', () {
        final source = '''
BinaryDigit := zero | one.
List(A) := [] | [A | List(A)].
And := and(List(BinaryDigit)?, List(BinaryDigit)?, List(BinaryDigit)).
and([two|Xs], [one|Ys], [one|Zs?]) :- and(Xs?, Ys?, Zs).
''';
        final module = parseModule(source);
        final checker = createChecker(module);

        final errors = checker.checkModule(module);
        expect(errors, isNotEmpty);
        expect(errors.first.message, contains('two'));
        expect(errors.first.message, contains('BinaryDigit'));
      });

      test('true is valid Goals atom', () {
        final source = '''
Gates := And.
Goals := true | Gates.
And := and(List?, List?, List).
Reduce := reduce(Gates?, Goals).
reduce(and([], [], []), true).
''';
        final module = parseModule(source);
        final checker = createChecker(module);

        final errors = checker.checkModule(module);
        expect(errors, isEmpty);
      });
    });

    group('struct type checking', () {
      test('valid struct matches type reference', () {
        final source = '''
Gates := And | Or.
Goals := true | Gates.
And := and(List?, List?, List).
Or := or(List?, List?, List).
Reduce := reduce(Gates?, Goals).
reduce(and([], [], []), true).
''';
        final module = parseModule(source);
        final checker = createChecker(module);

        final errors = checker.checkModule(module);
        expect(errors, isEmpty);
      });

      test('invalid struct fails type check', () {
        final source = '''
Gates := And | Or.
And := and(List?, List?, List).
Or := or(List?, List?, List).
Reduce := reduce(Gates?, Gates).
reduce(foo([], [], []), bar).
''';
        final module = parseModule(source);
        final checker = createChecker(module);

        final errors = checker.checkModule(module);
        expect(errors, isNotEmpty);
        expect(errors.first.message, contains('foo'));
      });

      test('struct in body is checked', () {
        final source = '''
Gates := And.
And := and(List?, List?, List).
Reduce := reduce(Gates?, Gates).
reduce(and([X|Xs], [Y|Ys], [Z|Zs?]), and(Xs?, Ys?, Zs)).
''';
        final module = parseModule(source);
        final checker = createChecker(module);

        final errors = checker.checkModule(module);
        expect(errors, isEmpty);
      });
    });

    group('list type checking', () {
      test('empty list is valid List', () {
        final source = '''
And := and(List?, List?, List).
and([], [], []).
''';
        final module = parseModule(source);
        final checker = createChecker(module);

        final errors = checker.checkModule(module);
        expect(errors, isEmpty);
      });

      test('non-empty list is valid', () {
        final source = '''
And := and(List?, List?, List).
and([X|Xs], [Y|Ys], [Z|Zs?]) :- and(Xs?, Ys?, Zs).
''';
        final module = parseModule(source);
        final checker = createChecker(module);

        final errors = checker.checkModule(module);
        expect(errors, isEmpty);
      });

      test('list with typed elements', () {
        final source = '''
BinaryDigit := zero | one.
List(A) := [] | [A | List(A)].
And := and(List(BinaryDigit)?, List(BinaryDigit)?, List(BinaryDigit)).
and([], [], []).
and([zero|Xs], [one|Ys], [one|Zs?]) :- and(Xs?, Ys?, Zs).
''';
        final module = parseModule(source);
        final checker = createChecker(module);

        final errors = checker.checkModule(module);
        expect(errors, isEmpty);
      });
    });

    group('type reference resolution', () {
      test('type references are resolved', () {
        final source = '''
Gates := And | Or | Not.
And := and(List?, List?, List).
Or := or(List?, List?, List).
Not := not(List?, List).
Reduce := reduce(Gates?, Gates).
reduce(and([], [], []), and([], [], [])).
''';
        final module = parseModule(source);
        final checker = createChecker(module);

        final errors = checker.checkModule(module);
        expect(errors, isEmpty);
      });

      test('or is valid Gates', () {
        final source = '''
Gates := And | Or.
And := and(List?, List?, List).
Or := or(List?, List?, List).
Reduce := reduce(Gates?, Gates).
reduce(and([], [], []), or([], [], [])).
''';
        final module = parseModule(source);
        final checker = createChecker(module);

        final errors = checker.checkModule(module);
        expect(errors, isEmpty);
      });
    });

    group('variables', () {
      test('variables are not type-checked', () {
        final source = '''
BinaryDigit := zero | one.
Foo := foo(BinaryDigit?, BinaryDigit).
foo(X, X?).
''';
        final module = parseModule(source);
        final checker = createChecker(module);

        final errors = checker.checkModule(module);
        expect(errors, isEmpty);
      });
    });

    group('unknown types', () {
      test('unknown types are allowed', () {
        // If type is not defined, we allow any value (might be external)
        final source = '''
Foo := foo(Unknown?, Unknown).
foo(x, y).
''';
        final module = parseModule(source);
        final checker = createChecker(module);

        final errors = checker.checkModule(module);
        expect(errors, isEmpty);
      });
    });

    group('error messages', () {
      test('error includes line and column', () {
        final source = '''
BinaryDigit := zero | one.
List(A) := [] | [A | List(A)].
And := and(List(BinaryDigit)?, List(BinaryDigit)?, List(BinaryDigit)).
and([bad|Xs], [], []).
''';
        final module = parseModule(source);
        final checker = createChecker(module);

        final errors = checker.checkModule(module);
        expect(errors, isNotEmpty);
        expect(errors.first.line, greaterThan(0));
        expect(errors.first.column, greaterThan(0));
      });

      test('error includes suggestion', () {
        final source = '''
BinaryDigit := zero | one.
List(A) := [] | [A | List(A)].
And := and(List(BinaryDigit)?, List(BinaryDigit)?, List(BinaryDigit)).
and([bad|Xs], [], []).
''';
        final module = parseModule(source);
        final checker = createChecker(module);

        final errors = checker.checkModule(module);
        expect(errors, isNotEmpty);
        expect(errors.first.suggestion, isNotNull);
        expect(errors.first.suggestion, contains('zero'));
        expect(errors.first.suggestion, contains('one'));
      });
    });

    group('TypeError', () {
      test('toString formats correctly', () {
        final error = TypeError('test message', 5, 10);
        expect(error.toString(), contains('test message'));
        expect(error.toString(), contains('Line 5'));
        expect(error.toString(), contains('Column 10'));
      });

      test('toString includes suggestion when present', () {
        final error = TypeError('test message', 5, 10, suggestion: 'try this');
        expect(error.toString(), contains('Suggestion: try this'));
      });
    });
  });
}
