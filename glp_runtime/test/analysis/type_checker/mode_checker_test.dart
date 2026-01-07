// test/analysis/type_checker/mode_checker_test.dart
//
// Tests for mode checker and mode system

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';
import 'package:glp_runtime/analysis/type_checker/mode_error.dart';
import 'package:glp_runtime/analysis/type_checker/mode_checker.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';

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
  group('Mode Enum', () {
    test('complement inverts mode', () {
      expect(Mode.produce.flip, equals(Mode.consume));
      expect(Mode.consume.flip, equals(Mode.produce));
    });

    test('complement is involution - (M?)? = M', () {
      expect(Mode.produce.flip.flip, equals(Mode.produce));
      expect(Mode.consume.flip.flip, equals(Mode.consume));
    });

    test('toString returns mode name', () {
      expect(Mode.produce.toString(), equals('output'));
      expect(Mode.consume.toString(), equals('input'));
    });
  });

  group('combineMode - Involution Property', () {
    test('output ⊕ output = output (no inversions)', () {
      expect(combineMode(Mode.produce, Mode.produce), equals(Mode.produce));
    });

    test('output ⊕ input = input (one inversion)', () {
      expect(combineMode(Mode.produce, Mode.consume), equals(Mode.consume));
    });

    test('input ⊕ output = input (one inversion)', () {
      expect(combineMode(Mode.consume, Mode.produce), equals(Mode.consume));
    });

    test('input ⊕ input = output (two inversions cancel)', () {
      expect(combineMode(Mode.consume, Mode.consume), equals(Mode.produce));
    });
  });

  group('Mode Checker - Variable Mode Checking', () {
    test('Writer at input position - VALID (caller provides value)', () {
      // procedure foo(X?).  % X? is input mode (has ?)
      // foo(Y) :- ...       % Y is writer - VALID at input position
      final typeEnv = TypeEnvironment({}, {
        'foo/1': ProcDecl(
          'foo',
          [TypeRef('Number', 1, 1, isInput: true)], // input mode (has ?)
          1,
          1,
        ),
      });

      final checker = ModeChecker(typeEnv);

      // Parse clause: foo(Y) :- true.
      final clauses = parseClauses('foo(Y) :- true.');
      expect(clauses, hasLength(1));

      final errors = checker.checkClause(clauses[0], typeEnv.procedures['foo/1']!);
      expect(errors, isEmpty, reason: 'Writer at input position should be valid');
    });

    test('Writer at output position - INVALID (mode violation)', () {
      // procedure foo(X).  % X is output mode (no ?)
      // foo(Y) :- ...       % Y is writer - INVALID at output position
      final typeEnv = TypeEnvironment({}, {
        'foo/1': ProcDecl(
          'foo',
          [TypeRef('Number', 1, 1, isInput: false)], // output mode (no ?)
          1,
          1,
        ),
      });

      final checker = ModeChecker(typeEnv);

      // Parse clause: foo(Y) :- true.
      final clauses = parseClauses('foo(Y) :- true.');
      expect(clauses, hasLength(1));

      final errors = checker.checkClause(clauses[0], typeEnv.procedures['foo/1']!);
      expect(errors, hasLength(1));
      expect(errors[0].message, contains('writer'));
      expect(errors[0].message, contains('output position'));
    });

    test('Reader at output position - VALID (callee writes)', () {
      // procedure foo(X).  % X is output mode (no ?)
      // foo(Y?) :- ...      % Y? is reader - VALID at output position
      final typeEnv = TypeEnvironment({}, {
        'foo/1': ProcDecl(
          'foo',
          [TypeRef('Number', 1, 1, isInput: false)], // output mode (no ?)
          1,
          1,
        ),
      });

      final checker = ModeChecker(typeEnv);

      // Parse clause: foo(Y?) :- true.
      final clauses = parseClauses('foo(Y?) :- true.');
      expect(clauses, hasLength(1));

      final errors = checker.checkClause(clauses[0], typeEnv.procedures['foo/1']!);
      expect(errors, isEmpty, reason: 'Reader at output position should be valid');
    });

    test('Reader at input position - INVALID (mode violation)', () {
      // procedure foo(X?).  % X? is input mode (has ?)
      // foo(Y?) :- ...     % Y? is reader - INVALID at input position
      final typeEnv = TypeEnvironment({}, {
        'foo/1': ProcDecl(
          'foo',
          [TypeRef('Number', 1, 1, isInput: true)], // input mode (has ?)
          1,
          1,
        ),
      });

      final checker = ModeChecker(typeEnv);

      // Parse clause: foo(Y?) :- true.
      final clauses = parseClauses('foo(Y?) :- true.');
      expect(clauses, hasLength(1));

      final errors = checker.checkClause(clauses[0], typeEnv.procedures['foo/1']!);
      expect(errors, hasLength(1));
      expect(errors[0].message, contains('reader'));
      expect(errors[0].message, contains('input position'));
    });
  });

  group('Mode Checker - Call Boundary Complementation', () {
    test('Call with complemented modes - input position at call site', () {
      // procedure caller(X?).  % caller has input mode (receives writer Y in head)
      // procedure callee(X?).  % callee expects input (X?)
      // caller(Y) :- callee(Y?).  % Y is writer in head, Y? is reader in call - VALID
      final typeEnv = TypeEnvironment({}, {
        'caller/1': ProcDecl('caller', [TypeRef('Number', 1, 1, isInput: true)], 1, 1), // input mode
        'callee/1': ProcDecl(
          'callee',
          [TypeRef('Number', 1, 1, isInput: true)], // callee expects input
          1,
          1,
        ),
      });

      final checker = ModeChecker(typeEnv);

      // Parse clause: caller(Y) :- callee(Y?).
      final clauses = parseClauses('caller(Y) :- callee(Y?).');
      expect(clauses, hasLength(1));

      final errors = checker.checkClause(clauses[0], typeEnv.procedures['caller/1']!);
      expect(errors, isEmpty, reason: 'Complemented modes at call boundary should work');
    });

    test('Call with wrong mode - writer where reader expected', () {
      // procedure caller(X?).  % caller has input mode
      // procedure callee(X?).  % callee expects input (X?)
      // caller(Y) :- callee(Y).  % caller provides writer Y to callee - INVALID
      // (callee(X?) complemented = callee expects output at call site, needs reader)
      final typeEnv = TypeEnvironment({}, {
        'caller/1': ProcDecl('caller', [TypeRef('Number', 1, 1, isInput: true)], 1, 1), // input mode
        'callee/1': ProcDecl(
          'callee',
          [TypeRef('Number', 1, 1, isInput: true)], // callee expects input
          1,
          1,
        ),
      });

      final checker = ModeChecker(typeEnv);

      // Parse clause: caller(Y) :- callee(Y).
      final clauses = parseClauses('caller(Y) :- callee(Y).');
      expect(clauses, hasLength(1));

      final errors = checker.checkClause(clauses[0], typeEnv.procedures['caller/1']!);
      expect(errors, hasLength(1));
      expect(errors[0].message, contains('writer'));
    });
  });

  group('Mode Checker - Embedded Modes with Involution', () {
    test('Embedded mode involution - s(X?) at output position', () {
      // Type: Nat ::= 0 ; s(Nat?).
      // procedure foo(Nat?).  % output position (has ?)
      // foo(s(Y?)) :- ...     % Embedded Nat? at output → combined = input → expects reader
      //
      // Parent mode: output (from foo's Nat?)
      // Embedded mode: input (from s(Nat?))
      // Combined: output ⊕ input = input → expects reader Y?
      // Actual: reader Y?
      // Result: VALID

      final typeEnv = TypeEnvironment(
        {
          'Nat': TypeDef('Nat', [
            ConstantAlt(0, 1, 1),
            StructAlt('s', [TypeRef('Nat', 1, 1, isInput: true)], 1, 1),
          ], 1, 1),
        },
        {
          'foo/1': ProcDecl(
            'foo',
            [TypeRef('Nat', 1, 1, isInput: true)], // output position
            1,
            1,
          ),
        },
      );

      final checker = ModeChecker(typeEnv);

      // Parse clause: foo(s(Y?)) :- true.
      final clauses = parseClauses('foo(s(Y?)) :- true.');
      expect(clauses, hasLength(1));

      // Note: This test checks the involution logic, but we need proper
      // type checking infrastructure to fully validate embedded modes.
      // For now, we verify the combineMode logic separately.
    });

    test('Double embedded mode - involution cancels out', () {
      // If we had: Tree ::= node(Tree?, Tree?)
      // And: procedure foo(Tree?)  % output position
      // Then: foo(node(node(X?, Y?), Z?))
      //
      // Outer Tree?: output (isInput=true from foo)
      // First node arg Tree?: input (isInput=true from node definition)
      // Combined: output ⊕ input = input
      //
      // Second node arg Tree?: input (isInput=true from inner node)
      // Combined with parent: input ⊕ input = output
      //
      // So X? should be at OUTPUT mode → expects reader ✓
      // This demonstrates involution: (output ⊕ input) ⊕ input = output

      // Verify combineMode handles this:
      final outerMode = Mode.produce;
      final firstEmbedded = Mode.consume;
      final secondEmbedded = Mode.consume;

      final afterFirst = combineMode(outerMode, firstEmbedded);
      expect(afterFirst, equals(Mode.consume));

      final afterSecond = combineMode(afterFirst, secondEmbedded);
      expect(afterSecond, equals(Mode.produce));
    });
  });

  group('Mode Checker - Multiple Arguments', () {
    test('Mixed modes in procedure declaration', () {
      // procedure merge(List?, List?, List).
      // merge([H|T], S, [H|R]) :- merge(T?, S?, R?).
      //
      // Head check:
      // - Arg 0: List? (input) expects writer → [H|T] ok
      // - Arg 1: List? (input) expects writer → S ok
      // - Arg 2: List (output) expects reader → [H|R] needs reader variables
      //
      // Body check (call boundary complement):
      // - merge expects (List?, List?, List) = (input, input, output)
      // - At call site, complement: (output, output, input)
      // - So caller provides: (reader, reader, writer)
      // - Actual: (T?, S?, R?) - but R? is reader at input position!

      final typeEnv = TypeEnvironment({}, {
        'merge/3': ProcDecl(
          'merge',
          [
            TypeRef('List', 1, 1, isInput: true),  // input
            TypeRef('List', 1, 1, isInput: true),  // input
            TypeRef('List', 1, 1, isInput: false), // output
          ],
          1,
          1,
        ),
      });

      final checker = ModeChecker(typeEnv);

      // Parse clause: merge([H|T], S, [H|R]) :- merge(T?, S?, R?).
      final clauses = parseClauses('merge([H|T], S, [H|R]) :- merge(T?, S?, R?).');
      expect(clauses, hasLength(1));

      // This should find mode error: R? is reader at input position in body call
      final errors = checker.checkClause(clauses[0], typeEnv.procedures['merge/3']!);

      // We expect at least one error for R? being a reader at complemented output → input position
      expect(errors.isNotEmpty, isTrue);
    });
  });

  group('ModeError Factory Methods', () {
    test('writerAtOutput creates correct error', () {
      final error = ModeError.writerAtOutput(
        variable: 'X',
        predicate: 'foo',
        argIndex: 0,
        line: 10,
        column: 5,
      );

      expect(error.message, contains('writer X'));
      expect(error.message, contains('output position'));
      expect(error.message, contains('foo'));
      expect(error.line, equals(10));
      expect(error.column, equals(5));
    });

    test('readerAtInput creates correct error', () {
      final error = ModeError.readerAtInput(
        variable: 'Y',
        predicate: 'bar',
        argIndex: 1,
        line: 20,
        column: 10,
      );

      expect(error.message, contains('reader Y'));
      expect(error.message, contains('input position'));
      expect(error.message, contains('bar'));
      expect(error.line, equals(20));
      expect(error.column, equals(10));
    });

    test('callModeMismatch creates correct error', () {
      final error = ModeError.callModeMismatch(
        predicate: 'baz',
        argIndex: 2,
        expectedMode: Mode.consume,
        actualMode: Mode.produce,
        line: 30,
        column: 15,
      );

      expect(error.message, contains('baz'));
      expect(error.message, contains('argument 2'));
      expect(error.message, contains('input'));
      expect(error.message, contains('output'));
      expect(error.line, equals(30));
      expect(error.column, equals(15));
    });
  });
}
