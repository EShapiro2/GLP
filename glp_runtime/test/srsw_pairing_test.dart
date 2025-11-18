/// Tests for SRSW exact pairing requirement
///
/// Per revised spec: Variables must occur as reader/writer PAIRS with
/// exactly one writer AND one reader per clause.
///
/// Exception: Ground guards allow multiple reader occurrences.

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/analyzer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/error.dart';

void main() {
  group('SRSW Pairing Validation', () {
    test('Reject clause with writer but no reader', () {
      // Clause: foo(X).
      // X has writer occurrence but no reader - violates exact pairing

      final source = '''
        foo(X).
      ''';

      expect(
        () => _parseAndAnalyze(source),
        throwsA(isA<CompileError>().having(
          (e) => e.toString(),
          'message',
          contains('SRSW'),
        )),
        reason: 'Should reject variable with writer but no reader',
      );
    });

    test('Reject clause with reader but no writer', () {
      // Clause: bar(X?) :- true.
      // X? has reader occurrence but no writer - violates exact pairing

      final source = '''
        bar(X?) :- true.
      ''';

      expect(
        () => _parseAndAnalyze(source),
        throwsA(isA<CompileError>().having(
          (e) => e.toString(),
          'message',
          contains('SRSW'),
        )),
        reason: 'Should reject variable with reader but no writer',
      );
    });

    test('Accept clause with complete writer/reader pair', () {
      // Clause: baz(X, Y) :- qux(X?, Y?).
      // Both X and Y have exactly one writer and one reader - valid

      final source = '''
        baz(X, Y) :- qux(X?, Y?).
      ''';

      expect(
        () => _parseAndAnalyze(source),
        returnsNormally,
        reason: 'Should accept variables with complete pairs',
      );
    });

    test('Reject multiple writers for same variable', () {
      // Clause: same(X, X).
      // X occurs twice as writer - violates SRSW

      final source = '''
        same(X, X).
      ''';

      expect(
        () => _parseAndAnalyze(source),
        throwsA(isA<CompileError>().having(
          (e) => e.toString(),
          'message',
          contains('SRSW'),
        )),
        reason: 'Should reject multiple writer occurrences',
      );
    });

    test('Reject multiple readers without ground guard', () {
      // Clause: test(X, Y) :- foo(X?, X?).
      // X? occurs twice as reader without ground guard - violates SRSW

      final source = '''
        test(X, Y) :- foo(X?, X?).
      ''';

      expect(
        () => _parseAndAnalyze(source),
        throwsA(isA<CompileError>().having(
          (e) => e.toString(),
          'message',
          contains('SRSW'),
        )),
        reason: 'Should reject multiple readers without ground guard',
      );
    });

    test('Accept multiple readers with ground guard', () {
      // Clause: partition([X|Xs], Pivot, [X?|Smaller], Larger) :-
      //           number(X?) | X? < Pivot?, ...
      // X? occurs multiple times but number(X?) guarantees ground - valid

      final source = '''
        partition([X|Xs], Pivot, [X?|Smaller], Larger) :-
          number(X?) | helper(X?, Xs?).
      ''';

      expect(
        () => _parseAndAnalyze(source),
        returnsNormally,
        reason: 'Should accept multiple readers with ground guard',
      );
    });

    test('Accept clause with ground guard exception', () {
      // ground(X?) guard allows multiple reader occurrences

      final source = '''
        test(X, Y) :- ground(X?) | foo(X?, X?).
      ''';

      expect(
        () => _parseAndAnalyze(source),
        returnsNormally,
        reason: 'ground/1 guard should allow multiple reader occurrences',
      );
    });

    test('Reject incomplete pairing in structure', () {
      // Clause: foo(f(X, Y)).
      // X and Y have writers in structure but no readers - invalid

      final source = '''
        foo(f(X, Y)).
      ''';

      expect(
        () => _parseAndAnalyze(source),
        throwsA(isA<CompileError>()),
        reason: 'Should reject variables in structure without readers',
      );
    });

    test('Accept complete pairing in head and body', () {
      // Clause: member(X, [X?|Tail]) :- true.
      // X has one writer in head, one reader in head - valid

      final source = '''
        member(X, [X?|Tail]) :- true.
      ''';

      expect(
        () => _parseAndAnalyze(source),
        returnsNormally,
        reason: 'Should accept variable with writer and reader in same clause',
      );
    });

    test('Reject variable used only in guard', () {
      // Clause: test(X) :- number(Y?) | foo(X?).
      // Y has only reader in guard, no writer - invalid

      final source = '''
        test(X) :- number(Y?) | foo(X?).
      ''';

      expect(
        () => _parseAndAnalyze(source),
        throwsA(isA<CompileError>()),
        reason: 'Should reject variable with only reader occurrence',
      );
    });

    test('Accept variables in all clause parts', () {
      // Test that pairing works across head, guard, and body

      final source = '''
        qsort([Pivot|Rest], Sorted) :-
          number(Pivot?) |
          partition(Rest?, Pivot?, Less, Greater),
          qsort(Less?, Sorted).
      ''';

      expect(
        () => _parseAndAnalyze(source),
        returnsNormally,
        reason: 'Should accept variables distributed across clause parts',
      );
    });
  });

  group('SRSW Pairing Edge Cases', () {
    test('Anonymous variables not allowed', () {
      // All _ have been eliminated from codebase
      // Compiler should reject them or treat as fresh variables

      final source = '''
        test(_, X) :- foo(X?).
      ''';

      // Depending on compiler implementation, either:
      // 1. _ rejected as syntax error, or
      // 2. _ treated as fresh variable and fails pairing check

      expect(
        () => _parseAndAnalyze(source),
        throwsA(anything),
        reason: 'Anonymous variables should be rejected',
      );
    });

    test('Variable in head only as writer', () {
      final source = '''
        head_only(A, B, C).
      ''';

      expect(
        () => _parseAndAnalyze(source),
        throwsA(isA<CompileError>()),
        reason: 'Variables in head need readers (or trivial body)',
      );
    });

    test('Variable in body only as reader', () {
      final source = '''
        body_only :- foo(X?).
      ''';

      expect(
        () => _parseAndAnalyze(source),
        throwsA(isA<CompileError>()),
        reason: 'Reader without writer violates pairing',
      );
    });
  });
}

/// Helper to parse and analyze GLP source
void _parseAndAnalyze(String source) {
  final parser = Parser(source);
  final clause = parser.parseClause();

  // Create analyzer and verify SRSW
  final analyzer = SRSWAnalyzer(clause);
  analyzer.verifySRSW(); // Should throw if SRSW violated
}
