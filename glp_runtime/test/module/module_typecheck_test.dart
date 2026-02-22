import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart';

void main() {
  group('Phase 3 - 2a: remote goal type-checks against imported declaration', () {
    test('math # factorial(N?, R) passes with matching imported declaration', () {
      final result = checkSource('''
        imported procedure math#factorial(Integer?, Integer).
        procedure compute(Integer?, Integer).
        compute(N, R) :- true | math # factorial(N?, R).
      ''');

      expect(result.errors, isEmpty,
          reason: 'Remote call should type-check against imported declaration');
    });
  });

  group('Phase 3 - 2b: remote goal fails without imported declaration', () {
    test('math # factorial(N?, R) fails without imported declaration', () {
      final result = checkSource('''
        procedure compute(Integer?, Integer).
        compute(N, R) :- true | math # factorial(N?, R).
      ''');

      expect(result.errors, isNotEmpty,
          reason: 'Remote call without imported declaration should produce type error');
      expect(result.errors.any((e) => e.message.contains('math#factorial')), isTrue,
          reason: 'Error should mention the missing imported declaration');
    });
  });

  group('Phase 3 - 2c: remote goal fails on type mismatch', () {
    test('type mismatch between call args and imported declaration', () {
      final result = checkSource('''
        imported procedure math#factorial(Integer?, Integer).
        MyType ::= foo ; bar.
        procedure compute(MyType?, Integer).
        compute(M, R) :- true | math # factorial(M?, R).
      ''');

      expect(result.errors, isNotEmpty,
          reason: 'Passing MyType? where Integer? expected should be a type error');
    });
  });

  group('Phase 3 - 2d: deep module path', () {
    test('ui#actors # render type-checks against deep imported declaration', () {
      final result = checkSource('''
        imported procedure ui#actors#render(Integer?, Integer).
        procedure start(Integer?, Integer).
        start(X, Y) :- true | ui#actors # render(X?, Y).
      ''');

      expect(result.errors, isEmpty,
          reason: 'Deep module path should type-check correctly');
    });
  });

  group('Phase 3 - 2e: imported ancestor procedure (no path)', () {
    test('imported procedure without path type-checks local calls', () {
      final result = checkSource('''
        imported procedure merge(Stream?, Stream?, Stream).
        procedure combine(Stream?, Stream?, Stream).
        combine(A, B, C) :- true | merge(A?, B?, C).
      ''');

      expect(result.errors, isEmpty,
          reason: 'Imported procedure without path should work like local declaration');
    });
  });

  group('Phase 3 - 2f: multiple imported procedures', () {
    test('multiple imported declarations each checked independently', () {
      final result = checkSource('''
        imported procedure math#factorial(Integer?, Integer).
        imported procedure io#print(String?).
        procedure main.
        main :- true | math # factorial(5, R), io # print(hello).
      ''');

      expect(result.errors, isEmpty,
          reason: 'Multiple imported procedures should each be found');
    });
  });

  group('Phase 3 - 2g: dynamic remote goal skipped', () {
    test('M # goal(X) where M is a variable is not type-checked', () {
      final result = checkSource('''
        procedure dispatch(_, Integer?, Integer).
        dispatch(M, X, Y) :- true | M # compute(X?, Y).
      ''');

      expect(result.errors, isEmpty,
          reason: 'Dynamic module dispatch should skip type checking');
    });
  });
}
