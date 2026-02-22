import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart';

/// Filter to only body-atom type errors (Phase 3 scope).
/// Head errors are pre-existing and unrelated to cross-module type checking.
List<TypeError> bodyErrors(TypeCheckResult result) =>
    result.errors.where((e) => e.message.contains('Body atom')).toList();

void main() {
  group('Phase 3 - 2a: remote goal type-checks against imported declaration', () {
    test('math # check(N?) passes with matching imported declaration', () {
      final result = checkSource('''
        imported procedure math#check(Integer?).
        procedure validate(Integer?).
        validate(N) :- true | math # check(N?).
      ''');

      expect(bodyErrors(result), isEmpty,
          reason: 'Remote call should type-check against imported declaration');
    });
  });

  group('Phase 3 - 2b: remote goal fails without imported declaration', () {
    test('math # check(N?) fails without imported declaration', () {
      final result = checkSource('''
        procedure validate(Integer?).
        validate(N) :- true | math # check(N?).
      ''');

      final errors = bodyErrors(result);
      expect(errors, isNotEmpty,
          reason: 'Remote call without imported declaration should produce type error');
      expect(errors.any((e) => e.message.contains('math#check')), isTrue,
          reason: 'Error should mention the missing imported declaration');
    });
  });

  group('Phase 3 - 2c: remote goal fails on arity mismatch', () {
    test('arity mismatch between call and imported declaration', () {
      final result = checkSource('''
        imported procedure math#check(Integer?).
        procedure compute(Integer?, Integer?).
        compute(M, N) :- true | math # check(M?, N?).
      ''');

      final errors = bodyErrors(result);
      expect(errors, isNotEmpty,
          reason: 'Calling with 2 args when declaration has 1 should be a type error');
    });
  });

  group('Phase 3 - 2d: deep module path', () {
    test('ui#actors # render type-checks against deep imported declaration', () {
      final result = checkSource('''
        imported procedure ui#actors#render(Integer?).
        procedure start(Integer?).
        start(X) :- true | ui#actors # render(X?).
      ''');

      expect(bodyErrors(result), isEmpty,
          reason: 'Deep module path should type-check correctly');
    });
  });

  group('Phase 3 - 2e: imported ancestor procedure (no path)', () {
    test('imported procedure without path type-checks local calls', () {
      final result = checkSource('''
        imported procedure check(Integer?).
        procedure validate(Integer?).
        validate(N) :- true | check(N?).
      ''');

      expect(bodyErrors(result), isEmpty,
          reason: 'Imported procedure without path should work like local declaration');
    });
  });

  group('Phase 3 - 2f: multiple imported procedures', () {
    test('multiple imported declarations each checked independently', () {
      final result = checkSource('''
        imported procedure math#check(Integer?).
        imported procedure io#log(String?).
        procedure main.
        main :- true | math # check(5), io # log(hello).
      ''');

      expect(bodyErrors(result), isEmpty,
          reason: 'Multiple imported procedures should each be found');
    });
  });

  group('Phase 3 - 2g: dynamic remote goal skipped', () {
    test('M # goal(X) where M is a variable is not type-checked', () {
      final result = checkSource('''
        procedure dispatch(_, Integer?).
        dispatch(M, X) :- true | M # compute(X?).
      ''');

      expect(bodyErrors(result), isEmpty,
          reason: 'Dynamic module dispatch should skip type checking');
    });
  });
}
