// glp_runtime/test/analysis/type_checker/moded_term_test.dart
//
// Unit tests for the leaf classification of a moded constant.
// Paper: TGLP def:consistent-paths, row 7 of the consistency table --- a term
// constant `c` is compatible with the type symbol `c`, with `String` and with
// `_`, and quoting plays no part --- and sec:root-self, where `[]` is a
// `String` and hence a `Constant`.

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/moded_term.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';

void main() {
  group('ModedConstant leaf classification (def:consistent-paths row 7)', () {
    test('an unquoted constant is a String leaf', () {
      expect(ModedConstant(Mode.produce, 'red').isString, isTrue);
      expect(ModedConstant(Mode.consume, 'befriend').isString, isTrue);
    });

    test('a quoted constant is a String leaf', () {
      expect(ModedConstant(Mode.produce, '"hello"').isString, isTrue);
      expect(ModedConstant(Mode.produce, "'hello'").isString, isTrue);
    });

    test('nil is a String leaf', () {
      expect(ModedConstant.nil(Mode.produce).isString, isTrue);
      expect(ModedConstant.nil(Mode.produce).isNil, isTrue);
    });

    test('a number is not a String leaf', () {
      expect(ModedConstant(Mode.produce, 42).isString, isFalse);
      expect(ModedConstant(Mode.produce, 3.5).isString, isFalse);
      expect(ModedConstant(Mode.produce, 42).isInteger, isTrue);
      expect(ModedConstant(Mode.produce, 3.5).isReal, isTrue);
      expect(ModedConstant(Mode.produce, 42).isNumeric, isTrue);
    });
  });
}
