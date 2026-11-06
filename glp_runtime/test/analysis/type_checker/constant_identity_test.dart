// glp_runtime/test/analysis/type_checker/constant_identity_test.dart
//
// 1 and 1.0 are distinct constants, an Integer and a Real, Number their union
// (GLP paper 3620eeb, appendix-guards.tex:6; GLP #11 Cowork, Code_inbox
// 2026-11-03 16:40 UTC, item 3): sameTypeExpr, and shadowedBy through it,
// compare the constants of two type expressions as sameConstant does, holding
// 1 and 1.0 apart, where until 2026-11-05 they compared them by ==, under
// which Dart holds 1 == 1.0.

import 'package:glp_runtime/analysis/type_checker/moded_term.dart' show sameConstant;
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:test/test.dart';

void main() {
  test('sameConstant holds 1 and 1.0 apart', () {
    expect(sameConstant(1, 1.0), isFalse);
    expect(sameConstant(1, 1), isTrue);
    expect(sameConstant(1.0, 1.0), isTrue);
  });

  test('sameTypeExpr holds the constant alternatives 1 and 1.0 apart', () {
    expect(sameTypeExpr(ConstantAlt(1, 0, 0), ConstantAlt(1.0, 0, 0)), isFalse);
    expect(sameTypeExpr(ConstantAlt(1.0, 0, 0), ConstantAlt(1, 0, 0)), isFalse);
    expect(sameTypeExpr(ConstantAlt(1, 0, 0), ConstantAlt(1, 0, 0)), isTrue);
    expect(sameTypeExpr(ConstantAlt(1.0, 0, 0), ConstantAlt(1.0, 0, 0)), isTrue);
    expect(sameTypeExpr(ConstantAlt('a', 0, 0), ConstantAlt('a', 0, 0)), isTrue);
  });
}
