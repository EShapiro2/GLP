import 'package:test/test.dart';
import 'package:glp_runtime/bytecode/asm.dart';
import 'package:glp_runtime/lint/linter.dart';

void main() {
  test('Body op before commit is flagged', () {
    final prog = BC.prog([
      BC.L('C1'),
      BC.TRY(),
      BC.W(10),
      BC.BCONST(10, 42),  // body op before COMMIT (illegal)
      BC.U('END'),
      BC.L('END'),
      BC.SUSP(),
    ]);

    final res = Linter().lint(prog);
    expect(res.ok, isFalse);
    expect(res.issues.any((e) => e.code == 'BODY_BEFORE_COMMIT'), isTrue,
        reason: res.issues.join('\n'));
  });
}
