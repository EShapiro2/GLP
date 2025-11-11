import 'package:test/test.dart';
import 'package:glp_runtime/bytecode/asm.dart';
import 'package:glp_runtime/lint/linter.dart';

void main() {
  test('Valid shape: head/guards only pre-commit; single SuspendEnd after clauses', () {
    final prog = BC.prog([
      BC.L('C1'),
      BC.TRY(),
      BC.R(1),                // need reader
      BC.U('C2'),

      BC.L('C2'),
      BC.TRY(),
      BC.R(2),
      BC.U('END'),

      BC.L('END'),
      BC.SUSP(),
    ]);

    final res = Linter().lint(prog);
    expect(res.ok, isTrue, reason: res.issues.join('\n'));
  });
}
