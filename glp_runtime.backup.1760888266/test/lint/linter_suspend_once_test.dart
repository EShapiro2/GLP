import 'package:test/test.dart';
import 'package:glp_runtime/bytecode/asm.dart';
import 'package:glp_runtime/lint/linter.dart';

void main() {
  test('Multiple SuspendEnd or ClauseTry after SuspendEnd is flagged', () {
    final prog = BC.prog([
      BC.L('C1'),
      BC.TRY(),
      BC.R(1),
      BC.U('END'),

      BC.L('END'),
      BC.SUSP(),

      // Illegal extra ClauseTry after final suspend:
      BC.L('C2'),
      BC.TRY(),
      BC.R(2),
      BC.U('END2'),
      BC.L('END2'),
      BC.SUSP(),
    ]);

    final res = Linter().lint(prog);
    expect(res.ok, isFalse);
    expect(res.issues.any((e) => e.code == 'SUSPEND_ONCE_AT_END'), isTrue,
        reason: res.issues.join('\n'));
  });
}
