import 'package:test/test.dart';
import 'package:glp_runtime/runtime/hanger.dart';
import 'package:glp_runtime/runtime/roq.dart';
import 'package:glp_runtime/runtime/suspend.dart';

void main() {
  test('FIFO wake on single RO queue', () {
    final roq = ROQueues();
    final r = 1; // ReaderId
    // Two different goals suspended on the same reader, in order g1 then g2.
    final g1 = Hanger(goalId: 101, kappa: 7, armed: true);
    final g2 = Hanger(goalId: 202, kappa: 7, armed: true);

    roq.enqueue(r, SuspensionNote(r, g1)); // first in
    roq.enqueue(r, SuspensionNote(r, g2)); // second in

    final acts = roq.processOnBind(r);
    expect(acts.map((e) => e.id).toList(), [101, 202]); // FIFO order
    expect(acts.map((e) => e.pc).toSet(), {7});
    expect(roq.isEmpty(r), isTrue);
  });
}
