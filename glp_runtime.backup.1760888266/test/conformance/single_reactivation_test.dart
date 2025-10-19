import 'package:test/test.dart';
import 'package:glp_runtime/runtime/hanger.dart';
import 'package:glp_runtime/runtime/roq.dart';
import 'package:glp_runtime/runtime/suspend.dart';

void main() {
  test('Single reactivation with one hanger registered on two readers', () {
    final roq = ROQueues();
    final r1 = 11, r2 = 22;
    // One goal suspended on two different readers (same hanger shared).
    final h = Hanger(goalId: 303, kappa: 5, armed: true);

    roq.enqueue(r1, SuspensionNote(r1, h));
    roq.enqueue(r2, SuspensionNote(r2, h));

    // Bind r1 first → one activation
    final acts1 = roq.processOnBind(r1);
    expect(acts1.map((a) => a.id).toList(), [303]);
    expect(h.armed, isFalse);

    // Later bind r2 → no second activation (armed already false)
    final acts2 = roq.processOnBind(r2);
    expect(acts2, isEmpty);
  });
}
