import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';

void main() {
  test('26-step tail recursion budget yields and resets', () {
    final rt = GlpRuntime();
    const GoalId g = 123;

    // First 25 tail reductions: do not yield.
    for (var i = 0; i < 25; i++) {
      final y = rt.tailReduce(g);
      expect(y, isFalse, reason: 'should not yield on step ${i+1}');
    }
    // 26th: yield and reset.
    final y26 = rt.tailReduce(g);
    expect(y26, isTrue, reason: 'should yield on step 26');
    expect(rt.budgetOf(g), 26, reason: 'budget resets after yielding');

    // One more step after reset: budget decrements, no yield.
    final y1 = rt.tailReduce(g);
    expect(y1, isFalse);
    expect(rt.budgetOf(g), 25);
  });
}
