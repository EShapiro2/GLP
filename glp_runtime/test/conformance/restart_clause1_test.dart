import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/suspend_ops.dart';
import 'package:glp_runtime/runtime/heap_fcp.dart';
import 'package:glp_runtime/runtime/commit.dart';
import 'package:glp_runtime/runtime/terms.dart';

void main() {
  test('On wake, activation pc equals kappa (restart at clause 1)', () {
    final rt = GlpRuntime();
    final heap = rt.heap as HeapFCP;

    const GoalId g = 77;
    const Pc kappa = 1;

    // Allocate variable for suspension
    final varId = heap.allocateVariable();

    // Suspend goal g on reader varId with restart point kappa
    SuspendOps.suspendGoalFCP(
      heap: heap,
      goalId: g,
      kappa: kappa,
      readerVarIds: {varId},
    );

    // Binding the variable should activate the goal
    // Create σ̂w with binding for varId
    final sigmaHat = {varId: ConstTerm('ground')};
    final acts = CommitOps.applySigmaHatFCP(heap: heap, sigmaHat: sigmaHat);

    expect(acts, hasLength(1));
    expect(acts.first.id, g);
    expect(acts.first.pc, kappa);
  });
}
