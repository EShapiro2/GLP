import 'machine_state.dart';
import 'heap.dart';
import 'roq.dart';
import 'suspend_ops.dart';
import 'commit.dart';
import 'abandon.dart';
import 'fairness.dart';
import 'hanger.dart';

class GlpRuntime {
  final Heap heap;
  final ROQueues roq;
  final GoalQueue gq;

  final Map<GoalId, int> _budgets = <GoalId, int>{};

  GlpRuntime({Heap? heap, ROQueues? roq, GoalQueue? gq})
      : heap = heap ?? Heap(),
        roq = roq ?? ROQueues(),
        gq = gq ?? GoalQueue();

  List<GoalRef> commitWriters(Iterable<int> writerIds) {
    final acts = CommitOps.applySigmaHat(
      heap: heap,
      roq: roq,
      writerIds: writerIds,
    );
    _enqueueAll(acts);
    return acts;
  }

  List<GoalRef> abandonWriter(int writerId) {
    final acts = AbandonOps.abandonWriter(
      heap: heap,
      roq: roq,
      writerId: writerId,
    );
    _enqueueAll(acts);
    return acts;
  }

  Hanger suspendGoal({
    required GoalId goalId,
    required Pc kappa,
    required Iterable<ReaderId> readers,
  }) {
    return SuspendOps.suspendGoal(
      goalId: goalId,
      kappa: kappa,
      roq: roq,
      readers: readers,
    );
  }

  bool tailReduce(GoalId g) {
    final current = _budgets[g] ?? tailRecursionBudgetInit;
    final next = nextTailBudget(current);
    if (next == 0) {
      _budgets[g] = resetTailBudget();
      return true;
    } else {
      _budgets[g] = next;
      return false;
    }
  }

  int budgetOf(GoalId g) => _budgets[g] ?? tailRecursionBudgetInit;

  void _enqueueAll(List<GoalRef> acts) {
    for (final a in acts) {
      gq.enqueue(a);
    }
  }
}
