import 'machine_state.dart';

/// Returns the next tail budget after one tail reduction.
/// If zero, the scheduler should yield and reset to tailRecursionBudgetInit.
int nextTailBudget(int current) => (current <= 0) ? 0 : current - 1;

/// Reset the budget after a yield.
int resetTailBudget() => tailRecursionBudgetInit;
