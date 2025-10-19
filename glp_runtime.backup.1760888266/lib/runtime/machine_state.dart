import 'package:collection/collection.dart';

/// Identifier types (opaque ints for now).
typedef GoalId = int;
typedef Pc = int;        // program counter
typedef ReaderId = int;  // RO identity (queue owner)
typedef WriterId = int;  // WR identity

/// Goal status per v2.16.
enum GoalStatus { active, suspended, failed, succeeded }

/// A reference to a goal scheduled to run at a PC.
class GoalRef {
  final GoalId id;
  final Pc pc;
  const GoalRef(this.id, this.pc);
}

/// σ̂w: clause-local, goal-local tentative writer substitution (no heap writes).
/// Represented here as a map from WriterId → Object? (placeholder Value type).
typedef SigmaHat = Map<WriterId, Object?>;

/// Each goal maintains its own tail-recursion budget (initially 26).
const int tailRecursionBudgetInit = 26;

/// Minimal GoalState record (no logic yet).
class GoalState {
  final GoalId goalId;
  Pc pc;                // current PC
  final Pc kappa;       // clause-selection entry PC (restart point)
  GoalStatus status;
  int tailBudget;
  SigmaHat sigmaHat;    // cleared at clause_next and suspend

  GoalState({
    required this.goalId,
    required this.pc,
    required this.kappa,
    this.status = GoalStatus.active,
    this.tailBudget = tailRecursionBudgetInit,
    SigmaHat? sigmaHat,
  }) : sigmaHat = sigmaHat ?? <WriterId, Object?>{};
}

/// Process/Goal queue: FIFO of scheduled (goalId, pc).
class GoalQueue {
  final QueueList<GoalRef> _q = QueueList<GoalRef>();
  bool get isEmpty => _q.isEmpty;
  int get length => _q.length;
  void enqueue(GoalRef r) => _q.add(r);
  GoalRef? dequeue() => _q.isEmpty ? null : _q.removeFirst();
}
