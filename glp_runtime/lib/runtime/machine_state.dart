import 'package:collection/collection.dart';

/// Identifier types (opaque ints for now).
typedef GoalId = int;
typedef Pc = int;        // program counter
typedef ReaderId = int;  // RO identity (queue owner)
typedef WriterId = int;  // WR identity

/// A reference to a goal scheduled to run at a PC.
class GoalRef {
  final GoalId id;
  final Pc pc;
  const GoalRef(this.id, this.pc);

  @override
  bool operator ==(Object other) =>
      other is GoalRef && other.id == id && other.pc == pc;

  @override
  int get hashCode => Object.hash(id, pc);
}

/// Each goal maintains its own tail-recursion budget (initially 26).
const int tailRecursionBudgetInit = 26;

/// Process/Goal queue: FIFO of scheduled (goalId, pc).
class GoalQueue {
  final QueueList<GoalRef> _q = QueueList<GoalRef>();
  bool get isEmpty => _q.isEmpty;
  int get length => _q.length;
  void enqueue(GoalRef r) => _q.add(r);
  GoalRef? dequeue() => _q.isEmpty ? null : _q.removeFirst();
  Iterable<GoalRef> get items => _q;
}
