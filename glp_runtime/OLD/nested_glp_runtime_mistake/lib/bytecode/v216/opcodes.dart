library glp_bytecode_v216;

/// v2.16 bytecode — instruction model (data only).
/// Runner/interpreter will come next; for now we define the ops as types.

/// All instructions derive from [Op].
sealed class Op {
  const Op();
}

/// Pseudo-instruction to mark a label (branch target).
final class Label extends Op {
  const Label(this.name);
  final String name;
  @override
  String toString() => 'Label($name)';
}

/// Unconditional branch to a label.
final class Goto extends Op {
  const Goto(this.label);
  final String label;
  @override
  String toString() => 'Goto($label)';
}

/// --- Clause control (HEAD/GUARDS/BODY framing) ---

/// Start trying a clause. Resets the clause-local suspension set Si.
final class ClauseTry extends Op {
  const ClauseTry();
}

/// Skip to the next clause of the current predicate (on fail).
final class ClauseNext extends Op {
  const ClauseNext();
}

/// No more clauses in the predicate; inspect U and suspend or succeed.
final class NoMoreClauses extends Op {
  const NoMoreClauses({this.kappa = 'κ'});
  /// Where to resume after suspension (predicate entry).
  final String kappa;
}

/// Commit to this clause and enter BODY (σ̂w is committed here).
final class Commit extends Op {
  const Commit();
}

/// Succeed and return from the predicate.
final class Proceed extends Op {
  const Proceed();
}

/// Suspend the goal now (runner records κ for resume).
final class SuspendNow extends Op {
  const SuspendNow({this.kappa = 'κ'});
  final String kappa;
}

/// --- HEAD instructions (pure; compute tentative writer substitution σ̂w) ---

/// Head writer at [slot].
final class HeadWriter extends Op {
  const HeadWriter(this.slot);
  final int slot;
}

/// Head reader at [slot]; may add its blocker to Si if uninstantiated.
final class HeadReader extends Op {
  const HeadReader(this.slot);
  final int slot;
}

/// Head constant match at [slot] with a symbol.
final class HeadConstant extends Op {
  const HeadConstant(this.slot, this.symbol);
  final int slot;
  final String symbol;
}

/// Head structure match at [slot] with functor/arity.
final class HeadStructure extends Op {
  const HeadStructure(this.slot, this.functor, this.arity);
  final int slot;
  final String functor;
  final int arity;
}

/// --- GUARD instructions (pure; may succeed/fail/suspend) ---

/// Guard that the arg at [slot] is known (bound).
final class GuardKnown extends Op {
  const GuardKnown(this.slot);
  final int slot;
}

/// Guard that the arg at [slot] is ground (fully instantiated).
final class GuardGround extends Op {
  const GuardGround(this.slot);
  final int slot;
}

/// Guard that arg at [slot] equals [symbol] (for facts).
final class GuardEqConst extends Op {
  const GuardEqConst(this.slot, this.symbol);
  final int slot;
  final String symbol;
}

/// Else-branch for guards (used in structured guard sequences).
final class Otherwise extends Op {
  const Otherwise();
}

/// --- BODY instructions (mutating; actual heap writes happen here) ---

/// Put constant [symbol] into writer at [slot].
final class PutConstant extends Op {
  const PutConstant(this.slot, this.symbol);
  final int slot;
  final String symbol;
}

/// Allocate temporary frame space (if needed).
final class Allocate extends Op {
  const Allocate(this.size);
  final int size;
}

/// Deallocate temporary frame.
final class Deallocate extends Op {
  const Deallocate();
}

/// Spawn a new goal identified by [label] (runner decides how to resolve it).
final class Spawn extends Op {
  const Spawn(this.label);
  final String label;
}

/// Requeue current goal (cooperative fairness).
final class Requeue extends Op {
  const Requeue();
}
