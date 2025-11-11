sealed class Op {
  const Op();
}

/* Control / flow */
final class Label extends Op {
  const Label(this.name);
  final String name;
}
final class Goto extends Op {
  const Goto(this.label);
  final String label;
}
final class ClauseTry extends Op { const ClauseTry(); }
final class ClauseNext extends Op { const ClauseNext(); }
final class NoMoreClauses extends Op {
  const NoMoreClauses({this.kappa = 'κ'});
  final String kappa; // resume label after suspend
}
final class Commit extends Op { const Commit(); }
final class Proceed extends Op { const Proceed(); }
final class SuspendNow extends Op {
  const SuspendNow({this.kappa = 'κ'});
  final String kappa;
}

/* HEAD (pure) */
final class HeadWriter extends Op {
  const HeadWriter(this.slot);
  final int slot;
}
final class HeadReader extends Op {
  const HeadReader(this.slot);
  final int slot;
}
final class HeadConstant extends Op {
  const HeadConstant(this.slot, this.symbol);
  final int slot;
  final String symbol;
}
final class HeadStructure extends Op {
  const HeadStructure(this.slot, this.functor, this.arity);
  final int slot;
  final String functor;
  final int arity;
}

/* GUARDS (pure) */
final class GuardKnown extends Op {
  const GuardKnown(this.slot);
  final int slot;
}
final class GuardGround extends Op {
  const GuardGround(this.slot);
  final int slot;
}
final class GuardEqConst extends Op {
  const GuardEqConst(this.slot, this.symbol);
  final int slot;
  final String symbol;
}
final class Otherwise extends Op { const Otherwise(); }

/* BODY (mutating after Commit) */
final class PutConstant extends Op {
  const PutConstant(this.slot, this.symbol);
  final int slot;
  final String symbol;
}
final class Allocate extends Op {
  const Allocate(this.size);
  final int size;
}
final class Deallocate extends Op { const Deallocate(); }
final class Spawn extends Op {
  const Spawn(this.label);
  final String label;
}
final class Requeue extends Op { const Requeue(); }
