typedef LabelName = String;

/// Minimal IR for GLP bytecode runner.
abstract class Op {}

class Label implements Op {
  final LabelName name;
  Label(this.name);
}

class ClauseTry implements Op {}
class GuardFail implements Op {}
class Commit implements Op {}
class UnionSiAndGoto implements Op {
  final LabelName label;
  UnionSiAndGoto(this.label);
}
class ResetAndGoto implements Op {
  final LabelName label;
  ResetAndGoto(this.label);
}
class SuspendEnd implements Op {}
class Proceed implements Op {}

// Body ops (post-commit heap mutation)
class BodySetConst implements Op {
  final int writerId;
  final Object? value;
  BodySetConst(this.writerId, this.value);
}
class BodySetStructConstArgs implements Op {
  final int writerId;
  final String functor;
  final List<Object?> constArgs;
  BodySetStructConstArgs(this.writerId, this.functor, this.constArgs);
}

// Head ops with concrete ids
class HeadBindWriter implements Op {
  final int writerId;
  HeadBindWriter(this.writerId);
}
class GuardNeedReader implements Op {
  final int readerId;
  GuardNeedReader(this.readerId);
}

// ===== Argument-slot variants (program fixed; ids supplied at runtime) =====
class RequireWriterArg implements Op {
  final int slot;            // argument index (0 for p/1)
  final LabelName failLabel; // jump if not a writer call
  RequireWriterArg(this.slot, this.failLabel);
}
class RequireReaderArg implements Op {
  final int slot;            // argument index (0 for p/1)
  final LabelName failLabel; // jump if not a reader call
  RequireReaderArg(this.slot, this.failLabel);
}
class HeadBindWriterArg implements Op {
  final int slot;            // add writer(slot) to σ̂w
  HeadBindWriterArg(this.slot);
}
class GuardNeedReaderArg implements Op {
  final int slot;            // add reader(slot) to Sᵢ iff unbound
  GuardNeedReaderArg(this.slot);
}
class BodySetConstArg implements Op {
  final int slot;            // bind writer(slot) := value (post-commit only)
  final Object? value;
  BodySetConstArg(this.slot, this.value);
}

// Scheduler / fairness
class TailStep implements Op {
  final LabelName label;
  TailStep(this.label);
}
