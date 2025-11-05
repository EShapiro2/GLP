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

/// Place writer variable into argument register (BODY phase)
/// Used to pass writer variables to spawned goals
class PutWriter implements Op {
  final int varIndex;  // clause variable index holding writer ID
  final int argSlot;   // target argument register
  PutWriter(this.varIndex, this.argSlot);
}

/// Place reader variable into argument register (BODY phase)
/// Derives reader from writer in clause variable
class PutReader implements Op {
  final int varIndex;  // clause variable index holding writer ID
  final int argSlot;   // target argument register
  PutReader(this.varIndex, this.argSlot);
}

/// Place constant value into argument register (BODY phase)
class PutConstant implements Op {
  final Object? value;
  final int argSlot;
  PutConstant(this.value, this.argSlot);
}

/// Create structure on heap and place reference in argument register (BODY phase)
/// WAM semantics: HEAP[H] ← <STR, H+1>; HEAP[H+1] ← F/n; Ai ← HEAP[H]; H ← H+2; mode ← WRITE
class PutStructure implements Op {
  final String functor;
  final int arity;
  final int argSlot;   // target argument register
  PutStructure(this.functor, this.arity, this.argSlot);
}

/// Build structure argument: allocate new writer (BODY phase, WRITE mode)
/// Creates writer/reader pair, stores WriterTerm at HEAP[H], increments H
class SetWriter implements Op {
  final int varIndex;  // clause variable index to store writer ID
  SetWriter(this.varIndex);
}

/// Build structure argument: place reader for writer (BODY phase, WRITE mode)
/// Extracts paired reader from writer in varIndex, stores ReaderTerm at HEAP[H], increments H
class SetReader implements Op {
  final int varIndex;  // clause variable index holding writer ID
  SetReader(this.varIndex);
}

/// Build structure argument: place constant (BODY phase, WRITE mode)
/// Stores ConstTerm at HEAP[H], increments H
class SetConstant implements Op {
  final Object? value;
  SetConstant(this.value);
}

// ===== v2.16 HEAD instructions (encode clause patterns) =====
/// Match constant c with argument at argSlot
/// Behavior: Writer(w) → σ̂w[w]=c; Reader(r) → Si+={r}; Ground(t) → check t==c
class HeadConstant implements Op {
  final Object? value;
  final int argSlot;
  HeadConstant(this.value, this.argSlot);
}

/// Match structure f/n with argument at argSlot
/// Sets READ/WRITE mode and S register for subsequent structure traversal
class HeadStructure implements Op {
  final String functor;
  final int arity;
  final int argSlot;
  HeadStructure(this.functor, this.arity, this.argSlot);
}

/// Process writer variable in structure (at S register position)
/// Operates in READ or WRITE mode
class HeadWriter implements Op {
  final int varIndex;  // clause variable index
  HeadWriter(this.varIndex);
}

/// Process reader variable in structure (at S register position)
/// Operates in READ or WRITE mode, may add to Si
class HeadReader implements Op {
  final int varIndex;  // clause variable index
  HeadReader(this.varIndex);
}

/// Match constant at current S position in structure
/// Operates in READ or WRITE mode
class UnifyConstant implements Op {
  final Object? value;
  UnifyConstant(this.value);
}

/// Match void (anonymous variable) at current S position
/// In READ mode: skip, In WRITE mode: create fresh variable
class UnifyVoid implements Op {
  final int count; // number of void positions to skip/create
  UnifyVoid({this.count = 1});
}

/// Load argument into clause variable (first occurrence)
/// Records tentative association in σ̂w during HEAD phase
class GetVariable implements Op {
  final int varIndex;  // clause variable index
  final int argSlot;   // argument register
  GetVariable(this.varIndex, this.argSlot);
}

/// Unify argument with clause variable (subsequent occurrence)
/// Performs writer MGU, updates σ̂w during HEAD phase
class GetValue implements Op {
  final int varIndex;  // clause variable index
  final int argSlot;   // argument register
  GetValue(this.varIndex, this.argSlot);
}

// ===== GUARD instructions (pure tests during HEAD/GUARDS phase) =====
/// Otherwise guard: succeeds if all previous clauses failed (not suspended)
/// Checks if Si is empty when executed - if so, all previous clauses definitely failed
/// If Si is non-empty, previous clauses suspended, so this fails
class Otherwise implements Op {}

// Legacy opcodes (for backward compatibility with existing tests)
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

/// Spawn new goal for procedure P with arguments in A1-An
/// Non-tail call: saves continuation and schedules new goal
class Spawn implements Op {
  final LabelName procedureLabel;  // procedure entry label
  final int arity;                  // number of arguments
  Spawn(this.procedureLabel, this.arity);
}

/// Tail call to procedure P with arguments in A1-An
/// Reuses current goal frame, implements fair scheduling via tail recursion budget
class Requeue implements Op {
  final LabelName procedureLabel;  // procedure entry label
  final int arity;                  // number of arguments
  Requeue(this.procedureLabel, this.arity);
}
