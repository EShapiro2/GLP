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

/// clause_next: Unified instruction for clause failure/suspension
/// Combines the behavior of UnionSiAndGoto (when Si non-empty) and ResetAndGoto (when Si empty)
/// From spec 2.2: "discard σ̂w; jump to label of Cj"
class ClauseNext implements Op {
  final LabelName label;
  ClauseNext(this.label);
}

/// try_next_clause: Attempt next clause if current fails during selection phase (spec 2.4)
/// Behavior: If current clause head fails to unify or guard fails, discard σ̂w and try next clause
class TryNextClause implements Op {}

/// no_more_clauses: All clauses exhausted without success (spec 2.5)
/// Behavior: If suspension set non-empty, suspend goal; otherwise mark as permanently failed
class NoMoreClauses implements Op {}

// Legacy instructions (to be replaced by ClauseNext)
@deprecated
class UnionSiAndGoto implements Op {
  final LabelName label;
  UnionSiAndGoto(this.label);
}
@deprecated
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

/// Place empty list [] in argument register (optimized put_constant)
/// Special case of put_constant for empty list
class PutNil implements Op {
  final int argSlot;
  PutNil(this.argSlot);
}

/// Begin list construction in argument register (optimized put_structure)
/// Equivalent to put_structure './2' or '[|]/2' depending on list functor
class PutList implements Op {
  final int argSlot;
  PutList(this.argSlot);
}

/// Put a reader pointing to a writer bound to a constant value
/// Used for passing constants as arguments in queries
class PutBoundConst implements Op {
  final Object? value;
  final int argSlot;
  PutBoundConst(this.value, this.argSlot);
}

/// Put a reader pointing to a writer bound to 'nil'
/// Used for passing empty lists as arguments in queries
class PutBoundNil implements Op {
  final int argSlot;
  PutBoundNil(this.argSlot);
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

/// Match empty list [] with argument (optimized head_constant)
/// Same unification semantics as head_constant with '[]' value
class HeadNil implements Op {
  final int argSlot;
  HeadNil(this.argSlot);
}

/// Match list structure [H|T] with argument (optimized head_structure)
/// Equivalent to head_structure './2' or '[|]/2' depending on list functor
class HeadList implements Op {
  final int argSlot;
  HeadList(this.argSlot);
}

/// Match void (anonymous variable) at current S position
/// In READ mode: skip, In WRITE mode: create fresh variable
class UnifyVoid implements Op {
  final int count; // number of void positions to skip/create
  UnifyVoid({this.count = 1});
}

/// Match writer variable at current S position
/// In READ mode: unify with writer, In WRITE mode: add writer to structure
class UnifyWriter implements Op {
  final int varIndex;  // clause variable index holding the writer
  UnifyWriter(this.varIndex);
}

/// Match reader variable at current S position
/// In READ mode: unify with reader, In WRITE mode: add reader to structure
class UnifyReader implements Op {
  final int varIndex;  // clause variable index holding the writer (reader derived from it)
  UnifyReader(this.varIndex);
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

/// IfWriter guard: succeeds if variable is a writer (not reader, not constant)
/// Used for type checking in guards
class IfWriter implements Op {
  final int varIndex;  // clause variable index to test
  IfWriter(this.varIndex);
}

/// IfReader guard: succeeds if variable is a reader (not writer, not constant)
/// Used for type checking in guards
class IfReader implements Op {
  final int varIndex;  // clause variable index to test
  IfReader(this.varIndex);
}

/// Guard predicate call: execute guard without side effects
/// If succeeds: continue; If fails: try next clause; If suspends: suspend entire goal
class Guard implements Op {
  final LabelName procedureLabel;  // guard predicate entry
  final int arity;                  // number of arguments
  Guard(this.procedureLabel, this.arity);
}

/// Ground test: test if variable contains no unbound variables
/// Succeed if X is ground, fail otherwise. Pure test, no side effects.
class Ground implements Op {
  final int varIndex;  // clause variable index to test
  Ground(this.varIndex);
}

/// Known test: test if variable is not an unbound variable
/// Succeed if X is not a variable, fail otherwise. Pure test operation.
class Known implements Op {
  final int varIndex;  // clause variable index to test
  Known(this.varIndex);
}

// ===== SYSTEM PREDICATE execution =====
/// Execute system predicate: call registered Dart function
/// Used for I/O, arithmetic, and other operations requiring side effects
/// System predicates can succeed, fail, or suspend on unbound readers
class Execute implements Op {
  final String predicateName;     // name of system predicate (e.g., "evaluate", "file_read")
  final List<int> argSlots;       // clause variable indices for arguments
  Execute(this.predicateName, this.argSlots);
}

/// SetClauseVar: Directly set a clause variable to a term value
/// Used for setting up constants/terms before Execute calls
class SetClauseVar implements Op {
  final int slot;      // clause variable index
  final Object? value; // Term or primitive value to set
  SetClauseVar(this.slot, this.value);
}

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

/// Create environment frame with n permanent variables
/// Push new frame on local stack, save E and CP in frame, update E to point to new frame
class Allocate implements Op {
  final int slots;  // number of permanent variable slots (Y1-Yn)
  Allocate(this.slots);
}

/// Remove current environment frame
/// Restore previous E and CP from frame, pop frame from stack
class Deallocate implements Op {}

/// No operation - advance PC without other effects
/// Used for alignment or patching
class Nop implements Op {}

/// Terminate execution - mark goal as completed, return control to scheduler
class Halt implements Op {}
