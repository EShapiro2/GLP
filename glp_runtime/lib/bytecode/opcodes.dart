typedef LabelName = String;

/// Minimal IR for GLP bytecode runner.
abstract class Op {}

class Label implements Op {
  final LabelName name;
  Label(this.name);
}

class ClauseTry implements Op {}
class Commit implements Op {}

/// clause_next: Unified instruction for clause failure/suspension
/// Unions Si into U as needed, then discards σ̂w and jumps to the next clause.
/// From spec 2.2: "discard σ̂w; jump to label of Cj"
class ClauseNext implements Op {
  final LabelName label;
  ClauseNext(this.label);
}

/// no_more_clauses: All clauses exhausted without success (spec 2.5)
/// Behavior: If suspension set non-empty, suspend goal; otherwise mark as permanently failed
class NoMoreClauses implements Op {}

class Proceed implements Op {}

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

// ===== GUARD instructions (pure tests during HEAD/GUARDS phase) =====
/// Otherwise guard: succeeds if all previous clauses failed (not suspended)
/// Checks if Si is empty when executed - if so, all previous clauses definitely failed
/// If Si is non-empty, previous clauses suspended, so this fails
class Otherwise implements Op {}

/// Push: Save current structure processing state before entering nested structure
/// Stores (S, mode, currentStructure) triple in clause variable Xi
/// Following FCP AM design for nested structure handling
class Push implements Op {
  final int regIndex;  // Xi register to store state
  Push(this.regIndex);

  @override
  String toString() => 'Push(X$regIndex)';
}

/// Pop: Restore structure processing state after completing nested structure
/// Retrieves (S, mode, currentStructure) from clause variable Xi
/// Must correspond to a previous Push instruction
class Pop implements Op {
  final int regIndex;  // Xi register to restore from
  Pop(this.regIndex);

  @override
  String toString() => 'Pop(X$regIndex)';
}

/// UnifyStructure: Process nested structure at current S position
/// Following FCP AM's unify_compound instruction
/// Matches/creates structure at args[S], then enters that structure for processing
class UnifyStructure implements Op {
  final String functor;
  final int arity;

  UnifyStructure(this.functor, this.arity);

  @override
  String toString() => 'UnifyStructure($functor, $arity)';
}

/// Guard predicate call: execute guard without side effects
/// If succeeds: continue; If fails: try next clause; If suspends: suspend entire goal
/// If negated: invert success/fail result (suspend unchanged)
class Guard implements Op {
  final LabelName procedureLabel;  // guard predicate entry
  final int arity;                  // number of arguments
  final bool negated;               // true if ~G (guard negation)
  Guard(this.procedureLabel, this.arity, {this.negated = false});
}

/// Ground test: test if variable contains no unbound variables
/// Succeed if X is ground, fail otherwise. Pure test, no side effects.
/// If negated: succeed if X is NOT ground (contains unbound variables)
class Ground implements Op {
  final int varIndex;  // clause variable index to test
  final bool negated;  // true if ~ground(X)
  Ground(this.varIndex, {this.negated = false});
}

/// Known test: test if variable is not an unbound variable
/// Succeed if X is not a variable, fail otherwise. Pure test operation.
/// If negated: succeed if X IS an unbound variable
class Known implements Op {
  final int varIndex;  // clause variable index to test
  final bool negated;  // true if ~known(X)
  Known(this.varIndex, {this.negated = false});
}

/// NoReaders test: test if term contains no readers
/// Three-valued semantics:
/// - SUCCESS: Term contains no readers (ground terms and/or writers only)
/// - SUSPEND: Term contains readers (even bound ones need to be traversed)
/// - FAILURE: Never fails (per spec)
/// If negated: ~no_readers(X) succeeds if X contains readers
class NoReaders implements Op {
  final int varIndex;  // clause variable index to test
  final bool negated;  // true if ~no_readers(X)
  NoReaders(this.varIndex, {this.negated = false});
}

/// Ground equality test: X =?= Y
/// Tests if two terms are structurally equal when both are ground.
/// Three-valued semantics:
/// - SUCCESS: Both terms ground and structurally equal
/// - SUSPEND: Either term contains unbound readers (add to Si)
/// - FAILURE: Both terms ground but not equal
/// Left-to-right evaluation order: checks X first, then Y.
/// If negated: inverts success/failure (suspend unchanged)
class GroundEqual implements Op {
  final int leftVarIndex;   // clause variable index for left operand
  final int rightVarIndex;  // clause variable index for right operand
  final bool negated;       // true if ~(X =?= Y)
  GroundEqual(this.leftVarIndex, this.rightVarIndex, {this.negated = false});

  @override
  String toString() => negated 
      ? '~(X$leftVarIndex =?= X$rightVarIndex)' 
      : 'X$leftVarIndex =?= X$rightVarIndex';
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

// ============================================================================
// Module System Opcodes (Phase 2)
// ============================================================================

/// Distribute: Static RPC to imported module at known index
/// Following FCP: distribute # {Index, Goal}
///
/// Writes message to import vector at Index, which routes to target module.
/// Index is 1-based (FCP convention).
class Distribute implements Op {
  final int importIndex;      // Index in import vector (1-based)
  final String functor;       // Goal functor
  final int arity;            // Goal arity

  Distribute(this.importIndex, this.functor, this.arity);

  @override
  String toString() => 'Distribute([$importIndex] $functor/$arity)';
}

/// Transmit: Dynamic RPC to module resolved at runtime
/// Following FCP: transmit # {ModuleVar, Goal}
///
/// Resolves module name from variable, looks up in registry, sends message.
/// Used when target module is not known at compile time.
class Transmit implements Op {
  final int moduleVarIndex;   // Register holding module name variable
  final String functor;       // Goal functor
  final int arity;            // Goal arity

  Transmit(this.moduleVarIndex, this.functor, this.arity);

  @override
  String toString() => 'Transmit(X$moduleVarIndex, $functor/$arity)';
}
