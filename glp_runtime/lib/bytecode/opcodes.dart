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

/// Put a reader pointing to a writer bound to [] (the runtime's nil)
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

/// NoReaders test: test if term contains no readers
/// Three-valued semantics:
/// - SUCCESS: Term contains no readers (ground terms and/or writers only)
/// - SUSPEND: Term contains readers (even bound ones need to be traversed)
/// - FAILURE: Never fails (per spec)
class NoReaders implements Op {
  final int varIndex;  // clause variable index to test
  NoReaders(this.varIndex);
}

/// Ground equality test: X =?= Y
/// Tests if two terms are structurally equal when both are ground.
/// Three-valued semantics:
/// - SUCCESS: Both terms ground and structurally equal
/// - SUSPEND: Either term contains unbound readers (add to Si)
/// - FAILURE: Both terms ground but not equal
/// Left-to-right evaluation order: checks X first, then Y.
class GroundEqual implements Op {
  final int leftVarIndex;   // clause variable index for left operand
  final int rightVarIndex;  // clause variable index for right operand
  GroundEqual(this.leftVarIndex, this.rightVarIndex);

  @override
  String toString() => 'X$leftVarIndex =?= X$rightVarIndex';
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
// Variable instructions: one instruction serves writer and reader alike, its
// isReader flag the polarity operand of IGLP's opcode table; and unknown.
// ============================================================================

// ============================================================================
// HEAD PHASE - Unified Instructions
// ============================================================================

/// Match variable in clause head (unifies HeadWriter and HeadReader)
/// Behavior depends on isReader flag:
/// - isReader=false (writer): Tentatively bind in σ̂w
/// - isReader=true (reader): Add to Si if unbound
class HeadVariable implements Op {
  final int varIndex;    // clause variable index
  final bool isReader;   // true for reader mode, false for writer mode

  HeadVariable(this.varIndex, {required this.isReader});

  String get mnemonic => isReader ? 'head_reader' : 'head_writer';

  @override
  String toString() => '$mnemonic($varIndex)';
}

/// Get variable from argument register - first occurrence (unifies GetWriterVariable and GetReaderVariable)
/// Used in HEAD phase to load argument into clause variable for first occurrence
/// Behavior depends on isReader flag:
/// - isReader=false (writer): Load argument as writer into varIndex
/// - isReader=true (reader): Load argument as reader into varIndex
class GetVariable implements Op {
  final int varIndex;    // clause variable index
  final int argSlot;     // argument register
  final bool isReader;   // true for reader mode, false for writer mode

  GetVariable(this.varIndex, this.argSlot, {required this.isReader});

  String get mnemonic => isReader ? 'get_reader_variable' : 'get_writer_variable';

  @override
  String toString() => '$mnemonic(X$varIndex, A$argSlot)';
}

/// Get value from argument register - subsequent occurrence (unifies GetWriterValue and GetReaderValue)
/// Used in HEAD phase to unify argument with existing clause variable
/// Behavior depends on isReader flag:
/// - isReader=false (writer): Unify argument with writer in varIndex
/// - isReader=true (reader): Unify argument with reader in varIndex
class GetValue implements Op {
  final int varIndex;    // clause variable index
  final int argSlot;     // argument register
  final bool isReader;   // true for reader mode, false for writer mode

  GetValue(this.varIndex, this.argSlot, {required this.isReader});

  String get mnemonic => isReader ? 'get_reader_value' : 'get_writer_value';

  @override
  String toString() => '$mnemonic(X$varIndex, A$argSlot)';
}

// ============================================================================
// STRUCTURE TRAVERSAL - Unified Instructions
// ============================================================================

/// Match variable at current S position in structure (unifies UnifyWriter and UnifyReader)
/// Operates in READ or WRITE mode based on HeadStructure/PutStructure context
/// Behavior depends on isReader flag:
/// - isReader=false (writer): Unify with writer variable
/// - isReader=true (reader): Unify with reader variable (may add to Si)
class UnifyVariable implements Op {
  final int varIndex;    // clause variable index
  final bool isReader;   // true for reader mode, false for writer mode

  UnifyVariable(this.varIndex, {required this.isReader});

  String get mnemonic => isReader ? 'unify_reader' : 'unify_writer';

  @override
  String toString() => '$mnemonic($varIndex)';
}

// ============================================================================
// BODY PHASE - Unified Instructions
// ============================================================================

/// Place variable into argument register (unifies PutWriter and PutReader)
/// Used in BODY phase to pass variables to spawned goals
/// Behavior depends on isReader flag:
/// - isReader=false (writer): Place writer from varIndex into argSlot
/// - isReader=true (reader): Derive reader from writer, place into argSlot
class PutVariable implements Op {
  final int varIndex;    // clause variable index holding writer ID
  final int argSlot;     // target argument register
  final bool isReader;   // true for reader mode, false for writer mode

  PutVariable(this.varIndex, this.argSlot, {required this.isReader});

  String get mnemonic => isReader ? 'put_reader' : 'put_writer';

  @override
  String toString() => '$mnemonic(X$varIndex, A$argSlot)';
}

/// Build structure argument (unifies SetWriter and SetReader)
/// Used in BODY phase WRITE mode to construct structure subterms
/// Behavior depends on isReader flag:
/// - isReader=false (writer): Create writer, store in varIndex, add WriterTerm to heap
/// - isReader=true (reader): Derive reader from writer in varIndex, add ReaderTerm to heap
class SetVariable implements Op {
  final int varIndex;    // clause variable index
  final bool isReader;   // true for reader mode, false for writer mode

  SetVariable(this.varIndex, {required this.isReader});

  String get mnemonic => isReader ? 'set_reader' : 'set_writer';

  @override
  String toString() => '$mnemonic(X$varIndex)';
}

// ============================================================================
// GUARD PHASE - Guard Instructions
// ============================================================================

/// Test if variable is unbound (value unknown)
/// Succeeds if the variable is unbound, fails if bound to any value.
/// Used for dispatch based on binding status.
class Unknown implements Op {
  final int varIndex;    // clause variable index to test

  Unknown(this.varIndex);

  String get mnemonic => 'unknown';

  @override
  String toString() => 'unknown(X$varIndex)';
}
