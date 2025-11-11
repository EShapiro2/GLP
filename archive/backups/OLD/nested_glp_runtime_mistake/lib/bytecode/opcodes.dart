/// GLP Bytecode v2.16 opcodes for the abstract machine
/// Based on WAM and FCP abstract machines

typedef LabelName = String;

/// Base class for all bytecode operations
abstract class Op {}

// ===== LABELS AND CONTROL =====

/// Label marking a position in the bytecode
class Label implements Op {
  final LabelName name;
  Label(this.name);

  @override
  String toString() => 'Label($name)';
}

/// Try a clause - marks beginning of a clause
class ClauseTry implements Op {
  @override
  String toString() => 'ClauseTry()';
}

/// Commit to current clause (apply σ̂w, enter BODY phase)
class Commit implements Op {
  @override
  String toString() => 'Commit()';
}

/// Proceed - successfully complete clause with empty body
class Proceed implements Op {
  @override
  String toString() => 'Proceed()';
}

/// Suspend - end of procedure, suspend if U non-empty
class SuspendEnd implements Op {
  @override
  String toString() => 'SuspendEnd()';
}

// ===== HEAD INSTRUCTIONS (tentative unification) =====

/// Match constant with argument
class HeadConstant implements Op {
  final Object? value;
  final int argSlot;

  HeadConstant(this.value, this.argSlot);

  @override
  String toString() => 'HeadConstant($value, arg$argSlot)';
}

/// Match structure with argument, set READ/WRITE mode
class HeadStructure implements Op {
  final String functor;
  final int arity;
  final int argSlot;

  HeadStructure(this.functor, this.arity, this.argSlot);

  @override
  String toString() => 'HeadStructure($functor/$arity, arg$argSlot)';
}

/// Process writer variable in structure
class HeadWriter implements Op {
  final int varIndex;

  HeadWriter(this.varIndex);

  @override
  String toString() => 'HeadWriter(var$varIndex)';
}

/// Process reader variable in structure
class HeadReader implements Op {
  final int varIndex;

  HeadReader(this.varIndex);

  @override
  String toString() => 'HeadReader(var$varIndex)';
}

/// Match constant at current S position in structure
class UnifyConstant implements Op {
  final Object? value;

  UnifyConstant(this.value);

  @override
  String toString() => 'UnifyConstant($value)';
}

/// Match void (anonymous variable) at current S position
class UnifyVoid implements Op {
  final int count;

  UnifyVoid({this.count = 1});

  @override
  String toString() => 'UnifyVoid($count)';
}

/// Load argument into clause variable (first occurrence)
class GetVariable implements Op {
  final int varIndex;
  final int argSlot;

  GetVariable(this.varIndex, this.argSlot);

  @override
  String toString() => 'GetVariable(var$varIndex, arg$argSlot)';
}

/// Unify argument with clause variable (subsequent occurrence)
class GetValue implements Op {
  final int varIndex;
  final int argSlot;

  GetValue(this.varIndex, this.argSlot);

  @override
  String toString() => 'GetValue(var$varIndex, arg$argSlot)';
}

// ===== GUARD INSTRUCTIONS (pure tests) =====

/// Otherwise guard - succeeds if all previous clauses failed (not suspended)
class Otherwise implements Op {
  @override
  String toString() => 'Otherwise()';
}

// ===== BODY INSTRUCTIONS (post-commit, mutations allowed) =====

/// Place writer variable into argument register
class PutWriter implements Op {
  final int varIndex;
  final int argSlot;

  PutWriter(this.varIndex, this.argSlot);

  @override
  String toString() => 'PutWriter(var$varIndex, arg$argSlot)';
}

/// Place reader variable into argument register (derives from writer)
class PutReader implements Op {
  final int varIndex;
  final int argSlot;

  PutReader(this.varIndex, this.argSlot);

  @override
  String toString() => 'PutReader(var$varIndex, arg$argSlot)';
}

/// Place constant into argument register
class PutConstant implements Op {
  final Object? value;
  final int argSlot;

  PutConstant(this.value, this.argSlot);

  @override
  String toString() => 'PutConstant($value, arg$argSlot)';
}

/// Create structure on heap and place in argument register
class PutStructure implements Op {
  final String functor;
  final int arity;
  final int argSlot;

  PutStructure(this.functor, this.arity, this.argSlot);

  @override
  String toString() => 'PutStructure($functor/$arity, arg$argSlot)';
}

/// Build structure argument: allocate new writer
class SetWriter implements Op {
  final int varIndex;

  SetWriter(this.varIndex);

  @override
  String toString() => 'SetWriter(var$varIndex)';
}

/// Build structure argument: place reader
class SetReader implements Op {
  final int varIndex;

  SetReader(this.varIndex);

  @override
  String toString() => 'SetReader(var$varIndex)';
}

/// Build structure argument: place constant
class SetConstant implements Op {
  final Object? value;

  SetConstant(this.value);

  @override
  String toString() => 'SetConstant($value)';
}

// ===== PROCESS/GOAL MANAGEMENT =====

/// Spawn new goal for procedure P with arguments in registers
class Spawn implements Op {
  final LabelName procedureLabel;
  final int arity;

  Spawn(this.procedureLabel, this.arity);

  @override
  String toString() => 'Spawn($procedureLabel, arity=$arity)';
}

/// Tail call to procedure P (reuses current goal frame)
class Requeue implements Op {
  final LabelName procedureLabel;
  final int arity;

  Requeue(this.procedureLabel, this.arity);

  @override
  String toString() => 'Requeue($procedureLabel, arity=$arity)';
}
