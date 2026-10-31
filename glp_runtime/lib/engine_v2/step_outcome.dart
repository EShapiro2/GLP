/// The result of executing one instruction, PC-agnostic.
///
/// Each opcode's semantics live once, in `OpExecutors` (in `bytecode/runner.dart`,
/// which holds the private clause/structure state types they touch). An executor
/// returns a [StepOutcome] instead of mutating a program counter, and the byte
/// loop (`ByteRunner`, `engine_v2/interp.dart`) maps it to a byte-offset PC over
/// the `CodeImage` code section: it decodes the operands inline from the code
/// bytes and calls the executor, so there is no second in-memory instruction
/// form.
library;

/// What the driver must do after an instruction's executor runs.
enum StepKind {
  /// Continue to the next instruction in sequence.
  advance,

  /// Soft-fail the current clause: merge Si into U, clear clause state, and go
  /// to the next clause (the byte loop's `_applyNextClauseByte`, which finds
  /// the next clause-control byte offset).
  nextClause,

  /// `proceed`: the goal's clause body is launched / the goal terminates.
  proceed,

  /// The goal suspended (drivers return `RunResult.suspended`). The executor has
  /// already recorded the suspension on its blocking readers.
  suspended,

  /// The goal failed — no clause applied and none suspended (drivers return
  /// `RunResult.terminated`, which the scheduler reads as failure when no
  /// reduction occurred).
  failed,

  /// `halt`: stop the goal.
  halt,
}

/// The PC-agnostic outcome of one instruction.
class StepOutcome {
  final StepKind kind;

  const StepOutcome._(this.kind);

  static const StepOutcome advance = StepOutcome._(StepKind.advance);
  static const StepOutcome nextClause = StepOutcome._(StepKind.nextClause);
  static const StepOutcome proceed = StepOutcome._(StepKind.proceed);
  static const StepOutcome suspended = StepOutcome._(StepKind.suspended);
  static const StepOutcome failed = StepOutcome._(StepKind.failed);
  static const StepOutcome halt = StepOutcome._(StepKind.halt);

  @override
  String toString() => 'StepOutcome(${kind.name})';
}
