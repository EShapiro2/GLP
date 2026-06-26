/// The result of executing one instruction, PC-agnostic (B3).
///
/// Each opcode's semantics live once, in `OpExecutors` (in `bytecode/runner.dart`,
/// which holds the private clause/structure state types they touch). An executor
/// returns a [StepOutcome] instead of mutating a program counter, so the two
/// drivers can map it to their own PC world:
///  - the object loop (`runWithStatus`) maps to an instruction-index PC over
///    `BytecodeProgram.ops`;
///  - the byte loop (`engine_v2/interp.dart`) maps to a byte-offset PC over the
///    `CodeImage` code section.
///
/// This keeps one source of opcode semantics shared by both, with no second
/// in-memory instruction form — the byte loop decodes operands inline (FCP
/// style) and calls the same executor.
library;

/// What the driver must do after an instruction's executor runs.
enum StepKind {
  /// Continue to the next instruction in sequence.
  advance,

  /// Soft-fail the current clause: merge Si into U, clear clause state, and go
  /// to the next clause (object loop: `_findNextClauseTry`; byte loop: scan to
  /// the next `clause_try` byte offset).
  nextClause,

  /// `commit`: apply the tentative writer bindings and enter the body.
  commit,

  /// `proceed`: the goal's clause body is launched / the goal terminates.
  proceed,

  /// `no_more_clauses`: suspend the goal on U if non-empty, else fail it.
  noMoreClauses,

  /// The goal suspended (drivers return `RunResult.suspended`). The executor has
  /// already recorded the suspension on its blocking readers.
  suspended,

  /// The goal failed — no clause applied and none suspended (drivers return
  /// `RunResult.terminated`, which the scheduler reads as failure when no
  /// reduction occurred).
  failed,

  /// `spawn`: enqueue a new goal for [procIndex] (a `CodeImage` symbol index in
  /// the byte loop / a label in the object loop) with [arity].
  spawn,

  /// `requeue`: the bounded-tail continuation — continue this goal at [procIndex]
  /// with [arity], under the b-bounded tail discipline.
  requeue,

  /// `halt`: stop the goal.
  halt,
}

/// The PC-agnostic outcome of one instruction.
class StepOutcome {
  final StepKind kind;

  /// For [StepKind.spawn] / [StepKind.requeue]: the callee's symbol index
  /// (byte loop) or resolved via label (object loop), and its arity.
  final int? procIndex;
  final int? arity;

  const StepOutcome._(this.kind, {this.procIndex, this.arity});

  static const StepOutcome advance = StepOutcome._(StepKind.advance);
  static const StepOutcome nextClause = StepOutcome._(StepKind.nextClause);
  static const StepOutcome commit = StepOutcome._(StepKind.commit);
  static const StepOutcome proceed = StepOutcome._(StepKind.proceed);
  static const StepOutcome noMoreClauses = StepOutcome._(StepKind.noMoreClauses);
  static const StepOutcome suspended = StepOutcome._(StepKind.suspended);
  static const StepOutcome failed = StepOutcome._(StepKind.failed);
  static const StepOutcome halt = StepOutcome._(StepKind.halt);

  static StepOutcome spawn(int procIndex, int arity) =>
      StepOutcome._(StepKind.spawn, procIndex: procIndex, arity: arity);
  static StepOutcome requeue(int procIndex, int arity) =>
      StepOutcome._(StepKind.requeue, procIndex: procIndex, arity: arity);

  @override
  String toString() => 'StepOutcome(${kind.name}'
      '${procIndex != null ? ', proc=$procIndex/$arity' : ''})';
}
