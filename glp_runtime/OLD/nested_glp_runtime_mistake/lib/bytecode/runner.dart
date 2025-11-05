import 'opcodes.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';

/// Compiled bytecode program with label resolution
class BytecodeProgram {
  final List<Op> ops;
  final Map<String, int> labels;

  BytecodeProgram(this.ops) : labels = _buildLabels(ops);

  static Map<String, int> _buildLabels(List<Op> ops) {
    final labels = <String, int>{};
    for (var i = 0; i < ops.length; i++) {
      if (ops[i] is Label) {
        labels[(ops[i] as Label).name] = i;
      }
    }
    return labels;
  }
}

/// Execution context for a single goal reduction
class RunnerContext {
  final GlpRuntime rt;
  final GoalId goalId;
  final int kappa; // Entry PC for this goal
  final CallEnv env;

  // Clause-local state (cleared on each TRY)
  final Map<int, Object?> sigmaHat = {}; // σ̂w: tentative writer bindings
  final Set<int> si = {}; // Si: clause-local suspended readers

  // Goal-accumulated state (across all clauses)
  final Set<int> U = {}; // U: goal-accumulated suspended readers

  // Clause variables (indexed by varIndex)
  final Map<int, int> clauseVars = {}; // varIndex → writerId

  // Argument registers (for passing arguments to spawned goals)
  final Map<int, int> argWriters = {}; // argSlot → writerId
  final Map<int, int> argReaders = {}; // argSlot → readerId

  // Structure traversal state
  int S = 0; // Structure subterm pointer
  int mode = 0; // 0=READ, 1=WRITE

  // Body execution flag
  bool inBody = false;

  // Reduction budget (optional)
  final int? reductionBudget;
  int reductionsUsed = 0;

  RunnerContext({
    required this.rt,
    required this.goalId,
    required this.kappa,
    required this.env,
    this.reductionBudget,
  });

  /// Clear clause-local state (called on each TRY)
  void clearClause() {
    sigmaHat.clear();
    si.clear();
    clauseVars.clear();
    S = 0;
    mode = 0;
    inBody = false;
  }
}

/// Result of bytecode execution
enum RunResult {
  success,
  suspended,
  failed,
  outOfReductions,
}

/// BytecodeRunner executes GLP bytecode according to WAM/FCP semantics
class BytecodeRunner {
  final BytecodeProgram prog;

  BytecodeRunner(this.prog);

  /// Execute a goal starting from its kappa (entry PC)
  RunResult runWithStatus(RunnerContext cx) {
    var pc = cx.kappa; // Start at goal's entry point (NOT 0!)
    final debug = true;

    if (debug) {
      print('>>> TRY: Goal ${cx.goalId} at PC $pc');
    }

    while (pc < prog.ops.length) {
      // Check reduction budget
      if (cx.reductionBudget != null &&
          cx.reductionsUsed >= cx.reductionBudget!) {
        return RunResult.outOfReductions;
      }
      cx.reductionsUsed++;

      final op = prog.ops[pc];

      // Skip labels
      if (op is Label) {
        pc++;
        continue;
      }

      // === CONTROL FLOW ===

      if (op is ClauseTry) {
        cx.clearClause();
        pc++;
        continue;
      }

      if (op is Commit) {
        // Apply σ̂w to heap (commit tentative bindings)
        for (final entry in cx.sigmaHat.entries) {
          final writerId = entry.key;
          final value = entry.value;
          _bindWriter(cx.rt, writerId, value);
        }

        cx.inBody = true;

        if (debug) {
          print(
              '>>> REDUCTION: Goal ${cx.goalId} at PC $pc (commit succeeded, σ̂w has ${cx.sigmaHat.length} bindings)');
        }

        pc++;
        continue;
      }

      if (op is Proceed) {
        // Successfully complete goal
        return RunResult.success;
      }

      if (op is SuspendEnd) {
        // End of procedure - suspend if U non-empty, else fail
        if (cx.U.isNotEmpty) {
          if (debug) {
            print('>>> SUSPENSION: Goal ${cx.goalId} suspended on readers: ${cx.U}');
          }
          return RunResult.suspended;
        } else {
          if (debug) {
            print('>>> FAIL: Goal ${cx.goalId} (all clauses exhausted, U empty)');
          }
          return RunResult.failed;
        }
      }

      // === HEAD INSTRUCTIONS ===

      if (op is HeadConstant) {
        final readerId = cx.env.readers[op.argSlot];
        if (readerId == null) {
          throw Exception('HeadConstant: no reader at arg ${op.argSlot}');
        }

        final cell = cx.rt.heap.getReader(readerId);
        if (cell == null) {
          throw Exception('HeadConstant: reader $readerId not found');
        }

        if (debug && cx.goalId >= 100) {
          print(
              '  [DEBUG] HeadConstant checking reader $readerId: value=${cell.value} vs pattern=${op.value}');
        }

        // Check if reader is bound
        if (cell.value == null) {
          // Unbound reader - add to Si (suspend)
          cx.si.add(readerId);
          // Soft-fail to next clause
          pc = _findNextClauseTry(pc);
          continue;
        }

        // Reader is bound - check value
        final value = cell.value!;
        if (!_matchConstant(value, op.value)) {
          // Mismatch - soft-fail to next clause
          pc = _findNextClauseTry(pc);
          continue;
        }

        // Match succeeded
        pc++;
        continue;
      }

      if (op is HeadStructure) {
        final readerId = cx.env.readers[op.argSlot];
        if (readerId == null) {
          throw Exception('HeadStructure: no reader at arg ${op.argSlot}');
        }

        final cell = cx.rt.heap.getReader(readerId);
        if (cell == null) {
          throw Exception('HeadStructure: reader $readerId not found');
        }

        // Check if reader is bound
        if (cell.value == null) {
          // Unbound reader - add to Si (suspend)
          cx.si.add(readerId);
          // Soft-fail to next clause
          pc = _findNextClauseTry(pc);
          continue;
        }

        // Reader is bound - check if it's a structure
        final value = cell.value!;
        if (value is! StructTerm) {
          // Not a structure - fail
          pc = _findNextClauseTry(pc);
          continue;
        }

        // Check functor and arity
        if (value.functor != op.functor || value.args.length != op.arity) {
          // Mismatch - fail
          pc = _findNextClauseTry(pc);
          continue;
        }

        // Match succeeded - enter READ mode for structure traversal
        cx.mode = 0; // READ
        cx.S = 0; // Start at first argument
        // Store structure args for traversal (simplified - would need proper handling)

        pc++;
        continue;
      }

      if (op is UnifyConstant) {
        // Simplified - assumes we're in READ mode matching structure args
        // This would need full implementation with S register management
        pc++;
        continue;
      }

      if (op is HeadWriter) {
        // Extract writer from structure argument into clause variable
        // Simplified implementation
        pc++;
        continue;
      }

      if (op is HeadReader) {
        // Extract reader from structure argument
        // Simplified implementation
        pc++;
        continue;
      }

      if (op is GetVariable) {
        // Get argument and store writer in clause variable
        final readerId = cx.env.readers[op.argSlot];
        if (readerId == null) {
          throw Exception('GetVariable: no reader at arg ${op.argSlot}');
        }

        // Get paired writer for this reader
        final readerCell = cx.rt.heap.getReader(readerId);
        if (readerCell == null) {
          throw Exception('GetVariable: reader $readerId not found');
        }

        // For now, store the reader's paired writer ID
        // This is simplified - need proper writer extraction
        cx.clauseVars[op.varIndex] = readerId; // Temporary hack

        pc++;
        continue;
      }

      // === GUARD INSTRUCTIONS ===

      if (op is Otherwise) {
        // Succeeds if Si is empty (all previous clauses failed, not suspended)
        if (cx.si.isNotEmpty) {
          // Previous clauses suspended - union Si to U, fail to next clause
          cx.U.addAll(cx.si);
          pc = _findNextClauseTry(pc);
          continue;
        }

        // Si is empty - all previous clauses definitely failed, so succeed
        pc++;
        continue;
      }

      // === BODY INSTRUCTIONS ===

      if (op is PutReader) {
        // Put reader (derived from writer) into argument slot
        final writerId = cx.clauseVars[op.varIndex];
        if (writerId == null) {
          throw Exception('PutReader: clause var ${op.varIndex} not bound');
        }

        // Get paired reader for this writer
        final writerCell = cx.rt.heap.getWriter(writerId);
        if (writerCell != null) {
          cx.argReaders[op.argSlot] = writerCell.pairedReader;
        }

        pc++;
        continue;
      }

      if (op is PutWriter) {
        // Put writer into argument slot
        final value = cx.clauseVars[op.varIndex];

        if (value == null) {
          // Variable doesn't exist yet - allocate fresh writer/reader pair
          final (freshWriterId, freshReaderId) = cx.rt.heap.allocateFreshPair();
          cx.rt.heap.addWriter(WriterCell(freshWriterId, freshReaderId));
          cx.rt.heap.addReader(ReaderCell(freshReaderId));
          cx.argWriters[op.argSlot] = freshWriterId;
          cx.clauseVars[op.varIndex] = freshWriterId;
        } else {
          cx.argWriters[op.argSlot] = value;
        }

        pc++;
        continue;
      }

      if (op is Spawn) {
        // Spawn new goal with arguments from argWriters/argReaders
        final targetPc = prog.labels[op.procedureLabel];
        if (targetPc == null) {
          throw Exception('Spawn: label ${op.procedureLabel} not found');
        }

        // Create new goal with arguments
        final newGoalId = cx.rt.freshGoalId();
        final newEnv = CallEnv(
          readers: Map.from(cx.argReaders),
          writers: Map.from(cx.argWriters),
        );

        cx.rt.setGoalEnv(newGoalId, newEnv);
        cx.rt.gq.enqueue(GoalRef(newGoalId, targetPc));

        // Clear argument registers for next call
        cx.argWriters.clear();
        cx.argReaders.clear();

        pc++;
        continue;
      }

      if (op is Requeue) {
        // Tail call - reuse current goal frame with new arguments and PC
        final targetPc = prog.labels[op.procedureLabel];
        if (targetPc == null) {
          throw Exception('Requeue: label ${op.procedureLabel} not found');
        }

        // Update current goal's environment and re-enqueue at target PC
        final newEnv = CallEnv(
          readers: Map.from(cx.argReaders),
          writers: Map.from(cx.argWriters),
        );

        cx.rt.setGoalEnv(cx.goalId, newEnv);
        cx.rt.gq.enqueue(GoalRef(cx.goalId, targetPc));

        // Goal has been requeued - exit current execution
        return RunResult.success;
      }

      // Unknown opcode
      throw Exception('Unknown opcode: ${op.runtimeType}');
    }

    // Reached end of program without PROCEED or SUSP
    return RunResult.failed;
  }

  /// Find next ClauseTry instruction after current PC
  int _findNextClauseTry(int fromPc) {
    for (var i = fromPc + 1; i < prog.ops.length; i++) {
      if (prog.ops[i] is ClauseTry) return i;
      if (prog.ops[i] is SuspendEnd) return i;
    }
    return prog.ops.length;
  }

  /// Bind a writer to a value on the heap
  void _bindWriter(GlpRuntime rt, int writerId, Object? value) {
    if (value is ConstTerm) {
      rt.heap.bindWriterConst(writerId, value.value);
    } else if (value is StructTerm) {
      rt.heap.bindWriterStruct(writerId, value.functor, value.args);
    } else if (value is WriterTerm) {
      // Unification with another writer - need MGU
      // Simplified for now
    } else if (value is ReaderTerm) {
      // Binding to a reader - need to check reader value
      // Simplified for now
    }
  }

  /// Check if a term matches a constant value
  bool _matchConstant(Term term, Object? expected) {
    if (term is ConstTerm) {
      return term.value == expected;
    }
    return false;
  }
}
