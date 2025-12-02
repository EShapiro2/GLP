import 'dart:async' show Timer;

import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/commit.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/system_predicates.dart';
import 'package:glp_runtime/runtime/body_kernels.dart';
import 'opcodes.dart';
import 'opcodes_v2.dart' as opv2;

enum RunResult { terminated, suspended, yielded, outOfReductions }

/// Unification mode for structure traversal (WAM-style)
enum UnifyMode { read, write }

/// Result of guard evaluation
enum GuardResult {
  success,  // Guard succeeded, continue with clause
  failure,  // Guard failed, try next clause
  suspend,  // Would suspend, but we handle this before evaluation
}

typedef LabelName = String;

class BytecodeProgram {
  final List<dynamic> ops;  // Can hold both v1 (Op) and v2 (OpV2) instructions
  final Map<LabelName, int> labels;
  BytecodeProgram(this.ops) : labels = _indexLabels(ops);
  static Map<LabelName, int> _indexLabels(List<dynamic> ops) {
    final m = <LabelName,int>{};
    for (var i = 0; i < ops.length; i++) {
      final op = ops[i];
      // Keep first occurrence of each label (for multi-clause procedures)
      if (op is Label && !m.containsKey(op.name)) {
        m[op.name] = i;
      }
    }
    return m;
  }

  /// Merge another program into this one (prepend stdlib)
  /// Returns a new BytecodeProgram with all ops from both
  BytecodeProgram merge(BytecodeProgram other) {
    final mergedOps = [...other.ops, ...ops];
    return BytecodeProgram(mergedOps);
  }

  /// Generate human-readable disassembly of bytecode
  String toDisassembly() {
    final buffer = StringBuffer();
    for (var i = 0; i < ops.length; i++) {
      buffer.writeln('PC $i: ${_instructionToString(ops[i])}');
    }
    return buffer.toString();
  }

  String _instructionToString(dynamic op) {
    // Handle v2 PutVariable (the critical one for debugging)
    if (op is opv2.PutVariable) {
      final mode = op.isReader ? 'reader' : 'writer';
      return 'PutVariable(X${op.varIndex} → A${op.argSlot}, $mode)';
    }

    // Handle other v2 instructions
    if (op is opv2.HeadVariable) {
      final mode = op.isReader ? 'reader' : 'writer';
      return 'HeadVariable(X${op.varIndex}, $mode)';
    }
    if (op is opv2.UnifyVariable) {
      final mode = op.isReader ? 'reader' : 'writer';
      return 'UnifyVariable(X${op.varIndex}, $mode)';
    }
    if (op is opv2.SetVariable) {
      final mode = op.isReader ? 'reader' : 'writer';
      return 'SetVariable(X${op.varIndex}, $mode)';
    }

    // Fallback: use toString()
    return op.toString();
  }
}

/// Goal-call environment: maps arg slots to heterogeneous Terms (VarRef, ConstTerm, StructTerm).
/// Per spec v2.16 section 1.1: argument registers hold Terms, not just variable IDs.
class CallEnv {
  final Map<int, Term> argBySlot;

  CallEnv({Map<int, Term>? args})
      : argBySlot = args ?? <int, Term>{};

  /// Get argument term at slot (A1, A2, ..., An)
  Term? arg(int slot) => argBySlot[slot];

  /// Update environment with new argument mappings (for requeue/tail calls)
  void update(Map<int, Term> newArgs) {
    argBySlot.clear();
    argBySlot.addAll(newArgs);
  }
}

/// Environment frame for permanent variables (Y registers)
/// Used by non-tail-recursive predicates to save local state across procedure calls
class EnvironmentFrame {
  final EnvironmentFrame? parent;  // Previous environment (E register)
  final int continuationPointer;   // Return address (CP register)
  final List<Object?> permanentVars; // Y1, Y2, ..., Yn permanent variables

  EnvironmentFrame({
    required this.parent,
    required this.continuationPointer,
    required int size,
  }) : permanentVars = List.filled(size, null);

  /// Get permanent variable Yi (1-indexed)
  Object? getY(int index) => permanentVars[index - 1];

  /// Set permanent variable Yi (1-indexed)
  void setY(int index, Object? value) => permanentVars[index - 1] = value;
}

class RunnerContext {
  final GlpRuntime rt;
  final int goalId;
  int kappa;  // Mutable - updated by Requeue for tail calls
  final CallEnv env;
  final Map<int, Object?> sigmaHat = <int, Object?>{}; // σ̂w: tentative writer bindings
  final Set<int> U = <int>{};        // goal-level suspension set (reader IDs)
  bool inBody = false;

  // WAM-style structure traversal state
  UnifyMode mode = UnifyMode.read;   // Current unification mode
  int S = 0;                          // Structure pointer (current position in structure)
  Object? currentStructure;           // Current structure being traversed
  final Map<int, Object?> clauseVars = {}; // Clause variable bindings (varIndex → value)

  // Parent structure context for nested structure building (single level sufficient for GLP)
  Object? parentStructure;
  int parentS = 0;
  UnifyMode parentMode = UnifyMode.read;
  Object? parentWriterId;  // Save parent's writer ID when nesting

  // Argument registers for goal calls (A1, A2, ..., An)
  // Per spec v2.16 section 1.1: heterogeneous term storage
  final Map<int, Term> argSlots = {};  // argSlot → Term (VarRef, ConstTerm, StructTerm)

  // Guard argument building mode (for pre-commit structure building)
  int? guardArgSlot;  // Target argSlot when building structure for guard argument

  // Reduction budget (null = unlimited)
  int? reductionBudget;
  int reductionsUsed = 0;

  // Environment frames for permanent variables (Y registers)
  EnvironmentFrame? E;  // Current environment pointer
  int? CP;              // Continuation pointer (return address)

  final void Function(GoalRef)? onActivation; // host log hook

  // Track spawned goals for display
  final List<String> spawnedGoals = [];

  // Track reduction for trace output
  String? goalHead;  // Formatted head goal for trace (mutable for tail calls)
  final void Function(int goalId, String head, String body)? onReduction;

  // Control trace output
  final bool showBindings;
  final bool debugOutput;

  RunnerContext({
    required this.rt,
    required this.goalId,
    required this.kappa,
    CallEnv? env,
    this.onActivation,
    this.reductionBudget,
    this.goalHead,
    this.onReduction,
    this.showBindings = true,
    this.debugOutput = false,
  }) : env = env ?? CallEnv();

  void clearClause() {
    sigmaHat.clear();
    inBody = false;
    mode = UnifyMode.read;
    S = 0;
    currentStructure = null;
    clauseVars.clear();
    guardArgSlot = null;
  }
}

class BytecodeRunner {
  final BytecodeProgram prog;
  BytecodeRunner(this.prog);

  void run(RunnerContext cx) { runWithStatus(cx); }

  /// Helper: find next ClauseTry instruction after current PC
  /// If no more ClauseTry, look for SuspendEnd/NoMoreClauses to check for suspension/failure
  int _findNextClauseTry(int fromPc) {
    for (var i = fromPc + 1; i < prog.ops.length; i++) {
      if (prog.ops[i] is ClauseNext) return i; // Find ClauseNext first (unions Si to U)
      if (prog.ops[i] is ClauseTry) return i;
      if (prog.ops[i] is SuspendEnd) return i; // Jump to SUSP to check U
      if (prog.ops[i] is NoMoreClauses) return i; // Jump to NoMoreClauses to check U
    }
    return prog.ops.length; // End of program if no more clauses or SUSP
  }

  /// Soft-fail to next clause: clear clause state, jump to next ClauseTry
  void _softFailToNextClause(RunnerContext cx, int currentPc) {
    // Clear clause-local state (σ̂w, etc.)
    // Note: U is not cleared - it accumulates across clause attempts
    cx.clearClause();
    // Jump to next clause (will be handled by returning new PC)
  }

  /// Find the final unbound variable in a chain (FCP: follow var→var bindings)
  /// If readerId's writer is bound to another unbound variable, return that variable's ID
  /// Otherwise return the original readerId
  /// derefAddr already follows the FULL chain, so we just use it once
  int _finalUnboundVar(RunnerContext cx, int readerId) {
    final wid = cx.rt.heap.writerIdForReader(readerId);
    if (wid == null) return readerId;

    final (wAddr, _) = cx.rt.heap.varTable[wid]!;
    // derefAddr follows the entire VarRef chain automatically
    final derefResult = cx.rt.heap.derefAddr(wAddr);

    if (cx.debugOutput) print('[DEBUG _finalUnboundVar] R$readerId -> W$wid -> derefResult=$derefResult');

    if (derefResult is VarRef) {
      // derefAddr returned the final unbound variable in the chain
      if (cx.debugOutput) print('[DEBUG _finalUnboundVar] Suspending on final var: ${derefResult.varId} isReader=${derefResult.isReader}');
      return derefResult.varId;
    }

    // Writer is bound to a ground term, reader is effectively bound
    if (cx.debugOutput) print('[DEBUG _finalUnboundVar] Writer bound to ground term, returning original: $readerId');
    return readerId;
  }

  /// Suspend on unbound reader: add to U and fail to next clause atomically
  /// Per spec: "add reader to U and immediately fail to next clause" is ONE operation
  int _suspendAndFail(RunnerContext cx, int readerId, int currentPc) {
    // print('[TRACE _suspendAndFail] Goal ${cx.goalId} adding R$readerId to U, failing to next clause');
//     print('  Current PC: $currentPc');
    cx.U.add(readerId);
    _softFailToNextClause(cx, currentPc);
    final nextPc = _findNextClauseTry(currentPc);
//     print('  Next PC: $nextPc');
    if (nextPc < prog.ops.length) {
//       print('  Next instruction: ${prog.ops[nextPc].runtimeType}');
    } else {
//       print('  ⚠️  Next PC beyond program end!');
    }
    return nextPc;
  }

  /// Suspend on multiple unbound readers: add all to U and fail to next clause
  int _suspendAndFailMulti(RunnerContext cx, Set<int> readerIds, int currentPc) {
    cx.U.addAll(readerIds);
    _softFailToNextClause(cx, currentPc);
    return _findNextClauseTry(currentPc);
  }

  /// Format a term for display
  static String _formatTerm(GlpRuntime rt, Term term, {bool markReaders = true}) {
    if (term is ConstTerm) {
      if (term.value == 'nil') return '[]';
      if (term.value == null) return '<null>';
      return term.value.toString();
    } else if (term is VarRef && !term.isReader) {
      final wid = term.varId;
      if (rt.heap.isWriterBound(wid)) {
        final value = rt.heap.valueOfWriter(wid);
        if (value != null) return _formatTerm(rt, value, markReaders: markReaders);
      }
      final displayId = wid >= 1000 ? wid - 1000 : wid;
      return 'X$displayId';
    } else if (term is VarRef && term.isReader) {
      final rid = term.varId;
      final wid = rt.heap.writerIdForReader(rid);
      if (wid != null && rt.heap.isWriterBound(wid)) {
        final value = rt.heap.valueOfWriter(wid);
        if (value != null) {
          // Bound reader - just return the formatted value without ?
          return _formatTerm(rt, value, markReaders: markReaders);
        }
      }
      // Unbound reader - show with ?
      final displayId = rid >= 1000 ? rid - 1000 : rid;
      return markReaders ? 'X$displayId?' : 'X$displayId';
    } else if (term is StructTerm) {
      // Special formatting for list structures
      if (term.functor == '.' && term.args.length == 2) {
        final elements = <String>[];
        var listTerm = term;
        final visited = <int>{};

        while (true) {
          if (listTerm is! StructTerm || listTerm.functor != '.') break;

          final head = listTerm.args[0];
          final tail = listTerm.args[1];

          // Format head element
          String headStr = _formatTerm(rt, head, markReaders: markReaders);

          // Check for circular reference in head (if VarRef)
          if (head is VarRef && visited.contains(head.varId)) {
            headStr = '<circular>';
          } else if (head is VarRef) {
            visited.add(head.varId);
          }

          elements.add(headStr);

          // Process tail
          if (tail is ConstTerm && (tail.value == 'nil' || tail.value == null)) {
            break; // Proper list ending
          } else if (tail is StructTerm && tail.functor == '.') {
            listTerm = tail;
          } else if (tail is VarRef) {
            // Unbound tail - improper list
            if (visited.contains(tail.varId)) {
              return '[${elements.join(', ')} | <circular>]';
            }
            visited.add(tail.varId);
            final tailStr = _formatTerm(rt, tail, markReaders: markReaders);
            return '[${elements.join(', ')} | $tailStr]';
          } else {
            // Non-list tail
            final tailStr = _formatTerm(rt, tail, markReaders: markReaders);
            return '[${elements.join(', ')} | $tailStr]';
          }
        }

        return '[${elements.join(', ')}]';
      }

      // General structure formatting
      final args = term.args.map((a) => _formatTerm(rt, a, markReaders: markReaders)).join(',');
      return '${term.functor}($args)';
    }
    return term.toString();
  }

  RunResult runWithStatus(RunnerContext cx) {
    var pc = cx.kappa;  // Start at goal's entry point (not 0!)
    final debug = false; // Set to true to enable trace

    // Print try start
    if (debug) {
//       print('>>> TRY: Goal ${cx.goalId} at PC ${cx.kappa}');
    }

    while (pc < prog.ops.length) {
      // Check reduction budget
      if (cx.reductionBudget != null && cx.reductionsUsed >= cx.reductionBudget!) {
        return RunResult.outOfReductions;
      }
      cx.reductionsUsed++;

      final op = prog.ops[pc];

      if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) {
        print('  [G${cx.goalId}] PC=$pc ${op.runtimeType} | U=${cx.U} inBody=${cx.inBody}');
      }

      if (op is Label) { pc++; continue; }
      if (op is ClauseTry) {
        if (cx.debugOutput) print('[DEBUG] PC $pc: ClauseTry - Starting new clause');
        cx.clearClause();
        pc++; continue;
      }
      if (op is GuardFail) { pc++; continue; }

      // Otherwise guard: succeeds if Si is empty (all previous clauses failed, not suspended)
      if (op is Otherwise) {
        // Otherwise succeeds only if all previous clauses definitively failed
        // If any clause suspended (U non-empty), then otherwise should also suspend
        if (cx.U.isNotEmpty) {
          // Previous clauses suspended, so this clause also suspends
          _softFailToNextClause(cx, pc);
          pc = _findNextClauseTry(pc);
          continue;
        }
        // U and Si both empty - all previous clauses definitely failed, so succeed
        pc++;
        continue;
      }

      // Push: Save structure processing state
      if (op is Push) {
        if (cx.debugOutput) print('[DEBUG] PC $pc: Push(X${op.regIndex}) - Saving state: S=${cx.S}, mode=${cx.mode}, struct=${cx.currentStructure}');
        cx.clauseVars[op.regIndex] = _StructureState(
          cx.S,
          cx.mode,
          cx.currentStructure
        );
        pc++;
        continue;
      }

      // Pop: Restore structure processing state (FCP AM semantics)
      if (op is Pop) {
        final state = cx.clauseVars[op.regIndex] as _StructureState;
        if (cx.debugOutput) print('[DEBUG] PC $pc: Pop(X${op.regIndex}) - Current nested struct: ${cx.currentStructure}');

        // FCP AM: Pop saves the built nested structure to register
        // This makes it available for subsequent UnifyWriter/UnifyVariable
        cx.clauseVars[op.regIndex] = cx.currentStructure;

        // Restore parent context
        cx.S = state.S;
        cx.mode = state.mode;
        cx.currentStructure = state.currentStructure;
        if (cx.debugOutput) print('[DEBUG] PC $pc: Pop - Restored to: S=${cx.S}, mode=${cx.mode}, struct=${cx.currentStructure}');
        if (cx.debugOutput) print('[DEBUG] PC $pc: Pop - Saved to X${op.regIndex}: ${cx.clauseVars[op.regIndex]}');
        pc++;
        continue;
      }

      // UnifyStructure: Process nested structure at S position
      if (op is UnifyStructure) {
        if (cx.debugOutput) print('[DEBUG] PC $pc: UnifyStructure(${op.functor}/${op.arity}) - mode=${cx.mode}, S=${cx.S}');
        if (cx.mode == UnifyMode.read) {
          // READ mode: Match structure at args[S]
          if (cx.currentStructure is StructTerm) {
            final parent = cx.currentStructure as StructTerm;
            if (cx.S < parent.args.length) {
              Object? value = parent.args[cx.S];

              if (cx.debugOutput) print('[DEBUG] PC $pc: UnifyStructure - Raw value at S=${cx.S}: $value (type=${value.runtimeType})');

              // CRITICAL FIX: Dereference if it's a variable reference
              // This handles metainterpreter/reduce cases where nested structures
              // come through variable bindings
              if (value is VarRef) {
                final varId = value.varId;
                if (cx.debugOutput) print('[DEBUG] PC $pc: UnifyStructure - VarRef detected: varId=$varId, isReader=${value.isReader}');
                // Check sigma-hat first (tentative bindings)
                if (cx.sigmaHat.containsKey(varId)) {
                  value = cx.sigmaHat[varId];
                  if (cx.debugOutput) print('[DEBUG] PC $pc: UnifyStructure - Dereferenced from σ̂w: $value');
                }
                // Then check heap bindings
                else if (cx.rt.heap.isBound(varId)) {
                  final boundValue = cx.rt.heap.getValue(varId);
                  if (cx.debugOutput) print('[DEBUG] PC $pc: UnifyStructure - isBound=true, getValue=$boundValue');
                  value = boundValue;
                  if (cx.debugOutput) print('[DEBUG] PC $pc: UnifyStructure - Dereferenced from heap: $value');
                }
                else {
                  if (cx.debugOutput) print('[DEBUG] PC $pc: UnifyStructure - isBound($varId)=false, VarRef is UNBOUND');
                }
              }

              if (value is StructTerm && value.functor == op.functor && value.args.length == op.arity) {
                // Match! Enter this structure
                if (cx.debugOutput) print('[DEBUG] PC $pc: UnifyStructure - MATCH! Entering nested structure: $value');
                cx.currentStructure = value;
                cx.S = 0;
              } else if (value is VarRef && !value.isReader) {
                // Mode conversion: unbound writer where structure expected
                // Following HeadStructure behavior (spec 6.1 line 254)
                // Switch to WRITE mode and build the structure
                if (cx.debugOutput) print('[DEBUG] PC $pc: UnifyStructure - MODE CONVERSION! Writer ${value.varId} → building ${op.functor}/${op.arity}');

                // Create tentative structure
                final nested = _TentativeStruct(op.functor, op.arity, List.filled(op.arity, null));

                // Record binding in σ̂w (writer will be bound to this structure at commit)
                // Store as Object? to avoid type issues (will be converted to StructTerm at commit)
                cx.sigmaHat[value.varId] = nested;

                // Switch to WRITE mode
                cx.mode = UnifyMode.write;

                // Enter the nested structure
                cx.currentStructure = nested;
                cx.S = 0;
              } else if (value is VarRef && value.isReader) {
                // Unbound reader where structure expected
                // Following three-valued unification: suspend on unbound reader
                if (cx.debugOutput) print('[DEBUG] PC $pc: UnifyStructure - SUSPEND! Unbound reader ${value.varId} where ${op.functor}/${op.arity} expected');
                cx.U.add(value.varId);
                _softFailToNextClause(cx, pc);
                pc = _findNextClauseTry(pc);
                continue;
              } else {
                // Mismatch - fail to next clause
                if (cx.debugOutput) print('[DEBUG] PC $pc: UnifyStructure - MISMATCH! Expected ${op.functor}/${op.arity}, got: $value');
                _softFailToNextClause(cx, pc);
                pc = _findNextClauseTry(pc);
                continue;
              }
            }
          }
        } else {
          // WRITE mode: Create nested structure at args[S]
          if (cx.currentStructure is _TentativeStruct) {
            final parent = cx.currentStructure as _TentativeStruct;
            final nested = _TentativeStruct(op.functor, op.arity, List.filled(op.arity, null));
            parent.args[cx.S] = nested;
            if (cx.debugOutput) print('[DEBUG] PC $pc: UnifyStructure WRITE - Created nested ${op.functor}/${op.arity} at parent.args[${cx.S}]');
            cx.currentStructure = nested;
            cx.S = 0;
          }
        }
        pc++;
        continue;
      }

      // ===== v2 UNIFIED INSTRUCTIONS =====

      // IfVariable: unified writer/reader type guard
      if (op is opv2.IfVariable) {
        final term = cx.clauseVars[op.varIndex];
        if (op.isReader) {
          // Check if it's a reader
          if (term is VarRef && term.isReader) {
            pc++;
            continue;
          } else {
            _softFailToNextClause(cx, pc);
            pc = _findNextClauseTry(pc);
            continue;
          }
        } else {
          // Check if it's a writer
          if (term is VarRef && !term.isReader) {
            pc++;
            continue;
          } else {
            _softFailToNextClause(cx, pc);
            pc = _findNextClauseTry(pc);
            continue;
          }
        }
      }

      // HeadVariable: unified writer/reader structure variable (at S position)
      if (op is opv2.HeadVariable) {
        if (cx.mode == UnifyMode.write) {
          // WRITE mode: Building a structure
          if (cx.currentStructure is _TentativeStruct) {
            final struct = cx.currentStructure as _TentativeStruct;

            // Check if this clause variable already has a value
            final existingValue = cx.clauseVars[op.varIndex];
            if (existingValue != null) {
              // Variable already bound
              if (op.isReader && existingValue is int) {
                // Reader mode with variable ID - wrap in VarRef
                struct.args[cx.S] = VarRef(existingValue, isReader: true);
              } else {
                // Use value as is
                struct.args[cx.S] = existingValue;
              }
            } else {
              // New variable - create placeholder
              final placeholder = _ClauseVar(op.varIndex, isWriter: !op.isReader);
              struct.args[cx.S] = placeholder;
              cx.clauseVars[op.varIndex] = placeholder;
            }
            cx.S++; // Advance to next arg
          }
        } else {
          // READ mode: Extract value from structure at S position
          if (cx.currentStructure is StructTerm) {
            final struct = cx.currentStructure as StructTerm;
            if (cx.S < struct.args.length) {
              final value = struct.args[cx.S];

              // Check if variable already bound
              final existingValue = cx.clauseVars[op.varIndex];
              if (existingValue != null) {
                // Need to unify
                if (existingValue != value) {
                  _softFailToNextClause(cx, pc);
                  pc = _findNextClauseTry(pc);
                  continue;
                }
              } else {
                // First occurrence - store it
                cx.clauseVars[op.varIndex] = value;
              }
              cx.S++; // Advance to next arg
            } else {
              // Structure arity mismatch - soft fail
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            }
          } else {
            // Not a structure - soft fail
            _softFailToNextClause(cx, pc);
            pc = _findNextClauseTry(pc);
            continue;
          }
        }
        pc++; continue;
      }

      // Mode selection (Arg)
      if (op is RequireWriterArg) {
        final arg = cx.env.arg(op.slot);
        if (arg == null || (arg is VarRef && arg.isReader)) {
          pc = prog.labels[op.failLabel]!; continue;
        }
        pc++; continue;
      }
      if (op is RequireReaderArg) {
        final arg = cx.env.arg(op.slot);
        if (arg == null || (arg is VarRef && !arg.isReader)) {
          pc = prog.labels[op.failLabel]!; continue;
        }
        pc++; continue;
      }

      // ===== v2.16 HEAD instructions =====
      if (op is HeadConstant) {
        final arg = _getArg(cx, op.argSlot);
        if (arg == null) { pc++; continue; } // No argument at this slot

        if (arg is VarRef && !arg.isReader) {
          // Writer VarRef: check if already bound, else record tentative binding in σ̂w
          if (cx.rt.heap.isWriterBound(arg.varId)) {
            // Already bound - check if value matches
            var value = cx.rt.heap.valueOfWriter(arg.varId);

            // Dereference VarRef chains to get actual value
            while (value is VarRef) {
              if (value.isReader) {
                final wid = cx.rt.heap.writerIdForReader(value.varId);
                if (wid != null && cx.rt.heap.isWriterBound(wid)) {
                  value = cx.rt.heap.valueOfWriter(wid);
                } else {
                  break;
                }
              } else {
                if (cx.rt.heap.isWriterBound(value.varId)) {
                  value = cx.rt.heap.valueOfWriter(value.varId);
                } else {
                  break;
                }
              }
            }

            if (value is VarRef) {
              // Unbound after dereferencing
              if (value.isReader) {
                // Unbound reader - add to U and fail to next clause
                pc = _suspendAndFail(cx, value.varId, pc);
                continue;
              } else {
                // Unbound writer - create tentative binding
                cx.sigmaHat[arg.varId] = ConstTerm(op.value);
              }
            } else if (value is ConstTerm && value.value != op.value) {
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            } else if (value is StructTerm) {
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            }
          } else {
            // Unbound writer - record tentative binding in σ̂w
            cx.sigmaHat[arg.varId] = ConstTerm(op.value);
          }
        } else if (arg is VarRef && arg.isReader) {
          // Reader VarRef: check if bound, else add to U and fail
          final wid = cx.rt.heap.writerIdForReader(arg.varId);
          if (wid == null || !cx.rt.heap.isWriterBound(wid)) {
            final suspendOnVar = _finalUnboundVar(cx, arg.varId);
            pc = _suspendAndFail(cx, suspendOnVar, pc);
            continue;
          } else {
            // Bound reader - check if value matches constant
            final value = cx.rt.heap.valueOfWriter(wid);
            if (value is ConstTerm && value.value != op.value) {
              // Value mismatch - soft fail to next clause
              if (debug) {
                print('  [DEBUG] Mismatch! Soft-failing to next clause');
              }
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            } else if (value is StructTerm && op.value != null) {
              // Structure doesn't match constant - soft fail
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            } else if (value is StructTerm && op.value == null) {
              // Structure doesn't match null [] - soft fail
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            }
            // else: values match or value is compatible, continue
          }
        } else {
          // Ground: check if value matches
          // TODO: implement proper ground term matching
        }
        pc++; continue;
      }

      if (op is HeadStructure) {
        // print('DEBUG: HeadStructure ${op.functor}/${op.arity} at argSlot ${op.argSlot}');
        // Check if argSlot refers to a clause variable (for nested structures) or argument register
        // Clause variables are used when matching extracted nested structures (argSlot >= 10 by convention)
        final bool isClauseVar = op.argSlot >= 10;
        if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadStructure: argSlot=${op.argSlot}, isClauseVar=$isClauseVar, functor=${op.functor}/${op.arity}');
        final arg = isClauseVar ? null : _getArg(cx, op.argSlot);

        if (!isClauseVar && arg == null) {
          // No argument - soft fail to next clause
          // print('DEBUG: HeadStructure - arg is null, failing to next clause');
          if (debug && cx.goalId >= 4000) print('  HeadStructure: arg is null, failing');
          _softFailToNextClause(cx, pc);
          pc = _findNextClauseTry(pc);
          continue;
        }

        if (!isClauseVar) {
          // print('DEBUG: HeadStructure - got arg from argSlot ${op.argSlot}: ${arg?.runtimeType}');
        } else {
          // print('DEBUG: HeadStructure - isClauseVar=true, checking clauseVars[${op.argSlot}]');
        }

        // For clause variables, get the value from clauseVars
        if (isClauseVar) {
          final clauseVarValue = cx.clauseVars[op.argSlot];
          if (cx.debugOutput) print('DEBUG MetaInterp: HeadStructure checking clauseVars[${op.argSlot}]: ${clauseVarValue?.runtimeType} = $clauseVarValue');
          if (clauseVarValue == null) {
            // Unbound clause variable - soft fail
            if (cx.debugOutput) print('DEBUG MetaInterp: clauseVar ${op.argSlot} is NULL, failing');
            _softFailToNextClause(cx, pc);
            pc = _findNextClauseTry(pc);
            continue;
          }

          // If clauseVarValue is a WriterTerm or ReaderId, treat it as if it came from argument
          if (clauseVarValue is int) {
            // It's a writer ID - check if bound
            final wid = clauseVarValue;
            if (cx.rt.heap.isWriterBound(wid)) {
              // Writer is bound - check if it matches
              final value = cx.rt.heap.valueOfWriter(wid);
              if (value is StructTerm && value.functor == op.functor && value.args.length == op.arity) {
                if (debug && cx.goalId >= 4000) print('  HeadStructure: clause var ${op.argSlot} = W$wid = $value, MATCH!');
                cx.currentStructure = value;
                cx.mode = UnifyMode.read;
                cx.S = 0;
                pc++; continue;
              }
              // Bound but doesn't match
              if (debug && cx.goalId >= 4000) print('  HeadStructure: clause var ${op.argSlot} = W$wid = $value, NO MATCH');
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            } else {
              // Writer is unbound - enter WRITE mode to create structure
              final struct = _TentativeStruct(op.functor, op.arity, List.filled(op.arity, null));
              cx.sigmaHat[wid] = struct;
              cx.currentStructure = struct;
              cx.mode = UnifyMode.write;
              cx.S = 0;
              pc++; continue;
            }
          } else if (clauseVarValue is VarRef && !clauseVarValue.isReader) {
            // VarRef writer - check if bound, or create tentative structure
            final wid = clauseVarValue.varId;
            if (cx.rt.heap.isWriterBound(wid)) {
              // Writer is bound - check if it matches
              final value = cx.rt.heap.valueOfWriter(wid);
              if (value is StructTerm && value.functor == op.functor && value.args.length == op.arity) {
                if (debug && cx.goalId >= 4000) print('  HeadStructure: clause var ${op.argSlot} = W$wid = $value, MATCH!');
                cx.currentStructure = value;
                cx.mode = UnifyMode.read;
                cx.S = 0;
                pc++; continue;
              }
              // Bound but doesn't match
              if (debug && cx.goalId >= 4000) print('  HeadStructure: clause var ${op.argSlot} = W$wid = $value, NO MATCH');
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            } else {
              // FIX: Unbound writer - create tentative structure in σ̂w
              if (debug && cx.goalId >= 4000) print('  HeadStructure: clause var ${op.argSlot} = W$wid (unbound), creating tentative structure');
              final struct = _TentativeStruct(op.functor, op.arity, List.filled(op.arity, null));
              cx.sigmaHat[wid] = struct;
              cx.currentStructure = struct;
              cx.mode = UnifyMode.write;
              cx.S = 0;
              pc++; continue;
            }
          } else if (clauseVarValue is VarRef && clauseVarValue.isReader) {
            // VarRef reader - dereference and check if bound to matching structure
            final rid = clauseVarValue.varId;
            if (cx.debugOutput) print('DEBUG SUSPEND: HeadStructure checking VarRef reader R$rid');
            final wid = cx.rt.heap.writerIdForReader(rid);
            if (cx.debugOutput) print('DEBUG SUSPEND: writerIdForReader(R$rid) = $wid');
            if (wid == null || !cx.rt.heap.isWriterBound(wid)) {
              // Unbound reader - add to U and soft fail
              if (cx.debugOutput) print('DEBUG SUSPEND: Reader R$rid is UNBOUND! Adding to U');
              print('  wid = $wid');
              if (wid != null) {
                print('  isWriterBound($wid) = ${cx.rt.heap.isWriterBound(wid)}');
              }
              pc = _suspendAndFail(cx, rid, pc);
              continue;
            }
            if (cx.debugOutput) print('DEBUG SUSPEND: Reader R$rid is bound to W$wid, dereferencing...');
            // Bound reader - dereference and check structure
            final rawValue = cx.rt.heap.valueOfWriter(wid);
            if (rawValue == null) {
              if (debug && cx.goalId >= 4000) print('  HeadStructure: reader $rid -> writer $wid has null value, failing');
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            }
            final value = cx.rt.heap.dereference(rawValue);
            if (debug && cx.goalId >= 4000) print('  HeadStructure: reader $rid -> writer $wid dereferenced = $value');
            if (value is StructTerm && value.functor == op.functor && value.args.length == op.arity) {
              // Match!
              if (debug && cx.goalId >= 4000) print('  HeadStructure: MATCH! Entering READ mode');
              cx.currentStructure = value;
              cx.mode = UnifyMode.read;
              cx.S = 0;
              pc++; continue;
            } else {
              // No match
              if (debug && cx.goalId >= 4000) print('  HeadStructure: NO MATCH, failing');
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            }
          } else if (clauseVarValue is StructTerm) {
            // Direct structure value (from dereferencing a bound reader)
            if (cx.debugOutput) print('DEBUG MetaInterp: StructTerm path - functor="${clauseVarValue.functor}" vs op.functor="${op.functor}"');
            if (clauseVarValue.functor == op.functor && clauseVarValue.args.length == op.arity) {
              if (cx.debugOutput) print('DEBUG MetaInterp: MATCH! Entering READ mode');
              cx.currentStructure = clauseVarValue;
              cx.mode = UnifyMode.read;
              cx.S = 0;
              pc++; continue;
            } else {
              if (cx.debugOutput) print('DEBUG MetaInterp: NO MATCH - failing to next clause');
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            }
          } else if (clauseVarValue is ConstTerm) {
            // Constant value (e.g., [] or atom) - cannot match structure
            if (debug && cx.goalId >= 4000) print('  HeadStructure: clause var ${op.argSlot} = $clauseVarValue (constant), NO MATCH');
            _softFailToNextClause(cx, pc);
            pc = _findNextClauseTry(pc);
            continue;
          }

          // Unexpected clauseVar type
          _softFailToNextClause(cx, pc);
          pc = _findNextClauseTry(pc);
          continue;
        }

        if (arg is VarRef && !arg.isReader) {
          // Writer VarRef: check if writer is already bound
          if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadStructure: arg is writer ${arg.varId}, bound=${cx.rt.heap.isWriterBound(arg.varId)}');
          if (cx.rt.heap.isWriterBound(arg.varId)) {
            // Already bound - check if matches structure
            var value = cx.rt.heap.valueOfWriter(arg.varId);
            if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadStructure: writer ${arg.varId} value = $value');

            // Dereference VarRef chains to get actual value
            while (value is VarRef) {
              if (value.isReader) {
                final wid = cx.rt.heap.writerIdForReader(value.varId);
                if (wid != null && cx.rt.heap.isWriterBound(wid)) {
                  value = cx.rt.heap.valueOfWriter(wid);
                } else {
                  break;
                }
              } else {
                if (cx.rt.heap.isWriterBound(value.varId)) {
                  value = cx.rt.heap.valueOfWriter(value.varId);
                } else {
                  break;
                }
              }
            }

            if (value is VarRef) {
              // Unbound after dereferencing
              if (value.isReader) {
                // Unbound reader - add to U and fail to next clause
                pc = _suspendAndFail(cx, value.varId, pc);
                continue;
              } else {
                // Unbound writer - enter WRITE mode
                final struct = _TentativeStruct(op.functor, op.arity, List.filled(op.arity, null));
                cx.sigmaHat[arg.varId] = struct;
                cx.currentStructure = struct;
                cx.mode = UnifyMode.write;
                cx.S = 0;
                pc++; continue;
              }
            } else if (value is StructTerm && value.functor == op.functor && value.args.length == op.arity) {
              // MATCH! Enter READ mode
              if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadStructure: MATCH! Entering READ mode');
              cx.currentStructure = value;
              cx.mode = UnifyMode.read;
              cx.S = 0;
              pc++; continue;
            } else {
              // No match - soft fail
              if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadStructure: NO MATCH, soft failing');
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            }
          }
          // Unbound writer - WRITE mode: create tentative structure for writer
          if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadStructure: WRITE mode for unbound writer ${arg.varId}');
          final struct = _TentativeStruct(op.functor, op.arity, List.filled(op.arity, null));
          cx.sigmaHat[arg.varId] = struct;
          cx.currentStructure = struct;
          cx.mode = UnifyMode.write;
          cx.S = 0; // Start at first arg
          pc++; continue;
        }

        if (arg is VarRef && arg.isReader) {
          // Reader VarRef: check if bound and has matching structure
          final wid = cx.rt.heap.writerIdForReader(arg.varId);
          if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadStructure: READ mode, reader ${arg.varId} -> writer $wid');
          if (wid == null || !cx.rt.heap.isWriterBound(wid)) {
            // Unbound reader - add to U and soft fail
            if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadStructure: writer $wid unbound or null, adding to U and failing');
            final suspendOnVar = _finalUnboundVar(cx, arg.varId);
            pc = _suspendAndFail(cx, suspendOnVar, pc);
            continue;
          }

          // Bound reader - dereference fully and check if it's a matching structure
          final rawValue = cx.rt.heap.valueOfWriter(wid);
          if (rawValue == null) {
            // Null value - should not happen for bound writer
            if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadStructure: writer $wid has null value, failing');
            _softFailToNextClause(cx, pc);
            pc = _findNextClauseTry(pc);
            continue;
          }
          // Dereference recursively in case value is a VarRef chain
          final value = cx.rt.heap.dereference(rawValue);
          // print('DEBUG: HeadStructure - Writer $wid value = ${value.runtimeType}: $value, expecting ${op.functor}/${op.arity}');
          if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadStructure: writer $wid dereferenced value = $value, expecting ${op.functor}/${op.arity}');
          if (value is StructTerm && value.functor == op.functor && value.args.length == op.arity) {
            // Matching structure - enter READ mode
            if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadStructure: MATCH! Entering READ mode');
            cx.currentStructure = value;
            cx.mode = UnifyMode.read;
            cx.S = 0;
            pc++; continue;
          } else {
            // Non-matching structure or not a structure - soft fail
            if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadStructure: NO MATCH, failing');
            _softFailToNextClause(cx, pc);
            pc = _findNextClauseTry(pc);
            continue;
          }
        }

        // Ground term case (not writer or reader)
        if (arg is StructTerm) {
          // StructTerm argument - check if it matches and enter READ mode
          if (arg.functor == op.functor && arg.args.length == op.arity) {
            // Match! Enter READ mode to unify structure arguments
            if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadStructure: StructTerm arg matches ${op.functor}/${op.arity}, entering READ mode');
            cx.currentStructure = arg;
            cx.mode = UnifyMode.read;
            cx.S = 0;
            pc++; continue;
          } else {
            // Functor/arity mismatch - soft fail
            if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadStructure: StructTerm arg mismatch, failing');
            _softFailToNextClause(cx, pc);
            pc = _findNextClauseTry(pc);
            continue;
          }
        }

        // Other ground terms (ConstTerm, etc.) - fail
        if (cx.debugOutput) print('DEBUG: HeadStructure line 610 - GROUND TERM PATH (non-struct)');
        print('  op.argSlot = ${op.argSlot}');
        print('  arg = $arg');
        print('  arg is VarRef = ${arg is VarRef}');
        print('  isClauseVar = $isClauseVar');
        if (isClauseVar && op.argSlot < 100) {
          // print('  clauseVars[${op.argSlot}] = ${cx.clauseVars[op.argSlot]}');
        }
        _softFailToNextClause(cx, pc);
        pc = _findNextClauseTry(pc);
        continue;
      }

      // ===== Argument loading instructions (GET class) =====
      if (op is GetVariable) {
        // Load argument into clause variable (first occurrence)
        final arg = _getArg(cx, op.argSlot);
        if (arg == null) {
          // No argument provided
          _softFailToNextClause(cx, pc);
          pc = _findNextClauseTry(pc);
          continue;
        }

        // Store argument value in clauseVars
        if (arg is VarRef && !arg.isReader) {
          cx.clauseVars[op.varIndex] = arg.varId;
        } else if (arg is VarRef && arg.isReader) {
          // Reader: check if bound
          final wid = cx.rt.heap.writerIdForReader(arg.varId);
          if (wid == null || !cx.rt.heap.isWriterBound(wid)) {
            // Unbound reader - add to U and fail to next clause
            pc = _suspendAndFail(cx, arg.varId, pc);
            continue;
          }
          // Bound reader - store the writer ID for dereferencing
          cx.clauseVars[op.varIndex] = wid;
        } else if (arg is ConstTerm || arg is StructTerm) {
          // Ground term - store directly
          cx.clauseVars[op.varIndex] = arg;
        } else {
          _softFailToNextClause(cx, pc);
          pc = _findNextClauseTry(pc);
          continue;
        }
        pc++; continue;
      }

      if (op is GetValue) {
        // Unify argument with clause variable (subsequent occurrence)
        final arg = _getArg(cx, op.argSlot);
        if (arg == null) {
          _softFailToNextClause(cx, pc);
          pc = _findNextClauseTry(pc);
          continue;
        }

        // Get the previously stored value
        final storedValue = cx.clauseVars[op.varIndex];
        if (storedValue == null) {
          // Variable not initialized - error
          _softFailToNextClause(cx, pc);
          pc = _findNextClauseTry(pc);
          continue;
        }

        // Unify argument with stored value
        if (arg is VarRef && !arg.isReader) {
          // Argument is writer VarRef - bind it to stored value in σ̂w
          if (storedValue is VarRef && !storedValue.isReader) {
            // storedValue is a writer VarRef - check they match
            if (arg.varId != storedValue.varId) {
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            }
          } else if (storedValue is int) {
            // Legacy: bare writer ID - check they match
            if (arg.varId != storedValue) {
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            }
          } else if (storedValue is VarRef && storedValue.isReader) {
            // storedValue is a reader (e.g., Xs?) - bind writer to reader's value
            final readerId = storedValue.varId;
            final wid = cx.rt.heap.writerIdForReader(readerId);
            if (wid != null && cx.rt.heap.isWriterBound(wid)) {
              // Reader's writer is bound - bind arg writer to that value
              final readerValue = cx.rt.heap.valueOfWriter(wid);
              cx.sigmaHat[arg.varId] = readerValue;
            } else {
              // Reader's writer is unbound - add reader to Si (suspend)
              pc = _suspendAndFail(cx, readerId, pc); continue;
            }
          } else {
            // storedValue is a Term - bind writer to it
            cx.sigmaHat[arg.varId] = storedValue;
          }
        } else if (arg is VarRef && arg.isReader) {
          // Argument is reader VarRef - verify it matches stored value
          if (storedValue is VarRef && storedValue.isReader) {
            // storedValue is also a reader - fail definitively
            _softFailToNextClause(cx, pc);
            pc = _findNextClauseTry(pc);
            continue;
          }

          final wid = cx.rt.heap.writerIdForReader(arg.varId);
          if (wid != null && cx.rt.heap.isWriterBound(wid)) {
            // Reader is bound - check value matches
            final readerValue = cx.rt.heap.valueOfWriter(wid);
            if (storedValue is Term) {
              if (readerValue != storedValue) {
                _softFailToNextClause(cx, pc);
                pc = _findNextClauseTry(pc);
                continue;
              }
            } else if (storedValue is int && wid != storedValue) {
              // storedValue is a writer ID
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            }
          } else if (storedValue is int) {
            // Reader unbound, storedValue is writer ID - check they match
            if (wid != storedValue) {
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            }
          } else {
            // Reader unbound, storedValue is a Term - add to Si
            final suspendOnVar = _finalUnboundVar(cx, arg.varId);
            pc = _suspendAndFail(cx, suspendOnVar, pc); continue;
          }
        } else {
          // Ground term - TODO: handle ConstTerm/StructTerm
          _softFailToNextClause(cx, pc);
          pc = _findNextClauseTry(pc);
          continue;
        }
        pc++; continue;
      }

      // ===== Structure subterm matching instructions =====
      if (op is UnifyConstant) {
        // Match constant at current S position
        if (cx.mode == UnifyMode.write) {
          // WRITE mode: Add constant to structure being built
          if (cx.currentStructure is _TentativeStruct) {
            final struct = cx.currentStructure as _TentativeStruct;
            struct.args[cx.S] = op.value;
            cx.S++; // Advance to next arg

            // Check if structure is complete
            if (cx.S >= struct.args.length) {
              // Structure complete - bind the target writer (stored at clauseVars[-1])
              final targetWriterId = cx.clauseVars[-1];
              if (targetWriterId is int) {
                // Convert args to Terms
                final termArgs = <Term>[];
                for (final arg in struct.args) {
                  if (arg is Term) {
                    termArgs.add(arg);
                  } else {
                    termArgs.add(ConstTerm(arg));
                  }
                }
                // Bind the writer to the completed structure
                cx.rt.heap.bindWriterStruct(targetWriterId, struct.functor, termArgs);

                // Reset structure building state
                cx.currentStructure = null;
                cx.mode = UnifyMode.read;
                cx.S = 0;
                cx.clauseVars.remove(-1);
              }
            }
          } else if (cx.currentStructure is StructTerm) {
            // Structure building (BODY or guard argument)
            final struct = cx.currentStructure as StructTerm;
            // If value is already a Term (e.g., StructTerm), use it directly
            // Otherwise wrap in ConstTerm
            struct.args[cx.S] = op.value is Term ? op.value as Term : ConstTerm(op.value);
            cx.S++; // Advance to next arg

            // Check if structure is complete
            if (cx.S >= struct.args.length) {
              // Check if we're in guard argument building mode (pre-commit)
              if (cx.guardArgSlot != null) {
                // Guard argument mode: store structure directly in argSlots
                // No heap binding needed - just temporary for guard call
                cx.argSlots[cx.guardArgSlot!] = struct;
                cx.currentStructure = null;
                cx.mode = UnifyMode.read;
                cx.S = 0;
                cx.guardArgSlot = null;
              } else {
                // BODY phase: bind the target writer (stored at clauseVars[-1])
                final targetWriterId = cx.clauseVars[-1];
                if (targetWriterId is int) {
                  // Bind the writer to the completed structure
                  cx.rt.heap.bindWriterStruct(targetWriterId, struct.functor, struct.args);

                  // Put the structure reference into argSlots if we have a target slot
                  // PutStructure stores target slot in clauseVars[-2] for slots 0-9
                  final targetSlot = cx.clauseVars[-2];
                  if (targetSlot is int && targetSlot >= 0 && targetSlot < 10) {
                    // Put a reader reference to the structure in the target arg slot
                    cx.argSlots[targetSlot] = VarRef(targetWriterId, isReader: true);
                    cx.clauseVars.remove(-2);
                  }

                  // Reset structure building state
                  cx.currentStructure = null;
                  cx.mode = UnifyMode.read;
                  cx.S = 0;
                  cx.clauseVars.remove(-1);
                }
              }
            }
          }
        } else {
          // READ mode: Verify value at S position matches constant
          if (cx.currentStructure is StructTerm) {
            final struct = cx.currentStructure as StructTerm;
            if (cx.S < struct.args.length) {
              final value = struct.args[cx.S];
              // print('DEBUG: UnifyConstant - S=${cx.S}, value=$value (${value.runtimeType}), expecting ${op.value}');
              if (debug && cx.goalId >= 4000) print('  UnifyConstant: S=${cx.S}, value=$value (${value.runtimeType}), expecting ${op.value}');

              if (value is ConstTerm && value.value == op.value) {
                // Constant matches - advance
                cx.S++;
              } else if (value is VarRef && !value.isReader) {
                // Writer variable - bind to constant in σ̂w
                final wid = value.varId;
                if (cx.rt.heap.isWriterBound(wid)) {
                  // Already bound - check if it matches
                  final boundValue = cx.rt.heap.valueOfWriter(wid);
                  if (boundValue is ConstTerm && boundValue.value == op.value) {
                    cx.S++; // Match successful
                  } else {
                    // Bound to different value - fail
                    if (debug && cx.goalId >= 4000) print('  UnifyConstant: writer already bound to $boundValue, failing');
                    _softFailToNextClause(cx, pc);
                    pc = _findNextClauseTry(pc);
                    continue;
                  }
                } else {
                  // Unbound writer - add tentative binding to σ̂w
                  if (debug && cx.goalId >= 4000) print('  UnifyConstant: binding writer $wid to ${op.value} in σ̂w');
                  cx.sigmaHat[wid] = ConstTerm(op.value);
                  cx.S++;
                }
              } else if (value is VarRef && value.isReader) {
                // Reader variable - check if bound, else suspend
                final rid = value.varId;
                final wid = cx.rt.heap.writerIdForReader(rid);
                if (wid != null && cx.rt.heap.isWriterBound(wid)) {
                  // Reader is bound - check if it matches
                  final boundValue = cx.rt.heap.valueOfWriter(wid);
                  if (boundValue is ConstTerm && boundValue.value == op.value) {
                    if (debug && cx.goalId >= 4000) print('  UnifyConstant: reader $rid bound to $boundValue, matches!');
                    cx.S++; // Match successful
                  } else {
                    // Bound to different value - fail
                    if (debug && cx.goalId >= 4000) print('  UnifyConstant: reader $rid bound to $boundValue, mismatch');
                    _softFailToNextClause(cx, pc);
                    pc = _findNextClauseTry(pc);
                    continue;
                  }
                } else {
                  // Unbound reader - add to Si (suspend)
                  if (debug && cx.goalId >= 4000) print('  UnifyConstant: reader $rid unbound, suspending');
                  pc = _suspendAndFail(cx, rid, pc); continue;
                  cx.S++;
                }
              } else {
                // Mismatch - soft fail
                if (debug && cx.goalId >= 4000) print('  UnifyConstant: MISMATCH, failing');
                _softFailToNextClause(cx, pc);
                pc = _findNextClauseTry(pc);
                continue;
              }
            } else {
              // Structure arity mismatch - soft fail
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            }
          } else {
            // Not a structure - soft fail
            _softFailToNextClause(cx, pc);
            pc = _findNextClauseTry(pc);
            continue;
          }
        }
        pc++; continue;
      }

      if (op is UnifyVoid) {
        // Skip/create void (anonymous) variables
        if (cx.mode == UnifyMode.write) {
          // WRITE mode: Create fresh unbound variables
          if (cx.currentStructure is _TentativeStruct) {
            final struct = cx.currentStructure as _TentativeStruct;
            for (var i = 0; i < op.count && cx.S < struct.args.length; i++) {
              struct.args[cx.S] = null; // Void/unbound
              cx.S++;
            }
          }
        } else {
          // READ mode: Skip over positions
          cx.S += op.count;
        }
        pc++; continue;
      }

      // UnifyVariable: unified writer/reader structure traversal (native V2 handler)
      if (op is opv2.UnifyVariable) {
        final varIndex = op.varIndex;
        final isReaderMode = op.isReader;

        if (cx.mode == UnifyMode.write) {
          // WRITE mode: Add variable to structure being built
          if (cx.currentStructure is _TentativeStruct) {
            // HEAD phase tentative structure
            final struct = cx.currentStructure as _TentativeStruct;
            final clauseVarValue = cx.clauseVars[varIndex];

            if (clauseVarValue is VarRef) {
              // Subsequent use: extract varId, create VarRef with appropriate mode
              struct.args[cx.S] = VarRef(clauseVarValue.varId, isReader: isReaderMode);
            } else if (clauseVarValue is int) {
              // Bare varId - create VarRef with appropriate mode
              struct.args[cx.S] = VarRef(clauseVarValue, isReader: isReaderMode);
            } else if (clauseVarValue is Term) {
              if (isReaderMode) {
                // Reader mode with ground term: create fresh var, bind tentatively
                final varId = cx.rt.heap.allocateFreshVar();
                cx.rt.heap.addVariable(varId);
                cx.sigmaHat[varId] = clauseVarValue;
                struct.args[cx.S] = VarRef(varId, isReader: true);
              } else {
                // Writer mode: use ground term directly
                struct.args[cx.S] = clauseVarValue;
              }
            } else if (clauseVarValue is _TentativeStruct) {
              // Nested tentative structure
              struct.args[cx.S] = clauseVarValue;
            } else if (clauseVarValue == null) {
              // First occurrence - allocate fresh variable
              final varId = cx.rt.heap.allocateFreshVar();
              cx.rt.heap.addVariable(varId);
              // Store WRITER in clauseVars (base variable)
              cx.clauseVars[varIndex] = VarRef(varId, isReader: false);
              // Store with requested mode in structure
              struct.args[cx.S] = VarRef(varId, isReader: isReaderMode);
            } else {
              // Fallback: use _ClauseVar placeholder
              struct.args[cx.S] = _ClauseVar(varIndex, isWriter: !isReaderMode);
            }
            cx.S++;

          } else if (cx.currentStructure is StructTerm) {
            // BODY phase structure building
            final struct = cx.currentStructure as StructTerm;
            final clauseVarValue = cx.clauseVars[varIndex];

            if (clauseVarValue is VarRef) {
              // Subsequent use: create VarRef with appropriate mode
              struct.args[cx.S] = VarRef(clauseVarValue.varId, isReader: isReaderMode);
            } else if (clauseVarValue is int) {
              // Bare varId (from get_reader_variable etc.) - create VarRef with requested mode
              struct.args[cx.S] = VarRef(clauseVarValue, isReader: isReaderMode);
            } else if (clauseVarValue is Term) {
              if (isReaderMode) {
                // Reader mode with ground term: create fresh var, bind it
                final varId = cx.rt.heap.allocateFreshVar();
                cx.rt.heap.addVariable(varId);
                cx.rt.heap.bindVariable(varId, clauseVarValue);
                struct.args[cx.S] = VarRef(varId, isReader: true);
              } else {
                // Writer mode: use ground term directly
                struct.args[cx.S] = clauseVarValue;
              }
            } else if (clauseVarValue == null) {
              // First occurrence - allocate fresh variable
              final varId = cx.rt.heap.allocateFreshVar();
              cx.rt.heap.addVariable(varId);
              cx.clauseVars[varIndex] = VarRef(varId, isReader: false);
              struct.args[cx.S] = VarRef(varId, isReader: isReaderMode);
            }
            cx.S++;

            // Check if structure is complete
            if (cx.S >= struct.args.length) {
              // Check if we're in guard argument building mode (pre-commit)
              if (cx.guardArgSlot != null) {
                // Guard argument mode: store structure directly in argSlots
                // No heap binding needed - just temporary for guard call
                cx.argSlots[cx.guardArgSlot!] = struct;
                cx.currentStructure = null;
                cx.mode = UnifyMode.read;
                cx.S = 0;
                cx.guardArgSlot = null;
              } else {
                // BODY phase: bind to heap writer
                final targetValue = cx.clauseVars[-1];
                int? targetWriterId;
                if (targetValue is VarRef) {
                  targetWriterId = targetValue.varId;
                } else if (targetValue is int) {
                  targetWriterId = targetValue;
                }

                if (targetWriterId != null) {
                  cx.rt.heap.bindWriterStruct(targetWriterId, struct.functor, struct.args);

                  // Store VarRef to bound writer in argSlots
                  final targetSlot = cx.clauseVars[-2];
                  if (targetSlot is int && targetSlot >= 0 && targetSlot < 10) {
                    cx.argSlots[targetSlot] = VarRef(targetWriterId, isReader: true);
                    cx.clauseVars.remove(-2);
                  }

                  cx.currentStructure = null;
                  cx.mode = UnifyMode.read;
                  cx.S = 0;
                  cx.clauseVars.remove(-1);
                }
              }
            }
          }
        } else {
          // READ mode: Unify with value at S position
          if (cx.currentStructure is StructTerm) {
            final struct = cx.currentStructure as StructTerm;
            if (cx.S < struct.args.length) {
              final value = struct.args[cx.S];
              final existingValue = cx.clauseVars[varIndex];

              if (isReaderMode) {
                // UnifyReader READ mode logic
                if (value is VarRef && value.isReader) {
                  // Query has reader, clause expects reader
                  final rid = value.varId;
                  final wid = cx.rt.heap.writerIdForReader(rid);
                  if (wid != null) {
                    cx.clauseVars[varIndex] = wid;
                  }
                  cx.S++;
                } else if (value is VarRef && !value.isReader) {
                  // Query has writer, clause expects reader
                  if (existingValue != null) {
                    // Xi already allocated from previous writer occurrence
                    // Bind query writer to existing value (per spec 8.2)
                    if (existingValue is ConstTerm || existingValue is StructTerm) {
                      // Ground value - bind writer directly to it
                      cx.sigmaHat[value.varId] = existingValue;
                    } else if (existingValue is VarRef) {
                      // Existing VarRef - bind writer to reader of it
                      cx.sigmaHat[value.varId] = VarRef(existingValue.varId, isReader: true);
                    } else if (existingValue is int) {
                      // Bare varId - bind writer to reader of it
                      cx.sigmaHat[value.varId] = VarRef(existingValue, isReader: true);
                    }
                    cx.S++;
                  } else {
                    // First occurrence - allocate fresh variable
                    final freshVar = cx.rt.heap.allocateFreshVar();
                    cx.rt.heap.addVariable(freshVar);
                    final readerRef = VarRef(freshVar, isReader: true);
                    cx.sigmaHat[value.varId] = readerRef;
                    cx.clauseVars[varIndex] = VarRef(freshVar, isReader: false);
                    cx.S++;
                  }
                } else if (value is ConstTerm || value is StructTerm) {
                  // Query has ground term, clause expects reader
                  final freshVar = cx.rt.heap.allocateFreshVar();
                  cx.rt.heap.addVariable(freshVar);
                  cx.sigmaHat[freshVar] = value;
                  cx.clauseVars[varIndex] = freshVar;
                  cx.S++;
                } else {
                  _softFailToNextClause(cx, pc);
                  pc = _findNextClauseTry(pc);
                  continue;
                }
              } else {
                // UnifyWriter READ mode logic
                if (existingValue is int || (existingValue is VarRef && !existingValue.isReader)) {
                  // Clause variable is a fresh variable ID from previous UnifyReader
                  final clauseVarId = existingValue is int ? existingValue : (existingValue as VarRef).varId;

                  if (value is VarRef && !value.isReader) {
                    // Query has writer - check for WxW violation
                    final clauseVarBound = cx.rt.heap.isWriterBound(clauseVarId);
                    final queryVarBound = cx.rt.heap.isWriterBound(value.varId);
                    if (!clauseVarBound && !queryVarBound) {
                      _softFailToNextClause(cx, pc);
                      pc = _findNextClauseTry(pc);
                      continue;
                    }
                    cx.sigmaHat[clauseVarId] = value;
                    cx.S++;
                  } else if (value is VarRef && value.isReader) {
                    cx.sigmaHat[clauseVarId] = value;
                    cx.S++;
                  } else if (value is ConstTerm || value is StructTerm) {
                    cx.sigmaHat[clauseVarId] = value;
                    cx.S++;
                  } else {
                    _softFailToNextClause(cx, pc);
                    pc = _findNextClauseTry(pc);
                    continue;
                  }
                } else if (existingValue != null) {
                  // Clause variable already bound - advance
                  cx.S++;
                } else {
                  // First occurrence - store the value
                  if (value is VarRef && !value.isReader) {
                    cx.clauseVars[varIndex] = value;
                    cx.S++;
                  } else if (value is VarRef && value.isReader) {
                    final rid = value.varId;
                    final wid = cx.rt.heap.writerIdForReader(rid);
                    if (wid != null && cx.rt.heap.isWriterBound(wid)) {
                      final writerValue = cx.rt.heap.valueOfWriter(wid);
                      cx.clauseVars[varIndex] = writerValue;
                    } else {
                      cx.clauseVars[varIndex] = value;
                    }
                    cx.S++;
                  } else if (value is ConstTerm || value is StructTerm) {
                    cx.clauseVars[varIndex] = value;
                    cx.S++;
                  } else {
                    _softFailToNextClause(cx, pc);
                    pc = _findNextClauseTry(pc);
                    continue;
                  }
                }
              }
            }
          }
        }
        pc++; continue;
      }

      // GetVariable: unified first-occurrence argument loading (native V2 handler)
      if (op is opv2.GetVariable) {
        final varIndex = op.varIndex;
        final argSlot = op.argSlot;
        final isReaderMode = op.isReader;

        final arg = _getArg(cx, argSlot);
        if (arg == null) {
          _softFailToNextClause(cx, pc);
          pc = _findNextClauseTry(pc);
          continue;
        }

        if (!isReaderMode) {
          // GetWriterVariable logic: Load argument into clause WRITER variable
          // IMPORTANT: Check if clauseVars[varIndex] already has a writer from
          // an earlier occurrence (e.g., inside a structure via UnifyVariable).
          // If so, bind that writer to the argument value via sigmaHat.
          final existing = cx.clauseVars[varIndex];

          if (arg is VarRef && !arg.isReader) {
            if (existing is VarRef && !existing.isReader) {
              // Both are writers - bind arg writer to existing writer's reader
              cx.sigmaHat[arg.varId] = VarRef(existing.varId, isReader: true);
            } else if (existing is int) {
              // existing is bare writer varId - bind arg to reader of it
              cx.sigmaHat[arg.varId] = VarRef(existing, isReader: true);
            } else {
              cx.clauseVars[varIndex] = arg.varId;
            }
          } else if (arg is VarRef && arg.isReader) {
            final wid = cx.rt.heap.writerIdForReader(arg.varId);
            if (wid != null && cx.rt.heap.isWriterBound(wid)) {
              final value = cx.rt.heap.valueOfWriter(wid);
              if (existing is VarRef && !existing.isReader) {
                cx.sigmaHat[existing.varId] = value;
              } else if (existing is int) {
                cx.sigmaHat[existing] = value;
              } else {
                cx.clauseVars[varIndex] = value;
              }
            } else {
              final suspendOnVar = _finalUnboundVar(cx, arg.varId);
              pc = _suspendAndFail(cx, suspendOnVar, pc); continue;
            }
          } else if (arg is ConstTerm) {
            if (existing is VarRef && !existing.isReader) {
              // Already have a writer from earlier occurrence - bind it
              cx.sigmaHat[existing.varId] = arg;
            } else if (existing is int) {
              // Bare writer varId - bind it
              cx.sigmaHat[existing] = arg;
            } else {
              cx.clauseVars[varIndex] = arg;
            }
          } else if (arg is StructTerm) {
            if (existing is VarRef && !existing.isReader) {
              cx.sigmaHat[existing.varId] = arg;
            } else if (existing is int) {
              cx.sigmaHat[existing] = arg;
            } else {
              cx.clauseVars[varIndex] = arg;
            }
          }
        } else {
          // GetReaderVariable logic: Load argument into clause READER variable
          if (arg is VarRef && !arg.isReader) {
            // Writer VarRef → reader param (mode conversion)
            final freshVar = cx.rt.heap.allocateFreshVar();
            cx.rt.heap.addVariable(freshVar);
            cx.sigmaHat[arg.varId] = VarRef(freshVar, isReader: true);
            cx.clauseVars[varIndex] = freshVar;
          } else if (arg is VarRef && arg.isReader) {
            cx.clauseVars[varIndex] = arg.varId;
          } else if (arg is ConstTerm) {
            cx.clauseVars[varIndex] = arg;
          } else if (arg is StructTerm) {
            cx.clauseVars[varIndex] = arg;
          }
        }
        pc++; continue;
      }

      // GetValue: unified subsequent-occurrence argument unification (native V2 handler)
      if (op is opv2.GetValue) {
        final varIndex = op.varIndex;
        final argSlot = op.argSlot;
        final isReaderMode = op.isReader;

        final arg = _getArg(cx, argSlot);
        if (arg == null) {
          _softFailToNextClause(cx, pc);
          pc = _findNextClauseTry(pc);
          continue;
        }

        var storedValue = cx.clauseVars[varIndex];
        if (storedValue == null) {
          _softFailToNextClause(cx, pc);
          pc = _findNextClauseTry(pc);
          continue;
        }

        if (!isReaderMode) {
          // GetWriterValue logic: Unify argument with clause WRITER variable
          // Check if storedValue is a reader ID
          if (storedValue is int) {
            final wid = cx.rt.heap.writerIdForReader(storedValue);
            if (wid != null) {
              storedValue = wid;
            }
          }

          if (arg is VarRef && !arg.isReader) {
            final argBound = cx.rt.heap.isWriterBound(arg.varId);
            if (argBound) {
              final argValue = cx.rt.heap.valueOfWriter(arg.varId);
              if (storedValue is int) {
                final storedBound = cx.rt.heap.isWriterBound(storedValue);
                if (storedBound) {
                  final storedVal = cx.rt.heap.valueOfWriter(storedValue);
                  bool match = false;
                  if (argValue is ConstTerm && storedVal is ConstTerm) {
                    match = argValue.value == storedVal.value;
                  } else if (argValue is StructTerm && storedVal is StructTerm) {
                    match = argValue.functor == storedVal.functor && argValue.args.length == storedVal.args.length;
                  } else {
                    match = argValue == storedVal;
                  }
                  if (!match) {
                    _softFailToNextClause(cx, pc);
                    pc = _findNextClauseTry(pc);
                    continue;
                  }
                } else {
                  cx.sigmaHat[storedValue] = argValue;
                }
              } else if (storedValue is Term) {
                bool match = false;
                if (argValue is ConstTerm && storedValue is ConstTerm) {
                  match = argValue.value == storedValue.value;
                } else if (argValue is StructTerm && storedValue is StructTerm) {
                  match = argValue.functor == storedValue.functor && argValue.args.length == storedValue.args.length;
                } else {
                  match = argValue == storedValue;
                }
                if (!match) {
                  _softFailToNextClause(cx, pc);
                  pc = _findNextClauseTry(pc);
                  continue;
                }
              }
            } else {
              if (storedValue is int) {
                final freshVarBinding = cx.sigmaHat[storedValue];
                if (freshVarBinding != null) {
                  cx.sigmaHat[arg.varId] = freshVarBinding;
                } else if (arg.varId != storedValue) {
                  _softFailToNextClause(cx, pc);
                  pc = _findNextClauseTry(pc);
                  continue;
                }
              } else if (storedValue is Term) {
                cx.sigmaHat[arg.varId] = storedValue;
              }
            }
          } else if (arg is VarRef && arg.isReader) {
            final wid = cx.rt.heap.writerIdForReader(arg.varId);
            if (wid != null && cx.rt.heap.isWriterBound(wid)) {
              final readerValue = cx.rt.heap.valueOfWriter(wid);
              if (storedValue is int) {
                cx.sigmaHat[storedValue] = readerValue;
              } else if (storedValue != readerValue) {
                _softFailToNextClause(cx, pc);
                pc = _findNextClauseTry(pc);
                continue;
              }
            } else {
              final suspendOnVar = _finalUnboundVar(cx, arg.varId);
              pc = _suspendAndFail(cx, suspendOnVar, pc); continue;
            }
          } else if (arg is ConstTerm) {
            if (storedValue is int) {
              cx.sigmaHat[storedValue] = arg;
            } else if (storedValue is ConstTerm && storedValue.value != arg.value) {
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            }
          } else if (arg is StructTerm) {
            if (storedValue is int) {
              cx.sigmaHat[storedValue] = arg;
            } else if (storedValue is StructTerm && storedValue.functor != arg.functor) {
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            }
          }
        } else {
          // GetReaderValue logic: Unify argument with clause READER variable
          if (arg is VarRef && !arg.isReader) {
            if (storedValue is int) {
              final wid = cx.rt.heap.writerIdForReader(storedValue);
              if (wid != null && cx.rt.heap.isWriterBound(wid)) {
                final readerValue = cx.rt.heap.valueOfWriter(wid);
                cx.sigmaHat[arg.varId] = readerValue;
              } else {
                pc = _suspendAndFail(cx, storedValue, pc); continue;
              }
            } else if (storedValue is Term) {
              cx.sigmaHat[arg.varId] = storedValue;
            }
          } else if (arg is VarRef && arg.isReader) {
            if (storedValue is int && arg.varId != storedValue) {
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            }
          } else if (arg is ConstTerm || arg is StructTerm) {
            if (storedValue != arg) {
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            }
          }
        }
        pc++; continue;
      }

      // SetVariable: unified structure building in BODY (native V2 handler)
      if (op is opv2.SetVariable) {
        final varIndex = op.varIndex;
        final isReaderMode = op.isReader;

        if (cx.inBody && cx.mode == UnifyMode.write && cx.currentStructure is StructTerm) {
          // Check what value exists in clause variables
          final existingValue = cx.clauseVars[varIndex];
          final struct = cx.currentStructure as StructTerm;

          if (existingValue is VarRef) {
            // VarRef: use its varId with appropriate mode
            struct.args[cx.S] = VarRef(existingValue.varId, isReader: isReaderMode);
          } else if (existingValue is int) {
            // Legacy: bare int (use it as varId directly)
            struct.args[cx.S] = VarRef(existingValue, isReader: isReaderMode);
          } else if (existingValue is Term) {
            // Term (ConstTerm, StructTerm, etc.): embed directly in structure
            struct.args[cx.S] = existingValue;
          } else {
            // Uninitialized: allocate new variable
            final varId = cx.rt.heap.allocateFreshVar();
            cx.rt.heap.addVariable(varId);
            cx.clauseVars[varIndex] = VarRef(varId, isReader: false);
            struct.args[cx.S] = VarRef(varId, isReader: isReaderMode);
          }
          cx.S++;

          // Check if structure is complete
          if (cx.S >= struct.args.length) {
            final targetValue = cx.clauseVars[-1];
            int? targetWriterId;
            if (targetValue is VarRef) {
              targetWriterId = targetValue.varId;
            } else if (targetValue is int) {
              targetWriterId = targetValue;
            }

            if (targetWriterId != null) {
              cx.rt.heap.bindWriterStruct(targetWriterId, struct.functor, struct.args);

              final acts = cx.rt.heap.processBindSuspensions(targetWriterId);
              for (final a in acts) {
                cx.rt.gq.enqueue(a);
                if (cx.onActivation != null) cx.onActivation!(a);
              }

              // SetWriter-specific: Store VarRef in argSlots
              if (!isReaderMode) {
                final targetSlot = cx.clauseVars[-2];
                if (targetSlot is int && targetSlot >= 0 && targetSlot < 10) {
                  cx.argSlots[targetSlot] = VarRef(targetWriterId, isReader: true);
                  cx.clauseVars.remove(-2);
                }
              }
            }

            // Handle parent structure restoration
            if (cx.parentStructure != null && targetWriterId is int) {
              final nestedWriterId = targetWriterId;
              final parentWriterId = cx.parentWriterId;

              if (cx.parentStructure is StructTerm) {
                final parentStruct = cx.parentStructure as StructTerm;
                parentStruct.args[cx.parentS] = VarRef(nestedWriterId, isReader: true);
              }

              cx.currentStructure = cx.parentStructure;
              cx.S = cx.parentS + 1;
              cx.mode = cx.parentMode;

              cx.parentStructure = null;
              cx.parentS = 0;
              cx.parentMode = UnifyMode.read;
              cx.parentWriterId = null;

              cx.clauseVars[-1] = parentWriterId;

              // Check if parent is now complete
              if (cx.currentStructure is StructTerm) {
                final parentStruct = cx.currentStructure as StructTerm;
                if (cx.S >= parentStruct.args.length && parentWriterId is int) {
                  cx.rt.heap.bindWriterStruct(parentWriterId, parentStruct.functor, parentStruct.args);

                  final acts = cx.rt.heap.processBindSuspensions(parentWriterId);
                  for (final a in acts) {
                    cx.rt.gq.enqueue(a);
                    if (cx.onActivation != null) cx.onActivation!(a);
                  }

                  // Store parent structure in argSlots (parent's argSlot is still in clauseVars[-2])
                  final parentTargetSlot = cx.clauseVars[-2];
                  if (parentTargetSlot is int && parentTargetSlot >= 0 && parentTargetSlot < 10) {
                    cx.argSlots[parentTargetSlot] = VarRef(parentWriterId, isReader: true);
                    cx.clauseVars.remove(-2);
                  }

                  cx.currentStructure = null;
                  cx.mode = UnifyMode.read;
                  cx.S = 0;
                  cx.clauseVars.remove(-1);
                }
              }
            } else {
              cx.currentStructure = null;
              cx.mode = UnifyMode.read;
              cx.S = 0;
              cx.clauseVars.remove(-1);
            }
          }
        }
        pc++; continue;
      }

      // Legacy HEAD opcodes (for backward compatibility)
      if (op is HeadBindWriter) {
        // Mark writer as involved (no value binding for legacy opcode)
        cx.sigmaHat[op.writerId] = null;
        pc++; continue;
      }
      if (op is HeadBindWriterArg) {
        final arg = cx.env.arg(op.slot);
        if (arg is VarRef && !arg.isReader) {
          cx.sigmaHat[arg.varId] = null;
        }
        pc++; continue;
      }
      if (op is GuardNeedReader) {
        final rid = op.readerId;
        final wid = cx.rt.heap.writerIdForReader(rid);
        final bound = (wid != null) && cx.rt.heap.isWriterBound(wid);
        if (!bound) pc = _suspendAndFail(cx, rid, pc); continue;
        pc++; continue;
      }
      if (op is GuardNeedReaderArg) {
        final arg = cx.env.arg(op.slot);
        if (arg is VarRef && arg.isReader) {
          final wid = cx.rt.heap.writerIdForReader(arg.varId);
          final bound = (wid != null) && cx.rt.heap.isWriterBound(wid);
          if (!bound) pc = _suspendAndFail(cx, arg.varId, pc); continue;
        }
        pc++; continue;
      }

      // Commit (apply σ̂w and wake suspended goals) - v2.16 semantics
      if (op is Commit) {
        // Commit only reached if HEAD and GUARD phases succeeded
        // Apply σ̂w to heap atomically

        if (cx.showBindings && cx.sigmaHat.isNotEmpty) {
          cx.sigmaHat.forEach((writerId, value) {
            print('  W$writerId → $value');
          });
        }

        if (cx.debugOutput) {
          if (cx.debugOutput) print('[DEBUG] PC $pc: COMMIT - σ̂w contains ${cx.sigmaHat.length} bindings:');
          if (!cx.showBindings) {
            cx.sigmaHat.forEach((writerId, value) {
              print('  W$writerId → $value');
            });
          }
        }

        // Convert tentative structures to real Terms before committing
        final convertedSigmaHat = <int, Object?>{};
        for (final entry in cx.sigmaHat.entries) {
          final writerId = entry.key;
          final value = entry.value;

          if (value is _TentativeStruct) {
            // Convert tentative structure to StructTerm
            final termArgs = <Term>[];
            for (final arg in value.args) {
              if (arg is _ClauseVar) {
                // Clause variable placeholder - need to resolve to actual writer/reader
                // Check if already resolved in clauseVars
                final resolved = cx.clauseVars[arg.varIndex];
                if (resolved is VarRef) {
                  // Already a VarRef - use it directly or extract reader if needed
                  if (arg.isWriter && !resolved.isReader) {
                    // Writer placeholder, resolved to writer VarRef - use as-is
                    termArgs.add(resolved);
                  } else if (arg.isWriter && resolved.isReader) {
                    // Writer placeholder but resolved to reader? Get paired writer
                    final wid = cx.rt.heap.writerIdForReader(resolved.varId);
                    if (wid != null) {
                      termArgs.add(VarRef(wid, isReader: false));
                    }
                  } else if (!arg.isWriter && resolved.isReader) {
                    // Reader placeholder, resolved to reader VarRef - use as-is
                    termArgs.add(resolved);
                  } else if (!arg.isWriter && !resolved.isReader) {
                    // Reader placeholder but resolved to writer? Use same varId as reader
                    termArgs.add(VarRef(resolved.varId, isReader: true));
                  }
                } else if (resolved is Term) {
                  // Already a term - use as-is
                  termArgs.add(resolved);
                } else {
                  // Not yet resolved - create fresh variable (WAM-style)
                  final varId = cx.rt.heap.allocateFreshVar();
                  cx.rt.heap.addVariable(varId);
                  // CRITICAL FIX: Store VarRef, not bare ID
                  cx.clauseVars[arg.varIndex] = VarRef(varId, isReader: !arg.isWriter);
                  if (arg.isWriter) {
                    termArgs.add(VarRef(varId, isReader: false));
                  } else {
                    termArgs.add(VarRef(varId, isReader: true));
                  }
                }
              } else if (arg is _TentativeStruct) {
                // Nested tentative structure - recursively convert
                termArgs.add(_convertTentativeToStruct(arg, cx));
              } else if (arg == null) {
                // Void/unbound - create fresh writer?
                // For now, leave as null constant
                termArgs.add(ConstTerm(null));
              } else if (arg is Term) {
                // Already a Term (ConstTerm, StructTerm, etc.) - use as-is
                termArgs.add(arg);
              } else {
                // Raw constant value - wrap in ConstTerm
                termArgs.add(ConstTerm(arg));
              }
            }
            convertedSigmaHat[writerId] = StructTerm(value.functor, termArgs);
          } else {
            // Direct value (constant)
            convertedSigmaHat[writerId] = value;
          }
        }

        // Print reduction (successful commit)
        if (debug) {
//           print('>>> REDUCTION: Goal ${cx.goalId} at PC $pc (commit succeeded, σ̂w has ${convertedSigmaHat.length} bindings)');
        }

        // TRACE: Show all sigmaHat bindings before applying to heap
        // print('[TRACE Commit] Applying sigmaHat to heap (${convertedSigmaHat.length} bindings):');
        for (final entry in convertedSigmaHat.entries) {
          final writerId = entry.key;
          final value = entry.value;
          // print('  W$writerId → $value');
          // Enforce WxW: writer→writer bindings are prohibited
          if (value is VarRef && !value.isReader) {
            throw StateError('WxW violation in commit: W$writerId → W${value.varId} (both unbound writers)');
          }
        }

        // Apply σ̂w: bind writers to tentative values, then wake suspended goals
        if (cx.debugOutput) print('[DEBUG] PC $pc: COMMIT - Applying ${convertedSigmaHat.length} bindings to heap...');
        final acts = CommitOps.applySigmaHatFCP(
          heap: cx.rt.heap,
          sigmaHat: convertedSigmaHat,
        );
        if (cx.debugOutput) print('[DEBUG] PC $pc: COMMIT - Applied successfully, reactivating ${acts.length} goal(s)');

        // print('[TRACE Post-Commit] Enqueueing ${acts.length} reactivated goal(s):');
        for (final a in acts) {
//           print('  → Goal ${a.id} at PC ${a.pc}');
          cx.rt.gq.enqueue(a);
          if (cx.onActivation != null) cx.onActivation!(a);
        }
        if (acts.isEmpty) {
//           print('  (no goals to reactivate)');
        }
        cx.sigmaHat.clear();
        // Clear argument registers after commit (guards may have set them up)
        cx.argSlots.clear();
        cx.inBody = true;
        pc++; continue;
      }

      // Clause control / suspend

      // clause_next: Unified instruction for moving to next clause (spec 2.2)
      // Discard σ̂w, union Si into U, clear clause state, jump to next clause
      if (op is ClauseNext) {
        cx.clearClause();
        pc = prog.labels[op.label]!;
        continue;
      }

      // try_next_clause: Soft-fail to next clause (spec 2.4)
      // When HEAD/GUARD fails, discard σ̂w, union Si to U, jump to next ClauseTry
      if (op is TryNextClause) {
        _softFailToNextClause(cx, pc);
        pc = _findNextClauseTry(pc);
        continue;
      }

      // no_more_clauses: All clauses exhausted (spec 2.5)
      // If U non-empty: suspend; otherwise: fail definitively
      if (op is NoMoreClauses) {
        if (cx.debugOutput) print('[DEBUG] PC $pc: NoMoreClauses - U=${cx.U}');
        if (cx.U.isNotEmpty) {
          if (cx.debugOutput) print('[DEBUG] NoMoreClauses - SUSPENDING on readers: ${cx.U.toList()}');
          // print('[TRACE NoMoreClauses] Goal ${cx.goalId} suspending:');
//           print('  U (blocked readers): ${cx.U.toList()}');
//           print('  κ (resume PC): ${cx.kappa}');
//           print('  Calling suspendGoalFCP to add to reader suspension lists...');

          cx.rt.suspendGoalFCP(goalId: cx.goalId, kappa: cx.kappa, readerVarIds: cx.U);

//           print('  ✓ Goal ${cx.goalId} suspended (added to reader cells)');
          cx.U.clear();
          cx.inBody = false;
          return RunResult.suspended;
        }
        if (cx.debugOutput) print('[DEBUG] NoMoreClauses - FAILING (no suspension, U is empty)');
        // U is empty - all clauses failed definitively (no suspension)
        if (debug) {
//           print('>>> FAIL: Goal ${cx.goalId} (all clauses exhausted, U empty)');
        }
        cx.inBody = false;
        // According to spec, failed goals should be added to F set
        // For now, just terminate - the goal is done (failed)
        return RunResult.terminated;
      }

      // Legacy instructions (deprecated, use ClauseNext instead)
      if (op is UnionSiAndGoto) {
        // Si removed - U updated directly by HEAD/GUARD opcodes
        cx.clearClause();
        pc = prog.labels[op.label]!;
        continue;
      }
      if (op is ResetAndGoto) { cx.clearClause(); pc = prog.labels[op.label]!; continue; }

      // Legacy SuspendEnd (use NoMoreClauses instead)
      if (op is SuspendEnd) {
        if (cx.U.isNotEmpty) {
          if (debug) {
//             print('>>> SUSPENSION: Goal ${cx.goalId} suspended on readers: ${cx.U}');
          }
          cx.rt.suspendGoalFCP(goalId: cx.goalId, kappa: cx.kappa, readerVarIds: cx.U);
          cx.U.clear();
          cx.inBody = false;
          return RunResult.suspended;
        }
        // U is empty - all clauses failed definitively (no suspension)
        if (debug) {
//           print('>>> FAIL: Goal ${cx.goalId} (all clauses exhausted, U empty)');
        }
        cx.inBody = false;
        // According to spec, failed goals should be added to F set
        // For now, just terminate - the goal is done (failed)
        return RunResult.terminated;
      }

      // Body (bind then wake + log)
      if (op is BodySetConst) {
        if (cx.inBody) {
          // bindWriterConst now returns activations (FCP: all bindings wake goals)
          final acts = cx.rt.heap.bindWriterConst(op.writerId, op.value);
          for (final a in acts) {
            cx.rt.gq.enqueue(a);
            if (cx.onActivation != null) cx.onActivation!(a);
          }
        }
        pc++; continue;
      }
      if (op is BodySetStructConstArgs) {
        if (cx.inBody) {
          final args = <Term>[
            for (final v in op.constArgs)
              v is Term ? v : ConstTerm(v)
          ];
          // bindWriterStruct now returns activations (FCP: all bindings wake goals)
          final acts = cx.rt.heap.bindWriterStruct(op.writerId, op.functor, args);
          for (final a in acts) {
            cx.rt.gq.enqueue(a);
            if (cx.onActivation != null) cx.onActivation!(a);
          }
        }
        pc++; continue;
      }
      if (op is BodySetConstArg) {
        final arg = cx.env.arg(op.slot);
        final wid = (arg is VarRef && !arg.isReader) ? arg.varId : null;
        if (cx.inBody && wid != null) {
          // bindWriterConst now returns activations (FCP: all bindings wake goals)
          final acts = cx.rt.heap.bindWriterConst(wid, op.value);
          for (final a in acts) {
            cx.rt.gq.enqueue(a);
            if (cx.onActivation != null) cx.onActivation!(a);
          }
        }
        pc++; continue;
      }

      // ===== BODY argument setup instructions =====

      // PutVariable: unified writer/reader argument placement (native V2 handler)
      if (op is opv2.PutVariable) {
        final varIndex = op.varIndex;
        final argSlot = op.argSlot;
        final isReaderMode = op.isReader;

        if (debug) print('  [G${cx.goalId}] PC=$pc PutVariable varIndex=$varIndex argSlot=$argSlot isReader=$isReaderMode');
        final value = cx.clauseVars[varIndex];

        if (value is VarRef) {
          // Already a VarRef - store with appropriate mode
          cx.argSlots[argSlot] = VarRef(value.varId, isReader: isReaderMode);
        } else if (value is int) {
          // Legacy: bare int ID
          cx.argSlots[argSlot] = VarRef(value, isReader: isReaderMode);
        } else if (value is _ClauseVar && !isReaderMode) {
          // Placeholder (PutWriter only) - allocate fresh variable
          final varId = cx.rt.heap.allocateFreshVar();
          cx.rt.heap.addVariable(varId);
          cx.argSlots[argSlot] = VarRef(varId, isReader: false);
          cx.clauseVars[varIndex] = VarRef(varId, isReader: false);
        } else if (value is StructTerm && isReaderMode) {
          // Structure (PutReader only) - create fresh variable and bind it
          final varId = cx.rt.heap.allocateFreshVar();
          cx.rt.heap.addVariable(varId);
          cx.rt.heap.bindWriterStruct(varId, value.functor, value.args);
          cx.argSlots[argSlot] = VarRef(varId, isReader: true);
        } else if (value is ConstTerm && isReaderMode) {
          // Constant (PutReader only) - create fresh variable and bind it
          final varId = cx.rt.heap.allocateFreshVar();
          cx.rt.heap.addVariable(varId);
          cx.rt.heap.bindWriterConst(varId, value.value);
          cx.argSlots[argSlot] = VarRef(varId, isReader: true);
        } else if (value == null) {
          // First occurrence - allocate fresh variable
          final varId = cx.rt.heap.allocateFreshVar();
          cx.rt.heap.addVariable(varId);
          cx.clauseVars[varIndex] = VarRef(varId, isReader: false);
          cx.argSlots[argSlot] = VarRef(varId, isReader: isReaderMode);
        } else {
          print('WARNING: PutVariable got unexpected value: $value (isReader=$isReaderMode)');
        }
        pc++; continue;
      }

      if (op is PutConstant) {
        // Create fresh variable, bind to constant, store reader VarRef in argSlot
        // Per baseline behavior: constants are stored as VarRefs to bound variables
        final varId = cx.rt.heap.allocateFreshVar();
        cx.rt.heap.addVariable(varId);
        cx.rt.heap.bindWriterConst(varId, op.value);
        cx.argSlots[op.argSlot] = VarRef(varId, isReader: true);
        pc++; continue;
      }

      // ===== WAM-style structure creation =====
      if (op is PutStructure) {
        if (cx.inBody) {
          // BODY phase: Build StructTerm with heap allocation
          // Per spec v2.16 section 7.1: Build StructTerm incrementally via set_* instructions
          // Structure will be stored in argSlots when complete

          // Create fresh variable for binding the structure
          final varId = cx.rt.heap.allocateFreshVar();
          cx.rt.heap.addVariable(varId);

          // Handle nested structures - save parent context
          if (op.argSlot == -1 || cx.currentStructure != null) {
            cx.parentStructure = cx.currentStructure;
            cx.parentS = cx.S;
            cx.parentMode = cx.mode;
            cx.parentWriterId = cx.clauseVars[-1];
          }

          // Store variable ID for structure binding
          cx.clauseVars[-1] = varId;

          // Store target argSlot for later (when structure is complete)
          if (op.argSlot >= 0 && op.argSlot < 10) {
            cx.clauseVars[-2] = op.argSlot; // Temporary storage of target slot
          } else {
            cx.clauseVars[op.argSlot] = VarRef(varId, isReader: false);
          }

          // Create structure with placeholder args (filled by Set* instructions)
          final structArgs = List<Term>.filled(op.arity, ConstTerm(null));
          cx.currentStructure = StructTerm(op.functor, structArgs);
          cx.S = 0;
          cx.mode = UnifyMode.write;
        } else {
          // PRE-COMMIT phase (guard argument building): Build StructTerm WITHOUT heap allocation
          // The structure is temporary, just for passing to the guard predicate
          // No writer variable binding needed - store directly in argSlots when complete

          // Remember target argSlot for when structure is complete
          cx.guardArgSlot = op.argSlot;

          // Create structure with placeholder args (filled by UnifyVariable/UnifyConstant)
          final structArgs = List<Term>.filled(op.arity, ConstTerm(null));
          cx.currentStructure = StructTerm(op.functor, structArgs);
          cx.S = 0;
          cx.mode = UnifyMode.write;
        }
        pc++; continue;
      }

      if (op is SetConstant) {
        if (cx.inBody && cx.mode == UnifyMode.write && cx.currentStructure is StructTerm) {
          // Store ConstTerm in current structure at position S
          final struct = cx.currentStructure as StructTerm;
          struct.args[cx.S] = ConstTerm(op.value);
          cx.S++; // Move to next position

          // Check if structure is complete (all arguments filled)
          if (cx.S >= struct.args.length) {
            // Structure complete - bind the target writer (stored at clauseVars[-1])
            final targetWriterId = cx.clauseVars[-1];
            if (targetWriterId is int) {
              // Bind the writer to the completed structure
              cx.rt.heap.bindWriterStruct(targetWriterId, struct.functor, struct.args);

              // Activate any suspended goals
              final acts = cx.rt.heap.processBindSuspensions(targetWriterId);
              for (final a in acts) {
                cx.rt.gq.enqueue(a);
                if (cx.onActivation != null) cx.onActivation!(a);
              }
            }

            // Reset structure building state
            cx.currentStructure = null;
            cx.mode = UnifyMode.read;
            cx.S = 0;
            cx.clauseVars.remove(-1); // Clear the marker
          }
        }
        pc++; continue;
      }

      // Fairness
      if (op is TailStep) {
        final shouldYield = cx.rt.tailReduce(cx.goalId);
        if (shouldYield) {
          cx.rt.gq.enqueue(GoalRef(cx.goalId, cx.kappa));
          return RunResult.yielded;
        } else {
          pc = prog.labels[op.label]!;
          continue;
        }
      }

      // ===== Goal spawning and control flow =====
      if (op is Spawn) {
        if (cx.inBody) {
          // Get entry point for procedure
          final entryPc = prog.labels[op.procedureLabel];

          // If procedure not found in program, check if it's a body kernel
          if (entryPc == null) {
            // Extract procedure name from label (may be "name" or "name/arity")
            final labelParts = op.procedureLabel.split('/');
            final procName = labelParts[0];

            // Look up body kernel
            final kernel = cx.rt.bodyKernels.lookup(procName, op.arity);
            if (kernel != null) {
              // Execute body kernel inline
              // Collect arguments from argSlots
              final args = <Object?>[];
              for (int i = 0; i < op.arity; i++) {
                args.add(cx.argSlots[i]);
              }

              // Execute kernel
              final result = kernel(cx.rt, args);

              if (result == BodyKernelResult.abort) {
                print('ERROR: Body kernel ${procName}/${op.arity} aborted');
                return RunResult.terminated;
              }

              // Success - clear args and continue (no goal spawned)
              cx.argSlots.clear();
              pc++; continue;
            }

            // Not a body kernel either - error
            print('ERROR: Spawn could not find procedure label: ${op.procedureLabel}');
            return RunResult.terminated;
          }

          // Spawn a new goal with heterogeneous argument Terms
          // Per spec v2.16 section 1.1: Create CallEnv from argSlots
          final newEnv = CallEnv(
            args: Map<int, Term>.from(cx.argSlots),
          );

          // Create and enqueue new goal with unique ID
          final newGoalId = cx.rt.nextGoalId++;
          final newGoalRef = GoalRef(newGoalId, entryPc);

          // Format spawned goal as GLP predicate with arguments
          final args = <String>[];
          for (int i = 0; i < 10; i++) {
            final term = newEnv.arg(i);
            if (term != null) {
              args.add(_formatTerm(cx.rt, term));
            } else {
              break;
            }
          }
          final goalStr = args.isEmpty ? op.procedureLabel : '${op.procedureLabel}(${args.join(', ')})';
          cx.spawnedGoals.add(goalStr);

          // Register environment with the runtime
          cx.rt.setGoalEnv(newGoalId, newEnv);

          // Inherit program from parent goal
          final parentProgram = cx.rt.getGoalProgram(cx.goalId);
          if (parentProgram != null) {
            cx.rt.setGoalProgram(newGoalId, parentProgram);
          }

          // Enqueue the goal
          cx.rt.gq.enqueue(newGoalRef);

          // Clear argument registers for next spawn
          cx.argSlots.clear();
        }
        pc++; continue;
      }

      if (op is Requeue) {
        if (cx.inBody) {
          // Tail call - reuse current goal, jump to procedure entry
          // Get entry point for procedure
          final entryPc = prog.labels[op.procedureLabel];
          if (entryPc == null) {
            print('ERROR: Requeue could not find procedure label: ${op.procedureLabel}');
            return RunResult.terminated;
          }

          // Format requeued goal as GLP predicate with arguments
          final args = <String>[];
          for (int i = 0; i < 10; i++) {
            final term = cx.argSlots[i];
            if (term != null) {
              args.add(_formatTerm(cx.rt, term));
            } else {
              break;
            }
          }
          final newHeadGoalStr = args.isEmpty ? op.procedureLabel : '${op.procedureLabel}(${args.join(', ')})';
          cx.spawnedGoals.add(newHeadGoalStr);

          // Print reduction trace before tail call
          if (cx.onReduction != null && cx.goalHead != null) {
            final body = cx.spawnedGoals.join(', ');
            cx.onReduction!(cx.goalId, cx.goalHead!, body);
          }

          // Update environment with new heterogeneous arguments
          cx.env.update(Map<int, Term>.from(cx.argSlots));

          // Clear argument registers
          cx.argSlots.clear();

          // Clear spawned goals and update head for next reduction
          cx.spawnedGoals.clear();
          cx.goalHead = newHeadGoalStr;  // New head for next iteration

          // Reset clause state for new procedure
          cx.sigmaHat.clear();
          // Si removed - U persists across clause attempts
          cx.U.clear();
          cx.clauseVars.clear();
          cx.inBody = false;
          cx.mode = UnifyMode.read;
          cx.S = 0;
          cx.currentStructure = null;

          // Update kappa to new procedure's entry point
          // This ensures suspension/reactivation uses the correct procedure
          cx.kappa = entryPc;

          // Jump to procedure entry
          pc = entryPc;
          continue;
        }
        pc++; continue;
      }

      // ===== VARIABLE INSTRUCTIONS =====

      // REMOVED: Duplicate GetVariable handler
      // The correct GetVariable handler is at line 690 and stores VarRef objects
      // This duplicate handler was storing bare IDs which caused guard comparison bugs

      // ===== BODY INSTRUCTIONS =====
      // These execute after COMMIT

      // REMOVED: Duplicate incorrect PutWriter implementation (was lines 1826-1844)
      // The correct PutWriter handler is at line 1396 and writes to cx.argWriters

      // REMOVED: Duplicate dead code PutReader implementation (was lines 1847-1863)
      // The actual PutReader handler is at line 1434 and always executes first

      // REMOVED: Duplicate incorrect PutConstant implementation (was lines 1850-1853)  
      // The correct PutConstant handler is at line 1502 and writes to cx.argReaders

      // Note: PutStructure, Spawn, and Requeue handlers are earlier in the file (lines 1162, 1324, 1303)
      // Removed duplicate dead code that was unreachable

      // ===== GUARD INSTRUCTIONS =====
      if (op is Guard) {
        // Execute guard predicate with three-valued semantics
        // Guards can SUCCESS (continue), FAIL (try next clause), or SUSPEND (add to Si)
        //
        // The compiler emits PutWriter/PutReader/PutConstant to set up argument registers
        // before this instruction, so we read from cx.argWriters and cx.argReaders

        final predicateName = op.procedureLabel;  // Actually the predicate name (e.g., '<', '>')
        final arity = op.arity;
        if (cx.debugOutput) print('[DEBUG] PC $pc: Guard(${predicateName}/$arity) - argSlots=${cx.argSlots}');

        if (debug) {
          // print('[GUARD] Evaluating: $predicateName/$arity');
          // print('[GUARD] argWriters: ${cx.argWriters}');
          // print('[GUARD] argReaders: ${cx.argReaders}');
        }

        // Extract and dereference arguments from argument registers
        final args = <Object?>[];
        final unboundReaders = <int>{};

        for (int i = 0; i < arity; i++) {
          Object? argValue;

          // Get argument from argSlots (heterogeneous term storage)
          final arg = cx.argSlots[i];
          if (arg != null) {
            argValue = arg; // Store Term directly (VarRef, ConstTerm, or StructTerm)
          }
          // Check clauseVars for HEAD variables
          else if (cx.clauseVars.containsKey(i)) {
            argValue = cx.clauseVars[i];
            // print('[GUARD] Arg $i from clauseVars: $argValue');
          }
          else {
            // No argument at this slot
            if (debug) {
              // print('[GUARD] WARNING: Argument $i not found in argWriters, argReaders, or clauseVars');
            }
            argValue = null;
          }

          // Dereference to get actual values, tracking unbound readers
          if (argValue != null) {
            // print('[GUARD] Before deref - Arg $i: $argValue (${argValue.runtimeType})');
            final (derefValue, readers) = _dereferenceWithTracking(argValue, cx);
            // print('[GUARD] After deref - Arg $i: $derefValue (${derefValue.runtimeType})');
            args.add(derefValue);
            unboundReaders.addAll(readers);

            if (debug) {
              // print('[GUARD] Arg $i: $argValue → $derefValue');
            }
          } else {
            args.add(null);
          }
        }

        // If any arguments have unbound readers, suspend
        if (unboundReaders.isNotEmpty) {
          if (debug) {
            // print('[GUARD] SUSPEND - unbound readers: $unboundReaders');
          }
          pc = _suspendAndFailMulti(cx, unboundReaders, pc);
          continue;
        }

        // All arguments are ground - evaluate the guard
        final result = _evaluateGuard(predicateName, args, cx);

        if (result == GuardResult.success) {
          if (cx.debugOutput) print('[DEBUG] Guard - SUCCESS with args: $args');
          if (debug) {
            // print('[GUARD] SUCCESS - continuing');
          }
          pc++;
          continue;
        } else {
          // FAIL - try next clause
          if (cx.debugOutput) print('[DEBUG] Guard - FAILED with args: $args');
          if (debug) {
            // print('[GUARD] FAIL - trying next clause');
          }
          _softFailToNextClause(cx, pc);
          pc = _findNextClauseTry(pc);
          continue;
        }
      }

      if (op is Ground) {
        // ground(X): Succeeds if X is ground (contains no unbound variables)
        //
        // Three-valued semantics:
        // 1. If X is ground → SUCCEED (test passes, pc++)
        // 2. If X contains unbound readers (but no unbound writers) → SUSPEND
        //    (add readers to Si, pc++ - may become ground when readers bind)
        // 3. If X contains unbound writers → FAIL (soft-fail to next clause)
        //    (due to SRSW, cannot wait for unknown future binding)
        //
        // Per spec section 5: "A guard that demands an uninstantiated reader
        // adds that reader to Si and continues scanning"

        final value = cx.clauseVars[op.varIndex];
        if (value == null) {
          // Variable doesn't exist - fail
          _softFailToNextClause(cx, pc);
          pc = _findNextClauseTry(pc);
          continue;
        }

        // Collect unbound readers and check for unbound writers
        final unboundReaders = <int>{};
        bool hasUnboundWriter = false;

        void collectUnbound(Object? term) {
          if (term is VarRef && !term.isReader) {
            final wid = term.varId;
            if (!cx.rt.heap.isWriterBound(wid)) {
              hasUnboundWriter = true;
            } else {
              collectUnbound(cx.rt.heap.valueOfWriter(wid));
            }
          } else if (term is VarRef && term.isReader) {
            final rid = term.varId;
            final wid = cx.rt.heap.writerIdForReader(rid);
            if (wid == null || !cx.rt.heap.isWriterBound(wid)) {
              unboundReaders.add(rid);
            } else {
              collectUnbound(cx.rt.heap.valueOfWriter(wid));
            }
          } else if (term is StructTerm) {
            for (final arg in term.args) {
              collectUnbound(arg);
            }
          }
          // Constants contribute nothing
        }

        // Dereference the clause variable
        if (value is int) {
          // Could be writer ID or reader ID
          final wc = cx.rt.heap.writer(value);
          if (wc != null) {
            // It's a writer ID
            if (!cx.rt.heap.isWriterBound(value)) {
              hasUnboundWriter = true;
            } else {
              collectUnbound(cx.rt.heap.valueOfWriter(value));
            }
          } else {
            // It's a reader ID
            final wid = cx.rt.heap.writerIdForReader(value);
            if (wid == null || !cx.rt.heap.isWriterBound(wid)) {
              unboundReaders.add(value);
            } else {
              collectUnbound(cx.rt.heap.valueOfWriter(wid));
            }
          }
        } else {
          // It's a Term - analyze it
          collectUnbound(value);
        }

        // Decision logic (three-valued):
        if (hasUnboundWriter) {
          // Contains unbound writer(s) → FAIL (cannot become ground via SRSW)
          _softFailToNextClause(cx, pc);
          pc = _findNextClauseTry(pc);
          continue;
        } else if (unboundReaders.isNotEmpty) {
          // Contains unbound readers but no unbound writers → SUSPEND
          // May become ground when readers bind, add to Si and continue
          pc = _suspendAndFailMulti(cx, unboundReaders, pc); continue;
          pc++;
          continue;
        } else {
          // No unbound variables → SUCCEED (is ground)
          pc++;
          continue;
        }
      }

      if (op is Known) {
        // known(X): Succeeds if X is not an unbound variable
        //
        // Three-valued semantics:
        // 1. If X is bound (to anything) → SUCCEED (test passes, pc++)
        // 2. If X is an unbound reader → SUSPEND
        //    (add reader to Si, pc++ - may become known when reader binds)
        // 3. If X is an unbound writer → FAIL (soft-fail to next clause)
        //    (due to SRSW, cannot wait for unknown future binding)
        //
        // Note: known(X) differs from ground(X) - known only checks if X itself
        // is bound, not whether X contains unbound variables internally

        final value = cx.clauseVars[op.varIndex];
        if (value == null) {
          // Variable doesn't exist - fail
          _softFailToNextClause(cx, pc);
          pc = _findNextClauseTry(pc);
          continue;
        }

        // Check if value is known
        bool isKnown = false;
        int? unboundReader = null;

        if (value is int) {
          // Could be writer ID or reader ID
          final wc = cx.rt.heap.writer(value);
          if (wc != null) {
            // It's a writer ID - check if bound
            isKnown = cx.rt.heap.isWriterBound(value);
          } else {
            // It's a reader ID - check if its paired writer is bound
            final wid = cx.rt.heap.writerIdForReader(value);
            if (wid != null && cx.rt.heap.isWriterBound(wid)) {
              isKnown = true;
            } else {
              // Unbound reader - could become known later
              unboundReader = value;
            }
          }
        } else if (value is VarRef && !value.isReader) {
          isKnown = cx.rt.heap.isWriterBound(value.varId);
        } else if (value is VarRef && value.isReader) {
          final rid = value.varId;
          final wid = cx.rt.heap.writerIdForReader(rid);
          if (wid != null && cx.rt.heap.isWriterBound(wid)) {
            isKnown = true;
          } else {
            unboundReader = rid;
          }
        } else {
          // Constant or structure - always known
          isKnown = true;
        }

        if (isKnown) {
          // Variable is known - succeed
          pc++;
          continue;
        } else if (unboundReader != null) {
          // Variable is unbound reader - could become known later, add to Si
          pc = _suspendAndFail(cx, unboundReader, pc); continue;
          pc++;
          continue;
        } else {
          // Variable is unbound writer - fail
          _softFailToNextClause(cx, pc);
          pc = _findNextClauseTry(pc);
          continue;
        }
      }

      // ===== SET CLAUSE VARIABLE =====
      if (op is SetClauseVar) {
        // Set a clause variable directly to a value
        cx.clauseVars[op.slot] = op.value;
        pc++;
        continue;
      }

      // ===== SYSTEM PREDICATE EXECUTION =====
      if (op is Execute) {
        // Execute system predicate: call registered Dart function
        // System predicates can succeed, fail, or suspend on unbound readers

        // Look up the system predicate
        final predicate = cx.rt.systemPredicates.lookup(op.predicateName);
        if (predicate == null) {
          print('[ERROR] System predicate not found: ${op.predicateName}');
          _softFailToNextClause(cx, pc);
          pc = _findNextClauseTry(pc);
          continue;
        }

        // Process arguments directly - they're already Terms (per spec Section 18.1)
        // No need to look up in clauseVars - args are passed directly
        print('[EXECUTE] Direct args: ${op.args}');
        final processedArgs = <Object?>[];

        for (final arg in op.args) {
          print('[EXECUTE] Processing arg: $arg (${arg.runtimeType})');

          // Resolve VarRefs that reference HEAD variables
          // HEAD variables stored via GetVariable remain in clauseVars
          final resolved = _resolveHeadVarRefs(arg, cx);
          print('[EXECUTE] After HEAD resolve: $resolved');

          // Dereference to get actual values from heap
          final derefArg = _dereferenceForExecute(resolved, cx.rt, cx);
          print('[EXECUTE] After deref: $derefArg (${derefArg.runtimeType})');

          processedArgs.add(derefArg);
        }

        // Create system call context
        final call = SystemCall(op.predicateName, processedArgs);

        // Execute the system predicate
        final result = predicate(cx.rt, call);

        // Handle result based on three-valued semantics
        if (result == SystemResult.success) {
          // Predicate succeeded - continue execution
          pc++;
          continue;
        } else if (result == SystemResult.failure) {
          // Predicate failed - try next clause
          _softFailToNextClause(cx, pc);
          pc = _findNextClauseTry(pc);
          continue;
        } else {
          // result == SystemResult.suspend
          // Predicate suspended on unbound readers - add to Si and continue
          pc = _suspendAndFailMulti(cx, call.suspendedReaders, pc); continue;
          pc++;
          continue;
        }
      }

      // ===== LIST-SPECIFIC HEAD INSTRUCTIONS =====
      if (op is HeadNil) {
        // Match empty list [] with argument or clause variable
        // Check if argSlot refers to a clause variable (for nested structures) or argument register
        final bool isClauseVar = op.argSlot >= 10;
        if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadNil: argSlot=${op.argSlot}, isClauseVar=$isClauseVar');
        final arg = isClauseVar ? null : _getArg(cx, op.argSlot);
        if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadNil: arg=$arg');

        // For clause variables, get the value from clauseVars
        if (isClauseVar) {
          final clauseVarValue = cx.clauseVars[op.argSlot];
          if (clauseVarValue == null) {
            // Unbound clause variable - soft fail
            if (debug && cx.goalId >= 4000) print('  HeadNil: clause var ${op.argSlot} is unbound, failing');
            _softFailToNextClause(cx, pc);
            pc = _findNextClauseTry(pc);
            continue;
          }

          // Check if the value is [] (empty list)
          if (clauseVarValue is ConstTerm) {
            if (clauseVarValue.value == 'nil') {
              // Match!
              if (debug && cx.goalId >= 4000) print('  HeadNil: clause var ${op.argSlot} = $clauseVarValue, MATCH');
              pc++;
              continue;
            } else {
              // Non-empty constant
              if (debug && cx.goalId >= 4000) print('  HeadNil: clause var ${op.argSlot} = $clauseVarValue, NO MATCH');
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            }
          } else if (clauseVarValue is StructTerm) {
            // Structure (non-empty list) doesn't match []
            if (debug && cx.goalId >= 4000) print('  HeadNil: clause var ${op.argSlot} is struct, NO MATCH');
            _softFailToNextClause(cx, pc);
            pc = _findNextClauseTry(pc);
            continue;
          } else if (clauseVarValue is VarRef) {
            // VarRef stored in clauseVars - extract varId and handle
            final varId = clauseVarValue.varId;
            if (cx.rt.heap.isBound(varId)) {
              final value = cx.rt.heap.getValue(varId);
              if (value is ConstTerm && value.value == 'nil') {
                if (debug && cx.goalId >= 4000) print('  HeadNil: clause var ${op.argSlot} = VarRef($varId) = $value, MATCH');
                pc++;
                continue;
              } else {
                if (debug && cx.goalId >= 4000) print('  HeadNil: clause var ${op.argSlot} = VarRef($varId) = $value, NO MATCH');
                _softFailToNextClause(cx, pc);
                pc = _findNextClauseTry(pc);
                continue;
              }
            } else {
              // Unbound variable - bind to nil in σ̂w
              cx.sigmaHat[varId] = ConstTerm('nil');
              if (debug && cx.goalId >= 4000) print('  HeadNil: clause var ${op.argSlot} = VarRef($varId) (unbound), binding to nil');
              pc++;
              continue;
            }
          } else if (clauseVarValue is int) {
            // Writer ID - check if bound
            final wid = clauseVarValue;
            if (cx.rt.heap.isWriterBound(wid)) {
              final value = cx.rt.heap.valueOfWriter(wid);
              if (value is ConstTerm && value.value == 'nil') {
                if (debug && cx.goalId >= 4000) print('  HeadNil: clause var ${op.argSlot} = W$wid = $value, MATCH');
                pc++;
                continue;
              } else {
                if (debug && cx.goalId >= 4000) print('  HeadNil: clause var ${op.argSlot} = W$wid = $value, NO MATCH');
                _softFailToNextClause(cx, pc);
                pc = _findNextClauseTry(pc);
                continue;
              }
            } else {
              // Unbound writer - enter WRITE mode to bind to []
              cx.sigmaHat[wid] = ConstTerm('nil');
              if (debug && cx.goalId >= 4000) print('  HeadNil: clause var ${op.argSlot} = W$wid (unbound), binding to nil');
              pc++;
              continue;
            }
          }

          // Unexpected clauseVar type
          _softFailToNextClause(cx, pc);
          pc = _findNextClauseTry(pc);
          continue;
        }

        // Regular argument handling
        if (arg == null) { pc++; continue; } // No argument at this slot

        // Note: valueOfWriter() dereferences automatically per FCP AM semantics
        if (arg is VarRef && !arg.isReader) {
          if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadNil: arg is writer ${arg.varId}');
          // Writer: check if already bound, else record tentative binding in σ̂w
          if (cx.rt.heap.isWriterBound(arg.varId)) {
            // Already bound - check if value matches []
            final value = cx.rt.heap.valueOfWriter(arg.varId);
            if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadNil: writer ${arg.varId} value = $value');
            if (value is ConstTerm && value.value != 'nil') {
              if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadNil: value does not match nil, failing');
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            } else if (value is StructTerm) {
              if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadNil: value is struct, failing');
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            }
          } else {
            // Unbound writer - record tentative binding in σ̂w
            cx.sigmaHat[arg.varId] = ConstTerm('nil');
          }
        } else if (arg is VarRef && arg.isReader) {
          if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadNil: arg is reader ${arg.varId}');
          // Reader: check if bound, else add to U and fail
          final wid = cx.rt.heap.writerIdForReader(arg.varId);
          final bound = wid != null ? cx.rt.heap.isWriterBound(wid) : false;
          final value = (wid != null && bound) ? cx.rt.heap.valueOfWriter(wid) : null;

          if (wid == null || !bound) {
            // Find the final unbound variable in the chain
            final suspendOnVar = _finalUnboundVar(cx, arg.varId);
            pc = _suspendAndFail(cx, suspendOnVar, pc);
            continue;
          } else {
            // Bound reader - check if value matches []
            // print('[DEBUG HeadNil] → Bound reader, checking value matches nil');
            if (value is ConstTerm && value.value == 'nil') {
              // Match! Empty list
            } else if (value is StructTerm) {
              // Structure doesn't match []
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            } else {
              // Non-empty constant doesn't match []
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            }
          }
        }
        if (debug && (cx.goalId >= 10002 && cx.goalId <= 10008)) print('[TRACE HeadNil] After HeadNil, U = {${cx.U.join(', ')}}');
        pc++;
        continue;
      }

      if (op is HeadList) {
        // Match list structure [H|T] with argument
        // Equivalent to HeadStructure('[|]', 2, op.argSlot)
        final arg = _getArg(cx, op.argSlot);
        if (arg == null) { pc++; continue; } // No argument at this slot

        if (arg is VarRef && !arg.isReader) {
          // Writer: create tentative structure in σ̂w
          if (cx.rt.heap.isWriterBound(arg.varId)) {
            // Already bound - check if it's a list structure
            final value = cx.rt.heap.valueOfWriter(arg.varId);
            if (value is StructTerm && value.functor == '[|]' && value.args.length == 2) {
              cx.currentStructure = value;
              cx.S = 0;
              cx.mode = UnifyMode.read;
            } else {
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            }
          } else {
            // Unbound writer - create tentative structure
            final struct = StructTerm('[|]', []);
            cx.sigmaHat[arg.varId] = struct;
            cx.currentStructure = struct;
            cx.S = 0;
            cx.mode = UnifyMode.write;
          }
        } else if (arg is VarRef && arg.isReader) {
          // Reader: check if bound, else add to U and fail
          final wid = cx.rt.heap.writerIdForReader(arg.varId);
          final bound = wid != null ? cx.rt.heap.isWriterBound(wid) : false;
          final value = (wid != null && bound) ? cx.rt.heap.valueOfWriter(wid) : null;

          if (wid == null || !bound) {
            final suspendOnVar = _finalUnboundVar(cx, arg.varId);
            pc = _suspendAndFail(cx, suspendOnVar, pc);
            continue;
          } else{
            // Bound reader - check if it's a list structure
            // print('[DEBUG HeadList] → Bound reader, checking value is list');
            if (value is StructTerm && value.functor == '[|]' && value.args.length == 2) {
              cx.currentStructure = value;
              cx.S = 0;
              cx.mode = UnifyMode.read;
            } else {
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            }
          }
        }
        pc++;
        continue;
      }

      // ===== LIST-SPECIFIC BODY INSTRUCTIONS =====
      if (op is PutNil) {
        if (cx.inBody) {
          // Place empty list [] in argument register
          // Create a fresh variable bound to [] (same as PutConstant)
          final varId = cx.rt.heap.allocateFreshVar();
          cx.rt.heap.addVariable(varId);
          cx.rt.heap.bindWriterConst(varId, 'nil'); // [] represented as 'nil'
          cx.argSlots[op.argSlot] = VarRef(varId, isReader: true);
        }
        pc++;
        continue;
      }

      if (op is PutBoundConst) {
        // Put a variable bound to a constant value
        // Used for passing constants as arguments in queries
        final varId = cx.rt.heap.allocateFreshVar();
        cx.rt.heap.addVariable(varId);
        cx.rt.heap.bindWriterConst(varId, op.value);
        cx.argSlots[op.argSlot] = VarRef(varId, isReader: true);
        pc++;
        continue;
      }

      if (op is PutBoundNil) {
        // Put a variable bound to 'nil'
        // Used for passing empty lists as arguments in queries
        final varId = cx.rt.heap.allocateFreshVar();
        cx.rt.heap.addVariable(varId);
        cx.rt.heap.bindWriterConst(varId, 'nil');
        cx.argSlots[op.argSlot] = VarRef(varId, isReader: true);
        pc++;
        continue;
      }

      if (op is PutList) {
        // Begin list construction in argument register
        // Equivalent to PutStructure('[|]', 2, op.argSlot)
        if (cx.inBody) {
          // Store target writer ID from environment
          final arg = cx.env.arg(op.argSlot);
          final targetWriterId = (arg is VarRef && !arg.isReader) ? arg.varId : null;
          if (targetWriterId == null) {
            print('WARNING: PutList argSlot ${op.argSlot} has no writer in environment');
            pc++; continue;
          }

          // Store the writer ID in context for later binding
          cx.clauseVars[-1] = targetWriterId; // Use -1 as special marker for structure binding

          // Create list structure [H|T] with placeholder args (will be filled by Set* instructions)
          final structArgs = List<Term>.filled(2, ConstTerm(null)); // Lists have arity 2
          cx.currentStructure = StructTerm('[|]', structArgs);
          cx.S = 0; // Start at first argument position
          cx.mode = UnifyMode.write;
        }
        pc++;
        continue;
      }

      // ===== ENVIRONMENT FRAME INSTRUCTIONS =====
      if (op is Allocate) {
        // allocate N: Create environment frame with N permanent variable slots
        // WAM semantics: E' = newFrame(E, CP, N); CP = P+1
        // Used by non-tail-recursive predicates to save local state
        if (!cx.inBody) {
          throw StateError('Allocate must be in BODY phase (after commit)');
        }

        final newFrame = EnvironmentFrame(
          parent: cx.E,
          continuationPointer: cx.CP ?? (pc + 1),  // Save continuation (next instruction)
          size: op.slots,
        );

        cx.E = newFrame;
        cx.CP = pc + 1;  // Update CP to point to next instruction

        if (debug) {
          print('  [G${cx.goalId}] PC=$pc Allocate ${op.slots} slots - created frame with CP=${cx.CP}');
        }

        pc++;
        continue;
      }

      if (op is Deallocate) {
        // deallocate: Remove current environment frame
        // WAM semantics: CP = E.CP; E = E.parent; P = CP
        // Restores previous environment and returns to saved continuation
        if (cx.E == null) {
          throw StateError('Deallocate with no environment frame');
        }

        final frame = cx.E!;
        cx.CP = frame.continuationPointer;  // Restore continuation pointer
        cx.E = frame.parent;                 // Restore previous environment

        if (debug) {
          print('  [G${cx.goalId}] PC=$pc Deallocate - restored CP=${cx.CP}, parent frame=${cx.E != null}');
        }

        // Note: Unlike WAM, we don't jump to CP here - deallocate just pops the frame
        // The subsequent proceed or return instruction will handle the jump
        pc++;
        continue;
      }

      // ===== UTILITY INSTRUCTIONS =====
      if (op is Nop) {
        // No operation - just advance PC
        pc++;
        continue;
      }

      if (op is Halt) {
        // Terminate execution
        return RunResult.terminated;
      }

      if (op is Proceed) {
        // Call reduction callback if trace is on
        if (cx.onReduction != null && cx.goalHead != null) {
          final body = cx.spawnedGoals.isEmpty ? 'true' : cx.spawnedGoals.join(', ');
          cx.onReduction!(cx.goalId, cx.goalHead!, body);
        }
        // Complete current procedure - terminate execution
        return RunResult.terminated;
      }

      pc++; // default progress
    }
    return RunResult.terminated;
  }

  /// Helper to get argument term from call environment
  /// Per spec v2.16 section 1.1: arguments are heterogeneous Terms
  Term? _getArg(RunnerContext cx, int slot) {
    return cx.env.arg(slot);
  }

  /// Resolves VarRefs that reference HEAD variables to actual heap IDs
  /// HEAD variables are stored in clauseVars via GetVariable
  /// This converts VarRef(headIndex) → VarRef(heapId)
  static Object? _resolveHeadVarRefs(Object? term, RunnerContext cx) {
    if (term is VarRef) {
      // Check if this VarRef's ID is a HEAD variable index
      // GetVariable stores heap IDs in clauseVars, so we can look them up
      if (cx.clauseVars.containsKey(term.varId)) {
        final binding = cx.clauseVars[term.varId];
        if (binding is int) {
          // It's a heap ID from HEAD phase - create new VarRef with actual ID
          return VarRef(binding, isReader: term.isReader);
        } else if (binding is VarRef) {
          // Already a VarRef - return as is
          return binding;
        } else if (binding is Term) {
          // It's a Term - return as is
          return binding;
        }
      }
      // Not in clauseVars or not resolvable - return as is
      return term;
    } else if (term is StructTerm) {
      // Recursively resolve arguments in structures
      final resolvedArgs = <Term>[];
      for (final arg in term.args) {
        final resolved = _resolveHeadVarRefs(arg, cx);
        if (resolved is Term) {
          resolvedArgs.add(resolved);
        } else {
          resolvedArgs.add(ConstTerm(resolved));
        }
      }
      return StructTerm(term.functor, resolvedArgs);
    } else {
      // Other types - return as is
      return term;
    }
  }

  /// Helper to dereference values for execute() arguments
  /// Recursively dereferences readers to get actual bound values
  static Object? _dereferenceForExecute(Object? term, GlpRuntime rt, RunnerContext cx) {
    if (term is VarRef) {
      // Now varId should be an actual heap ID (after _resolveHeadVarRefs)
      final varId = term.varId;
      print('[DEREF] VarRef: $term (id=$varId, isReader=${term.isReader})');

      // Check sigma-hat for tentative bindings (during HEAD/GUARD phase)
      if (cx.sigmaHat.containsKey(varId)) {
        final value = cx.sigmaHat[varId];
        print('[DEREF] Found in sigma-hat: $value');
        return _dereferenceForExecute(value, rt, cx);
      }

      print('[DEREF] isWriterBound($varId) = ${rt.heap.isWriterBound(varId)}');

      // For readers, check if the paired writer is bound
      // For writers, check if the writer itself is bound
      if (rt.heap.isWriterBound(varId)) {
        final value = rt.heap.getValue(varId);
        print('[DEREF] Bound to: $value');
        // Recursively dereference in case value contains more VarRefs
        return _dereferenceForExecute(value, rt, cx);
      } else {
        // Unbound - return the VarRef as-is
        // (system predicate will handle suspension)
        print('[DEREF] Unbound - returning as-is');
        return term;
      }
    } else if (term is StructTerm) {
      print('[DEREF] StructTerm: ${term.functor}/${term.args.length}');
      // Recursively dereference arguments in structures
      final derefArgs = <Term>[];
      for (final arg in term.args) {
        final derefArg = _dereferenceForExecute(arg, rt, cx);
        // Wrap primitives in ConstTerm
        if (derefArg is Term) {
          derefArgs.add(derefArg);
        } else {
          derefArgs.add(ConstTerm(derefArg));
        }
      }
      return StructTerm(term.functor, derefArgs);
    } else {
      // Const, List, or other - return as-is
      print('[DEREF] Other: $term (${term.runtimeType})');
      return term;
    }
  }

  /// Dereference a term and track any unbound readers encountered
  /// Used by guard evaluation to detect suspension conditions
  static (Object?, Set<int>) _dereferenceWithTracking(Object? term, RunnerContext cx) {
    final unboundReaders = <int>{};

    Object? dereference(Object? t) {
      // Resolve clauseVars first (same pattern as Execute fix)
      if (t is VarRef && cx.clauseVars.containsKey(t.varId)) {
        // Resolve clause variable index to actual heap ID
        final resolved = cx.clauseVars[t.varId];
        if (resolved is int) {
          t = VarRef(resolved, isReader: t.isReader);
        } else if (resolved != null) {
          // Already resolved to a term
          return dereference(resolved);
        }
      }

      if (t is VarRef) {
        if (t.isReader) {
          // Reader - get paired writer and check if bound
          final readerId = t.varId;
          final wid = cx.rt.heap.writerIdForReader(readerId);

          // Check sigma-hat first for tentative bindings (before commit)
          if (wid != null && cx.sigmaHat.containsKey(wid)) {
            return dereference(cx.sigmaHat[wid]);
          }

          if (wid != null && cx.rt.heap.isWriterBound(wid)) {
            final boundValue = cx.rt.heap.valueOfWriter(wid);
            // CRITICAL FIX: Recursively dereference the bound value
            return dereference(boundValue);
          } else {
            // Unbound reader - track it
            unboundReaders.add(readerId);
            return t;
          }
        } else {
          // Writer variable
          final varId = t.varId;

          // Check sigma-hat first (tentative bindings)
          if (cx.sigmaHat.containsKey(varId)) {
            return dereference(cx.sigmaHat[varId]);
          }

          // Check heap
          if (cx.rt.heap.isWriterBound(varId)) {
            final boundValue = cx.rt.heap.getValue(varId);
            // CRITICAL FIX: Recursively dereference the bound value
            return dereference(boundValue);
          } else {
            // Unbound writer - can't evaluate
            return t;
          }
        }
      } else if (t is StructTerm) {
        // Return structure as-is (don't evaluate arithmetic here)
        // Guards like =:= will evaluate explicitly using evaluateNumeric
        return t;
      } else if (t is ConstTerm) {
        // CRITICAL FIX: Unwrap ConstTerm to get primitive value
        return t.value;
      } else if (t is int) {
        // Bare int represents a variable ID - look up its bound value
        if (cx.rt.heap.isWriterBound(t)) {
          final boundValue = cx.rt.heap.getValue(t);
          // Recursively dereference the bound value
          return dereference(boundValue);
        } else {
          // Unbound variable - return as VarRef for proper handling
          return VarRef(t, isReader: false);
        }
      } else {
        return t;
      }
    }

    final result = dereference(term);
    return (result, unboundReaders);
  }

  /// Check if a functor is an arithmetic operator
  static bool _isArithmeticOp(String functor) {
    return const {'+', '-', '*', '/', 'mod', 'neg'}.contains(functor);
  }

  /// Evaluate arithmetic expression (already ground)
  static num _evaluateArithmetic(String op, List<Object?> args) {
    // Extract numeric values
    num getNum(Object? v) {
      if (v is num) return v;
      if (v is ConstTerm && v.value is num) return v.value as num;
      throw StateError('Non-numeric value in arithmetic: $v');
    }

    if (args.isEmpty) {
      throw StateError('Arithmetic operator $op requires arguments');
    }

    final a = getNum(args[0]);

    // Unary operators
    if (op == 'neg' || (op == '-' && args.length == 1)) {
      return -a;
    }

    // Binary operators
    if (args.length < 2) {
      throw StateError('Binary operator $op requires two arguments');
    }
    final b = getNum(args[1]);

    switch (op) {
      case '+': return a + b;
      case '-': return a - b;
      case '*': return a * b;
      case '/': return a / b;
      case 'mod': return a.toInt() % b.toInt();
      default: throw StateError('Unknown arithmetic operator: $op');
    }
  }

  /// Evaluate a guard predicate with ground arguments
  static GuardResult _evaluateGuard(String predicateName, List<Object?> args, RunnerContext cx) {
    // Extract values from any remaining ConstTerms
    Object? getValue(Object? v) {
      if (v is ConstTerm) return v.value;
      return v;
    }

    // Evaluate arithmetic expressions to numeric values
    // Supports: X, X + Y, X - Y, X * Y, X / Y, X // Y, X mod Y, -X
    num? evaluateNumeric(Object? v) {
      if (v is num) return v;
      if (v is ConstTerm && v.value is num) return v.value as num;
      // Handle VarRef - dereference to get actual value
      if (v is VarRef) {
        final deref = cx.rt.heap.getValue(v.varId);
        if (deref == null) return null; // Unbound
        return evaluateNumeric(deref);
      }
      if (v is StructTerm) {
        // Evaluate arithmetic expression
        switch (v.functor) {
          case '+':
            if (v.args.length != 2) return null;
            final a = evaluateNumeric(v.args[0]);
            final b = evaluateNumeric(v.args[1]);
            if (a == null || b == null) return null;
            return a + b;
          case '-':
            if (v.args.length == 1) {
              // Unary minus
              final a = evaluateNumeric(v.args[0]);
              return a == null ? null : -a;
            } else if (v.args.length == 2) {
              final a = evaluateNumeric(v.args[0]);
              final b = evaluateNumeric(v.args[1]);
              if (a == null || b == null) return null;
              return a - b;
            }
            return null;
          case '*':
            if (v.args.length != 2) return null;
            final a = evaluateNumeric(v.args[0]);
            final b = evaluateNumeric(v.args[1]);
            if (a == null || b == null) return null;
            return a * b;
          case '/':
            if (v.args.length != 2) return null;
            final a = evaluateNumeric(v.args[0]);
            final b = evaluateNumeric(v.args[1]);
            if (a == null || b == null || b == 0) return null;
            return a / b;
          case '//':
            if (v.args.length != 2) return null;
            final a = evaluateNumeric(v.args[0]);
            final b = evaluateNumeric(v.args[1]);
            if (a == null || b == null || b == 0) return null;
            return a ~/ b;
          case 'mod':
            if (v.args.length != 2) return null;
            final a = evaluateNumeric(v.args[0]);
            final b = evaluateNumeric(v.args[1]);
            if (a == null || b == null || b == 0) return null;
            return a.toInt() % b.toInt();
          case 'neg':
            if (v.args.length != 1) return null;
            final a = evaluateNumeric(v.args[0]);
            return a == null ? null : -a;
          default:
            return null; // Not an arithmetic functor
        }
      }
      return null;
    }

    switch (predicateName) {
      // Comparison guards (with arithmetic expression support)
      case '<':
        if (args.length < 2) return GuardResult.failure;
        final a = evaluateNumeric(args[0]);
        final b = evaluateNumeric(args[1]);

        // Debug output
        // print('[EVAL_GUARD] < comparison:');
        // print('[EVAL_GUARD]   args[0] = ${args[0]} (${args[0].runtimeType})');
        // print('[EVAL_GUARD]   args[1] = ${args[1]} (${args[1].runtimeType})');
        // print('[EVAL_GUARD]   a = $a (${a.runtimeType})');
        // print('[EVAL_GUARD]   b = $b (${b.runtimeType})');
        // print('[EVAL_GUARD]   a is num = ${a is num}');
        // print('[EVAL_GUARD]   b is num = ${b is num}');

        if (a != null && b != null) {
          return a < b ? GuardResult.success : GuardResult.failure;
        }
        return GuardResult.failure;

      case '>':
        if (args.length < 2) return GuardResult.failure;
        final a = evaluateNumeric(args[0]);
        final b = evaluateNumeric(args[1]);
        if (a != null && b != null) {
          return a > b ? GuardResult.success : GuardResult.failure;
        }
        return GuardResult.failure;

      case '=<':
        if (args.length < 2) return GuardResult.failure;
        final a = evaluateNumeric(args[0]);
        final b = evaluateNumeric(args[1]);
        if (a != null && b != null) {
          return a <= b ? GuardResult.success : GuardResult.failure;
        }
        return GuardResult.failure;

      case '>=':
        if (args.length < 2) return GuardResult.failure;
        final a = evaluateNumeric(args[0]);
        final b = evaluateNumeric(args[1]);
        if (a != null && b != null) {
          return a >= b ? GuardResult.success : GuardResult.failure;
        }
        return GuardResult.failure;

      case '=:=':
        if (args.length < 2) return GuardResult.failure;
        final a = evaluateNumeric(args[0]);
        final b = evaluateNumeric(args[1]);
        if (a != null && b != null) {
          return a == b ? GuardResult.success : GuardResult.failure;
        }
        return GuardResult.failure;

      case '=\\=':
        if (args.length < 2) return GuardResult.failure;
        final a = evaluateNumeric(args[0]);
        final b = evaluateNumeric(args[1]);
        if (a != null && b != null) {
          return a != b ? GuardResult.success : GuardResult.failure;
        }
        return GuardResult.failure;

      // Type guards
      case 'ground':
        // Already checked for unbound readers in caller
        return GuardResult.success;

      case 'known':
        // Check if argument is not a variable
        if (args.isEmpty) return GuardResult.failure;
        final arg = args[0];
        if (arg is VarRef) {
          return GuardResult.failure;
        }
        return GuardResult.success;

      case 'integer':
        // Per spec 19.4.3: Test if Xi is an integer
        if (args.isEmpty) return GuardResult.failure;
        final val = getValue(args[0]);
        return (val is int) ? GuardResult.success : GuardResult.failure;

      case 'string':
        // Succeeds if X is a string (lowercase identifier or quoted string)
        if (args.isEmpty) return GuardResult.failure;
        final val = getValue(args[0]);
        // String: ConstTerm with String value (not 'nil' which represents [])
        if (val is ConstTerm && val.value is String && val.value != 'nil') {
          return GuardResult.success;
        }
        if (val is String && val != 'nil') {
          return GuardResult.success;
        }
        return GuardResult.failure;

      case 'constant':
        // Succeeds if X is a constant (a string, a number, or [])
        if (args.isEmpty) return GuardResult.failure;
        final val = getValue(args[0]);
        // String or nil (which represents [])
        if (val is ConstTerm && val.value is String) {
          return GuardResult.success;
        }
        if (val is String) {
          return GuardResult.success;
        }
        // Number
        if (val is num) {
          return GuardResult.success;
        }
        if (val is ConstTerm && val.value is num) {
          return GuardResult.success;
        }
        return GuardResult.failure;

      case 'number':
        // Succeeds if X is a number
        if (args.isEmpty) return GuardResult.failure;
        final val = getValue(args[0]);
        if (val is num) return GuardResult.success;
        if (val is ConstTerm && val.value is num) return GuardResult.success;
        return GuardResult.failure;

      case 'atom':
        // Succeeds if X is an atom (a constant or a tuple)
        if (args.isEmpty) return GuardResult.failure;
        final val = getValue(args[0]);
        // Constant: string (including 'nil'), number
        if (val is ConstTerm) return GuardResult.success;
        if (val is String) return GuardResult.success;
        if (val is num) return GuardResult.success;
        // Tuple: any StructTerm
        if (val is StructTerm) return GuardResult.success;
        return GuardResult.failure;

      case 'list':
        // Succeeds if X is a list ([] or [H|T])
        if (args.isEmpty) return GuardResult.failure;
        final val = getValue(args[0]);
        // Empty list: ConstTerm('nil') or raw String 'nil'
        if (val is ConstTerm && val.value == 'nil') {
          return GuardResult.success;
        }
        if (val is String && val == 'nil') {
          return GuardResult.success;
        }
        // Non-empty list: StructTerm('.', [head, tail])
        if (val is StructTerm && val.functor == '.' && val.args.length == 2) {
          return GuardResult.success;
        }
        return GuardResult.failure;

      case 'tuple':
        // Succeeds if X is a tuple (a term with a functor and arguments)
        if (args.isEmpty) return GuardResult.failure;
        final val = getValue(args[0]);
        // Tuple: any StructTerm (including '.' list cons cells per paper definition)
        if (val is StructTerm) {
          return GuardResult.success;
        }
        return GuardResult.failure;

      case 'writer':
        // Per spec 19.4.5: Test if Xi is an unbound writer
        if (args.isEmpty) return GuardResult.failure;
        final arg = args[0];
        if (arg is VarRef && !arg.isReader) {
          final heapVal = cx.rt.heap.getValue(arg.varId);
          if (heapVal == null) return GuardResult.success; // Unbound writer
        }
        return GuardResult.failure;

      case 'reader':
        // Per spec 19.4.6: Test if Xi is an unbound reader
        if (args.isEmpty) return GuardResult.failure;
        final arg = args[0];
        if (arg is VarRef && arg.isReader) {
          final heapVal = cx.rt.heap.getValue(arg.varId);
          if (heapVal == null) return GuardResult.success; // Unbound reader
        }
        return GuardResult.failure;

      // Control guards
      case 'otherwise':
        // This is handled by the compiler - should not reach runtime
        return GuardResult.success;

      case 'true':
        return GuardResult.success;

      // Time guards
      case 'wait':
        // wait(Duration) - Wait for Duration milliseconds using GLP suspension
        // Semantics:
        // - Unbound Duration: handled by caller (suspend on reader)
        // - Non-number: fail
        // - Duration <= 0: succeed immediately
        // - Duration > 0: create reader/writer pair, start timer, suspend on reader
        //   Timer fires → binds writer → ROQ reactivates goal
        // IMPORTANT: On resume, check if timer has already fired (avoid infinite loop)
        if (args.isEmpty) return GuardResult.failure;
        final duration = evaluateNumeric(args[0]);
        if (duration == null) return GuardResult.failure;
        if (duration <= 0) return GuardResult.success;

        // Check if this goal already has a pending wait
        final existingReader = cx.rt.getWaitReader(cx.goalId);
        if (existingReader != null) {
          // Goal resumed after suspension - check if timer fired
          if (cx.rt.heap.isFullyBound(existingReader)) {
            // Timer fired, reader is bound - clear state and succeed
            cx.rt.clearWaitState(cx.goalId);
            return GuardResult.success;
          } else {
            // Timer hasn't fired yet - keep suspending on same reader
            cx.U.add(existingReader);
            return GuardResult.failure;
          }
        }

        // First call - create fresh reader/writer pair for timer notification
        final (writerId, readerId) = cx.rt.heap.allocateFreshPair();

        // Store wait state for this goal
        cx.rt.setWaitReader(cx.goalId, readerId);

        // Track pending timer
        cx.rt.incrementPendingTimers();

        // Start timer that binds writer when it fires
        Timer(Duration(milliseconds: duration.toInt()), () {
          // Bind writer to 0 (any value works)
          final reactivated = cx.rt.heap.bindVariableConst(writerId, 0);
          // Enqueue reactivated goals
          for (final goalRef in reactivated) {
            cx.rt.gq.enqueue(goalRef);
          }
          // Decrement pending timer count
          cx.rt.decrementPendingTimers();
        });

        // Add reader to suspension set U and fail → triggers normal suspension
        cx.U.add(readerId);
        return GuardResult.failure;

      case 'wait_until':
        // wait_until(Timestamp) - Test if absolute time has passed
        // Semantics:
        // - Unbound Timestamp: handled by caller (suspend on reader)
        // - Non-number: fail
        // - current time >= Timestamp: succeed
        // - current time < Timestamp: FAIL (not suspend!)
        if (args.isEmpty) return GuardResult.failure;
        final timestamp = evaluateNumeric(args[0]);
        if (timestamp == null) return GuardResult.failure;
        final now = DateTime.now().millisecondsSinceEpoch;
        return now >= timestamp ? GuardResult.success : GuardResult.failure;

      default:
        print('[WARN] Unknown guard predicate: $predicateName');
        return GuardResult.failure;
    }
  }
}

/// Helper class to represent argument information
class _ArgInfo {
  final int? writerId;
  final int? readerId;

  _ArgInfo({this.writerId, this.readerId});

  bool get isWriter => writerId != null;
  bool get isReader => readerId != null;
}

/// Tentative structure during HEAD phase (before commit)
class _TentativeStruct {
  final String functor;
  final int arity;
  final List<Object?> args;

  _TentativeStruct(this.functor, this.arity, this.args);

  @override
  String toString() => '$functor/${arity}(${args.join(", ")})';
}

/// Helper to represent clause variables (before actual binding)
class _ClauseVar {
  final int varIndex;
  final bool isWriter;

  _ClauseVar(this.varIndex, {required this.isWriter});

  @override
  String toString() => isWriter ? 'W$varIndex' : 'R$varIndex';
}

/// Helper to represent list structures
class _ListStruct {
  final Object? head;
  final Object? tail;

  _ListStruct(this.head, this.tail);

  @override
  String toString() => '[$head|$tail]';
}

/// Helper to save/restore structure processing state for Push/Pop
class _StructureState {
  final int S;
  final UnifyMode mode;
  final dynamic currentStructure;

  _StructureState(this.S, this.mode, this.currentStructure);

  @override
  String toString() => 'StructureState(S=$S, mode=$mode, struct=$currentStructure)';
}

/// Helper function to recursively convert _TentativeStruct to StructTerm
StructTerm _convertTentativeToStruct(_TentativeStruct tentative, RunnerContext cx) {
  final termArgs = <Term>[];
  for (final arg in tentative.args) {
    if (arg is _TentativeStruct) {
      // Recursively convert nested tentative structures
      termArgs.add(_convertTentativeToStruct(arg, cx));
    } else if (arg is Term) {
      // Already a Term - use as-is
      termArgs.add(arg);
    } else if (arg == null) {
      // Null -> ConstTerm(null)
      termArgs.add(ConstTerm(null));
    } else {
      // Raw value -> ConstTerm
      termArgs.add(ConstTerm(arg));
    }
  }
  return StructTerm(tentative.functor, termArgs);
}
