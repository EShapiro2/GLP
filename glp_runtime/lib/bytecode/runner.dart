import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/commit.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/system_predicates.dart';
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
      if (op is Label) m[op.name] = i;
    }
    return m;
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

/// Goal-call environment: maps arg slots to WR/RO ids (set at run time).
class CallEnv {
  final Map<int,int> writerBySlot;
  final Map<int,int> readerBySlot;
  CallEnv({Map<int,int>? writers, Map<int,int>? readers})
      : writerBySlot = writers ?? <int,int>{},
        readerBySlot = readers ?? <int,int>{};
  int? w(int slot) => writerBySlot[slot];
  int? r(int slot) => readerBySlot[slot];

  /// Update environment with new argument mappings (for requeue/tail calls)
  void update(Map<int,int> newWriters, Map<int,int> newReaders) {
    writerBySlot.clear();
    writerBySlot.addAll(newWriters);
    readerBySlot.clear();
    readerBySlot.addAll(newReaders);
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
  final Set<int> si = <int>{};       // clause-local blockers (reader IDs)
  final Set<int> U = <int>{};        // union across clauses (reader IDs)
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
  final Map<int, int> argWriters = {};  // argSlot → writer ID
  final Map<int, int> argReaders = {};  // argSlot → reader ID

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

  RunnerContext({
    required this.rt,
    required this.goalId,
    required this.kappa,
    CallEnv? env,
    this.onActivation,
    this.reductionBudget,
    this.goalHead,
    this.onReduction,
  }) : env = env ?? CallEnv();

  void clearClause() {
    sigmaHat.clear();
    si.clear();
    inBody = false;
    mode = UnifyMode.read;
    S = 0;
    currentStructure = null;
    clauseVars.clear();
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

  /// Soft-fail to next clause: union Si to U, clear clause state, jump to next ClauseTry
  void _softFailToNextClause(RunnerContext cx, int currentPc) {
    // Union Si into U
    if (cx.si.isNotEmpty) cx.U.addAll(cx.si);
    // Clear clause-local state
    cx.clearClause();
    // Jump to next clause (will be handled by returning new PC)
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
      return 'W$wid';
    } else if (term is VarRef && term.isReader) {
      final rid = term.varId;
      final wid = rt.heap.writerIdForReader(rid);
      if (wid != null && rt.heap.isWriterBound(wid)) {
        final value = rt.heap.valueOfWriter(wid);
        if (value != null) {
          final formatted = _formatTerm(rt, value, markReaders: markReaders);
          return markReaders ? '$formatted?' : formatted;
        }
      }
      return markReaders ? 'R$rid?' : 'R$rid';
    } else if (term is StructTerm) {
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
      print('>>> TRY: Goal ${cx.goalId} at PC ${cx.kappa}');
    }

    while (pc < prog.ops.length) {
      // Check reduction budget
      if (cx.reductionBudget != null && cx.reductionsUsed >= cx.reductionBudget!) {
        return RunResult.outOfReductions;
      }
      cx.reductionsUsed++;

      final op = prog.ops[pc];

      if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) {
        print('  [G${cx.goalId}] PC=$pc ${op.runtimeType} | Si=${cx.si} U=${cx.U} inBody=${cx.inBody}');
      }

      if (op is Label) { pc++; continue; }
      if (op is ClauseTry) { cx.clearClause(); pc++; continue; }
      if (op is GuardFail) { pc++; continue; }

      // Otherwise guard: succeeds if Si is empty (all previous clauses failed, not suspended)
      if (op is Otherwise) {
        // Otherwise succeeds only if all previous clauses definitively failed
        // If any clause suspended (U non-empty), then otherwise should also suspend
        if (cx.U.isNotEmpty || cx.si.isNotEmpty) {
          // Previous clauses suspended, so this clause also suspends
          _softFailToNextClause(cx, pc);
          pc = _findNextClauseTry(pc);
          continue;
        }
        // U and Si both empty - all previous clauses definitely failed, so succeed
        pc++;
        continue;
      }

      // IfWriter guard: succeeds if variable is a writer
      if (op is IfWriter) {
        final term = cx.clauseVars[op.varIndex];
        if (term is VarRef && !term.isReader) {
          // It's a writer - succeed
          pc++;
          continue;
        } else {
          // Not a writer - fail this clause
          _softFailToNextClause(cx, pc);
          pc = _findNextClauseTry(pc);
          continue;
        }
      }

      // IfReader guard: succeeds if variable is a reader
      if (op is IfReader) {
        final term = cx.clauseVars[op.varIndex];
        if (term is VarRef && term.isReader) {
          // It's a reader - succeed
          pc++;
          continue;
        } else {
          // Not a reader - fail this clause
          _softFailToNextClause(cx, pc);
          pc = _findNextClauseTry(pc);
          continue;
        }
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

      // Mode selection (Arg)
      if (op is RequireWriterArg) {
        final wid = cx.env.w(op.slot);
        if (wid == null) { pc = prog.labels[op.failLabel]!; continue; }
        pc++; continue;
      }
      if (op is RequireReaderArg) {
        final rid = cx.env.r(op.slot);
        if (rid == null) { pc = prog.labels[op.failLabel]!; continue; }
        pc++; continue;
      }

      // ===== v2.16 HEAD instructions =====
      if (op is HeadConstant) {
        final arg = _getArg(cx, op.argSlot);
        if (arg == null) { pc++; continue; } // No argument at this slot

        if (arg.isWriter) {
          // Writer: check if already bound, else record tentative binding in σ̂w
          if (cx.rt.heap.isWriterBound(arg.writerId!)) {
            // Already bound - check if value matches
            final value = cx.rt.heap.valueOfWriter(arg.writerId!);
            if (value is ConstTerm && value.value != op.value) {
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
            // Wrap in ConstTerm so it's properly handled during Commit
            cx.sigmaHat[arg.writerId!] = ConstTerm(op.value);
          }
        } else if (arg.isReader) {
          // Reader: check if bound, else add to Si
          final wid = cx.rt.heap.writerIdForReader(arg.readerId!);
          if (wid == null || !cx.rt.heap.isWriterBound(wid)) {
            cx.si.add(arg.readerId!);
          } else {
            // Bound reader - check if value matches constant
            final value = cx.rt.heap.valueOfWriter(wid);
            if (debug && cx.goalId == 100) {
              print('  [DEBUG] HeadConstant checking reader ${arg.readerId}: value=$value vs pattern=${op.value}');
            }
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
          print('DEBUG MetaInterp: HeadStructure checking clauseVars[${op.argSlot}]: ${clauseVarValue?.runtimeType} = $clauseVarValue');
          if (clauseVarValue == null) {
            // Unbound clause variable - soft fail
            print('DEBUG MetaInterp: clauseVar ${op.argSlot} is NULL, failing');
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
            print('DEBUG SUSPEND: HeadStructure checking VarRef reader R$rid');
            final wid = cx.rt.heap.writerIdForReader(rid);
            print('DEBUG SUSPEND: writerIdForReader(R$rid) = $wid');
            if (wid == null || !cx.rt.heap.isWriterBound(wid)) {
              // Unbound reader - add to Si and soft fail
              print('DEBUG SUSPEND: Reader R$rid is UNBOUND! Adding to Si');
              print('  wid = $wid');
              if (wid != null) {
                print('  isWriterBound($wid) = ${cx.rt.heap.isWriterBound(wid)}');
              }
              cx.si.add(rid);
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            }
            print('DEBUG SUSPEND: Reader R$rid is bound to W$wid, dereferencing...');
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
            print('DEBUG MetaInterp: StructTerm path - functor="${clauseVarValue.functor}" vs op.functor="${op.functor}"');
            if (clauseVarValue.functor == op.functor && clauseVarValue.args.length == op.arity) {
              print('DEBUG MetaInterp: MATCH! Entering READ mode');
              cx.currentStructure = clauseVarValue;
              cx.mode = UnifyMode.read;
              cx.S = 0;
              pc++; continue;
            } else {
              print('DEBUG MetaInterp: NO MATCH - failing to next clause');
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

        // print('DEBUG: HeadStructure - about to check arg.isWriter, arg=${arg?.runtimeType}');
        if (arg!.isWriter) {
          // Check if writer is already bound
          // print('DEBUG: HeadStructure - arg is Writer ${arg.writerId}, bound=${cx.rt.heap.isWriterBound(arg.writerId!)}');
          if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadStructure: arg is writer ${arg.writerId}, bound=${cx.rt.heap.isWriterBound(arg.writerId!)}');
          if (cx.rt.heap.isWriterBound(arg.writerId!)) {
            // Already bound - check if matches structure
            final value = cx.rt.heap.valueOfWriter(arg.writerId!);
            // print('DEBUG: HeadStructure - Writer ${arg.writerId} value = ${value.runtimeType}: $value, expecting ${op.functor}/${op.arity}');
            if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadStructure: writer ${arg.writerId} value = $value');
            if (value is StructTerm && value.functor == op.functor && value.args.length == op.arity) {
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
          if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadStructure: WRITE mode for unbound writer ${arg.writerId}');
          final struct = _TentativeStruct(op.functor, op.arity, List.filled(op.arity, null));
          cx.sigmaHat[arg.writerId!] = struct;
          cx.currentStructure = struct;
          cx.mode = UnifyMode.write;
          cx.S = 0; // Start at first arg
          pc++; continue;
        }

        if (arg.isReader) {
          // print('DEBUG: HeadStructure - arg is Reader ${arg.readerId}');
          // Reader: check if bound and has matching structure
          final wid = cx.rt.heap.writerIdForReader(arg.readerId!);
          // print('DEBUG: HeadStructure - Reader ${arg.readerId} -> Writer $wid');
          if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadStructure: READ mode, reader ${arg.readerId} -> writer $wid');
          if (wid == null || !cx.rt.heap.isWriterBound(wid)) {
            // Unbound reader - add to Si and soft fail
            // print('DEBUG: HeadStructure - Writer $wid is unbound or null, adding to Si and soft failing');
            if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadStructure: writer $wid unbound or null, adding to Si and failing');
            cx.si.add(arg.readerId!);
            _softFailToNextClause(cx, pc);
            pc = _findNextClauseTry(pc);
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
        print('DEBUG: HeadStructure line 610 - GROUND TERM PATH');
        print('  op.argSlot = ${op.argSlot}');
        print('  arg = $arg');
        print('  arg?.isWriter = ${arg?.isWriter}');
        print('  arg?.isReader = ${arg?.isReader}');
        print('  isClauseVar = $isClauseVar');
        if (isClauseVar && op.argSlot < 100) {
          print('  clauseVars[${op.argSlot}] = ${cx.clauseVars[op.argSlot]}');
        }
        // TODO: Handle ground structures when CallEnv supports them
        _softFailToNextClause(cx, pc);
        pc = _findNextClauseTry(pc);
        continue;
      }

      if (op is HeadWriter) {
        // Process writer variable in structure (at S position)
        if (cx.mode == UnifyMode.write) {
          // WRITE mode: Building a structure - check if variable already bound
          if (cx.currentStructure is _TentativeStruct) {
            final struct = cx.currentStructure as _TentativeStruct;

            // Check if this clause variable already has a value (from get_variable)
            final existingValue = cx.clauseVars[op.varIndex];
            if (existingValue != null) {
              // Variable already bound - use its value
              struct.args[cx.S] = existingValue;
            } else {
              // New variable - create placeholder
              final placeholder = _ClauseVar(op.varIndex, isWriter: true);
              struct.args[cx.S] = placeholder;
              cx.clauseVars[op.varIndex] = placeholder;
            }
            cx.S++; // Advance to next arg
          }
        } else {
          // READ mode: Extract value from structure at S position into clause variable
          if (cx.currentStructure is StructTerm) {
            final struct = cx.currentStructure as StructTerm;
            if (cx.S < struct.args.length) {
              final value = struct.args[cx.S];

              // Check if variable already bound (from get_variable/get_value)
              final existingValue = cx.clauseVars[op.varIndex];
              if (existingValue != null) {
                // Need to unify - for now, just check equality
                // TODO: proper unification
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

      if (op is HeadReader) {
        // Process reader variable in structure (at S position)
        if (cx.mode == UnifyMode.write) {
          // WRITE mode: Building structure - add reader value/reference
          if (cx.currentStructure is _TentativeStruct) {
            final struct = cx.currentStructure as _TentativeStruct;

            // Check if this clause variable already has a value (from get_variable)
            final existingValue = cx.clauseVars[op.varIndex];
            if (existingValue != null) {
              // Variable already has a value
              if (existingValue is int) {
                // It's a variable ID from GetVariable - wrap in VarRef
                struct.args[cx.S] = VarRef(existingValue, isReader: true);
              } else {
                // It's already a Term - use as is
                struct.args[cx.S] = existingValue;
              }
            } else {
              // New variable - create placeholder
              final placeholder = _ClauseVar(op.varIndex, isWriter: false);
              struct.args[cx.S] = placeholder;
              cx.clauseVars[op.varIndex] = placeholder;
            }
            cx.S++; // Advance to next arg
          }
        } else {
          // READ mode: Verify value at S matches paired writer in tentative state
          if (cx.currentStructure is StructTerm) {
            final struct = cx.currentStructure as StructTerm;
            if (cx.S < struct.args.length) {
              final value = struct.args[cx.S];

              // Check if variable already bound (from get_variable/get_value)
              final existingValue = cx.clauseVars[op.varIndex];
              if (existingValue != null) {
                // Need to unify - for now, just check equality
                // TODO: proper unification
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

        // Store bare IDs (not VarRef) - HEAD instructions expect bare IDs
        // Guards handle both VarRef and bare int for dereferencing
        if (arg.isWriter) {
          cx.clauseVars[op.varIndex] = arg.writerId!;
        } else if (arg.isReader) {
          cx.clauseVars[op.varIndex] = arg.readerId!;
        } else {
          // Ground term - would need to handle if CallEnv supported it
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
        if (arg.isWriter) {
          // Argument is a writer - bind it to stored value in σ̂w
          if (storedValue is VarRef && !storedValue.isReader) {
            // storedValue is a writer VarRef - check they match
            if (arg.writerId != storedValue.varId) {
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            }
          } else if (storedValue is int) {
            // Legacy: bare writer ID - check they match
            if (arg.writerId != storedValue) {
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
              cx.sigmaHat[arg.writerId!] = readerValue;
            } else {
              // Reader's writer is unbound - add reader to Si (suspend)
              cx.si.add(readerId);
            }
          } else {
            // storedValue is a Term - bind writer to it
            cx.sigmaHat[arg.writerId!] = storedValue;
          }
        } else if (arg.isReader) {
          // Argument is a reader - verify it matches stored value
          if (storedValue is VarRef && storedValue.isReader) {
            // storedValue is also a reader - fail definitively
            // (clause-local reader can never be bound in the future)
            _softFailToNextClause(cx, pc);
            pc = _findNextClauseTry(pc);
            continue;
          }

          final wid = cx.rt.heap.writerIdForReader(arg.readerId!);
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
            cx.si.add(arg.readerId!);
          }
        } else {
          // Ground term - TODO
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
            // BODY phase structure building
            final struct = cx.currentStructure as StructTerm;
            // If value is already a Term (e.g., StructTerm), use it directly
            // Otherwise wrap in ConstTerm
            struct.args[cx.S] = op.value is Term ? op.value as Term : ConstTerm(op.value);
            cx.S++; // Advance to next arg

            // Check if structure is complete
            if (cx.S >= struct.args.length) {
              // Structure complete - bind the target writer (stored at clauseVars[-1])
              final targetWriterId = cx.clauseVars[-1];
              if (targetWriterId is int) {
                // Bind the writer to the completed structure
                cx.rt.heap.bindWriterStruct(targetWriterId, struct.functor, struct.args);

                // Reset structure building state
                cx.currentStructure = null;
                cx.mode = UnifyMode.read;
                cx.S = 0;
                cx.clauseVars.remove(-1);
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
                  cx.si.add(rid);
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

      if (op is UnifyWriter) {
        // Match/add writer variable at current S position
        if (cx.mode == UnifyMode.write) {
          // WRITE mode: Add writer to structure being built
          if (debug && cx.goalId == 100) print('  [G${cx.goalId}] UnifyWriter: WRITE mode, varIndex=${op.varIndex}');
          if (cx.currentStructure is _TentativeStruct) {
            final struct = cx.currentStructure as _TentativeStruct;
            final value = cx.clauseVars[op.varIndex];
            if (value is VarRef) {
              // Already a VarRef - use directly
              struct.args[cx.S] = value;
            } else if (value is int) {
              // Legacy: bare ID - create VarRef
              struct.args[cx.S] = VarRef(value, isReader: false);
            } else if (value is ConstTerm || value is StructTerm) {
              // It's a ground term extracted from READ mode - use directly
              struct.args[cx.S] = value;
            } else if (value == null) {
              // Create fresh variable
              final varId = cx.rt.heap.allocateFreshVar();
              cx.rt.heap.addVariable(varId);
              // CRITICAL FIX: Store VarRef, not bare ID
              cx.clauseVars[op.varIndex] = VarRef(varId, isReader: false);
              struct.args[cx.S] = VarRef(varId, isReader: false);
            } else {
              struct.args[cx.S] = _ClauseVar(op.varIndex, isWriter: true);
            }
            cx.S++;
          } else if (cx.currentStructure is StructTerm) {
            // BODY phase structure building
            final struct = cx.currentStructure as StructTerm;
            final value = cx.clauseVars[op.varIndex];
            if (value is VarRef) {
              // Already a VarRef - use directly
              struct.args[cx.S] = value;
            } else if (value is int) {
              // Legacy: bare ID - create VarRef
              struct.args[cx.S] = VarRef(value, isReader: false);
            } else if (value is Term) {
              // Ground term (ConstTerm or StructTerm) - use directly
              struct.args[cx.S] = value;
            } else if (value == null) {
              // Create fresh variable
              final varId = cx.rt.heap.allocateFreshVar();
              cx.rt.heap.addVariable(varId);
              // CRITICAL FIX: Store VarRef, not bare ID
              cx.clauseVars[op.varIndex] = VarRef(varId, isReader: false);
              struct.args[cx.S] = VarRef(varId, isReader: false);
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
                cx.currentStructure = null;
                cx.mode = UnifyMode.read;
                cx.S = 0;
                cx.clauseVars.remove(-1);
              }
            }
          }
        } else {
          // READ mode: Unify with writer at S position
          // Similar to UnifyConstant logic but for writers
          if (debug && cx.goalId == 100) print('  [G${cx.goalId}] UnifyWriter: READ mode, varIndex=${op.varIndex}, S=${cx.S}');
          if (cx.currentStructure is StructTerm) {
            final struct = cx.currentStructure as StructTerm;
            if (cx.S < struct.args.length) {
              final value = struct.args[cx.S];
              if (debug && cx.goalId == 100) print('  [G${cx.goalId}] UnifyWriter: struct.args[${cx.S}] = $value');
              // Store the writer in clause var
              if (value is VarRef && !value.isReader) {
                // CRITICAL FIX: Store the VarRef itself, not just the ID
                cx.clauseVars[op.varIndex] = value;
                cx.S++;
              } else if (value is VarRef && value.isReader) {
                // Extract reader term - dereference if bound, store as-is if unbound
                final rid = value.varId;
                final wid = cx.rt.heap.writerIdForReader(rid);
                if (wid != null && cx.rt.heap.isWriterBound(wid)) {
                  // Reader is bound - extract the actual value from the paired writer
                  final writerValue = cx.rt.heap.valueOfWriter(wid);
                  cx.clauseVars[op.varIndex] = writerValue;
                  cx.S++;
                } else {
                  // Unbound reader - store the reader term itself
                  // Suspension will be handled later if/when we try to match against it
                  cx.clauseVars[op.varIndex] = value;
                  cx.S++;
                }
              } else if (value is ConstTerm || value is StructTerm) {
                // Direct term value - store it
                cx.clauseVars[op.varIndex] = value;
                // print('DEBUG: UnifyWriter stored ${value.runtimeType} in clauseVars[${op.varIndex}]: $value');
                cx.S++;
              } else {
                // Unexpected type - mismatch
                _softFailToNextClause(cx, pc);
                pc = _findNextClauseTry(pc);
                continue;
              }
            }
          }
        }
        pc++; continue;
      }

      if (op is UnifyReader) {
        // Match/add reader variable at current S position
        if (cx.mode == UnifyMode.write) {
          // WRITE mode: Add reader to structure being built
          if (cx.currentStructure is _TentativeStruct) {
            final struct = cx.currentStructure as _TentativeStruct;
            final clauseVarValue = cx.clauseVars[op.varIndex];
            if (clauseVarValue is VarRef && clauseVarValue.isReader) {
              // Clause var is already a reader term - use it directly
              struct.args[cx.S] = clauseVarValue;
            } else if (clauseVarValue is VarRef && !clauseVarValue.isReader) {
              // It's a writer VarRef - get paired reader
              final wc = cx.rt.heap.writer(clauseVarValue.varId);
              if (wc != null) {
                struct.args[cx.S] = VarRef(wc.readerId, isReader: true);
              }
            } else if (clauseVarValue is int) {
              // Legacy: bare ID - create VarRef as reader
              final wc = cx.rt.heap.writer(clauseVarValue);
              if (wc != null) {
                struct.args[cx.S] = VarRef(wc.readerId, isReader: true);
              }
            } else if (clauseVarValue is Term) {
              // Clause var is bound to a term (e.g., [] or a structure)
              // Create a fresh variable and tentatively bind it to this value in σ̂w
              final varId = cx.rt.heap.allocateFreshVar();
              cx.rt.heap.addVariable(varId);
              // Add tentative binding to σ̂w (will be applied at commit)
              cx.sigmaHat[varId] = clauseVarValue;
              // Don't update clauseVars - keep the original value
              struct.args[cx.S] = VarRef(varId, isReader: true);
            } else if (clauseVarValue == null) {
              // First occurrence of this variable is a reader!
              // Must create the variable first (unbound)
              final varId = cx.rt.heap.allocateFreshVar();
              cx.rt.heap.addVariable(varId);
              // CRITICAL FIX: Store VarRef, not bare ID
              cx.clauseVars[op.varIndex] = VarRef(varId, isReader: true);
              struct.args[cx.S] = VarRef(varId, isReader: true);
            }
            cx.S++;
          } else if (cx.currentStructure is StructTerm) {
            // BODY phase structure building
            final struct = cx.currentStructure as StructTerm;
            final clauseVarValue = cx.clauseVars[op.varIndex];
            if (debug) {
              print('  [G${cx.goalId}] UnifyReader BODY: varIndex=${op.varIndex}, clauseVarValue=$clauseVarValue, clauseVars=${cx.clauseVars}');
            }
            if (clauseVarValue is VarRef && clauseVarValue.isReader) {
              // Already a reader term - use directly
              struct.args[cx.S] = clauseVarValue;
              if (debug) print('  [G${cx.goalId}] UnifyReader BODY: Using existing ReaderTerm $clauseVarValue');
            } else if (clauseVarValue is VarRef && !clauseVarValue.isReader) {
              // It's a writer VarRef - get paired reader
              final wc = cx.rt.heap.writer(clauseVarValue.varId);
              if (wc != null) {
                struct.args[cx.S] = VarRef(wc.readerId, isReader: true);
                if (debug) print('  [G${cx.goalId}] UnifyReader BODY: Using paired reader ${wc.readerId} for writer ${clauseVarValue.varId}');
              }
            } else if (clauseVarValue is int) {
              // Legacy: bare ID - create VarRef as reader
              final wc = cx.rt.heap.writer(clauseVarValue);
              if (wc != null) {
                struct.args[cx.S] = VarRef(wc.readerId, isReader: true);
                if (debug) print('  [G${cx.goalId}] UnifyReader BODY: Using paired reader ${wc.readerId} for writer $clauseVarValue');
              }
            } else if (clauseVarValue == null) {
              // First occurrence as reader - create variable
              final varId = cx.rt.heap.allocateFreshVar();
              cx.rt.heap.addVariable(varId);
              // CRITICAL FIX: Store VarRef, not bare ID
              cx.clauseVars[op.varIndex] = VarRef(varId, isReader: true);
              struct.args[cx.S] = VarRef(varId, isReader: true);
              if (debug) print('  [G${cx.goalId}] UnifyReader BODY: Creating FRESH variable V$varId for varIndex=${op.varIndex}');
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
                cx.currentStructure = null;
                cx.mode = UnifyMode.read;
                cx.S = 0;
                cx.clauseVars.remove(-1);
              }
            }
          }
        } else {
          // READ mode: Unify with reader at S position
          if (cx.currentStructure is StructTerm) {
            final struct = cx.currentStructure as StructTerm;
            if (cx.S < struct.args.length) {
              final value = struct.args[cx.S];
              if (value is VarRef && value.isReader) {
                // Store the writer ID (not the reader ID) in clause var
                final rid = value.varId;
                final wid = cx.rt.heap.writerIdForReader(rid);
                if (wid != null) {
                  cx.clauseVars[op.varIndex] = wid;
                }
                cx.S++;
              } else if (value is VarRef && !value.isReader) {
                // Writer term - store the writer ID directly
                // This happens when clause head expects reader but structure has writer
                cx.clauseVars[op.varIndex] = value.varId;
                cx.S++;
              } else {
                // Mismatch
                _softFailToNextClause(cx, pc);
                pc = _findNextClauseTry(pc);
                continue;
              }
            }
          }
        }
        pc++; continue;
      }

      // UnifyVariable: unified writer/reader structure traversal
      if (op is opv2.UnifyVariable) {
        // Transform to v1 instruction based on isReader flag and re-execute
        final v1Op = op.isReader
            ? UnifyReader(op.varIndex)
            : UnifyWriter(op.varIndex);

        // Replace current instruction with v1 equivalent and re-execute
        // This is safe because we're just dispatching based on the flag
        prog.ops[pc] = v1Op;
        continue; // Re-execute at same PC with v1 instruction
      }

      // Legacy HEAD opcodes (for backward compatibility)
      if (op is HeadBindWriter) {
        // Mark writer as involved (no value binding for legacy opcode)
        cx.sigmaHat[op.writerId] = null;
        pc++; continue;
      }
      if (op is HeadBindWriterArg) {
        final wid = cx.env.w(op.slot);
        if (wid != null) cx.sigmaHat[wid] = null;
        pc++; continue;
      }
      if (op is GuardNeedReader) {
        final rid = op.readerId;
        final wid = cx.rt.heap.writerIdForReader(rid);
        final bound = (wid != null) && cx.rt.heap.isWriterBound(wid);
        if (!bound) cx.si.add(rid);
        pc++; continue;
      }
      if (op is GuardNeedReaderArg) {
        final rid = cx.env.r(op.slot);
        if (rid != null) {
          final wid = cx.rt.heap.writerIdForReader(rid);
          final bound = (wid != null) && cx.rt.heap.isWriterBound(wid);
          if (!bound) cx.si.add(rid);
        }
        pc++; continue;
      }

      // Commit (apply σ̂w and wake suspended goals) - v2.16 semantics
      if (op is Commit) {
        // print('DEBUG: Commit - Si=${cx.si}, sigmaHat has ${cx.sigmaHat.length} entries');
        for (final entry in cx.sigmaHat.entries) {
          // print('DEBUG: Commit - σ̂w[${ entry.key}] = ${entry.value}');
        }
        // If Si is non-empty, this clause soft-failed - skip commit and jump to next clause
        if (cx.si.isNotEmpty) {
          // print('DEBUG: Commit - SKIPPING (Si non-empty)');
          _softFailToNextClause(cx, pc);
          pc = _findNextClauseTry(pc);
          continue;
        }
        // print('DEBUG: Commit - APPLYING σ̂w to heap');

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
                    // Reader placeholder but resolved to writer? Get paired reader
                    final wc = cx.rt.heap.writer(resolved.varId);
                    if (wc != null) {
                      termArgs.add(VarRef(wc.readerId, isReader: true));
                    }
                  }
                } else if (resolved is int) {
                  // Legacy: bare ID - use writer or reader based on isWriter flag
                  final wc = cx.rt.heap.writer(resolved);
                  if (wc != null) {
                    if (arg.isWriter) {
                      termArgs.add(VarRef(resolved, isReader: false));
                    } else {
                      termArgs.add(VarRef(wc.readerId, isReader: true));
                    }
                  } else {
                    // Shouldn't happen - create fresh variable as fallback using heap allocation
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
          print('>>> REDUCTION: Goal ${cx.goalId} at PC $pc (commit succeeded, σ̂w has ${convertedSigmaHat.length} bindings)');
        }

        // Apply σ̂w: bind writers to tentative values, then wake suspended goals
        final acts = CommitOps.applySigmaHatV216(
          heap: cx.rt.heap,
          roq: cx.rt.roq,
          sigmaHat: convertedSigmaHat,
        );
        for (final a in acts) {
          if (debug) {
            print('>>> ACTIVATION: Goal ${a.id} awakened at PC ${a.pc}');
          }
          cx.rt.gq.enqueue(a);
          if (cx.onActivation != null) cx.onActivation!(a);
        }
        cx.sigmaHat.clear();
        cx.inBody = true;
        pc++; continue;
      }

      // Clause control / suspend

      // clause_next: Unified instruction for moving to next clause (spec 2.2)
      // Discard σ̂w, union Si into U, clear clause state, jump to next clause
      if (op is ClauseNext) {
        if (cx.si.isNotEmpty) cx.U.addAll(cx.si);
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
        if (cx.U.isNotEmpty) {
          if (debug) {
            print('>>> SUSPENSION: Goal ${cx.goalId} suspended on readers: ${cx.U}');
          }
          cx.rt.suspendGoal(goalId: cx.goalId, kappa: cx.kappa, readers: cx.U);
          cx.U.clear();
          cx.inBody = false;
          return RunResult.suspended;
        }
        // U is empty - all clauses failed definitively (no suspension)
        if (debug) {
          print('>>> FAIL: Goal ${cx.goalId} (all clauses exhausted, U empty)');
        }
        cx.inBody = false;
        // According to spec, failed goals should be added to F set
        // For now, just terminate - the goal is done (failed)
        return RunResult.terminated;
      }

      // Legacy instructions (deprecated, use ClauseNext instead)
      if (op is UnionSiAndGoto) {
        if (cx.si.isNotEmpty) cx.U.addAll(cx.si);
        cx.clearClause();
        pc = prog.labels[op.label]!;
        continue;
      }
      if (op is ResetAndGoto) { cx.clearClause(); pc = prog.labels[op.label]!; continue; }

      // Legacy SuspendEnd (use NoMoreClauses instead)
      if (op is SuspendEnd) {
        if (cx.U.isNotEmpty) {
          if (debug) {
            print('>>> SUSPENSION: Goal ${cx.goalId} suspended on readers: ${cx.U}');
          }
          cx.rt.suspendGoal(goalId: cx.goalId, kappa: cx.kappa, readers: cx.U);
          cx.U.clear();
          cx.inBody = false;
          return RunResult.suspended;
        }
        // U is empty - all clauses failed definitively (no suspension)
        if (debug) {
          print('>>> FAIL: Goal ${cx.goalId} (all clauses exhausted, U empty)');
        }
        cx.inBody = false;
        // According to spec, failed goals should be added to F set
        // For now, just terminate - the goal is done (failed)
        return RunResult.terminated;
      }

      // Body (bind then wake + log)
      if (op is BodySetConst) {
        if (cx.inBody) {
          cx.rt.heap.bindWriterConst(op.writerId, op.value);
          final w = cx.rt.heap.writer(op.writerId);
          if (w != null) {
            final acts = cx.rt.roq.processOnBind(w.readerId);
            for (final a in acts) {
              cx.rt.gq.enqueue(a);
              if (cx.onActivation != null) cx.onActivation!(a);
            }
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
          cx.rt.heap.bindWriterStruct(op.writerId, op.functor, args);
          final w = cx.rt.heap.writer(op.writerId);
          if (w != null) {
            final acts = cx.rt.roq.processOnBind(w.readerId);
            for (final a in acts) {
              cx.rt.gq.enqueue(a);
              if (cx.onActivation != null) cx.onActivation!(a);
            }
          }
        }
        pc++; continue;
      }
      if (op is BodySetConstArg) {
        final wid = cx.env.w(op.slot);
        if (cx.inBody && wid != null) {
          cx.rt.heap.bindWriterConst(wid, op.value);
          final w = cx.rt.heap.writer(wid);
          if (w != null) {
            final acts = cx.rt.roq.processOnBind(w.readerId);
            for (final a in acts) {
              cx.rt.gq.enqueue(a);
              if (cx.onActivation != null) cx.onActivation!(a);
            }
          }
        }
        pc++; continue;
      }

      // ===== BODY argument setup instructions =====
      if (op is PutWriter) {
        if (cx.inBody) {
          if (debug) print('  [G${cx.goalId}] PC=$pc PutWriter varIndex=${op.varIndex} argSlot=${op.argSlot} clauseVars=${cx.clauseVars}');
          // Get writer ID from clause variable
          final value = cx.clauseVars[op.varIndex];
          if (debug) print('  [G${cx.goalId}] PutWriter: value=$value, type=${value.runtimeType}');
          if (value is VarRef) {
            // It's a VarRef - extract writer ID and store in argument register
            if (debug) print('  [G${cx.goalId}] PutWriter: storing writer ${value.varId} in argWriters[${op.argSlot}]');
            cx.argWriters[op.argSlot] = value.varId;
          } else if (value is int) {
            // Legacy: bare int (should not happen after our fixes, but keep for safety)
            if (debug) print('  [G${cx.goalId}] PutWriter: storing writer $value in argWriters[${op.argSlot}]');
            cx.argWriters[op.argSlot] = value;
          } else if (value is _ClauseVar) {
            // It's a placeholder - we need to create an actual variable (WAM-style)
            // This happens when HeadWriter created a placeholder in WRITE mode
            final varId = cx.rt.heap.allocateFreshVar();
            cx.rt.heap.addVariable(varId);
            cx.argWriters[op.argSlot] = varId;
            // Update clause var to point to the actual variable
            // CRITICAL FIX: Store VarRef, not bare ID
            cx.clauseVars[op.varIndex] = VarRef(varId, isReader: false);
          } else if (value == null) {
            // Variable doesn't exist yet - allocate fresh variable
            // This happens when a variable first appears in BODY phase
            final varId = cx.rt.heap.allocateFreshVar();
            cx.rt.heap.addVariable(varId);
            cx.argWriters[op.argSlot] = varId;
            // Store in clause var for future references
            // CRITICAL FIX: Store VarRef, not bare ID
            cx.clauseVars[op.varIndex] = VarRef(varId, isReader: false);
          } else {
            print('WARNING: PutWriter got unexpected value: $value');
          }
        }
        if (debug) print('  [G${cx.goalId}] PutWriter: about to increment PC to ${pc+1}');
        pc++;
        if (debug) print('  [G${cx.goalId}] PutWriter: continuing to PC $pc');
        continue;
      }

      if (op is PutReader) {
        if (cx.inBody) {
          if (debug) {
            print('  [G${cx.goalId}] PC=$pc PutReader varIndex=${op.varIndex} argSlot=${op.argSlot} clauseVars=${cx.clauseVars}');
          }
          // Get ID from clause variable - could be VarRef, StructTerm, ConstTerm
          final value = cx.clauseVars[op.varIndex];
          if (value is VarRef) {
            // It's a VarRef - handle writer vs reader
            if (value.isReader) {
              // Already a reader - use its ID directly
              cx.argReaders[op.argSlot] = value.varId;
            } else {
              // It's a writer - get its paired reader
              final wc = cx.rt.heap.writer(value.varId);
              if (wc != null) {
                if (debug && cx.goalId == 100) print('  [G${cx.goalId}] PutReader: Setting argReaders[${op.argSlot}] = ${wc.readerId}');
                cx.argReaders[op.argSlot] = wc.readerId;
              } else {
                print('WARNING: PutReader got writer VarRef but no WriterCell found');
              }
            }
          } else if (value is int) {
            // Legacy: bare int (should not happen after our fixes, but keep for safety)
            final wc = cx.rt.heap.writer(value);
            if (debug && cx.goalId == 100) print('  [G${cx.goalId}] PutReader: value=$value, wc=$wc, wc.readerId=${wc?.readerId}');
            if (wc != null) {
              // It's a writer ID - get its paired reader
              if (debug && cx.goalId == 100) print('  [G${cx.goalId}] PutReader: Setting argReaders[${op.argSlot}] = ${wc.readerId}');
              cx.argReaders[op.argSlot] = wc.readerId;
            } else {
              // Not a writer - assume it's already a reader ID
              if (debug && cx.goalId == 100) print('  [G${cx.goalId}] PutReader: Not a writer, assuming reader ID, setting argReaders[${op.argSlot}] = $value');
              cx.argReaders[op.argSlot] = value;
            }
          } else if (value is StructTerm) {
            // It's a structure - create fresh variable and bind it (WAM-style)
            final varId = cx.rt.heap.allocateFreshVar();
            cx.rt.heap.addVariable(varId);
            cx.rt.heap.bindWriterStruct(varId, value.functor, value.args);
            cx.argReaders[op.argSlot] = varId;
          } else if (value is ConstTerm) {
            // It's a ground constant - create fresh variable and bind it (WAM-style)
            if (debug) print('  [G${cx.goalId}] PutReader: creating fresh variable for ground constant ${value.value}');
            final varId = cx.rt.heap.allocateFreshVar();
            cx.rt.heap.addVariable(varId);
            cx.rt.heap.bindWriterConst(varId, value.value);
            cx.argReaders[op.argSlot] = varId;
            if (debug) print('  [G${cx.goalId}] PutReader: created V$varId for constant');
          } else if (value == null) {
            // First occurrence of this variable - create fresh unbound variable
            final varId = cx.rt.heap.allocateFreshVar();
            cx.rt.heap.addVariable(varId);
            // CRITICAL FIX: Store VarRef, not bare ID
            cx.clauseVars[op.varIndex] = VarRef(varId, isReader: false);
            cx.argReaders[op.argSlot] = varId;
            if (debug) print('  [G${cx.goalId}] PutReader: created fresh unbound variable V$varId for first occurrence');
          } else {
            // Unknown - skip
            print('WARNING: PutReader got unexpected value: $value');
          }
        }
        if (debug) print('  [G${cx.goalId}] PutReader: continuing to PC ${pc+1}');
        pc++; continue;
      }

      // PutVariable: unified writer/reader argument placement
      if (op is opv2.PutVariable) {
        // Transform to v1 instruction based on isReader flag and re-execute
        final v1Op = op.isReader
            ? PutReader(op.varIndex, op.argSlot)
            : PutWriter(op.varIndex, op.argSlot);

        // Replace current instruction with v1 equivalent and re-execute
        prog.ops[pc] = v1Op;
        continue; // Re-execute at same PC with v1 instruction
      }

      if (op is PutConstant) {
        if (cx.inBody) {
          // For constants, we create a fresh variable and bind immediately (WAM-style)
          final varId = cx.rt.heap.allocateFreshVar();
          cx.rt.heap.addVariable(varId);
          cx.rt.heap.bindWriterConst(varId, op.value);
          cx.argReaders[op.argSlot] = varId;
        }
        pc++; continue;
      }

      // ===== WAM-style structure creation (BODY phase) =====
      if (op is PutStructure) {
        if (cx.inBody) {
          // WAM semantics: HEAP[H] ← <STR, H+1>; HEAP[H+1] ← F/n; Ai ← HEAP[H]; H ← H+2; mode ← WRITE
          // In GLP: Create fresh variable for argument passing

          // Create fresh variable for this structure
          final varId = cx.rt.heap.allocateFreshVar();
          cx.rt.heap.addVariable(varId);

          // Handle different argSlot cases FIRST - save parent before overwriting
          // -1 = nested structure being built inside parent (don't put in argReaders)
          // 0-9 = argument slot (put reader in argReaders for goal passing)
          // 10+ = temp register (store writer in clauseVars)
          if (op.argSlot == -1 || cx.currentStructure != null) {
            // Nested structure - save parent context before starting nested build
            // This triggers not just for -1 but also when we're already building a structure
            // print('DEBUG: PutStructure - SAVING PARENT CONTEXT: currentStruct=${(cx.currentStructure as StructTerm?)?.functor}, S=${cx.S}, parentWriterId=${cx.clauseVars[-1]}');
            cx.parentStructure = cx.currentStructure;
            cx.parentS = cx.S;
            cx.parentMode = cx.mode;
            cx.parentWriterId = cx.clauseVars[-1]; // Save parent's writer ID BEFORE we overwrite it
          }

          // NOW store the variable ID for this new structure
          // print('DEBUG: PutStructure ${op.functor}/${op.arity} (argSlot=${op.argSlot}) - storing varId $varId at clauseVars[-1]');
          cx.clauseVars[-1] = varId; // Use -1 as special marker for structure binding

          if (op.argSlot >= 0 && op.argSlot < 10) {
            // This is an argument slot - store the variable for passing to spawned goal
            cx.argReaders[op.argSlot] = varId;
          } else {
            // This is a temp register - store variable in clauseVars for later reference
            // CRITICAL FIX: Store VarRef, not bare ID
            cx.clauseVars[op.argSlot] = VarRef(varId, isReader: false);
          }

          // Create structure with placeholder args (will be filled by subsequent Set* instructions)
          // Use ConstTerm(null) as placeholder, will be replaced
          final structArgs = List<Term>.filled(op.arity, ConstTerm(null));
          cx.currentStructure = StructTerm(op.functor, structArgs);
          cx.S = 0; // Start at first argument position
          cx.mode = UnifyMode.write;
        }
        pc++; continue;
      }

      if (op is SetWriter) {
        if (cx.inBody && cx.mode == UnifyMode.write && cx.currentStructure is StructTerm) {
          // Check if writer already exists in clause variables
          final existingWriterId = cx.clauseVars[op.varIndex];
          final int writerId;

          if (existingWriterId is VarRef) {
            // Use existing writer from VarRef
            writerId = existingWriterId.varId;
          } else if (existingWriterId is int) {
            // Legacy: bare int (use it directly)
            writerId = existingWriterId;
          } else {
            // Allocate new variable only if uninitialized
            final varId = cx.rt.heap.allocateFreshVar();
            cx.rt.heap.addVariable(varId);

            // Store variable ID in clause variable
            // CRITICAL FIX: Store VarRef, not bare ID
            cx.clauseVars[op.varIndex] = VarRef(varId, isReader: false);
            writerId = varId;
          }

          // Store VarRef (writer mode) in current structure at position S
          final struct = cx.currentStructure as StructTerm;
          struct.args[cx.S] = VarRef(writerId, isReader: false);
          cx.S++; // Move to next position

          // Check if structure is complete (all arguments filled)
          if (cx.S >= struct.args.length) {
            // Structure complete - bind the target writer (stored at clauseVars[-1])
            final targetValue = cx.clauseVars[-1];
            int? targetWriterId;
            if (targetValue is VarRef) {
              targetWriterId = targetValue.varId;
            } else if (targetValue is int) {
              targetWriterId = targetValue;
            }
            // print('DEBUG: SetWriter - structure complete! functor=${struct.functor}, S=${cx.S}/${struct.args.length}, targetWriter=$targetWriterId, hasParent=${cx.parentStructure != null}');
            if (targetWriterId != null) {
              // Bind the writer to the completed structure
              cx.rt.heap.bindWriterStruct(targetWriterId, struct.functor, struct.args);

              // Activate any suspended goals
              final w = cx.rt.heap.writer(targetWriterId);
              if (w != null) {
                final acts = cx.rt.roq.processOnBind(w.readerId);
                for (final a in acts) {
                  cx.rt.gq.enqueue(a);
                  if (cx.onActivation != null) cx.onActivation!(a);
                }
              }
            }

            // Reset structure building state
            // If there's a parent structure, restore it; otherwise clear completely
            if (cx.parentStructure != null && targetWriterId is int) {
              // We just completed a nested structure - add it to parent and restore context
              final nestedWriterId = targetWriterId as int;

              // Get the saved parent's writer ID
              final parentWriterId = cx.parentWriterId;
              // print('DEBUG: SetWriter - restoring parent context, parentWriterId=$parentWriterId, parentStruct=${(cx.parentStructure as StructTerm?)?.functor}');

              // Add the completed nested structure to the parent BEFORE restoring context
              if (cx.parentStructure is StructTerm) {
                final parentStruct = cx.parentStructure as StructTerm;
                final wc = cx.rt.heap.writer(nestedWriterId);
                if (wc != null) {
                  parentStruct.args[cx.parentS] = VarRef(wc.readerId, isReader: true);
                }
              }

              // Now restore parent context with incremented S
              cx.currentStructure = cx.parentStructure;
              cx.S = cx.parentS + 1; // Move to next position in parent
              cx.mode = cx.parentMode;

              // Clear parent context for next potential nesting
              cx.parentStructure = null;
              cx.parentS = 0;
              cx.parentMode = UnifyMode.read;
              cx.parentWriterId = null;

              // Restore parent's writer ID to clauseVars[-1] for potential completion
              cx.clauseVars[-1] = parentWriterId;

              // Check if parent is now complete!
              if (cx.currentStructure is StructTerm) {
                final parentStruct = cx.currentStructure as StructTerm;
                // print('DEBUG: SetWriter - checking parent completion: S=${cx.S}, arity=${parentStruct.args.length}, parentWriterId=$parentWriterId');
                if (cx.S >= parentStruct.args.length && parentWriterId is int) {
                  // print('DEBUG: SetWriter - PARENT COMPLETE! Binding parent ${parentStruct.functor}/${parentStruct.args.length}');
                  // Bind the parent structure
                  cx.rt.heap.bindWriterStruct(parentWriterId as int, parentStruct.functor, parentStruct.args);

                  // Activate suspended goals
                  final w = cx.rt.heap.writer(parentWriterId as int);
                  if (w != null) {
                    final acts = cx.rt.roq.processOnBind(w.readerId);
                    for (final a in acts) {
                      cx.rt.gq.enqueue(a);
                      if (cx.onActivation != null) cx.onActivation!(a);
                    }
                  }

                  // Clear structure building state
                  cx.currentStructure = null;
                  cx.mode = UnifyMode.read;
                  cx.S = 0;
                  cx.clauseVars.remove(-1);
                }
              }
            } else {
              // Top-level structure complete - clear everything
              cx.currentStructure = null;
              cx.mode = UnifyMode.read;
              cx.S = 0;
              cx.clauseVars.remove(-1); // Clear the marker
            }
          }
        }
        pc++; continue;
      }

      if (op is SetReader) {
        if (cx.inBody && cx.mode == UnifyMode.write && cx.currentStructure is StructTerm) {
          // Check if writer already exists in clause variables
          final existingWriterId = cx.clauseVars[op.varIndex];
          final int writerId;

          if (existingWriterId is VarRef) {
            // Use existing writer from VarRef
            writerId = existingWriterId.varId;
          } else if (existingWriterId is int) {
            // Legacy: bare int (use it directly)
            writerId = existingWriterId;
          } else {
            // Allocate new variable only if uninitialized
            final varId = cx.rt.heap.allocateFreshVar();
            cx.rt.heap.addVariable(varId);

            // Store variable ID in clause variable
            // CRITICAL FIX: Store VarRef, not bare ID
            cx.clauseVars[op.varIndex] = VarRef(varId, isReader: false);
            writerId = varId;
          }

          // Get the writer cell and extract reader ID
          final wc = cx.rt.heap.writer(writerId);
          if (wc != null) {
            // Store VarRef (reader mode) in current structure at position S
            final struct = cx.currentStructure as StructTerm;
            struct.args[cx.S] = VarRef(wc.readerId, isReader: true);
            cx.S++; // Move to next position

            // Check if structure is complete (all arguments filled)
            if (cx.S >= struct.args.length) {
              // Structure complete - bind the target writer (stored at clauseVars[-1])
              final targetValue = cx.clauseVars[-1];
              int? targetWriterId;
              if (targetValue is VarRef) {
                targetWriterId = targetValue.varId;
              } else if (targetValue is int) {
                targetWriterId = targetValue;
              }
              if (targetWriterId != null) {
                // Bind the writer to the completed structure
                cx.rt.heap.bindWriterStruct(targetWriterId, struct.functor, struct.args);

                // Activate any suspended goals
                final w = cx.rt.heap.writer(targetWriterId);
                if (w != null) {
                  final acts = cx.rt.roq.processOnBind(w.readerId);
                  for (final a in acts) {
                    cx.rt.gq.enqueue(a);
                    if (cx.onActivation != null) cx.onActivation!(a);
                  }
                }
              }

              // Reset structure building state
              // If there's a parent structure, restore it; otherwise clear completely
              if (cx.parentStructure != null && targetWriterId is int) {
                // We just completed a nested structure - add it to parent and restore context
                final nestedWriterId = targetWriterId as int;

                // Get the saved parent's writer ID
                final parentWriterId = cx.parentWriterId;

                // Add the completed nested structure to the parent BEFORE restoring context
                if (cx.parentStructure is StructTerm) {
                  final parentStruct = cx.parentStructure as StructTerm;
                  final wc = cx.rt.heap.writer(nestedWriterId);
                  if (wc != null) {
                    parentStruct.args[cx.parentS] = VarRef(wc.readerId, isReader: true);
                  }
                }

                // Now restore parent context with incremented S
                cx.currentStructure = cx.parentStructure;
                cx.S = cx.parentS + 1; // Move to next position in parent
                cx.mode = cx.parentMode;

                // Clear parent context for next potential nesting
                cx.parentStructure = null;
                cx.parentS = 0;
                cx.parentMode = UnifyMode.read;
                cx.parentWriterId = null;

                // Restore parent's writer ID to clauseVars[-1] for potential completion
                cx.clauseVars[-1] = parentWriterId;

                // Check if parent is now complete!
                if (cx.currentStructure is StructTerm) {
                  final parentStruct = cx.currentStructure as StructTerm;
                  if (cx.S >= parentStruct.args.length && parentWriterId is int) {
                    // Bind the parent structure
                    cx.rt.heap.bindWriterStruct(parentWriterId as int, parentStruct.functor, parentStruct.args);

                    // Activate suspended goals
                    final w = cx.rt.heap.writer(parentWriterId as int);
                    if (w != null) {
                      final acts = cx.rt.roq.processOnBind(w.readerId);
                      for (final a in acts) {
                        cx.rt.gq.enqueue(a);
                        if (cx.onActivation != null) cx.onActivation!(a);
                      }
                    }

                    // Clear structure building state
                    cx.currentStructure = null;
                    cx.mode = UnifyMode.read;
                    cx.S = 0;
                    cx.clauseVars.remove(-1);
                  }
                }
              } else{
                // Top-level structure complete - clear everything
                cx.currentStructure = null;
                cx.mode = UnifyMode.read;
                cx.S = 0;
                cx.clauseVars.remove(-1); // Clear the marker
              }
            }
          }
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
              final w = cx.rt.heap.writer(targetWriterId);
              if (w != null) {
                final acts = cx.rt.roq.processOnBind(w.readerId);
                for (final a in acts) {
                  cx.rt.gq.enqueue(a);
                  if (cx.onActivation != null) cx.onActivation!(a);
                }
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
          // Spawn a new goal with arguments from argWriters/argReaders
          // Create CallEnv from current argument registers
          final newEnv = CallEnv(
            writers: Map.from(cx.argWriters),
            readers: Map.from(cx.argReaders),
          );

          // Get entry point for procedure
          final entryPc = prog.labels[op.procedureLabel];
          if (entryPc == null) {
            print('ERROR: Spawn could not find procedure label: ${op.procedureLabel}');
            return RunResult.terminated;
          }

          // Create and enqueue new goal with unique ID
          final newGoalId = cx.rt.nextGoalId++;
          final newGoalRef = GoalRef(newGoalId, entryPc);

          // Format spawned goal as GLP predicate with arguments
          final args = <String>[];
          for (int i = 0; i < 10; i++) {
            final w = newEnv.writerBySlot[i];
            final r = newEnv.readerBySlot[i];
            if (w != null) {
              args.add(_formatTerm(cx.rt, VarRef(w, isReader: false)));
            } else if (r != null) {
              args.add(_formatTerm(cx.rt, VarRef(r, isReader: true)));
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
          cx.argWriters.clear();
          cx.argReaders.clear();
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
            final w = cx.argWriters[i];
            final r = cx.argReaders[i];
            if (w != null) {
              args.add(_formatTerm(cx.rt, VarRef(w, isReader: false)));
            } else if (r != null) {
              args.add(_formatTerm(cx.rt, VarRef(r, isReader: true)));
            } else {
              break;
            }
          }
          final newHeadGoalStr = args.isEmpty ? op.procedureLabel : '${op.procedureLabel}(${args.join(', ')})';
          cx.spawnedGoals.add(newHeadGoalStr);

          // Print reduction trace before tail call
          // The current head reduces to the body (which includes this requeued goal)
          if (cx.onReduction != null && cx.goalHead != null) {
            final body = cx.spawnedGoals.join(', ');
            cx.onReduction!(cx.goalId, cx.goalHead!, body);
          }

          // Update environment with new arguments
          cx.env.update(Map.from(cx.argWriters), Map.from(cx.argReaders));

          // Clear argument registers
          cx.argWriters.clear();
          cx.argReaders.clear();

          // Clear spawned goals and update head for next reduction
          cx.spawnedGoals.clear();
          cx.goalHead = newHeadGoalStr;  // New head for next iteration

          // Reset clause state for new procedure
          cx.sigmaHat.clear();
          cx.si.clear();
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

        if (debug) {
          print('[GUARD] Evaluating: $predicateName/$arity');
          print('[GUARD] argWriters: ${cx.argWriters}');
          print('[GUARD] argReaders: ${cx.argReaders}');
        }

        // Extract and dereference arguments from argument registers
        final args = <Object?>[];
        final unboundReaders = <int>{};

        for (int i = 0; i < arity; i++) {
          Object? argValue;

          // Check argWriters first (for unbound writers)
          if (cx.argWriters.containsKey(i)) {
            final writerId = cx.argWriters[i]!;
            print('[GUARD] Arg $i from argWriters: $writerId');
            argValue = VarRef(writerId, isReader: false);
          }
          // Then check argReaders (contains BOTH bound writers AND actual readers)
          else if (cx.argReaders.containsKey(i)) {
            final varId = cx.argReaders[i]!;
            print('[GUARD] Arg $i from argReaders: $varId');

            // CRITICAL FIX: Determine if this is a writer ID or reader ID
            // Check if varId exists as a writer in the heap
            final writerCell = cx.rt.heap.writer(varId);

            if (writerCell != null) {
              // It's a WRITER ID (from PutBoundConst storing bound constants)
              // Treat as a writer variable
              print('[GUARD] Arg $i is WRITER ID (from PutBoundConst)');
              argValue = VarRef(varId, isReader: false);

              if (debug) {
                print('[GUARD] Slot $i: Writer ID $varId (bound: ${cx.rt.heap.isWriterBound(varId)})');
              }
            } else {
              // It's a READER ID (from PutReader for actual readers)
              // Treat as a reader variable
              print('[GUARD] Arg $i is READER ID (from PutReader)');
              argValue = VarRef(varId, isReader: true);

              if (debug) {
                // Check if this reader's paired writer is bound
                final writerId = cx.rt.heap.writerIdForReader(varId);
                print('[GUARD] Slot $i: Reader ID $varId (writer $writerId bound: ${writerId != null ? cx.rt.heap.isWriterBound(writerId) : false})');
              }
            }
          }
          // Check clauseVars for HEAD variables
          else if (cx.clauseVars.containsKey(i)) {
            argValue = cx.clauseVars[i];
            print('[GUARD] Arg $i from clauseVars: $argValue');
          }
          else {
            // No argument at this slot
            if (debug) {
              print('[GUARD] WARNING: Argument $i not found in argWriters, argReaders, or clauseVars');
            }
            argValue = null;
          }

          // Dereference to get actual values, tracking unbound readers
          if (argValue != null) {
            print('[GUARD] Before deref - Arg $i: $argValue (${argValue.runtimeType})');
            final (derefValue, readers) = _dereferenceWithTracking(argValue, cx);
            print('[GUARD] After deref - Arg $i: $derefValue (${derefValue.runtimeType})');
            args.add(derefValue);
            unboundReaders.addAll(readers);

            if (debug) {
              print('[GUARD] Arg $i: $argValue → $derefValue');
            }
          } else {
            args.add(null);
          }
        }

        // If any arguments have unbound readers, suspend
        if (unboundReaders.isNotEmpty) {
          if (debug) {
            print('[GUARD] SUSPEND - unbound readers: $unboundReaders');
          }
          cx.si.addAll(unboundReaders);
          _softFailToNextClause(cx, pc);
          pc = _findNextClauseTry(pc);
          continue;
        }

        // All arguments are ground - evaluate the guard
        final result = _evaluateGuard(predicateName, args, cx);

        if (result == GuardResult.success) {
          if (debug) {
            print('[GUARD] SUCCESS - continuing');
          }
          pc++;
          continue;
        } else {
          // FAIL - try next clause
          if (debug) {
            print('[GUARD] FAIL - trying next clause');
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
          cx.si.addAll(unboundReaders);
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
          cx.si.add(unboundReader);
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
          cx.si.addAll(call.suspendedReaders);
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

        if (arg.isWriter) {
          if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadNil: arg is writer ${arg.writerId}');
          // Writer: check if already bound, else record tentative binding in σ̂w
          if (cx.rt.heap.isWriterBound(arg.writerId!)) {
            // Already bound - check if value matches []
            final value = cx.rt.heap.valueOfWriter(arg.writerId!);
            if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadNil: writer ${arg.writerId} value = $value');
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
            cx.sigmaHat[arg.writerId!] = ConstTerm('nil');
          }
        } else if (arg.isReader) {
          if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadNil: arg is reader ${arg.readerId}');
          // Reader: check if bound, else add to Si
          final wid = cx.rt.heap.writerIdForReader(arg.readerId!);
          if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadNil: reader ${arg.readerId} -> writer $wid');
          if (wid == null || !cx.rt.heap.isWriterBound(wid)) {
            if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadNil: writer unbound, adding reader to Si');
            cx.si.add(arg.readerId!);
          } else {
            // Bound reader - check if value matches []
            final value = cx.rt.heap.valueOfWriter(wid);
            if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadNil: writer $wid value = $value');
            if (value is ConstTerm && value.value == 'nil') {
              // Match! Empty list
              if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadNil: MATCH! value is nil (empty list)');
            } else if (value is StructTerm) {
              // Structure doesn't match []
              if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadNil: value is struct, failing');
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            } else {
              // Non-empty constant doesn't match []
              if (debug && (cx.goalId >= 4000 || cx.goalId == 100)) print('  HeadNil: value does not match [], failing');
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            }
          }
        }
        pc++;
        continue;
      }

      if (op is HeadList) {
        // Match list structure [H|T] with argument
        // Equivalent to HeadStructure('[|]', 2, op.argSlot)
        final arg = _getArg(cx, op.argSlot);
        if (arg == null) { pc++; continue; } // No argument at this slot

        if (arg.isWriter) {
          // Writer: create tentative structure in σ̂w
          if (cx.rt.heap.isWriterBound(arg.writerId!)) {
            // Already bound - check if it's a list structure
            final value = cx.rt.heap.valueOfWriter(arg.writerId!);
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
            cx.sigmaHat[arg.writerId!] = struct;
            cx.currentStructure = struct;
            cx.S = 0;
            cx.mode = UnifyMode.write;
          }
        } else if (arg.isReader) {
          // Reader: check if bound, else add to Si
          final wid = cx.rt.heap.writerIdForReader(arg.readerId!);
          if (wid == null || !cx.rt.heap.isWriterBound(wid)) {
            cx.si.add(arg.readerId!);
          } else {
            // Bound reader - check if it's a list structure
            final value = cx.rt.heap.valueOfWriter(wid);
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
          cx.argReaders[op.argSlot] = varId;
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
        cx.argReaders[op.argSlot] = varId;
        pc++;
        continue;
      }

      if (op is PutBoundNil) {
        // Put a variable bound to 'nil'
        // Used for passing empty lists as arguments in queries
        final varId = cx.rt.heap.allocateFreshVar();
        cx.rt.heap.addVariable(varId);
        cx.rt.heap.bindWriterConst(varId, 'nil');
        cx.argReaders[op.argSlot] = varId;
        pc++;
        continue;
      }

      if (op is PutList) {
        // Begin list construction in argument register
        // Equivalent to PutStructure('[|]', 2, op.argSlot)
        if (cx.inBody) {
          // Store target writer ID from environment
          final targetWriterId = cx.env.w(op.argSlot);
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

  /// Helper to get argument info from call environment
  _ArgInfo? _getArg(RunnerContext cx, int slot) {
    final wid = cx.env.w(slot);
    if (wid != null) return _ArgInfo(writerId: wid);

    final rid = cx.env.r(slot);
    if (rid != null) return _ArgInfo(readerId: rid);

    // TODO: Handle ground terms
    return null;
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
          // Reader - check if paired writer is bound
          final varId = t.varId;
          if (cx.rt.heap.isWriterBound(varId)) {
            final boundValue = cx.rt.heap.getValue(varId);
            // CRITICAL FIX: Recursively dereference the bound value
            return dereference(boundValue);
          } else {
            // Unbound reader - track it
            unboundReaders.add(varId);
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
        // For arithmetic expressions, recursively evaluate
        if (_isArithmeticOp(t.functor)) {
          final evalArgs = <Object?>[];
          for (final arg in t.args) {
            evalArgs.add(dereference(arg));
          }
          // Check if all args are ground numbers
          if (evalArgs.every((a) => a is num || (a is ConstTerm && a.value is num))) {
            return _evaluateArithmetic(t.functor, evalArgs);
          }
        }
        // Return structure as-is if not arithmetic
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

    switch (predicateName) {
      // Comparison guards
      case '<':
        if (args.length < 2) return GuardResult.failure;
        final a = getValue(args[0]);
        final b = getValue(args[1]);

        // Debug output
        print('[EVAL_GUARD] < comparison:');
        print('[EVAL_GUARD]   args[0] = ${args[0]} (${args[0].runtimeType})');
        print('[EVAL_GUARD]   args[1] = ${args[1]} (${args[1].runtimeType})');
        print('[EVAL_GUARD]   a = $a (${a.runtimeType})');
        print('[EVAL_GUARD]   b = $b (${b.runtimeType})');
        print('[EVAL_GUARD]   a is num = ${a is num}');
        print('[EVAL_GUARD]   b is num = ${b is num}');

        if (a is num && b is num) {
          final result = a < b;
          print('[EVAL_GUARD]   $a < $b = $result');
          return result ? GuardResult.success : GuardResult.failure;
        }
        print('[EVAL_GUARD]   Type mismatch - returning failure');
        return GuardResult.failure;

      case '>':
        if (args.length < 2) return GuardResult.failure;
        final a = getValue(args[0]);
        final b = getValue(args[1]);
        if (a is num && b is num) {
          return a > b ? GuardResult.success : GuardResult.failure;
        }
        return GuardResult.failure;

      case '=<':
        if (args.length < 2) return GuardResult.failure;
        final a = getValue(args[0]);
        final b = getValue(args[1]);
        if (a is num && b is num) {
          return a <= b ? GuardResult.success : GuardResult.failure;
        }
        return GuardResult.failure;

      case '>=':
        if (args.length < 2) return GuardResult.failure;
        final a = getValue(args[0]);
        final b = getValue(args[1]);
        if (a is num && b is num) {
          return a >= b ? GuardResult.success : GuardResult.failure;
        }
        return GuardResult.failure;

      case '=:=':
        if (args.length < 2) return GuardResult.failure;
        final a = getValue(args[0]);
        final b = getValue(args[1]);
        if (a is num && b is num) {
          return a == b ? GuardResult.success : GuardResult.failure;
        }
        return GuardResult.failure;

      case '=\\=':
        if (args.length < 2) return GuardResult.failure;
        final a = getValue(args[0]);
        final b = getValue(args[1]);
        if (a is num && b is num) {
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
        if (args.isEmpty) return GuardResult.failure;
        final val = getValue(args[0]);
        return (val is int) ? GuardResult.success : GuardResult.failure;

      case 'number':
        if (args.isEmpty) return GuardResult.failure;
        final val = getValue(args[0]);
        return (val is num) ? GuardResult.success : GuardResult.failure;

      // Control guards
      case 'otherwise':
        // This is handled by the compiler - should not reach runtime
        return GuardResult.success;

      case 'true':
        return GuardResult.success;

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
