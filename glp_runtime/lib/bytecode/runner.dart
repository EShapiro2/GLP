import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/commit.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'opcodes.dart';

enum RunResult { terminated, suspended, yielded }

/// Unification mode for structure traversal (WAM-style)
enum UnifyMode { read, write }

typedef LabelName = String;

class BytecodeProgram {
  final List<Op> ops;
  final Map<LabelName, int> labels;
  BytecodeProgram(this.ops) : labels = _indexLabels(ops);
  static Map<LabelName, int> _indexLabels(List<Op> ops) {
    final m = <LabelName,int>{};
    for (var i = 0; i < ops.length; i++) {
      final op = ops[i];
      if (op is Label) m[op.name] = i;
    }
    return m;
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

class RunnerContext {
  final GlpRuntime rt;
  final int goalId;
  final int kappa;
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

  // Argument registers for goal calls (A1, A2, ..., An)
  final Map<int, int> argWriters = {};  // argSlot → writer ID
  final Map<int, int> argReaders = {};  // argSlot → reader ID

  final void Function(GoalRef)? onActivation; // host log hook

  RunnerContext({
    required this.rt,
    required this.goalId,
    required this.kappa,
    CallEnv? env,
    this.onActivation,
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
  int _findNextClauseTry(int fromPc) {
    for (var i = fromPc + 1; i < prog.ops.length; i++) {
      if (prog.ops[i] is ClauseTry) return i;
    }
    return prog.ops.length; // End of program if no more clauses
  }

  /// Soft-fail to next clause: union Si to U, clear clause state, jump to next ClauseTry
  void _softFailToNextClause(RunnerContext cx, int currentPc) {
    // Union Si into U
    if (cx.si.isNotEmpty) cx.U.addAll(cx.si);
    // Clear clause-local state
    cx.clearClause();
    // Jump to next clause (will be handled by returning new PC)
  }

  RunResult runWithStatus(RunnerContext cx) {
    var pc = 0;
    while (pc < prog.ops.length) {
      final op = prog.ops[pc];

      if (op is Label) { pc++; continue; }
      if (op is ClauseTry) { cx.clearClause(); pc++; continue; }
      if (op is GuardFail) { pc++; continue; }

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
          // Writer: record tentative binding in σ̂w
          cx.sigmaHat[arg.writerId!] = op.value;
        } else if (arg.isReader) {
          // Reader: check if bound, else add to Si
          final wid = cx.rt.heap.writerIdForReader(arg.readerId!);
          if (wid == null || !cx.rt.heap.isWriterBound(wid)) {
            cx.si.add(arg.readerId!);
          }
          // TODO: if bound, check value matches op.value
        } else {
          // Ground: check if value matches
          // TODO: implement proper ground term matching
        }
        pc++; continue;
      }

      if (op is HeadStructure) {
        final arg = _getArg(cx, op.argSlot);
        if (arg == null) {
          // No argument - soft fail to next clause
          _softFailToNextClause(cx, pc);
          pc = _findNextClauseTry(pc);
          continue;
        }

        if (arg.isWriter) {
          // WRITE mode: create tentative structure for writer
          final struct = _TentativeStruct(op.functor, op.arity, List.filled(op.arity, null));
          cx.sigmaHat[arg.writerId!] = struct;
          cx.currentStructure = struct;
          cx.mode = UnifyMode.write;
          cx.S = 0; // Start at first arg
          pc++; continue;
        }

        if (arg.isReader) {
          // Reader: check if bound and has matching structure
          final wid = cx.rt.heap.writerIdForReader(arg.readerId!);
          if (wid == null || !cx.rt.heap.isWriterBound(wid)) {
            // Unbound reader - add to Si and soft fail
            cx.si.add(arg.readerId!);
            _softFailToNextClause(cx, pc);
            pc = _findNextClauseTry(pc);
            continue;
          }

          // Bound reader - get value and check if it's a matching structure
          final value = cx.rt.heap.valueOfWriter(wid);
          if (value is StructTerm && value.functor == op.functor && value.args.length == op.arity) {
            // Matching structure - enter READ mode
            cx.currentStructure = value;
            cx.mode = UnifyMode.read;
            cx.S = 0;
            pc++; continue;
          } else {
            // Non-matching structure or not a structure - soft fail
            _softFailToNextClause(cx, pc);
            pc = _findNextClauseTry(pc);
            continue;
          }
        }

        // Ground term case (not writer or reader)
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
              // Variable already bound - use its value
              struct.args[cx.S] = existingValue;
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

        // Store the argument value/reference in clause variable
        if (arg.isWriter) {
          cx.clauseVars[op.varIndex] = arg.writerId;
        } else if (arg.isReader) {
          // For reader, we need to check if it's bound and get the value
          final wid = cx.rt.heap.writerIdForReader(arg.readerId!);
          if (wid != null && cx.rt.heap.isWriterBound(wid)) {
            // Bound reader - store the actual value
            final value = cx.rt.heap.valueOfWriter(wid);
            cx.clauseVars[op.varIndex] = value;
          } else {
            // Unbound reader - store reader reference
            cx.clauseVars[op.varIndex] = arg.readerId;
          }
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
          if (storedValue is int) {
            // storedValue is a writer ID - check they match
            if (arg.writerId != storedValue) {
              _softFailToNextClause(cx, pc);
              pc = _findNextClauseTry(pc);
              continue;
            }
          } else {
            // storedValue is a Term - bind writer to it
            cx.sigmaHat[arg.writerId!] = storedValue;
          }
        } else if (arg.isReader) {
          // Argument is a reader - verify it matches stored value
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
          }
        } else {
          // READ mode: Verify value at S position matches constant
          if (cx.currentStructure is StructTerm) {
            final struct = cx.currentStructure as StructTerm;
            if (cx.S < struct.args.length) {
              final value = struct.args[cx.S];
              // Check if value is a constant term matching op.value
              if (value is ConstTerm && value.value == op.value) {
                cx.S++; // Match successful, advance
              } else {
                // Mismatch - soft fail
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
                // TODO: resolve clause variables to actual writer/reader IDs
                // For now, leave as placeholder
                termArgs.add(ConstTerm('${arg.toString()}'));
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

        // Apply σ̂w: bind writers to tentative values, then wake suspended goals
        final acts = CommitOps.applySigmaHatV216(
          heap: cx.rt.heap,
          roq: cx.rt.roq,
          sigmaHat: convertedSigmaHat,
        );
        for (final a in acts) {
          cx.rt.gq.enqueue(a);
          if (cx.onActivation != null) cx.onActivation!(a);
        }
        cx.sigmaHat.clear();
        cx.inBody = true;
        pc++; continue;
      }

      // Clause control / suspend
      if (op is UnionSiAndGoto) {
        if (cx.si.isNotEmpty) cx.U.addAll(cx.si);
        cx.clearClause();
        pc = prog.labels[op.label]!;
        continue;
      }
      if (op is ResetAndGoto) { cx.clearClause(); pc = prog.labels[op.label]!; continue; }

      if (op is SuspendEnd) {
        if (cx.U.isNotEmpty) {
          cx.rt.suspendGoal(goalId: cx.goalId, kappa: cx.kappa, readers: cx.U);
          cx.U.clear();
          cx.inBody = false;
          return RunResult.suspended;
        }
        cx.inBody = false;
        pc++; continue;
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
          // Get writer ID from clause variable
          final value = cx.clauseVars[op.varIndex];
          if (value is int) {
            // It's a writer ID - store in argument register
            cx.argWriters[op.argSlot] = value;
          } else if (value is _ClauseVar) {
            // It's a placeholder - we need to create an actual writer
            // This happens when HeadWriter created a placeholder in WRITE mode
            final freshWriterId = cx.goalId * 10000 + op.varIndex * 100 + 70;
            final freshReaderId = cx.goalId * 10000 + op.varIndex * 100 + 71;
            cx.rt.heap.addWriter(WriterCell(freshWriterId, freshReaderId));
            cx.rt.heap.addReader(ReaderCell(freshReaderId));
            cx.argWriters[op.argSlot] = freshWriterId;
            // Update clause var to point to the actual writer
            cx.clauseVars[op.varIndex] = freshWriterId;
          } else {
            print('WARNING: PutWriter got unexpected value: $value');
          }
        }
        pc++; continue;
      }

      if (op is PutReader) {
        if (cx.inBody) {
          // Get writer ID from clause variable, derive its reader
          final value = cx.clauseVars[op.varIndex];
          if (value is int) {
            // It's a writer ID - get its paired reader
            final wc = cx.rt.heap.writer(value);
            if (wc != null) {
              cx.argReaders[op.argSlot] = wc.readerId;
            } else {
              print('ERROR: PutReader could not find writer $value in heap');
            }
          } else if (value is ConstTerm) {
            // It's a ground term - create fresh writer/reader and bind it
            final freshWriterId = cx.goalId * 10000 + op.argSlot * 100 + 60;
            final freshReaderId = cx.goalId * 10000 + op.argSlot * 100 + 61;
            cx.rt.heap.addWriter(WriterCell(freshWriterId, freshReaderId));
            cx.rt.heap.addReader(ReaderCell(freshReaderId));
            cx.rt.heap.bindWriterConst(freshWriterId, value.value);
            cx.argReaders[op.argSlot] = freshReaderId;
          } else {
            // Unknown - skip
            print('WARNING: PutReader got unexpected value: $value');
          }
        }
        pc++; continue;
      }

      if (op is PutConstant) {
        if (cx.inBody) {
          // For constants, we create a fresh writer/reader pair and bind immediately
          // Generate IDs (simple approach: use goalId * 10000 + slot offset)
          final freshWriterId = cx.goalId * 10000 + op.argSlot * 100 + 50;
          final freshReaderId = cx.goalId * 10000 + op.argSlot * 100 + 51;
          cx.rt.heap.addWriter(WriterCell(freshWriterId, freshReaderId));
          cx.rt.heap.addReader(ReaderCell(freshReaderId));
          cx.rt.heap.bindWriterConst(freshWriterId, op.value);
          cx.argReaders[op.argSlot] = freshReaderId;
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

          // Create and enqueue new goal
          final newGoalId = cx.goalId * 1000 + pc;  // Simple ID generation
          final newGoalRef = GoalRef(newGoalId, entryPc);

          // TODO: Need to actually run the goal or queue it properly
          // For now, just queue it
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

          // Update environment with new arguments
          cx.env.update(Map.from(cx.argWriters), Map.from(cx.argReaders));

          // Clear argument registers
          cx.argWriters.clear();
          cx.argReaders.clear();

          // Reset clause state for new procedure
          cx.sigmaHat.clear();
          cx.si.clear();
          cx.U.clear();
          cx.clauseVars.clear();
          cx.inBody = false;
          cx.mode = UnifyMode.read;
          cx.S = 0;
          cx.currentStructure = null;

          // Jump to procedure entry
          pc = entryPc;
          continue;
        }
        pc++; continue;
      }

      if (op is Proceed) {
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
