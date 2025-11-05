import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/commit.dart';
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
        if (arg == null) { pc++; continue; }

        if (arg.isWriter) {
          // WRITE mode: create tentative structure for writer
          final struct = _TentativeStruct(op.functor, op.arity, List.filled(op.arity, null));
          cx.sigmaHat[arg.writerId!] = struct;
          cx.currentStructure = struct;
          cx.mode = UnifyMode.write;
          cx.S = 0; // Start at first arg
        } else if (arg.isReader) {
          // Reader: check if bound and has matching structure
          final wid = cx.rt.heap.writerIdForReader(arg.readerId!);
          if (wid == null || !cx.rt.heap.isWriterBound(wid)) {
            cx.si.add(arg.readerId!);
            pc++; continue;
          }
          // TODO: READ mode - traverse existing structure
          // For now, just continue
        } else {
          // Ground structure: READ mode
          // TODO: implement structure matching for ground terms
        }
        pc++; continue;
      }

      if (op is HeadWriter) {
        // Process writer variable in structure (at S position)
        if (cx.mode == UnifyMode.write) {
          // WRITE mode: We're building a structure - create placeholder for writer
          // The actual writer ID will be determined later (this is a clause variable)
          if (cx.currentStructure is _TentativeStruct) {
            final struct = cx.currentStructure as _TentativeStruct;
            // Create a placeholder - actual writer binding happens separately
            struct.args[cx.S] = 'W${op.varIndex}'; // Placeholder
            cx.clauseVars[op.varIndex] = 'W${op.varIndex}';
            cx.S++; // Advance to next arg
          }
        } else {
          // READ mode: Extract value from structure at S position
          // TODO: implement READ mode structure traversal
        }
        pc++; continue;
      }

      if (op is HeadReader) {
        // Process reader variable in structure (at S position)
        if (cx.mode == UnifyMode.write) {
          // WRITE mode: Building structure - add reader placeholder
          if (cx.currentStructure is _TentativeStruct) {
            final struct = cx.currentStructure as _TentativeStruct;
            struct.args[cx.S] = 'R${op.varIndex}'; // Placeholder
            cx.clauseVars[op.varIndex] = 'R${op.varIndex}';
            cx.S++; // Advance to next arg
          }
        } else {
          // READ mode: Check reader at S position
          // TODO: implement READ mode reader checking
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
        // Apply σ̂w: bind writers to tentative values, then wake suspended goals
        final acts = CommitOps.applySigmaHatV216(
          heap: cx.rt.heap,
          roq: cx.rt.roq,
          sigmaHat: cx.sigmaHat,
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
          final args = <Term>[for (final v in op.constArgs) ConstTerm(v)];
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

/// Helper to represent list structures
class _ListStruct {
  final Object? head;
  final Object? tail;

  _ListStruct(this.head, this.tail);

  @override
  String toString() => '[$head|$tail]';
}
