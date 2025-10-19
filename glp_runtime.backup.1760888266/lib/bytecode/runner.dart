import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'opcodes.dart';

enum RunResult { terminated, suspended, yielded }

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
  final Set<int> sigmaHat = <int>{}; // tentative writers
  final Set<int> si = <int>{};       // clause-local blockers
  final Set<int> U = <int>{};        // union across clauses
  bool inBody = false;

  final void Function(GoalRef)? onActivation; // host log hook

  RunnerContext({
    required this.rt,
    required this.goalId,
    required this.kappa,
    CallEnv? env,
    this.onActivation,
  }) : env = env ?? CallEnv();

  void clearClause() { sigmaHat.clear(); si.clear(); inBody = false; }
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

      // Head (concrete or Arg)
      if (op is HeadBindWriter) { cx.sigmaHat.add(op.writerId); pc++; continue; }
      if (op is HeadBindWriterArg) {
        final wid = cx.env.w(op.slot);
        if (wid != null) cx.sigmaHat.add(wid);
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

      // Commit (phase change only)
      if (op is Commit) {
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

      if (op is Proceed) { pc++; continue; }

      pc++; // default progress
    }
    return RunResult.terminated;
  }
}
