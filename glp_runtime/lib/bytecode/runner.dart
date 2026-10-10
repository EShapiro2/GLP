import 'dart:async' show Timer;
import 'dart:collection' show Queue, SetBase;

import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/heap_fcp.dart' show HeapCell;
import 'package:glp_runtime/runtime/commit.dart';
import 'package:glp_runtime/runtime/body_kernels.dart';
import 'opcodes.dart';
import 'package:glp_runtime/engine_v2/step_outcome.dart';

enum RunResult { terminated, suspended, yielded }

/// A runner that executes one goal's [RunnerContext] to a [RunResult]. The
/// scheduler holds runners behind this interface so a goal can be driven by the
/// direct byte loop (`ByteRunner` in `engine_v2/interp.dart`) — the PC in
/// `cx.kappa` interpreted as a byte offset into the code section.
abstract interface class GoalRunner {
  void run(RunnerContext cx);
  RunResult runWithStatus(RunnerContext cx);

  /// The procedure name (signature) whose entry is at program-counter [pc], or
  /// null if none — used by the scheduler for trace display. The PC is an
  /// instruction index for the object runner, a byte offset for the byte runner.
  String? procNameForPc(int pc);
}

/// Unification mode for structure traversal (WAM-style)
enum UnifyMode { read, write }

/// Result of guard evaluation
enum GuardResult {
  success,  // Guard succeeded, continue with clause
  failure,  // Guard failed, try next clause
  suspend,  // Guard blocked on unbound readers (already added to cx.Si)
}

typedef LabelName = String;

class BytecodeProgram {
  final List<Op> ops;  // The instruction objects (opcodes.dart), labels among them
  final Map<LabelName, int> labels;
  BytecodeProgram(this.ops) : labels = _indexLabels(ops);
  static Map<LabelName, int> _indexLabels(List<Op> ops) {
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
    // PutVariable (the critical one for debugging)
    if (op is PutVariable) {
      final mode = op.isReader ? 'reader' : 'writer';
      return 'PutVariable(X${op.varIndex} → A${op.argSlot}, $mode)';
    }

    // The other variable instructions
    if (op is HeadVariable) {
      final mode = op.isReader ? 'reader' : 'writer';
      return 'HeadVariable(X${op.varIndex}, $mode)';
    }
    if (op is UnifyVariable) {
      final mode = op.isReader ? 'reader' : 'writer';
      return 'UnifyVariable(X${op.varIndex}, $mode)';
    }
    if (op is SetVariable) {
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

/// Parent context for nested structure building
class _ParentContext {
  final Object? structure;
  final int s;
  final UnifyMode mode;
  final Object? writerId;

  _ParentContext({
    required this.structure,
    required this.s,
    required this.mode,
    required this.writerId,
  });
}

/// The goal's suspension set U: the readers the goal suspends on when no
/// clause selects it (IGLP Implementation Notes, "Clause try").  It notes
/// whether the current clause attempt added to it, [touched], and that is how
/// a clause that suspended is told from one that failed: a reader is added here
/// where the commit suspends, and never where a match or a guard fails --- a
/// guard member that suspends puts its readers in Si, as a head match does
/// ([_guardUndecided]).  A clause that meets a mismatch fails, whatever it
/// suspended on before
/// (GLP-Spec appendix-term-matching.tex, Definition "Term Matching": "The writer
/// mgu is the union of all writer assignments if no fail was encountered and
/// the suspension set is empty"), so its own suspension set Si reaches U only
/// when it suspended ([ByteRunner]'s `_applyNextClauseByte`).
class SuspensionSet extends SetBase<HeapCell> {
  final Set<HeapCell> _readers = <HeapCell>{};

  /// Whether a reader was added since the clause attempt began.
  bool touched = false;

  @override
  bool add(HeapCell value) {
    touched = true;
    return _readers.add(value);
  }

  @override
  bool contains(Object? element) => _readers.contains(element);

  @override
  HeapCell? lookup(Object? element) => _readers.lookup(element);

  @override
  bool remove(Object? value) => _readers.remove(value);

  @override
  Iterator<HeapCell> get iterator => _readers.iterator;

  @override
  int get length => _readers.length;

  @override
  Set<HeapCell> toSet() => _readers.toSet();
}

/// The subterm a head pattern meets at an unbound goal reader.  The reader is
/// suspended on (GLP-Spec appendix-term-matching.tex, row "Reader X1?", column
/// "Term f2/n2": "suspend on X1?") and the pattern under it is SKIPPED, not
/// read against whatever structure the traversal last held: its positions match
/// nothing and fail nothing, and a clause variable whose writer occurrence lies
/// in it, and has no value yet, is UNKNOWN ([RunnerContext.unknownVars]).  The
/// rest of the head is still matched, so a later mismatch fails the clause.
class _SkippedSubterm {
  const _SkippedSubterm();
  @override
  String toString() => 'skipped';
}

const _skipped = _SkippedSubterm();

/// Enter [_skipped] for the subterm under an unbound goal reader.
void _skipSubterm(RunnerContext cx) {
  cx.currentStructure = _skipped;
  cx.mode = UnifyMode.read;
  cx.S = 0;
}

class RunnerContext {
  final GlpRuntime rt;
  final int goalId;
  int kappa;  // Mutable - updated by Requeue for tail calls
  final CallEnv env;
  final Map<HeapCell, Object?> sigmaHat = <HeapCell, Object?>{}; // σ̂w: tentative writer bindings
  final Set<HeapCell> Si = <HeapCell>{};       // clause-level preliminary suspension set
  final SuspensionSet U = SuspensionSet(); // goal-level suspension set (reader IDs)
  bool inBody = false;

  /// The UNKNOWN clause variables: those whose writer occurrence lies inside a
  /// skipped subterm ([_skipped]) and that had no value when it was met.  The
  /// writer occurrence is the one that gives a head variable its value, the
  /// subterm of the goal it is matched against (appendix-term-matching.tex,
  /// column "Writer X2", "X2 := T1"), and under a suspended reader that
  /// subterm is not there yet; a reader occurrence gives none, so a variable
  /// met there only as a reader is not unknown, and takes its value from its
  /// writer occurrence elsewhere.  An unknown variable stays unknown for the
  /// rest of the clause attempt: a later occurrence never gives it a value,
  /// and fails only where the table fails whatever the variable --- a head
  /// reader against a goal reader or a goal term (column "Reader X2?"), and a
  /// head writer against a goal writer (row "Writer X1", column "Writer X2";
  /// [_isGoalWriter]).  A guard over it is decided by the guard's own
  /// decision, the variable standing for any term, as the goal reader above
  /// it may be assigned any term ([unknownPlaceholders], [_undecidedMember]):
  /// it fails where no term makes it succeed, and is otherwise passed by (GLP
  /// #3 Cowork, 2026-10-02 15:31 UTC, B).  The clause cannot commit, its
  /// suspension set being non-empty; what is still asked of it is whether it
  /// fails.
  final Set<int> unknownVars = <int>{};

  /// The variable that stands for each unknown variable ([unknownVars]),
  /// by its clause-variable index, as its writer and reader cells: an
  /// occurrence placed in a structure built for a goal writer or in a guard's
  /// argument holds it ([_unknownPlaceholder]), one variable for every
  /// occurrence of one unknown variable, so that a guard decision sees them as
  /// one.
  final Map<int, (HeapCell, HeapCell)> unknownPlaceholders = <int, (HeapCell, HeapCell)>{};

  /// The writer cells of [unknownPlaceholders]: a guard decision meeting one
  /// meets an unknown variable, which stands for any term, and not a variable
  /// the clause alone holds ([_undecidedMember]).
  final Set<HeapCell> unknownKeys = <HeapCell>{};

  /// Whether clause variable [varIndex] is unknown ([unknownVars]).
  bool isUnknown(int varIndex) => unknownVars.contains(varIndex);

  // WAM-style structure traversal state
  UnifyMode mode = UnifyMode.read;   // Current unification mode
  int S = 0;                          // Structure pointer (current position in structure)
  Object? currentStructure;           // Current structure being traversed
  final Map<int, Object?> clauseVars = {}; // Clause variable bindings (varIndex → value)

  // Clause-variable index for the next anonymous variable an `unify_void`
  // creates. Each occurrence of an anonymous variable is a variable of its own
  // that nothing else in the clause names, so it gets an index nothing else
  // can hold: indices run downwards from -3, below the structure sentinels -1
  // and -2, while codegen's clause variables and temp registers are all
  // non-negative. It is never reset, so two occurrences never share an index.
  int _nextVoidVar = -3;
  int freshVoidVar() => _nextVoidVar--;

  // Parent structure stack for nested structure building (supports arbitrary depth)
  final List<_ParentContext> parentStack = [];

  // Argument registers for goal calls (A1, A2, ..., An)
  // Per spec v2.16 section 1.1: heterogeneous term storage
  final Map<int, Term> argSlots = {};  // argSlot → Term (VarRef, ConstTerm, StructTerm)

  // Guard argument building mode (for pre-commit structure building)
  int? guardArgSlot;  // Target argSlot when building structure for guard argument

  // Environment frames for permanent variables (Y registers)
  EnvironmentFrame? E;  // Current environment pointer
  int? CP;              // Continuation pointer (return address)

  // Track spawned goals for display
  final List<String> spawnedGoals = [];

  // Track reduction for trace output
  String? goalHead;  // Formatted head goal for trace (mutable for tail calls)
  String? goalProcName;  // Procedure name for delayed head formatting
  final void Function(int goalId, String head, String body)? onReduction;

  /// Set at each reduction of this run --- its proceed, and each tail call ---
  /// whether or not the run is traced: the scheduler reads it after the run to
  /// tell a goal that reduced from one that failed.
  bool reduced = false;

  /// Whether this run keeps the reduction trace.  The scheduler passes
  /// [onReduction] and [goalHead] only when it traces, and a goal is formatted
  /// only for the trace, so a run that is not traced formats no goal.
  bool get tracing => onReduction != null && goalHead != null;

  /// Re-format the goal head from current env state (after σ̂ applied to heap).
  /// This shows bound values instead of unbound variable names.
  String reformatHead() {
    final name = goalProcName ?? goalHead ?? '?';
    final args = <String>[];
    // No arity cap: argument slots are dense from 0 and env.arg returns null past
    // the last one, so break on the first null. The former `i < 10` truncated the
    // trace of goals with more than ten arguments.
    for (int i = 0; ; i++) {
      final arg = env.arg(i);
      if (arg == null) break;
      args.add(termFormatter != null
          ? termFormatter!(arg)
          : arg.toString());
    }
    if (args.isEmpty) return name;
    return '$name(${args.join(', ')})';
  }

  // Control trace output
  final bool showBindings;
  final bool debugOutput;

  // Custom term formatter for consistent variable naming
  final String Function(Term, {bool markReaders})? termFormatter;

  RunnerContext({
    required this.rt,
    required this.goalId,
    required this.kappa,
    CallEnv? env,
    this.goalHead,
    this.goalProcName,
    this.onReduction,
    this.showBindings = true,
    this.debugOutput = false,
    this.termFormatter,
  }) : env = env ?? CallEnv();

  void clearClause() {
    sigmaHat.clear();
    Si.clear();
    U.touched = false;
    unknownVars.clear();
    unknownPlaceholders.clear();
    unknownKeys.clear();
    inBody = false;
    mode = UnifyMode.read;
    S = 0;
    currentStructure = null;
    clauseVars.clear();
    guardArgSlot = null;
    parentStack.clear();
  }
}


// Pure helpers relocated from the former BytecodeRunner class (object loop,
// removed). They operate only on RunnerContext and are shared by OpExecutors.

HeapCell _finalUnboundVar(RunnerContext cx, HeapCell addr) {
  // derefAddr follows the entire chain automatically
  final derefResult = cx.rt.heap.derefAddr(addr);

  if (cx.debugOutput) print('[DEBUG _finalUnboundVar] @$addr -> derefResult=$derefResult');

  if (derefResult is VarRef) {
    // derefAddr returned the final unbound variable in the chain
    final finalAddr = derefResult.addr;
    final isWriter = cx.rt.heap.isWriter(finalAddr);

    // Per GLP semantics: goals suspend on READERS, not writers
    // If the final unbound var is a writer, return its paired reader
    // Per spec v3.2: use readerForWriter() instead of +1 arithmetic
    final readerAddr = isWriter ? cx.rt.heap.pairedReaderAddr(finalAddr) : finalAddr;
    if (cx.debugOutput) print('[DEBUG _finalUnboundVar] Final var: $finalAddr (${isWriter ? "writer" : "reader"}), returning reader: $readerAddr');
    return readerAddr;
  }

  // Writer is bound to a ground term, reader is effectively bound
  if (cx.debugOutput) print('[DEBUG _finalUnboundVar] Bound to ground term, returning original: $addr');
  return addr;
}

Term? _getArg(RunnerContext cx, int slot) {
  final arg = cx.env.arg(slot);
  // Per spec v2.16.3 Section 1.1: CallEnv arguments must be VarRefs
  assert(arg == null || arg is VarRef,
         'CallEnv arguments must be VarRefs, got ${arg.runtimeType}');
  return arg;
}

/// A ground value at an argument position: a constant, a structure, or an
/// opaque constant of the runtime's own — a module value (the `Module`
/// constant of the type system) or a mutual reference. Every branch that
/// matches or binds a ground argument admits all of them alike: a module value
/// arriving bare inside a delivered structure is as ground as a constant.
bool _isGroundValue(Object? v) => v is Term && v is! VarRef;

/// Whether the goal subterm at [addr] is a writer still unbound: the one goal
/// subterm a head reader matches.  GLP-Spec appendix-term-matching.tex,
/// Definition "Term Matching", column "Reader X2?": a goal writer X1 is
/// assigned the head's reader (X1 := X2?); a goal reader X1? is "fail"; a goal
/// term f1/n1 is "fail".  glp.tex Definition "Writer MGU" agrees, the mgu being
/// a writers substitution, which leaves a reader as it is.  A goal subterm is
/// taken as the goal holds it: a writer or reader already bound stands for its
/// value, a term or a reader, and fails with it.
bool _isUnboundWriterCell(RunnerContext cx, HeapCell addr) {
  final heap = cx.rt.heap;
  if (!heap.isWriter(addr)) return false;
  final end = heap.derefAddr(addr);
  return end is VarRef && end.addr == addr;
}

/// Whether the goal subterm [t] is a goal writer, which a head writer fails:
/// GLP-Spec appendix-term-matching.tex, Definition "Term Matching", row
/// "Writer X1", column "Writer X2": "fail".  It does whatever the head writer
/// is --- named, or `_`, a writer of its own (glp.tex, Remark "Anonymous
/// Variables") --- and whatever the occurrence, first or later, and typed or
/// not (GLP #3 Cowork, 2026-10-02 13:00 UTC, and 17:12 UTC, 1(a)).  A goal
/// writer is a writer still unbound ([_isUnboundWriterCell]) that this clause
/// attempt has not assigned: one it has assigned in σ̂w stands for what it was
/// assigned, as a bound one stands for its value, so the placement of a nested
/// structure after `pop`, which meets again the goal writer `unify_structure`
/// assigned the structure, is not a vertex of its own.  Until 2026-10-02 the
/// runtime took a goal writer at a head writer as the head's variable, and
/// `s(_)`, `s(f(_))` and `s2(X) :- t(X?)` each succeeded with a goal writer
/// there.
bool _isGoalWriter(RunnerContext cx, Object? t) =>
    t is VarRef &&
    _isUnboundWriterCell(cx, t.addr) &&
    !cx.sigmaHat.containsKey(t.addr);

/// Whether clause variable [varIndex] has no value yet in this clause attempt,
/// so that its writer occurrence met in a skipped subterm makes it unknown
/// ([RunnerContext.unknownVars]): it is unset, or a placeholder, or a writer
/// that neither the heap nor the tentative substitution binds --- the goal's
/// writer, which an earlier reader occurrence was assigned, or a fresh one an
/// earlier occurrence placed in a structure built for a goal writer.  Either
/// writer would take its value from the writer occurrence that lies in the
/// skipped subterm.  A term or a reader is a value.
bool _hasNoValue(RunnerContext cx, int varIndex) {
  final v = cx.clauseVars[varIndex];
  if (v == null || v is _ClauseVar) return true;
  final HeapCell? w = v is HeapCell ? v : (v is VarRef ? v.addr : null);
  if (w == null || !cx.rt.heap.isWriter(w)) return false;
  return !cx.sigmaHat.containsKey(w) && _isUnboundWriterCell(cx, w);
}

/// What a guard's reader occurrence X? stands for, clause variable X holding
/// [value].  A guard argument is a reader (GLP-Spec glp.tex, Definition
/// "Guarded Clause": "Guard arguments are readers paired to head writers"),
/// and ground (0x41), known (0x42), no_readers (0x44) and ground_equal (0x45)
/// name the variable alone.  A variable held as a writer that neither the heap
/// nor the tentative substitution binds --- a fresh one of the head, in a
/// structure it gives a goal writer, or the goal's writer a head reader was
/// matched against --- is that variable, and X? is its reader, as put_variable
/// gives it to the generic guard call; anything else is what X? stands for.
/// Until 2026-10-02 these instructions read the writer itself, and
/// no_readers(X?) over the fresh X of hn(f(X), yes) succeeded, a writer
/// holding no reader.
Object? _guardReaderOperand(RunnerContext cx, Object? value) {
  final heap = cx.rt.heap;
  final HeapCell? w = value is HeapCell ? value : (value is VarRef ? value.addr : null);
  if (w == null || !heap.isWriter(w) || cx.sigmaHat.containsKey(w)) {
    return value;
  }
  // A bound writer stands for its value.
  if (heap.isFullyBound(w)) {
    return value;
  }
  return VarRef(heap.pairedReaderAddr(w));
}

/// The unbound variables of a guard argument: the unbound readers met, and
/// whether an unbound writer was, and a mutual reference, which "holds the
/// writer of a stream tail, so it is neither ground nor a constant type" (TGLP
/// typed-glp.tex) ([_termVariables]).
class _TermVariables {
  final Set<HeapCell> readers = <HeapCell>{};
  bool writer = false;
  bool mutualRef = false;
}

/// The unbound variables of [term], a guard argument --- a clause variable's
/// value, a variable's address at the top, or a term --- its bindings
/// followed, the tentative ones (σ̂) before the heap's, a reader's through its
/// writer as a writer's.  Until 2026-10-02 the collectors of ground (0x41) and
/// no_readers (0x44) looked a reader's address up in σ̂, whose keys are
/// writers, and so missed what the head had tentatively bound its writer to.
_TermVariables _termVariables(RunnerContext cx, Object? term) {
  final heap = cx.rt.heap;
  final out = _TermVariables();
  // A bare int is a variable's address at the top only: inside a tentative
  // structure it is a constant.
  final pending = <Object?>[term is HeapCell ? VarRef(term) : term];
  final visited = <HeapCell>{};
  final seenStructs = Set<Object>.identity();
  while (pending.isNotEmpty) {
    final t = pending.removeLast();
    if (t is VarRef) {
      final addr = t.addr;
      if (!visited.add(addr)) continue;
      if (heap.isReader(addr)) {
        final w = heap.tryWriterForReader(addr);
        if (w != null && cx.sigmaHat.containsKey(w)) {
          pending.add(cx.sigmaHat[w]);
        } else if (heap.isReaderBound(addr)) {
          pending.add(heap.getReaderValue(addr));
        } else {
          out.readers.add(addr);
        }
      } else if (heap.isWriter(addr)) {
        if (cx.sigmaHat.containsKey(addr)) {
          pending.add(cx.sigmaHat[addr]);
        } else if (heap.isFullyBound(addr)) {
          pending.add(heap.getValue(addr));
        } else {
          out.writer = true;
        }
      } else if (heap.isValue(addr)) {
        pending.add(heap.getValue(addr));
      }
    } else if (t is StructTerm) {
      if (seenStructs.add(t)) pending.addAll(t.args);
    } else if (t is _TentativeStruct) {
      if (seenStructs.add(t)) pending.addAll(t.args);
    } else if (t is MutualRefTerm) {
      out.mutualRef = true;
    }
  }
  return out;
}

/// ground/1 on [arg], the argument the generic guard call built
/// ([OpExecutors.execGuard]), decided as the ground instruction (0x41) decides
/// a variable: an unbound writer or a mutual reference fails it, no readers
/// substitution grounding either; unbound readers leave it undecided
/// ([_undecidedMember]); with none, it succeeds.
GuardResult _groundGuard(RunnerContext cx, Object? arg) {
  final vars = _termVariables(cx, _equalityOperand(arg));
  if (vars.writer || vars.mutualRef) return GuardResult.failure;
  if (vars.readers.isNotEmpty) return _undecidedMember(cx, vars.readers);
  return GuardResult.success;
}

/// What a structure slot holds for an occurrence of unknown variable
/// [varIndex] ([RunnerContext.unknownVars]) --- in a structure built for a
/// goal writer, or for a guard's argument: the variable that stands for it
/// ([RunnerContext.unknownPlaceholders]), fresh at its first occurrence and
/// the same at every other, of the occurrence's polarity, which the clause does
/// not keep, so the unknown variable stays unknown.  The structure is never
/// committed, the clause's suspension set being non-empty.
VarRef _unknownPlaceholder(RunnerContext cx, int varIndex, bool isReader) {
  final (writerAddr, readerAddr) =
      cx.unknownPlaceholders.putIfAbsent(varIndex, () {
    final cells = cx.rt.heap.allocateVariable();
    cx.unknownKeys.add(cells.$1);
    return cells;
  });
  return VarRef(isReader ? readerAddr : writerAddr);
}

(Object?, Set<HeapCell>) _dereferenceWithTracking(Object? term, RunnerContext cx) {
  final unboundReaders = <HeapCell>{};

  Object? dereference(Object? t) {
    // NOTE: A VarRef carries a HEAP ADDRESS (terms.dart §3.2.1 — varId was
    // removed). clauseVars is keyed by CLAUSE-VARIABLE INDEX. A former
    // shortcut here looked up clauseVars[t.addr], which after the varId→addr
    // migration (commit 57cf5d96) became a category error: it indexed the
    // clause-index map with a heap address and fired on any numeric
    // collision, silently swapping a guard argument for whatever clause var
    // shared that number — e.g. a reader for an unbound writer, making a
    // patient guard fail instead of suspend (known-issues.md Issue 12).
    // VarRefs never carry clause indices, so no such resolution is needed.

    if (t is VarRef) {
      final addr = t.addr;
      if (cx.rt.heap.isReader(addr)) {
        // Reader - check if bound using abstraction methods for imported reader support
        final readerAddr = addr;

        // Check sigma-hat first for tentative bindings (before commit)
        final writerAddr = cx.rt.heap.tryWriterForReader(readerAddr);
        if (writerAddr != null && cx.sigmaHat.containsKey(writerAddr)) {
          return dereference(cx.sigmaHat[writerAddr]);
        }

        if (cx.rt.heap.isReaderBound(readerAddr)) {
          final boundValue = cx.rt.heap.getReaderValue(readerAddr);
          // CRITICAL FIX: Recursively dereference the bound value
          return dereference(boundValue);
        } else {
          // Unbound reader - track it
          unboundReaders.add(readerAddr);
          return t;
        }
      } else {
        // Writer variable
        final writerAddr = addr;

        // Check sigma-hat first (tentative bindings)
        if (cx.sigmaHat.containsKey(writerAddr)) {
          return dereference(cx.sigmaHat[writerAddr]);
        }

        // Check heap
        if (cx.rt.heap.isFullyBound(writerAddr)) {
          final boundValue = cx.rt.heap.getValue(writerAddr);
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
    } else if (t is HeapCell) {
      // Bare int represents a variable addr - check sigmaHat first, then heap
      if (cx.sigmaHat.containsKey(t)) {
        return dereference(cx.sigmaHat[t]);
      } else if (cx.rt.heap.isFullyBound(t)) {
        final boundValue = cx.rt.heap.getValue(t);
        // Recursively dereference the bound value
        return dereference(boundValue);
      } else {
        // Unbound variable - return as VarRef for proper handling
        return VarRef(t);
      }
    } else {
      return t;
    }
  }

  final result = dereference(term);
  return (result, unboundReaders);
}

/// A guard member left undecided on [readers], as [_undecidedMember] decides
/// it: suspended, its readers of the goal in the clause's suspension set Si and
/// the next member tried, or failed.  "A guard conjunction succeeds if all
/// members succeed; it suspends if any member suspends and none fail; it fails
/// if any member fails" (GLP-Spec glp.tex, Guards), whatever the order of its
/// members: a member that fails leaves the clause, its Si with it (the drivers'
/// next clause, [SuspensionSet.touched] unset), and a clause whose Si is
/// non-empty at commit suspends ([OpExecutors.execCommit]).  Until 2026-10-02 a
/// member that suspended added its readers to the goal's U and left the clause,
/// so a later member that fails was never tried: c1(N, M) :- N? > 5, M? > 5
/// suspended c1(X?, 3), where the same guards swapped failed it (GLP #3
/// Cowork, 2026-10-02 08:40 UTC, G).
StepOutcome _guardUndecided(RunnerContext cx, Iterable<HeapCell> readers) =>
    _undecidedMember(cx, readers) == GuardResult.failure
        ? StepOutcome.nextClause
        : StepOutcome.advance;

/// A guard member that does not succeed, where some instance of it under a
/// readers substitution would (GLP-Spec glp.tex, Guards): it suspends, or it
/// fails, by whose readers [readers] are --- the unbound readers it was decided
/// on.  [negated] is `=?\=`'s, whose success is that no readers substitution
/// makes its two arguments ground and equal, and [unknown] that the decision
/// met an unknown variable besides [readers] ([RunnerContext.unknownVars]).
///
/// "If a GLP goal A cannot be reduced now, but there is a readers substitution
/// σ such that Aσ can be reduced, such readers are identified, the goal A
/// suspends on these readers" (glp.tex): a goal suspends on its own readers,
/// those it holds ([_readersOfGoal]), and no readers substitution binds a
/// variable the clause alone holds --- a fresh one of the head, in a structure
/// it gives a goal writer, or the clause's own output (GLP #3 Cowork,
/// 2026-10-02 15:31 UTC, A).  So the member waits on the goal's readers among
/// [readers], and fails where waiting cannot decide it:
///
/// - Every guard but `=?\=` succeeds only where each of [readers] is bound:
///   one the clause alone holds stays unbound in every instance, and the
///   member fails.
/// - `=?\=` succeeds in an instance assigning a reader of the goal a term with
///   a fresh writer in it, which no readers substitution grounds, and fails
///   where [readers] hold none of the goal's.
///
/// An unknown variable --- its writer occurrence under a goal reader the head
/// suspends on, its value the goal's subterm there, not yet given --- stands
/// for any term ([RunnerContext.unknownKeys]): it is a variable of the goal,
/// and adds no reader to Si, the clause already suspending on the goal reader
/// above it.  Until 2026-10-02 the member waited on every one of [readers],
/// and a goal whose clause guarded a variable it alone held waited for ever:
/// hq(f(X), Y, yes) :- X? =?= w(Y?) | true and hw(f(X), Y, yes) :- X? =?\= Y? |
/// true held hq(W, b, R) and hw(W, b, R).
GuardResult _undecidedMember(RunnerContext cx, Iterable<HeapCell> readers,
    {bool negated = false, bool unknown = false}) {
  var metUnknown = unknown;
  // Each reader not of an unknown variable, by the variable it stands for.
  final variableOf = <HeapCell, Object>{};
  for (final r in readers) {
    final v = _variableAt(cx, r);
    if (v == null) continue;
    if (v is HeapCell && cx.unknownKeys.contains(v)) {
      metUnknown = true;
    } else {
      variableOf[r] = v;
    }
  }
  final ofGoal = _readersOfGoal(cx, variableOf.values.toSet());
  final waitOn = [
    for (final e in variableOf.entries)
      if (ofGoal.contains(e.value)) e.key,
  ];
  final heldByClause = waitOn.length < variableOf.length;
  if (negated ? (waitOn.isEmpty && !metUnknown) : heldByClause) {
    return GuardResult.failure;
  }
  cx.Si.addAll(waitOn);
  return GuardResult.suspend;
}

/// The variable the occurrence at [addr] stands for, its bindings on the heap
/// followed: the address of the unbound writer cell its chain ends at; null
/// where the chain ends at a value.
Object? _variableAt(RunnerContext cx, HeapCell addr) {
  final end = cx.rt.heap.derefAddr(addr);
  if (end is VarRef) return end.addr;
  return null;
}

/// Which of [variables] --- each as [_variableAt] gives it --- the goal holds a
/// reader of: a reader reachable from the goal's arguments through the
/// bindings on the heap, the tentative ones (σ̂) aside, they being the
/// clause's.  An occurrence is a reader where its chain passes a reader: one
/// that does not start at the unbound writer cell it ends at, a writer being
/// bound to a reader or a term and never to a writer.  The goal is searched
/// breadth first, until every one is found: a guard waits on readers the head
/// matched, near the top of the goal's arguments, whatever lies deeper in
/// them.
Set<Object> _readersOfGoal(RunnerContext cx, Set<Object> variables) {
  final heap = cx.rt.heap;
  final found = <Object>{};
  if (variables.isEmpty) return found;
  final pending = Queue<Object?>.of(cx.env.argBySlot.values);
  final seen = <HeapCell>{};
  final seenStructs = Set<StructTerm>.identity();
  while (pending.isNotEmpty && found.length < variables.length) {
    final t = pending.removeFirst();
    if (t is VarRef) {
      final addr = t.addr;
      if (!seen.add(addr)) continue;
      final end = heap.derefAddr(addr);
      if (end is VarRef) {
        if (end.addr != addr && variables.contains(end.addr)) {
          found.add(end.addr);
        }
      } else {
        pending.add(end);
      }
    } else if (t is StructTerm) {
      if (seenStructs.add(t)) pending.addAll(t.args);
    }
  }
  return found;
}

/// The guards [_evaluateGuard] evaluates, by name and arity: the guard
/// predicates of the catalogue (GLP-Spec appendix-guards.tex) the runtime
/// implements and the generic guard instruction names.  `ground/1`, `known/1`,
/// `otherwise/0` and `=?=/2` have instructions of their own besides (codegen.dart,
/// _generateGuard); `no_readers/1` has only its instruction, which takes a
/// variable.  The compiler refuses a guard instruction naming anything else, so
/// an unknown guard is refused at compile time (codegen.dart) and never reaches
/// the evaluator.  `valid_attestation/4` is no guard: the catalogue's guard
/// table does not carry it and `signature/2` does its work (GLP, 2026-09-20
/// 11:53 UTC); it was evaluated here until 2026-10-10.
const Set<String> runtimeGuards = {
  '</2', '>/2', '=</2', '>=/2', '=:=/2', '=\\=/2', '@</2',
  'ground/1', 'known/1', 'integer/1', 'string/1', 'constant/1', 'number/1',
  'real/1', 'list/1', 'compound/1', 'module/1', 'is_mutual_ref/1', 'unknown/1',
  'otherwise/0', 'wait/1', 'wait_until/1', 'when_idle/0', 'no_readers/1',
  '=?=/2', '=?\\=/2',
};

/// The arithmetic comparison guards (GLP-Spec appendix-guards.tex,
/// "Arithmetic comparison guards"), by name: each evaluates both operands as
/// arithmetic expressions in [_evaluateGuard] before it is decided.
const Set<String> _arithmeticComparisons = {'<', '>', '=<', '>=', '=:=', '=\\='};

/// Whether constant [a] precedes constant [b] in the standard order of
/// constants, which `@<` decides (GLP-Spec appendix-guards.tex, 2bfb42b): "a
/// number precedes a string; numbers compare by value, and strings by the codes
/// of their characters, lexicographically".  Each is a [num], a [String] or the
/// empty list [Nil].  No paper places the empty list in this order; it keeps
/// the place it had before the order was written, by its text `[]`.
bool _precedesInStandardOrder(Object a, Object b) {
  final x = a is Nil ? a.toString() : a;
  final y = b is Nil ? b.toString() : b;
  if (x is num && y is num) return x < y;
  if (x is num) return true; // a number precedes a string
  if (y is num) return false;
  return _compareCharacterCodes(x as String, y as String) < 0;
}

/// [a] against [b] by the codes of their characters, lexicographically: the
/// first differing code decides, and a proper prefix precedes.  A character's
/// code is its Unicode code point, so a character beyond U+FFFF, which Dart
/// holds as two UTF-16 code units, is compared as the one character it is.
int _compareCharacterCodes(String a, String b) {
  final ia = a.runes.iterator;
  final ib = b.runes.iterator;
  while (true) {
    final moreA = ia.moveNext();
    final moreB = ib.moveNext();
    if (!moreA) return moreB ? -1 : 0;
    if (!moreB) return 1;
    final d = ia.current - ib.current;
    if (d != 0) return d;
  }
}

GuardResult _evaluateGuard(String predicateName, List<Object?> args, RunnerContext cx) {
  // Extract values from any remaining ConstTerms
  Object? getValue(Object? v) {
    if (v is ConstTerm) return v.value;
    return v;
  }

  // Unbound readers that blocked arithmetic/constant evaluation of a guard
  // operand (readers nested inside expression structures — the top-level
  // dereference in execGuard does not descend into structures). Per the guards
  // reference (Comparison Guards), a comparison whose operand is an unbound
  // reader SUSPENDS; it fails only on bound, non-numeric operands or a false
  // comparison. When evaluation returns null AND this set is non-empty, the
  // guard suspends on these readers instead of failing.
  final blockedReaders = <HeapCell>{};

  // Whether an operand evaluated has no value under any readers substitution:
  // a bound term that is neither a number nor an arithmetic expression, an
  // unbound writer, which no readers substitution assigns, a quotient or
  // remainder whose divisor is zero or, under `//` and `mod`, which take
  // integers only, whose operand is no integer, or a function whose argument
  // is outside its domain.  Every arithmetic operator and function needs a
  // value of each of its operands, so then no instance of the comparison
  // succeeds, and it fails, whatever readers blocked the rest of it: "A guard
  // fails if no such instance exists" (GLP-Spec glp.tex, Guards).  Until
  // 2026-10-02 it waited on those readers: cz(X, yes) :- X? / 0 > 1 | true
  // held cz(Q?, R) (GLP #3 Cowork, 2026-10-02 17:12 UTC, S3).
  var undefinedInEveryInstance = false;
  num? undefined() {
    undefinedInEveryInstance = true;
    return null;
  }

  // The number an arithmetic expression of type Exp evaluates to (the root
  // self.glp): numbers, +, -, *, /, // and mod, unary negation (neg, as -X
  // parses), pow and the sixteen unary functions, each as its kernel computes
  // it --- "Arithmetic comparison guards evaluate their arguments as
  // arithmetic expressions of type Exp" (GLP-Spec appendix-guards.tex,
  // 026515d).  Null where an unbound reader blocks it ([blockedReaders]) or it
  // has no value ([undefined]).
  num? evaluateNumeric(Object? v) {
    if (v is num) return v;
    if (v is ConstTerm && v.value is num) return v.value as num;
    // Handle VarRef - dereference to get actual value
    if (v is VarRef) {
      if (cx.rt.heap.isReader(v.addr)) {
        // Tentative σ̂w binding of the paired writer (this clause try) wins
        // over the heap, mirroring _dereferenceWithTracking.
        final writerAddr = cx.rt.heap.tryWriterForReader(v.addr);
        if (writerAddr != null && cx.sigmaHat.containsKey(writerAddr)) {
          return evaluateNumeric(cx.sigmaHat[writerAddr]);
        }
        // Use isReaderBound/getReaderValue for imported reader support
        if (!cx.rt.heap.isReaderBound(v.addr)) {
          blockedReaders.add(v.addr); // Unbound reader blocks evaluation
          return null;
        }
        final deref = cx.rt.heap.getReaderValue(v.addr);
        return evaluateNumeric(deref);
      } else {
        if (cx.sigmaHat.containsKey(v.addr)) {
          return evaluateNumeric(cx.sigmaHat[v.addr]);
        }
        final deref = cx.rt.heap.getValue(v.addr);
        if (deref == null) {
          // An unbound writer: no readers substitution assigns it.  One that
          // stands for an unknown variable, any term, is left as it was.
          if (cx.unknownKeys.contains(v.addr)) return null;
          return undefined();
        }
        return evaluateNumeric(deref);
      }
    }
    if (v is StructTerm) {
      // Evaluate arithmetic expression
      switch (v.functor) {
        case '+':
          if (v.args.length != 2) return undefined();
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
          return undefined();
        case '*':
          if (v.args.length != 2) return undefined();
          final a = evaluateNumeric(v.args[0]);
          final b = evaluateNumeric(v.args[1]);
          if (a == null || b == null) return null;
          return a * b;
        case '/':
          if (v.args.length != 2) return undefined();
          final a = evaluateNumeric(v.args[0]);
          final b = evaluateNumeric(v.args[1]);
          // A zero divisor aborts whatever the dividend becomes.
          if (b == 0) return undefined();
          if (a == null || b == null) return null;
          return a / b;
        case '//':
        case 'mod':
          // Integers only, as '_idiv' and '_mod' take them: an operand that
          // is no integer has no value, nor has a zero divisor, whatever
          // readers stand in the other (GLP-Spec appendix-guards.tex,
          // 026515d; GLP #3 Cowork, 2026-10-02 20:58 UTC, "20:10" B).  Until
          // 2026-10-02 `//` divided reals and `mod` truncated its operands, so
          // X? mod 0.5 =:= 1 with X = 5 threw IntegerDivisionByZeroException.
          if (v.args.length != 2) return undefined();
          final a = evaluateNumeric(v.args[0]);
          final b = evaluateNumeric(v.args[1]);
          if (a != null && a is! int) return undefined();
          if (b != null && (b is! int || b == 0)) return undefined();
          if (a == null || b == null) return null;
          return v.functor == '//' ? a ~/ b : a % b;
        case 'neg':
          if (v.args.length != 1) return undefined();
          final a = evaluateNumeric(v.args[0]);
          return a == null ? null : -a;
        default:
          // pow and the sixteen unary functions of Exp, each as its kernel
          // computes it ([expFunction]): an argument outside the function's
          // domain has no value, whatever readers stand elsewhere (GLP-Spec
          // appendix-guards.tex, 026515d; GLP #3 Cowork, 2026-10-02 20:58
          // UTC, "20:10" C).  Any other functor is no arithmetic one and has
          // no value either.  Until 2026-10-02 every function came here and
          // had none, so sqrt(X?) > 1 never succeeded.
          if (!isExpFunction(v.functor, v.args.length)) return undefined();
          final xs = [for (final a in v.args) evaluateNumeric(a)];
          if (xs.contains(null)) return null;
          return expFunction(v.functor, [for (final x in xs) x!]) ??
              undefined();
      }
    }
    // A bound term that is not a number: a constant of another kind, a
    // string, a module value.
    return undefined();
  }

  // A guard operand failed to evaluate: undecided if unbound readers blocked
  // the evaluation (guards-reference: comparison guards suspend on unbound
  // reader operands), suspending on the goal's and failing on one the clause
  // alone holds ([_undecidedMember]); fail otherwise (bound but non-numeric —
  // a type error).
  GuardResult blockedOrFail() {
    if (blockedReaders.isNotEmpty) return _undecidedMember(cx, blockedReaders);
    return GuardResult.failure;
  }

  // A comparison whose operands did not both evaluate: it fails where one has
  // no value under any readers substitution ([undefinedInEveryInstance]),
  // whatever readers blocked the other, and is otherwise as above.
  GuardResult comparisonBlockedOrFail() =>
      undefinedInEveryInstance ? GuardResult.failure : blockedOrFail();

  switch (predicateName) {
    // Comparison guards (with arithmetic expression support)
    case '<':
      if (args.length < 2) return GuardResult.failure;
      final a = evaluateNumeric(args[0]);
      final b = evaluateNumeric(args[1]);
      if (a != null && b != null) {
        return a < b ? GuardResult.success : GuardResult.failure;
      }
      return comparisonBlockedOrFail();

    case '>':
      if (args.length < 2) return GuardResult.failure;
      final a = evaluateNumeric(args[0]);
      final b = evaluateNumeric(args[1]);
      if (a != null && b != null) {
        return a > b ? GuardResult.success : GuardResult.failure;
      }
      return comparisonBlockedOrFail();

    case '=<':
      if (args.length < 2) return GuardResult.failure;
      final a = evaluateNumeric(args[0]);
      final b = evaluateNumeric(args[1]);
      if (a != null && b != null) {
        return a <= b ? GuardResult.success : GuardResult.failure;
      }
      return comparisonBlockedOrFail();

    case '>=':
      if (args.length < 2) return GuardResult.failure;
      final a = evaluateNumeric(args[0]);
      final b = evaluateNumeric(args[1]);
      if (a != null && b != null) {
        return a >= b ? GuardResult.success : GuardResult.failure;
      }
      return comparisonBlockedOrFail();

    case '=:=':
      if (args.length < 2) return GuardResult.failure;
      final a = evaluateNumeric(args[0]);
      final b = evaluateNumeric(args[1]);
      if (a != null && b != null) {
        return a == b ? GuardResult.success : GuardResult.failure;
      }
      return comparisonBlockedOrFail();

    case '=\\=':
      if (args.length < 2) return GuardResult.failure;
      final a = evaluateNumeric(args[0]);
      final b = evaluateNumeric(args[1]);
      if (a != null && b != null) {
        return a != b ? GuardResult.success : GuardResult.failure;
      }
      return comparisonBlockedOrFail();

    // The standard order of constants (GLP-Spec appendix-guards.tex, 2bfb42b):
    // "@< succeeds if both arguments are ground constants and the first
    // precedes the second in the standard order of constants: a number
    // precedes a string; numbers compare by value, and strings by the codes of
    // their characters, lexicographically" ([_precedesInStandardOrder]).  An
    // argument bound to a term that is no constant fails the guard, whatever
    // the other is: no instance of it succeeds (glp.tex, Guards).  Until
    // 2026-10-09 it compared the printed text of the two, so 10 @< 9 and
    // -1 @< -2 succeeded, 9 @< 10 and 5 @< '!' failed, and f(a) @< X? waited
    // on X?.
    case '@<':
      if (args.length < 2) return GuardResult.failure;
      // Set where an argument is bound to a term that is no constant.
      var noConstant = false;
      // The constant an argument is, as the order takes it: a number (num), a
      // string (String) or the empty list ([Nil]); null where it is not yet
      // known (its reader recorded) or is no constant.
      Object? evalConst(dynamic v) {
        if (v == null) return null;
        if (v is ConstTerm) return evalConst(v.value);
        if (v is String || v is num || v is Nil) return v;
        if (v is VarRef) {
          if (cx.rt.heap.isReader(v.addr)) {
            final writerAddr = cx.rt.heap.tryWriterForReader(v.addr);
            if (writerAddr != null && cx.sigmaHat.containsKey(writerAddr)) {
              return evalConst(cx.sigmaHat[writerAddr]);
            }
            if (!cx.rt.heap.isReaderBound(v.addr)) {
              blockedReaders.add(v.addr);
              return null;
            }
            return evalConst(cx.rt.heap.getReaderValue(v.addr));
          }
          if (cx.sigmaHat.containsKey(v.addr)) {
            return evalConst(cx.sigmaHat[v.addr]);
          }
          final deref = cx.rt.heap.getValue(v.addr);
          return deref == null ? null : evalConst(deref);
        }
        noConstant = true;
        return null;
      }
      final lc = evalConst(args[0]);
      final rc = evalConst(args[1]);
      if (lc != null && rc != null) {
        return _precedesInStandardOrder(lc, rc)
            ? GuardResult.success
            : GuardResult.failure;
      }
      if (noConstant) return GuardResult.failure;
      return blockedOrFail();

    // Type guards
    case 'ground':
      // A term built in the guard, decided as ground (0x41) decides a
      // variable ([_groundGuard]).  Until 2026-10-02 this succeeded on any
      // term, the caller having looked for unbound readers at its top alone:
      // ground(h(Z?)) succeeded with Z? unbound, ground(h(W)) with W an
      // unbound writer, and ground(h(M?)) on a mutual reference.
      if (args.isEmpty) return GuardResult.failure;
      return _groundGuard(cx, args[0]);

    case 'no_readers':
      // A term built in the guard, decided as no_readers (0x44) decides a
      // variable: its unbound readers ([_termVariables]), none succeeding and
      // some leaving it undecided ([_undecidedMember]).  GLP-Spec
      // appendix-guards.tex: "no_readers(f(X?)) suspends but known(f(X?))
      // succeeds."  Until 2026-10-02 no case evaluated it: the call failed
      // the clause with a warning, an unknown guard (GLP #3 Cowork,
      // 2026-10-02 15:31 UTC, item 5, B9).
      if (args.isEmpty) return GuardResult.failure;
      final readers = _termVariables(cx, _equalityOperand(args[0])).readers;
      if (readers.isEmpty) return GuardResult.success;
      return _undecidedMember(cx, readers);

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
      // Succeeds if X is a String: a string, or the empty list [], whose type
      // is String (TGLP appendix-root-self.tex: "The empty list is a String,
      // hence a Constant"), though as a value it is the constant [] and no
      // string ([nil]).
      if (args.isEmpty) return GuardResult.failure;
      final val = getValue(args[0]);
      if (val is ConstTerm && (val.value is String || val.value is Nil)) {
        return GuardResult.success;
      }
      if (val is String || val is Nil) {
        return GuardResult.success;
      }
      return GuardResult.failure;

    case 'constant':
      // Succeeds if X is a constant. Per the root self.glp type definitions
      // `Constant ::= Number ; String ; Module` — a String (the empty list []
      // among them, by its type: TGLP appendix-root-self.tex), a Number, or a
      // Module term. The guard previously rejected module terms, disagreeing
      // with the types.
      if (args.isEmpty) return GuardResult.failure;
      final val = getValue(args[0]);
      // String, or the empty list
      if (val is ConstTerm && (val.value is String || val.value is Nil)) {
        return GuardResult.success;
      }
      if (val is String || val is Nil) {
        return GuardResult.success;
      }
      // Number
      if (val is num) {
        return GuardResult.success;
      }
      if (val is ConstTerm && val.value is num) {
        return GuardResult.success;
      }
      // Module — the same representation module/1 tests
      if (val is ModuleTerm) {
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

    case 'real':
      // Succeeds if X is a Real (GLP-Spec appendix-guards.tex, 12be29b:
      // `procedure real(Real?).`, Ground yes), the runtime's floating-point
      // number, a double: the lexer reads a literal with a decimal point as
      // one, and `/` and '_real' give one.  So real(2.0) succeeds and real(2),
      // an Integer, fails, as integer(2.0) does.  An unbound reader leaves it
      // undecided before it is reached ([OpExecutors.execGuard]).
      if (args.isEmpty) return GuardResult.failure;
      final val = getValue(args[0]);
      return (val is double) ? GuardResult.success : GuardResult.failure;

    case 'list':
      // Succeeds if X is a list ([] or [H|T])
      if (args.isEmpty) return GuardResult.failure;
      final val = getValue(args[0]);
      // Empty list: ConstTerm(nil) or the raw value nil
      if (val is ConstTerm && val.value == nil) {
        return GuardResult.success;
      }
      if (val is Nil) {
        return GuardResult.success;
      }
      // Non-empty list: StructTerm('.', [head, tail])
      if (val is StructTerm && val.functor == '.' && val.args.length == 2) {
        return GuardResult.success;
      }
      return GuardResult.failure;

    case 'compound':
      // Succeeds if X is a compound term (structure with functor and arity > 0)
      // Per guards-reference.md: "Test for compound term"
      // Lists are compound since [X|Xs] = '.'(X, Xs)
      // Does NOT imply groundness - may contain unbound subterms
      if (args.isEmpty) return GuardResult.failure;
      final val = getValue(args[0]);
      if (val is StructTerm && val.args.isNotEmpty) {
        return GuardResult.success;
      }
      return GuardResult.failure;

    case 'module':
      // Succeeds if X is a ModuleTerm (ground module reference)
      if (args.isEmpty) return GuardResult.failure;
      final mval = getValue(args[0]);
      if (mval is ModuleTerm) {
        return GuardResult.success;
      }
      return GuardResult.failure;

    case 'is_mutual_ref':
      // Succeeds if X is a MutualRefTerm.  It grounds nothing ("Ground: no",
      // GLP-Spec appendix-guards.tex); a repeated reader of a mutual reference
      // is licensed by its type, MutualRef (TGLP typed-glp.tex, 350eb7d).
      if (args.isEmpty) return GuardResult.failure;
      final val = getValue(args[0]);
      if (val is MutualRefTerm) {
        return GuardResult.success;
      }
      return GuardResult.failure;

    case 'unknown':
      // Test if dereferencing leads to an unbound variable
      // Per spec: "Succeeds if X is bound to an unbound variable"
      // This means we follow the binding chain to its end
      if (args.isEmpty) return GuardResult.failure;
      Object? value = args[0];

      // Follow binding chain to end
      while (value is VarRef) {
        final addr = value.addr;
        if (cx.rt.heap.isReader(addr)) {
          // Use abstraction methods for imported reader support
          final writerAddr = cx.rt.heap.tryWriterForReader(addr);
          if (writerAddr != null && cx.sigmaHat.containsKey(writerAddr)) {
            value = cx.sigmaHat[writerAddr];
            continue;
          }
          // Check heap using isReaderBound/getReaderValue
          if (cx.rt.heap.isReaderBound(addr)) {
            value = cx.rt.heap.getReaderValue(addr);
            continue;
          }
          // Reached an unbound reader → SUCCESS
          return GuardResult.success;
        } else {
          // Writer - check σ̂w first, then heap
          if (cx.sigmaHat.containsKey(addr)) {
            value = cx.sigmaHat[addr];
            continue;
          }
          if (cx.rt.heap.isFullyBound(addr)) {
            value = cx.rt.heap.getValue(addr);
            continue;
          }
          // Reached an unbound writer → SUCCESS
          return GuardResult.success;
        }
      }
      // Dereferenced to a non-variable (ground term) → FAILURE
      return GuardResult.failure;

    // Note: duplicate 'unknown' case removed - the first one handles it

    // Control guards
    case 'otherwise':
      // Unreachable: execGuard routes a generic call of otherwise/0 to
      // execOtherwise (0x46), the one rule for it.  Reaching here is a fault.
      throw StateError(
          'otherwise/${args.length} reached the guard evaluator; otherwise '
          'takes no arguments and is decided by execOtherwise (0x46)');

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
      if (duration == null) return blockedOrFail();
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
          cx.Si.add(existingReader);
          return GuardResult.suspend;
        }
      }

      // First call - create fresh reader/writer pair for timer notification
      final (writerAddr, readerAddr) = cx.rt.heap.allocateVariable();

      // Store wait state for this goal
      cx.rt.setWaitReader(cx.goalId, readerAddr);

      // Track pending timer
      cx.rt.incrementPendingTimers();

      // Start timer that binds writer when it fires
      Timer(Duration(milliseconds: duration.toInt()), () {
        // Bind writer to 0 (any value works)
        final reactivated = cx.rt.heap.bindWriterConst(writerAddr, 0);
        // Enqueue reactivated goals and clean up suspended map
        for (final goalRef in reactivated) {
          cx.rt.enqueueReactivatedGoal(goalRef);
        }
        // Decrement pending timer count
        cx.rt.decrementPendingTimers();
      });

      // Suspend on the timer's reader, which the runtime binds: it is no
      // reader of the guard's argument ([_guardUndecided] decides those).
      // Until 2026-10-02 the reader went to U and the guard reported failure,
      // U telling the clause's suspension from a failure.
      cx.Si.add(readerAddr);
      return GuardResult.suspend;

    case 'wait_until':
      // wait_until(Timestamp) - Suspend until absolute time has passed
      // Semantics:
      // - Unbound Timestamp: handled by caller (suspend on reader)
      // - Non-number: fail
      // - current time >= Timestamp: succeed
      // - current time < Timestamp: suspend until time passes (timer-based)
      if (args.isEmpty) return GuardResult.failure;
      final timestamp = evaluateNumeric(args[0]);
      if (timestamp == null) return blockedOrFail();
      final now = DateTime.now().millisecondsSinceEpoch;
      if (now >= timestamp) return GuardResult.success;

      // Time hasn't arrived yet — use timer-based suspension (same as wait)
      final remaining = timestamp.toInt() - now;

      // Check if this goal already has a pending wait_until
      final existingReaderWU = cx.rt.getWaitReader(cx.goalId);
      if (existingReaderWU != null) {
        if (cx.rt.heap.isFullyBound(existingReaderWU)) {
          cx.rt.clearWaitState(cx.goalId);
          return GuardResult.success;
        } else {
          cx.Si.add(existingReaderWU);
          return GuardResult.suspend;
        }
      }

      // First call — create fresh reader/writer pair for timer notification
      final (writerAddrWU, readerAddrWU) = cx.rt.heap.allocateVariable();
      cx.rt.setWaitReader(cx.goalId, readerAddrWU);
      cx.rt.incrementPendingTimers();

      Timer(Duration(milliseconds: remaining), () {
        final reactivated = cx.rt.heap.bindWriterConst(writerAddrWU, 0);
        for (final goalRef in reactivated) {
          cx.rt.enqueueReactivatedGoal(goalRef);
        }
        cx.rt.decrementPendingTimers();
      });

      cx.Si.add(readerAddrWU);
      return GuardResult.suspend;

    case 'when_idle':
      // GLP-Spec appendix-guards.tex (e3a8d52), the time guards: "when_idle
      // suspends while the machine has a Reduce or a Communicate to make, and
      // succeeds when it has none."  The machine's idleness decides it: its
      // queue empty, this goal having been taken from it, and in madGLP its
      // outbox too (IGLP eadadcd, Implementation Notes, "The when_idle
      // Guard"; GlpRuntime.isIdle).  Otherwise the goal suspends on a reader
      // the scheduler assigns when the machine is idle (GlpRuntime.wakeIdle),
      // and is re-tried then.
      if (cx.rt.isIdle) {
        cx.rt.clearIdleWait(cx.goalId);
        return GuardResult.success;
      }
      cx.Si.add(cx.rt.idleReader(cx.goalId));
      return GuardResult.suspend;

    case '=?=':
    case '=?\\=':
      // The ground equality guards (GLP-Spec appendix-guards.tex, bbff21d),
      // decided as ground_equal (0x45) decides =?= ([_groundEqualityGuard]).
      // =?\= has no instruction of its own (IGLP code-format-fragment.tex,
      // 9b45225), so every =?\= comes here, and every =?= with an operand that
      // is not a variable; [execGuard] leaves their unbound readers to the
      // decision.
      if (args.length < 2) return GuardResult.failure;
      return _groundEqualityGuard(predicateName, _equalityOperand(args[0]),
          _equalityOperand(args[1]), cx);

    default:
      // Unreachable: the compiler refuses a guard instruction that names no
      // guard of [runtimeGuards] (codegen.dart, _generateGuard).  Until
      // 2026-10-02 an unknown guard printed a [WARN] here and failed the
      // clause at run time.
      throw StateError('unknown guard predicate $predicateName/${args.length} '
          'reached the guard evaluator; the compiler refuses an unknown guard');
  }
}

/// What the ground equality guards decide of their two arguments, `=?=` and
/// `=?\=` alike.  A guard predicate states its success condition and nothing
/// else (GLP-Spec appendix-guards.tex, bbff21d): "=?= succeeds if both
/// arguments are ground and equal.  =?\= succeeds if no readers substitution
/// makes them ground and equal."  Suspension and failure follow from the guard
/// semantics (glp.tex, Guards): "A guard suspends if it does not succeed but
/// some instance of it under a readers substitution would succeed.  A guard
/// fails if no such instance exists."  So one question decides both guards,
/// whether some readers substitution makes the two ground and equal
/// ([_decideGroundEquality], for ground_equal (0x45) and the generic guard call
/// alike), and [_groundEqualityGuard] reads its answer for each.
enum _GroundEquality {
  /// Both are ground and equal.  `=?=` succeeds.  `=?\=` fails: a readers
  /// substitution leaves ground terms as they are, so its every instance is the
  /// guard itself, which the empty substitution makes ground and equal.
  equal,

  /// Not both ground, and some readers substitution makes them ground and
  /// equal: no unbound writer stands in either, and the two unify with readers
  /// alone assigned.  Neither guard succeeds, and each is decided on the
  /// unbound readers of the two by whose readers they are ([_undecidedMember]):
  /// `=?=`'s instance under that substitution succeeds, where it binds only
  /// readers of the goal; `=?\=`'s instance under a substitution assigning one
  /// of the goal's readers a term with a fresh writer in it, which no readers
  /// substitution grounds, does.
  unifiable,

  /// No readers substitution makes them ground and equal: an unbound writer
  /// stands in one or the other, which no readers substitution grounds, or the
  /// two do not unify with readers alone assigned --- two constants that
  /// differ, a clash of functor or arity, a reader that would have to stand
  /// for two different terms or for a term containing itself --- whatever
  /// stands elsewhere in either.  `=?=` fails, no instance of it succeeding.
  /// `=?\=` succeeds.
  never,
}

/// An unbound variable met by [_decideGroundEquality]: [key], the variable ---
/// the address of the writer cell its chain of bindings ends at ---; whether
/// the occurrence
/// met is a reader ([isReader]: a reader cell, or a bound writer whose chain
/// ends at a reader); and [readerAddr], the reader a suspension waits on.
class _UnboundVariable {
  final HeapCell key;
  final bool isReader;
  final HeapCell readerAddr;
  const _UnboundVariable(this.key, this.isReader, this.readerAddr);
}

/// The arguments of a compound value, a heap structure or a tentative one;
/// null for any other value.
List<Object?>? _compoundArgs(Object? value) => value is StructTerm
    ? value.args
    : (value is _TentativeStruct ? value.args : null);

/// The functor of a compound value ([_compoundArgs]).
String? _compoundFunctor(Object? value) => value is StructTerm
    ? value.functor
    : (value is _TentativeStruct ? value.functor : null);

/// What a constant compares by: a [ConstTerm]'s value, any other value itself.
Object? _constantValue(Object? value) =>
    value is ConstTerm ? value.value : value;

/// A guard argument as [execGuard] gives it, dereferenced, for
/// [_decideGroundEquality]: a constant comes unwrapped there, so it is wrapped
/// again, an int being a number and not a variable's address.
Object? _equalityOperand(Object? value) =>
    value is Term || value is _TentativeStruct ? value : ConstTerm(value);

/// Decide whether some readers substitution makes [left] and [right] ground
/// and equal ([_GroundEquality]), and give the unbound readers of the two, on
/// which each guard suspends where it does.
///
/// The two are unified with readers alone assigned, traversed jointly as term
/// matching traverses a goal and a head (GLP-Spec appendix-term-matching.tex),
/// the assignments kept here and never applied: two compounds of one functor
/// and arity are descended into, two constants compared, an unbound reader
/// assigned the value it meets or aliased to the reader it meets, and anything
/// else is a clash.  A clash, or an unbound writer met, decides `never`
/// whatever stands elsewhere in either; so does a mutual reference, which holds
/// the writer of a stream tail and is "neither ground nor a constant type"
/// (TGLP typed-glp.tex).  Where the two unify, the values the readers were
/// assigned are scanned for the variables in them: a writer decides `never`,
/// and so does a reader that would stand for a term containing itself (the
/// occurs check).  Then no unbound reader met is `equal`, and some is
/// `unifiable`.  A variable that stands for an unknown variable
/// ([RunnerContext.unknownKeys]) stands for any term, and is assigned as a
/// reader is, whatever its polarity: so `X? =?= g(W)`, X unknown, is `never`,
/// no term X stands for making the two ground and equal (GLP #3 Cowork,
/// 2026-10-02 15:31 UTC, B).
///
/// A value is a heap term, a tentative structure or a bare variable address
/// (an int), as a clause variable may hold; a constant at the top of a generic
/// guard's argument comes wrapped ([_equalityOperand]).  Any other value --- a
/// module, or a placeholder of the head --- is compared as a constant, by
/// equality.
(_GroundEquality, Set<HeapCell>) _decideGroundEquality(
    Object? left, Object? right, RunnerContext cx) {
  final heap = cx.rt.heap;
  const never = (_GroundEquality.never, <HeapCell>{});

  // The unbound readers met, by key, each with the address suspended on.
  final readers = <HeapCell, HeapCell>{};
  // The assignments to readers that make the two equal: a reader's key to the
  // value it stands for, or to the unbound reader it is aliased to.
  final assigned = <HeapCell, Object?>{};
  // An assigned reader's key to the keys of the readers in what it was
  // assigned, for the occurs check.
  final contains = <HeapCell, Set<HeapCell>>{};
  // The compound values assigned to readers, scanned once the two unify.
  final assignedCompounds = <(HeapCell, Object?)>[];

  // [term] with the bindings of its variables followed, tentative (σ̂w) before
  // the heap: a value, or the unbound variable a chain ends at.  A constant
  // stays in its ConstTerm, so that what this gives is given back unchanged,
  // and no number is taken for a variable's address.
  Object? resolve(Object? term) {
    var t = term;
    final seen = <HeapCell>{};
    while (true) {
      final HeapCell? addr = t is VarRef ? t.addr : (t is HeapCell ? t : null);
      if (addr == null) return t;
      final isReaderCell = heap.isReader(addr);
      if (!seen.add(addr)) return _UnboundVariable(addr, isReaderCell, addr);
      final writerAddr = isReaderCell ? heap.tryWriterForReader(addr) : addr;
      if (writerAddr != null && cx.sigmaHat.containsKey(writerAddr)) {
        t = cx.sigmaHat[writerAddr];
        continue;
      }
      final end = heap.derefAddr(addr);
      if (end is VarRef) {
        // An unbound variable of this heap, its writer cell at the chain's end.
        // The occurrence met is a writer only where it is that cell itself: a
        // reader cell, or a bound writer, whose chain ends there met a reader.
        final v = end.addr;
        if (v != addr && cx.sigmaHat.containsKey(v)) {
          t = cx.sigmaHat[v];
          continue;
        }
        if (v == addr) return _UnboundVariable(v, false, addr);
        return _UnboundVariable(
            v, true, isReaderCell ? addr : heap.pairedReaderAddr(v));
      }
      return end;
    }
  }

  // [term] resolved ([resolve]), and an unbound reader the assignments above
  // give a value or an alias followed to what it stands for.
  Object? resolveAssigned(Object? term) {
    var value = resolve(term);
    while (value is _UnboundVariable &&
        value.isReader &&
        assigned.containsKey(value.key)) {
      value = resolve(assigned[value.key]);
    }
    return value;
  }

  void noteReader(_UnboundVariable v) =>
      readers.putIfAbsent(v.key, () => v.readerAddr);

  // Whether [v] may be assigned a term: a reader, or a variable that stands
  // for an unknown variable, any term, of either polarity
  // ([RunnerContext.unknownKeys]).
  bool assignable(_UnboundVariable v) =>
      v.isReader || cx.unknownKeys.contains(v.key);

  // Unify [a] with [b], readers alone assigned: false at a clash, an unbound
  // writer or a mutual reference, which decide `never`.
  bool unify(Object? a, Object? b) {
    final pairs = <(Object?, Object?)>[(a, b)];
    final walked = <(Object, Object)>{};
    while (pairs.isNotEmpty) {
      final (x, y) = pairs.removeLast();
      final vx = resolveAssigned(x);
      final vy = resolveAssigned(y);
      if (vx is MutualRefTerm || vy is MutualRefTerm) return false;
      if (vx is _UnboundVariable || vy is _UnboundVariable) {
        for (final v in [vx, vy]) {
          if (v is! _UnboundVariable) continue;
          if (!assignable(v)) return false;
          noteReader(v);
        }
        if (vx is _UnboundVariable && vy is _UnboundVariable) {
          if (vx.key != vy.key) {
            assigned[vx.key] = vy;
            contains[vx.key] = {vy.key};
          }
          continue;
        }
        final (reader, value) = vx is _UnboundVariable
            ? (vx, vy)
            : (vy as _UnboundVariable, vx);
        assigned[reader.key] = value;
        if (_compoundArgs(value) != null) {
          assignedCompounds.add((reader.key, value));
        }
        continue;
      }
      final argsX = _compoundArgs(vx);
      final argsY = _compoundArgs(vy);
      if (argsX == null && argsY == null) {
        if (_constantValue(vx) != _constantValue(vy)) return false;
        continue;
      }
      if (argsX == null ||
          argsY == null ||
          _compoundFunctor(vx) != _compoundFunctor(vy) ||
          argsX.length != argsY.length) {
        return false;
      }
      if (argsX.isEmpty || !walked.add((vx!, vy!))) continue;
      for (var i = argsX.length - 1; i >= 0; i--) {
        pairs.add((argsX[i], argsY[i]));
      }
    }
    return true;
  }

  // The variables in [value], assigned to the reader [key]: false at an
  // unbound writer or a mutual reference; each reader met is noted, as
  // contained in what [key] stands for.
  bool scan(HeapCell key, Object? value) {
    final terms = <Object?>[value];
    final visited = <Object>{};
    while (terms.isNotEmpty) {
      final v = resolve(terms.removeLast());
      if (v is MutualRefTerm) return false;
      if (v is _UnboundVariable) {
        if (!assignable(v)) return false;
        noteReader(v);
        (contains[key] ??= <HeapCell>{}).add(v.key);
        continue;
      }
      final args = _compoundArgs(v);
      if (args == null || args.isEmpty || !visited.add(v!)) continue;
      terms.addAll(args);
    }
    return true;
  }

  // Whether some assigned reader stands, through the assignments, for a term
  // containing itself: a cycle in [contains].
  bool occurs() {
    final state = <HeapCell, bool>{}; // false: on the path; true: done
    for (final start in contains.keys) {
      if (state[start] == true) continue;
      state[start] = false;
      final path = <(HeapCell, Iterator<HeapCell>)>[(start, contains[start]!.iterator)];
      while (path.isNotEmpty) {
        final (node, next) = path.last;
        if (!next.moveNext()) {
          state[node] = true;
          path.removeLast();
          continue;
        }
        final k = next.current;
        final s = state[k];
        if (s == false) return true;
        if (s == null) {
          state[k] = false;
          path.add((k, (contains[k] ?? const <HeapCell>{}).iterator));
        }
      }
    }
    return false;
  }

  if (!unify(left, right)) return never;
  for (final (key, value) in assignedCompounds) {
    if (!scan(key, value)) return never;
  }
  if (readers.isEmpty) return (_GroundEquality.equal, const <HeapCell>{});
  if (occurs()) return never;
  return (_GroundEquality.unifiable, readers.values.toSet());
}

/// [guard], `=?=` or `=?\=`, on [left] and [right], as [_decideGroundEquality]
/// decides them: success, failure, or, where neither, as [_undecidedMember]
/// decides the member on their unbound readers --- suspension on the goal's,
/// which it adds to the clause's suspension set Si, the next member tried
/// ([_guardUndecided]), or failure.
GuardResult _groundEqualityGuard(
    String guard, Object? left, Object? right, RunnerContext cx) {
  final (decision, readers) = _decideGroundEquality(left, right, cx);
  final negated = guard == '=?\\=';
  switch (decision) {
    case _GroundEquality.equal:
      return negated ? GuardResult.failure : GuardResult.success;
    case _GroundEquality.never:
      return negated ? GuardResult.success : GuardResult.failure;
    case _GroundEquality.unifiable:
      return _undecidedMember(cx, readers, negated: negated);
  }
}


/// Helper class to represent argument information
class _ArgInfo {
  final HeapCell? writerId;
  final HeapCell? readerId;

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

/// A guard argument's structure being built (before commit) where its last
/// position has just been filled: placed where it goes --- in its parent, a
/// structure built for the same argument and waiting on
/// [RunnerContext.parentStack], whose next position it fills, or, the
/// outermost, in the guard's argument slot --- and each parent completed in
/// turn that it fills.  The structures are terms held for the guard call
/// alone, nothing bound on the heap, as in the body each nested structure is
/// placed in its parent ([OpExecutors.execPutStructure]).
void _completeGuardStructure(RunnerContext cx) {
  var struct = cx.currentStructure as StructTerm;
  while (cx.S >= struct.args.length) {
    if (cx.parentStack.isEmpty) {
      cx.argSlots[cx.guardArgSlot!] = struct;
      cx.currentStructure = null;
      cx.mode = UnifyMode.read;
      cx.S = 0;
      cx.guardArgSlot = null;
      return;
    }
    final parent = cx.parentStack.removeLast();
    final parentStruct = parent.structure as StructTerm;
    parentStruct.args[parent.s] = struct;
    cx.currentStructure = parentStruct;
    cx.S = parent.s + 1;
    cx.mode = parent.mode;
    struct = parentStruct;
  }
}

/// PC-agnostic opcode semantics, shared by the object loop (`runWithStatus`)
/// and the direct byte loop (`engine_v2/interp.dart`). Each method holds one
/// opcode's semantics ONCE and returns a [StepOutcome] the caller maps to its
/// own PC world (instruction index vs byte offset). It lives in this library so
/// it can reach the private clause/structure state types; cx-only helpers
/// migrate here arm by arm (B3a). PC/clause routing (`_findNextClauseTry`,
/// `_softFailToNextClause`) stays in the drivers.
///
/// Extraction is incremental and behaviour-identical: each converted arm keeps
/// the object loop's outcome unchanged. See `GLP-bc/docs/bytecode-exec-design.md`.
mixin OpExecutors {
  /// `clause_try` (0x01): reset clause-local state for a fresh clause attempt.
  StepOutcome execClauseTry(RunnerContext cx) {
    cx.clearClause();
    return StepOutcome.advance;
  }

  /// `nop` (0x07): no operation.
  StepOutcome execNop() => StepOutcome.advance;

  /// `halt` (0x06): terminate the goal.
  StepOutcome execHalt() => StepOutcome.halt;

  /// `proceed` (0x05): the clause body has been launched (or the goal is a
  /// fact); fire the reduction trace callback, then terminate this goal run.
  StepOutcome execProceed(RunnerContext cx) {
    cx.reduced = true;
    if (cx.tracing) {
      final body = cx.spawnedGoals.isEmpty ? 'true' : cx.spawnedGoals.join(', ');
      cx.onReduction!(cx.goalId, cx.reformatHead(), body);
    }
    return StepOutcome.proceed;
  }

  /// `otherwise` (0x46): succeeds only if all previous clauses definitely
  /// failed; if any suspended (U non-empty) this clause suspends too.
  StepOutcome execOtherwise(RunnerContext cx) =>
      cx.U.isNotEmpty ? StepOutcome.nextClause : StepOutcome.advance;

  /// `push` (0x24): save the structure-traversal state into a clause register.
  StepOutcome execPush(RunnerContext cx, int regIndex) {
    cx.clauseVars[regIndex] =
        _StructureState(cx.S, cx.mode, cx.currentStructure);
    return StepOutcome.advance;
  }

  /// `pop` (0x25): store the built nested structure into the register, then
  /// restore the saved parent traversal state (FCP AM semantics).
  StepOutcome execPop(RunnerContext cx, int regIndex) {
    final state = cx.clauseVars[regIndex] as _StructureState;
    cx.clauseVars[regIndex] = cx.currentStructure;
    cx.S = state.S;
    cx.mode = state.mode;
    cx.currentStructure = state.currentStructure;
    return StepOutcome.advance;
  }

  /// `put_nil` (0x32): in BODY, place a fresh variable bound to `[]` in argSlot.
  StepOutcome execPutNil(RunnerContext cx, int argSlot) {
    if (cx.inBody) {
      final (writerAddr, readerAddr) = cx.rt.heap.allocateVariable();
      cx.rt.heap.bindWriterConst(writerAddr, nil);
      cx.argSlots[argSlot] = VarRef(readerAddr);
    }
    return StepOutcome.advance;
  }

  /// `put_bound_const` (0x39): place a fresh variable bound to [value] in argSlot
  /// (passing a constant as an argument).
  StepOutcome execPutBoundConst(RunnerContext cx, Object? value, int argSlot) {
    final (writerAddr, readerAddr) = cx.rt.heap.allocateVariable();
    cx.rt.heap.bindWriterConst(writerAddr, value);
    cx.argSlots[argSlot] = VarRef(readerAddr);
    return StepOutcome.advance;
  }

  /// `put_bound_nil` (0x3A): place a fresh variable bound to `[]` in argSlot.
  StepOutcome execPutBoundNil(RunnerContext cx, int argSlot) {
    final (writerAddr, readerAddr) = cx.rt.heap.allocateVariable();
    cx.rt.heap.bindWriterConst(writerAddr, nil);
    cx.argSlots[argSlot] = VarRef(readerAddr);
    return StepOutcome.advance;
  }

  /// `put_list` (0x33): in BODY, begin building a `[H|T]` structure into argSlot's
  /// writer; subsequent Set* instructions fill the two positions.
  ///
  /// The structure is a list cell, `'.'/2`: "Lists are structures: a cell is
  /// the structure '.'/2; the empty list is the constant nil" (IGLP
  /// code-format-fragment.tex, "Terms"; GLP #3 Cowork, 2026-10-02 20:58 UTC:
  /// "yes, put_list builds '.'").  Until 2026-10-02 it built a `'[|]'` cell,
  /// which no head matches as a list.  No compiler of this tree emits
  /// put_list, a list in a body compiling to put_structure `'.'/2`; an
  /// artefact may carry it.
  StepOutcome execPutList(RunnerContext cx, int argSlot) {
    if (cx.inBody) {
      final arg = cx.env.arg(argSlot);
      final targetWriterAddr =
          (arg is VarRef && cx.rt.heap.isWriter(arg.addr)) ? arg.addr : null;
      if (targetWriterAddr == null) {
        // The compiler places a writer in the slot a body list is built into;
        // a slot without one is a fault of the compiled code, not of the
        // program run.  Until 2026-10-02 it printed a warning and went on.
        throw StateError('put_list: argument slot $argSlot holds no writer');
      }
      cx.clauseVars[-1] = targetWriterAddr; // -1 marks structure binding target
      final structArgs = List<Term>.filled(2, ConstTerm(null));
      cx.currentStructure = StructTerm('.', structArgs);
      cx.S = 0;
      cx.mode = UnifyMode.write;
    }
    return StepOutcome.advance;
  }

  /// `allocate` (0x37): push an environment frame of [slots] permanent vars.
  /// [nextPc] is the continuation address (instruction index in the object
  /// loop, byte offset in the byte loop).
  StepOutcome execAllocate(RunnerContext cx, int slots, int nextPc) {
    if (!cx.inBody) {
      throw StateError('Allocate must be in BODY phase (after commit)');
    }
    cx.E = EnvironmentFrame(
      parent: cx.E,
      continuationPointer: cx.CP ?? nextPc,
      size: slots,
    );
    cx.CP = nextPc;
    return StepOutcome.advance;
  }

  /// `deallocate` (0x38): pop the current environment frame.
  StepOutcome execDeallocate(RunnerContext cx) {
    if (cx.E == null) {
      throw StateError('Deallocate with no environment frame');
    }
    final frame = cx.E!;
    cx.CP = frame.continuationPointer;
    cx.E = frame.parent;
    return StepOutcome.advance;
  }

  /// `unify_void` (0x22): skip (READ) or create a fresh unbound writer (WRITE)
  /// at [count] structure positions.
  ///
  /// READ passes over what the goal holds at each position but a goal writer,
  /// which fails: `_` is a head writer, and a goal writer against a head
  /// writer fails ([_isGoalWriter]).  It passed over a goal writer too until
  /// 2026-10-02, and `s(f(_))` took `s(f(W))`.
  ///
  /// An anonymous variable is a fresh writer with no paired reader (TGLP
  /// typed-glp.tex, "Anonymous variables"), so WRITE puts a fresh writer in
  /// the slot, as `glp_engine.dart`'s `_anonymousWriter` does for `_` in a
  /// goal argument.  This is `_`'s instruction alone: a head `_?`, "an output
  /// the clause never produces", is a head reader of a variable of its own and
  /// compiles to `unify_variable` in reader mode, which fails on a goal term
  /// or reader and in WRITE places a reader (codegen, 2026-10-02).  It used
  /// to leave the slot `null`, which `_convertTentativeToStruct` turned into
  /// `ConstTerm(null)`: that closes a stream the clause left open, and the
  /// payload serializer refuses it across a link.
  ///
  /// Each position is one occurrence, so each gets a clause-variable index of
  /// its own that nothing else in the clause names, and the placement is
  /// `unify_variable`'s in writer mode --- head (`_TentativeStruct`) and body
  /// (`StructTerm`) alike, the body arm completing the structure and unwinding
  /// the parent stack when the last position is filled. The body arm used to
  /// do nothing at all and left `S` where it was, so a body structure holding
  /// a `_` never completed.
  StepOutcome execUnifyVoid(RunnerContext cx, int count) {
    if (cx.mode != UnifyMode.write) {
      final struct = cx.currentStructure;
      if (struct is StructTerm) {
        for (var i = 0; i < count && cx.S + i < struct.args.length; i++) {
          if (_isGoalWriter(cx, struct.args[cx.S + i])) {
            return StepOutcome.nextClause;
          }
        }
      }
      cx.S += count;
      return StepOutcome.advance;
    }
    for (var i = 0; i < count; i++) {
      // Re-read each time: completing a structure clears it, restores a parent
      // or switches back to READ mode.
      if (cx.mode != UnifyMode.write) break;
      final struct = cx.currentStructure;
      final int arity;
      if (struct is _TentativeStruct) {
        arity = struct.args.length;
      } else if (struct is StructTerm) {
        arity = struct.args.length;
      } else {
        break;
      }
      if (cx.S >= arity) break;
      execUnifyVariable(cx, cx.freshVoidVar(), false);
    }
    return StepOutcome.advance;
  }

  /// `no_more_clauses` (0x03): all clauses exhausted. If U is non-empty the goal
  /// suspends on those readers; otherwise it fails definitively. Performs the
  /// suspension side effect itself (loop-agnostic: uses cx.goalId/kappa/U).
  StepOutcome execNoMoreClauses(RunnerContext cx) {
    if (cx.U.isNotEmpty) {
      cx.rt.suspendGoalFCP(goalId: cx.goalId, kappa: cx.kappa, readerVarIds: cx.U);
      cx.U.clear();
      cx.inBody = false;
      return StepOutcome.suspended;
    }
    cx.inBody = false;
    return StepOutcome.failed;
  }

  /// `put_constant` (0x31): place a fresh variable bound to [value] in argSlot.
  StepOutcome execPutConstant(RunnerContext cx, Object? value, int argSlot) {
    final (writerAddr, readerAddr) = cx.rt.heap.allocateVariable();
    cx.rt.heap.bindWriterConst(writerAddr, value);
    cx.argSlots[argSlot] = VarRef(readerAddr);
    return StepOutcome.advance;
  }

  /// `put_structure` (0x34): begin building a `functor/arity` structure. In BODY
  /// it allocates a writer for the structure (nesting pushes the parent onto
  /// `parentStack`); pre-commit (guard-arg building) it builds without heap
  /// allocation into `guardArgSlot`. Set*/Unify* fill the positions.
  ///
  /// A guard's argument is built as a body's is --- a structure nested in it
  /// pushes its parent, is filled by the set_* and unify_* instructions that
  /// follow, and is placed in the parent when complete --- into a tentative
  /// structure: a term held for the guard call alone, nothing bound on the
  /// heap ([_completeGuardStructure]; IGLP Implementation Notes, "Clause
  /// try": "The tentative substitution reaches the heap only at commit").  It
  /// is nested where a guard argument is being built ([RunnerContext
  /// .guardArgSlot] set), the structure state the head left behind being no
  /// parent of it.  Until 2026-10-02 a nested structure overwrote its parent,
  /// and the set_* instructions filling it acted in the body alone, so
  /// `Z? =?= g(f(c))`, `X? + Y? * 2 > 3` and `X? =?= [a, b]` were decided on
  /// a term never completed (GLP #3 Cowork, 2026-10-02 17:12 UTC, S1).
  StepOutcome execPutStructure(
      RunnerContext cx, String functor, int arity, int argSlot) {
    if (cx.inBody) {
      final (writerAddr, _) = cx.rt.heap.allocateVariable();
      if (argSlot == -1 || cx.currentStructure != null) {
        cx.parentStack.add(_ParentContext(
          structure: cx.currentStructure,
          s: cx.S,
          mode: cx.mode,
          writerId: cx.clauseVars[-1],
        ));
      }
      cx.clauseVars[-1] = writerAddr;
      // Top-level argument of the body goal (place the completed structure into
      // its argument register at completion) vs a nested sub-structure held in a
      // temp register. Distinguish by nesting — the parent was pushed above iff
      // currentStructure was non-null — not by `argSlot < 10`, which capped body
      // arguments at 10 and misdirected the 11th+ compound argument into a temp
      // register (arity-dispatch bug, body path).
      if (argSlot >= 0 && cx.currentStructure == null) {
        cx.clauseVars[-2] = argSlot; // top-level body arg: place into argSlots when complete
      } else {
        cx.clauseVars[argSlot] = VarRef(writerAddr); // nested sub-structure: temp register
      }
      cx.currentStructure =
          StructTerm(functor, List<Term>.filled(arity, ConstTerm(null)));
      cx.S = 0;
      cx.mode = UnifyMode.write;
    } else {
      if (cx.guardArgSlot != null) {
        // Nested in the guard argument being built: its parent waits on the
        // stack for it.
        cx.parentStack.add(_ParentContext(
          structure: cx.currentStructure,
          s: cx.S,
          mode: cx.mode,
          writerId: null,
        ));
      } else {
        cx.guardArgSlot = argSlot;
      }
      cx.currentStructure =
          StructTerm(functor, List<Term>.filled(arity, ConstTerm(null)));
      cx.S = 0;
      cx.mode = UnifyMode.write;
    }
    return StepOutcome.advance;
  }

  /// `commit` (0x04): a clause whose suspension set Si is non-empty cannot
  /// commit --- its readers go to U and [StepOutcome.nextClause] is returned ---
  /// and otherwise the tentative structures are converted to terms, the writer
  /// bindings applied to the heap (waking suspended goals), and BODY entered.
  /// Si holds the readers the head match suspended on and those of the guard
  /// members that suspended, no member having failed ([_guardUndecided]).
  ///
  /// GLP-Spec appendix-term-matching.tex, Definition "Term Matching": a goal
  /// reader against a head term is "suspend on X1?", and "the writer mgu is the
  /// union of all writer assignments if no fail was encountered and the
  /// suspension set is empty".  Until 2026-10-02 a resolution pass first
  /// removed from Si every reader whose paired writer the same head's tentative
  /// substitution binds (IGLP Implementation Notes, "Clause try"), and the
  /// clause committed although the pattern at that reader was never matched
  /// against the value: `e1(Y?, Y)` reduced with `e1(f(2), f(1))`, and
  /// `e3(Y?, Y)` with `e3(2, 1)`.  The table is implemented as written here;
  /// whether the resolution pass is the language's is put to GLP (GLP
  /// 2026-10-01 23:58 UTC item 4, the open edge).
  StepOutcome execCommit(RunnerContext cx) {
    if (cx.Si.isNotEmpty) {
      cx.U.addAll(cx.Si);
      cx.Si.clear();
      return StepOutcome.nextClause;
    }

    // Convert tentative structures to real Terms before committing.
    final convertedSigmaHat = <HeapCell, Object?>{};
    for (final entry in cx.sigmaHat.entries) {
      final writerAddr = entry.key;
      final value = entry.value;
      if (value is _TentativeStruct) {
        final termArgs = <Term>[];
        for (final arg in value.args) {
          if (arg is _ClauseVar) {
            final resolved = cx.clauseVars[arg.varIndex];
            if (resolved is VarRef) {
              final isResolvedWriter = cx.rt.heap.isWriter(resolved.addr);
              if (arg.isWriter && isResolvedWriter) {
                termArgs.add(resolved);
              } else if (arg.isWriter && !isResolvedWriter) {
                final wid = cx.rt.heap.tryWriterForReader(resolved.addr);
                termArgs.add(wid != null ? VarRef(wid) : resolved);
              } else if (!arg.isWriter && !isResolvedWriter) {
                termArgs.add(resolved);
              } else {
                termArgs.add(VarRef(cx.rt.heap.pairedReaderAddr(resolved.addr)));
              }
            } else if (resolved is Term) {
              termArgs.add(resolved);
            } else {
              final (freshWriterAddr, freshReaderAddr) =
                  cx.rt.heap.allocateVariable();
              cx.clauseVars[arg.varIndex] =
                  VarRef(arg.isWriter ? freshWriterAddr : freshReaderAddr);
              termArgs.add(
                  VarRef(arg.isWriter ? freshWriterAddr : freshReaderAddr));
            }
          } else if (arg is _TentativeStruct) {
            termArgs.add(_convertTentativeToStruct(arg, cx));
          } else if (arg == null) {
            termArgs.add(ConstTerm(null));
          } else if (arg is Term) {
            termArgs.add(arg);
          } else {
            termArgs.add(ConstTerm(arg));
          }
        }
        convertedSigmaHat[writerAddr] = StructTerm(value.functor, termArgs);
      } else {
        convertedSigmaHat[writerAddr] = value;
      }
    }

    // Enforce WxW: writer→writer bindings are prohibited.
    for (final entry in convertedSigmaHat.entries) {
      final value = entry.value;
      if (value is VarRef && cx.rt.heap.isWriter(value.addr)) {
        throw StateError(
            'WxW violation in commit: W${entry.key} → W${value.addr} (both unbound writers)');
      }
    }

    final acts = CommitOps.applySigmaHatFCP(
      heap: cx.rt.heap,
      sigmaHat: convertedSigmaHat,
    );
    for (final a in acts) {
      cx.rt.gq.enqueue(a);
    }
    cx.sigmaHat.clear();
    cx.argSlots.clear();
    cx.currentStructure = null;
    cx.S = 0;
    cx.mode = UnifyMode.read;
    cx.parentStack.clear();
    cx.inBody = true;
    return StepOutcome.advance;
  }

  /// `ground` (0x41): three-valued, on what X? stands for
  /// ([_guardReaderOperand]), as the generic guard call decides a term
  /// ([_groundGuard]).  ground(X?): ground→advance; an unbound writer or a
  /// mutual reference→fail (nextClause); unbound readers and neither→undecided
  /// on them ([_guardUndecided]: suspended on the goal's, the next member
  /// tried, or failed on one the clause alone holds).
  StepOutcome execGround(RunnerContext cx, int varIndex) {
    // An unknown variable ([RunnerContext.unknownVars]) stands for any term,
    // some making the guard succeed and some not: undecided, passed by.
    if (cx.isUnknown(varIndex)) return StepOutcome.advance;
    final value = cx.clauseVars[varIndex];
    if (value == null) return StepOutcome.nextClause; // missing var → fail
    final vars = _termVariables(cx, _guardReaderOperand(cx, value));
    // A mutual reference is not ground (TGLP typed-glp.tex): until 2026-10-02
    // the instruction passed it by as a constant, and ground(M?) succeeded
    // where M? =?= M? fails.
    if (vars.writer || vars.mutualRef) return StepOutcome.nextClause; // fail
    if (vars.readers.isNotEmpty) return _guardUndecided(cx, vars.readers);
    return StepOutcome.advance; // ground → succeed
  }

  /// `known` (0x42): three-valued, on what X? stands for
  /// ([_guardReaderOperand]).  known(X?): bound→advance; unbound
  /// reader→undecided ([_guardUndecided]); unbound writer→fail. Unlike ground,
  /// only X itself is inspected, not its sub-terms.
  StepOutcome execKnown(RunnerContext cx, int varIndex) {
    // An unknown variable ([RunnerContext.unknownVars]) stands for any term,
    // some making the guard succeed and some not: undecided, passed by.
    if (cx.isUnknown(varIndex)) return StepOutcome.advance;
    final raw = cx.clauseVars[varIndex];
    if (raw == null) return StepOutcome.nextClause; // missing var → fail
    final value = _guardReaderOperand(cx, raw);

    // An unbound writer is neither known nor waited on: it fails below.
    bool isKnown = false;
    HeapCell? unboundReader;

    if (value is HeapCell) {
      if (cx.sigmaHat.containsKey(value)) {
        isKnown = true;
      } else if (cx.rt.heap.isWriter(value)) {
        if (cx.rt.heap.isFullyBound(value)) {
          isKnown = true;
        }
      } else {
        final writerAddr = cx.rt.heap.tryWriterForReader(value);
        if (writerAddr != null && cx.sigmaHat.containsKey(writerAddr)) {
          isKnown = true;
        } else if (cx.rt.heap.isReaderBound(value)) {
          isKnown = true;
        } else {
          unboundReader = value;
        }
      }
    } else if (value is VarRef && cx.rt.heap.isWriter(value.addr)) {
      if (cx.sigmaHat.containsKey(value.addr)) {
        isKnown = true;
      } else if (cx.rt.heap.isFullyBound(value.addr)) {
        isKnown = true;
      }
    } else if (value is VarRef && cx.rt.heap.isReader(value.addr)) {
      final readerAddr = value.addr;
      if (cx.sigmaHat.containsKey(readerAddr)) {
        isKnown = true;
      } else {
        final writerAddr = cx.rt.heap.tryWriterForReader(readerAddr);
        if (writerAddr != null && cx.sigmaHat.containsKey(writerAddr)) {
          isKnown = true;
        } else if (cx.rt.heap.isReaderBound(readerAddr)) {
          isKnown = true;
        } else {
          unboundReader = readerAddr;
        }
      }
    } else {
      isKnown = true; // constant or structure
    }

    if (isKnown) return StepOutcome.advance;
    if (unboundReader != null) {
      return _guardUndecided(cx, [unboundReader]);
    }
    return StepOutcome.nextClause; // unbound writer → fail
  }

  /// `no_readers` (0x44): the unbound readers of what X? stands for
  /// ([_guardReaderOperand], [_termVariables]).  no_readers(X?): none→advance;
  /// some→undecided on them ([_guardUndecided]: suspended on the goal's, or
  /// failed on one the clause alone holds).  Missing var counts as no readers.
  StepOutcome execNoReaders(RunnerContext cx, int varIndex) {
    // An unknown variable ([RunnerContext.unknownVars]) stands for any term,
    // some making the guard succeed and some not: undecided, passed by.
    if (cx.isUnknown(varIndex)) return StepOutcome.advance;
    final value = cx.clauseVars[varIndex];
    if (value == null) return StepOutcome.advance;
    final readers =
        _termVariables(cx, _guardReaderOperand(cx, value)).readers;
    if (readers.isEmpty) return StepOutcome.advance;
    return _guardUndecided(cx, readers);
  }

  /// `ground_equal` (0x45): X? =?= Y?, decided as every ground equality guard
  /// is ([_decideGroundEquality]), on what X? and Y? stand for
  /// ([_guardReaderOperand]): it succeeds where both are ground and equal,
  /// fails where no readers substitution makes them so, and is otherwise
  /// undecided ([_undecidedMember]).  Until 2026-10-02 it read a fresh
  /// variable's writer, and he(f(X), Y, yes) :- X? =?= Y? | true failed
  /// he(W, b, R) on an unbound writer, where the generic guard call suspended
  /// on the reader.
  StepOutcome execGroundEqual(
      RunnerContext cx, int leftVarIndex, int rightVarIndex) {
    // An unknown variable ([RunnerContext.unknownVars]) is the variable that
    // stands for it, any term, which the decision assigns as it does a reader
    // and the member decision counts with the goal's ([_undecidedMember]).
    // Until 2026-10-02 the guard was passed by, undecided, and pu(P?, g(W), R)
    // waited on P? where no term makes X? =?= g(W) succeed.
    Object? operand(int varIndex) => cx.isUnknown(varIndex)
        ? _unknownPlaceholder(cx, varIndex, true)
        : cx.clauseVars[varIndex];
    final leftValue = operand(leftVarIndex);
    final rightValue = operand(rightVarIndex);
    if (leftValue == null || rightValue == null) return StepOutcome.nextClause;
    // A suspension's readers are in cx.Si ([_groundEqualityGuard]), and the
    // next member is tried as after a success ([_guardUndecided]).
    return _groundEqualityGuard('=?=', _guardReaderOperand(cx, leftValue),
                _guardReaderOperand(cx, rightValue), cx) ==
            GuardResult.failure
        ? StepOutcome.nextClause
        : StepOutcome.advance;
  }

  /// `unknown` (0x43): succeed iff the clause variable is currently unbound (no
  /// σ̂w tentative binding and not heap-bound). A dispatch test; never suspends.
  StepOutcome execUnknown(RunnerContext cx, int varIndex) {
    // An unknown variable ([RunnerContext.unknownVars]) stands for any term,
    // some making the guard succeed and some not: undecided, passed by.
    if (cx.isUnknown(varIndex)) return StepOutcome.advance;
    final term = cx.clauseVars[varIndex];
    if (term is VarRef) {
      if (cx.sigmaHat.containsKey(term.addr)) return StepOutcome.nextClause;
      if (cx.rt.heap.isBound(term.addr)) return StepOutcome.nextClause;
      return StepOutcome.advance; // unbound → unknown → succeed
    }
    return StepOutcome.nextClause; // non-variable is known
  }

  /// `guard` (0x40): a generic guard-predicate call. Gather the [arity] args from
  /// argSlots/clauseVars, dereferencing and tracking unbound readers; if any are
  /// unbound (except for `unknown`, `=?=` and `=?\=`), the guard is undecided
  /// on them ([_guardUndecided]). Otherwise evaluate via the runtime guard
  /// table. success→advance; suspension→advance, its readers in Si;
  /// failure→nextClause.
  StepOutcome execGuard(RunnerContext cx, String predicateName, int arity) {
    // A generic guard call of `otherwise` --- hand-assembled bytecode, or an
    // artefact whose encoder did not use 0x46 --- takes 0x46's rule and no
    // other: it succeeds if all previous clauses for this procedure fail
    // (GLP-Spec appendix-guards.tex), so it waits while any of them suspends.
    if (predicateName == 'otherwise' && arity == 0) {
      return execOtherwise(cx);
    }
    // An argument holding an unknown variable ([RunnerContext.unknownVars])
    // holds the variable that stands for it, any term: the guard's own
    // decision meets it ([_undecidedMember]), and fails where no term makes
    // the guard succeed.  Until 2026-10-02 such a guard was passed by,
    // undecided, whatever else stood in its arguments.
    final args = <Object?>[];
    final unboundReaders = <HeapCell>{};
    for (var i = 0; i < arity; i++) {
      Object? argValue;
      final arg = cx.argSlots[i];
      if (arg != null) {
        argValue = arg;
      } else if (cx.clauseVars.containsKey(i)) {
        argValue = cx.clauseVars[i];
      } else {
        argValue = null;
      }
      if (argValue != null) {
        final (derefValue, readers) =
            _dereferenceWithTracking(argValue, cx);
        args.add(derefValue);
        unboundReaders.addAll(readers);
      } else {
        args.add(null);
      }
    }

    // The ground equality guards decide their unbound readers themselves, as
    // ground_equal (0x45) does ([_groundEqualityGuard]): a clash, or an unbound
    // writer, decides them whatever readers stand beside it.  So do the
    // arithmetic comparisons, which evaluate both operands before they wait on
    // one: `X? > 1 / 0` fails, its right operand having no value whatever X?
    // becomes, where it waited on X? until 2026-10-02 (GLP #3 Cowork,
    // 2026-10-02 17:12 UTC, S3).  So does @<, which fails on an argument
    // that is no constant whatever readers stand beside it: f(a) @< X? has no
    // instance that succeeds, where it waited on X? until 2026-10-09.
    if (unboundReaders.isNotEmpty &&
        predicateName != 'unknown' &&
        predicateName != '=?=' &&
        predicateName != '=?\\=' &&
        predicateName != '@<' &&
        !_arithmeticComparisons.contains(predicateName)) {
      return _guardUndecided(cx, unboundReaders);
    }

    // A suspension's readers are already in cx.Si (_evaluateGuard added them),
    // and the next member is tried as after a success ([_guardUndecided]).
    return _evaluateGuard(predicateName, args, cx) == GuardResult.failure
        ? StepOutcome.nextClause
        : StepOutcome.advance;
  }

  /// `head_nil` (0x11): match `[]` against the arg (or a clause var when
  /// argSlot ≥ 10). Two-phase: an unbound writer is tentatively bound to nil in
  /// σ̂w (advance); an unbound reader is added to Si (advance — resolved at
  /// commit); a bound non-nil / structure mismatches (nextClause).
  StepOutcome execHeadNil(RunnerContext cx, int argSlot) {
    // A clause/temp register (not a top-level argument slot) iff it is not one of
    // the current goal's argument positions (slots 0..arity-1). Replaces a
    // hard-coded `argSlot >= 10` that capped argument registers at 10 and misread
    // the 11th+ argument of a high-arity goal as a temp register (arity-dispatch
    // bug; codegen keeps temp indices above the arity so they never alias).
    final bool isClauseVar = !cx.env.argBySlot.containsKey(argSlot);
    final arg = isClauseVar ? null : _getArg(cx, argSlot);

    if (isClauseVar) {
      final clauseVarValue = cx.clauseVars[argSlot];
      if (clauseVarValue == null) return StepOutcome.nextClause;
      if (clauseVarValue is ConstTerm) {
        return clauseVarValue.value == nil
            ? StepOutcome.advance
            : StepOutcome.nextClause;
      } else if (clauseVarValue is StructTerm) {
        return StepOutcome.nextClause;
      } else if (clauseVarValue is VarRef) {
        final addr = clauseVarValue.addr;
        if (cx.rt.heap.isWriter(addr)) {
          if (cx.rt.heap.isFullyBound(addr)) {
            final value = cx.rt.heap.getValue(addr);
            return (value is ConstTerm && value.value == nil)
                ? StepOutcome.advance
                : StepOutcome.nextClause;
          } else {
            cx.sigmaHat[addr] = ConstTerm(nil);
            return StepOutcome.advance;
          }
        } else {
          if (cx.rt.heap.isReaderBound(addr)) {
            final value = cx.rt.heap.getReaderValue(addr);
            return (value is ConstTerm && value.value == nil)
                ? StepOutcome.advance
                : StepOutcome.nextClause;
          } else {
            cx.Si.add(_finalUnboundVar(cx, addr));
            return StepOutcome.advance;
          }
        }
      } else if (clauseVarValue is HeapCell) {
        final writerAddr = clauseVarValue;
        if (cx.rt.heap.isFullyBound(writerAddr)) {
          final value = cx.rt.heap.getValue(writerAddr);
          return (value is ConstTerm && value.value == nil)
              ? StepOutcome.advance
              : StepOutcome.nextClause;
        } else {
          cx.sigmaHat[writerAddr] = ConstTerm(nil);
          return StepOutcome.advance;
        }
      }
      return StepOutcome.nextClause; // unexpected clauseVar type
    }

    // Regular argument handling
    if (arg == null) return StepOutcome.advance;
    if (arg is VarRef && cx.rt.heap.isValue(arg.addr)) {
      final value = cx.rt.heap.getValue(arg.addr);
      return (value is ConstTerm && value.value == nil)
          ? StepOutcome.advance
          : StepOutcome.nextClause;
    }
    if (arg is VarRef && cx.rt.heap.isWriter(arg.addr)) {
      if (cx.rt.heap.isFullyBound(arg.addr)) {
        final value = cx.rt.heap.getValue(arg.addr);
        if (value is ConstTerm && value.value != nil) {
          return StepOutcome.nextClause;
        } else if (value is StructTerm) {
          return StepOutcome.nextClause;
        }
      } else {
        cx.sigmaHat[arg.addr] = ConstTerm(nil);
      }
    } else if (arg is VarRef && cx.rt.heap.isReader(arg.addr)) {
      final bound = cx.rt.heap.isReaderBound(arg.addr);
      final value = bound ? cx.rt.heap.getReaderValue(arg.addr) : null;
      if (!bound) {
        cx.Si.add(_finalUnboundVar(cx, arg.addr));
        return StepOutcome.advance;
      } else {
        if (value is ConstTerm && value.value == nil) {
          // match
        } else if (value is StructTerm) {
          return StepOutcome.nextClause;
        } else {
          return StepOutcome.nextClause;
        }
      }
    }
    return StepOutcome.advance;
  }

  /// `head_variable` (read/write): in WRITE mode place the clause var (new
  /// placeholder or existing binding) into the structure being built; in READ
  /// mode extract the value at S and unify with the clause var (a writer: a
  /// goal writer fails, and otherwise a first occurrence stores and a later
  /// occurrence must match; a reader: an unbound goal writer is assigned it,
  /// and anything else fails).  An unknown variable
  /// ([RunnerContext.unknownVars]) is given no value by any of these.
  StepOutcome execHeadVariable(RunnerContext cx, int varIndex, bool isReader) {
    if (cx.mode == UnifyMode.write) {
      if (cx.currentStructure is _TentativeStruct) {
        final struct = cx.currentStructure as _TentativeStruct;
        final existingValue = cx.clauseVars[varIndex];
        if (cx.isUnknown(varIndex)) {
          // An unknown variable placed in a structure built for a goal
          // writer: it stays unknown, the slot holding the variable that
          // stands for it, which the clause does not keep --- the clause
          // cannot commit.
          struct.args[cx.S] = _unknownPlaceholder(cx, varIndex, isReader);
        } else if (existingValue != null) {
          if (isReader && existingValue is HeapCell) {
            struct.args[cx.S] =
                VarRef(cx.rt.heap.pairedReaderAddr(existingValue));
          } else {
            struct.args[cx.S] = existingValue;
          }
        } else {
          final placeholder = _ClauseVar(varIndex, isWriter: !isReader);
          struct.args[cx.S] = placeholder;
          cx.clauseVars[varIndex] = placeholder;
        }
        cx.S++;
      }
    } else {
      if (cx.currentStructure is _SkippedSubterm) {
        // Under a suspended goal reader: nothing to match.  A writer
        // occurrence of a variable with no value yet makes it unknown
        // ([RunnerContext.unknownVars]); a reader occurrence gives no value
        // and leaves the variable as it is.
        if (!isReader && _hasNoValue(cx, varIndex)) {
          cx.unknownVars.add(varIndex);
        }
        return StepOutcome.advance;
      }
      if (cx.currentStructure is StructTerm) {
        final struct = cx.currentStructure as StructTerm;
        if (cx.S < struct.args.length) {
          final value = struct.args[cx.S];
          final existingValue = cx.clauseVars[varIndex];
          if (isReader) {
            // A head reader at this subterm: an unbound goal writer is
            // assigned it; a goal reader and a goal term fail
            // (appendix-term-matching.tex, column "Reader X2?"; see
            // _isUnboundWriterCell), as unify_variable has it.
            if (value is! VarRef || !_isUnboundWriterCell(cx, value.addr)) {
              return StepOutcome.nextClause;
            }
            if (cx.isUnknown(varIndex)) {
              // The goal writer would be assigned the reader of an unknown
              // variable: no fail, and nothing the clause keeps.
            } else if (existingValue == null) {
              cx.clauseVars[varIndex] = value.addr;
            } else if (existingValue is VarRef) {
              cx.sigmaHat[value.addr] = cx.rt.heap.isWriter(existingValue.addr)
                  ? VarRef(cx.rt.heap.pairedReaderAddr(existingValue.addr))
                  : existingValue;
            } else if (existingValue is HeapCell) {
              cx.sigmaHat[value.addr] =
                  VarRef(cx.rt.heap.pairedReaderAddr(existingValue));
            } else if (_isGroundValue(existingValue)) {
              cx.sigmaHat[value.addr] = existingValue;
            } else {
              // A placeholder the WRITE arm above left in a structure built
              // for a goal writer: the variable takes its cell now, and the
              // placeholder resolves to it at commit.
              final (writerAddr, readerAddr) = cx.rt.heap.allocateVariable();
              cx.clauseVars[varIndex] = VarRef(writerAddr);
              cx.sigmaHat[value.addr] = VarRef(readerAddr);
            }
          } else if (_isGoalWriter(cx, value)) {
            // A head writer at this subterm: a goal writer fails, whatever
            // the variable ([_isGoalWriter]), as unify_variable has it.
            return StepOutcome.nextClause;
          } else if (cx.isUnknown(varIndex)) {
            // A later writer occurrence of an unknown variable: what it meets
            // is matched against a value not yet there, so it is undecided,
            // and gives the variable no value.
          } else if (existingValue != null) {
            if (existingValue != value) {
              return StepOutcome.nextClause;
            }
          } else {
            cx.clauseVars[varIndex] = value;
          }
          cx.S++;
        } else {
          return StepOutcome.nextClause;
        }
      } else {
        return StepOutcome.nextClause;
      }
    }
    return StepOutcome.advance;
  }

  /// `head_constant` (match arg against a constant). Writer: bind tentatively
  /// in σ̂w if unbound (else compare deref); reader: suspend (Si) if unbound,
  /// else compare; mismatch → next clause.
  StepOutcome execHeadConstant(RunnerContext cx, Object? opValue, int argSlot) {
    final arg = _getArg(cx, argSlot);
    if (arg == null) return StepOutcome.advance;

    if (arg is VarRef && cx.rt.heap.isWriter(arg.addr)) {
      if (cx.rt.heap.isWriterBound(arg.addr)) {
        var value = cx.rt.heap.valueOfWriter(arg.addr);
        while (value is VarRef) {
          if (cx.rt.heap.isReader(value.addr)) {
            if (cx.rt.heap.isReaderBound(value.addr)) {
              final readerValue = cx.rt.heap.getReaderValue(value.addr);
              if (readerValue != null) {
                value = readerValue;
              } else {
                break;
              }
            } else {
              break;
            }
          } else {
            if (cx.rt.heap.isWriterBound(value.addr)) {
              value = cx.rt.heap.valueOfWriter(value.addr);
            } else {
              break;
            }
          }
        }

        if (value is VarRef) {
          if (cx.rt.heap.isReader(value.addr)) {
            cx.Si.add(value.addr);
            return StepOutcome.advance;
          } else {
            cx.sigmaHat[arg.addr] = ConstTerm(opValue);
          }
        } else if (value is ConstTerm && value.value != opValue) {
          return StepOutcome.nextClause;
        } else if (value is StructTerm) {
          return StepOutcome.nextClause;
        }
      } else {
        cx.sigmaHat[arg.addr] = ConstTerm(opValue);
      }
    } else if (arg is VarRef && cx.rt.heap.isReader(arg.addr)) {
      final deref = cx.rt.heap.derefAddr(arg.addr);
      if (deref is VarRef) {
        cx.Si.add(_finalUnboundVar(cx, arg.addr));
        return StepOutcome.advance;
      } else if (deref is Term) {
        final value = deref;
        if (value is ConstTerm && value.value != opValue) {
          return StepOutcome.nextClause;
        } else if (value is StructTerm && opValue != null) {
          return StepOutcome.nextClause;
        } else if (value is StructTerm && opValue == null) {
          return StepOutcome.nextClause;
        }
      }
    }
    return StepOutcome.advance;
  }

  /// `head_structure` (match arg against a functor/arity). For a clause var or a
  /// goal arg: bound writer/reader matching the functor → READ mode over it;
  /// unbound writer → WRITE mode building a tentative struct in σ̂w; unbound
  /// reader → suspend (Si) and skip the pattern under it ([_skipped]);
  /// mismatch → next clause.
  StepOutcome execHeadStructure(
      RunnerContext cx, String functor, int arity, int argSlot) {
    // A clause/temp register (not a top-level argument slot) iff it is not one of
    // the current goal's argument positions (slots 0..arity-1). Replaces a
    // hard-coded `argSlot >= 10` that capped argument registers at 10 and misread
    // the 11th+ argument of a high-arity goal as a temp register (arity-dispatch
    // bug; codegen keeps temp indices above the arity so they never alias).
    final bool isClauseVar = !cx.env.argBySlot.containsKey(argSlot);
    final arg = isClauseVar ? null : _getArg(cx, argSlot);

    if (!isClauseVar && arg == null) {
      return StepOutcome.nextClause;
    }

    if (isClauseVar) {
      final clauseVarValue = cx.clauseVars[argSlot];
      if (clauseVarValue == null) {
        return StepOutcome.nextClause;
      }

      if (clauseVarValue is HeapCell) {
        final wid = clauseVarValue;
        if (cx.rt.heap.isWriterBound(wid)) {
          final value = cx.rt.heap.valueOfWriter(wid);
          if (value is StructTerm &&
              value.functor == functor &&
              value.args.length == arity) {
            cx.currentStructure = value;
            cx.mode = UnifyMode.read;
            cx.S = 0;
            return StepOutcome.advance;
          }
          return StepOutcome.nextClause;
        } else {
          final struct =
              _TentativeStruct(functor, arity, List.filled(arity, null));
          cx.sigmaHat[wid] = struct;
          cx.currentStructure = struct;
          cx.mode = UnifyMode.write;
          cx.S = 0;
          return StepOutcome.advance;
        }
      } else if (clauseVarValue is VarRef &&
          cx.rt.heap.isWriter(clauseVarValue.addr)) {
        final wid = clauseVarValue.addr;
        if (cx.rt.heap.isWriterBound(wid)) {
          final value = cx.rt.heap.valueOfWriter(wid);
          if (value is StructTerm &&
              value.functor == functor &&
              value.args.length == arity) {
            cx.currentStructure = value;
            cx.mode = UnifyMode.read;
            cx.S = 0;
            return StepOutcome.advance;
          }
          return StepOutcome.nextClause;
        } else {
          final struct =
              _TentativeStruct(functor, arity, List.filled(arity, null));
          cx.sigmaHat[wid] = struct;
          cx.currentStructure = struct;
          cx.mode = UnifyMode.write;
          cx.S = 0;
          return StepOutcome.advance;
        }
      } else if (clauseVarValue is VarRef &&
          cx.rt.heap.isReader(clauseVarValue.addr)) {
        final rid = clauseVarValue.addr;
        final bound = cx.rt.heap.isReaderBound(rid);
        if (!bound) {
          cx.Si.add(rid);
          _skipSubterm(cx);
          return StepOutcome.advance;
        }
        final rawValue = cx.rt.heap.getReaderValue(rid);
        if (rawValue == null) {
          return StepOutcome.nextClause;
        }
        final value = cx.rt.heap.dereference(rawValue);
        if (value is StructTerm &&
            value.functor == functor &&
            value.args.length == arity) {
          cx.currentStructure = value;
          cx.mode = UnifyMode.read;
          cx.S = 0;
          return StepOutcome.advance;
        } else {
          return StepOutcome.nextClause;
        }
      } else if (clauseVarValue is StructTerm) {
        if (clauseVarValue.functor == functor &&
            clauseVarValue.args.length == arity) {
          cx.currentStructure = clauseVarValue;
          cx.mode = UnifyMode.read;
          cx.S = 0;
          return StepOutcome.advance;
        } else {
          return StepOutcome.nextClause;
        }
      } else if (clauseVarValue is ConstTerm) {
        return StepOutcome.nextClause;
      }

      return StepOutcome.nextClause;
    }

    if (arg is VarRef && cx.rt.heap.isWriter(arg.addr)) {
      if (cx.rt.heap.isWriterBound(arg.addr)) {
        var value = cx.rt.heap.valueOfWriter(arg.addr);

        while (value is VarRef) {
          if (cx.rt.heap.isReader(value.addr)) {
            if (cx.rt.heap.isReaderBound(value.addr)) {
              final readerValue = cx.rt.heap.getReaderValue(value.addr);
              if (readerValue != null) {
                value = readerValue;
              } else {
                break;
              }
            } else {
              break;
            }
          } else {
            if (cx.rt.heap.isWriterBound(value.addr)) {
              value = cx.rt.heap.valueOfWriter(value.addr);
            } else {
              break;
            }
          }
        }

        if (value is VarRef) {
          if (cx.rt.heap.isReader(value.addr)) {
            cx.Si.add(value.addr);
            _skipSubterm(cx);
            return StepOutcome.advance;
          } else {
            final struct =
                _TentativeStruct(functor, arity, List.filled(arity, null));
            cx.sigmaHat[arg.addr] = struct;
            cx.currentStructure = struct;
            cx.mode = UnifyMode.write;
            cx.S = 0;
            return StepOutcome.advance;
          }
        } else if (value is StructTerm &&
            value.functor == functor &&
            value.args.length == arity) {
          cx.currentStructure = value;
          cx.mode = UnifyMode.read;
          cx.S = 0;
          return StepOutcome.advance;
        } else {
          return StepOutcome.nextClause;
        }
      }
      final struct = _TentativeStruct(functor, arity, List.filled(arity, null));
      cx.sigmaHat[arg.addr] = struct;
      cx.currentStructure = struct;
      cx.mode = UnifyMode.write;
      cx.S = 0;
      return StepOutcome.advance;
    }

    if (arg is VarRef && cx.rt.heap.isReader(arg.addr)) {
      if (!cx.rt.heap.isReaderBound(arg.addr)) {
        cx.Si.add(_finalUnboundVar(cx, arg.addr));
        _skipSubterm(cx);
        return StepOutcome.advance;
      }

      final rawValue = cx.rt.heap.getReaderValue(arg.addr);
      if (rawValue == null) {
        return StepOutcome.nextClause;
      }
      final value = cx.rt.heap.dereference(rawValue);
      if (value is StructTerm &&
          value.functor == functor &&
          value.args.length == arity) {
        cx.currentStructure = value;
        cx.mode = UnifyMode.read;
        cx.S = 0;
        return StepOutcome.advance;
      } else {
        return StepOutcome.nextClause;
      }
    }

    if (arg is VarRef && cx.rt.heap.isValue(arg.addr)) {
      final value = cx.rt.heap.getValue(arg.addr);
      if (value is StructTerm &&
          value.functor == functor &&
          value.args.length == arity) {
        cx.currentStructure = value;
        cx.mode = UnifyMode.read;
        cx.S = 0;
        return StepOutcome.advance;
      } else {
        return StepOutcome.nextClause;
      }
    }

    throw StateError(
        'HeadStructure: unexpected argument type ${arg.runtimeType}');
  }

  /// `unify_constant` (constant at the current S subterm). WRITE mode: place it
  /// into the structure under construction (binding the target writer when the
  /// struct completes). READ mode: match it against the subterm — writer binds
  /// tentatively, reader suspends (Si) if unbound, mismatch → next clause.
  StepOutcome execUnifyConstant(RunnerContext cx, Object? opValue) {
    if (cx.mode == UnifyMode.write) {
      if (cx.currentStructure is _TentativeStruct) {
        final struct = cx.currentStructure as _TentativeStruct;
        struct.args[cx.S] = opValue;
        cx.S++;

        if (cx.S >= struct.args.length) {
          final targetWriterId = cx.clauseVars[-1];
          if (targetWriterId is HeapCell) {
            final termArgs = <Term>[];
            for (final arg in struct.args) {
              if (arg is Term) {
                termArgs.add(arg);
              } else {
                termArgs.add(ConstTerm(arg));
              }
            }
            cx.rt.heap.bindWriterStruct(targetWriterId, struct.functor, termArgs);

            cx.currentStructure = null;
            cx.mode = UnifyMode.read;
            cx.S = 0;
            cx.clauseVars.remove(-1);
          }
        }
      } else if (cx.currentStructure is StructTerm) {
        final struct = cx.currentStructure as StructTerm;
        struct.args[cx.S] = opValue is Term ? opValue : ConstTerm(opValue);
        cx.S++;

        if (cx.S >= struct.args.length) {
          if (cx.guardArgSlot != null) {
            _completeGuardStructure(cx);
          } else {
            final targetWriterId = cx.clauseVars[-1];
            if (targetWriterId is HeapCell) {
              cx.rt.heap
                  .bindWriterStruct(targetWriterId, struct.functor, struct.args);

              final targetSlot = cx.clauseVars[-2];
              if (targetSlot is int && targetSlot >= 0) {
                cx.argSlots[targetSlot] =
                    VarRef(cx.rt.heap.pairedReaderAddr(targetWriterId));
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
      if (cx.currentStructure is StructTerm) {
        final struct = cx.currentStructure as StructTerm;
        if (cx.S < struct.args.length) {
          final value = struct.args[cx.S];

          if (value is ConstTerm && value.value == opValue) {
            cx.S++;
          } else if (value is VarRef && cx.rt.heap.isWriter(value.addr)) {
            final wid = value.addr;
            if (cx.rt.heap.isWriterBound(wid)) {
              final boundValue = cx.rt.heap.valueOfWriter(wid);
              if (boundValue is ConstTerm && boundValue.value == opValue) {
                cx.S++;
              } else {
                return StepOutcome.nextClause;
              }
            } else {
              cx.sigmaHat[wid] = ConstTerm(opValue);
              cx.S++;
            }
          } else if (value is VarRef && cx.rt.heap.isReader(value.addr)) {
            final rid = value.addr;
            if (cx.rt.heap.isReaderBound(rid)) {
              final boundValue = cx.rt.heap.getReaderValue(rid);
              if (boundValue is ConstTerm && boundValue.value == opValue) {
                cx.S++;
              } else {
                return StepOutcome.nextClause;
              }
            } else {
              cx.Si.add(rid);
              cx.S++;
            }
          } else {
            return StepOutcome.nextClause;
          }
        } else {
          return StepOutcome.nextClause;
        }
      } else {
        return StepOutcome.advance;
      }
    }
    return StepOutcome.advance;
  }

  /// `unify_variable` (variable at the current S subterm). WRITE mode places
  /// the clause var (fresh or existing, mode-adjusted) into the structure being
  /// built, completing/popping nested structures on the parent stack. READ mode
  /// unifies it with the subterm per the reader/writer match rules.  An unknown
  /// variable ([RunnerContext.unknownVars]) is given no value by any of these.
  StepOutcome execUnifyVariable(
      RunnerContext cx, int varIndex, bool isReaderMode) {

        if (cx.mode == UnifyMode.write) {
          // WRITE mode: Add variable to structure being built
          if (cx.currentStructure is _TentativeStruct) {
            // HEAD phase tentative structure
            final struct = cx.currentStructure as _TentativeStruct;
            final clauseVarValue = cx.clauseVars[varIndex];

            if (cx.isUnknown(varIndex)) {
              // An unknown variable placed in a structure built for a goal
              // writer: it stays unknown, the slot holding the variable that
              // stands for it, which the clause does not keep --- the clause
              // cannot commit.  It was given a fresh variable here as at a
              // first occurrence, and a guard over it then failed on an
              // unbound writer instead of being undecided (f1(same(To),
              // out(To?)) :- ground(To?)).
              struct.args[cx.S] =
                  _unknownPlaceholder(cx, varIndex, isReaderMode);
            } else if (clauseVarValue is VarRef) {
              // Subsequent use: clauseVarValue holds an addr
              final addr = clauseVarValue.addr;

              // Per spec v2.16.3: Check if VarRef points to ValueTag (ground value)
              if (cx.rt.heap.isValue(addr)) {
                // VarRef points to ground value - dereference and use
                final groundValue = cx.rt.heap.getValue(addr);
                if (groundValue != null) {
                  if (isReaderMode) {
                    // Reader mode with ground term: create fresh var, bind tentatively
                    final (writerAddr, readerAddr) = cx.rt.heap.allocateVariable();
                    cx.sigmaHat[writerAddr] = groundValue;
                    struct.args[cx.S] = VarRef(readerAddr);
                  } else {
                    // Writer mode: use ground term directly
                    struct.args[cx.S] = groundValue;
                  }
                } else {
                  struct.args[cx.S] = clauseVarValue;
                }
              } else if (isReaderMode && cx.rt.heap.isWriter(addr)) {
                // Per spec v3.2: use readerForWriter() instead of +1 arithmetic
                struct.args[cx.S] = VarRef(cx.rt.heap.pairedReaderAddr(addr));  // reader addr
              } else if (!isReaderMode && cx.rt.heap.isReader(addr)) {
                // Per spec v3.2: use tryWriterForReader() instead of -1 arithmetic
                struct.args[cx.S] = VarRef(cx.rt.heap.tryWriterForReader(addr)!);  // writer addr
              } else {
                struct.args[cx.S] = VarRef(addr);  // mode already matches
              }
            } else if (clauseVarValue is HeapCell) {
              // Bare writer addr - create VarRef with appropriate mode
              if (isReaderMode) {
                // Per spec v3.2: use readerForWriter() instead of +1 arithmetic
                struct.args[cx.S] = VarRef(cx.rt.heap.pairedReaderAddr(clauseVarValue));  // reader addr
              } else {
                struct.args[cx.S] = VarRef(clauseVarValue);  // writer addr
              }
            } else if (clauseVarValue is Term) {
              if (isReaderMode) {
                // Reader mode with ground term: create fresh var, bind tentatively
                final (writerAddr, readerAddr) = cx.rt.heap.allocateVariable();
                cx.sigmaHat[writerAddr] = clauseVarValue;
                struct.args[cx.S] = VarRef(readerAddr);
              } else {
                // Writer mode: use ground term directly
                struct.args[cx.S] = clauseVarValue;
              }
            } else if (clauseVarValue is _TentativeStruct) {
              // Nested tentative structure
              struct.args[cx.S] = clauseVarValue;
            } else if (clauseVarValue == null) {
              // First occurrence - allocate fresh variable
              final (writerAddr, readerAddr) = cx.rt.heap.allocateVariable();
              // Store WRITER in clauseVars (base variable)
              cx.clauseVars[varIndex] = VarRef(writerAddr);
              // Store with requested mode in structure
              struct.args[cx.S] = VarRef(isReaderMode ? readerAddr : writerAddr);
            } else {
              // Fallback: use _ClauseVar placeholder
              struct.args[cx.S] = _ClauseVar(varIndex, isWriter: !isReaderMode);
            }
            cx.S++;

          } else if (cx.currentStructure is StructTerm) {
            // BODY phase structure building
            final struct = cx.currentStructure as StructTerm;
            final clauseVarValue = cx.clauseVars[varIndex];

            if (!cx.inBody && cx.isUnknown(varIndex)) {
              // A guard argument's structure holding an unknown variable
              // ([RunnerContext.unknownVars]): the variable that stands for
              // it, any term, which the guard's decision meets
              // ([_undecidedMember]); the variable stays unknown.
              struct.args[cx.S] =
                  _unknownPlaceholder(cx, varIndex, isReaderMode);
            } else if (clauseVarValue is VarRef) {
              // Subsequent use: clauseVarValue holds an addr
              final addr = clauseVarValue.addr;

              // Per spec v2.16.3: Check if VarRef points to ValueTag (ground value)
              if (cx.rt.heap.isValue(addr)) {
                // VarRef points to ground value - dereference and use
                final groundValue = cx.rt.heap.getValue(addr);
                if (groundValue != null) {
                  if (isReaderMode) {
                    // Reader mode with ground term: create fresh var, bind it
                    final (writerAddr, readerAddr) = cx.rt.heap.allocateVariable();
                    cx.rt.heap.bindVariable(writerAddr, groundValue);
                    struct.args[cx.S] = VarRef(readerAddr);
                  } else {
                    // Writer mode: use ground term directly
                    struct.args[cx.S] = groundValue;
                  }
                } else {
                  struct.args[cx.S] = clauseVarValue;
                }
              } else if (isReaderMode && cx.rt.heap.isWriter(addr)) {
                // Per spec v3.2: use readerForWriter() instead of +1 arithmetic
                struct.args[cx.S] = VarRef(cx.rt.heap.pairedReaderAddr(addr));  // reader addr
              } else if (!isReaderMode && cx.rt.heap.isReader(addr)) {
                // Per spec v3.2: use tryWriterForReader() instead of -1 arithmetic
                struct.args[cx.S] = VarRef(cx.rt.heap.tryWriterForReader(addr)!);  // writer addr
              } else {
                struct.args[cx.S] = VarRef(addr);  // mode matches
              }
            } else if (clauseVarValue is HeapCell) {
              // Bare writer addr - create VarRef with requested mode
              if (isReaderMode) {
                // Per spec v3.2: use readerForWriter() instead of +1 arithmetic
                struct.args[cx.S] = VarRef(cx.rt.heap.pairedReaderAddr(clauseVarValue));  // reader addr
              } else {
                struct.args[cx.S] = VarRef(clauseVarValue);  // writer addr
              }
            } else if (clauseVarValue is Term) {
              if (isReaderMode) {
                // Reader mode with ground term: create fresh var, bind it
                final (writerAddr, readerAddr) = cx.rt.heap.allocateVariable();
                cx.rt.heap.bindVariable(writerAddr, clauseVarValue);
                struct.args[cx.S] = VarRef(readerAddr);
              } else {
                // Writer mode: use ground term directly
                struct.args[cx.S] = clauseVarValue;
              }
            } else if (clauseVarValue == null) {
              // First occurrence - allocate fresh variable
              final (writerAddr, readerAddr) = cx.rt.heap.allocateVariable();
              cx.clauseVars[varIndex] = VarRef(writerAddr);
              struct.args[cx.S] = VarRef(isReaderMode ? readerAddr : writerAddr);
            }
            cx.S++;

            // Check if structure is complete
            if (cx.S >= struct.args.length) {
              // Check if we're in guard argument building mode (pre-commit)
              if (cx.guardArgSlot != null) {
                // Guard argument mode: a term for the guard call alone, placed
                // in its parent or its argument slot, no heap binding
                // ([_completeGuardStructure]).
                _completeGuardStructure(cx);
              } else {
                // BODY phase: bind to heap writer
                final targetValue = cx.clauseVars[-1];
                HeapCell? targetWriterAddr;
                if (targetValue is VarRef) {
                  targetWriterAddr = targetValue.addr;
                } else if (targetValue is HeapCell) {
                  targetWriterAddr = targetValue;
                }

                if (targetWriterAddr != null) {
                  final acts = cx.rt.heap.bindWriterStruct(targetWriterAddr, struct.functor, struct.args);
                  for (final a in acts) {
                    cx.rt.gq.enqueue(a);
                  }
                }

                // Handle parent structure restoration - pop from stack
                if (cx.parentStack.isNotEmpty && targetWriterAddr != null) {
                  final nestedWriterAddr = targetWriterAddr;
                  final parent = cx.parentStack.removeLast();
                  final parentWriterId = parent.writerId;

                  if (parent.structure is StructTerm) {
                    final parentStruct = parent.structure as StructTerm;
                    // Per spec v3.2: use readerForWriter() instead of +1 arithmetic
                    parentStruct.args[parent.s] = VarRef(cx.rt.heap.pairedReaderAddr(nestedWriterAddr));  // reader addr
                  }

                  cx.currentStructure = parent.structure;
                  cx.S = parent.s + 1;
                  cx.mode = parent.mode;
                  cx.clauseVars[-1] = parentWriterId;

                  // Check if parent is now complete - and recursively complete ancestors
                  while (cx.currentStructure is StructTerm) {
                    final parentStruct = cx.currentStructure as StructTerm;
                    final currentWriterId = cx.clauseVars[-1];
                    final currentWriterAddrInt = currentWriterId is VarRef ? currentWriterId.addr : (currentWriterId is HeapCell ? currentWriterId : null);

                    if (cx.S >= parentStruct.args.length && currentWriterAddrInt != null) {
                      final acts = cx.rt.heap.bindWriterStruct(currentWriterAddrInt, parentStruct.functor, parentStruct.args);
                      for (final a in acts) {
                        cx.rt.gq.enqueue(a);
                      }

                      // Check for more ancestors
                      if (cx.parentStack.isNotEmpty) {
                        final ancestor = cx.parentStack.removeLast();
                        if (ancestor.structure is StructTerm) {
                          final ancestorStruct = ancestor.structure as StructTerm;
                          // Per spec v3.2: use readerForWriter() instead of +1 arithmetic
                          ancestorStruct.args[ancestor.s] = VarRef(cx.rt.heap.pairedReaderAddr(currentWriterAddrInt));  // reader addr
                        }
                        cx.currentStructure = ancestor.structure;
                        cx.S = ancestor.s + 1;
                        cx.mode = ancestor.mode;
                        cx.clauseVars[-1] = ancestor.writerId;
                      } else {
                        // No more ancestors - store in argSlots and reset
                        final parentTargetSlot = cx.clauseVars[-2];
                        if (parentTargetSlot is int && parentTargetSlot >= 0) {
                          // Per spec v3.2: use readerForWriter() instead of +1 arithmetic
                          cx.argSlots[parentTargetSlot] = VarRef(cx.rt.heap.pairedReaderAddr(currentWriterAddrInt));  // reader addr
                          cx.clauseVars.remove(-2);
                        }
                        cx.currentStructure = null;
                        cx.mode = UnifyMode.read;
                        cx.S = 0;
                        cx.clauseVars.remove(-1);
                        break;
                      }
                    } else {
                      // Parent not complete yet, stop
                      break;
                    }
                  }
                } else {
                  // No parent - store in argSlots and reset
                  final targetSlot = cx.clauseVars[-2];
                  if (targetSlot is int && targetSlot >= 0) {
                    // Per spec v3.2: use readerForWriter() instead of +1 arithmetic
                    cx.argSlots[targetSlot] = VarRef(cx.rt.heap.pairedReaderAddr(targetWriterAddr!));  // reader addr
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
          if (cx.currentStructure is _SkippedSubterm) {
            // Under a suspended goal reader: nothing to match.  A writer
            // occurrence of a variable with no value yet makes it unknown
            // ([RunnerContext.unknownVars]); a reader occurrence gives no
            // value and leaves the variable as it is, to take its value from
            // its writer occurrence --- marked unknown here, a variable met
            // only as a reader stayed unknown and a guard over it was passed
            // by, so a clause that fails suspended.
            if (!isReaderMode && _hasNoValue(cx, varIndex)) {
              cx.unknownVars.add(varIndex);
            }
            return StepOutcome.advance;
          }
          if (cx.currentStructure is StructTerm) {
            final struct = cx.currentStructure as StructTerm;
            if (cx.S < struct.args.length) {
              var value = struct.args[cx.S];

              // Per spec v2.16.3: Dereference VarRef pointing to value cell
              if (value is VarRef && cx.rt.heap.isValue(value.addr)) {
                value = cx.rt.heap.getValue(value.addr)!;
              }

              final existingValue = cx.clauseVars[varIndex];

              if (isReaderMode) {
                // UnifyReader READ mode logic: the head has a reader at this
                // subterm.  An unbound goal writer is assigned it; a goal
                // reader and a goal term fail (appendix-term-matching.tex,
                // column "Reader X2?"; see _isUnboundWriterCell).
                if (value is! VarRef || !_isUnboundWriterCell(cx, value.addr)) {
                  return StepOutcome.nextClause;
                } else {
                  // Query has writer, clause expects reader
                  if (cx.isUnknown(varIndex)) {
                    // The goal writer would be assigned the reader of an
                    // unknown variable: no fail, and nothing the clause keeps.
                    // It was stored here as at a first occurrence, and a guard
                    // over the variable then failed on the goal's unbound
                    // writer instead of being undecided.
                    cx.S++;
                  } else if (existingValue != null) {
                    // Xi already allocated from previous writer occurrence
                    // Bind query writer to existing value (per spec 8.2)
                    if (_isGroundValue(existingValue)) {
                      // Ground value - bind writer directly to it
                      cx.sigmaHat[value.addr] = existingValue;
                    } else if (existingValue is VarRef) {
                      // Existing VarRef - bind writer to reader of it
                      final addr = existingValue.addr;
                      // Per spec v3.2: use readerForWriter() instead of +1 arithmetic
                      final readerAddr = cx.rt.heap.isWriter(addr) ? cx.rt.heap.pairedReaderAddr(addr) : addr;
                      cx.sigmaHat[value.addr] = VarRef(readerAddr);
                    } else if (existingValue is HeapCell) {
                      // Bare writer addr - bind writer to reader of it
                      // Per spec v3.2: use readerForWriter() instead of +1 arithmetic
                      cx.sigmaHat[value.addr] = VarRef(cx.rt.heap.pairedReaderAddr(existingValue));  // reader addr
                    }
                    cx.S++;
                  } else {
                    // First occurrence: head reader receives goal writer
                    // Store the goal's writer directly - clause can write to it (output stream)
                    // or read from it when bound. No indirection needed.
                    // This is consistent with GetVariable reader mode (line 1877).
                    cx.clauseVars[varIndex] = value.addr;
                    cx.S++;
                  }
                }
              } else {
                // UnifyWriter READ mode logic: the head has a writer at this
                // subterm.  A goal writer fails, whatever the variable
                // (appendix-term-matching.tex, row "Writer X1", column
                // "Writer X2"; see _isGoalWriter).  The placement after `pop`
                // of a structure built for a goal writer meets that writer
                // assigned, and is passed.
                if (_isGoalWriter(cx, value)) {
                  return StepOutcome.nextClause;
                }
                if (cx.isUnknown(varIndex)) {
                  // A later writer occurrence of an unknown variable: what it
                  // meets is matched against a value not yet there, so it is
                  // undecided, and gives the variable no value.
                  cx.S++;
                } else if (existingValue is HeapCell || (existingValue is VarRef && cx.rt.heap.isWriter(existingValue.addr))) {
                  // Clause variable is a fresh variable addr from previous UnifyReader
                  final clauseVarAddr = existingValue is HeapCell ? existingValue : (existingValue as VarRef).addr;

                  if (value is VarRef && cx.rt.heap.isWriter(value.addr)) {
                    // Query has writer - check for WxW violation
                    final clauseVarBound = cx.rt.heap.isWriterBound(clauseVarAddr);
                    final queryVarBound = cx.rt.heap.isWriterBound(value.addr);
                    if (!clauseVarBound && !queryVarBound) {
                      return StepOutcome.nextClause;
                    }
                    cx.sigmaHat[clauseVarAddr] = value;
                    cx.S++;
                  } else if (value is VarRef && cx.rt.heap.isReader(value.addr)) {
                    cx.sigmaHat[clauseVarAddr] = value;
                    cx.S++;
                  } else if (_isGroundValue(value)) {
                    cx.sigmaHat[clauseVarAddr] = value;
                    cx.S++;
                  } else {
                    return StepOutcome.nextClause;
                  }
                } else if (existingValue != null) {
                  // Clause variable already bound - advance
                  cx.S++;
                } else {
                  // First occurrence - store the value
                  if (value is VarRef && cx.rt.heap.isWriter(value.addr)) {
                    cx.clauseVars[varIndex] = value;
                    cx.S++;
                  } else if (value is VarRef && cx.rt.heap.isReader(value.addr)) {
                    final rid = value.addr;
                    // Use abstraction methods for imported reader support
                    if (cx.rt.heap.isReaderBound(rid)) {
                      final readerValue = cx.rt.heap.getReaderValue(rid);
                      cx.clauseVars[varIndex] = readerValue;
                    } else {
                      cx.clauseVars[varIndex] = value;
                    }
                    cx.S++;
                  } else if (_isGroundValue(value)) {
                    cx.clauseVars[varIndex] = value;
                    cx.S++;
                  } else {
                    return StepOutcome.nextClause;
                  }
                }
              }
            }
          }
        }
    return StepOutcome.advance;
  }

  /// `unify_structure` (nested structure at the current S subterm). READ mode:
  /// match the subterm functor/arity, entering it (or mode-converting an unbound
  /// writer to WRITE, or suspending on an unbound reader (Si) and skipping the
  /// pattern under it). WRITE mode: create the nested tentative struct in the
  /// parent and descend into it.
  StepOutcome execUnifyStructure(RunnerContext cx, String functor, int arity) {
        if (cx.mode == UnifyMode.read) {
          // READ mode: Match structure at args[S]
          if (cx.currentStructure is StructTerm) {
            final parent = cx.currentStructure as StructTerm;
            if (cx.S < parent.args.length) {
              Object? value = parent.args[cx.S];

              // CRITICAL FIX: Dereference if it's a variable reference
              // This handles metainterpreter/reduce cases where nested structures
              // come through variable bindings
              if (value is VarRef) {
                final addr = value.addr;
                final isReaderVar = cx.rt.heap.isReader(addr);
                // Check sigma-hat first (tentative bindings)
                if (cx.sigmaHat.containsKey(addr)) {
                  value = cx.sigmaHat[addr];
                }
                // Then check heap bindings
                else if (cx.rt.heap.isBound(addr)) {
                  final boundValue = cx.rt.heap.getValue(addr);
                  value = boundValue;
                }
                else {
                }
              }

              if (value is StructTerm && value.functor == functor && value.args.length == arity) {
                // Match! Enter this structure
                cx.currentStructure = value;
                cx.S = 0;
              } else if (value is VarRef && cx.rt.heap.isWriter(value.addr)) {
                // Mode conversion: unbound writer where structure expected
                // Following HeadStructure behavior (spec 6.1 line 254)
                // Switch to WRITE mode and build the structure

                // Create tentative structure
                final nested = _TentativeStruct(functor, arity, List.filled(arity, null));

                // Record binding in σ̂w (writer will be bound to this structure at commit)
                // Store as Object? to avoid type issues (will be converted to StructTerm at commit)
                cx.sigmaHat[value.addr] = nested;

                // Switch to WRITE mode
                cx.mode = UnifyMode.write;

                // Enter the nested structure
                cx.currentStructure = nested;
                cx.S = 0;
              } else if (value is VarRef && cx.rt.heap.isReader(value.addr)) {
                // Unbound reader where structure expected: suspend on it and
                // skip the pattern under it, matching the rest of the head
                // (appendix-term-matching.tex: "suspend on X1?"; a fail
                // anywhere is a fail).  Until 2026-10-02 the clause was
                // abandoned here, so a later mismatch was never seen.
                cx.Si.add(value.addr);
                _skipSubterm(cx);
              } else {
                // Mismatch - fail to next clause
                return StepOutcome.nextClause;
              }
            }
          }
        } else {
          // WRITE mode: Create nested structure at args[S]
          if (cx.currentStructure is _TentativeStruct) {
            final parent = cx.currentStructure as _TentativeStruct;
            final nested = _TentativeStruct(functor, arity, List.filled(arity, null));
            parent.args[cx.S] = nested;
            cx.currentStructure = nested;
            cx.S = 0;
          }
        }
    return StepOutcome.advance;
  }

  /// `get_variable` (load goal arg argSlot into clause var). Writer mode is a
  /// head writer: a goal writer fails it ([_isGoalWriter]), and a goal reader
  /// or term is bound to the clause var (or to its earlier-occurrence writer
  /// via σ̂w); reader mode has the clause reader observe an unbound goal
  /// writer, failing on a goal reader or term. Null arg or fail → next clause.
  /// It is the first occurrence among the head's arguments, and may follow one
  /// inside a structure; an unknown variable ([RunnerContext.unknownVars]) met
  /// there is given no value.
  StepOutcome execGetVariable(
      RunnerContext cx, int varIndex, int argSlot, bool isReaderMode) {
    final arg = _getArg(cx, argSlot);
    if (arg == null) {
      return StepOutcome.nextClause;
    }

        if (!isReaderMode) {
          // A goal writer against a head writer fails, whatever the variable
          // (appendix-term-matching.tex, row "Writer X1", column "Writer X2";
          // see _isGoalWriter).
          if (_isGoalWriter(cx, arg)) return StepOutcome.nextClause;
          // A later writer occurrence of an unknown variable: what it meets is
          // matched against a value not yet there, so it is undecided, and
          // gives the variable no value.
          if (cx.isUnknown(varIndex)) return StepOutcome.advance;
          // GetWriterVariable logic: Load argument into clause WRITER variable
          // IMPORTANT: Check if clauseVars[varIndex] already has a writer from
          // an earlier occurrence (e.g., inside a structure via UnifyVariable).
          // If so, bind that writer to the argument value via sigmaHat.
          final existing = cx.clauseVars[varIndex];

          if (arg is VarRef && cx.rt.heap.isWriter(arg.addr)) {
            if (existing is VarRef && cx.rt.heap.isWriter(existing.addr)) {
              // Both are writers - bind arg writer to existing writer's reader
              // Per spec v3.2: use readerForWriter() instead of +1 arithmetic
              cx.sigmaHat[arg.addr] = VarRef(cx.rt.heap.pairedReaderAddr(existing.addr));  // reader addr
            } else if (existing is HeapCell) {
              // existing is bare writer addr - bind arg to reader of it
              // Per spec v3.2: use readerForWriter() instead of +1 arithmetic
              cx.sigmaHat[arg.addr] = VarRef(cx.rt.heap.pairedReaderAddr(existing));  // reader addr
            } else {
              // First occurrence: goal writer vs head writer
              // Store the goal's writer reference - clause can bind through it
              if (cx.rt.heap.isWriterBound(arg.addr)) {
                // Goal writer already bound - use its value
                final boundValue = cx.rt.heap.valueOfWriter(arg.addr);
                cx.clauseVars[varIndex] = boundValue;
              } else {
                // Goal writer unbound - store writer ref, clause can bind it later
                cx.clauseVars[varIndex] = arg;
              }
            }
          } else if (arg is VarRef && cx.rt.heap.isReader(arg.addr)) {
            // Use abstraction methods that work for both local and imported readers
            if (cx.rt.heap.isReaderBound(arg.addr)) {
              final value = cx.rt.heap.getReaderValue(arg.addr);
              if (existing is VarRef && cx.rt.heap.isWriter(existing.addr)) {
                cx.sigmaHat[existing.addr] = value;
              } else if (existing is HeapCell) {
                cx.sigmaHat[existing] = value;
              } else {
                cx.clauseVars[varIndex] = value;
              }
            } else {
              // Reader is unbound - but clause expects a writer (isReaderMode=false)
              // Per spec: Goal reader X? vs Head writer V → V receives X? (the reader reference)
              // Store the reader reference itself, not just the underlying writer addr
              if (existing is VarRef && cx.rt.heap.isWriter(existing.addr)) {
                // Already have a writer from earlier occurrence - bind it to goal's reader
                cx.sigmaHat[existing.addr] = arg;  // arg is the reader VarRef
              } else if (existing is HeapCell) {
                cx.sigmaHat[existing] = arg;
              } else {
                // First occurrence - store the reader reference
                cx.clauseVars[varIndex] = arg;  // Store reader VarRef, not wid
              }
            }
          } else if (arg is ConstTerm) {
            if (existing is VarRef && cx.rt.heap.isWriter(existing.addr)) {
              // Already have a writer from earlier occurrence - bind it
              cx.sigmaHat[existing.addr] = arg;
            } else if (existing is HeapCell) {
              // Bare writer addr - bind it
              cx.sigmaHat[existing] = arg;
            } else {
              cx.clauseVars[varIndex] = arg;
            }
          } else if (arg is StructTerm) {
            if (existing is VarRef && cx.rt.heap.isWriter(existing.addr)) {
              cx.sigmaHat[existing.addr] = arg;
            } else if (existing is HeapCell) {
              cx.sigmaHat[existing] = arg;
            } else {
              cx.clauseVars[varIndex] = arg;
            }
          } else if (arg is Term) {
            // Handle other Term types (e.g., MutualRefTerm)
            if (existing is VarRef && cx.rt.heap.isWriter(existing.addr)) {
              cx.sigmaHat[existing.addr] = arg;
            } else if (existing is HeapCell) {
              cx.sigmaHat[existing] = arg;
            } else {
              cx.clauseVars[varIndex] = arg;
            }
          }
        } else {
          // GetReaderVariable logic: the head has a reader here.  An unbound
          // goal writer is assigned it; a goal reader and a goal term fail
          // (appendix-term-matching.tex, column "Reader X2?"; see
          // _isUnboundWriterCell).
          final existing = cx.clauseVars[varIndex];

          if (arg is! VarRef || !_isUnboundWriterCell(cx, arg.addr)) {
            return StepOutcome.nextClause;
          }
          // Goal writer → head reader (clause observes goal's variable)
          if (cx.isUnknown(varIndex)) {
            // The goal writer would be assigned the reader of an unknown
            // variable: no fail, and nothing the clause keeps.  It was stored
            // here as at a first occurrence, and a guard over the variable
            // then failed on the goal's unbound writer instead of being
            // undecided (f7(same(To), To?) :- ground(To?)).
          } else if (existing != null) {
            // clauseVars already has a value (from earlier occurrence like UnifyVariable)
            // Bind the writer arg to the READER of that value
            // BUG FIX: When existing is a writer VarRef, convert to reader
            if (existing is VarRef && cx.rt.heap.isWriter(existing.addr)) {
              // existing is a writer - bind to its reader
              // Per spec v3.2: use readerForWriter() instead of +1 arithmetic
              cx.sigmaHat[arg.addr] = VarRef(cx.rt.heap.pairedReaderAddr(existing.addr));  // reader addr
            } else if (existing is HeapCell) {
              // existing is bare writer addr - bind to reader of it
              // Per spec v3.2: use readerForWriter() instead of +1 arithmetic
              cx.sigmaHat[arg.addr] = VarRef(cx.rt.heap.pairedReaderAddr(existing));  // reader addr
            } else {
              // existing is already a reader or a term - use as-is
              cx.sigmaHat[arg.addr] = existing;
            }
          } else {
            // First occurrence: head reader observes goal writer
            // Store the goal's writer addr so clause can read through it
            // No sigmaHat binding needed - goal owns the writer
            cx.clauseVars[varIndex] = arg.addr;
          }
        }
    return StepOutcome.advance;
  }

  /// `get_value` (unify goal arg argSlot with the already-bound clause var).
  /// Writer mode fails on a goal writer ([_isGoalWriter]) and otherwise
  /// unifies/binds via σ̂w; reader mode binds an unbound goal
  /// writer to the stored value's reader (suspending (Si) on an unbound stored
  /// reader) and fails on a goal reader or term. An unknown variable
  /// ([RunnerContext.unknownVars]) is given no value and fails only where the
  /// table fails whatever the variable. Null arg, unset clause var, or any
  /// mismatch → next clause.
  StepOutcome execGetValue(
      RunnerContext cx, int varIndex, int argSlot, bool isReaderMode) {

        final arg = _getArg(cx, argSlot);
        if (arg == null) {
          return StepOutcome.nextClause;
        }

        // A goal writer against a head writer fails, whatever the variable,
        // unknown or not (appendix-term-matching.tex, row "Writer X1",
        // column "Writer X2"; see _isGoalWriter).
        if (!isReaderMode && _isGoalWriter(cx, arg)) {
          return StepOutcome.nextClause;
        }

        if (cx.isUnknown(varIndex)) {
          // The variable is unknown: nothing to match it with, so this
          // occurrence fails only where the table fails it whatever the
          // variable --- a head reader against a goal reader or term
          // (appendix-term-matching.tex, column "Reader X2?"), and a head
          // writer against a goal writer, above.
          if (isReaderMode &&
              (arg is! VarRef || !_isUnboundWriterCell(cx, arg.addr))) {
            return StepOutcome.nextClause;
          }
          return StepOutcome.advance;
        }

        var storedValue = cx.clauseVars[varIndex];
        if (storedValue == null) return StepOutcome.nextClause;

        if (!isReaderMode) {
          // GetWriterValue logic: Unify argument with clause WRITER variable
          // storedValue is already the writer addr (or term)

          if (arg is VarRef && cx.rt.heap.isWriter(arg.addr)) {
            final argBound = cx.rt.heap.isWriterBound(arg.addr);
            if (argBound) {
              final argValue = cx.rt.heap.valueOfWriter(arg.addr);
              if (storedValue is HeapCell) {
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
                    return StepOutcome.nextClause;
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
                  return StepOutcome.nextClause;
                }
              }
            } else {
              if (storedValue is HeapCell) {
                final freshVarBinding = cx.sigmaHat[storedValue];
                if (freshVarBinding != null) {
                  cx.sigmaHat[arg.addr] = freshVarBinding;
                } else if (arg.addr != storedValue) {
                  return StepOutcome.nextClause;
                }
              } else if (storedValue is Term) {
                cx.sigmaHat[arg.addr] = storedValue;
              }
            }
          } else if (arg is VarRef && cx.rt.heap.isReader(arg.addr)) {
            final rid = arg.addr;
            // Use abstraction methods for imported reader support
            if (cx.rt.heap.isReaderBound(rid)) {
              final readerValue = cx.rt.heap.getReaderValue(rid);
              if (storedValue is HeapCell) {
                cx.sigmaHat[storedValue] = readerValue;
              } else if (storedValue != readerValue) {
                return StepOutcome.nextClause;
              }
            } else {
              // Reader is unbound - alias storedValue to reader.  A reader
              // cell always points to its writer (heap_fcp.dart: a reader is
              // made with a Pointer and only a writer is rewritten), so the
              // writer is there.  The null branch, for an "imported reader"
              // with no local writer, which the heap has not represented
              // since VariableEntry went (ae141816), went on 2026-10-07.
              final wid = cx.rt.heap.tryWriterForReader(rid)!;
              if (storedValue is HeapCell) {
                // Per spec v3.2: use readerForWriter() instead of +1 arithmetic
                cx.sigmaHat[storedValue] = VarRef(cx.rt.heap.pairedReaderAddr(wid));  // reader addr
              }
            }
          } else if (arg is ConstTerm) {
            if (storedValue is HeapCell) {
              cx.sigmaHat[storedValue] = arg;
            } else if (storedValue is ConstTerm && storedValue.value != arg.value) {
              return StepOutcome.nextClause;
            }
          } else if (arg is StructTerm) {
            if (storedValue is HeapCell) {
              cx.sigmaHat[storedValue] = arg;
            } else if (storedValue is StructTerm && storedValue.functor != arg.functor) {
              return StepOutcome.nextClause;
            }
          }
        } else {
          // GetReaderValue logic: the head has a reader here, whose variable an
          // earlier head argument holds.  An unbound goal writer is assigned
          // the reader; a goal reader and a goal term fail, whatever the
          // earlier argument bound the variable to: `q(X, X?)` fails on
          // `q(1, 2)` and on `q(1, 1)` alike (appendix-term-matching.tex,
          // column "Reader X2?"; see _isUnboundWriterCell).
          if (arg is! VarRef || !_isUnboundWriterCell(cx, arg.addr)) {
            return StepOutcome.nextClause;
          }
          // Goal has writer, head has reader - bind goal writer to stored value
          if (storedValue is VarRef) {
            // storedValue is a reader/writer reference - bind goal writer to it.
            //
            // The clause variable is held by its WRITER address whenever an
            // earlier head argument bound it as a writer — `g(_, r(D?), D,
            // D?)` reaches this reader occurrence with `D` already stored as
            // its writer. A writer is never bound to a writer (the writer mgu
            // binds writers to terms and to readers), and what a reader
            // occurrence denotes is the variable's READER: pair it across.
            // Reported by Currencies Code, 2026-09-03, as "the engine binds an
            // output term's reader to a fresh writer when the variable is bound
            // by a later head argument"; the commit's WxW check caught it as
            // `WxW violation in commit`.
            cx.sigmaHat[arg.addr] = cx.rt.heap.isWriter(storedValue.addr)
                ? VarRef(cx.rt.heap.pairedReaderAddr(storedValue.addr))
                : storedValue;
          } else if (storedValue is HeapCell) {
            // storedValue is a reader addr - use abstraction methods for imported reader support
            if (cx.rt.heap.isReaderBound(storedValue)) {
              final readerValue = cx.rt.heap.getReaderValue(storedValue);
              cx.sigmaHat[arg.addr] = readerValue;
            } else {
              // Suspend on it and match the rest of the head (a fail anywhere
              // is a fail).  Until 2026-10-02 the clause was abandoned here,
              // so a later mismatch was never seen.
              cx.Si.add(storedValue);
            }
          } else if (storedValue is Term) {
            cx.sigmaHat[arg.addr] = storedValue;
          }
        }
    return StepOutcome.advance;
  }

  /// `set_variable` (place a clause var into the BODY structure being built).
  /// Mode-adjusts the existing binding (or allocates fresh), and on completion
  /// binds the target writer, restoring/completing parent structures up the
  /// stack and storing the result reader into the target arg slot.  Before
  /// commit it fills a structure nested in a guard's argument, as
  /// `unify_variable` fills the argument's own positions there
  /// ([OpExecutors.execPutStructure]); until 2026-10-02 it did nothing there.
  StepOutcome execSetVariable(RunnerContext cx, int varIndex, bool isReaderMode) {
        if (!cx.inBody &&
            cx.guardArgSlot != null &&
            cx.mode == UnifyMode.write &&
            cx.currentStructure is StructTerm) {
          return execUnifyVariable(cx, varIndex, isReaderMode);
        }

        if (cx.inBody && cx.mode == UnifyMode.write && cx.currentStructure is StructTerm) {
          // Check what value exists in clause variables
          final existingValue = cx.clauseVars[varIndex];
          final struct = cx.currentStructure as StructTerm;
          // DEBUG: trace clauseVars for accept_intro Ch variable

          if (existingValue is VarRef) {
            // VarRef: use its addr with appropriate mode
            final addr = existingValue.addr;
            if (isReaderMode && cx.rt.heap.isWriter(addr)) {
              // Per spec v3.2: use readerForWriter() instead of +1 arithmetic
              struct.args[cx.S] = VarRef(cx.rt.heap.pairedReaderAddr(addr));  // reader addr
            } else if (!isReaderMode && cx.rt.heap.isReader(addr)) {
              // Per spec v3.2: use tryWriterForReader() instead of -1 arithmetic
              struct.args[cx.S] = VarRef(cx.rt.heap.tryWriterForReader(addr)!);  // writer addr
            } else {
              struct.args[cx.S] = VarRef(addr);  // mode matches
            }
          } else if (existingValue is HeapCell) {
            // Legacy: bare writer addr
            if (isReaderMode) {
              // Per spec v3.2: use readerForWriter() instead of +1 arithmetic
              struct.args[cx.S] = VarRef(cx.rt.heap.pairedReaderAddr(existingValue));  // reader addr
            } else {
              struct.args[cx.S] = VarRef(existingValue);  // writer addr
            }
          } else if (existingValue is Term) {
            // Term (ConstTerm, StructTerm, etc.): embed directly in structure
            struct.args[cx.S] = existingValue;
          } else {
            // Uninitialized: allocate new variable
            final (writerAddr, readerAddr) = cx.rt.heap.allocateVariable();
            cx.clauseVars[varIndex] = VarRef(writerAddr);
            struct.args[cx.S] = VarRef(isReaderMode ? readerAddr : writerAddr);
          }
          cx.S++;

          // Check if structure is complete
          if (cx.S >= struct.args.length) {
            final targetValue = cx.clauseVars[-1];
            HeapCell? targetWriterAddr;
            if (targetValue is VarRef) {
              targetWriterAddr = targetValue.addr;
            } else if (targetValue is HeapCell) {
              targetWriterAddr = targetValue;
            }

            if (targetWriterAddr != null) {
              final acts = cx.rt.heap.bindWriterStruct(targetWriterAddr, struct.functor, struct.args);
              for (final a in acts) {
                cx.rt.gq.enqueue(a);
              }

              // SetWriter-specific: Store VarRef in argSlots ONLY if no parent
              // (nested structures should not store until outermost is complete)
              if (!isReaderMode && cx.parentStack.isEmpty) {
                final targetSlot = cx.clauseVars[-2];
                if (targetSlot is int && targetSlot >= 0) {
                  // Per spec v3.2: use readerForWriter() instead of +1 arithmetic
                  cx.argSlots[targetSlot] = VarRef(cx.rt.heap.pairedReaderAddr(targetWriterAddr));  // reader addr
                  cx.clauseVars.remove(-2);
                }
              }
            }

            // Handle parent structure restoration - pop from stack
            if (cx.parentStack.isNotEmpty && targetWriterAddr is HeapCell) {
              final nestedWriterAddr = targetWriterAddr;
              final parent = cx.parentStack.removeLast();
              final parentWriterId = parent.writerId;
              final parentWriterAddrInt = parentWriterId is VarRef ? parentWriterId.addr : (parentWriterId is HeapCell ? parentWriterId : null);

              if (parent.structure is StructTerm) {
                final parentStruct = parent.structure as StructTerm;
                // Per spec v3.2: use readerForWriter() instead of +1 arithmetic
                parentStruct.args[parent.s] = VarRef(cx.rt.heap.pairedReaderAddr(nestedWriterAddr));  // reader addr
              }

              cx.currentStructure = parent.structure;
              cx.S = parent.s + 1;
              cx.mode = parent.mode;
              cx.clauseVars[-1] = parentWriterId;

              // Check if parent is now complete - and recursively complete ancestors
              while (cx.currentStructure is StructTerm) {
                final parentStruct = cx.currentStructure as StructTerm;
                final currentWriterAddr = cx.clauseVars[-1];
                final currentWriterAddrInt = currentWriterAddr is VarRef ? currentWriterAddr.addr : (currentWriterAddr is HeapCell ? currentWriterAddr : null);

                if (cx.S >= parentStruct.args.length && currentWriterAddrInt != null) {
                  // bindWriterStruct returns activations directly
                  final acts = cx.rt.heap.bindWriterStruct(currentWriterAddrInt, parentStruct.functor, parentStruct.args);
                  for (final a in acts) {
                    cx.rt.gq.enqueue(a);
                  }

                  // Check for more ancestors
                  if (cx.parentStack.isNotEmpty) {
                    final ancestor = cx.parentStack.removeLast();
                    if (ancestor.structure is StructTerm) {
                      final ancestorStruct = ancestor.structure as StructTerm;
                      // Use reader address (writer + 1) for structure args
                      ancestorStruct.args[ancestor.s] = VarRef(cx.rt.heap.pairedReaderAddr(currentWriterAddrInt));
                    }
                    cx.currentStructure = ancestor.structure;
                    cx.S = ancestor.s + 1;
                    cx.mode = ancestor.mode;
                    cx.clauseVars[-1] = ancestor.writerId;
                  } else {
                    // No more ancestors - store in argSlots and reset
                    final parentTargetSlot = cx.clauseVars[-2];
                    if (parentTargetSlot is int && parentTargetSlot >= 0) {
                      // Use reader address (writer + 1) for argSlots
                      cx.argSlots[parentTargetSlot] = VarRef(cx.rt.heap.pairedReaderAddr(currentWriterAddrInt));
                      cx.clauseVars.remove(-2);
                    }
                    cx.currentStructure = null;
                    cx.mode = UnifyMode.read;
                    cx.S = 0;
                    cx.clauseVars.remove(-1);
                    break;
                  }
                } else {
                  // Parent not complete yet, stop
                  break;
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
    return StepOutcome.advance;
  }

  /// `put_variable` (place a clause var into goal arg slot argSlot for a body
  /// call). Resolves the clause var (VarRef/int/placeholder/term/first-occurrence)
  /// to a mode-appropriate VarRef in argSlots, allocating or heap-storing as
  /// needed so every CallEnv argument is a VarRef.
  StepOutcome execPutVariable(
      RunnerContext cx, int varIndex, int argSlot, bool isReaderMode) {
        // A guard's argument (before commit) that is an unknown variable
        // ([RunnerContext.unknownVars]): the slot gets the variable that
        // stands for it, any term, which the guard's decision meets
        // ([_undecidedMember]) and the clause does not keep, so the variable
        // stays unknown.
        if (!cx.inBody && cx.isUnknown(varIndex)) {
          cx.argSlots[argSlot] =
              _unknownPlaceholder(cx, varIndex, isReaderMode);
          return StepOutcome.advance;
        }
        final value = cx.clauseVars[varIndex];

        if (value is VarRef) {
          // Already a VarRef - determine writer addr and store with appropriate mode
          final addr = value.addr;
          final isWriter = cx.rt.heap.isWriter(addr);
          final isReader = cx.rt.heap.isReader(addr);

          if (!isWriter && !isReader) {
            // Bound to ground value (ValueTag) - store on heap and pass VarRef
            // Per spec v2.16.3 Section 1.1: CallEnv arguments must be VarRefs
            final groundValue = cx.rt.heap.getValue(addr);
            if (groundValue != null) {
              // Store value on heap and return VarRef
              final heapAddr = cx.rt.heap.storeTermOnHeap(groundValue);
              cx.argSlots[argSlot] = VarRef(heapAddr);
            } else {
              cx.argSlots[argSlot] = value;  // Fallback: already VarRef
            }
          } else {
            // Writer or reader
            if (isWriter) {
              final writerAddr = addr;
              cx.argSlots[argSlot] = VarRef(isReaderMode ? cx.rt.heap.pairedReaderAddr(writerAddr) : writerAddr);
            } else {
              // A reader: its writer, which a reader cell always points to
              // (the "imported reader" null branch went on 2026-10-07, as
              // above), used by mode.
              final writerAddr = cx.rt.heap.tryWriterForReader(addr)!;
              cx.argSlots[argSlot] = VarRef(isReaderMode ? cx.rt.heap.pairedReaderAddr(writerAddr) : writerAddr);
            }
          }
        } else if (value is HeapCell) {
          // Legacy: bare int ID (assumed to be writer addr)
          cx.argSlots[argSlot] = VarRef(isReaderMode ? cx.rt.heap.pairedReaderAddr(value) : value);
        } else if (value is _ClauseVar && !isReaderMode) {
          // Placeholder (PutWriter only) - allocate fresh variable
          final (writerAddr, _) = cx.rt.heap.allocateVariable();
          cx.argSlots[argSlot] = VarRef(writerAddr);
          cx.clauseVars[varIndex] = VarRef(writerAddr);
        } else if (value is StructTerm && isReaderMode) {
          // Structure (PutReader only) - create fresh variable and bind it
          final (writerAddr, readerAddr) = cx.rt.heap.allocateVariable();
          cx.rt.heap.bindWriterStruct(writerAddr, value.functor, value.args);
          cx.argSlots[argSlot] = VarRef(readerAddr);
        } else if (value is ConstTerm && isReaderMode) {
          // Constant (PutReader only) - create fresh variable and bind it
          final (writerAddr, readerAddr) = cx.rt.heap.allocateVariable();
          cx.rt.heap.bindWriterConst(writerAddr, value.value);
          cx.argSlots[argSlot] = VarRef(readerAddr);
        } else if (value == null) {
          // First occurrence - allocate fresh variable
          final (writerAddr, readerAddr) = cx.rt.heap.allocateVariable();
          cx.clauseVars[varIndex] = VarRef(writerAddr);
          cx.argSlots[argSlot] = VarRef(isReaderMode ? readerAddr : writerAddr);
        } else if (value is Term && isReaderMode) {
          // Ground term (e.g., MutualRefTerm) - store on heap and pass VarRef
          // Per spec v2.16.3 Section 1.1: CallEnv arguments must be VarRefs
          final heapAddr = cx.rt.heap.storeTermOnHeap(value);
          cx.argSlots[argSlot] = VarRef(heapAddr);
        } else {
          // No case above places this value: a fault of the compiled code.
          // Until 2026-10-02 it printed a warning and went on.
          throw StateError('put_variable: unexpected clause value $value '
              '(reader mode: $isReaderMode)');
        }
    return StepOutcome.advance;
  }

  /// `set_constant` (place a constant into the BODY structure being built).
  /// On completion binds the target writer and restores/completes parent
  /// structures up the stack, storing the result reader into the target slot.
  /// Before commit it fills a structure nested in a guard's argument, as
  /// `unify_constant` fills the argument's own positions there
  /// ([OpExecutors.execPutStructure]); until 2026-10-02 it did nothing there.
  StepOutcome execSetConstant(RunnerContext cx, Object? opValue) {
        if (!cx.inBody &&
            cx.guardArgSlot != null &&
            cx.mode == UnifyMode.write &&
            cx.currentStructure is StructTerm) {
          return execUnifyConstant(cx, opValue);
        }
        if (cx.inBody && cx.mode == UnifyMode.write && cx.currentStructure is StructTerm) {
          // Store ConstTerm in current structure at position S
          final struct = cx.currentStructure as StructTerm;
          struct.args[cx.S] = ConstTerm(opValue);
          cx.S++; // Move to next position

          // Check if structure is complete (all arguments filled)
          if (cx.S >= struct.args.length) {
            // Structure complete - bind the target writer (stored at clauseVars[-1])
            final targetWriterAddr = cx.clauseVars[-1];
            // Extract int from VarRef if needed
            final targetWriterAddrInt = targetWriterAddr is VarRef ? targetWriterAddr.addr : (targetWriterAddr is HeapCell ? targetWriterAddr : null);
            if (targetWriterAddrInt != null) {
              // Bind the writer to the completed structure (returns activations)
              final acts = cx.rt.heap.bindWriterStruct(targetWriterAddrInt, struct.functor, struct.args);
              for (final a in acts) {
                cx.rt.gq.enqueue(a);
              }
            }

            // Handle parent structure restoration (nested structures) - pop from stack
            if (cx.parentStack.isNotEmpty && targetWriterAddrInt != null) {
              final nestedWriterAddr = targetWriterAddrInt;
              final parent = cx.parentStack.removeLast();
              final parentWriterAddr = parent.writerId;
              // Extract int from parentWriterAddr if it's a VarRef
              final parentWriterAddrInt = parentWriterAddr is VarRef ? parentWriterAddr.addr : (parentWriterAddr is HeapCell ? parentWriterAddr : null);

              if (parent.structure is StructTerm) {
                final parentStruct = parent.structure as StructTerm;
                // Use reader address (writer + 1)
                parentStruct.args[parent.s] = VarRef(cx.rt.heap.pairedReaderAddr(nestedWriterAddr));
              }

              cx.currentStructure = parent.structure;
              cx.S = parent.s + 1;
              cx.mode = parent.mode;
              cx.clauseVars[-1] = parentWriterAddr;

              // Check if parent is now complete - and recursively complete ancestors
              while (cx.currentStructure is StructTerm) {
                final parentStruct = cx.currentStructure as StructTerm;
                final currentWriterAddr = cx.clauseVars[-1];
                final currentWriterAddrInt = currentWriterAddr is VarRef ? currentWriterAddr.addr : (currentWriterAddr is HeapCell ? currentWriterAddr : null);

                if (cx.S >= parentStruct.args.length && currentWriterAddrInt != null) {
                  // bindWriterStruct returns activations directly
                  final acts = cx.rt.heap.bindWriterStruct(currentWriterAddrInt, parentStruct.functor, parentStruct.args);
                  for (final a in acts) {
                    cx.rt.gq.enqueue(a);
                  }

                  // Check for more ancestors
                  if (cx.parentStack.isNotEmpty) {
                    final ancestor = cx.parentStack.removeLast();
                    if (ancestor.structure is StructTerm) {
                      final ancestorStruct = ancestor.structure as StructTerm;
                      // Use reader address (writer + 1)
                      ancestorStruct.args[ancestor.s] = VarRef(cx.rt.heap.pairedReaderAddr(currentWriterAddrInt));
                    }
                    cx.currentStructure = ancestor.structure;
                    cx.S = ancestor.s + 1;
                    cx.mode = ancestor.mode;
                    cx.clauseVars[-1] = ancestor.writerId;
                  } else {
                    // No more ancestors - store in argSlots and reset
                    final parentTargetSlot = cx.clauseVars[-2];
                    if (parentTargetSlot is int && parentTargetSlot >= 0) {
                      // Use reader address (writer + 1)
                      cx.argSlots[parentTargetSlot] = VarRef(cx.rt.heap.pairedReaderAddr(currentWriterAddrInt));
                      cx.clauseVars.remove(-2);
                    }
                    cx.currentStructure = null;
                    cx.mode = UnifyMode.read;
                    cx.S = 0;
                    cx.clauseVars.remove(-1);
                    break;
                  }
                } else {
                  // Parent not complete yet, stop
                  break;
                }
              }
            } else {
              // No parent - reset structure building state
              cx.currentStructure = null;
              cx.mode = UnifyMode.read;
              cx.S = 0;
              cx.clauseVars.remove(-1); // Clear the marker
            }
          }
        }
    return StepOutcome.advance;
  }

  /// `head_list` (0x13): match the argument against a list cell, which is the
  /// structure `'.'/2` (IGLP code-format-fragment.tex: "Lists are structures:
  /// a cell is the structure '.'/2; the empty list is the constant nil"), so
  /// it is `head_structure` for `'.'/2` at [argSlot] and does what that does,
  /// by the table's column "Term f2/n2" (GLP-Spec appendix-term-matching.tex,
  /// Definition "Term Matching"): a goal writer is assigned a tentative cell of
  /// two slots, which the head's elements fill (X1 := T2); a goal reader
  /// unbound suspends, the pattern under it skipped; a goal cell is matched in
  /// READ mode; anything else fails (GLP #3 Cowork, 2026-10-02 17:12 UTC, 2).
  /// No compiler of this tree emits it, a list in a head compiling to
  /// head_structure `'.'/2`; an artefact may carry it.  Until 2026-10-02 it
  /// gave an unbound goal writer a `'[|]'` cell with no slots, so the first
  /// element placed in it threw, and matched a bound list only as `'[|]'/2`,
  /// which is no list cell.
  StepOutcome execHeadList(RunnerContext cx, int argSlot) =>
      execHeadStructure(cx, '.', 2, argSlot);
}
