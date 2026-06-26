import 'dart:async' show Timer;
import 'dart:typed_data' show Uint8List;

import 'package:glp_runtime/multiagent/mad_context.dart' show MadContext;
import 'package:glp_runtime/multiagent/glp_network.dart' show PubKey;
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/commit.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/system_predicates.dart';
import 'package:glp_runtime/runtime/body_kernels.dart';
import 'package:glp_runtime/multiagent/variable_table.dart' show VariableEntry;
import 'opcodes.dart';
import 'opcodes_v2.dart' as opv2;
import 'package:glp_runtime/engine_v2/step_outcome.dart';

enum RunResult { terminated, suspended, yielded, outOfReductions }

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

/// Module target for REPL imports
class ReplModuleTarget {
  final String name;
  final BytecodeProgram program;
  ReplModuleTarget(this.name, this.program);
}

/// Simple module context for REPL (synchronous goal spawning)
class ReplModuleContext {
  final String moduleName;
  final Map<int, ReplModuleTarget> imports;  // importIndex (1-based) -> target
  final BytecodeProgram? combinedProgram;    // Combined program for entry point lookup
  final String programKey;                    // Key for scheduler's runners map

  ReplModuleContext({
    required this.moduleName,
    required this.imports,
    this.combinedProgram,
    this.programKey = 'main',
  });
}

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

class RunnerContext {
  final GlpRuntime rt;
  final int goalId;
  int kappa;  // Mutable - updated by Requeue for tail calls
  final CallEnv env;
  final Map<int, Object?> sigmaHat = <int, Object?>{}; // σ̂w: tentative writer bindings
  final Set<int> Si = <int>{};       // clause-level preliminary suspension set
  final Set<int> U = <int>{};        // goal-level suspension set (reader IDs)
  bool inBody = false;

  // WAM-style structure traversal state
  UnifyMode mode = UnifyMode.read;   // Current unification mode
  int S = 0;                          // Structure pointer (current position in structure)
  Object? currentStructure;           // Current structure being traversed
  final Map<int, Object?> clauseVars = {}; // Clause variable bindings (varIndex → value)

  // Parent structure stack for nested structure building (supports arbitrary depth)
  final List<_ParentContext> parentStack = [];

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
  String? goalProcName;  // Procedure name for delayed head formatting
  final void Function(int goalId, String head, String body)? onReduction;

  /// Re-format the goal head from current env state (after σ̂ applied to heap).
  /// This shows bound values instead of unbound variable names.
  String reformatHead() {
    final name = goalProcName ?? goalHead ?? '?';
    final args = <String>[];
    for (int i = 0; i < 10; i++) {
      final arg = env.arg(i);
      if (arg != null) {
        args.add(termFormatter != null
            ? termFormatter!(arg)
            : arg.toString());
      } else {
        break;
      }
    }
    if (args.isEmpty) return name;
    return '$name(${args.join(', ')})';
  }

  // Control trace output
  final bool showBindings;
  final bool debugOutput;

  // Custom term formatter for consistent variable naming
  final String Function(Term, {bool markReaders})? termFormatter;

  // Module context for distribute/transmit handlers (Phase 5 integration)
  final Object? moduleContext;

  RunnerContext({
    required this.rt,
    required this.goalId,
    required this.kappa,
    CallEnv? env,
    this.onActivation,
    this.reductionBudget,
    this.goalHead,
    this.goalProcName,
    this.onReduction,
    this.showBindings = true,
    this.debugOutput = false,
    this.termFormatter,
    this.moduleContext,
  }) : env = env ?? CallEnv();

  void clearClause() {
    sigmaHat.clear();
    Si.clear();
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

int _finalUnboundVar(RunnerContext cx, int addr) {
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

(Object?, Set<int>) _dereferenceWithTracking(Object? term, RunnerContext cx) {
  final unboundReaders = <int>{};

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
    } else if (t is int) {
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

GuardResult _evaluateGuard(String predicateName, List<Object?> args, RunnerContext cx) {
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
      if (cx.rt.heap.isReader(v.addr)) {
        // Use isReaderBound/getReaderValue for imported reader support
        if (!cx.rt.heap.isReaderBound(v.addr)) return null; // Unbound
        final deref = cx.rt.heap.getReaderValue(v.addr);
        return evaluateNumeric(deref);
      } else {
        final deref = cx.rt.heap.getValue(v.addr);
        if (deref == null) return null; // Unbound
        return evaluateNumeric(deref);
      }
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

    // Lexicographic comparison of ground constants (atoms/strings/numbers)
    case '@<':
      if (args.length < 2) return GuardResult.failure;
      String? evalConst(dynamic v) {
        if (v is ConstTerm) {
          final cv = v.value;
          return cv?.toString();
        }
        if (v is String || v is num) return v.toString();
        if (v is VarRef) {
          if (cx.rt.heap.isReader(v.addr)) {
            if (!cx.rt.heap.isReaderBound(v.addr)) return null;
            return evalConst(cx.rt.heap.getReaderValue(v.addr));
          }
          final deref = cx.rt.heap.getValue(v.addr);
          return deref == null ? null : evalConst(deref);
        }
        return null;
      }
      final lc = evalConst(args[0]);
      final rc = evalConst(args[1]);
      if (lc != null && rc != null) {
        return lc.compareTo(rc) < 0 ? GuardResult.success : GuardResult.failure;
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

    case 'list':
      // Succeeds if X is a list ([] or [H|T])
      // Per spec: list(X?) - Succeeds if X is a list
      if (args.isEmpty) return GuardResult.failure;
      final val = getValue(args[0]);
      // Empty list: ConstTerm with 'nil' or null
      if (val is ConstTerm && (val.value == 'nil' || val.value == null)) {
        return GuardResult.success;
      }
      // Cons cell: StructTerm with functor '.'
      if (val is StructTerm && val.functor == '.') {
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
      // Succeeds if X is a MutualRefTerm (enables SRSW multiple reads)
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
      // This is handled by the compiler - should not reach runtime
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

      // Add reader to suspension set U and fail → triggers normal suspension
      cx.U.add(readerAddr);
      return GuardResult.failure;

    case 'wait_until':
      // wait_until(Timestamp) - Suspend until absolute time has passed
      // Semantics:
      // - Unbound Timestamp: handled by caller (suspend on reader)
      // - Non-number: fail
      // - current time >= Timestamp: succeed
      // - current time < Timestamp: suspend until time passes (timer-based)
      if (args.isEmpty) return GuardResult.failure;
      final timestamp = evaluateNumeric(args[0]);
      if (timestamp == null) return GuardResult.failure;
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
          cx.U.add(existingReaderWU);
          return GuardResult.failure;
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

      cx.U.add(readerAddrWU);
      return GuardResult.failure;

    case '=?=':
      // Ground equality test
      // Semantics:
      // - Unbound reader: suspend (handled by caller via _dereferenceWithTracking)
      // - Unbound writer: fail
      // - Both ground and equal: succeed
      // - Both ground and not equal: fail
      if (args.length < 2) return GuardResult.failure;
      final left = args[0];
      final right = args[1];

      // Check for unbound writers (VarRef that reached here is unbound writer)
      // Unbound readers would have caused suspension in caller
      if (left is VarRef || right is VarRef) {
        return GuardResult.failure;  // Unbound writer → fail
      }

      // Both ground - check structural equality
      final result = _termsEqual(left, right, cx);
      return result ? GuardResult.success : GuardResult.failure;

    // Attestation guard (madGLP, seam spec §4).
    // valid_attestation(Signer?, PkA?, PkB?, Sig?) holds iff Sig is Signer's
    // valid Ed25519 signature over the canonical serialization of attest(PkA,
    // PkB). Inputs are lowercase-hex string constants (keys 64 chars, signature
    // 128 chars). Any invalid/malformed input, or absence of a network on the
    // context, is guard failure — the guard never aborts. Unbound readers were
    // already suspended by the caller.
    case 'valid_attestation':
      if (args.length != 4) return GuardResult.failure;
      final ctx = cx.rt.madContext;
      if (ctx is! MadContext) return GuardResult.failure;
      final network = ctx.network;
      if (network == null) return GuardResult.failure;

      String? hexConst(dynamic v) {
        if (v is ConstTerm) {
          final cv = v.value;
          return cv is String ? cv : null;
        }
        if (v is String) return v;
        if (v is VarRef) {
          if (cx.rt.heap.isReader(v.addr)) {
            if (!cx.rt.heap.isReaderBound(v.addr)) return null;
            return hexConst(cx.rt.heap.getReaderValue(v.addr));
          }
          final deref = cx.rt.heap.getValue(v.addr);
          return deref == null ? null : hexConst(deref);
        }
        return null;
      }

      Uint8List? hexToBytes(String? hex, int expectedBytes) {
        if (hex == null || hex.length != expectedBytes * 2) return null;
        final out = Uint8List(expectedBytes);
        for (var i = 0; i < expectedBytes; i++) {
          final b = int.tryParse(hex.substring(i * 2, i * 2 + 2), radix: 16);
          if (b == null) return null;
          out[i] = b;
        }
        return out;
      }

      final signerBytes = hexToBytes(hexConst(args[0]), 32);
      final pkAHex = hexConst(args[1]);
      final pkBHex = hexConst(args[2]);
      final sigBytes = hexToBytes(hexConst(args[3]), 64);
      if (signerBytes == null ||
          pkAHex == null ||
          pkBHex == null ||
          sigBytes == null) {
        return GuardResult.failure;
      }

      try {
        final attest =
            StructTerm('attest', [ConstTerm(pkAHex), ConstTerm(pkBHex)]);
        final canonical = ctx.canonicalSerialize(attest);
        final ok = network.verify(
            PubKey(signerBytes), Uint8List.fromList(canonical), sigBytes);
        return ok ? GuardResult.success : GuardResult.failure;
      } catch (_) {
        return GuardResult.failure;
      }

    default:
      print('[WARN] Unknown guard predicate: $predicateName');
      return GuardResult.failure;
  }
}

bool _termsEqual(Object? a, Object? b, RunnerContext cx, [Set<(int, int)>? visited]) {
  visited ??= <(int, int)>{};

  // Handle null
  if (a == null && b == null) return true;
  if (a == null || b == null) return false;

  // Unwrap ConstTerm
  if (a is ConstTerm) a = a.value;
  if (b is ConstTerm) b = b.value;

  // Dereference VarRefs with cycle detection
  if (a is VarRef) {
    final aAddr = a.addr;
    Object? aDeref;
    if (cx.rt.heap.isReader(aAddr)) {
      // Use abstraction methods for imported reader support
      final writerAddr = cx.rt.heap.tryWriterForReader(aAddr);
      if (writerAddr != null && cx.sigmaHat.containsKey(writerAddr)) {
        aDeref = cx.sigmaHat[writerAddr];
      } else if (cx.rt.heap.isReaderBound(aAddr)) {
        aDeref = cx.rt.heap.getReaderValue(aAddr);
      } else {
        return false; // Unbound - can't compare
      }
    } else {
      if (cx.sigmaHat.containsKey(aAddr)) {
        aDeref = cx.sigmaHat[aAddr];
      } else if (cx.rt.heap.isFullyBound(aAddr)) {
        aDeref = cx.rt.heap.getValue(aAddr);
      } else {
        return false; // Unbound writer
      }
    }

    // If b is also a VarRef, check for cycle
    if (b is VarRef) {
      final bAddr = b.addr;
      final pair = (aAddr, bAddr);
      if (visited.contains(pair)) {
        return true; // Cycle detected at corresponding positions - equal
      }
      visited.add(pair);
    }

    return _termsEqual(aDeref, b, cx, visited);
  }
  if (b is VarRef) {
    final bAddr = b.addr;
    Object? bDeref;
    if (cx.rt.heap.isReader(bAddr)) {
      // Use abstraction methods for imported reader support
      final writerAddr = cx.rt.heap.tryWriterForReader(bAddr);
      if (writerAddr != null && cx.sigmaHat.containsKey(writerAddr)) {
        bDeref = cx.sigmaHat[writerAddr];
      } else if (cx.rt.heap.isReaderBound(bAddr)) {
        bDeref = cx.rt.heap.getReaderValue(bAddr);
      } else {
        return false;
      }
    } else {
      if (cx.sigmaHat.containsKey(bAddr)) {
        bDeref = cx.sigmaHat[bAddr];
      } else if (cx.rt.heap.isFullyBound(bAddr)) {
        bDeref = cx.rt.heap.getValue(bAddr);
      } else {
        return false;
      }
    }
    return _termsEqual(a, bDeref, cx, visited);
  }

  // Simple values (numbers, strings)
  if (a is num && b is num) return a == b;
  if (a is String && b is String) return a == b;

  // Structures
  if (a is StructTerm && b is StructTerm) {
    if (a.functor != b.functor) return false;
    if (a.args.length != b.args.length) return false;
    for (int i = 0; i < a.args.length; i++) {
      if (!_termsEqual(a.args[i], b.args[i], cx, visited)) return false;
    }
    return true;
  }

  // Default: use Dart equality
  return a == b;
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
    if (cx.onReduction != null && cx.goalHead != null) {
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
      cx.rt.heap.bindWriterConst(writerAddr, 'nil');
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
    cx.rt.heap.bindWriterConst(writerAddr, 'nil');
    cx.argSlots[argSlot] = VarRef(readerAddr);
    return StepOutcome.advance;
  }

  /// `put_list` (0x33): in BODY, begin building a `[H|T]` structure into argSlot's
  /// writer; subsequent Set* instructions fill the two positions.
  StepOutcome execPutList(RunnerContext cx, int argSlot) {
    if (cx.inBody) {
      final arg = cx.env.arg(argSlot);
      final targetWriterAddr =
          (arg is VarRef && cx.rt.heap.isWriter(arg.addr)) ? arg.addr : null;
      if (targetWriterAddr == null) {
        print('WARNING: PutList argSlot $argSlot has no writer in environment');
        return StepOutcome.advance;
      }
      cx.clauseVars[-1] = targetWriterAddr; // -1 marks structure binding target
      final structArgs = List<Term>.filled(2, ConstTerm(null));
      cx.currentStructure = StructTerm('[|]', structArgs);
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

  /// `unify_void` (0x22): skip (READ) or create fresh unbound (WRITE) [count]
  /// structure positions.
  StepOutcome execUnifyVoid(RunnerContext cx, int count) {
    if (cx.mode == UnifyMode.write) {
      if (cx.currentStructure is _TentativeStruct) {
        final struct = cx.currentStructure as _TentativeStruct;
        for (var i = 0; i < count && cx.S < struct.args.length; i++) {
          struct.args[cx.S] = null; // void / unbound
          cx.S++;
        }
      }
    } else {
      cx.S += count;
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
      if (argSlot >= 0 && argSlot < 10) {
        cx.clauseVars[-2] = argSlot; // remember target slot until complete
      } else {
        cx.clauseVars[argSlot] = VarRef(writerAddr);
      }
      cx.currentStructure =
          StructTerm(functor, List<Term>.filled(arity, ConstTerm(null)));
      cx.S = 0;
      cx.mode = UnifyMode.write;
    } else {
      cx.guardArgSlot = argSlot;
      cx.currentStructure =
          StructTerm(functor, List<Term>.filled(arity, ConstTerm(null)));
      cx.S = 0;
      cx.mode = UnifyMode.write;
    }
    return StepOutcome.advance;
  }

  /// `commit` (0x04): two-phase HEAD resolution then apply σ̂w. First resolve Si
  /// against σ̂w; any reader whose writer is not tentatively bound stays
  /// unresolved → the clause cannot commit, return [StepOutcome.nextClause].
  /// Otherwise convert tentative structures to terms, apply the writer bindings
  /// to the heap (waking suspended goals), reset to BODY phase, and advance.
  StepOutcome execCommit(RunnerContext cx) {
    final resolvedSi = <int>{};
    for (final readerAddr in cx.Si) {
      final writerAddr = cx.rt.heap.tryWriterForReader(readerAddr);
      if (writerAddr == null || !cx.sigmaHat.containsKey(writerAddr)) {
        resolvedSi.add(readerAddr);
      }
    }
    if (resolvedSi.isNotEmpty) {
      cx.U.addAll(resolvedSi);
      cx.Si.clear();
      return StepOutcome.nextClause;
    }
    cx.Si.clear();

    // Convert tentative structures to real Terms before committing.
    final convertedSigmaHat = <int, Object?>{};
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
                termArgs.add(VarRef(resolved.addr + 1));
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
      if (cx.onActivation != null) cx.onActivation!(a);
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

  /// `ground` (0x41): three-valued, with negation. Collect the term's unbound
  /// readers and note any unbound writer. ground(X): ground→advance; unbound
  /// readers (no writer)→suspend on them (nextClause with readers added to U);
  /// unbound writer→fail (nextClause). ~ground(X) inverts the ground/fail ends;
  /// the unbound-reader case still suspends.
  StepOutcome execGround(RunnerContext cx, int varIndex, bool negated) {
    final value = cx.clauseVars[varIndex];
    if (value == null) return StepOutcome.nextClause; // missing var → fail

    final unboundReaders = <int>{};
    final visited = <int>{};
    bool hasUnboundWriter = false;

    void collectUnbound(Object? term) {
      if (term is VarRef && cx.rt.heap.isWriter(term.addr)) {
        final writerAddr = term.addr;
        if (visited.contains(writerAddr)) return;
        visited.add(writerAddr);
        final sigmaBinding = cx.sigmaHat[writerAddr];
        if (sigmaBinding != null) {
          collectUnbound(sigmaBinding);
        } else if (!cx.rt.heap.isFullyBound(writerAddr)) {
          hasUnboundWriter = true;
        } else {
          collectUnbound(cx.rt.heap.getValue(writerAddr));
        }
      } else if (term is VarRef && cx.rt.heap.isReader(term.addr)) {
        final readerAddr = term.addr;
        if (visited.contains(readerAddr)) return;
        visited.add(readerAddr);
        final sigmaBinding = cx.sigmaHat[readerAddr];
        if (sigmaBinding != null) {
          collectUnbound(sigmaBinding);
        } else if (!cx.rt.heap.isReaderBound(readerAddr)) {
          unboundReaders.add(readerAddr);
        } else {
          collectUnbound(cx.rt.heap.getReaderValue(readerAddr));
        }
      } else if (term is StructTerm) {
        for (final arg in term.args) {
          collectUnbound(arg);
        }
      } else if (term is _TentativeStruct) {
        for (final arg in term.args) {
          collectUnbound(arg);
        }
      }
    }

    if (value is int) {
      final sigmaBinding = cx.sigmaHat[value];
      if (sigmaBinding != null) {
        collectUnbound(sigmaBinding);
      } else if (cx.rt.heap.isWriter(value)) {
        if (!cx.rt.heap.isFullyBound(value)) {
          hasUnboundWriter = true;
        } else {
          collectUnbound(cx.rt.heap.getValue(value));
        }
      } else {
        if (!cx.rt.heap.isReaderBound(value)) {
          unboundReaders.add(value);
        } else {
          collectUnbound(cx.rt.heap.getReaderValue(value));
        }
      }
    } else {
      collectUnbound(value);
    }

    if (negated) {
      if (hasUnboundWriter) return StepOutcome.advance; // not ground → succeed
      if (unboundReaders.isNotEmpty) {
        cx.U.addAll(unboundReaders);
        return StepOutcome.nextClause; // suspend
      }
      return StepOutcome.nextClause; // ground → fail
    } else {
      if (hasUnboundWriter) return StepOutcome.nextClause; // fail
      if (unboundReaders.isNotEmpty) {
        cx.U.addAll(unboundReaders);
        return StepOutcome.nextClause; // suspend
      }
      return StepOutcome.advance; // ground → succeed
    }
  }

  /// `known` (0x42): three-valued, with negation. known(X): bound→advance;
  /// unbound reader→suspend; unbound writer→fail. ~known(X) inverts bound/writer
  /// ends. Unlike ground, only X itself is inspected, not its sub-terms.
  StepOutcome execKnown(RunnerContext cx, int varIndex, bool negated) {
    final value = cx.clauseVars[varIndex];
    if (value == null) return StepOutcome.nextClause; // missing var → fail

    bool isKnown = false;
    int? unboundReader;
    bool isUnboundWriter = false;

    if (value is int) {
      if (cx.sigmaHat.containsKey(value)) {
        isKnown = true;
      } else if (cx.rt.heap.isWriter(value)) {
        if (cx.rt.heap.isFullyBound(value)) {
          isKnown = true;
        } else {
          isUnboundWriter = true;
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
      } else {
        isUnboundWriter = true;
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

    if (negated) {
      if (isUnboundWriter) return StepOutcome.advance; // unknown → succeed
      if (unboundReader != null) {
        cx.U.add(unboundReader);
        return StepOutcome.nextClause; // suspend
      }
      return StepOutcome.nextClause; // known → fail
    } else {
      if (isKnown) return StepOutcome.advance;
      if (unboundReader != null) {
        cx.U.add(unboundReader);
        return StepOutcome.nextClause; // suspend
      }
      return StepOutcome.nextClause; // unbound writer → fail
    }
  }

  /// `no_readers` (0x44): collect the term's unbound readers. no_readers(X):
  /// none→advance; some→suspend on them (never fails). ~no_readers(X): some
  /// readers→advance; none→fail. Missing var counts as no readers.
  StepOutcome execNoReaders(RunnerContext cx, int varIndex, bool negated) {
    final value = cx.clauseVars[varIndex];
    if (value == null) {
      return negated ? StepOutcome.nextClause : StepOutcome.advance;
    }

    final readers = <int>{};
    final visited = <int>{};

    void collectReaders(Object? term) {
      if (term is VarRef && cx.rt.heap.isReader(term.addr)) {
        final readerAddr = term.addr;
        if (visited.contains(readerAddr)) return;
        visited.add(readerAddr);
        final sigmaBinding = cx.sigmaHat[readerAddr];
        if (sigmaBinding != null) {
          collectReaders(sigmaBinding);
        } else if (cx.rt.heap.isReaderBound(readerAddr)) {
          collectReaders(cx.rt.heap.getReaderValue(readerAddr));
        } else {
          readers.add(readerAddr);
        }
      } else if (term is VarRef && cx.rt.heap.isWriter(term.addr)) {
        final writerAddr = term.addr;
        if (visited.contains(writerAddr)) return;
        visited.add(writerAddr);
        final sigmaBinding = cx.sigmaHat[writerAddr];
        if (sigmaBinding != null) {
          collectReaders(sigmaBinding);
        } else if (cx.rt.heap.isFullyBound(writerAddr)) {
          collectReaders(cx.rt.heap.getValue(writerAddr));
        }
      } else if (term is StructTerm) {
        for (final arg in term.args) {
          collectReaders(arg);
        }
      } else if (term is _TentativeStruct) {
        for (final arg in term.args) {
          collectReaders(arg);
        }
      }
    }

    if (value is int) {
      final sigmaBinding = cx.sigmaHat[value];
      if (sigmaBinding != null) {
        collectReaders(sigmaBinding);
      } else if (cx.rt.heap.isWriter(value)) {
        if (cx.rt.heap.isFullyBound(value)) {
          collectReaders(cx.rt.heap.getValue(value));
        }
      } else {
        if (visited.contains(value)) {
          // already visited
        } else if (cx.rt.heap.isReaderBound(value)) {
          collectReaders(cx.rt.heap.getReaderValue(value));
        } else {
          readers.add(value);
        }
      }
    } else {
      collectReaders(value);
    }

    if (negated) {
      return readers.isNotEmpty ? StepOutcome.advance : StepOutcome.nextClause;
    } else {
      if (readers.isEmpty) return StepOutcome.advance;
      cx.U.addAll(readers);
      return StepOutcome.nextClause; // suspend (never fails)
    }
  }

  /// `ground_equal` (0x45): X =?= Y. Unbound writer in either → fail; unbound
  /// readers → suspend on them; both ground → compare (negation inverts the
  /// equal/not-equal ends).
  StepOutcome execGroundEqual(
      RunnerContext cx, int leftVarIndex, int rightVarIndex, bool negated) {
    final leftValue = cx.clauseVars[leftVarIndex];
    final rightValue = cx.clauseVars[rightVarIndex];
    if (leftValue == null || rightValue == null) return StepOutcome.nextClause;

    final unboundReaders = <int>{};
    final visited = <int>{};
    bool hasUnboundWriter = false;

    void collectUnbound(Object? term) {
      if (term is VarRef && cx.rt.heap.isWriter(term.addr)) {
        final writerAddr = term.addr;
        if (visited.contains(writerAddr)) return;
        visited.add(writerAddr);
        final sigmaBinding = cx.sigmaHat[writerAddr];
        if (sigmaBinding != null) {
          collectUnbound(sigmaBinding);
        } else if (!cx.rt.heap.isFullyBound(writerAddr)) {
          hasUnboundWriter = true;
        } else {
          collectUnbound(cx.rt.heap.getValue(writerAddr));
        }
      } else if (term is VarRef && cx.rt.heap.isReader(term.addr)) {
        final readerAddr = term.addr;
        if (visited.contains(readerAddr)) return;
        visited.add(readerAddr);
        final sigmaBinding = cx.sigmaHat[readerAddr];
        if (sigmaBinding != null) {
          collectUnbound(sigmaBinding);
        } else if (!cx.rt.heap.isReaderBound(readerAddr)) {
          unboundReaders.add(readerAddr);
        } else {
          collectUnbound(cx.rt.heap.getReaderValue(readerAddr));
        }
      } else if (term is StructTerm) {
        for (final arg in term.args) {
          collectUnbound(arg);
        }
      } else if (term is _TentativeStruct) {
        for (final arg in term.args) {
          collectUnbound(arg);
        }
      } else if (term is int) {
        if (visited.contains(term)) return;
        visited.add(term);
        final sigmaBinding = cx.sigmaHat[term];
        if (sigmaBinding != null) {
          collectUnbound(sigmaBinding);
        } else if (cx.rt.heap.isWriter(term)) {
          if (!cx.rt.heap.isFullyBound(term)) {
            hasUnboundWriter = true;
          } else {
            collectUnbound(cx.rt.heap.getValue(term));
          }
        } else {
          if (!cx.rt.heap.isReaderBound(term)) {
            unboundReaders.add(term);
          } else {
            collectUnbound(cx.rt.heap.getReaderValue(term));
          }
        }
      }
    }

    collectUnbound(leftValue);
    collectUnbound(rightValue);

    if (hasUnboundWriter) return StepOutcome.nextClause; // fail
    if (unboundReaders.isNotEmpty) {
      cx.U.addAll(unboundReaders);
      return StepOutcome.nextClause; // suspend
    }
    final (leftDeref, _) = _dereferenceWithTracking(leftValue, cx);
    final (rightDeref, _) =
        _dereferenceWithTracking(rightValue, cx);
    final areEqual = _termsEqual(leftDeref, rightDeref, cx);
    final success = negated ? !areEqual : areEqual;
    return success ? StepOutcome.advance : StepOutcome.nextClause;
  }

  /// `unknown` (0x43): succeed iff the clause variable is currently unbound (no
  /// σ̂w tentative binding and not heap-bound). A dispatch test; never suspends.
  StepOutcome execUnknown(RunnerContext cx, int varIndex) {
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
  /// unbound (except for `unknown`), suspend on them. Otherwise evaluate via the
  /// runtime guard table; negation inverts success/fail. success→advance,
  /// anything else→nextClause (suspension already handled).
  StepOutcome execGuard(
      RunnerContext cx, String predicateName, int arity, bool negated) {
    final args = <Object?>[];
    final unboundReaders = <int>{};
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

    if (unboundReaders.isNotEmpty && predicateName != 'unknown') {
      cx.U.addAll(unboundReaders);
      return StepOutcome.nextClause; // suspend
    }

    var result = _evaluateGuard(predicateName, args, cx);
    if (negated) {
      if (result == GuardResult.success) {
        result = GuardResult.failure;
      } else if (result == GuardResult.failure) {
        result = GuardResult.success;
      }
    }
    return result == GuardResult.success
        ? StepOutcome.advance
        : StepOutcome.nextClause;
  }

  /// `head_nil` (0x11): match `[]` against the arg (or a clause var when
  /// argSlot ≥ 10). Two-phase: an unbound writer is tentatively bound to nil in
  /// σ̂w (advance); an unbound reader is added to Si (advance — resolved at
  /// commit); a bound non-nil / structure mismatches (nextClause).
  StepOutcome execHeadNil(RunnerContext cx, int argSlot) {
    final bool isClauseVar = argSlot >= 10;
    final arg = isClauseVar ? null : _getArg(cx, argSlot);

    if (isClauseVar) {
      final clauseVarValue = cx.clauseVars[argSlot];
      if (clauseVarValue == null) return StepOutcome.nextClause;
      if (clauseVarValue is ConstTerm) {
        return clauseVarValue.value == 'nil'
            ? StepOutcome.advance
            : StepOutcome.nextClause;
      } else if (clauseVarValue is StructTerm) {
        return StepOutcome.nextClause;
      } else if (clauseVarValue is VarRef) {
        final addr = clauseVarValue.addr;
        if (cx.rt.heap.isWriter(addr)) {
          if (cx.rt.heap.isFullyBound(addr)) {
            final value = cx.rt.heap.getValue(addr);
            return (value is ConstTerm && value.value == 'nil')
                ? StepOutcome.advance
                : StepOutcome.nextClause;
          } else {
            cx.sigmaHat[addr] = ConstTerm('nil');
            return StepOutcome.advance;
          }
        } else {
          if (cx.rt.heap.isReaderBound(addr)) {
            final value = cx.rt.heap.getReaderValue(addr);
            return (value is ConstTerm && value.value == 'nil')
                ? StepOutcome.advance
                : StepOutcome.nextClause;
          } else {
            cx.Si.add(_finalUnboundVar(cx, addr));
            return StepOutcome.advance;
          }
        }
      } else if (clauseVarValue is int) {
        final writerAddr = clauseVarValue;
        if (cx.rt.heap.isFullyBound(writerAddr)) {
          final value = cx.rt.heap.getValue(writerAddr);
          return (value is ConstTerm && value.value == 'nil')
              ? StepOutcome.advance
              : StepOutcome.nextClause;
        } else {
          cx.sigmaHat[writerAddr] = ConstTerm('nil');
          return StepOutcome.advance;
        }
      }
      return StepOutcome.nextClause; // unexpected clauseVar type
    }

    // Regular argument handling
    if (arg == null) return StepOutcome.advance;
    if (arg is VarRef && cx.rt.heap.isValue(arg.addr)) {
      final value = cx.rt.heap.getValue(arg.addr);
      return (value is ConstTerm && value.value == 'nil')
          ? StepOutcome.advance
          : StepOutcome.nextClause;
    }
    if (arg is VarRef && cx.rt.heap.isWriter(arg.addr)) {
      if (cx.rt.heap.isFullyBound(arg.addr)) {
        final value = cx.rt.heap.getValue(arg.addr);
        if (value is ConstTerm && value.value != 'nil') {
          return StepOutcome.nextClause;
        } else if (value is StructTerm) {
          return StepOutcome.nextClause;
        }
      } else {
        cx.sigmaHat[arg.addr] = ConstTerm('nil');
      }
    } else if (arg is VarRef && cx.rt.heap.isReader(arg.addr)) {
      final bound = cx.rt.heap.isReaderBound(arg.addr);
      final value = bound ? cx.rt.heap.getReaderValue(arg.addr) : null;
      if (!bound) {
        cx.Si.add(_finalUnboundVar(cx, arg.addr));
        return StepOutcome.advance;
      } else {
        if (value is ConstTerm && value.value == 'nil') {
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
  /// mode extract the value at S and unify with the clause var (first
  /// occurrence stores, later occurrence must match).
  StepOutcome execHeadVariable(RunnerContext cx, int varIndex, bool isReader) {
    if (cx.mode == UnifyMode.write) {
      if (cx.currentStructure is _TentativeStruct) {
        final struct = cx.currentStructure as _TentativeStruct;
        final existingValue = cx.clauseVars[varIndex];
        if (existingValue != null) {
          if (isReader && existingValue is int) {
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
      if (cx.currentStructure is StructTerm) {
        final struct = cx.currentStructure as StructTerm;
        if (cx.S < struct.args.length) {
          final value = struct.args[cx.S];
          final existingValue = cx.clauseVars[varIndex];
          if (existingValue != null) {
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
      if (deref is VariableEntry || deref is VarRef) {
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
  /// reader → suspend (Si); mismatch → next clause.
  StepOutcome execHeadStructure(
      RunnerContext cx, String functor, int arity, int argSlot) {
    final bool isClauseVar = argSlot >= 10;
    final arg = isClauseVar ? null : _getArg(cx, argSlot);

    if (!isClauseVar && arg == null) {
      return StepOutcome.nextClause;
    }

    if (isClauseVar) {
      final clauseVarValue = cx.clauseVars[argSlot];
      if (clauseVarValue == null) {
        return StepOutcome.nextClause;
      }

      if (clauseVarValue is int) {
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
          if (targetWriterId is int) {
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
            cx.argSlots[cx.guardArgSlot!] = struct;
            cx.currentStructure = null;
            cx.mode = UnifyMode.read;
            cx.S = 0;
            cx.guardArgSlot = null;
          } else {
            final targetWriterId = cx.clauseVars[-1];
            if (targetWriterId is int) {
              cx.rt.heap
                  .bindWriterStruct(targetWriterId, struct.functor, struct.args);

              final targetSlot = cx.clauseVars[-2];
              if (targetSlot is int && targetSlot >= 0 && targetSlot < 10) {
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
  /// unifies it with the subterm per the reader/writer match rules.
  StepOutcome execUnifyVariable(
      RunnerContext cx, int varIndex, bool isReaderMode) {

        if (cx.mode == UnifyMode.write) {
          // WRITE mode: Add variable to structure being built
          if (cx.currentStructure is _TentativeStruct) {
            // HEAD phase tentative structure
            final struct = cx.currentStructure as _TentativeStruct;
            final clauseVarValue = cx.clauseVars[varIndex];

            if (clauseVarValue is VarRef) {
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
            } else if (clauseVarValue is int) {
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

            if (clauseVarValue is VarRef) {
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
            } else if (clauseVarValue is int) {
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
                int? targetWriterAddr;
                if (targetValue is VarRef) {
                  targetWriterAddr = targetValue.addr;
                } else if (targetValue is int) {
                  targetWriterAddr = targetValue;
                }

                if (targetWriterAddr != null) {
                  final acts = cx.rt.heap.bindWriterStruct(targetWriterAddr, struct.functor, struct.args);
                  for (final a in acts) {
                    cx.rt.gq.enqueue(a);
                    if (cx.onActivation != null) cx.onActivation!(a);
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
                    final currentWriterAddrInt = currentWriterId is VarRef ? currentWriterId.addr : (currentWriterId is int ? currentWriterId : null);

                    if (cx.S >= parentStruct.args.length && currentWriterAddrInt != null) {
                      final acts = cx.rt.heap.bindWriterStruct(currentWriterAddrInt, parentStruct.functor, parentStruct.args);
                      for (final a in acts) {
                        cx.rt.gq.enqueue(a);
                        if (cx.onActivation != null) cx.onActivation!(a);
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
                        if (parentTargetSlot is int && parentTargetSlot >= 0 && parentTargetSlot < 10) {
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
                  if (targetSlot is int && targetSlot >= 0 && targetSlot < 10) {
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
                // UnifyReader READ mode logic
                if (value is VarRef && cx.rt.heap.isReader(value.addr)) {
                  // Spec §12.2 Case 2 / §6.3: Reader × Reader = FAIL
                  // A writers substitution cannot make two readers equal.
                  return StepOutcome.nextClause;
                } else if (value is VarRef && cx.rt.heap.isWriter(value.addr)) {
                  // Query has writer, clause expects reader
                  if (existingValue != null) {
                    // Xi already allocated from previous writer occurrence
                    // Bind query writer to existing value (per spec 8.2)
                    if (existingValue is ConstTerm || existingValue is StructTerm) {
                      // Ground value - bind writer directly to it
                      cx.sigmaHat[value.addr] = existingValue;
                    } else if (existingValue is VarRef) {
                      // Existing VarRef - bind writer to reader of it
                      final addr = existingValue.addr;
                      // Per spec v3.2: use readerForWriter() instead of +1 arithmetic
                      final readerAddr = cx.rt.heap.isWriter(addr) ? cx.rt.heap.pairedReaderAddr(addr) : addr;
                      cx.sigmaHat[value.addr] = VarRef(readerAddr);
                    } else if (existingValue is int) {
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
                } else if (value is ConstTerm || value is StructTerm) {
                  // Query has ground term, clause expects reader
                  final (writerAddr, _) = cx.rt.heap.allocateVariable();
                  cx.sigmaHat[writerAddr] = value;
                  cx.clauseVars[varIndex] = writerAddr;
                  cx.S++;
                } else {
                  return StepOutcome.nextClause;
                }
              } else {
                // UnifyWriter READ mode logic
                if (existingValue is int || (existingValue is VarRef && cx.rt.heap.isWriter(existingValue.addr))) {
                  // Clause variable is a fresh variable addr from previous UnifyReader
                  final clauseVarAddr = existingValue is int ? existingValue : (existingValue as VarRef).addr;

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
                  } else if (value is ConstTerm || value is StructTerm) {
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
                  } else if (value is ConstTerm || value is StructTerm) {
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
  /// writer to WRITE, or suspending an unbound reader via U). WRITE mode: create
  /// the nested tentative struct in the parent and descend into it.
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
                // Unbound reader where structure expected
                // Following three-valued unification: suspend on unbound reader
                cx.U.add(value.addr);
                return StepOutcome.nextClause;
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

  /// `get_variable` (load goal arg argSlot into clause var). Writer mode binds
  /// the goal writer/reader/term to the clause var (or to its earlier-occurrence
  /// writer via σ̂w); reader mode has the clause reader observe the goal writer,
  /// failing on reader×reader. Null arg or reader×reader → next clause.
  StepOutcome execGetVariable(
      RunnerContext cx, int varIndex, int argSlot, bool isReaderMode) {
    final arg = _getArg(cx, argSlot);
    if (arg == null) {
      return StepOutcome.nextClause;
    }

        if (!isReaderMode) {
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
            } else if (existing is int) {
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
              } else if (existing is int) {
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
              } else if (existing is int) {
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
            } else if (existing is int) {
              // Bare writer addr - bind it
              cx.sigmaHat[existing] = arg;
            } else {
              cx.clauseVars[varIndex] = arg;
            }
          } else if (arg is StructTerm) {
            if (existing is VarRef && cx.rt.heap.isWriter(existing.addr)) {
              cx.sigmaHat[existing.addr] = arg;
            } else if (existing is int) {
              cx.sigmaHat[existing] = arg;
            } else {
              cx.clauseVars[varIndex] = arg;
            }
          } else if (arg is Term) {
            // Handle other Term types (e.g., MutualRefTerm)
            if (existing is VarRef && cx.rt.heap.isWriter(existing.addr)) {
              cx.sigmaHat[existing.addr] = arg;
            } else if (existing is int) {
              cx.sigmaHat[existing] = arg;
            } else {
              cx.clauseVars[varIndex] = arg;
            }
          }
        } else {
          // GetReaderVariable logic: Load argument into clause READER variable
          final existing = cx.clauseVars[varIndex];

          if (arg is VarRef && cx.rt.heap.isWriter(arg.addr)) {
            // Goal writer → head reader (clause observes goal's variable)
            if (existing != null) {
              // clauseVars already has a value (from earlier occurrence like UnifyVariable)
              // Bind the writer arg to the READER of that value
              // BUG FIX: When existing is a writer VarRef, convert to reader
              if (existing is VarRef && cx.rt.heap.isWriter(existing.addr)) {
                // existing is a writer - bind to its reader
                // Per spec v3.2: use readerForWriter() instead of +1 arithmetic
                cx.sigmaHat[arg.addr] = VarRef(cx.rt.heap.pairedReaderAddr(existing.addr));  // reader addr
              } else if (existing is int) {
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
          } else if (arg is VarRef && cx.rt.heap.isReader(arg.addr)) {
            // Spec §12.2 Case 2: Reader × Reader = FAIL
            // A writers substitution cannot make two readers equal (CGLP Definition 5).
            return StepOutcome.nextClause;
          } else if (arg is ConstTerm) {
            if (existing == null) {
              cx.clauseVars[varIndex] = arg;
            }
          } else if (arg is StructTerm) {
            if (existing == null) {
              cx.clauseVars[varIndex] = arg;
            }
          } else if (arg is Term) {
            // Handle other Term types (e.g., MutualRefTerm)
            if (existing == null) {
              cx.clauseVars[varIndex] = arg;
            }
          }
        }
    return StepOutcome.advance;
  }

  /// `get_value` (unify goal arg argSlot with the already-bound clause var).
  /// Writer mode unifies/binds via σ̂w; reader mode binds the goal writer to the
  /// stored value, suspends (U) on an unbound stored reader, or compares. Null
  /// arg, unset clause var, or any mismatch → next clause.
  StepOutcome execGetValue(
      RunnerContext cx, int varIndex, int argSlot, bool isReaderMode) {

        final arg = _getArg(cx, argSlot);
        if (arg == null) {
          return StepOutcome.nextClause;
        }

        var storedValue = cx.clauseVars[varIndex];
        if (storedValue == null) {
          return StepOutcome.nextClause;
        }

        if (!isReaderMode) {
          // GetWriterValue logic: Unify argument with clause WRITER variable
          // storedValue is already the writer addr (or term)

          if (arg is VarRef && cx.rt.heap.isWriter(arg.addr)) {
            final argBound = cx.rt.heap.isWriterBound(arg.addr);
            if (argBound) {
              final argValue = cx.rt.heap.valueOfWriter(arg.addr);
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
              if (storedValue is int) {
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
              if (storedValue is int) {
                cx.sigmaHat[storedValue] = readerValue;
              } else if (storedValue != readerValue) {
                return StepOutcome.nextClause;
              }
            } else {
              // Reader is unbound - alias storedValue to reader
              // Use tryWriterForReader to get writer if available (local reader)
              final wid = cx.rt.heap.tryWriterForReader(rid);
              if (storedValue is int) {
                if (wid != null) {
                  // Per spec v3.2: use readerForWriter() instead of +1 arithmetic
                  cx.sigmaHat[storedValue] = VarRef(cx.rt.heap.pairedReaderAddr(wid));  // reader addr
                } else {
                  // Imported reader - alias to reader directly
                  cx.sigmaHat[storedValue] = VarRef(rid);
                }
              }
            }
          } else if (arg is ConstTerm) {
            if (storedValue is int) {
              cx.sigmaHat[storedValue] = arg;
            } else if (storedValue is ConstTerm && storedValue.value != arg.value) {
              return StepOutcome.nextClause;
            }
          } else if (arg is StructTerm) {
            if (storedValue is int) {
              cx.sigmaHat[storedValue] = arg;
            } else if (storedValue is StructTerm && storedValue.functor != arg.functor) {
              return StepOutcome.nextClause;
            }
          }
        } else {
          // GetReaderValue logic: Unify argument with clause READER variable
          if (arg is VarRef && cx.rt.heap.isWriter(arg.addr)) {
            // Goal has writer, head has reader - bind goal writer to stored value
            if (storedValue is VarRef) {
              // storedValue is a reader/writer reference - bind goal writer to it
              cx.sigmaHat[arg.addr] = storedValue;
            } else if (storedValue is int) {
              // storedValue is a reader addr - use abstraction methods for imported reader support
              if (cx.rt.heap.isReaderBound(storedValue)) {
                final readerValue = cx.rt.heap.getReaderValue(storedValue);
                cx.sigmaHat[arg.addr] = readerValue;
              } else {
                cx.U.add(storedValue); return StepOutcome.nextClause;
              }
            } else if (storedValue is Term) {
              cx.sigmaHat[arg.addr] = storedValue;
            }
          } else if (arg is VarRef && cx.rt.heap.isReader(arg.addr)) {
            // Use tryWriterForReader for imported reader support
            final wid = cx.rt.heap.tryWriterForReader(arg.addr);
            // For imported readers (wid == null), compare reader addresses directly
            final compareTo = wid ?? arg.addr;
            if (storedValue is int && compareTo != storedValue) {
              return StepOutcome.nextClause;
            }
          } else if (arg is ConstTerm || arg is StructTerm) {
            if (storedValue != arg) {
              return StepOutcome.nextClause;
            }
          }
        }
    return StepOutcome.advance;
  }

  /// `set_variable` (place a clause var into the BODY structure being built).
  /// Mode-adjusts the existing binding (or allocates fresh), and on completion
  /// binds the target writer, restoring/completing parent structures up the
  /// stack and storing the result reader into the target arg slot.
  StepOutcome execSetVariable(RunnerContext cx, int varIndex, bool isReaderMode) {

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
          } else if (existingValue is int) {
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
            int? targetWriterAddr;
            if (targetValue is VarRef) {
              targetWriterAddr = targetValue.addr;
            } else if (targetValue is int) {
              targetWriterAddr = targetValue;
            }

            if (targetWriterAddr != null) {
              final acts = cx.rt.heap.bindWriterStruct(targetWriterAddr, struct.functor, struct.args);
              for (final a in acts) {
                cx.rt.gq.enqueue(a);
                if (cx.onActivation != null) cx.onActivation!(a);
              }

              // SetWriter-specific: Store VarRef in argSlots ONLY if no parent
              // (nested structures should not store until outermost is complete)
              if (!isReaderMode && cx.parentStack.isEmpty) {
                final targetSlot = cx.clauseVars[-2];
                if (targetSlot is int && targetSlot >= 0 && targetSlot < 10) {
                  // Per spec v3.2: use readerForWriter() instead of +1 arithmetic
                  cx.argSlots[targetSlot] = VarRef(cx.rt.heap.pairedReaderAddr(targetWriterAddr));  // reader addr
                  cx.clauseVars.remove(-2);
                }
              }
            }

            // Handle parent structure restoration - pop from stack
            if (cx.parentStack.isNotEmpty && targetWriterAddr is int) {
              final nestedWriterAddr = targetWriterAddr;
              final parent = cx.parentStack.removeLast();
              final parentWriterId = parent.writerId;
              final parentWriterAddrInt = parentWriterId is VarRef ? parentWriterId.addr : (parentWriterId is int ? parentWriterId : null);

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
                final currentWriterAddrInt = currentWriterAddr is VarRef ? currentWriterAddr.addr : (currentWriterAddr is int ? currentWriterAddr : null);

                if (cx.S >= parentStruct.args.length && currentWriterAddrInt != null) {
                  // bindWriterStruct returns activations directly
                  final acts = cx.rt.heap.bindWriterStruct(currentWriterAddrInt, parentStruct.functor, parentStruct.args);
                  for (final a in acts) {
                    cx.rt.gq.enqueue(a);
                    if (cx.onActivation != null) cx.onActivation!(a);
                  }

                  // Check for more ancestors
                  if (cx.parentStack.isNotEmpty) {
                    final ancestor = cx.parentStack.removeLast();
                    if (ancestor.structure is StructTerm) {
                      final ancestorStruct = ancestor.structure as StructTerm;
                      // Use reader address (writer + 1) for structure args
                      ancestorStruct.args[ancestor.s] = VarRef(currentWriterAddrInt + 1);
                    }
                    cx.currentStructure = ancestor.structure;
                    cx.S = ancestor.s + 1;
                    cx.mode = ancestor.mode;
                    cx.clauseVars[-1] = ancestor.writerId;
                  } else {
                    // No more ancestors - store in argSlots and reset
                    final parentTargetSlot = cx.clauseVars[-2];
                    if (parentTargetSlot is int && parentTargetSlot >= 0 && parentTargetSlot < 10) {
                      // Use reader address (writer + 1) for argSlots
                      cx.argSlots[parentTargetSlot] = VarRef(currentWriterAddrInt + 1);
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
              cx.argSlots[argSlot] = VarRef(isReaderMode ? writerAddr + 1 : writerAddr);
            } else {
              // Reader - try to get writer (will be null for imported readers)
              final writerAddr = cx.rt.heap.tryWriterForReader(addr);
              if (writerAddr != null) {
                // Local reader - use writer/reader based on mode
                cx.argSlots[argSlot] = VarRef(isReaderMode ? writerAddr + 1 : writerAddr);
              } else {
                // Imported reader - no local writer
                // Pass reader address directly (can only be used in reader mode)
                cx.argSlots[argSlot] = VarRef(addr);
              }
            }
          }
        } else if (value is int) {
          // Legacy: bare int ID (assumed to be writer addr)
          cx.argSlots[argSlot] = VarRef(isReaderMode ? value + 1 : value);
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
          print('WARNING: PutVariable got unexpected value: $value (isReader=$isReaderMode)');
        }
    return StepOutcome.advance;
  }

  /// `set_constant` (place a constant into the BODY structure being built).
  /// On completion binds the target writer and restores/completes parent
  /// structures up the stack, storing the result reader into the target slot.
  StepOutcome execSetConstant(RunnerContext cx, Object? opValue) {
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
            final targetWriterAddrInt = targetWriterAddr is VarRef ? targetWriterAddr.addr : (targetWriterAddr is int ? targetWriterAddr : null);
            if (targetWriterAddrInt != null) {
              // Bind the writer to the completed structure (returns activations)
              final acts = cx.rt.heap.bindWriterStruct(targetWriterAddrInt, struct.functor, struct.args);
              for (final a in acts) {
                cx.rt.gq.enqueue(a);
                if (cx.onActivation != null) cx.onActivation!(a);
              }
            }

            // Handle parent structure restoration (nested structures) - pop from stack
            if (cx.parentStack.isNotEmpty && targetWriterAddrInt != null) {
              final nestedWriterAddr = targetWriterAddrInt;
              final parent = cx.parentStack.removeLast();
              final parentWriterAddr = parent.writerId;
              // Extract int from parentWriterAddr if it's a VarRef
              final parentWriterAddrInt = parentWriterAddr is VarRef ? parentWriterAddr.addr : (parentWriterAddr is int ? parentWriterAddr : null);

              if (parent.structure is StructTerm) {
                final parentStruct = parent.structure as StructTerm;
                // Use reader address (writer + 1)
                parentStruct.args[parent.s] = VarRef(nestedWriterAddr + 1);
              }

              cx.currentStructure = parent.structure;
              cx.S = parent.s + 1;
              cx.mode = parent.mode;
              cx.clauseVars[-1] = parentWriterAddr;

              // Check if parent is now complete - and recursively complete ancestors
              while (cx.currentStructure is StructTerm) {
                final parentStruct = cx.currentStructure as StructTerm;
                final currentWriterAddr = cx.clauseVars[-1];
                final currentWriterAddrInt = currentWriterAddr is VarRef ? currentWriterAddr.addr : (currentWriterAddr is int ? currentWriterAddr : null);

                if (cx.S >= parentStruct.args.length && currentWriterAddrInt != null) {
                  // bindWriterStruct returns activations directly
                  final acts = cx.rt.heap.bindWriterStruct(currentWriterAddrInt, parentStruct.functor, parentStruct.args);
                  for (final a in acts) {
                    cx.rt.gq.enqueue(a);
                    if (cx.onActivation != null) cx.onActivation!(a);
                  }

                  // Check for more ancestors
                  if (cx.parentStack.isNotEmpty) {
                    final ancestor = cx.parentStack.removeLast();
                    if (ancestor.structure is StructTerm) {
                      final ancestorStruct = ancestor.structure as StructTerm;
                      // Use reader address (writer + 1)
                      ancestorStruct.args[ancestor.s] = VarRef(currentWriterAddrInt + 1);
                    }
                    cx.currentStructure = ancestor.structure;
                    cx.S = ancestor.s + 1;
                    cx.mode = ancestor.mode;
                    cx.clauseVars[-1] = ancestor.writerId;
                  } else {
                    // No more ancestors - store in argSlots and reset
                    final parentTargetSlot = cx.clauseVars[-2];
                    if (parentTargetSlot is int && parentTargetSlot >= 0 && parentTargetSlot < 10) {
                      // Use reader address (writer + 1)
                      cx.argSlots[parentTargetSlot] = VarRef(currentWriterAddrInt + 1);
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

  /// `head_list` (match arg against a `[H|T]` cons cell — like
  /// head_structure for `'[|]'`/2). Bound list → READ mode; unbound writer →
  /// WRITE mode building a tentative cons; unbound reader → suspend (Si, two
  /// phase); non-list → next clause.
  StepOutcome execHeadList(RunnerContext cx, int argSlot) {
        // Match list structure [H|T] with argument
        // Equivalent to HeadStructure('[|]', 2, op.argSlot)
        final arg = _getArg(cx, argSlot);
        if (arg == null) return StepOutcome.advance;

        // Per spec v2.16.3 Section 12.0.1: Handle VarRef pointing to ValueTag cell
        if (arg is VarRef && cx.rt.heap.isValue(arg.addr)) {
          final value = cx.rt.heap.getValue(arg.addr);
          // Check for list structure (functor '.' or '[|]')
          if (value is StructTerm && (value.functor == '.' || value.functor == '[|]') && value.args.length == 2) {
            cx.currentStructure = value;
            cx.S = 0;
            cx.mode = UnifyMode.read;
            return StepOutcome.advance;
          } else {
            // Not a list structure - fail
            return StepOutcome.nextClause;
          }
        }

        if (arg is VarRef && cx.rt.heap.isWriter(arg.addr)) {
          // Writer: create tentative structure in σ̂w
          if (cx.rt.heap.isFullyBound(arg.addr)) {
            // Already bound - check if it's a list structure
            final value = cx.rt.heap.getValue(arg.addr);
            if (value is StructTerm && value.functor == '[|]' && value.args.length == 2) {
              cx.currentStructure = value;
              cx.S = 0;
              cx.mode = UnifyMode.read;
            } else {
              return StepOutcome.nextClause;
            }
          } else {
            // Unbound writer - create tentative structure
            final struct = StructTerm('[|]', []);
            cx.sigmaHat[arg.addr] = struct;
            cx.currentStructure = struct;
            cx.S = 0;
            cx.mode = UnifyMode.write;
          }
        } else if (arg is VarRef && cx.rt.heap.isReader(arg.addr)) {
          // Reader: check if bound, else add to Si (two-phase)
          // Use abstraction methods that work for both local and imported readers
          final bound = cx.rt.heap.isReaderBound(arg.addr);
          final value = bound ? cx.rt.heap.getReaderValue(arg.addr) : null;

          if (!bound) {
            // Unbound reader - add to Si and continue (two-phase)
            final suspendOnVar = _finalUnboundVar(cx, arg.addr);
            cx.Si.add(suspendOnVar);
            return StepOutcome.advance;
          } else {
            // Bound reader - check if it's a list structure
            if (value is StructTerm && value.functor == '[|]' && value.args.length == 2) {
              cx.currentStructure = value;
              cx.S = 0;
              cx.mode = UnifyMode.read;
            } else {
              return StepOutcome.nextClause;
            }
          }
        }
    return StepOutcome.advance;
  }
}
