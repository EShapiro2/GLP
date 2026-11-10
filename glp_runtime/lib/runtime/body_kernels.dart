/// Body kernel infrastructure for GLP arithmetic
///
/// Body kernels are runtime-implemented predicates that:
/// - Execute inline (not spawned as separate goals)
/// - Have two-valued semantics (success or abort)
/// - Are only accessible to system predicates (GLP-Spec appendix-guards,
///   "Naming and admission of body kernels")
/// - Expect all preconditions met (guards should verify before calling)
///
/// Per IGLP app:in-heap, Variable pairs (the cell's tag gives its polarity):
/// - VarRef has only addr field
/// - Use heap.isWriter/isReader to check cell type

import 'dart:convert';
import 'dart:io';
import 'dart:math' as math;
import 'dart:typed_data';

import 'runtime.dart';
import 'terms.dart';
import 'package:glp_runtime/compiler/glp_printer.dart'
    show constantSource, functorNameSource;
import 'machine_state.dart' show GoalRef;
import 'sim_clock.dart' show PendingToken;
import 'package:glp_runtime/multiagent/mad_context.dart';
import 'package:glp_runtime/multiagent/mad_helpers.dart' show GlobalName;
import 'package:glp_runtime/multiagent/glp_network.dart'
    show Application, GlpNetwork, ProximityUnderlay, PubKey, TrustLevel;
import 'package:glp_runtime/multiagent/simulation_network.dart'
    show SimulationNetworkClient;
import 'package:glp_runtime/multiagent/identity.dart' show PersonIdentity;
import 'package:glp_runtime/wire/artefact.dart' show Artefact;
import 'package:glp_runtime/wire/codec.dart';
import 'package:glp_runtime/wire/payload_codec.dart' show PayloadCodec;

/// Result of executing a body kernel
enum BodyKernelResult {
  /// Kernel succeeded - continue execution
  success,

  /// Kernel aborted - fatal error (e.g., type error, unbound reader)
  abort,
}

/// Body kernel function signature
typedef BodyKernel = BodyKernelResult Function(
  GlpRuntime rt,
  List<Object?> args,
);

/// A kernel's abort: [message] printed and [BodyKernelResult.abort] returned.
BodyKernelResult kernelAbort(String message) {
  print(message);
  return BodyKernelResult.abort;
}

/// Registry of body kernels
class BodyKernelRegistry {
  final Map<String, BodyKernel> _kernels = {};

  /// The kernels by name and then arity, for a lookup that builds no string.
  final Map<String, Map<int, BodyKernel>> _byName = {};

  void register(String name, int arity, BodyKernel kernel) {
    _kernels['$name/$arity'] = kernel;
    (_byName[name] ??= <int, BodyKernel>{})[arity] = kernel;
  }

  BodyKernel? lookup(String name, int arity) => _byName[name]?[arity];

  bool has(String name, int arity) => lookup(name, arity) != null;

  Iterable<String> get names => _kernels.keys;
}

/// Register all standard body kernels
void registerStandardBodyKernels(BodyKernelRegistry registry) {
  // Arithmetic operations
  registry.register('_add', 3, addKernel);
  registry.register('_sub', 3, subKernel);
  registry.register('_mul', 3, mulKernel);
  registry.register('_div', 3, divKernel);
  registry.register('_idiv', 3, idivKernel);
  registry.register('_mod', 3, modKernel);

  // Math functions
  registry.register('_abs', 2, absKernel);
  registry.register('_sqrt', 2, sqrtKernel);
  registry.register('_sin', 2, sinKernel);
  registry.register('_cos', 2, cosKernel);
  registry.register('_tan', 2, tanKernel);
  registry.register('_exp', 2, expKernel);
  registry.register('_ln', 2, lnKernel);
  registry.register('_log10', 2, log10Kernel);
  registry.register('_pow', 3, powKernel);
  registry.register('_asin', 2, asinKernel);
  registry.register('_acos', 2, acosKernel);
  registry.register('_atan', 2, atanKernel);

  // Type conversions
  registry.register('_integer', 2, integerKernel);
  registry.register('_real', 2, realKernel);
  registry.register('_round', 2, roundKernel);
  registry.register('_floor', 2, floorKernel);
  registry.register('_ceil', 2, ceilKernel);

  // Structure manipulation
  registry.register('_list_to_tuple', 2, listToTupleKernel);
  registry.register('_tuple_to_list', 2, tupleToListKernel);

  // Text (GLP-Spec appendix-guards, "Text")
  registry.register('_string_concat', 2, stringConcatKernel);
  registry.register('_term_to_string', 2, termToStringKernel);

  // Time operations
  registry.register('_now', 1, nowKernel);
  registry.register('_random', 4, randomKernel);
  registry.register('_split_random_seed', 3, splitRandomSeedKernel);

  // Simulated time (GLP-Spec appendix-guards, "Simulated time")
  registry.register('_start_clock', 1, startClockKernel);
  registry.register('_rated', 2, ratedKernel);
  registry.register('_step_clock', 2, stepClockKernel);

  // MutualRef operations (O(1) stream append)
  registry.register('_allocate_mutual_reference', 2, mutualRefKernel);
  registry.register('_stream_append', 3, streamAppendKernel);
  registry.register('_close_mutual_reference', 1, mutualRefCloseKernel);

  // madGLP kernels
  registry.register('_send', 3, sendKernel);
  registry.register('_authorise_link', 2, authoriseLinkKernel);

  // Signature kernels (GLP-Spec appendix-guards, the Signature rows) and the
  // module decomposition (the Modules rows): the person's key is the
  // runtime's, so none of these needs madGLP mode.
  registry.register('_self_key', 1, selfKeyKernel);
  registry.register('_sign', 3, signKernel);
  registry.register('_signature', 2, signatureKernel);
  registry.register('_decompose_module', 4, decomposeModuleKernel);
  registry.register('_load_file', 2, loadFileKernel);

  // Networking seam kernels (Definition Seam Predicates)
  registry.register('_peer_address', 2, peerAddressKernel);
  registry.register('_peer_app', 2, peerAppKernel);
  registry.register('_self_app', 2, selfAppKernel);
  registry.register('_punch_udp', 1, punchUdpKernel);
  registry.register('_declare', 1, declareKernel);
  registry.register('_remove', 1, removeKernel);

  // Module-as-value: producer half. (`_run`/2 — the consumer — is registered by
  // the engine from engine_v2/module_kernels.dart, where CodeImage is in scope.)
  // The unprefixed `self_module`/`run` are the user-facing system predicates.
  registry.register('_self_module', 1, selfModuleKernel);

  // I/O kernels
  registry.register('_output', 1, outputKernel);
}

/// Helper to get numeric value from argument (with arithmetic evaluation)
num? _getNum(GlpRuntime rt, Object? arg) {
  if (arg is num) return arg;
  if (arg is ConstTerm && arg.value is num) return arg.value as num;
  if (arg is VarRef) {
    final term = rt.heap.getValue(arg.addr);
    return _getNum(rt, term);
  }
  if (arg is StructTerm) {
    return _evaluateArithmetic(rt, arg);
  }
  return null;
}

/// Evaluate arithmetic structure to numeric value
num? _evaluateArithmetic(GlpRuntime rt, StructTerm struct) {
  final args = struct.args.map((a) => _getNum(rt, a)).toList();
  if (args.any((a) => a == null)) return null;

  switch (struct.functor) {
    case '+': return args[0]! + args[1]!;
    case '-': return args[0]! - args[1]!;
    case '*': return args[0]! * args[1]!;
    case '/': return args[1] == 0 ? null : args[0]! / args[1]!;
    case '//': return args[1] == 0 ? null : args[0]! ~/ args[1]!;
    case 'mod': return args[1] == 0 ? null : args[0]! % args[1]!;
    default: return null;
  }
}

/// Helper to bind result to output writer.  An abort names [kernel], the
/// module kernels giving their own name ([registerModuleKernels]).
BodyKernelResult bindResult(GlpRuntime rt, Object? outputArg, Object value,
    {String kernel = 'Body kernel'}) {
  if (outputArg is VarRef && rt.heap.isWriter(outputArg.addr)) {
    final List<GoalRef> activations;
    if (value is Term) {
      activations = rt.heap.bindVariable(outputArg.addr, value);
    } else {
      activations = rt.heap.bindVariableConst(outputArg.addr, value);
    }
    for (final act in activations) {
      rt.enqueueReactivatedGoal(act);
    }
    return BodyKernelResult.success;
  }
  return kernelAbort('[ABORT] $kernel: output argument is not a writer');
}

// ============================================================================
// ARITHMETIC KERNELS
// ============================================================================

BodyKernelResult addKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    return kernelAbort('[ABORT] add/3: expected 3 arguments, got ${args.length}');
  }
  final x = _getNum(rt, args[0]);
  final y = _getNum(rt, args[1]);
  if (x == null || y == null) {
    return kernelAbort('[ABORT] add/3: operands must be numbers');
  }
  return bindResult(rt, args[2], x + y);
}

BodyKernelResult subKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    return kernelAbort('[ABORT] sub/3: expected 3 arguments, got ${args.length}');
  }
  final x = _getNum(rt, args[0]);
  final y = _getNum(rt, args[1]);
  if (x == null || y == null) {
    return kernelAbort('[ABORT] sub/3: operands must be numbers');
  }
  return bindResult(rt, args[2], x - y);
}

BodyKernelResult mulKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    return kernelAbort('[ABORT] mul/3: expected 3 arguments, got ${args.length}');
  }
  final x = _getNum(rt, args[0]);
  final y = _getNum(rt, args[1]);
  if (x == null || y == null) {
    return kernelAbort('[ABORT] mul/3: operands must be numbers');
  }
  return bindResult(rt, args[2], x * y);
}

BodyKernelResult divKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    return kernelAbort('[ABORT] div/3: expected 3 arguments, got ${args.length}');
  }
  final x = _getNum(rt, args[0]);
  final y = _getNum(rt, args[1]);
  if (x == null || y == null) {
    return kernelAbort('[ABORT] div/3: operands must be numbers');
  }
  if (y == 0) {
    return kernelAbort('[ABORT] div/3: division by zero');
  }
  return bindResult(rt, args[2], x / y);
}

BodyKernelResult idivKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    return kernelAbort('[ABORT] idiv/3: expected 3 arguments, got ${args.length}');
  }
  final x = _getNum(rt, args[0]);
  final y = _getNum(rt, args[1]);
  if (x == null || y == null || x is! int || y is! int) {
    return kernelAbort('[ABORT] idiv/3: operands must be integers');
  }
  if (y == 0) {
    return kernelAbort('[ABORT] idiv/3: division by zero');
  }
  return bindResult(rt, args[2], x ~/ y);
}

BodyKernelResult modKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    return kernelAbort('[ABORT] mod/3: expected 3 arguments, got ${args.length}');
  }
  final x = _getNum(rt, args[0]);
  final y = _getNum(rt, args[1]);
  if (x == null || y == null || x is! int || y is! int) {
    return kernelAbort('[ABORT] mod/3: operands must be integers');
  }
  if (y == 0) {
    return kernelAbort('[ABORT] mod/3: modulo by zero');
  }
  return bindResult(rt, args[2], x % y);
}

// ============================================================================
// THE FUNCTIONS OF Exp
// ============================================================================

/// The sixteen unary functions of `Exp` (the root self.glp; TGLP
/// appendix-root-self.tex, 2e39edb) by functor, each as its kernel computes
/// it, `log` being `'_log10'`: its value at a number, or null where the number
/// is outside its domain and the kernel aborts --- "A body kernel whose
/// precondition fails --- a zero divisor, an argument out of its domain ---
/// aborts" (GLP-Spec appendix-guards.tex).
///
/// A real that is NaN or infinite --- `'_pow'`, `'_exp'` and real arithmetic
/// that overflows yield them --- has no integer, so it is outside the domain
/// of `integer`, `round`, `floor` and `ceil`.  Until 2026-10-02 their kernels
/// threw on one and the run ended, its other goals with it:
/// `X := integer(pow(-8, 0.5)), Y := 2 + 2` printed "Unsupported operation:
/// Infinity or NaN toInt" and bound neither.
final Map<String, num? Function(num)> _expUnaryFunctions = {
  'abs': (x) => x.abs(),
  'sqrt': (x) => x < 0 ? null : math.sqrt(x),
  'sin': math.sin,
  'cos': math.cos,
  'tan': math.tan,
  'asin': (x) => x < -1 || x > 1 ? null : math.asin(x),
  'acos': (x) => x < -1 || x > 1 ? null : math.acos(x),
  'atan': math.atan,
  'exp': math.exp,
  'ln': (x) => x <= 0 ? null : math.log(x),
  'log': (x) => x <= 0 ? null : math.log(x) / math.ln10,
  'integer': (x) => x.isFinite ? x.toInt() : null,
  'real': (x) => x.toDouble(),
  'round': (x) => x.isFinite ? x.round() : null,
  'floor': (x) => x.isFinite ? x.floor() : null,
  'ceil': (x) => x.isFinite ? x.ceil() : null,
};

/// Whether [functor] of [arity] arguments is a function of `Exp`: `pow` of
/// two, or one of the sixteen unary functions ([expFunction]).
bool isExpFunction(String functor, int arity) => arity == 1
    ? _expUnaryFunctions.containsKey(functor)
    : arity == 2 && functor == 'pow';

/// The value of the function [functor] of `Exp` at the numbers [args], one
/// per argument, as its kernel computes it --- `pow` as `'_pow'` does, its
/// domain every pair of numbers, and the unary functions as
/// [_expUnaryFunctions] has them --- or null where an argument is outside the
/// function's domain, and where [functor] of that many arguments is no
/// function of `Exp` ([isExpFunction]).
///
/// The kernels compute through it ([_functionKernel]), and so does an
/// arithmetic comparison guard evaluating an `Exp` (bytecode/runner.dart), in
/// which an argument outside a function's domain has no value and fails the
/// guard (GLP-Spec appendix-guards.tex, 026515d): one definition of each
/// function and its domain, so that a comparison and `:=` cannot disagree on
/// one.
num? expFunction(String functor, List<num> args) {
  if (args.length == 2) {
    return functor == 'pow' ? math.pow(args[0], args[1]) : null;
  }
  if (args.length == 1) return _expUnaryFunctions[functor]?.call(args[0]);
  return null;
}

/// The body kernel of the function [functor] of `Exp` of [arity] arguments,
/// the output after them: it binds the output to the function's value at its
/// arguments ([expFunction]), and aborts where it is not given [arity]
/// arguments and the output, where an argument is no number, and where one is
/// outside the function's domain.
BodyKernelResult _functionKernel(
    GlpRuntime rt, List<Object?> args, String functor, int arity) {
  if (args.length != arity + 1) return BodyKernelResult.abort;
  final xs = <num>[];
  for (var i = 0; i < arity; i++) {
    final x = _getNum(rt, args[i]);
    if (x == null) return BodyKernelResult.abort;
    xs.add(x);
  }
  final value = expFunction(functor, xs);
  if (value == null) return BodyKernelResult.abort;
  return bindResult(rt, args[arity], value);
}

// ============================================================================
// MATH FUNCTION KERNELS
// ============================================================================

BodyKernelResult absKernel(GlpRuntime rt, List<Object?> args) =>
    _functionKernel(rt, args, 'abs', 1);

BodyKernelResult sqrtKernel(GlpRuntime rt, List<Object?> args) =>
    _functionKernel(rt, args, 'sqrt', 1);

BodyKernelResult sinKernel(GlpRuntime rt, List<Object?> args) =>
    _functionKernel(rt, args, 'sin', 1);

BodyKernelResult cosKernel(GlpRuntime rt, List<Object?> args) =>
    _functionKernel(rt, args, 'cos', 1);

BodyKernelResult tanKernel(GlpRuntime rt, List<Object?> args) =>
    _functionKernel(rt, args, 'tan', 1);

BodyKernelResult expKernel(GlpRuntime rt, List<Object?> args) =>
    _functionKernel(rt, args, 'exp', 1);

BodyKernelResult lnKernel(GlpRuntime rt, List<Object?> args) =>
    _functionKernel(rt, args, 'ln', 1);

BodyKernelResult log10Kernel(GlpRuntime rt, List<Object?> args) =>
    _functionKernel(rt, args, 'log', 1);

BodyKernelResult powKernel(GlpRuntime rt, List<Object?> args) =>
    _functionKernel(rt, args, 'pow', 2);

BodyKernelResult asinKernel(GlpRuntime rt, List<Object?> args) =>
    _functionKernel(rt, args, 'asin', 1);

BodyKernelResult acosKernel(GlpRuntime rt, List<Object?> args) =>
    _functionKernel(rt, args, 'acos', 1);

BodyKernelResult atanKernel(GlpRuntime rt, List<Object?> args) =>
    _functionKernel(rt, args, 'atan', 1);

// ============================================================================
// TYPE CONVERSION KERNELS
// ============================================================================

BodyKernelResult integerKernel(GlpRuntime rt, List<Object?> args) =>
    _functionKernel(rt, args, 'integer', 1);

BodyKernelResult realKernel(GlpRuntime rt, List<Object?> args) =>
    _functionKernel(rt, args, 'real', 1);

BodyKernelResult roundKernel(GlpRuntime rt, List<Object?> args) =>
    _functionKernel(rt, args, 'round', 1);

BodyKernelResult floorKernel(GlpRuntime rt, List<Object?> args) =>
    _functionKernel(rt, args, 'floor', 1);

BodyKernelResult ceilKernel(GlpRuntime rt, List<Object?> args) =>
    _functionKernel(rt, args, 'ceil', 1);

// ============================================================================
// STRUCTURE MANIPULATION KERNELS
// ============================================================================

/// Helper to fully dereference a term (shallow - only follows top-level VarRef)
Object? _deref(GlpRuntime rt, Object? term) {
  while (term is VarRef) {
    final val = rt.heap.getValue(term.addr);
    if (val == null) return term;
    term = val;
  }
  return term;
}

/// Helper to deeply dereference a term (recursively follows all VarRefs in structure)
///
/// This is required for serialization/globalization where we need the actual
/// heap structure, not VarRef placeholders. Without this, nested structures
/// like `msg(bob, intro(alice, Resp))` would be seen as `msg(VarRef, VarRef)`.
///
/// The copy keeps a stack of its own, a frame for each structure being
/// rebuilt, and takes the arguments of each left to right, as the recursion it
/// replaces did: until 2026-10-02 it recursed once a list element, and
/// `'_output'` of a list of some 10,000 elements and more overflowed the Dart
/// stack.
///
/// Public for the `global_send` callback ([MadContext]), which sends the value as `'_send'` does ([sendKernel]): a reader whose writer is assigned is its value, not a variable of the term sent (IGLP Implementation Notes, "Dereferencing" and "The global_send goal").
Term deepDeref(GlpRuntime rt, Term term) {
  // The term itself dereferenced ([_deref]): its value, or the unbound
  // variable it is.
  final root = _deref(rt, term) as Term;
  if (root is! StructTerm) return root; // ConstTerm or unbound VarRef
  // Each frame: a structure, and its arguments dereferenced so far.
  final frames = <(StructTerm, List<Term>)>[(root, <Term>[])];
  Term? built;
  while (true) {
    final (source, args) = frames.last;
    if (built != null) {
      args.add(built);
      built = null;
    }
    if (args.length == source.args.length) {
      frames.removeLast();
      final copy = StructTerm(source.functor, args);
      if (frames.isEmpty) return copy;
      built = copy;
      continue;
    }
    final next = _deref(rt, source.args[args.length]) as Term;
    if (next is StructTerm) {
      frames.add((next, <Term>[]));
    } else {
      args.add(next);
    }
  }
}

/// Helper to convert Dart list to GLP list structure
Term _dartListToGlpList(List<Object?> items) {
  Term result = ConstTerm(nil);
  for (var i = items.length - 1; i >= 0; i--) {
    final item = items[i];
    final termItem = item is Term ? item : ConstTerm(item);
    result = StructTerm('.', [termItem, result]);
  }
  return result;
}

/// Helper to convert GLP list to Dart list
List<Object?>? _glpListToDartList(GlpRuntime rt, Object? list) {
  final result = <Object?>[];
  var current = _deref(rt, list);

  while (current != null) {
    if (current is ConstTerm && current.value == nil) {
      return result;
    }
    if (current is StructTerm && current.functor == '.' && current.args.length == 2) {
      result.add(_deref(rt, current.args[0]));
      current = _deref(rt, current.args[1]);
    } else {
      return null;
    }
  }
  return result;
}

BodyKernelResult listToTupleKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    return kernelAbort('[ABORT] list_to_tuple/2: expected 2 arguments, got ${args.length}');
  }

  final listArg = _deref(rt, args[0]);
  final items = _glpListToDartList(rt, listArg);

  if (items == null || items.isEmpty) {
    return kernelAbort('[ABORT] list_to_tuple/2: first argument must be a non-empty list');
  }

  final functorTerm = items[0];
  String? functor;
  if (functorTerm is ConstTerm && functorTerm.value is String) {
    functor = functorTerm.value as String;
  } else if (functorTerm is String) {
    functor = functorTerm;
  }

  if (functor == null) {
    return kernelAbort('[ABORT] list_to_tuple/2: first element must be an atom (functor)');
  }

  final structArgs = <Term>[];
  for (var i = 1; i < items.length; i++) {
    final item = items[i];
    structArgs.add(item is Term ? item : ConstTerm(item));
  }

  final tuple = StructTerm(functor, structArgs);
  return bindResult(rt, args[1], tuple);
}

BodyKernelResult tupleToListKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    return kernelAbort('[ABORT] tuple_to_list/2: expected 2 arguments, got ${args.length}');
  }

  final tupleArg = _deref(rt, args[0]);

  if (tupleArg is! StructTerm) {
    return kernelAbort('[ABORT] tuple_to_list/2: first argument must be a structure');
  }

  final items = <Object?>[ConstTerm(tupleArg.functor)];
  for (final arg in tupleArg.args) {
    items.add(_deref(rt, arg));
  }

  final list = _dartListToGlpList(items);
  return bindResult(rt, args[1], list);
}

// ============================================================================
// TIME KERNELS
// ============================================================================

BodyKernelResult nowKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 1) {
    return kernelAbort('[ABORT] now/1: expected 1 argument, got ${args.length}');
  }
  final currentTime = DateTime.now().millisecondsSinceEpoch;
  return bindResult(rt, args[0], currentTime);
}

/// `_self_module`/1 — module-as-value, producer half; the kernel behind the
/// user-facing `self_module` system predicate.
///
/// Binds its single output (`args[0]`, a writer) to the module VALUE the calling
/// goal runs: a [ModuleTerm] carrying that app's artefact — h(M) and code.
/// Following the FCP abstract machine, a goal carries the module value of the
/// app it belongs to, so this returns what the computation already holds. Takes
/// no input. The inverse of `_run`/2.
BodyKernelResult selfModuleKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 1) {
    return kernelAbort('[ABORT] _self_module/1: expected 1 argument, got ${args.length}');
  }
  final goalId = rt.currentGoalId;
  final module = goalId == null ? null : rt.getGoalModule(goalId);
  if (module == null) {
    return kernelAbort('[ABORT] _self_module/1: calling goal carries no module value');
  }
  return bindResult(rt, args[0], module);
}

/// `'_random'(Seed?, N?, Value, NextSeed)` — seeded random integer (GLP-ICLP
/// appendix-guards, "Seeded random numbers"). The generator is SplitMix64
/// (Steele, Lea and Flood 2014, Figure 16), in 64-bit arithmetic modulo 2^64:
/// a seed is a 64-bit integer and every 64-bit integer is a seed; NextSeed is
/// Seed plus the increment 0x9E3779B97F4A7C15; Value is one plus the mix of
/// NextSeed, taken as an unsigned 64-bit integer, modulo N. Dart's int is the
/// paper's Integer, 64-bit two's complement with wrapping `+` and `*`. The
/// system-predicate guard integer(Seed?), integer(N?), N? >= 1 guarantees
/// ground integer inputs with N ≥ 1, so the checks below are defensive.
BodyKernelResult randomKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 4) {
    return kernelAbort('[ABORT] random/4: expected 4 arguments, got ${args.length}');
  }
  final seed = _getNum(rt, args[0]);
  final n = _getNum(rt, args[1]);
  if (seed == null || n == null || seed is! int || n is! int) {
    return kernelAbort('[ABORT] random/4: Seed and N must be integers');
  }
  if (n < 1) {
    return kernelAbort('[ABORT] random/4: bound N must be >= 1');
  }
  final int nextSeed = seed + _splitMixIncrement;
  final int value = 1 + _unsignedRemainder(_mix64(nextSeed), n);
  final r1 = bindResult(rt, args[2], value);
  if (r1 != BodyKernelResult.success) return r1;
  return bindResult(rt, args[3], nextSeed);
}

/// `'_split_random_seed'(Seed?, S1, S2)` — two seeds from one (GLP-ICLP
/// appendix-guards, "Splitting a seed"): S1 is the mix of Seed plus the
/// increment, and S2 the mix of Seed plus twice the increment, the increment
/// and the mix those of random/4. The guard integer(Seed?) guarantees a ground
/// integer, so the check below is defensive.
BodyKernelResult splitRandomSeedKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    print('[ABORT] split_random_seed/3: expected 3 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }
  final seed = _getNum(rt, args[0]);
  if (seed == null || seed is! int) {
    print('[ABORT] split_random_seed/3: Seed must be an integer');
    return BodyKernelResult.abort;
  }
  final int s1 = _mix64(seed + _splitMixIncrement);
  final int s2 = _mix64(seed + 2 * _splitMixIncrement);
  final r1 = bindResult(rt, args[1], s1);
  if (r1 != BodyKernelResult.success) return r1;
  return bindResult(rt, args[2], s2);
}

// ============================================================================
// SIMULATED TIME (start_clock/1, rated/2, step_clock/2)
// ============================================================================
// GLP-Spec appendix-guards.tex at 5fd9a15, "Simulated time (start_clock/1,
// rated/2, step_clock/2)".  The clock and the queue are the machine's,
// [GlpRuntime.simClock].

/// Bind each of [tokens] to stop, every one checked an unbound writer first.
BodyKernelResult _stopAll(GlpRuntime rt, List<PendingToken> tokens, String who) {
  for (final t in tokens) {
    if (rt.heap.isFullyBound(t.writer)) {
      return kernelAbort('[ABORT] $who: a pending token @${t.writer} is already bound');
    }
  }
  for (final t in tokens) {
    for (final act in rt.heap.bindVariable(t.writer, ConstTerm('stop'))) {
      rt.enqueueReactivatedGoal(act);
    }
  }
  return BodyKernelResult.success;
}

/// `'_start_clock'(Seed?)`: "start_clock(Seed) binds every pending token to
/// stop, sets the clock to 0, and seeds the clock's generator with Seed".
BodyKernelResult startClockKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 1) {
    return kernelAbort('[ABORT] start_clock/1: expected 1 argument, got ${args.length}');
  }
  final seed = _getNum(rt, args[0]);
  if (seed is! int) {
    return kernelAbort('[ABORT] start_clock/1: Seed must be an integer');
  }
  final pending = rt.simClock.start(seed);
  return _stopAll(rt, pending, 'start_clock/1');
}

/// `'_rated'(Rate?, Tok)`: "rated(Rate, Tok) makes the token Tok pending, with
/// activation time the clock plus D, where D is drawn from the exponential
/// distribution with rate Rate per second".  The draw: "A draw replaces the
/// state by the state plus the increment, and D = -ln(U)/Rate, where U is one
/// plus the mix of the new state, taken as an unsigned 64-bit integer and
/// shifted right by 11 bits, over 2^53, so that 0 < U <= 1."  Aborts before
/// the first start_clock.
BodyKernelResult ratedKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    return kernelAbort('[ABORT] rated/2: expected 2 arguments, got ${args.length}');
  }
  final clock = rt.simClock;
  if (!clock.started) {
    return kernelAbort('[ABORT] rated/2: the simulated clock is not started (start_clock/1)');
  }
  final rate = _getNum(rt, args[0]);
  if (rate == null || rate <= 0) {
    return kernelAbort('[ABORT] rated/2: Rate must be a number > 0');
  }
  final tok = _deref(rt, args[1]);
  if (tok is! VarRef || !rt.heap.isWriter(tok.addr) || rt.heap.isFullyBound(tok.addr)) {
    return kernelAbort('[ABORT] rated/2: Tok must be an unbound writer');
  }
  clock.state = clock.state + _splitMixIncrement;
  final u = (1 + (_mix64(clock.state) >>> 11)) / _two53;
  final d = -math.log(u) / rate;
  clock.add(d, tok.addr);
  return BodyKernelResult.success;
}

/// 2^53, as a double.
const double _two53 = 9007199254740992.0;

/// `'_step_clock'(Horizon?, Step)`: "Where a token is pending whose activation
/// time T is the least and at most Horizon, the step sets the clock to T,
/// binds that token to go(T) and assigns Step the term released(T); two tokens
/// of one activation time are released in the order in which they were made
/// pending.  Otherwise the step binds every pending token to stop and assigns
/// Step the term stopped(C), C being the clock."  Aborts before the first
/// start_clock.
BodyKernelResult stepClockKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    return kernelAbort('[ABORT] step_clock/2: expected 2 arguments, got ${args.length}');
  }
  final clock = rt.simClock;
  if (!clock.started) {
    return kernelAbort('[ABORT] step_clock/2: the simulated clock is not started (start_clock/1)');
  }
  final horizon = _getNum(rt, args[0]);
  if (horizon == null) {
    return kernelAbort('[ABORT] step_clock/2: Horizon must be a number');
  }
  final step = _deref(rt, args[1]);
  if (step is! VarRef || !rt.heap.isWriter(step.addr) || rt.heap.isFullyBound(step.addr)) {
    return kernelAbort('[ABORT] step_clock/2: Step must be an unbound writer');
  }
  final first = clock.earliest;
  if (first != null && first.time <= horizon) {
    if (rt.heap.isFullyBound(first.writer)) {
      return kernelAbort('[ABORT] step_clock/2: the pending token @${first.writer} is already bound');
    }
    final t = clock.release();
    for (final act in rt.heap.bindVariable(t.writer, StructTerm('go', [ConstTerm(t.time)]))) {
      rt.enqueueReactivatedGoal(act);
    }
    return bindResult(rt, step, StructTerm('released', [ConstTerm(t.time)]),
        kernel: 'step_clock/2');
  }
  final r = _stopAll(rt, clock.takeAll(), 'step_clock/2');
  if (r != BodyKernelResult.success) return r;
  return bindResult(rt, step, StructTerm('stopped', [ConstTerm(clock.clock)]),
      kernel: 'step_clock/2');
}

/// SplitMix64's increment, GOLDEN_GAMMA of the source's Figure 16.
const int _splitMixIncrement = 0x9E3779B97F4A7C15;

/// The source's mix64 (Figure 16): z = (z xor (z >>> 33)) * 0xFF51AFD7ED558CCD;
/// z = (z xor (z >>> 33)) * 0xC4CEB9FE1A85EC53; z xor (z >>> 33), with >>> the
/// unsigned shift. Dart's `>>>` is the unsigned shift and its `*` wraps modulo
/// 2^64.
int _mix64(int z) {
  z = (z ^ (z >>> 33)) * 0xFF51AFD7ED558CCD;
  z = (z ^ (z >>> 33)) * 0xC4CEB9FE1A85EC53;
  return z ^ (z >>> 33);
}

/// The remainder of [x], taken as an unsigned 64-bit integer, divided by
/// [n] ≥ 1. A non-negative [x] is its own unsigned value; a negative [x]
/// stands for x + 2^64, whose remainder is computed from the quotient of
/// x >>> 1, which is non-negative, as in Java's Long.remainderUnsigned.
int _unsignedRemainder(int x, int n) {
  if (x >= 0) return x % n;
  final int q = ((x >>> 1) ~/ n) << 1;
  final int r = x - q * n; // the true remainder, or it plus n; below 2n < 2^64
  return _unsignedLess(r, n) ? r : r - n;
}

/// Whether [a] < [b] as unsigned 64-bit integers.
bool _unsignedLess(int a, int b) =>
    (a ^ 0x8000000000000000) < (b ^ 0x8000000000000000);

// ============================================================================
// MUTUAL REFERENCE KERNELS (O(1) Stream Append)
// ============================================================================

BodyKernelResult mutualRefKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    return kernelAbort('[ABORT] allocate_mutual_reference/2: expected 2 arguments, got ${args.length}');
  }

  final output = _deref(rt, args[1]);
  if (output is! VarRef || !rt.heap.isWriter(output.addr)) {
    return kernelAbort('[ABORT] allocate_mutual_reference/2: second argument must be an unbound writer');
  }

  if (rt.heap.isFullyBound(output.addr)) {
    return kernelAbort('[ABORT] allocate_mutual_reference/2: writer @${output.addr} is already bound');
  }

  final mutualRef = MutualRefTerm(output.addr);
  return bindResult(rt, args[0], mutualRef);
}

BodyKernelResult streamAppendKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    return kernelAbort('[ABORT] kernel_stream_append/3: expected 3 arguments, got ${args.length}');
  }

  final refArg = _deref(rt, args[0]);
  if (refArg is! MutualRefTerm) {
    return kernelAbort('[ABORT] kernel_stream_append/3: first argument must be a MutualRef');
  }

  final currentWriterAddr = refArg.currentWriterAddr;

  if (rt.heap.isFullyBound(currentWriterAddr)) {
    return kernelAbort('[ABORT] kernel_stream_append/3: MutualRef points to already-bound writer @$currentWriterAddr');
  }

  final value = _deref(rt, args[1]);
  final termValue = value is Term ? value : ConstTerm(value);

  // Allocate fresh variable for new tail
  final (newTailWriter, newTailReader) = rt.heap.allocateVariable();

  // Build cons cell: '.'(Value, NewTail?)
  final consCell = StructTerm('.', [termValue, VarRef(newTailReader)]);

  // Bind current writer to the cons cell
  final activations = rt.heap.bindVariable(currentWriterAddr, consCell);

  for (final act in activations) {
    rt.enqueueReactivatedGoal(act);
  }

  // Update MutualRef to point to the new tail's writer
  refArg.currentWriterAddr = newTailWriter;

  return bindResult(rt, args[2], refArg);
}

BodyKernelResult mutualRefCloseKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 1) {
    return kernelAbort('[ABORT] kernel_close_mutual_reference/1: expected 1 argument, got ${args.length}');
  }

  final refArg = _deref(rt, args[0]);
  if (refArg is! MutualRefTerm) {
    return kernelAbort('[ABORT] kernel_close_mutual_reference/1: argument must be a MutualRef');
  }

  final currentWriterAddr = refArg.currentWriterAddr;

  if (rt.heap.isFullyBound(currentWriterAddr)) {
    return kernelAbort('[ABORT] kernel_close_mutual_reference/1: MutualRef points to already-bound writer @$currentWriterAddr');
  }

  final activations = rt.heap.bindVariable(currentWriterAddr, ConstTerm(nil));

  for (final act in activations) {
    rt.enqueueReactivatedGoal(act);
  }

  return BodyKernelResult.success;
}

// ============================================================================
// MADGLP KERNELS
// ============================================================================

/// Send kernel for madGLP
///
/// '_send'(T, G, Q) - sends term T via global name G to agent Q.
/// It is called by the root self.glp's `send_to_net/1` (GLP-Spec
/// appendix-guards, "Output to the network") and `global_send/3` (IGLP
/// Definition "global_send Predicate").
///
/// Per IGLP Definition global_send Predicate (the '_send' builtin) and
/// code-format-fragment.tex (a serializer message to q is
/// _w(q,0) := [T↑ | _w(q,0)]):
/// - Case G = _w(q, 0) (Serializer): wraps T in list [T↑ | _w(q,0)]
/// - Case G = _w(p, i) or _r(p, i) with i > 0: sends T directly
///
/// The global name G determines the routing and message format.
BodyKernelResult sendKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    return kernelAbort('[ABORT] \'_send\'/3: expected 3 arguments, got ${args.length}');
  }

  // Get MadContext from runtime
  final ctx = rt.madContext;
  if (ctx == null) {
    return kernelAbort('[ABORT] \'_send\'/3: not in madGLP mode (no MadContext)');
  }

  // Get term T (first argument)
  // IMPORTANT: Use deepDeref to fully resolve nested structures.
  // _deref only follows top-level VarRefs, leaving nested structure args as VarRefs.
  // This causes serialization bugs where msg(bob, intro(alice, X)) becomes msg(VarRef, VarRef).
  final termArg = deepDeref(rt, args[0] as Term);

  // The global name G (second argument), the 3-ary structure a program
  // names a link by, '_w'(Agent, Epoch, Index) or '_r'(Agent, Epoch, Index)
  // (IGLP cec4682, "Global Variable Names"; the GLP paper 07835e1,
  // appendix-guards.tex, "Output to the network" and "Held links").
  final globalNameArg = _deref(rt, args[1]);
  if (globalNameArg is! StructTerm) {
    return kernelAbort('[ABORT] \'_send\'/3: second argument (G) must be a struct _w/3 or _r/3, got ${globalNameArg.runtimeType}');
  }

  // Parse the global name structure
  final functor = globalNameArg.functor;
  if (functor != '\'_w\'' && functor != '\'_r\'' && functor != '_w' && functor != '_r') {
    return kernelAbort('[ABORT] \'_send\'/3: global name must be _w/3 or _r/3, got $functor');
  }
  if (globalNameArg.args.length != 3) {
    return kernelAbort('[ABORT] \'_send\'/3: global name must have 3 arguments, got ${globalNameArg.args.length}');
  }

  // Extract agent from global name (first arg of _w/_r)
  final gnAgentArg = _deref(rt, globalNameArg.args[0]);
  String? gnAgent;
  if (gnAgentArg is ConstTerm && gnAgentArg.value is String) {
    gnAgent = gnAgentArg.value as String;
  } else if (gnAgentArg is String) {
    gnAgent = gnAgentArg;
  }
  if (gnAgent == null) {
    return kernelAbort('[ABORT] \'_send\'/3: global name agent must be an atom, got $gnAgentArg');
  }

  // Extract the epoch and the index from the global name (second and third
  // args of _w/_r)
  final gnEpoch = _intArg(rt, globalNameArg.args[1]);
  if (gnEpoch == null) {
    return kernelAbort('[ABORT] \'_send\'/3: global name epoch must be a number, got ${_deref(rt, globalNameArg.args[1])}');
  }
  final gnIndex = _intArg(rt, globalNameArg.args[2]);
  if (gnIndex == null) {
    return kernelAbort('[ABORT] \'_send\'/3: global name index must be a number, got ${_deref(rt, globalNameArg.args[2])}');
  }

  // Get destination agent Q (third argument)
  final destArg = _deref(rt, args[2]);
  String? destAgent;
  if (destArg is ConstTerm && destArg.value is String) {
    destAgent = destArg.value as String;
  } else if (destArg is String) {
    destAgent = destArg;
  }
  if (destAgent == null) {
    return kernelAbort('[ABORT] \'_send\'/3: third argument (Q) must be an atom (destination agent), got $destArg');
  }

  // Determine if this is a writer or reader global name
  final isWriter = functor == '\'_w\'' || functor == '_w';

  // Unified send handles both serializer (index 0) and normal (index > 0) cases
  ctx.send(termArg, isWriter, gnAgent, gnEpoch, gnIndex, destAgent);

  return BodyKernelResult.success;
}

/// '_authorise\_link'(L?, A?) — answer a reported held link (Definition
/// authorise\_link Predicate).
///
/// L is the link's global name in its ground presentation — `'_r'(P, E, I)`
/// or `'_w'(P, E, I)`, the same form the `pending_link(G, S)` report carried
/// (IGLP cec4682, "Global Variable Names").  A is
/// the atom `authorise` or `refuse`. The GLP wrapper gates on `known(A?)`, so
/// A is bound here.
///
/// On authorise the runtime releases what it held — a held outgoing message
/// becomes unsent, a retained assignment is applied, a retained request is
/// performed. On refuse it drops the link. A refusal is accepted in every case.
BodyKernelResult authoriseLinkKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    return kernelAbort('[ABORT] \'_authorise_link\'/2: expected 2 arguments, got ${args.length}');
  }
  final ctx = rt.madContext;
  if (ctx is! MadContext) {
    return kernelAbort('[ABORT] \'_authorise_link\'/2: not in madGLP mode (no MadContext)');
  }

  final gn = _parseGroundGlobalName(rt, args[0]);
  if (gn == null) {
    return kernelAbort('[ABORT] \'_authorise_link\'/2: first argument (L) must be '
        "'_r'(P, E, I) or '_w'(P, E, I), got ${_deref(rt, args[0])}");
  }

  final answerArg = _deref(rt, args[1]);
  String? answer;
  if (answerArg is ConstTerm && answerArg.value is String) {
    answer = answerArg.value as String;
  } else if (answerArg is String) {
    answer = answerArg;
  }
  if (answer != 'authorise' && answer != 'refuse') {
    return kernelAbort('[ABORT] \'_authorise_link\'/2: second argument (A) must be '
        'authorise or refuse, got $answerArg');
  }

  ctx.answerLink(gn, answer == 'authorise');
  return BodyKernelResult.success;
}

/// Parse a global name in its ground presentation: the 3-ary structures
/// `'_r'(P, E, I)` and `'_w'(P, E, I)`, "the functor giving the polarity, the
/// first argument the anchor, the second its epoch and the third the index"
/// (IGLP cec4682, "Global Variable Names").  Returns null when [arg] is not
/// one.
GlobalName? _parseGroundGlobalName(GlpRuntime rt, Object? arg) {
  final s = _deref(rt, arg);
  if (s is! StructTerm || s.args.length != 3) return null;
  final functor = s.functor;
  final isWriter = functor == '_w' || functor == '\'_w\'';
  final isReader = functor == '_r' || functor == '\'_r\'';
  if (!isWriter && !isReader) return null;

  final agentArg = _deref(rt, s.args[0]);
  String? agent;
  if (agentArg is ConstTerm && agentArg.value is String) {
    agent = agentArg.value as String;
  } else if (agentArg is String) {
    agent = agentArg;
  }
  if (agent == null) return null;

  final epoch = _intArg(rt, s.args[1]);
  final index = _intArg(rt, s.args[2]);
  if (epoch == null || index == null) return null;

  return isWriter
      ? GlobalName.writer(agent, index, epoch: epoch)
      : GlobalName.reader(agent, index, epoch: epoch);
}

/// The integer [arg] is, dereferenced, or null where it is no number.
int? _intArg(GlpRuntime rt, Object? arg) {
  final v = _deref(rt, arg);
  if (v is num) return v.toInt();
  if (v is ConstTerm && v.value is num) return (v.value as num).toInt();
  return null;
}

/// Lowercase hex of [bytes].
String bytesToHex(List<int> bytes) {
  final sb = StringBuffer();
  for (final b in bytes) {
    sb.write((b & 0xff).toRadixString(16).padLeft(2, '0'));
  }
  return sb.toString();
}

// =============================================================================
// SIGNATURE KERNELS — '_self_key'/1, '_sign'/3, '_signature'/2,
// '_decompose_module'/4 (GLP-Spec appendix-guards, "Identity and signature"
// and "Module as a value"; Secure GLP core.tex §The Seam; IGLP code format
// §Offer and Handshake Messages, "Signed content", and §Program Artefact)
// =============================================================================
//
// The runtime holds one identity for its person (multiagent/identity.dart):
// `self_key/1` answers its public half, `sign/3` signs under it and under no
// other key, and the compiler's certificate is written under it. Keys, hashes
// and signed terms are the catalogue's Key, Hash and SignedTerm — strings —
// carried as lowercase hex: a key is the 32-byte Ed25519 public key, a hash
// the 32-byte SHA-256, and a signed term the byte string S of "Signed content":
// agent (the signer's key), bytes (the signature), then e(sig(HSrc, T)), the
// canonical encoding of the 2-ary structure `sig` whose arguments are the
// source identity of the signing instance's module and the ground term. The
// signature is over those encoded bytes. The runtime supplies HSrc from the
// calling goal's module value; the caller cannot choose it.

Uint8List? _bytesOfHex(String hex) {
  if (hex.length.isOdd) return null;
  final out = Uint8List(hex.length ~/ 2);
  for (var i = 0; i < out.length; i++) {
    final v = int.tryParse(hex.substring(i * 2, i * 2 + 2), radix: 16);
    if (v == null) return null;
    out[i] = v;
  }
  return out;
}

/// The runtime's person identity, or null with a diagnostic printed.
PersonIdentity? _personIdentity(GlpRuntime rt, String kernel) {
  final id = rt.identity;
  if (id == null) {
    print('[ABORT] $kernel: the runtime holds no key for its person');
    return null;
  }
  return id;
}

/// '_self_key'(Key) — bind Key to the public key of the person operating the
/// agent, as 64 lowercase hex characters. Every instance on the machine is
/// assigned the same one.
BodyKernelResult selfKeyKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 1) {
    return kernelAbort('[ABORT] _self_key/1: expected 1 argument, got ${args.length}');
  }
  final id = _personIdentity(rt, '_self_key/1');
  if (id == null) return BodyKernelResult.abort;
  return bindResult(rt, args[0], ConstTerm(id.pub.hex));
}

/// '_sign'(T?, Key?, SignedTerm) — bind SignedTerm to the signed term of the
/// ground term T under Key, which must be a key whose private half the runtime
/// holds for its own person (Secure GLP G2), else an error. The module identity
/// in the signed term is the calling goal's module's source identity, supplied
/// here and not by the caller.
BodyKernelResult signKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    return kernelAbort('[ABORT] _sign/3: expected 3 arguments, got ${args.length}');
  }
  final id = _personIdentity(rt, '_sign/3');
  if (id == null) return BodyKernelResult.abort;

  final key = _groundString(rt, args[1]);
  if (key == null) {
    return kernelAbort('[ABORT] _sign/3: second argument (Key) must be a ground key, got '
        '${_deref(rt, args[1])}');
  }
  if (key != id.pub.hex) {
    return kernelAbort('[ABORT] _sign/3: the runtime holds no private key for $key; it '
        'signs only under ${id.pub.hex}, its own person\'s');
  }

  final goalId = rt.currentGoalId;
  final module = goalId == null ? null : rt.getGoalModule(goalId);
  final artefact = module is ModuleTerm ? module.artefact : null;
  if (artefact is! Artefact) {
    return kernelAbort('[ABORT] _sign/3: the calling goal carries no module value, so the '
        'signature has no module identity');
  }
  final hSrc = artefact.hM;

  final term = deepDeref(rt, args[0] as Term);
  final WireTerm wired;
  try {
    wired = PayloadCodec.termToWire(term);
  } catch (e) {
    return kernelAbort('[ABORT] _sign/3: term is not ground: $e');
  }
  final content =
      encodeTermToBytes(WStruct('sig', [WConst(WBlob(hSrc)), wired]));
  final signature = id.sign(content);

  final w = WireWriter();
  w.bytes(id.pub.bytes);
  w.bytes(signature);
  final signed = Uint8List.fromList([...w.toBytes(), ...content]);
  return bindResult(rt, args[2], ConstTerm(bytesToHex(signed)));
}

/// '_signature'(S?, Signature) — take a signed term apart and report what it
/// finds. Signature is `signed(Key, Hash, T)` — the signer's key, the source
/// identity of the module under which the signature was made, and the term
/// signed — where S is a signed term whose signature verifies under the key it
/// carries, and the constant `unsigned` otherwise: S not a string, not a hex
/// byte string, not a signed term, or one whose signature does not verify.
/// Neither key is taken as input — a reader learns who signed — and the signing
/// module is not run. `unsigned` is a value and not a failure, so the caller
/// discriminates by matching (GLP-Spec appendix-guards, "Identity and
/// signature"): a predicate that failed on a forgery could not refuse one and
/// carry on. The kernel aborts only on a malformed call — the wrong arity, or a
/// first argument that is not known — as the other kernels do.
BodyKernelResult signatureKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    return kernelAbort('[ABORT] _signature/2: expected 2 arguments, got ${args.length}');
  }
  final s = _deref(rt, args[0]);
  if (s == null || s is VarRef) {
    return kernelAbort('[ABORT] _signature/2: first argument is not known');
  }
  return bindResult(rt, args[1], _signatureOf(s));
}

/// The constant `unsigned`: the Signature of what is not a verified signed term.
Term get _unsigned => ConstTerm('unsigned');

/// The Signature of the known term [s]: `signed(Key, Hash, T)` where [s] is a
/// signed term whose signature verifies under the key it carries, `unsigned`
/// otherwise.
Term _signatureOf(Object s) {
  final String hex;
  if (s is ConstTerm && s.value is String) {
    hex = s.value as String;
  } else if (s is String) {
    hex = s;
  } else {
    return _unsigned;
  }
  final bytes = _bytesOfHex(hex);
  if (bytes == null) return _unsigned;

  final PubKey signer;
  final Uint8List signature;
  final Uint8List content;
  final Uint8List hSrc;
  final Term term;
  try {
    final r = WireReader(bytes);
    signer = PubKey(Uint8List.fromList(r.bytes()));
    signature = Uint8List.fromList(r.bytes());
    content = Uint8List.sublistView(bytes, r.offset);
    final sig = decodeTermFromBytes(Uint8List.fromList(content));
    if (sig is! WStruct || sig.functor != 'sig' || sig.args.length != 2) {
      return _unsigned; // the signed content is not sig/2
    }
    final h = sig.args[0];
    if (h is! WConst || h.constant is! WBlob) {
      return _unsigned; // the signed content carries no module identity
    }
    hSrc = (h.constant as WBlob).value;
    term = PayloadCodec.wireToTerm(sig.args[1]);
  } catch (_) {
    return _unsigned; // not a signed term
  }
  if (!PersonIdentity.verify(signer, content, signature)) return _unsigned;
  return StructTerm(
      'signed', [ConstTerm(signer.hex), ConstTerm(bytesToHex(hSrc)), term]);
}

/// '_decompose_module'(Module?, Key, Hash, Hash) — take a module value apart:
/// the public key of the compiler that produced it, the identity of the source
/// it was compiled from, and the identity of the compiled module, read from
/// the artefact's certificate. It does not verify the certificate: a module
/// value exists only where the runtime compiled it or the loader admitted it.
/// A module refused a certificate has no compiler's key to give: an error.
BodyKernelResult decomposeModuleKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 4) {
    return kernelAbort('[ABORT] _decompose_module/4: expected 4 arguments, got '
        '${args.length}');
  }
  final module = _deref(rt, args[0]);
  if (module is! ModuleTerm) {
    return kernelAbort('[ABORT] _decompose_module/4: first argument is not a module value');
  }
  final artefact = module.artefact;
  if (artefact is! Artefact) {
    return kernelAbort('[ABORT] _decompose_module/4: module carries no artefact');
  }
  final cert = artefact.certificate;
  if (cert.isRefused) {
    return kernelAbort('[ABORT] _decompose_module/4: module ${module.name} carries no '
        'certificate (it was refused one, or no one compiled it for a person)');
  }
  final r1 = bindResult(rt, args[1], ConstTerm(bytesToHex(cert.agent)));
  if (r1 != BodyKernelResult.success) return r1;
  final r2 = bindResult(rt, args[2], ConstTerm(bytesToHex(cert.hSrc)));
  if (r2 != BodyKernelResult.success) return r2;
  return bindResult(rt, args[3], ConstTerm(bytesToHex(cert.hBin)));
}

// =============================================================================
// FILE READING — '_load_file'/2 (GLP-Spec appendix-guards, "Compilation and
// file reading"; IGLP code format §Loader)
// =============================================================================

/// '_load_file'(Name?, Content) — read the file Name names and assign Content
/// the module it holds where it is a certified compiled program — its
/// certificate verified as the loader verifies it — and its text otherwise, so
/// a forged artefact is a string and never a Module. Name is resolved within
/// the calling module's own directory, which compilation assigned it: no
/// absolute form, no `..`, and a symbolic link that leaves the directory is
/// refused too. The caller can neither escape the directory nor choose
/// otherwise. A module that arrived as a value has no directory and can read
/// no file.
BodyKernelResult loadFileKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    return kernelAbort('[ABORT] _load_file/2: expected 2 arguments, got ${args.length}');
  }
  final name = _groundString(rt, args[0]);
  if (name == null) {
    return kernelAbort('[ABORT] _load_file/2: first argument (Name) must be a ground '
        'string, got ${_deref(rt, args[0])}');
  }
  final goalId = rt.currentGoalId;
  final module = goalId == null ? null : rt.getGoalModule(goalId);
  final dir = module is ModuleTerm ? module.directory : null;
  if (dir == null) {
    return kernelAbort('[ABORT] _load_file/2: the calling goal\'s module has no directory '
        'to resolve $name in (a module that arrived as a value has none)');
  }
  if (name.isEmpty ||
      name.startsWith('/') ||
      name.split('/').any((seg) => seg == '..' || seg.isEmpty)) {
    return kernelAbort('[ABORT] _load_file/2: $name is not resolved within the calling '
        'module\'s directory: a name is relative to it, has no absolute form, '
        'and does not leave it');
  }
  // After a refused update the agent runs the compiled program the store
  // holds, and load_file/2 reads the files the store holds with it (IGLP
  // 96cd44a, Implementation Notes, "An update"), by the same name.
  final kept = (module as ModuleTerm).keptFiles;
  if (kept != null) {
    final bytes = kept[name];
    if (bytes == null) {
      return kernelAbort('[ABORT] _load_file/2: no file $name among the files '
          'the store keeps with the program of directory $dir');
    }
    return _loadedContent(rt, args[1], bytes, name);
  }

  final file = File('$dir/$name');
  if (!file.existsSync()) {
    return kernelAbort('[ABORT] _load_file/2: no file $name in the calling module\'s '
        'directory $dir');
  }
  final String base;
  final String resolved;
  try {
    base = Directory(dir).resolveSymbolicLinksSync();
    resolved = file.resolveSymbolicLinksSync();
  } catch (e) {
    return kernelAbort('[ABORT] _load_file/2: cannot resolve $name: $e');
  }
  if (!resolved.startsWith('$base${Platform.pathSeparator}')) {
    return kernelAbort('[ABORT] _load_file/2: $name is not resolved within the calling '
        'module\'s directory: it leaves $dir');
  }

  return _loadedContent(rt, args[1], file.readAsBytesSync(), name);
}

/// Assign [content] what a file of [bytes] holds: the module where it is a
/// certified compiled program, its certificate verified as the loader
/// verifies it, and its text otherwise.
BodyKernelResult _loadedContent(
    GlpRuntime rt, Object? content, Uint8List bytes, String name) {
  final artefact = Artefact.certifiedFromBytes(bytes);
  if (artefact != null) {
    return bindResult(
        rt, content, ModuleTerm(artefact, name: artefact.moduleName));
  }
  return bindResult(
      rt, content, ConstTerm(utf8.decode(bytes, allowMalformed: true)));
}

// =============================================================================
// NETWORKING SEAM KERNELS (Definition Seam Predicates)
// =============================================================================
//
// The five kernels behind the five seam predicates (IGLP, Definition "Seam Predicates", 3b736bb, peer_app/2 since 4a334c2; GLP-Spec appendix-guards, "Networking seam", 1ce0263, peer_app/2 since 5fab8ac): peer_address/2, peer_app/2, punch_udp/1, declare/1 and remove/1, '_declare' and '_remove' taking each alternative of Declaration and Removal to the layer function behind it.  They replace, since 2026-11-03, the five kernels of place_declare/3, place_remove/1, trust_declare/2, peer_declare/1 and address_declare/2.  Each reaches the networking layer through MadContext.network, a GlpNetwork; the layer functions are GLP-Networking-API's, specified with the seam contract.  Their effects lie outside the madGLP transition system, as sign's and self_module's do.
//
// Each GLP clause gates its arguments on ground/1, all but a place's stream, so they are ground here.  A realization that does not provide the function throws UnsupportedError, which is a violated precondition and so an abort.
//
// Where no layer is bound at all --- the REPL, a test run without the simulation --- the kernel stands in for a layer that observes nothing and does nothing, and none aborts for want of a layer (IGLP appendix-implementation-notes.tex, "One interface"): "peer_address assigns none, a place declaration records the place and assigns its stream unobservable, the stream closed by its removal or a further declaration as ever, the closing being the runtime's and not the layer's, and the other seam predicates return having done nothing".  peer_app/2, which the passage does not name, assigns none likewise: a layer that observes nothing has no session with any peer.  An argument the predicate does not take is still a violated precondition and an abort, layer or none.

/// The [MadContext] and the [GlpNetwork] bound to it, or null where no layer
/// is bound at all: no MadContext, or one with no GlpNetwork.
({MadContext ctx, GlpNetwork network})? _seamLayer(GlpRuntime rt) {
  final ctx = rt.madContext;
  if (ctx is! MadContext) return null;
  final network = ctx.network;
  if (network == null) return null;
  return (ctx: ctx, network: network);
}

/// The ground constant string at [arg], or null if it is not one.
String? _groundString(GlpRuntime rt, Object? arg) {
  final v = _deref(rt, arg);
  if (v is ConstTerm && v.value is String) return v.value as String;
  if (v is String) return v;
  return null;
}

/// '_peer_address'(P?, A) — assign A `address(S)`, S the address at which the
/// layer observes peer P, or `none` where it observes no address for P
/// (GLP-Spec appendix-guards, "Networking seam", 090e647: `peer_address(Key?,
/// PeerAddress)`, `PeerAddress ::= address(String) ; none`; IGLP, Definition
/// "Seam Predicates", 72b5efa).  `none` is a value and not a failure, as
/// signature/2's `unsigned` is.  Until 2026-10-09 the kernel bound the bare
/// address and aborted where the layer observed none.  P is the peer's name,
/// which over a real network is its public key (§Agent Names), presented as 64
/// lowercase hex characters.  Where no layer is bound, A is `none` whatever P
/// names: a layer that observes nothing observes no address for any peer
/// ("One interface", 6ba0329).
BodyKernelResult peerAddressKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    return kernelAbort('[ABORT] \'_peer_address\'/2: expected 2 arguments, got ${args.length}');
  }

  final name = _groundString(rt, args[0]);
  if (name == null) {
    return kernelAbort('[ABORT] \'_peer_address\'/2: first argument (P) must be a ground '
        'constant naming a peer, got ${_deref(rt, args[0])}');
  }

  final seam = _seamLayer(rt);
  if (seam == null) return bindResult(rt, args[1], ConstTerm('none'));

  final PubKey pk;
  try {
    pk = PubKey.fromHex(name);
  } catch (e) {
    return kernelAbort('[ABORT] \'_peer_address\'/2: peer name $name is not a 64-character '
        'hex public key: $e');
  }

  final String? address;
  try {
    address = seam.network.observedPeerAddress(pk);
  } on UnsupportedError catch (e) {
    return kernelAbort('[ABORT] \'_peer_address\'/2: ${e.message}');
  }
  return bindResult(
      rt,
      args[1],
      address == null
          ? ConstTerm('none')
          : StructTerm('address', [ConstTerm(address)]));
}

/// '_peer_app'(P?, A) — assign A `app(D, N, V)`, the developer D, the name N
/// and the version number V of the app attested in the layer's session with
/// peer P, `unattested` where that session is unattested, or `none` where the
/// layer has no session with P (GLP-Spec appendix-guards, "Networking seam",
/// 5fab8ac: `peer_app(Key?, App)`, `App ::= app(String, String, Integer) ;
/// unattested ; none`; IGLP, Definition "Seam Predicates", 4a334c2).  The
/// kernel reads what the layer reported with `onPeerConnected(pk, underlay,
/// application?)` (GNA api.tex, Section 2.3), which the runtime records
/// ([MadContext.peerSession]): the application where the session is attested,
/// null where it is unattested; a peer the layer has reported connected over
/// no underlay it has not since reported disconnected has no session.  Like
/// `none` of peer_address/2, `none` and `unattested` are values and not
/// failures.  P is the peer's name, its key, or in a simulation a readable
/// name standing for one ([_peerKey]).  Where no layer is bound, A is `none`
/// whatever P names: a layer that observes nothing has no session with any
/// peer ("One interface").
BodyKernelResult peerAppKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    return kernelAbort('[ABORT] \'_peer_app\'/2: expected 2 arguments, got ${args.length}');
  }

  final name = _groundString(rt, args[0]);
  if (name == null) {
    return kernelAbort('[ABORT] \'_peer_app\'/2: first argument (P) must be a ground '
        'constant naming a peer, got ${_deref(rt, args[0])}');
  }

  final seam = _seamLayer(rt);
  if (seam == null) return bindResult(rt, args[1], ConstTerm('none'));

  final pk = _peerKey(seam.network, name);
  if (pk == null) {
    return kernelAbort('[ABORT] \'_peer_app\'/2: $name names no peer: neither a '
        '64-character hex public key nor an agent the layer names');
  }

  final session = seam.ctx.peerSession(pk);
  final Term app;
  if (session == null) {
    app = ConstTerm('none');
  } else {
    final Application? a = session.application;
    app = a == null
        ? ConstTerm('unattested')
        : StructTerm('app', [
            ConstTerm(a.developer),
            ConstTerm(a.name),
            ConstTerm(a.version),
          ]);
  }
  return bindResult(rt, args[1], app);
}

/// '_self_app'(R, I) — assign R `app(D, N, V)` for the app whose program the
/// agent runs, the app the layer attests to the agent's peers, and I the same
/// for the app installed on the device; the two differ after an update the
/// runtime refused (GLP-Spec cce13ce, appendix-guards, "Networking seam":
/// `self_app(App, App)`; IGLP 59c40e2, Definition "Seam Predicates":
/// `self_app(R?, I?) :- '_self_app'(R, I)`).  The kernel reads what the host
/// gave the runtime ([GlpRuntime.selfApps]): the app the store keeps with the
/// program where the update was refused, and the installed app otherwise.
/// Where the host gave no app, as the REPL and the simulation give none,
/// `none`.
BodyKernelResult selfAppKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    return kernelAbort(
        '[ABORT] \'_self_app\'/2: expected 2 arguments, got ${args.length}');
  }
  final apps = rt.selfApps;
  final r = bindResult(rt, args[0], _appTerm(apps?.running));
  if (r != BodyKernelResult.success) return r;
  return bindResult(rt, args[1], _appTerm(apps?.installed));
}

/// `app(D, N, V)` of [a], or `none` where there is none.
Term _appTerm(Application? a) => a == null
    ? ConstTerm('none')
    : StructTerm('app',
        [ConstTerm(a.developer), ConstTerm(a.name), ConstTerm(a.version)]);

/// '_punch_udp'(A?) — open a path to address A and return nothing.  Where no
/// layer is bound it returns having done nothing ("One interface", 6ba0329).
BodyKernelResult punchUdpKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 1) {
    return kernelAbort('[ABORT] \'_punch_udp\'/1: expected 1 argument, got ${args.length}');
  }

  final address = _groundString(rt, args[0]);
  if (address == null) {
    return kernelAbort('[ABORT] \'_punch_udp\'/1: argument (A) must be a ground constant '
        'address, got ${_deref(rt, args[0])}');
  }

  final seam = _seamLayer(rt);
  if (seam == null) return BodyKernelResult.success;

  try {
    seam.network.punchUdp(address);
  } on UnsupportedError catch (e) {
    return kernelAbort('[ABORT] \'_punch_udp\'/1: ${e.message}');
  }
  return BodyKernelResult.success;
}

/// The key of the peer a program names in a seam predicate's `Key` argument
/// [name], or null where [name] names none.  Over a real network an agent's
/// name is its public key (IGLP, Remark "Agent Names"), presented as 64 hex
/// characters; in a simulation a readable name standing for a key names the
/// same peer, "the seam resolves both to the one agent" (IGLP 4f84612,
/// Implementation Notes, "Simulation realisation"), through the
/// identifier--key directory its router owns, as its hosts resolve a
/// message's destination ([NetworkDirectory.agentNamed]).
PubKey? _peerKey(GlpNetwork network, String name) {
  if (network is SimulationNetworkClient) {
    final pk = network.directory.pkOf(name);
    if (pk != null) return pk;
  }
  if (!RegExp(r'^[0-9a-fA-F]{64}$').hasMatch(name)) return null;
  return PubKey.fromHex(name);
}

/// '_declare'(D?) — make the declaration D to the networking layer or the platform, and assign nothing but a place's stream (GLP-Spec appendix-guards, "Networking seam", 1ce0263: `Declaration ::= peer(Key) ; address(Key, String) ; rendezvous(Key, String) ; trust(Underlay, TrustLevel) ; place(Constant, Number, Stream(PlaceEvent)?)`; IGLP, Definition "Seam Predicates", 3b736bb).  Each alternative reaches the layer function behind it (GNA, Sections 2.4, 3 and 4.2): `peer(P)` [GlpNetwork.putKnownPeer], `address(P, A)` [GlpNetwork.putPeerAddress], `rendezvous(P, A)` [GlpNetwork.putRendezvousServer], `trust(U, L)` [GlpNetwork.setTrustLevel], `place(P, R, E)` [GlpNetwork.declarePlace] through [MadContext.declarePlace], which grows E, `nearby(U, S)` through [MadContext.declareNearby], which grows S from what the layer reports, `onPeerDiscovered` and `onPeerLost`, and calls no layer function (GLP-Spec 210cee1; IGLP b20951d), and `reachable(S)` through [MadContext.declareReachable], which grows S from what the layer reports, `onPeerConnected` and `onPeerDisconnected` over the WAN, and calls no layer function (GLP-Spec cb9df43; IGLP 0c77e47).  A term no alternative of Declaration matches is a violated precondition, and so an abort; the type checker refuses it in a program.  Where no layer is bound the declaration returns having done nothing, but for a place, whose stream is the runtime's ("One interface", 6ba0329).
BodyKernelResult declareKernel(GlpRuntime rt, List<Object?> args) {
  const who = "'_declare'/1";
  if (args.length != 1) {
    return kernelAbort('[ABORT] $who: expected 1 argument, got ${args.length}');
  }
  final d = _deref(rt, args[0]);
  final a = d is StructTerm ? d.args : const <Term>[];
  final form = d is StructTerm ? '${d.functor}/${a.length}' : null;
  switch (form) {
    case 'peer/1':
      return _declarePeer(rt, a);
    case 'address/2':
      return _declareAddress(rt, a);
    case 'rendezvous/2':
      return _declareRendezvous(rt, a);
    case 'trust/2':
      return _declareTrust(rt, a);
    case 'place/3':
      return _declarePlace(rt, a);
    case 'nearby/2':
      return _declareNearby(rt, a);
    case 'reachable/1':
      return _declareReachable(rt, a);
  }
  return kernelAbort('[ABORT] $who: argument (D) must be a Declaration --- '
      'peer(P), address(P, A), rendezvous(P, A), trust(U, L), place(P, R, E), '
      'nearby(U, S) or reachable(S) --- got $d');
}

/// '_remove'(R?) — end the declaration R, and assign nothing (GLP-Spec appendix-guards, "Networking seam", 1ce0263: `Removal ::= peer(Key) ; rendezvous(Key) ; place(Constant)`; IGLP, Definition "Seam Predicates", 3b736bb): `peer(P)` [GlpNetwork.removeKnownPeer], `rendezvous(P)` [GlpNetwork.removeRendezvousServer], `place(P)` [GlpNetwork.removePlace] through [MadContext.removePlace], which closes its stream, `nearby(U)`, which closes the stream of the nearby declaration on U (GLP-Spec 210cee1), and `reachable`, which closes the stream of the reachable declaration (GLP-Spec cb9df43).  A term no alternative of Removal matches is a violated precondition, and so an abort.  Where no layer is bound it returns having done nothing, but for a place, whose stream it closes all the same ("One interface", 6ba0329).
BodyKernelResult removeKernel(GlpRuntime rt, List<Object?> args) {
  const who = "'_remove'/1";
  if (args.length != 1) {
    return kernelAbort('[ABORT] $who: expected 1 argument, got ${args.length}');
  }
  final r = _deref(rt, args[0]);
  final a = r is StructTerm ? r.args : const <Term>[];
  final form = r is StructTerm
      ? '${r.functor}/${a.length}'
      : _groundString(rt, r) == 'reachable'
          ? 'reachable/0'
          : null;
  switch (form) {
    case 'peer/1':
      return _removePeer(rt, a);
    case 'rendezvous/1':
      return _removeRendezvous(rt, a);
    case 'place/1':
      return _removePlace(rt, a);
    case 'nearby/1':
      return _removeNearby(rt, a);
    case 'reachable/0':
      return _removeReachable(rt);
  }
  return kernelAbort('[ABORT] $who: argument (R) must be a Removal --- '
      'peer(P), rendezvous(P), place(P), nearby(U) or reachable --- got $r');
}

/// Call the layer function [call] with the layer bound to [rt] and the key a ground [arg] names in it, for the alternative [what] of [who].  It returns having done nothing where no layer is bound; an [arg] that is not a ground constant, or that names no peer, is a violated precondition, and so an abort.
BodyKernelResult _peerCall(GlpRuntime rt, String who, String what, Object? arg,
    void Function(GlpNetwork network, PubKey pk) call) {
  final name = _groundString(rt, arg);
  if (name == null) {
    return kernelAbort('[ABORT] $who: $what: the key (P) must be a ground '
        'constant naming a peer, got ${_deref(rt, arg)}');
  }
  final seam = _seamLayer(rt);
  if (seam == null) return BodyKernelResult.success;
  final pk = _peerKey(seam.network, name);
  if (pk == null) {
    return kernelAbort('[ABORT] $who: $what: $name names no peer: neither a '
        '64-character hex public key nor an agent the layer names');
  }
  return _layerCall(who, what, () => call(seam.network, pk));
}

/// Run the layer function [call], an [UnsupportedError] from it an abort of [who] and [what].
BodyKernelResult _layerCall(String who, String what, void Function() call) {
  try {
    call();
  } on UnsupportedError catch (e) {
    return kernelAbort('[ABORT] $who: $what: ${e.message}');
  }
  return BodyKernelResult.success;
}

/// `declare(peer(P))`: give the layer the public key P of a peer the program knows, `putKnownPeer(pk)` (GLP-Spec appendix-guards, "Networking seam": "peer(P) gives the networking layer the public key P of a peer the program knows").  The known-peer set is what Closed trust recognises (GNA, "Known Peers"); in the simulation the router answers a first contact from a declared peer under closed ("Simulation realisation").
BodyKernelResult _declarePeer(GlpRuntime rt, List<Term> a) {
  const who = "'_declare'/1", what = 'peer(P)';
  return _peerCall(rt, who, what, a[0],
      (network, pk) {
    network.putKnownPeer(pk);
    rt.seamDeclarations.peer(pk.hex);
  });
}

/// `declare(address(P, A))`: give the layer the address A at which peer P is reached, `putPeerAddress(pk, address)` ("address(P, A) gives the layer the address A at which peer P is reached").  The layer records it with the known peer (GNA, Section 4.2); the simulation realisation takes it to no effect, "the effect of an address or a rendezvous declaration" not being provided ("Simulation realisation").
BodyKernelResult _declareAddress(GlpRuntime rt, List<Term> a) {
  const who = "'_declare'/1", what = 'address(P, A)';
  final address = _groundString(rt, a[1]);
  if (address == null) {
    return kernelAbort('[ABORT] $who: $what: the address (A) must be a ground '
        'constant, got ${_deref(rt, a[1])}');
  }
  return _peerCall(rt, who, what, a[0],
      (network, pk) {
    network.putPeerAddress(pk, address);
    rt.seamDeclarations.address(pk.hex, address);
  });
}

/// `declare(rendezvous(P, A))`: give the layer a rendezvous server, the agent of key P at address A, or replace its address, `putRendezvousServer(pk, address)` (GLP-Spec appendix-guards, "Networking seam"; GNA 776016c, Section 4.2, "Declare server": "behind declare(rendezvous(Key, Address))").  The simulation realisation takes it to no effect ("Simulation realisation").
BodyKernelResult _declareRendezvous(GlpRuntime rt, List<Term> a) {
  const who = "'_declare'/1", what = 'rendezvous(P, A)';
  final address = _groundString(rt, a[1]);
  if (address == null) {
    return kernelAbort('[ABORT] $who: $what: the address (A) must be a ground '
        'constant, got ${_deref(rt, a[1])}');
  }
  return _peerCall(rt, who, what, a[0],
      (network, pk) {
    network.putRendezvousServer(pk, address);
    rt.seamDeclarations.rendezvous(pk.hex, address);
  });
}

/// `declare(trust(U, L))`: set the cold-call trust level of proximity underlay U, pan or lan, to L, open or closed, `setTrustLevel(underlay, level)` (GLP-Spec appendix-guards, "Proximity trust": "Both underlays stand at closed until declared, a later declaration of an underlay replaces the level then standing, and the two are declared independently").  An underlay or level the layer does not have is a violated precondition, and so an abort.
BodyKernelResult _declareTrust(GlpRuntime rt, List<Term> a) {
  const who = "'_declare'/1", what = 'trust(U, L)';
  final underlay = switch (_groundString(rt, a[0])) {
    'pan' => ProximityUnderlay.pan,
    'lan' => ProximityUnderlay.lan,
    _ => null,
  };
  if (underlay == null) {
    return kernelAbort('[ABORT] $who: $what: the underlay (U) must be pan or '
        'lan, got ${_deref(rt, a[0])}');
  }
  final level = switch (_groundString(rt, a[1])) {
    'open' => TrustLevel.open,
    'closed' => TrustLevel.closed,
    _ => null,
  };
  if (level == null) {
    return kernelAbort('[ABORT] $who: $what: the level (L) must be open or '
        'closed, got ${_deref(rt, a[1])}');
  }
  final seam = _seamLayer(rt);
  if (seam == null) return BodyKernelResult.success;
  return _layerCall(who, what, () {
    seam.network.setTrustLevel(underlay, level);
    rt.seamDeclarations.trust(underlay.name, level.name);
  });
}

/// The name the layer is given for the place a ground constant [arg] names: the constant's printed form, as [formatTerm] prints it, `home` for home and `7` for 7 (IGLP 0e1c9e0, Implementation Notes, "The contract": "The runtime gives the layer a place's name, a constant, as its printed form"; GLP-Spec appendix-guards, "Networking seam": `place(Constant, Number, Stream(PlaceEvent)?)`).  The printed form reads back as the constant, so two constants are two places, 7 and '7' among them.  Null where [arg] is not a ground constant.  Until 2026-11-03 the name was taken only from an atom or a string, and declare(place(7, 100, E)) checked and then aborted.
String? _placeName(GlpRuntime rt, Object? arg) {
  final v = _deref(rt, arg);
  if (v is ConstTerm) return formatTerm(v);
  if (v is String) return constantSource(v);
  return null;
}

/// `declare(place(P, R, E))`: declare place P of radius R about this agent's location at the time of the call, and grow this agent's own event stream for P from E's writer.  The layer answers with the centre it registered P about, which the runtime keeps with the standing declaration and declares P about again at a restart ([MadContext.keepPlaceCentre]; IGLP 96cd44a, Implementation Notes, "Persistence"); no program reads it.  The stream is fed serializer-fashion and closes only on `remove(place(P))` or a superseding declaration (IGLP, madglp-spec.tex, after Definition "Seam Predicates").
///
/// Where no layer is bound, E is assigned `[unobservable | E']` and nothing more is reported ("One interface", 6ba0329): no event and no `observable` ever follows.  The declaration stands in the runtime's bookkeeping ([GlpRuntime.placeStreams]) as it does with a layer, so its removal or a further declaration closes the stream, E' assigned `[]` (GLP, 2026-10-11 12:59 UTC, "23:12" (a)).
BodyKernelResult _declarePlace(GlpRuntime rt, List<Term> a) {
  const who = "'_declare'/1", what = 'place(P, R, E)';
  final place = _placeName(rt, a[0]);
  if (place == null) {
    return kernelAbort('[ABORT] $who: $what: the place (P) must be a ground '
        'constant, got ${_deref(rt, a[0])}');
  }
  final radius = _getNum(rt, a[1]);
  if (radius == null) {
    return kernelAbort('[ABORT] $who: $what: the radius (R) must be a ground '
        'number of metres, got ${_deref(rt, a[1])}');
  }
  final stream = _deref(rt, a[2]);
  if (stream is! VarRef || !rt.heap.isWriter(stream.addr)) {
    return kernelAbort('[ABORT] $who: $what: the stream (E) must be a writer');
  }

  final seam = _seamLayer(rt);
  if (seam == null) {
    rt.placeStreams.declare(place, stream.addr);
    rt.placeStreams.deliver(place, ConstTerm('unobservable'));
    rt.seamDeclarations.place(place, radius);
    return BodyKernelResult.success;
  }
  return _layerCall(who, what, () {
    seam.ctx.declarePlace(place, radius.toDouble(), stream.addr);
    rt.seamDeclarations.place(place, radius);
  });
}

/// The proximity underlay a ground constant [arg] names, `pan` or `lan`, or null where it names neither.
ProximityUnderlay? _proximityUnderlay(GlpRuntime rt, Object? arg) =>
    switch (_groundString(rt, arg)) {
      'pan' => ProximityUnderlay.pan,
      'lan' => ProximityUnderlay.lan,
      _ => null,
    };

/// `declare(nearby(U, S))`: assign S a stream of the agents nearby over the proximity underlay U, `seen(K)` when the layer discovers on U an agent of key K that it recognises or, under open trust, contacts, and `gone(K)` when that agent can no longer be reached over U; a further declaration on the same underlay supersedes the earlier one and closes its stream (GLP-Spec 210cee1, appendix-guards, "Networking seam"; IGLP b20951d, Definition "Seam Predicates" and the paragraph after it).  The stream is fed and closed as a place's, by the runtime's bookkeeping ([GlpRuntime.nearbyStreams]); what feeds it is what the layer reports ([MadContext.declareNearby]).
///
/// Where no layer is bound the declaration stands in the runtime's bookkeeping all the same, so its removal or a further declaration closes the stream, and nothing is reported on it: a layer that observes nothing discovers no agent (IGLP, Implementation Notes, "One interface", which states this of a place's stream).
BodyKernelResult _declareNearby(GlpRuntime rt, List<Term> a) {
  const who = "'_declare'/1", what = 'nearby(U, S)';
  final underlay = _proximityUnderlay(rt, a[0]);
  if (underlay == null) {
    return kernelAbort('[ABORT] $who: $what: the underlay (U) must be pan or '
        'lan, got ${_deref(rt, a[0])}');
  }
  final stream = _deref(rt, a[1]);
  if (stream is! VarRef || !rt.heap.isWriter(stream.addr)) {
    return kernelAbort('[ABORT] $who: $what: the stream (S) must be a writer');
  }
  final ctx = rt.madContext;
  if (ctx is MadContext) {
    ctx.declareNearby(underlay, stream.addr);
  } else {
    rt.nearbyStreams.declare(underlay.name, stream.addr);
  }
  return BodyKernelResult.success;
}

/// `remove(nearby(U))`: end the nearby declaration on U, closing its stream, its tail assigned `[]`; does nothing where none stands (GLP-Spec 210cee1, appendix-guards, "Networking seam").  The closing is the runtime's, layer or none.
BodyKernelResult _removeNearby(GlpRuntime rt, List<Term> a) {
  const who = "'_remove'/1", what = 'nearby(U)';
  final underlay = _proximityUnderlay(rt, a[0]);
  if (underlay == null) {
    return kernelAbort('[ABORT] $who: $what: the underlay (U) must be pan or '
        'lan, got ${_deref(rt, a[0])}');
  }
  rt.nearbyStreams.close(underlay.name);
  return BodyKernelResult.success;
}

/// `declare(reachable(S))`: assign S a stream of the agents the layer connects to over the WAN, `connected(K)` when the layer has a session with the agent of key K over the WAN, beginning with every agent it has one with when it is declared, and `lost(K)` when that session ends; a further declaration supersedes the earlier one and closes its stream (GLP-Spec cb9df43, appendix-guards, "Networking seam"; IGLP 0c77e47, Definition "Seam Predicates" and the paragraph after it).  The stream is fed and closed as a place's, by the runtime's bookkeeping ([GlpRuntime.reachableStreams]); what feeds it is what the layer reports, `onPeerConnected` and `onPeerDisconnected` over the WAN ([MadContext.declareReachable]).
///
/// Where no layer is bound the declaration stands all the same, and its stream carries nothing (IGLP 0c77e47, Implementation Notes, "One interface": "a nearby or reachable declaration stands and its stream carries nothing").
BodyKernelResult _declareReachable(GlpRuntime rt, List<Term> a) {
  const who = "'_declare'/1", what = 'reachable(S)';
  final stream = _deref(rt, a[0]);
  if (stream is! VarRef || !rt.heap.isWriter(stream.addr)) {
    return kernelAbort('[ABORT] $who: $what: the stream (S) must be a writer');
  }
  final ctx = rt.madContext;
  if (ctx is MadContext) {
    ctx.declareReachable(stream.addr);
  } else {
    rt.reachableStreams.declare(reachableKey, stream.addr);
  }
  return BodyKernelResult.success;
}

/// `remove(reachable)`: end the reachable declaration, closing its stream, its tail assigned `[]`; does nothing where none stands (GLP-Spec cb9df43, appendix-guards, "Networking seam").  The closing is the runtime's, layer or none.
BodyKernelResult _removeReachable(GlpRuntime rt) {
  rt.reachableStreams.close(reachableKey);
  return BodyKernelResult.success;
}

/// The one key of [GlpRuntime.reachableStreams]: an agent has at most one reachable declaration standing.
const String reachableKey = 'wan';

/// `remove(peer(P))`: take P, and any address recorded for it, off the layer's known peers, `removeKnownPeer(pk)` (GLP-Spec appendix-guards, "Networking seam"; GNA, "Known Peers").
BodyKernelResult _removePeer(GlpRuntime rt, List<Term> a) {
  const who = "'_remove'/1", what = 'peer(P)';
  return _peerCall(rt, who, what, a[0],
      (network, pk) {
    network.removeKnownPeer(pk);
    rt.seamDeclarations.removePeer(pk.hex);
  });
}

/// `remove(rendezvous(P))`: take the rendezvous server of key P off and end the layer's session with it, a key not declared doing nothing, `removeRendezvousServer(pk)` (GLP-Spec appendix-guards, "Networking seam"; GNA 776016c, Section 4.2, "Remove server": "behind remove(rendezvous(Key))").
BodyKernelResult _removeRendezvous(GlpRuntime rt, List<Term> a) {
  const who = "'_remove'/1", what = 'rendezvous(P)';
  return _peerCall(rt, who, what, a[0],
      (network, pk) {
    network.removeRendezvousServer(pk);
    rt.seamDeclarations.removeRendezvous(pk.hex);
  });
}

/// `remove(place(P))`: end the declaration of place P, closing its stream; does nothing where P is not declared.  Where no layer is bound it closes the stream all the same, the runtime's bookkeeping, and tells no layer ("One interface", 6ba0329; GLP, 2026-10-11 12:59 UTC, "23:12" (a)).
BodyKernelResult _removePlace(GlpRuntime rt, List<Term> a) {
  const who = "'_remove'/1", what = 'place(P)';
  final place = _placeName(rt, a[0]);
  if (place == null) {
    return kernelAbort('[ABORT] $who: $what: the place (P) must be a ground '
        'constant, got ${_deref(rt, a[0])}');
  }
  rt.seamDeclarations.removePlace(place);
  final seam = _seamLayer(rt);
  if (seam == null) {
    rt.placeStreams.close(place);
    return BodyKernelResult.success;
  }
  return _layerCall(who, what, () => seam.ctx.removePlace(place));
}

// =============================================================================
// '_output'/1 - An element of send_to_user/1 to the person
// =============================================================================

/// '_output'(T) - emits the ground term T to the person operating the agent
/// (GLP-Spec appendix-guards.tex, "Output to the person": send_to_user(Stream)
/// "emits each element of Stream to the person operating the agent").
///
/// Called by send_to_user/1, whose clause guards T ground.  Where the runtime
/// has a person's bridge, [GlpRuntime.outputTermCallback], the term goes to it
/// whole and no text is made; otherwise a terminal is the person, and T goes
/// to [GlpRuntime.outputCallback], or to stdout, as a terminal shows it
/// ([formatGroundTerm]).  Until 2026-10-11 every element was printed, and a
/// screen read the line back.
BodyKernelResult outputKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 1) {
    return kernelAbort('[ABORT] \'_output\'/1: expected 1 argument, got ${args.length}');
  }

  final term = deepDeref(rt, args[0] as Term);

  final toPerson = rt.outputTermCallback;
  if (toPerson != null) {
    toPerson(term);
    return BodyKernelResult.success;
  }

  final shown = formatGroundTerm(term);
  final callback = rt.outputCallback;
  if (callback != null) {
    callback(shown);
  } else {
    print(shown);
  }

  return BodyKernelResult.success;
}

// =============================================================================
// TEXT (string_concat/2, term_to_string/2)
// =============================================================================
// GLP-Spec appendix-guards.tex at ca3c8e5, "Text (string_concat/2,
// term_to_string/2)".

/// `'_string_concat'(Ss?, S)` --- "assigns S the string of the characters of
/// the strings of the list Ss, one after another; the empty list contributes
/// the characters []", and aborts "where an element is not a string".  The
/// clause of string_concat/2 guards Ss ground.
BodyKernelResult stringConcatKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    return kernelAbort(
        "[ABORT] '_string_concat'/2: expected 2 arguments, got ${args.length}");
  }
  final out = StringBuffer();
  var current = _deref(rt, args[0]);
  while (true) {
    if (current is ConstTerm && (current.value == nil || current.value == null)) {
      break;
    }
    if (current is! StructTerm ||
        current.functor != '.' ||
        current.args.length != 2) {
      return kernelAbort(
          "[ABORT] '_string_concat'/2: the first argument is not a list: $current");
    }
    final element = _deref(rt, current.args[0]);
    if (element is ConstTerm && (element.value == nil || element.value == null)) {
      out.write('[]');
    } else if (element is ConstTerm && element.value is String) {
      out.write(element.value as String);
    } else {
      return kernelAbort(
          "[ABORT] '_string_concat'/2: an element is not a string: $element");
    }
    current = _deref(rt, current.args[1]);
  }
  return bindResult(rt, args[1], out.toString(), kernel: "'_string_concat'/2");
}

/// `'_term_to_string'(T?, S)` --- "assigns S a text that the reader reads as
/// the ground term T", one text per term: [formatTerm], each constant and
/// functor as the printer writes it, a list as `[a, b]` or `[a, b | T]`,
/// arguments and elements separated by a comma and a space, and a real as
/// [realSource].  The clause of term_to_string/2 guards T ground.  A real that
/// is NaN or infinite has no text the reader reads (GLP-Spec appendix-guards,
/// "Predefined types": "A number is written as its digits, with an optional
/// minus directly before them"), so it is out of the kernel's domain and the
/// kernel aborts ("A body kernel whose precondition fails --- a zero divisor,
/// an argument out of its domain --- aborts").
BodyKernelResult termToStringKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    return kernelAbort(
        "[ABORT] '_term_to_string'/2: expected 2 arguments, got ${args.length}");
  }
  final term = deepDeref(rt, args[0] as Term);
  final String text;
  try {
    text = formatTerm(term, number: realSource);
  } on ArgumentError catch (e) {
    return kernelAbort("[ABORT] '_term_to_string'/2: ${e.message}");
  }
  return bindResult(rt, args[1], text, kernel: "'_term_to_string'/2");
}

/// [value] as the reader reads it back: an integer as its digits, and a real
/// as its digits with a decimal point, the shortest that reads back as the
/// real, never in exponent form, which the reader does not read (GLP-Spec
/// appendix-guards, "Predefined types": "A number is written as its digits,
/// with an optional minus directly before them"; appendix-lp.tex).  Dart
/// prints `1e-7` and `1e+21`; this prints `0.0000001` and
/// `1000000000000000000000.0`.  Throws [ArgumentError] on a NaN or infinite
/// real, which no text reads as.
String realSource(num value) {
  if (value is int) return value.toString();
  final d = value as double;
  if (!d.isFinite) {
    throw ArgumentError('the real $d has no text the reader reads as it');
  }
  final shown = d.toString();
  final e = shown.indexOf('e');
  if (e < 0) return shown;
  var mantissa = shown.substring(0, e);
  final exponent = int.parse(shown.substring(e + 1));
  final negative = mantissa.startsWith('-');
  if (negative) mantissa = mantissa.substring(1);
  final point = mantissa.indexOf('.');
  final digits = mantissa.replaceAll('.', '');
  // The position of the decimal point in digits, after the shift.
  final at = (point < 0 ? mantissa.length : point) + exponent;
  String body;
  if (at <= 0) {
    body = '0.${'0' * -at}$digits';
  } else if (at >= digits.length) {
    body = '$digits${'0' * (at - digits.length)}.0';
  } else {
    body = '${digits.substring(0, at)}.${digits.substring(at)}';
  }
  // Trailing zeros after the point, but one digit after it.
  if (body.contains('.')) {
    body = body.replaceFirst(RegExp(r'0+$'), '');
    if (body.endsWith('.')) body = '${body}0';
  }
  return negative ? '-$body' : body;
}

/// Format a ground term as readable GLP syntax.
///
/// Lists are shown as [a, b, c], atoms as-is, structs as f(a, b).
String formatGroundTerm(Term term) {
  if (term is ConstTerm) {
    if (term.value == nil || term.value == null) return '[]';
    return term.value.toString();
  }
  if (term is StructTerm) {
    // List: .(H, T) → [H, ...]
    if (term.functor == '.' && term.args.length == 2) {
      final elements = <String>[];
      Term current = term;
      while (current is StructTerm && current.functor == '.' && current.args.length == 2) {
        elements.add(formatGroundTerm(current.args[0]));
        current = current.args[1];
      }
      if (current is ConstTerm && (current.value == nil || current.value == null)) {
        return '[${elements.join(', ')}]';
      }
      return '[${elements.join(', ')} | ${formatGroundTerm(current)}]';
    }
    final args = term.args.map(formatGroundTerm).join(', ');
    return '${term.functor}($args)';
  }
  return term.toString();
}

/// [term] as GLP text that reads back as the term (GLP-Spec appendix-lp.tex,
/// Definition "Logic Programs Syntax"): each constant and functor as the
/// printer writes it ([constantSource], [functorNameSource]) --- `'G'`, `'+'`,
/// `'42'` and `'Flood on Main Street'` in single quotes --- `[]` for the empty
/// list, a list as `[a, b, c]` or `[a, b | T]`, and a structure as `f(a, b)`.
/// The one printer of the REPL's bindings (bin/glp_repl.dart) and of the
/// lines a boot prints, each element an agent emits to its person
/// (multiagent/isolate_manager.dart) (GLP #9 Cowork, 2026-10-20 09:22 UTC,
/// item 4.2: "the boot prints a term in GLP syntax, as the REPL does, a
/// quoted constant quoted, so that what is printed reads back as the term;
/// one printer serves both").  Until 2026-10-20 the boot printed by
/// [formatGroundTerm], `utterance(Flood on Main Street)` where the REPL
/// printed `utterance('Flood on Main Street')` (Code #11, 2026-10-19 11:45
/// UTC).  send_to_user/1 on a terminal prints by [formatGroundTerm], a
/// constant unquoted, as it stays (bin/glp_repl.dart; GLP #3 Cowork,
/// 2026-10-04 09:06 UTC).
///
/// [number], where given, prints each number, as [realSource] does for
/// '_term_to_string'; without it a number prints by [constantSource].
///
/// [deref] is called at each variable met and gives the term it is bound to,
/// or a variable where it is unbound, which [variableName] names; with no
/// [deref] a variable is shown as the runtime shows it.  A variable met again
/// inside its own value is shown `<circular>`, or, as a list's tail,
/// `<circular X>` after the elements before it.  The list is walked along its
/// spine in a loop and its elements by recursion, as the checker walks it.
String formatTerm(Term term,
    {Term Function(VarRef v)? deref,
    String Function(VarRef v)? variableName,
    String Function(num value)? number}) {
  final path = <Object>{};
  String name(VarRef v) => variableName?.call(v) ?? v.toString();

  String show(Term t) {
    if (t is VarRef) {
      final d = deref;
      if (d == null) return t.toString();
      if (path.contains(t.addr)) return '<circular>';
      path.add(t.addr);
      final value = d(t);
      final s = value is VarRef ? name(value) : show(value);
      path.remove(t.addr);
      return s;
    }
    if (t is ConstTerm) {
      final value = t.value;
      if (value == null || value == nil) return '[]';
      if (value is num && number != null) return number(value);
      return constantSource(value);
    }
    if (t is StructTerm && t.functor == '.' && t.args.length == 2) {
      final elements = <String>[];
      Term current = t;
      final d = deref;
      while (true) {
        if (current is VarRef && d != null) {
          if (path.contains(current.addr)) {
            return '[${elements.join(', ')} | <circular ${name(current)}>]';
          }
          final value = d(current);
          if (value is VarRef) return '[${elements.join(', ')} | ${name(value)}]';
          current = value;
          continue;
        }
        if (current is StructTerm &&
            current.functor == '.' &&
            current.args.length == 2) {
          elements.add(show(current.args[0]));
          current = current.args[1];
          continue;
        }
        break;
      }
      if (current is ConstTerm &&
          (current.value == nil || current.value == null)) {
        return '[${elements.join(', ')}]';
      }
      return '[${elements.join(', ')} | ${show(current)}]';
    }
    if (t is StructTerm) {
      return '${functorNameSource(t.functor)}(${t.args.map(show).join(', ')})';
    }
    return t.toString();
  }

  return show(term);
}

