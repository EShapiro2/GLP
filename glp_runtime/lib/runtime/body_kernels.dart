/// Body kernel infrastructure for GLP arithmetic
///
/// Body kernels are runtime-implemented predicates that:
/// - Execute inline (not spawned as separate goals)
/// - Have two-valued semantics (success or abort)
/// - Are only accessible to system predicates (assign.glp)
/// - Expect all preconditions met (guards should verify before calling)
///
/// Per heap-pointer-architecture-spec.md v3.0:
/// - VarRef has only addr field
/// - Use heap.isWriter/isReader to check cell type

import 'dart:math' as math;

import 'runtime.dart';
import 'terms.dart';
import 'machine_state.dart' show GoalRef;
import 'package:glp_runtime/bytecode/runner.dart' show BytecodeRunner, BytecodeProgram, CallEnv;
import 'package:glp_runtime/multiagent/mad_context.dart';

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

/// Registry of body kernels
class BodyKernelRegistry {
  final Map<String, BodyKernel> _kernels = {};

  void register(String name, int arity, BodyKernel kernel) {
    _kernels['$name/$arity'] = kernel;
  }

  BodyKernel? lookup(String name, int arity) => _kernels['$name/$arity'];

  bool has(String name, int arity) => _kernels.containsKey('$name/$arity');

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
  registry.register('_neg', 2, negKernel);

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
  registry.register('struct_arg', 3, structArgKernel);

  // Identity/copy
  registry.register('_copy', 2, copyKernel);

  // Time operations
  registry.register('_now', 1, nowKernel);

  // MutualRef operations (O(1) stream append)
  registry.register('_allocate_mutual_reference', 2, mutualRefKernel);
  registry.register('_stream_append', 3, streamAppendKernel);
  registry.register('_close_mutual_reference', 1, mutualRefCloseKernel);

  // madGLP kernels
  registry.register('_send', 3, sendKernel);

  // I/O kernels
  registry.register('_output', 1, outputKernel);

  // Map operations (O(1) key-value lookup)
  registry.register('map_new', 1, mapNewKernel);
  registry.register('map_put', 4, mapPutKernel);
  registry.register('_map_get', 3, mapGetKernel);
  registry.register('map_remove', 3, mapRemoveKernel);
  registry.register('map_keys', 2, mapKeysKernel);
  registry.register('map_show', 3, mapShowKernel);
  registry.register('fofmap_show', 3, fofmapShowKernel);
  registry.register('map_list_append', 4, mapListAppendKernel);

  // SharedBroadcastStream operations
  registry.register('sbs_new', 2, sbsNewKernel);
  registry.register('sbs_add_recipient', 3, sbsAddRecipientKernel);
  registry.register('sbs_write_update', 4, sbsWriteUpdateKernel);
  registry.register('sbs_get_checkpoint', 2, sbsGetCheckpointKernel);

  // Arithmetic assignment
  registry.register(':=', 2, assignKernel);

  // Module dispatch kernels
  registry.register('_activate', 2, activateKernel);
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
    case 'neg': return -args[0]!;
    default: return null;
  }
}

/// Helper to bind result to output writer
BodyKernelResult _bindResult(GlpRuntime rt, Object? outputArg, Object value) {
  if (outputArg is VarRef && rt.heap.isWriter(outputArg.addr)) {
    final List<GoalRef> activations;
    if (value is Term) {
      activations = rt.heap.bindVariable(outputArg.addr, value);
    } else {
      activations = rt.heap.bindVariableConst(outputArg.addr, value);
    }
    for (final act in activations) {
      rt.gq.enqueue(act);
    }
    return BodyKernelResult.success;
  }
  print('[ABORT] Body kernel: output argument is not a writer');
  return BodyKernelResult.abort;
}

// ============================================================================
// ARITHMETIC KERNELS
// ============================================================================

BodyKernelResult addKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    print('[ABORT] add/3: expected 3 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }
  final x = _getNum(rt, args[0]);
  final y = _getNum(rt, args[1]);
  if (x == null || y == null) {
    print('[ABORT] add/3: operands must be numbers');
    return BodyKernelResult.abort;
  }
  return _bindResult(rt, args[2], x + y);
}

BodyKernelResult subKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    print('[ABORT] sub/3: expected 3 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }
  final x = _getNum(rt, args[0]);
  final y = _getNum(rt, args[1]);
  if (x == null || y == null) {
    print('[ABORT] sub/3: operands must be numbers');
    return BodyKernelResult.abort;
  }
  return _bindResult(rt, args[2], x - y);
}

BodyKernelResult mulKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    print('[ABORT] mul/3: expected 3 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }
  final x = _getNum(rt, args[0]);
  final y = _getNum(rt, args[1]);
  if (x == null || y == null) {
    print('[ABORT] mul/3: operands must be numbers');
    return BodyKernelResult.abort;
  }
  return _bindResult(rt, args[2], x * y);
}

BodyKernelResult divKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    print('[ABORT] div/3: expected 3 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }
  final x = _getNum(rt, args[0]);
  final y = _getNum(rt, args[1]);
  if (x == null || y == null) {
    print('[ABORT] div/3: operands must be numbers');
    return BodyKernelResult.abort;
  }
  if (y == 0) {
    print('[ABORT] div/3: division by zero');
    return BodyKernelResult.abort;
  }
  return _bindResult(rt, args[2], x / y);
}

BodyKernelResult idivKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    print('[ABORT] idiv/3: expected 3 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }
  final x = _getNum(rt, args[0]);
  final y = _getNum(rt, args[1]);
  if (x == null || y == null || x is! int || y is! int) {
    print('[ABORT] idiv/3: operands must be integers');
    return BodyKernelResult.abort;
  }
  if (y == 0) {
    print('[ABORT] idiv/3: division by zero');
    return BodyKernelResult.abort;
  }
  return _bindResult(rt, args[2], x ~/ y);
}

BodyKernelResult modKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    print('[ABORT] mod/3: expected 3 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }
  final x = _getNum(rt, args[0]);
  final y = _getNum(rt, args[1]);
  if (x == null || y == null || x is! int || y is! int) {
    print('[ABORT] mod/3: operands must be integers');
    return BodyKernelResult.abort;
  }
  if (y == 0) {
    print('[ABORT] mod/3: modulo by zero');
    return BodyKernelResult.abort;
  }
  return _bindResult(rt, args[2], x % y);
}

BodyKernelResult negKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    print('[ABORT] neg/2: expected 2 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }
  final x = _getNum(rt, args[0]);
  if (x == null) {
    print('[ABORT] neg/2: operand must be a number');
    return BodyKernelResult.abort;
  }
  return _bindResult(rt, args[1], -x);
}

// ============================================================================
// MATH FUNCTION KERNELS
// ============================================================================

BodyKernelResult absKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], x.abs());
}

BodyKernelResult sqrtKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null || x < 0) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], math.sqrt(x));
}

BodyKernelResult sinKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], math.sin(x));
}

BodyKernelResult cosKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], math.cos(x));
}

BodyKernelResult tanKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], math.tan(x));
}

BodyKernelResult expKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], math.exp(x));
}

BodyKernelResult lnKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null || x <= 0) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], math.log(x));
}

BodyKernelResult log10Kernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null || x <= 0) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], math.log(x) / math.ln10);
}

BodyKernelResult powKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  final y = _getNum(rt, args[1]);
  if (x == null || y == null) return BodyKernelResult.abort;
  return _bindResult(rt, args[2], math.pow(x, y));
}

BodyKernelResult asinKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null || x < -1 || x > 1) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], math.asin(x));
}

BodyKernelResult acosKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null || x < -1 || x > 1) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], math.acos(x));
}

BodyKernelResult atanKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], math.atan(x));
}

// ============================================================================
// TYPE CONVERSION KERNELS
// ============================================================================

BodyKernelResult integerKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], x.toInt());
}

BodyKernelResult realKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], x.toDouble());
}

BodyKernelResult roundKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], x.round());
}

BodyKernelResult floorKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], x.floor());
}

BodyKernelResult ceilKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], x.ceil());
}

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
Term _deepDeref(GlpRuntime rt, Term term) {
  // First, dereference the term itself if it's a VarRef
  var current = term;
  while (current is VarRef) {
    final val = rt.heap.getValue(current.addr);
    if (val == null || val is! Term) return current; // Unbound variable
    current = val;
  }

  // Now recursively dereference structure arguments
  if (current is StructTerm) {
    final newArgs = <Term>[];
    for (final arg in current.args) {
      newArgs.add(_deepDeref(rt, arg));
    }
    return StructTerm(current.functor, newArgs);
  }

  return current; // ConstTerm or unbound VarRef
}

/// Helper to convert Dart list to GLP list structure
Term _dartListToGlpList(List<Object?> items) {
  Term result = ConstTerm('nil');
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
    if (current is ConstTerm && current.value == 'nil') {
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
    print('[ABORT] list_to_tuple/2: expected 2 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }

  final listArg = _deref(rt, args[0]);
  final items = _glpListToDartList(rt, listArg);

  if (items == null || items.isEmpty) {
    print('[ABORT] list_to_tuple/2: first argument must be a non-empty list');
    return BodyKernelResult.abort;
  }

  final functorTerm = items[0];
  String? functor;
  if (functorTerm is ConstTerm && functorTerm.value is String) {
    functor = functorTerm.value as String;
  } else if (functorTerm is String) {
    functor = functorTerm;
  }

  if (functor == null) {
    print('[ABORT] list_to_tuple/2: first element must be an atom (functor)');
    return BodyKernelResult.abort;
  }

  final structArgs = <Term>[];
  for (var i = 1; i < items.length; i++) {
    final item = items[i];
    structArgs.add(item is Term ? item : ConstTerm(item));
  }

  final tuple = StructTerm(functor, structArgs);
  return _bindResult(rt, args[1], tuple);
}

BodyKernelResult tupleToListKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    print('[ABORT] tuple_to_list/2: expected 2 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }

  final tupleArg = _deref(rt, args[0]);

  if (tupleArg is! StructTerm) {
    print('[ABORT] tuple_to_list/2: first argument must be a structure');
    return BodyKernelResult.abort;
  }

  final items = <Object?>[ConstTerm(tupleArg.functor)];
  for (final arg in tupleArg.args) {
    items.add(_deref(rt, arg));
  }

  final list = _dartListToGlpList(items);
  return _bindResult(rt, args[1], list);
}

/// struct_arg(Struct?, Index?, Value) — Extract the Nth argument (1-based) from a struct.
/// Used to destructure structs when head pattern matching is not available.
BodyKernelResult structArgKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    print('[ABORT] struct_arg/3: expected 3 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }

  final structArg = _deref(rt, args[0]);
  if (structArg is! StructTerm) {
    print('[ABORT] struct_arg/3: first argument must be a structure, got ${structArg.runtimeType}');
    return BodyKernelResult.abort;
  }

  final indexArg = _deref(rt, args[1]);
  final index = (indexArg is ConstTerm && indexArg.value is int)
      ? indexArg.value as int
      : (indexArg is int ? indexArg : null);
  if (index == null || index < 1 || index > structArg.args.length) {
    print('[ABORT] struct_arg/3: index $index out of range [1..${structArg.args.length}]');
    return BodyKernelResult.abort;
  }

  final value = _deepDeref(rt, structArg.args[index - 1]);
  return _bindResult(rt, args[2], value);
}

BodyKernelResult copyKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    print('[ABORT] copy/2: expected 2 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }

  final source = _deref(rt, args[0]);
  return _bindResult(rt, args[1], source!);
}

// ============================================================================
// TIME KERNELS
// ============================================================================

BodyKernelResult nowKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 1) {
    print('[ABORT] now/1: expected 1 argument, got ${args.length}');
    return BodyKernelResult.abort;
  }
  final currentTime = DateTime.now().millisecondsSinceEpoch;
  return _bindResult(rt, args[0], currentTime);
}

// ============================================================================
// MUTUAL REFERENCE KERNELS (O(1) Stream Append)
// ============================================================================

BodyKernelResult mutualRefKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    print('[ABORT] allocate_mutual_reference/2: expected 2 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }

  final output = _deref(rt, args[1]);
  if (output is! VarRef || !rt.heap.isWriter(output.addr)) {
    print('[ABORT] allocate_mutual_reference/2: second argument must be an unbound writer');
    return BodyKernelResult.abort;
  }

  if (rt.heap.isFullyBound(output.addr)) {
    print('[ABORT] allocate_mutual_reference/2: writer @${output.addr} is already bound');
    return BodyKernelResult.abort;
  }

  final mutualRef = MutualRefTerm(output.addr);
  return _bindResult(rt, args[0], mutualRef);
}

BodyKernelResult streamAppendKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    print('[ABORT] kernel_stream_append/3: expected 3 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }

  final refArg = _deref(rt, args[0]);
  if (refArg is! MutualRefTerm) {
    print('[ABORT] kernel_stream_append/3: first argument must be a MutualRef');
    return BodyKernelResult.abort;
  }

  final currentWriterAddr = refArg.currentWriterAddr;

  if (rt.heap.isFullyBound(currentWriterAddr)) {
    print('[ABORT] kernel_stream_append/3: MutualRef points to already-bound writer @$currentWriterAddr');
    return BodyKernelResult.abort;
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
    rt.gq.enqueue(act);
  }

  // Update MutualRef to point to the new tail's writer
  refArg.currentWriterAddr = newTailWriter;

  return _bindResult(rt, args[2], refArg);
}

BodyKernelResult mutualRefCloseKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 1) {
    print('[ABORT] kernel_close_mutual_reference/1: expected 1 argument, got ${args.length}');
    return BodyKernelResult.abort;
  }

  final refArg = _deref(rt, args[0]);
  if (refArg is! MutualRefTerm) {
    print('[ABORT] kernel_close_mutual_reference/1: argument must be a MutualRef');
    return BodyKernelResult.abort;
  }

  final currentWriterAddr = refArg.currentWriterAddr;

  if (rt.heap.isFullyBound(currentWriterAddr)) {
    print('[ABORT] kernel_close_mutual_reference/1: MutualRef points to already-bound writer @$currentWriterAddr');
    return BodyKernelResult.abort;
  }

  final activations = rt.heap.bindVariable(currentWriterAddr, ConstTerm('nil'));

  for (final act in activations) {
    rt.gq.enqueue(act);
  }

  return BodyKernelResult.success;
}

// ============================================================================
// MADGLP KERNELS
// ============================================================================

/// Send kernel for madGLP
///
/// '_send'(T, G, Q) - sends term T via global name G to agent Q.
/// This is called by the GLP `global_send/3` predicate.
///
/// Per madGLP-spec.md Section 11.5:
/// - Case G = _w(q, 0) (Serializer): wraps T in list [T↑ | _w(q,0)]
/// - Case G = _w(p, i) or _r(p, i) with i > 0: sends T directly
///
/// The global name G determines the routing and message format.
BodyKernelResult sendKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    print('[ABORT] \'_send\'/3: expected 3 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }

  // Get MadContext from runtime
  final ctx = rt.madContext;
  if (ctx == null || ctx is! MadContext) {
    print('[ABORT] \'_send\'/3: not in madGLP mode (no MadContext)');
    return BodyKernelResult.abort;
  }

  // Get term T (first argument)
  // IMPORTANT: Use _deepDeref to fully resolve nested structures.
  // _deref only follows top-level VarRefs, leaving nested structure args as VarRefs.
  // This causes serialization bugs where msg(bob, intro(alice, X)) becomes msg(VarRef, VarRef).
  final termArg = _deepDeref(rt, args[0] as Term);
  if (termArg is! Term) {
    print('[ABORT] \'_send\'/3: first argument (T) must be a term, got ${termArg.runtimeType}');
    return BodyKernelResult.abort;
  }

  // Get global name G (second argument) - should be _w(Agent, Index) or _r(Agent, Index)
  final globalNameArg = _deref(rt, args[1]);
  if (globalNameArg is! StructTerm) {
    print('[ABORT] \'_send\'/3: second argument (G) must be a struct _w/2 or _r/2, got ${globalNameArg.runtimeType}');
    return BodyKernelResult.abort;
  }

  // Parse the global name structure
  final functor = globalNameArg.functor;
  if (functor != '\'_w\'' && functor != '\'_r\'' && functor != '_w' && functor != '_r') {
    print('[ABORT] \'_send\'/3: global name must be _w/2 or _r/2, got $functor');
    return BodyKernelResult.abort;
  }
  if (globalNameArg.args.length != 2) {
    print('[ABORT] \'_send\'/3: global name must have 2 arguments, got ${globalNameArg.args.length}');
    return BodyKernelResult.abort;
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
    print('[ABORT] \'_send\'/3: global name agent must be an atom, got $gnAgentArg');
    return BodyKernelResult.abort;
  }

  // Extract index from global name (second arg of _w/_r)
  final gnIndexArg = _deref(rt, globalNameArg.args[1]);
  int? gnIndex;
  if (gnIndexArg is num) {
    gnIndex = gnIndexArg.toInt();
  } else if (gnIndexArg is ConstTerm && gnIndexArg.value is num) {
    gnIndex = (gnIndexArg.value as num).toInt();
  }
  if (gnIndex == null) {
    print('[ABORT] \'_send\'/3: global name index must be a number, got $gnIndexArg');
    return BodyKernelResult.abort;
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
    print('[ABORT] \'_send\'/3: third argument (Q) must be an atom (destination agent), got $destArg');
    return BodyKernelResult.abort;
  }

  // Determine if this is a writer or reader global name
  final isWriter = functor == '\'_w\'' || functor == '_w';

  // Unified send handles both serializer (index 0) and normal (index > 0) cases
  ctx.send(termArg, isWriter, gnAgent, gnIndex, destAgent);

  return BodyKernelResult.success;
}

// =============================================================================
// '_output'/1 - Print a ground term as a line
// =============================================================================

/// '_output'(T) - prints ground term T to stdout.
///
/// Called by send_to_user/1 after ui_relay has ensured the term is ground.
/// The output callback can be overridden via GlpRuntime.outputCallback
/// for testing or Flutter integration.
BodyKernelResult outputKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 1) {
    print('[ABORT] \'_output\'/1: expected 1 argument, got ${args.length}');
    return BodyKernelResult.abort;
  }

  final term = _deepDeref(rt, args[0] as Term);
  final formatted = formatGroundTerm(term);

  // Use callback if set (for testing/Flutter), otherwise print
  final callback = rt.outputCallback;
  if (callback != null) {
    callback(formatted);
  } else {
    print(formatted);
  }

  return BodyKernelResult.success;
}

// ============================================================================
// MAP KERNELS (O(1) Key-Value Lookup)
// ============================================================================

/// Extract a Dart-level key from a GLP term.
/// Keys must be ground constants (string, int, double).
Object? _extractMapKey(GlpRuntime rt, Object? arg) {
  final val = _deref(rt, arg);
  if (val is ConstTerm) return val.value;
  if (val is num) return val;
  if (val is String) return val;
  return null; // not a valid map key
}

/// map_new(M) — Create empty map, bind M to it.
BodyKernelResult mapNewKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 1) {
    print('[ABORT] map_new/1: expected 1 argument, got ${args.length}');
    return BodyKernelResult.abort;
  }
  return _bindResult(rt, args[0], MapTerm({}));
}

/// map_put(M?, Key?, Val?, M1) — Mutate map in place + bind M1.
/// O(1) — SRSW guarantees the old map variable (M) is dead after this
/// read, so in-place mutation is safe. No copy needed.
BodyKernelResult mapPutKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 4) {
    print('[ABORT] map_put/4: expected 4 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }

  final mapArg = _deref(rt, args[0]);
  if (mapArg is! MapTerm) {
    print('[ABORT] map_put/4: first argument must be a MapTerm, got ${mapArg.runtimeType}');
    return BodyKernelResult.abort;
  }

  final key = _extractMapKey(rt, args[1]);
  if (key == null) {
    print('[ABORT] map_put/4: second argument must be a ground constant');
    return BodyKernelResult.abort;
  }

  final val = _deref(rt, args[2]);
  mapArg.entries[key] = (val is Term) ? val : ConstTerm(val);

  return _bindResult(rt, args[3], mapArg);
}

/// _map_get(M?, Key?, Val) — Look up key, bind Val to value.
/// Only called after map_contains guard confirms key exists.
BodyKernelResult mapGetKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    print('[ABORT] _map_get/3: expected 3 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }

  final mapArg = _deref(rt, args[0]);
  if (mapArg is! MapTerm) {
    print('[ABORT] _map_get/3: first argument must be a MapTerm, got ${mapArg.runtimeType}');
    return BodyKernelResult.abort;
  }

  final key = _extractMapKey(rt, args[1]);
  if (key == null) {
    print('[ABORT] _map_get/3: second argument must be a ground constant');
    return BodyKernelResult.abort;
  }

  final val = mapArg.entries[key];
  if (val == null) {
    print('[ABORT] _map_get/3: key not found (guard should have checked)');
    return BodyKernelResult.abort;
  }

  return _bindResult(rt, args[2], val);
}

/// map_remove(M?, Key?, M1) — Remove key from map in place + bind M1.
/// O(1) — SRSW guarantees the old map variable (M) is dead after this
/// read, so in-place mutation is safe.
/// If the key is not present, the map is unchanged (no error).
BodyKernelResult mapRemoveKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    print('[ABORT] map_remove/3: expected 3 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }

  final mapArg = _deref(rt, args[0]);
  if (mapArg is! MapTerm) {
    print('[ABORT] map_remove/3: first argument must be a MapTerm, got ${mapArg.runtimeType}');
    return BodyKernelResult.abort;
  }

  final key = _extractMapKey(rt, args[1]);
  if (key == null) {
    print('[ABORT] map_remove/3: second argument must be a ground constant');
    return BodyKernelResult.abort;
  }

  mapArg.entries.remove(key); // O(1) in-place removal; no-op if absent

  return _bindResult(rt, args[2], mapArg);
}

/// map_keys(M?, Keys) — Extract all keys as a GLP list.
/// O(n) where n = number of entries — must iterate all keys and
/// build the list.  Identical complexity to Dart HashMap.keys.
BodyKernelResult mapKeysKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    print('[ABORT] map_keys/2: expected 2 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }

  final mapArg = _deref(rt, args[0]);
  if (mapArg is! MapTerm) {
    print('[ABORT] map_keys/2: first argument must be a MapTerm, got ${mapArg.runtimeType}');
    return BodyKernelResult.abort;
  }

  // Convert Dart keys to GLP list: [k1, k2, ..., kn]
  final keys = mapArg.entries.keys.toList();
  final glpList = _dartListToGlpList(keys);

  return _bindResult(rt, args[1], glpList);
}

/// map_show(AgentId?, MapIn?, MapOut) — Print FMap contents to screen.
/// Outputs tagged(id, notify(fmap({...}))) via _output path so it
/// routes to the correct agent panel in Flutter UI.
/// Pass-through: binds MapOut to a copy of MapIn so the map remains usable.
BodyKernelResult mapShowKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    print('[ABORT] map_show/3: expected 3 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }

  final idArg = _deref(rt, args[0]);
  final id = (idArg is ConstTerm) ? idArg.value.toString() : idArg.toString();

  final mapArg = _deref(rt, args[1]);
  if (mapArg is! MapTerm) {
    print('[ABORT] map_show/3: second argument must be a MapTerm, got ${mapArg.runtimeType}');
    return BodyKernelResult.abort;
  }

  // Format map entries (deep-deref values to resolve VarRefs inside structs)
  String mapStr;
  if (mapArg.entries.isEmpty) {
    mapStr = '{}';
  } else {
    final pairs = mapArg.entries.entries.map((e) {
      final derefed = (e.value is Term) ? _deepDeref(rt, e.value) : e.value;
      final val = (derefed is Term) ? formatGroundTerm(derefed) : derefed.toString();
      return '${e.key}: $val';
    }).join(', ');
    mapStr = '{$pairs}';
  }

  final line = 'tagged($id, notify(fmap($mapStr)))';
  final callback = rt.outputCallback;
  if (callback != null) {
    callback(line);
  } else {
    print(line);
  }

  // Pass-through: copy the map so it remains usable
  final copy = MapTerm(Map<Object, Term>.from(mapArg.entries));
  return _bindResult(rt, args[2], copy);
}

/// fofmap_show(AgentId?, FoFMapIn?, FoFMapOut) — Print FoFMap contents to screen.
/// FoFMap is a nested map: HashMap<name, HashMap<name, FLv2>>.
/// Outputs tagged(id, notify(fofmap({...}))) for Flutter UI routing.
BodyKernelResult fofmapShowKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    print('[ABORT] fofmap_show/3: expected 3 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }

  final idArg = _deref(rt, args[0]);
  final id = (idArg is ConstTerm) ? idArg.value.toString() : idArg.toString();

  final mapArg = _deref(rt, args[1]);
  if (mapArg is! MapTerm) {
    print('[ABORT] fofmap_show/3: second argument must be a MapTerm, got ${mapArg.runtimeType}');
    return BodyKernelResult.abort;
  }

  String mapStr;
  if (mapArg.entries.isEmpty) {
    mapStr = '{}';
  } else {
    final outerPairs = mapArg.entries.entries.map((outerEntry) {
      final innerMap = _deref(rt, outerEntry.value);
      String innerStr;
      if (innerMap is MapTerm && innerMap.entries.isNotEmpty) {
        final innerPairs = innerMap.entries.entries.map((e) {
          final derefed = (e.value is Term) ? _deepDeref(rt, e.value) : e.value;
          final val = (derefed is Term) ? formatGroundTerm(derefed) : derefed.toString();
          return '${e.key}: $val';
        }).join(', ');
        innerStr = '{$innerPairs}';
      } else if (innerMap is MapTerm) {
        innerStr = '{}';
      } else {
        innerStr = innerMap.toString();
      }
      return '${outerEntry.key}: $innerStr';
    }).join(', ');
    mapStr = '{$outerPairs}';
  }

  final line = 'tagged($id, notify(fofmap($mapStr)))';
  final callback = rt.outputCallback;
  if (callback != null) {
    callback(line);
  } else {
    print(line);
  }

  final copy = MapTerm(Map<Object, Term>.from(mapArg.entries));
  return _bindResult(rt, args[2], copy);
}

/// map_list_append(MapIn?, Key?, Value?, MapOut) — Append Value to list at Key.
/// Looks up Key in the map. If found and value is a list, prepends Value.
/// If Key not found, creates new entry with [Value].
BodyKernelResult mapListAppendKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 4) {
    print('[ABORT] map_list_append/4: expected 4 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }

  final mapArg = _deref(rt, args[0]);
  if (mapArg is! MapTerm) {
    print('[ABORT] map_list_append/4: first argument must be a MapTerm, got ${mapArg.runtimeType}');
    return BodyKernelResult.abort;
  }

  final key = _extractMapKey(rt, args[1]);
  if (key == null) {
    print('[ABORT] map_list_append/4: second argument must be a ground constant');
    return BodyKernelResult.abort;
  }

  final value = _deref(rt, args[2]);
  final valueTerm = (value is Term) ? value : ConstTerm(value);

  // Get current list (or start with nil = empty list)
  final current = mapArg.entries[key];
  // Prepend value: [Value | CurrentList]
  final newList = StructTerm('.', [valueTerm, current ?? ConstTerm('nil')]);
  mapArg.entries[key] = newList;

  return _bindResult(rt, args[3], mapArg);
}

/// :=(Result, Expr) — Arithmetic assignment.
BodyKernelResult assignKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    print('[ABORT] :=/2: expected 2 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }
  final result = _getNum(rt, args[1]);
  if (result == null) {
    print('[ABORT] :=/2: right-hand side must evaluate to a number');
    return BodyKernelResult.abort;
  }
  final value = result == result.toInt() ? result.toInt() : result;
  return _bindResult(rt, args[0], ConstTerm(value));
}

// =============================================================================
// SharedBroadcastStream Kernels
// =============================================================================

/// sbs_new(Id?, SBS) — Create a new SharedBroadcastStream for agent Id.
BodyKernelResult sbsNewKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    print('[ABORT] sbs_new/2: expected 2 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }
  final idArg = _deref(rt, args[0]);
  final id = (idArg is ConstTerm) ? idArg.value! : idArg!;
  return _bindResult(rt, args[1], SbsTerm(id));
}

/// sbs_add_recipient(SBS?, Name?, SBS1) — Add Name to SBS recipients.
BodyKernelResult sbsAddRecipientKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    print('[ABORT] sbs_add_recipient/3: expected 3 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }
  final sbsArg = _deref(rt, args[0]);
  if (sbsArg is! SbsTerm) {
    print('[ABORT] sbs_add_recipient/3: first argument must be SbsTerm, got ${sbsArg.runtimeType}');
    return BodyKernelResult.abort;
  }
  final nameArg = _deref(rt, args[1]);
  final name = (nameArg is ConstTerm) ? nameArg.value! : nameArg!;
  if (!sbsArg.recipients.contains(name)) {
    sbsArg.recipients.add(name);
  }
  // SRSW: old SBS is dead after read, return the same (mutated) object
  return _bindResult(rt, args[2], sbsArg);
}

/// sbs_write_update(SBS?, Value?, SBS1, RecipientList) — Write update to SBS.
/// Appends Value to pending, increments counter, returns recipients as GLP list.
/// If counter == |recipients|, rotates: checkpoint += pending, pending = [], counter = 0.
BodyKernelResult sbsWriteUpdateKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 4) {
    print('[ABORT] sbs_write_update/4: expected 4 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }
  final sbsArg = _deref(rt, args[0]);
  if (sbsArg is! SbsTerm) {
    print('[ABORT] sbs_write_update/4: first argument must be SbsTerm, got ${sbsArg.runtimeType}');
    return BodyKernelResult.abort;
  }
  final valArg = _deref(rt, args[1]);
  final value = (valArg is Term) ? valArg : ConstTerm(valArg);

  // Append to pending
  sbsArg.pending.add(value);
  sbsArg.updateCounter++;

  // Check for rotation
  if (sbsArg.recipients.isNotEmpty &&
      sbsArg.updateCounter >= sbsArg.recipients.length) {
    // Rotate: checkpoint = checkpoint + pending, pending = [], counter = 0
    sbsArg.checkpoint.addAll(sbsArg.pending);
    sbsArg.pending.clear();
    sbsArg.updateCounter = 0;
  }

  // Build recipients as GLP list
  final recipientTerms = sbsArg.recipients
      .map<Object?>((r) => r is Term ? r : ConstTerm(r))
      .toList();
  final recipientList = _dartListToGlpList(recipientTerms);

  // Bind SBS1 (output SBS — same object, mutated)
  final bindSbs = _bindResult(rt, args[2], sbsArg);
  if (bindSbs == BodyKernelResult.abort) return bindSbs;

  // Bind RecipientList
  return _bindResult(rt, args[3], recipientList);
}

/// sbs_get_checkpoint(SBS?, List) — Get current checkpoint as GLP list.
BodyKernelResult sbsGetCheckpointKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    print('[ABORT] sbs_get_checkpoint/2: expected 2 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }
  final sbsArg = _deref(rt, args[0]);
  if (sbsArg is! SbsTerm) {
    print('[ABORT] sbs_get_checkpoint/2: first argument must be SbsTerm, got ${sbsArg.runtimeType}');
    return BodyKernelResult.abort;
  }
  final list = _dartListToGlpList(sbsArg.checkpoint.cast<Object?>());
  return _bindResult(rt, args[1], list);
}

/// Format a ground term as readable GLP syntax.
///
/// Lists are shown as [a, b, c], atoms as-is, structs as f(a, b).
String formatGroundTerm(Term term) {
  if (term is ConstTerm) {
    if (term.value == 'nil' || term.value == null) return '[]';
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
      if (current is ConstTerm && (current.value == 'nil' || current.value == null)) {
        return '[${elements.join(', ')}]';
      }
      return '[${elements.join(', ')} | ${formatGroundTerm(current)}]';
    }
    final args = term.args.map(formatGroundTerm).join(', ');
    return '${term.functor}($args)';
  }
  return term.toString();
}

// ============================================================================
// MODULE DISPATCH KERNELS
// ============================================================================

/// '_activate'(Module?, Goal) — dispatch Goal directly to the exported procedure.
///
/// Module? is a reader referencing a ModuleTerm (wrapping a BytecodeProgram).
/// Goal is a term representing the remote procedure call (e.g., double(5, F)).
///
/// The kernel extracts the functor and args from the Goal, looks up the
/// procedure in the module's bytecode labels, and spawns it with the original
/// args.  This direct dispatch preserves argument polarity (writer/reader),
/// which is essential for output parameters.
///
/// If the procedure is not found, the goal silently succeeds (fallback
/// behavior matching _select/1's otherwise clause).
BodyKernelResult activateKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    print('[ABORT] _activate/2: expected 2 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }

  // Dereference Module? (first arg) to get the ModuleTerm
  final moduleArg = _deref(rt, args[0]);
  if (moduleArg is! ModuleTerm) {
    print('[ABORT] _activate/2: first argument must be a ModuleTerm, got ${moduleArg.runtimeType}');
    return BodyKernelResult.abort;
  }

  final bytecode = moduleArg.bytecode;
  if (bytecode is! BytecodeProgram) {
    print('[ABORT] _activate/2: ModuleTerm does not contain a BytecodeProgram');
    return BodyKernelResult.abort;
  }

  // Dereference the Goal term (second arg)
  final goalArg = _deref(rt, args[1]);
  if (goalArg is! StructTerm) {
    // Not a structured goal — silently succeed (fallback)
    return BodyKernelResult.success;
  }

  // Extract functor/arity and look up the procedure directly
  final functor = goalArg.functor;
  final arity = goalArg.args.length;
  final label = '$functor/$arity';
  final entryPc = bytecode.labels[label];

  if (entryPc == null) {
    // Procedure not found — silently succeed (fallback behavior)
    return BodyKernelResult.success;
  }

  // Spawn the procedure with the goal's original arguments.
  // Each arg is stored on the heap as a VarRef. For VarRef args (e.g.,
  // unbound writers for output params), storeTermOnHeap returns the
  // existing heap address, preserving writer/reader polarity.
  final argSlots = <int, Term>{};
  for (int i = 0; i < goalArg.args.length; i++) {
    final addr = rt.heap.storeTermOnHeap(goalArg.args[i]);
    argSlots[i] = VarRef(addr);
  }

  final newGoalId = rt.nextGoalId++;
  final env = CallEnv(args: argSlots);
  rt.setGoalEnv(newGoalId, env);
  rt.setGoalProgram(newGoalId, bytecode);

  // Ensure a BytecodeRunner exists for this program in rt.runners
  if (!rt.runners.containsKey(bytecode)) {
    rt.runners[bytecode] = BytecodeRunner(bytecode);
  }

  // Enqueue the goal
  rt.gq.enqueue(GoalRef(newGoalId, entryPc));

  return BodyKernelResult.success;
}
