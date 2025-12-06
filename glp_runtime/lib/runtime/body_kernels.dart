/// Body kernel infrastructure for GLP arithmetic
///
/// Body kernels are runtime-implemented predicates that:
/// - Execute inline (not spawned as separate goals)
/// - Have two-valued semantics (success or abort)
/// - Are only accessible to system predicates (assign.glp)
/// - Expect all preconditions met (guards should verify before calling)

import 'dart:math' as math;

import 'runtime.dart';
import 'terms.dart';
import 'machine_state.dart' show GoalRef;

/// Result of executing a body kernel
enum BodyKernelResult {
  /// Kernel succeeded - continue execution
  success,

  /// Kernel aborted - fatal error (e.g., type error, unbound reader)
  abort,
}

/// Body kernel function signature
///
/// Takes:
/// - GlpRuntime: access to heap, variable operations
/// - List<Object?>: arguments (readers should be bound, last arg is output writer)
///
/// Returns:
/// - BodyKernelResult indicating success or abort
typedef BodyKernel = BodyKernelResult Function(
  GlpRuntime rt,
  List<Object?> args,
);

/// Registry of body kernels
///
/// Body kernels are registered at runtime initialization and called
/// by system predicates like := defined in assign.glp.
class BodyKernelRegistry {
  final Map<String, BodyKernel> _kernels = {};

  /// Register a body kernel
  void register(String name, int arity, BodyKernel kernel) {
    _kernels['$name/$arity'] = kernel;
  }

  /// Look up a body kernel by name and arity
  BodyKernel? lookup(String name, int arity) => _kernels['$name/$arity'];

  /// Check if a kernel is registered
  bool has(String name, int arity) => _kernels.containsKey('$name/$arity');

  /// Get all registered kernel names
  Iterable<String> get names => _kernels.keys;
}

/// Register all standard body kernels
void registerStandardBodyKernels(BodyKernelRegistry registry) {
  // Arithmetic operations
  registry.register('add', 3, addKernel);
  registry.register('sub', 3, subKernel);
  registry.register('mul', 3, mulKernel);
  registry.register('div', 3, divKernel);
  registry.register('idiv', 3, idivKernel);
  registry.register('mod', 3, modKernel);
  registry.register('neg', 2, negKernel);

  // Math functions
  registry.register('abs_kernel', 2, absKernel);
  registry.register('sqrt_kernel', 2, sqrtKernel);
  registry.register('sin_kernel', 2, sinKernel);
  registry.register('cos_kernel', 2, cosKernel);
  registry.register('tan_kernel', 2, tanKernel);
  registry.register('exp_kernel', 2, expKernel);
  registry.register('ln_kernel', 2, lnKernel);
  registry.register('log10_kernel', 2, log10Kernel);
  registry.register('pow_kernel', 3, powKernel);
  registry.register('asin_kernel', 2, asinKernel);
  registry.register('acos_kernel', 2, acosKernel);
  registry.register('atan_kernel', 2, atanKernel);

  // Type conversions
  registry.register('integer_kernel', 2, integerKernel);
  registry.register('real_kernel', 2, realKernel);
  registry.register('round_kernel', 2, roundKernel);
  registry.register('floor_kernel', 2, floorKernel);
  registry.register('ceil_kernel', 2, ceilKernel);

  // Structure manipulation
  registry.register('list_to_tuple', 2, listToTupleKernel);
  registry.register('tuple_to_list', 2, tupleToListKernel);

  // Identity/copy
  registry.register('copy', 2, copyKernel);

  // Time operations
  registry.register('now', 1, nowKernel);

  // MutualRef operations (O(1) stream append)
  registry.register('kernel_mutual_ref', 2, mutualRefKernel);
  registry.register('kernel_stream_append', 3, streamAppendKernel);
  registry.register('kernel_mutual_ref_close', 1, mutualRefCloseKernel);
}

/// Helper to get numeric value from argument (with arithmetic evaluation)
num? _getNum(GlpRuntime rt, Object? arg) {
  if (arg is num) return arg;
  if (arg is ConstTerm && arg.value is num) return arg.value as num;
  if (arg is VarRef) {
    final term = rt.heap.getValue(arg.varId);
    return _getNum(rt, term); // Recursively evaluate
  }
  if (arg is StructTerm) {
    // Evaluate arithmetic expressions
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
  if (outputArg is VarRef && !outputArg.isReader) {
    // Bind the writer variable to the result value
    // CRITICAL: bindVariable returns goals to reactivate - must enqueue them!
    // Use bindVariable directly for Term values to avoid double-wrapping
    final List<GoalRef> activations;
    if (value is Term) {
      activations = rt.heap.bindVariable(outputArg.varId, value);
    } else {
      activations = rt.heap.bindVariableConst(outputArg.varId, value);
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

/// add(X, Y, Result) - Addition
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

/// sub(X, Y, Result) - Subtraction
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

/// mul(X, Y, Result) - Multiplication
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

/// div(X, Y, Result) - Division (always returns float)
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

/// idiv(X, Y, Result) - Integer division
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

/// mod(X, Y, Result) - Modulo
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

/// neg(X, Result) - Unary negation
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

/// abs_kernel(X, Result) - Absolute value
BodyKernelResult absKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], x.abs());
}

/// sqrt_kernel(X, Result) - Square root
BodyKernelResult sqrtKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null || x < 0) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], math.sqrt(x));
}

/// sin_kernel(X, Result) - Sine
BodyKernelResult sinKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], math.sin(x));
}

/// cos_kernel(X, Result) - Cosine
BodyKernelResult cosKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], math.cos(x));
}

/// tan_kernel(X, Result) - Tangent
BodyKernelResult tanKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], math.tan(x));
}

/// exp_kernel(X, Result) - Exponential
BodyKernelResult expKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], math.exp(x));
}

/// ln_kernel(X, Result) - Natural logarithm
BodyKernelResult lnKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null || x <= 0) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], math.log(x));
}

/// log10_kernel(X, Result) - Base-10 logarithm
BodyKernelResult log10Kernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null || x <= 0) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], math.log(x) / math.ln10);
}

/// pow_kernel(X, Y, Result) - Power
BodyKernelResult powKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  final y = _getNum(rt, args[1]);
  if (x == null || y == null) return BodyKernelResult.abort;
  return _bindResult(rt, args[2], math.pow(x, y));
}

/// asin_kernel(X, Result) - Arc sine
BodyKernelResult asinKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null || x < -1 || x > 1) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], math.asin(x));
}

/// acos_kernel(X, Result) - Arc cosine
BodyKernelResult acosKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null || x < -1 || x > 1) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], math.acos(x));
}

/// atan_kernel(X, Result) - Arc tangent
BodyKernelResult atanKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], math.atan(x));
}

// ============================================================================
// TYPE CONVERSION KERNELS
// ============================================================================

/// integer_kernel(X, Result) - Convert to integer
BodyKernelResult integerKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], x.toInt());
}

/// real_kernel(X, Result) - Convert to float
BodyKernelResult realKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], x.toDouble());
}

/// round_kernel(X, Result) - Round to nearest integer
BodyKernelResult roundKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], x.round());
}

/// floor_kernel(X, Result) - Floor
BodyKernelResult floorKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], x.floor());
}

/// ceil_kernel(X, Result) - Ceiling
BodyKernelResult ceilKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) return BodyKernelResult.abort;
  final x = _getNum(rt, args[0]);
  if (x == null) return BodyKernelResult.abort;
  return _bindResult(rt, args[1], x.ceil());
}

// ============================================================================
// STRUCTURE MANIPULATION KERNELS
// ============================================================================

/// Helper to fully dereference a term (follow VarRefs to their bound values)
Object? _deref(GlpRuntime rt, Object? term) {
  while (term is VarRef) {
    final val = rt.heap.getValue(term.varId);
    if (val == null) return term; // Unbound
    term = val;
  }
  return term;
}

/// Helper to convert Dart list to GLP list structure
Term _dartListToGlpList(List<Object?> items) {
  Term result = ConstTerm('nil'); // Empty list
  for (var i = items.length - 1; i >= 0; i--) {
    final item = items[i];
    final termItem = item is Term ? item : ConstTerm(item);
    result = StructTerm('.', [termItem, result]); // '.' is the cons functor
  }
  return result;
}

/// Helper to convert GLP list to Dart list
List<Object?>? _glpListToDartList(GlpRuntime rt, Object? list) {
  final result = <Object?>[];
  var current = _deref(rt, list);

  while (current != null) {
    // Empty list (nil)
    if (current is ConstTerm && current.value == 'nil') {
      return result;
    }
    // Non-empty list [H|T] - compiler uses '.' as cons functor
    if (current is StructTerm && current.functor == '.' && current.args.length == 2) {
      result.add(_deref(rt, current.args[0]));
      current = _deref(rt, current.args[1]);
    } else {
      // Not a proper list
      return null;
    }
  }
  return result;
}

/// list_to_tuple(List?, Tuple) - Convert list to structure
/// [foo, a, b] -> foo(a, b)
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

  // First element is the functor
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

  // Remaining elements are arguments
  final structArgs = <Term>[];
  for (var i = 1; i < items.length; i++) {
    final item = items[i];
    structArgs.add(item is Term ? item : ConstTerm(item));
  }

  final tuple = StructTerm(functor, structArgs);
  return _bindResult(rt, args[1], tuple);
}

/// tuple_to_list(Tuple?, List) - Convert structure to list
/// foo(a, b) -> [foo, a, b]
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

  // Build list: [functor, arg1, arg2, ...]
  final items = <Object?>[ConstTerm(tupleArg.functor)];
  for (final arg in tupleArg.args) {
    items.add(_deref(rt, arg));  // Dereference each arg
  }

  final list = _dartListToGlpList(items);
  return _bindResult(rt, args[1], list);
}

/// copy(Source?, Target) - Copy value from source to target
/// Used as base case for := when right-hand side is already a number
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

/// now(T) - Bind T to current Unix milliseconds since epoch
/// Always succeeds.
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

/// kernel_mutual_ref(StreamEnd, Ref) - Create MutualRef from unbound writer
///
/// StreamEnd must be an unbound writer variable (the end of a stream).
/// Ref will be bound to a MutualRefTerm pointing to StreamEnd.
///
/// Example: mutual_ref(Tail, Ref) where Tail is unbound
BodyKernelResult mutualRefKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    print('[ABORT] kernel_mutual_ref/2: expected 2 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }

  // First arg must be an unbound writer
  final streamEnd = _deref(rt, args[0]);
  if (streamEnd is! VarRef || streamEnd.isReader) {
    print('[ABORT] kernel_mutual_ref/2: first argument must be an unbound writer');
    return BodyKernelResult.abort;
  }

  // Check that writer is actually unbound
  if (rt.heap.isWriterBound(streamEnd.varId)) {
    print('[ABORT] kernel_mutual_ref/2: writer W${streamEnd.varId} is already bound');
    return BodyKernelResult.abort;
  }

  // Create MutualRef pointing to the unbound writer
  final mutualRef = MutualRefTerm(streamEnd.varId);

  // Bind second arg (output) to the MutualRef
  return _bindResult(rt, args[1], mutualRef);
}

/// kernel_stream_append(Ref, Value, NewEnd) - Append value to stream via MutualRef
///
/// Ref must be a MutualRefTerm (from kernel_mutual_ref).
/// Value is the value to append to the stream.
/// NewEnd will be bound to a reader for the new stream tail.
///
/// Operation:
/// 1. Get current writer from MutualRef
/// 2. Allocate fresh variable for new tail
/// 3. Bind current writer to [Value | NewTail?]
/// 4. Update MutualRef to point to new writer
/// 5. Bind NewEnd to reader of new tail
/// 6. Trigger reader notifications for bound writer
BodyKernelResult streamAppendKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    print('[ABORT] kernel_stream_append/3: expected 3 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }

  // First arg must be MutualRefTerm
  final refArg = _deref(rt, args[0]);
  if (refArg is! MutualRefTerm) {
    print('[ABORT] kernel_stream_append/3: first argument must be a MutualRef');
    return BodyKernelResult.abort;
  }

  // Get the current tail writer from MutualRef
  final currentWriterId = refArg.currentWriterId;

  // Check that current writer is still unbound
  if (rt.heap.isWriterBound(currentWriterId)) {
    print('[ABORT] kernel_stream_append/3: MutualRef points to already-bound writer W$currentWriterId');
    return BodyKernelResult.abort;
  }

  // Get value to append (second arg)
  final value = _deref(rt, args[1]);
  final termValue = value is Term ? value : ConstTerm(value);

  // Allocate fresh variable for new tail
  final newTailId = rt.heap.allocateFreshVar();
  rt.heap.addVariable(newTailId);

  // Build cons cell: '.'(Value, NewTail?)
  final newTailReader = VarRef(newTailId, isReader: true);
  final consCell = StructTerm('.', [termValue, newTailReader]);

  // Bind current writer to the cons cell (triggers notifications!)
  final activations = rt.heap.bindVariable(currentWriterId, consCell);

  // Enqueue all reactivations
  for (final act in activations) {
    rt.gq.enqueue(act);
  }

  // Update MutualRef to point to the new tail's writer
  refArg.currentWriterId = newTailId;

  // Bind third arg (NewEnd) to reader of new tail
  return _bindResult(rt, args[2], newTailReader);
}

/// kernel_mutual_ref_close(Ref) - Close stream by binding tail to []
///
/// Ref must be a MutualRefTerm.
/// Binds the current tail to empty list (nil), closing the stream.
BodyKernelResult mutualRefCloseKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 1) {
    print('[ABORT] kernel_mutual_ref_close/1: expected 1 argument, got ${args.length}');
    return BodyKernelResult.abort;
  }

  // First arg must be MutualRefTerm
  final refArg = _deref(rt, args[0]);
  if (refArg is! MutualRefTerm) {
    print('[ABORT] kernel_mutual_ref_close/1: argument must be a MutualRef');
    return BodyKernelResult.abort;
  }

  // Get the current tail writer from MutualRef
  final currentWriterId = refArg.currentWriterId;

  // Check that current writer is still unbound
  if (rt.heap.isWriterBound(currentWriterId)) {
    print('[ABORT] kernel_mutual_ref_close/1: MutualRef points to already-bound writer W$currentWriterId');
    return BodyKernelResult.abort;
  }

  // Bind current writer to nil (empty list)
  final activations = rt.heap.bindVariable(currentWriterId, ConstTerm('nil'));

  // Enqueue all reactivations
  for (final act in activations) {
    rt.gq.enqueue(act);
  }

  return BodyKernelResult.success;
}
