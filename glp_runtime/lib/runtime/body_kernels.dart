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

  // Identity/copy
  registry.register('_copy', 2, copyKernel);

  // Time operations
  registry.register('_now', 1, nowKernel);

  // MutualRef operations (O(1) stream append)
  registry.register('_allocate_mutual_reference', 2, mutualRefKernel);
  registry.register('_stream_append', 3, streamAppendKernel);
  registry.register('_close_mutual_reference', 1, mutualRefCloseKernel);

  // Equator operations (many-to-one signaling)
  registry.register('_equator', 1, equatorKernel);
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

/// Helper to fully dereference a term
Object? _deref(GlpRuntime rt, Object? term) {
  while (term is VarRef) {
    final val = rt.heap.getValue(term.addr);
    if (val == null) return term;
    term = val;
  }
  return term;
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
// EQUATOR KERNELS (Many-to-One Signaling)
// ============================================================================

BodyKernelResult equatorKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 1) {
    return BodyKernelResult.success;
  }

  final deref = _deref(rt, args[0]);

  if (deref is! StructTerm ||
      deref.functor != '_equator' ||
      deref.args.length != 2) {
    return BodyKernelResult.success;
  }

  final e = deref.args[0];
  final c = _deref(rt, deref.args[1]);

  if (e is VarRef && rt.heap.isWriter(e.addr)) {
    if (!rt.heap.isFullyBound(e.addr)) {
      final List<GoalRef> activations;
      if (c is Term) {
        activations = rt.heap.bindVariable(e.addr, c);
      } else {
        activations = rt.heap.bindVariableConst(e.addr, c);
      }

      for (final act in activations) {
        rt.gq.enqueue(act);
      }
    }
  }

  return BodyKernelResult.success;
}
