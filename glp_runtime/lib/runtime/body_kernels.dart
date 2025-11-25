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
    // CRITICAL: bindVariableConst returns goals to reactivate - must enqueue them!
    final activations = rt.heap.bindVariableConst(outputArg.varId, value);
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
