/// Standard system predicate implementations for GLP
///
/// This module provides the built-in system predicates that can be called
/// via the Execute instruction. These predicates handle:
/// - Arithmetic evaluation
/// - File I/O operations
/// - System information (time, IDs, etc.)

import 'runtime.dart';
import 'system_predicates.dart';
import 'terms.dart';

/// Register all standard system predicates with the runtime
void registerStandardPredicates(SystemPredicateRegistry registry) {
  registry.register('evaluate', evaluatePredicate);
  // More predicates will be registered here as they are implemented
}

/// evaluate/2: Arithmetic evaluation
///
/// Usage: execute('evaluate', [ExpressionVar, ResultVar])
///
/// Evaluates an arithmetic expression and unifies the result with the output variable.
/// The expression can contain:
/// - Constants (integers, floats)
/// - Arithmetic operators (+, -, *, /, mod)
/// - Nested expressions
///
/// Behavior:
/// - If ExpressionVar contains unbound readers → SUSPEND (add readers to suspendedReaders)
/// - If ExpressionVar is ground → evaluate and bind ResultVar to result → SUCCESS
/// - If ResultVar is a writer → bind it to the result
/// - If ResultVar is a reader → verify it matches the result (fail if mismatch)
/// - If evaluation fails (e.g., division by zero, type error) → FAILURE
///
/// Example:
///   evaluate(+(2, *(3, 4)), R)  % R = 14
///   evaluate(+(X?, 5), R)       % Suspend on X
SystemResult evaluatePredicate(GlpRuntime rt, SystemCall call) {
  if (call.args.length != 2) {
    print('[ERROR] evaluate/2 requires exactly 2 arguments, got ${call.args.length}');
    return SystemResult.failure;
  }

  final exprTerm = call.args[0];
  final resultTerm = call.args[1];

  // Step 1: Check if expression is ground (no unbound variables)
  final unboundReaders = <int>{};
  bool hasUnboundWriter = false;

  void collectUnbound(Object? term) {
    if (term is WriterTerm) {
      final wid = term.writerId;
      if (!rt.heap.isWriterBound(wid)) {
        hasUnboundWriter = true;
      } else {
        // Writer is bound - check its value recursively
        final value = rt.heap.writerValue[wid];
        if (value != null) {
          collectUnbound(value);
        }
      }
    } else if (term is ReaderTerm) {
      final rid = term.readerId;
      final wid = rt.heap.writerIdForReader(rid);
      if (wid != null) {
        if (!rt.heap.isWriterBound(wid)) {
          unboundReaders.add(rid);
        } else {
          // Reader's writer is bound - check value recursively
          final value = rt.heap.writerValue[wid];
          if (value != null) {
            collectUnbound(value);
          }
        }
      }
    } else if (term is StructTerm) {
      for (final arg in term.args) {
        collectUnbound(arg);
      }
    }
    // Constants are always ground
  }

  collectUnbound(exprTerm);

  // If expression has unbound writers, fail
  if (hasUnboundWriter) {
    return SystemResult.failure;
  }

  // If expression has unbound readers, suspend
  if (unboundReaders.isNotEmpty) {
    call.suspendedReaders.addAll(unboundReaders);
    return SystemResult.suspend;
  }

  // Step 2: Expression is ground - evaluate it
  final result = _evaluate(rt, exprTerm);
  if (result == null) {
    // Evaluation failed (e.g., type error, division by zero)
    return SystemResult.failure;
  }

  // Step 3: Bind or verify result variable
  if (resultTerm is WriterTerm) {
    // ResultVar is a writer - bind it to the result
    final wid = resultTerm.writerId;
    if (rt.heap.isWriterBound(wid)) {
      // Writer already bound - verify it matches the result
      final existingValue = rt.heap.writerValue[wid];

      // Compare the result with the existing value
      // Need to handle both Term types and primitive types
      bool matches = false;
      if (existingValue is ConstTerm && existingValue.value == result) {
        matches = true;
      } else if (existingValue == result) {
        matches = true;
      }

      if (!matches) {
        return SystemResult.failure;
      }
    } else {
      // Writer is unbound - bind it to the result
      // System predicates in BODY phase can directly mutate the heap
      if (result is num || result is String || result is bool || result == null) {
        rt.heap.bindWriterConst(wid, result);
      } else if (result is Term) {
        // Result is already a term - store it directly
        rt.heap.writerValue[wid] = result;
      } else {
        // Unknown result type
        return SystemResult.failure;
      }
    }
    return SystemResult.success;
  } else if (resultTerm is ReaderTerm) {
    // ResultVar is a reader - verify its writer's value matches the result
    final rid = resultTerm.readerId;
    final wid = rt.heap.writerIdForReader(rid);
    if (wid == null) {
      return SystemResult.failure;
    }
    if (!rt.heap.isWriterBound(wid)) {
      // Reader is unbound - cannot verify
      return SystemResult.failure;
    }
    final writerValue = rt.heap.writerValue[wid];
    if (writerValue != result) {
      return SystemResult.failure;
    }
    return SystemResult.success;
  } else {
    // ResultVar is a constant - verify it matches the result
    if (resultTerm != result) {
      return SystemResult.failure;
    }
    return SystemResult.success;
  }
}

/// Internal helper: Evaluate an arithmetic expression
/// Returns null if evaluation fails
Object? _evaluate(GlpRuntime rt, Object? term) {
  // Dereference writers and readers
  if (term is WriterTerm) {
    final wid = term.writerId;
    if (!rt.heap.isWriterBound(wid)) {
      return null; // Unbound writer - should have been caught earlier
    }
    final value = rt.heap.writerValue[wid];
    return _evaluate(rt, value);
  }

  if (term is ReaderTerm) {
    final rid = term.readerId;
    final wid = rt.heap.writerIdForReader(rid);
    if (wid == null || !rt.heap.isWriterBound(wid)) {
      return null; // Unbound reader - should have been caught earlier
    }
    final value = rt.heap.writerValue[wid];
    return _evaluate(rt, value);
  }

  // Handle ConstTerm wrapper
  if (term is ConstTerm) {
    return term.value;
  }

  // Handle constants (numbers)
  if (term is int || term is double) {
    return term;
  }

  // Handle structures (arithmetic operators)
  if (term is StructTerm) {
    final functor = term.functor;
    final args = term.args;

    switch (functor) {
      case '+':
        if (args.length == 2) {
          final left = _evaluate(rt, args[0]);
          final right = _evaluate(rt, args[1]);
          if (left is num && right is num) {
            return left + right;
          }
        }
        break;

      case '-':
        if (args.length == 2) {
          final left = _evaluate(rt, args[0]);
          final right = _evaluate(rt, args[1]);
          if (left is num && right is num) {
            return left - right;
          }
        }
        break;

      case '*':
        if (args.length == 2) {
          final left = _evaluate(rt, args[0]);
          final right = _evaluate(rt, args[1]);
          if (left is num && right is num) {
            return left * right;
          }
        }
        break;

      case '/':
        if (args.length == 2) {
          final left = _evaluate(rt, args[0]);
          final right = _evaluate(rt, args[1]);
          if (left is num && right is num) {
            if (right == 0) {
              return null; // Division by zero
            }
            return left / right;
          }
        }
        break;

      case 'mod':
        if (args.length == 2) {
          final left = _evaluate(rt, args[0]);
          final right = _evaluate(rt, args[1]);
          if (left is int && right is int) {
            if (right == 0) {
              return null; // Division by zero
            }
            return left % right;
          }
        }
        break;

      default:
        // Unknown operator
        return null;
    }
  }

  // Unknown term type or evaluation failed
  return null;
}
