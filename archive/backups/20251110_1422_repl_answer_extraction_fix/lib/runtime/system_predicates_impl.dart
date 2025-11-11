/// Standard system predicate implementations for GLP
///
/// This module provides the built-in system predicates that can be called
/// via the Execute instruction. These predicates handle:
/// - Arithmetic evaluation
/// - File I/O operations
/// - System information (time, IDs, etc.)

import 'dart:io';

import 'runtime.dart';
import 'system_predicates.dart';
import 'terms.dart';

/// Register all standard system predicates with the runtime
void registerStandardPredicates(SystemPredicateRegistry registry) {
  // Arithmetic
  registry.register('evaluate', evaluatePredicate);

  // Utilities
  registry.register('current_time', currentTimePredicate);
  registry.register('unique_id', uniqueIdPredicate);
  registry.register('variable_name', variableNamePredicate);
  registry.register('copy_term', copyTermPredicate);

  // File I/O
  registry.register('file_read', fileReadPredicate);
  registry.register('file_write', fileWritePredicate);
  registry.register('file_exists', fileExistsPredicate);
  registry.register('file_open', fileOpenPredicate);
  registry.register('file_close', fileClosePredicate);
  registry.register('file_read_handle', fileReadHandlePredicate);
  registry.register('file_write_handle', fileWriteHandlePredicate);

  // Directory operations
  registry.register('directory_list', directoryListPredicate);

  // Terminal I/O
  registry.register('write', writePredicate);
  registry.register('nl', nlPredicate);
  registry.register('read', readPredicate);

  // Module loading
  registry.register('link', linkPredicate);
  registry.register('load_module', loadModulePredicate);

  // Channel primitives (stream distribution)
  registry.register('distribute_stream', distributeStreamPredicate);
  registry.register('copy_term_multi', copyTermMultiPredicate);
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

/// current_time/1: Get current system time
///
/// Usage: execute('current_time', [TimeVar])
///
/// Binds TimeVar to the current Unix timestamp (milliseconds since epoch).
///
/// Behavior:
/// - TimeVar is unbound writer → bind to current timestamp → SUCCESS
/// - TimeVar is bound → verify it matches current time (likely fails) → SUCCESS/FAILURE
/// - Always succeeds if binding to unbound writer
///
/// Example:
///   current_time(T)  % T = 1699364123456
SystemResult currentTimePredicate(GlpRuntime rt, SystemCall call) {
  if (call.args.length != 1) {
    print('[ERROR] current_time/1 requires exactly 1 argument, got ${call.args.length}');
    return SystemResult.failure;
  }

  final timeTerm = call.args[0];
  final now = DateTime.now().millisecondsSinceEpoch;

  if (timeTerm is WriterTerm) {
    final wid = timeTerm.writerId;
    if (rt.heap.isWriterBound(wid)) {
      // Writer already bound - verify it matches current time
      final existingValue = rt.heap.writerValue[wid];
      bool matches = false;
      if (existingValue is ConstTerm && existingValue.value == now) {
        matches = true;
      } else if (existingValue == now) {
        matches = true;
      }
      return matches ? SystemResult.success : SystemResult.failure;
    } else {
      // Bind to current time
      rt.heap.bindWriterConst(wid, now);
      return SystemResult.success;
    }
  } else if (timeTerm is ConstTerm) {
    // Constant - verify it matches current time (unlikely)
    return (timeTerm.value == now) ? SystemResult.success : SystemResult.failure;
  } else {
    // Other term types not supported
    return SystemResult.failure;
  }
}

/// unique_id/1: Generate unique identifier
///
/// Usage: execute('unique_id', [IdVar])
///
/// Generates a unique integer ID and binds it to IdVar.
/// Uses a simple counter that increments with each call.
///
/// Behavior:
/// - IdVar is unbound writer → bind to new unique ID → SUCCESS
/// - IdVar is bound → verify it matches generated ID (likely fails) → SUCCESS/FAILURE
///
/// Example:
///   unique_id(Id1)  % Id1 = 1
///   unique_id(Id2)  % Id2 = 2
int _uniqueIdCounter = 1;

SystemResult uniqueIdPredicate(GlpRuntime rt, SystemCall call) {
  if (call.args.length != 1) {
    print('[ERROR] unique_id/1 requires exactly 1 argument, got ${call.args.length}');
    return SystemResult.failure;
  }

  final idTerm = call.args[0];
  final newId = _uniqueIdCounter++;

  if (idTerm is WriterTerm) {
    final wid = idTerm.writerId;
    if (rt.heap.isWriterBound(wid)) {
      // Writer already bound - verify it matches new ID
      final existingValue = rt.heap.writerValue[wid];
      bool matches = false;
      if (existingValue is ConstTerm && existingValue.value == newId) {
        matches = true;
      } else if (existingValue == newId) {
        matches = true;
      }
      return matches ? SystemResult.success : SystemResult.failure;
    } else {
      // Bind to new unique ID
      rt.heap.bindWriterConst(wid, newId);
      return SystemResult.success;
    }
  } else if (idTerm is ConstTerm) {
    // Constant - verify it matches new ID (unlikely)
    return (idTerm.value == newId) ? SystemResult.success : SystemResult.failure;
  } else {
    // Other term types not supported
    return SystemResult.failure;
  }
}

/// file_read/2: Read file contents
///
/// Usage: execute('file_read', [PathVar, ContentsVar])
///
/// Reads the entire contents of a file as a string.
///
/// Behavior:
/// - PathVar must be ground (string constant or bound writer)
/// - If PathVar is unbound reader → SUSPEND
/// - ContentsVar is unbound writer → bind to file contents → SUCCESS
/// - ContentsVar is bound → verify it matches file contents → SUCCESS/FAILURE
/// - If file doesn't exist or can't be read → FAILURE
///
/// Example:
///   file_read('/path/to/file.txt', Contents)
SystemResult fileReadPredicate(GlpRuntime rt, SystemCall call) {
  if (call.args.length != 2) {
    print('[ERROR] file_read/2 requires exactly 2 arguments, got ${call.args.length}');
    return SystemResult.failure;
  }

  final pathTerm = call.args[0];
  final contentsTerm = call.args[1];

  // Step 1: Extract and verify path
  String? path;

  if (pathTerm is ConstTerm && pathTerm.value is String) {
    path = pathTerm.value as String;
  } else if (pathTerm is WriterTerm) {
    final wid = pathTerm.writerId;
    if (!rt.heap.isWriterBound(wid)) {
      // Unbound writer - fail
      return SystemResult.failure;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm && value.value is String) {
      path = value.value as String;
    }
  } else if (pathTerm is ReaderTerm) {
    final rid = pathTerm.readerId;
    final wid = rt.heap.writerIdForReader(rid);
    if (wid == null || !rt.heap.isWriterBound(wid)) {
      // Unbound reader - suspend
      call.suspendedReaders.add(rid);
      return SystemResult.suspend;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm && value.value is String) {
      path = value.value as String;
    }
  }

  if (path == null) {
    print('[ERROR] file_read/2: path must be a string');
    return SystemResult.failure;
  }

  // Step 2: Read file contents
  String contents;
  try {
    final file = File(path);
    if (!file.existsSync()) {
      print('[ERROR] file_read/2: File not found: $path');
      return SystemResult.failure;
    }
    contents = file.readAsStringSync();
  } catch (e) {
    print('[ERROR] file_read/2: Failed to read file $path: $e');
    return SystemResult.failure;
  }

  // Step 3: Bind or verify contents variable
  if (contentsTerm is WriterTerm) {
    final wid = contentsTerm.writerId;
    if (rt.heap.isWriterBound(wid)) {
      // Writer already bound - verify it matches file contents
      final existingValue = rt.heap.writerValue[wid];
      bool matches = false;
      if (existingValue is ConstTerm && existingValue.value == contents) {
        matches = true;
      } else if (existingValue == contents) {
        matches = true;
      }
      return matches ? SystemResult.success : SystemResult.failure;
    } else {
      // Bind to file contents
      rt.heap.bindWriterConst(wid, contents);
      return SystemResult.success;
    }
  } else if (contentsTerm is ConstTerm) {
    // Constant - verify it matches file contents
    return (contentsTerm.value == contents) ? SystemResult.success : SystemResult.failure;
  } else {
    // Other term types not supported
    return SystemResult.failure;
  }
}

/// file_write/2: Write contents to file
///
/// Usage: execute('file_write', [PathVar, ContentsVar])
///
/// Writes string contents to a file (overwrites if exists).
///
/// Behavior:
/// - PathVar must be ground (string constant or bound writer)
/// - ContentsVar must be ground (string constant or bound writer)
/// - If either is unbound reader → SUSPEND
/// - On success → SUCCESS
/// - If file can't be written → FAILURE
///
/// Example:
///   file_write('/path/to/file.txt', 'Hello, World!')
SystemResult fileWritePredicate(GlpRuntime rt, SystemCall call) {
  if (call.args.length != 2) {
    print('[ERROR] file_write/2 requires exactly 2 arguments, got ${call.args.length}');
    return SystemResult.failure;
  }

  final pathTerm = call.args[0];
  final contentsTerm = call.args[1];

  // Step 1: Extract and verify path
  String? path;

  if (pathTerm is ConstTerm && pathTerm.value is String) {
    path = pathTerm.value as String;
  } else if (pathTerm is WriterTerm) {
    final wid = pathTerm.writerId;
    if (!rt.heap.isWriterBound(wid)) {
      return SystemResult.failure;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm && value.value is String) {
      path = value.value as String;
    }
  } else if (pathTerm is ReaderTerm) {
    final rid = pathTerm.readerId;
    final wid = rt.heap.writerIdForReader(rid);
    if (wid == null || !rt.heap.isWriterBound(wid)) {
      call.suspendedReaders.add(rid);
      return SystemResult.suspend;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm && value.value is String) {
      path = value.value as String;
    }
  }

  if (path == null) {
    print('[ERROR] file_write/2: path must be a string');
    return SystemResult.failure;
  }

  // Step 2: Extract and verify contents
  String? contents;

  if (contentsTerm is ConstTerm && contentsTerm.value is String) {
    contents = contentsTerm.value as String;
  } else if (contentsTerm is WriterTerm) {
    final wid = contentsTerm.writerId;
    if (!rt.heap.isWriterBound(wid)) {
      return SystemResult.failure;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm && value.value is String) {
      contents = value.value as String;
    }
  } else if (contentsTerm is ReaderTerm) {
    final rid = contentsTerm.readerId;
    final wid = rt.heap.writerIdForReader(rid);
    if (wid == null || !rt.heap.isWriterBound(wid)) {
      call.suspendedReaders.add(rid);
      return SystemResult.suspend;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm && value.value is String) {
      contents = value.value as String;
    }
  }

  if (contents == null) {
    print('[ERROR] file_write/2: contents must be a string');
    return SystemResult.failure;
  }

  // Step 3: Write file
  try {
    final file = File(path);
    file.writeAsStringSync(contents);
    return SystemResult.success;
  } catch (e) {
    print('[ERROR] file_write/2: Failed to write file $path: $e');
    return SystemResult.failure;
  }
}

// ============================================================================
// TERMINAL I/O PREDICATES
// ============================================================================

/// write/1: Write term to stdout
///
/// Usage: execute('write', [Term])
///
/// Writes a term to standard output (no newline).
///
/// Behavior:
/// - Term can be constant, bound writer, or reader
/// - If Term is unbound reader → SUSPEND
/// - Otherwise → print to stdout → SUCCESS
///
/// Example:
///   write('Hello')  % prints: Hello
///   write(42)       % prints: 42
SystemResult writePredicate(GlpRuntime rt, SystemCall call) {
  if (call.args.length != 1) {
    print('[ERROR] write/1 requires exactly 1 argument, got ${call.args.length}');
    return SystemResult.failure;
  }

  final term = call.args[0];
  
  // Extract the actual value to write
  Object? value;
  
  if (term is ConstTerm) {
    value = term.value;
  } else if (term is WriterTerm) {
    final wid = term.writerId;
    if (!rt.heap.isWriterBound(wid)) {
      // Unbound writer - fail
      return SystemResult.failure;
    }
    final writerValue = rt.heap.writerValue[wid];
    if (writerValue is ConstTerm) {
      value = writerValue.value;
    } else {
      value = writerValue;
    }
  } else if (term is ReaderTerm) {
    final rid = term.readerId;
    final wid = rt.heap.writerIdForReader(rid);
    if (wid == null || !rt.heap.isWriterBound(wid)) {
      // Unbound reader - suspend
      call.suspendedReaders.add(rid);
      return SystemResult.suspend;
    }
    final writerValue = rt.heap.writerValue[wid];
    if (writerValue is ConstTerm) {
      value = writerValue.value;
    } else {
      value = writerValue;
    }
  } else {
    value = term;
  }

  // Write to stdout
  stdout.write(value);
  return SystemResult.success;
}

/// nl/0: Write newline to stdout
///
/// Usage: execute('nl', [])
///
/// Writes a newline character to standard output.
///
/// Behavior:
/// - Always succeeds
///
/// Example:
///   nl  % prints newline
SystemResult nlPredicate(GlpRuntime rt, SystemCall call) {
  if (call.args.isNotEmpty) {
    print('[ERROR] nl/0 requires no arguments, got ${call.args.length}');
    return SystemResult.failure;
  }

  stdout.writeln();
  return SystemResult.success;
}

/// read/1: Read line from stdin
///
/// Usage: execute('read', [Result])
///
/// Reads a line from standard input and binds it to Result.
///
/// Behavior:
/// - Result is unbound writer → bind to input line → SUCCESS
/// - Result is bound → verify it matches input → SUCCESS/FAILURE
/// - If read fails → FAILURE
///
/// Example:
///   read(Line)  % reads line from stdin, binds to Line
SystemResult readPredicate(GlpRuntime rt, SystemCall call) {
  if (call.args.length != 1) {
    print('[ERROR] read/1 requires exactly 1 argument, got ${call.args.length}');
    return SystemResult.failure;
  }

  final resultTerm = call.args[0];

  // Read line from stdin
  String? line;
  try {
    line = stdin.readLineSync();
    if (line == null) {
      return SystemResult.failure;
    }
  } catch (e) {
    print('[ERROR] read/1: Failed to read from stdin: $e');
    return SystemResult.failure;
  }

  // Bind or verify result
  if (resultTerm is WriterTerm) {
    final wid = resultTerm.writerId;
    if (rt.heap.isWriterBound(wid)) {
      // Verify
      final existingValue = rt.heap.writerValue[wid];
      bool matches = false;
      if (existingValue is ConstTerm && existingValue.value == line) {
        matches = true;
      } else if (existingValue == line) {
        matches = true;
      }
      return matches ? SystemResult.success : SystemResult.failure;
    } else {
      // Bind
      rt.heap.bindWriterConst(wid, line);
      return SystemResult.success;
    }
  } else if (resultTerm is ConstTerm) {
    return (resultTerm.value == line) ? SystemResult.success : SystemResult.failure;
  } else {
    return SystemResult.failure;
  }
}

// ============================================================================
// ADDITIONAL FILE OPERATIONS
// ============================================================================

/// file_exists/1: Check if file exists
///
/// Usage: execute('file_exists', [Path])
///
/// Checks if a file exists at the given path.
///
/// Behavior:
/// - Path must be ground string
/// - If Path is unbound reader → SUSPEND
/// - If file exists → SUCCESS
/// - If file doesn't exist → FAILURE
///
/// Example:
///   file_exists('/path/to/file.txt')
SystemResult fileExistsPredicate(GlpRuntime rt, SystemCall call) {
  if (call.args.length != 1) {
    print('[ERROR] file_exists/1 requires exactly 1 argument, got ${call.args.length}');
    return SystemResult.failure;
  }

  final pathTerm = call.args[0];

  // Extract path
  String? path;
  if (pathTerm is ConstTerm && pathTerm.value is String) {
    path = pathTerm.value as String;
  } else if (pathTerm is WriterTerm) {
    final wid = pathTerm.writerId;
    if (!rt.heap.isWriterBound(wid)) {
      return SystemResult.failure;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm && value.value is String) {
      path = value.value as String;
    }
  } else if (pathTerm is ReaderTerm) {
    final rid = pathTerm.readerId;
    final wid = rt.heap.writerIdForReader(rid);
    if (wid == null || !rt.heap.isWriterBound(wid)) {
      call.suspendedReaders.add(rid);
      return SystemResult.suspend;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm && value.value is String) {
      path = value.value as String;
    }
  }

  if (path == null) {
    print('[ERROR] file_exists/1: path must be a string');
    return SystemResult.failure;
  }

  // Check if file exists
  try {
    final file = File(path);
    return file.existsSync() ? SystemResult.success : SystemResult.failure;
  } catch (e) {
    return SystemResult.failure;
  }
}

/// file_open/3: Open file handle
///
/// Usage: execute('file_open', [Path, Mode, Handle])
///
/// Opens a file and returns a handle ID.
///
/// Behavior:
/// - Path must be ground string
/// - Mode must be 'read', 'write', 'append', or 'read_write'
/// - If Path/Mode is unbound reader → SUSPEND
/// - Handle is unbound writer → bind to handle ID → SUCCESS
/// - If file can't be opened → FAILURE
///
/// Example:
///   file_open('/path/to/file.txt', 'read', H)  % H = 1
SystemResult fileOpenPredicate(GlpRuntime rt, SystemCall call) {
  if (call.args.length != 3) {
    print('[ERROR] file_open/3 requires exactly 3 arguments, got ${call.args.length}');
    return SystemResult.failure;
  }

  final pathTerm = call.args[0];
  final modeTerm = call.args[1];
  final handleTerm = call.args[2];

  // Extract path
  String? path;
  if (pathTerm is ConstTerm && pathTerm.value is String) {
    path = pathTerm.value as String;
  } else if (pathTerm is WriterTerm) {
    final wid = pathTerm.writerId;
    if (!rt.heap.isWriterBound(wid)) {
      return SystemResult.failure;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm && value.value is String) {
      path = value.value as String;
    }
  } else if (pathTerm is ReaderTerm) {
    final rid = pathTerm.readerId;
    final wid = rt.heap.writerIdForReader(rid);
    if (wid == null || !rt.heap.isWriterBound(wid)) {
      call.suspendedReaders.add(rid);
      return SystemResult.suspend;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm && value.value is String) {
      path = value.value as String;
    }
  }

  if (path == null) {
    print('[ERROR] file_open/3: path must be a string');
    return SystemResult.failure;
  }

  // Extract mode
  String? mode;
  if (modeTerm is ConstTerm && modeTerm.value is String) {
    mode = modeTerm.value as String;
  } else if (modeTerm is WriterTerm) {
    final wid = modeTerm.writerId;
    if (!rt.heap.isWriterBound(wid)) {
      return SystemResult.failure;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm && value.value is String) {
      mode = value.value as String;
    }
  } else if (modeTerm is ReaderTerm) {
    final rid = modeTerm.readerId;
    final wid = rt.heap.writerIdForReader(rid);
    if (wid == null || !rt.heap.isWriterBound(wid)) {
      call.suspendedReaders.add(rid);
      return SystemResult.suspend;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm && value.value is String) {
      mode = value.value as String;
    }
  }

  if (mode == null) {
    print('[ERROR] file_open/3: mode must be a string');
    return SystemResult.failure;
  }

  // Open file based on mode
  RandomAccessFile? file;
  try {
    final fileObj = File(path);

    switch (mode) {
      case 'read':
        file = fileObj.openSync(mode: FileMode.read);
        break;
      case 'write':
        file = fileObj.openSync(mode: FileMode.write);
        break;
      case 'append':
        file = fileObj.openSync(mode: FileMode.append);
        break;
      case 'read_write':
        file = fileObj.openSync(mode: FileMode.writeOnly);
        break;
      default:
        print('[ERROR] file_open/3: invalid mode: $mode (must be read/write/append/read_write)');
        return SystemResult.failure;
    }
  } catch (e) {
    print('[ERROR] file_open/3: Failed to open file $path: $e');
    return SystemResult.failure;
  }

  // Allocate handle
  final handle = rt.allocateFileHandle(file);

  // Bind or verify handle
  if (handleTerm is WriterTerm) {
    final wid = handleTerm.writerId;
    if (rt.heap.isWriterBound(wid)) {
      // Verify
      final existingValue = rt.heap.writerValue[wid];
      bool matches = false;
      if (existingValue is ConstTerm && existingValue.value == handle) {
        matches = true;
      } else if (existingValue == handle) {
        matches = true;
      }
      if (!matches) {
        // Close the file we just opened since verification failed
        rt.closeFileHandle(handle);
        return SystemResult.failure;
      }
    } else {
      // Bind to handle
      rt.heap.bindWriterConst(wid, handle);
    }
    return SystemResult.success;
  } else {
    // Close the file since we can't return the handle
    rt.closeFileHandle(handle);
    return SystemResult.failure;
  }
}

/// file_close/1: Close file handle
///
/// Usage: execute('file_close', [Handle])
///
/// Closes an open file handle.
///
/// Behavior:
/// - Handle must be ground integer
/// - If Handle is unbound reader → SUSPEND
/// - If handle is valid → close file → SUCCESS
/// - If handle is invalid → FAILURE
///
/// Example:
///   file_close(H)
SystemResult fileClosePredicate(GlpRuntime rt, SystemCall call) {
  if (call.args.length != 1) {
    print('[ERROR] file_close/1 requires exactly 1 argument, got ${call.args.length}');
    return SystemResult.failure;
  }

  final handleTerm = call.args[0];

  // Extract handle
  int? handle;
  if (handleTerm is ConstTerm && handleTerm.value is int) {
    handle = handleTerm.value as int;
  } else if (handleTerm is WriterTerm) {
    final wid = handleTerm.writerId;
    if (!rt.heap.isWriterBound(wid)) {
      return SystemResult.failure;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm && value.value is int) {
      handle = value.value as int;
    }
  } else if (handleTerm is ReaderTerm) {
    final rid = handleTerm.readerId;
    final wid = rt.heap.writerIdForReader(rid);
    if (wid == null || !rt.heap.isWriterBound(wid)) {
      call.suspendedReaders.add(rid);
      return SystemResult.suspend;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm && value.value is int) {
      handle = value.value as int;
    }
  }

  if (handle == null) {
    print('[ERROR] file_close/1: handle must be an integer');
    return SystemResult.failure;
  }

  // Close the file
  if (rt.isValidHandle(handle)) {
    rt.closeFileHandle(handle);
    return SystemResult.success;
  } else {
    print('[ERROR] file_close/1: invalid file handle: $handle');
    return SystemResult.failure;
  }
}

/// file_read_handle/2: Read from file handle
///
/// Usage: execute('file_read_handle', [Handle, Contents])
///
/// Reads remaining contents from an open file handle as a string.
///
/// Behavior:
/// - Handle must be ground integer
/// - If Handle is unbound reader → SUSPEND
/// - Contents is unbound writer → bind to file contents → SUCCESS
/// - If handle invalid or read fails → FAILURE
///
/// Example:
///   file_read_handle(H, Data)
SystemResult fileReadHandlePredicate(GlpRuntime rt, SystemCall call) {
  if (call.args.length != 2) {
    print('[ERROR] file_read_handle/2 requires exactly 2 arguments, got ${call.args.length}');
    return SystemResult.failure;
  }

  final handleTerm = call.args[0];
  final contentsTerm = call.args[1];

  // Extract handle
  int? handle;
  if (handleTerm is ConstTerm && handleTerm.value is int) {
    handle = handleTerm.value as int;
  } else if (handleTerm is WriterTerm) {
    final wid = handleTerm.writerId;
    if (!rt.heap.isWriterBound(wid)) {
      return SystemResult.failure;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm && value.value is int) {
      handle = value.value as int;
    }
  } else if (handleTerm is ReaderTerm) {
    final rid = handleTerm.readerId;
    final wid = rt.heap.writerIdForReader(rid);
    if (wid == null || !rt.heap.isWriterBound(wid)) {
      call.suspendedReaders.add(rid);
      return SystemResult.suspend;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm && value.value is int) {
      handle = value.value as int;
    }
  }

  if (handle == null) {
    print('[ERROR] file_read_handle/2: handle must be an integer');
    return SystemResult.failure;
  }

  // Get file
  final file = rt.getFile(handle);
  if (file == null) {
    print('[ERROR] file_read_handle/2: invalid file handle: $handle');
    return SystemResult.failure;
  }

  // Read contents
  String contents;
  try {
    final length = file.lengthSync();
    final position = file.positionSync();
    final remaining = length - position;

    if (remaining <= 0) {
      contents = '';
    } else {
      final bytes = file.readSync(remaining);
      contents = String.fromCharCodes(bytes);
    }
  } catch (e) {
    print('[ERROR] file_read_handle/2: Failed to read from handle $handle: $e');
    return SystemResult.failure;
  }

  // Bind or verify contents
  if (contentsTerm is WriterTerm) {
    final wid = contentsTerm.writerId;
    if (rt.heap.isWriterBound(wid)) {
      // Verify
      final existingValue = rt.heap.writerValue[wid];
      bool matches = false;
      if (existingValue is ConstTerm && existingValue.value == contents) {
        matches = true;
      } else if (existingValue == contents) {
        matches = true;
      }
      return matches ? SystemResult.success : SystemResult.failure;
    } else {
      // Bind
      rt.heap.bindWriterConst(wid, contents);
      return SystemResult.success;
    }
  } else {
    return SystemResult.failure;
  }
}

/// file_write_handle/2: Write to file handle
///
/// Usage: execute('file_write_handle', [Handle, Contents])
///
/// Writes string contents to an open file handle.
///
/// Behavior:
/// - Handle must be ground integer
/// - Contents must be ground string
/// - If either is unbound reader → SUSPEND
/// - On success → SUCCESS
/// - If handle invalid or write fails → FAILURE
///
/// Example:
///   file_write_handle(H, 'Hello')
SystemResult fileWriteHandlePredicate(GlpRuntime rt, SystemCall call) {
  if (call.args.length != 2) {
    print('[ERROR] file_write_handle/2 requires exactly 2 arguments, got ${call.args.length}');
    return SystemResult.failure;
  }

  final handleTerm = call.args[0];
  final contentsTerm = call.args[1];

  // Extract handle
  int? handle;
  if (handleTerm is ConstTerm && handleTerm.value is int) {
    handle = handleTerm.value as int;
  } else if (handleTerm is WriterTerm) {
    final wid = handleTerm.writerId;
    if (!rt.heap.isWriterBound(wid)) {
      return SystemResult.failure;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm && value.value is int) {
      handle = value.value as int;
    }
  } else if (handleTerm is ReaderTerm) {
    final rid = handleTerm.readerId;
    final wid = rt.heap.writerIdForReader(rid);
    if (wid == null || !rt.heap.isWriterBound(wid)) {
      call.suspendedReaders.add(rid);
      return SystemResult.suspend;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm && value.value is int) {
      handle = value.value as int;
    }
  }

  if (handle == null) {
    print('[ERROR] file_write_handle/2: handle must be an integer');
    return SystemResult.failure;
  }

  // Extract contents
  String? contents;
  if (contentsTerm is ConstTerm && contentsTerm.value is String) {
    contents = contentsTerm.value as String;
  } else if (contentsTerm is WriterTerm) {
    final wid = contentsTerm.writerId;
    if (!rt.heap.isWriterBound(wid)) {
      return SystemResult.failure;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm && value.value is String) {
      contents = value.value as String;
    }
  } else if (contentsTerm is ReaderTerm) {
    final rid = contentsTerm.readerId;
    final wid = rt.heap.writerIdForReader(rid);
    if (wid == null || !rt.heap.isWriterBound(wid)) {
      call.suspendedReaders.add(rid);
      return SystemResult.suspend;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm && value.value is String) {
      contents = value.value as String;
    }
  }

  if (contents == null) {
    print('[ERROR] file_write_handle/2: contents must be a string');
    return SystemResult.failure;
  }

  // Get file
  final file = rt.getFile(handle);
  if (file == null) {
    print('[ERROR] file_write_handle/2: invalid file handle: $handle');
    return SystemResult.failure;
  }

  // Write contents
  try {
    file.writeStringSync(contents);
    return SystemResult.success;
  } catch (e) {
    print('[ERROR] file_write_handle/2: Failed to write to handle $handle: $e');
    return SystemResult.failure;
  }
}

// ============================================================================
// DIRECTORY OPERATIONS
// ============================================================================

/// directory_list/2: List directory contents
///
/// Usage: execute('directory_list', [Path, List])
///
/// Lists all files and directories in the given directory.
///
/// Behavior:
/// - Path must be ground string
/// - If Path is unbound reader → SUSPEND
/// - List is unbound writer → bind to list of filenames → SUCCESS
/// - List is bound → verify it matches directory contents → SUCCESS/FAILURE
///
/// Example:
///   directory_list('/path/to/dir', Files)
SystemResult directoryListPredicate(GlpRuntime rt, SystemCall call) {
  if (call.args.length != 2) {
    print('[ERROR] directory_list/2 requires exactly 2 arguments, got ${call.args.length}');
    return SystemResult.failure;
  }

  final pathTerm = call.args[0];
  final listTerm = call.args[1];

  // Extract path
  String? path;
  if (pathTerm is ConstTerm && pathTerm.value is String) {
    path = pathTerm.value as String;
  } else if (pathTerm is WriterTerm) {
    final wid = pathTerm.writerId;
    if (!rt.heap.isWriterBound(wid)) {
      return SystemResult.failure;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm && value.value is String) {
      path = value.value as String;
    }
  } else if (pathTerm is ReaderTerm) {
    final rid = pathTerm.readerId;
    final wid = rt.heap.writerIdForReader(rid);
    if (wid == null || !rt.heap.isWriterBound(wid)) {
      call.suspendedReaders.add(rid);
      return SystemResult.suspend;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm && value.value is String) {
      path = value.value as String;
    }
  }

  if (path == null) {
    print('[ERROR] directory_list/2: path must be a string');
    return SystemResult.failure;
  }

  // List directory
  List<String> entries;
  try {
    final dir = Directory(path);
    if (!dir.existsSync()) {
      print('[ERROR] directory_list/2: Directory not found: $path');
      return SystemResult.failure;
    }
    entries = dir.listSync().map((e) => e.path.split('/').last).toList();
  } catch (e) {
    print('[ERROR] directory_list/2: Failed to list directory $path: $e');
    return SystemResult.failure;
  }

  // Bind or verify result (as list of strings)
  if (listTerm is WriterTerm) {
    final wid = listTerm.writerId;
    if (rt.heap.isWriterBound(wid)) {
      // Verify - would need to compare list structures
      print('[WARN] directory_list/2: List verification not fully implemented');
      return SystemResult.failure;
    } else {
      // Bind to list (store as ConstTerm with List value for now)
      rt.heap.bindWriterConst(wid, entries);
      return SystemResult.success;
    }
  } else {
    return SystemResult.failure;
  }
}

// ============================================================================
// UTILITY PREDICATES
// ============================================================================

/// variable_name/2: Get unique name for a variable
///
/// Usage: execute('variable_name', [Var, Name])
///
/// Returns a unique string identifier for a variable (useful for debugging/RPC).
///
/// Behavior:
/// - Var can be writer or reader
/// - Name is unbound writer → bind to unique string → SUCCESS
/// - Name is bound → verify it matches → SUCCESS/FAILURE
///
/// Example:
///   variable_name(X, Name)  % Name = 'W1234' or 'R5678'
SystemResult variableNamePredicate(GlpRuntime rt, SystemCall call) {
  if (call.args.length != 2) {
    print('[ERROR] variable_name/2 requires exactly 2 arguments, got ${call.args.length}');
    return SystemResult.failure;
  }

  final varTerm = call.args[0];
  final nameTerm = call.args[1];

  // Get variable name
  String name;
  if (varTerm is WriterTerm) {
    name = 'W${varTerm.writerId}';
  } else if (varTerm is ReaderTerm) {
    name = 'R${varTerm.readerId}';
  } else {
    print('[ERROR] variable_name/2: first argument must be a variable');
    return SystemResult.failure;
  }

  // Bind or verify name
  if (nameTerm is WriterTerm) {
    final wid = nameTerm.writerId;
    if (rt.heap.isWriterBound(wid)) {
      // Verify
      final existingValue = rt.heap.writerValue[wid];
      bool matches = false;
      if (existingValue is ConstTerm && existingValue.value == name) {
        matches = true;
      } else if (existingValue == name) {
        matches = true;
      }
      return matches ? SystemResult.success : SystemResult.failure;
    } else {
      // Bind
      rt.heap.bindWriterConst(wid, name);
      return SystemResult.success;
    }
  } else if (nameTerm is ConstTerm) {
    return (nameTerm.value == name) ? SystemResult.success : SystemResult.failure;
  } else {
    return SystemResult.failure;
  }
}

/// copy_term/2: Copy a term (shallow copy)
///
/// Usage: execute('copy_term', [Original, Copy])
///
/// Creates a copy of a term.
///
/// Behavior:
/// - Original must be ground
/// - If Original is unbound reader → SUSPEND
/// - Copy is unbound writer → bind to copy → SUCCESS
/// - Copy is bound → verify it matches → SUCCESS/FAILURE
///
/// Note: This is a simplified version - full copy_term would need
/// to handle variable renaming for non-ground terms
///
/// Example:
///   copy_term(f(a,b), C)  % C = f(a,b)
SystemResult copyTermPredicate(GlpRuntime rt, SystemCall call) {
  if (call.args.length != 2) {
    print('[ERROR] copy_term/2 requires exactly 2 arguments, got ${call.args.length}');
    return SystemResult.failure;
  }

  final originalTerm = call.args[0];
  final copyTerm = call.args[1];

  // Extract original value
  Object? original;
  if (originalTerm is ConstTerm) {
    original = originalTerm;
  } else if (originalTerm is WriterTerm) {
    final wid = originalTerm.writerId;
    if (!rt.heap.isWriterBound(wid)) {
      return SystemResult.failure;
    }
    original = rt.heap.writerValue[wid];
  } else if (originalTerm is ReaderTerm) {
    final rid = originalTerm.readerId;
    final wid = rt.heap.writerIdForReader(rid);
    if (wid == null || !rt.heap.isWriterBound(wid)) {
      call.suspendedReaders.add(rid);
      return SystemResult.suspend;
    }
    original = rt.heap.writerValue[wid];
  } else {
    original = originalTerm;
  }

  // Make a copy (shallow for now)
  final copy = original;

  // Bind or verify copy
  if (copyTerm is WriterTerm) {
    final wid = copyTerm.writerId;
    if (rt.heap.isWriterBound(wid)) {
      // Verify
      final existingValue = rt.heap.writerValue[wid];
      return (existingValue == copy) ? SystemResult.success : SystemResult.failure;
    } else {
      // Bind
      if (copy is Term) {
        rt.heap.writerValue[wid] = copy;
      } else {
        rt.heap.bindWriterConst(wid, copy);
      }
      return SystemResult.success;
    }
  } else {
    return SystemResult.failure;
  }
}

// ============================================================================
// MODULE LOADING (PLACEHOLDERS)
// ============================================================================

/// link/2: Link external modules
///
/// Usage: execute('link', [ModuleList, Offset])
///
/// Links external dynamic libraries (C libraries via FFI).
///
/// Behavior:
/// - ModuleList can be: single path string, or list of path strings
/// - If ModuleList is unbound reader → SUSPEND
/// - Offset is unbound writer → bind to library handle → SUCCESS
/// - If library loading fails → FAILURE
///
/// Example:
///   link('libmath.so', Handle)
///   link(['libmath.so', 'libfile.so'], Handle)
SystemResult linkPredicate(GlpRuntime rt, SystemCall call) {
  if (call.args.length != 2) {
    print('[ERROR] link/2 requires exactly 2 arguments, got ${call.args.length}');
    return SystemResult.failure;
  }

  final moduleListTerm = call.args[0];
  final offsetTerm = call.args[1];

  // Extract module list
  List<String>? modulePaths;
  if (moduleListTerm is ConstTerm) {
    final value = moduleListTerm.value;
    if (value is String) {
      modulePaths = [value];
    } else if (value is List) {
      // Validate all elements are strings
      if (value.every((e) => e is String)) {
        modulePaths = value.cast<String>();
      } else {
        print('[ERROR] link/2: ModuleList contains non-string elements');
        return SystemResult.failure;
      }
    } else {
      print('[ERROR] link/2: ModuleList must be string or list of strings');
      return SystemResult.failure;
    }
  } else if (moduleListTerm is ReaderTerm) {
    final rid = moduleListTerm.readerId;
    final wid = rt.heap.writerIdForReader(rid);
    if (wid == null || !rt.heap.isWriterBound(wid)) {
      call.suspendedReaders.add(rid);
      return SystemResult.suspend;
    }

    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm) {
      final constValue = value.value;
      if (constValue is String) {
        modulePaths = [constValue];
      } else if (constValue is List && constValue.every((e) => e is String)) {
        modulePaths = constValue.cast<String>();
      }
    }
  } else if (moduleListTerm is WriterTerm) {
    final wid = moduleListTerm.writerId;
    if (!rt.heap.isWriterBound(wid)) {
      print('[ERROR] link/2: ModuleList writer is unbound');
      return SystemResult.failure;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm) {
      final constValue = value.value;
      if (constValue is String) {
        modulePaths = [constValue];
      } else if (constValue is List && constValue.every((e) => e is String)) {
        modulePaths = constValue.cast<String>();
      }
    }
  }

  if (modulePaths == null) {
    print('[ERROR] link/2: Could not extract module paths');
    return SystemResult.failure;
  }

  // Load libraries (for now, just load the first one as a proof of concept)
  // In full implementation, you'd load all libraries and return some composite handle
  try {
    final handle = rt.loadLibrary(modulePaths.first);

    // Bind offset to handle
    if (offsetTerm is WriterTerm) {
      final wid = offsetTerm.writerId;
      if (rt.heap.isWriterBound(wid)) {
        // Verify match
        final existing = rt.heap.writerValue[wid];
        if (existing is ConstTerm && existing.value == handle) {
          return SystemResult.success;
        } else {
          return SystemResult.failure;
        }
      } else {
        rt.heap.bindWriterConst(wid, handle);
        return SystemResult.success;
      }
    } else if (offsetTerm is ConstTerm) {
      // Verify handle matches
      return (offsetTerm.value == handle) ? SystemResult.success : SystemResult.failure;
    } else {
      print('[ERROR] link/2: Offset must be writer or constant');
      return SystemResult.failure;
    }
  } catch (e) {
    print('[ERROR] link/2: Failed to load libraries: $e');
    return SystemResult.failure;
  }
}

/// load_module/2: Load compiled GLP module
///
/// Usage: execute('load_module', [FileName, Module])
///
/// Loads a compiled GLP bytecode module from disk.
///
/// Behavior:
/// - FileName must be ground string path
/// - If FileName is unbound reader → SUSPEND
/// - Module is unbound writer → bind to loaded module data → SUCCESS
/// - If file not found or deserialization fails → FAILURE
///
/// Note: Currently loads as raw data. Full implementation needs:
/// - Bytecode format specification
/// - Deserialization to instruction list
/// - Module metadata (exports, imports)
///
/// Example:
///   load_module('mymodule.glp', M)
SystemResult loadModulePredicate(GlpRuntime rt, SystemCall call) {
  if (call.args.length != 2) {
    print('[ERROR] load_module/2 requires exactly 2 arguments, got ${call.args.length}');
    return SystemResult.failure;
  }

  final fileNameTerm = call.args[0];
  final moduleTerm = call.args[1];

  // Extract filename
  String? filePath;
  if (fileNameTerm is ConstTerm && fileNameTerm.value is String) {
    filePath = fileNameTerm.value as String;
  } else if (fileNameTerm is ReaderTerm) {
    final rid = fileNameTerm.readerId;
    final wid = rt.heap.writerIdForReader(rid);
    if (wid == null || !rt.heap.isWriterBound(wid)) {
      call.suspendedReaders.add(rid);
      return SystemResult.suspend;
    }

    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm && value.value is String) {
      filePath = value.value as String;
    }
  } else if (fileNameTerm is WriterTerm) {
    final wid = fileNameTerm.writerId;
    if (!rt.heap.isWriterBound(wid)) {
      print('[ERROR] load_module/2: FileName writer is unbound');
      return SystemResult.failure;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm && value.value is String) {
      filePath = value.value as String;
    }
  }

  if (filePath == null) {
    print('[ERROR] load_module/2: Could not extract filename');
    return SystemResult.failure;
  }

  // Load file contents
  // In a full implementation, this would deserialize bytecode
  // For now, just read the raw data as proof of concept
  try {
    final file = File(filePath);
    if (!file.existsSync()) {
      print('[ERROR] load_module/2: File not found: $filePath');
      return SystemResult.failure;
    }

    final contents = file.readAsStringSync();

    // TODO: Deserialize bytecode format
    // For now, return contents as a Map with metadata
    final module = {
      'path': filePath,
      'contents': contents,
      'loaded_at': DateTime.now().millisecondsSinceEpoch,
    };

    // Bind module to result
    if (moduleTerm is WriterTerm) {
      final wid = moduleTerm.writerId;
      if (rt.heap.isWriterBound(wid)) {
        // Verify match (would need deep equality for maps)
        print('[WARN] load_module/2: Module writer already bound, cannot verify');
        return SystemResult.failure;
      } else {
        rt.heap.bindWriterConst(wid, module);
        return SystemResult.success;
      }
    } else if (moduleTerm is ConstTerm) {
      // Verify module matches
      print('[WARN] load_module/2: Cannot verify against constant module');
      return SystemResult.failure;
    } else {
      print('[ERROR] load_module/2: Module must be writer or constant');
      return SystemResult.failure;
    }
  } catch (e) {
    print('[ERROR] load_module/2: Failed to load module: $e');
    return SystemResult.failure;
  }
}

// ============================================================================
// CHANNEL PRIMITIVES (STREAM DISTRIBUTION)
// ============================================================================

/// distribute_stream/2: Copy stream to multiple outputs (1-to-N distribution)
///
/// Usage: execute('distribute_stream', [Input, OutputList])
///
/// Distributes a stream (list) to multiple output writers by deep copying.
///
/// Behavior:
/// - Input must be ground list (all elements bound)
/// - OutputList must be list of unbound writers
/// - If Input is unbound reader → SUSPEND
/// - Copies Input to each output writer → SUCCESS
/// - If any output already bound or type mismatch → FAILURE
///
/// Example:
///   distribute_stream([a,b,c], [W1, W2])  % W1 = [a,b,c], W2 = [a,b,c]
SystemResult distributeStreamPredicate(GlpRuntime rt, SystemCall call) {
  if (call.args.length != 2) {
    print('[ERROR] distribute_stream/2 requires exactly 2 arguments, got ${call.args.length}');
    return SystemResult.failure;
  }

  final inputTerm = call.args[0];
  final outputListTerm = call.args[1];

  // Extract and validate input
  Object? inputValue;
  if (inputTerm is ConstTerm) {
    inputValue = inputTerm.value;
  } else if (inputTerm is ReaderTerm) {
    final rid = inputTerm.readerId;
    final wid = rt.heap.writerIdForReader(rid);
    if (wid == null || !rt.heap.isWriterBound(wid)) {
      call.suspendedReaders.add(rid);
      return SystemResult.suspend;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm) {
      inputValue = value.value;
    } else {
      inputValue = value;
    }
  } else if (inputTerm is WriterTerm) {
    final wid = inputTerm.writerId;
    if (!rt.heap.isWriterBound(wid)) {
      print('[ERROR] distribute_stream/2: Input writer is unbound');
      return SystemResult.failure;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm) {
      inputValue = value.value;
    } else {
      inputValue = value;
    }
  } else {
    inputValue = inputTerm;
  }

  // Extract output list
  List<int>? outputWriters;
  if (outputListTerm is ConstTerm && outputListTerm.value is List) {
    // List of WriterTerm objects
    final list = outputListTerm.value as List;
    outputWriters = [];
    for (final item in list) {
      if (item is WriterTerm) {
        outputWriters.add(item.writerId);
      } else if (item is int) {
        outputWriters.add(item);
      } else {
        print('[ERROR] distribute_stream/2: OutputList contains non-writer: $item');
        return SystemResult.failure;
      }
    }
  } else {
    print('[ERROR] distribute_stream/2: OutputList must be list of writers');
    return SystemResult.failure;
  }

  // Distribute (deep copy) input to each output
  for (final wid in outputWriters) {
    if (rt.heap.isWriterBound(wid)) {
      print('[ERROR] distribute_stream/2: Output writer $wid already bound');
      return SystemResult.failure;
    }
    // Deep copy the input value
    final copy = _deepCopyValue(inputValue);
    rt.heap.bindWriterConst(wid, copy);
  }

  return SystemResult.success;
}

/// copy_term/3: Deep copy term to two outputs
///
/// Usage: execute('copy_term', [Term, Copy1, Copy2])
///
/// Creates two independent deep copies of a term.
///
/// Behavior:
/// - Term is input (can be constant, bound writer, or reader)
/// - If Term is unbound reader → SUSPEND
/// - Copy1 and Copy2 are unbound writers → bind to deep copies → SUCCESS
/// - If outputs already bound → FAILURE
///
/// Example:
///   copy_term([a,b], W1, W2)  % W1 = [a,b], W2 = [a,b] (independent)
SystemResult copyTermMultiPredicate(GlpRuntime rt, SystemCall call) {
  if (call.args.length != 3) {
    print('[ERROR] copy_term/3 requires exactly 3 arguments, got ${call.args.length}');
    return SystemResult.failure;
  }

  final termArg = call.args[0];
  final copy1Arg = call.args[1];
  final copy2Arg = call.args[2];

  // Extract source term
  Object? sourceValue;
  if (termArg is ConstTerm) {
    sourceValue = termArg.value;
  } else if (termArg is ReaderTerm) {
    final rid = termArg.readerId;
    final wid = rt.heap.writerIdForReader(rid);
    if (wid == null || !rt.heap.isWriterBound(wid)) {
      call.suspendedReaders.add(rid);
      return SystemResult.suspend;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm) {
      sourceValue = value.value;
    } else {
      sourceValue = value;
    }
  } else if (termArg is WriterTerm) {
    final wid = termArg.writerId;
    if (!rt.heap.isWriterBound(wid)) {
      print('[ERROR] copy_term/3: Source writer is unbound');
      return SystemResult.failure;
    }
    final value = rt.heap.writerValue[wid];
    if (value is ConstTerm) {
      sourceValue = value.value;
    } else {
      sourceValue = value;
    }
  } else {
    sourceValue = termArg;
  }

  // Extract output writers
  int? w1, w2;
  if (copy1Arg is WriterTerm) {
    w1 = copy1Arg.writerId;
  } else {
    print('[ERROR] copy_term/3: Copy1 must be writer');
    return SystemResult.failure;
  }

  if (copy2Arg is WriterTerm) {
    w2 = copy2Arg.writerId;
  } else {
    print('[ERROR] copy_term/3: Copy2 must be writer');
    return SystemResult.failure;
  }

  // Check outputs are unbound
  if (rt.heap.isWriterBound(w1)) {
    print('[ERROR] copy_term/3: Copy1 writer already bound');
    return SystemResult.failure;
  }
  if (rt.heap.isWriterBound(w2)) {
    print('[ERROR] copy_term/3: Copy2 writer already bound');
    return SystemResult.failure;
  }

  // Create two independent deep copies
  final copy1 = _deepCopyValue(sourceValue);
  final copy2 = _deepCopyValue(sourceValue);

  rt.heap.bindWriterConst(w1, copy1);
  rt.heap.bindWriterConst(w2, copy2);

  return SystemResult.success;
}

/// Deep copy helper for stream distribution
Object? _deepCopyValue(Object? value) {
  if (value == null) return null;

  if (value is List) {
    return value.map((e) => _deepCopyValue(e)).toList();
  } else if (value is Map) {
    return value.map((k, v) => MapEntry(k, _deepCopyValue(v)));
  } else if (value is Set) {
    return value.map((e) => _deepCopyValue(e)).toSet();
  } else if (value is String || value is num || value is bool) {
    // Primitives are immutable in Dart, safe to share
    return value;
  } else {
    // For other types, return as-is (may need custom deep copy logic)
    return value;
  }
}
